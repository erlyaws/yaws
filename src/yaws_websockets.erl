%%%----------------------------------------------------------------------
%%% File    : yaws_websockets.erl
%%% Author  : Davide Marques <nesrait@gmail.com>
%%% Purpose : support for WebSockets
%%% Created : 18 Dec 2009 by Davide Marques <nesrait@gmail.com>
%%% Modified: extensively revamped in 2011 by J.D. Bothma <jbothma@gmail.com>
%%%----------------------------------------------------------------------

-module(yaws_websockets).
-author('nesrait@gmail.com').
-author('jbothma@gmail.com').
-behaviour(gen_server).

-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").

-include_lib("kernel/include/file.hrl").


%% RFC 6455 section 7.4.1: Defined Status Codes
-define(WS_STATUS_NORMAL,           1000).
-define(WS_STATUS_PROTO_ERROR,      1002).
-define(WS_STATUS_ABNORMAL_CLOSURE, 1006).
-define(WS_STATUS_INVALID_PAYLOAD,  1007).
-define(WS_STATUS_MSG_TOO_BIG,      1009).
-define(WS_STATUS_INTERNAL_ERROR,   1011).


-record(state, {arg,
                opts,
                wsstate,
                cbinfo,
                cbstate,
                cbtype,
                timeout=infinity,
                wait_pong_frame=false,
                close_frame_received=false,
                close_timer,
                reason=normal}).

-record(cbinfo, {module,
                 init_fun,
                 terminate_fun,
                 open_fun,
                 msg_fun_1,
                 msg_fun_2,
                 info_fun}).

-export([start/3, send/2, close/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%----------------------------------------------------------------------
%% API
%%%----------------------------------------------------------------------
start(Arg, CallbackMod, Opts) ->
    PrepdOpts = preprocess_opts(Opts),

    %% Do some checks on request headers before starting the WebSocket process:
    %%   o "Connection:" header field should contain "websocket"
    %%   o "Upgrade:" header field should contain "upgrade"
    %%   o "Origin:" header field should be valid
    case check_connection(query_header("connection", Arg#arg.headers)) of
        ok ->
            ok;
        error ->
            error_logger:error_msg("Invalid connection header", []),
            deliver_xxx(Arg#arg.clisock, 400),
            exit(normal)
    end,

    case check_upgrade(query_header("upgrade", Arg#arg.headers)) of
        ok ->
            ok;
        error ->
            error_logger:error_msg("Invalid upgrade header", []),
            deliver_xxx(Arg#arg.clisock, 400),
            exit(normal)
    end,
    OriginOpt = get_opts(origin, PrepdOpts),
    Origin    = get_origin_header(Arg#arg.headers),
    case check_origin(Origin, OriginOpt) of
        ok ->
            ok;
        error ->
            error_logger:error_msg("Expected origin ~p but found ~p",
                                   [OriginOpt, Origin]),
            deliver_xxx(Arg#arg.clisock, 403),
            exit(normal)
    end,

    %% Start websocket process
    OwnerPid =
        case gen_server:start(?MODULE, [Arg, CallbackMod, PrepdOpts], []) of
            {ok, Pid} ->
                Pid;
            {error, Reason1} ->
                error_logger:error_msg("Failed to start websocket process: ~p",
                                       [Reason1]),
                deliver_xxx(Arg#arg.clisock, 500),
                exit(normal)
        end,

    %% Change the controller process of the client socket
    CliSock = Arg#arg.clisock,
    Res = case yaws_api:get_sslsocket(CliSock) of
              {ok, SslSocket} ->
                  ssl:setopts(SslSocket, [{packet, raw}, {active, once}]),
                  ssl:controlling_process(SslSocket, OwnerPid);
              undefined ->
                  inet:setopts(CliSock, [{packet, raw}, {active, once}]),
                  gen_tcp:controlling_process(CliSock, OwnerPid)
          end,
    case Res of
        ok ->
            gen_server:cast(OwnerPid, ok);
        {error, Reason2} ->
            error_logger:error_msg("Failed to start websocket process: ~p",
                                   [Reason2]),
            gen_server:cast(OwnerPid, {stop, Reason2}),
            deliver_xxx(Arg#arg.clisock, 500)
    end,
    exit(normal).


send(#ws_state{}=WSState, {Type, Data}) ->
    do_send(WSState, {Type, Data});
send(#ws_state{}=WSState, #ws_frame{}=Frame) ->
    do_send(WSState, Frame);
send(Pid, {Type, Data}) ->
    gen_server:cast(Pid, {send, {Type, Data}});
send(Pid, #ws_frame{}=Frame) ->
    gen_server:cast(Pid, {send, Frame}).

close(#ws_state{}=WSState, Reason) ->
    do_close(WSState, Reason);
close(Pid, Reason) ->
    gen_server:cast(Pid, {close, Reason}).

%%%----------------------------------------------------------------------
%% gen_server functions
%%%----------------------------------------------------------------------
init([Arg, CbMod, Opts]) ->
    case get_opts(auto_fragment_message, Opts) of
        true ->
            AutoFrag = {true, get_opts(auto_fragment_threshold, Opts)},
            put(auto_fragment_message, AutoFrag);
        false ->
            put(auto_fragment_message, false)
    end,
    {CbType, FrameState, InitState} =
        case get_opts(callback, Opts) of
            {basic,    St} -> {basic, {none, <<>>}, St};
            {advanced, St} -> {advanced, undefined, St};
            _              -> {basic, {none, <<>>}, []}
        end,
    KeepAliveTout = case get_opts(keepalive, Opts) of
                        true  -> get_opts(keepalive_timeout, Opts);
                        false -> infinity
                    end,
    case get_callback_info(CbMod) of
        {ok, #cbinfo{init_fun=undefined}=CbInfo} ->
            {ok, #state{arg     = Arg,
                        opts    = Opts,
                        cbinfo  = CbInfo,
                        cbtype  = CbType,
                        cbstate = {FrameState,InitState},
                        timeout = KeepAliveTout}, KeepAliveTout};
        {ok, #cbinfo{init_fun=InitFun}=CbInfo} ->
            case CbMod:InitFun([Arg, InitState]) of
                {ok, InitState1} ->
                    {ok, #state{arg     = Arg,
                                opts    = Opts,
                                cbinfo  = CbInfo,
                                cbtype  = CbType,
                                cbstate = {FrameState,InitState1},
                                timeout = KeepAliveTout}, KeepAliveTout};
                {ok, InitState1, Timeout} ->
                    {ok, #state{arg     = Arg,
                                opts    = Opts,
                                cbinfo  = CbInfo,
                                cbtype  = CbType,
                                cbstate = {FrameState,InitState1},
                                timeout = KeepAliveTout}, Timeout};
                {error, Reason} ->
                    {stop, Reason}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.


%% ----
%% Skip all sync requests
handle_call(_Req, _From, State) ->
    {reply, ok, State, State#state.timeout}.


%% ----
%% Send the handshake response
handle_cast(ok, #state{arg=Arg, cbinfo=CbInfo}=State) ->
    CliSock = Arg#arg.clisock,

    WSVersion = ws_version(Arg#arg.headers),
    WSKey     = get_nonce_header(Arg#arg.headers),
    if
        element(1, WSVersion) == unsupported_version ->
            error_logger:error_msg("Unsupported protocol version", []),
            deliver_xxx(CliSock, 400, ["Sec-WebSocket-Version: 13, 8"]),
            {stop, normal, State#state{reason={error, WSVersion}}};

        undefined == WSKey ->
            error_logger:error_msg("No key found", []),
            deliver_xxx(CliSock, 400),
            {stop, normal, State#state{reason={error, key_notfound}}};

        true ->
            Protocol = get_protocol_header(Arg#arg.headers),
            WSState  = #ws_state{sock      = CliSock,
                                 vsn       = WSVersion,
                                 frag_type = none},
            handshake(CliSock, WSKey, Protocol),

            case CbInfo#cbinfo.open_fun of
                undefined ->
                    {noreply, State#state{wsstate=WSState},
                     State#state.timeout};
                OpenFun ->
                    CbMod = CbInfo#cbinfo.module,
                    {FrameState, CbState} = State#state.cbstate,
                    case CbMod:OpenFun(WSState, CbState) of
                        {ok, CbState1} ->
                            {noreply,
                             State#state{wsstate=WSState,
                                         cbstate={FrameState,CbState1}},
                             State#state.timeout};
                        {error, Reason} ->
                            do_close(WSState, Reason),
                            {stop, normal, State#state{reason={error, Reason}}}
                    end
            end
    end;

%% ----
%% Stop the current websocket process. Used when an error occured during process
%% startup.
handle_cast({stop, Reason}, State) ->
    {stop, normal, State#state{reason={error,Reason}}};

%% Send a frame to the client
handle_cast({send, {Type, Data}}, #state{wsstate=WSState}=State) ->
    do_send(WSState, {Type, Data}),
    {noreply, State, State#state.timeout};
handle_cast({send, #ws_frame{}=Frame}, #state{wsstate=WSState}=State) ->
    do_send(WSState, Frame),
    {noreply, State, State#state.timeout};
handle_cast({close, Reason}, #state{wsstate=WSState}=State) ->
    do_close(WSState, Reason),
    {noreply, State, State#state.timeout};

%% Skip all other async messages
handle_cast(_Msg, State) ->
    {noreply, State, State#state.timeout}.


%% ----
%% Receive a TCP packet from the client
handle_info({tcp, Socket, FirstPacket},
            #state{wsstate=#ws_state{sock=Socket}}=State) ->
    handle_frames(FirstPacket, State);
handle_info({ssl, Socket, FirstPacket},
            #state{wsstate=#ws_state{sock={ssl, Socket}}}=State) ->
    handle_frames(FirstPacket, State);

%% Abnormal socket closure: only if no Close frame was received or sent.
handle_info({tcp_closed, Socket},
            #state{wsstate=#ws_state{sock=Socket}}=State) ->
    if
        State#state.close_frame_received == true orelse
        State#state.close_timer /= undefined ->
            {stop, normal, State};
        true ->
            handle_abnormal_closure(State)
    end;
handle_info({ssl_closed, Socket},
            #state{wsstate=#ws_state{sock={ssl, Socket}}}=State) ->
    if
        State#state.close_frame_received == true orelse
        State#state.close_timer /= undefined ->
            {stop, normal, State};
        true ->
            handle_abnormal_closure(State)
    end;


%% Ignore timeout if a Close frame was sent or received
handle_info(timeout, #state{close_frame_received=true}=State) ->
    {noreply, State};
handle_info(timeout, #state{close_timer=TRef}=State) when TRef /= undefined ->
    {noreply, State};

%% Keepalive timeout: send a ping frame and wait for any reply
handle_info(timeout, #state{wait_pong_frame=false}=State) ->
    GracePeriod = get_opts(keepalive_grace_period, State#state.opts),
    do_send(State#state.wsstate, {ping, <<>>}),
    {noreply, State#state{wait_pong_frame=true}, GracePeriod};

%% Grace period timeout
handle_info(timeout, #state{wait_pong_frame=true}=State) ->
    State1 = State#state{wait_pong_frame=false},
    case get_opts(drop_on_timeout, State1#state.opts) of
        true  -> handle_abnormal_closure(State1);
        false -> handle_callback_info(timeout, State1)
    end;

%% Close timeout: just shutdown the gen_server
handle_info(close, State) ->
    case State#state.close_timer of
        undefined -> ok;
        TRef      -> erlang:cancel_timer(TRef)
    end,
    {stop, normal, State};

%% All other messages
handle_info(Info, State) ->
    handle_callback_info(Info, State).


%% ----
terminate(_, #state{arg=Arg, cbinfo=CbInfo}=State) ->
    case CbInfo#cbinfo.terminate_fun of
        undefined ->
            ok;
        TerminateFun ->
            CbMod = CbInfo#cbinfo.module,
            {_, CbState} = State#state.cbstate,
            CbMod:TerminateFun(State#state.reason, CbState)
    end,
    CliSock = case State#state.wsstate of
                  undefined -> Arg#arg.clisock;
                  WSState   -> WSState#ws_state.sock
              end,
    if
        CliSock /= undefined ->
            case yaws_api:get_sslsocket(CliSock) of
                {ok, SslSocket} -> ssl:close(SslSocket);
                undefined       -> gen_tcp:close(CliSock)
            end;
        true ->
            ok
    end.


%% ----
code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.


%%%----------------------------------------------------------------------
%% internal functions
%%%----------------------------------------------------------------------
get_callback_info(Mod) ->
    case code:ensure_loaded(Mod) of
        {module, _} ->
            InitFun = case erlang:function_exported(Mod, init, 1) of
                          true  -> init;
                          false -> undefined
                      end,
            TerminateFun = case erlang:function_exported(Mod, terminate, 2) of
                               true  -> terminate;
                               false -> undefined
                           end,
            OpenFun = case erlang:function_exported(Mod, handle_open, 2) of
                          true  -> handle_open;
                          false -> undefined
                      end,
            MsgFun1 = case erlang:function_exported(Mod, handle_message, 1) of
                          true  -> handle_message;
                          false -> undefined
                      end,
            MsgFun2 = case erlang:function_exported(Mod, handle_message, 2) of
                          true  -> handle_message;
                          false -> undefined
                      end,
            InfoFun = case erlang:function_exported(Mod, handle_info, 2) of
                          true  -> handle_info;
                          false -> undefined
                      end,
            {ok, #cbinfo{module        = Mod,
                         init_fun      = InitFun,
                         terminate_fun = TerminateFun,
                         open_fun      = OpenFun,
                         msg_fun_1     = MsgFun1,
                         msg_fun_2     = MsgFun2,
                         info_fun      = InfoFun}};
        {error, Reason} ->
            error_logger:error_msg("Cannot load callback module '~p': ~p",
                                   [Mod, Reason]),
            {error, Reason}
    end.


preprocess_opts(GivenOpts) ->
    Fun = fun({Key, Default}, Opts) ->
                  case lists:keyfind(Key, 1, Opts) of
                      false -> [{Key, Default}|Opts];
                      _     -> Opts
                  end
          end,
    Defaults = [
                {origin,                  any},
                {callback,                basic},
                {max_frame_size,          16 * 1024 * 1024}, %% 16 MB
                {max_message_size,        16 * 1024 * 1024}, %% 16 MB
                {close_if_unmasked,       false},
                {auto_fragment_message,   false},
                {auto_fragment_threshold, 1024 * 1024},      %%  1 MB
                {close_timeout,           5000},             %%  5 secs
                {keepalive,               false},
                {keepalive_timeout,       30000},            %% 30 secs
                {keepalive_grace_period,  2000},             %%  2 secs
                {drop_on_timeout,         false}
               ],
    lists:foldl(Fun, GivenOpts, Defaults).


get_opts(Key, Opts) ->
    {Key, Value} = lists:keyfind(Key, 1, Opts),
    Value.

check_origin(_Origin, any)       -> ok;
check_origin(Actual,  Actual )   -> ok;
check_origin(_Actual, _Expected) -> error.

check_connection(undefined) ->
    error;
check_connection(Connection) ->
    Vals = yaws:split_sep(string:to_lower(Connection), $,),
    case lists:member("upgrade", Vals) of
        true  -> ok;
        false -> error
    end.

check_upgrade(undefined) ->
    error;
check_upgrade(Upgrade) ->
    Vals = yaws:split_sep(string:to_lower(Upgrade), $,),
    case lists:member("websocket", Vals) of
        true  -> ok;
        false -> error
    end.



handshake(CliSock, Key, _Protocol) ->
    AcceptHash = hash_nonce(Key),
    Handshake  = ["HTTP/1.1 101 Switching Protocols\r\n",
                  "Upgrade: websocket\r\n",
                  "Connection: Upgrade\r\n",
                  "Sec-WebSocket-Accept: ", AcceptHash , "\r\n",
                  "\r\n"],
    case yaws_api:get_sslsocket(CliSock) of
        {ok, SslSocket} -> ssl:send(SslSocket, Handshake);
        undefined       -> gen_tcp:send(CliSock, Handshake)
    end.


do_send(#ws_state{sock=undefined}, _) ->
    ok;
do_send(WSState, Messages) when is_list(Messages) ->
    [do_send(WSState, Msg) || Msg <- Messages],
    ok;
do_send(WSState, {Type, Data}) ->
    do_send(WSState, #ws_frame{opcode=Type, payload=Data});
do_send(#ws_state{sock=Socket}, #ws_frame{}=Frame) ->
    case yaws_api:get_sslsocket(Socket) of
        {ok, SslSocket} -> [ssl:send(SslSocket, F)  || F <- frames(Frame)];
        undefined       -> [gen_tcp:send(Socket, F) || F <- frames(Frame)]
    end.


%% Add this clause to be compatible with previous versions.
do_close(WSState, normal) ->
    do_close(WSState, ?WS_STATUS_NORMAL);
%% Special case for status=1006, do not send this close frame
do_close(_WSState, ?WS_STATUS_ABNORMAL_CLOSURE) ->
    ok;
do_close(_WSState, {?WS_STATUS_ABNORMAL_CLOSURE, _}) ->
    ok;
do_close(WSState, Status) when is_integer(Status) ->
    do_send(WSState, {close, <<Status:16/big>>});
do_close(WSState, {Status,Reason}) when is_integer(Status), is_binary(Reason) ->
    do_send(WSState, {close, <<Status:16/big, Reason/binary>>});
do_close(WSState, _) ->
    Status = ?WS_STATUS_INTERNAL_ERROR,
    do_send(WSState, {close, <<Status:16/big>>}).


deliver_xxx(CliSock, Code) ->
    deliver_xxx(CliSock, Code, []).

deliver_xxx(CliSock, Code, Hdrs) ->
    Reply = ["HTTP/1.1 ",integer_to_list(Code), $\s,
             yaws_api:code_to_phrase(Code), "\r\n",
             "Connection: close\r\n",
             lists:flatmap(fun(X) -> X ++ "\r\n" end, Hdrs),
             "\r\n"],
    case yaws_api:get_sslsocket(CliSock) of
        {ok, SslSocket} -> ssl:send(SslSocket, Reply);
        undefined       -> gen_tcp:send(CliSock, Reply)
    end.


%% ----
handle_frames(FirstPacket, #state{wsstate=WSState, opts=Opts}=State) ->
    FrameInfos = unframe_active_once(WSState, FirstPacket, Opts),
    Result = case State#state.cbtype of
                 basic    -> basic_messages(FrameInfos, State);
                 advanced -> advanced_messages(FrameInfos, State)
             end,
    case Result of
        {ok, State1, Timeout} ->
            Last = lists:last(FrameInfos),
            {noreply, State1#state{wsstate=Last#ws_frame_info.ws_state,
                                  wait_pong_frame=false},
             Timeout};
        {stop, State1} ->
            case State1#state.close_frame_received of
                true  ->
                    {stop, normal, State1};
                false ->
                    Tout = get_opts(close_timeout, State1#state.opts),
                    TRef = erlang:send_after(Tout, self(), close),
                    {noreply, State1#state{close_timer=TRef}}
            end
    end.


handle_callback_info(Info, #state{cbinfo=CbInfo}=State) ->
    case CbInfo#cbinfo.info_fun of
        undefined ->
            {noreply, State, State#state.timeout};
        InfoFun ->
            {_, CbState} = State#state.cbstate,
            CbMod = CbInfo#cbinfo.module,
            Res   = CbMod:InfoFun(Info, CbState),
            case handle_callback_result(Res, State) of
                {ok, State1, Timeout} -> {noreply, State1, Timeout};
                {stop, State1}        -> {stop, normal, State1}
            end
    end.

handle_abnormal_closure(#state{wsstate=WSState}=State) ->
    %% The only way we should get here is due to an abnormal close. Section
    %% 7.1.5 of RFC 6455 specifies 1006 as the connection close code for
    %% abnormal closure. It's also described in section 7.4.1.
    CloseStatus    = ?WS_STATUS_ABNORMAL_CLOSURE,
    ClosePayload   = <<CloseStatus:16/big>>,
    CloseWSState   = WSState#ws_state{sock=undefined,frag_type=none},
    CloseFrameInfo = #ws_frame_info{fin         = 1,
                                    rsv         = 0,
                                    opcode      = close,
                                    masked      = 0,
                                    masking_key = <<>>,
                                    length      = 2,
                                    payload     = ClosePayload,
                                    data        = ClosePayload,
                                    ws_state    = CloseWSState},
    Result = case State#state.cbtype of
                 basic ->
                     basic_messages([CloseFrameInfo],
                                    State#state{wsstate=CloseWSState});
                 advanced ->
                     advanced_messages([CloseFrameInfo],
                                       State#state{wsstate=CloseWSState})
             end,
    case Result of
        {ok, State1, _} -> {stop, normal, State1};
        {stop, State1}  -> {stop, normal, State1}
    end.


%% ----
basic_messages(FrameInfos, State) ->
    basic_messages(FrameInfos, State, State#state.timeout).

basic_messages([], State, Tout) ->
    {ok, State, Tout};
basic_messages([FrameInfo|Rest],
               #state{cbinfo=CbInfo, cbstate={FrameState,CbState}}=State,
               Tout) ->
    case handle_message(FrameInfo, FrameState, State#state.opts) of
        {continue, FrameState1} ->
            basic_messages(Rest, State#state{cbstate={FrameState1,CbState}},
                           Tout);

        {message, Message} when State#state.close_timer /= undefined ->
            case Message of
                {close, _, _} ->
                    %% The server is waiting for the Close frame. We can stop it
                    %% now.
                    erlang:cancel_timer(State#state.close_timer),
                    {stop, State#state{close_frame_received=true}};
                _ ->
                    %% The server is waiting for the Close frame, all other
                    %% messages are ignored.
                    basic_messages(Rest, State, Tout)
            end;

        {message, {pong, Data}} ->
            do_send(State#state.wsstate, {pong, Data}),
            basic_messages(Rest, State, Tout);

        {message, Message} ->
            CbMod = CbInfo#cbinfo.module,
            Res = case CbInfo#cbinfo.msg_fun_2 of
                      undefined ->
                          MsgFun1 = CbInfo#cbinfo.msg_fun_1,
                          catch CbMod:MsgFun1(Message);
                      MsgFun2 ->
                          catch CbMod:MsgFun2(Message, CbState)
                  end,
            IsCloseFrame = case Message of
                               {close, _, _} -> true;
                               _             -> false
                           end,
            State1 = State#state{cbstate={{none, <<>>}, CbState},
                                 close_frame_received=IsCloseFrame},
            case handle_callback_result(Res, State1) of
                {ok, State2, _} when IsCloseFrame == true ->
                    %% The callback module must return a Close frame. If not, we
                    %% force the server to close the connection
                    {stop, State2};
                {ok, State2, Tout1} ->
                    basic_messages(Rest, State2, Tout1);
                {stop, State2} ->
                    {stop, State2}
            end;

        {error, Reason} ->
            do_close(State#state.wsstate, Reason),
            {stop, State#state{close_frame_received=true, reason=Reason}}
    end.


%% unfragmented text message
handle_message(#ws_frame_info{fin=1, opcode=text, data=Data}, {none, <<>>},
               _Opts) ->
    case unicode:characters_to_binary(Data, utf8, utf8) of
        Data -> {message, {text, Data}};
        _    -> {error, {?WS_STATUS_INVALID_PAYLOAD, <<"invalid utf-8">>}}
    end;

%% start of a fragmented text message
handle_message(#ws_frame_info{fin=0, opcode=text, data=Data}, {none, <<>>},
               _Opts) ->
    case unicode:characters_to_binary(Data, utf8, utf8) of
        Data ->
            {continue, {text, [Data], <<>>}};
        {incomplete,Dec,Rest} ->
            {continue, {text, [Dec],  Rest}};
        _ ->
            {error, {?WS_STATUS_INVALID_PAYLOAD, <<"invalid utf-8">>}}
    end;

%% non-final continuation of a fragmented text message
handle_message(#ws_frame_info{fin=0, data=Data, opcode=continuation},
               {text, Dec0, Rest0}, Opts) ->
    MaxLen = get_opts(max_message_size, Opts),
    Len    = iolist_size(Dec0) + byte_size(Rest0) + byte_size(Data),
    if
        Len > MaxLen ->
            {error, {?WS_STATUS_MSG_TOO_BIG, <<"Message too big">>}};
        true ->
            Data1 = <<Rest0/binary, Data/binary>>,
            case unicode:characters_to_binary(Data1, utf8, utf8) of
                Data1 ->
                    {continue, {text, [Data1|Dec0], <<>>}};
                {incomplete, Dec1, Rest1} ->
                    {continue, {text, [Dec1|Dec0],  Rest1}};
                _ ->
                    {error, {?WS_STATUS_INVALID_PAYLOAD, <<"invalid utf-8">>}}
            end
    end;

%% end of text fragmented message
handle_message(#ws_frame_info{fin=1, opcode=continuation, data=Data},
               {text, Dec, Rest}, Opts) ->
    MaxLen = get_opts(max_message_size, Opts),
    Len    = iolist_size(Dec) + byte_size(Rest) + byte_size(Data),
    if
        Len > MaxLen ->
            {error, {?WS_STATUS_MSG_TOO_BIG, <<"Message too big">>}};
        true ->
            Data1 = <<Rest/binary, Data/binary>>,
            case unicode:characters_to_binary(Data1, utf8, utf8) of
                Data1 ->
                    Msg = list_to_binary(lists:reverse([Data1|Dec])),
                    {message, {text, Msg}};
                _ ->
                    {error, {?WS_STATUS_INVALID_PAYLOAD, <<"invalid utf-8">>}}
            end
    end;

%% unfragmented binary message
handle_message(#ws_frame_info{fin=1, opcode=binary, data=Data}, {none, <<>>},
               _Opts) ->
    {message, {binary, Data}};

%% start of a fragmented binary message
handle_message(#ws_frame_info{fin=0, opcode=binary, data=Data}, {none, <<>>},
               _Opts) ->
    {continue, {binary, Data}};

%% non-final continuation of a fragmented binary message
handle_message(#ws_frame_info{fin=0, data=Data, opcode=continuation},
               {binary, FragAcc}, Opts) ->
    MaxLen = get_opts(max_message_size, Opts),
    Len    = byte_size(FragAcc) + byte_size(Data),
    if
        Len > MaxLen ->
            {error, {?WS_STATUS_MSG_TOO_BIG, <<"Message too big">>}};
        true ->
            {continue, {binary, <<FragAcc/binary,Data/binary>>}}
    end;

%% end of binary fragmented message
handle_message(#ws_frame_info{fin=1, opcode=continuation, data=Data},
               {binary, FragAcc}, Opts) ->
    MaxLen = get_opts(max_message_size, Opts),
    Len    = byte_size(FragAcc) + byte_size(Data),
    if
        Len > MaxLen ->
            {error, {?WS_STATUS_MSG_TOO_BIG, <<"Message too big">>}};
        true ->
            Unfragged = <<FragAcc/binary, Data/binary>>,
            {message, {binary, Unfragged}}
    end;

handle_message(#ws_frame_info{opcode=ping, data=Data}, _Acc, _Opts) ->
    {message, {pong, Data}};

handle_message(#ws_frame_info{opcode=pong}, Acc, _Opts) ->
    %% A response to an unsolicited pong frame is not expected.
    %% http://tools.ietf.org/html/rfc6455#section-5.5.3
    {continue, Acc};

%% According to RFC 6455 section 5.4, control messages like close MAY be
%% injected in the middle of a fragmented message, which is why we pass FragType
%% and FragAcc along below. Whether any clients actually do this in practice, I
%% don't know.
handle_message(#ws_frame_info{opcode=close, length=Len,
                              data=Data, ws_state=WSState},
               _Acc, _Opts) ->
    case Len of
        0 ->
            {message, {close, ?WS_STATUS_NORMAL, <<>>}};
        1 ->
            {error, {?WS_STATUS_PROTO_ERROR, <<"protocol error">>}};
        _ ->
            <<Status:16/big, Msg/binary>> = Data,
            case unicode:characters_to_binary(Msg, utf8, utf8) of
                Msg ->
                    {message, {close, check_close_code(Status, WSState), Msg}};
                _ ->
                    {error, {?WS_STATUS_INVALID_PAYLOAD, <<"invalid utf-8">>}}
            end
    end;

handle_message(#ws_frame_info{}, _Acc, _Opts) ->
    {error, {?WS_STATUS_PROTO_ERROR, <<"protocol error">>}};

handle_message({fail_connection, Status, Msg}, _State, _Tout) ->
    {error, {Status, Msg}}.



advanced_messages(FrameInfos, State) ->
    advanced_messages(FrameInfos, State, State#state.timeout).

advanced_messages([], State, Tout) ->
    {ok, State, Tout};
advanced_messages([FrameInfo|Rest], #state{close_timer=TRef}=State,
                  Tout) when TRef /= undefined ->
    case FrameInfo of
        #ws_frame_info{opcode=close} ->
            %% The server is waiting for the Close frame. We can stop it now.
            erlang:cancel_timer(TRef),
            {stop, State#state{close_frame_received=true}};
        _ ->
            %% The server is waiting for the Close frame, all other messages are
            %% ignored.
            advanced_messages(Rest, State, Tout)
    end;
advanced_messages([FrameInfo|Rest],
                  #state{cbinfo=CbInfo, cbstate={undefined,CbState}}=State,
                  _Tout) ->
    CbMod   = CbInfo#cbinfo.module,
    MsgFun2 = CbInfo#cbinfo.msg_fun_2,
    Res     = (catch CbMod:MsgFun2(FrameInfo, CbState)),
    IsCloseFrame = case FrameInfo of
                       #ws_frame_info{opcode=close} -> true;
                       _                            -> false
                   end,
    State1 = State#state{close_frame_received=IsCloseFrame},
    case handle_callback_result(Res, State1) of
        {ok, State2, _} when IsCloseFrame == true ->
            %% The callback module must return a Close frame. If not, we force
            %% the server to close the connection
            {stop, State2};
        {ok, State2, Tout1} ->
            advanced_messages(Rest, State2, Tout1);
        {stop, State2} ->
            {stop, State2}
    end.


%% The checks for close status codes here are based on RFC 6455 and on the
%% autobahn testsuite (http://autobahn.ws/testsuite).
check_close_code(Code, WSState) ->
    if
        Code >= 3000 andalso Code =< 4999 ->
            Code;
        Code < 1000 ->
            ?WS_STATUS_PROTO_ERROR;
        Code == 1006 andalso WSState#ws_state.sock == undefined ->
            Code;
        Code >= 1004 andalso Code =< 1006 ->
            ?WS_STATUS_PROTO_ERROR;
        Code > 1011 ->
            ?WS_STATUS_PROTO_ERROR;
        true ->
            Code
    end.


handle_callback_result({reply, Messages}, State) ->
    do_send(State#state.wsstate, Messages),
    {ok, State, State#state.timeout};
handle_callback_result({reply, Messages, CbState1}, State) ->
    do_send(State#state.wsstate, Messages),
    {FrameState, _} = State#state.cbstate,
    {ok, State#state{cbstate={FrameState, CbState1}}, State#state.timeout};
handle_callback_result({reply, Messages, CbState1, Timeout}, State) ->
    do_send(State#state.wsstate, Messages),
    {FrameState, _} = State#state.cbstate,
    {ok, State#state{cbstate={FrameState, CbState1}}, Timeout};

handle_callback_result(noreply, State) ->
    {ok, State, State#state.timeout};
handle_callback_result({noreply, CbState1}, State) ->
    {FrameState, _} = State#state.cbstate,
    {ok, State#state{cbstate={FrameState, CbState1}}, State#state.timeout};
handle_callback_result({noreply, CbState1, Timeout}, State) ->
    {FrameState, _} = State#state.cbstate,
    {ok, State#state{cbstate={FrameState, CbState1}}, Timeout};

handle_callback_result({close, Reason}, State) ->
    do_close(State#state.wsstate, Reason),
    case is_fatal_error(Reason) of
        true  -> {stop, State#state{close_frame_received=true, reason=Reason}};
        false -> {stop, State#state{reason=Reason}}
    end;
handle_callback_result({close, Reason, CbState1}, State) ->
    do_close(State#state.wsstate, Reason),
    {FrameState, _} = State#state.cbstate,
    State1 = State#state{cbstate={FrameState, CbState1}},
    case is_fatal_error(Reason) of
        true  -> {stop, State1#state{close_frame_received=true, reason=Reason}};
        false -> {stop, State1#state{reason=Reason}}
    end;
handle_callback_result({close, Reason, Messages, CbState1}, State) ->
    do_send(State#state.wsstate, Messages),
    do_close(State#state.wsstate, Reason),
    {FrameState, _} = State#state.cbstate,
    State1 = State#state{cbstate={FrameState, CbState1}},
    case is_fatal_error(Reason) of
        true  -> {stop, State1#state{close_frame_received=true, reason=Reason}};
        false -> {stop, State1#state{reason=Reason}}
    end;

handle_callback_result({'EXIT', Reason}, State) ->
    do_close(State#state.wsstate, ?WS_STATUS_INTERNAL_ERROR),
    {stop, State#state{close_frame_received=true, reason={error,Reason}}}.


is_fatal_error(?WS_STATUS_PROTO_ERROR)           -> true;
is_fatal_error({?WS_STATUS_PROTO_ERROR, _})      -> true;
is_fatal_error(?WS_STATUS_ABNORMAL_CLOSURE)      -> true;
is_fatal_error({?WS_STATUS_ABNORMAL_CLOSURE, _}) -> true;
is_fatal_error(?WS_STATUS_INVALID_PAYLOAD)       -> true;
is_fatal_error({?WS_STATUS_INVALID_PAYLOAD, _})  -> true;
is_fatal_error(?WS_STATUS_MSG_TOO_BIG)           -> true;
is_fatal_error({?WS_STATUS_MSG_TOO_BIG, _})      -> true;
is_fatal_error(?WS_STATUS_INTERNAL_ERROR)        -> true;
is_fatal_error({?WS_STATUS_INTERNAL_ERROR, _})   -> true;
is_fatal_error(_)                                -> false.


%% ----
ws_version(Headers) ->
    VersionVal = query_header("sec-websocket-version", Headers),
    case VersionVal of
        "8"  -> 8;
        "13" -> 13;
        V    -> {unsupported_version, V}
    end.

buffer(Socket, Len, Buffered) ->
    case Buffered of
        <<_Expected:Len/binary>> -> % exactly enough
            Buffered;
        <<_Expected:Len/binary,_Extra/binary>> -> % more than expected
            Buffered;
        _ ->
            %% not enough
            More = do_recv(Socket, Len - byte_size(Buffered), []),
            <<Buffered/binary, More/binary>>
    end.

do_recv(_, 0, Acc) ->
    list_to_binary(lists:reverse(Acc));
do_recv(Socket, Sz, Acc) ->
    %% TODO: add configurable timeout to receive data
    Res = case yaws_api:get_sslsocket(Socket) of
              {ok, SslSocket} -> ssl:recv(SslSocket, Sz,  5000);
              undefined       -> gen_tcp:recv(Socket, Sz, 5000)
          end,
    case Res of
        {ok, Bin}       -> do_recv(Socket, Sz - byte_size(Bin), [Bin|Acc]);
        {error, Reason} -> throw({tcp_error, Reason})
    end.


checks(Unframed) ->
    check_reserved_bits(Unframed).

check_control_frame(Len, Opcode, Fin) ->
    if
        (Len > 125) and (Opcode > 7) ->
            %% http://tools.ietf.org/html/rfc6455#section-5.5
            {fail_connection, ?WS_STATUS_PROTO_ERROR,
             <<"control frame > 125 bytes">>};
        (Fin == 0) and (Opcode > 7) ->
            {fail_connection, ?WS_STATUS_PROTO_ERROR,
             <<"control frame may not be fragmented">>};
        true ->
            ok
    end.

%% no extensions are supported yet
%% http://tools.ietf.org/html/rfc6455#section-5.2
check_reserved_bits(Unframed = #ws_frame_info{rsv=0}) ->
    check_reserved_opcode(Unframed);
check_reserved_bits(#ws_frame_info{rsv=RSV}) ->
    {fail_connection, ?WS_STATUS_PROTO_ERROR, <<"rsv bits were ", (RSV+48),
                              " but should be unset">>}.


check_reserved_opcode(#ws_frame_info{opcode = undefined}) ->
    {fail_connection, ?WS_STATUS_PROTO_ERROR, <<"Reserved opcode">>};
check_reserved_opcode(_) ->
    ok.


ws_frame_info(#ws_state{sock=Socket},
              <<Fin:1, Rsv:3, Opcode:4, Masked:1, Len1:7, Rest/binary>>,
              Opts) ->
    case check_control_frame(Len1, Opcode, Fin) of
        ok ->
            case ws_frame_info_secondary(Socket, Masked, Len1, Rest, Opts) of
                {ws_frame_info_secondary,Length,MaskingKey,Payload,Excess} ->
                    FrameInfo = #ws_frame_info{fin=Fin,
                                               rsv=Rsv,
                                               opcode=opcode_to_atom(Opcode),
                                               masked=Masked,
                                               masking_key=MaskingKey,
                                               length=Length,
                                               payload=Payload},
                    {FrameInfo, Excess};
                Else ->
                    Else
            end;
        Other ->
            Other
    end;

ws_frame_info(WSState = #ws_state{sock=Socket}, FirstPacket, Opts) ->
    ws_frame_info(WSState, buffer(Socket, 2, FirstPacket), Opts).


ws_frame_info_secondary(Socket, Masked, Len1, Rest, Opts) ->
    MaxLen = get_opts(max_frame_size, Opts),
    CloseIfUnmasked = get_opts(close_if_unmasked, Opts),
    MaskLength = case Masked of
                     0 -> 0;
                     1 -> 4
                 end,
    case Len1 of
        126 ->
            <<Len:16, MaskingKey:MaskLength/binary, Rest2/binary>> =
                buffer(Socket, MaskLength + 2 , Rest);
        127 ->
            <<Len:64, MaskingKey:MaskLength/binary, Rest2/binary>> =
                buffer(Socket, MaskLength + 8 , Rest);
        Len ->
            <<MaskingKey:MaskLength/binary, Rest2/binary>> =
                buffer(Socket, MaskLength, Rest)
    end,
    if
        CloseIfUnmasked == true andalso MaskingKey == <<>> ->
            error_logger:error_msg("Unmasked frame forbidden", []),
            {fail_connection, ?WS_STATUS_PROTO_ERROR,
             <<"Unmasked frame forbidden">>};
        Len > MaxLen ->
            error_logger:error_msg(
              "Payload length ~p longer than max allowed of ~p",
              [Len, MaxLen]
             ),
            {fail_connection, ?WS_STATUS_MSG_TOO_BIG, <<"Frame too big">>};
        true ->
            <<Payload:Len/binary, Excess/binary>> = buffer(Socket, Len, Rest2),
            {ws_frame_info_secondary, Len, MaskingKey, Payload, Excess}
    end.

unframe_active_once(WSState, FirstPacket, Opts) ->
    Frames = unframe(WSState, FirstPacket, Opts),
    websocket_setopts(WSState, [{active, once}]),
    Frames.


%% Returns all the WebSocket frames fully or partially contained in FirstPacket,
%% reading exactly as many more bytes from Socket as are needed to finish
%% unframing the last frame partially included in FirstPacket, if needed.
%%
%% The length of this list and depth of this recursion is limited by the size of
%% your socket receive buffer.
%%
%% -> { #ws_state, [#ws_frame_info,...,#ws_frame_info] }
unframe(_WSState, <<>>, _Opts) ->
    [];
unframe(WSState, FirstPacket, Opts) ->
    case unframe_one(WSState, FirstPacket, Opts) of
        {FrameInfo = #ws_frame_info{ws_state = NewWSState}, RestBin} ->
            %% Every new recursion uses the #ws_state from the calling
            %% recursion.
            [FrameInfo | unframe(NewWSState, RestBin, Opts)];
        Fail ->
            [Fail]
    end.

%% -> {#ws_frame_info, RestBin} | {fail_connection, Reason}
unframe_one(WSState, FirstPacket, Opts) ->
    case catch ws_frame_info(WSState, FirstPacket, Opts) of
        {FrameInfo = #ws_frame_info{}, RestBin} ->
            Unmasked = mask(FrameInfo#ws_frame_info.masking_key,
                            FrameInfo#ws_frame_info.payload),
            case frag_state_machine(WSState, FrameInfo) of
                #ws_state{}=NewWSState ->
                    Unframed = FrameInfo#ws_frame_info{data     = Unmasked,
                                                       ws_state = NewWSState},
                    case checks(Unframed) of
                        ok   -> {Unframed, RestBin};
                        Else -> Else
                    end;
                Else ->
                    Else
            end;
        {tcp_error, Reason} ->
            %% FIXME: close the connection ?
            error_logger:error_msg("Abnormal Closure: ~p", [Reason]),
            CloseStatus    = ?WS_STATUS_ABNORMAL_CLOSURE,
            ClosePayload   = <<CloseStatus:16/big>>,
            CloseWSState   = WSState#ws_state{sock=undefined,frag_type=none},
            {#ws_frame_info{fin         = 1,
                            rsv         = 0,
                            opcode      = close,
                            masked      = 0,
                            masking_key = 0,
                            length      = 2,
                            payload     = ClosePayload,
                            data        = ClosePayload,
                            ws_state    = CloseWSState}, <<>>};
        Else ->
            Else
    end.

websocket_setopts(#ws_state{sock=Socket}, Opts) ->
    case yaws_api:get_sslsocket(Socket) of
        {ok, SslSocket} -> ssl:setopts(SslSocket, Opts);
        undefined       -> inet:setopts(Socket, Opts)
    end.

is_control_op(Op) ->
    atom_to_opcode(Op) > 7.

%% Unfragmented message
frag_state_machine(#ws_state{frag_type=none} = State,
                   #ws_frame_info{fin=1}) ->
    State;

%% Beginning of fragmented text message
frag_state_machine(#ws_state{frag_type=none} = State,
                   #ws_frame_info{fin=0, opcode=text}) ->
    State#ws_state{frag_type = text};

%% Beginning of fragmented binary message
frag_state_machine(#ws_state{frag_type=none} = State,
                   #ws_frame_info{fin=0, opcode=binary}) ->
    State#ws_state{frag_type = binary};

%% Expecting text continuation
frag_state_machine(#ws_state{frag_type=text} = State,
                   #ws_frame_info{fin=0, opcode=continuation}) ->
    State;

%% Expecting binary continuation
frag_state_machine(#ws_state{frag_type=binary}=State,
                   #ws_frame_info{fin=0, opcode=continuation}) ->
    State;

%% End of fragmented text message
frag_state_machine(#ws_state{frag_type=text} = State,
                   #ws_frame_info{fin=1, opcode=continuation}) ->
    State#ws_state{frag_type = none};

%% End of fragmented binary message
frag_state_machine(#ws_state{frag_type=binary} = State,
                   #ws_frame_info{fin=1, opcode=continuation}) ->
    State#ws_state{frag_type = none};


frag_state_machine(State, #ws_frame_info{opcode = Op}) ->
    IsControl = is_control_op(Op),
    if
        IsControl == true ->
            %% Control message never changes fragmentation state
            State;
        true ->
            %% Everything else is wrong
            {fail_connection, ?WS_STATUS_PROTO_ERROR,
             <<"fragmentation rules violated">>}
    end.


opcode_to_atom(16#0) -> continuation;
opcode_to_atom(16#1) -> text;
opcode_to_atom(16#2) -> binary;
opcode_to_atom(16#8) -> close;
opcode_to_atom(16#9) -> ping;
opcode_to_atom(16#A) -> pong;
opcode_to_atom(_)    -> undefined.

atom_to_opcode(continuation) -> 16#0;
atom_to_opcode(text)         -> 16#1;
atom_to_opcode(binary)       -> 16#2;
atom_to_opcode(close)        -> 16#8;
atom_to_opcode(ping)         -> 16#9;
atom_to_opcode(pong)         -> 16#A.


frames(#ws_frame{fin=true, opcode=C, payload=Payload}=Frame) when C == text;
                                                                  C == binary ->
    %% try auto-fragmentation only on unfragmented data frames
    case get(auto_fragment) of
        {true, Sz} when byte_size(Payload) > Sz ->
            <<Chunk:Sz/binary, Rest/binary>> = Payload,
            [frame(#ws_frame{fin=false, opcode=C, payload=Chunk}) |
             fragment_frame(
               #ws_frame{fin=false, opcode=continuation, payload=Rest}, Sz
              )];
        _ ->
            [frame(Frame)]
    end;
frames(Frame) ->
    [frame(Frame)].



frame(#ws_frame{}=Frame) ->
    Fin     = case Frame#ws_frame.fin of
                  true  -> 1;
                  false -> 0
              end,
    Rsv     = Frame#ws_frame.rsv,
    OpCode  = atom_to_opcode(Frame#ws_frame.opcode),
    Payload = Frame#ws_frame.payload,
    Length  = byte_size(Payload),
    if
        Length < 126 ->
            <<Fin:1, Rsv:3, OpCode:4, 0:1, Length:7, Payload/binary>>;
        Length < 65536 ->
            <<Fin:1, Rsv:3, OpCode:4, 0:1, 126:7, Length:16, Payload/binary>>;
        true ->
            <<Fin:1, Rsv:3, OpCode:4, 0:1, 127:7, Length:64, Payload/binary>>
    end.


fragment_frame(#ws_frame{payload=Payload}=Frame, Sz) ->
    case Payload of
        <<_:Sz/binary>> ->
            [frame(Frame#ws_frame{fin=true})];
        <<Chunk:Sz/binary, Rest/binary>> ->
            [frame(Frame#ws_frame{payload=Chunk})|
             fragment_frame(Frame#ws_frame{payload=Rest}, Sz)];
        _ ->
            [frame(Frame#ws_frame{fin=true})]
    end.


mask(MaskBin, Data) ->
    list_to_binary(rmask(MaskBin, Data)).

%% unmask == mask. It's XOR of the four-byte masking key.
rmask(_,<<>>) ->
    [<<>>];
rmask(<<>>, Data) ->
    [Data];
rmask(MaskBin = <<Mask:4/integer-unit:8>>,
      <<Data:4/integer-unit:8, Rest/binary>>) ->
    Masked = Mask bxor Data,
    MaskedRest = rmask(MaskBin, Rest),
    [<<Masked:4/integer-unit:8>> | MaskedRest ];
rmask(<<Mask:3/integer-unit:8, _Rest/binary>>, <<Data:3/integer-unit:8>>) ->
    Masked = Mask bxor Data,
    [<<Masked:3/integer-unit:8>>];
rmask(<<Mask:2/integer-unit:8, _Rest/binary>>, <<Data:2/integer-unit:8>>) ->
    Masked = Mask bxor Data,
    [<<Masked:2/integer-unit:8>>];
rmask(<<Mask:1/integer-unit:8, _Rest/binary>>, <<Data:1/integer-unit:8>>) ->
    Masked = Mask bxor Data,
    [<<Masked:1/integer-unit:8>>].


%% Internal functions
get_origin_header(Headers) ->
    case query_header("origin", Headers) of
        undefined -> query_header("sec-websocket-origin", Headers);
        Origin    -> Origin
    end.

get_protocol_header(Headers) ->
    query_header("sec-websocket-protocol", Headers, "unknown").

get_nonce_header(Headers) ->
    query_header("sec-websocket-key", Headers).

query_header(HeaderName, Headers) ->
    query_header(HeaderName, Headers, undefined).

query_header(Header, Headers, Default) ->
    yaws_api:get_header(Headers, Header, Default).

hash_nonce(Nonce) ->
    Salted = Nonce ++ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11",
    HashBin = crypto:hash(sha, Salted),
    base64:encode_to_string(HashBin).
