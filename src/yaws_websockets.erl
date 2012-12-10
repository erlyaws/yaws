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

-define(MAX_PAYLOAD, 16*1024*1024). % 16MB

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
                reason=normal}).

-record(cbinfo, {module,
                 init_fun,
                 terminate_fun,
                 open_fun,
                 msg_fun_1,
                 msg_fun_2,
		 info_fun}).

-export([start/3, send/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%----------------------------------------------------------------------
%% API
%%%----------------------------------------------------------------------
start(Arg, CallbackMod, Opts) ->
    PrepdOpts = preprocess_opts(Opts),

    %% Check origin header
    {origin, OriginOpt} = lists:keyfind(origin, 1, Opts),
    Origin = get_origin_header(Arg#arg.headers),
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
send(Pid, {Type, Data}) ->
    gen_server:cast(Pid, {send, {Type, Data}}).


%%%----------------------------------------------------------------------
%% gen_server functions
%%%----------------------------------------------------------------------
init([Arg, CbMod, Opts]) ->
    {CbType, FrameState, InitState} =
        case lists:keyfind(callback, 1, Opts) of
            {basic,    St} -> {basic, {none, <<>>}, St};
            {advanced, St} -> {advanced, undefined, St};
            _              -> {basic, {none, <<>>}, []}
        end,
    case get_callback_info(CbMod) of
        {ok, #cbinfo{init_fun=undefined}=CbInfo} ->
            {ok, #state{arg     = Arg,
                        opts    = Opts,
                        cbinfo  = CbInfo,
                        cbtype  = CbType,
                        cbstate = {FrameState,InitState}}};
        {ok, #cbinfo{init_fun=InitFun}=CbInfo} ->
            case CbMod:InitFun([Arg, InitState]) of
                {ok, InitState1} ->
                    {ok, #state{arg     = Arg,
                                opts    = Opts,
                                cbinfo  = CbInfo,
                                cbtype  = CbType,
                                cbstate = {FrameState,InitState1}}};
                {ok, InitState1, Timeout} ->
                    {ok, #state{arg     = Arg,
                                opts    = Opts,
                                cbinfo  = CbInfo,
                                cbtype  = CbType,
                                cbstate = {FrameState,InitState1},
                                timeout = Timeout}};
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

    ProtocolVersion = ws_version(Arg#arg.headers),
    Protocol        = get_protocol_header(Arg#arg.headers),
    handshake(ProtocolVersion, Arg, CliSock, Protocol),

    WSState  = #ws_state{sock      = CliSock,
                         vsn       = ProtocolVersion,
                         frag_type = none},

    case CbInfo#cbinfo.open_fun of
        undefined ->
            {noreply, State#state{wsstate=WSState}};
        OpenFun ->
            CbMod = CbInfo#cbinfo.module,
            {FrameState, CbState} = State#state.cbstate,
            case CbMod:OpenFun(WSState, CbState) of
                {ok, CbState1} ->
                    {noreply,
                     State#state{wsstate=WSState, cbstate={FrameState,CbState1}},
                     State#state.timeout};
                {error, Reason} ->
                    do_close(WSState, Reason),
                    {stop, normal, State#state{reason={error, Reason}}}
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

%% Skip all other async messages
handle_cast(_Msg, State) ->
    {noreply, State, State#state.timeout}.


%% ----
%% Receive a TCP packet from the client
handle_info({tcp, Socket, FirstPacket},
            #state{wsstate=#ws_state{sock=Socket}=WSState}=State) ->
    FrameInfos = unframe_active_once(WSState, FirstPacket),
    Result = case State#state.cbtype of
                 basic    -> catch basic_messages(FrameInfos, State);
                 advanced -> catch advanced_messages(FrameInfos, State)
             end,
    case Result of
        {ok, State1} ->
            Last = lists:last(FrameInfos),
            {noreply, State1#state{wsstate=Last#ws_frame_info.ws_state},
             State1#state.timeout};
        {close, Reason} ->
            do_close(WSState, Reason),
            {stop, normal, State};
        {error, Reason} ->
            do_close(WSState, Reason),
            {stop, normal, State#state{reason={error,Reason}}};
        {'EXIT', Reason} ->
            do_close(WSState, ?WS_STATUS_INTERNAL_ERROR),
            {stop, normal, State#state{reason={error,Reason}}}
    end;

%% Abnormal socket closure.
handle_info({tcp_closed, Socket},
            #state{wsstate=#ws_state{sock=Socket}=WSState}=State) ->
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
                                    masking_key = 0,
                                    length      = 2,
                                    payload     = ClosePayload,
                                    ws_state    = CloseWSState},
    Result = case State#state.cbtype of
                 basic    -> catch basic_messages([CloseFrameInfo], State);
                 advanced -> catch advanced_messages([CloseFrameInfo], State)
             end,
    case Result of
        {ok, State1}  ->
            {stop, normal, State1#state{wsstate=CloseWSState}};
        {close, _} ->
            {stop, normal, State#state{wsstate=CloseWSState}};
        {error, Reason} ->
            {stop, normal, State#state{wsstate=CloseWSState,
                                       reason={error,Reason}}};
        {'EXIT', Reason} ->
            {stop, normal, State#state{wsstate=CloseWSState,
                                       reason={error,Reason}}}
    end;

%% In absence of frame, call periodically the callback module.
handle_info(timeout, #state{wsstate=WSState}=State) ->
    Result = case State#state.cbtype of
                 basic    -> catch basic_messages([timeout], State);
                 advanced -> catch advanced_messages([timeout], State)
             end,
    case Result of
        {ok, State1} ->
            {noreply, State1, State1#state.timeout};
        {close, Reason} ->
            do_close(WSState, Reason),
            {stop, normal, State};
        {error, Reason} ->
            do_close(WSState, Reason),
            {stop, normal, State#state{reason={error,Reason}}};
        {'EXIT', Reason} ->
            do_close(WSState, ?WS_STATUS_INTERNAL_ERROR),
            {stop, normal, State#state{reason={error,Reason}}}
    end;

%% handle info messages
handle_info(Info, State) ->
    case (CbInfo=State#state.cbinfo)#cbinfo.info_fun of
	undefined ->
	    {noreply, State, State#state.timeout};
	InfoFun ->
	    CbMod = CbInfo#cbinfo.module,
	    {_, CbState} = State#state.cbstate,
	    Res = CbMod:InfoFun(Info, CbState),
		case Res of
		    {noreply, NewCbState} ->
			{noreply, State#state{cbstate=NewCbState}};
		    {noreply, NewCbState, Timeout} ->
			{noreply, State#state{cbstate=NewCbState}, Timeout};
		    {close, Reason, NewCbState} ->
			{stop, normal, State#state{reason=Reason, cbstate=NewCbState}}
		    end
    end.



%% ----
terminate(_, #state{wsstate=#ws_state{sock=CliSock}, cbinfo=CbInfo}=State) ->
    case CbInfo#cbinfo.terminate_fun of
        undefined ->
            ok;
        TerminateFun ->
            CbMod = CbInfo#cbinfo.module,
            {_, CbState} = State#state.cbstate,
            CbMod:TerminateFun(State#state.reason, CbState)
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
    Defaults = [{origin,any}, {callback,basic}],
    lists:foldl(Fun, GivenOpts, Defaults).


check_origin(_Origin, any)       -> ok;
check_origin(Actual,  Actual )   -> ok;
check_origin(_Actual, _Expected) -> error.


handshake(8, Arg, CliSock, _Protocol) ->
    Key        = get_nonce_header(Arg#arg.headers),
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
do_send(#ws_state{sock=Socket, vsn=ProtoVsn}, {Type, Data}) ->
    DataFrame = frame(ProtoVsn, Type,  Data),
    case yaws_api:get_sslsocket(Socket) of
        {ok, SslSocket} -> ssl:send(SslSocket, DataFrame);
        undefined       -> gen_tcp:send(Socket, DataFrame)
    end.


do_close(WSState, Status) when is_integer(Status) ->
    do_send(WSState, {close, <<Status:16/big>>});
do_close(WSState, {Status,Reason}) when is_integer(Status), is_binary(Reason) ->
    do_send(WSState, {close, <<Status:16/big, Reason/binary>>});
do_close(WSState, _) ->
    Status = ?WS_STATUS_INTERNAL_ERROR,
    do_send(WSState, {close, <<Status:16/big>>}).


deliver_xxx(CliSock, Code) ->
    Reply = ["HTTP/1.1 ",integer_to_list(Code), $\s,
             yaws_api:code_to_phrase(Code), "\r\n",
             "Connection: close\r\n",
             "\r\n"],
    case yaws_api:get_sslsocket(CliSock) of
        {ok, SslSocket} -> ssl:send(SslSocket, Reply);
        undefined       -> gen_tcp:send(CliSock, Reply)
    end.


%% ----
basic_messages([], State) ->
    {ok, State};
basic_messages([timeout|Rest],
               #state{cbinfo=CbInfo, cbstate={FrameState,CbState}}=State) ->
    CbMod = CbInfo#cbinfo.module,
    Res = case CbInfo#cbinfo.msg_fun_2 of
              undefined ->
                  MsgFun1 = CbInfo#cbinfo.msg_fun_1,
                  CbMod:MsgFun1(timeout);
              MsgFun2 ->
                  CbMod:MsgFun2(timeout, CbState)
          end,
    case Res of
        {reply, Messages} ->
            do_send(State#state.wsstate, Messages),
            basic_messages(Rest, State);
        {reply, Messages, CbState1} ->
            do_send(State#state.wsstate, Messages),
            basic_messages(Rest, State#state{cbstate={FrameState, CbState1}});

        noreply ->
            basic_messages(Rest, State);
        {noreply, CbState1} ->
            basic_messages(Rest, State#state{cbstate={FrameState, CbState1}});

        {close, Messages, Reason} ->
            do_send(State#state.wsstate, Messages),
            {close, Reason};

        {close, Reason} ->
            {close, Reason};
        {error, Reason} ->
            {error, Reason}
    end;
basic_messages([FrameInfo|Rest],
               #state{cbinfo=CbInfo, cbstate={FrameState,CbState}}=State) ->
    case handle_message(FrameInfo, FrameState) of
        {continue, FrameState1} ->
            basic_messages(Rest, State#state{cbstate={FrameState1,CbState}});
        {message, Message} ->
            CbMod = CbInfo#cbinfo.module,
            Res = case CbInfo#cbinfo.msg_fun_2 of
                      undefined ->
                          MsgFun1 = CbInfo#cbinfo.msg_fun_1,
                          CbMod:MsgFun1(Message);
                      MsgFun2 ->
                          CbMod:MsgFun2(Message, CbState)
                  end,
            case Res of
                {reply, Messages} ->
                    do_send(State#state.wsstate, Messages),
                    basic_messages(Rest, State#state{cbstate={{none,<<>>},
                                                              CbState}});
                {reply, Messages, CbState1} ->
                    do_send(State#state.wsstate, Messages),
                    basic_messages(Rest, State#state{cbstate={{none,<<>>},
                                                              CbState1}});

                noreply ->
                    basic_messages(Rest, State#state{cbstate={{none,<<>>},
                                                              CbState}});
                {noreply, CbState1} ->
                    basic_messages(Rest, State#state{cbstate={{none,<<>>},
                                                              CbState1}});

                {close, Messages, Reason} ->
                    do_send(State#state.wsstate, Messages),
                    {close, Reason};

                {close, Reason} ->
                    {close, Reason};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%% unfragmented text message
handle_message(#ws_frame_info{fin=1, opcode=text, data=Data}, {none, <<>>}) ->
    case unicode:characters_to_binary(Data, utf8, utf8) of
        Data -> {message, {text, Data}};
        _    -> {error, {?WS_STATUS_INVALID_PAYLOAD, <<"invalid utf-8">>}}
    end;

%% start of a fragmented text message
handle_message(#ws_frame_info{fin=0, opcode=text, data=Data}, {none, <<>>}) ->
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
               {text, Dec0, Rest0}) ->
    Len = iolist_size(Dec0) + byte_size(Rest0) + byte_size(Data),
    if
        Len > ?MAX_PAYLOAD ->
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
               {text, Dec, Rest}) ->
    Len = iolist_size(Dec) + byte_size(Rest) + byte_size(Data),
    if
        Len > ?MAX_PAYLOAD ->
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
handle_message(#ws_frame_info{fin=1, opcode=binary, data=Data}, {none, <<>>}) ->
    {message, {binary, Data}};

%% start of a fragmented binary message
handle_message(#ws_frame_info{fin=0, opcode=binary, data=Data}, {none, <<>>}) ->
    {continue, {binary, Data}};

%% non-final continuation of a fragmented binary message
handle_message(#ws_frame_info{fin=0, data=Data, opcode=continuation},
               {binary, FragAcc}) ->
    Len = byte_size(FragAcc) + byte_size(Data),
    if
        Len > ?MAX_PAYLOAD ->
            {error, {?WS_STATUS_MSG_TOO_BIG, <<"Message too big">>}};
        true ->
            {continue, {binary, <<FragAcc/binary,Data/binary>>}}
    end;

%% end of binary fragmented message
handle_message(#ws_frame_info{fin=1, opcode=continuation, data=Data},
               {binary, FragAcc}) ->
    Len = byte_size(FragAcc) + byte_size(Data),
    if
        Len > ?MAX_PAYLOAD ->
            {error, {?WS_STATUS_MSG_TOO_BIG, <<"Message too big">>}};
        true ->
            Unfragged = <<FragAcc/binary, Data/binary>>,
            {message, {binary, Unfragged}}
    end;


handle_message(#ws_frame_info{opcode=ping, data=Data, ws_state=WSState}, Acc) ->
    do_send(WSState, {pong, Data}),
    {continue, Acc};

handle_message(#ws_frame_info{opcode=pong}, Acc) ->
    %% A response to an unsolicited pong frame is not expected.
    %% http://tools.ietf.org/html/\
    %%        draft-ietf-hybi-thewebsocketprotocol-08#section-4
    {continue, Acc};

%% According to RFC 6455 section 5.4, control messages like close MAY be
%% injected in the middle of a fragmented message, which is why we pass FragType
%% and FragAcc along below. Whether any clients actually do this in practice, I
%% don't know.
handle_message(#ws_frame_info{opcode=close, length=Len, data=Data}, _Acc) ->
    Message = case Len of
                  0 -> {close, ?WS_STATUS_NORMAL, <<>>};
                  1 -> {close, ?WS_STATUS_PROTO_ERROR, <<"protocol error">>};
                  _ ->
                      <<Status:16/big, Msg/binary>> = Data,
                      case unicode:characters_to_binary(Msg, utf8, utf8) of
                          Msg ->
                              {close, check_close_code(Status), Msg};
                          _ ->
                              {close, ?WS_STATUS_INVALID_PAYLOAD,
                               <<"invalid utf-8">>}
                      end
              end,
    {message, Message};

handle_message(#ws_frame_info{}, _Acc) ->
    {error, {?WS_STATUS_PROTO_ERROR, <<"protocol error">>}};

handle_message({fail_connection, Status, Msg}, _Acc) ->
    {error, {Status, Msg}}.


advanced_messages([], State) ->
    {ok, State};
advanced_messages([FrameInfo|Rest],
                  #state{cbinfo=CbInfo, cbstate={undefined,CbState}}=State) ->
    CbMod   = CbInfo#cbinfo.module,
    MsgFun2 = CbInfo#cbinfo.msg_fun_2,
    case CbMod:MsgFun2(FrameInfo, CbState) of
        {reply, Messages, CbState1} ->
            do_send(State#state.wsstate, Messages),
            advanced_messages(Rest, State#state{cbstate={undefined,CbState1}});
        {noreply, CbState1} ->
            advanced_messages(Rest, State#state{cbstate={undefined,CbState1}});

        {close, Messages, Reason} ->
            do_send(State#state.wsstate, Messages),
            {close, Reason};

        {close, Reason} ->
            {close, Reason};
        {error, Reason} ->
            {error, Reason}
    end.


%% The checks for close status codes here are based on RFC 6455 and on the
%% autobahn testsuite (http://autobahn.ws/testsuite).
check_close_code(Code) ->
    if
        Code >= 3000 andalso Code =< 4999 ->
            Code;
        Code < 1000 ->
            ?WS_STATUS_PROTO_ERROR;
        Code >= 1004 andalso Code =< 1006 ->
            ?WS_STATUS_PROTO_ERROR;
        Code >= 1012 andalso Code =< 1016 ->
            ?WS_STATUS_PROTO_ERROR;
        Code > 1016 ->
            ?WS_STATUS_PROTO_ERROR;
        true ->
            Code
    end.


%% ----
ws_version(Headers) ->
    VersionVal = query_header("sec-websocket-version", Headers),
    case VersionVal of
        "8"  -> 8;
        "13" -> 8 % treat 13 like 8. Right now 13 support is as good as that
                  % of 8, according to autobahn 0.4.3
    end.

buffer(Socket, Len, Buffered) ->
    case Buffered of
        <<_Expected:Len/binary>> = Return -> % exactly enough
            Return;
        <<_Expected:Len/binary,_Extra/binary>> = Return-> % more than expected
            Return;
        _ ->
            %% not enough
            Needed = Len - binary_length(Buffered),
            {ok, More} = case yaws_api:get_sslsocket(Socket) of
                             {ok, SslSocket} -> ssl:recv(SslSocket, Needed);
                             undefined       -> gen_tcp:recv(Socket, Needed)
                         end,
            <<Buffered/binary, More/binary>>
    end.

binary_length(<<>>) ->
    0;
binary_length(<<_First:1/binary, Rest/binary>>) ->
    1 + binary_length(Rest).


checks(Unframed) ->
    check_reserved_bits(Unframed).

check_control_frame(Len, Opcode, Fin) ->
    if
        (Len > 125) and (Opcode > 7) ->
            %% http://tools.ietf.org/html/\
            %%          draft-ietf-hybi-thewebsocketprotocol-08#section-4.5
            {fail_connection, ?WS_STATUS_PROTO_ERROR,
             <<"control frame > 125 bytes">>};
        (Fin == 0) and (Opcode > 7) ->
            {fail_connection, ?WS_STATUS_PROTO_ERROR,
             <<"control frame may not be fragmented">>};
        true ->
            ok
    end.

%% no extensions are supported yet.
%% http://tools.ietf.org/html/\
%%               draft-ietf-hybi-thewebsocketprotocol-08#section-4.2
check_reserved_bits(Unframed = #ws_frame_info{rsv=0}) ->
    check_reserved_opcode(Unframed);
check_reserved_bits(#ws_frame_info{rsv=RSV}) ->
    {fail_connection, ?WS_STATUS_PROTO_ERROR, <<"rsv bits were ", (RSV+48),
                              " but should be unset">>}.


check_reserved_opcode(#ws_frame_info{opcode = undefined}) ->
    {fail_connection, ?WS_STATUS_PROTO_ERROR, <<"Reserved opcode">>};
check_reserved_opcode(Unframed) ->
    Unframed.


ws_frame_info(#ws_state{sock=Socket},
              <<Fin:1, Rsv:3, Opcode:4, Masked:1, Len1:7, Rest/binary>>) ->
    case check_control_frame(Len1, Opcode, Fin) of
        ok ->
            case ws_frame_info_secondary(Socket, Masked, Len1, Rest) of
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

ws_frame_info(State = #ws_state{sock=Socket}, FirstPacket) ->
    ws_frame_info(State, buffer(Socket, 2, FirstPacket)).


ws_frame_info_secondary(Socket, Masked, Len1, Rest) ->
    MaskLength = case Masked of
                     0 -> 0;
                     1 -> 4
                 end,
    case Len1 of
        126 ->
            <<Len:16, MaskingKey:MaskLength/binary, Rest2/binary>> =
                buffer(Socket, 6, Rest);
        127 ->
            <<Len:64, MaskingKey:MaskLength/binary, Rest2/binary>> =
                buffer(Socket, 12, Rest);
        Len ->
            <<MaskingKey:MaskLength/binary, Rest2/binary>> =
                buffer(Socket, 4, Rest)
    end,
    if
        Len > ?MAX_PAYLOAD ->
            error_logger:error_msg(
              "Payload length ~p longer than max allowed of ~p",
              [Len, ?MAX_PAYLOAD]
             ),
            {fail_connection, ?WS_STATUS_MSG_TOO_BIG, <<"Message too big">>};
        true ->
            <<Payload:Len/binary, Excess/binary>> = buffer(Socket, Len, Rest2),
            {ws_frame_info_secondary, Len, MaskingKey, Payload, Excess}
    end.

unframe_active_once(State, FirstPacket) ->
    Frames = unframe(State, FirstPacket),
    websocket_setopts(State, [{active, once}]),
    Frames.

%% Returns all the WebSocket frames fully or partially contained in FirstPacket,
%% reading exactly as many more bytes from Socket as are needed to finish
%% unframing the last frame partially included in FirstPacket, if needed.
%%
%% The length of this list and depth of this recursion is limited by the size of
%% your socket receive buffer.
%%
%% -> { #ws_state, [#ws_frame_info,...,#ws_frame_info] }
unframe(_State, <<>>) ->
    [];
unframe(State, FirstPacket) ->
    case unframe_one(State, FirstPacket) of
        {FrameInfo = #ws_frame_info{ws_state = NewState}, RestBin} ->
            %% Every new recursion uses the #ws_state from the calling
            %% recursion.
            [FrameInfo | unframe(NewState, RestBin)];
        Fail ->
            [Fail]
    end.

%% -> {#ws_frame_info, RestBin} | {fail_connection, Reason}
unframe_one(State = #ws_state{vsn=8}, FirstPacket) ->
    case ws_frame_info(State, FirstPacket) of
        {FrameInfo = #ws_frame_info{}, RestBin} ->
            Unmasked = mask(FrameInfo#ws_frame_info.masking_key,
                            FrameInfo#ws_frame_info.payload),
            NewState = frag_state_machine(State, FrameInfo),
            Unframed = FrameInfo#ws_frame_info{data = Unmasked,
                                               ws_state = NewState},

            case checks(Unframed) of
                #ws_frame_info{} when is_record(NewState, ws_state) ->
                    {Unframed, RestBin};
                #ws_frame_info{} when not is_record(NewState, ws_state) ->
                    NewState; % pass back the error details
                Fail ->
                    Fail
            end;
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
            {fail_connection, ?WS_STATUS_PROTO_ERROR, <<"fragmentation rules violated">>}
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


frame(8, Type, Data) ->
    %% FIN=true because we're not fragmenting.
    %% OPCODE=1 for text
    FirstByte = 128 bor atom_to_opcode(Type),
    Length    = byte_size(Data),
    if
        Length < 126 ->
            <<FirstByte, 0:1, Length:7, Data:Length/binary>>;
        Length =< 65535 ->
            <<FirstByte, 0:1, 126:7, Length:16, Data:Length/binary>>;
        true ->
            Defined = (Length =< math:pow(2,64)),
            %% TODO: Is the correctness of this pow call better than the speed
            %% and danger of not checking?
            case Defined of
                true ->
                    <<FirstByte, 0:1, 127:7, Length:64, Data:Length/binary>>;
                _ ->
                    undefined
            end
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
    HashBin = crypto:sha(Salted),
    base64:encode_to_string(HashBin).
