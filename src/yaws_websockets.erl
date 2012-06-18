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
-include("yaws_debug.hrl").

-include_lib("kernel/include/file.hrl").

-define(MAX_PAYLOAD, 16777216). %16MB

%% RFC 6455 section 7.4.1: status code 1000 means "normal"
-define(NORMAL_WS_STATUS, 1000).

-record(state, {
          arg,
          sconf,
          cbmod,
          opts,
          wsstate,
          cbstate,
          cbtype
         }).

-export([start/3, send/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%%
%% API
%%
start(Arg, CallbackMod, Opts) ->
    SC = get(sc),
    CliSock = Arg#arg.clisock,
    PrepdOpts = preprocess_opts(Opts),
    {ok, OwnerPid} = gen_server:start(?MODULE, [Arg, SC, CallbackMod, PrepdOpts], []),
    CliSock = Arg#arg.clisock,
    TakeOverResult = case SC#sconf.ssl of
                         undefined ->
                             inet:setopts(CliSock, [{packet, raw}, {active, once}]),
                             gen_tcp:controlling_process(CliSock, OwnerPid);
                         _ ->
                             ssl:setopts(CliSock, [{packet, raw}, {active, once}]),
                             ssl:controlling_process(CliSock, OwnerPid)
                     end,
    case TakeOverResult of
        ok ->
            gen_server:cast(OwnerPid, ok),
            exit(normal);
        {error, Reason}=Error ->
            gen_server:cast(OwnerPid, Error),
            exit({websocket, Reason})
    end.

send(Pid, {Type, Data}) ->
    gen_server:cast(Pid, {send, {Type, Data}}).

%%
%% gen_server functions
%%
init([Arg, SC, CallbackMod, Opts]) ->
    {ok, #state{arg=Arg, sconf=SC, cbmod=CallbackMod, opts=Opts}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(ok, #state{arg=Arg, sconf=SC, opts=Opts}=State) ->
    CliSock = Arg#arg.clisock,
    OriginOpt = lists:keyfind(origin, 1, Opts),
    Origin = get_origin_header(Arg#arg.headers),
    case origin_check(Origin, OriginOpt) of
        {error, Error} ->
            error_logger:error_msg(Error),
            {stop, {error, Error}, State};
        ok ->
            ProtocolVersion = ws_version(Arg#arg.headers),
            Protocol = get_protocol_header(Arg#arg.headers),
            Host = (Arg#arg.headers)#headers.host,
            {abs_path, Path} = (Arg#arg.req)#http_request.path,

            WebSocketLocation =
                case SC#sconf.ssl of
                    undefined -> "ws://" ++ Host ++ Path;
                    _ -> "wss://" ++ Host ++ Path
                end,

            Handshake = handshake(ProtocolVersion, Arg, CliSock,
                                  WebSocketLocation, Origin, Protocol),
            gen_tcp:send(CliSock, Handshake),   % TODO: use the yaws way of
                                                % supporting normal
                                                % and ssl sockets
            {callback, CallbackType} = lists:keyfind(callback, 1, Opts),
            WSState = #ws_state{sock = CliSock,
                                vsn  = ProtocolVersion,
                                frag_type = none},
            CallbackState = case CallbackType of
                               basic ->
                                    {none, <<>>};
                                {advanced, InitialState} ->
                                    InitialState
                            end,
            NState = State#state{wsstate=WSState,
                                 cbstate=CallbackState,
                                 cbtype=CallbackType},
            {noreply, NState}
    end;
handle_cast({send, {Type, Data}}, #state{wsstate=WSState}=State) ->
    do_send(WSState, {Type, Data}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, FirstPacket}, #state{wsstate=#ws_state{sock=Socket}}=State) ->
    #state{cbmod=CallbackMod, wsstate=WSState,
           cbstate=CallbackState, cbtype=CallbackType} = State,
    FrameInfos = unframe_active_once(WSState, FirstPacket),
    {Results, NewCallbackState} =
        case CallbackType of
            basic ->
                {BasicMessages, NewCallbackState0} =
                    basic_messages(FrameInfos, CallbackState),
                CallbackResults = lists:map(
                                    fun(M) ->
                                            CallbackMod:handle_message(M)
                                    end,
                                    BasicMessages),
                FoldFun = handle_result_fun(WSState),
                {catch lists:foldl(FoldFun, ok, lists:zip(FrameInfos, CallbackResults)),
                 NewCallbackState0};
            {advanced,_} ->
                catch lists:foldl(do_callback_fun(WSState, CallbackMod),
                                  {ok, CallbackState},
                                  FrameInfos)
        end,
    case Results of
        {close, Reason} ->
            do_close(WSState, Reason),
            {stop, Reason, State#state{cbstate=NewCallbackState}};
        _ ->
            Last = lists:last(FrameInfos),
            NewWSState = Last#ws_frame_info.ws_state,
            {noreply, State#state{wsstate=NewWSState, cbstate=NewCallbackState}}
    end;
handle_info({tcp_closed, Socket}, #state{wsstate=#ws_state{sock=Socket}}=State) ->
    %% The only way we should get here is due to an abnormal close.
    %% Section 7.1.5 of RFC 6455 specifies 1006 as the connection
    %% close code for abnormal closure. It's also described in
    %% section 7.4.1.
    #state{cbmod=CallbackMod, wsstate=WSState,
           cbstate=CallbackState, cbtype=CallbackType} = State,
    CloseStatus = 1006,
    case CallbackType of
        basic ->
            CallbackMod:handle_message({close, CloseStatus, <<>>});
        {advanced, _} ->
            ClosePayload = <<CloseStatus:16/big>>,
            CloseWSState = WSState#ws_state{sock=undefined,
                                            frag_type=none},
            CloseFrameInfo = #ws_frame_info{fin=1,
                                            rsv=0,
                                            opcode=close,
                                            masked=0,
                                            masking_key=0,
                                            length=byte_size(ClosePayload),
                                            payload=ClosePayload,
                                            ws_state=CloseWSState},
            CallbackMod:handle_message(CloseFrameInfo, CallbackState)
    end,
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

%% internal functions

do_send(#ws_state{sock=Socket, vsn=ProtoVsn}, {Type, Data}) ->
    DataFrame = frame(ProtoVsn, Type,  Data),
    case Socket of
        {sslsocket,_,_} ->
            ssl:send(Socket, DataFrame);
        _ ->
            gen_tcp:send(Socket, DataFrame)
    end.

preprocess_opts(GivenOpts) ->
    Fun = fun({Key, Default}, Opts) ->
                  case lists:keyfind(Key, 1, Opts) of
                      false ->
                          [{Key, Default}|Opts];
                      _ -> Opts
                  end
          end,
    Defaults = [{origin, any},
                {callback, basic}],
    lists:foldl(Fun, GivenOpts, Defaults).

origin_check(_Origin, {origin, any}) ->
    ok;
origin_check(Actual, {origin, _Expected=Actual}) ->
    ok;
origin_check(Actual, {origin, Expected}) ->
    Error = io_lib:format("Expected origin ~p but found ~p.",
                          [Expected, Actual]),
    {error, Error}.

handshake(8, Arg, _CliSock, _WebSocketLocation, _Origin, _Protocol) ->
    Key = get_nonce_header(Arg#arg.headers),
    AcceptHash = hash_nonce(Key),
    ["HTTP/1.1 101 Switching Protocols\r\n",
     "Upgrade: websocket\r\n",
     "Connection: Upgrade\r\n",
     "Sec-WebSocket-Accept: ", AcceptHash , "\r\n",
     "\r\n"].

handle_result_fun(WSState) ->
    fun({FrameInfo, Result}, Acc) ->
            case Result of
                {reply, {Type, Data}} ->
                    do_send(WSState, {Type, Data}),
                    Acc;
                noreply ->
                    Acc;
                {close, Reason} ->
                    %% check if client sent a close, and if so, verify that the
                    %% close code is legal
                    NReason = check_close_code(FrameInfo, Reason),
                    throw({close, NReason})
            end
    end.

do_callback_fun(WSState, CallbackMod) ->
    fun(FrameInfo, {Results, CallbackState}) ->
            case CallbackMod:handle_message(FrameInfo, CallbackState) of
                {reply, {Type, Data}, NewCallbackState} ->
                    do_send(WSState, {Type, Data}),
                    {Results, NewCallbackState};
                {noreply, NewCallbackState} ->
                    {Results, NewCallbackState};
                {close, Reason} ->
                    %% check if client sent a close, and if so, verify that the
                    %% close code is legal
                    NReason = check_close_code(FrameInfo, Reason),
                    throw({{close, NReason}, CallbackState})
            end
    end.

%% The checks for close status codes here are based on RFC 6455 and on
%% the autobahn testsuite (http://autobahn.ws/testsuite).
check_close_code(#ws_frame_info{opcode=close, length=Len, data=Data}, Reason) ->
    if
        Len == 0 ->
            Reason;
        Len < 2 ->
            1002;
        true ->
            <<Code:16/big, _/binary>> = Data,
            if
                Code >= 3000 andalso Code =< 4999 ->
                    Reason;
                Code < 1000 ->
                    1002;
                Code >= 1004 andalso Code =< 1006 ->
                    1002;
                Code >= 1012 andalso Code =< 1016 ->
                    1002;
                Code > 1016 ->
                    1002;
                true ->
                    Reason
            end
    end;
check_close_code(_, Reason) ->
    Reason.

do_close(WSState, Reason) ->
    Status = case Reason of
                 _ when is_integer(Reason) -> Reason;
                 _ -> ?NORMAL_WS_STATUS
             end,
    do_send(WSState, {close, <<Status:16/big>>}).

basic_messages(FrameInfos, {FragType, FragAcc}) ->
    {Messages, NewFragType, NewFragAcc} =
        lists:foldl(fun handle_message/2, {[], FragType, FragAcc}, FrameInfos),
    {Messages, {NewFragType, NewFragAcc}}.

%% start of a fragmented message
handle_message(#ws_frame_info{fin=0,
                              opcode=FragType,
                              data=Data},
               {Messages, none, <<>>}) ->
    {Messages, FragType, Data};

%% non-final continuation of a fragmented message
handle_message(#ws_frame_info{fin=0,
                              data=Data,
                              opcode=continuation},
               {Messages, FragType, FragAcc}) ->
    {Messages, FragType, <<FragAcc/binary,Data/binary>>};

%% end of text fragmented message
handle_message(#ws_frame_info{fin=1,
                              opcode=continuation,
                              data=Data},
               {Messages, text, FragAcc}) ->
    Unfragged = <<FragAcc/binary, Data/binary>>,
    NewMessage = {text, Unfragged},
    {Messages ++ [NewMessage], none, <<>>};

%% unfragmented text message
handle_message(#ws_frame_info{opcode=text, data=Data},
               {Messages, none, <<>>}) ->
    NewMessage = {text, Data},
    {Messages ++ [NewMessage], none, <<>>};

%% end of binary fragmented message
handle_message(#ws_frame_info{fin=1,
                              opcode=continuation,
                              data=Data},
               {Messages, binary, FragAcc}) ->
    Unfragged = <<FragAcc/binary, Data/binary>>,
    NewMessage = {binary, Unfragged},
    {Messages ++ [NewMessage], none, <<>>};

handle_message(#ws_frame_info{opcode=binary,
                              data=Data},
               {Messages, none, <<>>}) ->
    NewMessage = {binary, Data},
    {Messages ++ [NewMessage], none, <<>>};

handle_message(#ws_frame_info{opcode=ping,
                              data=Data,
                              ws_state=WSState},
                Acc) ->
    do_send(WSState, {pong, Data}),
    Acc;

handle_message(#ws_frame_info{opcode=pong}, Acc) ->
    %% A response to an unsolicited pong frame is not expected.
    %% http://tools.ietf.org/html/\
    %%        draft-ietf-hybi-thewebsocketprotocol-08#section-4
    Acc;

%% According to RFC 6455 section 5.4, control messages like close
%% MAY be injected in the middle of a fragmented message, which is
%% why we pass FragType and FragAcc along below. Whether any clients
%% actually do this in practice, I don't know.
handle_message(#ws_frame_info{opcode=close,
                              length=Len,
                              data=Data},
               {Messages, FragType, FragAcc}) ->
    NewMessage = case Len of
                     0 ->
                         {close, ?NORMAL_WS_STATUS, <<>>};
                     _ ->
                         <<Status:16/big, Msg/binary>> = Data,
                         {close, Status, Msg}
                 end,
    {Messages ++ [NewMessage], FragType, FragAcc};

handle_message(#ws_frame_info{}, Acc) ->
    Acc.

ws_version(Headers) ->
    VersionVal = query_header("sec-websocket-version", Headers),
    case VersionVal of
        "8" -> 8;
        "13" -> 8 % treat 13 like 8. Right now 13 support is as good as that
                  % of 8, according to autobahn 0.4.3
    end.

buffer(Socket, Len, Buffered) ->
    case Buffered of
        <<_Expected:Len/binary>> = Return ->    % exactly enough
            %% debug(val, {buffering, "got:", Len}),
            Return;
        <<_Expected:Len/binary,_Extra/binary>> = Return-> % more than expected
            %% debug(val, {buffering, "got:", Len, "and more!"}),
            Return;
        _ ->
            %% not enough
            %% debug(val, {buffering, "need:", Len, "waiting for more..."}),
            %% TODO: take care of ssl sockets
            Needed = Len - binary_length(Buffered),
            {ok, More} = gen_tcp:recv(Socket, Needed),
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
            {fail_connection, "control frame > 125 bytes"};
        (Fin == 0) and (Opcode > 7) ->
            {fail_connection, "control frame may not be fragmented"};
        true ->
            ok
    end.

%% no extensions are supported yet.
%% http://tools.ietf.org/html/\
%%               draft-ietf-hybi-thewebsocketprotocol-08#section-4.2
check_reserved_bits(Unframed = #ws_frame_info{rsv=0}) ->
    check_utf8(Unframed);
check_reserved_bits(#ws_frame_info{rsv=RSV}) ->
    {fail_connection,
     "rsv bits were " ++ integer_to_list(RSV) ++ " but should be unset."}.

%% http://www.erlang.org/doc/apps/stdlib/unicode_usage.html#id191467
%% Heuristic identification of UTF-8
check_utf8(Unframed = #ws_frame_info{opcode=text, data=Bin})
  when is_binary(Bin) ->
    case unicode:characters_to_binary(Bin,utf8,utf8) of
        Bin ->
            Unframed;
        _ ->
            {fail_connection, "not valid utf-8."}
    end;
check_utf8(Unframed) ->
    check_reserved_opcode(Unframed).

check_reserved_opcode(#ws_frame_info{opcode = undefined}) ->
    {fail_connection, "Reserved opcode."};
check_reserved_opcode(Unframed) ->
    Unframed.


ws_frame_info(#ws_state{sock=Socket},
              <<Fin:1, Rsv:3, Opcode:4, Masked:1, Len1:7, Rest/binary>>) ->
    case check_control_frame(Len1, Opcode, Fin) of
        ok ->
            {ws_frame_info_secondary, Length, MaskingKey, Payload, Excess}
                = ws_frame_info_secondary(Socket, Len1, Rest),
            FrameInfo = #ws_frame_info{fin=Fin,
                                       rsv=Rsv,
                                       opcode=opcode_to_atom(Opcode),
                                       masked=Masked,
                                       masking_key=MaskingKey,
                                       length=Length,
                                       payload=Payload},
            {FrameInfo, Excess};
        Other ->
            Other
    end;

ws_frame_info(State = #ws_state{sock=Socket}, FirstPacket) ->
    ws_frame_info(State, buffer(Socket, 2,FirstPacket)).

ws_frame_info_secondary(Socket, Len1, Rest) ->
    case Len1 of
        126 ->
            <<Len:16, MaskingKey:4/binary, Rest2/binary>> =
                buffer(Socket, 6, Rest);
        127 ->
            <<Len:64, MaskingKey:4/binary, Rest2/binary>> =
                buffer(Socket, 12, Rest);
        Len ->
            <<MaskingKey:4/binary, Rest2/binary>> = buffer(Socket, 4, Rest)
    end,
    if
	Len > ?MAX_PAYLOAD ->
	    Error = io_lib:format(
                      "Payload length ~p longer than max allowed of ~p",
                      [Len, ?MAX_PAYLOAD]),
	    exit({error, Error});
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
%% The length of this list and depth of this recursion is limited by
%% the size of your socket receive buffer.
%%
%% -> { #ws_state, [#ws_frame_info,...,#ws_frame_info] }
unframe(_State, <<>>) ->
    [];
unframe(State, FirstPacket) ->
    case unframe_one(State, FirstPacket) of
        {FrameInfo = #ws_frame_info{ws_state = NewState}, RestBin} ->
            %% Every new recursion uses the #ws_state from the calling recursion.
            [FrameInfo | unframe(NewState, RestBin)];
        Fail ->
            [Fail]
    end.

%% -> {#ws_frame_info, RestBin} | {fail_connection, Reason}
unframe_one(State = #ws_state{vsn=8}, FirstPacket) ->
    {FrameInfo = #ws_frame_info{}, RestBin} = ws_frame_info(State, FirstPacket),
    Unmasked = mask(FrameInfo#ws_frame_info.masking_key,
                    FrameInfo#ws_frame_info.payload),
    NewState = frag_state_machine(State, FrameInfo),
    Unframed = FrameInfo#ws_frame_info{data = Unmasked,
                                       ws_state = NewState},

    case checks(Unframed) of
        #ws_frame_info{} when is_record(NewState, ws_state) ->
            {Unframed, RestBin};
        #ws_frame_info{} when not is_record(NewState, ws_state) ->
            NewState;   % pass back the error details
        Fail ->
            Fail
    end.

websocket_setopts(#ws_state{sock=Socket={sslsocket,_,_}}, Opts) ->
    ssl:setopts(Socket, Opts);
websocket_setopts(#ws_state{sock=Socket}, Opts) ->
    inet:setopts(Socket, Opts).

is_control_op(Op) ->
    atom_to_opcode(Op) > 7.

%% Unfragmented message
frag_state_machine(State = #ws_state{frag_type = none},
                   #ws_frame_info{fin = 1}) ->
    State;

%% Beginning of fragmented text message
frag_state_machine(State = #ws_state{frag_type = none},
                   #ws_frame_info{fin = 0,
                                  opcode = text}) ->
    State#ws_state{frag_type = text};

%% Beginning of fragmented binary message
frag_state_machine(State = #ws_state{frag_type = none},
                   #ws_frame_info{fin = 0,
                                  opcode = binary}) ->
    State#ws_state{frag_type = binary};

%% Expecting text continuation
frag_state_machine(State = #ws_state{frag_type = text},
                   #ws_frame_info{fin = 0,
                                  opcode = continuation}) ->
    State;

%% Expecting binary continuation
frag_state_machine(State = #ws_state{frag_type = binary},
                   #ws_frame_info{fin = 0,
                                  opcode = continuation}) ->
    State;

%% End of fragmented text message
frag_state_machine(State = #ws_state{frag_type = text},
                   #ws_frame_info{fin = 1,
                                  opcode = continuation}) ->
    State#ws_state{frag_type = none};

%% End of fragmented binary message
frag_state_machine(State = #ws_state{frag_type = binary},
                   #ws_frame_info{fin = 1,
                                  opcode = continuation}) ->
    State#ws_state{frag_type = none};


frag_state_machine(State, #ws_frame_info{opcode = Op}) ->
    IsControl = is_control_op(Op),
    if
        IsControl == true ->
            %% Control message never changes fragmentation state
            State;
        true ->
            %% Everything else is wrong
            {error, "fragmentation rules violated"}
    end.


opcode_to_atom(16#0) -> continuation;
opcode_to_atom(16#1) -> text;
opcode_to_atom(16#2) -> binary;
opcode_to_atom(16#8) -> close;
opcode_to_atom(16#9) -> ping;
opcode_to_atom(16#A) -> pong;
opcode_to_atom(_) -> undefined.

atom_to_opcode(continuation) -> 16#0;
atom_to_opcode(text) -> 16#1;
atom_to_opcode(binary) -> 16#2;
atom_to_opcode(close) -> 16#8;
atom_to_opcode(ping) -> 16#9;
atom_to_opcode(pong) -> 16#A.


frame(8, Type, Data) ->
    %% FIN=true because we're not fragmenting.
    %% OPCODE=1 for text
    FirstByte = 128 bor atom_to_opcode(Type),
    Length = byte_size(Data),
    if
        Length < 126 ->
            << FirstByte, 0:1, Length:7, Data:Length/binary >>;
        Length =< 65535 ->
            << FirstByte, 0:1, 126:7, Length:16, Data:Length/binary >>;
        true ->
            Defined = Length =< math:pow(2,64),
            %% TODO: Is the correctness of this pow call
            %% better than the speed and danger of not checking?
            case Defined of
                true ->
                    << FirstByte, 0:1, 127:7, Length:64, Data:Length/binary >>;
                _ ->
                    undefined
            end
    end.


mask(MaskBin, Data) ->
    list_to_binary(rmask(MaskBin, Data)).

%% unmask == mask. It's XOR of the four-byte masking key.
rmask(_,<<>>) ->
    [<<>>];

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
