%%%----------------------------------------------------------------------
%%% File    : yaws_websockets.erl
%%% Author  : Davide Marques <nesrait@gmail.com>
%%% Purpose :
%%% Created :  18 Dec 2009 by Davide Marques <nesrait@gmail.com>
%%% Modified:
%%%----------------------------------------------------------------------

-module(yaws_websockets).
-author('nesrait@gmail.com').

-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").

-include_lib("kernel/include/file.hrl").
-export([handshake/3, unframe/2, frame/3]).

handshake(Arg, ContentPid, SocketMode) ->
    CliSock = Arg#arg.clisock,
    case get_origin_header(Arg#arg.headers) of
%	undefined ->
	    %% TODO: Lack of origin header is allowed for non-browser clients 
            %% in hybi 17 but for simplicity the connection is closed for now.
	    %jdtodo io:format("No origin header.~n"),
	    %% Yaws will take care of closing the socket
%	    ContentPid ! discard;
	Origin ->
	    ProtocolVersion = ws_version(Arg#arg.headers),
	    Protocol = get_protocol_header(Arg#arg.headers),
	    Host = (Arg#arg.headers)#headers.host,
	    {abs_path, Path} = (Arg#arg.req)#http_request.path,
	    SC = get(sc),
	    WebSocketLocation =
		case SC#sconf.ssl of
                    undefined -> "ws://" ++ Host ++ Path;
                    _ -> "wss://" ++ Host ++ Path
		end,
	    Handshake = handshake(ProtocolVersion, Arg, CliSock,
                                  WebSocketLocation, Origin, Protocol),
	    case SC#sconf.ssl of
		undefined ->
		    gen_tcp:send(CliSock, Handshake),
		    inet:setopts(CliSock, [{packet, raw}, SocketMode]),
		    TakeOverResult =
			gen_tcp:controlling_process(CliSock, ContentPid);
		_ ->
		    ssl:send(CliSock, Handshake),
		    ssl:setopts(CliSock, [{packet, raw}, SocketMode]),
		    TakeOverResult =
			ssl:controlling_process(CliSock, ContentPid)
	    end,
	    case TakeOverResult of
		ok ->
		    State = #ws_state{ sock = CliSock, 
				       vsn  = ProtocolVersion,
				       frag_type = none },
		    ContentPid ! {ok, State};
		{error, Reason} ->
		    ContentPid ! discard,
		    exit({websocket, Reason})
	    end
    end,
    exit(normal).

handshake(8, Arg, _CliSock, _WebSocketLocation, _Origin, _Protocol) ->
    io:format("Version 8!~n",[]),
    Key = get_nonce_header(Arg#arg.headers),
    AcceptHash = hash_nonce(Key), 
    ["HTTP/1.1 101 Switching Protocols\r\n",
     "Upgrade: websocket\r\n",
     "Connection: Upgrade\r\n",
     "Sec-WebSocket-Accept: ", AcceptHash , "\r\n",
     "\r\n"].

ws_version(Headers) ->
    case query_header("sec-websocket-version", Headers) of
	"8" -> 8
    end.

buffer(Socket, Len, Buffered) ->
    case Buffered of 
	<<_Expected:Len/binary>> = Return -> % exactly enough
	    %debug(val, {buffering, "got:", Len}),
	    Return;
	<<_Expected:Len/binary,_Extra/binary>> = Return-> % more than expected
	    %debug(val, {buffering, "got:", Len, "and more!"}),
	    Return;
	_ -> % not enough
	    %debug(val, {buffering, "need:", Len, "waiting for more..."}),
	    % TODO: take care of ssl sockets
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
	    % http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-08#section-4.5
	    {fail_connection, "control frame > 125 bytes"};
	(Fin == 0) and (Opcode > 7) ->
	    {fail_connection, "control frame may not be fragmented"};
	true ->
	    ok
    end.

% no extensions are supported yet.
% http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-08#section-4.2
check_reserved_bits(Unframed = #ws_frame_info{rsv=0}) ->
    check_utf8(Unframed);
check_reserved_bits(#ws_frame_info{rsv=RSV}) ->
    {fail_connection, "rsv bits were " ++ integer_to_list(RSV) ++ " but should be unset."}.

% http://www.erlang.org/doc/apps/stdlib/unicode_usage.html#id191467
% Heuristic identification of UTF-8
check_utf8(Unframed = #ws_frame_info{opcode = text, data=Bin}) when is_binary(Bin) ->
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
%    debug(val,"ws_frame_info"),
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
%    debug(val, "ws_frame_info input was short, buffering..."),
    ws_frame_info(State, buffer(Socket, 2,FirstPacket)).
	    
ws_frame_info_secondary(Socket, Len1, Rest) ->
    case Len1 of
	126 ->
	    <<Len:16, MaskingKey:4/binary, Rest2/binary>> = buffer(Socket, 6, Rest);
	127 ->
	    <<Len:64, MaskingKey:4/binary, Rest2/binary>> = buffer(Socket, 12, Rest);
	Len ->
%	    debug(val, {'Len', Len}),
	    <<MaskingKey:4/binary, Rest2/binary>> = buffer(Socket, 4, Rest)
    end,
    <<Payload:Len/binary, Excess/binary>> = buffer(Socket, Len, Rest2),
    {ws_frame_info_secondary, Len, MaskingKey, Payload, Excess}.

% Returns all the WebSocket frames fully or partially contained in FirstPacket,
% reading exactly as many more bytes from Socket as are needed to finish unframing
% the last frame partially included in FirstPacket, if needed.
%
% The length of this list and depth of this recursion is limited by
% the size of your socket receive buffer.
%
% -> { #ws_state, [#ws_frame_info,...,#ws_frame_info] }
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

% -> {#ws_frame_info, RestBin} | {fail_connection, Reason}
unframe_one(State = #ws_state{vsn=8}, FirstPacket) ->
    {FrameInfo = #ws_frame_info{}, RestBin} = ws_frame_info(State, FirstPacket),
    Unmasked = mask(FrameInfo#ws_frame_info.masking_key, FrameInfo#ws_frame_info.payload),
    NewState = frag_state_machine(State, FrameInfo),
    Unframed = FrameInfo#ws_frame_info{ data = Unmasked,
					ws_state = NewState },

    case checks(Unframed) of
	#ws_frame_info{} when is_record(NewState, ws_state) ->
	    {Unframed, RestBin};
	#ws_frame_info{} when not is_record(NewState, ws_state) ->
	    NewState;  %% pass back the error details
	Fail ->
	    Fail
    end.

is_control_op(Op) ->
    atom_to_opcode(Op) > 7.

%% Unfragmented message
frag_state_machine(State = #ws_state{ frag_type = none },
		   #ws_frame_info{ fin = 1 }) ->
    State;

%% Beginning of fragmented text message
frag_state_machine(State = #ws_state{ frag_type = none },
		   #ws_frame_info{ fin = 0,
				   opcode = text }) ->
    State#ws_state{ frag_type = text };

%% Beginning of fragmented binary message
frag_state_machine(State = #ws_state{ frag_type = none },
		   #ws_frame_info{ fin = 0,
				   opcode = binary }) ->
    State#ws_state{ frag_type = binary };

%% Expecting text continuation
frag_state_machine(State = #ws_state{ frag_type = text },
		   #ws_frame_info{ fin = 0,
				   opcode = continuation }) ->
    State;

%% Expecting binary continuation
frag_state_machine(State = #ws_state{ frag_type = binary },
		   #ws_frame_info{ fin = 0,
				   opcode = continuation }) ->
    State;

%% End of fragmented text message
frag_state_machine(State = #ws_state{ frag_type = text },
		   #ws_frame_info{ fin = 1,
				   opcode = continuation }) ->
    State#ws_state{ frag_type = none };

%% End of fragmented binary message
frag_state_machine(State = #ws_state{ frag_type = binary },
		   #ws_frame_info{ fin = 1,
				   opcode = continuation }) ->
    State#ws_state{ frag_type = none };


frag_state_machine(State, #ws_frame_info{ opcode = Op }) ->
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
    %FIN=true because we're not fragmenting.
    %OPCODE=1 for text
    FirstByte = 128 bor atom_to_opcode(Type),
    ByteList = binary_to_list(Data),
    Length = length(ByteList),
    if
	Length < 126 ->
	    << FirstByte, 0:1, Length:7, Data:Length/binary >>;
	Length =< 65535 ->
	    << FirstByte, 0:1, 126:7, Length:16, Data:Length/binary >>;
	true ->
	    Defined = Length =< math:pow(2,64),
	    % TODO: Is the correctness of this pow call 
	    % better than the speed and danger of not checking?
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

rmask(MaskBin = <<Mask:4/integer-unit:8>>, <<Data:4/integer-unit:8, Rest/binary>>) ->
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

query_header(Header, #headers{other=L}, Default) ->
    lists:foldl(fun({http_header,_,K0,_,V}, undefined) ->
                        K = case is_atom(K0) of
                                true ->
                                    atom_to_list(K0);
                                false ->
                                    K0
                            end,
                        case string:to_lower(K) of
                            Header ->
                                V;
                            _ ->
                                Default
                        end;
                   (_, Acc) ->
                        Acc
                end, Default, L).

hash_nonce(Nonce) ->
    Salted = Nonce ++ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11",
    HashBin = crypto:sha(Salted),
    base64:encode_to_string(HashBin).


debug(val, Val) ->
    io:format("~p~n",[Val]).
