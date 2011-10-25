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
    %jdtodo io:format("CliSock ~p~n",[CliSock]),
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
		    ContentPid ! {ok, {CliSock, ProtocolVersion}};
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

check_control_frame(Len, Opcode) ->
    if
	(Len > 125) and (Opcode > 7) ->
	    % http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-08#section-4.5
	    {error, "control frame > 125 bytes"};
	true ->
	    ok
    end.

frame_info(Socket, <<Fin:1, Rsv:3, Opcode:4, Masked:1, Len1:7, Rest/binary>>) ->
%    debug(val,"frame_info"),
    case check_control_frame(Len1, Opcode) of
	ok ->
	    {frame_info_secondary, Length, MaskingKey, Payload, Excess} 
		= frame_info_secondary(Socket, Len1, Rest),
	    FrameInfo = #frame_info{fin=Fin, 
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

frame_info(Socket, FirstPacket) ->
%    debug(val, "frame_info input was short, buffering..."),
    frame_info(Socket, buffer(Socket, 2,FirstPacket)).
	    
frame_info_secondary(Socket, Len1, Rest) ->
    case Len1 of
	126 ->
	    <<Len:16, MaskingKey:4/binary, Rest2/binary>> = buffer(Socket, 6, Rest);
	127 ->
	    <<Len:64, MaskingKey:4/binary, Rest2/binary>> = buffer(Socket, 12, Rest);
	Len ->
	    debug(val, {'Len', Len}),
	    <<MaskingKey:4/binary, Rest2/binary>> = buffer(Socket, 4, Rest)
    end,
    <<Payload:Len/binary, Excess/binary>> = buffer(Socket, Len, Rest2),
    {frame_info_secondary, Len, MaskingKey, Payload, Excess}.

% Returns all the WebSocket frames fully or partially contained in FirstPacket,
% reading exactly as many more bytes from Socket as are needed to finish unframing
% the last frame partially included in FirstPacket, if needed.
% -> [#frame_info,...,#frame_info]
unframe(_WebSocket, <<>>) ->
    [];
unframe(WebSocket, FirstPacket) ->
    {#frame_info{}=FrameInfo, RestBin} = unframe_one(WebSocket, FirstPacket),
    [FrameInfo | unframe(WebSocket, RestBin)].

% -> {#frame_info, RestBin}
unframe_one({Socket,8}=_WebSocket, FirstPacket) ->
    {FrameInfo, RestBin} = frame_info(Socket, FirstPacket),
    
    Unframed = FrameInfo#frame_info{
		 data = list_to_binary(mask(1, binary_to_list(FrameInfo#frame_info.masking_key), 
					    FrameInfo#frame_info.payload))
		},
    
    case Unframed#frame_info.opcode of
	text -> 
	    case test_utf8(Unframed#frame_info.data) of
		error ->
		    exit({error, "not utf8"});
		_ -> 
		    ok
	    end;
	_ -> ok
    end,
    {Unframed, RestBin}.
    

opcode_to_atom(16#0) -> continuation;
opcode_to_atom(16#1) -> text;
opcode_to_atom(16#2) -> binary;
opcode_to_atom(16#8) -> close;
opcode_to_atom(16#9) -> ping;
opcode_to_atom(16#A) -> pong.

%atom_to_opcode(continuation) -> 16#0; commented out because I don't know what continuation is for.
atom_to_opcode(text) -> 16#1;
atom_to_opcode(binary) -> 16#2;
atom_to_opcode(close) -> 16#8;
atom_to_opcode(ping) -> 16#9;
atom_to_opcode(pong) -> 16#A.

% http://www.erlang.org/doc/apps/stdlib/unicode_usage.html#id191467
% Heuristic identification of UTF-8
test_utf8(Bin) when is_binary(Bin) ->
    case unicode:characters_to_binary(Bin,utf8,utf8) of
	Bin ->
	    ok;
	_ ->
	    error
    end.

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

%%  
%% mask(Pos, Mask, Data)
%%
%% N: integer, the position in the masking key to use.
%% MaskingKey: list of 8-bit integers
%% Data: bit string divisible by 8
%%
%% This could probably be done better but seems to work for now.
%%
mask(_, _, <<>>) -> [];
mask(5, MaskingKey, Data) -> mask(1, MaskingKey, Data);
mask(N, MaskingKey, <<Head:8/integer,Rest/binary>>) ->
    Masked = lists:nth(N, MaskingKey) bxor Head,
    [Masked | mask(N + 1, MaskingKey, Rest)].


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
