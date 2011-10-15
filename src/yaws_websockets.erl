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
-export([handshake/3, unframe_one/2, unframe_all/3, frame/2]).

handshake(Arg, ContentPid, SocketMode) ->
    io:format("~p~n",[SocketMode]),
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

handshake(hi_76, Arg, CliSock, WebSocketLocation, Origin, Protocol) ->
    io:format("hi_76!~n",[]),
    {ok, Challenge} = case CliSock of
                          {sslsocket, _, _} ->
                              ssl:recv(CliSock, 8);
                          _ ->
                              gen_tcp:recv(CliSock, 8)
                      end,
    Key1 = secret_key("sec-websocket-key1", Arg#arg.headers),
    Key2 = secret_key("sec-websocket-key2", Arg#arg.headers),
    ChallengeResponse = challenge(Key1, Key2, binary_to_list(Challenge)),
    ["HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
     "Upgrade: WebSocket\r\n",
     "Connection: Upgrade\r\n",
     "Sec-WebSocket-Origin: ", Origin, "\r\n",
     "Sec-WebSocket-Location: ", WebSocketLocation, "\r\n",
     "Sec-WebSocket-Protocol: ", Protocol, "\r\n",
     "\r\n", ChallengeResponse];

handshake(hi_75, _Arg, _CliSock, WebSocketLocation, Origin, _Protocol) ->
    io:format("hi_75!~n",[]),
    ["HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
     "Upgrade: WebSocket\r\n",
     "Connection: Upgrade\r\n",
     "WebSocket-Origin: ", Origin, "\r\n",
     "WebSocket-Location: ", WebSocketLocation, "\r\n",
     "\r\n"];

handshake(_Ver, Arg, _CliSock, _WebSocketLocation, _Origin, _Protocol) ->
    io:format("10!~n",[]),
    Key = get_nonce_header(Arg#arg.headers),
    AcceptHash = hash_nonce(Key), 
    ["HTTP/1.1 101 Switching Protocols\r\n",
     "Upgrade: websocket\r\n",
     "Connection: Upgrade\r\n",
     "Sec-WebSocket-Accept: ", AcceptHash , "\r\n",
     "\r\n"].

ws_version(Headers) ->
    %io:format("IDing Websocket Version...~n~p~n",[Headers]),
    case query_header("sec-websocket-version", Headers) of
	"8" -> 8;
	"17" -> 17;
	_ ->
	    case query_header("sec-websocket-key1", Headers) of
		undefined -> 
		    %% might be hixie 75 but...
		    %% TODO: should really negotiate a hybi version here
		    hi_75; 
		_ -> hi_76
	    end
    end.

unframe_one(8, DataFrames) ->
    <<1:1,_Rsv:3,_Opcode:4,1:1,Len1:7,Rest/binary>> = DataFrames,

    case Len1 of
	126 ->
	    <<Len:16, MaskingKey:4/binary, Payload:Len/binary>> = Rest;
	127 ->
	    <<Len:64, MaskingKey:4/binary, Payload:Len/binary>> = Rest;
	Len ->
	    <<_:0, MaskingKey:4/binary, Payload:Len/binary>> = Rest
    end,
    UnmaskedData = mask(1, binary_to_list(MaskingKey), Payload),

    {ok, list_to_binary(UnmaskedData), <<>>}; %jdtodo baaaaad

%% This should take care of all the Data Framing scenarios specified in
%% http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol-66#page-26
unframe_one(hi_75, DataFrames) -> unframe_one(hi_76, DataFrames);
unframe_one(hi_76, DataFrames) ->
    <<Type, _/bitstring>> = DataFrames,
    case Type of
	T when (T =< 127) ->
	    %% {ok, FF_Ended_Frame} = re:compile("^.(.*)\\xFF(.*?)", [ungreedy]),
	    FF_Ended_Frame = {re_pattern,2,0,
			      <<69,82,67,80,71,0,0,0,16,2,0,0,5,0,0,0,2,0,0,0,
                                0,0,255,2,40,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,93,0,
                                27,25,12,94,0,7,0,1,57,12,84,0,7,27,255,94,0,7,
                                0,2,56,12,84,0,7,84,0,27,0>>},
	    {match, [Data, NextFrame]} =
		re:run(DataFrames, FF_Ended_Frame,
		       [{capture, all_but_first, binary}]),
	    {ok, Data, NextFrame};

	_ -> %% Type band 16#80 =:= 16#80
	    {Length, LenBytes} = unpack_length(DataFrames, 0, 0),
	    <<_, _:LenBytes/bytes, Data:Length/bytes,
              NextFrame/bitstring>> = DataFrames,
	    {ok, Data, NextFrame}
    end.

unframe_all(_, <<>>, Acc) ->
    lists:reverse(Acc);
unframe_all(ProtocolVersion, DataFramesBin, Acc) ->
    {ok, Msg, Rem} = unframe_one(ProtocolVersion, DataFramesBin),
    unframe_all(ProtocolVersion, Rem, [Msg|Acc]).

frame(hi_75, IoList) ->
    frame(hi_76, IoList);
frame(hi_76, IoList) ->
    [0, IoList, 255];
frame(_, Data) ->
    %FIN=true because we're not fragmenting.
    %OPCODE=1 for text
    FirstByte = 128 bor 1,
    ByteList = binary_to_list(Data),
    Length = length(ByteList),
    if
	Length =< 126 ->
	    << FirstByte, 0:1, Length:7, Data:Length/binary >>;
	Length =< 65535 ->
	    << FirstByte, 0:1, 126:7, Length:16, Data:Length/binary >>;
	true ->
	    Defined = Length =< math:pow(2,64),
	    % TODO: Is the correctness of this pow call 
	    % better than the speed and danger of not checking?
	    case Defined of
		true ->
		    << FirstByte, 0:1, 127:7, Length:16, Data:Length/binary >>;
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

secret_key(KeyName, Headers) ->
    case query_header(KeyName, Headers) of
	undefined ->
	    0;
	Key ->
	    Digits = lists:filter(fun(C) -> C >= 48 andalso C =< 57 end, Key),
	    NumberOfSpaces = length(lists:filter(fun(C) -> C == 32 end, Key)),
	    list_to_integer(Digits) div NumberOfSpaces
    end.

challenge(Key1, Key2, Challenge) ->
    erlang:md5(digits32(Key1) ++ digits32(Key2) ++ Challenge).

digits32(Num) ->
    Digit4 = Num rem 256,
    Num2 = Num div 256,
    Digit3 = Num2 rem 256,
    Num3 = Num2 div 256,
    Digit2 = Num3 rem 256,
    Digit1 = Num3 div 256,
    [Digit1, Digit2, Digit3, Digit4].

hash_nonce(Nonce) ->
    Salted = Nonce ++ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11",
    HashBin = crypto:sha(Salted),
    base64:encode_to_string(HashBin).

unpack_length(Binary, LenBytes, Length) ->
    <<_, _:LenBytes/bytes, B, _/bitstring>> = Binary,
    B_v = B band 16#7F,
    NewLength = (Length * 128) + B_v,
    case B band 16#80 of
	16#80 ->
	    unpack_length(Binary, LenBytes + 1, NewLength);
	0 ->
	    {NewLength, LenBytes + 1}
    end.
