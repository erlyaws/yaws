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
-export([handshake/3, unframe_one/1, unframe_all/2]).

handshake(Arg, ContentPid, SocketMode) ->
    CliSock = Arg#arg.clisock,
    case get_origin_header(Arg#arg.headers) of
	undefined ->
	    %% Yaws will take care of closing the socket
	    ContentPid ! discard;
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
		    inet:setopts(CliSock, [{packet, raw}, {active, SocketMode}]),
		    TakeOverResult =
			gen_tcp:controlling_process(CliSock, ContentPid);
		_ ->
		    ssl:send(CliSock, Handshake),
		    ssl:setopts(CliSock, [{packet, raw}, {active, SocketMode}]),
		    TakeOverResult =
			ssl:controlling_process(CliSock, ContentPid)
	    end,
	    case TakeOverResult of
		ok ->
		    ContentPid ! {ok, CliSock};
		{error, Reason} ->
		    ContentPid ! discard,
		    exit({websocket, Reason})
	    end
    end,
    exit(normal).

handshake(ws_76, Arg, CliSock, WebSocketLocation, Origin, Protocol) ->
    {ok, Challenge} = gen_tcp:recv(CliSock, 8),
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

handshake(ws_75, _Arg, _CliSock, WebSocketLocation, Origin, _Protocol) ->
    ["HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
     "Upgrade: WebSocket\r\n",
     "Connection: Upgrade\r\n",
     "WebSocket-Origin: ", Origin, "\r\n",
     "WebSocket-Location: ", WebSocketLocation, "\r\n",
     "\r\n"].

ws_version(Headers) ->
    case query_header("sec-websocket-key1", Headers) of
	undefined ->  ws_75;
	_         ->  ws_76
    end.

%% This should take care of all the Data Framing scenarios specified in
%% http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol-66#page-26
unframe_one(DataFrames) ->
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

unframe_all(<<>>, Acc) ->
    lists:reverse(Acc);
unframe_all(DataFramesBin, Acc) ->
    {ok, Msg, Rem} = unframe_one(DataFramesBin),
    unframe_all(Rem, [Msg|Acc]).


%% Internal functions
get_origin_header(Headers) ->
    query_header("origin", Headers).

get_protocol_header(Headers) ->
    query_header("sec-websocket-protocol", Headers, "unknown").

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
