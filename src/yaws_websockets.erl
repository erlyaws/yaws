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
-export([handshake/3, unframe/2, frame/2]).

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

buffer(Len, Buffered) ->
    case Buffered of 
	<<Payload:Len/binary>> ->
	    Payload;
	_ ->
	    receive
		{tcp, _Socket, More} ->
		    buffer(Len, <<Buffered/binary, More/binary>>)
	    end
    end.


unframe(8, DataFrames) ->
    debug(val, {"Frame bytes list length:",length(binary_to_list(DataFrames))}),
    <<1:1,_Rsv:3,_Opcode:4,1:1,Len1:7,Rest/binary>> = DataFrames,
    debug(val,{"Len",Len1}),
    case Len1 of
	126 ->
	    <<Len:16, MaskingKey:4/binary, Payload:Len/binary>> = Rest;
	127 ->
	    <<Len:64, Rest2/binary>> = Rest,
	    debug(val,{"Len",Len}),
	    <<MaskingKey:4/binary, Rest3/binary>> = Rest2,
	    debug(val, {"Payload bytes list length:",length(binary_to_list(Rest3))}),
	    Payload = buffer(Len, Rest3);
	Len ->
	    <<_:0, MaskingKey:4/binary, Payload:Len/binary>> = Rest
    end,
    UnmaskedData = mask(1, binary_to_list(MaskingKey), Payload),
    {ok, list_to_binary(UnmaskedData)}.

frame(8, Data) ->
    %FIN=true because we're not fragmenting.
    %OPCODE=1 for text
    FirstByte = 128 bor 1,
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
