%%%----------------------------------------------------------------------
%%% File    : yaws_websockets.erl
%%% Author  : Davide Marquês <nesrait@gmail.com>
%%% Purpose : 
%%% Created :  18 Dec 2009 by Davide Marquês <nesrait@gmail.com>
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
	    Host = (Arg#arg.headers)#headers.host,
	    {abs_path, Path} = (Arg#arg.req)#http_request.path,
	    %% TODO: Support for wss://
	    WebSocketLocation = "ws://" ++ Host ++ Path,
	    Handshake =
		["HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
		 "Upgrade: WebSocket\r\n",
		 "Connection: Upgrade\r\n",
		 "WebSocket-Origin: ", Origin, "\r\n",
		 "WebSocket-Location: ", WebSocketLocation, "\r\n",
		 "\r\n"],
	    SC = get(sc),
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


%% This should take care of all the Data Framing scenarios specified in
%% http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol-66#page-26
unframe_one(DataFrames) ->
    <<Type, _/bitstring>> = DataFrames,
    case Type of
	T when (T =< 127) ->
	    %% {ok, FF_Ended_Frame} = re:compile("^.(.*)\\xFF(.*?)", [ungreedy]),
	    FF_Ended_Frame = {re_pattern,2,0,
			      <<69,82,67,80,71,0,0,0,16,2,0,0,5,0,0,0,2,0,0,0,0,0,255,2,40,
			       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,93,0,27,25,12,94,0,7,0,1,57,
			       12,84,0,7,27,255,94,0,7,0,2,56,12,84,0,7,84,0,27,0>>},
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
get_origin_header(#headers{other=L}) ->
    lists:foldl(fun({http_header,_,K0,_,V}, undefined) ->
                        K = case is_atom(K0) of
                                true ->
                                    atom_to_list(K0);
                                false ->
                                    K0
                            end,
                        case string:to_lower(K) of
                            "origin" ->
                                V;
                            _ ->
                                undefined
                        end;
                   (_, Acc) ->
                        Acc
                end, undefined, L).

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

