%%% File    : httpc.erl
%%% Author  :  <klacke@hyber.org>
%%% Description : Basic 1.0 HTTP(S) client
%%% Created : 23 Jan 2005 by  <klacke@hyber.org>

-module(httpc).
-include("../../include/yaws_api.hrl").
-export([request/4, get_line/1]).

%% dialyzer ???
-export([recv_resp/2]).

request(Request, URL, ExtraHeaders, Body) ->
    case catch (request3(Request, URL, ExtraHeaders, Body)) of
	{'EXIT', Reason} ->
	    {error, Reason};
	RES ->
	    RES
    end.

request3(Request, URL, ExtraHeaders, Body) ->
    U = yaws_api:parse_url(URL),
    case cnct(U) of
	{ok, Socket} ->
	    BodyBin = list_to_binary(Body),
	    Size = size(BodyBin),
	    Mod = case U#url.scheme of
		      https -> ssl;
		      http -> gen_tcp
		  end,
	    Head = 
		io_lib:format(
		  "~s ~s~s HTTP/1.0\r\n"
		  "Host: ~s\r\n"
		  "~s"
		  "~s"
		  "\r\n",
		  [Request,
		   U#url.path,
		   if U#url.querypart == [] ->
			   [];
		      true ->
			   [$?|U#url.querypart]
		   end,
		   U#url.host,
		   ExtraHeaders,
		   if
		       Size == 0 ->
			   [];
		       true ->
			   io_lib:format("Content-Length: ~w\r\n",[Size])
		   end]),
	    case Mod:send(Socket, [Head, BodyBin]) of
		ok ->
		    %recv_resp(Mod, Socket);
		    Res = read_all_and_parse(Mod, Socket, []),
		    Mod:close(Socket),
		    Res;
		Err ->
		    Mod:close(Socket),
		    Err
	    end;
	Err ->
	    Err
    end.


cnct(U) ->
    case U#url.scheme of
	http ->
	    gen_tcp:connect(U#url.host,
			    case U#url.port of
				undefined ->
				     80;
				Port ->
				    Port
			    end,
			    [{packet, 0},
			     {active, false},
			     {reuseaddr, true}]);
	https ->
	    ssl:connect(U#url.host,
			    case U#url.port of
				undefined ->
				     443;
				Port ->
				    Port
			    end,
			    [{packet, 0},
			     {active, false}
			    ])
    end.



read_all_and_parse(Mod, Socket, Ack) ->
    case Mod:recv(Socket, 0) of
	{ok, Data} ->
	    read_all_and_parse(Mod, Socket, Ack ++ Data);
	{error, closed} ->
	    parse_reply(Ack);
	Err ->
	    error_logger:format("Error ~p while reading http reply\n",
                                [Err]),
	    Err
    end.

parse_reply(Reply) ->
    case get_line(Reply) of
	{line, Line, Trail} ->
	    case string:tokens(Line, " ") of
		[_HTTP, "200" | _] ->
		    parse_reply_headers(Trail);
		_Other ->
		    error_logger:format("Got non 200 reply ~p~n",[Line]),
		    {error, {status, Line}}
	    end;
	Other ->
	    error_logger:format("Got bad status line ~n",[]),
	    {error, {statusline, Other, Reply}}
    end.

%% Just eat all headers
parse_reply_headers(Data) ->
    case get_line(Data) of
	{line, _LIne, Trail} ->
	    parse_reply_headers(Trail);
	{lastline, _Line, Trail} ->
	    {ok, 200, Trail};
	Other ->
	    error_logger:format("God bad header \n",[]),
	    {error, {header, Other, Data}}
    end.




recv_resp(Mod, S) ->
    %% First we need to get the Resp header
    case get_head(Mod, S, []) of
	{ok, Code, Trail} ->
	    case get_headers(Mod, S, Trail, undefined) of
		{ok, ContLen, Trail2} ->
		    if
			ContLen == undefined ->
			    %% Read until EOF
			    read_all(Code, Mod, S, Trail2);
			true ->
			    Sofar = length(Trail2),
			    read_int(Code, Mod, S, ContLen - Sofar, Trail2)
		    end;
		Err ->
		    error_logger:format("get_headers: ~p~n", [Err]),
		    Err
	    end;
	Err ->
	    error_logger:format("get_head: ~p~n", [Err]),
	    Err
    end.

read_all(Code, Mod, S, Ack) ->
    case Mod:recv(S,0) of
	{ok, Data} ->
	    read_all(Code, Mod, S, Ack ++ Data);
	{error, closed } ->
	    {ok, Code, Ack};
	Err ->
	    Err
    end.

read_int(Code, Mod, S, Num,  Ack) ->
    if
	Num == 0 ->
	    {ok, Code, Ack};
	true ->
	    case Mod:recv(S, Num) of
		{ok, Data} ->
		    JustGot = length(Data),
		    read_int(Code, Mod, S, Num - JustGot, Ack ++ Data);
		{error, closed} ->
		    {ok, Code, Ack};
		Err ->
		    Err
	    end
    end.


get_headers(Mod, S, Trail, ContLen) ->
    case get_line(Trail, []) of
	{lastline, Line, Trail2} ->
	    {ok, clen(ContLen, Line), Trail2};
	{line, Line, Trail2} ->
	    get_headers(Mod, S, Trail2, clen(ContLen, Line));
	need_more ->
	    case Mod:recv(S, 0) of
		{ok, More} ->
		    get_headers(Mod, S, Trail ++ More, ContLen);
		Err ->
		    Err
	    end
    end.

clen(_Old, "Content-Length:"++Con) ->
    list_to_integer(string:strip(Con,both,$ ));
clen(_Old, "Content-length:"++Con) ->
    list_to_integer(string:strip(Con,both,$ ));
clen(Old, _) ->
    Old.

   


get_head(Mod, S, Prev) ->
    case Mod:recv(S, 0) of
	{ok, Data0} ->
	    Data = Prev ++ Data0,
	    case get_line(Data) of
		{line, Line, Trail} ->
		    case string:tokens(Line, " ") of
			[_HTTP, RetCode | _] ->
			    {ok, list_to_integer(RetCode), Trail};
			_ ->
			    {error, Line}
		    end;
		need_more ->
		    get_head(Mod, S, Data);
		_ ->
		    {error, Data}
	    end;
	Err ->
	    Err
    end.
    




is_nb_space(X) ->
    lists:member(X, [$\s, $\t]).
    

% ret: {line, Line, Trail} | {lastline, Line, Trail}

get_line(L) ->    
    get_line(L, []).
get_line("\r\n\r\n" ++ Tail, Cur) ->
    {lastline, lists:reverse(Cur), Tail};
get_line("\r\n" ++ Tail, Cur) when Tail /= []  ->
    case is_nb_space(hd(Tail)) of
	true ->  %% multiline ... continue 
	    get_line(Tail, [$\n, $\r | Cur]);
	false ->
	    {line, lists:reverse(Cur), Tail}
    end;
get_line("\r\n", Cur)   ->
     {line, lists:reverse(Cur), []};
get_line([H|T], Cur) ->
    get_line(T, [H|Cur]);
get_line([], _) ->
    need_more.
		
		     








