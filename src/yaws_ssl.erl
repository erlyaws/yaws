%%%----------------------------------------------------------------------
%%% File    : yaws_ssl.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 24 May 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_ssl).
-author('klacke@hyber.org').

-compile(export_all).
-include("yaws.hrl").
-include_lib("kernel/include/file.hrl").




%% Boring code, we need to parse HTTP here since 
%% the standard ssl mode in erlang doesn't have tonys
%% funcky http mode parsing .... and I dont have the 
%% energy to add it there :-(

ssl_get_headers(CliSock, GC) ->
    case yaws_server:cli_recv(CliSock, 1024, GC, ssl) of
	{ok, Data0} ->
	    Data = binary_to_list(Data0),
	    ?Debug("GOT ssl data ~p~n", [Data]),
	    case get_req(Data) of
		{ok, R, Trail} ->
		    case get_headers(CliSock, GC,  #headers{}, Trail) of
			{ok, H, Trail2} ->
			    {ok, R, H, list_to_binary(Trail2)};
			Err ->
			    Err
		    end;
		Err ->
		    Err
	    end;
	_Err ->
	    ?Debug("cli recv ret ~p~n", [_Err]),
	    done
    end.


get_req("GET " ++ Tail) ->
    get_req(skip_space(Tail), #http_request{method = 'GET'});

get_req("POST " ++ Tail) ->
    get_req(skip_space(Tail), #http_request{method = 'POST'});

get_req("HEAD " ++ Tail) ->
    get_req(skip_space(Tail), #http_request{method = 'HEAD'});

get_req("TRACE " ++ Tail) ->
    get_req(skip_space(Tail), #http_request{method = 'TRACE'});
get_req([]) ->
    done.


get_req(Line, R) ->
    ?Debug("Line = ~p~n", [Line]),
    {[Url, Vers], Trail} = spacesplit(Line, 2,  [], []),
    R2 = R#http_request{path = {abs_path, Url},
			version = parse_version(Vers)},
    {ok, R2, Trail}.


get_headers(CliSock, GC, H, Tail) ->
    case yaws_api:get_line(Tail) of
	{line, Line, Tail2} ->
	    get_headers(CliSock, GC, parse_line(Line, H), Tail2);
	{lastline, Line, Tail2} ->
	    {ok, parse_line(Line, H), Tail2}
    end.


parse_line("Connection:" ++ Con, H) ->
    H#headers{connection = space_strip(Con)};
parse_line("Date:" ++ _Con, H) ->
    %%H#headers{date = Con};
    H;
parse_line("Host:" ++ Con, H) ->
    H#headers{host = space_strip(Con)};
parse_line("Authorization" ++ Con, H) ->
    H#headers{authorization = yaws_server:parse_auth(space_strip(Con))};
parse_line(_, H) ->
    H.



%% FIXME many more needed here .....


%% strip all space from a string
space_strip(L) ->
    space_strip(L, head).
space_strip([H|T], Mode) ->
    case yaws:is_space(H) of
	true when Mode == head ->
	    space_strip(T, Mode);
	true when Mode == tail ->
	    [];
	false -> 
	[H| space_strip(T, Mode)]
    end;
space_strip([], _) ->
    [].





parse_version("HTTP/1.1") ->
    {1,1};
parse_version("HTTP/1.0") ->
    {1,0}.


spacesplit(Line, 0, Ack, _Cur) ->
    {lists:reverse(Ack), skip_space(Line)};
spacesplit([H|T], Num, Ack, Cur) ->
    case yaws:is_space(H) of
	true when Cur == [] ->
	    spacesplit(T, Num, Ack, Cur);
	true ->
	    spacesplit(T, Num-1, [lists:reverse(Cur) | Ack], []);
	false ->
	    spacesplit(T, Num, Ack, [H|Cur])
    end.


skip_space(L) ->
    lists:dropwhile(fun(X) -> yaws:is_space(X) end, L).
