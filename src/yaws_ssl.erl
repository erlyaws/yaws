%%%----------------------------------------------------------------------
%%% File    : yaws_ssl.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 24 May 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_ssl).
-author('klacke@hyber.org').

-export([ssl_get_headers/2]).


-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include("yaws_debug.hrl").

-include_lib("kernel/include/file.hrl").


%% Boring code, we need to parse HTTP here since 
%% the standard ssl mode in erlang doesn't have tonys
%% funcky http mode parsing .... and I dont have the 
%% energy to add it there :-(


%% FIXME:  Stupid code...
%%
%% To be more exact: Gets data until "\r\n\r\n" is received, but at
%% most Num bytes.  Returns data as a list.
%%
%% Should be replaced by something cleaner (and faster), but at least
%% it works for now (as long as the header is not longer than Num
%% bytes, that is).
%%
%% cschultz


contains_rnrn([]) ->
    false;
contains_rnrn("\r\n\r\n" ++ _) ->
    true;
contains_rnrn([X|XS]) ->
    contains_rnrn(XS).


recv(CliSock, Num, GC) ->    
    recv(CliSock, Num, GC, []).

recv(CliSock, 0, GC, D) -> {ok, D};
recv(CliSock, Num, GC, D) -> 
    case yaws_server:cli_recv(CliSock, Num, GC, ssl) of
	{ok, Data} ->
	    ?Debug("GOT chunk of size ~p.~n", [size(Data)]),
	    D2 = D ++ binary_to_list(Data),
	    case contains_rnrn(D2) of
		true -> {ok, D2};
		false ->
		    recv(CliSock, Num - size(Data), GC, D2)
	    end;
	{error, closed} -> 
	    ?Debug("No more chunk to get.~n", []),
	    {ok, D};
	E -> E
    end.
    

ssl_get_headers(CliSock, GC) ->
    case recv(CliSock, 2048, GC) of
	{ok, []} ->
	    closed;
	{ok, Data} ->
	    ?Debug("GOT ssl data ~p~n", [Data]),
	    {R, Trail} = get_req(Data),
	    ?Debug("Parsed request ~p~n", [R]),
            case R of
		bad_request ->
		    {#http_request{method=bad_request, version={0,9}},
		     #headers{},
						% Returning <<>> here
						% is wrong if we got
						% data after
						% "\r\n\r\n".
		     <<>>};
		_ ->
		    {H,Trail2} = get_headers(CliSock, GC,  #headers{}, Trail),
		    {R, H, list_to_binary(Trail2)}
	    end;
	_Err ->
	    ?Debug("cli recv ret ~p~n", [_Err]),
	    exit(normal)
    end.

get_req("\r\n\r\n" ++ _) ->
    bad_request;
get_req("\r\n" ++ Data) ->
    get_req(Data);
get_req(Data) ->
    {FirstLine, Trail} = lists:splitwith(fun not_eol/1, Data),
    R = parse_req(FirstLine),
    {R, Trail}.
	    

not_eol($\r)->
    false;
not_eol($\n) ->
    false;
not_eol(_) ->
    true.


get_word(Line)->
    {Word, T} = lists:splitwith(fun(X)-> X /= $\  end, Line),
    {Word, lists:dropwhile(fun(X) -> X == $\  end, T)}.


parse_req(Line) ->
    {MethodStr, L1} = get_word(Line),
    ?Debug("Method: ~p~n", [MethodStr]),
    case L1 of
	[] ->
	    bad_request;
	_ ->
	    {URI, L2} = get_word(L1),
	    {VersionStr, L3} = get_word(L2),
	    ?Debug("URI: ~p~nVersion: ~p~nL3: ~p~n",
		[URI, VersionStr, L3]),
	    case L3 of
		[] ->
		    R = #http_request{method=case MethodStr of
						 "GET" -> 'GET';
						 "POST" -> 'POST';
						 "HEAD" -> 'HEAD';
						 "OPTIONS" -> 'OPTIONS';
						 "TRACE" -> 'TRACE';
						 "PUT" -> 'PUT';
						 "DELETE" -> 'DELETE';
						 S -> S
					     end,
				      path = case URI of
						 "*" ->
						% Is this correct?
						     "*";
						 P ->
						% FIXME: Handle
						% absolute URIs
						     {abs_path, P}
					     end
				     },
		    case VersionStr of
			[] -> R#http_request{version={0,9}};
			"HTTP/1.0" ->
			    R#http_request{version={1,0}};
			"HTTP/1.1" ->
			    R#http_request{version={1,1}};
			_ ->
			    bad_request
		    end;
		_ ->
		    bad_request
	    end
    end.
			    
					
				       
				    
get_headers(CliSock, GC, H, Tail) ->
    case yaws_api:get_line(Tail) of
	{line, Line, Tail2} ->
	    get_headers(CliSock, GC, parse_line(Line, H), Tail2);
	{lastline, Line, Tail2} ->
	    {parse_line(Line, H), Tail2}
    end.


parse_line("Connection:" ++ Con, H) ->
    H#headers{connection = space_strip(Con)};
parse_line("Host:" ++ Con, H) ->
    H#headers{host = space_strip(Con)};
parse_line("Accept:" ++ Con, H) ->
    H#headers{accept = space_strip(Con)};
parse_line("If-Modified-Since:" ++ Con, H) ->
    H#headers{accept = space_strip(Con)};
parse_line("If-Match:" ++ Con, H) ->
    H#headers{if_match = space_strip(Con)};
parse_line("If-None-Match:" ++ Con, H) ->
    H#headers{if_none_match = space_strip(Con)};
parse_line("If-Range:" ++ Con, H) ->
    H#headers{if_range = space_strip(Con)};
parse_line("If-Unmodified-Since:" ++ Con, H) ->
    H#headers{if_unmodified_since = space_strip(Con)};
parse_line("Range:" ++ Con, H) ->
    H#headers{range = space_strip(Con)};
parse_line("User-Agent:" ++ Con, H) ->
    H#headers{user_agent = space_strip(Con)};
parse_line("Accept-Ranges:" ++ Con, H) ->
    H#headers{accept_ranges = space_strip(Con)};
parse_line("Authorization:" ++ Con, H) ->
    A = yaws_server:parse_auth(space_strip(Con)),
    H#headers{authorization = A};
parse_line("Keep-Alive:" ++ Con, H) ->
    H#headers{keep_alive = space_strip(Con)};
parse_line("Referer:" ++ Con, H) ->
    H#headers{referer = space_strip(Con)};
parse_line("Content-type:"++Con, H) ->
    H#headers{content_type = space_strip(Con)};
parse_line("Content-Type:"++Con, H) ->
    H#headers{content_type = space_strip(Con)};
parse_line("Content-Length:"++Con, H) ->
    H#headers{content_length = space_strip(Con)};
parse_line("Content-length:"++Con, H) ->
    H#headers{content_length = space_strip(Con)};
parse_line("Cookie:"++Con, H) ->
    H#headers{cookie = [space_strip(Con)|H#headers.cookie]};
parse_line(S, H) ->
    case lists:splitwith(fun(C)->C /= $: end, S) of
	{Name, [$:|Val]} ->
	    Other = H#headers.other,
	    H#headers{other=[{http_header, undefined, Name, undefined, 
			      space_strip(Val)}
			     |Other]};
	_ -> H
    end.



space_strip(S) ->
    yaws:strip_spaces(S, both).
