%%%-------------------------------------------------------------------
%%% File    : yaws_revproxy.erl
%%% Author  :  <klacke@hyber.org>
%%% Description : reverse proxy
%%%
%%% Created :  3 Dec 2003 by  <klacke@hyber.org>
%%%-------------------------------------------------------------------
-module(yaws_revproxy).
-compile(export_all).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include("yaws_debug.hrl").


%% reverse proxy implementation.



	
-record(psock, {s,      %% the socket
		mode,   %% chunked,len,undefined,expectheaders,expectchunked
		prefix, %% The prefix to strip and add
		url,    %% The url we're proxying to
		type,   %% client | server
		state}).%% various depending on mode, ......




init(CliSock, GC, SC, ARG, DecPath, QueryPart, {Prefix, URL}) ->
    Port = if
	       URL#url.port == undefined -> 80;
	       true -> URL#url.port
	   end,

    case gen_tcp:connect(URL#url.host, Port, [{active, false},
					      {packet, http}]) of
	{ok, Ssock} ->
	    ?Debug("Connected to proxy host ok ~p~n",[URL]),
	    Headers0 = ARG#arg.headers,
	    Cli = sockmode(Headers0, #psock{s = CliSock, prefix = Prefix,
					    url = URL, type = client}),
	    Headers = rewrite_headers(Cli, Headers0),
	    ReqStr = yaws_api:reformat_request(
		       rewrite_path(ARG#arg.req, Prefix)),
	    Hstr = lists:map(
		     fun(H) -> [H, "\r\n"] end,
		     yaws_api:reformat_header(Headers)),
	    yaws:gen_tcp_send(Ssock, [ReqStr, "\r\n", Hstr, "\r\n"], SC,GC),
	    Srv = #psock{s = Ssock, prefix = Prefix, url = URL,
			 mode = expectheaders, type = server},
	    
	    %% Now we _must_ spawn a process here, casuse we
	    %% can't use {active, once} due to the inefficencies
	    %% that would occur with chunked encodings
	    
	    P1 = proc_lib:spawn_link(?MODULE, ploop, [Cli, Srv, GC, SC]),
	    ?Debug("Client=~p, Srv=~p", [P1, self()]),
	    ploop(Srv, Cli, GC, SC);
	ERR ->
	    yaws:outh_set_dyn_headers(ARG#arg.req, ARG#arg.headers),
	    yaws_server:deliver_dyn_part(
	      CliSock, GC, SC, 
	      0, "404",
	      0,
	      ARG,
	      fun(A)->(SC#sconf.errormod_404):out404(A,GC,SC) 
	      end,
	      fun()->yaws_server:finish_up_dyn_file(CliSock, GC, SC)
	      end
	     )
    end.




rewrite_path(Req, Pref) ->
    {abs_path, P} = Req#http_request.path,
    New = Req#http_request{path = {abs_path,strip_prefix(P, Pref)}},
    ?Debug("Rewritten path=~p~nP=~p Pref=~p", [New,P,Pref]),
    New.


strip_prefix(P,[]) -> P;
strip_prefix("/","/") ->
    "/";
strip_prefix([H|T1],[H|T2]) ->
    strip_prefix(T1,T2).


%% Once we have read the headers, what comes after
%% the headers, 
%% This is applicable both for cli and srv sockets


sockmode(H,Psock) ->
    case H#headers.content_length of
	undefined ->
	    case H#headers.transfer_encoding of
		"chunked" ->
		    Psock#psock{mode = expectchunked, state = init};
		undefined ->
		    Psock#psock{mode = expectheaders, state = undefined}
	    end;
	Int when integer(Int) ->
	    Psock#psock{mode = len,
			state = Int};
	List when list(List) ->
	    Psock#psock{mode = len,
			state = list_to_integer(List)}
    
    end.

set_sock_mode(PS) ->
    S = PS#psock.s,
    case PS#psock.mode of
	expectheaders ->
	    inet:setopts(S, [{packet, http}]);
	expectchunked ->
	    inet:setopts(S, [binary, {packet, line}]);
	expect_nl_before_chunked ->
	    inet:setopts(S, [binary, {packet, line}]);
	_ ->
	    inet:setopts(S, [binary, {packet, raw}])
    end.


get_chunk_num(Fd) ->
    case gen_tcp:recv(Fd, 0) of
	{ok, Line} ->
	    ?Debug("Get chunk num from line ~p~n",[Line]),
	    erlang:list_to_integer(nonl(Line),16);
	{error, Rsn} ->
	    exit(normal)
    end.

nonl(B) when binary(B) ->
    nonl(binary_to_list(B));
nonl([10|T]) ->
    nonl(T);
nonl([13|T]) ->
    nonl(T);
nonl([H|T]) ->
    [H|nonl(T)];
nonl([]) ->
    [].
	    


get_chunk(Fd, N, N) ->
    [];
get_chunk(Fd, N, Asz) ->
    case gen_tcp:recv(Fd, N, 20000) of
	{ok, Bin} ->
	    SZ = size(Bin),
	    [Bin|get_chunk(Fd, N, SZ+Asz)];
	_ ->
	    exit(normal)
    end.



ploop(From, To, GC, SC) ->
    set_sock_mode(From),
    TS = To#psock.s,
    case From#psock.mode of
	expectheaders ->
	    SSL = nossl,
	    case yaws:http_get_headers(From#psock.s, GC, SSL) of
		{R, H0} ->
		    ?Debug("GOT proxy hdrs: ~p~n", [H0]),
		    RStr = 
			if
			    %% FIXME handle bad_request here
			    record(R, http_response) ->
				yaws_api:reformat_response(R);
			    record(R, http_request) ->
				yaws_api:reformat_request(
				  rewrite_path(R, From#psock.prefix))
			end,
		    H = rewrite_headers(From, H0),
		    Hstr = lists:map(
			     fun(Hh) -> [Hh, "\r\n"] end,
			     yaws_api:reformat_header(H)),
		    yaws:gen_tcp_send(TS, [RStr, "\r\n", Hstr, "\r\n"] ,
				      SC,GC),
		    From2 = sockmode(H, From),
		    ploop(From2, To, GC,SC);
		closed ->
		    done
	    end;
	expectchunked ->
	    %% read the chunk number, we're in line mode
	    N = get_chunk_num(From#psock.s),
	    if N == 0 ->
		    ok=eat_crnl(From#psock.s),
		    ploop(From#psock{mode = expectheaders, 
				     state = undefined},To, GC, SC);
	       true ->
		    ploop(From#psock{mode = chunk, 
				    state = N},To, GC, SC)
	    end;
	chunk ->
	    CG = get_chunk(From#psock.s,From#psock.state, 0),
	    SZ = From#psock.state,
	    Data2 = ["\r\n", yaws:integer_to_hex(SZ) , 
		     "\r\n", CG],
	    yaws:gen_tcp_send(TS, Data2, SC, GC),
	    ploop(From#psock{mode = expect_nl_before_chunked,
			     state = undefined}, To, GC,SC);
	expect_nl_before_chunked ->
	    case gen_tcp:recv(From#psock.s, 0, 20000) of
		{ok, <<13,10>>} ->
		    yaws:gen_tcp_send(TS,<<13,10>>,SC, GC),
		    ploop(From#psock{mode = expectchunked,
				     state = undefined}, To, GC,SC);
		Other ->
		    exit(normal)
	    end;
	len when From#psock.state == 0 ->
	    ploop(From#psock{mode = expectheaders,
			     state = undefined},To, GC,SC);
	len ->
	    case gen_tcp:recv(From#psock.s, From#psock.state, 20000) of
		{ok, Bin} ->
		    SZ = size(Bin),
		    yaws:gen_tcp_send(TS, Bin, SC, GC),
		    ploop(From#psock{state = From#psock.state - SZ},
			  To, GC,SC);
		_ ->
		    exit(normal)
	    end;
	undefined ->
	    case gen_tcp:recv(From#psock.s, From#psock.state, 20000) of
		{ok, Bin} ->
		    yaws:gen_tcp_send(TS, Bin, SC, GC),
		    ploop(From, To, GC,SC);
		_ ->
		    exit(normal)
	    end
    end.



rewrite_headers(PS, H) when PS#psock.type == client ->
    %% On the way from the client to the server, we need
    %% rewrite the Host header
    Host =
	if H#headers.host == undefined ->
		undefined;
	   true ->
		U = PS#psock.url,
		[U#url.host,
		 if
		     U#url.port == undefined ->
			 [];
		     true ->
			 [$: | integer_to_list(U#url.port)]
		 end]
	end,
    %% FIXME, do cookies
    ?Debug("New Host hdr: ~p~n", [Host]),
    H#headers{host = Host};


rewrite_headers(PS, H) when PS#psock.type == server ->
    ?Debug("HHH ~p~n", [H#headers.location]),
    Loc = if
	      H#headers.location == undefined ->
		  undefined;
	      true ->
		  ?Debug("parse_url(~p)~n", [H#headers.location]),
		  LocUrl = yaws_api:parse_url(H#headers.location),
		  ProxyUrl = PS#psock.url,
		  if
		      LocUrl#url.host == ProxyUrl#url.host,
		      LocUrl#url.port == ProxyUrl#url.port,
		      LocUrl#url.scheme == ProxyUrl#url.scheme ->
			  P = PS#psock.prefix ++  LocUrl#url.path,
			  ?Debug("New Loc = ~p~n", [P]),
			  yaws_api:reformat_url(LocUrl#url{path=P});
		      true ->
			  ?Debug("Not rew ~p~n~p~n", [LocUrl, ProxyUrl]),
			  H#headers.location
		  end
	  end,
    %% And we also should do cookies here ... FIXME
    H#headers{location = Loc}.



eat_crnl(Fd) ->
    inet:setopts(Fd, [{packet, line}]),
    case gen_tcp:recv(Fd,0, 20000) of
	{ok, <<13,10>>} ->
	    ok;
	{ok, [13,10]} ->
	    ok;
	Err ->
	    {error, Err}
    end.

    
	




	


	







