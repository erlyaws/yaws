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
		r_req,  %% if'we're server, what req method are we processing
		r_host, %% and value of Host: for the cli request
		state}).%% various depending on mode, ......




init(CliSock, ARG, DecPath, QueryPart, {Prefix, URL}, N) ->
    GC=get(gc), SC=get(sc),
    case connect_url(URL) of
	{ok, Ssock} ->
	    Headers0 = ARG#arg.headers,
	    Cli = sockmode(Headers0, ARG#arg.req,
			   #psock{s = CliSock, prefix = Prefix,
				  url = URL, type = client}),
	    ?Debug("CLI: ~p~n",[?format_record(Cli, psock)]),
	    Headers = rewrite_headers(Cli, Headers0),
	    ReqStr = yaws_api:reformat_request(
		       rewrite_path(ARG#arg.req, Prefix)),
	    Hstr = headers_to_str(Headers),
	    yaws:gen_tcp_send(Ssock, [ReqStr, "\r\n", Hstr, "\r\n"]),
	    if
		N /= 0 ->
		    yaws:gen_tcp_send(Ssock,ARG#arg.clidata);
		true ->
		    ok
	    end,
	    Srv = #psock{s = Ssock, 
			 prefix = Prefix, 
			 url = URL,
			 r_req = (ARG#arg.req)#http_request.method,
			 r_host = (ARG#arg.headers)#headers.host,
			 mode = expectheaders, type = server},
	    
	    %% Now we _must_ spawn a process here, because we
	    %% can't use {active, once} due to the inefficencies
	    %% that would occur with chunked encodings
	    P1 = proc_lib:spawn_link(?MODULE, ploop, [Cli, Srv, GC, SC]),
	    ?Debug("Client=~p, Srv=~p", [P1, self()]),
	    ploop(Srv, Cli, GC, SC);
	ERR ->
	    yaws:outh_set_dyn_headers(ARG#arg.req, ARG#arg.headers,
				      #urltype{}),
	    yaws_server:deliver_dyn_part(
	      CliSock,  
	      0, "404",
	      0,
	      ARG, no_UT_defined,
	      fun(A)->(SC#sconf.errormod_404):out404(A,get(gc),get(sc)) 
	      end,
	      fun()->yaws_server:finish_up_dyn_file(ARG, CliSock)
	      end
	     )
    end.



headers_to_str(Headers) ->
    lists:map(
      fun(H) -> [H, "\r\n"] end,
      yaws_api:reformat_header(Headers)).


connect_url(URL) ->
    Port = if
	       URL#url.port == undefined -> 80;
	       true -> URL#url.port
	   end,
    gen_tcp:connect(URL#url.host, Port, [{active, false},
					 {packet, http}]).



rewrite_path(Req, Pref) ->
    {abs_path, P} = Req#http_request.path,
    New = Req#http_request{path = {abs_path,strip_prefix(P, Pref)}},
    ?Debug("Rewritten path=~p~nP=~p Pref=~p", [New,P,Pref]),
    New.



strip_prefix(P,"/") ->
    P;
strip_prefix([H|T1],[H|T2]) ->
    strip_prefix(T1,T2).




%% Once we have read the headers, what comes after
%% the headers, 
%% This is applicable both for cli and srv sockets

sockmode(H,Req,Psock) ->
    case Psock#psock.type of
	server ->
	    s_sockmode(H, Req, Psock);
	client ->
	    cont_len_check(H,Psock)
    end.

s_sockmode(H,Resp,Psock) ->
    if Psock#psock.r_req == 'HEAD' ->
	    %% we're replying to a HEAD
	    %% no body
	    Psock#psock{mode = expectheaders, state = undefined};
	
	true ->
	    case lists:member(Resp#http_response.status, 
			      [100,204,205,304,406]) of
		true ->
		    %% no body, illegal
		    Psock#psock{mode = expectheaders, state = undefined};
		false ->
		    cont_len_check(H, Psock)
	    end
    end.


cont_len_check(H,Psock) ->
    case H#headers.content_length of
	undefined ->
	    case H#headers.transfer_encoding of
		"chunked" ->
		    Psock#psock{mode = expectchunked, state = init};
		undefined when Psock#psock.type == client ->
		    Psock#psock{mode = expectheaders, state = undefined};
		undefined when Psock#psock.type == server ->
		    Psock#psock{mode = undefined, state = undefined}
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



ploop(From0, To, GC, SC) ->
    put(sc, SC),
    put(gc, GC),
    ploop(From0, To).


ploop(From0, To) ->
    From = receive
	       {cli2srv, Method, Host} ->
		   From0#psock{r_req = Method,
			       r_host = Host}
	   after 0 ->
		   From0
	   end,
    set_sock_mode(From),
    TS = To#psock.s,
    case From#psock.mode of
	expectheaders ->
	    SSL = nossl,
	    case yaws:http_get_headers(From#psock.s, SSL) of
		{R, H0} ->
		    ?Debug("R = ~p~n",[R]),
		    RStr = 
			if
			    %% FIXME handle bad_request here
			    record(R, http_response) ->
				yaws_api:reformat_response(R);
			    record(R, http_request) ->
				To ! {cli2srv, R#http_request.method, 
				      H0#headers.host},
				yaws_api:reformat_request(
				  rewrite_path(R, From#psock.prefix))
			end,
		    Hstr = headers_to_str(H = rewrite_headers(From, H0)),
		    yaws:gen_tcp_send(TS, [RStr, "\r\n", Hstr]),
		    From2 = sockmode(H, R, From),
		    if
			From2#psock.mode == expectchunked ->
			    true;
			true ->
			    yaws:gen_tcp_send(TS,"\r\n")
		    end,
		    ploop(From2, To);
		closed ->
		    done
	    end;
	expectchunked ->
	    %% read the chunk number, we're in line mode
	    N = get_chunk_num(From#psock.s),
	    if N == 0 ->
		    ok=eat_crnl(From#psock.s),
		    yaws:gen_tcp_send(TS,["\r\n0\r\n\r\n"]),
		    ?Debug("SEND final 0 ",[]),
		    ploop(From#psock{mode = expectheaders, 
				     state = undefined},To);
	       true ->
		    ploop(From#psock{mode = chunk, 
				    state = N},To)
	    end;
	chunk ->
	    CG = get_chunk(From#psock.s,From#psock.state, 0),
	    SZ = From#psock.state,
	    Data2 = ["\r\n", yaws:integer_to_hex(SZ),"\r\n", CG],
	    yaws:gen_tcp_send(TS, Data2),
	    ploop(From#psock{mode = expect_nl_before_chunked,
			     state = undefined}, To);
	expect_nl_before_chunked ->
	    case gen_tcp:recv(From#psock.s, 0, 20000) of
		{ok, <<13,10>>} ->
		    yaws:gen_tcp_send(TS,<<13,10>>),
		    ploop(From#psock{mode = expectchunked,
				     state = undefined}, To);
		Other ->
		    exit(normal)
	    end;
	len when From#psock.state == 0 ->
	    ploop(From#psock{mode = expectheaders,
			     state = undefined},To);
	len ->
	    case gen_tcp:recv(From#psock.s, From#psock.state, ?READ_TIMEOUT) of
		{ok, Bin} ->
		    SZ = size(Bin),
                    ?Debug("Read ~p bytes~n", [SZ]),
		    yaws:gen_tcp_send(TS, Bin),
		    ploop(From#psock{state = From#psock.state - SZ},
			  To);
		Rsn ->
                    ?Debug("Failed to read :~p~n", [Rsn]),  
		    exit(normal)
	    end;
	undefined ->
	    case gen_tcp:recv(From#psock.s, From#psock.state, ?READ_TIMEOUT) of
		{ok, Bin} ->
		    yaws:gen_tcp_send(TS, Bin),
		    ploop(From, To);
		_ ->
		    exit(normal)
	    end
    end.


%% On the way from the client to the server, we need
%% rewrite the Host header

rewrite_headers(PS, H) when PS#psock.type == client ->
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

    ?Debug("New Host hdr: ~p~n", [Host]),
    H#headers{host = Host};



%% And on the way from the server to the client we 
%% need to rewrite the Location header, and the
%% Set-Cookie header

rewrite_headers(PS, H) when PS#psock.type == server ->
    ?Debug("Location header to rewrite:  ~p~n", [H#headers.location]),
    Loc = if
	      H#headers.location == undefined ->
		  undefined;
	      true ->
		  ?Debug("parse_url(~p)~n", [H#headers.location]),
		  LocUrl = (catch yaws_api:parse_url(H#headers.location)),
		  ProxyUrl = PS#psock.url,
		  if
		      LocUrl#url.host == ProxyUrl#url.host,
		      LocUrl#url.port == ProxyUrl#url.port,
		      LocUrl#url.scheme == ProxyUrl#url.scheme ->
			  rewrite_loc_url(LocUrl, PS);
		      
		      element(1, LocUrl) == 'EXIT' ->
			  rewrite_loc_rel(PS, H#headers.location);
		      true ->
			  ?Debug("Not rew ~p~n~p~n", 
			      [LocUrl, ProxyUrl]),
			  H#headers.location
		  end
	  end,
    ?Debug("New loc=~p~n", [Loc]),

    %% And we also should do cookies here ... FIXME

    H#headers{location = Loc}.


%% Rewrite a properly formatted location redir
rewrite_loc_url(LocUrl, PS) ->
    SC=get(sc),
    Scheme = yaws_server:redirect_scheme(SC),
    RedirHost = yaws_server:redirect_host(SC, PS#psock.r_host),
    RealPath = LocUrl#url.path,
    [Scheme, RedirHost, yaws:slash_append(PS#psock.prefix, LocUrl#url.path)].


%% This is the case for broken webservers that reply with
%% Location: /path
%% or even worse, Location: path

rewrite_loc_rel(PS, Loc) ->
    SC=get(sc),
    Scheme = yaws_server:redirect_scheme(SC),
    RedirHost = yaws_server:redirect_host(SC, PS#psock.r_host),
    [Scheme, RedirHost,Loc].



eat_crnl(Fd) ->
    inet:setopts(Fd, [{packet, line}]),
    case gen_tcp:recv(Fd,0, ?READ_TIMEOUT) of
	{ok, <<13,10>>} ->
	    ok;
	{ok, [13,10]} ->
	    ok;
	Err ->
	    {error, Err}
    end.

    
	




	


	







