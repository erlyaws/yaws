%%%----------------------------------------------------------------------
%%% File    : yaws_server.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 16 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_server).
-author('klacke@hyber.org').

-compile(export_all).
%%-export([Function/Arity, ...]).

-behaviour(gen_server).
-include("yaws.hrl").
-include_lib("kernel/include/file.hrl").

%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-import(filename, [join/1]).
-import(lists, [foreach/2, map/2, flatten/1, flatmap/2]).


-record(gs, {gconf,
	     group,         %% list of #sconf{} s
	     ssl,           %% ssl | nossl
	     l,             %% listen socket
	     mnum = 0,      %% dyn compiled erl module  number
	     sessions = 0,  %% number of HTTP sessions
	     reqs = 0}).    %% number of HTTP requests




-record(urltype, {type,   %% error | yaws | regular | directory | dotdot
		  finfo,
		  path,
		  fullpath,
		  dir,     %% relative dir where the path leads to
		  data,    %% Binary | FileDescriptor | DirListing | undefined
		  mime = "text/html",    %% MIME type
		  q,       %% query for GET requests
		  wwwauth = undefined  %% or #auth{}
		 }).



%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, yaws_server}, yaws_server, [], []).

status() ->
    gen_server:call(?MODULE, status).

stats() -> 
    {S, Time} = status(),
    {_GC, Srvs, _} = S,
    Diff = calendar:time_difference(Time, calendar:local_time()),
    G = fun(L) -> lists:reverse(lists:keysort(2, L)) end,
    
    R= flatmap(
      fun({_Pid, SCS}) ->
	      map(
		fun(SC) ->
			
			E = SC#sconf.ets,
			L = ets:match(E, {{urlc_total, '$1'}, '$2'}),
			{SC#sconf.servername,  
			 flatten(yaws:fmt_ip(SC#sconf.listen)),
			 G(map(fun(P) -> list_to_tuple(P) end, L))}
		end, SCS)
      end, Srvs),
    {Diff, R}.

			      
get_app_args() ->
    AS=init:get_arguments(),
    Debug = case application:get_env(yaws, debug) of
		undefined ->
		    lists:member({yaws, ["debug"]}, AS);
		_ ->
		    true
	    end,
    Trace = case application:get_env(yaws, trace) of
		undefined ->
		    case {lists:member({yaws, ["trace", "http"]}, AS),
			  lists:member({yaws, ["trace", "traffic"]}, AS)} of
			{true, _} ->
			    {true, http};
			{_, true} ->
			    {true, traffic};
			_ ->
			    false
		    end;
		{ok, http} ->
		    {true, http};
		{ok, traffic} ->
		    {true, traffic};
		_ ->
		    false
	    end,
    Conf = case application:get_env(yaws, conf) of
		undefined ->
		   find_c(AS);
		{ok, File} ->
		    {file, File}
	    end,
    {Debug, Trace, Conf}.

find_c([{conf, [File]} |_]) ->
    {file, File};
find_c([_|T]) ->
    find_c(T);
find_c([]) ->
    false.




%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    put(start_time, calendar:local_time()),  %% for uptime
    {Debug, Trace, Conf} = get_app_args(),

    case yaws_config:load(Conf, Trace, Debug) of
	{ok, Gconf, Sconfs} ->
	    erase(logdir),
	    ?Debug("Conf = ~p~n", [?format_record(Gconf, gconf)]),
	    yaws_log:setdir(Gconf#gconf.logdir, Sconfs),
	    case Gconf#gconf.trace of
		{true, What} ->
		    yaws_log:open_trace(What);
		_ ->
		    ok
	    end,

	    init2(Gconf, Sconfs);
	{error, E} ->
	    error_logger:format("Bad conf: ~p", [E]),
	    case erase(logdir) of
		undefined ->
		    {stop, E};
		Dir ->
		    yaws_log:setdir(Dir),
		    yaws_log:sync_errlog("bad conf: ~s",[E]),
		    {stop, E}
	    end
    end.


init2(Gconf, Sconfs) ->
    lists:foreach(
      fun(D) ->
	      code:add_pathz(D)
      end, Gconf#gconf.ebin_dir),

    process_flag(trap_exit, true),

    %% start the individual server processes
    L = lists:map(
	  fun(Group) ->
		  proc_lib:start_link(?MODULE, gserv, [Gconf, Group])
	  end, Sconfs),
    L2 = lists:zf(fun({error, F, A}) ->
			  yaws_log:sync_errlog(F, A),
			  false;
		     ({_Pid, _SCs}) ->
			  true
		  end, L),
    if
	length(L) == length(L2) ->
	    proc_lib:spawn_link(yaws_ctl, start, [self()]),
	    {ok, {Gconf, L2, 0}};
	true ->
	    {stop, "failed to start server "}
    end.

		     

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(status, _From, State) ->
    Reply = {State, get(start_time)},
    {reply, Reply, State};
handle_call(pids, _From, State) ->  %% for gprof
    L = lists:map(fun(X) ->element(1, X) end, element(2, State)),
    {reply, [self() | L], State};
handle_call(mnum, _From, {GC, Group, Mnum}) ->
    {reply, Mnum+1,   {GC, Group, Mnum+1}}.



%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_info(_Msg, State) ->
    ?Debug("GOT ~p~n", [_Msg]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%% we search and setup www authenticate for each directory
%% specified as an auth directory

setup_auth(_SC, _E) ->
    ok.


do_listen(SC) ->
    case SC#sconf.ssl of
	undefined ->
	    {nossl, gen_tcp:listen(SC#sconf.port, opts(SC))};
	SSL ->
	    {ssl, ssl:listen(SC#sconf.port, ssl_opts(SC, SSL))}
    end.


%% One server per IP we listen to
gserv(GC, Group0) ->
    ?TC([{record, GC, gconf}]),
    Group = map(fun(SC) -> 
			E = ets:new(yaws_code, [public, set]),
			ets:insert(E, {num_files, 0}),
			ets:insert(E, {num_bytes, 0}),
			setup_auth(SC, E),
			SC#sconf{ets = E}
		end, Group0),
    SC = hd(Group),
    case do_listen(SC) of
	{SSLBOOL, {ok, Listen}} ->
	    error_logger:info_msg("Listening to ~s:~w for servers ~p~n",
			      [yaws:fmt_ip(SC#sconf.listen),
			       SC#sconf.port,
			       catch map(fun(S) ->  S#sconf.servername end, 
					 Group)]),
	    file:make_dir("/tmp/yaws"),
	    {ok, Files} = file:list_dir("/tmp/yaws"),
	    lists:foreach(
	      fun(F) -> file:delete("/tmp/yaws/" ++ F) end, Files),
	    proc_lib:init_ack({self(), Group}),
	    GS = #gs{gconf = GC,
		     group = Group,
		     ssl = SSLBOOL,
		     l = Listen},
	    acceptor(GS),
	    gserv(GS, [], 0);
	{_,Err} ->
	    error_logger:format("Failed to listen ~s:~w  : ~p~n",
				[yaws:fmt_ip(SC#sconf.listen),
				 SC#sconf.port, Err]),
	    proc_lib:init_ack({error, "Can't listen to socket: ~p ",[Err]}),
	    exit(normal)
    end.
			

gserv(GS, Ready, Rnum) ->
    receive
	{From , status} ->
	    From ! {self(), GS},
	    gserv(GS, Ready, Rnum);
	{From, next} when Ready == [] ->
	    acceptor(GS),
	    gserv(GS, Ready, Rnum);
	{From, next} ->
	    [R|RS] = Ready,
	    R ! {self(), accept},
	    gserv(GS, RS, Rnum-1);
	{From, done_client, Int} ->
	    GS2 = GS#gs{sessions = GS#gs.sessions + 1,
			reqs = GS#gs.reqs + Int},
	    if
		Rnum == 8 ->
		    From ! {self(), stop},
		    gserv(GS2, Ready, Rnum);
		Rnum < 8 ->
		    gserv(GS2, [From | Ready], Rnum+1)
	    end
    end.
	    


opts(SC) ->
    [binary, 
     {ip, SC#sconf.listen},
     {packet, http},
     {reuseaddr, true},
     {active, false}
    ].



ssl_opts(SC, SSL) ->
    Opts = [
	    binary,
	    {ip, SC#sconf.listen},
	    {packet, 0},
	    {active, false} | ssl_opts(SSL)],
    ?Debug("SSL opts  ~p~n", [Opts]),
    Opts.




ssl_opts(SSL) ->
    L = [if SSL#ssl.keyfile /= undefined ->
		 {keyfile, SSL#ssl.keyfile};
	    true ->
		 false
	 end,
	 
	 if SSL#ssl.certfile /= undefined ->
		 {certfile, SSL#ssl.certfile};
	    true ->
		 false
	 end,

	 if SSL#ssl.cacertfile /= undefined ->
		 {cacertfile, SSL#ssl.cacertfile};
	    true ->
		 false
	 end,
	 
	 if SSL#ssl.verify /= undefined ->
		 {verify, SSL#ssl.verify};
	    true ->
		 false
	 end,
	 
	 if SSL#ssl.password /= undefined ->
		 {password, SSL#ssl.password};
	    true ->
		 false
	 end,
	 if SSL#ssl.ciphers /= undefined ->
		 {ciphers, SSL#ssl.ciphers};
	    true ->
		 false
	 end
	],
    filter_false(L).


filter_false(L) ->
    [X || X <- L, X /= false].

	   
	 
do_accept(GS) when GS#gs.ssl == nossl ->
    gen_tcp:accept(GS#gs.l);
do_accept(GS) when GS#gs.ssl == ssl ->
    ssl:accept(GS#gs.l).


acceptor(GS) ->
    proc_lib:spawn_link(yaws_server, acceptor0, [GS, self()]).
acceptor0(GS, Top) ->
    ?TC([{record, GS, gs}]),
    %%L = GS#gs.l,
    X = do_accept(GS),
    Top ! {self(), next},
    case X of
	{ok, Client} ->
	    case (GS#gs.gconf)#gconf.trace of  %% traffic trace
		{true, _} ->
		    {ok, {IP, Port}} = case GS#gs.ssl of
					   ssl ->
					       ssl:peername(Client);
					   nossl ->
					       inet:peername(Client)
				       end,
		    Str = ?F("New (~p) connection from ~s:~w~n", 
			     [GS#gs.ssl, yaws:fmt_ip(IP),Port]),
		    yaws_log:trace_traffic(from_client, Str);
		_ ->
		    ok
	    end,

	    Res = (catch aloop(Client, GS,  0)),
	    ?Debug("RES = ~p~n", [Res]),
	    if
		GS#gs.ssl == nossl ->
		    gen_tcp:close(Client);
		GS#gs.ssl == ssl ->
		    ssl:close(Client)
	    end,

	    case  (GS#gs.gconf)#gconf.debug of
		true  ->
		    io:format("DIE: ~p~n", [Res]);
		_ ->
		    ok
	    end,
	    case Res of
		{ok, Int} when integer(Int) ->
		    Top ! {self(), done_client, Int};
		{'EXIT', normal} ->
		    Top ! {self(), done_client, 0};
		{'EXIT', Reason} ->
		    error_logger:format("~p~n", [Reason]),
		    Top ! {self(), done_client, 0}
	    end,
	    %% we cache processes
	    receive
		{Top, stop} ->
		    exit(normal);
		{Top, accept} ->
		    acceptor0(GS, Top)
	    end;
	_ ->
	    exit(normal)
    end.
     


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

aloop(CliSock, GS, Num) when GS#gs.ssl == nossl ->
    ?TC([{record, GS, gs}]),
    case http_get_headers(CliSock, GS#gs.gconf) of
	done ->
	    {ok, Num};
	{Req, H} ->
	    Sconf = pick_sconf(H, GS#gs.group),
	    ?Debug("Sconf: ~p", [?format_record(Sconf, sconf)]),
	    ?TC([{record, Sconf, sconf}]),
	    ?Debug("Headers = ~p~n", [?format_record(H, headers)]),
	    Res = apply(yaws_server, Req#http_request.method, 
			[CliSock, GS#gs.gconf, Sconf, Req, H]),
	    maybe_access_log(CliSock, Sconf, Req),
	    case Res of
		continue ->
		    aloop(CliSock, GS, Num+1);
		done ->
		    {ok, Num+1}
	    end
    end;



aloop(CliSock, GS, Num) when GS#gs.ssl == ssl ->
    ?TC([{record, GS, gs}]),
    case yaws_ssl:ssl_get_headers(CliSock, GS#gs.gconf) of
	done ->
	    {ok, Num};
	{ok, Req, H, Trail} ->
	    put(ssltrail, Trail),  %% hack hack hack
	    Sconf = hd(GS#gs.group),
	    Res = apply(yaws_server, Req#http_request.method, 
			[CliSock, GS#gs.gconf, Sconf, Req, H]),
	    maybe_access_log(CliSock, Sconf, Req),
	    case Res of
		continue ->
		    aloop(CliSock, GS, Num+1);
		done ->
		    {ok, Num+1}
	    end
    end.



pick_sconf(H, Group) ->
    case H#headers.host of
	undefined ->
	    pick_default(Group);
	Host ->
	    pick_host(Host, Group, Group)
    end.

pick_default([]) ->
    yaws_log:errlog("No default host found in server group ",[]),
    exit(normal);
pick_default([H|_T]) when H#sconf.default_server_on_this_ip == true ->
    H;
pick_default([_|T]) ->
    pick_default(T).


pick_host(Host, [H|_T], _Group) when H#sconf.servername == Host ->
    H;
pick_host(Host, [_|T], Group) ->
    pick_host(Host, T, Group);
pick_host(_Host, [], Group) ->
    pick_default(Group).


maybe_access_log(CliSock, SC, Req) ->
    ?TC([{record, SC, sconf}]),
    case SC#sconf.access_log of
	true ->
	    {ok, {Ip, _Port}} = case SC#sconf.ssl of
				   undefined ->
				       inet:peername(CliSock);
				   _SSL ->
				       ssl:peername(CliSock)
			       end,
	    
	    Status = case erase(status_code) of
			 undefined -> "-";
			 I -> integer_to_list(I)
		     end,
	    Len = case erase(content_length) of
		      undefined ->
			  "-";
		      I2 -> integer_to_list(I2)
		  end,
	    Path = get_path(Req#http_request.path),
	    Meth = atom_to_list(Req#http_request.method),
	    yaws_log:accesslog(SC#sconf.servername, Ip, 
			       [Meth, $\s, Path] , Status, Len);
	false ->
	    ignore
    end.


get_path({abs_path, Path}) ->
    Path.


do_recv(Sock, Num, TO, nossl) ->
    gen_tcp:recv(Sock, Num, TO);
do_recv(Sock, Num, _TO, ssl) ->
    case erase(ssltrail) of %% hack from above ...
	undefined ->
	    split_recv(ssl:recv(Sock, 0), Num);   %% ignore Num val ??? TO ??
	Bin ->
	    {ok, Bin}
    end.

%% weird ... ssl module doesn't respect Num arg for binary socks
split_recv({ok, B}, Num) when integer(Num) ->
    case B of
	<<Bin:Num/binary , Tail/binary>> ->
	    put(ssltrail, Tail),
	    {ok, Bin};
	_ ->
	    {ok, B}
    end;
split_recv(E, _) ->
    E.


	
	 

cli_recv(S, Num, GC, SslBool) when GC#gconf.trace == false ->
    do_recv(S, Num, GC#gconf.timeout, SslBool);
cli_recv(S, Num, GC, SslBool) ->
    Res = do_recv(S, Num, GC#gconf.timeout, SslBool),
    case Res of
	{ok, Val} when tuple(Val) ->
	    yaws_log:trace_traffic(from_client, ?F("~p~n", [Val]));
	{error, What} ->
	    yaws_log:trace_traffic(from_client, ?F("~n~p~n", [What]));
	{ok, http_eoh} ->
	    ok;
	{ok, Val} when GC#gconf.trace == {true, traffic} ->
	    yaws_log:trace_traffic(from_client, Val);
	{ok, Val} ->
	    {ok, Val}
    end,
    Res.



cli_write(S, Data, GC, SC) when GC#gconf.trace /= {true, traffic} ->
    gen_tcp_send(S, Data, SC);
cli_write(S, Data, _GC, SC) ->
    yaws_log:trace_traffic(from_server, Data),
    gen_tcp_send(S, Data, SC).


cli_write_headers(S, Data, GC, SC) when GC#gconf.trace == false ->
    gen_tcp_send(S, Data, SC);
cli_write_headers(S, Data, _GC, SC) ->
    yaws_log:trace_traffic(from_server, Data),
    gen_tcp_send(S, Data, SC).


gen_tcp_send(S, Data, SC) ->
    case SC#sconf.ssl of
	undefined ->
	    gen_tcp:send(S, Data);
	_SSL ->
	    ssl:send(S, Data)
    end.




http_get_headers(CliSock, GC) ->
    ?TC([{record, GC, gconf}]),
    inet:setopts(CliSock, [{packet, http}]),
    case cli_recv(CliSock, 0, GC, nossl) of
	{ok, R} when element(1, R) == http_request ->
	    H = http_get_headers(CliSock, R, GC, #headers{}),
	    {R, H};
	{error, timeout} ->
	    done;
	{error, closed} ->
	    done;
	_Err ->
	    ?Debug("Got ~p~n", [_Err]),
	    exit(normal)
    end.

http_get_headers(CliSock, Req, GC, H) ->
    ?TC([{record, GC, gconf}]),
    case cli_recv(CliSock, 0, GC, nossl) of
	{ok, {http_header,  _Num, 'Host', _, Host}} ->
	    http_get_headers(CliSock, Req, GC, H#headers{host = Host});
	{ok, {http_header, _Num, 'Connection', _, Conn}} ->
	    http_get_headers(CliSock, Req, GC, H#headers{connection = Conn});
	{ok, {http_header, _Num, 'Accept', _, Accept}} ->
	    http_get_headers(CliSock, Req, GC, H#headers{accept = Accept});
	{ok, {http_header, _Num, 'If-Modified-Since', _, X}} ->
	    http_get_headers(CliSock, Req, GC, 
			     H#headers{if_modified_since = X});
	{ok, {http_header, _Num, 'If-Match', _, X}} ->
	    http_get_headers(CliSock, Req, GC, H#headers{if_match = X});
	{ok, {http_header, _Num, 'If-None-Match', _, X}} ->
	    http_get_headers(CliSock, Req, GC, H#headers{if_none_match = X});
	{ok, {http_header, _Num, 'If-Range', _, X}} ->
	    http_get_headers(CliSock, Req, GC, H#headers{if_range = X});
	{ok, {http_header, _Num, 'If-Unmodified-Since', _, X}} ->
	    http_get_headers(CliSock, Req, GC, 
			H#headers{if_unmodified_since = X});
	{ok, {http_header, _Num, 'Range', _, X}} ->
	    http_get_headers(CliSock, Req, GC, H#headers{range = X});
	{ok, {http_header, _Num, 'Referer',_, X}} ->
	    http_get_headers(CliSock, Req, GC, H#headers{referer = X});
	{ok, {http_header, _Num, 'User-Agent', _, X}} ->
	    http_get_headers(CliSock, Req, GC, H#headers{user_agent = X});
	{ok, {http_header, _Num, 'Accept-Ranges', _, X}} ->
	    http_get_headers(CliSock, Req, GC, H#headers{accept_ranges = X});
	{ok, {http_header, _Num, 'Cookie', _, X}} ->
	    http_get_headers(CliSock, Req, GC, 
			H#headers{cookie = [X|H#headers.cookie]});
	{ok, {http_header, _Num, 'Keep-Alive', _, X}} ->
	    http_get_headers(CliSock, Req, GC, H#headers{keep_alive = X});
	{ok, {http_header, _Num, 'Content-Length', _, X}} ->
	    http_get_headers(CliSock, Req, GC, H#headers{content_length = X});
	{ok, {http_header, _Num, 'Authorization', _, X}} ->
	    http_get_headers(CliSock, Req, GC, 
			H#headers{authorization = parse_auth(X)});
	{ok, http_eoh} ->
	    H;
	{ok, X} ->
	    ?Debug("OTHER header ~p~n", [X]),
	    http_get_headers(CliSock, Req, GC, 
			     H#headers{other=[X|H#headers.other]});
	_Err ->
	    exit(normal)
    
    end.


inet_setopts(SC, S, Opts) when SC#sconf.ssl == nossl ->
    inet:setopts(S, Opts);
inet_setopts(_,_,_) ->
    ok.  %% noop for ssl


%% ret:  continue | done
'GET'(CliSock, GC, SC, Req, Head) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    ?Debug("GET ~p", [Req#http_request.path]),
    ok = inet_setopts(SC, CliSock, [{packet, raw}, binary]),
    flush(SC, CliSock, Head#headers.content_length),
    ARG = make_arg(CliSock, Head, Req, GC, SC),
    handle_request(CliSock, GC, SC, Req, Head, ARG).


'POST'(CliSock, GC, SC, Req, Head) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    ?Debug("POST Req=~p H=~p~n", [?format_record(Req, http_request),
				  ?format_record(Head, headers)]),
    ok = inet_setopts(SC, CliSock, [{packet, raw}, binary]),
    Bin = case Head#headers.content_length of
	undefined ->
	    case Head#headers.connection of
		"close" ->
		    get_client_data(CliSock, all, GC, SC#sconf.ssl);
		_ ->
		    exit(normal)
	    end;
	Len ->
	    get_client_data(CliSock, list_to_integer(Len), GC, SC#sconf.ssl)
    end,
    ?Debug("POST data = ~s~n", [binary_to_list(Bin)]),
    ARG = make_arg(CliSock, Head, Req, GC, SC),
    ARG2 = ARG#arg{clidata = Bin},
    handle_request(CliSock, GC, SC, Req, Head, ARG2).

%% will throw
'HEAD'(CliSock, GC, SC, Req, Head) ->
    'GET'(CliSock, GC, SC, Req, Head).

'TRACE'(_CliSock, _GC, _SC, _Req, _Head) ->
    nyi.

make_arg(CliSock, Head, Req, _GC, SC) ->
    ?TC([{record, _GC, gconf}, {record, SC, sconf}]),
    #arg{clisock = CliSock,
	 headers = Head,
	 req = Req,
	 docroot = SC#sconf.docroot}.

handle_request(CliSock, GC, SC, Req, H, ARG) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    UT =  url_type(GC, SC, get_path(Req#http_request.path)),
    ?Debug("UT: ~p", [?format_record(UT, urltype)]),
    case SC#sconf.authdirs of
	[] ->
	    handle_ut(CliSock, GC, SC, Req, H, ARG, UT);
	_Adirs ->
	    %% we have authentication enabled, check auth
	    UT2 = unflat(UT),
	    case lists:member(UT2#urltype.dir, SC#sconf.authdirs) of
		false ->
		    handle_ut(CliSock, GC, SC, Req, H, ARG, UT2);
		true ->
		    case is_authenticated(SC, UT2, Req, H) of
			true ->
			    handle_ut(CliSock, GC, SC, Req, H, ARG, UT2);
			{false, Realm} ->
			    deliver_401(CliSock, Req, GC, Realm, SC)
		    end
	    end
    end.


unflat(U) ->
    U2 = case U#urltype.dir of
	     {noflat, F} ->
		 U#urltype{dir = lists:flatten(F)};
	     _ ->
		 U
	 end,
    U2.



handle_ut(CliSock, GC, SC, Req, H, ARG, UT) ->
    case UT#urltype.type of
	error ->
	    deliver_404(CliSock, GC, SC, Req, SC);
	directory ->
	    P = UT#urltype.dir,
	    yaws_ls:list_directory(CliSock, UT#urltype.data, P, Req, GC, SC);
	regular -> 
	    deliver_file(CliSock, GC, SC, Req, H, UT);
	yaws ->
	    do_yaws(CliSock, GC, SC, Req, H, 
		    ARG#arg{querydata = UT#urltype.q}, UT);
	dotdot ->
	    deliver_403(CliSock, Req, GC, SC)
    end.

	

-record(auth,
	{dir,
	 realm = "",
	 type = "Basic",
	 users
	}).


parse_auth(Dir) ->
    case file:consult([Dir, [$/|".yaws_access"]]) of
	{ok, [{realm, Realm} |TermList]} ->
	    {ok, #auth{dir = Dir,
		       realm = Realm,
		       users = TermList}};
	{ok, TermList} ->
	    {ok, #auth{dir = Dir,
		       users = TermList}};
	Err ->
	    Err
    end.



is_authenticated(_SC, UT, _Req, H) ->
    case ets:info(auth_tab, size) of
	undefined ->
	    ets:new(auth_tab, [public, set, named_table]);
	_ ->
	    ok
    end,
    N = now_secs(),
    case ets:lookup(auth_tab, UT#urltype.dir) of
	[{_Dir, Auth, Then}] when Then+200 < N ->
	    case H#headers.authorization of
		undefined ->
		    {false, Auth#auth.realm};
		{_User, _Password} ->
		    uhhhhh
	    end
    end.



deliver_401(CliSock, _Req, GC, Realm, SC) ->
    H = make_date_and_server_headers(),
    B = list_to_binary("<html> <h1> 401 authentication needed  "
		       "</h1></html>"),
    D = [make_400(401), H, 
	 make_connection_close(true),
	 make_www_authenticate(Realm),
	 make_content_length(size(B)), crnl()],
    send_headers_and_data(true, CliSock, D, B, GC, SC),
    done.
    


deliver_403(CliSock, _Req, GC, SC) ->
    H = make_date_and_server_headers(),
    B = list_to_binary("<html> <h1> 403 Forbidden, no .. paths "
		       "allowed  </h1></html>"),
    D = [make_400(403), H, make_connection_close(true),
	 make_content_length(size(B)), crnl()],
    send_headers_and_data(true, CliSock, D, B, GC, SC),
    done.



deliver_404(CliSock, GC, SC,  Req, SC) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    H = make_date_and_server_headers(),
    B = not_found_body(get_path(Req#http_request.path), GC, SC),
    D = [make_400(404), H, make_connection_close(true),
	 make_content_length(size(B)), crnl()],
    send_headers_and_data(true, CliSock, D, B, GC, SC),
    done.


do_yaws(CliSock, GC, SC, Req, H, ARG, UT) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    FileAtom = list_to_atom(UT#urltype.fullpath),
    Mtime = mtime(UT#urltype.finfo),
    case ets:lookup(SC#sconf.ets, FileAtom) of
	[{FileAtom, Mtime1, Spec}] when Mtime1 == Mtime ->
	    deliver_dyn_file(CliSock, GC, SC, Req, H, Spec, ARG,UT);
	Other  ->
	    del_old_files(Other),
	    {ok, Spec} = yaws_compile:compile_file(UT#urltype.fullpath, GC, SC),
	    ?Debug("Spec for file ~s is:~n~p~n",[UT#urltype.fullpath, Spec]),
	    ets:insert(SC#sconf.ets, {FileAtom, Mtime, Spec}),
	    deliver_dyn_file(CliSock, GC, SC, Req, H, Spec, ARG, UT)
    end.


del_old_files([]) ->
    ok;
del_old_files([{_FileAtom, _Mtime1, Spec}]) ->
    lists:foreach(
      fun({mod, _, _, _,  Mod, _Func}) ->
	      F="/tmp/yaws/" ++ yaws:to_list(Mod) ++ ".erl",
	      file:delete(F);
	 (_) ->
	      ok
      end, Spec).
		     

get_client_data(CliSock, all, GC, SSlBool) ->
    get_client_data(CliSock, all, cli_recv(CliSock, 4000, GC, SSlBool), 
		    GC, SSlBool);

get_client_data(CliSock, Len, GC, SSlBool) ->
    case cli_recv(CliSock, Len, GC, SSlBool) of
	{ok, B} when size(B) == Len ->
	    B;
	_ ->
	    exit(normal)
    end.

get_client_data(CliSock, all, {ok, B}, GC, SSlBool) ->
    B2 = get_client_data(CliSock, all, 
			 cli_recv(CliSock, 4000, GC, SSlBool), SSlBool),
    <<B/binary, B2/binary>>;
get_client_data(_CliSock, all, eof, _GC, _) ->
    <<>>.


do_dyn_headers(_CliSock, _GC, _SC, Bin, Fd, Req, Head, [H|T], ARG) ->
    ?TC([{record, _GC, gconf}, {record, _SC, sconf}]),
    {DoClose, Chunked} = case Req#http_request.version of
			     {1, 0} -> {true, []};
			     {1, 1} -> {false, make_chunked()}
			 end,
    case H of
	{mod, _LineNo, _YawsFile, SkipChars, Mod, some_headers} ->
	    MimeType = "text/html",
	    OutH = make_dyn_headers(DoClose, MimeType),
	    {_, Bin2} = skip_data(Bin, Fd, SkipChars),
	    case (catch apply(Mod, some_headers, [ARG])) of
		{ok, Out} ->
		    {[make_200(),OutH, Chunked, Out, 
		      crnl_if_not_chunked(DoClose)], 
		     [],T, DoClose, Bin2};
		ok ->
		    {[make_200(),OutH, Chunked, 
		      crnl_if_not_chunked(DoClose)], [], T, DoClose, Bin2};
		close ->
		    exit(normal);
		Err ->
		    {[make_200(), OutH, crnl()],
		     ?F("<p><br> <pre>yaws code ~w:~w(~p) crashed or ret bad val:"
			" ~n~p~n</pre><br>~n", 
			[Mod, some_headers, [Head], Err]),
		     T, true, Bin2}
	    end;
	{mod, _LineNo, _YawsFile, SkipChars, Mod, all_headers} ->
	    {_, Bin2} = skip_data(Bin, Fd, SkipChars),
	    case (catch apply(Mod, all_headers, [ARG])) of
		{ok, StatusCode, Out} when integer(StatusCode) ->
		    put(status_code, StatusCode),
		    {[Out, crnl_if_not_chunked(DoClose)], 
		     [], T, DoClose, Bin2};
		close ->
		    exit(normal);
		Err ->
		    MimeType = "text/html",
		    OutH = make_dyn_headers(DoClose, MimeType),
		    {[make_200(), OutH, crnl()],
		      ?F("<p> yaws code ~w:~w(~p) crashed or returned bad value:"
			 " ~n~p~n", 
			 [Mod, all_headers, [Head], Err]), T,  true, Bin2}
	    end;
	_ ->
	    MimeType = "text/html",
	    OutH = make_dyn_headers(DoClose, MimeType),
	    {[make_200(),OutH, Chunked, crnl_if_not_chunked(DoClose)], 
	     [], [H|T], DoClose, Bin}
    end.



crnl_if_not_chunked(false) ->
    [];
crnl_if_not_chunked(true) ->
    crnl().


	   

%% do the header and continue
deliver_dyn_file(CliSock, GC, SC, Req, Head, Specs, ARG, UT) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    Fd = ut_open(UT),
    Bin = ut_read(Fd),
    {S2, Content, Tail, DoClose, Bin2} = 
	do_dyn_headers(CliSock,GC, SC,Bin,Fd,  Req,Head,Specs,ARG),
    ?Debug("TAIL =~p~n", [Tail]),
    send_headers(CliSock, S2, GC, SC),
    close_if_HEAD(Req, fun() -> do_tcp_close(CliSock, SC), throw({ok, 1}) end),
    if
	Content /= [] ->
	    safe_send(true, CliSock, Content, GC, SC);
	true ->
	    ok
    end,
    deliver_dyn_file(CliSock, GC, SC, Req, Head, UT, DoClose, Bin2, Fd, Tail, ARG).



deliver_dyn_file(CliSock, GC, SC, Req, Head, UT, DC, Bin, Fd, [H|T],ARG) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}, {record, UT,urltype}]),
    ?Debug("deliver_dyn_file: ~p~n", [H]),
    case H of
	{mod, LineNo, YawsFile, NumChars, Mod, out} ->
	    {_, Bin2} = skip_data(Bin, Fd, NumChars),
	    safe_call(DC, LineNo, YawsFile, CliSock, Mod, out, [ARG], GC, SC),
	    deliver_dyn_file(CliSock, GC, SC, Req, Head, UT, DC,
			     Bin2,Fd,T,ARG);
	{data, 0} ->
	    deliver_dyn_file(CliSock, GC, SC, Req, Head, UT,DC, Bin, Fd,T,ARG);
	{data, NumChars} ->
	    {Send, Bin2} = skip_data(Bin, Fd, NumChars),
	    safe_send(DC, CliSock, Send, GC, SC),
	    deliver_dyn_file(CliSock, GC, SC, Req, Bin2,
			     UT, DC, Bin2, Fd, T,ARG);
	{error, NumChars, Str} ->
	    {_, Bin2} = skip_data(Bin, Fd, NumChars),
	    safe_send(DC, CliSock, Str, GC, SC),
	    deliver_dyn_file(CliSock, GC, SC, Req, Bin2,
			     UT, DC, Bin2, Fd, T,ARG)
    end;
deliver_dyn_file(CliSock, GC, SC, _Req, _Head,_UT,DC, _Bin, _Fd, [], _ARG) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    ?Debug("deliver_dyn: done~n", []),
    case DC of
	true ->
	    done;
	false ->
	    cli_write(CliSock, [crnl(), "0", crnl2()], GC, SC),
	    continue
    end.


%% what about trailers ??

skip_data(List, Fd, Sz) when list(List) ->
    skip_data(list_to_binary(List), Fd, Sz);
skip_data(Bin, Fd, Sz) when binary(Bin) ->
    ?Debug("Skip data ~p bytes ", [Sz]),
     case  Bin of
	 <<Head:Sz/binary ,Tail/binary>> ->
	     {Head, Tail};
	 _ ->
	     case file:read(Fd, 4000) of
		 {ok, Bin2} -> 
		     Bin3 = <<Bin/binary, Bin2/binary>>,
		     skip_data(Bin3, Fd, Sz);
		 _Err ->
		     ?Debug("EXIT in skip_data: ~p  ~p~n", [Bin, Sz]),
		     exit(normal)
	     end
     end;
skip_data({bin, Bin}, _, Sz) ->
    <<Head:Sz/binary ,Tail/binary>> = Bin,
    {Head, {bin, Tail}}.



to_binary(B) when binary(B) ->
    B;
to_binary(L) when list(L) ->
    list_to_binary(L).


send_headers_and_data(DC, Sock, Headers, Data, GC, SC) ->
    send_headers(Sock, Headers, GC, SC),
    safe_send(DC, Sock, Data, GC, SC).


send_headers(Sock, Data, GC, SC) ->
    case cli_write_headers(Sock, Data, GC, SC) of
	ok ->
	    ok;
	Err ->
	    yaws_debug:format(GC, 
			      "Failed to send ~p on socket: ~p~n",
			      [Data, Err]),
	    exit(normal)
    end.


safe_send(DoClose, Sock, Data, GC, SC) ->
    if
	DoClose == true  ->
	    case cli_write(Sock, Data, GC, SC) of
		ok ->
		    ok;
		Err ->
		    yaws_debug:format(GC, 
				      "Failed to send ~p on socket: ~p~n",
				      [Data, Err]),
		    exit(normal)
	    end;
	DoClose == false ->
	    B = to_binary(Data),
	    Len = size(B),
	    Data2 = [crnl(), yaws:integer_to_hex(Len) , crnl(), B],
	    case cli_write(Sock, Data2, GC, SC) of
		ok ->
		    ok;

		Err ->
		    yaws_debug:format(GC, 
				      "Failed to send ~p on socket: ~p~n",
				      [Data, Err]),

		    exit(normal)
	    end
    end.

		    
safe_call(DoClose, LineNo, YawsFile, CliSock, M, F, A, GC, SC) ->
    ?Debug("safe_call ~w:~w(~p)~n", 
	   [M, F, ?format_record(hd(A), arg)]),
    case (catch apply(M,F,A)) of
	{'EXIT', Err} ->
	    safe_send(DoClose, CliSock, 
		      ?F("~n<pre> ~nERROR erl ~w/1 code  crashed:~n "
			 "File: ~s:~w~n"
			 "Reason: ~p~n</pre>~n",
			 [F, YawsFile, LineNo, Err]), GC, SC),
	    yaws:elog("erl code  ~w/1 crashed: ~n"
		      "File: ~s:~w~n"
		      "Reason: ~p", [F, YawsFile, LineNo, Err]);
	
	{ok, Out} ->
	    safe_send(DoClose, CliSock, Out, GC, SC);
	Other ->
	    safe_send(DoClose, CliSock, 
		      ?F("~n<pre>~nERROR erl code ~w/1 returned bad value: ~n "
			 "File: ~s:~w~n"
			 "Value: ~p~n</pre>~n",
			 [F, YawsFile, LineNo, Other]), GC, SC),
	    yaws:elog("erl code  ~w/1 returned bad value: ~n"
		      "File: ~s:~w~n"
		      "Value: ~p", [F, YawsFile, LineNo, Other]);

	close ->
	    do_tcp_close(CliSock, SC),
	    exit(normal)
    end.


do_tcp_close(Sock, SC) ->
    case SC#sconf.ssl of
	nossl ->
	    gen_tcp:close(Sock);
	ssl ->
	    ssl:close(Sock)
    end.



ut_open(UT) ->
    case UT#urltype.data of
	undefined ->
	    {ok, Fd} = file:open(UT#urltype.fullpath, [read, raw]),
	    Fd;
	B when binary(B) ->
	    {bin, B}
    end.


ut_read({bin, B}) ->
    B;
ut_read(Fd) ->
    file:read(Fd, 4000).


ut_close({bin, _}) ->
    ok;
ut_close(Fd) ->
    file:close(Fd).

	


deliver_file(CliSock, GC, SC, Req, InH, UT) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}, {record, UT, urltype}]),
    DoClose = do_close(Req, InH),
    OutH = make_static_headers(Req, UT, DoClose),
    Fd = ut_open(UT),
    Bin = ut_read(Fd),
    send_headers(CliSock, [make_200(), OutH, crnl()], GC, SC),
    close_if_HEAD(Req, fun() -> ut_close(Fd), throw({ok, 1}) end),
    case Bin of
	{bin, Binary} ->
	    cli_write(CliSock, Binary, GC, SC);
	{ok, Binary} ->
	    send_loop(CliSock, Binary, Fd, GC, SC);
	Binary2 when binary(Binary2) ->
	    cli_write(CliSock, Binary2, GC, SC)
    end,
    ut_close(Fd),
    if
	DoClose == true ->
	    done;
	DoClose == false ->
	    continue
    end.

close_if_HEAD(Req, F) ->
    if
	Req#http_request.method == 'HEAD' ->
	    F();
	true ->
	    ok
    end.


do_close(Req, H) ->
    case Req#http_request.version of
	{1, 0} ->
	     case H#headers.keep_alive of
		 undefined ->
		     true;
		 _ ->
		     false
	     end;
	_ ->
	    false
    end.


send_loop(CliSock, Data, Fd, GC, SC) ->
    case cli_write(CliSock, Data, GC, SC) of
	ok ->
	    case ut_read(Fd) of
		{ok, Data2} ->
		    send_loop(CliSock, Data2, Fd, GC, SC);
		eof ->
		    ok
	    end;
	Err ->
	    Err
    end.
	     


make_dyn_headers(DoClose, MimeType) ->
    [make_date_header(),
     make_server_header(),
     make_content_type(MimeType),
     make_connection_close(DoClose),
     make_non_cache_able()
    ].


make_x_pad() ->
    "X-Pad: avoid browser bug\r\n".


make_non_cache_able() ->
    []. %% FIXME

make_date_and_server_headers() ->
    [make_date_header(),
     make_server_header()
    ].

make_static_headers(_Req, UT, DoClose) ->    
    [make_date_and_server_headers(),
     make_last_modified(UT#urltype.finfo),
     make_etag(UT#urltype.finfo),
     make_accept_ranges(),
     make_content_length(UT#urltype.finfo),
     make_content_type(UT#urltype.mime),
     make_connection_close(DoClose)
    ].



make_200() ->
    put(status_code, 200),
    "HTTP/1.1 200 OK\r\n".

make_400(Code) ->
    put(status_code, Code),
    ["HTTP/1.1 ", integer_to_list(Code), " ",
     yaws_api:code_to_phrase(Code), crnl()].



crnl() ->
    "\r\n".
crnl2() ->
    "\r\n\r\n".


%% FIXME optimize the goddamn date generations
make_date_header() ->
    N = element(2, now()),
    case get(date_header) of
	{Str, Secs} when (Secs+10) > N ->
	    H = ["Date: ", yaws:universal_time_as_string(), crnl()],
	    put(date_header, {H, N}),
	    H;
	{Str, Secs} ->
	    Str;
	undefined ->
	    H = ["Date: ", yaws:universal_time_as_string(), crnl()],
	    put(date_header, {H, N}),
	    H
    end.
%% FIXME read vsn from conf at compile time
make_server_header() ->
    ["Server: Yaws/0.32 Yet Another Web Server", crnl()].
make_last_modified(_) ->
    [];
make_last_modified(FI) ->
    N = element(2, now()),
    Inode = FI#file_info.inode,  %% unix only
    case get({last_modified, Inode}) of
	{Str, Secs} when N < (Secs+10) ->
	    Str;
	_ ->
	    S = do_make_last_modified(FI),
	    put({last_modified, Inode}, {S, N}),
	    S
    end.

do_make_last_modified(FI) ->
    Then = FI#file_info.mtime,
    UTC = calendar:now_to_universal_time(Then),
    Local = calendar:universal_time_to_local_time(UTC),
    Str = yaws:date_and_time(Local, UTC),
    ["Last-Modified: ", yaws:date_and_time_to_string(Str), crnl()].
make_etag(_File) ->
    [].
make_accept_ranges() ->
    "Accept-Ranges: bytes\r\n".
make_content_length(Size) when integer(Size) ->
    put(content_length, Size),
    ["Content-Length: ", integer_to_list(Size), crnl()];
make_content_length(FI) ->
    Size = FI#file_info.size,
    put(content_length, Size),
    ["Content-Length: ", integer_to_list(Size), crnl()].
make_content_type(MimeType) ->
    ["Content-Type: ", MimeType, crnl()].

make_connection_close(true) ->
    "Connection: close\r\n";
make_connection_close(false) ->
    [].
make_chunked() ->
    ["Transfer-Encoding: chunked",  crnl()].

make_www_authenticate(Realm) ->
    ["WWW-Authenticate: Basic realm=\"", Realm, [$"|crnl()]].



now_secs() ->
    {M,S,_}=now(),
    (M*1000000)+S.


%% a file cache,
url_type(GC, SC, Path) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    E = SC#sconf.ets,
    update_total(E, Path),
    case ets:lookup(E, {url, Path}) of
	[] ->
	    UT = do_url_type(SC#sconf.docroot, Path),
	    ?TC([{record, UT, urltype}]),
	    cache_file(GC, SC, Path, UT);
	[{_, When, UT}] ->
	    N = now_secs(),
	    FI = UT#urltype.finfo,
	    if
		When + 30 > N ->
		    %% more than 30 secs old entry
		    ets:delete(E, {url, Path}),
		    ets:delete(E, {urlc, Path}),
		    ets:update_counter(E, num_files, -1),
		    ets:update_counter(E, num_bytes, -FI#file_info.size),
		    url_type(GC, SC, Path);
		true ->
		    ets:update_counter(E, {urlc, Path}, 1),
		    UT
	    end
    end.


update_total(E, Path) ->
    case (catch ets:update_counter(E, {urlc_total, Path}, 1)) of
	{'EXIT', _} ->
	    ets:insert(E, {{urlc_total, Path}, 1});
	_ ->
	    ok
    end.


cache_file(GC, SC, Path, UT) when UT#urltype.type == regular ;
				  UT#urltype.type == yaws ->
    E = SC#sconf.ets,
    [{num_files, N}] = ets:lookup(E, num_files),
    [{num_bytes, B}] = ets:lookup(E, num_bytes),
    FI = UT#urltype.finfo,
    if
	N + 1 > GC#gconf.max_num_cached_files ->
	    error_logger:info("Max NUM cached files reached for server "
			      "~p", [SC#sconf.servername]),
	    cleanup_cache(E, num),
	    cache_file(GC, SC, Path, UT);
	B + FI#file_info.size > GC#gconf.max_num_cached_bytes ->
	    error_logger:info("Max size cached bytes reached for server "
			      "~p", [SC#sconf.servername]),
	    cleanup_cache(E, size),
	    cache_file(GC, SC, Path, UT);
	true ->
	    if
		FI#file_info.size > GC#gconf.max_size_cached_file ->
		    UT;
		true ->
		    {ok, Bin} = file:read_file(UT#urltype.fullpath),
		    UT2 = UT#urltype{data = Bin},
		    ets:insert(E, {{url, Path}, now_secs(), UT2}),
		    ets:insert(E, {{urlc, Path}, 1}),
		    ets:update_counter(E, num_files, 1),
		    ets:update_counter(E, num_bytes, FI#file_info.size),
		    UT2
	    end
    end;
cache_file(_GC, _SC, _Path, UT) ->
    UT.


cleanup_cache(_E, size) ->
    %% remove the largest files with the least hit count  (urlc)
    uhhh;
cleanup_cache(_E, num) ->
    %% remove all files with a low hit count
    uhhh.


%% return #urltype{}
do_url_type(Droot, Path) ->
    case split_path(Droot, Path, [], []) of
	slash ->
	    maybe_return_dir(Droot, lists:flatten(Path));
	dotdot ->  %% generate 403 forbidden
	    #urltype{type=dotdot};
	OK ->
	    OK
    end.


maybe_return_dir(DR, FlatPath) ->
    case file:read_file_info([DR, FlatPath, "/index.yaws"]) of
	{ok, FI} ->
	    #urltype{type = yaws,
		     finfo = FI,
		     mime = "text/html",
		     dir = FlatPath,
		     fullpath = ?f([DR, FlatPath,"/index.yaws"])};
	_ ->
	    case file:read_file_info([DR, FlatPath, "/index.html"]) of
		{ok, FI} ->
		    #urltype{type = regular,
			     finfo = FI,
			     mime = "text/html",
			     dir = FlatPath,
			     fullpath =?f([DR,FlatPath,"/index.html"])};
		_ ->
		    case file:list_dir([DR, FlatPath]) of
			{ok, List} ->
			    #urltype{type = directory, 
				     dir = FlatPath,
				     data=List};
			_Err ->
			    #urltype{type=error}
		    end
	    end
    end.


split_path(_DR, [$/], _Comps, []) ->
    %% its a URL that ends with /
    slash;
split_path(DR, [$/, $/ |Tail], Comps, Part) ->  %% security clause
    split_path(DR, [$/|Tail], Comps, Part);
split_path(DR, [$/, $., $., $/ |Tail], _, [_H|T]) ->  %% security clause
    split_path(DR, Tail, [], T);
split_path(_DR, [$/, $., $., $/ |_Tail], _, []) -> %% security clause
    dotdot;
split_path(DR, [], Comps, Part) ->
    ret_reg_split(DR, Comps, Part, []);
split_path(DR, [$?|Tail], Comps, Part) ->
    ret_reg_split(DR, Comps, Part, Tail);
split_path(DR, [$/|Tail], Comps, Part)  when Part /= [] ->
    ?Debug("Tail=~s Part=~s", [Tail,Part]),
    split_path(DR, [$/|Tail], [lists:reverse(Part) | Comps], []);
split_path(DR, [$~|Tail], Comps, Part) ->  %% user dir
    ret_user_dir(DR, Comps, Part, Tail);
split_path(DR, [H|T], Comps, Part) ->
    split_path(DR, T, Comps, [H|Part]).



%% http:/a.b.c/~user URLs
ret_user_dir(DR, [], "/", Upath) ->
    ?Debug("UserPart = ~p~n", [Upath]),
    {User, Path} = parse_user_path(DR, Upath, []),
    
    ?Debug("User=~p Path = ~p~n", [User, Path]),

    %% FIXME doesn't work if passwd contains :: 
    %% also this is unix only
    %% and it ain't the fastest code around.

    Home = lists:nth(6, string:tokens(
			  os:cmd(["grep ", User, " /etc/passwd "]),[$:])),
    DR2 = [Home ++ "/public_html/"],
    do_url_type(DR2, Path). %% recurse



parse_user_path(_DR, [], User) ->
    {lists:reverse(User), []};
parse_user_path(_DR, [$/], User) ->
    {lists:reverse(User), []};
parse_user_path(_DR, [$/|Tail], User) ->
    {lists:reverse(User), Tail};
parse_user_path(DR, [H|T], User) ->
    parse_user_path(DR, T, [H|User]).



ret_reg_split(DR, Comps, RevFile, Query) ->
    ?Debug("ret_reg_split(~p)", [[DR, Comps, RevFile]]),
    Dir = lists:reverse(Comps),
    %%FlatDir = lists:flatten(Dir),
    FlatDir = {noflat, Dir},
    File = lists:reverse(RevFile),
    L = [DR, Dir, File],
    ?Debug("ret_reg_split: L =~p~n",[L]),
    case file:read_file_info(L) of
	{ok, FI} when FI#file_info.type == regular ->
	    {X, Mime} = suffix_type(RevFile),
	    #urltype{type=X, 
		     finfo=FI,
		     dir = FlatDir,
		     fullpath = lists:flatten(L),
		     mime=Mime, q=Query};
	{ok, FI} when FI#file_info.type == directory ->
	    maybe_return_dir(DR, lists:flatten(Dir) ++ File);
	Err ->
	    #urltype{type=error, data=Err}
    end.

%% FIXME add all mime types here
suffix_type("sway." ++ _) ->
    {yaws, "text/html"};
suffix_type("lmth." ++ _) ->
    {regular, "text/html"};
suffix_type("gpj." ++ _) ->
    {regular, "image/jpeg"};
suffix_type("gnp." ++ _) ->
    {regular, "image/png"};
suffix_type("fig." ++ _) ->
    {regular, "image/gif"};
suffix_type("3pm." ++ _) ->
    {regular, "audio/mpeg"};
suffix_type("zg." ++ _) ->
    {regular, "application/x-gzip"};
suffix_type(_) ->
    {regular, "application/octet-stream"}.



flush(SC, Sock, Sz) ->
    case SC#sconf.ssl of
	undefined ->
	    tcp_flush(Sock, Sz);
	_ ->
	    ssl_flush(Sock, Sz)
    end.

	    


tcp_flush(_Sock, undefined) ->
    ok;
tcp_flush(_Sock, 0) ->
    ok;
tcp_flush(Sock, Sz) ->
    gen_tcp:recv(Sock, Sz, 1000).


ssl_flush(_Sock, undefined) ->
    ok;
ssl_flush(_Sock, 0) ->
    ok;
ssl_flush(Sock, Sz) ->
    split_recv(ssl:recv(Sock, Sz, 1000), Sz).


			


mtime(F) ->
    F#file_info.mtime.



not_found_body(Url, GC, SC) ->
    ?Debug("Sconf: ~p", [?format_record(SC, sconf)]),
    L = ["<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">"
	 "<HTML><HEAD>"
	 "<TITLE>404 Not Found</TITLE>"
	 "</HEAD><BODY>"
	 "<H1>Not Found</H1>"
	 "The requested URL ", 
	 Url, 
	 " was not found on this server.<P>"
	 "<HR>",
	 yaws:address(GC, SC),
	 "  </BODY></HTML>"
	],
    list_to_binary(L).




    

