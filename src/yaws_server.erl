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
		           %% flat | unflat need flat for authentication
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
    RunMod = case application:get_env(yaws, runmod) of
		 undefined ->
		     find_runmod(AS);
		 {ok,Mod} ->
		     {ok,Mod}
	     end,
    {Debug, Trace, Conf, RunMod}.

find_c([{conf, [File]} |_]) ->
    {file, File};
find_c([_|T]) ->
    find_c(T);
find_c([]) ->
    false.

find_runmod([{runmod, [Mod]} |_]) ->
    {ok,l2a(Mod)};
find_runmod([_|T]) ->
    find_runmod(T);
find_runmod([]) ->
    false.

l2a(L) when list(L) -> list_to_atom(L);
l2a(A) when atom(A) -> A.



%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    put(start_time, calendar:local_time()),  %% for uptime
    {Debug, Trace, Conf, RunMod} = get_app_args(),

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

	    init2(Gconf, Sconfs, RunMod);
	{error, E} ->
	    case erase(logdir) of
		undefined ->
		    error_logger:format("Bad conf: ~p~n", [E]),
		    init:stop(),
		    {stop, E};
		Dir ->
		    yaws_log:setdir(Dir, []),
		    yaws_log:sync_errlog("bad conf: ~s~n",[E]),
		    init:stop(),
		    {stop, E}
		end
    end.


init2(Gconf, Sconfs, RunMod) ->
    lists:foreach(
      fun(D) ->
	      code:add_pathz(D)
      end, Gconf#gconf.ebin_dir),

    process_flag(trap_exit, true),

    %% start user provided application
    case RunMod of
	{ok,Mod} -> 
	    runmod(Mod);
	_ -> 
	    false
    end,

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
	    case Res of
		{ok, Int} when integer(Int) ->
		    Top ! {self(), done_client, Int};
		{'EXIT', normal} ->
		    Top ! {self(), done_client, 0};
		{'EXIT', Reason} ->
		    yaws_log:errlog("~p~n", [Reason]),
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


inet_peername(Sock, SC) ->
    case SC#sconf.ssl of
	undefined ->
	    inet:peername(Sock);
	_SSL ->
	    ssl:peername(Sock)
    end.
	    

maybe_access_log(CliSock, SC, Req) ->
    ?TC([{record, SC, sconf}]),
    case SC#sconf.access_log of
	true ->
	    {ok, {Ip, _Port}} = inet_peername(CliSock, SC),
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





gen_tcp_send(S, Data, SC, GC) ->
    Res = case SC#sconf.ssl of
	undefined ->
	    gen_tcp:send(S, Data);
	_SSL ->
	    ssl:send(S, Data)
    end,
    case Res of
	ok ->
	    ok;
	Err ->
	    yaws_debug:derror(GC, "Failed to send ~p on socket ~p: ~p~n",
			      [Data, S, Err])
	    
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


inet_setopts(SC, S, Opts) when SC#sconf.ssl == undefined ->
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
		    get_client_data(CliSock, all, GC, is_ssl(SC#sconf.ssl));
		_ ->
		    ?Debug("No content length header ",[]),
		    exit(normal)
	    end;
	Len ->
	    get_client_data(CliSock, list_to_integer(Len), GC, 
			    is_ssl(SC#sconf.ssl))
    end,
    ?Debug("POST data = ~s~n", [binary_to_list(Bin)]),
    ARG = make_arg(CliSock, Head, Req, GC, SC),
    ARG2 = ARG#arg{clidata = Bin},
    handle_request(CliSock, GC, SC, Req, Head, ARG2).


is_ssl(undefined) ->
    nossl;
is_ssl(R) when record(R, ssl) ->
    ssl.


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
	    deliver_403(CliSock, Req, GC, SC);
	redir_dir ->
	     deliver_303(CliSock, Req, GC, SC)
    end.

	

-record(auth,
	{dir,
	 realm = "",
	 type = "Basic",
	 users
	}).


parse_auth(Dir) ->
    case file:consult([Dir, [$/|".yaws_auth"]]) of
	{ok, [{realm, Realm} |TermList]} ->
	    {ok, #auth{dir = Dir,
		       realm = Realm,
		       users = TermList}};
	{ok, TermList} ->
	    {ok, #auth{dir = Dir,
		       users = TermList}};
	Err ->
	    error_logger:format("Bad .yaws_auth file in dir ~p~n", [Dir]),
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


% we must deliver a 303 if the browser asks for a dir
% without a trailing / in the HTTP req
% otherwise the relative urls in /dir/index.html will be broken.

deliver_303(CliSock, Req, GC, SC) ->
    Scheme = case SC#sconf.ssl of
		 undefined ->
		     "http://";
		 _SSl ->
		     "https://"
	     end,
    PortPart = case {SC#sconf.ssl, SC#sconf.port} of
		   {undefined, 80} ->
		       "";
		   {undefined, Port} ->
		       io_lib:format(":~w",[Port]);
		   {_SSL, 443} ->
		       "";
		   {_SSL, Port} ->
		       io_lib:format(":~w",[Port])
	       end,

    set_status_code(303),
    make_connection_close(true),
    make_content_length(0),
    accumulate_header(["Location: ", Scheme, SC#sconf.servername, 
		       PortPart, get_path(Req#http_request.path), "/"]),
    
    deliver_accumulated(#dcc{}, CliSock, GC, SC),
    done.

deliver_401(CliSock, _Req, GC, Realm, SC) ->
    set_status_code(401),
    make_date_and_server_headers(),
    B = list_to_binary("<html> <h1> 401 authentication needed  "
		       "</h1></html>"),
    make_connection_close(true),
    make_www_authenticate(Realm),
    make_content_length(size(B)),
    accumulate_content(B),
    make_content_type(),
    deliver_accumulated(#dcc{}, CliSock, GC, SC),
    done.
    


deliver_403(CliSock, _Req, GC, SC) ->
    set_status_code(403),
    make_date_and_server_headers(),
    B = list_to_binary("<html> <h1> 403 Forbidden, no .. paths "
		       "allowed  </h1></html>"),
    make_connection_close(true),
    make_content_length(size(B)),
    make_content_type(),
    accumulate_content(B),
    deliver_accumulated(#dcc{}, CliSock, GC, SC),
    done.


deliver_404(CliSock, GC, SC,  Req, SC) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    set_status_code(404),
    make_date_and_server_headers(),
    B = not_found_body(get_path(Req#http_request.path), GC, SC),
    make_connection_close(true),
    make_content_length(size(B)),
    make_content_type(),
    accumulate_content(B),
    deliver_accumulated(#dcc{}, CliSock, GC, SC),
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
	_Other ->
	    ?Debug("get_client_data: ~p~n", [_Other]),
	    exit(normal)
    end.

get_client_data(CliSock, all, {ok, B}, GC, SSlBool) ->
    B2 = get_client_data(CliSock, all, 
			 cli_recv(CliSock, 4000, GC, SSlBool), SSlBool),
    <<B/binary, B2/binary>>;
get_client_data(_CliSock, all, eof, _GC, _) ->
    <<>>.

	   

%% do the header and continue
deliver_dyn_file(CliSock, GC, SC, Req, Head, Specs, ARG, UT) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    Fd = ut_open(UT),
    Bin = ut_read(Fd),
    {DoClose, Chunked} = case Req#http_request.version of
			     {1, 0} -> {true, false};
			     {1, 1} -> make_chunked(), 
				       {false, true}
			 end,
    DCC = #dcc{doclose = DoClose,
	       chunked = Chunked},
    
    MimeType = "text/html",
    make_dyn_headers(DCC, Req, MimeType),
    close_if_HEAD(Req, fun() ->
			       deliver_accumulated(DCC, CliSock,GC,SC),
			       do_tcp_close(CliSock, SC), 
			       throw({ok, 1}) 
		       end),
    deliver_dyn_file(CliSock, GC, SC, Req, Head, UT, DCC, Bin, Fd, Specs, ARG).



deliver_dyn_file(CliSock, GC, SC, Req, Head, UT, DCC, Bin, Fd, [H|T],ARG) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}, {record, UT,urltype}]),
    ?Debug("deliver_dyn_file: ~p~n", [H]),
    case H of
	{mod, LineNo, YawsFile, NumChars, Mod, out} ->
	    {_, Bin2} = skip_data(Bin, Fd, NumChars),
	    yaws_call(DCC, LineNo, YawsFile, Mod, out, [ARG], GC, SC),
	    deliver_dyn_file(CliSock, GC, SC, Req, Head, UT, DCC, Bin2,Fd,T,ARG);
	{data, 0} ->
	    deliver_dyn_file(CliSock, GC, SC, Req, Head, UT,DCC, Bin, Fd,T,ARG);
	{data, NumChars} ->
	    {Send, Bin2} = skip_data(Bin, Fd, NumChars),
	    accumulate_chunk(DCC, Send),
	    deliver_dyn_file(CliSock, GC, SC, Req, Bin2, UT, DCC, Bin2, Fd, T,ARG);
	{error, NumChars, Str} ->
	    {_, Bin2} = skip_data(Bin, Fd, NumChars),
	    accumulate_chunk(DCC, Str),
	    deliver_dyn_file(CliSock, GC, SC, Req, Bin2, UT, DCC, Bin2, Fd, T,ARG)
    end;

deliver_dyn_file(CliSock, GC, SC, _Req, _Head,_UT, DCC, _Bin, _Fd, [], _ARG) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    ?Debug("deliver_dyn: done~n", []),
    Ret = case {DCC#dcc.chunked, get(status_code)} of
	      {false, _} ->
		  done;
	      {true, undefined} ->
		  accumulate_content([crnl(), "0", crnl2()]),
		  continue;
	      {true, 200} ->
		  accumulate_content([crnl(), "0", crnl2()]),
		  continue;
	      {true, _} ->
		  accumulate_header("Connection: close"),
		  done
	  end,
    deliver_accumulated(DCC, CliSock, GC, SC),
    Ret.


%% what about trailers ??

skip_data(List, Fd, Sz) when list(List) ->
    skip_data(list_to_binary(List), Fd, Sz);
skip_data(Bin, Fd, Sz) when binary(Bin) ->
    ?Debug("Skip data ~p bytes from", [Sz]),
     case  Bin of
	 <<Head:Sz/binary ,Tail/binary>> ->
	     {Head, Tail};
	 _ ->
	     case (catch file:read(Fd, 4000)) of
		 {ok, Bin2} when binary(Bin2) -> 
		     Bin3 = <<Bin/binary, Bin2/binary>>,
		     skip_data(Bin3, Fd, Sz);
		 _Err ->
		     ?Debug("EXIT in skip_data: ~p  ~p ~p~n", [Bin, Sz, _Err]),
		     exit(normal)
	     end
     end;
skip_data({bin, Bin}, _, Sz) ->
    ?Debug("Skip bin data ~p bytes ", [Sz]),
    <<Head:Sz/binary ,Tail/binary>> = Bin,
    {Head, {bin, Tail}};
skip_data({ok, X}, Fd, Sz) ->
    skip_data(X, Fd, Sz).



to_binary(B) when binary(B) ->
    B;
to_binary(L) when list(L) ->
    list_to_binary(L).



accumulate_header(Data) ->
    case get(acc_headers) of
	undefined ->
	    put(acc_headers, [Data, crnl()]);
	List ->
	    put(acc_headers, [List, Data, crnl()])
    end.

accumulate_content(Data) ->
    case get(acc_content) of
	undefined ->
	    put(acc_content, [Data]);
	List ->
	    put(acc_content, [List, Data])
    end.

accumulate_chunk(DCC, Data) ->
    if 
	DCC#dcc.chunked == false ->
	    accumulate_content(Data);
	DCC#dcc.chunked == true ->
	    B = to_binary(Data),
	    Len = size(B),
	    CRNL = crnl(),
	    Data2 = [CRNL, yaws:integer_to_hex(Len) , crnl(), B], 
	    accumulate_content(Data2)
    end.

set_status_code(Code) ->
    put(status_code, Code).



handle_out_reply(L, DCC, LineNo, YawsFile) when list (L) ->
    lists:foreach(
      fun(Item) -> handle_out_reply(Item, DCC, LineNo, YawsFile) end, L);

handle_out_reply({html, Html}, DCC, _LineNo, _YawsFile) ->
    accumulate_chunk(DCC, Html);

handle_out_reply({header, H}, _DCC, _LineNo, _YawsFile) ->
    accumulate_header(H);


handle_out_reply({allheaders, Hs}, _DCC, _LineNo, _YawsFile) ->
    erase(acc_headers),
    lists:foreach(fun({header, Head}) -> accumulate_header(Head) end, Hs);

handle_out_reply({status, Code}, _DCC,_LineNo,_YawsFile) when integer(Code) ->
    set_status_code(Code);

handle_out_reply({'EXIT', normal}, _DCC, _LineNo, _YawsFile) ->
    exit(normal);

handle_out_reply(ok, _DCC, _LineNo, _YawsFile) ->
    ok;

handle_out_reply({'EXIT', Err}, DCC, LineNo, YawsFile) ->
    L = ?F("~n<pre> ~nERROR erl  code  crashed:~n "
	   "File: ~s:~w~n"
	   "Reason: ~p~n</pre>~n",
	   [YawsFile, LineNo, Err]),
    yaws:elog("erl code crashed: ~n"
	      "File: ~s:~w~n"
	      "Reason: ~p", [YawsFile, LineNo, Err]),
    accumulate_chunk(DCC, L);


handle_out_reply(Reply, DCC, LineNo, YawsFile) ->
    yaws_log:sync_errlog("Bad return code from yaws function: ~p~n", [Reply]),
    L =  ?F("<p><br> <pre>yaws code at ~s:~p crashed or "
	    "ret bad val:~p ~n</pre>",
	    [YawsFile, LineNo, Reply]),
    accumulate_chunk(DCC, L).

			  


deliver_accumulated(DCC, Sock, GC, SC) ->
    Code = case get(status_code) of
	       undefined -> 200;
	       Int -> Int
	   end,
    StatusLine = ["HTTP/1.1 ", integer_to_list(Code), " ",
		  yaws_api:code_to_phrase(Code), crnl()],
    Headers = erase(acc_headers),
    Cont = case erase(acc_content) of
	       undefined ->
		   [];
	       Content ->
		   Content
	   end,
    CRNL = if
	       DCC#dcc.chunked == true ->
		   [];
	       DCC#dcc.chunked == false ->
		   crnl()
	   end,
    All = [StatusLine, Headers, CRNL, Cont],
    gen_tcp_send(Sock, All, SC, GC),
    if
	GC#gconf.trace == false ->
	    ok;
	GC#gconf.trace == {true, http} ->
	    yaws_log:trace_traffic(from_server, [StatusLine, Headers]);
	GC#gconf.trace == {true, traffic} ->
	    yaws_log:trace_traffic(from_server, All)
    end.

		

		    
yaws_call(DCC, LineNo, YawsFile, M, F, A, _GC, _SC) ->
    ?Debug("safe_call ~w:~w(~p)~n", 
	   [M, F, ?format_record(hd(A), arg)]),
    Res = (catch apply(M,F,A)),
    handle_out_reply(Res, DCC, LineNo, YawsFile).



do_tcp_close(Sock, SC) ->
    case SC#sconf.ssl of
	undefined ->
	    gen_tcp:close(Sock);
	_SSL ->
	    ssl:close(Sock)
    end.



ut_open(UT) ->
    case UT#urltype.data of
	undefined ->
	    {ok, Bin} = file:read_file(UT#urltype.fullpath),
	    {bin, Bin};
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
    DCC = static_do_close(Req, InH),
    make_static_headers(Req, UT, DCC),
    Fd = ut_open(UT),
    Bin = ut_read(Fd),
    close_if_HEAD(Req, fun() ->
			       deliver_accumulated(DCC, CliSock, GC,SC),
			       ut_close(Fd), throw({ok, 1}) 
		       end),
    accumulate_content(Bin),
    ut_close(Fd),
    deliver_accumulated(DCC, CliSock, GC, SC),
    if
	DCC#dcc.doclose == true ->
	    done;
	DCC#dcc.doclose == false ->
	    continue
    end.

close_if_HEAD(Req, F) ->
    if
	Req#http_request.method == 'HEAD' ->
	    F();
	true ->
	    ok
    end.


static_do_close(Req, H) ->
    Close = case Req#http_request.version of
		{1, 0} ->
		    case H#headers.keep_alive of
			undefined ->
			    true;
			_ ->
			    false
		    end;
		_ ->
		    false
	    end,
    #dcc{doclose = Close,
	 chunked = false}.


make_dyn_headers(DCC, Req, MimeType) ->
    make_date_header(),
    make_server_header(),
    make_content_type(MimeType),
    make_connection_close(DCC#dcc.doclose),
    make_non_cache_able(Req#http_request.version).


make_x_pad() ->
    accumulate_header("X-Pad: avoid browser bug").


make_non_cache_able({1, 0}) -> 
    accumulate_header("Cache-Control: no-cache"),
    accumulate_header("Pragma: no-cache");
make_non_cache_able({1,1}) -> 
    accumulate_header("Cache-Control: no-cache").


make_date_and_server_headers() ->
    make_date_header(),
    make_server_header().


make_static_headers(_Req, UT, DCC) ->    
    make_date_and_server_headers(),
    make_last_modified(UT#urltype.finfo),
    make_etag(UT#urltype.finfo),
    make_accept_ranges(),
    make_content_length(UT#urltype.finfo),
    make_content_type(UT#urltype.mime),
    make_connection_close(DCC#dcc.doclose).



crnl() ->
    "\r\n".
crnl2() ->
    "\r\n\r\n".



%% FIXME optimize the goddamn date generations
make_date_header() ->
    N = element(2, now()),
    Head=case get(date_header) of
	{Str, Secs} when (Secs+10) > N ->
	    H = ["Date: ", yaws:universal_time_as_string()],
	    put(date_header, {H, N}),
	    H;
	{Str, Secs} ->
	    Str;
	undefined ->
	    H = ["Date: ", yaws:universal_time_as_string()],
	    put(date_header, {H, N}),
	    H
    end,
    accumulate_header(Head).


make_server_header() ->
    accumulate_header(["Server: Yaws/", yaws_vsn:version(), 
		       " Yet Another Web Server"]).

make_last_modified(FI) ->
    N = element(2, now()),
    Inode = FI#file_info.inode,  %% unix only
    H=case get({last_modified, Inode}) of
	{Str, Secs} when N < (Secs+10) ->
	    Str;
	_ ->
	    S = do_make_last_modified(FI),
	    put({last_modified, Inode}, {S, N}),
	    S
    end,
    accumulate_header(H).

do_make_last_modified(FI) ->
    Then = FI#file_info.mtime,
    ["Last-Modified: ", yaws:local_time_as_gmt_string(Then)].

make_etag(FI) ->
    {{_Y,M,D}, {H,Min, S}}  = FI#file_info.mtime,
    Inode = FI#file_info.inode,
    accumulate_header(["Etag: ", pack_ints([M, D, H, Min, S, Inode])]).


make_accept_ranges() ->
    accumulate_header("Accept-Ranges: bytes").
make_content_length(Size) when integer(Size) ->
    put(content_length, Size),
    accumulate_header(["Content-Length: ", integer_to_list(Size)]);
make_content_length(FI) ->
    Size = FI#file_info.size,
    put(content_length, Size),
    accumulate_header(["Content-Length: ", integer_to_list(Size)]).
make_content_type() ->
    make_content_type("text/html").
make_content_type(MimeType) ->
    accumulate_header(["Content-Type: ", MimeType]).


make_connection_close(true) ->
    accumulate_header("Connection: close");
make_connection_close(false) ->
    ok.

make_chunked() ->
    accumulate_header("Transfer-Encoding: chunked").

make_www_authenticate(Realm) ->
    accumulate_header(["WWW-Authenticate: Basic realm=\"", Realm, $"]).



pack_ints(L) ->
    [$" | pack_ints2(L) ].


pack_ints2([0|T]) ->
    pack_ints2(T);
pack_ints2([H|T]) ->
    X = H band 2#11111,
    Val = X + $A,
    V2 = if 
	     Val > $Z, Val < $a ->
		 Val + ($a-$Z);
	     true ->
		 Val
	 end,
    [V2 |pack_ints2([H bsr 5|T])];
pack_ints2([]) ->
    [$"].

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
	    Refresh = GC#gconf.cache_refresh_secs,
	    if 
		((N-When) > Refresh) ->
		    ?Debug("Timed out entry for ~s ~p~n", [Path, {When, N}]),
		    %% more than 30 secs old entry
		    ets:delete(E, {url, Path}),
		    ets:delete(E, {urlc, Path}),
		    ets:update_counter(E, num_files, -1),
		    ets:update_counter(E, num_bytes, -FI#file_info.size),
		    url_type(GC, SC, Path);
		true ->
		    ?Debug("Serve page from cache ~p", [{When , N, N-When}]),
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


cache_file(GC, SC, Path, UT) when 
  UT#urltype.type == regular ;
  UT#urltype.type == yaws  ->

    E = SC#sconf.ets,
    [{num_files, N}] = ets:lookup(E, num_files),
    [{num_bytes, B}] = ets:lookup(E, num_bytes),
    FI = UT#urltype.finfo,
    if
	N + 1 > GC#gconf.max_num_cached_files ->
	    error_logger:info_msg("Max NUM cached files reached for server "
			      "~p", [SC#sconf.servername]),
	    cleanup_cache(E, num),
	    cache_file(GC, SC, Path, UT);
	B + FI#file_info.size > GC#gconf.max_num_cached_bytes ->
	    error_logger:info_msg("Max size cached bytes reached for server "
			      "~p", [SC#sconf.servername]),
	    cleanup_cache(E, size),
	    cache_file(GC, SC, Path, UT);
	true ->
	    if
		FI#file_info.size > GC#gconf.max_size_cached_file ->
		    UT;
		true ->
		    {ok, Bin} = prim_file:read_file(UT#urltype.fullpath),
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
	redir_dir ->
	    #urltype{type = redir_dir,
		     dir = [Path, $/]};
	OK ->
	    OK
    end.


maybe_return_dir(DR, FlatPath) ->
    ?Debug("maybe_return_dir(~p, ~p)", [DR, FlatPath]),
    case prim_file:read_file_info([DR, FlatPath, "/index.yaws"]) of
	{ok, FI} ->
	    #urltype{type = yaws,
		     finfo = FI,
		     mime = "text/html",
		     dir = FlatPath,
		     fullpath = ?f([DR, FlatPath,"/index.yaws"])};
	_ ->
	    case prim_file:read_file_info([DR, FlatPath, "/index.html"]) of
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
    case prim_file:read_file_info(L) of
	{ok, FI} when FI#file_info.type == regular ->
	    {X, Mime} = suffix_type(RevFile),
	    #urltype{type=X, 
		     finfo=FI,
		     dir = FlatDir,
		     fullpath = lists:flatten(L),
		     mime=Mime, q=Query};
	{ok, FI} when FI#file_info.type == directory, hd(RevFile) == $/ ->
	    maybe_return_dir(DR, lists:flatten(Dir) ++ File);
	{ok, FI} when FI#file_info.type == directory, hd(RevFile) /= $/ ->
	    redir_dir;
	{error, enoent} ->
	    %% kind of hackish, defer url decode 
	    Dir2 = lists:flatmap(fun(X) -> yaws_api:url_decode(X) end, Dir),
	    File2 = yaws_api:url_decode(File),
	    L2 = [DR, Dir2, File2],
	    case prim_file:read_file_info(L) of
		{ok, FI} when  FI#file_info.type == regular ->
		    {X, Mime} = suffix_type(RevFile),
		    #urltype{type=X, 
			     finfo=FI,
			     dir = Dir2,
			     fullpath = lists:flatten(L2),
			     mime=Mime, q=Query};
		{ok, FI} when FI#file_info.type == directory ->
		    maybe_return_dir(DR, lists:flatten(Dir2) ++ File2);
		Err ->
		    #urltype{type=error, data=Err}
	    end;
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


runmod(Mod) ->
    spawn(fun() -> load_and_run(Mod) end).

load_and_run(Mod) ->
    case code:ensure_loaded(Mod) of
	{module,Mod} ->
	    Mod:start();
	Error ->
	    yaws_log:errlog("Loading '~w' failed, reason ~p~n",[Mod,Error])
    end.

