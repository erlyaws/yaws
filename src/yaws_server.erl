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

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include("yaws_debug.hrl").

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


-record(urltype, {type,   %% error | yaws | regular | directory | dotdot|appmod
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
		{ok, Val}  ->
		    Val
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
    Embed = case application:get_env(yaws, embedded) of
		undefined ->
		    false;
		{ok, Val0} ->
		    Val0
	    end,
    {Debug, Trace, Conf, RunMod, Embed}.

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
    {Debug, Trace, Conf, RunMod, Embed} = get_app_args(),
    case Embed of 
	false ->
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
		    init2(Gconf, Sconfs, RunMod, true);
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
	    end;
	true ->
	    init2(yaws_config:make_default_gconf(Debug), [], undef, true)
    end.


init2(Gconf, Sconfs, RunMod, FirstTime) ->
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
    io:format("L=~p~n", [L]),
    if
	length(L) == length(L2) ->
	    if
		FirstTime == true ->
		    proc_lib:spawn_link(yaws_ctl, start, 
					[self(), Gconf#gconf.uid]);
		true ->
		    ok
	    end,
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
    {reply, Mnum+1,   {GC, Group, Mnum+1}};

handle_call({setconf, GC, Groups}, _From, State) ->
    %% First off, terminate all currently running processes
    Curr = lists:map(fun(X) ->element(1, X) end, element(2, State)),
    lists:foreach(fun(Pid) ->
			  Pid ! {self(), stop},
			  receive
			      {Pid, ok} ->
				  ok
			  end
		  end, Curr),
    case init2(GC, Groups, undef, false) of
	{ok, State2} ->
	    {reply, ok, State2};
	Err ->
	    {reply, Err, {GC, [], 0}}
    end.
			      



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
%% specified as an auth directory. These are merged with server conf.

setup_auth(SC) ->
    ?f(lists:map(fun(Auth) ->
		      add_yaws_auth(Auth#auth.dir, Auth)
	      end, SC#sconf.authdirs)).

add_yaws_auth(Dirs, A) ->
    lists:map(
      fun(Dir) ->
	  case file:consult([Dir, [$/|".yaws_auth"]]) of
	      {ok, [{realm, Realm} |TermList]} ->
		  {Dir, A#auth{realm = Realm,
			       users = TermList++A#auth.users}};
	      {ok, TermList} ->
		  {Dir, A#auth{users = TermList++A#auth.users}};
	      {error, enoent} ->
		  {Dir, A};
	      _Err ->
		  error_logger:format("Bad .yaws_auth file in dir ~p~n", [Dir]),
		  {Dir, A}
	  end
      end, Dirs).


do_listen(SC) ->
    case SC#sconf.ssl of
	undefined ->
	    {nossl, gen_tcp:listen(SC#sconf.port, opts(SC))};
	SSL ->
	    {ssl, ssl:listen(SC#sconf.port, ssl_opts(SC, SSL))}
    end.

set_writeable(Dir) ->
    {ok, FI} = file:read_file_info(Dir),
    Mode = 8#777,
    file:write_file_info(Dir, FI#file_info{mode = Mode}).


%% One server per IP we listen to
gserv(GC, Group0) ->
    process_flag(trap_exit, true),
    ?TC([{record, GC, gconf}]),
    Group = map(fun(SC) -> 
			E = ets:new(yaws_code, [public, set]),
			ets:insert(E, {num_files, 0}),
			ets:insert(E, {num_bytes, 0}),
			Auth = setup_auth(SC),
			SC#sconf{ets = E,
				 authdirs = Auth}
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
	    set_writeable("/tmp/yaws"),
	    Tdir = "/tmp/yaws/" ++ GC#gconf.uid,
	    file:make_dir(Tdir),
	    Files = case file:list_dir(Tdir) of
			{ok, Files0} ->
			    Files0;
			{error, Reason} ->
			    error_logger:format("Failed to list ~p probably"
						"due to permission errs: ~p",
						[Tdir, Reason]),
			    proc_lib:init_ack({error, "Can't list dir " 
					       ++ Tdir}),
			    exit(normal)
		    end,
	    lists:foreach(
	      fun(F) -> file:delete(Tdir ++ "/" ++ F) end, Files),
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
	    end;
	{'EXIT', _Pid, _} ->
	    gserv(GS, Ready, Rnum);
	{From, stop} ->   
	    unlink(From),
	    {links, Ls} = process_info(self(), links),
	    lists:foreach(fun(X) -> unlink(X), exit(X, kill) end, Ls),
	    From ! {self(), ok},
	    exit(normal)
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
	    Sconf = pick_sconf(GS#gs.gconf, H, GS#gs.group),
	    ?Debug("Sconf: ~p", [?format_record(Sconf, sconf)]),
	    ?TC([{record, Sconf, sconf}]),
	    ?Debug("Headers = ~p~n", [?format_record(H, headers)]),
	    ?Debug("Request = ~p~n", [?format_record(Req, http_request)]),
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



pick_sconf(GS, H, Group) ->
    case H#headers.host of
	undefined ->
	    hd(Group);
	Host ->
	    pick_host(GS, Host, Group, Group)
    end.


pick_host(_GC, Host, [H|_T], _Group) when H#sconf.servername == Host ->
    H;
pick_host(GC, Host, [_|T], Group) ->
    pick_host(GC, Host, T, Group);
pick_host(GC, Host, [], Group) ->
    H = hd(Group),
    yaws_debug:dinfo(GC, "Got Host: header ~p which didn't match any host "
		     "field in config, ~npicking the first one : ~p~n ",
		     [Host, H#sconf.servername]),
    H.

inet_peername(Sock, SC) ->
    case SC#sconf.ssl of
	undefined ->
	    inet:peername(Sock);
	_SSL ->
	    ssl:peername(Sock)
    end.
	    

maybe_access_log(CliSock, SC, Req) ->
    ?TC([{record, SC, sconf}]),
    Status = case erase(status_code) of
		 undefined -> "-";
		 I -> integer_to_list(I)
	     end,
    Len = case erase(content_length) of
	      undefined ->
		  "-";
	      I2 -> integer_to_list(I2)
	  end,
    case SC#sconf.access_log of
	true ->
	    {ok, {Ip, _Port}} = inet_peername(CliSock, SC),
	    Path = get_path(Req#http_request.path),
	    Meth = atom_to_list(Req#http_request.method),
	    yaws_log:accesslog(SC#sconf.servername, Ip, 
			       [Meth, $\s, Path] , Status, Len);
	false ->
	    ignore
    end.


get_path({abs_path, Path}) ->
%    io:format("Path = ~p\n", [Path]),
%    P = yaws_api:url_decode(Path),
%    io:format("P = ~p\n", [P]),
%    P.
    yaws_api:url_decode(Path).

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
			      [strip(Data), S, Err]),
	    exit(normal)
    end.


strip(Data) ->
    L = list_to_binary([Data]),
    case L of
	<<Head:50/binary, _/binary>> ->
	    <<Head/binary, ".....">>;
	_ ->
	    L
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
    ?Debug("GET ~p", [?format_record(Req, http_request)]),
    ok = inet_setopts(SC, CliSock, [{packet, raw}, binary]),
    flush(SC, CliSock, Head#headers.content_length),
    ARG = make_arg(CliSock, Head, Req, GC, SC),
    handle_request(CliSock, GC, SC, Req, Head, ARG, 0).


'POST'(CliSock, GC, SC, Req, Head) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    ?Debug("POST Req=~p H=~p~n", [?format_record(Req, http_request),
				  ?format_record(Head, headers)]),
    ok = inet_setopts(SC, CliSock, [{packet, raw}, binary]),
    PPS = SC#sconf.partial_post_size,
    Bin = case Head#headers.content_length of
	      undefined ->
		  case Head#headers.connection of
		      "close" ->
			  get_client_data(CliSock, all, GC,
					  is_ssl(SC#sconf.ssl));
		      _ ->
			  ?Debug("No content length header ",[]),
			  exit(normal)
		  end;
	      Len when integer(PPS) ->
		  Int_len = list_to_integer(Len),
		  if 
		      Int_len == 0 ->
			  <<>>;
		      PPS < Int_len ->
			  {partial, get_client_data(CliSock, PPS, GC,
						    is_ssl(SC#sconf.ssl))};
		      true ->
			  get_client_data(CliSock, Int_len, GC, 
					  is_ssl(SC#sconf.ssl))
		  end;
	      Len when PPS == nolimit ->
		  Int_len = list_to_integer(Len),
		  if
		      Int_len == 0 ->
			  <<>>;
		      true ->
			  get_client_data(CliSock, Int_len, GC, 
					  is_ssl(SC#sconf.ssl))
		  end
	  end,
    ?Debug("POST data = ~s~n", [binary_to_list(un_partial(Bin))]),
    ARG = make_arg(CliSock, Head, Req, GC, SC),
    ARG2 = ARG#arg{clidata = Bin},
    handle_request(CliSock, GC, SC, Req, Head, ARG2, size(un_partial(Bin))).


is_ssl(undefined) ->
    nossl;
is_ssl(R) when record(R, ssl) ->
    ssl.

un_partial({partial, Bin}) ->
    Bin;
un_partial(Bin) ->
    Bin.

%% will throw
'HEAD'(CliSock, GC, SC, Req, Head) ->
    'GET'(CliSock, GC, SC, Req, Head).

'TRACE'(_CliSock, _GC, _SC, _Req, _Head) ->
    nyi.

'OPTIONS'(CliSock, GC, SC, Req, Head) ->
    ?Debug("OPTIONS", []),
    ok = inet_setopts(SC, CliSock, [{packet, raw}, binary]),
    flush(SC, CliSock, Head#headers.content_length),
    _ARG = make_arg(CliSock, Head, Req, GC, SC),
    ?Debug("OPTIONS delivering", []),
    deliver_options(CliSock, Req, GC, SC).

make_arg(CliSock, Head, Req, _GC, SC) ->
    ?TC([{record, _GC, gconf}, {record, SC, sconf}]),
    #arg{clisock = CliSock,
	 headers = Head,
	 req = Req,
	 pid = self(),
	 docroot = SC#sconf.docroot}.

handle_request(CliSock, GC, SC, Req, H, ARG, N) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    UT =  url_type(GC, SC, get_path(Req#http_request.path)),
    ARG2 = ARG#arg{fullpath=UT#urltype.fullpath},
    case SC#sconf.authdirs of
	[] ->
	    handle_ut(CliSock, GC, SC, Req, H, ARG2, UT, N);
	_Adirs ->
	    %% we have authentication enabled, check auth
	    UT2 = unflat(UT),
	    case is_authenticated(SC, UT2, Req, H) of
		true ->
		    handle_ut(CliSock, GC, SC, Req, H, ARG2, UT2, N);
		{false, Realm} ->
		    deliver_401(CliSock, Req, GC, Realm, SC)
	    end
    end.


unflat(U) ->
    U2 = case U#urltype.path of
	     {noflat, F} ->
		 U#urltype{path = lists:flatten(F)};
	     _ ->
		 U
	 end,
    U2.

is_authdir(Req_dir, [{Auth_dir, Auth}|T]) ->
    case lists:prefix(Auth_dir, Req_dir) of
	true ->
	    {true, Auth};
	false ->
	    is_authdir(Req_dir, T)
    end;
is_authdir(_, []) ->
    false.

is_authenticated(SC, UT, _Req, H) ->
    case is_authdir(UT#urltype.path, SC#sconf.authdirs) of
	{true, #auth{dir = _Dir, realm = Realm, users = Users}} ->
	    case H#headers.authorization of
		undefined ->
		    {false, Realm};
		{User, Password} ->
		    case lists:member({User, Password}, Users) of
			true ->
			    true;
			false ->
			    {false, "Got username"}
		    end
	    end;
	false ->
	    true
    end.


handle_ut(CliSock, GC, SC, Req, H, ARG, UT, N) ->
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
		    ARG#arg{querydata = UT#urltype.q}, UT, N);
	dotdot ->
	    deliver_403(CliSock, Req, GC, SC);
	redir_dir ->
	     deliver_303(CliSock, Req, GC, SC);
	appmod ->
	    {Mod, PathData} = UT#urltype.data,
	    A2 = ARG#arg{appmoddata = PathData,
			 querydata = UT#urltype.q},
	    do_appmod(Mod, CliSock, GC, SC, Req, H, A2, UT, N)
    end.

	
parse_auth("Basic " ++ Auth64) ->
    case decode_base64(Auth64) of
	{error, _Err} ->
	    undefined;
	Auth ->
	    case string:tokens(Auth, ":") of
		[User, Pass] ->
		    {User, Pass};
		_ ->
		    undefined
	    end
    end;
parse_auth(_) ->
    undefined.







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
    accumulate_header(["Location: ", Scheme,
		       servername_sans_port(SC#sconf.servername), 
		       PortPart, get_path(Req#http_request.path), "/"]),
    
    deliver_accumulated(#dcc{}, CliSock, GC, SC),
    done.

servername_sans_port(Servername) ->
    case string:chr(Servername, $:) of
	0 ->
	    Servername;
	N ->
	    lists:sublist(Servername, N-1)
    end.

deliver_options(CliSock, _Req, GC, SC) ->
    set_status_code(200),
    make_date_and_server_headers(),
    make_allow_header(),
    B = list_to_binary(""),
    make_content_length(size(B)),
    accumulate_content(B),
    make_content_type(),
    deliver_accumulated(#dcc{}, CliSock, GC, SC),
    continue.

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


do_yaws(CliSock, GC, SC, Req, H, ARG, UT, N) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    FileAtom = list_to_atom(UT#urltype.fullpath),
    Mtime = mtime(UT#urltype.finfo),
    case ets:lookup(SC#sconf.ets, FileAtom) of
	[{FileAtom, spec, Mtime1, Spec, Es}] when Mtime1 == Mtime,
						  Es == 0 ->
	    deliver_dyn_file(CliSock, GC, SC, Req, H, Spec, ARG, UT, N);
	Other  ->
	    del_old_files(Other),
	    {ok, [{errors, Errs}| Spec]} = 
		yaws_compile:compile_file(UT#urltype.fullpath, GC, SC),
	    ?Debug("Spec for file ~s is:~n~p~n",[UT#urltype.fullpath, Spec]),
	    ets:insert(SC#sconf.ets, {FileAtom, spec, Mtime, Spec, Errs}),
	    deliver_dyn_file(CliSock, GC, SC, Req, H, Spec, ARG, UT, N)
    end.


del_old_files([]) ->
    ok;
del_old_files([{_FileAtom, spec, _Mtime1, Spec, _}]) ->
    lists:foreach(
      fun({mod, _, _, _,  Mod, _Func}) ->
	      F="/tmp/yaws/" ++ yaws:to_list(Mod) ++ ".erl",
	      code:purge(Mod),
	      code:purge(Mod),
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



do_appmod(Mod, CliSock, GC, SC, Req, H, ARG, UT, N) ->
    DCC = req_to_dcc(Req),
    case yaws_call(DCC, 0, "appmod", Mod, out, [ARG], GC,SC, N) of
	{streamcontent, MimeType, FirstChunk} ->
	    put(content_type, MimeType),
	    accumulate_chunk(DCC, FirstChunk),
	    set_status_code(200),
	    deliver_accumulated(DCC, CliSock, GC, SC),
	    stream_loop(DCC, CliSock, GC, SC),
	    case DCC#dcc.chunked of
		true ->
		    gen_tcp_send(CliSock, [crnl(), "0", crnl2()], SC, GC),
		    continue;
		false ->
		    done
	    end;
	_ ->
	    %% finish up
	    deliver_dyn_file(CliSock, GC,SC,Req, H,UT,DCC, [], [], [], ARG,N)
    end.



req_to_dcc(Req) ->
    {DoClose, Chunked} = case Req#http_request.version of
			     {1, 0} -> {true, false};
			     {1, 1} -> make_chunked(), 
				       {false, true}
			 end,
    DCC = #dcc{doclose = DoClose,
	       chunked = Chunked}.

%% do the header and continue
deliver_dyn_file(CliSock, GC, SC, Req, Head, Specs, ARG, UT, N) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    Fd = ut_open(UT),
    Bin = ut_read(Fd),
    DCC = req_to_dcc(Req),
    make_dyn_headers(DCC, Req),
    close_if_HEAD(Req, fun() ->
			       deliver_accumulated(DCC, CliSock,GC,SC),
			       do_tcp_close(CliSock, SC), 
			       throw({ok, 1}) 
		       end),
    deliver_dyn_file(CliSock, GC, SC, Req, Head, UT, DCC, Bin, Fd, Specs, ARG, N).



deliver_dyn_file(CliSock, GC, SC, Req, Head, UT, DCC, Bin, Fd, [H|T],ARG,N) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}, {record, UT,urltype}]),
    ?Debug("deliver_dyn_file: ~p~n", [H]),
    case H of
	{mod, LineNo, YawsFile, NumChars, Mod, out} ->
	    {_, Bin2} = skip_data(Bin, Fd, NumChars),
	    case yaws_call(DCC, LineNo, YawsFile, Mod, out, [ARG], GC,SC, N) of
		break ->
		    deliver_dyn_file(CliSock, GC, SC, Req, Head, 
				     UT, DCC, Bin, Fd, [],ARG,N) ;

		{streamcontent, MimeType, FirstChunk} ->
		    put(content_type, MimeType),
		    accumulate_chunk(DCC, FirstChunk),
		    set_status_code(200),
		    deliver_accumulated(DCC, CliSock, GC, SC),
		    stream_loop(DCC, CliSock, GC, SC),
		    case DCC#dcc.chunked of
			true ->
			    gen_tcp_send(CliSock, [crnl(), "0", crnl2()], SC, GC),
			    continue;
			false ->
			    done
		    end;
		    
		_ ->
		    deliver_dyn_file(CliSock, GC, SC, Req, Head, 
				     UT, DCC, Bin2,Fd,T,ARG,0)
	    end;
	{data, 0} ->
	    deliver_dyn_file(CliSock, GC, SC, Req, Head, UT,DCC, Bin, Fd,T,ARG,N);
	{data, NumChars} ->
	    {Send, Bin2} = skip_data(Bin, Fd, NumChars),
	    accumulate_chunk(DCC, Send),
	    deliver_dyn_file(CliSock, GC, SC, Req, Bin2, UT, DCC, Bin2, Fd, T,ARG,N);
	{error, NumChars, Str} ->
	    {_, Bin2} = skip_data(Bin, Fd, NumChars),
	    accumulate_chunk(DCC, Str),
	    deliver_dyn_file(CliSock, GC, SC, Req, Bin2, UT, DCC, Bin2, Fd, T,ARG,N)
    end;

deliver_dyn_file(CliSock, GC, SC, _Req, _Head,_UT, DCC, _Bin, _Fd, [], _ARG,_N) ->
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
    case erase(content_type) of
	undefined ->
	    make_content_type("text/html");
	Other ->
	    make_content_type(Other)
    end,
    
    deliver_accumulated(DCC, CliSock, GC, SC),
    Ret.






stream_loop(DCC, CliSock, GC, SC) ->
    receive
	{streamcontent, Cont} ->
	    accumulate_chunk(DCC, Cont),
	    gen_tcp_send(CliSock, erase(acc_content), SC, GC),
	    stream_loop(DCC, CliSock, GC, SC) ;
	endofstreamcontent ->
	    ok
    end.



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
	    case size(B) of
		0 ->
		    skip;
		Len ->
		    CRNL = crnl(),
		    Data2 = [CRNL, yaws:integer_to_hex(Len) , crnl(), B], 
		    accumulate_content(Data2)
	    end
    end.

set_status_code(Code) ->
    put(status_code, Code).



handle_out_reply(L, DCC, LineNo, YawsFile, SC, A) when list (L) ->
    handle_out_reply_l(L, DCC, LineNo, YawsFile, SC, A, undefined);

handle_out_reply({html, Html}, DCC, _LineNo, _YawsFile, _SC, _A) ->
    accumulate_chunk(DCC, Html);

handle_out_reply({ehtml, E}, DCC, _LineNo, _YawsFile, _SC, _A) ->
    accumulate_chunk(DCC, safe_ehtml_expand(E));

handle_out_reply({content, MimeType, Cont}, DCC, _LineNo,_YawsFile, _SC, _A) ->
    put(content_type, MimeType),
    accumulate_chunk(DCC, Cont);

handle_out_reply({streamcontent, MimeType, First}, 
		 _DCC, _LineNo,_YawsFile, _SC, _A) ->
    {streamcontent, MimeType, First};


handle_out_reply({header, H}, _DCC, _LineNo, _YawsFile, _SC, _A) ->
    accumulate_header(H);

handle_out_reply({allheaders, Hs}, _DCC, _LineNo, _YawsFile, _SC, _A) ->
    erase(acc_headers),
    lists:foreach(fun({header, Head}) -> accumulate_header(Head) end, Hs);

handle_out_reply({status, Code}, _DCC,_LineNo,_YawsFile, _SC, _A) when integer(Code) ->
    set_status_code(Code);

handle_out_reply({'EXIT', normal}, _DCC, _LineNo, _YawsFile, _SC, _A) ->
    exit(normal);

handle_out_reply(break, _DCC, _LineNo, _YawsFile, _SC, _A) ->
    break;

handle_out_reply({redirect_local, Path}, _DCC, _LineNo, _YawsFile, SC, A) ->
    erase(acc_headers),
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

    set_status_code(redirect_code(A)),
    make_content_length(0),
    accumulate_header(["Location: ", Scheme,
		       servername_sans_port(SC#sconf.servername), 
		       PortPart, Path]),
    ok;

handle_out_reply({redirect, URL}, _DCC, _LineNo, _YawsFile, _SC, A) ->
    erase(acc_headers),
    set_status_code(redirect_code(A)),
    make_content_length(0),
    accumulate_header(["Location: ", URL]),
    ok;

handle_out_reply(ok, _DCC, _LineNo, _YawsFile, _SC, _A) ->
    ok;

handle_out_reply({'EXIT', Err}, DCC, LineNo, YawsFile, _SC, _A) ->
    L = ?F("~n<pre> ~nERROR erl  code  crashed:~n "
	   "File: ~s:~w~n"
	   "Reason: ~p~n</pre>~n",
	   [YawsFile, LineNo, Err]),
    yaws:elog("erl code crashed: ~n"
	      "File: ~s:~w~n"
	      "Reason: ~p", [YawsFile, LineNo, Err]),
    accumulate_chunk(DCC, L);

handle_out_reply({get_more, Cont, State}, _DCC, _LineNo, _YawsFile, _SC, _A) ->
    {get_more, Cont, State};

handle_out_reply(Reply, DCC, LineNo, YawsFile, _SC, _A) ->
    yaws_log:sync_errlog("Bad return code from yaws function: ~p~n", [Reply]),
    L =  ?F("<p><br> <pre>yaws code at ~s:~p crashed or "
	    "ret bad val:~p ~n</pre>",
	    [YawsFile, LineNo, Reply]),
    accumulate_chunk(DCC, L).

    

handle_out_reply_l([Reply|T], DCC, LineNo, YawsFile, SC, A, Res) ->		  
    case handle_out_reply(Reply, DCC, LineNo, YawsFile, SC, A) of
	{get_more, Cont, State} ->
	    handle_out_reply_l(T, DCC, LineNo, YawsFile, SC, A, {get_more, Cont, State});
	break ->
	    break;
	_ ->
	    handle_out_reply_l(T, DCC, LineNo, YawsFile, SC, A, Res)
    end;
handle_out_reply_l([], _DCC, _LineNo, _YawsFile, _SC, _A, Res) ->
    Res.

%% Get redirect reply code.
%% HTTP/1.0 clients (e.g. Netscape 4, aka The Last Simple Browser)
%% don't understand 303, so we send them 302. This is suggested by
%% the HTTP 1.1 RFC (2616, page 62). -luke
redirect_code(A) ->
    #arg{req=Req} = hd(A),
    case Req#http_request.version of
	{1,0} -> 302;
	_     -> 303
    end.

check_headers(L) ->
    Hs = string:tokens(lists:flatten(L), "\r\n"),
    io:format("XX ~p~n", [Hs]),
    lists:foreach(
      fun(H) ->
	  case lists:reverse(H) of
	      [_,_,$\r|_] ->
		  yaws_log:errlog("Bad header ~p, it contains"
					      " '\\r' or '\\n' at end ", 
					      [lists:flatten(H)]),
		  exit(normal);
	      [_,_,$\n|_] ->
		  yaws_log:errlog("Bad header ~p, it contains"
					      " '\\r' or '\\n' at end ", 
					      [lists:flatten(H)]),
		  exit(normal);
	      _ ->
		  ok
	  end
      end, Hs).


check_headers([$\r, $\n |Tail], Last) when Tail /= [] ->
    case lists:member(hd(Tail), [$\r, $\n]) of
	true ->
	     yaws_log:errlog("Bad header ~p, it contains"
			     " '\\r' or '\\n' at end ", [lists:reverse(Last)]),
	    exit(normal);
	_ ->
	    check_headers(Tail, [])
    end;

check_headers([H|T], Last) ->
    check_headers(T, [H|Last]);
check_headers([], _) ->
    ok.


deliver_accumulated(DCC, Sock, GC, SC) ->
    Code = case get(status_code) of
	       undefined -> 200;
	       Int -> Int
	   end,
    StatusLine = ["HTTP/1.1 ", integer_to_list(Code), " ",
		  yaws_api:code_to_phrase(Code), crnl()],
    Headers = erase(acc_headers),

    if
	GC#gconf.debug == true ->
	    check_headers(lists:flatten(Headers), []);
	true ->
	    ok
    end,

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

		

		    
yaws_call(DCC, LineNo, YawsFile, M, F, A, GC, SC, N) ->
%    ?Debug("safe_call ~w:~w(~p)~n", 
%	   [M, F, ?format_record(hd(A), arg)]),
    ?Debug("safe_call ~w:~w~n",[M,F]),
    Res = (catch apply(M,F,A)),
    ?Debug("safe_call result = ~p~n",[Res]),
    A1 = hd(A),
    case handle_out_reply(Res, DCC, LineNo, YawsFile, SC, A) of
	{get_more, Cont, State} when element(1, A1#arg.clidata) == partial  ->
	    More = get_more_post_data(SC#sconf.partial_post_size, N, SC, GC, A1),
	    case un_partial(More) of
		Bin when binary(Bin) ->
		    A2 = A1#arg{clidata = More,
				cont = Cont,
				state = State},
		    yaws_call(DCC, LineNo, YawsFile, M, F,
			      [A2], GC, SC, N+size(un_partial(More)));
		Err ->
		    A2 = A1#arg{clidata = Err,
				cont = undefined,
				state = State},
		    catch apply(M,F,[A2]),
		    exit(normal)
	    end;
	Else ->
	    Else
    end.

get_more_post_data(PPS, N, SC, GC, ARG) ->
    Len = list_to_integer((ARG#arg.headers)#headers.content_length),
    if N + PPS < Len ->
	    case cli_recv(ARG#arg.clisock, PPS, GC, is_ssl(SC#sconf.ssl)) of
		{ok, Bin} ->
		    {partial, Bin};
		Else ->
		    {error, Else}
	    end;
       true ->
	    case cli_recv(ARG#arg.clisock, Len - N, GC, is_ssl(SC#sconf.ssl)) of
		{ok, Bin} ->
		    Bin;
		Else ->
		    {error, Else}
	    end
    end.
    

do_tcp_close(Sock, SC) ->
    case SC#sconf.ssl of
	undefined ->
	    gen_tcp:close(Sock);
	_SSL ->
	    ssl:close(Sock)
    end.



ut_open(UT) ->
    ?Debug("ut_open() UT.fullpath = ~p~n", [UT#urltype.fullpath]),
    case UT#urltype.data of
	undefined ->
	    
	    ?Debug("ut_open reading\n",[]),
	    {ok, Bin} = file:read_file(UT#urltype.fullpath),
	    ?Debug("ut_open read ~p\n",[size(Bin)]),
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
    if
	binary(UT#urltype.data) ->
	    %% cached
	    deliver_small_file(CliSock, GC, SC, Req, InH, UT);
	true ->
	    case (UT#urltype.finfo)#file_info.size of
		N when N < GC#gconf.large_file_chunk_size ->
		    deliver_small_file(CliSock, GC, SC, Req, InH, UT);
		_ ->
		    deliver_large_file(CliSock, GC, SC, Req, InH, UT)
	    end
    end.

deliver_small_file(CliSock, GC, SC, Req, InH, UT) ->
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

deliver_large_file(CliSock, GC, SC, Req, InH, UT) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}, {record, UT, urltype}]),
    DCC = static_do_close(Req, InH),
    make_static_headers(Req, UT, DCC),
    close_if_HEAD(Req, fun() ->
			       deliver_accumulated(DCC, CliSock, GC,SC),
			       throw({ok, 1}) 
		       end),
    deliver_accumulated(DCC, CliSock, GC, SC),
    {ok,Fd} = file:open(UT#urltype.fullpath, [raw, read]),
    send_file(CliSock, Fd, DCC, SC, GC),
    if
	DCC#dcc.doclose == true ->
	    done;
	DCC#dcc.doclose == false ->
	    continue
    end.

send_file(CliSock, Fd, DCC, SC, GC) ->
    case file:read(Fd, GC#gconf.large_file_chunk_size) of
	{ok, Bin} ->
	    send_file_chunk(Bin, CliSock, DCC, SC, GC),
	    send_file(CliSock, Fd, DCC, SC, GC);
	eof ->
	    file:close(Fd),
	    if
		DCC#dcc.chunked == true ->
		    gen_tcp_send(CliSock, [crnl(), "0", crnl2()], SC, GC),
		    done;
		true ->
		    done
	    end
    end.

send_file_chunk(Bin, CliSock, DCC, SC, GC) ->
    if
	DCC#dcc.chunked == false ->
	    gen_tcp_send(CliSock, Bin, SC, GC);
	DCC#dcc.chunked == true ->
	    CRNL = crnl(),
	    Data2 = [CRNL, yaws:integer_to_hex(size(Bin)) , crnl(), Bin],
	    gen_tcp_send(CliSock, Data2, SC, GC)
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


make_dyn_headers(DCC, Req) ->
    make_date_header(),
    make_server_header(),
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


make_allow_header() ->
    accumulate_header(["Allow: GET, POST, PUT, OPTIONS, HEAD"]).

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
    [$"]. %"

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
	    UT = do_url_type(SC, Path),
	    ?TC([{record, UT, urltype}]),
	    ?Debug("UT=~p\n", [UT]),
	    CF = cache_file(GC, SC, Path, UT),
	    ?Debug("CF=~p\n", [CF]),
	    CF;
	[{_, When, UT}] ->
	    N = now_secs(),
	    FI = UT#urltype.finfo,
	    Refresh = GC#gconf.cache_refresh_secs,
	    if 
		((N-When) >= Refresh) ->
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
    ?Debug("FI=~p\n", [FI]),
    if
	N + 1 > GC#gconf.max_num_cached_files ->
	    error_logger:info_msg("Max NUM cached files reached for server "
			      "~p", [SC#sconf.servername]),
	    cleanup_cache(E, num),
	    cache_file(GC, SC, Path, UT);
	FI#file_info.size < GC#gconf.max_size_cached_file,
	B + FI#file_info.size > GC#gconf.max_num_cached_bytes ->
	    error_logger:info_msg("Max size cached bytes reached for server "
				  "~p", [SC#sconf.servername]),
	    cleanup_cache(E, size),
	    cache_file(GC, SC, Path, UT);
	true ->
	    ?Debug("Check file size\n",[]),
	    if
		FI#file_info.size > GC#gconf.max_size_cached_file ->
		    ?Debug("To large\n",[]),
		    UT;
		true ->
		    ?Debug("File fits\n",[]),
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



%% FIXME, should not wipe entire ets table this way
cleanup_cache(E, size) ->
    %% remove the largest files with the least hit count  (urlc)
    yaws_log:infolog("Clearing yaws internal content cache, size overflow",[]),
    clear_ets(E);

cleanup_cache(E, num) ->
    yaws_log:infolog("Clearing yaws internal content cache, num overflow",[]),
    clear_ets(E).



clear_ets(E) ->
    ets:match_delete(E, {'_', spec, '_', '_', '_'}),
    ets:match_delete(E, {{url, '_'}, '_', '_'}),
    ets:match_delete(E, {{urlc, '_'}, '_', '_'}),
    ets:insert(E, {num_files, 0}),
    ets:insert(E, {num_bytes, 0}).
    

%% return #urltype{}
do_url_type(SC, Path) ->
    case split_path(SC, Path, [], []) of
	slash ->
	    maybe_return_dir(SC#sconf.docroot, lists:flatten(Path));
	dotdot ->  %% generate 403 forbidden
	    #urltype{type=dotdot};
	redir_dir ->
	    #urltype{type = redir_dir,
		     dir = [Path, $/]};
	OK ->
	    OK
    end.


maybe_return_dir(DR, FlatPath) ->
    ?Debug("maybe_return_dir(~p,, ~p)", [DR, FlatPath]),
    case prim_file:read_file_info([DR, FlatPath, "/index.yaws"]) of
	{ok, FI} ->
	    #urltype{type = yaws,
		     finfo = FI,
		     path = {noflat, [DR, FlatPath]},
		     mime = "text/html",
		     dir = FlatPath,
		     fullpath = ?f([DR, "/index.yaws"])};
	_ ->
	    case prim_file:read_file_info([DR, FlatPath, "/index.html"]) of
		{ok, FI} ->
		    #urltype{type = regular,
			     finfo = FI,
			     path = {noflat, [DR, FlatPath]},
			     mime = "text/html",
			     dir = FlatPath,
			     fullpath = ?f([DR, FlatPath, "/index.html"])};
		_ ->
		    case file:list_dir([DR, FlatPath]) of
			{ok, List} ->
			    #urltype{type = directory,
				     path = {noflat, [DR, FlatPath]},
				     dir = FlatPath,
				     data=List};
			_Err ->
			    #urltype{type=error}
		    end
	    end
    end.


split_path(_SC, [$/], _Comps, []) ->
    %% its a URL that ends with /
    slash;
split_path(SC, [$/, $/ |Tail], Comps, Part) ->  %% security clause
    split_path(SC, [$/|Tail], Comps, Part);
split_path(SC, [$/, $., $., $/ |Tail], _, [_H|T]) ->  %% security clause
    split_path(SC, Tail, [], T);
split_path(_SC, [$/, $., $., $/ |_Tail], _, []) -> %% security clause
    dotdot;
split_path(SC, [], Comps, Part) ->
    ret_reg_split(SC, Comps, Part, []);
split_path(SC, [$?|Tail], Comps, Part) ->
    ret_reg_split(SC, Comps, Part, Tail);
split_path(SC, [$/|Tail], Comps, Part)  when Part /= [] ->
    ?Debug("Tail=~s Part=~s", [Tail,Part]),
    Component = lists:reverse(Part),
    CName = tl(Component),
    case lists:member(CName, SC#sconf.appmods) of
	false ->
	    split_path(SC, [$/|Tail], [lists:reverse(Part) | Comps], []);
	true ->
	    %% we've found an appmod
	    PrePath = conc_path(Comps),
	    ret_app_mod(SC, [$/|Tail], list_to_atom(CName), PrePath)
    end;
split_path(SC, [$~|Tail], Comps, Part) ->  %% user dir
    ret_user_dir(SC, Comps, Part, Tail);
split_path(SC, [H|T], Comps, Part) ->
    split_path(SC, T, Comps, [H|Part]).




conc_path([]) ->
    [];
conc_path([H|T]) ->
    H ++ conc_path(T).


ret_app_mod(SC, Path, Mod, PrePath) ->
    {PathData, Query} = q_splitpath(Path, []),
    #urltype{type = appmod,
	     data = {Mod, PathData},
	     path = PrePath,   %% need to set for WWW-Autenticate to work
	     q = Query}.



q_splitpath([$?|T], Ack) ->
    {lists:reverse(Ack), if T == [] -> undefined; true -> T end};
    
q_splitpath([], Ack) ->
     {lists:reverse(Ack), undefined};

q_splitpath([H|T], Ack) ->
    q_splitpath(T, [H|Ack]).
     
	     
	


%% http://a.b.c/~user URLs
ret_user_dir(SC, [], "/", Upath) ->
    ?Debug("UserPart = ~p~n", [Upath]),
    case parse_user_path(SC#sconf.docroot, Upath, []) of
	{ok, User, Path} ->
    
	    ?Debug("User=~p Path = ~p~n", [User, Path]),
	    
	    %% FIXME doesn't work if passwd contains :: 
	    %% also this is unix only
	    %% and it ain't the fastest code around.
	    OS = os:cmd(["grep ", User, " /etc/passwd "]),
	    case (catch lists:nth(6, string:tokens(OS, [$:]))) of
		{'EXIT', _} ->
		    #urltype{type=error};
		Home ->
		    DR2 = Home ++ "/public_html/",
		    do_url_type(SC#sconf{docroot=DR2}, Path) %% recurse
	    end;
	redir_dir ->
	    redir_dir
    end.



parse_user_path(_DR, [], _User) ->
    redir_dir;
parse_user_path(_DR, [$/], User) ->
    {ok, lists:reverse(User), [$/]};
parse_user_path(_DR, [$/|Tail], User) ->
    {ok, lists:reverse(User), [$/|Tail]};
parse_user_path(DR, [H|T], User) ->
    parse_user_path(DR, T, [H|User]).



ret_reg_split(SC, Comps, RevFile, Query) ->
    ?Debug("ret_reg_split(~p)", [[SC#sconf.docroot, Comps, RevFile, Query]]),
    Dir = lists:reverse(Comps),
    %%FlatDir = lists:flatten(Dir),
    FlatDir = {noflat, Dir},
    DR = SC#sconf.docroot,
    File = lists:reverse(RevFile),
    L = [DR, Dir, File],
    ?Debug("ret_reg_split: L =~p~n",[L]),
    case prim_file:read_file_info(L) of
	{ok, FI} when FI#file_info.type == regular ->
	    {X, Mime} = suffix_type(RevFile),
	    #urltype{type=X, 
		     finfo=FI,
		     path = {noflat, [DR, Dir]},
		     dir = FlatDir,
		     fullpath = lists:flatten(L),
		     mime=Mime, q=Query};
	{ok, FI} when FI#file_info.type == directory, hd(RevFile) == $/ ->
	    maybe_return_dir(DR, lists:flatten(Dir) ++ File);
	{ok, FI} when FI#file_info.type == directory, hd(RevFile) /= $/ ->
	    redir_dir;
	{error, enoent} ->
	    ?Debug("Try url_decode of ~p",[lists:flatten([Dir,File])]),
	    %% kind of hackish, defer url decode 
	    Dir2 = lists:flatmap(fun(X) -> yaws_api:url_decode(X) end, Dir),
	    File2 = yaws_api:url_decode(File),
	    L2 = [DR, Dir2, File2],
	    ?Debug("Try open ~p~n", [lists:flatten(File2)]),
	    case prim_file:read_file_info(L2) of
		{ok, FI} when  FI#file_info.type == regular ->
		    {X, Mime} = suffix_type(RevFile),
		    #urltype{type=X, 
			     finfo=FI,
			     path = {noflat, [DR, Dir2]},
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


suffix_type(L) ->
    L2 = drop_till_dot(L),
    mime_types:revt(L2).

drop_till_dot([$.|_]) ->
    [];
drop_till_dot([H|T]) ->
    [H|drop_till_dot(T)];
drop_till_dot([]) ->
    [].


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
tcp_flush(Sock, Sz) when list(Sz) ->
    tcp_flush(Sock, list_to_integer(Sz));
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
    proc_lib:spawn(?MODULE, load_and_run, [Mod]).

load_and_run(Mod) ->
    case code:ensure_loaded(Mod) of
	{module,Mod} ->
	    Mod:start();
	Error ->
	    yaws_log:errlog("Loading '~w' failed, reason ~p~n",[Mod,Error])
    end.

decode_base64([]) ->
  [];
decode_base64([Sextet1,Sextet2,$=,$=|Rest]) ->
  Bits2x6=
    (d(Sextet1) bsl 18) bor
    (d(Sextet2) bsl 12),
  Octet1=Bits2x6 bsr 16,
  [Octet1|decode_base64(Rest)];
decode_base64([Sextet1,Sextet2,Sextet3,$=|Rest]) ->
  Bits3x6=
    (d(Sextet1) bsl 18) bor
    (d(Sextet2) bsl 12) bor
    (d(Sextet3) bsl 6),
  Octet1=Bits3x6 bsr 16,
  Octet2=(Bits3x6 bsr 8) band 16#ff,
  [Octet1,Octet2|decode_base64(Rest)];
decode_base64([Sextet1,Sextet2,Sextet3,Sextet4|Rest]) ->
  Bits4x6=
    (d(Sextet1) bsl 18) bor
    (d(Sextet2) bsl 12) bor
    (d(Sextet3) bsl 6) bor
    d(Sextet4),
  Octet1=Bits4x6 bsr 16,
  Octet2=(Bits4x6 bsr 8) band 16#ff,
  Octet3=Bits4x6 band 16#ff,
  [Octet1,Octet2,Octet3|decode_base64(Rest)];
decode_base64(_CatchAll) ->
  {error, bad_base64}.

d(X) when X >= $A, X =<$Z ->
    X-65;
d(X) when X >= $a, X =<$z ->
    X-71;
d(X) when X >= $0, X =<$9 ->
    X+4;
d($+) -> 62;
d($/) -> 63;
d(_) -> 63.



safe_ehtml_expand(X) ->
    case (catch ehtml_expand(X)) of
	{'EXIT', R} ->
	    io_lib:format("<pre> ~n~p~n </pre>~n", [R]);
	Val ->
	    Val
    end.


%% simple erlang term representation of HTML:
%% EHTML = [EHTML] | {Tag, Attrs, Body} | {Tag, Attrs} | {Tag} |
%%         binary() | character()
%% Tag 	 = atom()
%% Attrs = [{Key, Value}]  or {EventTag, {jscall, FunName, [Args]}}
%% Key 	 = atom()
%% Value = string()
%% Body  = EHTML
ehtml_expand(Ch) when Ch >= 0, Ch =< 255 ->
    yaws_api:htmlize_char(Ch);
ehtml_expand(Bin) when binary(Bin) ->
    yaws_api:htmlize(Bin);
ehtml_expand({Tag}) ->
    ehtml_expand({Tag,[]});
ehtml_expand({Tag, Attrs}) ->
    io_lib:format("<~s~s>~n", [Tag, ehtml_attrs(Attrs)]);
ehtml_expand({Tag, Attrs, Body}) ->
    [io_lib:format("<~s~s>~n", [Tag, ehtml_attrs(Attrs)]),
     ehtml_expand(Body),
     io_lib:format("</~s>~n", [Tag])];
ehtml_expand(L) when list(L) ->
    map(fun ehtml_expand/1, L).





ehtml_attrs([]) ->
    [];
ehtml_attrs([{Attr, JsCall} |Tail]) when element(1, JsCall) == jscall ->
    [io_lib:format(" ~s=~s", [Attr, jscall(JsCall)]) | ehtml_attrs(Tail)];
ehtml_attrs([{Name, Value} | Tail]) ->
    [io_lib:format(" ~s=\"~s\"", [Name, Value]) | ehtml_attrs(Tail)].



%% javascript calls
%% example: {onClick, {jscall, xyz, [f2,bala2]}}
%% which expands to: onClick="xyz('f2' , 'bala2')"


jscall({jscall, Fname, ArgList}) ->
    io_lib:format("\"~s(~s)\"; ", [Fname, jscall_args(ArgList)]).

jscall_args([H]) ->
    jscall_arg(H);
jscall_args([H|T]) ->
    [jscall_arg(H), " , " | jscall_args(T)];
jscall_args([]) ->
    [].


jscall_arg(Arg) when integer(Arg) ->
    Arg;
jscall_arg(Arg) when list(Arg) ->
    io_lib:format("\'~s\'", [Arg]);
jscall_arg(Arg) when atom(Arg) ->
    io_lib:format("\'~s\'", [Arg]).



