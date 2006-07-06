%%%----------------------------------------------------------------------
%%% File    : yaws_ctl.erl
%%% Author  : Claes Wikstrom <klacke@bluetail.com>
%%% Purpose : 
%%% Created : 29 Apr 2002 by Claes Wikstrom <klacke@bluetail.com>
%%%----------------------------------------------------------------------


%% some code to remoteley control a running yaws server

-module(yaws_ctl).
-author('klacke@bluetail.com').

-compile(export_all).
-include_lib("kernel/include/file.hrl").
-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").



%% assumes the appropriate file structures 
%% are already created with the right perms

start(_GC, FirstTime) when FirstTime == false ->
    ok;
start(GC, true) ->
    case proc_lib:start_link(?MODULE, run, [GC]) of
	ok ->
	    ok;
	{error, RSN} ->
	    error_logger:format("~s~n",[RSN]),
	    erlang:fault(RSN)
    end.


run(GC) ->
    %% First check if there is already a Yaws system running
    %% with the same sid.

    case connect(GC#gconf.id) of
	{ok, Sock, _Key} ->

	    %% Not good, let's get some sys info
	    %% from that system so we can produce a good error
	    %% message
	    gen_tcp:close(Sock),
	    e("There is already a yaws system running with the same ~n"
	      " id <~p> on this computer, ~n"
	      " set another id in the yaws conf file ~n", 
	      [GC#gconf.id]);
	{error, eaccess} ->
	    %% We're not allowed to open the ctl file
	    e("Error reading ~s, you are probably (sometimes) running ~n"
	      " yaws as another userid, but with the same yaws id <~p> ~n"
	      " set another id in the yaws conf file ~n", 
	      [ctl_file(GC#gconf.id), GC#gconf.id]);
	{error, _} ->
	    %% Fine, this should be the case
	    run_listen(GC)
    end.


ctl_args() ->
     [{packet, 2},
      {active, false},
      binary,
      {ip, {127,0,0,1}},
      {reuseaddr, true}].

run_listen(GC) ->
    case gen_tcp:listen(0, ctl_args()) of
	{ok,  L} ->
	    case inet:sockname(L) of
		{ok, {_, Port}} ->
		    {A1, A2, A3}=now(),
		    random:seed(A1, A2, A3),
		    Key = random:uniform(1 bsl 64),
		    case w_ctl_file(GC#gconf.id, Port, Key) of
			ok ->
			    proc_lib:init_ack(ok),
			    aloop(L, GC, Key);
			error ->
			    error_logger:format(
			      "Failed to create/manipulate the ctlfile ~n"
			      "called ~s~n"
			      "Either problems with permissions or "
			      " earlier runs of yaws ~nwith the same id "
			      " <~p> as this, check dir for perms~n"
			      "None of Yaws ctl functions will work~n",
			      [ctl_file(GC#gconf.id), GC#gconf.id]),
			    proc_lib:init_ack(ok),
			    aloop(L, GC, Key)
		    end;
		Err ->
		    e("Cannot get sockname for ctlsock: ~p",[Err] )
	    end;
	Err ->
	    e("Cannot listen on ctl socket, fatal: ~p", [Err])
    end.


e(Fmt, Args) ->
    proc_lib:init_ack({error, io_lib:format(Fmt, Args)}),
    exit(normal).



%% write the control file, set perms of the file
%% so that only this user can read the file
%% That way we're making sure different users
%% cannot manipulate eachothers webservers
w_ctl_file(Sid, Port, Key) ->
    case catch
	begin
	    F = ctl_file(Sid),
	    ?Debug("Ctlfile : ~s~n", [F]),
	    file:write_file(F, io_lib:format("~w.", [{Port,Key}])),
	    {ok, FI} = file:read_file_info(F),
	    ok = file:write_file_info(F, FI#file_info{mode = 8#00600})
	end of
	{'EXIT', _} ->
	    error;
	_ ->
	    ok
    end.


ctl_file(Sid) ->
    FN = filename:join([yaws_generated:ctldir(), "ctl-" ++ yaws:to_list(Sid)]),
    filelib:ensure_dir(FN),
    FN.


aloop(L, GC, Key) ->
    case gen_tcp:accept(L) of
	{ok, A} ->
	    handle_a(A, GC, Key);
	Err ->
	    error_logger:format("yaws_ctl failed to accept: ~p~n",
				[Err]),
	    timer:sleep(2000),
	    ignore
    end,
    ?MODULE:aloop(L, GC, Key).

handle_a(A, GC, Key) ->
    case gen_tcp:recv(A, 0) of
	{ok, Data} ->
	    case catch binary_to_term(Data) of
		{hup, Key} ->
		    Res = yaws:dohup(A),
		    Res;
		{stop, Key} ->
		    gen_tcp:send(A, io_lib:format(
				      "stopping yaws with id=~p\n",
				      [GC#gconf.id])),
		    file:delete(ctl_file(GC#gconf.id)),
		    init:stop();
		{{trace, What}, Key} ->
		    Res = actl_trace(What),
		    gen_tcp:send(A, Res),
		    gen_tcp:close(A);
		{status, Key} ->
		    a_status(A),
		    gen_tcp:close(A);
		{{load, Mods}, Key} ->
		    a_load(A, Mods),
		    gen_tcp:close(A);
		{id, Key} ->
		    a_id(A),
		    gen_tcp:close(A);
		{Other, Key} ->
		    gen_tcp:send(A, io_lib:format("Other: ~p~n", [Other])),
		    gen_tcp:close(A);
		_Other ->
		    gen_tcp:close(A)
	    
	    end;
	{error, _} ->
	    gen_tcp:close(A);
	_Err ->
	    ignore
    end.


%% We implement this by reloading a patched config
actl_trace(What) ->
    case lists:member(What, [traffic, http, off]) of
	true ->
	    {ok, GC, SCs} = yaws_api:getconf(),
	    case GC#gconf.trace of
		false when What /= off->
		    yaws_api:setconf(GC#gconf{trace = {true, What}},SCs),
		    io_lib:format(
		      "Turning on trace of ~p to file ~s~n",
		      [What, 
		       filename:join([GC#gconf.logdir, 
				      "trace." ++ atom_to_list(What)])]);
		false when What == off ->
		    io_lib:format("Tracing is already turned off ~n",[]);
		{true, _} when What == off ->
		    yaws_api:setconf(GC#gconf{trace = false},SCs),
		    "Turning trace off \n";
		{true, What} ->
		      io_lib:format("Trace of ~p is already turned on, ose 'off' "
				    "to turn off~n", [What]);
		{true, _Other} ->
		    yaws_api:setconf(GC#gconf{trace = {true, What}},SCs),
		    io_lib:format(
		      "Turning on trace of ~p to file ~s~n",
		      [What, 
		       filename:join([GC#gconf.logdir, 
				      "trace." ++ atom_to_list(What)])])
	    end;
	false ->
	    "Need either http | traffic | off  as argument\n"
    end.



f(Fmt, As) ->
    io_lib:format(Fmt, As).


a_id(Sock) ->
    ID = gen_server:call(yaws_server, id, []),
    gen_tcp:send(Sock, ID),
    ok.


a_status(Sock) ->
    {UpTime, L} = yaws_server:stats(),
    {Days, {Hours, Minutes, _Secs}} = UpTime,
    H = f("~n Uptime: ~w Days, ~w Hours, ~w Minutes  ~n",
           [Days, Hours, Minutes]),
    
    T =lists:map(
	 fun({Host,IP,Hits}) ->
		 L1= f("stats for ~p at ~p  ~n",
		       [Host,IP]),
		 T = "\n"
                     "URL                  Number of hits\n",
		 L2=lists:map(
		      fun({Url, Hits2}) ->
			      f("~-30s ~-7w ~n",
				[Url, Hits2])
		      end, Hits),
		 END = "\n",
		 [L1, T, L2, END]
	 end, L),
    gen_tcp:send(Sock, [H, T]),
    
    %% Now lets' figure out the status of loaded modules
    ok.

a_load(A, Mods) ->
    case purge(Mods) of
	ok ->
	    gen_tcp:send(A, f("~p~n", [loadm(Mods)]));
	Err ->
	    gen_tcp:send(A, f("~p~n", [Err]))
    end.

loadm([]) ->
    [];
loadm([M|Ms]) ->
    [code:load_file(M)|loadm(Ms)].

purge(Ms) ->
    case purge(Ms, []) of 
	[] -> ok;
	L -> {cannot_purge, L}
    end.

purge([], Ack) ->
    Ack;
purge([M|Ms], Ack) ->
    case code:soft_purge(M) of
	true ->
	    purge(Ms, Ack);
	false ->
	    purge(Ms, [M|Ack])
    end.



connect(Sid) ->
    connect_file(ctl_file(Sid)).


%% The ctl file contains the port number the yaws server
%% is listening at and secret key string.

connect_file(CtlFile) ->
    case file:consult(CtlFile) of
	{ok, [{Port, Key}]} ->
	    case gen_tcp:connect({127,0,0,1}, Port,
				 [{active, false},
				  {reuseaddr, true},
				  binary,
				  {packet, 2}], 2000) of
		{ok, Socket} ->
		    {ok, Socket, Key};
		Err ->
		    Err
	    end;
	Err ->
	    Err
    end.



actl(SID, Term) ->
    case connect(SID) of
	{error, eaccess} ->
	    io:format("Another user is using the yaws sid <~p>, ~n"
		      "You are not allowd to read the file <~s>, ~n"
		      "specify by <-I id> which yaws system you want "
		      " to control~n",
		      [SID, ctl_file(SID)]);
	{error, econnrefused} ->
	    io:format("No yaws system responds~n",[]);
	{error, Reason} ->
	    io:format("You failed to read the ctlfile ~s~n"
		      "error was: <~p>~n"
		      "specify by <-I id> which yaws system you want "
		      " to control~n",
		      [ctl_file(SID), Reason]);
	{ok, Socket, Key} ->
	    Str = s_cmd(Socket, SID, Key, Term),
	    io:format("~s", [Str])
    end,
    init:stop().


s_cmd(Fd, SID, Key, Term) ->
    gen_tcp:send(Fd, term_to_binary({Term, Key})),
    Res = case gen_tcp:recv(Fd, 0) of
	      {ok, Bin} ->
		  binary_to_list(Bin);
	      Err ->
		  io_lib:format("yaws server for yaws id <~p> not "
				"responding: ~p ~n", [SID, Err])
	  end,
    gen_tcp:close(Fd),
    Res.


%% List existing yaws nodes on this machine
ls(_) ->
    case file:list_dir(yaws_generated:ctldir()) of
	{ok, List} ->
	    io:format("~-15s~-10s~-10s~n",
		      ["Id", "Status", "Owner"]),
	    io:format("-------------------------------------~n",[]),
	    lists:foreach(
	      fun(CtlFile) ->
		      lls(CtlFile)
	      end, List);
	_ ->
	    ok
    
    end,
    init:stop().


lls(CtlFile0 = "ctl-" ++ Id) ->
    CtlFile = filename:join([yaws_generated:ctldir(), CtlFile0]),
    case {file:read_file_info(CtlFile),
	  file:read_file(CtlFile)} of
	{{ok, FI}, {error, eacces}} ->
	    User = yaws:uid_to_name(FI#file_info.uid),
	    io:format("~-15s~-10s~-10s~n",
		      [Id, "noaccess", User]);
	{{ok, FI}, {ok, _Bin}} ->
	    Running = case connect(Id) of
			  {ok, Sock, _Key} ->
			      gen_tcp:close(Sock),
			      "running";
			  {error, timeout} ->
			      "hanging??";
			  {error, eacces} ->
			      "unknown";
			  _Err ->
			      "stopped"
		      end,
	    User = yaws:uid_to_name(FI#file_info.uid),
	    io:format("~-15s~-10s~-10s~n",
		      [Id, Running, User]);
	_ ->
	    ok
    end;
lls(_) ->
    ok.

	      

%% send a hup (kindof) to the yaws server to make it
%% reload its configuration and clear its caches

hup([SID]) ->
    actl(SID, hup).


%% stop a daemon
stop([SID]) ->	
    actl(SID, stop).

%% query a daemon for status/stats
status([SID]) ->
    actl(SID, status).

load(X) ->
    [SID | Modules] = lists:reverse(X),
    actl(SID, {load, Modules}).

check([Id, File| IncludeDirs]) ->
    GC = yaws_config:make_default_gconf(false, undefined),
    GC2 = GC#gconf{include_dir = lists:map(fun(X) -> atom_to_list(X) end, 
					   IncludeDirs),
		   id = atom_to_list(Id)
		  },
    yaws_server:setup_dirs(GC2),
    put(sc, #sconf{}),
    put(gc, GC2),
    case yaws_compile:compile_file(atom_to_list(File)) of
	{ok, [{errors, 0}| _Spec]} ->
	    io:format("ok~n",[]),
	    init:stop();
	_Other ->
	    io:format("~nErrors in ~p~n", [File]),
	    init:stop()
    end.

%% control a daemon http/traffic tracer
trace([What, SID]) ->
    actl(SID, {trace, What}).


    



		    

		    


		

