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
-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
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
	    exit(RSN)
    end.


%% syncronous start, 
%% If we're later supposed
%% to change uid, we're still root here.

run(GC) ->
    %% First check if there is already a Yaws system running
    %% with the same sid.

    case connect(GC#gconf.id) of
	{ok, Sock} ->

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
		    case w_ctl_file(GC#gconf.id,Port) of
			ok ->
			    proc_lib:init_ack(ok),
			    aloop(L, GC);
			error ->
			    e("Failed to create/manipulate the ctlfile ~n"
			      "called ~s~n"
			      "either problems with permissions or "
			      " earlier runs of yaws ~nwith the same id "
			      " <~p> as this, check /tmp/yaws/* for perms~n",
			      [ctl_file(GC#gconf.id), GC#gconf.id])
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
w_ctl_file(Sid, Port) ->
    case catch 
	begin
	    F = ctl_file(Sid),
	    ?Debug("Ctlfile : ~s~n", [F]),
	    file:write_file(F, io_lib:format("~w", [Port])),
	    {ok, FI} = file:read_file_info(F),
	    ok = file:write_file_info(F, FI#file_info{mode = 8#00600})
	end of
	{'EXIT', _} ->
	    error;
	_ ->
	    ok
    end.


ctl_file(Sid) ->
    filename:join([yaws:tmp_dir(),
		   "yaws",
		   Sid,
		   "ctl"]).



aloop(L, GC) ->
    case gen_tcp:accept(L) of
	{ok, A} ->
	    handle_a(A, GC);
	_Err ->
	    ignore
    end,
    ?MODULE:aloop(L, GC).

handle_a(A, GC) ->
    case gen_tcp:recv(A, 0) of
	{ok, Data} ->
	    case binary_to_term(Data) of
		hup ->
		    Res = yaws:dohup(A),
		    Res;
		stop ->
		    gen_tcp:send(A, "stopping\n"),
		    file:delete(ctl_file(GC#gconf.id)),
		    init:stop();
		{trace, What} ->
		    Res = actl_trace(What),
		    gen_tcp:send(A, Res),
		    gen_tcp:close(A);
		status ->
		    a_status(A),
		    gen_tcp:close(A);
		{load, Mods} ->
		    a_load(A, Mods),
		    gen_tcp:close(A);
		id ->
		    a_id(A),
		    gen_tcp:close(A);
		Other ->
		    gen_tcp:send(A, io_lib:format("Other: ~p~n", [Other])),
		    gen_tcp:close(A)
	    
	    end;
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
		{true, Other} ->
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
%% is listening at.

connect_file(CtlFile) ->
    case file:read_file(CtlFile) of
	{ok, Bin} ->
	    L = binary_to_list(Bin),
	    I = list_to_integer(L),
	    gen_tcp:connect({127,0,0,1}, I,
			    [{active, false},
			     {reuseaddr, true},
			     binary,
			     {packet, 2}], 2000);
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
	{ok, Socket} ->
	    Str = s_cmd(Socket, SID, Term),
	    io:format("~s", [Str])
    end,
    init:stop().


s_cmd(Fd, SID, Term) ->	    
    gen_tcp:send(Fd, term_to_binary(Term)),
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
    case file:list_dir("/tmp/yaws") of
	{ok, List} ->
	    io:format("~-15s~-10s~-10s~n",
		      ["Id", "Status", "Owner"]),
	    io:format("-------------------------------------~n",[]),
	    lists:foreach(
	      fun(D) ->
		      lls(D)
	      end, List);
	_ ->
	    ok
    
    end,
    init:stop().


lls(Dir) ->
    Ctl = ctl_file(Dir),
    case {file:read_file_info(Ctl),
	  file:read_file_info(filename:join("/tmp/yaws/", Dir))} of
	{{ok, FI}, {ok, DI}} ->
	    User = yaws:uid_to_name(DI#file_info.uid),
	    Running = case connect(Dir) of
			  {ok, Sock} ->
			      gen_tcp:close(Sock),
			      "running";
			  {error, timeout} ->
			      "hanging??";
			  _ ->
			      "crashed"
		      end,
	    io:format("~-15s~-10s~-10s~n",
		      [Dir, Running, User]);


	
	{{ok, FI}, {error, _}} ->
	    %% sick case,
	    ignore;

	{{error, _}, {ok, DI}} ->
	    %% nicely terminated system
	    User = yaws:uid_to_name(DI#file_info.uid),
	    io:format("~-15s~-10s~-10s~n",
		      [Dir, "stopped", User]);


	_Err ->
	    io:format("~-15s~-10s~-10s~n",
		      [Dir, "unknown", "unknown"])
		
    end.

	      

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
    GC = yaws_config:make_default_gconf(false),
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


    



		    

		    


		

