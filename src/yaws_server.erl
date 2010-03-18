%%%----------------------------------------------------------------------
%%% File    : yaws_server.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose :
%%% Created : 16 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_server).
-author('klacke@hyber.org').

-behaviour(gen_server).
-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").

-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/inet.hrl").

-export([mappath/3, vdirpath/3]).


%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).
-export([status/0,
         getconf/0,
         stats/0,
         gs_status/0,
         ssi/3,ssi/5,ssi/6
        ]).

%% internal exports
-export([gserv/3,acceptor0/2, load_and_run/2, done_or_continue/0,
         accumulate_content/1, deliver_accumulated/5, setup_dirs/1,
         deliver_dyn_part/8, finish_up_dyn_file/2
        ]).

-export(['GET'/3,
         'POST'/3,
         'HEAD'/3,
         'TRACE'/3,
         'OPTIONS'/3,
         'PUT'/3,
         'DELETE'/3]).

-import(lists, [member/2, foreach/2, map/2,
                flatten/1, flatmap/2, reverse/1]).

-import(yaws_api, [ehtml_expand/1]).

-record(gs, {gconf,
             group,         %% list of #sconf{} s
             ssl,           %% ssl | nossl
             certinfo,      %% undefined | #certinfo{}
             l,             %% listen socket
             mnum = 0,
 	     connections = 0, %% number of TCP connections opened now
             sessions = 0,  %% number of active HTTP sessions
             reqs = 0}).    %% total number of processed HTTP requests


-record(state, {gc,         %% Global conf #gc{} record
                pairs,      %% [{GservPid, ScList}]
                mnum = 0,   %% dyn compiled erl module  number
                embedded    %% true if in embedded mode, false otherwise
               }).

%% undefined | mtime from #file_info
-record(certinfo, {keyfile,
                   certfile,
                   cacertfile
                  }).

-define(elog(X,Y), error_logger:info_msg("*elog ~p:~p: " X,
                                         [?MODULE, ?LINE | Y])).


start_link(A) ->
    gen_server:start_link({local, yaws_server}, yaws_server, A, []).

status() ->
    gen_server:call(?MODULE, status, 10000).
gs_status() ->
    [_|Pids] = gen_server:call(?MODULE, pids, 10000),
    lists:map(
      fun(P) ->
              P ! {self(), status},
              receive {P, Stat} -> Stat end
      end, Pids).
getconf() ->
    gen_server:call(?MODULE,getconf, infinity).

stats() ->
    {_S, Time} = status(),
    Diff = calendar:time_difference(Time, calendar:local_time()),
    {Diff, []}.

l2a(L) when is_list(L) -> list_to_atom(L);
l2a(A) when is_atom(A) -> A.



%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------

init(Env) -> %% #env{Trace, TraceOut, Conf, RunMod, Embedded, Id}) ->
    process_flag(trap_exit, true),
    put(start_time, calendar:local_time ()),  %% for uptime
    case Env#env.embedded of
        false ->
            Config = (catch yaws_config:load(Env)),
            case Config of
                {ok, Gconf, Sconfs} ->
                    erase(logdir),
                    ?Debug("GC = ~s~n", [?format_record(Gconf, gconf)]),
                    lists:foreach(
                      fun(Group) ->
                              lists:foreach(
                                fun(_SC) ->
                                        ?Debug("SC = ~s~n",
                                               [?format_record(_SC, sconf)])
                                end, Group)
                      end, Sconfs),
                    yaws_log:setdir(Gconf, Sconfs),
                    case Gconf#gconf.trace of
                        {true, What} ->
                            yaws_log:open_trace(What),
                            yaws_api:set_tty_trace(?gc_has_tty_trace(Gconf));
                        _ ->
                            ok
                    end,
                    init2(Gconf, Sconfs, Env#env.runmod,
                          Env#env.embedded, true);
                {error, E} ->
                    case erase(logdir) of
                        undefined ->
                            error_logger:error_msg("Yaws: Bad conf: ~p~n",[E]),
                            init:stop(),
                            {stop, E};
                        Dir ->
                            GC = yaws_config:make_default_gconf(true,
                                                                Env#env.id),
                            yaws_log:setdir(GC#gconf{logdir = Dir}, []),
                            error_logger:error_msg("Yaws: bad conf: ~s "
                                                   "terminating~n",[E]),
                            init:stop(),
                            {stop, E}
                    end;
                EXIT ->
                    error_logger:format("FATAL ~p~n", [EXIT]),
                    erlang:error(badconf)
            end;
        true ->
            {ok, #state{gc = undefined,
                        embedded = Env#env.embedded,
                        pairs = [],
                        mnum = 0}}
    end.


init2(GC, Sconfs, RunMod, Embedded, FirstTime) ->
    put(gc, GC),
    case GC#gconf.mnesia_dir of
        MD when length(MD) > 0 ->
            yaws_debug:format("loading mnesia ~p~n", [MD]),
            application:set_env(mnesia,dir,MD),
            mnesia:start();
        _ ->
            ok
    end,
    foreach(
      fun(D) ->
              yaws_debug:format("Add path ~p~n", [D]),
              code:add_pathz(D)
      end, GC#gconf.ebin_dir),
    yaws_debug:format("Running with id=~p (localinstall=~p) ~n"
                      "~s"
                      "Logging to directory ~p~n",
                      [GC#gconf.id, yaws_generated:is_local_install(),
                       if ?gc_has_debug(GC) ->
                               "Running with debug checks "
                               "turned on (slower server) \n";
                          true ->
                               ""
                       end,
                       GC#gconf.logdir]),

    setup_dirs(GC),

    case Embedded of
        false ->
            case yaws_ctl:start(GC, FirstTime) of
                ok ->
                    ok;
                {error, RSN} ->
                    %% Must call init stop here otherwise heart
                    %% will restart us
                    error_logger:format("Failed to start: ~s~n", [RSN]),
                    init:stop(),
                    receive nothing -> ok end
            end;
        true ->
            ok
    end,

    runmod(RunMod, GC),

    L2 = lists:zf(
           fun(Group) -> start_group(GC, Group) end,
           Sconfs),
    {ok, #state{gc = GC,
                pairs = L2,
                mnum = 0,
                embedded = Embedded}}.



start_group(GC, Group) ->
    FailOnBind = ?gc_fail_on_bind_err(GC),
    case proc_lib:start_link(?MODULE, gserv, [self(), GC, Group]) of
        {error, F, A} when FailOnBind == false ->
            error_logger:error_msg(F, A),
            false;
        {error, F, A} ->
            error_logger:error_msg(F, A),
            erlang:error(badbind);
        {error, Reason} when FailOnBind == false ->
            error_logger:error_msg("FATAL: ~p~n", [Reason]),
            false;
        {error, Reason} ->
            error_logger:error_msg("FATAL: ~p~n", [Reason]),
            erlang:error(badbind);
        {Pid, SCs} ->
            {true, {Pid, SCs}};
        none ->
            false
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

handle_call(id, _From, State) ->
    {reply, (State#state.gc)#gconf.id, State};

handle_call(pids, _From, State) ->  %% for gprof
    L = map(fun(X) ->element(1, X) end, State#state.pairs),
    {reply, [self() | L], State};

handle_call(mnum, _From, State) ->
    Mnum = State#state.mnum +1,
    {reply, Mnum, State#state{mnum = Mnum}};



%% This is a brutal restart of everything
handle_call({setconf, GC, Groups}, _From, State) ->
    %% First off, terminate all currently running processes
    Curr = map(fun(X) ->element(1, X) end, State#state.pairs),
    foreach(fun(Pid) ->
                    gserv_stop(Pid)
            end, Curr),
    {ok, State2} = init2(GC, Groups, undef, State#state.embedded, false),
    {reply, ok, State2};

handle_call(getconf, _From, State) ->
    Groups = map(fun({_Pid, SCs}) -> SCs end, State#state.pairs),
    {reply, {ok, State#state.gc, Groups}, State};

%% If cert has changed, server will stop implicitly
handle_call(check_certs, _From, State) ->
    L = lists:map(fun({Pid, _SCs}) ->
                          Pid ! {check_cert_changed, self()},
                          receive {Pid, YesNo} ->
                                  YesNo
                          end
                  end, State#state.pairs),
    {reply, L, State};

handle_call({update_sconf, NewSc}, From, State) ->
    case yaws_config:search_sconf(NewSc, State#state.pairs) of
        {Pid, OldSc, _Group} ->
            case yaws_config:eq_sconfs(OldSc,NewSc) of
                true ->
                    error_logger:info_msg("Keeping conf for ~s intact\n",
                                          [yaws:sconf_to_srvstr(OldSc)]),
                    {reply, ok, State};
                false ->
                    Pid ! {update_sconf, NewSc, OldSc, From, self()},
                    receive
                        {updated_sconf, Pid, NewSc2} ->
                            P2 = yaws_config:update_sconf(NewSc2,
                                                          State#state.pairs),
                            {noreply, State#state{pairs = P2}}
                    after 2000 ->
                            {reply, {error, "Failed to update new conf"}, State}
                    end
            end;
        false ->
            {reply, {error, "No matching group"}, State}
    end;


handle_call({delete_sconf, SC}, From, State) ->
    case yaws_config:search_sconf(SC, State#state.pairs) of
        {Pid, OldSc, Group} when length(Group) == 1 ->
            error_logger:info_msg("Terminate whole ~s virt server group \n",
                                  [yaws:sconf_to_srvstr(OldSc)]),
            gserv_stop(Pid),
            NewPairs = lists:keydelete(Pid, 1, State#state.pairs),
            {reply, ok, State#state{pairs = NewPairs}};
        {Pid, OldSc, _Group} ->
            Pid ! {delete_sconf, OldSc, From},
            P2 = yaws_config:delete_sconf(OldSc, State#state.pairs),
            {noreply, State#state{pairs = P2}};
        false ->
            {reply, {error, "No matching group"}, State}
    end;

handle_call({add_sconf, SC}, From, State) ->
    case yaws_config:search_group(SC, State#state.pairs) of
        [{Pid, Group}] ->
            Pid ! {add_sconf, From, SC, self()},
	    receive
		{added_sconf, Pid, SC2} ->
		    P2 = lists:keyreplace(Pid, 1, State#state.pairs,
					  {Pid, Group ++ [SC2]}),
		    {noreply, State#state{pairs = P2}}
	    after 2000 ->
		    {reply, {error, "Failed to add new conf"}, State}
	    end;
        [] ->
            %% Need to create a new group
            error_logger:info_msg("Creating new virt server ~s\n",
                                  [yaws:sconf_to_srvstr(SC)]),
            GC = State#state.gc,
            case start_group(GC, [SC]) of
                false ->
                    {reply, ok, State};
                {true, Pair} ->
                                                %Pid = element(1, Pair),
                                                %Pid ! {newuid, GC#gconf.uid},
                    P2 = [Pair | State#state.pairs],
                    {reply, ok, State#state{pairs = P2}}
            end
    end;

handle_call({update_gconf, GC}, _From, State) ->
    lists:foreach(fun({Pid, _Group}) ->
                          Pid ! {update_gconf, GC}
                  end, State#state.pairs),
    %% no need to tell yaws_log, new vals must be compatible
    put(gc, GC),
    {reply, ok, State#state{gc = GC}}.





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
handle_info({Pid, certchanged},  State) ->
    {noreply, State#state{pairs = lists:keydelete(Pid, 1, State#state.pairs)}};
handle_info({'EXIT', Pid, Reason},  State) ->
    case lists:keysearch(Pid, 1, State#state.pairs) of
        {value, _} ->
            %% one of our gservs died
            error_logger:format("yaws: FATAL gserv died ~p~n", [Reason]),
            erlang:error(restartme);
        false ->
            ignore
    end,
    {noreply, State};


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

do_listen(GC, SC) ->
    case SC#sconf.ssl of
        undefined when ?gc_use_fdsrv(GC) ->
            %% Use fdsrv kit from jungerl, requires fdsrv
            %% to be properly installed
            %% This solves the problem of binding to priviliged ports. A
            %% better solution is to use privbind or authbind. Also this
            %% doesn't work for ssl.

            fdsrv:start(),
            case fdsrv:bind_socket(tcp, {SC#sconf.listen, SC#sconf.port}) of
                {ok, Fd} ->
                    {nossl, undefined,
                     gen_tcp_listen(SC#sconf.port,[{fd, Fd}|listen_opts(SC)])};
                Err ->
                    {nossl, undefined, Err}
            end;
        undefined ->
            {nossl, undefined, gen_tcp_listen(SC#sconf.port, listen_opts(SC))};
        SSL ->
            {ssl, certinfo(SSL),
             ssl:listen(SC#sconf.port, ssl_listen_opts(GC, SC, SSL))}
    end.

certinfo(SSL) ->
    #certinfo{
          keyfile = if SSL#ssl.keyfile /= undefined ->
                            case file:read_file_info(SSL#ssl.keyfile) of
                                {ok, FI} ->
                                    FI#file_info.mtime;
                                _ ->
                                    undefined
                            end;
                       true ->
                            undefined
                    end,
          certfile = if SSL#ssl.certfile /= undefined ->
                             case file:read_file_info(SSL#ssl.certfile) of
                                 {ok, FI} ->
                                     FI#file_info.mtime;
                                 _ ->
                                     undefined
                             end;
                        true ->
                            undefined
                     end,
          cacertfile = if SSL#ssl.cacertfile /= undefined ->
                             case file:read_file_info(SSL#ssl.cacertfile) of
                                 {ok, FI} ->
                                     FI#file_info.mtime;
                                 _ ->
                                     undefined
                             end;
                        true ->
                            undefined
                     end
         }.

gen_tcp_listen(Port, Opts) ->
    ?Debug("Listen ~p:~p~n", [Port, Opts]),
    gen_tcp:listen(Port, Opts).


gserv(_Top, _, []) ->
    proc_lib:init_ack(none);

%% One server per IP we listen to
gserv(Top, GC, Group0) ->
    process_flag(trap_exit, true),
    ?TC([{record, GC, gconf}]),
    put(gc, GC),
    put(top, Top),
    Group1 = map(fun(SC) -> setup_ets(SC)
		 end, Group0),
    Group = map(fun(SC) ->
			case ?sc_has_statistics(SC) of
			    true ->
				start_stats(SC);
			    false ->
				SC
			end
		end, Group1),
    SC = hd(Group),
    case do_listen(GC, SC) of
        {SSLBOOL, CertInfo, {ok, Listen}} ->
            lists:foreach(fun(XSC) -> call_start_mod(XSC) end, Group),
            error_logger:info_msg(
              "Yaws: Listening to ~s:~w for <~p> virtual servers:~s~n",
              [inet_parse:ntoa(SC#sconf.listen),
               SC#sconf.port,
               length(Group),
               catch map(
                       fun(S) ->
                               io_lib:format("~n - ~s under ~s",
                                             [yaws:sconf_to_srvstr(S),
                                              S#sconf.docroot])
                       end, Group)
                    ]),
            proc_lib:init_ack({self(), Group}),
            GS = #gs{gconf = GC,
                     group = Group,
                     ssl = SSLBOOL,
                     certinfo = CertInfo,
                     l = Listen},
            Last = initial_acceptor(GS),
            gserv_loop(GS, [], 0, Last);
        {_,_,Err} ->
            error_logger:format("Yaws: Failed to listen ~s:~w  : ~p~n",
                                [inet_parse:ntoa(SC#sconf.listen),
                                 SC#sconf.port, Err]),
            proc_lib:init_ack({error, "Can't listen to socket: ~p ",[Err]}),
            exit(normal)
    end.



setup_dirs(GC) ->
    Dir = yaws:id_dir(GC#gconf.id),
    Ctl = yaws:ctl_file(GC#gconf.id),
    filelib:ensure_dir(Ctl),
    case file:list_dir(Dir) of
        {ok, LL} ->
            foreach(
              fun(F) ->
                      file:delete(filename:join([Dir, F]))
              end, LL -- ["CTL"]);
        {error, RSN} ->
            error_logger:format("Failed to list ~p probably "
                                "due to permission errs: ~p",
                                [Dir, RSN]),
            erlang:error(RSN)
    end.


setup_ets(SC) ->
    E = ets:new(yaws_code, [public, set]),
    ets:insert(E, {num_files, 0}),
    ets:insert(E, {num_bytes, 0}),
    SC#sconf{ets = E}.

clear_ets_complete(SC) ->
    case SC#sconf.ets of
        undefined ->
             setup_ets(SC);
         E ->
             ets:match_delete(E,'_'),
             ets:insert(E, {num_files, 0}),
             ets:insert(E, {num_bytes, 0}),
             SC
    end.


start_stats(SC) ->
    {ok, Pid} = yaws_stats:start_link(),
    SC#sconf{stats = Pid}.


gserv_loop(GS, Ready, Rnum, Last) ->
    receive
        {From , status} ->
            From ! {self(), GS},
            gserv_loop(GS, Ready, Rnum, Last);
        {_From, next, Accepted} when Ready == [] ->
 	    close_accepted_if_max(GS,Accepted),
 	    New = acceptor(GS),
 	    GS2 = inc_con(GS),
            gserv_loop(GS2#gs{sessions = GS2#gs.sessions + 1}, Ready, Rnum, New);
        {_From, next, Accepted} ->
	    close_accepted_if_max(GS,Accepted),
            [{_Then, R}|RS] = Ready,
            R ! {self(), accept},
 	    GS2 = inc_con(GS),
            gserv_loop(GS2, RS, Rnum-1, R);
	{_From, decrement} ->
 	    GS2 = dec_con(GS),
 	    gserv_loop(GS2, Ready, Rnum, Last);
        {From, done_client, Int} ->
            GS2 = if
		      Int == 0 -> dec_con(GS#gs{sessions = GS#gs.sessions - 1});
		      Int > 0 -> dec_con(GS#gs{sessions = GS#gs.sessions - 1,
 					       reqs = GS#gs.reqs + Int})
                  end,
            if
                Rnum == 8 ->
                    From ! {self(), stop},
                    gserv_loop(GS2, Ready, Rnum, Last);
                Rnum < 8 ->
                    %% cache this process for 10 secs
                    gserv_loop(GS2, [{now(), From} | Ready], Rnum+1, Last)
            end;
        {'EXIT', Pid, Reason} ->
            case get(top) of
                Pid when Reason /= shutdown ->
                    error_logger:format("Top proc died, terminate gserv",[]),
                    {links, Ls} = process_info(self(), links),
                    foreach(fun(X) -> unlink(X), exit(X, shutdown) end, Ls),
                    exit(noserver);
                Pid ->
                    {links, Ls} = process_info(self(), links),
                    foreach(fun(X) -> unlink(X), exit(X, shutdown) end, Ls),
                    exit(normal);
                _ when Reason == failaccept ->
                     error_logger:format(
                       "Accept proc died, terminate gserv",[]),
                    {links, Ls} = process_info(self(), links),
                    foreach(fun(X) -> unlink(X), exit(X, shutdown) end, Ls),
                    exit(noserver);
                _ ->
                    gserv_loop(GS#gs{sessions = GS#gs.sessions - 1},
                               Ready, Rnum, Last)
            end;
        {From, stop} ->
            unlink(From),
            {links, Ls} = process_info(self(), links),
            foreach(fun(X) -> unlink(X), exit(X, shutdown) end, Ls),
            From ! {self(), ok},
            exit(normal);


        %% This code will shutdown all ready procs as well as the
        %% acceptor()
        {update_sconf, NewSc, OldSc, From, Updater} ->
            case lists:member(OldSc, GS#gs.group) of
                false ->
                    error_logger:error_msg("gserv: No found SC ~p/~p~n",
                                           [OldSc, GS#gs.group]),
                    erlang:error(nosc);
                true ->
		    Pid = OldSc#sconf.stats,
		    error_logger:info_msg("update_sconf: Stats pid ~p~n", [Pid]),
		    case Pid of
			undefined ->
			    ok;
			Pid when is_pid(Pid) ->
			    yaws_stats:stop(Pid)
		    end,
		    NewSc1 = case ?sc_has_statistics(NewSc) of
				 true ->
				     start_stats(NewSc);
				 false ->
				     NewSc
			     end,
                    stop_ready(Ready, Last),
                    NewSc2 = clear_ets_complete(NewSc1),
                    %% Need to insert the sconf at the same position
                    %% it previously was
                    NewG = lists:map(fun(Sc) when Sc == OldSc->
                                             NewSc2;
                                        (Other) ->
                                             Other
                                     end, GS#gs.group),

                    GS2 = GS#gs{group = NewG},
                    Ready2 = [],
                    Updater ! {updated_sconf, self(), NewSc2},
                    gen_server:reply(From, ok),
                    error_logger:info_msg("Updating sconf for server ~s~n",
                                          [yaws:sconf_to_srvstr(NewSc2)]),
                    New = acceptor(GS2),
                    gserv_loop(GS2, Ready2, 0, New)
            end;

        {delete_sconf, OldSc, From} ->
            case lists:member(OldSc, GS#gs.group) of
                false ->
                    error_logger:error_msg("gserv: No found SC ~n",[]),
                    erlang:error(nosc);
                true ->
		    Pid = OldSc#sconf.stats,
                    stop_ready(Ready, Last),
                    GS2 = GS#gs{group =  lists:delete(OldSc,GS#gs.group)},
                    Ready2 = [],
		    case ?sc_has_statistics(OldSc) of
			true ->
			    error_logger:info_msg("delete_sconf: Pid= ~p~n", [Pid]),
			    yaws_stats:stop(Pid);
			false ->
			    ok
		    end,
		    ets:delete(OldSc#sconf.ets),
                    gen_server:reply(From, ok),
                    error_logger:info_msg("Deleting sconf for server ~s~n",
                                          [yaws:sconf_to_srvstr(OldSc)]),
                    New = acceptor(GS2),
                    gserv_loop(GS2, Ready2, 0, New)
            end;

        {add_sconf, From, SC0, Adder} ->
	    SC = case ?sc_has_statistics(SC0) of
		     true ->
			 {ok, Pid} = yaws_stats:start_link(),
			 error_logger:info_msg("add_sconf: Pid= ~p~n", [Pid]),
			 SC0#sconf{stats=Pid};
		     false ->
			 SC0
		 end,
            stop_ready(Ready, Last),
            SC2 = setup_ets(SC),
	    GS2 = GS#gs{group =  GS#gs.group ++ [SC2]},
            Ready2 = [],
            Adder ! {added_sconf, self(), SC2},
            gen_server:reply(From, ok),
            error_logger:info_msg("Adding sconf for server ~s~n",
                                  [yaws:sconf_to_srvstr(SC)]),
            New = acceptor(GS2),
            gserv_loop(GS2, Ready2, 0, New);
        {check_cert_changed, From} ->
            Changed = 
                case GS#gs.ssl of
                    ssl ->
                        CertInfo = GS#gs.certinfo,			
			case lists:any(
                               fun(SC) ->  
                                       certinfo(SC#sconf.ssl) =/= CertInfo end,
                               GS#gs.group) of
                            true ->
                                yes;
                            false ->
                                no
                        end;
                    nossl ->
                        no
                end,
            if
                Changed == no ->
                    From ! {self(), no},
                    gserv_loop(GS, Ready, Rnum, Last);
                Changed == yes ->
                    error_logger:info_msg(
                      "Stopping ~s due to cert change\n",
                      [yaws:sconf_to_srvstr(hd(GS#gs.group))]),
                    {links, Ls0} = process_info(self(), links),
                    Ls = Ls0 -- [get(top)],
                    foreach(fun(X) -> unlink(X), exit(X, shutdown) end, Ls),
                    From ! {self(), yes},
                    unlink(get(top)),
                    get(top) ! {self(), certchanged},
                    exit(normal)
            end;
        {update_gconf, GC} ->
            stop_ready(Ready, Last),
            GS2 = GS#gs{gconf = GC},
            Ready2 = [],
            put(gc, GC),
            error_logger:info_msg("Updating gconf \n",[]),
            New = acceptor(GS2),
            gserv_loop(GS2, Ready2, 0, New)
    after (10 * 1000) ->
            %% collect old procs, to save memory
            {NowMega, NowSecs, _} = now(),
            R2 = lists:filter(fun({{ThenMega, ThenSecs, _}, Pid}) ->
                                      if
                                          NowMega > ThenMega;
                                          (NowSecs > (ThenSecs + 8)) ->
                                              Pid ! {self(), stop},
                                              false;
                                          true ->
                                              true
                                      end
                              end, Ready),
            gserv_loop(GS, R2, Rnum, Last)
    end.


stop_ready(Ready, Last) ->
    error_logger:info_msg("stop_ready(~p, ~p)~n", [Ready, Last]),
    unlink(Last),
    exit(Last, shutdown),

    lists:foreach(
      fun({_,Pid}) ->
              Pid ! {self(), stop}
      end, Ready).

gserv_stop(Gpid) ->
    Gpid ! {self(), stop},
    receive
        {Gpid, ok} ->
            ok
    end.

call_start_mod(SC) ->
    case SC#sconf.start_mod of
        undefined ->
            ok;
        Mod0 ->
            Mod = l2a(Mod0),
            case code:ensure_loaded(Mod) of
                {module, Mod} ->
                    error_logger:info_msg(
                      "Yaws: calling start_mod: ~p:start/1~n", [Mod]),
                    spawn(Mod, start, [SC]);
                Err ->
                    error_logger:format("Cannot load module ~p: ~p~n",
                                        [Mod,Err])
            end
    end.

listen_opts(SC) ->
    InetType = if 
                   is_tuple( SC#sconf.listen), size( SC#sconf.listen) == 8 ->
                       [inet6];
                 true ->
                       []
             end,
    [binary,
     {ip, SC#sconf.listen},
     {packet, http},
     {recbuf, 8192},
     {reuseaddr, true},
     {active, false}
     | proplists:get_value(listen_opts, SC#sconf.soptions, [])
    ] ++ InetType.

ssl_listen_opts(GC, SC, SSL) ->
    InetType = if 
                   is_tuple( SC#sconf.listen), size( SC#sconf.listen) == 8 ->
                       [inet6];
                 true ->
                       []
             end,
    [binary,
     {ip, SC#sconf.listen},
     {packet, http},
     {active, false} | ssl_listen_opts(GC, SSL)] ++ InetType.

ssl_listen_opts(GC, SSL) ->
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

         if SSL#ssl.cacertfile /= undefined  ->
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
         end,
         if SSL#ssl.depth /= undefined ->
                 {depth, SSL#ssl.depth};
            true ->
                 false
         end,
         if ?gc_use_old_ssl(GC) ->
                 false;
            true ->
                 {ssl_imp, new}
         end
        ],
    filter_false(L).


filter_false(L) ->
    [X || X <- L, X /= false].

do_accept(GS) when GS#gs.ssl == nossl ->
    ?Debug("wait in accept ... ~n",[]),
    gen_tcp:accept(GS#gs.l);
do_accept(GS) when GS#gs.ssl == ssl ->
    ssl:transport_accept(GS#gs.l).


initial_acceptor(GS) ->
    acceptor(GS).


acceptor(GS) ->
    proc_lib:spawn_link(?MODULE, acceptor0, [GS, self()]).
acceptor0(GS, Top) ->
    ?TC([{record, GS, gs}]),
    put(gc, GS#gs.gconf),
    X = do_accept(GS),
    Top ! {self(), next, X},
    case X of
        {ok, Client} ->
            if
                GS#gs.ssl == ssl ->
                    case ssl:ssl_accept(Client) of
                        ok ->
                            ok;
                        {error, closed} ->
                            Top ! {self(), decrement},
                            exit(normal);
                        {error, Reason} ->
                            error_logger:format("SSL accept failed: ~p~n",
                                                [Reason]),
			    Top ! {self(), decrement},
                            exit(normal)
                    end;
                true ->
                    ok
            end,

            case (GS#gs.gconf)#gconf.trace of  %% traffic trace
                {true, _} ->
                    case peername(Client, GS#gs.ssl) of
                        {ok, {IP, Port}} ->
                            Str = ?F("New (~p) connection from ~s:~w~n",
                                     [GS#gs.ssl, inet_parse:ntoa(IP),Port]),
                            yaws_log:trace_traffic(from_client, Str);
                        _ ->
                            ignore
                    end;
                _ ->
                    ok
            end,
            Res = (catch aloop(Client, GS,  0)),
            %% Skip closing the socket, as required by web sockets & stream
            %% processes.
            CloseSocket = (get(outh) =:= undefined) orelse 
                                (done_or_continue() =:= done),
            case CloseSocket of
                false -> ok;
                true ->
                    if
                        GS#gs.ssl == nossl ->
                            gen_tcp:close(Client);
                        GS#gs.ssl == ssl ->
                            ssl:close(Client)
                    end
            end,
            case Res of
                {ok, Int} when is_integer(Int) ->
                    Top ! {self(), done_client, Int};
                {'EXIT', normal} ->
		    Top ! {self(), decrement},
                    exit(normal);
                {'EXIT', shutdown} ->
                    exit(shutdown);
                {'EXIT', {error, einval}} ->
                    %% Typically clients that close their end of the socket
                    %% don't log. Happens all the time.
		    Top ! {self(), decrement},
                    exit(normal);
                {'EXIT', {{error, einval}, _}} ->
		    Top ! {self(), decrement},
                    exit(normal);
                {'EXIT', {error, closed}} ->
		    Top ! {self(), decrement},
                    exit(normal);
                {'EXIT', {{error, closed}, _}} ->
		    Top ! {self(), decrement},
                    exit(normal);
                {'EXIT', {{error, econnreset},_}} ->
		    Top ! {self(), decrement},
                    exit(normal);
                {'EXIT', Reason2} ->
                    error_logger:error_msg("Yaws process died: ~p~n",
                                           [Reason2]),
                    exit(shutdown)
            end,

            %% we cache processes
            receive
                {Top, stop} ->
                    exit(normal);
                {Top, accept} ->
                    erase_transients(),
                    acceptor0(GS, Top)
            end;
        {error, Reason} when ((Reason == timeout) or
                              (Reason == einval) or
                              (Reason == normal) or
                              (Reason == econnaborted)) ->
            %% The econnaborted is
            %% caused by recieving a RST when a SYN or SYN+ACK was expected.
            %% einval is reported to happen, it could be accept attempts
            %% on just recently reconfigured servers through --hup ... ???
            Top ! {self(), done_client, 0},
            receive
                {Top, stop} ->
                    exit(normal);
                {Top, accept} ->
                    acceptor0(GS, Top)
            end;
        {error, closed} ->
            %% This is what happens when we call yaws --stop
	    Top ! {self(), decrement},
            exit(normal);
        ERR ->
            %% When we fail to accept, the correct thing to do
            %% is to terminate yaws as an application, if we're running
            %% yaws as a standalone webserver, we want to restart the
            %% entire webserver, preferably through heart
            %% typical errors here are shortage of fds or memory
            error_logger:format("yaws: Failed to accept - terminating: ~p~n",
                                [ERR]),
            exit(failaccept)
    end.



%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

aloop(CliSock, GS, Num) ->
    init_db(),
    SSL = GS#gs.ssl,
    Head = yaws:http_get_headers(CliSock, SSL),
    ?Debug("Head = ~p~n", [Head]),
    case Head of
        {Req0, H0} when Req0#http_request.method /= bad_request ->
            {Req, H} = fix_abs_uri(Req0, H0),
            ?Debug("{Req, H} = ~p~n", [{Req, H}]),
            SC = pick_sconf(GS#gs.gconf, H, GS#gs.group),
            ?Debug("SC: ~s", [?format_record(SC, sconf)]),
            ?TC([{record, SC, sconf}]),
            ?Debug("Headers = ~s~n", [?format_record(H, headers)]),
            ?Debug("Request = ~s~n", [?format_record(Req, http_request)]),
            IP = case ?sc_has_access_log(SC) of
                     true ->
                         case peername(CliSock, SSL) of
                             {ok, {Ip, _Port}}  ->
                                 case ?gc_log_has_resolve_hostname(
                                         (GS#gs.gconf)) of
                                     true ->
                                         case inet:gethostbyaddr(Ip) of
                                             {ok, HE} ->
                                                 HE#hostent.h_name;
                                             _ ->
                                                 Ip
                                         end;
                                     false ->
                                         Ip
                                 end;
                             _ ->
                                 undefined
                         end;
                     _ ->
                         undefined
                 end,
            put(outh, #outh{}),
            put(sc, SC),
	    yaws_stats:hit(),
            Call = call_method(Req#http_request.method, CliSock, Req, H),
            handle_method_result(Call, CliSock, IP, GS, Req, H, Num);
        closed ->
            {ok, Num};
        _ ->
            % not even HTTP traffic
            exit(normal)
    end.


%% keep original dictionary but filter out eventual previous init_db
%% in erase_transients/0
init_db() ->
    put(init_db, lists:keydelete(init_db, 1, get())).

erase_transients() ->
    %% flush all messages
    Fun = fun(G) -> receive
                       _X -> G(G)
                    after 0 -> ok
                    end
          end,
    Fun(Fun),
    I = get(init_db),
    if I == undefined ->
            ok;
       is_list(I) ->
            erase(),
            lists:foreach(fun({K,V}) -> put(K,V) end, I)
    end.


handle_method_result(Res, CliSock, IP, GS, Req, H, Num) ->
    case Res of
        continue ->
            maybe_access_log(IP, Req, H),
            erase_transients(),
            aloop(CliSock, GS, Num+1);
        done ->
            maybe_access_log(IP, Req, H),
            erase_transients(),
            {ok, Num+1};
        {page, P} ->
            %% keep post_parse
            erase(query_parse),
            put(outh, #outh{}),
            case P of
                {Options, Page} ->
                    %% We got additional headers
                    %% for the page to deliver.
                    %%
                    %% Might be useful for
                    %% `Vary' or
                    %% `Content-Location'.
                    deepforeach(
                      fun(X) -> case X of
                                    {header, Header} ->
                                        yaws:accumulate_header(Header);
                                    _Something ->
                                        ?Debug("Got ~p in page option list.",
                                               [_Something])
                                end
                      end,
                      Options);
                Page ->
                    ok
            end,
            put (sc, (get(sc))#sconf{appmods = []}),
            Call = call_method(Req#http_request.method,
                               CliSock,
                               Req#http_request{path = {abs_path, Page}},
                               H#headers{content_length = undefined}),
            handle_method_result(Call, CliSock, IP, GS, Req, H, Num)
    end.


peername(CliSock, ssl) ->
    ssl:peername(CliSock);
peername(CliSock, nossl) ->
    inet:peername(CliSock).


deepforeach(_F, []) ->
    ok;
deepforeach(F, [H|T]) ->
    deepforeach(F, H),
    deepforeach(F, T);
deepforeach(F, X) ->
    F(X).


fix_abs_uri(Req, H) ->
    case Req#http_request.path of
        {absoluteURI, _Scheme, Host0, Port, RawPath} ->
            Host = case Port of
                       P when is_integer(P) ->
                           Host0 ++ [$: | integer_to_list(P)];
                                                % Is this ok?
                       _ ->
                           Host0
                   end,
            {Req#http_request{path={abs_path, RawPath}},
             H#headers{host=Host}};
        _ -> {Req, H}
    end.


%% case-insensitive compare servername and ignore any optional
%% :Port postfix
comp_sname(Hname, Sname) ->
    hd(string:tokens(yaws:to_lower(Hname), ":")) =:=
        hd(string:tokens(yaws:to_lower(Sname), ":")).

pick_sconf(GC, H, Group) ->
    case H#headers.host of
        undefined when ?gc_pick_first_virthost_on_nomatch(GC) ->
            hd(Group);
        Host ->
            pick_host(GC, Host, Group, Group)
    end.

%% Compare Host against [] in case caller sends an empty Host header
pick_host(GC, Host, SCs, Group)
  when Host == []; SCs == [] ->
    if
        ?gc_pick_first_virthost_on_nomatch(GC) ->
            hd(Group);
        true ->
            yaws_debug:format("Drop req since ~p doesn't match any "
                              "servername \n", [Host]),
            exit(normal)
    end;
pick_host(GC, Host, [SC|T], Group) ->
    case comp_sname(Host, SC#sconf.servername) of
        true -> SC;
        false -> pick_host(GC, Host, T, Group)
    end.

maybe_auth_log(Item, ARG) ->
    GC=get(gc),
    SC=get(sc),
    case ?gc_has_auth_log(GC) of
        false ->
            ok;
        true ->
            Req = ARG#arg.req,
            {IP,_} = ARG#arg.client_ip_port,
            Path = safe_decode_path(Req#http_request.path),
            yaws_log:authlog(SC#sconf.servername, IP, Path, Item)
    end.



maybe_access_log(Ip, Req, H) ->
    SC=get(sc),
    case ?sc_has_access_log(SC) of
        true ->
            Status = case yaws:outh_get_status_code() of
                         undefined -> "-";
                         I -> integer_to_list(I)
                     end,
            Len = case Req#http_request.method of
                      'HEAD' -> "-";
                      _ -> case yaws:outh_get_contlen() of
                               undefined ->
                                   case yaws:outh_get_act_contlen() of
                                       undefined -> "-";
                                       Actlen -> integer_to_list(Actlen)
                                   end;
                               I2 -> integer_to_list(I2)
                           end
                  end,
            Ver = case Req#http_request.version of
                      {1,0} ->
                          "HTTP/1.0";
                      {1,1} ->
                          "HTTP/1.1";
                      {0,9} ->
                          "HTTP/0.9"
                  end,
            Path = safe_decode_path(Req#http_request.path),
            Meth = yaws:to_list(Req#http_request.method),
            Referrer = optional_header(H#headers.referer),
            UserAgent = optional_header(H#headers.user_agent),
            User = case H#headers.authorization of
                       {User0, _Pass, _OrigString} ->
                           User0;
                       _ ->
                           "-"
                   end,
            yaws_log:accesslog(SC#sconf.servername, Ip, User,
                               [Meth, $\s, Path, $\s, Ver],
                               Status, Len, Referrer, UserAgent);
        false ->
            ignore
    end.

safe_decode_path(Path) ->
    case (catch decode_path(Path)) of
        {'EXIT', _} ->
            "/undecodable_path";
        Val ->
            Val
    end.


decode_path({abs_path, Path}) ->
    yaws_api:url_decode(Path).

optional_header(Item) ->
    case Item of
        undefined -> "-";
        Item -> Item
    end.

%% ret:  continue | done
'GET'(CliSock, Req, Head) ->
    no_body_method(CliSock, Req, Head).


'POST'(CliSock, Req, Head) ->
    ?Debug("POST Req=~s~n H=~s~n", [?format_record(Req, http_request),
                                    ?format_record(Head, headers)]),

    OtherHeaders = Head#headers.other,
    Continue =
        case lists:keysearch("Expect", 3, OtherHeaders) of
            {value, {_,_,"Expect",_,Value}} ->
                Value;
            _ ->
                ""
        end,
    case yaws:to_lower(Continue) of
        "100-continue" ->
            deliver_100(CliSock),
            body_method(CliSock, Req, Head);
        _ ->
            body_method(CliSock, Req, Head)
    end.



un_partial({partial, Bin}) ->
    Bin;
un_partial(Bin) ->
    Bin.


call_method(Method, CliSock, Req, H) ->
    case Method of
        F when is_atom(F) ->
            ?MODULE:F(CliSock, Req, H);
        L when is_list(L) ->
            handle_extension_method(L, CliSock, Req, H)
    end.


'HEAD'(CliSock, Req, Head) ->
    put(acc_content, discard),
    no_body_method(CliSock, Req, Head).

not_implemented(CliSock, Req, Head) ->
    SC=get(sc),
    ok = yaws:setopts(CliSock, [{packet, raw}, binary], yaws:is_ssl(SC)),
    flush(CliSock, Head#headers.content_length),
    deliver_501(CliSock, Req).


'TRACE'(CliSock, Req, Head) ->
    not_implemented(CliSock, Req, Head).

'OPTIONS'(CliSock, Req, Head) ->
    case Req#http_request.path of
        '*' ->
            % Handle "*" as per RFC2616 section 5.1.2
            deliver_options(CliSock, Req, ['GET', 'HEAD', 'OPTIONS',
                                           'PUT', 'POST', 'DELETE']);
        _ ->
            no_body_method(CliSock, Req, Head)
    end.

'PUT'(CliSock, Req, Head) ->
    ?Debug("PUT Req=~p~n H=~p~n", [?format_record(Req, http_request),
                                   ?format_record(Head, headers)]),
    body_method(CliSock, Req, Head).


'DELETE'(CliSock, Req, Head) ->
    no_body_method(CliSock, Req, Head).


%%%
%%% WebDav specifics: PROPFIND, MKCOL,....
%%%
'PROPFIND'(CliSock, Req, Head) ->
    %%?elog("PROPFIND Req=~p H=~p~n",
    %%                   [?format_record(Req, http_request),
    %%                    ?format_record(Head, headers)]),
    body_method(CliSock, Req, Head).

'MOVE'(CliSock, Req, Head) ->
    no_body_method(CliSock, Req, Head).

'COPY'(CliSock, Req, Head) ->
    no_body_method(CliSock, Req, Head).


body_method(CliSock, Req, Head) ->
    SC=get(sc),
    ok = yaws:setopts(CliSock, [{packet, raw}, binary], yaws:is_ssl(SC)),
    PPS = SC#sconf.partial_post_size,
    Bin = case Head#headers.content_length of
              undefined ->
                  case Head#headers.transfer_encoding of
                      "chunked" ->
                          get_chunked_client_data(CliSock, [],
                                                  yaws:is_ssl(SC));
                      _ ->
                          <<>>
                              end;
              Len when is_integer(PPS) ->
                  Int_len = list_to_integer(Len),
                  if
                      Int_len == 0 ->
                          <<>>;
                      PPS < Int_len ->
                          {partial, get_client_data(CliSock, PPS,
                                                    yaws:is_ssl(SC))};
                      true ->
                          get_client_data(CliSock, Int_len,
                                          yaws:is_ssl(SC))
                  end;
              Len when PPS == nolimit ->
                  Int_len = list_to_integer(Len),
                  if
                      Int_len == 0 ->
                          <<>>;
                      true ->
                          get_client_data(CliSock, Int_len,
                                          yaws:is_ssl(SC))
                  end
          end,
    ?Debug("Request data = ~s~n", [binary_to_list(un_partial(Bin))]),
    ARG = make_arg(CliSock, Head, Req, Bin),
    handle_request(CliSock, ARG, size(un_partial(Bin))).


'MKCOL'(CliSock, Req, Head) ->
    no_body_method(CliSock, Req, Head).

no_body_method(CliSock, Req, Head) ->
    SC=get(sc),
    ok = yaws:setopts(CliSock, [{packet, raw}, binary], yaws:is_ssl(SC)),
    flush(CliSock, Head#headers.content_length),
    ARG = make_arg(CliSock, Head, Req, undefined),
    handle_request(CliSock, ARG, 0).


make_arg(CliSock, Head, Req, Bin) ->
    SC = get(sc),
    IP = if
             is_port(CliSock) ->
                 case inet:peername(CliSock) of
                     {ok, IpPort} ->
                         IpPort;
                     _ ->
                         {unknown, unknown}
                 end;
             true ->
                 case ssl:peername(CliSock) of
                     {ok, IpPort} ->
                         IpPort;
                     _ ->
                         {unknown, unknown}
                 end
         end,
    ARG = #arg{clisock = CliSock,
               client_ip_port = IP,
               headers = Head,
               req = Req,
               opaque = SC#sconf.opaque,
               pid = self(),
               docroot = SC#sconf.docroot,
               docroot_mount = "/",
               clidata = Bin
              },
    apply(SC#sconf.arg_rewrite_mod, arg_rewrite, [ARG]).


handle_extension_method("PROPFIND", CliSock, Req, Head) ->
    'PROPFIND'(CliSock, Req, Head);
handle_extension_method("MKCOL", CliSock, Req, Head) ->
    'MKCOL'(CliSock, Req, Head);
handle_extension_method("MOVE", CliSock, Req, Head) ->
    'MOVE'(CliSock, Req, Head);
handle_extension_method("COPY", CliSock, Req, Head) ->
    'COPY'(CliSock, Req, Head);
handle_extension_method(_Method, CliSock, Req, Head) ->
    not_implemented(CliSock, Req, Head).


%% Return values:
%% continue, done, {page, Page}

handle_request(CliSock, ARG, N) ->
    Req = ARG#arg.req,
    ?Debug("SrvReq=~s~n",[?format_record(Req, http_request)]),
    case Req#http_request.path of
        {abs_path, RawPath} ->
            case (catch yaws_api:url_decode_q_split(RawPath)) of
                {'EXIT', _} ->   %% weird broken cracker requests
                    deliver_400(CliSock, Req);
                {DecPath, QueryPart} ->
                    %% http://<server><port><DecPath>?<QueryPart>
                    %% DecPath is stored in arg.server_path and is equiv to
                    %%SCRIPT_PATH + PATH_INFO  (where PATH_INFO may be empty)

                    QueryString = case QueryPart of
                                      [] ->
                                          undefined;
                                      _ ->
                                          QueryPart
                                  end,


                    SC=get(sc),

                    %% by this stage, ARG#arg.docroot_mount is either "/" ,
                    %% or has been set by a rewrite module.
                    %%!todo - retrieve 'vdir' definitions from main part of
                    %% config file rather than
                    %% rely on rewrite module to dig them out of opaque.

                    ARGvdir = ARG#arg.docroot_mount,
                    %% here we make sure that the conf file, or any rewrite mod
                    %% wrote  nothing, or something sensible into
                    %% arg.docroot_mount
                    %% It must be empty, or of the form "/path/" where path
                    %% may be  further slash-separated.
                    %%
                    %%!todo - review - is handle_request
                    %% (which is presumably performance
                    %%sensitive) really the place for sanity checks?
                    %% Presumably this sort of check is trivial enough
                    %% that it'll have negligible impact.

                    VdirSanity = case ARGvdir of
                                     "/" ->
                                         sane;
                                     [$/|_] ->
                                         case string:right(ARGvdir,1) of
                                             "/" when length(ARGvdir) > 2 ->
                                                 sane;
                                             _ ->
                                                 loopy
                                         end;
                                     _ ->
                                         loopy
                                 end,


                    case VdirSanity of
                        loopy ->
                            %%!todo - log somewhere?
                            error_logger:format(
                              "BAD arg.docroot_mount data: '~p'\n",[ARGvdir]),
                            deliver_xxx(CliSock, Req, 500),
                            exit(normal);
                        _ ->
                            ok
                    end,

                    {IsAuth, ARG1} =
                        case is_auth(ARG, DecPath,ARG#arg.headers,
                                     SC#sconf.authdirs) of
                            {true, User} ->
                                {true, set_auth_user(ARG, User)};
                            E ->
                                {E, ARG}
                        end,


                    IsRev = is_revproxy(ARG, DecPath, SC),
                    IsRedirect = is_redirect_map(DecPath,
                                                 SC#sconf.redirect_map),

                    case {IsAuth, IsRev, IsRedirect} of
                        {_, _, {true, Redir}} ->
                            deliver_302_map(CliSock, Req, ARG1, Redir);
                        {true, false, _} ->
                            %%'main' branch so to speak. Most
                            %% requests pass through here.

                            UT   = url_type(DecPath, ARG1#arg.docroot,
                                            ARG1#arg.docroot_mount),
                            ARG2 = ARG1#arg{
                                     server_path = DecPath,
                                     querydata = QueryString,
                                     fullpath = UT#urltype.fullpath,
                                     prepath = UT#urltype.dir,
                                     pathinfo = UT#urltype.pathinfo
                                    },

                            %%!todo - remove special treatment of
                            %% appmod here.
                            %% (after suitable deprecation period)
                            %% - prepath & pathinfo are applicable to
                            %% other types  of dynamic url too
                            %%   replace: appmoddata with pathinfo &
                            %% appmod_prepath with prepath.
                            case UT#urltype.type of
                                appmod ->
                                    {_Mod, PathInfo} = UT#urltype.data,
                                    ARG3 =
                                        ARG2#arg{
                                          appmoddata =
                                          case PathInfo of
                                              undefined ->
                                                  undefined;
                                              "/" ->
                                                  "/";
                                              _ ->
                                                  lists:dropwhile(
                                                    fun(C) -> C == $/
                                                                  end,
                                                    PathInfo)
                                          end,
                                          appmod_prepath =
                                          UT#urltype.dir
                                         };
                                _ ->
                                    ARG3 = ARG2
                            end,

                            handle_ut(CliSock, ARG3, UT, N);
                        {true, {true, PP}, _} ->
                            yaws_revproxy:init(CliSock, ARG1, DecPath,
                                               QueryString, PP, N);
                        {false_403, _, _} ->
                            deliver_403(CliSock, Req);
                        {{false, AuthMethods, Realm}, _, _} ->
                            UT = #urltype{
                              type = {unauthorized, AuthMethods, Realm},
                              path = DecPath},
                            handle_ut(CliSock, ARG, UT, N)

                    end
            end;
        {scheme, _Scheme, _RequestString} ->
            deliver_501(CliSock, Req);
        _ ->                                    % for completeness
            deliver_400(CliSock, Req)
    end.

set_auth_user(ARG, User) ->
    H = ARG#arg.headers,
    Auth =
        case H#headers.authorization of
            {_User, Pass, Orig} ->
                {User, Pass, Orig};
            undefined ->
                {User, undefined, undefined};
            E ->
                E
        end,
    H2 = H#headers{authorization = Auth},
    ARG#arg{headers = H2}.

%% Call is_auth(...)/5 with a default value.
is_auth(ARG, Req_dir, H, L) ->
    is_auth(ARG, Req_dir, H, L, {true, []}).

%% Either no authentication was done or all methods returned false
is_auth(_ARG, _Req_dir, _H, [], {Ret, Auth_headers}) ->
    yaws:outh_set_auth(Auth_headers),
    Ret;

is_auth(ARG, Req_dir, H, [{Auth_dir, Auth_methods}|T], {Ret, Auth_headers}) ->
    case lists:prefix(Auth_dir, Req_dir) of
	true ->
	    Auth_H = H#headers.authorization,
	    case handle_auth(ARG, Auth_H, Auth_methods, false) of

		%% If we auth using an authmod we need to return User
		%% so that we can set it in ARG.
		{true, User} ->
		    {true, User};
		false ->
		    L = Auth_methods#auth.headers,
		    Realm = Auth_methods#auth.realm,
		    is_auth(ARG, Req_dir, H, T, {{false, Auth_methods, Realm},
						 L ++ Auth_headers});
                {false, Realm} ->
                    L = Auth_methods#auth.headers,
                    is_auth(ARG, Req_dir, H, T, {{false, Auth_methods, Realm},
                                                 L ++ Auth_headers});
		Is_auth ->
		    Is_auth
	    end;
	false ->
	    is_auth(ARG, Req_dir, H, T, {Ret, Auth_headers})
    end.

handle_auth(ARG, _Auth_H, #auth{realm = Realm,
                                users=[], pam=false, mod = []}, Ret) ->
    maybe_auth_log({401, Realm}, ARG),
    Ret;

handle_auth(ARG, Auth_H, Auth_methods = #auth{mod = Mod}, Ret) when Mod /= [] ->
    case catch Mod:auth(ARG, Auth_methods) of
	{'EXIT', Reason} ->
            L = ?F("authmod crashed ~n~p:auth(~p, ~n ~p) \n"
		   "Reason: ~p~n"
                   "Stack: ~p~n",
		   [Mod, ARG, Auth_methods, Reason,
                    erlang:get_stacktrace()]),
            handle_crash(ARG, L),
            deliver_accumulated(ARG#arg.clisock),
            exit(normal);

	%% appmod means the auth headers are undefined, i.e. false.
	%% TODO: change so that authmods simply return true/false
	{appmod, _AppMod} ->
	    handle_auth(ARG, Auth_H, Auth_methods#auth{mod = []}, Ret);
	{true, User} ->
	    {true, User};
	true ->
	    true;
	{false, Realm} ->
	    handle_auth(ARG, Auth_H, Auth_methods#auth{mod = []},
                        {false, Realm});
	_ ->
	    maybe_auth_log(403, ARG),
	    false_403
    end;

%% if the headers are undefined we do not need to check Pam or Users
handle_auth(ARG, undefined, Auth_methods, Ret) ->
    handle_auth(ARG, undefined, Auth_methods#auth{pam = false, users= []}, Ret);

handle_auth(ARG, {User, Password, OrigString},
            Auth_methods = #auth{pam = Pam}, Ret) when Pam /= false ->
    case yaws_pam:auth(User, Password) of
	{yes, _} ->
	    maybe_auth_log({ok, User}, ARG),
	    true;
	{no, _Rsn} ->
	    handle_auth(ARG, {User, Password, OrigString},
                        Auth_methods#auth{pam = false}, Ret)
    end;

handle_auth(ARG, {User, Password, OrigString},
            Auth_methods = #auth{users = Users}, Ret) when Users /= [] ->
    case member({User, Password}, Users) of
	true ->
	    maybe_auth_log({ok, User}, ARG),
	    true;
	false ->
	    handle_auth(ARG, {User, Password, OrigString},
                        Auth_methods#auth{users = []}, Ret)
    end.


is_revproxy(ARG, Path, SC = #sconf{revproxy = RevConf}) ->
    IsFwd = ?sc_forward_proxy(SC),
    %% Note: these are mututally exclusive.
    case {IsFwd, RevConf} of
        {false, []} ->
            false;
        {false, _} ->
            is_revproxy1(Path, RevConf);
        {true, _} ->
            {true, {"/", fwdproxy_url(ARG)}}
    end.

is_revproxy1(_,[]) ->
    false;
is_revproxy1(Path, [{Prefix, URL} | Tail]) ->
    case yaws:is_prefix(Prefix, Path) of
        {true,_} ->
            {true, {Prefix,URL}};
        false ->
            is_revproxy1(Path, Tail)
    end.

is_redirect_map(_, []) ->
    false;
is_redirect_map(Path, [E={Prefix, _URL, _AppendMode}|Tail]) ->
    case yaws:is_prefix(Prefix, Path) of
        {true, _} ->
            {true, E};
        false ->
            is_redirect_map(Path, Tail)
    end.

%% Find out what which module to call when urltype is unauthorized
%% Precedence is:
%% 1. SC#errormod401 if it's not default
%% 2. First authdir which prefix the requested dir and has an authmod set
%% 3. yaws_outmod

get_unauthorized_outmod(_Req_dir, _Auth_dirs, Errormod401)
  when Errormod401 /= yaws_outmod ->
    Errormod401;

get_unauthorized_outmod(_Req_dir, [], Errormod401) ->
    Errormod401;

get_unauthorized_outmod(Req_dir, [{Auth_dir, Auth}|T], Errormod401) ->
    case lists:prefix(Auth_dir, Req_dir) of
	true ->
	    case Auth#auth.mod /= [] of
		true ->
		    Auth#auth.mod;
		false ->
		    get_unauthorized_outmod(Req_dir, T, Errormod401)
	    end;
	false ->
	    get_unauthorized_outmod(Req_dir, T, Errormod401)
    end.



%% Return values:
%% continue, done, {page, Page}

handle_ut(CliSock, ARG, UT = #urltype{type = regular}, _N) ->
    Req = ARG#arg.req,
    H = ARG#arg.headers,

    Regular_allowed = ['GET', 'HEAD', 'OPTIONS'],
    if
        Req#http_request.method == 'GET';
        Req#http_request.method == 'HEAD' ->
            ETag = yaws:make_etag(UT#urltype.finfo),
            Range = case H#headers.if_range of
                        [34|_] = Range_etag when Range_etag /= ETag ->
                            all;
                        _ ->
                            requested_range(
                              H#headers.range,
                              (UT#urltype.finfo)#file_info.size)
                    end,
            case Range of
                error -> deliver_416(
                           CliSock, Req,
                           (UT#urltype.finfo)#file_info.size);
                _ ->
                    Do_deliver =
                        case Req#http_request.method of
                            'GET' -> fun() -> deliver_file(CliSock, Req,
                                                           UT, Range) end;
                            'HEAD' -> fun() -> deliver_accumulated(CliSock),
                                               done end
                        end,
                    case H#headers.if_none_match of
                        undefined ->
                            case H#headers.if_match of
                                undefined ->
                                    case H#headers.if_modified_since of
                                        undefined ->
                                            yaws:outh_set_static_headers
                                              (Req, UT, H, Range),
                                            Do_deliver();
                                        UTC_string ->
                                            case yaws:is_modified_p(
                                                   UT#urltype.finfo,
                                                   UTC_string) of
                                                true ->
                                                    yaws:outh_set_static_headers
                                                      (Req, UT, H, Range),
                                                    Do_deliver();
                                                false ->
                                                    yaws:outh_set_304_headers(
                                                      Req, UT, H),
                                                    deliver_accumulated(
                                                      CliSock),
                                                    done_or_continue()
                                            end
                                    end;
                                Line ->
                                    case member(ETag,
                                                yaws:split_sep(Line, $,)) of
                                        true ->
                                            yaws:outh_set_static_headers(
                                              Req, UT, H, Range),
                                            Do_deliver();
                                        false ->
                                            deliver_xxx(CliSock, Req, 412)
                                    end
                            end;
                        Line ->
                            case member(ETag,yaws:split_sep(Line, $,)) of
                                true ->
                                    yaws:outh_set_304_headers(Req, UT, H),
                                    deliver_accumulated(CliSock),
                                    done_or_continue();
                                false ->
                                    yaws:outh_set_static_headers
                                      (Req, UT, H, Range),
                                    Do_deliver()
                            end
                    end
            end;
        Req#http_request.method == 'OPTIONS' ->
            deliver_options(CliSock, Req, Regular_allowed);
        true ->
            deliver_405(CliSock, Req, Regular_allowed)
    end;

handle_ut(CliSock, ARG, UT = #urltype{type = yaws}, N) ->
    Req = ARG#arg.req,
    H = ARG#arg.headers,

    ?Debug("UT = ~s~n", [?format_record(UT, urltype)]),
    Yaws_allowed = ['GET', 'POST', 'HEAD', 'OPTIONS'],
    if
        Req#http_request.method == 'GET';
        Req#http_request.method == 'POST';
        Req#http_request.method == 'HEAD' ->
            yaws:outh_set_dyn_headers(Req, H, UT),
            do_yaws(CliSock, ARG, UT, N);
        Req#http_request.method == 'OPTIONS' ->
            deliver_options(CliSock, Req, Yaws_allowed);
        true ->
            deliver_405(CliSock, Req, Yaws_allowed)
    end;

handle_ut(CliSock, ARG, UT = #urltype{type = {unauthorized, Auth, Realm}}, N) ->
    Req = ARG#arg.req,
    H   = ARG#arg.headers,
    SC  = get(sc),
    yaws:outh_set_dyn_headers(Req, H, UT),

    %% outh_set_dyn headers sets status to 200 by default
    %% so we need to set it 401
    yaws:outh_set_status_code(401),
    Outmod = get_unauthorized_outmod(UT#urltype.path, SC#sconf.authdirs,
                                     SC#sconf.errormod_401),
    OutFun = fun (A) ->
                     case catch Outmod:out401(A, Auth, Realm) of
                         {'EXIT', {undef, _}} ->
                             %% Possibly a deprecated warning
                             Outmod:out(A);
                         {'EXIT', Reason} ->
                             exit(Reason);
                         Result ->
                             Result
                     end
             end,
    DeliverFun = fun (A) -> finish_up_dyn_file(A, CliSock) end,
    deliver_dyn_part(CliSock, 0, "appmod", N, ARG, UT, OutFun, DeliverFun);

handle_ut(CliSock, ARG, UT = #urltype{type = error}, N) ->
    Req = ARG#arg.req,
    H = ARG#arg.headers,
    SC=get(sc),GC=get(gc),
    case UT#urltype.type of
        error when SC#sconf.xtra_docroots == [] ->
            yaws:outh_set_dyn_headers(Req, H, UT),
            deliver_dyn_part(CliSock,
                             0, "404",
                             N,
                             ARG,UT,
                             fun(A)->(SC#sconf.errormod_404):
                                         out404(A,GC,SC)
                             end,
                             fun(A)->finish_up_dyn_file(A, CliSock)
                             end
                            );
        error ->
            SC2 = SC#sconf{docroot = hd(SC#sconf.xtra_docroots),
                           xtra_docroots = tl(SC#sconf.xtra_docroots)},
            put(sc, SC2),

            %%!todo - review & change. rewriting the docroot and xtra_docroots
            %% is not a good way to handle the xtra_docroot feature because
            %% it makes less information available to the subsequent calls -
            %% this is especially an issue for a nested ssi.
            ARG2 = ARG#arg{docroot = SC2#sconf.docroot},
            handle_request(CliSock, ARG2, N)
    end;


handle_ut(CliSock, ARG, UT = #urltype{type = directory}, N) ->
    Req = ARG#arg.req,
    H = ARG#arg.headers,
    SC=get(sc),

    if (?sc_has_dir_listings(SC)) ->
            Directory_allowed = ['GET', 'HEAD', 'OPTIONS'],
            if
                Req#http_request.method == 'GET';
                Req#http_request.method == 'HEAD' ->
                    yaws:outh_set_dyn_headers(Req, H, UT),
                    P = UT#urltype.fullpath,
                    yaws_ls:list_directory(ARG, CliSock, UT#urltype.data,
                                           P, Req,
                                           ?sc_has_dir_all_zip(SC));
                Req#http_request.method == 'OPTIONS' ->
                    deliver_options(CliSock, Req, Directory_allowed);
                true ->
                    deliver_405(CliSock, Req, Directory_allowed)
            end;
       true ->
            handle_ut(CliSock, ARG, #urltype{type = error}, N)
    end;



handle_ut(CliSock, ARG, UT = #urltype{type = redir}, _N) ->
    Req = ARG#arg.req,
    H = ARG#arg.headers,
    yaws:outh_set_dyn_headers(Req, H, UT),
    deliver_302(CliSock, Req, ARG, UT#urltype.path);

handle_ut(CliSock, ARG, UT = #urltype{type = appmod}, N) ->
    Req = ARG#arg.req,
    H = ARG#arg.headers,
    yaws:outh_set_dyn_headers(Req, H, UT),
    {Mod,_} = UT#urltype.data,
    deliver_dyn_part(CliSock,
                     0, "appmod",
                     N,
                     ARG,UT,
                     fun(A)->Mod:out(A) end,
                     fun(A)->finish_up_dyn_file(A, CliSock) end
                    );


handle_ut(CliSock, ARG, UT = #urltype{type = cgi}, N) ->
    Req = ARG#arg.req,
    H = ARG#arg.headers,
    yaws:outh_set_dyn_headers(Req, H, UT),
    deliver_dyn_part(CliSock,
                     0, "cgi",
                     N,
                     ARG,UT,
                     fun(A)->yaws_cgi:call_cgi(
                               A,flatten(UT#urltype.fullpath))
                     end,
                     fun(A)->finish_up_dyn_file(A, CliSock) end
                    );

handle_ut(CliSock, ARG, UT = #urltype{type = fcgi}, N) ->
    error_logger:error_msg("*** handle_ut: type=fcgi~n"),    %%@@@
    Req = ARG#arg.req,
    H = ARG#arg.headers,
    yaws:outh_set_dyn_headers(Req, H, UT),
    deliver_dyn_part(CliSock,
                     0, "fcgi",
                     N,
                     ARG,UT,
                     fun(A)->yaws_cgi:call_fcgi_responder(A)
                     end,
                     fun(A)->finish_up_dyn_file(A, CliSock)
                     end
                    );

handle_ut(CliSock, ARG, UT = #urltype{type = dav}, N) ->
    Req = ARG#arg.req,
    H = ARG#arg.headers,
    SC=get(sc),
    Next =
        if
            Req#http_request.method == 'PUT' ->
                fun(A) -> yaws_dav:put(SC, A) end;
            Req#http_request.method == 'DELETE' ->
                fun(A) -> yaws_dav:delete(A) end;
            Req#http_request.method == "PROPFIND" ->
                fun(A)-> yaws_dav:propfind(A) end;
            Req#http_request.method == "MOVE" ->
                fun(A)-> yaws_dav:move(A) end;
            Req#http_request.method == "COPY" ->
                fun(A)-> yaws_dav:copy(A) end;
            Req#http_request.method == "MKCOL" ->
                fun(A)-> yaws_dav:mkcol(A) end;
            Req#http_request.method == 'GET';
            Req#http_request.method == 'HEAD' ->
                case prim_file:read_file_info(UT#urltype.fullpath) of
                    {ok, FI} when FI#file_info.type == regular ->
                        {regular, FI};
                    _ ->
                        error
                end;
            true ->
                error
        end,
    case Next of
        error ->
            handle_ut(CliSock, ARG, #urltype{type = error}, N);
        {regular, Finfo} ->
            handle_ut(CliSock, ARG, UT#urltype{type = regular,
                                               finfo = Finfo}, N);
        _ ->
            yaws:outh_set_dyn_headers(Req, H, UT),
            deliver_dyn_part(CliSock,
                             0, "dav",
                             N,
                             ARG,UT,
                             Next,
                             fun(A)->finish_up_dyn_file(A, CliSock)  end
                            )
    end;

handle_ut(CliSock, ARG, UT = #urltype{type = php}, N) ->
    Req = ARG#arg.req,
    H = ARG#arg.headers,
    GC=get(gc),
    yaws:outh_set_dyn_headers(Req, H, UT),
    deliver_dyn_part(CliSock,
                     0, "php",
                     N,
                     ARG,UT,
                     fun(A)->yaws_cgi:call_cgi(
                               A,
                               GC#gconf.phpexe,
                               flatten(UT#urltype.fullpath))
                     end,
                     fun(A)->finish_up_dyn_file(A, CliSock)
                     end
                    ).

done_or_continue() ->
    case yaws:outh_get_doclose() of
        true -> done;
        false -> continue;
        keep_alive -> continue;
	undefined -> continue
    end.

%% we may have content,

new_redir_h(OH, Loc) ->
    new_redir_h(OH, Loc, 302).

new_redir_h(OH, Loc, Status) ->
    OH2 = OH#outh{status = Status,
                  location = Loc},
    put(outh, OH2).



%% we must deliver a 302 if the browser asks for a dir
%% without a trailing / in the HTTP req
%% otherwise the relative urls in /dir/index.html will be broken.

%%!todo - review.
%% Why is DecPath being tokenized around "?" - only to reassemble??
%% What happens when Path is not flat?

deliver_302(CliSock, _Req, Arg, Path) ->
    ?Debug("in redir 302 ",[]),
    H = get(outh),
    SC=get(sc),
    Scheme = yaws:redirect_scheme(SC),
    Headers = Arg#arg.headers,
    DecPath = yaws_api:url_decode(Path),
    RedirHost = yaws:redirect_host(SC, Headers#headers.host),
    Loc = case string:tokens(DecPath, "?") of
              [P] ->
                  ["Location: ", Scheme, RedirHost, P, "\r\n"];
              [P, Q] ->
                  ["Location: ", Scheme, RedirHost, P, "?", Q, "\r\n"]
          end,

    new_redir_h(H, Loc),
    deliver_accumulated(CliSock),
    done_or_continue().


deliver_302_map(CliSock, Req, Arg,
                {_Prefix,URL,Mode})  when is_record(URL,url) ->
    ?Debug("in redir 302 ",[]),
    H = get(outh),
    DecPath = safe_decode_path(Req#http_request.path),
    {P, Q} = yaws:split_at(DecPath, $?),
    LocPath = yaws_api:format_partial_url(URL, get(sc)),
    Loc = if
              Mode == append ->
                  Newpath = filename:join([URL#url.path ++ P]),
                  NLocPath = yaws_api:format_partial_url(
                               URL#url{path = Newpath}, get(sc)),
                  case Q of
                      [] ->
                          ["Location: ", NLocPath, "\r\n"];
                      _Q ->
                          ["Location: ", NLocPath, "?", Q, "\r\n"]
                  end;
              Mode == noappend,Q == [] ->
                  ["Location: ", LocPath, "\r\n"];
              Mode == noappend,Q /= [] ->
                  ["Location: ", LocPath, "?", Q, "\r\n"]
          end,
    Headers = Arg#arg.headers,
    {DoClose, _Chunked} = yaws:dcc(Req, Headers),
    new_redir_h(H#outh{
                  connection  = yaws:make_connection_close_header(DoClose),
                  doclose = DoClose,
                  server = yaws:make_server_header(),
                  chunked = false,
                  date = yaws:make_date_header()
                 }, Loc),

    deliver_accumulated(CliSock),
    done_or_continue().

deliver_options(CliSock, _Req, Options) ->
    H = #outh{status = 200,
              doclose = false,
              chunked = false,
              server = yaws:make_server_header(),
              date = yaws:make_date_header(),
              allow = yaws:make_allow_header(Options)},
    put(outh, H),
    deliver_accumulated(CliSock),
    continue.

deliver_100(CliSock) ->
    H = #outh{status = 100,
              doclose = false,
              chunked = false,
              server = yaws:make_server_header(),
              allow = yaws:make_allow_header()},
    put(outh, H),
    deliver_accumulated(CliSock),
    continue.


deliver_xxx(CliSock, _Req, Code) ->
    deliver_xxx(CliSock, _Req, Code, "").
deliver_xxx(CliSock, _Req, Code, ExtraHtml) ->
    B = list_to_binary(["<html><h1>",
                        integer_to_list(Code), $\ ,
                        yaws_api:code_to_phrase(Code),
                        "</h1></html>",
                       ExtraHtml]),
    H = #outh{status = Code,
              doclose = true,
              chunked = false,
              server = yaws:make_server_header(),
              connection = yaws:make_connection_close_header(true),
              content_length = yaws:make_content_length_header(size(B)),
              contlen = size(B),
              content_type = yaws:make_content_type_header("text/html")},
    put(outh, H),
    accumulate_content(B),
    deliver_accumulated(CliSock),
    done.

deliver_400(CliSock, Req) ->
    deliver_xxx(CliSock, Req, 400).% Bad Request

deliver_403(CliSock, Req) ->
    deliver_xxx(CliSock, Req, 403).        % Forbidden

deliver_405(CliSock, Req, Methods) ->
    Methods_msg = lists:flatten(
                    ["<p>This resource allows ",
                     yaws:join_sep([atom_to_list(M) || M <- Methods], ", "),
                     "</p>"]),
    deliver_xxx(CliSock, Req, 405, Methods_msg).

deliver_416(CliSock, _Req, Tot) ->
    B = list_to_binary(["<html><h1>416 ",
                        yaws_api:code_to_phrase(416),
                        "</h1></html>"]),
    H = #outh{status = 416,
              doclose = true,
              chunked = false,
              server = yaws:make_server_header(),
              connection = yaws:make_connection_close_header(true),
              content_range = ["Content-Range: */",
                               integer_to_list(Tot), $\r, $\n],
              content_length = yaws:make_content_length_header(size(B)),
              contlen = size(B),
              content_type = yaws:make_content_type_header("text/html")},
    put(outh, H),
    accumulate_content(B),
    deliver_accumulated(CliSock),
    done.

deliver_501(CliSock, Req) ->
    deliver_xxx(CliSock, Req, 501). % Not implemented



do_yaws(CliSock, ARG, UT, N) ->
    Key = UT#urltype.getpath, %% always flat
    Mtime = mtime(UT#urltype.finfo),
    SC=get(sc),
    case ets:lookup(SC#sconf.ets, Key) of
        [{_Key, spec, Mtime1, Spec, Es}] when Mtime1 == Mtime,
                                              Es == 0 ->
            deliver_dyn_file(CliSock, Spec, ARG, UT, N);
        Other  ->
            del_old_files(get(gc),Other),
            {ok, [{errors, Errs}| Spec]} =
                yaws_compile:compile_file(UT#urltype.fullpath),
            ?Debug("Spec for file ~s is:~n~p~n",[UT#urltype.fullpath, Spec]),
            ets:insert(SC#sconf.ets, {Key, spec, Mtime, Spec, Errs}),
            deliver_dyn_file(CliSock, Spec, ARG, UT, N)
    end.


del_old_files(_, []) ->
    ok;
del_old_files(_GC, [{_FileAtom, spec, _Mtime1, Spec, _}]) ->
    foreach(
      fun({mod, _, _, _,  Mod, _Func}) ->
              F=filename:join([yaws:tmpdir(), "yaws",
                               yaws:to_list(Mod) ++ ".erl"]),
              code:purge(Mod),
              code:purge(Mod),
              file:delete(F);
         (_) ->
              ok
      end, Spec).


get_client_data(CliSock, Len, SSlBool) ->
    get_client_data(CliSock, Len, [], SSlBool).

get_client_data(_CliSock, 0, Bs, _SSlBool) ->
    list_to_binary(Bs);
get_client_data(CliSock, Len, Bs, SSlBool) ->
    case yaws:cli_recv(CliSock, Len, SSlBool) of
        {ok, B} ->
            get_client_data(CliSock, Len-size(B), [Bs,B], SSlBool);
        _Other ->
            error_logger:format("get_client_data: ~p~n", [_Other]),
            exit(normal)
    end.

%% not nice to support this for ssl sockets
get_chunked_client_data(CliSock,Bs,SSL) ->
    yaws:setopts(CliSock, [binary, {packet, line}],SSL),
    N = yaws:get_chunk_num(CliSock,SSL),
    yaws:setopts(CliSock, [binary, {packet, raw}],SSL),
    if
        N == 0 ->
            _Tmp=yaws:do_recv(CliSock, 2, SSL),%% flush last crnl
            list_to_binary(Bs);
        true ->
            B = yaws:get_chunk(CliSock, N, 0,SSL),
            yaws:eat_crnl(CliSock,SSL),
            get_chunked_client_data(CliSock, [Bs,B], SSL)
    end.

%% Return values:
%% continue, done, {page, Page}

deliver_dyn_part(CliSock,                       % essential params
                 LineNo, YawsFile,              % for diagnostic output
                 CliDataPos,                    % for `get_more'
                 Arg,UT,
                 YawsFun,                       % call YawsFun(Arg)
                 DeliverCont                    % call DeliverCont(Arg)
                                                % to continue normally
                ) ->
    put(yaws_ut, UT),
    put(yaws_arg, Arg),
    Res = (catch YawsFun(Arg)),
    case handle_out_reply(Res, LineNo, YawsFile, UT, Arg) of
        {get_more, Cont, State} when
        element(1, Arg#arg.clidata) == partial  ->
            More = get_more_post_data(CliDataPos, Arg),
            A2 = Arg#arg{clidata = More,
                         cont = Cont,
                         state = State},
            deliver_dyn_part(
              CliSock, LineNo, YawsFile, CliDataPos+size(un_partial(More)),
              A2, UT, YawsFun, DeliverCont
             );
        break ->
            finish_up_dyn_file(Arg, CliSock);
        {page, Page} ->
            {page, Page};
        Arg2 = #arg{} ->
            DeliverCont(Arg2);
        {streamcontent, MimeType, FirstChunk} ->
            yaws:outh_set_content_type(MimeType),
            accumulate_content(FirstChunk),
            Priv = deliver_accumulated(Arg, CliSock,
                                       decide, undefined, stream),
            stream_loop_send(Priv, CliSock, 30000);
        %% For other timeout values (other than 30 second)
        {streamcontent_with_timeout, MimeType, FirstChunk, TimeOut} ->
            yaws:outh_set_content_type(MimeType),
            accumulate_content(FirstChunk),
            Priv = deliver_accumulated(Arg, CliSock,
                                       decide, undefined, stream),
            stream_loop_send(Priv, CliSock, TimeOut);
        {streamcontent_with_size, Sz, MimeType, FirstChunk} ->
            yaws:outh_set_content_type(MimeType),
            accumulate_content(FirstChunk),
            Priv = deliver_accumulated(Arg, CliSock,
                                       decide, Sz, stream),
            stream_loop_send(Priv, CliSock, 30000);
        {streamcontent_from_pid, MimeType, Pid} ->
            yaws:outh_set_content_type(MimeType),
            Priv = deliver_accumulated(Arg, CliSock,
                                       no, undefined, stream),
            wait_for_streamcontent_pid(Priv, CliSock, Pid);
        {websocket, OwnerPid, SocketMode} ->
            %% The handshake passes control over the socket to OwnerPid
            %% and terminates the Yaws worker!
            yaws_websockets:handshake(Arg, OwnerPid, SocketMode);
        _ ->
            DeliverCont(Arg)
    end.

finish_up_dyn_file(Arg, CliSock) ->
    deliver_accumulated(Arg, CliSock, decide, undefined, final),
    done_or_continue().



%% do the header and continue
deliver_dyn_file(CliSock, Specs, ARG, UT, N) ->
    Fd = ut_open(UT),
    Bin = ut_read(Fd),
    deliver_dyn_file(CliSock, Bin, Fd, Specs, ARG, UT, N).



deliver_dyn_file(CliSock, Bin, Fd, [H|T],Arg, UT, N) ->
    ?Debug("deliver_dyn_file: ~p~n", [H]),
    case H of
        {mod, LineNo, YawsFile, NumChars, Mod, out} ->
            {_, Bin2} = skip_data(Bin, Fd, NumChars),
            deliver_dyn_part(CliSock, LineNo, YawsFile,
                             N, Arg, UT,
                             fun(A)->Mod:out(A) end,
                             fun(A)->deliver_dyn_file(
                                       CliSock,Bin2,Fd,T,A,UT,0)
                             end);
        {data, 0} ->
            deliver_dyn_file(CliSock, Bin, Fd,T,Arg,UT,N);
        {data, NumChars} ->
            {Send, Bin2} = skip_data(Bin, Fd, NumChars),
            accumulate_content(Send),
            deliver_dyn_file(CliSock, Bin2, Fd, T,Arg,UT,N);
        {skip, 0} ->
            deliver_dyn_file(CliSock, Bin, Fd,T,Arg,UT,N);
        {skip, NumChars} ->
            {_, Bin2} = skip_data(Bin, Fd, NumChars),
            deliver_dyn_file(CliSock, Bin2, Fd, T,Arg,UT,N);
        {binding, NumChars} ->
            {Send, Bin2} = skip_data(Bin, Fd, NumChars),
            "%%"++Key = binary_to_list(Send),
            Chunk =
                case get({binding, Key--"%%"}) of
                    undefined -> Send;
                    Value -> Value
                end,
            accumulate_content(Chunk),
            deliver_dyn_file(CliSock, Bin2, Fd, T, Arg, UT, N);
        {error, NumChars, Str} ->
            {_, Bin2} = skip_data(Bin, Fd, NumChars),
            accumulate_content(Str),
            deliver_dyn_file(CliSock, Bin2, Fd, T,Arg,UT, N);
        {verbatim, NumChars, Data} ->
            {_Send, Bin2} = skip_data(Bin, Fd, NumChars),
            accumulate_content(Data),
            deliver_dyn_file(CliSock, Bin2, Fd, T,Arg,UT,N);
        yssi ->
            ok
    end;

deliver_dyn_file(CliSock, _Bin, _Fd, [], ARG,_UT,_N) ->
    ?Debug("deliver_dyn: done~n", []),
    finish_up_dyn_file(ARG, CliSock).


stream_loop_send(Priv, CliSock, Timeout) ->
    {ok, FlushTimer} = timer:send_after(300, flush_timer),
    case Timeout of
	infinity ->
	    untimed_stream_loop_send(Priv, CliSock, FlushTimer);
	_ ->
	    {ok, TimeoutTimer} = timer:send_after(Timeout, timeout_timer),
	    stream_loop_send(Priv, CliSock, Timeout,
			     FlushTimer, TimeoutTimer)
    end.

untimed_stream_loop_send(Priv, CliSock, FlushTimer) ->
    receive
        {streamcontent, Cont} ->
            P = send_streamcontent_chunk(Priv, CliSock, Cont),
            untimed_stream_loop_send(P, CliSock, FlushTimer) ;
        {streamcontent_with_ack, From, Cont} ->        % acknowledge after send
            P = send_streamcontent_chunk(Priv, CliSock, Cont),
            From ! {self(), streamcontent_ack},
            untimed_stream_loop_send(P, CliSock, FlushTimer) ;
        endofstreamcontent ->
            cancel_t(FlushTimer, flush_timer),
            end_streaming(Priv, CliSock);
        timeout_timer  ->
            cancel_t(FlushTimer, flush_timer),
            erlang:error(stream_timeout);
        flush_timer  ->
            P = sync_streamcontent(Priv, CliSock),
            untimed_stream_loop_send(P, CliSock, FlushTimer)
    end.

cancel_t(T, Msg) ->
    timer:cancel(T),
    receive
        Msg -> ok
    after 0 -> ok
    end.

stream_loop_send(Priv, CliSock, Timeout,
                 FlushTimer, TimeoutTimer) ->
    receive
        {streamcontent, Cont} ->
            P = send_streamcontent_chunk(Priv, CliSock, Cont),
            cancel_t(TimeoutTimer, timeout_timer),
            {ok, TimeoutTimer2} = timer:send_after(Timeout, timeout_timer),
            stream_loop_send(P, CliSock, Timeout,
                             FlushTimer, TimeoutTimer2) ;
        {streamcontent_with_ack, From, Cont} ->        % acknowledge after send
            P = send_streamcontent_chunk(Priv, CliSock, Cont),
            From ! {self(), streamcontent_ack},
            cancel_t(TimeoutTimer, timeout_timer),
            {ok, TimeoutTimer2} = timer:send_after(Timeout, timeout_timer),
            stream_loop_send(P, CliSock, Timeout,
                             FlushTimer, TimeoutTimer2) ;
        endofstreamcontent ->
            cancel_t(TimeoutTimer, timeout_timer),
            cancel_t(FlushTimer, flush_timer),
            end_streaming(Priv, CliSock);
        timeout_timer  ->
            cancel_t(TimeoutTimer, timeout_timer),
            cancel_t(FlushTimer, flush_timer),
            erlang:error(stream_timeout);
        flush_timer  ->
            P = sync_streamcontent(Priv, CliSock),
            stream_loop_send(P, CliSock, Timeout,
                             FlushTimer, TimeoutTimer)
    end.

make_chunk(Data) ->
    case yaws:outh_get_chunked() of
        true ->
            case binary_size(Data) of
                0 ->
                    empty;
                S ->
                    CRNL = crnl(),
                    {S, [yaws:integer_to_hex(S), CRNL, Data, CRNL]}
            end;
        false ->
            {binary_size(Data), Data}
    end.

make_final_chunk(Data) ->
    case yaws:outh_get_chunked() of
        true ->
            CRNL = crnl(),
            case binary_size(Data) of
                0 ->
                    {0, ["0",CRNL,CRNL]};
                S ->
                    {S, [yaws:integer_to_hex(S), CRNL, Data, CRNL,
                         "0", CRNL, CRNL]}
            end;
        false ->
            {binary_size(Data), Data}
    end.

send_streamcontent_chunk(discard, _, _) ->
    discard;
send_streamcontent_chunk(undeflated, CliSock, Data) ->
    case make_chunk(Data) of
        empty -> ok;
        {Size, Chunk} ->
            ?Debug("send ~p bytes to ~p ~n",
                   [Size, CliSock]),
            yaws:outh_inc_act_contlen(Size),
            yaws:gen_tcp_send(CliSock, Chunk)
    end,
    undeflated;
send_streamcontent_chunk({Z, Priv}, CliSock, Data) ->
    ?Debug("send ~p bytes to ~p ~n",
           [binary_size(Data), CliSock]),
    {ok, P, D} = yaws_zlib:gzipDeflate(Z, Priv, to_binary(Data), none),
    case make_chunk(D) of
        empty -> ok;
        {Size, Chunk} ->
            yaws:outh_inc_act_contlen(Size),
            yaws:gen_tcp_send(CliSock, Chunk)
    end,
    {Z, P}.


sync_streamcontent(discard, _CliSock) ->
    discard;
sync_streamcontent(undeflated, _CliSock) ->
    undeflated;
sync_streamcontent({Z, Priv}, CliSock) ->
    ?Debug("syncing~n", []),
    {ok, P, D} = yaws_zlib:gzipDeflate(Z, Priv, <<>>, sync),
    case make_chunk(D) of
        empty -> ok;
        {Size, Chunk} ->
            yaws:outh_inc_act_contlen(Size),
            yaws:gen_tcp_send(CliSock, Chunk)
    end,
    {Z, P}.


end_streaming(discard, _) ->
    done_or_continue();
end_streaming(undeflated, CliSock) ->
    ?Debug("end_streaming~n", []),
    {_, Chunk} = make_final_chunk(<<>>),
    yaws:gen_tcp_send(CliSock, Chunk),
    done_or_continue();
end_streaming({Z, Priv}, CliSock) ->
    ?Debug("end_streaming~n", []),
    {ok, _P, Data} = yaws_zlib:gzipDeflate(Z, Priv, <<>>, finish),
    {Size, Chunk} = make_final_chunk(Data),
    yaws:outh_inc_act_contlen(Size),
    yaws:gen_tcp_send(CliSock, Chunk),
    yaws_zlib:gzipEnd(Z),
    zlib:close(Z),
    done_or_continue().


%% what about trailers ??
%% vinoski -- I think trailers should be added as an optional argument to
%% yaws_api:stream_chunk_end(). The end_streaming() function above could
%% then easily deal with sending them.

wait_for_streamcontent_pid(Priv, CliSock, ContentPid) ->
    Ref = erlang:monitor(process, ContentPid),
    case Priv of
        discard ->
            ContentPid ! {discard, self()};
        _ ->
	    SC = get(sc),
	    case SC#sconf.ssl of
		undefined ->
		    gen_tcp:controlling_process(CliSock, ContentPid);
		_ ->
		    ssl:controlling_process(CliSock, ContentPid)
	    end,
	    ContentPid ! {ok, self()}
    end,
    receive
        endofstreamcontent ->
            erlang:demonitor(Ref),
            %% should just use demonitor [flush] option instead?
            receive
                {'DOWN', Ref, _, _, _} ->
                    ok
            after 0 ->
                    ok
            end;
        {'DOWN', Ref, _, _, _} ->
            ok
    end,
    done_or_continue().

skip_data(List, Fd, Sz) when is_list(List) ->
    skip_data(list_to_binary(List), Fd, Sz);
skip_data(Bin, Fd, Sz) when is_binary(Bin) ->
    ?Debug("Skip data ~p bytes from", [Sz]),
    case  Bin of
        <<Head:Sz/binary ,Tail/binary>> ->
            {Head, Tail};
        _ ->
            case (catch file:read(Fd, 4000)) of
                {ok, Bin2} when is_binary(Bin2) ->
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



to_binary(B) when is_binary(B) ->
    B;
to_binary(L) when is_list(L) ->
    list_to_binary(L).


%% binary_size(X) -> size(to_binary(X)).

binary_size(X) -> binary_size(0,X).

binary_size(I, []) ->
    I;
binary_size(I, [H|T]) ->
    J = binary_size(I, H),
    binary_size(J, T);
binary_size(I, B) when is_binary(B) ->
    I + size(B);
binary_size(I, _Int) when is_integer(_Int) ->
    I+1.

accumulate_content(Data) ->
    case get(acc_content) of
        undefined ->
            put(acc_content, [Data]);
        discard ->
            discard;
        List ->
            put(acc_content, [List, Data])
    end.


%% handle_out_reply(R, ...)
%%
%% R is a reply or a deep list of replies.  The special return values
%% `streamcontent', `get_more_data' etc, which are not handled here
%% completely but returned, have to be the last element of the list.

handle_out_reply(L, LineNo, YawsFile, UT, ARG) when is_list (L) ->
    handle_out_reply_l(L, LineNo, YawsFile, UT, ARG, undefined);




%% yssi, yaws include
handle_out_reply({yssi, Yfile}, LineNo, YawsFile, UT, ARG) ->
    SC = get(sc),

    %% special case for abs paths
    UT2=case Yfile of
            [$/|_] ->
                url_type( Yfile, ARG#arg.docroot, ARG#arg.docroot_mount);
            _Else ->
                %%why lists:flatten? is urltype.dir ever nested more than
                %% 1 level deep?
                %%!todo - replace with conc_path if 1 level - or specify that
                %% urltype fields should be written flat!
                %% All this deep listing of relatively *short* strings
                %seems unwieldy. just how much performance can it gain if we
                %% end up using slower funcs like lists:flatten anyway?
                %% review!.
                       url_type(lists:flatten(UT#urltype.dir) ++ [$/|Yfile],
                         ARG#arg.docroot, ARG#arg.docroot_mount)
        end,

    case UT2#urltype.type of
        yaws ->
            Mtime = mtime(UT2#urltype.finfo),
            Key = UT2#urltype.getpath,
            CliSock = ARG#arg.clisock,
            N = 0,
            case ets:lookup(SC#sconf.ets, Key) of
                [{_Key, spec, Mtime1, Spec, Es}] when Mtime1 == Mtime,
                                                      Es == 0 ->
                    deliver_dyn_file(CliSock, Spec ++ [yssi], ARG, UT2, N);
                Other  ->
                    del_old_files(get(gc), Other),
                    {ok, [{errors, Errs}| Spec]} =
                        yaws_compile:compile_file(UT2#urltype.fullpath),
                    ?Debug("Spec for file ~s is:~n~p~n",
                           [UT2#urltype.fullpath, Spec]),
                    ets:insert(SC#sconf.ets, {Key, spec, Mtime, Spec, Errs}),
                    deliver_dyn_file(CliSock, Spec ++ [yssi], ARG, UT2, N)
            end;
        error when SC#sconf.xtra_docroots /= [] ->
            SC2 = SC#sconf{docroot = hd(SC#sconf.xtra_docroots),
                           xtra_docroots = tl(SC#sconf.xtra_docroots)},
            put(sc, SC2), ARG2 = ARG#arg{docroot = SC2#sconf.docroot},
            Ret = handle_out_reply({yssi, Yfile}, LineNo, YawsFile, UT, ARG2),
            put(sc, SC),
            Ret;
        _ ->
            error_logger:format("Failed to yssi ~p~n", [Yfile]),
            ok
    end;


handle_out_reply({html, Html}, _LineNo, _YawsFile,  _UT, _ARG) ->
    accumulate_content(Html);

handle_out_reply({ehtml, E}, _LineNo, _YawsFile,  _UT, ARG) ->
    Res = case safe_ehtml_expand(E) of
              {ok, Val} ->
                  accumulate_content(Val);
              {error, ErrStr} ->
                  handle_crash(ARG, ErrStr)
          end,
    Res;

handle_out_reply({content, MimeType, Cont}, _LineNo,_YawsFile, _UT, _ARG) ->
    yaws:outh_set_content_type(MimeType),
    accumulate_content(Cont);

handle_out_reply({streamcontent, MimeType, First},
                 _LineNo,_YawsFile, _UT, _ARG) ->
    yaws:outh_set_content_type(MimeType),
    {streamcontent, MimeType, First};

handle_out_reply({streamcontent_with_timeout, MimeType, First, Timeout},
                 _LineNo,_YawsFile, _UT, _ARG) ->
    yaws:outh_set_content_type(MimeType),
    {streamcontent_with_timeout, MimeType, First, Timeout};

handle_out_reply(Res = {page, _Page},
                 _LineNo,_YawsFile, _UT, _ARG) ->
    Res;

handle_out_reply({streamcontent_with_size, Sz, MimeType, First},
                 _LineNo,_YawsFile, _UT, _ARG) ->
    yaws:outh_set_content_type(MimeType),
    {streamcontent_with_size, Sz, MimeType, First};

handle_out_reply({streamcontent_from_pid, MimeType, Pid},
                 _LineNo,_YawsFile, _UT, _ARG) ->
    yaws:outh_set_content_type(MimeType),
    {streamcontent_from_pid, MimeType, Pid};

handle_out_reply({websocket, _OwnerPid, _SocketMode}=Reply,
                 _LineNo,_YawsFile, _UT, _ARG) ->
    yaws:accumulate_header({connection, erase}),
    Reply;

handle_out_reply({header, H},  _LineNo, _YawsFile, _UT, _ARG) ->
    yaws:accumulate_header(H);

handle_out_reply({allheaders, Hs}, _LineNo, _YawsFile, _UT, _ARG) ->
    yaws:outh_clear_headers(),
    foreach(fun({header, Head}) -> yaws:accumulate_header(Head) end, Hs);

handle_out_reply({status, Code},_LineNo,_YawsFile,_UT,_ARG)
    when is_integer(Code) ->
    yaws:outh_set_status_code(Code);

handle_out_reply({'EXIT', normal}, _LineNo, _YawsFile, _UT, _ARG) ->
    exit(normal);

handle_out_reply({ssi, File, Delimiter, Bindings}, LineNo, YawsFile, UT, ARG) ->
    case ssi(File, Delimiter, Bindings, UT, ARG) of
        {error, Rsn} ->
            L = ?F("yaws code at~s:~p had the following err:~n~p",
                   [YawsFile, LineNo, Rsn]),
            handle_crash(ARG, L);
        OutData ->
            accumulate_content(OutData)
    end;


handle_out_reply(break, _LineNo, _YawsFile, _UT, _ARG) ->
    break;

handle_out_reply({redirect_local, Path}, LN, YF, UT, ARG) ->
    handle_out_reply({redirect_local, Path, 302}, LN, YF, UT, ARG);

%% What about:
%%
%% handle_out_reply({redirect_local, Path, Status}, LineNo,
%%                 YawsFile, SC, ARG) when string(Path) ->
%%   handle_out_reply({redirect_local, {any_path, Path}, Status}, LineNo,
%%                 YawsFile, SC, ARG);
%%
%% It would introduce a slight incompatibility with earlier versions,
%% but might be desirable.

handle_out_reply({redirect_local, {any_path, URL}, Status}, LineNo,
                 YawsFile, _UT, ARG) ->
    PathType =
        case yaws_api:is_absolute_URI(URL) of
            true -> net_path;
            false -> case URL of
                         [$/|_] -> abs_path;
                         _ -> rel_path
                     end
        end,
    handle_out_reply({redirect_local, {PathType, URL}, Status}, LineNo,
                     YawsFile, _UT, ARG);

handle_out_reply({redirect_local, {net_path, URL}, Status}, _LineNo,
                 _YawsFile,  _UT, _ARG) ->
    Loc = ["Location: ", URL, "\r\n"],
    new_redir_h(get(outh), Loc, Status),
    ok;

handle_out_reply({redirect_local, Path0, Status}, _LineNo,_YawsFile,_UT, ARG) ->
    SC=get(sc),
    Path = case Path0 of
               {abs_path, P} ->
                   P;
               {rel_path, P} ->
                   {abs_path, RP} = (ARG#arg.req)#http_request.path,
                   case string:rchr(RP, $/) of
                       0 ->
                           [$/|P];
                       N ->
                           [lists:sublist(RP, N),P]
                   end;
               P ->
                   P
           end,
    Scheme = yaws:redirect_scheme(SC),
    Headers = ARG#arg.headers,
    HostPort = yaws:redirect_host(SC, Headers#headers.host),
    Loc = ["Location: ", Scheme, HostPort, Path, "\r\n"],
    new_redir_h(get(outh), Loc, Status),
    ok;

handle_out_reply({redirect, URL}, LN, YF, UT, ARG) ->
    handle_out_reply({redirect, URL, 302}, LN, YF, UT, ARG);

handle_out_reply({redirect, URL, Status}, _LineNo, _YawsFile, _UT, _ARG) ->
    Loc = ["Location: ", URL, "\r\n"],
    new_redir_h(get(outh), Loc, Status),
    ok;

handle_out_reply({bindings, L}, _LineNo, _YawsFile, _UT, _ARG) ->
    foreach(fun({Key, Value}) -> put({binding, Key}, Value) end, L),
    ok;

handle_out_reply(ok, _LineNo, _YawsFile, _UT, _ARG) ->
    ok;

handle_out_reply({'EXIT', Err}, LineNo, YawsFile, _UT, ARG) ->
    L = ?F("~n~nERROR erlang  code  crashed:~n "
           "File: ~s:~w~n"
           "Reason: ~p~nReq: ~p~n"
           "Stack: ~p~n",
           [YawsFile, LineNo, Err, ARG#arg.req, erlang:get_stacktrace()]),
    handle_crash(ARG, L);

handle_out_reply({get_more, Cont, State}, _LineNo, _YawsFile, _UT, _ARG) ->
    {get_more, Cont, State};

handle_out_reply(Arg = #arg{},  _LineNo, _YawsFile, _UT, _ARG) ->
    Arg;

handle_out_reply(Reply, LineNo, YawsFile, _UT, ARG) ->
    L =  ?F("yaws code at ~s:~p crashed or "
            "ret bad val:~p ~nReq: ~p",
            [YawsFile, LineNo, Reply, ARG#arg.req]),
    handle_crash(ARG, L).



handle_out_reply_l([Reply|T], LineNo, YawsFile, UT, ARG, _Res) ->
    case handle_out_reply(Reply, LineNo, YawsFile, UT, ARG) of
        break ->
            break;
        {page, Page} ->
            {page, Page};
        RetVal ->
            handle_out_reply_l(T, LineNo, YawsFile, UT, ARG, RetVal)
    end;
handle_out_reply_l([], _LineNo, _YawsFile, _UT, _ARG, Res) ->
    Res.




%% fast server side include with macrolike variable bindings expansion
%%


ssi(File, Delimiter, Bindings) ->
    ssi(File, Delimiter, Bindings, get(yaws_ut), get(yaws_arg), get (sc)).


ssi(File, Delimiter, Bindings, UT, ARG) ->
    ssi(File, Delimiter, Bindings, UT, ARG, get(sc)).


ssi(File, Delimiter, Bindings, UT, ARG, SC) ->

    Dir = UT#urltype.dir,
    %%Dir here should be equiv to arg.prepath

    Docroot = ARG#arg.docroot,
    VirtualDir = ARG#arg.docroot_mount,


    %%JMN - line below looks suspicious, why are we not keying on
    %% {ssi, File, Dir} ???
    %%Surely a name like header.inc may be present in various parts of the
    %% hierarchy so Dir should form part of key.
    Key = {ssi, File, Delimiter},

    %%!todo - review rel_path & abs_path - define & document behaviour..
    %% or remove them.

    FullPath =
        case File of
            {rel_path, FileName} ->
                [Docroot, [$/|Dir],[$/|FileName]];
            {abs_path, FileName} ->
                [Docroot, [$/|FileName]];
            [$/|_] ->
                %%absolute path - need to determine Docroot and any Vdir
                %% that might apply
                {Vdir, DR} = vdirpath(SC, ARG, File),

                construct_fullpath(DR, File, Vdir);
            _ ->
                %%relative to the Docroot and VirtualDir that correspond
                %% to the request.

                construct_fullpath(Docroot, lists:flatten([Dir, [$/|File]]),
                                   VirtualDir)
        end,

    Mtime = path_to_mtime(FullPath),
    case ets:lookup(SC#sconf.ets, Key) of
        [{_, Parts, Mtime}] ->
            case (catch expand_parts(Parts, Bindings, [])) of
                {'EXIT', ErrStr} ->
                    {error, ErrStr};
                Value ->
                    Value
            end;
        _ ->
            case prim_file:read_file(FullPath) of
                {ok, Bin} ->
                    D =delim_split_file(Delimiter,binary_to_list(Bin),data,[]),
                    ets:insert(SC#sconf.ets,{Key,D, Mtime}),
                    ssi(File, Delimiter, Bindings, UT, ARG, SC);
                {error, _} when SC#sconf.xtra_docroots /= [] ->
                    SC2 = SC#sconf{docroot = hd(SC#sconf.xtra_docroots),
                                   xtra_docroots = tl(SC#sconf.xtra_docroots)},

                    ARG2 = ARG#arg{
                             docroot = hd(SC#sconf.xtra_docroots),
                             docroot_mount = "/"
                            },

                    ssi(File, Delimiter, Bindings, UT, ARG2, SC2);
                {error, Rsn} ->
                    error_logger:format("Failed to read/ssi file ~p~n",
                                        [FullPath]),
                    {error,Rsn}
            end
    end.


path_to_mtime(FullPath) ->
    case prim_file:read_file_info(FullPath) of
        {ok, FI} ->
            mtime(FI);
        Err ->
            Err
    end.


expand_parts([{data, D} |T], Bs, Ack) ->
    expand_parts(T, Bs, [D|Ack]);
expand_parts([{var, V} |T] , Bs, Ack) ->
    case lists:keysearch(V, 1, Bs) of
        {value, {_, {ehtml, E}}} ->
            case safe_ehtml_expand(E) of
                {ok, Val} ->
                    expand_parts(T, Bs, [Val|Ack]);
                {error, ErrStr} ->
                    erlang:error(ErrStr)
            end;
        {value, {_, Val}} ->
            expand_parts(T, Bs, [Val|Ack]);
        false  ->
            case get({binding,V}) of
                undefined -> expand_parts(T, Bs, Ack);
                Valb -> expand_parts(T, Bs, [Valb|Ack])
            end
    end;
expand_parts([], _,Ack) ->
    lists:reverse(Ack).



delim_split_file([], Data, _, _Ack) ->
    [{data, Data}];
delim_split_file(Del, Data, State, Ack) ->
    case delim_split(Del, Del, Data, [], []) of
        {H, []} when State == data ->
            %% Ok, last chunk
            lists:reverse([{data, H} | Ack]);
        {H, T} when State == data ->
            delim_split_file(Del, T, var, [{data, H}|Ack]);
        {H, []} when State == var ->
            lists:reverse([{var, H} | Ack]);
        {H, T} when State == var ->
            delim_split_file(Del, T, data, [{var, H}|Ack])
    end.


delim_split([H|T], Odel, [H|T1], Ack, DAcc) ->
    delim_split(T,Odel,T1,Ack, [H|DAcc]);
delim_split([], _Odel, T, Ack, _DAcc) ->
    {lists:reverse(Ack),T};
delim_split([H|_T],Odel, [H1|T1], Ack, []) when H /= H1 ->
    delim_split(Odel, Odel, T1, [H1|Ack], []);
delim_split([H|_T],Odel, [H1|T1], Ack, DAcc) when H /= H1 ->
    delim_split(Odel, Odel, T1, [H1|DAcc++Ack], []);
delim_split(_,_,[],Ack,[]) ->
    {lists:reverse(Ack),[]};
delim_split(_,_,[],Ack,DAcc) ->
    {lists:reverse(DAcc++Ack),[]}.



%% Erlang yaws code crashed, display either the
%% actual crash or a customized error message

handle_crash(ARG, L) ->
    ?Debug("handle_crash(~p)~n", [L]),
    SC=get(sc),
    yaws:elog("~s", [L]),
    yaws:outh_set_status_code(500),
    case catch apply(SC#sconf.errormod_crash, crashmsg, [ARG, SC, L]) of
        {content,MimeType,Cont} ->
	   yaws:outh_set_content_type(MimeType),
	   accumulate_content(Cont);
        {html, Str} ->
            accumulate_content(Str),
            break;
        {ehtml, Term} ->
            case safe_ehtml_expand(Term) of
                {error, Reason} ->
                    yaws:elog("~s", [Reason]),
                    %% Aghhh, yet another user crash :-(
                    T2 = [{h2, [], "Internal error"}, {hr},
                          {p, [], "Customized crash display code crashed !!!"}],
                    accumulate_content(ehtml_expand(T2)),
                    break;
                {ok, Out} ->
                    accumulate_content(Out),
                    break
            end;
        Other ->
            yaws:elog("Bad return value from errmod_crash ~n~p~n",[Other]),
            T2 = [{h2, [], "Internal error"}, {hr},
                  {p, [], "Customized crash display code returned bad val"}],
            accumulate_content(ehtml_expand(T2)),
            break

    end.

%% Ret: true | false | {data, Data}
decide_deflate(false, _, _, _, _) ->
    false;
decide_deflate(_, _, _, no, _) ->
    false;
decide_deflate(true, _, _, deflate, stream) ->
    true;
decide_deflate(true, Arg, Data, decide, Mode) ->
    case yaws:outh_get_content_encoding_header() of
        undefined ->
            ?Debug("No Content-Encoding defined~n",[]),
            Mime = yaws:outh_get_content_type(),
            ?Debug("Mime-Type: ~p~n", [Mime]),
            case compressible_mime_type(Mime) of
                true ->
                    case yaws:accepts_gzip(Arg#arg.headers,
                                           Mime) of
                        true ->
                            case Mode of
                                final ->
                                    case yaws_zlib:gzip(to_binary(Data)) of
                                        {ok, DB} ->
                                            {data, DB};
                                        _Err ->
                                            ?Debug(
                                               "gzip Err: ~p~n", [_Err]),
                                            false
                                    end;
                                stream ->
                                    true
                            end;
                        false -> false
                    end;
                false ->
                    false
            end;
        _CE -> ?Debug("Content-Encoding: ~p~n",[_CE]),
               false
    end.


deliver_accumulated(Sock) ->
    deliver_accumulated(undefined, Sock, no, undefined, final).

%% Mode = final | stream
%% Encoding = deflate    (gzip content)
%%          | no         (keep as is)
%%          | decide     (do as you like)
%% ContentLength = Int | undefined
%% Mode = final | stream
%%
%% For Mode==final: (all content has been accumulated before calling
%%                   deliver_accumulated)
%%     Ret: can be ignored
%%
%% For Mode==stream:
%%     Ret: opaque value to be threaded through
%%                   send_streamcontent_chunk / end_streaming
deliver_accumulated(Arg, Sock, Encoding, ContentLength, Mode) ->
    Cont = case erase(acc_content) of
               undefined ->
                   [];
               Cont2 ->
                   Cont2
           end,
    case Cont of
        discard ->
            yaws:outh_set_transfer_encoding_off(),
            {StatusLine, Headers} = yaws:outh_serialize(),
            ?Debug("discard accumulated~n", []),
            All = [StatusLine, Headers, crnl()],
            Ret = discard;
        _ ->
            SC = get(sc),
            case decide_deflate(?sc_has_deflate(SC),
                                Arg, Cont, Encoding, Mode) of
                {data, Data} -> % implies Mode==final
                    yaws:outh_set_content_encoding(deflate),
                    Size = binary_size(Data),
                    Ret = ok;
                true -> % implies Mode==stream
                    yaws:outh_set_content_encoding(deflate),
                    Z = zlib:open(),
                    {ok, Priv, Data} =
                        yaws_zlib:gzipDeflate(Z, yaws_zlib:gzipInit(Z),
                                              to_binary(Cont), none),
                    Ret = {Z, Priv},
                    Size = undefined;
                false ->
                    Ret = undeflated,
                    Data = Cont,
                    Size = case Mode of
                               final ->
                                   binary_size(Data);
                               stream ->
                                   ContentLength
                           end
            end,
            case Size of
                undefined ->
                    yaws:outh_fix_doclose();
                _ -> yaws:accumulate_header({content_length, Size})
            end,
            {StatusLine, Headers} = yaws:outh_serialize(),
            case make_chunk(Data) of
                empty ->
                    Chunk = [];
                {S, Chunk} ->
                    case Mode of
                        stream -> yaws:outh_inc_act_contlen(S);
                        _ -> ok
                    end
            end,
            All = [StatusLine, Headers, crnl(), Chunk]
    end,
    yaws:gen_tcp_send(Sock, All),
    GC=get(gc),
    if
        GC#gconf.trace == false ->
            ok;
        GC#gconf.trace == {true, http} ->
            yaws_log:trace_traffic(from_server, ["\n",StatusLine, Headers]);
        GC#gconf.trace == {true, traffic} ->
            yaws_log:trace_traffic(from_server, ["\n",All])
    end,
    Ret.


get_more_post_data(PPS, ARG) ->
    SC = get(sc),
    N = SC#sconf.partial_post_size,
    Len = list_to_integer((ARG#arg.headers)#headers.content_length),
    if N + PPS < Len ->
            Bin = get_client_data(ARG#arg.clisock, N,
                                  yaws:is_ssl(SC)),
            {partial, Bin};
       true ->
            get_client_data(ARG#arg.clisock, Len - PPS,
                            yaws:is_ssl(SC))
    end.


ut_open(UT) ->
    ?Debug("ut_open() UT.fullpath = ~p~n", [UT#urltype.fullpath]),
    case yaws:outh_get_content_encoding() of
        identity ->
            case UT#urltype.data of
                undefined ->

                    ?Debug("ut_open reading\n",[]),
                    {ok, Bin} = file:read_file(UT#urltype.fullpath),
                    ?Debug("ut_open read ~p\n",[size(Bin)]),
                    {bin, Bin};
                B when is_binary(B) ->
                    {bin, B}
            end;
        deflate ->
            case UT#urltype.deflate of
                B when is_binary(B) ->
                    ?Debug("ut_open using deflated binary of size ~p~n",
                           [size(B)]),
                    {bin, B}
            end
    end.


ut_read({bin, B}) ->
    B;
ut_read(Fd) ->
    file:read(Fd, 4000).


ut_close({bin, _}) ->
    ok;
ut_close(Fd) ->
    file:close(Fd).

parse_range(L, Tot) ->
    case catch parse_range_throw(L, Tot) of
        {'EXIT', _} ->
                                                % error
            error;
        R -> R
    end.

parse_range_throw(L, Tot) ->
    case lists:splitwith(fun(C)->C /= $- end, L) of
        {FromS, [$-|ToS]} ->
            case FromS of
                [] -> case list_to_integer(ToS) of
                          I when Tot >= I, I>0 ->
                              {fromto, Tot-I, Tot-1, Tot}
                      end;
                _ -> case list_to_integer(FromS) of
                         From when From>=0, From < Tot ->
                             case ToS of
                                 [] -> {fromto, From, Tot-1, Tot};
                                 _ -> case list_to_integer(ToS) of
                                          To when To<Tot ->
                                              {fromto, From, To, Tot};
                                          _ ->
                                              {fromto, From, Tot-1, Tot}
                                      end
                             end
                     end
            end
    end.


%% This is not exactly what the RFC describes, but we do not want to
%% deal with multipart/byteranges.
unite_ranges(all, _) ->
    all;
unite_ranges(error, R) ->
    R;
unite_ranges(_, all) ->
    all;
unite_ranges(R, error) ->
    R;
unite_ranges({fromto, F0, T0, Tot},{fromto,F1,T1, Tot}) ->
    {fromto,
     if F0 >= F1 -> F1;
        true -> F0
     end,
     if T0 >= T1 -> T0;
        true -> T1
     end,
     Tot
    }.


%% ret:  all | error | {fromto, From, To, Tot}
requested_range(RangeHeader, TotalSize) ->
    case yaws:split_sep(RangeHeader, $,) of
        ["bytes="++H|T] ->
            lists:foldl(fun(L, R)->
                                unite_ranges(parse_range(L, TotalSize), R)
                        end, parse_range(H, TotalSize), T);
        _ -> all
    end.


deliver_file(CliSock, Req, UT, Range) ->
    if
        is_binary(UT#urltype.data) ->
            %% cached
            deliver_small_file(CliSock, Req, UT, Range);
        true ->
            deliver_large_file(CliSock, Req, UT, Range)
    end.

deliver_small_file(CliSock, _Req, UT, Range) ->
    Fd = ut_open(UT),
    case Range of
        all ->
            Bin = ut_read(Fd);
        {fromto, From, To, _Tot} ->
            Length = To - From + 1,
            <<_:From/binary, Bin:Length/binary, _/binary>> = ut_read(Fd)
    end,
    accumulate_content(Bin),
    ut_close(Fd),
    deliver_accumulated(CliSock),
    done_or_continue().

deliver_large_file(CliSock,  _Req, UT, Range) ->
    Enc = case Range of
              all ->
                  case yaws:outh_get_content_encoding() of
                      identity -> no;
                      D -> D
                  end;
              _ -> no
          end,
    case deliver_accumulated(undefined, CliSock, Enc, undefined, stream) of
        discard ->
            ok;
        Priv ->
            send_file(CliSock, UT#urltype.fullpath, Range, Priv, Enc)
    end,
    done_or_continue().


send_file(CliSock, Path, all, _Priv, no) when is_port(CliSock) ->
    ?Debug("send_file(~p,~p,no ...)~n", [CliSock, Path]),
    yaws_sendfile_compat:send(CliSock, Path),
    {ok, Size} = yaws:filesize(Path),
    yaws_stats:sent(Size);
send_file(CliSock, Path, all, Priv, _Enc) ->
    ?Debug("send_file(~p,~p, ...)~n", [CliSock, Path]),
    {ok, Fd} = file:open(Path, [raw, binary, read]),
    send_file(CliSock, Fd, Priv);
send_file(CliSock, Path,  {fromto, From, To, _Tot}, undeflated, no) when is_port(CliSock) ->
    Size = To - From + 1,
    yaws_sendfile_compat:send(CliSock, Path, From, Size),
    yaws_stats:sent(Size);
send_file(CliSock, Path,  {fromto, From, To, _Tot}, undeflated, _Enc) ->
    {ok, Fd} = file:open(Path, [raw, binary, read]),
    file:position(Fd, {bof, From}),
    send_file_range(CliSock, Fd, To - From + 1).

send_file(CliSock, Fd, Priv) ->
    ?Debug("send_file(~p,~p, ...)~n", [CliSock, Fd]),
    case file:read(Fd, (get(gc))#gconf.large_file_chunk_size) of
        {ok, Bin} ->
            Priv1 = send_streamcontent_chunk(Priv, CliSock, Bin),
            send_file(CliSock, Fd, Priv1);
        eof ->
            file:close(Fd),
            end_streaming(Priv, CliSock)
    end.

send_file_range(CliSock, Fd, Len) when Len > 0 ->
    {ok, Bin} = file:read(Fd,
                          case (get(gc))#gconf.large_file_chunk_size of
                              S when S < Len -> S;
                              _ -> Len
                          end
                         ),
    send_streamcontent_chunk(undeflated, CliSock, Bin),
    send_file_range(CliSock, Fd, Len - size(Bin));
send_file_range(CliSock, Fd, 0) ->
    file:close(Fd),
    end_streaming(undeflated, CliSock).

crnl() ->
    "\r\n".

now_secs() ->
    {M,S,_}=now(),
    (M*1000000)+S.


%% a file cache,
url_type(GetPath, ArgDocroot, VirtualDir) ->
    SC=get(sc),
    GC=get(gc),
    E = SC#sconf.ets,
    case ets:lookup(E, {url, GetPath}) of
        [] ->
            UT = do_url_type(SC, GetPath, ArgDocroot, VirtualDir),
            ?TC([{record, UT, urltype}]),
            ?Debug("UT=~s\n", [?format_record(UT, urltype)]),
            CF = cache_file(SC, GC, GetPath, UT),
            ?Debug("CF=~s\n", [?format_record(CF, urltype)]),
            CF;
        [{_, When, UT}] ->
            N = now_secs(),
            Refresh = GC#gconf.cache_refresh_secs,
            if
                ((N-When) >= Refresh) ->
                    ?Debug("Timed out entry for ~s ~p~n",
                           [GetPath, {When, N}]),
                    %% more than 30 secs old entry
                    UT2 = do_url_type(SC, GetPath, ArgDocroot, VirtualDir),
                    case file_changed(UT, UT2) of
                        true ->
                            ?Debug("Recaching~n", []),
                            ets:delete(E, {url, GetPath}),
                            ets:delete(E, {urlc, GetPath}),
                            ets:update_counter(E, num_files, -1),
                            ets:update_counter(E, num_bytes, -cache_size(UT)),
                            cache_file(SC, GC, GetPath, UT2);
                        false ->
                            ?Debug("Using unchanged cached version~n", []),
                            (catch ets:update_counter(E, {urlc, GetPath}, 1)),
                            UT
                    end;
                true ->
                    ?Debug("Serve page from cache ~p", [{When , N, N-When}]),
                    (catch ets:update_counter(E, {urlc, GetPath}, 1)),
                    UT
            end
    end.


file_changed(UT1, UT2) ->
    case {UT1#urltype.type, UT2#urltype.type} of
        {T, T} when T==yaws; T==regular->
            F1 = UT1#urltype.finfo,
            F2 = UT2#urltype.finfo,
            {F1#file_info.inode, F1#file_info.mtime}
                /= {F2#file_info.inode, F2#file_info.mtime};
        _ ->
            true % don't care too much
    end.



cache_size(UT) when is_binary(UT#urltype.deflate),
is_binary(UT#urltype.data) ->
    size(UT#urltype.deflate) + size(UT#urltype.data);
cache_size(UT) when is_binary(UT#urltype.data) ->
    size(UT#urltype.data);
cache_size(_UT) ->
    0.




cache_file(_SC, GC, _Path, UT)
  when GC#gconf.max_num_cached_files == 0;
       GC#gconf.max_num_cached_bytes == 0;
       GC#gconf.max_size_cached_file == 0 ->
    UT;
cache_file(SC, GC, Path, UT)
  when ((UT#urltype.type == regular) or
        ((UT#urltype.type == yaws) and (UT#urltype.pathinfo == undefined))) ->
    E = SC#sconf.ets,
    [{num_files, N}] = ets:lookup(E, num_files),
    [{num_bytes, B}] = ets:lookup(E, num_bytes),
    FI = UT#urltype.finfo,
    ?Debug("FI=~s\n", [?format_record(FI, file_info)]),
    if
        N + 1 > GC#gconf.max_num_cached_files ->
            error_logger:info_msg("Max NUM cached files reached for server "
                                  "~p", [SC#sconf.servername]),
            cleanup_cache(E, num),
            cache_file(SC, GC, Path, UT);
        FI#file_info.size < GC#gconf.max_size_cached_file,
        FI#file_info.size < GC#gconf.max_num_cached_bytes,
        B + FI#file_info.size > GC#gconf.max_num_cached_bytes ->
            error_logger:info_msg("Max size cached bytes reached for server "
                                  "~p", [SC#sconf.servername]),
            cleanup_cache(E, size),
            cache_file(SC, GC, Path, UT);
        true ->
            ?Debug("Check file size\n",[]),
            if
                FI#file_info.size > GC#gconf.max_size_cached_file;
                FI#file_info.size > GC#gconf.max_num_cached_bytes ->
                    ?Debug("Too large\n",[]),
                    UT;
                true ->
                    ?Debug("File fits\n",[]),
                    {ok, Bin} = prim_file:read_file(
                                  UT#urltype.fullpath),
                    Deflated =
                        case ?sc_has_deflate(SC)
                            and (UT#urltype.type==regular) of
                            true ->
                                {ok, DBL} = yaws_zlib:gzip(Bin),
                                DB = list_to_binary(DBL),
                                if
                                    size(DB)*10<size(Bin)*9 ->
                                        ?Debug("storing deflated version "
                                               "of ~p~n",
                                               [UT#urltype.fullpath]),
                                        DB;
                                    true -> undefined
                                end;
                            false -> undefined
                        end,
                    UT2 = UT#urltype{data = Bin,
                                     deflate = Deflated},
                    ets:insert(E, {{url, Path}, now_secs(), UT2}),
                    ets:insert(E, {{urlc, Path}, 1}),
                    ets:update_counter(E, num_files, 1),
                    ets:update_counter(E, num_bytes,
                                       cache_size(UT2)),
                    UT2
            end
    end;
cache_file(_SC, _GC, _Path, UT) ->
    UT.



%% FIXME, should not wipe entire ets table this way
cleanup_cache(E, size) ->
    %% remove the largest files with the least hit count  (urlc)
    ?Debug("Clearing yaws internal content "
           "cache, size overflow",[]),
    clear_ets(E);

cleanup_cache(E, num) ->
    ?Debug("Clearing yaws internal content "
           "cache, num overflow",[]),
    clear_ets(E).



%% Clear everything, but *not* the Yaws specs, because otherwise we
%% would have orphan modules loaded.
clear_ets(E) ->
    ets:match_delete(E, {{url, '_'}, '_', '_'}),
    ets:match_delete(E, {{urlc, '_'}, '_', '_'}),
    ets:insert(E, {num_files, 0}),
    ets:insert(E, {num_bytes, 0}).


%% return #urltype record
do_url_type(SC, GetPath, ArgDocroot, VirtualDir) ->
    ?Debug("do_url_type SC=~s~nGetPath=~p~nVirtualDir=~p~n",
           [?format_record(SC,sconf), GetPath,VirtualDir]),


    case GetPath of
        _ when ?sc_has_dav(SC) ->
            {Comps, RevFile} = comp_split(GetPath),
            {_Type, _, Mime} = suffix_type(RevFile),

            FullPath = construct_fullpath(ArgDocroot, GetPath, VirtualDir),

            %%!!WARNING!!!
            %%!TODO - review & test!
            %%Implications of vdirs on DAV have not yet been fully
            %% considered by author of vdir support (JMN)

            #urltype{type = dav,
                     dir = conc_path(Comps),
                     getpath = GetPath,
                     path = GetPath,
                     fullpath = FullPath,
                     mime = Mime};
        "/" -> %% special case
            case lists:keysearch("/", 1, SC#sconf.appmods) of
                {value, AppmodDef} ->
                    %% AppmodDef can be either a 2-tuple or 3-tuple depending
                    %% on whether there are exclude paths present. We want
                    %% only the second element of the tuple in either case.
                    Mod = element(2, AppmodDef),
                    #urltype{type = appmod,
                             data = {Mod, []},
                             dir = "",
                             path = "",
                             fullpath = ArgDocroot};
                _ ->
                    maybe_return_dir(ArgDocroot, GetPath, VirtualDir)
            end;
        [$/, $~ |Tail] ->
            ret_user_dir(Tail);
        _ ->
            FullPath = construct_fullpath(ArgDocroot, GetPath, VirtualDir),

            {Comps, RevFile} = comp_split(GetPath),
            ?Debug("Comps = ~p RevFile = ~p~n",[Comps, RevFile]),

            RequestSegs = string:tokens(GetPath,"/"),
            case active_appmod(SC#sconf.appmods, RequestSegs) of
                false ->
                    ?Debug("FullPath = ~p~n", [FullPath]),

                    case prim_file:read_file_info(FullPath) of
                        {ok, FI} when FI#file_info.type == regular ->
                            {Type, Mime} = suffix_type(SC, RevFile),
                            #urltype{type=Type,
                                     finfo=FI,
                                     deflate=deflate_q(?sc_has_deflate(SC),
                                                       Type, Mime),
                                     dir = conc_path(Comps),
                                     path = GetPath,
                                     getpath = GetPath,
                                     fullpath = FullPath,
                                     mime=Mime};
                        {ok, FI} when FI#file_info.type == directory ->
                            case RevFile of
                                [] ->
                                    maybe_return_dir(ArgDocroot, GetPath,
                                                     VirtualDir);
                                _ ->
                                    %%Presence of RevFile indicates dir url
                                    %% had no trailing /
                                    #urltype{
                                  type = redir,
                                  path = [GetPath, "/"]}
                            end;
                        _Err ->
                            %% non-optimal, on purpose
                            maybe_return_path_info(SC, Comps, RevFile,
                                                   ArgDocroot, VirtualDir)
                    end;
                {ok, {Mount, Mod}} ->
                    %%active_appmod found the most specific appmod for this
                    %% request path
                    %% - now we need to determine the prepath & path_info

                    MountSegs = string:tokens(Mount,"/"),

                    case Mount of
                        [$/] ->
                            %%'root' appmod
                            PostSegments = lists:sublist(RequestSegs,1,
                                                         length(RequestSegs)),
                            Prepath = "";
                        [$/|_] ->
                            %%'anchored' appmod mount.
                            PreSegments = lists:sublist(RequestSegs,
                                                        length(MountSegs)-1),
                            PostSegments = lists:sublist(RequestSegs,
                                                         length(MountSegs)+1,
                                                         length(RequestSegs)),
                            Prepath = case PreSegments of
                                          "" ->
                                              "/";
                                          _ ->
                                              "/" ++
                                                  yaws:join_sep(PreSegments,"/")
                                                  ++ "/"
                                      end;
                        _ ->
                            %%'floating' appmod mount.
                            {PreSegments,PostSegments} =
                                split_at_segment(Mount,RequestSegs,[]),
                            Prepath = case PreSegments of
                                          "" ->
                                              "/";
                                          _ ->
                                              "/" ++
                                                  yaws:join_sep(PreSegments,"/")
                                                  ++ "/"
                                      end
                    end,
                    PathI = case PostSegments of
                                [] ->
                                    "";
                                _ ->
                                    "/" ++ yaws:join_sep(PostSegments,"/")
                            end,
                    %%absence of RevFile tells us there was a trailing slash.
                    PathInf = case RevFile of
                                  [] ->
                                      PathI ++ "/";
                                  _ ->
                                      PathI
                              end,
                    PathInfo = case PathInf of
                                   "" ->
                                       undefined;
                                   _ ->
                                       PathInf
                               end,
                    Path = case MountSegs of
                               [] ->
                                   %%'root' appmod
                                   Prepath;
                               _ ->
                                   Prepath ++ tl(MountSegs)
                           end,

                    #urltype{
                             type = appmod,
                             data = {Mod, PathInfo},
                             dir = Prepath,
                             path = Path,
                             fullpath = FullPath,
                             pathinfo = PathInfo
                            }

            end
    end.


%% comp_split/1 - split a path around "/" returning final segment as
%% reversed string.
%% return {Comps, RevPart} where Comps is a (possibly empty) list of path
%% components - always with trailing "/"
%% revPart is the final segment in reverse and has no "/".
%% e.g split( "/test/etc/index.html",[],[]) -> {["/test/", "etc/"], "lmth.xedni"}
%% revPart is useful in this form for looking up the file extension's mime-type.
%%
%% Terminology note to devs: reserve the word 'comp' to refer to a single
%% fragment of a path that we know has
%% a trailing slash. If you're dealing just with the part between slashes -
%% consider using the term 'segment' instead.
%% e.g  "x/" "/"   are all valid 'comps'
%% "/x" "/x/y/" "x" are not.
%%
comp_split(Path) ->
    do_comp_split(Path,[],[]).

%%when Part /= []
do_comp_split([$/|Tail], Comps, Part) ->
    NewComp = lists:reverse([$/|Part]),
    do_comp_split(Tail,  [NewComp | Comps], []);
do_comp_split([H|T], Comps, Part)  ->
    do_comp_split(T, Comps, [H|Part]);
do_comp_split([], Comps, Part) ->
    {lists:reverse(Comps), Part}.


%%active_appmod/2
%%find longest appmod match for request. (ie 'most specific' appmod)
%% - conceptually similar to the vdirpath scanning - but must also support
%% 'floating appmods' i.e an appmod specified as <path , appmodname> where
%% 'path' has no leading slash.
%%
%% a 'floating' appmod is not tied to a specific point in the URI structure
%% e.g for the configuration entry <myapp , myappAppmod>
%% the requests /docs/stuff/myapp/etc  & /otherpath/myapp   will both
%% trigger the myappAppmod module.
%% whereas for the configuration entry </docs/stuff/myapp , myappAppmod>
%% the request /otherpath/myapp will not trigger the appmod.
%%

active_appmod([], _RequestSegs) ->
    false;
active_appmod(AppMods, RequestSegs) ->

    %%!todo - review/test performance (e.g 'fun' calls are slower than a
    %% call to a local func - replace?)

    %%Accumulator is of form {RequestSegs, {AppmodMountPoint,Mod}}
    Matched =
        lists:foldl(
          fun(Pair,Acc) ->
                  {Mount, Mod, Excludes} = case Pair of
                                               {X, Y} -> {X, Y, []};
                                               {X,Y,Z} -> {X,Y,Z}
                                           end,
                  {ReqSegs, {LongestSoFar, _}} = Acc,

                  MountSegs = string:tokens(Mount,"/"),
                  case {is_excluded(ReqSegs, Excludes) ,
                        lists:prefix(MountSegs,ReqSegs)} of
                      {true, _} ->
                          Acc;
                      {false, true} ->
                          case LongestSoFar of
                              [$/|_] ->
                                  %%simple comparison of string length
                                  %% (as opposed to number of segments)
                                  %% should be ok here.
                                  if length(Mount) >
                                     length(LongestSoFar) ->
                                          {ReqSegs, {Mount, Mod}};
                                     true ->
                                          Acc
                                  end;
                              _ ->
                                  %%existing match is 'floating' -
                                  %% we trump it.

                                  {ReqSegs, {Mount, Mod}}
                          end;
                      {false, false} ->
                          case LongestSoFar of
                              [$/|_] ->
                                  %%There is already a match for an
                                  %% 'anchored' (ie absolute path)
                                  %% mount point.
                                  %% floating appmod can't override.
                                  Acc;
                              _ ->
                                  %%check for 'floating' match
                                  case lists:member(Mount, ReqSegs) of
                                      true ->
                                          %%!todo - review & document.
                                          %%latest 'floating' match wins
                                          %% if multiple match?
                                          %% (order in config vs position
                                          %% in request URI ?)

                                          {ReqSegs, {Mount, Mod}};
                                      false ->
                                          Acc
                                  end
                          end
                  end
          end, {RequestSegs, {"",""}}, AppMods),

    case Matched of
        {_RequestSegs, {"",""}} ->
            %%no appmod corresponding specifically to this http_request.path
            false;
        {_RequestSegs, {Mount, Mod}} ->
            {ok, {Mount, Mod}}
    end
        .

is_excluded(_, []) ->
    false;
is_excluded(RequestSegs, [ExcludeSegs|T]) ->
    case lists:prefix(ExcludeSegs, RequestSegs) of
        true ->
            true;
        false ->
            is_excluded(RequestSegs, T)
    end.


%%split a list of segments into 2 lists either side of element matching Seg.
%%(no elements contain slashes)
split_at_segment(_, [], _Acc) ->
    false;
split_at_segment(Seg,[Seg|Tail],Acc) ->
    {lists:reverse(Acc),Tail};
split_at_segment(Seg,[H|Tail],Acc) ->
    split_at_segment(Seg, Tail, [H|Acc]).




%% construct_fullpath
%%
%%preconditions:
%% - DR, GetPath, VirtualDir already validated &/or normalized
%% - VirtualDir is empty string, or a prefix of GetPath of the form "/path/"
%% where path may also contain "/"
%% - DocRoot is a valid physical path to a directory, with no trailing "/"
%%
%%i.e this is an inner function, so no sanity checks here.
%%
construct_fullpath(DocRoot,GetPath,VirtualDir) ->
    case VirtualDir of
        [] ->
            DocRoot ++ GetPath;
        _ ->
            %%trim the virtual base off the GET request path before appending
            %% to DocRoot.
            %%(leaving one "/" - therefore don't add 1 to length)
            DocRoot ++ string:substr(GetPath,length(VirtualDir))
    end
        .

%%preconditions:
%% - see 'construct_fullpath'
%%
try_index_file(DR, GetPath, VirtualDir) ->
    FullPath = construct_fullpath(DR, GetPath, VirtualDir),

    case prim_file:read_file_info([FullPath, "index.yaws"]) of
        {ok, FI} when FI#file_info.type == regular ->
            do_url_type(get(sc), GetPath ++ "index.yaws", DR, VirtualDir);
        _ ->
            case prim_file:read_file_info([FullPath, "index.html"]) of
                {ok, FI} when FI#file_info.type == regular ->
                    do_url_type(get(sc), GetPath ++ "index.html", DR,
                                VirtualDir);
                _ ->
                    case prim_file:read_file_info([FullPath, "index.php"]) of
                        {ok, FI} when FI#file_info.type == regular ->
                            do_url_type(get(sc), GetPath ++ "index.php", DR,
                                        VirtualDir);
                        _ ->
                            noindex
                    end
            end
    end.


maybe_return_dir(DR, GetPath,VirtualDir) ->
    case try_index_file(DR, GetPath,VirtualDir) of
        noindex ->
            FullPath = construct_fullpath(DR, GetPath, VirtualDir),

            case file:list_dir(FullPath) of
                {ok, List} ->
                    #urltype{type = directory,
                             fullpath = FullPath,
                             dir = GetPath,
                             data = List -- [".yaws_auth"]};
                _Err ->
                    #urltype{type=error}
            end;
        UT ->
            UT
    end.



maybe_return_path_info(SC, Comps, RevFile, DR, VirtualDir) ->

    case path_info_split(Comps, {DR, VirtualDir}) of
        {not_a_script, error} ->
            %%can we use urltype.data to return more info?
            %% - logging?
            #urltype{type=error};
        {ok, FI, FullPath, HeadComps, File, TrailComps, Type, Mime} ->
            %%'File' is the only comp that has been returned without trailing "/"

            {Type2, Mime2} =
                case member(Type, SC#sconf.allowed_scripts) of
                    true ->
                        {Type, Mime};
                    false ->
                        %%!todo review.
                        %%Should we really be returning the file as text/plain
                        %% when there is pathinfo present?
                        %%Perhaps a 403 error would be more appropriate.
                        {regular, "text/plain"}
                end,

            ?Debug("'script-selection' FullPath= ~p~n Mime=~p~n",
                   [FullPath, Mime2]),

            Trail = conc_path([ "/" ] ++ TrailComps ++
                              [ lists:reverse(RevFile) ]),


            #urltype{type = Type2,
                     finfo=FI,
                     deflate=deflate_q(?sc_has_deflate(SC),
                                       Type, Mime),
                     dir =  conc_path(HeadComps),
                     path = conc_path(HeadComps ++ [File]),
                     fullpath = FullPath,
                     pathinfo = Trail,
                     getpath = case HeadComps of
                                   [] -> [$/|File];
                                   [_|_] ->
                                       conc_path(HeadComps ++ [File])
                               end,
                     mime = Mime2}
    end.


%%scan a list of 'comps' of form "pathsegment/"   (trailing slash always present)
%% - looking for the rightmost dotted component that corresponds to a script
%% file.

%% By the time path_info_split is called - the fullpath has already been tested
%%  and found not to be a file or directory
%%
%% Limitation: we don't support a script file without a dot.
%%  - otherwise we'd have to hit the filesystem for too many path components
%% to see if they exist & are an executable file.
%%
%% !!todo - review (potential security issue).
%% Right-to-left scanning should stop once we reach a 'document root mount
%% point', otherwise the Docroot that has been determined based on the full
%%  request path becomes invalid!
%%
path_info_split(Comps,DR_Vdir) ->
    path_info_split(lists:reverse(Comps), DR_Vdir, []).

path_info_split([H|T], {DR, VirtualDir}, AccPathInfo) ->
    [$/|RevPath] = lists:reverse(H),
    case suffix_from_rev(RevPath) of
        [] ->   % shortcut clause, not necessary
            path_info_split(T, {DR, VirtualDir}, [H|AccPathInfo]);
        Suff ->
            {Type, Mime} = mime_types:t(Suff),
            case Type of
                regular ->
                    %%Don't hit the filesystem to test components that
                    %%'mime_types' indicates can't possibly be scripts
                    path_info_split(T, {DR, VirtualDir}, [H|AccPathInfo]);
                X ->

                    %%We may still be in the 'PATH_INFO' section
                    %%Test to see if it really is a script

                    TestPath = lists:flatten(lists:reverse(T)),
                    FullPath = construct_fullpath(DR, TestPath, VirtualDir) ++
                        string:strip(H,right,$/),

                    ?Debug("Testing for script at: ~p~n", [FullPath]),

                    case prim_file:read_file_info(FullPath) of
                        {ok, FI} when FI#file_info.type == regular ->
                            {ok, FI, FullPath, lists:reverse(T),
                             string:strip(H,right,$/), AccPathInfo, X, Mime};
                        {ok, FI} when FI#file_info.type == directory ->
                            %%just a case of a bad path starting at this point.
                            {not_a_script, error};
                        _Err ->
                            %%just looked like a script - keep going.
                            path_info_split(T, {DR, VirtualDir}, [H|AccPathInfo])
                    end
            end
    end;
path_info_split([], _DR_Vdir, _Acc) ->
    {not_a_script, error}.


suffix_from_rev(R) ->
    suffix_from_rev(R, []).

suffix_from_rev([$.|_], A) ->
    A;
suffix_from_rev([C|T], A) ->
    suffix_from_rev(T, [C|A]);
suffix_from_rev([], _A) ->
    [].

%%conc_path
%% - single-level concatenatenation of a list of path components which
%% already contain slashes.
%% tests suggest it's significantly faster than lists:flatten or lists:concat
%% & marginally faster than lists:append (for paths of 3 or more segments anyway)
%% tested with various fairly short path lists - see src/benchmarks folder
%%

%%Original
conc_path([]) ->
    [];
conc_path([H|T]) ->
    H ++ conc_path(T).

%% tail-recursive version slower for longer paths according to bench.erl
%% (mainly because we need to do 'Acc ++ H' rather than 'H ++ Acc')
%% Tail recursion not very useful here anyway as we're dealing with short strings.
%%conc_path2([]) ->
%%        [];
%%conc_path2([H|T]) ->
%%        cpath(T,H).

%%cpath([],Acc) ->
%%        Acc;
%%cpath([H|[]],Acc) ->
%%        H ++ Acc;
%%cpath([H|T],Acc) ->
%%        cpath(T,Acc ++ H).


%% ret_app_mod(Path, Mod, PrePath) ->
%%     #urltype{type = appmod,
%%              data = {Mod, Path},
%%              path = PrePath}.



%% http://a.b.c/~user URLs
ret_user_dir(Upath)  ->
    ?Debug("ret_user_dir ~p~n", [Upath]),
    SC = get(sc),
    if ?sc_has_tilde_expand(SC) ->
            case parse_user_path(SC#sconf.docroot, Upath, []) of
                {ok, User, Path} ->
                    %% FIXME doesn't work if passwd contains ::
                    %% also this is unix only
                    %% and it ain't the fastest code around.
                    case catch yaws:user_to_home(User) of
                        {'EXIT', _} ->
                            #urltype{type=error};
                        Home ->
                            DR2 = Home ++ "/public_html/",
                            SC2 = SC#sconf{
                                    allowed_scripts = SC#sconf.tilde_allowed_scripts,
                                    docroot=DR2},
                            put(sc, SC2),

                            %% !todo - review interactions between Virtual
                            %% Dirs & Home Dir paths.
                            %% VirtualDir hardcoded empty is not nice behaviour -
                            %% a rewrite mod author may reasonably expect to
                            %% be able to have influence here.

                            redir_user(do_url_type(SC2, Path, DR2,""), User)
                            %% recurse
                    end;
                {redir_dir, User} ->
                    #urltype {type = redir,
                              path = ["/~", User, "/"]}
            end;
       true ->
            #urltype{type=error}
    end.


redir_user(UT, User) ->
    case UT#urltype.type of
        redir ->
            UT#urltype{path = ["/~", User, UT#urltype.path]};
        _ ->
            UT
    end.




parse_user_path(_DR, [], User) ->
    {redir_dir, reverse(User)};
parse_user_path(_DR, [$/], User) ->
    {ok, reverse(User), [$/]};
parse_user_path(_DR, [$/|Tail], User) ->
    {ok, reverse(User), [$/|Tail]};
parse_user_path(DR, [H|T], User) ->
    parse_user_path(DR, T, [H|User]).


deflate_q(true, regular, Mime) ->
    case compressible_mime_type(Mime) of
        true -> dynamic;
        false -> undefined
    end;
deflate_q(_, _, _) ->
    undefined.


suffix_type(SC, L) ->
    R=suffix_type(L),
    case R of
        {regular, _Ext, Mime} ->
            {regular, Mime};
        {X, _Ext, Mime} ->
            case member(X, SC#sconf.allowed_scripts) of
                true -> {X, Mime};
                false -> {regular, "text/plain"}
            end
    end.

suffix_type(L) ->
    L2 = yaws:upto_char($., L),
    mime_types:revt(L2).



%% Some silly heuristics.

compressible_mime_type("text/"++_) ->
    true;
compressible_mime_type("application/rtf") ->
    true;
compressible_mime_type("application/msword") ->
    true;
compressible_mime_type("application/postscript") ->
    true;
compressible_mime_type("application/pdf") ->
    true;
compressible_mime_type("application/x-dvi") ->
    true;
compressible_mime_type("application/x-javascript") ->
    true;
compressible_mime_type(_) ->
    false.


flush(Sock, Sz) ->
    case (get(sc))#sconf.ssl of
        undefined ->
            tcp_flush(Sock, Sz);
        _ ->
            ssl_flush(Sock, Sz)
    end.


strip_list_to_integer(L) ->
    case catch list_to_integer(L) of
        {'EXIT', _} ->
            list_to_integer(string:strip(L, both));
        Int ->
            Int
    end.


tcp_flush(_Sock, undefined) ->
    ok;
tcp_flush(_Sock, 0) ->
    ok;
tcp_flush(Sock, Sz) when is_list(Sz) ->
    tcp_flush(Sock, strip_list_to_integer(Sz));
tcp_flush(Sock, Sz) ->
    gen_tcp:recv(Sock, Sz, 1000).


ssl_flush(_Sock, undefined) ->
    ok;
ssl_flush(_Sock, 0) ->
    ok;
ssl_flush(Sock, Sz) when is_list(Sz) ->
    ssl_flush(Sock, strip_list_to_integer(Sz));
ssl_flush(Sock, Sz) ->
    case ssl:recv(Sock, Sz, 1000) of
        {ok, Bin} ->
            ssl_flush(Sock, Sz - size(Bin));
        _ ->
            ok
    end.

mtime(F) ->
    F#file_info.mtime.

runmod({ok, Mod}, GC) ->
    runmod2(GC, [Mod | GC#gconf.runmods]);
runmod(_, GC) ->
    runmod2(GC, GC#gconf.runmods).

runmod2(GC, Mods) ->
    foreach(fun(M) ->
                    proc_lib:spawn(?MODULE, load_and_run,
                                   [M, ?gc_has_debug(GC)])
            end, Mods).



load_and_run(Mod, Debug) ->
    case code:ensure_loaded(Mod) of
        {module,Mod} when Debug == false ->
            Mod:start();
        {module,Mod} when Debug == true  ->
            error_logger:info_msg("sync call ~p:start ~n",[Mod]),
            Mod:start();
        Error ->
            error_logger:error_msg("Loading '~w' failed, reason ~p~n",
                                   [Mod,Error])
    end.


safe_ehtml_expand(X) ->
    case (catch ehtml_expand(X)) of
        {'EXIT', R} ->
            {error, err_pre(R)};
        Val ->
            {ok, Val}
    end.

err_pre(R) ->
    io_lib:format("<pre> ~n~p~n </pre>~n", [R]).


%%mappath/3    (virtual-path to physical-path)
%%- this returns physical path a URI would map to, taking into consideration
%% vdirs and assuming each path segment of the URI represents a folder
%% (or maybe filename at end).
%% ie it does not (and is not intended to) take into account 'script points'
%% in the path. (cgi,fcgi,php,appmod etc)
%% The result may not actually exists as a path.
%%
%% mappath/3 is analogous to the Microsoft ASP function Server.MapPath or
%% the 'filename' array member of the result
%% of the PHP function 'apache_lookup_uri'.
%%
mappath(SC, ARG, RequestPath) ->

    {VirtualDir, DR} = vdirpath(SC, ARG, RequestPath),

    PhysicalPath = construct_fullpath(DR, RequestPath, VirtualDir),

    %%Resultant path might not exist - that's not the concern of the
    %% 'mappath' function.
    PhysicalPath.


%%vdirpath/3
%%find longest "vdir" match.
%% (ie a 'document-root mount-point' -> DOCUMENT_ROOT_MOUNT)
%%
%%e.g if we have in our .conf:
%%   vdir = "/app/  /path1/somewhere"
%%   vdir = "/app/test/shared/ /path2/somewhere"
%%
%% A request path of /app/test/doc.html  must be served from under
%% /path1/somewhere
%% /app/test/shared/doc.html will be served from under /path2/somewhere
%%
%% Also must be able to handle:
%%   vdir = "/somewhere/ /path3/has spaces/in path/docs"
%%In this case, the 1st space separates the vdir from the physical path
%% i.e subsequent spaces are part of the path.

vdirpath(SC, ARG, RequestPath) ->
    Opaquelist = ARG#arg.opaque,
    %%!todo - move out of opaque.
    %% We don't want to scan all opaque entries each time
    %%- vdir directives should be pre-collated into a list somewhere.
    %% (own field in sconf record)


    RequestSegs = string:tokens(RequestPath,"/"),

    %%Accumulator is of form {RequestSegs,{VdirMountPoint,VdirPhysicalPath}}
    Matched =
        lists:foldl(
          fun(ListItem,Acc) ->
                  case ListItem of
                      {"vdir",Vmap} ->

                          {ReqSegs,VdirSpec} = Acc,

                          [Virt |PhysParts] = string:tokens(Vmap," \t"),
                          VirtSegs = string:tokens(Virt,"/"),
                          case lists:prefix(VirtSegs,ReqSegs) of
                              true ->
                                  {LongestSoFar,_} = VdirSpec,
                                  if length(Virt) > length(LongestSoFar) ->
                                          %%reassemble (because physical
                                          %% path may have spaces)
                                          Phys = yaws:join_sep(PhysParts, " "),

                                          {ReqSegs, {Virt, Phys}};
                                     true ->
                                          Acc
                                  end;
                              false ->
                                  Acc
                          end;
                      _Else ->
                          %%irrelevant member of opaque list. no change in
                          %% accumulator
                          Acc
                  end
          end, {RequestSegs,{"",""}}, Opaquelist),


    case Matched of
        {_RequestSegs, {"",""}} ->
            %%no virtual dir corresponding to this http_request.path
            %%NOTE - we *don't* know that the state of ARG#arg.docroot
            %% currently reflects the main docroot
            %% specified for the virtual server in the conf file.
            %% This is because we may be being called from a page that is
            %% under a vdir, and so docroot may
            %% have been rewritten. It may also have been rewritten by an
            %% appmod or arg_rewrite_mod.
            %% Therefore we need to get it directly from the sconf record.

            Result = {"",SC#sconf.docroot};
        {_RequestSegs, {Virt,DocRoot }} ->
            %%sanitize Virt & DocRoot so that they are correct with
            %% regards to leading & trailing slashes
            case string:right(Virt,1) of
                "/" ->
                    VirtualDir = Virt;
                _ ->
                    VirtualDir = Virt ++ "/"
            end,
            DR = string:strip(DocRoot,right,$/),

            Result = {VirtualDir, DR}
    end,

    %%return {VdirURI, Physpath}  - i.e tuple representing the data
    %% specified in conf file for the 'vdir' directive.
    Result.

close_accepted_if_max(GS,{ok, Socket}) ->
    MaxCon = (GS#gs.gconf)#gconf.max_connections,
    NumCon = GS#gs.connections,
    if (MaxCon == nolimit) or (NumCon < MaxCon) ->
	    ok;
       true ->
            S=case peername(Socket, GS#gs.ssl) of
                  {ok, {IP, Port}} ->
                      io_lib:format("~s:~w", [inet_parse:ntoa(IP), Port]);
                  _ ->
                      "unknown"
              end,
            error_logger:format(
              "Max connections reached - closing conn to ~s~n",[S]),
	    gen_tcp:close(Socket)
    end;
close_accepted_if_max(_,_) ->
    ok.

inc_con(GS) ->
    GS#gs{connections=GS#gs.connections + 1}.

dec_con(GS) ->
    GS#gs{connections=GS#gs.connections - 1}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.


fwdproxy_url(ARG) ->
    Headers = ARG#arg.headers,
    {abs_path, Path} = (ARG#arg.req)#http_request.path,

    {Host0, Port0} = yaws:split_at(Headers#headers.host, $:),
    {Host, Port} = case string:to_integer(Port0) of
                       {Port1, []} ->
                           {Host0, Port1};
                       _ ->
                           {Headers#headers.host, undefined}
                   end,

    #url{scheme = http,
         host = Host,
         port = Port,
         path = Path}.

