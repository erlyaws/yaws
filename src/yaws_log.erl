%%%----------------------------------------------------------------------
%%% File    : yaws_log.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 26 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_log).
-author('klacke@hyber.org').
-include_lib("kernel/include/file.hrl").
-compile(export_all).
%%-export([Function/Arity, ...]).

-behaviour(gen_server).

%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include("yaws.hrl").
-record(state, {
	  running,
	  dir,
	  now,
	  ack,
	  tracefd,
	  alogs =  []}).
		

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, yaws_log}, yaws_log, [], []).

accesslog(ServerName, Ip, Req, Status, Length) ->
    gen_server:cast(?MODULE, {access, ServerName, Ip, Req, 
			      Status, Length}).
errlog(F, A) ->     
    gen_server:cast(?MODULE, {errlog, F, A}).
infolog(F, A) ->     
    gen_server:cast(?MODULE, {infolog, F, A}).
sync_errlog(F, A) ->     
    gen_server:call(?MODULE, {errlog, F, A}).
setdir(Dir, Sconfs) ->
    gen_server:call(?MODULE, {setdir, Dir, Sconfs}).

open_trace(What) ->
    gen_server:call(?MODULE, {open_trace, What}).
trace_traffic(ServerOrClient , Data) ->
    gen_server:cast(?MODULE, {trace, ServerOrClient, Data}).


%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    yaws:ticker(3000, secs3),
    yaws:ticker(60 * 1000, minute),
    {ok, #state{running = false, now = fmtnow(), ack = []}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({setdir, Dir, Sconfs}, From, State) 
  when State#state.running == false ->
    ?Debug("setdir ~s~n~p", [Dir, State#state.ack]),

    error_logger:logfile({open,filename:join([Dir, "report.log"])}),
    SCs = lists:flatten(Sconfs),
    
    L = lists:zf(
	  fun(SC) ->
		  A = filename:join([Dir, SC#sconf.servername ++ ".access"]),
		  case file:open(A, [write, raw, append]) of
		      {ok, Fd} ->
			  {true, {SC#sconf.servername, Fd, A}};
		      Err ->
			  error_logger:format("Cannot open ~p",[A]),
			  false
		  end
	  end, SCs),

    lists:foreach(fun({err, F, A}) ->
			  error_logger:format(F, A);
		     ({info, F, A}) ->
			  error_logger:info_msg(F,A);
		     ({access, ServerName, Ip, Req, Status, Length}) ->
			  do_alog(ServerName, Ip, Req, Status, Length, State)
		  end, State#state.ack),
    S2 = State#state{running = true,
		     ack = [],
		     dir  = Dir,
		     alogs = L},
    {reply, ok, S2};


handle_call({errlog, F, A}, From, State) when State#state.running == true ->
    error_logger:format(F, A),
    {reply, ok, State};
handle_call({errlog, F, A}, From, State) when State#state.running == false ->
    {reply, ok, State#state{ack = [{err, F, A} | State#state.ack]}};

handle_call({open_trace, What}, From, State) ->
    F = lists:concat(["trace.", What]),
    case file:open(filename:join([State#state.dir, F]),[write, raw]) of
	{ok, Fd} ->
	    {reply, ok, State#state{tracefd = Fd}};
	Err ->
	    {reply,  Err, State}
    end.






%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast({errlog, F, A}, State) when State#state.running == true ->
    error_logger:format(F, A),
    {noreply, State};
handle_cast({errlog, F, A}, State) when State#state.running == false ->
    {noreply, State#state{ack = [{err, F, A} | State#state.ack]}};


handle_cast({infolog, F, A}, State) when State#state.running == true ->
    error_logger:info_report(F, A),
    {noreply, State};
handle_cast({infolog, F, A}, State) when State#state.running == false ->
    {noreply, State#state{ack = [{info, F, A} | State#state.ack]}};



handle_cast({access, ServerName, Ip, Req, Status, Length}, State) ->
    case State#state.running of 
	true ->
	    do_alog(ServerName, Ip, Req, Status, Length, State),
	    {noreply, State};
	false ->
	    {noreply, State#state{ack = [{access, ServerName, Ip, Req, 
					  Status,  Length} |
					 State#state.ack]}}
    end;
handle_cast({trace, from_server, Data}, State) ->
    file:write(State#state.tracefd, ["*** SRV -> CLI *** ", Data]),
    {noreply, State};
handle_cast({trace, from_client, Data}, State) ->
    file:write(State#state.tracefd, ["*** CLI -> SRV *** ", Data]),
    {noreply, State}.



do_alog(ServerName, Ip, Req, Status, Length, State) ->
    case lists:keysearch(ServerName, 1, State#state.alogs) of
	{value, {_, FD, _}} ->
	    I = fmt_alog(State#state.now, Ip, Req, Status,  Length),
	    file:write(FD, I);
	_ ->
	    false
    end.


%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(secs3, State) ->
    {noreply, State#state{now = fmtnow()}};

%% once a minute, check log sizes
handle_info(minute, State) ->
    L = lists:map(
	  fun({FD, Sname, Filename}) ->
		  {ok, FI} = file:read_file_info(Filename),
		  if
		      FI#file_info.size > 50000 ->
			  file:close(FD),
			  B = filename:basename(Filename, ".access"),
			  Old = B ++ ".old",
			  file:delete(Old),
			  file:rename(Filename, Old),
			  {ok, Fd2} = file:open(Filename, [write, raw]),
			  {Fd2, Sname, Filename};
		      true ->
			  {FD, Sname, Filename}
		  end
	  end, State#state.alogs),
    
    Dir = State#state.dir,
    E = filename:join([Dir, "report.log"]),
    {ok, FI} = file:read_file_info(E),
    if
	FI#file_info.size > 50000 ->
	    B = filename:basename(E, ".log"),
	    error_logger:logfile(close),
	    Old = B ++ ".old",
	    file:delete(Old),
	    file:rename(E, Old),
	    error_logger:logfile({open, E});
	true ->
	    ok
    end,
    {noreply, State#state{alogs= L}}.

    

		  






%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------


fmt_alog(Time, Ip, Req, Status,  Length) ->
    [yaws:fmt_ip(Ip), " - - ", Time, [$\s, $"], Req, [$",$\s], 
     Status, [$\s], Length, " - -\n"].

fmtnow() ->
    {{Year, Month, Date}, {Hour, Min, Sec}} = calendar:universal_time(),
    io_lib:format("[~w/~s/~w:~w:~w:~w GMT]",
		  [Date,yaws:month(Month),Year, Hour, Min, Sec]).

zone() ->
    {_, {U, _,_}} = erlang:universaltime(),
    {_, {L, _,_}} = erlang:localtimetime(),
    uhhh.

