%%%----------------------------------------------------------------------
%%% File    : yaws_log.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 26 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_log).
-author('klacke@hyber.org').

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
	  now,
	  ack,
	  accesslog,
	  afd}).

		

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, yaws_log}, yaws_log, [], []).

accesslog(Ip, Req, Status, Length) ->
    gen_server:cast(?MODULE, {access, Ip, Req, Status, Length}).
errlog(F, A) ->     
    gen_server:cast(?MODULE, {errlog, F, A}).
sync_errlog(F, A) ->     
    gen_server:call(?MODULE, {errlog, F, A}).
setdir(Dir) ->
    gen_server:call(?MODULE, {setdir, Dir}).


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
    yaws:ticker(3000),
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
handle_call({setdir, Dir}, From, State) when State#state.running == false ->
    ?Debug("setdei ~s~n~p", [Dir, State#state.ack]),
    error_logger:logfile({open,filename:join([Dir, "error.log"])}),
    Alog = filename:join([Dir, "access"]),
    case file:open(Alog, [write, raw, append]) of  %% FIXME wrap log
	{ok, Fd} ->
	    lists:foreach(fun({err, F, A}) ->
				  error_logger:format(F, A);
			     ({access, Ip, Req, Status, Length}) ->
				  I = fmt_alog(State#state.now,
					       Ip, Req, Status, 
					       Length),
				  file:write(Fd, I)
			  end, State#state.ack),
	    S2 = State#state{running = true,
			     ack = [],
			     accesslog = Alog,
			     afd = Fd},
	    {reply, ok, S2};
	Err ->
	    error_logger:format("Cannot open ~s~n", [Alog]),
	    {stop, Err}
    end;
handle_call({errlog, F, A}, From, State) when State#state.running == true ->
    error_logger:format(F, A),
    {reply, ok, State};
handle_call({errlog, F, A}, From, State) when State#state.running == false ->
    {reply, ok, State#state{ack = [{err, F, A} | State#state.ack]}}.




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
handle_cast({access, Ip, Req, Status, Length}, State) ->
    case State#state.running of 
	true ->
	    I = fmt_alog(State#state.now,Ip, Req, Status,  Length),
	    file:write(State#state.afd, I),
	    {noreply, State};
	false ->
	    {noreply, State#state{ack = [{access, Ip, Req, Status,  Length} |
					 State#state.ack]}}
    end.




%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(tick, State) ->
    {noreply, State#state{now = fmtnow()}}.

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

