%%%----------------------------------------------------------------------
%%% File    : yaws_session_server.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : maintain state for cookie sessions
%%% Created : 17 Sep 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_session_server).
-author('klacke@hyber.org').


-behaviour(gen_server).

%% External exports
-export([start_link/0, start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-compile(export_all).

-include("../include/yaws_api.hrl").


-record(state, 
	{ss,       %% time queue of sessions
	 ttl =    (1000 * 60 * 30)  %% 30 minutes
	}).




%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, yaws_session_server}, 
			  yaws_session_server, [], []).
start() ->
    gen_server:start({local, yaws_session_server}, 
		     yaws_session_server, [], []).
stop() ->
    gen_server:call(?MODULE, stop).


new_session(User, Passwd, Opaque) ->
    gen_server:call(?MODULE, {new_session, User, Passwd, Opaque}).
cookieval_to_session(C) ->
    gen_server:call(?MODULE, {cookieval_to_session, C}).
print_sessions() ->
    Ss = qto_list(gen_server:call(?MODULE, sessions)),
    io:format("** ~p users logged in ~n~n", [length(Ss)]),
    N = gnow(),
    lists:foreach(fun(S) ->
			  io:format("User     ~p ~n", [S#ysession.user]),
			  io:format("Cookie   ~p ~n", [S#ysession.cookie]),
			  io:format("Start    ~p ~n", [S#ysession.starttime]),
			  io:format("TTL      ~p secs~n", [S#ysession.to - N]),
			  io:format("Opaque   ~p ~n~n~n", [S#ysession.opaque]),
			  ok
		  end, Ss).


replace_session(Session, User) ->
    gen_server:call(?MODULE, {replace_session, Session, User}).



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
    {X,Y,Z} = seed(),
    random:seed(X, Y, Z),
    {ok, #state{ss = qnew()}}.



%% pretty good seed, but non portable
seed() ->
    case (catch list_to_binary(
	   os:cmd("dd if=/dev/random ibs=12 count=1 2>/dev/null"))) of
	<<X:32, Y:32, Z:32>> ->
	    {X, Y, Z};
	_ ->
	    now()
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



handle_call({new_session, User, Passwd, Opaque}, _From, State) ->
    Ss = State#state.ss,
    Now = gnow(),
    case qkeysearch(User, #ysession.user, Ss) of
	{value, S} ->
	    tret({error, {has_session, S}}, Now, State);
	false ->
	    N = random:uniform(16#ffffffffffffffff), %% 64 bits
	    TS = calendar:local_time(),
	    C = atom_to_list(node()) ++ [$-|integer_to_list(N)],
	    NS = #ysession{cookie = C,
			  user = User,
			  passwd = Passwd,
			  starttime = TS,
			  opaque = Opaque,
			  to = gnow() + State#state.ttl},
	    tret({ok, NS}, Now, State#state{ss = qin(NS, Ss)})
    end;


handle_call({cookieval_to_session, C}, _From, State) ->
    Q = State#state.ss,
    Now = gnow(),
    case qkey_delete(C, #ysession.cookie, Q) of
	{value, Session, Q2} ->
	    Q3 = qin(Session#ysession{to = Now + State#state.ttl}, Q2),
	    tret({ok, Session}, Now, State#state{ss = Q3});
	false ->
	    tret({error,no_session}, Now, State)
    end;

handle_call({replace_session, S, User}, From, State) ->
    Q = State#state.ss,
    Now = gnow(),
    case qkey_delete(User, #ysession.user, Q) of
	{value, Session, Q2} ->
	    Q3 = qin(S#ysession{to = Now + State#state.ttl}, Q2),
	    tret(ok, Now, State#state{ss = Q3});
	false ->
	    tret({error,no_session}, Now, State)
    end;


handle_call(sessions, _From, State) ->
    tret(State#state.ss, gnow(), State);
handle_call(stop, _From, State) ->
    {stop, stopped, State}.



%% a timeout return value based on the oldest session in
%% the queue

tret(Val, Now, State) ->
    {S2, TO} = state_to_timeout(State, Now),
    {reply, Val, S2, TO}.



state_to_timeout(State, Now) ->
    Q = State#state.ss,
    case qfirst(Q) of
	{{value, OldestSession}, Q2} ->
	    TO = OldestSession#ysession.to,
	    if
		 TO > Now ->
		    {State#state{ss = Q2},  (TO - Now) * 1000};
		true ->
		    %% oldest item has timed out
		    %% we need to remove it
		    {{value, O}, Q3} = qout(Q2),
		    report_timedout_sess(O),
		    state_to_timeout(State#state{ss = Q3}, Now)
	    end;
	{empty, _} ->
	    {State, infinity}
    end.


report_timedout_sess(S) ->
    error_logger:info_msg("Session for user ~p timedout ", [S#ysession.user]).


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
handle_info(timeout, State) ->
    Q = State#state.ss,
    case qout(Q) of
	{{value, S}, Q2} ->
	    report_timedout_sess(S),
	    {S2, TO} = state_to_timeout(State#state{ss = Q2}, gnow()),
	    {noreply, S2, TO};
	{empty, _} ->
	    {noreply, State}
    end.


%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------



gnow() ->
    calendar:datetime_to_gregorian_seconds(
      calendar:local_time()).




%% queue ops

qnew() -> {[], []}.

qin(X, {In, Out}) -> {[X|In], Out}.


qout({In, [H|Out]}) ->
        {{value, H}, {In, Out}};
qout({[], []}) ->
        {empty, {[],[]}};
qout({In, _}) ->
        qout({[], lists:reverse(In)}).



qfirst({In, [H|Out]}) ->
        {{value, H}, {In, [H|Out]}};
qfirst({[], []}) ->
        {empty, {[],[]}};
qfirst({In, _}) ->
        qfirst({[], lists:reverse(In)}).




qto_list({In, Out}) ->
    lists:append(Out, lists:reverse(In)).


qkey_delete(Key, Pos, {Head, Tail}) ->
    case qkey_search_delete(Key, Pos, Head, []) of
	false ->
	    case qkey_search_delete(Key,Pos, Tail, []) of
		false ->
		    false;
		{value, Val, L2} ->
		    {value, Val, {Head, L2}}
	    end;
	{value, Val, L3} ->
	    {value, Val, {L3, Tail}}
    end.

	

qkeysearch(Key, Pos, {Head, Tail}) ->
    case lists:keysearch(Key, Pos, Head) of
	{value, Val} ->
	    {value, Val};
	false ->
	    lists:keysearch(Key,Pos, Tail)
    end.



qkeyreplace(Key, Pos, Item, {Head, Tail}) ->
    {lists:keyreplace(Key,Pos,Head,Item),
     lists:keyreplace(Key,Pos,Tail,Item)}.



%% internal functions

qkey_search_delete(_Key, _Pos, [], _Ack) ->
    false;
qkey_search_delete(Key,Pos, [H|T], Ack) ->
    if
	element(Pos, H) == Key ->
	    {value, H, lists:reverse(Ack) ++ T};
	true ->
	    qkey_search_delete(Key,Pos, T, [H|Ack])
    end.

