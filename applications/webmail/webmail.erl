%%%----------------------------------------------------------------------
%%% File    : webmail.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 24 Jun 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(webmail).
-author('klacke@hyber.org').
-include("webmail.hrl").
-include("../../include/yaws_api.hrl").

-behaviour(gen_server).

-compile(export_all).
%%-export([Function/Arity, ...]).

%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {pophost = {127,0,0,1},
		popport = 110,

		smpthost = {127,0,0,1},
		smtpport = 25,

		sessions = []}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, webmail}, webmail, [], []).

state() ->
    gen_server:call(?MODULE, state).

is_connected(User) ->
    gen_server:call(?MODULE, {is_connected, User}).

newsession(User, Password) ->
    S = state(),
    case is_connected(User) of
	{true, Cookie} ->
	    {ok, Cookie};
	false ->
	    case pop3lib_cli:connect([{user, User},
				      {passwd, Password},
				      {addr, S#state.pophost},
				      {port, S#state.popport}]) of
		{ok, Session} ->
		    gen_server:call(?MODULE, {newsession, User, Session});
		Err ->
		    Err
	    end
    end.
			 
login_status(A) -> 
    H=A#arg.headers,
    C = H#headers.cookie,
    case yaws_api:find_cookie_val("ymailuser", C) of
	[] ->
	    "Unknown user, not lgged in";
	User ->
	    case is_connected(User) of
		{true, _} ->
		    io_lib:format("User ~s is logged in", [User]);
		false ->
		    io_lib:format("User ~s is not logged in", [User])
	    end
    end.


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
    {X, Y, Z} = now(),
    random:seed(X, Y, Z),
    {ok, #state{}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call(state, From, State) ->
    {reply, State, State};

handle_call({is_connected, User}, From, State) ->
    R = case lists:keysearch(User, 2, State#state.sessions) of
	{value, S} ->
	    {true, S#s.cookie};
	false ->
	    false
    end,
    {reply, R, State};


handle_call({newsession, User, PopSession}, From, State) ->
    N = random:uniform(16#ffffffff),
    Cookie = User ++ integer_to_list(N),
    S = #s{user = User,
	   cookie = Cookie,
	   pops = PopSession,
	   logintime = calendar:localtime(),
	   idletimer = spawn_link(?MODULE, idle_timer, [self()])},
    S2 = State#state{sessions = [S | State#state.sessions]},
    {reply, {ok, Cookie}, S2}.


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

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
