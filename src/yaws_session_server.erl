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


-define(TTL, (30 * 60)).  % 30 minutes

-record(ysession,
	{cookie,       %% the cookie assigned to the session
	 to,           %% greg secs untill timeout death
	 starttime,    %% When calendar:local_time() did sess start
	 opaque        %% any data the user supplies
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


%% will return a new cookie as a string
new_session(Opaque) ->
    gen_server:call(?MODULE, {new_session, Opaque}).

cookieval_to_opaque(CookieString) ->
    case ets:lookup(?MODULE, CookieString) of
	[Y] ->
	    Y2 = Y#ysession{to = gnow() + ?TTL},
	    ets:insert(?MODULE, Y2),
	    {ok, Y#ysession.opaque};
	[] ->
	    {error, no_session}
    end.


print_sessions() ->
    Ss = ets:tab2list(?MODULE),
    io:format("** ~p sessions active ~n~n", [length(Ss)]),
    N = gnow(),
    lists:foreach(fun(S) ->
			  io:format("Cookie   ~p ~n", [S#ysession.cookie]),
			  io:format("Start    ~p ~n", [S#ysession.starttime]),
			  io:format("TTL      ~p secs~n", [S#ysession.to - N]),
			  io:format("Opaque   ~p ~n~n~n", [S#ysession.opaque]),
			  ok
		  end, Ss).


replace_session(Cookie, NewOpaque) ->
    case ets:lookup(?MODULE, Cookie) of
	[Y] ->
	    Y2 = Y#ysession{to = gnow() + ?TTL},
	    ets:insert(?MODULE, Y2),
	    ets:insert(?MODULE, Y#ysession{opaque = NewOpaque});
	[] ->
	    error
    end.


delete_session(CookieVal) ->
    ets:delete(?MODULE, CookieVal).


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
    ets:new(?MODULE, [set, named_table, public, {keypos, 2}]),
    {ok, undefined, to()}.


to() ->
    2 * 60 * 1000.  


%% pretty good seed, but non portable
seed() ->
    case (catch list_to_binary(
	   os:cmd("dd if=/dev/urandom ibs=12 count=1 2>/dev/null"))) of
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



handle_call({new_session, Opaque}, _From, _State) ->
    Now = gnow(),
    N = random:uniform(16#ffffffffffffffff), %% 64 bits
    TS = calendar:local_time(),
    C = atom_to_list(node()) ++ [$-|integer_to_list(N)],
    NS = #ysession{cookie = C,
		   starttime = TS,
		   opaque = Opaque,
		   to = Now + ?TTL},
    ets:insert(?MODULE, NS),
    {reply, C, undefined, to()};

handle_call(stop, _From, State) ->
    {stop, stopped, State}.



report_timedout_sess(S) ->
    error_logger:info_msg("Session timedout: ~p ", [S#ysession.opaque]).


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
handle_info(timeout, _State) ->
    trav_ets(),
    {noreply, undefined, to()}.


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

trav_ets() ->
    N = gnow(),
    trav_ets(N, ets:first(?MODULE)).

trav_ets(_N, '$end_of_table') ->
    ok;
trav_ets(N, Key) ->
    case ets:lookup(?MODULE, Key) of
	[Y] ->
	    if
		Y#ysession.to > N ->
		    trav_ets(N, ets:next(?MODULE, Key));
		true ->
		    report_timedout_sess(Y),
		    Next = ets:next(?MODULE, Key),
		    ets:delete(?MODULE, Key),
		    trav_ets(N, Next)
		    
	    end;
	[] ->
	   trav_ets(N, ets:next(?MODULE, Key))
    end.

gnow() ->
    calendar:datetime_to_gregorian_seconds(
      calendar:local_time()).

