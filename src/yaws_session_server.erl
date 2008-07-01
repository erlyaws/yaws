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
-export([start_link/0, start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include("../include/yaws_api.hrl").

-export([new_session/1,new_session/2,new_session/3,
         cookieval_to_opaque/1,
         print_sessions/0,
         replace_session/2,
         delete_session/1]).
         

-define(TTL, (30 * 60)).  % 30 minutes

-record(ysession,
        {cookie,       %% the cookie assigned to the session
         to,           %% greg secs untill timeout death
         ttl,          %% default time to live
         starttime,    %% When calendar:local_time() did sess start
         cleanup,      %% PID to notify of session end
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
    gen_server:call(?MODULE, stop, infinity).


%% will return a new cookie as a string
new_session(Opaque) ->
    gen_server:call(?MODULE, {new_session, Opaque, ?TTL, undefined}, infinity).

new_session(Opaque, TTL) ->
    gen_server:call(?MODULE, {new_session, Opaque, TTL, undefined}, infinity).

new_session(Opaque, TTL, Cleanup) ->
    case TTL of
        undefined ->
            gen_server:call(?MODULE, 
                            {new_session, Opaque, ?TTL, Cleanup}, infinity);
        _ ->
            gen_server:call(?MODULE, 
                            {new_session, Opaque, TTL, Cleanup}, infinity)
    end.

cookieval_to_opaque(CookieString) ->
    case ets:lookup(?MODULE, CookieString) of
        [Y] ->
            Y2 = Y#ysession{to = gnow() + Y#ysession.ttl},
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
            Y2 = Y#ysession{to = gnow() + Y#ysession.ttl,
                            opaque = NewOpaque},
            ets:insert(?MODULE, Y2);
        [] ->
            error
    end.


delete_session(CookieVal) ->
    case ets:lookup(?MODULE, CookieVal) of
        [Y] -> 
            ets:delete(?MODULE, CookieVal),
            report_deleted_sess(Y);
        [] ->
            true
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
    {X,Y,Z} = seed(),
    random:seed(X, Y, Z),
    ets:new(?MODULE, [set, named_table, public, {keypos, 2}]),
    start_long_timer(),
    {ok, undefined, to()}.

%% timeout once every hour even if the server handles traffic all the time.
start_long_timer() ->
    erlang:send_after(long_to(), self(), long_timeout).

long_to() ->
    60 * 60 * 1000.

%% timeout if the server is idle for more than 2 minutes.
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


handle_call({new_session, Opaque, TTL, Cleanup}, _From, _State) ->
    Now = gnow(),
    N = random:uniform(16#ffffffffffffffff), %% 64 bits
    TS = calendar:local_time(),
    C = atom_to_list(node()) ++ [$-|integer_to_list(N)],
    NS = #ysession{cookie = C,
                   starttime = TS,
                   opaque = Opaque,
                   to = Now + TTL,
                   ttl = TTL,
                   cleanup = Cleanup},
    ets:insert(?MODULE, NS),
    {reply, C, undefined, to()};

handle_call(stop, _From, State) ->
    {stop, stopped, State}.


send_cleanup_message(Sess,Msg) ->
    case Sess#ysession.cleanup of
        undefined ->
            nocleanup;
        Pid ->
            Pid ! Msg
    end.

report_timedout_sess(S) ->
    send_cleanup_message(S,{yaws_session_end,timeout,
                            S#ysession.cookie, S#ysession.opaque}).

report_deleted_sess(S) ->
    send_cleanup_message(S,{yaws_session_end,normal,
                            S#ysession.cookie, S#ysession.opaque}).


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State, to()}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(timeout, _State) ->
    trav_ets(),
    {noreply, undefined, to()};

handle_info(long_timeout, _State) ->
    trav_ets(),
    start_long_timer(),
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

