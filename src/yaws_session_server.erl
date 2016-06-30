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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("../include/yaws_api.hrl").
-include("../include/yaws.hrl").

-export([new_session/1,new_session/2,new_session/3,new_session/4,
         cookieval_to_opaque/1,
         print_sessions/0,
         replace_session/2, replace_session/3,
         delete_session/1]).

%% Default ETS backend callbacks
-export ([init_backend/1, stop_backend/0,
          list/0, lookup/1, insert/1, delete/1,
          traverse/1, cleanup/0]).

%% Utility functions for callbacks
-export ([has_timedout/2, report_timedout_sess/1, cookie/1]).

-define(TTL, (30 * 60)).  % 30 minutes

-record(ysession,
        {cookie,       %% the cookie assigned to the session
         to,           %% greg secs untill timeout death
         ttl,          %% default time to live
         starttime,    %% When calendar:local_time() did sess start
         cleanup,      %% PID to notify of session end
         opaque        %% any data the user supplies
        }).

-record(state,
        {backend      %% storage engine module
        }).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    Backend = get_yaws_session_server_backend(),
    gen_server:start_link({local, yaws_session_server},
                          yaws_session_server, Backend, []).
start() ->
    Backend = get_yaws_session_server_backend(),
    gen_server:start({local, yaws_session_server},
                     yaws_session_server, Backend, []).
stop() ->
    gen_server:call(?MODULE, stop, infinity).


%% We are bending over here in our pursuit of finding a
%% proper ysession_server backend.
get_yaws_session_server_backend() ->
    #gconf{ysession_mod = DefaultBackend} = #gconf{},
    case yaws_server:getconf() of
        {ok, #gconf{ysession_mod = Backend}, _} -> Backend;
        _ ->
            case application:get_env(yaws, embedded) of
                {ok, true} ->
                    case application:get_env(yaws, embedded_conf) of
                        {ok, L} when is_list(L) ->
                            case lists:keyfind(gc, 1, L) of
                                {_, #gconf{ysession_mod = Backend}} ->
                                    Backend;
                                _ ->
                                    DefaultBackend
                            end;
                        _ ->
                            DefaultBackend
                    end;
                _ ->
                    DefaultBackend
            end
    end.


%% will return a new cookie as a string
new_session(Opaque) ->
    Call = {new_session, Opaque, ?TTL, undefined, undefined},
    gen_server:call(?MODULE, Call, infinity).

new_session(Opaque, TTL) ->
    Call = {new_session, Opaque, TTL, undefined, undefined},
    gen_server:call(?MODULE, Call, infinity).

new_session(Opaque, TTL, Cleanup) ->
    Call = {new_session, Opaque, TTL, Cleanup, undefined},
    gen_server:call(?MODULE, Call, infinity).

new_session(Opaque, TTL, Cleanup, Cookie) ->
    Call = {new_session, Opaque, TTL, Cleanup, Cookie},
    gen_server:call(?MODULE, Call, infinity).

cookieval_to_opaque(Cookie) ->
    gen_server:call(?MODULE, {cookieval_to_opaque, Cookie}, infinity).

print_sessions() ->
    gen_server:cast(?MODULE, print_sessions).

replace_session(Cookie, NewOpaque) ->
    gen_server:call(?MODULE, {replace_session, Cookie, NewOpaque, undefined},
                    infinity).
replace_session(Cookie, NewOpaque, Cleanup) ->
    gen_server:call(?MODULE, {replace_session, Cookie, NewOpaque, Cleanup},
                    infinity).

delete_session(CookieVal) ->
    gen_server:call(?MODULE, {delete_session, CookieVal}, infinity).

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
init(Backend) ->
    Backend:init_backend(record_info(fields, ysession)),
    start_long_timer(),
    {ok, #state{backend = Backend}, to()}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call({new_session, Opaque, undefined, Cleanup, Cookie}, From, State) ->
    handle_call({new_session, Opaque, ?TTL, Cleanup, Cookie}, From, State);

handle_call({new_session, Opaque, TTL, Cleanup, undefined}, From, State) ->
    N = bin2int(yaws_dynopts:rand_bytes(16)),
    Cookie = atom_to_list(node()) ++ [$-|integer_to_list(N)],
    handle_call({new_session, Opaque, TTL, Cleanup, Cookie}, From, State);

handle_call({new_session, Opaque, TTL, Cleanup, Cookie}, _From, State) ->
    Now = gnow(),
    TS = calendar:local_time(),
    NS = #ysession{cookie = Cookie,
                   starttime = TS,
                   opaque = Opaque,
                   to = Now + TTL,
                   ttl = TTL,
                   cleanup = Cleanup},
    Backend = State#state.backend,
    true = Backend:insert(NS),
    {reply, Cookie, State, to()};

handle_call({cookieval_to_opaque, Cookie}, _From, State) ->
    Backend = State#state.backend,
    Result =
        case Backend:lookup(Cookie) of
            [Y] ->
                Y2 = Y#ysession{to = gnow() + Y#ysession.ttl},
                Backend:insert(Y2),
                {ok, Y#ysession.opaque};
            [] ->
                {error, no_session}
        end,
    {reply, Result, State, to()};

handle_call({replace_session, Cookie, NewOpaque}, _From, State) ->
    handle_call({replace_session, Cookie, NewOpaque, undefined}, _From, State);
handle_call({replace_session, Cookie, NewOpaque, Cleanup}, _From, State) ->
    Backend = State#state.backend,
    Result =
        case Backend:lookup(Cookie) of
            [Y] ->
                Y2 = Y#ysession{to = gnow() + Y#ysession.ttl,
                                opaque = NewOpaque,
                                cleanup = case Cleanup of
                                              undefined ->
                                                  Y#ysession.cleanup;
                                              _ ->
                                                  Cleanup
                                          end},
                Backend:insert(Y2);
            [] ->
                error
        end,
    {reply, Result, State, to()};

handle_call({delete_session, CookieVal}, _From, State) ->
    Backend = State#state.backend,
    Result =
        case Backend:lookup(CookieVal) of
            [Y] ->
                Backend:delete(CookieVal),
                report_deleted_sess(Y);
            [] ->
                true
        end,
    {reply, Result, State, to()};

handle_call(stop, From, State) ->
    gen_server:reply(From, ok),
    {stop, normal, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(print_sessions, #state{backend = Backend} = State) ->
    Ss = Backend:list(),
    io:format("** ~p sessions active ~n~n", [length(Ss)]),
    N = gnow(),
    lists:foreach(fun(S) ->
                          io:format("Cookie   ~p ~n", [S#ysession.cookie]),
                          io:format("Start    ~p ~n", [S#ysession.starttime]),
                          io:format("TTL      ~p secs~n", [S#ysession.to - N]),
                          io:format("Opaque   ~p ~n~n~n", [S#ysession.opaque]),
                          ok
                  end, Ss),
    {noreply, State, to()};

handle_cast(_Msg, State) ->
    {noreply, State, to()}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(timeout, #state{backend = Backend} = State) ->
    Backend:traverse(gnow()),
    {noreply, State, to()};

handle_info(long_timeout, #state{backend = Backend} = State) ->
    Backend:traverse(gnow()),
    start_long_timer(),
    {noreply, State, to()}.


%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, #state{backend = Backend}) ->
    Backend:stop_backend(),
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Handle upgrade
%% Returns: new State data
%%----------------------------------------------------------------------
code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

bin2int(Bin) ->
    lists:foldl(fun(N, Acc) -> Acc * 256 + N end, 0, binary_to_list(Bin)).

%% timeout once every hour even if the server handles traffic all the time.
start_long_timer() ->
    erlang:send_after(long_to(), self(), long_timeout).

long_to() ->
    Default = 60 * 60 * 1000,
    try yaws_server:getconf() of
        {ok, #gconf{ysession_long_timeout = LongTO}, _} ->
            LongTO;
        _ ->
            case application:get_env(yaws, embedded) of
                {ok, true} ->
                    case application:get_env(yaws, embedded_conf) of
                        {ok, L} when is_list(L) ->
                            case lists:keyfind(gc, 1, L) of
                                {_, #gconf{ysession_long_timeout = LongTO}} ->
                                    LongTO;
                                _ ->
                                    Default
                            end;
                        _ ->
                            Default
                    end;
                _ ->
                    Default
            end
    catch
        _:_ ->
            %% server not running yet; timeout quickly so we can try again
            10 * 1000
    end.

%% timeout if the server is idle for more than 2 minutes.
to() ->
    Default = 2 * 60 * 1000,
    case catch yaws_server:getconf() of
        {ok, #gconf{ysession_idle_timeout = IdleTO}, _} ->
            IdleTO;
        _ ->
            case application:get_env(yaws, embedded) of
                {ok, true} ->
                    case application:get_env(yaws, embedded_conf) of
                        {ok, L} when is_list(L) ->
                            case lists:keyfind(gc, 1, L) of
                                {_, #gconf{ysession_idle_timeout = IdleTO}} ->
                                    IdleTO;
                                _ ->
                                    Default
                            end;
                        _ ->
                            Default
                    end;
                _ ->
                    Default
            end
    end.

gnow() ->
    calendar:datetime_to_gregorian_seconds(
      calendar:local_time()).

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

has_timedout(Y, Time) ->
    Y#ysession.to =< Time.

cookie(Y) ->
    Y#ysession.cookie.

%% Backend callbacks (ETS as default)

init_backend (_) ->
    ets:new(?MODULE, [set, named_table, public, {keypos, 2}]).

stop_backend() ->
    ok.

lookup(Key) ->
    ets:lookup(?MODULE, Key).

insert(Session) ->
    ets:insert(?MODULE, Session).

list() ->
    ets:tab2list(?MODULE).

delete(Key) ->
    ets:delete(?MODULE, Key).

cleanup() ->
    ets:delete_all_objects(?MODULE).

traverse(N) ->
    traverse(N, ets:first(?MODULE)).

traverse(_N, '$end_of_table') ->
    ok;
traverse(N, Key) ->
    case lookup(Key) of
        [Y] ->
            case has_timedout(Y, N) of
                false ->
                    traverse(N, ets:next(?MODULE, Key));
                true ->
                    report_timedout_sess(Y),
                    Next = ets:next(?MODULE, Key),
                    delete(Key),
                    traverse(N, Next)
            end;
        [] ->
            traverse(N, ets:next(?MODULE, Key))
    end.

