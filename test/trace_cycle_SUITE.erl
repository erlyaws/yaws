-module(trace_cycle_SUITE).

-include("testsuite.hrl").

-compile(nowarn_export_all).
-compile(export_all).

%%====================================================================
%% CT callbacks
%%====================================================================
all() ->
    [
     disable_trace_no_deadlock,
     enable_trace_no_deadlock,
     config_propagated_to_server,
     no_getconf_called
    ].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Test, Config) ->
    %% Start a mock yaws_server that records all messages.
    %% It never replies to getconf, simulating a blocked server.
    MockPid = start_mock_yaws_server(),
    %% Start yaws_trace.
    {ok, TracePid} = yaws_trace:start_link(),
    %% Set up yaws_trace with cached config (trace enabled, http).
    GC = #gconf{trace = {true, http}, flags = 0, logdir = "/tmp"},
    yaws_trace:setup(GC),
    %% Give the cast time to be processed.
    timer:sleep(50),
    [{mock_pid, MockPid}, {trace_pid, TracePid} | Config].

end_per_testcase(_Test, Config) ->
    TracePid = ?config(trace_pid, Config),
    MockPid = ?config(mock_pid, Config),
    %% Stop yaws_trace.
    unlink(TracePid),
    exit(TracePid, shutdown),
    %% Stop mock.
    unlink(MockPid),
    exit(MockPid, shutdown),
    %% Wait for name deregistrations.
    timer:sleep(50),
    ok.

%%====================================================================
%% Test cases
%%====================================================================

%% Verify disable_trace completes without deadlock. The mock yaws_server
%% never replies to getconf — if the old code path were used, this would
%% hang until the 2-second timeout fires.
disable_trace_no_deadlock(_Config) ->
    {Pid, Ref} = spawn_monitor(fun() ->
        exit({result, yaws_trace:disable_trace(infinity)})
    end),
    receive
        {'DOWN', Ref, process, Pid, {result, ok}} ->
            ok
    after 2000 ->
        demonitor(Ref, [flush]),
        exit(Pid, kill),
        ct:fail(deadlock_detected)
    end.

%% Verify enable_trace completes without deadlock after disabling.
enable_trace_no_deadlock(_Config) ->
    ok = yaws_trace:disable_trace(infinity),
    {Pid, Ref} = spawn_monitor(fun() ->
        exit({result, yaws_trace:enable_trace(http, infinity)})
    end),
    receive
        {'DOWN', Ref, process, Pid, {result, ok}} ->
            ok
    after 2000 ->
        demonitor(Ref, [flush]),
        exit(Pid, kill),
        ct:fail(deadlock_detected)
    end.

%% Verify that disable_trace sends an {update_trace, false} cast to
%% yaws_server (delta, not the full GC record).
config_propagated_to_server(_Config) ->
    ok = yaws_trace:disable_trace(infinity),
    %% Give cast time to arrive.
    timer:sleep(50),
    Messages = get_mock_messages(),
    TraceCasts = [Val || {cast, {update_trace, Val}} <- Messages],
    ?assert(length(TraceCasts) > 0),
    ?assertEqual(false, lists:last(TraceCasts)).

%% Verify that no getconf call was ever received by the mock.
no_getconf_called(_Config) ->
    ok = yaws_trace:disable_trace(infinity),
    ok = yaws_trace:enable_trace(http, infinity),
    timer:sleep(50),
    Messages = get_mock_messages(),
    GetconfCalls = [M || {call, getconf, _From} <- Messages, M <- [ok]],
    ?assertEqual([], GetconfCalls).

%%====================================================================
%% Mock yaws_server
%%====================================================================

%% A minimal gen_server registered as yaws_server that:
%% - Records all received calls and casts
%% - Never replies to getconf (simulates blocked server)
%% - Replies to get_messages with the recorded message list
start_mock_yaws_server() ->
    Parent = self(),
    Pid = spawn_link(fun() ->
        register(yaws_server, self()),
        Parent ! {mock_ready, self()},
        mock_loop([])
    end),
    receive
        {mock_ready, Pid} -> Pid
    after 1000 ->
        error(mock_start_timeout)
    end.

mock_loop(Messages) ->
    receive
        {'$gen_call', From, get_messages} ->
            gen_server:reply(From, lists:reverse(Messages)),
            mock_loop(Messages);
        {'$gen_call', From, getconf} ->
            %% Never reply — simulates a blocked/deadlocked server.
            mock_loop([{call, getconf, From} | Messages]);
        {'$gen_call', From, Msg} ->
            gen_server:reply(From, ok),
            mock_loop([{call, Msg, From} | Messages]);
        {'$gen_cast', Msg} ->
            mock_loop([{cast, Msg} | Messages]);
        _Other ->
            mock_loop(Messages)
    end.

get_mock_messages() ->
    gen_server:call(yaws_server, get_messages, 1000).
