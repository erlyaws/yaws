%% Regression test for yaws_rss startup ordering.
%%
%% yaws_rss:open/2 is called during yaws_config:load (inside
%% yaws_server:init/1) when the config file contains an RSS section.
%% yaws_rss must be registered before yaws_server starts, otherwise
%% yaws_rss:open/2 blocks for up to 10 seconds then crashes.
%%
%% The fix moves yaws_sup_restarts (which starts yaws_rss) before
%% yaws_server in yaws_sup's child spec list.
-module(rss_startup_SUITE).

-include("testsuite.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [
     config_load_blocks_without_rss,
     config_load_completes_with_rss,
     sup_starts_rss_before_server
    ].

groups() ->
    [].

%%====================================================================
init_per_suite(Config) ->
    _ = application:load(yaws),
    Dir = ?tempdir(?MODULE),
    ok = filelib:ensure_dir(filename:join([Dir, "www", "dummy"])),
    ok = filelib:ensure_dir(filename:join([Dir, "rss", "dummy"])),
    ConfPath = filename:join(Dir, "yaws.conf"),
    [{conf_path, ConfPath} | Config].

end_per_suite(_Config) ->
    _ = application:unload(yaws),
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Test, Config) ->
    Config.

end_per_testcase(_Test, _Config) ->
    %% Clean up any processes we may have started.
    catch gen_server:stop(yaws_rss),
    ok.

%%====================================================================

%% When yaws_rss is not running, yaws_config:load blocks at the </rss>
%% tag because yaws_rss:open calls wait_for_server, which polls 20
%% times at 500ms intervals. This reproduces the deadlock through the
%% real code path: yaws_server:init -> yaws_config:load -> yaws_rss:open.
config_load_blocks_without_rss(Config) ->
    ConfPath = ?config(conf_path, Config),
    Env = #env{debug = false, conf = {file, ConfPath}},

    %% Ensure yaws_rss is not running.
    undefined = erlang:whereis(yaws_rss),

    Self = self(),
    Pid = spawn_link(fun() ->
        Result = yaws_config:load(Env),
        Self ! {config_load_result, Result}
    end),

    %% Give it 1.5s. If yaws_rss were registered, load would return
    %% near-instantly. The wait_for_server loop runs at 500ms intervals,
    %% so after 1.5s it should still be polling.
    receive
        {config_load_result, _} ->
            ct:fail("yaws_config:load returned without yaws_rss running")
    after 1500 ->
        %% Still blocked — confirms the deadlock. Kill the helper to
        %% avoid waiting the full 10 seconds.
        unlink(Pid),
        exit(Pid, kill),
        ok
    end.

%% When yaws_rss is running, yaws_config:load gets past the RSS
%% section without blocking. This is the fixed scenario:
%% yaws_sup_restarts starts yaws_rss before yaws_server calls
%% yaws_config:load.
config_load_completes_with_rss(Config) ->
    ConfPath = ?config(conf_path, Config),
    Env = #env{debug = false, conf = {file, ConfPath}},

    {ok, _} = yaws_rss:start_link(),

    Self = self(),
    Pid = spawn_link(fun() ->
        Result = yaws_config:load(Env),
        Self ! {config_load_result, Result}
    end),

    %% Config loading should get past yaws_rss:open within 3s. The
    %% result (ok or error) doesn't matter — the point is it returned
    %% instead of blocking in wait_for_server.
    receive
        {config_load_result, _} ->
            ok
    after 3000 ->
        unlink(Pid),
        exit(Pid, kill),
        ct:fail("yaws_config:load blocked with yaws_rss running")
    end.

%% Verify that yaws_sup's child spec list starts yaws_sup_restarts
%% (which contains yaws_rss) before yaws_server. This is the fix.
sup_starts_rss_before_server(_Config) ->
    ChildSpecs = yaws_sup:child_specs(),
    Ids = [element(1, Spec) || Spec <- ChildSpecs],

    RestartsPos = index_of(yaws_sup_restarts, Ids),
    ServerPos   = index_of(yaws_server, Ids),

    ?assertNotEqual(false, RestartsPos),
    ?assertNotEqual(false, ServerPos),

    %% yaws_sup_restarts must start before yaws_server.
    ?assert(RestartsPos < ServerPos).

%%====================================================================
%% Helpers
%%====================================================================

index_of(Elem, List) ->
    index_of(Elem, List, 1).

index_of(_Elem, [], _N) ->
    false;
index_of(Elem, [Elem | _], N) ->
    N;
index_of(Elem, [_ | T], N) ->
    index_of(Elem, T, N + 1).
