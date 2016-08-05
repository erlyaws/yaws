-module(proc_cleanup_SUITE).
-behaviour(supervisor).

-include("testsuite.hrl").

-compile(export_all).

%% Explicitly export supervisor's callback because of a "bug" in R15/16
-export([init/1]).


all() ->
    [
     proc_cleanup
    ].


group() ->
    [
    ].

%%====================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Test, Config) ->
    Config.

end_per_testcase(_Test, _Config) ->
    ok.

%%====================================================================
proc_cleanup(_Config) ->
    Old = get_processes(),
    process_flag(trap_exit, true),
    P = start_link(),
    ?assert(is_process_alive(P)),
    erlang:exit(P, kill),
    timer:sleep(500),
    ?assertEqual([], [Pid || Pid <- get_processes(), not lists:member(Pid, Old)]),
    ok.

start_link() ->
    Id = "yaws",
    Docroot = ".",
    GconfList = [{id, Id}],
    SconfList = [{docroot, Docroot}],

    %% load yaws application (required by yaws_api:embedded_start_conf)
    case lists:keymember(yaws, 1, application:which_applications()) of
        true  -> ok = application:unload(yaws);
        false -> ok
    end,
    ok = application:load(yaws),
    {ok, SCList, GC, ChildSpecs} = yaws_api:embedded_start_conf(Docroot,
                                                                SconfList,
                                                                GconfList, Id),

    {ok, Pid} = supervisor:start_link(?MODULE, ChildSpecs),

    %% now configure Yaws
    ok = yaws_api:setconf(GC, SCList),
    Pid.

init(ChildSpecs) ->
    {ok, {{one_for_all, 10, 1}, ChildSpecs}}.

get_processes() ->
    [begin
         InitCall =
             case proc_lib:initial_call(X) of
                 false -> element(2, lists:keyfind(initial_call, 1, Data));
                 {Module, Function, Args} -> {Module, Function, length(Args)}
             end,
         {X, InitCall}
     end
     || X <- processes(),
        Data <- [process_info(X, [initial_call])],
        Data =/= undefined].
