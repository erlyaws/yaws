-module(proc_cleanup).
-author('kruber@zib.de').
-behaviour(supervisor).
-compile(export_all).
-include("../../include/yaws.hrl").
-include_lib("eunit/include/eunit.hrl").

proc_cleanup_test() ->
    Old = get_processes(),
    process_flag(trap_exit, true),
    {ok,P} = start_link(),
    erlang:exit(P, kill),
    timer:sleep(500),
    ?assertEqual([], [Proc || Proc <- get_processes(), not lists:member(Proc, Old)]),
    ok.

start_link() ->
    Id = "yaws",
    Docroot = ".",
    GconfList = [{id, Id}],
    SconfList = [{docroot, Docroot}],

    %% load yaws application (required by yaws_api:embedded_start_conf)
    ok = application:unload(yaws),
    ok = application:load(
           {application, yaws,
            [{description, "yaws WWW server"},
             {vsn, "1.98"},
             {modules, []},
             {registered, []},
             {mod, {yaws_app, []}},
             {env, []},
             {applications, [kernel, stdlib]}]}),

    {ok, SCList, GC, ChildSpecs} = yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),

    X = supervisor:start_link(?MODULE, ChildSpecs),

    %% now configure Yaws
    ok = yaws_api:setconf(GC, SCList),
    X.

init(ChildSpecs) ->
    {ok, {{one_for_all, 10, 1}, ChildSpecs}}.

get_processes() ->
    [begin
         InitCall =
             case proc_lib:initial_call(X) of
                 false -> element(2, lists:keyfind(initial_call, 1, Data));
                 {Module, Function, Args} -> {Module, Function, length(Args)}
             end,
         CurFun = element(2, lists:keyfind(current_function, 1, Data)),
         {X, InitCall, CurFun}
     end
     || X <- processes(),
        Data <- [process_info(X, [current_function, initial_call, registered_name])],
        Data =/= undefined].
