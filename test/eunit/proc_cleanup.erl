-module(proc_cleanup).
-author('kruber@zib.de').
-behaviour(supervisor).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

%% Explicitly export supervisor's callback because of a "bug" in R15/16
-export([init/1]).

-include("yaws.hrl").
-include("tftest.hrl").

proc_cleanup_test() ->
    Old = get_processes(),
    process_flag(trap_exit, true),
    P = start_link(),
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
    case lists:keymember(yaws, 1, application:which_applications()) of
        true  -> ok = application:unload(yaws);
        false -> ok
    end,
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
