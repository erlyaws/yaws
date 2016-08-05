-module(copy_error_log_SUITE).

-include("testsuite.hrl").

-compile(export_all).

all() ->
    [
     error_log_handler
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
error_log_handler(_Config) ->
    ?assertMatch({ok, _}, yaws_log:start_link()),
    GConf = yaws:create_gconf([], default),
    SConf = yaws:create_sconf(".", []),
    ?assertEqual(ok, yaws_log:setup(GConf, [SConf])),

    Handlers = gen_event:which_handlers(error_logger),
    ?assert(lists:member(yaws_log_file_h, Handlers)),

    Dir = yaws:gconf_logdir(GConf),
    File = filename:join([Dir, "report.log"]),
    ?assert(filelib:is_regular(File)),
    ok.
