%% coding: utf-8
-module(copy_error_log).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("yaws_api.hrl").
-include("tftest.hrl").

%% ignore it
error_log_handler_test() ->
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
