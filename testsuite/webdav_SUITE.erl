-module(webdav_SUITE).

-include("testsuite.hrl").
-include_lib("kernel/include/file.hrl").

-compile(export_all).

all() ->
    [
     webdav
    ].

group() ->
    [
    ].

%%====================================================================
init_per_suite(Config) ->
    ok = prepare_docroots(),

    Id    = "testsuite-server",
    YConf = filename:join(?tempdir(?MODULE), "yaws.conf"),
    application:load(yaws),
    application:set_env(yaws, id,   Id),
    application:set_env(yaws, conf, YConf),
    ok = yaws:start(),
    [{yaws_id, Id}, {yaws_config, YConf} | Config].

end_per_suite(_Config) ->
    ok = application:stop(yaws),
    ok = application:unload(yaws),
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Test, Config) ->
    case os:find_executable("cadaver") of
        false -> {skip, "cadaver not found"};
        _ -> Config
    end.

end_per_testcase(_Test, _Config) ->
    ok.

%%====================================================================
webdav(_Config) ->
    Script = filename:join(?tempdir(?MODULE), "davtest"),
    ?assertCmdStatus(0, "sh "++Script),
    ok.

%%====================================================================
prepare_docroots() ->
    WWW  = filename:join(?tempdir(?MODULE), "www"),
    File = filename:join(?tempdir(?MODULE), "tst-file"),
    ok = testsuite:create_dir(WWW),
    ok = file:write_file(filename:join(WWW, "test"), <<"Hello World test!">>, [write]),
    ok = file:write_file(File, <<"Hello World tst-file!">>, [write]),
    ok.
