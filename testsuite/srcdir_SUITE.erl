-module(srcdir_SUITE).

-include("testsuite.hrl").
-include_lib("kernel/include/file.hrl").

-compile(export_all).

all() ->
    [
     srcdir_v1,
     srcdir_v2,
     srcdir_invalid
    ].

groups() ->
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
    Config.

end_per_testcase(_Test, _Config) ->
    ok.

%%====================================================================
srcdir_v1(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url = testsuite:make_url(http, "127.0.0.1", Port, "/"),
    Res = <<"1.0">>,

    ?assertMatch({ok, {{_,200,_}, _, Res}}, testsuite:http_get(Url)),
    ok.

srcdir_v2(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url = testsuite:make_url(http, "127.0.0.1", Port, "/"),
    Res = <<"2.0">>,

    ?assertEqual(ok, set_srcdir_version(<<"2.0">>)),
    ?assertEqual({yaws_hupped, ok}, yaws:hup()),

    ?assertMatch({ok, {{_,200,_}, _, Res}}, testsuite:http_get(Url)),
    ok.

srcdir_invalid(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url = testsuite:make_url(http, "127.0.0.1", Port, "/"),
    Res = <<"2.0">>,

    ?assertEqual(ok, set_srcdir_version(undefined)),
    ?assertEqual({yaws_hupped, ok}, yaws:hup()),

    ?assertMatch({ok, {{_,200,_}, _, Res}}, testsuite:http_get(Url)),
    ok.

%%====================================================================
prepare_docroots() ->
    INC = filename:join(?tempdir(?MODULE), "include"),
    ok = testsuite:create_dir(INC),
    ok = set_srcdir_version(<<"1.0">>),
    ok.

set_srcdir_version(undefined) ->
    INC = filename:join(?tempdir(?MODULE), "include"),
    Bin = <<"invalid_version">>,
    file:write_file(filename:join(INC, "srcdir_test.hrl"), Bin, [write]);
set_srcdir_version(Vsn) ->
    INC = filename:join(?tempdir(?MODULE), "include"),
    Bin = <<"-define(SRCDIR_VERSION, \"", Vsn/binary, "\").">>,
    file:write_file(filename:join(INC, "srcdir_test.hrl"), Bin, [write]).
