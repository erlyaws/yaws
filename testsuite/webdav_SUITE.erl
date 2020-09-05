-module(webdav_SUITE).

-include("testsuite.hrl").
-include_lib("kernel/include/file.hrl").

-compile(export_all).

all() ->
    [
     webdav,
     reject_entity
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

%% This test ensure Yaws avoids processing external entities, which
%% guards against XXE attacks.
reject_entity(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/"),
    TstFile = tst_file(),
    %% TstFile is added in an external entity in an attempt to fetch its
    %% contents. The request should be rejected.
    Body = ["<?xml version='1.0' encoding='utf-8' ?>",
            "<!DOCTYPE r [",
            "<!ELEMENT r ANY >",
            "<!ENTITY sp SYSTEM 'file://" ++ TstFile ++ "'>",
            "]>",
            "<d:lockinfo xmlns:d='DAV:'>",
            "<d:lockscope><d:exclusive/></d:lockscope>",
            "<d:locktype><d:write/></d:locktype>",
            "<d:owner>",
            "<d:href><r>&sp;</r></d:href>",
            "</d:owner>",
            "</d:lockinfo>"],
    ?assertMatch({ok, {{_,400,_}, _, _}}, testsuite:http_req(lock, Url, [], Body)),
    ok.

%%====================================================================
prepare_docroots() ->
    WWW  = filename:join(?tempdir(?MODULE), "www"),
    TstFile = tst_file(),
    ok = testsuite:create_dir(WWW),
    ok = file:write_file(filename:join(WWW, "test"), <<"Hello World test!">>, [write]),
    ok = file:write_file(TstFile, <<"Hello World tst-file!">>, [write]),
    ok.

tst_file() ->
    filename:join(?tempdir(?MODULE), "tst-file").
