-module(no_docroot_SUITE).

-include("testsuite.hrl").

-compile(export_all).

all() ->
    [
     no_docroot_revproxy,
     no_docroot_fwdproxy,
     no_docroot_redirect,
     no_docroot_appmod
    ].

groups() ->
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
no_docroot_revproxy(_Config) ->
    File = filename:join(?tempdir(?MODULE), "no_docroot_revproxy.conf"),
    Env = #env{debug = false, conf = {file, File}},
    ?assertMatch({ok, _, _}, yaws_config:load(Env)),
    ok.

no_docroot_fwdproxy(_Config) ->
    File = filename:join(?tempdir(?MODULE), "no_docroot_fwdproxy.conf"),
    Env = #env{debug = false, conf = {file, File}},
    ?assertMatch({ok, _, _}, yaws_config:load(Env)),
    ok.

no_docroot_redirect(_Config) ->
    File = filename:join(?tempdir(?MODULE), "no_docroot_redirect.conf"),
    Env = #env{debug = false, conf = {file, File}},
    ?assertMatch({ok, _, _}, yaws_config:load(Env)),
    ok.

no_docroot_appmod(_Config) ->
    File = filename:join(?tempdir(?MODULE), "no_docroot_appmod.conf"),
    Env = #env{debug = false, conf = {file, File}},
    ?assertMatch({ok, _, _}, yaws_config:load(Env)),
    ok.
