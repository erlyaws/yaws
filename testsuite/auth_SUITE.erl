-module(auth_SUITE).

-include("testsuite.hrl").

-compile(export_all).

all() ->
    [
     basic_auth,
     basic_auth_with_docroot,
     basic_auth_subdirs,
     auth_with_authmod,
     auth_with_authmod_and_out401,
     auth_ip_deny_all_but_allow_loopback,
     auth_ip_allow_loopback_but_deny_all,
     auth_ip_allow_loopback_and_deny_nothing,
     auth_ip_nomatch_allow_deny,
     auth_ip_nomatch_deny_allow,
     auth_ip_and_basic_auth_or_auth_mod,
     yaws_auth_hidden_file,
     auth_on_redirect
    ].

group() ->
    [
    ].

%%====================================================================
init_per_suite(Config) ->
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
basic_auth(Config) ->
    Port  = testsuite:get_yaws_port(1, Config),
    Url   = testsuite:make_url(http, "127.0.0.1", Port, "/test1/a.txt"),
    Auth1 = auth_header("foo", "baz"),
    Auth2 = auth_header("foo", "bar"),

    {ok, {StatusLine, Hdrs, _}} = testsuite:http_get(Url),
    ?assertMatch({_, 401, _}, StatusLine),
    ?assertEqual("Basic realm=\"test1\"", proplists:get_value("www-authenticate", Hdrs)),

    ?assertMatch({ok, {{_,401,_}, _, _}}, testsuite:http_get(Url, [Auth1])),

    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_get(Url, [Auth2])),
    ok.

basic_auth_with_docroot(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url1 = testsuite:make_url(http, "127.0.0.1", Port, "/test2/a.txt"),
    Url2 = testsuite:make_url(http, "127.0.0.1", Port, "/test2/b.txt"),

    {ok, {StatusLine, Hdrs, _}} = testsuite:http_get(Url1),
    ?assertMatch({_, 401, _}, StatusLine),
    ?assertEqual("Basic realm=\"test2\"", proplists:get_value("www-authenticate", Hdrs)),

    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_get(Url2)),
    ok.

basic_auth_subdirs(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/test3/sub/a.txt"),
    Auth = auth_header("foo", "bar"),

    {ok, {StatusLine, Hdrs, _}} = testsuite:http_get(Url),
    ?assertMatch({_, 401, _}, StatusLine),
    ?assertEqual("Basic realm=\"test3\"", proplists:get_value("www-authenticate", Hdrs)),

    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_get(Url, [Auth])),
    ok.

auth_with_authmod(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/test4/a.txt"),
    Auth = auth_header("foo", "bar"),

    {ok, {StatusLine, Hdrs, _}} = testsuite:http_get(Url),
    ?assertMatch({_, 401, _}, StatusLine),
    ?assertEqual("Basic realm=\"test4\"", proplists:get_value("www-authenticate", Hdrs)),

    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_get(Url, [Auth])),
    ok.

auth_with_authmod_and_out401(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/test5/a.txt"),

    {ok, {StatusLine, Hdrs, _}} = testsuite:http_get(Url),
    ?assertMatch({_, 200, _}, StatusLine),
    ?assertEqual("Basic realm=\"test5\"", proplists:get_value("www-authenticate", Hdrs)),
    ?assertEqual("true", proplists:get_value("x-outmod-test", Hdrs)),
    ok.

auth_ip_deny_all_but_allow_loopback(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/test6/a.txt"),

    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_get(Url)),
    ok.

auth_ip_allow_loopback_but_deny_all(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/test7/a.txt"),

    ?assertMatch({ok, {{_,403,_}, _, _}}, testsuite:http_get(Url)),
    ok.

auth_ip_allow_loopback_and_deny_nothing(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/test7/b.txt"),

    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_get(Url)),
    ok.

auth_ip_nomatch_allow_deny(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/test8/a.txt"),

    ?assertMatch({ok, {{_,403,_}, _, _}}, testsuite:http_get(Url)),
    ok.

auth_ip_nomatch_deny_allow(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/test8/b.txt"),

    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_get(Url)),
    ok.

auth_ip_and_basic_auth_or_auth_mod(Config) ->
    Port  = testsuite:get_yaws_port(1, Config),
    Url   = testsuite:make_url(http, "127.0.0.1", Port, "/test9/a.txt"),
    Auth1 = auth_header("foo", "bar"),
    Auth2 = auth_header("foo", "baz"),

    {ok, {StatusLine, Hdrs, _}} = testsuite:http_get(Url),
    ?assertMatch({_, 401, _}, StatusLine),
    ?assertEqual("Basic realm=\"test9\"", proplists:get_value("www-authenticate", Hdrs)),

    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_get(Url, [Auth1])),

    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_get(Url, [Auth2])),
    ok.

yaws_auth_hidden_file(Config) ->
    Port  = testsuite:get_yaws_port(1, Config),
    Url1  = testsuite:make_url(http, "127.0.0.1", Port, "/test10/a.txt"),
    Url2  = testsuite:make_url(http, "127.0.0.1", Port, "/test10/b.txt"),
    Url3 = testsuite:make_url(http, "127.0.0.1", Port, "/test10/c.txt"),
    Auth1 = auth_header("foo", "bar"),
    Auth2 = auth_header("foo", "bar"),

    {ok, {StatusLine1, Hdrs1, _}} = testsuite:http_get(Url1),
    ?assertMatch({_, 401, _}, StatusLine1),
    ?assertEqual("Basic realm=\"test10\"", proplists:get_value("www-authenticate", Hdrs1)),

    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_get(Url1, [Auth1])),

    {ok, {StatusLine2, Hdrs2, _}} = testsuite:http_get(Url2),
    ?assertMatch({_, 401, _}, StatusLine2),
    ?assertEqual("Basic realm=\"test10\"", proplists:get_value("www-authenticate", Hdrs2)),

    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_get(Url2, [Auth2])),

    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_get(Url3)),
    ok.

auth_on_redirect(Config) ->
    Port    = testsuite:get_yaws_port(1, Config),
    Url1    = testsuite:make_url(http, "127.0.0.1", Port, "/test11_1/a.txt"),
    Url2    = testsuite:make_url(http, "127.0.0.1", Port, "/test11_2/a.txt"),
    Url3    = testsuite:make_url(http, "127.0.0.1", Port, "/test11_3/a.txt"),
    Url4_1  = testsuite:make_url(http, "127.0.0.1", Port, "/test11_4/a.txt"),
    Url4_2  = testsuite:make_url(http, "127.0.0.1", Port, "/test11_4/b.txt"),
    Auth1   = auth_header("foo", "bar"),
    Auth2   = auth_header("foo", "bar"),
    Auth3_2 = auth_header("foo", "bar"),
    Auth3_3 = auth_header("foo", "baz"),
    Auth4_2 = auth_header("foo", "bar"),

    {ok, {StatusLine1, Hdrs1, _}} = testsuite:http_get(Url1),
    ?assertMatch({_, 401, _}, StatusLine1),
    ?assertEqual("Basic realm=\"test1\"", proplists:get_value("www-authenticate", Hdrs1)),

    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_get(Url1, [Auth1])),

    {ok, {StatusLine2, Hdrs2, _}} = testsuite:http_get(Url2),
    ?assertMatch({_, 401, _}, StatusLine2),
    ?assertEqual("Basic realm=\"test11_2\"", proplists:get_value("www-authenticate", Hdrs2)),

    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_get(Url2, [Auth2])),


    {ok, {StatusLine3_1, Hdrs3_1, _}} = testsuite:http_get(Url3),
    ?assertMatch({_, 401, _}, StatusLine3_1),
    ?assertEqual("Basic realm=\"test11_3\"", proplists:get_value("www-authenticate", Hdrs3_1)),

    {ok, {StatusLine3_2, Hdrs3_2, _}} = testsuite:http_get(Url3, [Auth3_2]),
    ?assertMatch({_, 401, _}, StatusLine3_2),
    ?assertEqual("Basic realm=\"test11_3\"", proplists:get_value("www-authenticate", Hdrs3_2)),

    {ok, {StatusLine3_3, Hdrs3_3, _}} = testsuite:http_get(Url3, [Auth3_3]),
    ?assertMatch({_, 401, _}, StatusLine3_3),
    ?assertEqual("Basic realm=\"test1\"", proplists:get_value("www-authenticate", Hdrs3_3)),

    ?assertMatch({ok, {{_,403,_}, _, _}}, testsuite:http_get(Url4_1)),

    {ok, {StatusLine4_2, Hdrs4_2, _}} = testsuite:http_get(Url4_2),
    ?assertMatch({_, 401, _}, StatusLine4_2),
    ?assertEqual("Basic realm=\"test11_4\"", proplists:get_value("www-authenticate", Hdrs4_2)),

    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_get(Url4_2, [Auth4_2])),
    ok.

%%====================================================================
auth_header(User, Pass) ->
    Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
    {"Authorization","Basic " ++ Encoded}.
