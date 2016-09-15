-module(redirect_SUITE).

-include("testsuite.hrl").

-compile(export_all).

all() ->
    [
     redirect_default,
     redirect_301,
     redirect_404,
     redirect_url_encode,
     redirect_querystring,
     redirect_post,
     bad_redirect
    ].

groups() ->
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
redirect_default(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url1 = testsuite:make_url(http, "127.0.0.1", Port, "/default_redirect1/index.html"),
    Url2 = testsuite:make_url(http, "127.0.0.1", Port, "/default_redirect2/index.html"),
    Url3 = testsuite:make_url(http, "127.0.0.1", Port, "/default_redirect3/index.html"),
    Url4 = testsuite:make_url(http, "127.0.0.1", Port, "/default_redirect4/index.html"),

    %% /default_redirect1 -> /redir (relative-url + append)
    {ok, {{_,302,_}, Hdrs1, _}} = testsuite:http_get(Url1),
    ?assertEqual(testsuite:make_url(http, "127.0.0.1", Port, "/redir/default_redirect1/index.html"),
                 proplists:get_value("location", Hdrs1)),

    %% /default_redirect2 -> /redir (relative-url + noappend)
    {ok, {{_,302,_}, Hdrs2, _}} = testsuite:http_get(Url2),
    ?assertEqual(testsuite:make_url(http, "127.0.0.1", Port, "/redir"),
                 proplists:get_value("location", Hdrs2)),

    %% /default_redirect3 ->  http://yaws.hyber.org (absolute-url + append)
    {ok, {{_,302,_}, Hdrs3, _}} = testsuite:http_get(Url3),
    ?assertEqual("http://yaws.hyber.org/default_redirect3/index.html",
                 proplists:get_value("location", Hdrs3)),

    %% /default_redirect4 -> http://yaws.hyber.org (absolute-url + noappend)
    {ok, {{_,302,_}, Hdrs4, _}} = testsuite:http_get(Url4),
    ?assertEqual("http://yaws.hyber.org/",
                 proplists:get_value("location", Hdrs4)),
    ok.

redirect_301(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url1 = testsuite:make_url(http, "127.0.0.1", Port, "/301_redirect1/index.html"),
    Url2 = testsuite:make_url(http, "127.0.0.1", Port, "/301_redirect2/index.html"),
    Url3 = testsuite:make_url(http, "127.0.0.1", Port, "/301_redirect3/index.html"),
    Url4 = testsuite:make_url(http, "127.0.0.1", Port, "/301_redirect4/index.html"),

    %% /301_redirect1 -> /redir (relative-url + append)
    {ok, {{_,301,_}, Hdrs1, _}} = testsuite:http_get(Url1),
    ?assertEqual(testsuite:make_url(http, "127.0.0.1", Port, "/redir/301_redirect1/index.html"),
                 proplists:get_value("location", Hdrs1)),

    %% /301_redirect2 -> /redir (relative-url + noappend)
    {ok, {{_,301,_}, Hdrs2, _}} = testsuite:http_get(Url2),
    ?assertEqual(testsuite:make_url(http, "127.0.0.1", Port, "/redir"),
                 proplists:get_value("location", Hdrs2)),

    %% /301_redirect3 ->  http://yaws.hyber.org (absolute-url + append)
    {ok, {{_,301,_}, Hdrs3, _}} = testsuite:http_get(Url3),
    ?assertEqual("http://yaws.hyber.org/301_redirect3/index.html",
                 proplists:get_value("location", Hdrs3)),

    %% /301_redirect4 -> http://yaws.hyber.org (absolute-url + noappend)
    {ok, {{_,301,_}, Hdrs4, _}} = testsuite:http_get(Url4),
    ?assertEqual("http://yaws.hyber.org/",
                 proplists:get_value("location", Hdrs4)),
    ok.

redirect_404(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url1 = testsuite:make_url(http, "127.0.0.1", Port, "/404_redirect1/index.html"),
    Url2 = testsuite:make_url(http, "127.0.0.1", Port, "/404_redirect2/index.html"),
    Url3 = testsuite:make_url(http, "127.0.0.1", Port, "/404_redirect3/index.html"),
    Url4 = testsuite:make_url(http, "127.0.0.1", Port, "/404_redirect4/index.html"),

    Err404 = iolist_to_binary(
               ["<html><h1>404 ", yaws_api:code_to_phrase(404), "</h1></html>"]
              ),
    Err404_3 = <<"/error404.yaws/404_redirect3/index.html\n">>,
    Err404_4 = <<"/error404.yaws\n">>,

    %% /404_redirect1 -> default content (append)
    ?assertMatch({ok, {{_,404,_}, _, Err404}}, testsuite:http_get(Url1)),

    %% /404_redirect2 -> default content (noappend)
    ?assertMatch({ok, {{_,404,_}, _, Err404}}, testsuite:http_get(Url2)),

    %% /404_redirect3 -> /error404.yaws (append)
    ?assertMatch({ok, {{_,404,_}, _, Err404_3}}, testsuite:http_get(Url3)),

    %% /404_redirect4 -> /error404.yaws (noappend)
    ?assertMatch({ok, {{_,404,_}, _, Err404_4}}, testsuite:http_get(Url4)),
    ok.

redirect_url_encode(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url1 = testsuite:make_url(http, "127.0.0.1", Port, "/redirect%3Furl%3Fencode1/index.html"),
    Url2 = testsuite:make_url(http, "127.0.0.1", Port, "/redirect%3Furl%3Fencode2/index.html"),

    %% /redirect?url?encode1 -> /redir? (append)
    {ok, {{_,302,_}, Hdrs1, _}} = testsuite:http_get(Url1),
    ?assertEqual(testsuite:make_url(http, "127.0.0.1", Port, "/redir%3F/redirect%3Furl%3Fencode1/index.html"),
                 proplists:get_value("location", Hdrs1)),

    %% /redirect?url?encode2 -> /redir? (noappend)
    {ok, {{_,302,_}, Hdrs2, _}} = testsuite:http_get(Url2),
    ?assertEqual(testsuite:make_url(http, "127.0.0.1", Port, "/redir%3F"),
                 proplists:get_value("location", Hdrs2)),
    ok.

redirect_querystring(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url1 = testsuite:make_url(http, "127.0.0.1", Port, "/default_redirect1/index.html?a=b&c=d"),
    Url2 = testsuite:make_url(http, "127.0.0.1", Port, "/default_redirect2/index.html?a=b&c=d"),
    Url3 = testsuite:make_url(http, "127.0.0.1", Port, "/default_redirect3/index.html?a=b&c=d"),
    Url4 = testsuite:make_url(http, "127.0.0.1", Port, "/default_redirect4/index.html?a=b&c=d"),
    Url5 = testsuite:make_url(http, "127.0.0.1", Port, "/404_redirect3/index.html?a=b&c=d"),
    Url6 = testsuite:make_url(http, "127.0.0.1", Port, "/404_redirect4/index.html?a=b&c=d"),

    Err404_5 = <<"/error404.yaws/404_redirect3/index.html?a=b&c=d\n">>,
    Err404_6 = <<"/error404.yaws?a=b&c=d\n">>,


    %% /default_redirect1 -> /redir (relative-url + append)
    {ok, {{_,302,_}, Hdrs1, _}} = testsuite:http_get(Url1),
    ?assertEqual(testsuite:make_url(http, "127.0.0.1", Port, "/redir/default_redirect1/index.html?a=b&c=d"),
                 proplists:get_value("location", Hdrs1)),

    %% /default_redirect2 -> /redir (relative-url + noappend)
    {ok, {{_,302,_}, Hdrs2, _}} = testsuite:http_get(Url2),
    ?assertEqual(testsuite:make_url(http, "127.0.0.1", Port, "/redir?a=b&c=d"),
                 proplists:get_value("location", Hdrs2)),

    %% /default_redirect3 ->  http://yaws.hyber.org (absolute-url + append)
    {ok, {{_,302,_}, Hdrs3, _}} = testsuite:http_get(Url3),
    ?assertEqual("http://yaws.hyber.org/default_redirect3/index.html?a=b&c=d",
                 proplists:get_value("location", Hdrs3)),

    %% /default_redirect4 -> http://yaws.hyber.org (absolute-url + noappend)
    {ok, {{_,302,_}, Hdrs4, _}} = testsuite:http_get(Url4),
    ?assertEqual("http://yaws.hyber.org/?a=b&c=d",
                 proplists:get_value("location", Hdrs4)),

    %% /404_redirect3 -> /error404.yaws (append)
    ?assertMatch({ok, {{_,404,_}, _, Err404_5}}, testsuite:http_get(Url5)),

    %% /404_redirect4 -> /error404.yaws (noappend)
    ?assertMatch({ok, {{_,404,_}, _, Err404_6}}, testsuite:http_get(Url6)),
    ok.

redirect_post(Config) ->
    Port  = testsuite:get_yaws_port(1, Config),
    Path1 = "/test",
    Path2 = "/test/",
    Path3 = "/post_redirect1",
    Path4 = "/test/index.yaws",
    Path5 = "/post_redirect2",

    %% (partial_post_size = 5 / content-length = 11)
    CT   = "application/x-www-form-urlencoded",
    Body = <<"foo=1&bar=2">>,
    Res  = <<Body/binary, $\n>>,

    {ok, Sock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),

    %% Data must be flushed on redirect (here, trailing slash is added)
    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {post, Path1, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)}],
                   {CT, Body}
                  )),
    {ok, {{_,302,_}, Hdrs1, _}} = testsuite:receive_http_response(Sock),
    ?assert(lists:suffix(Path2, proplists:get_value("location", Hdrs1))),

    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {get, Path2, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)}]
                  )),
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:receive_http_response(Sock)),

    %% Data must be flushed on redirect (here, on configured redirect)
    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {post, Path3, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)}],
                   {CT, Body}
                  )),
    {ok, {{_,302,_}, Hdrs3, _}} = testsuite:receive_http_response(Sock),
    ?assert(lists:suffix(Path4, proplists:get_value("location", Hdrs3))),

    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {get, Path4, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)}]
                  )),
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:receive_http_response(Sock)),

    %% Data must be readable by /test/index.yaws
    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {post, Path5, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)}],
                   {CT, Body}
                  )),
    ?assertMatch({ok, {{_,200,_}, _, Res}}, testsuite:receive_http_response(Sock)),

    ?assertEqual(ok, gen_tcp:close(Sock)),
    ok.

bad_redirect(_Config) ->
    File1 = filename:join(?data_srcdir(?MODULE), "bad_redirect1.conf"),
    File2 = filename:join(?data_srcdir(?MODULE), "bad_redirect2.conf"),
    File3 = filename:join(?data_srcdir(?MODULE), "bad_redirect3.conf"),
    File4 = filename:join(?data_srcdir(?MODULE), "bad_redirect4.conf"),
    Env1  = #env{debug=false, conf={file, File1}},
    Env2  = #env{debug=false, conf={file, File2}},
    Env3  = #env{debug=false, conf={file, File3}},
    Env4  = #env{debug=false, conf={file, File4}},

    ?assertMatch({error, _}, yaws_config:load(Env1)),
    ?assertMatch({error, _}, yaws_config:load(Env2)),
    ?assertMatch({error, _}, yaws_config:load(Env3)),
    ?assertMatch({error, _}, yaws_config:load(Env4)),
    ok.
