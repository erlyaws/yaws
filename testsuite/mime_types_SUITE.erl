-module(mime_types_SUITE).

-include("testsuite.hrl").

-compile(export_all).

all() ->
    [
     generated_module,
     default_type,
     yaws_type,
     erlang_type,
     gzip_with_charset,
     multiple_accept_headers,
     charset_for_404
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
generated_module(Config) ->
    Port1  = testsuite:get_yaws_port(1, Config),
    Port2  = testsuite:get_yaws_port(2, Config),
    Vhost1 = {"localhost:"++integer_to_list(Port1), Port1},
    Vhost2 = {"localhost:"++integer_to_list(Port2), Port2},

    CInfo = mime_types:module_info(compile),
    ?assertEqual(yaws:id_dir(?config(yaws_id, Config)),
                 filename:dirname(proplists:get_value(source, CInfo))),

    ?assertEqual("text/html", mime_types:default_type()),
    ?assertEqual("text/html", mime_types:default_type(global)),
    ?assertEqual("text/html", mime_types:default_type(Vhost1)),
    ?assertEqual("text/plain; charset=UTF-8", mime_types:default_type(Vhost2)),

    ?assertEqual({yaws, "text/html"}, mime_types:t("yaws")),
    ?assertEqual({yaws, "text/html"}, mime_types:t(global,"yaws")),
    ?assertEqual({yaws, "text/html"}, mime_types:t(Vhost1,"yaws")),
    ?assertEqual({yaws, "text/xhtml; charset=ISO-8859-1"} ,mime_types:t(Vhost2,"yaws")),


    ?assertEqual({regular, "text/plain; charset=UTF-8"}, mime_types:t("tst")),
    ?assertEqual({regular, "text/plain; charset=UTF-8"}, mime_types:t(global,"tst")),
    ?assertEqual({regular, "text/plain; charset=UTF-8"}, mime_types:t(Vhost1,"tst")),
    ?assertEqual({regular, "application/x-test; charset=US-ASCII"}, mime_types:t(Vhost2,"tst")),

    ?assertEqual({regular, "text/html"}, mime_types:t("test")),
    ?assertEqual({regular, "text/html"}, mime_types:t(global,"test")),
    ?assertEqual({regular, "text/html"}, mime_types:t(Vhost1,"test")),
    ?assertEqual({regular, "application/x-test; charset=UTF-8"}, mime_types:t(Vhost2,"test")),

    ?assertEqual({php, "text/html"}, mime_types:t("php")),
    ?assertEqual({php, "text/html"}, mime_types:t(global, "php")),
    ?assertEqual({php, "text/html"}, mime_types:t(Vhost1, "php")),
    ?assertEqual({php, "application/x-httpd-php"}, mime_types:t(Vhost2,"php")),
    ?assertEqual({php, "application/x-httpd-php"}, mime_types:t(Vhost2,"PHP")),
    ?assertEqual({regular, "php5", "application/x-httpd-php5"}, mime_types:revt(Vhost2,"5php")),
    ?assertEqual({regular, "PHP5", "application/x-httpd-php5"}, mime_types:revt(Vhost2,"5PHP")),

    ?assertEqual({regular, "text/plain"}, mime_types:t("html")),
    ?assertEqual({regular, "text/plain"}, mime_types:t(global,"html")),
    ?assertEqual({regular, "text/plain"}, mime_types:t(Vhost1,"html")),
    ?assertEqual({regular, "text/plain; charset=UTF-8"}, mime_types:t(Vhost2,"html")),
    ok.

default_type(Config) ->
    Port1 = testsuite:get_yaws_port(1, Config),
    Port2 = testsuite:get_yaws_port(2, Config),
    Url1  = testsuite:make_url(http, "127.0.0.1", Port1, "/news"),
    Url2  = testsuite:make_url(http, "127.0.0.1", Port2, "/news"),

    {ok, {{_,200,_}, Hdrs1, _}} = testsuite:http_get(Url1),
    ?assertEqual("text/html", proplists:get_value("content-type", Hdrs1)),

    {ok, {{_,200,_}, Hdrs2, _}} = testsuite:http_get(Url2),
    ?assertEqual("text/plain; charset=UTF-8", proplists:get_value("content-type", Hdrs2)),
    ok.

yaws_type(Config) ->
    Port1 = testsuite:get_yaws_port(1, Config),
    Port2 = testsuite:get_yaws_port(2, Config),
    Url1  = testsuite:make_url(http, "127.0.0.1", Port1, "/index.yaws"),
    Url2  = testsuite:make_url(http, "127.0.0.1", Port2, "/index.yaws"),

    {ok, {{_,200,_}, Hdrs1, _}} = testsuite:http_get(Url1),
    ?assertEqual("text/html", proplists:get_value("content-type", Hdrs1)),

    {ok, {{_,200,_}, Hdrs2, _}} = testsuite:http_get(Url2),
    ?assertEqual("text/xhtml; charset=ISO-8859-1", proplists:get_value("content-type", Hdrs2)),
    ok.

erlang_type(Config) ->
    Port1 = testsuite:get_yaws_port(1, Config),
    Port2 = testsuite:get_yaws_port(2, Config),
    Url1  = testsuite:make_url(http, "127.0.0.1", Port1, "/code/myappmod.erl"),
    Url2  = testsuite:make_url(http, "127.0.0.1", Port2, "/code/myappmod.erl"),

    {ok, {{_,200,_}, Hdrs1, _}} = testsuite:http_get(Url1),
    ?assertEqual("text/html", proplists:get_value("content-type", Hdrs1)),

    {ok, {{_,200,_}, Hdrs2, _}} = testsuite:http_get(Url2),
    ?assertEqual("text/x-erlang; charset=UTF-8", proplists:get_value("content-type", Hdrs2)),
    ok.

gzip_with_charset(Config) ->
    Port = testsuite:get_yaws_port(2, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/index.yaws"),

    GzHdr = {"Accept-Encoding", "gzip, deflate"},
    {ok, {{_,200,_}, Hdrs, _}} = testsuite:http_get(Url, [GzHdr]),
    ?assertEqual("text/xhtml; charset=ISO-8859-1", proplists:get_value("content-type", Hdrs)),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs)),
    ok.

multiple_accept_headers(Config) ->
    Port1  = testsuite:get_yaws_port(1, Config),
    Url1  = testsuite:make_url(http, "127.0.0.1", Port1, "/multiple_accept_headers.yaws"),

    AcceptHdrs1 = [{"Accept", "text/html"}, {"Accept", "text/plain"}],
    {ok, {{_,200,_}, Hdrs1, _}} = testsuite:http_get(Url1, AcceptHdrs1),
    ?assertEqual("text/html", proplists:get_value("content-type", Hdrs1)),
    ?assertEqual("text/html, text/plain",
                 proplists:get_value("x-test-request-accept", Hdrs1)),

    AcceptHdrs2 = [{"Accept", "text/plain"}, {"Accept", "text/html"}],
    {ok, {{_,200,_}, Hdrs2, _}} = testsuite:http_get(Url1, AcceptHdrs2),
    ?assertEqual("text/plain", proplists:get_value("content-type", Hdrs2)),
    ?assertEqual("text/plain, text/html",
                 proplists:get_value("x-test-request-accept", Hdrs2)),

    AcceptHdrs3 = [{"Accept", "text/html, text/plain"}],
    {ok, {{_,200,_}, Hdrs3, _}} = testsuite:http_get(Url1, AcceptHdrs3),
    ?assertEqual("text/html", proplists:get_value("content-type", Hdrs3)),
    ?assertEqual("text/html, text/plain",
                 proplists:get_value("x-test-request-accept", Hdrs3)),

    AcceptHdrs4 = [{"Accept", "text/plain, text/html"}],
    {ok, {{_,200,_}, Hdrs4, _}} = testsuite:http_get(Url1, AcceptHdrs4),
    ?assertEqual("text/plain", proplists:get_value("content-type", Hdrs4)),
    ?assertEqual("text/plain, text/html",
                 proplists:get_value("x-test-request-accept", Hdrs4)),

    AcceptHdrs5 = [{"Accept", "text/plain, application/json"},
                   {"Accept", "text/html, text/*"}],
    {ok, {{_,200,_}, Hdrs5, _}} = testsuite:http_get(Url1, AcceptHdrs5),
    ?assertEqual("text/plain", proplists:get_value("content-type", Hdrs5)),
    ?assertEqual("text/plain, application/json, text/html, text/*",
                 proplists:get_value("x-test-request-accept", Hdrs5)),

    AcceptHdrs6 = [{"Accept", ",text/plain"}],
    {ok, {{_,200,_}, Hdrs6, _}} = testsuite:http_get(Url1, AcceptHdrs6),
    ?assertEqual("text/plain", proplists:get_value("content-type", Hdrs6)),
    ?assertEqual("text/plain",
                 proplists:get_value("x-test-request-accept", Hdrs6)),

    AcceptHdrs7 = [{"Accept", ",text/plain"}],
    {ok, {{_,200,_}, Hdrs7, _}} = testsuite:http_get(Url1, AcceptHdrs7),
    ?assertEqual("text/plain", proplists:get_value("content-type", Hdrs7)),
    ?assertEqual("text/plain",
                 proplists:get_value("x-test-request-accept", Hdrs7)),

    AcceptHdrs8 = [{"Accept", "text/plain, ,text/html"}],
    {ok, {{_,200,_}, Hdrs8, _}} = testsuite:http_get(Url1, AcceptHdrs8),
    ?assertEqual("text/plain", proplists:get_value("content-type", Hdrs8)),
    ?assertEqual("text/plain, text/html",
                 proplists:get_value("x-test-request-accept", Hdrs8)),

    AcceptHdrs9 = [{"Accept", ","}],
    {ok, {{_,400,_}, _, _}} = testsuite:http_get(Url1, AcceptHdrs9),

    ok.

charset_for_404(Config) ->
    Port1 = testsuite:get_yaws_port(1, Config),
    Port3 = testsuite:get_yaws_port(3, Config),
    Url1  = testsuite:make_url(http, "127.0.0.1", Port1, "/404"),
    Url3  = testsuite:make_url(http, "127.0.0.1", Port3, "/404"),

    {ok, {{_,404,_}, Hdrs1, _}} = testsuite:http_get(Url1),
    ?assertEqual("text/plain", proplists:get_value("content-type", Hdrs1)),

    {ok, {{_,404,_}, Hdrs3, _}} = testsuite:http_get(Url3),
    ?assertEqual("text/html; charset=UTF-8", proplists:get_value("content-type", Hdrs3)),

    ok.
