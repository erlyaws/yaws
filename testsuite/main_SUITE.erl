-module(main_SUITE).

-include("testsuite.hrl").
-include_lib("kernel/include/file.hrl").

-compile(export_all).

all() ->
    [
     http_options,
     http_head,
     slow_get,
     appmod,
     dispatchmod,
     chunked_response,
     small_post,
     large_post,
     small_chunked_post,
     large_chunked_post,
     flush_small_post,
     flush_large_post,
     flush_chunked_post,
     flush_small_get,
     flush_large_get,
     flush_chunked_get,
     te_trailer_and_extensions,
     expires,
     reentrant,
     cgi_redirect,
     php_handler,
     arg_rewrite_rewrite,
     arg_rewrite_redirect,
     arg_rewrite_response,
     shaper,
     sslaccept_timeout,
     ssl_multipart_post,
     throw_appmod,
     too_many_headers,
     index_files,
     embedded_id_dir,
     chained_appmods,
     appmod_with_yssi,
     appmod_with_yssi_strip_undefined_bindings,
     appmod_strip_undefined_bindings,
     cache_appmod,
     multi_forwarded_for,
     log_rotation,
     exhtml,
     accept_ranges
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
http_options(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    AllowRes = "GET, HEAD, OPTIONS, PUT, POST, DELETE",

    {ok, Sock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {options, "*", "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)}]
                  )),
    {ok, {{_,200,_}, Hdrs, <<>>}} = testsuite:receive_http_response(Sock),

    ?assertEqual("0", proplists:get_value("content-length", Hdrs)),
    ?assertEqual(AllowRes, proplists:get_value("allow", Hdrs)),
    ?assert(proplists:is_defined("server", Hdrs)),
    ?assert(proplists:is_defined("date", Hdrs)),
    ?assertEqual(ok, gen_tcp:close(Sock)),
    ok.

http_head(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/"),

    ?assertMatch({ok, {{_,200,_}, _, <<>>}}, testsuite:http_req(head, Url)),
    ok.

slow_get(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Parent = self(),
    Pids = lists:map(fun(I) ->
                             spawn(fun() -> slow_client(I, Parent, Port) end)
                     end, lists:seq(1,100)),
    %% max 5 connectors at a time
    ?assertEqual(ok, allow_connects(Pids, 5)),
    ?assertEqual({ok, {100,0}}, collect_pids(Pids)),
    ok.

appmod(Config) ->
    Port1 = testsuite:get_yaws_port(2, Config),
    Port2 = testsuite:get_yaws_port(3, Config),
    Port3 = testsuite:get_yaws_port(1, Config),
    Url1  = testsuite:make_url(http, "127.0.0.1", Port1, "/"),
    Url2  = testsuite:make_url(http, "127.0.0.1", Port2, "/"),
    Url3  = testsuite:make_url(http, "127.0.0.1", Port2, "/icons/layout.gif"),
    Url4  = testsuite:make_url(http, "127.0.0.1", Port3, "/non_root_appmod"),

    {ok, {{_,200,_}, Hdrs1, _}} = testsuite:http_get(Url1),
    ?assertEqual("true", proplists:get_value("appmod-called", Hdrs1)),

    {ok, {{_,200,_}, Hdrs2, _}} = testsuite:http_get(Url2),
    ?assertEqual("true", proplists:get_value("appmod-called", Hdrs2)),

    {ok, {{_,200,_}, Hdrs3, _}} = testsuite:http_get(Url3),
    ?assertEqual(undefined, proplists:get_value("appmod-called", Hdrs3)),

    {ok, {{_,200,_}, Hdrs4, _}} = testsuite:http_get(Url4),
    ?assertEqual("true", proplists:get_value("appmod-called", Hdrs4)),
    ok.

dispatchmod(Config) ->
    Port = testsuite:get_yaws_port(6, Config),
    Url1 = testsuite:make_url(http, "127.0.0.1", Port, "/done"),
    Url2 = testsuite:make_url(http, "127.0.0.1", Port, "/closed"),
    Url3 = testsuite:make_url(http, "127.0.0.1", Port, "/index.yaws"),

    {ok, {{_,204,_}, Hdrs1, _}} = testsuite:http_get(Url1),
    ?assertEqual("true", proplists:get_value("x-dispatchmod", Hdrs1)),

    {ok, {{_,200,_}, Hdrs2, _}} = testsuite:http_get(Url2),
    ?assertEqual("true", proplists:get_value("x-dispatchmod", Hdrs2)),
    ?assertEqual("close", proplists:get_value("connection", Hdrs2)),

    {ok, {{_,200,_}, Hdrs3, _}} = testsuite:http_get(Url3),
    ?assertEqual(undefined, proplists:get_value("x-dispatchmod", Hdrs3)),
    ok.

chunked_response(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url1 = testsuite:make_url(http, "127.0.0.1", Port, "/streamtest/1"),
    Url2 = testsuite:make_url(http, "127.0.0.1", Port, "/streamtest/2"),
    Res1 = <<"this is an iolist">>,
    Res2 = <<"closing the socket">>,

    {ok, {{_,200,_}, Hdrs1, Res1}} = testsuite:http_get(Url1),
    ?assertEqual("chunked", proplists:get_value("transfer-encoding", Hdrs1)),

    ?assertMatch({ok, {{_,200,_}, _, Res2}}, testsuite:http_get(Url2)),
    ok.

small_post(Config) ->
    File = filename:join(?tempdir(?MODULE), "www/1000.txt"),
    {ok, FI}  = file:read_file_info(File),
    Sz = integer_to_list(FI#file_info.size),

    Port = testsuite:get_yaws_port(1, Config),
    Url   = testsuite:make_url(http, "127.0.0.1", Port, "/posttest/"++Sz),
    ClHdr = {"Content-Length", Sz},
    CT    = "binary/octet-stream",
    Body  = {fun testsuite:post_file/1, {File,1024000}},
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_post(Url, [ClHdr], {CT, Body})),
    ok.

large_post(Config) ->
    File = filename:join(?tempdir(?MODULE), "www/10000.txt"),
    {ok, FI}  = file:read_file_info(File),
    Sz = integer_to_list(FI#file_info.size),

    Port  = testsuite:get_yaws_port(1, Config),
    Url   = testsuite:make_url(http, "127.0.0.1", Port, "/posttest/"++Sz),
    ClHdr = {"Content-Length", Sz},
    CT    = "binary/octet-stream",
    Body  = {fun testsuite:post_file/1, {File,FI#file_info.size}},
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_post(Url, [ClHdr], {CT, Body})),
    ok.

small_chunked_post(Config) ->
    File = filename:join(?tempdir(?MODULE), "www/3000.txt"),
    {ok, FI} = file:read_file_info(File),
    Sz = integer_to_list(FI#file_info.size),

    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/posttest/chunked/"++Sz),
    CT   = "binary/octet-stream",
    Body = {chunkify, fun testsuite:post_file/1, {File,1000*1000}},
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_post(Url, {CT, Body})),
    ok.

large_chunked_post(Config) ->
    File = filename:join(?tempdir(?MODULE), "www/10000.txt"),
    {ok, FI} = file:read_file_info(File),
    Sz = integer_to_list(FI#file_info.size),

    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/posttest/chunked/"++Sz),
    CT   = "binary/octet-stream",

    %% size of chunk _IS_NOT_ a multiple of partial_post_size
    Body1 = {chunkify, fun testsuite:post_file/1, {File,4000*1000}},
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_post(Url, {CT, Body1})),

    %% size of chunk _IS_ a multiple of partial_post_size
    Body2 = {chunkify, fun testsuite:post_file/1, {File,4000*1024}},
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_post(Url, {CT, Body2})),
    ok.

flush_small_post(Config) ->
    File = filename:join(?tempdir(?MODULE), "www/1000.txt"),
    {ok, FI} = file:read_file_info(File),
    Sz = integer_to_list(FI#file_info.size),

    Port  = testsuite:get_yaws_port(1, Config),
    Path1 = "/flushtest/"++Sz,
    Path2 = "/hello.txt",
    CT    = "binary/octet-stream",
    Body  = {fun testsuite:post_file/1, {File,FI#file_info.size}},

    {ok, Sock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {post, Path1, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)},
                    {"Content-Length", Sz}],
                   {CT, Body}
                  )),
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:receive_http_response(Sock)),

    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {get, Path2, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)}]
                  )),
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:receive_http_response(Sock)),
    ?assertEqual(ok, gen_tcp:close(Sock)),
    ok.

flush_large_post(Config) ->
    File = filename:join(?tempdir(?MODULE), "www/10000.txt"),
    {ok, FI} = file:read_file_info(File),
    Sz = integer_to_list(FI#file_info.size),

    Port  = testsuite:get_yaws_port(1, Config),
    Path1 = "/flushtest/"++Sz,
    Path2 = "/hello.txt",
    CT    = "binary/octet-stream",
    Body  = {fun testsuite:post_file/1, {File,FI#file_info.size}},

    {ok, Sock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {post, Path1, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)},
                    {"Content-Length", Sz}],
                   {CT, Body}
                  )),
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:receive_http_response(Sock)),

    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {get, Path2, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)}]
                  )),
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:receive_http_response(Sock)),
    ?assertEqual(ok, gen_tcp:close(Sock)),
    ok.

flush_chunked_post(Config) ->
    File = filename:join(?tempdir(?MODULE), "www/10000.txt"),
    {ok, FI} = file:read_file_info(File),
    Sz = integer_to_list(FI#file_info.size),

    Port  = testsuite:get_yaws_port(1, Config),
    Path1 = "/flushtest/chunked"++Sz,
    Path2 = "/hello.txt",
    CT    = "binary/octet-stream",
    Body  = {chunkify, fun testsuite:post_file/1, {File, 4000*1000}},

    {ok, Sock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {post, Path1, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)}],
                   {CT, Body}
                  )),
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:receive_http_response(Sock)),

    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {get, Path2, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)}]
                  )),
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:receive_http_response(Sock)),
    ?assertEqual(ok, gen_tcp:close(Sock)),
    ok.

flush_small_get(Config) ->
    File = filename:join(?tempdir(?MODULE), "www/1000.txt"),
    {ok, FI} = file:read_file_info(File),
    Sz = integer_to_list(FI#file_info.size),

    Port  = testsuite:get_yaws_port(1, Config),
    Path1 = "/flushtest/"++Sz,
    Path2 = "/hello.txt",
    CT    = "binary/octet-stream",
    Body  = {fun testsuite:post_file/1, {File,FI#file_info.size}},

    {ok, Sock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {get, Path1, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)},
                    {"Content-Length", Sz}],
                   {CT, Body}
                  )),
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:receive_http_response(Sock)),

    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {get, Path2, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)}]
                  )),
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:receive_http_response(Sock)),
    ?assertEqual(ok, gen_tcp:close(Sock)),
    ok.

flush_large_get(Config) ->
    File = filename:join(?tempdir(?MODULE), "www/10000.txt"),
    {ok, FI} = file:read_file_info(File),
    Sz = integer_to_list(FI#file_info.size),

    Port  = testsuite:get_yaws_port(1, Config),
    Path1 = "/flushtest/"++Sz,
    Path2 = "/hello.txt",
    CT    = "binary/octet-stream",
    Body  = {fun testsuite:post_file/1, {File,FI#file_info.size}},

    {ok, Sock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {get, Path1, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)},
                    {"Content-Length", Sz}],
                   {CT, Body}
                  )),
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:receive_http_response(Sock)),

    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {get, Path2, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)}]
                  )),
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:receive_http_response(Sock)),
    ?assertEqual(ok, gen_tcp:close(Sock)),
    ok.

flush_chunked_get(Config) ->
    File = filename:join(?tempdir(?MODULE), "www/10000.txt"),
    {ok, FI} = file:read_file_info(File),
    Sz = integer_to_list(FI#file_info.size),

    Port  = testsuite:get_yaws_port(1, Config),
    Path1 = "/flushtest/chunked/"++Sz,
    Path2 = "/hello.txt",
    CT    = "binary/octet-stream",
    Body  = {chunkify, fun testsuite:post_file/1, {File,4000*1000}},

    {ok, Sock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {get, Path1, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)}],
                   {CT, Body}
                  )),
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:receive_http_response(Sock)),

    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {get, Path2, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)}]
                  )),
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:receive_http_response(Sock)),
    ?assertEqual(ok, gen_tcp:close(Sock)),
    ok.

te_trailer_and_extensions(Config) ->
    Data = [<<"This is the data in the first chunk\n">>,
            <<"and this is the second one\n">>,
            <<"con">>, <<"sequence">>],
    Body = {chunks,
            [[[erlang:integer_to_list(iolist_size(X), 16),"; foo=bar\r\n", X,"\r\n"] || X <- Data],
             "0\r\n",
             "Extra-Headers-WooHoo: something\r\n",
             "Content-Type: text/plain\r\n\r\n"]},
    Sz   = integer_to_list(iolist_size(Data)),

    Port  = testsuite:get_yaws_port(1, Config),
    Path  = "/posttest/chunked/"++Sz,

    {ok, Sock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {post, Path, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)},
                    {"Trailer", "Content-Type"},
                    {"Trailer", "Extra-Headers-WooHoo"},
                    {"Transfer-Encoding", "Chunked"}],
                   {"binary/octet-stream", Body}
                  )),
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:receive_http_response(Sock)),
    ?assertEqual(ok, gen_tcp:close(Sock)),
    ok.

expires(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url1 = testsuite:make_url(http, "127.0.0.1", Port, "/hello.txt"),
    Url2 = testsuite:make_url(http, "127.0.0.1", Port, "/"),
    Url3 = testsuite:make_url(http, "127.0.0.1", Port, "/yaws_head.gif"),

    {ok, {{_,200,_}, Hdrs1, _}} = testsuite:http_get(Url1),
    {ok, {{_,200,_}, Hdrs2, _}} = testsuite:http_get(Url2),
    {ok, {{_,200,_}, Hdrs3, _}} = testsuite:http_get(Url3),

    %% Test "text/plain" rule
    %%   - Retrieve max-age value to test Expires header
    CCtrl1 = proplists:get_value("cache-control", Hdrs1),
    ?assertEqual({match, ["2592000"]}, re:run(CCtrl1, "max-age=(\\d+)",
                                              [{capture,all_but_first,list}])),

    %%   - Convert Date and Expires into datetime()
    Date       = proplists:get_value("date",    Hdrs1),
    Expires    = proplists:get_value("expires", Hdrs1),
    Date_DT    = httpd_util:convert_request_date(Date),
    Expires_DT = httpd_util:convert_request_date(Expires),

    %%   - Check if Expires value is equal to "Date + max-age"
    ?assertEqual(calendar:datetime_to_gregorian_seconds(Date_DT) + 2592000,
                 calendar:datetime_to_gregorian_seconds(Expires_DT)),

    %% Test "*/*" rule
    CCtrl2 = proplists:get_value("cache-control", Hdrs2),
    ?assertEqual({match, ["0"]}, re:run(CCtrl2, "max-age=(\\d+)",
                                        [{capture,all_but_first,list}])),

    %% Test "image/*" rule
    CCtrl3 = proplists:get_value("cache-control", Hdrs3),
    ?assertEqual({match, ["2592000"]}, re:run(CCtrl3, "max-age=(\\d+)",
                                              [{capture,all_but_first,list}])),
    ok.

reentrant(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url1 = testsuite:make_url(http, "127.0.0.1", Port, "/reentranttest/status"),
    Url2 = testsuite:make_url(http, "127.0.0.1", Port, "/reentranttest/delayed_headers"),

    ?assertMatch({ok, {{_,201,_}, _, _}}, testsuite:http_get(Url1)),

    {ok, {{_,200,_}, Hdrs, _}} = testsuite:http_get(Url2),
    ?assertEqual("no-cache",   proplists:get_value("cache-control", Hdrs)),
    ?assertEqual("static-tag", proplists:get_value("etag", Hdrs)),
    ?assertEqual("true",       proplists:get_value("x-delayed-header", Hdrs)),

    ok.

cgi_redirect(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url = testsuite:make_url(http, "127.0.0.1", Port, "/cgi-bin/redirect_test.cgi"),

    {ok, {{_,302,_}, Hdrs, _}} = testsuite:http_get(Url),
    ?assert(proplists:is_defined("location", Hdrs)),
    ok.

php_handler(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url = testsuite:make_url(http, "127.0.0.1", Port, "/test.php"),
    {ok, Res} = file:read_file(filename:join(?data_srcdir(?MODULE), "www/test.php")),

    ?assertMatch({ok, {{_,200,_}, _, Res}}, testsuite:http_get(Url)),
    ok.

arg_rewrite_rewrite(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url = testsuite:make_url(http, "127.0.0.1", Port, "/rewrite"),
    {ok, FI} = file:read_file_info(filename:join(?data_srcdir(?MODULE), "www/hello.txt")),
    Etag = yaws:make_etag(FI),

    {ok, {{_,200,_}, Hdrs, _}} = testsuite:http_get(Url),
    ?assertEqual(Etag, proplists:get_value("etag", Hdrs)),
    ok.

arg_rewrite_redirect(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url = testsuite:make_url(http, "127.0.0.1", Port, "/redirect"),

    {ok, {{_,301,_}, Hdrs, _}} = testsuite:http_get(Url),
    ?assertEqual("http://www.yakaz.com", proplists:get_value("location", Hdrs)),
    ok.

arg_rewrite_response(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url = testsuite:make_url(http, "127.0.0.1", Port, "/response"),
    Res = <<"Goodbye, Cruel World!">>,

    {ok, {{_,200,_}, Hdrs, Res}} = testsuite:http_get(Url),
    ?assert(lists:prefix("text/plain", proplists:get_value("content-type", Hdrs))),
    ok.

shaper(Config) ->
    Port = testsuite:get_yaws_port(4, Config),
    {ok, Sock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),

    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {get, "/", "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)}]
                  )),
    {ok, {{_,200,_}, _, _}} = testsuite:receive_http_response(Sock),

    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {get, "/", "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)}]
                  )),
    {ok, {{_,200,_}, _, _}} = testsuite:receive_http_response(Sock),

    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {get, "/", "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)}]
                  )),
    {ok, {{_,200,_}, _, _}} = testsuite:receive_http_response(Sock),

    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {get, "/", "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)}]
                  )),
    {ok, {{_,503,_}, _, _}} = testsuite:receive_http_response(Sock),

    ?assertEqual(ok, gen_tcp:close(Sock)),
    ok.

sslaccept_timeout(Config) ->
    Port = testsuite:get_yaws_port(7, Config),
    {ok, Sock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, true}]),
    ?assertEqual(ok, receive
                         {tcp_closed, Sock} -> ok
                     after
                         %% keepalive_timeout is set to 10 secs. So,
                         %% wait 15 secs before returning an error
                         15000 -> error
                     end),
    ?assertEqual(ok, gen_tcp:close(Sock)),
    ok.

ssl_multipart_post(Config) ->
    File      = filename:join(?tempdir(?MODULE), "www/1000.txt"),
    {ok, Bin} = file:read_file(File),
    Boundary  = "3e9876546ecf",
    Body      = iolist_to_binary(["--", Boundary, "\r\n",
                                  "Content-Disposition: form-data; name=\"file\"; filename=\"1000.txt\"\r\n",
                                  "Content-Type: text/plain\r\n\r\n",
                                  Bin,
                                  "\r\n--", Boundary, "\r\n",
                                  "Content-Disposition: form-data; name=\"submit-name\"\r\n\r\n",
                                  "Larry",
                                  "\r\n--", Boundary, "--\r\n"]),
    CT        = "multipart/form-data; boundary=" ++ Boundary,

    Port    = testsuite:get_yaws_port(7, Config),
    Url     = testsuite:make_url(https, "127.0.0.1", Port, "/test_upload_ssl.yaws"),

    ?assertMatch({ok, {{_,200,_}, _, Body}}, testsuite:http_post(Url, {CT, Body})),
    ok.

throw_appmod(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url = testsuite:make_url(http, "127.0.0.1", Port, "/throw"),
    ?assertMatch({ok, {{_,500,_}, _, _}}, testsuite:http_get(Url)),
    ok.

too_many_headers(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url = testsuite:make_url(http, "127.0.0.1", Port, "/"),
    Hdrs1 = [{"X-Hdrs-"++integer_to_list(I), "true"} || I <- lists:seq(1, 1000)],

    ?assertMatch({ok, {{_,431,_}, _, _}}, testsuite:http_get(Url, Hdrs1)),
    ok.

index_files(Config) ->
    Port   = testsuite:get_yaws_port(5, Config),
    Url1   = testsuite:make_url(http, "127.0.0.1", Port, "/"),
    Url1_1 = testsuite:make_url(http, "127.0.0.1", Port, "/testdir"),
    Url1_2 = testsuite:make_url(http, "127.0.0.1", Port, "/testdir/"),
    Url2   = testsuite:make_url(http, "127.0.0.1", Port, "/?a=1&b=2"),
    Url2_1 = testsuite:make_url(http, "127.0.0.1", Port, "/testdir?a=1&b=2"),
    Url2_2 = testsuite:make_url(http, "127.0.0.1", Port, "/testdir/?a=1&b=2"),

    {ok, Res} = file:read_file(filename:join(?wwwdir, "testdir/index.html")),

    %% "/" should be redirected to "/testdir", then to "/testdir/" and finally
    %% get "/testdir/index.html"
    {ok, {{_,302,_}, Hdrs1_1, _}} = testsuite:http_get(Url1),
    ?assertEqual(Url1_1, proplists:get_value("location", Hdrs1_1)),
    {ok, {{_,302,_}, Hdrs1_2, _}} = testsuite:http_get(Url1_1),
    ?assertEqual(Url1_2, proplists:get_value("location", Hdrs1_2)),
    ?assertMatch({ok, {{_,200,_}, _, Res}}, testsuite:http_get(Url1_2)),

    %% Do the same thing but with a query-string
    {ok, {{_,302,_}, Hdrs2_1, _}} = testsuite:http_get(Url2),
    ?assertMatch(Url2_1, proplists:get_value("location", Hdrs2_1)),
    {ok, {{_,302,_}, Hdrs2_2, _}} = testsuite:http_get(Url2_1),
    ?assertMatch(Url2_2, proplists:get_value("location", Hdrs2_2)),
    ?assertMatch({ok, {{_,200,_}, _, Res}}, testsuite:http_get(Url2_2)),
    ok.

embedded_id_dir(_Config) ->
    Id = "id_dir_test",
    GconfList = [{id, Id},
                 {logdir, filename:join(?tempdir(?MODULE), "logs")},
                 {ebin_dir, [filename:join(?tempdir(?MODULE), "ebin")]}],
    Docroot = ?tempdir(?MODULE),
    SconfList = [{port, 0},
                 {servername, Id},
                 {listen, {127,0,0,1}},
                 {docroot, Docroot}],
    ?assertMatch({ok, _SCList, _GC, _ChildSpecs},
                 yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id)),

    try
        ?assertMatch({ok, _}, file:read_file_info(yaws:id_dir(Id)))
    after
        ?assertEqual(ok, file:del_dir(yaws:id_dir(Id)))
    end,
    ok.

chained_appmods(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/chained"),

    {ok, Res} = file:read_file(filename:join(?data_srcdir(?MODULE), "www/chained.txt")),

    {ok, {{_,200,_}, Hdrs, Res}} = testsuite:http_get(Url),
    ?assertEqual("appmod1[/chained], appmod2[/appmod2], appmod1[/appmod1], appmod3[/chained.txt]",
                 proplists:get_value("x-appmods", Hdrs)),
    ok.

appmod_with_yssi(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/appmod_with_yssi"),

    {ok, {{_,200,_}, Hdrs, _}} = testsuite:http_get(Url),
    ?assertEqual("state=yssi", proplists:get_value("x-yssi", Hdrs)),
    ok.

appmod_with_yssi_strip_undefined_bindings(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/appmod_with_yssi_strip_undefined_bindings"),

    {ok, {{_,200,_}, _, Body}} = testsuite:http_get(Url),
    ?assertEqual(<<"<p></p>\n<p>hello world!</p>\n">>, Body),
    ok.

appmod_strip_undefined_bindings(Config) ->
    Port = testsuite:get_yaws_port(8, Config),
    Url1 = testsuite:make_url(http, "127.0.0.1", Port, "/appmod_strip_undefined_bindings"),
    Url2 = testsuite:make_url(http, "127.0.0.1", Port, "/variables.html"),

    %% verify undefined bindings are stripped
    {ok, {{_,200,_}, _, Body}} = testsuite:http_get(Url1),
    ?assertEqual(<<"<p></p>\n<p>hello world!</p>\n">>, Body),

    %% verify that text in regular pages that happens to look like a
    %% binding is not stripped
    {ok, Res} = file:read_file(filename:join(?data_srcdir(?MODULE), "www/variables.html")),
    {ok, {{_,200,_}, _, Res}} = testsuite:http_get(Url2),
    ok.

cache_appmod(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url1 = testsuite:make_url(http, "127.0.0.1", Port, "/cache_appmod.yaws?no-cache=1"),
    Url2 = testsuite:make_url(http, "127.0.0.1", Port, "/cache_appmod.yaws"),

    %% call cache_appmod_test and disable page cache
    {ok, {{_,200,_}, Hdrs1_1, _}} = testsuite:http_get(Url1),
    ?assertEqual("cache_appmod_test", proplists:get_value("x-appmod", Hdrs1_1)),
    {ok, {{_,200,_}, Hdrs1_2, _}} = testsuite:http_get(Url1),
    ?assertEqual("cache_appmod_test", proplists:get_value("x-appmod", Hdrs1_2)),

    %% check that index.yaws is not cached
    {ok, {{_,200,_}, Hdrs2_1, _}} = testsuite:http_get(Url2),
    ?assertEqual("cache_appmod_test", proplists:get_value("x-appmod", Hdrs2_1)),

    %% retrieve index.yaws from the cache, so cache_appmod_test is not called
    {ok, {{_,200,_}, Hdrs2_2, _}} = testsuite:http_get(Url2),
    ?assertEqual(undefined, proplists:get_value("x-appmod", Hdrs2_2)),
    ok.

multi_forwarded_for(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url = testsuite:make_url(http, "127.0.0.1", Port, "/x_forwarded_for"),
    XFHdrs = [{"X-Forwarded-For", "192.168.1.1"},
            {"X-Forwarded-For", "192.168.1.2"}],

    {ok, {{_,200,_}, Hdrs, _}} = testsuite:http_get(Url, XFHdrs),
    ?assertEqual("192.168.1.1, 192.168.1.2",
                 proplists:get_value("x-result", Hdrs)),
    ok.

log_rotation(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/wrap_log"),
    AccessLog = filename:join(?config(priv_dir, Config), "localhost:"++integer_to_list(Port)++".access"),
    AuthLog = filename:join(?config(priv_dir, Config),   "localhost:"++integer_to_list(Port)++".auth"),

    %% Write 1M of data in .access and .auth log to check the log rotation
    {ok, Fd1} = file:open(AccessLog, [write, append]),
    ?assertEqual(ok, file:write(Fd1, lists:duplicate(1000001, $a))),
    file:close(Fd1),
    file:sync(Fd1),

    {ok, Fd2} = file:open(AuthLog, [write, append]),
    ?assertEqual(ok, file:write(Fd2, lists:duplicate(1000001, $a))),
    file:close(Fd2),
    file:sync(Fd2),

    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_get(Url)),

    timer:sleep(1000),

    ?assertMatch({ok, _}, file:read_file_info(AccessLog++".old")),
    ?assertMatch({ok, _}, file:read_file_info(AuthLog++".old")),
    ?assertMatch({ok, _}, file:read_file_info(AccessLog)),
    ?assertMatch({ok, _}, file:read_file_info(AuthLog)),
    ok.

exhtml(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/exhtml.yaws"),
    Res  = <<"<p id=\"foo\">\n  bar\n</p>\n">>,

    ?assertMatch({ok, {{_,200,_}, _, Res}}, testsuite:http_get(Url)),
    ok.

accept_ranges(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url1  = testsuite:make_url(http, "127.0.0.1", Port, "/accept_ranges1.yaws"),
    {ok, {{_,200,_}, Hdrs1, _}} = testsuite:http_get(Url1),
    ?assertEqual("bytes", proplists:get_value("accept-ranges", Hdrs1)),
    Url2  = testsuite:make_url(http, "127.0.0.1", Port, "/accept_ranges2.yaws"),
    {ok, {{_,200,_}, Hdrs2, _}} = testsuite:http_get(Url2),
    ?assertEqual("bytes", proplists:get_value("accept-ranges", Hdrs2)),
    ok.

%%====================================================================
prepare_docroots() ->
    WWW = filename:join(?tempdir(?MODULE), "www"),
    ok = testsuite:create_dir(WWW),

    Data = lists:duplicate(1024, $0),
    ok = file:write_file(filename:join(WWW, "1000.txt"),  lists:duplicate(1000,  Data), [write]),
    ok = file:write_file(filename:join(WWW, "2000.txt"),  lists:duplicate(2000,  Data), [write]),
    ok = file:write_file(filename:join(WWW, "3000.txt"),  lists:duplicate(3000,  Data), [write]),
    ok = file:write_file(filename:join(WWW, "10000.txt"), lists:duplicate(10000, Data), [write]),

    ok.

allow_connects([], _) ->
    ct:log("all clients connected~n",[]);
allow_connects(Pids, 0) ->
    receive
        {Pid, connected} ->
            allow_connects(lists:delete(Pid, Pids), 1)
    end;
allow_connects(Pids, I) ->
    receive
        {Pid, allow} ->
            Pid ! allow,
            allow_connects(Pids, I-1);
        {Pid, connected} ->
            allow_connects(lists:delete(Pid, Pids), I+1)
    end.

collect_pids(Pids) ->
    collect_pids(Pids, {0, 0}).

collect_pids([], {Ok, Ko}) ->
    ct:log("all clients finished~n",[]),
    {ok, {Ok, Ko}};
collect_pids(Pids, {Ok, Ko}) ->
    receive
        {Pid, done}  -> collect_pids(lists:delete(Pid, Pids), {Ok+1,Ko});
        {Pid, error} -> collect_pids(lists:delete(Pid, Pids), {Ok,Ko+1})
    end.

read_loop(_Sock, _I, Sz) when Sz - 200000 < 0 ->
    ok;
read_loop(Sock, I, Sz)  ->
    {ok, B} = testsuite:receive_http_body(Sock, 0),
    timer:sleep(2),
    read_loop(Sock, I, Sz - size(B)).

slow_client(I, Parent, Port) ->
    try
        Parent ! {self(), allow},
        receive
            allow -> ok
        end,
        {ok, Sock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
        Parent ! {self(), connected},
        ?assertEqual(ok,
                     testsuite:send_http_request(
                       Sock, {get, "/1000.txt", "HTTP/1.1"},
                       [{"Host", "127.0.0.1:"++integer_to_list(Port)}]
                      )),
        {ok, {{_,200,_}, Hdrs}} = testsuite:receive_http_headers(Sock),
        ?assertEqual("1024000", proplists:get_value("content-length", Hdrs)),
        read_loop(Sock, I, 1024000),
        ?assertEqual(ok, gen_tcp:close(Sock)),
        Parent ! {self(), done}
    catch
        _:Error ->
            ct:log(error, "Client ~p failed: ~p~n", [I, Error]),
            Parent ! {self(), error}
    end.
