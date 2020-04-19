-module(revproxy_SUITE).

-include("testsuite.hrl").
-include_lib("kernel/include/file.hrl").

-compile(export_all).

all() ->
    [
     deflate_test1,
     deflate_test2,
     small_post,
     large_post,
     small_chunked_post,
     large_chunked_post,
     chunked_response,
     keepalive,
     rewrite_request,
     large_response,
     no_content_length,
     failed_req_interception,
     failed_resp_interception,
     good_interception,
     fwdproxy,
     ipv6_address
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
deflate_test1(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url = testsuite:make_url(http, "127.0.0.1", Port, "/revproxy1/hello.txt"),
    Res = <<"Hello World www2!">>,

    %% client: nodeflate - proxy: deflate - backend: deflate
    %%    ==> result: uncompressed
    ?assertMatch({ok, {{_,200,_}, _, Res}}, testsuite:http_get(Url)),

    %% client: deflate - proxy: deflate - backend: deflate
    %%    ==> result: compressed
    GzHdr = {"Accept-Encoding", "gzip, deflate"},
    {ok, {StatusLine, Hdrs, Body}} = testsuite:http_get(Url, [GzHdr]),
    ?assertMatch({_, 200, _}, StatusLine),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs)),
    ?assertEqual(Res, zlib:gunzip(Body)),
    ok.

deflate_test2(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url = testsuite:make_url(http, "127.0.0.1", Port, "/revproxy2/hello.txt"),
    Res = <<"Hello World www3!">>,

    %% client: nodeflate - proxy: deflate - backend: npdeflate
    %%    ==> result: uncompressed
    ?assertMatch({ok, {{_,200,_}, _, Res}}, testsuite:http_get(Url)),

    %% client: deflate - proxy: deflate - backend: nodeflate
    %%    ==> result: compressed
    GzHdr = {"Accept-Encoding", "gzip, deflate"},
    {ok, {StatusLine, Hdrs, Body}} = testsuite:http_get(Url, [GzHdr]),
    ?assertMatch({_, 200, _}, StatusLine),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs)),
    ?assertEqual(Res, zlib:gunzip(Body)),
    ok.

small_post(Config) ->
    File = filename:join(?tempdir(?MODULE), "www1/1000.txt"),
    {ok, FI}  = file:read_file_info(File),
    Sz = integer_to_list(FI#file_info.size),

    Port  = testsuite:get_yaws_port(1, Config),
    Url   = testsuite:make_url(http, "127.0.0.1", Port, "/revproxy1/posttest/"++Sz),
    ClHdr = {"Content-Length", Sz},
    CT    = "binary/octet-stream",
    Body  = {fun testsuite:post_file/1, {File,1024000}},
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_post(Url, [ClHdr], {CT, Body})),
    ok.

large_post(Config) ->
    File = filename:join(?tempdir(?MODULE), "www1/10000.txt"),
    {ok, FI}  = file:read_file_info(File),
    Sz = integer_to_list(FI#file_info.size),

    Port  = testsuite:get_yaws_port(1, Config),
    Url   = testsuite:make_url(http, "127.0.0.1", Port, "/revproxy1/posttest/"++Sz),
    ClHdr = {"Content-Length", Sz},
    CT    = "binary/octet-stream",
    Body  = {fun testsuite:post_file/1, {File,1024000}},
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_post(Url, [ClHdr], {CT, Body})),
    ok.

small_chunked_post(Config) ->
    File = filename:join(?tempdir(?MODULE), "www1/3000.txt"),
    {ok, FI} = file:read_file_info(File),
    Sz = integer_to_list(FI#file_info.size),

    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/revproxy1/posttest/chunked/"++Sz),
    CT   = "binary/octet-stream",
    Body = {chunkify, fun testsuite:post_file/1, {File,1000*1000}},
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_post(Url, {CT, Body})),
    ok.

large_chunked_post(Config) ->
    File = filename:join(?tempdir(?MODULE), "www1/10000.txt"),
    {ok, FI} = file:read_file_info(File),
    Sz = integer_to_list(FI#file_info.size),

    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/revproxy1/posttest/chunked/"++Sz),
    CT   = "binary/octet-stream",
    Body = {chunkify, fun testsuite:post_file/1, {File,4000*1000}},
    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_post(Url, {CT, Body})),
    ok.

chunked_response(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url = testsuite:make_url(http, "127.0.0.1", Port, "/revproxy1/streamtest"),
    Res = <<"This is the data in the first chunk\n"
            "and this is the second one\n"
            "consequence">>,

    {ok, {StatusLine1, Hdrs1, Body1}} = testsuite:http_get(Url),
    ?assertMatch({_, 200, _}, StatusLine1),
    ?assertEqual("chunked", proplists:get_value("transfer-encoding", Hdrs1)),
    ?assertEqual(Res, Body1),

    GzHdr = {"Accept-Encoding", "gzip, deflate"},
    {ok, {StatusLine2, Hdrs2, Body2}} = testsuite:http_get(Url, [GzHdr]),
    ?assertMatch({_, 200, _}, StatusLine2),
    ?assertEqual("chunked", proplists:get_value("transfer-encoding", Hdrs2)),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs2)),
    ?assertEqual(Res, zlib:gunzip(Body2)),
    ok.

keepalive(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Path1 = "/revproxy1/hello.txt",
    Path2 = "/",
    Path3 = "/revproxy2/hello.txt",
    Res1  = <<"Hello World www2!">>,
    Res2  = <<"Hello World www1!">>,
    Res3  = <<"Hello World www3!">>,

    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),

    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {get, Path1, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)},
                    {"Connection", "Keep-Alive"}]
                  )),
    ?assertMatch({ok, {{_,200,_}, _, Res1}}, testsuite:receive_http_response(Sock)),

    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {get, Path2, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)},
                    {"Connection", "Keep-Alive"}]
                  )),
    ?assertMatch({ok, {{_,200,_}, _, Res2}}, testsuite:receive_http_response(Sock)),

    ?assertEqual(ok,
                 testsuite:send_http_request(
                   Sock, {get, Path3, "HTTP/1.1"},
                   [{"Host", "127.0.0.1:"++integer_to_list(Port)},
                    {"Connection", "Keep-Alive"}]
                  )),
    ?assertMatch({ok, {{_,200,_}, _, Res3}}, testsuite:receive_http_response(Sock)),

    ?assertEqual(ok, gen_tcp:close(Sock)),
    ok.

rewrite_request(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url1 = testsuite:make_url(http, "127.0.0.1", Port, "/rewrite/revproxy1/hello.txt"),
    Url2 = testsuite:make_url(http, "127.0.0.1", Port, "/rewrite/revproxy2/hello.txt"),
    Res1  = <<"Hello World www2!">>,
    Res2  = <<"Hello World www3!">>,

    ?assertMatch({ok, {{_,200,_}, _, Res1}}, testsuite:http_get(Url1)),
    ?assertMatch({ok, {{_,200,_}, _, Res2}}, testsuite:http_get(Url2)),
    ok.

large_response(Config) ->
    Port = testsuite:get_yaws_port(5, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/revproxy/10000.txt"),

    {ok, Res} = file:read_file(filename:join(?tempdir(?MODULE), "www1/10000.txt")),
    {ok, {{_,200,_}, Hdrs, Body}} = testsuite:http_get(Url),
    ?assertEqual("10240000", proplists:get_value("content-length", Hdrs)),
    ?assertEqual(Res, Body),
    ok.

no_content_length(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/revproxy1/nolengthtest"),
    Res  = list_to_binary(lists:duplicate(512, $A)),

    {ok, {{_,200,_}, Hdrs, Body}} = testsuite:http_get(Url),
    ?assertEqual(undefined, proplists:get_value("content-length", Hdrs)),
    ?assertEqual(undefined, proplists:get_value("transfer-encoding", Hdrs)),
    ?assertEqual("close",   proplists:get_value("connection", Hdrs)),
    ?assertEqual(Res, Body),
    ok.

failed_req_interception(Config) ->
    Port = testsuite:get_yaws_port(6, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/revproxy1/failedreqinterception"),

    ?assertMatch({ok, {{_,500,_}, _, _}}, testsuite:http_get(Url)),
    ok.

failed_resp_interception(Config) ->
    Port = testsuite:get_yaws_port(6, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/revproxy2/failedrespinterception"),

    ?assertMatch({ok, {{_,500,_}, _, _}}, testsuite:http_get(Url)),
    ok.

good_interception(Config) ->
    Port = testsuite:get_yaws_port(6, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/revproxy3/hello.txt"),
    Res  = <<"Hello World www3!">>,

    {ok, {{_,200,_}, Hdrs, Body}} = testsuite:http_get(Url),
    ?assertEqual("true", proplists:get_value("x-test-interception", Hdrs)),
    ?assertEqual(Res, Body),
    ok.


fwdproxy(Config) ->
    PPort = testsuite:get_yaws_port(4, Config),
    Port1 = testsuite:get_yaws_port(2, Config),
    Port2 = testsuite:get_yaws_port(3, Config),
    Url1  = testsuite:make_url(http, "127.0.0.1", Port1, "/rewrite/hello.txt"),
    Url2  = testsuite:make_url(http, "127.0.0.1", Port2, "/rewrite/hello.txt"),
    Res1  = <<"Hello World www2!">>,
    Res2  = <<"Hello World www3!">>,

    HttpOpts = [{proxy, {"localhost", PPort}}],

    {ok, {{_,200,_}, _, Body1}} = testsuite:http_get(Url1, [], HttpOpts),
    ?assertEqual(Res1, Body1),

    {ok, {{_,200,_}, _, Body2}} = testsuite:http_get(Url2, [], HttpOpts),
    ?assertEqual(Res2, Body2),

    ok.

ipv6_address(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/revproxy3/hello.txt"),
    Res  = <<"Hello World www2!">>,

    {ok, {{_,200,_}, _, Body}} = testsuite:http_get(Url),
    ?assertEqual(Res, Body),
    ok.

%%====================================================================
prepare_docroots() ->
    WWW1 = filename:join(?tempdir(?MODULE), "www1"),
    WWW2 = filename:join(?tempdir(?MODULE), "www2"),
    WWW3 = filename:join(?tempdir(?MODULE), "www3"),
    ok = testsuite:create_dir(WWW1),
    ok = testsuite:create_dir(WWW2),
    ok = testsuite:create_dir(WWW3),

    ok = file:write_file(filename:join(WWW1, "index.html"), <<"Hello World www1!">>, [write]),
    ok = file:write_file(filename:join(WWW2, "hello.txt"),  <<"Hello World www2!">>, [write]),
    ok = file:write_file(filename:join(WWW3, "hello.txt"),  <<"Hello World www3!">>, [write]),

    Data = lists:duplicate(1024, $0),
    ok = file:write_file(filename:join(WWW1, "1000.txt"),  lists:duplicate(1000,  Data), [write]),
    ok = file:write_file(filename:join(WWW1, "2000.txt"),  lists:duplicate(2000,  Data), [write]),
    ok = file:write_file(filename:join(WWW1, "3000.txt"),  lists:duplicate(3000,  Data), [write]),
    ok = file:write_file(filename:join(WWW1, "10000.txt"), lists:duplicate(10000, Data), [write]),

    ok.
