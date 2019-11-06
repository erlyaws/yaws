-module(deflate_SUITE).

-include("testsuite.hrl").
-include_lib("kernel/include/file.hrl").

-compile(export_all).

all() ->
    [
     deflate_disabled,
     deflate_enabled,
     empty_response,
     chunked_response,
     deflate_options_mime_types,
     deflate_options_compress_size,
     deflate_options_gzip_static,
     deflate_options_otheroptions
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
deflate_disabled(Config) ->
    Port1  = testsuite:get_yaws_port(1, Config),
    Port2  = testsuite:get_yaws_port(2, Config),
    Url1_1 = testsuite:make_url(http, "127.0.0.1", Port1, "/1000.txt"),
    Url1_2 = testsuite:make_url(http, "127.0.0.1", Port1, "/index.yaws"),
    Url2_1 = testsuite:make_url(http, "127.0.0.1", Port2, "/1000.txt"),
    Url2_2 = testsuite:make_url(http, "127.0.0.1", Port2, "/index.yaws"),
    GzHdr  = {"Accept-Encoding", "gzip, deflate"},


    %% Static content (and cached) - Not supported by server
    {ok, {{_,200,_}, Hdrs1_1, _}} = testsuite:http_get(Url1_1, [GzHdr]),
    ?assertEqual(undefined, proplists:get_value("content-encoding", Hdrs1_1)),
    ?assertEqual(undefined, proplists:get_value("vary", Hdrs1_1)),

    %% Dynamic content - Not supported by server
    {ok, {{_,200,_}, Hdrs1_2, _}} = testsuite:http_get(Url1_2, [GzHdr]),
    ?assertEqual(undefined, proplists:get_value("content-encoding", Hdrs1_2)),
    ?assertEqual(undefined, proplists:get_value("vary", Hdrs1_2)),

    %% Static content (and cached) - Not supported by client
    {ok, {{_,200,_}, Hdrs2_1, _}} = testsuite:http_get(Url2_1),
    ?assertEqual(undefined, proplists:get_value("content-encoding", Hdrs2_1)),
    ?assertEqual("Accept-Encoding", proplists:get_value("vary", Hdrs2_1)),

    %% Dynamic content - Not supported by client
    {ok, {{_,200,_}, Hdrs2_2, _}} = testsuite:http_get(Url2_2),
    ?assertEqual(undefined, proplists:get_value("content-encoding", Hdrs2_2)),
    ?assertEqual("Accept-Encoding", proplists:get_value("vary", Hdrs2_2)),
    ok.

deflate_enabled(Config) ->
    Port = testsuite:get_yaws_port(2, Config),
    Url1 = testsuite:make_url(http, "127.0.0.1", Port, "/1000.txt"),
    Url2 = testsuite:make_url(http, "127.0.0.1", Port, "/index.yaws"),
    {ok, Res} = file:read_file(filename:join(?tempdir(?MODULE), "www/1000.txt")),
    GzHdr = {"Accept-Encoding", "gzip, deflate"},

    %% Static content (and cathed)
    {ok, {{_,200,_}, Hdrs1_1, Body1}} = testsuite:http_get(Url1, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs1_1)),
    ?assertEqual("Accept-Encoding", proplists:get_value("vary", Hdrs1_1)),
    ?assertEqual(Res, zlib:gunzip(Body1)),

    IFSHdr = {"If-Modified-Since", httpd_util:rfc1123_date()},
    ?assertMatch({ok, {{_,304,_}, _, _}}, testsuite:http_get(Url1, [GzHdr, IFSHdr])),

    %% Partial content is not compressed for small (and cached) files
    RgHdr = {"Range", "bytes=100-499"},
    {ok, {{_,206,_}, Hdrs1_3, _}} = testsuite:http_get(Url1, [GzHdr, RgHdr]),
    ?assertEqual("400", proplists:get_value("content-length", Hdrs1_3)),
    ?assertEqual("Accept-Encoding", proplists:get_value("vary", Hdrs1_3)),

    %% Dynamic content
    {ok, {{_,200,_}, Hdrs2, Body2}} = testsuite:http_get(Url2, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs2)),
    ?assertEqual("Accept-Encoding", proplists:get_value("vary", Hdrs2)),
    ?assert(is_binary(zlib:gunzip(Body2))),
    ok.

empty_response(Config) ->
    Port1  = testsuite:get_yaws_port(1, Config),
    Port2  = testsuite:get_yaws_port(2, Config),
    Url1_1 = testsuite:make_url(http, "127.0.0.1", Port1, "/0.txt"),
    Url1_2 = testsuite:make_url(http, "127.0.0.1", Port1, "/emptytest"),
    Url2_1 = testsuite:make_url(http, "127.0.0.1", Port2, "/0.txt"),
    Url2_2 = testsuite:make_url(http, "127.0.0.1", Port2, "/emptytest"),
    GzHdr = {"Accept-Encoding", "gzip, deflate"},

    %% Static content
    {ok, {{_,200,_}, Hdrs1_1, _}} = testsuite:http_get(Url1_1, [GzHdr]),
    ?assertEqual(undefined, proplists:get_value("content-encoding", Hdrs1_1)),
    ?assertEqual(undefined, proplists:get_value("vary", Hdrs1_1)),
    ?assertEqual("0", proplists:get_value("content-length", Hdrs1_1)),

    %% dynamic content
    {ok, {{_,200,_}, Hdrs1_2, _}} = testsuite:http_get(Url1_2, [GzHdr]),
    ?assertEqual(undefined, proplists:get_value("content-encoding", Hdrs1_2)),
    ?assertEqual(undefined, proplists:get_value("vary", Hdrs1_2)),
    ?assertEqual("0", proplists:get_value("content-length", Hdrs1_2)),

    %% Static content
    {ok, {{_,200,_}, Hdrs2_1, _}} = testsuite:http_get(Url2_1, [GzHdr]),
    ?assertEqual(undefined, proplists:get_value("content-encoding", Hdrs2_1)),
    ?assertEqual(undefined, proplists:get_value("vary", Hdrs2_1)),
    ?assertEqual("0", proplists:get_value("content-length", Hdrs2_1)),

    %% dynamic content
    {ok, {{_,200,_}, Hdrs2_2, _}} = testsuite:http_get(Url2_2, [GzHdr]),
    ?assertEqual(undefined, proplists:get_value("content-encoding", Hdrs2_2)),
    ?assertEqual(undefined, proplists:get_value("vary", Hdrs2_2)),
    ?assertEqual("0", proplists:get_value("content-length", Hdrs2_2)),
    ok.

chunked_response(Config) ->
    Port  = testsuite:get_yaws_port(2, Config),
    Url1  = testsuite:make_url(http, "127.0.0.1", Port, "/10000.txt"),
    Url2  = testsuite:make_url(http, "127.0.0.1", Port, "/10000.txt"),
    Url3  = testsuite:make_url(http, "127.0.0.1", Port, "/streamtest"),
    GzHdr = {"Accept-Encoding", "gzip, deflate"},

    %% Static content (cannot be cached so the file is chunked)
    {ok, {{_,200,_}, Hdrs1, _}} = testsuite:http_get(Url1, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs1)),
    ?assertEqual("Accept-Encoding", proplists:get_value("vary", Hdrs1)),
    ?assertEqual(undefined, proplists:get_value("content-length", Hdrs1)),
    ?assertEqual("chunked", proplists:get_value("transfer-encoding", Hdrs1)),

    %% Partial content is not compressed for large files
    RgHdr = {"Range", "bytes=500-599"},
    {ok, {{_,206,_}, Hdrs2, _}} = testsuite:http_get(Url2, [GzHdr, RgHdr]),
    ?assertEqual(undefined, proplists:get_value("content-encoding", Hdrs2)),
    ?assertEqual("Accept-Encoding", proplists:get_value("vary", Hdrs2)),
    ?assertEqual(undefined, proplists:get_value("transfer-encoding", Hdrs2)),
    ?assertEqual("100", proplists:get_value("content-length", Hdrs2)),

    %% Dynamic content (chunked)
    {ok, {{_,200,_}, Hdrs3, _}} = testsuite:http_get(Url3, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs3)),
    ?assertEqual("Accept-Encoding", proplists:get_value("vary", Hdrs3)),
    ?assertEqual(undefined, proplists:get_value("content-length", Hdrs3)),
    ?assertEqual("chunked", proplists:get_value("transfer-encoding", Hdrs3)),
    ok.

deflate_options_mime_types(Config) ->
    Port1 = testsuite:get_yaws_port(2, Config),
    Port2 = testsuite:get_yaws_port(3, Config),
    Port3 = testsuite:get_yaws_port(4, Config),
    Port4 = testsuite:get_yaws_port(5, Config),
    Port10 = testsuite:get_yaws_port(10, Config),
    Url1  = testsuite:make_url(http, "127.0.0.1", Port1, "/icons/yaws.gif"),
    Url2  = testsuite:make_url(http, "127.0.0.1", Port2, "/1000.txt"),
    Url3  = testsuite:make_url(http, "127.0.0.1", Port2, "/yaws.eps"),
    Url4  = testsuite:make_url(http, "127.0.0.1", Port3, "/1000.txt"),
    Url5  = testsuite:make_url(http, "127.0.0.1", Port3, "/yaws.eps"),
    Url6  = testsuite:make_url(http, "127.0.0.1", Port4, "/1000.txt"),
    Url7  = testsuite:make_url(http, "127.0.0.1", Port4, "/yaws.eps"),
    Url8  = testsuite:make_url(http, "127.0.0.1", Port4, "/binary_header/foo"),
    Url10 = testsuite:make_url(http, "127.0.0.1", Port10, "/1000.txt"),
    Url11  = testsuite:make_url(http, "127.0.0.1", Port10, "/yaws.eps"),
    GzHdr = {"Accept-Encoding", "gzip, deflate"},

    %% image/gif not compressed on localhost:yaws_port2
    {ok, {{_,200,_}, Hdrs1, _}} = testsuite:http_get(Url1, [GzHdr]),
    ?assertEqual(undefined, proplists:get_value("content-encoding", Hdrs1)),

    %% application/* compressed on localhost:yaws_port3 but text/plain not
    {ok, {{_,200,_}, Hdrs2, _}} = testsuite:http_get(Url2, [GzHdr]),
    ?assertEqual(undefined, proplists:get_value("content-encoding", Hdrs2)),

    {ok, {{_,200,_}, Hdrs3, Body3}} = testsuite:http_get(Url3, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs3)),
    ?assert(is_binary(zlib:gunzip(Body3))),

    %% application/* and text/* compressed on localhost:yaws_port4
    {ok, {{_,200,_}, Hdrs4, Body4}} = testsuite:http_get(Url4, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs4)),
    ?assert(is_binary(zlib:gunzip(Body4))),

    {ok, {{_,200,_}, Hdrs5, Body5}} = testsuite:http_get(Url5, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs5)),
    ?assert(is_binary(zlib:gunzip(Body5))),

    %% All mime types are compressed on localhost:yaws_port5
    {ok, {{_,200,_}, Hdrs6, Body6}} = testsuite:http_get(Url6, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs6)),
    ?assert(is_binary(zlib:gunzip(Body6))),

    {ok, {{_,200,_}, Hdrs7, Body7}} = testsuite:http_get(Url7, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs7)),
    ?assert(is_binary(zlib:gunzip(Body7))),

    {ok, {{_,200,_}, Hdrs8, Body8}} = testsuite:http_get(Url8, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs8)),
    ?assert(is_binary(zlib:gunzip(Body8))),

    %% Verify no Vary header from localhost:yaws_port10 when response
    %% is txt, but Vary is present when response is eps.
    {ok, {{_,200,_}, Hdrs10, _}} = testsuite:http_get(Url10, [GzHdr]),
    ?assertNot(proplists:is_defined("content-encoding", Hdrs10)),
    ?assertNot(proplists:is_defined("vary", Hdrs10)),
    {ok, {{_,200,_}, Hdrs11, _}} = testsuite:http_get(Url11, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs11)),
    ?assertEqual("Accept-Encoding", proplists:get_value("vary", Hdrs11)),
    ok.

deflate_options_compress_size(Config) ->
    Port1 = testsuite:get_yaws_port(6, Config),
    Port2 = testsuite:get_yaws_port(9, Config),
    Url1  = testsuite:make_url(http, "127.0.0.1", Port1, "/1000.txt"),
    Url2  = testsuite:make_url(http, "127.0.0.1", Port1, "/3000.txt"),
    Url3  = testsuite:make_url(http, "127.0.0.1", Port1, "/10000.txt"),
    Url4  = testsuite:make_url(http, "127.0.0.1", Port2, "/10000.txt"),
    Url5  = testsuite:make_url(http, "127.0.0.1", Port1, "/smalltest"),
    Url6  = testsuite:make_url(http, "127.0.0.1", Port1, "/bigtest"),
    GzHdr = {"Accept-Encoding", "gzip, deflate"},

    %% Small static content (cached)
    {ok, {{_,200,_}, Hdrs1, _}} = testsuite:http_get(Url1, [GzHdr]),
    ?assertEqual(undefined, proplists:get_value("content-encoding", Hdrs1)),

    {ok, {{_,200,_}, Hdrs2, Body2}} = testsuite:http_get(Url2, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs2)),
    ?assert(is_binary(zlib:gunzip(Body2))),

    %% Large static content (cached)
    {ok, {{_,200,_}, Hdrs3, Body3}} = testsuite:http_get(Url3, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs3)),
    ?assert(is_binary(zlib:gunzip(Body3))),

    {ok, {{_,200,_}, Hdrs4, _}} = testsuite:http_get(Url4, [GzHdr]),
    ?assertEqual(undefined, proplists:get_value("content-encoding", Hdrs4)),

    %% Dynamic content
    {ok, {{_,200,_}, Hdrs5, _}} = testsuite:http_get(Url5, [GzHdr]),
    ?assertEqual(undefined, proplists:get_value("content-encoding", Hdrs5)),

    {ok, {{_,200,_}, Hdrs6, Body6}} = testsuite:http_get(Url6, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs6)),
    ?assert(is_binary(zlib:gunzip(Body6))),
    ok.

deflate_options_gzip_static(Config) ->
    Port1 = testsuite:get_yaws_port(7, Config),
    Port2 = testsuite:get_yaws_port(8, Config),
    Url1  = testsuite:make_url(http, "127.0.0.1", Port1, "/10000.txt"),
    Url2  = testsuite:make_url(http, "127.0.0.1", Port2, "/10000.txt"),
    Url3  = testsuite:make_url(http, "127.0.0.1", Port2, "/10000.txt.old"),
    GzHdr = {"Accept-Encoding", "gzip, deflate"},

    %% when gzip_static is disabled, large static files are chunked
    {ok, {{_,200,_}, Hdrs1, Body1}} = testsuite:http_get(Url1, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs1)),
    ?assertEqual("chunked", proplists:get_value("transfer-encoding", Hdrs1)),
    ?assertEqual(undefined, proplists:get_value("content-length", Hdrs1)),
    ?assert(is_binary(zlib:gunzip(Body1))),

    %% when gzip_static is enabled, if precompressed static file is found, the
    %% response is not chunked
    {ok, {{_,200,_}, Hdrs2, Body2}} = testsuite:http_get(Url2, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs2)),
    ?assertEqual(undefined, proplists:get_value("transfer-encoding", Hdrs2)),
    ?assert(is_binary(zlib:gunzip(Body2))),

    %% if mtimes of compressed and uncompress files do not match, the compressed
    %% file is ignored
    {ok, {{_,200,_}, Hdrs3, Body3}} = testsuite:http_get(Url3, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs3)),
    ?assertEqual("chunked", proplists:get_value("transfer-encoding", Hdrs3)),
    ?assertEqual(undefined, proplists:get_value("content-length", Hdrs3)),
    ?assert(is_binary(zlib:gunzip(Body3))),
    ok.

deflate_options_otheroptions(Config) ->
    Port = testsuite:get_yaws_port(7, Config),
    Url1  = testsuite:make_url(http, "127.0.0.1", Port, "/1000.txt"),
    Url2  = testsuite:make_url(http, "127.0.0.1", Port, "/10000.txt"),
    Url3  = testsuite:make_url(http, "127.0.0.1", Port, "/smalltest"),
    Url4  = testsuite:make_url(http, "127.0.0.1", Port, "/bigtest"),
    Url5  = testsuite:make_url(http, "127.0.0.1", Port, "/streamtest"),
    GzHdr = {"Accept-Encoding", "gzip, deflate"},

    %% Static content
    {ok, {{_,200,_}, Hdrs1, Body1}} = testsuite:http_get(Url1, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs1)),
    ?assert(is_binary(zlib:gunzip(Body1))),

    {ok, {{_,200,_}, Hdrs2, Body2}} = testsuite:http_get(Url2, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs2)),
    ?assertEqual("chunked", proplists:get_value("transfer-encoding", Hdrs2)),
    ?assertEqual(undefined, proplists:get_value("content-length", Hdrs2)),
    ?assert(is_binary(zlib:gunzip(Body2))),

    %% Dynamic content
    {ok, {{_,200,_}, Hdrs3, Body3}} = testsuite:http_get(Url3, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs3)),
    ?assert(is_binary(zlib:gunzip(Body3))),

    {ok, {{_,200,_}, Hdrs4, Body4}} = testsuite:http_get(Url4, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs4)),
    ?assert(is_binary(zlib:gunzip(Body4))),

    {ok, {{_,200,_}, Hdrs5, Body5}} = testsuite:http_get(Url5, [GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs5)),
    ?assertEqual("chunked", proplists:get_value("transfer-encoding", Hdrs5)),
    ?assertEqual(undefined, proplists:get_value("content-length", Hdrs5)),
    ?assert(is_binary(zlib:gunzip(Body5))),

    %% Multiple Accept-Encoding headers
    %% This test reuses Uri1 and Body1. It sends two Accept-Encoding
    %% headers to make sure Yaws handles them correctly. The reply
    %% should be the same as for the static content test at the beginning
    %% of this test function.
    IdHdr = {"Accept-Encoding", "identity;q=0.5"},
    {ok, {{_,200,_}, Hdrs6, Body1}} = testsuite:http_get(Url1, [IdHdr, GzHdr]),
    ?assertEqual("gzip", proplists:get_value("content-encoding", Hdrs6)),
    ok.

%%====================================================================
prepare_docroots() ->
    WWW = filename:join(?tempdir(?MODULE), "www"),
    ok = testsuite:create_dir(WWW),

    Data = lists:duplicate(1024, $0),
    ok = file:write_file(filename:join(WWW, "0.txt"), <<>>, [write]),
    ok = file:write_file(filename:join(WWW, "1000.txt"),  lists:duplicate(1000,  Data), [write]),
    ok = file:write_file(filename:join(WWW, "3000.txt"),  lists:duplicate(3000,  Data), [write]),
    ok = file:write_file(filename:join(WWW, "10000.txt"), lists:duplicate(10000, Data), [write]),

    {ok, Bin} = file:read_file(filename:join(WWW, "10000.txt")),
    GzBin = zlib:gzip(Bin),
    ok = file:write_file(filename:join(WWW, "10000.txt.gz"), GzBin, [write]),

    {ok, _} = file:copy(filename:join(WWW, "10000.txt"), filename:join(WWW, "10000.txt.old")),
    {ok, _} = file:copy(filename:join(WWW, "10000.txt.gz"), filename:join(WWW, "10000.txt.old.gz")),

    %% 10000.txt.old should be older than 10000.txt.old.gz
    {ok, FI} = file:read_file_info(filename:join(WWW, "10000.txt.old"), [{time,posix}]),
    FI1 = FI#file_info{mtime=FI#file_info.mtime-60},
    ok = file:write_file_info(filename:join(WWW, "10000.txt.old.gz"), FI1, [{time,posix}]),
    ok.
