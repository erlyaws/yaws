-module(app_test).
-include("../include/tftest.hrl").
-include("../ibrowse/include/ibrowse.hrl").
-compile(export_all).


%% Way to invoke just one test
start([F]) ->
    ?line {ok, _} = ibrowse:start_link(),
    apply(app_test, F, []),
    ibrowse:stop().

start() ->
    io:format("\n ==== DEFLATE TESTS ==== \n\n", []),
    ?line {ok, _} = ibrowse:start_link(),
    deflate_disabled(),
    deflate_enabled(),
    deflate_empty_response(),
    deflate_streamcontent(),
    deflate_options(),
    ibrowse:stop().


deflate_disabled() ->
    io:format("deflate_disabled\n", []),

    %% Static content (and cached)
    Uri1 = "http://localhost:8000/1000.txt",
    ?line {ok, "200", Hdrs1, _} =
        ibrowse:send_req(Uri1, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line undefined = proplists:get_value("Content-Encoding", Hdrs1),

    %% Dynamic content
    Uri2 = "http://localhost:8000/index.yaws",
    ?line {ok, "200", Hdrs2, _} =
        ibrowse:send_req(Uri2, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line undefined = proplists:get_value("Content-Encoding", Hdrs2),
    ok.

deflate_enabled() ->
    io:format("deflate_enabled\n", []),

    %% Static content (and catched)
    Uri1 = "http://localhost:8001/1000.txt",
    ?line {ok, "200", Hdrs1, Body1} =
        ibrowse:send_req(Uri1, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs1),
    ?line true = is_binary(zlib:gunzip(Body1)),

    %% Partial content is not compressed for small (and catched) files
    Uri2 = "http://localhost:8001/1000.txt",
    ?line {ok, "206", Hdrs2, _} =
        ibrowse:send_req(Uri2, [{"Accept-Encoding", "gzip, deflate"},
                                {"Range", "bytes=100-499"}], get),
    ?line undefined = proplists:get_value("Content-Encoding", Hdrs2),
    ?line "400" = proplists:get_value("Content-Length", Hdrs2),

    %% Dynamic content
    Uri3 = "http://localhost:8001/index.yaws",
    ?line {ok, "200", Hdrs3, Body3} =
        ibrowse:send_req(Uri3, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs3),
    ?line true = is_binary(zlib:gunzip(Body3)),
    ok.


deflate_empty_response() ->
    io:format("deflate_empty_response\n", []),

    %% Static content
    Uri1 = "http://localhost:8000/0.txt",
    ?line {ok, "200", Hdrs1, _} =
        ibrowse:send_req(Uri1, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line undefined = proplists:get_value("Content-Encoding", Hdrs1),
    ?line "0" = proplists:get_value("Content-Length", Hdrs1),

    Uri2 = "http://localhost:8001/0.txt",
    ?line {ok, "200", Hdrs2, _} =
        ibrowse:send_req(Uri2, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line undefined = proplists:get_value("Content-Encoding", Hdrs2),
    ?line "0" = proplists:get_value("Content-Length", Hdrs2),

    %% Dynamic content
    Uri3 = "http://localhost:8000/emptytest",
    ?line {ok, "200", Hdrs3, _} =
        ibrowse:send_req(Uri3, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line undefined = proplists:get_value("Content-Encoding", Hdrs3),
    ?line "0" = proplists:get_value("Content-Length", Hdrs3),

    Uri4 = "http://localhost:8001/emptytest",
    ?line {ok, "200", Hdrs4, _} =
        ibrowse:send_req(Uri4, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line undefined = proplists:get_value("Content-Encoding", Hdrs4),
    ?line "0" = proplists:get_value("Content-Length", Hdrs4),
    ok.


deflate_streamcontent() ->
    io:format("deflate_streamcontent\n", []),

    %% Static content (cannot be cached so the file is chunked)
    Uri1 = "http://localhost:8001/10000.txt",
    ?line {ok, "200", Hdrs1, _} =
        ibrowse:send_req(Uri1, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs1),
    ?line "chunked" = proplists:get_value("Transfer-Encoding", Hdrs1),
    ?line undefined = proplists:get_value("Content-Length", Hdrs1),

    %% Partial content is not compressed for large files
    Uri2 = "http://localhost:8001/10000.txt",
    ?line {ok, "206", Hdrs2, _} =
        ibrowse:send_req(Uri2, [{"Accept-Encoding", "gzip, deflate"},
                                {"Range", "bytes=500-599"}], get),
    ?line undefined = proplists:get_value("Content-Encoding", Hdrs2),
    ?line undefined = proplists:get_value("Transfer-Encoding", Hdrs2),
    ?line "100"     = proplists:get_value("Content-Length", Hdrs2),

    %% Dynamic content (chunked)
    Uri3 = "http://localhost:8001/streamtest",
    ?line {ok, "200", Hdrs3, _} =
        ibrowse:send_req(Uri3, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs3),
    ?line "chunked" = proplists:get_value("Transfer-Encoding", Hdrs3),
    ?line undefined = proplists:get_value("Content-Length", Hdrs3),
    ok.


deflate_options() ->
    io:format("deflate_options\n", []),
    deflate_mime_types(),
    deflate_compress_size(),
    deflate_gzip_static(),
    deflate_otheroptions(),
    ok.

deflate_mime_types() ->
    io:format("  deflate_mime_types\n", []),

    %% image/gif not compressed on localhost:8001
    Uri1 = "http://localhost:8001/icons/yaws.gif",
    ?line {ok, "200", Hdrs1, _} =
        ibrowse:send_req(Uri1, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line undefined = proplists:get_value("Content-Encoding", Hdrs1),

    %% application/* compressed on localhost:8002 but text/plain not
    Uri2 = "http://localhost:8002/1000.txt",
    ?line {ok, "200", Hdrs2, _} =
        ibrowse:send_req(Uri2, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line undefined = proplists:get_value("Content-Encoding", Hdrs2),

    Uri3 = "http://localhost:8002/yaws.eps",
    ?line {ok, "200", Hdrs3, Body3} =
        ibrowse:send_req(Uri3, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs3),
    ?line true = is_binary(zlib:gunzip(Body3)),

    %% application/* and text/* compressed on localhost:8003
    Uri4 = "http://localhost:8003/1000.txt",
    ?line {ok, "200", Hdrs4, Body4} =
        ibrowse:send_req(Uri4, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs4),
    ?line true = is_binary(zlib:gunzip(Body4)),

    Uri5 = "http://localhost:8003/yaws.eps",
    ?line {ok, "200", Hdrs5, Body5} =
        ibrowse:send_req(Uri5, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs5),
    ?line true = is_binary(zlib:gunzip(Body5)),

    %% All mime types are compressed on localhost:8004
    Uri6 = "http://localhost:8004/1000.txt",
    ?line {ok, "200", Hdrs6, Body6} =
        ibrowse:send_req(Uri6, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs6),
    ?line true = is_binary(zlib:gunzip(Body6)),

    Uri7 = "http://localhost:8004/yaws.eps",
    ?line {ok, "200", Hdrs7, Body7} =
        ibrowse:send_req(Uri7, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs7),
    ?line true = is_binary(zlib:gunzip(Body7)),
    ok.


deflate_compress_size() ->
    io:format("  deflate_compress_size\n", []),

    %% Static content (cached)
    Uri1 = "http://localhost:8005/1000.txt",
    ?line {ok, "200", Hdrs1, _} =
        ibrowse:send_req(Uri1, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line undefined = proplists:get_value("Content-Encoding", Hdrs1),

    Uri2 = "http://localhost:8005/3000.txt",
    ?line {ok, "200", Hdrs2, Body2} =
        ibrowse:send_req(Uri2, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs2),
    ?line true = is_binary(zlib:gunzip(Body2)),

    %% Dynamic content
    Uri3 = "http://localhost:8005/smalltest",
    ?line {ok, "200", Hdrs3, _} =
        ibrowse:send_req(Uri3, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line undefined = proplists:get_value("Content-Encoding", Hdrs3),

    Uri4 = "http://localhost:8005/bigtest",
    ?line {ok, "200", Hdrs4, Body4} =
        ibrowse:send_req(Uri4, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs4),
    ?line true = is_binary(zlib:gunzip(Body4)),
    ok.


deflate_gzip_static() ->
    io:format("  deflate_gzip_static\n", []),

    %% when gzip_static is disabled, large static files are chunked
    Uri1 = "http://localhost:8006/10000.txt",
    ?line {ok, "200", Hdrs1, Body1} =
        ibrowse:send_req(Uri1, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs1),
    ?line "chunked" = proplists:get_value("Transfer-Encoding", Hdrs1),
    ?line undefined = proplists:get_value("Content-Length", Hdrs1),
    ?line true = is_binary(zlib:gunzip(Body1)),

    %% when gzip_static is enabled, if precompressed static file is found, the
    %% response is not chunked
    Uri2 = "http://localhost:8007/10000.txt",
    ?line {ok, "200", Hdrs2, Body2} =
        ibrowse:send_req(Uri2, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs2),
    ?line undefined = proplists:get_value("Transfer-Encoding", Hdrs2),
    ?line true = is_binary(zlib:gunzip(Body2)),

    ?line zlib:gunzip(Body1) == zlib:gunzip(Body2),

    %% if mtimes of compressed and uncompress files do not match, the compressed
    %% file is ignored
    Uri3 = "http://localhost:8007/10000.txt.old",
    ?line {ok, "200", Hdrs3, Body3} =
        ibrowse:send_req(Uri3, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs3),
    ?line "chunked" = proplists:get_value("Transfer-Encoding", Hdrs3),
    ?line undefined = proplists:get_value("Content-Length", Hdrs3),
    ?line true = is_binary(zlib:gunzip(Body3)),

    ok.


deflate_otheroptions() ->
    io:format("  deflate_otheroptions\n", []),

    %% Static content
    Uri1 = "http://localhost:8006/1000.txt",
    ?line {ok, "200", Hdrs1, Body1} =
        ibrowse:send_req(Uri1, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs1),
    ?line true = is_binary(zlib:gunzip(Body1)),

    Uri2 = "http://localhost:8006/10000.txt",
    ?line {ok, "200", Hdrs2, Body2} =
        ibrowse:send_req(Uri2, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs2),
    ?line "chunked" = proplists:get_value("Transfer-Encoding", Hdrs2),
    ?line undefined = proplists:get_value("Content-Length", Hdrs2),
    ?line true = is_binary(zlib:gunzip(Body2)),

    %% Dynamic content
    Uri3 = "http://localhost:8006/smalltest",
    ?line {ok, "200", Hdrs3, Body3} =
        ibrowse:send_req(Uri3, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs3),
    ?line true = is_binary(zlib:gunzip(Body3)),

    Uri4 = "http://localhost:8006/bigtest",
    ?line {ok, "200", Hdrs4, Body4} =
        ibrowse:send_req(Uri4, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs4),
    ?line true = is_binary(zlib:gunzip(Body4)),

    Uri5 = "http://localhost:8006/streamtest",
    ?line {ok, "200", Hdrs5, Body5} =
        ibrowse:send_req(Uri5, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs5),
    ?line "chunked" = proplists:get_value("Transfer-Encoding", Hdrs5),
    ?line undefined = proplists:get_value("Content-Length", Hdrs5),
    ?line true = is_binary(zlib:gunzip(Body5)),
    ok.
