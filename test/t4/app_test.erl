-module(app_test).
-compile(export_all).

-include_lib("ibrowse/include/ibrowse.hrl").
-include("tftest.hrl").


%% Way to invoke just one test
start([F]) ->
    ?line {ok, _} = ibrowse:start_link(),
    try
        apply(app_test, F, []),
        ok
    catch
        Error:Reason ->
            throw({Error, Reason})
    after
        ibrowse:stop()
    end.

start() ->
    io:format("\n ==== REVERSE PROXY TESTS ==== \n\n", []),
    ?line {ok, _} = ibrowse:start_link(),
    try
        deflate_revproxy_test1(),
        deflate_revproxy_test2(),
        test_post_revproxy(),
        test_streamcontent_revproxy(),
        test_keepalive_revproxy(),
        test_rewrite_revproxy(),
        test_large_content_revproxy(),
        test_no_content_length_revproxy(),
        test_failed_req_interception_revproxy(),
        test_failed_resp_interception_revproxy(),
        test_good_interception_revproxy(),
        test_fwdproxy(),
        test_ipv6_address(),
        ok
    catch
        Error:Reason ->
            throw({Error, Reason})
    after
        ibrowse:stop()
    end.


deflate_revproxy_test1() ->
    io:format("deflate_revproxy_test1\n", []),
    Uri = "http://localhost:8000/revproxy1/hello.txt",
    Res = "Hello World 1!\n",

    %% client: nodeflate - proxy: deflate - backend: deflate
    %%    ==> result: uncompressed
    ?line {ok, "200", Hdrs1, Body1} = ibrowse:send_req(Uri, [], get),
    ?line undefined = proplists:get_value("Content-Encoding", Hdrs1),
    ?line Res = Body1,


    %% client: deflate - proxy: deflate - backend: deflate
    %%    ==> result: compressed
    ?line {ok, "200", Hdrs2, Body2} =
        ibrowse:send_req(Uri, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs2),
    ?line Res = binary_to_list(zlib:gunzip(Body2)),
    ok.

deflate_revproxy_test2() ->
    io:format("deflate_revproxy_test2\n", []),
    Uri = "http://localhost:8000/revproxy2/hello.txt",
    Res = "Hello World 2!\n",

    %% client: nodeflate - proxy: deflate - backend: nodeflate
    %%    ==> result: uncompressed
    ?line {ok, "200", Hdrs1, Body1} = ibrowse:send_req(Uri, [], get),
    ?line undefined = proplists:get_value("Content-Encoding", Hdrs1),
    ?line Res = Body1,

    %% client: deflate - proxy: deflate - backend: nodeflate
    %%    ==> result: compressed
    ?line {ok, "200", Hdrs2, Body2} =
        ibrowse:send_req(Uri, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs2),
    ?line Res = binary_to_list(zlib:gunzip(Body2)),
    ok.

test_post_revproxy() ->
    io:format("post_revproxy_test\n",[]),
    small_post(),
    large_post(),
    small_chunked_post(),
    large_chunked_post(),
    ok.

small_post() ->
    io:format("  small post\n",[]),
    {ok, Bin} = file:read_file(?builddir ++ "/docroot-test/1000.txt"),
    Sz = erlang:integer_to_list(size(Bin)),
    Uri = "http://localhost:8000/revproxy1/posttest/" ++ Sz,
    Hdrs = [{content_length, Sz}, {content_type, "binary/octet-stream"}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, Hdrs, post, Bin, []),
    ok.

large_post() ->
    io:format("  large post\n",[]),
    {ok, Bin} = file:read_file(?builddir ++ "/docroot-test/10000.txt"),
    Sz   = erlang:integer_to_list(size(Bin)),
    Uri  = "http://localhost:8000/revproxy1/posttest/" ++ Sz,
    Hdrs = [{content_length, Sz}, {content_type, "binary/octet-stream"}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, Hdrs, post, Bin, []),
    ok.

small_chunked_post() ->
    %% Chunk size is less than partial_post_size
    io:format("  small chunked post\n",[]),
    {ok, Bin} = file:read_file(?builddir ++ "/docroot-test/3000.txt"),
    Sz   = erlang:integer_to_list(size(Bin)),
    Uri  = "http://localhost:8000/revproxy1/posttest/chunked/" ++ Sz,
    Hdrs = [{content_type, "binary/octet-stream"}],
    Opts = [{transfer_encoding, {chunked, 1000*1000}}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, Hdrs, post, Bin, Opts),
    ok.

large_chunked_post() ->
    %% Chunk size is greater than partial_post_size
    io:format("  large chunked post\n",[]),
    {ok, Bin} = file:read_file(?builddir ++ "/docroot-test/10000.txt"),
    Sz   = erlang:integer_to_list(size(Bin)),
    Uri  = "http://localhost:8000/revproxy1/posttest/chunked/" ++ Sz,
    Hdrs = [{content_type, "binary/octet-stream"}],
    Opts = [{transfer_encoding, {chunked, 4000*1000}}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, Hdrs, post, Bin, Opts),
    ok.



test_streamcontent_revproxy() ->
    io:format("streamcontent_revproxy_test\n", []),
    Uri = "http://localhost:8000/revproxy1/streamtest",
    Res = "This is the data in the first chunk\n"
        "and this is the second one\n"
        "consequence",

    ?line {ok, "200", Hdrs1, Body1} = ibrowse:send_req(Uri, [], get),
    ?line "chunked" = proplists:get_value("Transfer-Encoding", Hdrs1),
    ?line Res = Body1,

    ?line {ok, "200", Hdrs2, Body2} =
        ibrowse:send_req(Uri, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "chunked" = proplists:get_value("Transfer-Encoding", Hdrs2),
    ?line Res = binary_to_list(zlib:gunzip(Body2)),
    ok.


test_keepalive_revproxy() ->
    io:format("keepalive_revproxy_test\n", []),
    Path1 = "/revproxy1/hello.txt",
    Path2 = "/",
    Path3 = "/revproxy2/hello.txt",
    Res1  = "Hello World 1!\n",
    Res2  = "Hello World 2!\n",

    ?line {ok, Sock} = gen_tcp:connect("localhost", 8000,
                                       [list, {active, false}]),

    ?line ok = gen_tcp:send(Sock, "GET " ++ Path1 ++ " HTTP/1.1\r\n"
                            "Host: localhost:8000\r\n"
                            "Connection: Keep-Alive\r\n"
                            "\r\n"),
    ?line {ok, Len1} = recv_hdrs(Sock),
    ?line {ok, Res1} = recv_body(Sock, Len1),

    ?line ok = gen_tcp:send(Sock, "GET " ++ Path2 ++ " HTTP/1.1\r\n"
                            "Host: localhost:8000\r\n"
                            "Connection: Keep-Alive\r\n"
                            "\r\n"),
    ?line {ok, Len2} = recv_hdrs(Sock),
    ?line {ok, _}    = recv_body(Sock, Len2),

    ?line ok = gen_tcp:send(Sock, "GET " ++ Path3 ++ " HTTP/1.1\r\n"
                            "Host: localhost:8000\r\n"
                            "Connection: Keep-Alive\r\n"
                            "\r\n"),
    ?line {ok, Len3} = recv_hdrs(Sock),
    ?line {ok, Res2} = recv_body(Sock, Len3),

    gen_tcp:close(Sock),
    ok.


test_rewrite_revproxy() ->
    io:format("rewrite_revproxy_test\n", []),
    Uri1 = "http://localhost:8000/rewrite/revproxy1/hello.txt",
    Uri2 = "http://localhost:8000/rewrite/revproxy2/hello.txt",
    Res1 = "Hello World 1!\n",
    Res2 = "Hello World 2!\n",

    ?line {ok, "200", _, Body1} = ibrowse:send_req(Uri1, [], get),
    ?line Res1 = Body1,

    ?line {ok, "200", _, Body2} = ibrowse:send_req(Uri2, [], get),
    ?line Res2 = Body2,
    ok.

test_large_content_revproxy() ->
    io:format("large_content_revproxy_test\n", []),
    Uri = "http://localhost:8004/revproxy/8388608.txt",

    ?line {ok, Bin} = file:read_file(?builddir ++ "/docroot-test2/8388608.txt"),
    ?line {ok, "200", Hdrs, Body0} = ibrowse:send_req(Uri, [], get),
    ?line "8388608" = proplists:get_value("Content-Length", Hdrs),
    Body = list_to_binary(Body0),
    ?line true = (size(Body) == 8388608),
    ?line Bin = Body,
    ok.

test_no_content_length_revproxy() ->
    io:format("no_content_length_revproxy_test\n", []),
    Uri = "http://localhost:8001/revproxy1/nolengthtest",
    Res = lists:duplicate(512, $A),

    ?line {ok, "200", Hdrs, Body} = ibrowse:send_req(Uri, [], get),
    ?line undefined = proplists:get_value("Content-Length", Hdrs),
    ?line undefined = proplists:get_value("Transfer-Encoding", Hdrs),
    ?line "close"   = proplists:get_value("Connection", Hdrs),
    ?line Res = Body,
    ok.

test_failed_req_interception_revproxy() ->
    io:format("failed_req_interception_revproxy_test\n", []),
    Uri = "http://localhost:8005/revproxy1/failedreqinterception",
    ?line {ok, "500", _, _} = ibrowse:send_req(Uri, [], get),
    ok.

test_failed_resp_interception_revproxy() ->
    io:format("failed_resp_interception_revproxy_test\n", []),
    Uri = "http://localhost:8005/revproxy2/failedrespinterception",
    ?line {ok, "500", _, _} = ibrowse:send_req(Uri, [], get),
    ok.

test_good_interception_revproxy() ->
    io:format("good_interception_revproxy_test\n", []),
    Uri = "http://localhost:8005/revproxy3/hello.txt",
    Res = "Hello World 2!\n",

    ?line {ok, "200", Hdrs, Body} = ibrowse:send_req(Uri, [], get),
    ?line Body = Res,
    ?line "true"   = proplists:get_value("X-Test-Interception", Hdrs),
    ok.

test_fwdproxy() ->
    io:format("fwdproxy_test\n", []),
    Uri1 = "http://localhost:8001/rewrite/hello.txt",
    Uri2 = "http://localhost:8002/rewrite/hello.txt",
    Res1 = "Hello World 1!\n",
    Res2 = "Hello World 2!\n",

    Opts = [{proxy_host, "localhost"}, {proxy_port, 8003}],
    ?line {ok, "200", _, Body1} = ibrowse:send_req(Uri1, [], get, [], Opts),
    ?line Res1 = Body1,

    ?line {ok, "200", _, Body2} = ibrowse:send_req(Uri2, [], get, [], Opts),
    ?line Res2 = Body2,
    ok.

test_ipv6_address() ->
    io:format("revproxy_url_ipv6\n", []),
    Uri = "http://localhost:8000/revproxy3/hello.txt",
    Res = "Hello World 1!\n",

    ?line {ok, "200", _, Body} = ibrowse:send_req(Uri, [], get),
    ?line Body = Res,
    ok.

recv_hdrs(Sock) ->
    inet:setopts(Sock, [{packet, http}]),
    recv_hdrs(Sock, 0).
recv_hdrs(Sock, Len) ->
    inet:setopts(Sock, [{active, once}]),
    receive
        {http, Sock, http_eoh} ->
            {ok, Len};
        {http, Sock, {http_error, Error}} ->
            {error, Error};
        {http, Sock, {http_header, _, 'Content-Length', _, LenStr}} ->
            recv_hdrs(Sock, erlang:list_to_integer(LenStr));
        {http, Sock, {http_header, _, _, _, _}} ->
            recv_hdrs(Sock, Len);
        {http, Sock, {http_response, _, 200, "OK"}} ->
            recv_hdrs(Sock, Len);
        Other ->
            {error, {"unexpected message", Other}}
    end.

recv_body(Sock, Len) ->
    inet:setopts(Sock, [{packet, raw}, {active, false}]),
    gen_tcp:recv(Sock, Len).
