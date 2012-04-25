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
    io:format("\n ==== AUTH TESTS ==== \n\n", []),
    ?line {ok, _} = ibrowse:start_link(),
    test1(),
    test2(),
    test3(),
    test4(),
    test5(),
    test6(),
    test7(),
    test8(),
    test9(),
    test10(),
    ibrowse:stop().


test1() ->
    io:format("test1\n", []),
    Uri = "http://localhost:8000/test1/a.txt",
    ?line {ok, "401", Hdrs, _} = ibrowse:send_req(Uri, [], get),
    ?line "Basic realm=\"test1\"" = proplists:get_value("WWW-Authenticate", Hdrs),
    Opts1 = [{basic_auth, {"foo", "baz"}}],
    ?line {ok, "401", _, _} = ibrowse:send_req(Uri, [], get, [], Opts1),
    Opts2 = [{basic_auth, {"foo", "bar"}}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, [], get, [], Opts2),
    ok.


test2() ->
    io:format("test2\n", []),
    Uri1 = "http://localhost:8000/test2/a.txt",
    ?line {ok, "401", Hdrs, _} = ibrowse:send_req(Uri1, [], get),
    ?line "Basic realm=\"test2\"" = proplists:get_value("WWW-Authenticate", Hdrs),

    Uri2 = "http://localhost:8000/test2/b.txt",
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri2, [], get),
    ok.


test3() ->
    io:format("test3\n", []),
    Uri = "http://localhost:8000/test3/sub/a.txt",
    ?line {ok, "401", Hdrs, _} = ibrowse:send_req(Uri, [], get),
    ?line "Basic realm=\"test3\"" = proplists:get_value("WWW-Authenticate", Hdrs),
    Opts2 = [{basic_auth, {"foo", "bar"}}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, [], get, [], Opts2),
    ok.


test4() ->
    io:format("test4\n", []),
    Uri = "http://localhost:8000/test4/a.txt",
    ?line {ok, "401", Hdrs, _} = ibrowse:send_req(Uri, [], get),
    ?line "Basic realm=\"test4\"" = proplists:get_value("WWW-Authenticate", Hdrs),
    Opts = [{basic_auth, {"foo", "bar"}}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, [], get, [], Opts),
    ok.


test5() ->
    io:format("test5\n", []),
    Uri = "http://localhost:8000/test5/a.txt",
    ?line {ok, "200", Hdrs, "test5"} = ibrowse:send_req(Uri, [], get),
    ?line "true" = proplists:get_value("X-Outmod-Test", Hdrs),
    ?line "Basic realm=\"test5\"" = proplists:get_value("WWW-Authenticate", Hdrs),
    ok.


test6() ->
    io:format("test6\n", []),
    Uri = "http://localhost:8000/test6/a.txt",
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, [], get),
    ok.


test7() ->
    io:format("test7\n", []),
    Uri1 = "http://localhost:8000/test7/a.txt",
    ?line {ok, "403", _, _} = ibrowse:send_req(Uri1, [], get),
    Uri2 = "http://localhost:8000/test7/b.txt",
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri2, [], get),
    ok.


test8() ->
    io:format("test8\n", []),
    Uri1 = "http://localhost:8000/test8/a.txt",
    ?line {ok, "403", _, _} = ibrowse:send_req(Uri1, [], get),
    Uri2 = "http://localhost:8000/test8/b.txt",
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri2, [], get),
    ok.

test9() ->
    io:format("test9\n", []),
    Uri = "http://localhost:8000/test9/a.txt",
    Opts = [{basic_auth, {"foo", "baz"}}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, [], get, [], Opts),
    ok.


test10() ->
    io:format("test10\n", []),
    Opts = [{basic_auth, {"foo", "bar"}}],

    Uri1 = "http://localhost:8000/test10/a.txt",
    ?line {ok, "401", Hdrs1, _} = ibrowse:send_req(Uri1, [], get),
    ?line "Basic realm=\"test10\"" = proplists:get_value("WWW-Authenticate", Hdrs1),
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri1, [], get, [], Opts),

    Uri2 = "http://localhost:8000/test10/b.txt",
    ?line {ok, "401", Hdrs2, _} = ibrowse:send_req(Uri2, [], get),
    ?line "Basic realm=\"test10\"" = proplists:get_value("WWW-Authenticate", Hdrs2),
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri2, [], get, [], Opts),

    Uri3 = "http://localhost:8000/test10/c.txt",
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri3, [], get),
    ok.

