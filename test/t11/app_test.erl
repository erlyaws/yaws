-module(app_test).
-include("../include/tftest.hrl").
-compile(export_all).

%% Way to invoke just one test
start([F]) ->
    ?line {ok, _} = ibrowse:start_link(),
    apply(app_test, F, []),
    ibrowse:stop().


start() ->
    io:format("~n ==== SRC_DIR TESTS ==== ~n~n", []),
    ?line {ok, _} = ibrowse:start_link(),
    test_srcdir_v1(),
    test_srcdir_reload(),
    ibrowse:stop().


%% ----
test_srcdir_v1() ->
    io:format("srcdir_v1~n",[]),
    Uri = "http://localhost:8000/srcdir_test",
    Res = "1.0",
    ?line {ok, "200", _, Content} = ibrowse:send_req(Uri, [], get),
    ?line Res = Content,
    ok.

test_srcdir_reload() ->
    io:format("srcdir_reload~n",[]),
    Uri = "http://localhost:8000/srcdir_test",
    Res = "2.0",

    os:cmd("make inc_v2"),

    ?line {ok, "200", _, Content} = ibrowse:send_req(Uri, [], get),
    ?line Res = Content,
    ok.
