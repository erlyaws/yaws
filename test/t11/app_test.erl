-module(app_test).
-compile(export_all).

-include("tftest.hrl").

%% Way to invoke just one test
start([F]) ->
    ?line {ok, _} = ibrowse:start_link(),
    apply(app_test, F, []),
    ibrowse:stop().


start() ->
    io:format("~n ==== SRC_DIR TESTS ==== ~n~n", []),
    ?line {ok, _} = ibrowse:start_link(),
    test_srcdir_v1(),
    test_srcdir_v2(),
    test_srcdir_invalid(),
    ibrowse:stop().


%% ----
test_srcdir_v1() ->
    io:format("srcdir_v1~n",[]),
    Uri = "http://localhost:8000/srcdir_test",

    Res = "1.0",
    ?line {ok, "200", _, Content} = ibrowse:send_req(Uri, [], get),
    ?line Res = Content,
    ok.

test_srcdir_v2() ->
    io:format("srcdir_v2~n",[]),
    Uri = "http://localhost:8000/srcdir_test",

    ?line ok = file:write_file(?builddir ++ "/include/srcdir_test.hrl",
                               <<"-define(SRCDIR_VERSION, \"2.0\").">>),

    {ok, Host} = inet:gethostname(),
    Node       = list_to_atom("test@" ++ Host),
    ?line Pid  = rpc:call(Node, yaws, hup, [undefined]),
    ?line Ref  = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after
        10000 -> ?line throw(timeout)
    end,

    Res = "2.0",
    ?line {ok, "200", _, Content} = ibrowse:send_req(Uri, [], get),
    ?line Res = Content,
    ok.

test_srcdir_invalid() ->
    io:format("srcdir_invalid~n",[]),
    Uri = "http://localhost:8000/srcdir_test",

    ?line ok = file:write_file(?builddir ++ "/include/srcdir_test.hrl",
                               <<"invalid_header">>),

    {ok, Host} = inet:gethostname(),
    Node       = list_to_atom("test@" ++ Host),
    ?line Pid  = rpc:call(Node, yaws, hup, [undefined]),
    ?line Ref  = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after
        10000 -> ?line throw(timeout)
    end,

    Res = "2.0",
    ?line {ok, "200", _, Content} = ibrowse:send_req(Uri, [], get),
    ?line Res = Content,
    ok.
