-module(app_test).
-include("../include/tftest.hrl").
-include_lib("ibrowse/include/ibrowse.hrl").
-include("../../include/yaws.hrl").
-compile(export_all).


%% Way to invoke just one test
start([F]) ->
    ?line {ok, _} = ibrowse:start_link(),
    apply(app_test, F, []),
    ibrowse:stop().

start() ->
    io:format("\n ==== REDIRECT TESTS ==== \n\n", []),
    ?line {ok, _} = ibrowse:start_link(),
    test_default_redirect(),
    test_301_redirect(),
    test_404_redirect(),
    test_bad_redirect(),
    ibrowse:stop().


test_default_redirect() ->
    io:format("default_redirect (302)\n", []),

    %% /default_redirect1 -> /redir (relative-url + append)
    Uri1 = "http://localhost:8000/default_redirect1/index.html",
    ?line {ok, "302", Hdrs1, _} = ibrowse:send_req(Uri1, [], get),
    ?line "http://localhost:8000/redir/default_redirect1/index.html" =
        proplists:get_value("Location", Hdrs1),

    %% /default_redirect2 -> /redir (relative-url + noappend)
    Uri2 = "http://localhost:8000/default_redirect2/index.html",
    ?line {ok, "302", Hdrs2, _} = ibrowse:send_req(Uri2, [], get),
    ?line "http://localhost:8000/redir" =
        proplists:get_value("Location", Hdrs2),

    %% /default_redirect3 -> /redir (absolute-url + append)
    Uri3 = "http://localhost:8000/default_redirect3/index.html",
    ?line {ok, "302", Hdrs3, _} = ibrowse:send_req(Uri3, [], get),
    ?line "http://yaws.hyber.org/default_redirect3/index.html" =
        proplists:get_value("Location", Hdrs3),

    %% /default_redirect4 -> /redir (absolute-url + noappend)
    Uri4 = "http://localhost:8000/default_redirect4/index.html",
    ?line {ok, "302", Hdrs4, _} = ibrowse:send_req(Uri4, [], get),
    ?line "http://yaws.hyber.org/" =
        proplists:get_value("Location", Hdrs4),
    ok.


test_301_redirect() ->
    io:format("301_redirect\n", []),

    %% /301_redirect1 -> /redir (relative-url + append)
    Uri1 = "http://localhost:8000/301_redirect1/index.html",
    ?line {ok, "301", Hdrs1, _} = ibrowse:send_req(Uri1, [], get),
    ?line "http://localhost:8000/redir/301_redirect1/index.html" =
        proplists:get_value("Location", Hdrs1),

    %% /301_redirect2 -> /redir (relative-url + noappend)
    Uri2 = "http://localhost:8000/301_redirect2/index.html",
    ?line {ok, "301", Hdrs2, _} = ibrowse:send_req(Uri2, [], get),
    ?line "http://localhost:8000/redir" =
        proplists:get_value("Location", Hdrs2),

    %% /301_redirect3 -> /redir (absolute-url + append)
    Uri3 = "http://localhost:8000/301_redirect3/index.html",
    ?line {ok, "301", Hdrs3, _} = ibrowse:send_req(Uri3, [], get),
    ?line "http://yaws.hyber.org/301_redirect3/index.html" =
        proplists:get_value("Location", Hdrs3),

    %% /301_redirect4 -> /redir (absolute-url + noappend)
    Uri4 = "http://localhost:8000/301_redirect4/index.html",
    ?line {ok, "301", Hdrs4, _} = ibrowse:send_req(Uri4, [], get),
    ?line "http://yaws.hyber.org/" =
        proplists:get_value("Location", Hdrs4),
    ok.


test_404_redirect() ->
    io:format("404_redirect\n", []),

    Err404 = lists:flatten(["<html><h1>404 ",
                            yaws_api:code_to_phrase(404),
                            "</h1></html>"]),

    %% /404_redirect1 -> default content (append)
    Uri1 = "http://localhost:8000/404_redirect1/index.html",
    ?line {ok, "404", _, Body1} = ibrowse:send_req(Uri1, [], get),
    ?line Err404 = Body1,

    %% /404_redirect2 -> default content (noappend)
    Uri2 = "http://localhost:8000/404_redirect2/index.html",
    ?line {ok, "404", _, Body2} = ibrowse:send_req(Uri2, [], get),
    ?line Err404 = Body2,

    %% /404_redirect3 -> /error404 (append)
    Uri3 = "http://localhost:8000/404_redirect3/index.html",
    ?line {ok, "404", _, Body3} = ibrowse:send_req(Uri3, [], get),
    ?line "/404_redirect3/index.html\n" = Body3,

    %% /404_redirect4 -> /error404 (noappend)
    Uri4 = "http://localhost:8000/404_redirect4/index.html",
    ?line {ok, "404", _, Body4} = ibrowse:send_req(Uri4, [], get),
    ?line "\n" = Body4,
    ok.


test_bad_redirect() ->
    io:format("bad_redirect\n", []),
    Env = #env{debug=false, trace=false, id="test", embedded=false},

    ?line {error, _} =
        yaws_config:load(Env#env{conf={file, "bad_redirect1.conf"}}),
    ?line {error, _} =
        yaws_config:load(Env#env{conf={file, "bad_redirect2.conf"}}),
    ?line {error, _} =
        yaws_config:load(Env#env{conf={file, "bad_redirect3.conf"}}),
    ?line {error, _} =
        yaws_config:load(Env#env{conf={file, "bad_redirect4.conf"}}),
    ok.
