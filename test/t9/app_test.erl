-module(app_test).
-compile(export_all).

-include("yaws.hrl").
-include_lib("ibrowse/include/ibrowse.hrl").
-include("tftest.hrl").


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
    test_redirect_url_encode(),
    test_redirect_querystring(),
    test_redirect_post(),
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

    %% /404_redirect3 -> /error404.yaws (append)
    Uri3 = "http://localhost:8000/404_redirect3/index.html",
    ?line {ok, "404", _, Body3} = ibrowse:send_req(Uri3, [], get),
    ?line "/error404.yaws/404_redirect3/index.html\n" = Body3,

    %% /404_redirect4 -> /error404.yaws (noappend)
    Uri4 = "http://localhost:8000/404_redirect4/index.html",
    ?line {ok, "404", _, Body4} = ibrowse:send_req(Uri4, [], get),
    ?line "/error404.yaws\n" = Body4,
    ok.


test_bad_redirect() ->
    io:format("bad_redirect\n", []),
    Env = #env{debug=false, trace=false, id="test", embedded=false},

    ?line {error, _} =
        yaws_config:load(Env#env{conf={file, ?srcdir ++ "/bad_redirect1.conf"}}),
    ?line {error, _} =
        yaws_config:load(Env#env{conf={file, ?srcdir ++ "/bad_redirect2.conf"}}),
    ?line {error, _} =
        yaws_config:load(Env#env{conf={file, ?srcdir ++ "/bad_redirect3.conf"}}),
    ?line {error, _} =
        yaws_config:load(Env#env{conf={file, ?srcdir ++ "/bad_redirect4.conf"}}),
    ok.


test_redirect_url_encode() ->
    io:format("test_redirect_url_encode\n", []),

    %% /redirect?url?encode1 -> /redir? (append)
    Uri1 = "http://localhost:8000/redirect%3Furl%3Fencode1/index.html",
    ?line {ok, "302", Hdrs1, _} = ibrowse:send_req(Uri1, [], get),
    ?line "http://localhost:8000/redir%3F/redirect%3Furl%3Fencode1/index.html" =
        proplists:get_value("Location", Hdrs1),

    %% /redirect?url?encode2 -> /redir? (noappend)
    Uri2 = "http://localhost:8000/redirect%3Furl%3Fencode2/index.html",
    ?line {ok, "302", Hdrs2, _} = ibrowse:send_req(Uri2, [], get),
    ?line "http://localhost:8000/redir%3F" =
        proplists:get_value("Location", Hdrs2),
    ok.


test_redirect_querystring() ->
    io:format("test_redirect_querystring\n", []),

    %% /default_redirect1 -> /redir (relative-url + append)
    Uri1 = "http://localhost:8000/default_redirect1/index.html?a=b&c=d",
    ?line {ok, "302", Hdrs1, _} = ibrowse:send_req(Uri1, [], get),
    ?line "http://localhost:8000/redir/default_redirect1/index.html?a=b&c=d" =
        proplists:get_value("Location", Hdrs1),

    %% /default_redirect2 -> /redir (relative-url + noappend)
    Uri2 = "http://localhost:8000/default_redirect2/index.html?a=b&c=d",
    ?line {ok, "302", Hdrs2, _} = ibrowse:send_req(Uri2, [], get),
    ?line "http://localhost:8000/redir?a=b&c=d" =
        proplists:get_value("Location", Hdrs2),

    %% /default_redirect3 -> /redir (absolute-url + append)
    Uri3 = "http://localhost:8000/default_redirect3/index.html?a=b&c=d",
    ?line {ok, "302", Hdrs3, _} = ibrowse:send_req(Uri3, [], get),
    ?line "http://yaws.hyber.org/default_redirect3/index.html?a=b&c=d" =
        proplists:get_value("Location", Hdrs3),

    %% /default_redirect4 -> /redir (absolute-url + noappend)
    Uri4 = "http://localhost:8000/default_redirect4/index.html?a=b&c=d",
    ?line {ok, "302", Hdrs4, _} = ibrowse:send_req(Uri4, [], get),
    ?line "http://yaws.hyber.org/?a=b&c=d" =
        proplists:get_value("Location", Hdrs4),

    %% /404_redirect3 -> /error404.yaws (append)
    Uri5 = "http://localhost:8000/404_redirect3/index.html?a=b&c=d",
    ?line {ok, "404", _, Body5} = ibrowse:send_req(Uri5, [], get),
    ?line "/error404.yaws/404_redirect3/index.html?a=b&c=d\n" = Body5,

    %% /404_redirect4 -> /error404.yaws (noappend)
    Uri6 = "http://localhost:8000/404_redirect4/index.html?a=b&c=d",
    ?line {ok, "404", _, Body6} = ibrowse:send_req(Uri6, [], get),
    ?line "/error404.yaws?a=b&c=d\n" = Body6,
    ok.


test_redirect_post() ->
    io:format("test_redirect_post\n", []),

    %% (partial_post_size = 5 / content-length = 11)
    Data  = "foo=1&bar=2",
    Sz    = length(Data),
    Hdrs  = [{content_length, Sz},
             {content_type, "application/x-www-form-urlencoded"}],

    %% Use the same connection for all requests
    {ok, ConnPid} = ibrowse:spawn_worker_process("localhost", 8000),

    %% Data must be flushed on redirect (here, trailing slash is added)
    Uri1 = "http://localhost:8000/test",
    ?line {ok, "302", Hdrs1, _} = ibrowse:send_req_direct(ConnPid, Uri1, Hdrs,
                                                          post, Data, []),
    ?line Uri2 = proplists:get_value("Location", Hdrs1),
    ?line "http://localhost:8000/test/" = Uri2,
    ?line {ok, "200", _, _}  = ibrowse:send_req_direct(ConnPid, Uri2, [], get),

    %% Data must be flushed on redirect (here, on configured redirect)
    Uri3 = "http://localhost:8000/post_redirect1",
    ?line {ok, "302", Hdrs3, _} = ibrowse:send_req_direct(ConnPid, Uri3, Hdrs,
                                                          post, Data, []),
    ?line Uri4 = proplists:get_value("Location", Hdrs3),
    ?line "http://localhost:8000/test/index.yaws" = Uri4,
    ?line {ok, "200", _, _}  = ibrowse:send_req_direct(ConnPid, Uri4, [], get),


    %% Data must be readable by /test/index.yaws
    Uri5 = "http://localhost:8000/post_redirect2",
    ?line {ok, "200", _, Body5} = ibrowse:send_req_direct(ConnPid, Uri5, Hdrs,
                                                          post, Data, []),
    ?line Body5 = Data ++ "\n",

    ibrowse:stop_worker_process(ConnPid),

    ok.
