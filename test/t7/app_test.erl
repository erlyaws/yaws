-module(app_test).
-compile(export_all).

-include_lib("ibrowse/include/ibrowse.hrl").
-include("tftest.hrl").


%% Way to invoke just one test
start([F]) ->
    ?line {ok, _} = ibrowse:start_link(),
    apply(app_test, F, []),
    ibrowse:stop().

start() ->
    io:format("\n ==== MIME-TYPES TESTS ==== \n\n", []),
    ?line {ok, _} = ibrowse:start_link(),
    test_generated_module(),
    test_default_type(),
    test_yaws_type(),
    test_erlang_type(),
    test_gzip_with_charset(),
    ibrowse:stop().


test_generated_module() ->
    io:format("generated_module_test\n", []),
    [_, Host]  = string:tokens(atom_to_list(node()), "@"),
    Node       = list_to_atom("test@" ++ Host),
    Vhost1     = {"localhost:8000", 8000},
    Vhost2     = {"localhost:8001", 8001},

    ?line CInfo = rpc:call(Node,mime_types,module_info,[compile]),
    ?line IdDir = rpc:call(Node,yaws,id_dir,[testid]),
    ?line IdDir = filename:dirname(proplists:get_value(source, CInfo)),

    ?line "text/html" = rpc:call(Node,mime_types,default_type,[]),
    ?line "text/html" = rpc:call(Node,mime_types,default_type,[global]),
    ?line "text/html" = rpc:call(Node,mime_types,default_type,[Vhost1]),
    ?line "text/plain; charset=UTF-8" =
        rpc:call(Node,mime_types,default_type,[Vhost2]),

    ?line {yaws, "text/html"} = rpc:call(Node,mime_types,t,["yaws"]),
    ?line {yaws, "text/html"} = rpc:call(Node,mime_types,t,[global,"yaws"]),
    ?line {yaws, "text/html"} = rpc:call(Node,mime_types,t,[Vhost1,"yaws"]),
    ?line {yaws, "text/xhtml; charset=ISO-8859-1"} =
        rpc:call(Node,mime_types,t,[Vhost2,"yaws"]),

    ?line {regular, "text/plain; charset=UTF-8"} =
        rpc:call(Node,mime_types,t,["tst"]),
    ?line {regular, "text/plain; charset=UTF-8"} =
        rpc:call(Node,mime_types,t,[global,"tst"]),
    ?line {regular, "text/plain; charset=UTF-8"} =
        rpc:call(Node,mime_types,t,[Vhost1,"tst"]),
    ?line {regular, "application/x-test; charset=US-ASCII"} =
        rpc:call(Node,mime_types,t,[Vhost2,"tst"]),

    ?line {regular, "text/html"} =
        rpc:call(Node,mime_types,t,["test"]),
    ?line {regular, "text/html"} =
        rpc:call(Node,mime_types,t,[global,"test"]),
    ?line {regular, "text/html"} =
        rpc:call(Node,mime_types,t,[Vhost1,"test"]),
    ?line {regular, "application/x-test; charset=UTF-8"} =
        rpc:call(Node,mime_types,t,[Vhost2,"test"]),

    ?line {php, "text/html"} =
        rpc:call(Node,mime_types,t,["php"]),
    ?line {php, "text/html"} =
        rpc:call(Node,mime_types,t,[global, "php"]),
    ?line {php, "text/html"} =
        rpc:call(Node,mime_types,t,[Vhost1, "php"]),
    ?line {php, "application/x-httpd-php"} =
        rpc:call(Node,mime_types,t,[Vhost2,"php"]),
    ?line {php, "application/x-httpd-php"} =
        rpc:call(Node,mime_types,t,[Vhost2,"PHP"]),
    ?line {regular, "php5", "application/x-httpd-php5"} =
        rpc:call(Node,mime_types,revt,[Vhost2,"5php"]),
    ?line {regular, "PHP5", "application/x-httpd-php5"} =
        rpc:call(Node,mime_types,revt,[Vhost2,"5PHP"]),

    ?line {regular, "text/plain"} =
        rpc:call(Node,mime_types,t,["html"]),
    ?line {regular, "text/plain"} =
        rpc:call(Node,mime_types,t,[global,"html"]),
    ?line {regular, "text/plain"} =
        rpc:call(Node,mime_types,t,[Vhost1,"html"]),
    ?line {regular, "text/plain; charset=UTF-8"} =
        rpc:call(Node,mime_types,t,[Vhost2,"html"]),

    ok.

test_default_type() ->
    io:format("default_type_test\n", []),
    Uri1 = "http://localhost:8000/news",
    Uri2 = "http://localhost:8001/news",
    ?line {ok, "200", Hdrs1, _} = ibrowse:send_req(Uri1, [], get),
    ?line {ok, "200", Hdrs2, _} = ibrowse:send_req(Uri2, [], get),
    ?line "text/html" = proplists:get_value("Content-Type", Hdrs1),
    ?line "text/plain; charset=UTF-8" = proplists:get_value("Content-Type", Hdrs2),
    ok.

test_yaws_type() ->
    io:format("yaws_type_test\n", []),
    Uri1 = "http://localhost:8000/index.yaws",
    Uri2 = "http://localhost:8001/index.yaws",
    ?line {ok, "200", Hdrs1, _} = ibrowse:send_req(Uri1, [], get),
    ?line {ok, "200", Hdrs2, _} = ibrowse:send_req(Uri2, [], get),
    ?line "text/html" = proplists:get_value("Content-Type", Hdrs1),
    ?line "text/xhtml; charset=ISO-8859-1" = proplists:get_value("Content-Type", Hdrs2),
    ok.

test_erlang_type() ->
    io:format("erlang_type_test\n", []),
    Uri1 = "http://localhost:8000/code/myappmod.erl",
    Uri2 = "http://localhost:8001/code/myappmod.erl",
    ?line {ok, "200", Hdrs1, _} = ibrowse:send_req(Uri1, [], get),
    ?line {ok, "200", Hdrs2, _} = ibrowse:send_req(Uri2, [], get),
    ?line "text/html" = proplists:get_value("Content-Type", Hdrs1),
    ?line "text/x-erlang; charset=UTF-8" = proplists:get_value("Content-Type", Hdrs2),
    ok.


test_gzip_with_charset() ->
    io:format("test_gzip_with_charset\n", []),
    Uri = "http://localhost:8001/index.yaws",
    ?line {ok, "200", Hdrs, _} =
        ibrowse:send_req(Uri, [{"Accept-Encoding", "gzip, deflate"}], get),
    ?line "text/xhtml; charset=ISO-8859-1" = proplists:get_value("Content-Type", Hdrs),
    ?line "gzip" = proplists:get_value("Content-Encoding", Hdrs),
    ok.
