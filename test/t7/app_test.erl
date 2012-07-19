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
    io:format("\n ==== MIME-TYPES TESTS ==== \n\n", []),
    ?line {ok, _} = ibrowse:start_link(),
    generated_module_test(),
    default_type_test(),
    yaws_type_test(),
    erlang_type_test(),
    ibrowse:stop().


generated_module_test() ->
    io:format("generated_module_test\n", []),
    {ok, Host} = inet:gethostname(),
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

default_type_test() ->
    io:format("default_type_test\n", []),
    Uri1 = "http://localhost:8000/news",
    Uri2 = "http://localhost:8001/news",
    ?line {ok, "200", Hdrs1, _} = ibrowse:send_req(Uri1, [], get),
    ?line {ok, "200", Hdrs2, _} = ibrowse:send_req(Uri2, [], get),
    ?line "text/html" = proplists:get_value("Content-Type", Hdrs1),
    ?line "text/plain; charset=UTF-8" = proplists:get_value("Content-Type", Hdrs2),
    ok.

yaws_type_test() ->
    io:format("yaws_type_test\n", []),
    Uri1 = "http://localhost:8000/index.yaws",
    Uri2 = "http://localhost:8001/index.yaws",
    ?line {ok, "200", Hdrs1, _} = ibrowse:send_req(Uri1, [], get),
    ?line {ok, "200", Hdrs2, _} = ibrowse:send_req(Uri2, [], get),
    ?line "text/html" = proplists:get_value("Content-Type", Hdrs1),
    ?line "text/xhtml; charset=ISO-8859-1" = proplists:get_value("Content-Type", Hdrs2),
    ok.

erlang_type_test() ->
    io:format("erlang_type_test\n", []),
    Uri1 = "http://localhost:8000/code/myappmod.erl",
    Uri2 = "http://localhost:8001/code/myappmod.erl",
    ?line {ok, "200", Hdrs1, _} = ibrowse:send_req(Uri1, [], get),
    ?line {ok, "200", Hdrs2, _} = ibrowse:send_req(Uri2, [], get),
    ?line "text/html" = proplists:get_value("Content-Type", Hdrs1),
    ?line "text/x-erlang; charset=UTF-8" = proplists:get_value("Content-Type", Hdrs2),
    ok.
