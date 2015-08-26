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
    io:format("\n ==== MAIN TESTS ==== \n\n", []),
    ?line {ok, _} = ibrowse:start_link(),
    test_max_connections(),
    ibrowse:stop().

test_max_connections() ->
    io:format("max connections\n",[]),
    spawn(fun() ->
                  {ok,"204",_,_} = ibrowse:send_req("http://localhost:8000",
                                                    [], get),
                  exit(normal)
          end),
    timer:sleep(1000),
    {error, retry_later} = ibrowse:send_req("http://localhost:8000", [], get),
    ok.
