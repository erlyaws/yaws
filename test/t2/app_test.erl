-module(app_test).
-include("../include/tftest.hrl").
-include("../ibrowse/src/ibrowse.hrl").
-compile(export_all).

start() ->
    ?line ok,
    ?line {ok, _} = ibrowse:start_link(),
    test1(),
    test2(),
    test3().


test1() ->
    ?line ok.

test2() ->
    ?line ok.

test3() ->
    ?line {ok, "200", Headers, []} = ibrowse:send_req("http://localhost:8000", [], head),
    ok.




    


