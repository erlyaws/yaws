-module(app_test).
-include("../include/tftest.hrl").
-include("../ibrowse/src/ibrowse.hrl").
-compile(export_all).

start() ->
    ?line ok,
    ?line {ok, _} = ibrowse:start_link(),
    test1(),
    test2(),
    test3(),
    server_options_test().


test1() ->
    ?line ok.

test2() ->
    ?line ok.

test3() ->
    ?line {ok, "200", _Headers, []} = ibrowse:send_req("http://localhost:8000", [], head),
    ok.

server_options_test() ->
    {ok, S} = gen_tcp:connect("localhost", 8000, [{packet, raw}, list, {active, false}]),
    ok = gen_tcp:send(S, "OPTIONS * HTTP/1.1\r\nHost: localhost\r\n\r\n"),
    inet:setopts(S, [{packet, http}]),
    ?line ok = server_options_recv(S),
    gen_tcp:close(S).

server_options_recv(S) ->
    do_server_options_recv(S, []).
server_options_recv(_S, [http_eoh|_]) ->
    ok;
server_options_recv(S, [{http_response,{1,1},200,"OK"}|_]=Hdrs) ->
    do_server_options_recv(S, Hdrs);
server_options_recv(S, [{http_header,_,'Server',_,_}|_]=Hdrs) ->
    do_server_options_recv(S, Hdrs);
server_options_recv(S, [{http_header,_,'Allow',_,"GET, HEAD, OPTIONS, PUT, POST, DELETE"}|_]=Hdrs) ->
    do_server_options_recv(S, Hdrs);
server_options_recv(S, [{http_header,_,'Content-Length',_,"0"}|_]=Hdrs) ->
    do_server_options_recv(S, Hdrs);
server_options_recv(_S, _Hdrs) ->
    error.
do_server_options_recv(S, Hdrs) ->
    {ok, Hdr} = gen_tcp:recv(S, 0, 2000),
    server_options_recv(S, [Hdr|Hdrs]).
