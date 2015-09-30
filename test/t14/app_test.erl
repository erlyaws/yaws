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
    test_missing_host_header(),
    test_multiple_host_header(),
    ibrowse:stop().

test_missing_host_header() ->
    io:format("missing Host header returns 400\n",[]),
    {ok,S} = gen_tcp:connect("localhost", 8000, [binary, {active,false}]),
    ok = gen_tcp:send(S, "GET / HTTP/1.1\r\n\r\n"),
    ok = do_recv(S, <<"HTTP/1.1 400 Bad Request\r\n">>),
    gen_tcp:close(S),
    ok.

test_multiple_host_header() ->
    io:format("multiple Host headers returns 400\n",[]),
    {ok,S} = gen_tcp:connect("localhost", 8000, [binary, {active,false}]),
    ok = gen_tcp:send(S, ["GET / HTTP/1.1\r\n",
                          "Host: localhost\r\n",
                          "Host: foo\r\n\r\n"]),
    ok = do_recv(S, <<"HTTP/1.1 400 Bad Request\r\n">>),
    gen_tcp:close(S),
    ok.

do_recv(Sock, Expected) ->
    Size = byte_size(Expected),
    inet:setopts(Sock, [{active,once}]),
    receive
        {tcp_closed,_} ->
            do_recv(Sock, Expected);
        {tcp,_,Result} ->
            ?line <<Expected:Size/binary, _/binary>> = Result;
        Wrong ->
            error({{expected,Expected},{received,Wrong}})
    end,
    ok.
