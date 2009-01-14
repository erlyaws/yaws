-module(app_test).
-include("../include/tftest.hrl").
-include("../ibrowse/src/ibrowse.hrl").
-compile(export_all).


%% Way to invoke just one test
start([F]) ->
    ?line {ok, _} = ibrowse:start_link(),
    apply(app_test, F, []).

start() ->
    ?line ok,
    ?line {ok, _} = ibrowse:start_link(),
    test1(),
    test2(),
    test3(),
    server_options_test(),
    sendfile_get().


test1() ->
    io:format("test1\n",[]),
    ?line ok.

test2() ->
    io:format("test2\n",[]),
    blkget(10),
    timer:sleep(10000),
    test3().


blkget(0) ->
     ok;
blkget(I) ->
    spawn(fun() ->
                  {ok, C} = gen_tcp:connect(localhost, 8000, [{active, false}]),
                  gen_tcp:send(C, "GET /2000.txt\n"
                                  "Host: localhost:8000\n\n"),
                  gen_tcp:recv(C, 200),
                  timer:sleep(5000),
                  exit(normal)
          end),
    blkget(I-1).




test3() ->
    io:format("test3\n",[]),
    ?line {ok, "200", _Headers, []} = ibrowse:send_req("http://localhost:8000", [], head),
    ok.

server_options_test() ->
    io:format("server_options_test\n",[]),
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



sendfile_get() ->
    io:format("sendfile_get\n",[]),
    L = lists:seq(1, 20),
    SELF = self(),
    K1 = lists:map(
             fun(_) ->
                     spawn(fun() ->
                                   ?line {ok, "200", _Headers, _} = 
                                       ibrowse:send_req(
                                         "http://localhost:8000/1000.txt", 
                                         [], get),
                                   SELF ! {self(), k1, done}
                           end)
             end, L),

    K2 = lists:map(
             fun(_) ->
                     spawn(fun() ->
                                   ?line {ok, "200", _Headers, _} = 
                                       ibrowse:send_req(
                                         "http://localhost:8000/2000.txt", 
                                         [], get),
                                   SELF ! {self(), k2, done}
                           end)
             end, L),

    K3 = lists:map(
             fun(_) ->
                     spawn(fun() ->
                                   ?line {ok, "200", _Headers, _} = 
                                       ibrowse:send_req(
                                         "http://localhost:8000/3000.txt", 
                                         [], get),
                                   SELF ! {self(), k3, done}
                           end)
             end, L),


    io:format("K3 = ~p~n", [K3]),
    collect(K1, 1, k1),
    collect(K2, 1, k2),
    collect(K3, 1, k3),
    ok.

collect([], _, _) ->
    ok;
collect(L, Count, Tag) ->
    receive 
        {Pid, Tag, done} ->
            io:format("(~p ~p)", [Tag, Count]),
            collect(lists:delete(Pid, L), Count+1, Tag)
    after 30000 ->
            io:format("TIMEOUT ~p\n~p~n",[process_info(self()), L]),
            ?line exit(timeout)
    end.
                    
                
                
                              
    
           






