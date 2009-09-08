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
    server_options_test(),
    test1(),
    test2(),
    test3(),
    sendfile_get().


test1() ->
    io:format("test1\n",[]),
    L = lists:seq(1, 100),
    SELF = self(),
    Pids = lists:map(fun(I) -> 
			     spawn(fun() -> slow_client(I, SELF) end)
		     end, L),
    ?line ok = allow_connects(Pids, 5),
    ?line ok = collect_pids(Pids).
    

collect_pids([]) ->
    ok;
collect_pids(Pids) ->
    receive
	{Pid, done} ->
	    collect_pids(lists:delete(Pid, Pids))
    end.



%% max 5 connectors at a time
allow_connects([], _) ->
    io:format("All pids connected \n",[]);
allow_connects(Pids, 0) ->
    receive
	{Pid, connected} ->
	    allow_connects(lists:delete(Pid, Pids), 1)
    end;
allow_connects(Pids, I) ->
    receive 
	{Pid, allow} ->
	    Pid ! allow,
	    allow_connects(Pids, I-1);
	{Pid, connected} ->
	    allow_connects(lists:delete(Pid, Pids), I+1)
    end.



slow_client(I, Top) ->
    Top ! {self(), allow},
    receive 
	allow ->
	    ok
    end,
    ?line {ok, C} = gen_tcp:connect(localhost, 8000, [{active, false}, 
						      {packet, http}]),
    Top ! {self(), connected},
    ?line ok = gen_tcp:send(C, "GET /1000.txt HTTP/1.1\r\n"
			    "Host: localhost:8000\r\n\r\n"),
    ?line {ok, Sz} = get_cont_len(C),
    ?line ok = inet:setopts(C, [binary,{packet, 0}]),
    read_loop(C, I, Sz),
    Top ! {self(), done}.

read_loop(C, _I, Sz) when Sz - 200000 < 0 ->
    ?line ok = gen_tcp:close(C),
    ok;
read_loop(C, I, Sz)  ->
    ?line {ok, B} = gen_tcp:recv(C, 0),
    %%io:format("(I=~p Sz=~p) ", [I, size(B)]),
    timer:sleep(2),
    read_loop(C, I, Sz - size(B)).


get_cont_len(C) ->
    ?line {value, {http_header, _,_,_, LenStr}} = 
	lists:keysearch('Content-Length', 3, tftest:get_headers(C)),
    {ok, list_to_integer(LenStr)}.




test2() ->
    io:format("test2\n",[]),
    blkget(10).


blkget(0) ->
     ok;
blkget(I) ->
    spawn(fun() ->
                  {ok, C} = gen_tcp:connect(localhost, 8000, [{active, false}]),
                  gen_tcp:send(C, "GET /2000.txt HTTP/1.1\r\n"
                                  "Host: localhost:8000\r\n\r\n"),
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
server_options_recv(S, [{http_header,_,'Date',_,_}|_]=Hdrs) ->
    do_server_options_recv(S, Hdrs);
server_options_recv(_S, [Hdr|_Hdrs]) ->
    io:format("unexpected server options HTTP header: ~p~n", [Hdr]),
    error.
do_server_options_recv(S, Hdrs) ->
    {ok, Hdr} = gen_tcp:recv(S, 0, 5000),
    server_options_recv(S, [Hdr|Hdrs]).

-define(SENDFILE_GET_TIMEOUT, 120000).

sendfile_get() ->
    io:format("sendfile_get\n",[]),
    L = lists:seq(1, 5),
    SELF = self(),
    K1 = lists:map(
             fun(_) ->
                     spawn(fun() ->
                                   ?line {ok, "200", _Headers, _} = 
                                       ibrowse:send_req(
                                         "http://localhost:8000/1000.txt", 
                                         [], get, [], [], ?SENDFILE_GET_TIMEOUT),
                                   SELF ! {self(), k1, done}
                           end)
             end, L),

    K2 = lists:map(
             fun(_) ->
                     spawn(fun() ->
                                   ?line {ok, "200", _Headers, _} = 
                                       ibrowse:send_req(
                                         "http://localhost:8000/2000.txt", 
                                         [], get, [], [], ?SENDFILE_GET_TIMEOUT),
                                   SELF ! {self(), k2, done}
                           end)
             end, L),

    K3 = lists:map(
             fun(_) ->
                     spawn(fun() ->
                                   ?line {ok, "200", _Headers, _} = 
                                       ibrowse:send_req(
                                         "http://localhost:8000/3000.txt", 
                                         [], get, [], [], ?SENDFILE_GET_TIMEOUT),
                                   SELF ! {self(), k3, done}
                           end)
             end, L),


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
    after ?SENDFILE_GET_TIMEOUT ->
            io:format("TIMEOUT ~p\n~p~n",[process_info(self()), L]),
            ?line exit(timeout)
    end.
