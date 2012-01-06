-module(app_test).
-include("../include/tftest.hrl").
-include("../ibrowse/src/ibrowse.hrl").
-compile(export_all).


%% Way to invoke just one test
start([F]) ->
    ?line {ok, _} = ibrowse:start_link(),
    apply(app_test, F, []),
    ibrowse:stop().

start() ->
    ?line ok,
    ?line {ok, _} = ibrowse:start_link(),
    server_options_test(),
    test1(),
    test2(),
    test3(),
    appmod_test(),
    streamcontent_test(),
    sendfile_get(),
    json_test(),
    post_test(),
    flush_test(),
    expires_test(),
    reentrant_test(),
    cgi_redirect_test(),
    php_handler_test(),
    arg_rewrite_test(),
    shaper_test(),
    ibrowse:stop().


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
    io:format("  test1: all pids connected \n",[]);
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
    ?line {ok, "200", _Headers, []} = ibrowse:send_req("http://localhost:8000",
                                                       [], head),
    ok.

server_options_test() ->
    io:format("server_options_test\n",[]),
    {ok, S} = gen_tcp:connect("localhost", 8000, [{packet, raw}, list,
                                                  {active, false}]),
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
    io:format("\n",[]),
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

-define(APPMOD_HEADER, "Appmod-Called").

appmod_test() ->
    io:format("appmod_test\n",[]),
    Uri1 = "http://localhost:8002/",
    ?line {ok, "200", Headers1, _} = ibrowse:send_req(Uri1, [], get),
    ?line "true" = proplists:get_value(?APPMOD_HEADER, Headers1),
    Uri2 = "http://localhost:8003/",
    ?line {ok, "200", Headers2, _} = ibrowse:send_req(Uri2, [], get),
    ?line "true" = proplists:get_value(?APPMOD_HEADER, Headers2),
    Uri3 = "http://localhost:8003/icons/layout.gif",
    ?line {ok, "200", Headers3, _} = ibrowse:send_req(Uri3, [], get),
    ?line false = proplists:get_value(?APPMOD_HEADER, Headers3, false),
    Uri4 = "http://localhost:8004/non_root_appmod",
    ?line {ok, "200", Headers4, _} = ibrowse:send_req(Uri4, [], get),
    ?line "true" = proplists:get_value(?APPMOD_HEADER, Headers4),
    ok.

streamcontent_test() ->
    io:format("streamcontent_test\n",[]),
    Uri1 = "http://localhost:8000/streamtest/1",
    ?line {ok, "200", Headers1, Body1} = ibrowse:send_req(Uri1, [], get),
    ?line "chunked" = proplists:get_value("Transfer-Encoding", Headers1),
    ?line Body1 = "this is an iolist",

    %% The following test attempts to ensure that Yaws doesn't report the
    %% following problem due to the application closing the socket and then
    %% handing it back to Yaws via stream_process_end.
    %%
    %% =ERROR REPORT==== 12-May-2010::00:27:05 ===
    %% Yaws process died: {{badmatch,{error,einval}},
    %%                     [{yaws,setopts,3},
    %%                      {yaws,http_recv_request,2},
    %%                      {yaws,do_http_get_headers,2},
    %%                      {yaws,http_get_headers,2},
    %%                      {yaws_server,aloop,3},
    %%                      {yaws_server,acceptor0,2},
    %%                      {proc_lib,init_p_do_apply,3}]}
    %%
    %% The test uses plain sockets because closing the remote end makes
    %% ibrowse unhappy. Unfortunately the only way to currently check that
    %% the above message doesn't appear is to turn on traffic tracing in
    %% yaws.conf and then visually check the file logs/report.log.
    %%
    Path = "/streamtest/2",
    {ok, Sock} = gen_tcp:connect("localhost", 8000, [binary, {active, false}]),
    gen_tcp:send(Sock, "GET " ++ Path ++ " HTTP/1.1\r\nHost: localhost\r\n\r\n"),
    inet:setopts(Sock, [{packet, http}]),
    {ok, Len} = recv_hdrs(Sock),
    inet:setopts(Sock, [{packet, raw}, {active, false}]),
    {ok, <<"closing the socket">>} = gen_tcp:recv(Sock, Len),
    timer:sleep(10000),
    gen_tcp:close(Sock),
    ok.

-define(JSON_URI, "http://localhost:8005/jsontest").

json_test() ->
    io:format("json_test\n",[]),
    io:format("  param array1\n", []),
    ?line ok = do_json({struct, [{"jsonrpc", "2.0"},
                                 {"method", "subtract"},
                                 {"params", {array, [42, 23]}},
                                 {"id", 1}]},
                       {struct, [{"jsonrpc", "2.0"},
                                 {"result", 19},
                                 {"id", 1}]}),
    io:format("  param array2\n", []),
    ?line ok = do_json({struct, [{"jsonrpc", "2.0"},
                                 {"method", "subtract"},
                                 {"params", {array, [23, 42]}},
                                 {"id", 2}]},
                       {struct, [{"jsonrpc", "2.0"},
                                 {"result", -19},
                                 {"id", 2}]}),
    inets:start(),
    io:format("  encode/decode\n", []),
    ?line {ok,{response,[19]}} = jsonrpc:call(?JSON_URI, [],
                                              {call, "subtract", [42, 23]}),
    io:format("  param obj1\n", []),
    ?line ok = do_json({struct, [{"jsonrpc", "2.0"},
                                 {"method", "subtract"},
                                 {"params", {struct, [{"subtrahend", 23},
                                                      {"minuend", 42}]}},
                                 {"id", 3}]},
                       {struct, [{"jsonrpc", "2.0"},
                                 {"result", 19},
                                 {"id", 3}]}),
    io:format("  param obj2\n", []),
    ?line ok = do_json({struct, [{"jsonrpc", "2.0"},
                                 {"method", "subtract"},
                                 {"params", {struct, [{"minuend", 42},
                                                      {"subtrahend", 23}]}},
                                 {"id", 4}]},
                       {struct, [{"jsonrpc", "2.0"},
                                 {"result", 19},
                                 {"id", 4}]}),
    io:format("  notif1\n", []),
    ?line ok = do_json({struct, [{"jsonrpc", "2.0"},
                                 {"method", "update"},
                                 {"params", {array, [1,2,3,4,5]}}]},
                       notification),
    io:format("  notif2\n", []),
    ?line ok = do_json({struct, [{"jsonrpc", "2.0"},
                                 {"method", "foobar"}]},
                       notification),
    io:format("  missing method\n", []),
    ?line ok = do_json({struct, [{"jsonrpc", "2.0"},
                                 {"method", "foobar"},
                                 {"id", "1"}]},
                       {struct, [{"jsonrpc", "2.0"},
                                 {"id", "1"},
                                 {"error", {struct,
                                            [{"code", -32601},
                                             {"message", "method not found"}]}
                                  }]}),
    io:format("  invalid json\n", []),
    InvalidJson = "{\"jsonrpc\": \"2.0\", \"method\": \"foobar,"
        "\"params\": \"bar\", \"baz]",
    ?line ok = do_json(InvalidJson,
                       {struct, [{"jsonrpc", "2.0"},
                                 {"id", null},
                                 {"error", {struct,
                                            [{"code", -32700},
                                             {"message", "parse error"}]}
                                  }]},
                       no_encode),
    io:format("  invalid req1\n", []),
    InvalidReq1 = "{\"jsonrpc\": \"2.0\", \"method\": 1, \"params\": \"bar\"}",
    ?line ok = do_json(InvalidReq1,
                       {struct, [{"jsonrpc", "2.0"},
                                 {"id", null},
                                 {"error", {struct,
                                            [{"code", -32600},
                                             {"message", "invalid request"}]}
                                  }]},
                       no_encode),
    io:format("  invalid params\n", []),
    InvalidReq2 = "{\"jsonrpc\": \"2.0\", \"method\": \"x\","
        "\"params\": \"bar\"}",
    ?line ok = do_json(InvalidReq2,
                       {struct, [{"jsonrpc", "2.0"},
                                 {"id", null},
                                 {"error", {struct,
                                            [{"code", -32602},
                                             {"message", "invalid params"}]}
                                  }]},
                       no_encode),
    io:format("  invalid batch json\n", []),
    InvalidJsonBatch = "[ {\"jsonrpc\": \"2.0\", \"method\": \"sum\","
        "\"params\": [1,2,4],\"id\": \"1\"},{\"jsonrpc\": \"2.0\", \"method\" ]",
    ?line ok = do_json(InvalidJsonBatch,
                       {struct, [{"jsonrpc", "2.0"},
                                 {"id", null},
                                 {"error", {struct,
                                            [{"code", -32700},
                                             {"message", "parse error"}]}
                                  }]},
                       no_encode),
    io:format("  empty batch\n", []),
    EmptyBatch = "[]",
    ?line ok = do_json(EmptyBatch,
                       {struct, [{"jsonrpc", "2.0"},
                                 {"id", null},
                                 {"error", {struct,
                                            [{"code", -32600},
                                             {"message", "invalid request"}]}
                                  }]},
                       no_encode),
    io:format("  invalid batch1\n", []),
    BogusBatch1 = "[1]",
    ?line ok = do_json(BogusBatch1,
                       {array,
                        [{struct, [{"jsonrpc", "2.0"},
                                   {"id", null},
                                   {"error", {struct,
                                              [{"code", -32600},
                                               {"message", "invalid request"}]}
                                   }]}]},
                       no_encode),
    io:format("  invalid batch2\n", []),
    BogusBatch2 = "[1,2,3]",
    ?line ok = do_json(BogusBatch2,
                       {array,
                        [{struct, [{"jsonrpc", "2.0"},
                                   {"id", null},
                                   {"error", {struct,
                                              [{"code", -32600},
                                               {"message", "invalid request"}]}
                                   }]},
                         {struct, [{"jsonrpc", "2.0"},
                                   {"id", null},
                                   {"error", {struct,
                                              [{"code", -32600},
                                               {"message", "invalid request"}]}
                                   }]},
                         {struct, [{"jsonrpc", "2.0"},
                                   {"id", null},
                                   {"error", {struct,
                                              [{"code", -32600},
                                               {"message", "invalid request"}]}
                                   }]}]},
                       no_encode),
    io:format("  mixed batch\n", []),
    MixedBatch = "
    [{\"jsonrpc\":\"2.0\",\"method\":\"sum\",\"params\": [1,2,4], \"id\": \"1\"},
     {\"jsonrpc\":\"2.0\",\"method\":\"notify_hello\", \"params\": [7]},
     {\"jsonrpc\":\"2.0\",\"method\":\"subtract\",\"params\":[42,23],
      \"id\":\"2\"},
     {\"foo\": \"boo\"},
     {\"jsonrpc\":\"2.0\",\"method\":\"foo.get\",
      \"params\":{\"name\": \"myself\"}, \"id\": \"5\"},
     {\"jsonrpc\": \"2.0\", \"method\": \"get_data\", \"id\": \"9\"}]",
    ?line ok = do_json(MixedBatch,
                       {array,
                        [{struct,[{"jsonrpc","2.0"},
                                  {"result",7},
                                  {"id","1"}]},
                         {struct,[{"jsonrpc","2.0"},
                                  {"result",19},{"id","2"}]},
                         {struct,[{"jsonrpc","2.0"},
                                  {"error",
                                   {struct,[{"code",-32600},
                                            {"message","invalid request"}]}},
                                  {"id",null}]},
                         {struct,[{"jsonrpc","2.0"},
                                  {"error",
                                   {struct,[{"code",-32601},
                                            {"message","method not found"}]}},
                                  {"id","5"}]},
                         {struct,[{"jsonrpc","2.0"},
                                  {"result",{array,["hello",5]}},
                                  {"id","9"}]}]},
                       no_encode),
    io:format("  all-notification batch\n", []),
    NotifBatch = "[
        {\"jsonrpc\": \"2.0\", \"method\": \"notify_sum\", \"params\": [1,2,4]},
        {\"jsonrpc\": \"2.0\", \"method\": \"notify_hello\", \"params\": [7]}]",
    ?line ok = do_json(NotifBatch, notification, no_encode),
    ok.

do_json(Req, Expected) ->
    do_json(Req, Expected, encode).
do_json(Req, notification, NeedEncode) ->
    ?line {ok, "200", Headers, Body} = json_send(Req, NeedEncode),
    ?line "application/json" = proplists:get_value("Content-Type", Headers),
    ?line [] = Body,
    ok;
do_json(Req, {struct, _}=Expected, NeedEncode) ->
    ?line {ok, "200", Headers, Body} = json_send(Req, NeedEncode),
    ?line "application/json" = proplists:get_value("Content-Type", Headers),
    check_json(Expected, Body, true);
do_json(Req, {array, Array}, NeedEncode) ->
    ?line {ok, "200", Headers, Body} = json_send(Req, NeedEncode),
    ?line "application/json" = proplists:get_value("Content-Type", Headers),
    ?line {ok, {array, GotArray}} = json2:decode_string(Body),
    lists:map(fun({Obj, Got}) ->
                      ?line ok = check_json(Obj, Got, false)
              end, lists:zip(Array, GotArray)),
    ok.

check_json({struct, _}=Exp, Body, true) ->
    ?line {ok, DecodedBody} = json2:decode_string(Body),
    check_json(Exp, DecodedBody);
check_json({struct, _}=Exp, Body, false) ->
    check_json(Exp, Body).
check_json({struct, Members}, DecodedBody) ->
    lists:foreach(fun({Key, Val}) ->
                          ?line Val = jsonrpc:s(DecodedBody, Key)
                  end, Members),
    ok.

json_send(Req) ->
    json_send(Req, encode).
json_send(Req, encode) ->
    json_send(json2:encode(Req), no_encode);
json_send(Req, no_encode) ->
    Uri = ?JSON_URI,
    ReqHdrs = [{content_type, "application/json"}],
    ibrowse:send_req(Uri, ReqHdrs, post, Req).

recv_hdrs(Sock) ->
    recv_hdrs(Sock, 0).
recv_hdrs(Sock, Len) ->
    inet:setopts(Sock, [{active, once}]),
    receive
        {http, Sock, http_eoh} ->
            {ok, Len};
        {http, Sock, {http_error, Error}} ->
            {error, Error};
        {http, Sock, {http_header, _, 'Content-Length', _, LenStr}} ->
            recv_hdrs(Sock, list_to_integer(LenStr));
        {http, Sock, {http_header, _, _, _, _}} ->
            recv_hdrs(Sock, Len);
        {http, Sock, {http_response, _, 200, "OK"}} ->
            recv_hdrs(Sock, Len);
        Other ->
            {error, {"unexpected message", Other}}
    end.


%% partial_post_size = 2048000
post_test() ->
    io:format("post_test\n",[]),
    small_post(),
    large_post(),
    small_chunked_post(),
    large_chunked_post(),
    ok.

small_post() ->
    io:format("  small post\n",[]),
    {ok, Bin} = file:read_file("../../www/1000.txt"),
    Sz = size(Bin),
    Uri = "http://localhost:8006/posttest/" ++ integer_to_list(Sz),
    Hdrs = [{content_length, Sz}, {content_type, "binary/octet-stream"}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, Hdrs, post, Bin, []),
    ok.

large_post() ->
    io:format("  large post\n",[]),
    {ok, Bin} = file:read_file("../../www/10000.txt"),
    Sz = size(Bin),
    Uri = "http://localhost:8006/posttest/" ++ integer_to_list(Sz),
    Hdrs = [{content_length, Sz}, {content_type, "binary/octet-stream"}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, Hdrs, post, Bin, []),
    ok.

small_chunked_post() ->
    io:format("  small chunked post\n",[]),
    {ok, Bin} = file:read_file("../../www/3000.txt"),
    Sz = size(Bin),
    Uri = "http://localhost:8006/posttest/chunked/" ++ integer_to_list(Sz),
    Hdrs = [{content_type, "binary/octet-stream"}],
    Opts = [{transfer_encoding, {chunked, 1000*1000}}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, Hdrs, post, Bin, Opts),
    ok.

large_chunked_post() ->
    io:format("  large chunked post\n",[]),
    {ok, Bin} = file:read_file("../../www/10000.txt"),
    Sz = size(Bin),
    Uri = "http://localhost:8006/posttest/chunked/" ++ integer_to_list(Sz),
    Hdrs = [{content_type, "binary/octet-stream"}],

    %% size of chunk _IS_NOT_ a multiple of partial_post_size
    Opts1 = [{transfer_encoding, {chunked, 4000*1000}}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, Hdrs, post, Bin, Opts1),

    %% size of chunk _IS_ a multiple of partial_post_size
    Opts2 = [{transfer_encoding, {chunked, 4000*1024}}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, Hdrs, post, Bin, Opts2),
    ok.

flush_test() ->
    io:format("flush_test\n",[]),
    flush_small_post(),
    flush_large_post(),
    flush_chunked_post(),
    flush_small_get(),
    flush_large_get(),
    flush_chunked_get(),
    ok.

flush_small_post() ->
    io:format("  flush small post\n",[]),
    {ok, Bin} = file:read_file("../../www/1000.txt"),
    Sz = size(Bin),
    Uri1 = "http://localhost:8006/flushtest/" ++ integer_to_list(Sz),
    Uri2 = "http://localhost:8006/hello.txt",
    Hdrs = [{content_length, Sz}, {content_type, "binary/octet-stream"}],
    {ok, ConnPid} = ibrowse:spawn_worker_process("localhost", 8006),
    ?line {ok, "200", _, _} = ibrowse:send_req_direct(ConnPid, Uri1, Hdrs, post, Bin, []),
    ?line {ok, "200", _, _} = ibrowse:send_req_direct(ConnPid, Uri2, [], get, [], []),
    ibrowse:stop_worker_process(ConnPid),
    ok.

flush_large_post() ->
    io:format("  flush large post\n",[]),
    {ok, Bin} = file:read_file("../../www/10000.txt"),
    Sz = size(Bin),
    Uri1 = "http://localhost:8006/flushtest/" ++ integer_to_list(Sz),
    Uri2 = "http://localhost:8006/hello.txt",
    Hdrs = [{content_length, Sz}, {content_type, "binary/octet-stream"}],
    {ok, ConnPid} = ibrowse:spawn_worker_process("localhost", 8006),
    ?line {ok, "200", _, _} = ibrowse:send_req_direct(ConnPid, Uri1, Hdrs, post, Bin, []),
    ?line {ok, "200", _, _} = ibrowse:send_req_direct(ConnPid, Uri2, [], get, [], []),
    ibrowse:stop_worker_process(ConnPid),
    ok.

flush_chunked_post() ->
    io:format("  flush chunked post\n",[]),
    {ok, Bin} = file:read_file("../../www/10000.txt"),
    Sz = size(Bin),
    Uri1 = "http://localhost:8006/flushtest/chunked/" ++ integer_to_list(Sz),
    Uri2 = "http://localhost:8006/hello.txt",
    Hdrs = [{content_type, "binary/octet-stream"}],
    Opts = [{transfer_encoding, {chunked, 4000*1000}}],
    {ok, ConnPid} = ibrowse:spawn_worker_process("localhost", 8006),
    ?line {ok, "200", _, _} = ibrowse:send_req_direct(ConnPid, Uri1, Hdrs, post, Bin, Opts),
    ?line {ok, "200", _, _} = ibrowse:send_req_direct(ConnPid, Uri2, [], get, [], []),
    ibrowse:stop_worker_process(ConnPid),
    ok.

flush_small_get() ->
    io:format("  flush small get\n",[]),
    {ok, Bin} = file:read_file("../../www/1000.txt"),
    Sz = size(Bin),
    Uri = "http://localhost:8006/hello.txt",
    Hdrs = [{content_length, Sz}, {content_type, "binary/octet-stream"}],
    {ok, ConnPid} = ibrowse:spawn_worker_process("localhost", 8006),
    ?line {ok, "200", _, _} = ibrowse:send_req_direct(ConnPid, Uri, Hdrs, get, Bin, []),
    ?line {ok, "200", _, _} = ibrowse:send_req_direct(ConnPid, Uri, [], get, [], []),
    ibrowse:stop_worker_process(ConnPid),
    ok.

flush_large_get() ->
    io:format("  flush large get\n",[]),
    {ok, Bin} = file:read_file("../../www/10000.txt"),
    Sz = size(Bin),
    Uri = "http://localhost:8006/hello.txt",
    Hdrs = [{content_length, Sz}, {content_type, "binary/octet-stream"}],
    {ok, ConnPid} = ibrowse:spawn_worker_process("localhost", 8006),
    ?line {ok, "200", _, _} = ibrowse:send_req_direct(ConnPid, Uri, Hdrs, get, Bin, []),
    ?line {ok, "200", _, _} = ibrowse:send_req_direct(ConnPid, Uri, [], get, [], []),
    ibrowse:stop_worker_process(ConnPid),
    ok.

flush_chunked_get() ->
    io:format("  flush chunked post\n",[]),
    {ok, Bin} = file:read_file("../../www/10000.txt"),
    Uri = "http://localhost:8006/hello.txt",
    Hdrs = [{content_type, "binary/octet-stream"}],
    Opts = [{transfer_encoding, {chunked, 4000*1000}}],
    {ok, ConnPid} = ibrowse:spawn_worker_process("localhost", 8006),
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, Hdrs, get, Bin, Opts),
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, [], get, [], []),
    ibrowse:stop_worker_process(ConnPid),
    ok.


expires_test() ->
    io:format("expires_test\n", []),
    Uri = "http://localhost:8006/hello.txt",
    ?line {ok, "200", Hdrs, _} = ibrowse:send_req(Uri, [], get),

    %% Retrieve max-age value to test Expires header
    ?line "max-age=" ++ Rest = proplists:get_value("Cache-Control", Hdrs),
    ?line Secs = list_to_integer(Rest),

    %% Convert Date and Expires into datetime()
    ?line Date = proplists:get_value("Date", Hdrs),
    ?line Expires = proplists:get_value("Expires", Hdrs),
    Date_DT = httpd_util:convert_request_date(Date),
    Expires_DT = httpd_util:convert_request_date(Expires),

    %% Check if Expires value is equal to "Date + max-age"
    Val1 = calendar:datetime_to_gregorian_seconds(Date_DT) + Secs,
    Val2 = calendar:datetime_to_gregorian_seconds(Expires_DT),
    ?line Val1 = Val2,
    ok.


reentrant_test() ->
    io:format("reentrant_test\n", []),
    reentrant_test_status(),
    reentrant_test_delayed_headers(),
    ok.

reentrant_test_status() ->
    io:format("  status code\n", []),
    Uri = "http://localhost:8006/reentranttest/status",
    ?line {ok, "201", _, _} = ibrowse:send_req(Uri, [], post, <<"blob">>, []),
    ok.

reentrant_test_delayed_headers() ->
    io:format("  delayed headers\n", []),
    Uri = "http://localhost:8006/reentranttest/delayed_headers",
    ?line {ok, "200", Hdrs, _} = ibrowse:send_req(Uri, [], get),
    ?line "no-cache" = proplists:get_value("Cache-Control", Hdrs),
    ?line "static-tag" = proplists:get_value("Etag", Hdrs),
    ?line "true" = proplists:get_value("X-Delayed-Header", Hdrs),
    ok.

cgi_redirect_test() ->
    io:format("cgi_redirect_test\n", []),
    Uri = "http://localhost:8008/cgi-bin/redirect_test.cgi",
    ?line {ok, "302", Hdrs, _} = ibrowse:send_req(Uri, [], get),
    ?line true = lists:any(fun({"Location", _}) -> true; (_) -> false end, Hdrs),
    ok.

php_handler_test() ->
    io:format("php_handler_test\n", []),
    Uri = "http://localhost:8006/test.php",
    {ok, Binary} = file:read_file("./www/test.php"),
    Content = binary_to_list(Binary),
    ?line {ok, "200", _, Content} = ibrowse:send_req(Uri, [], get),
    ok.

arg_rewrite_test() ->
    io:format("arg_rewrite_test\n", []),
    arg_rewrite_test_rewrite(),
    arg_rewrite_test_redirect(),
    arg_rewrite_test_response(),
    ok.

arg_rewrite_test_rewrite() ->
    io:format("  rewrite\n", []),
    Uri = "http://localhost:8006/rewrite",
    ?line {ok, "200", Hdrs, _} = ibrowse:send_req(Uri, [], get),
    ?line "text/plain" = proplists:get_value("Content-Type", Hdrs),
    {ok, FI} = file:read_file_info("./www/hello.txt"),
    Etag = yaws:make_etag(FI),
    ?line Etag = proplists:get_value("Etag", Hdrs),
    ok.

arg_rewrite_test_redirect() ->
    io:format("  redirect\n", []),
    Uri = "http://localhost:8006/redirect",
    ?line {ok, "301", Hdrs, _} = ibrowse:send_req(Uri, [], get),
    ?line "http://www.yakaz.com" = proplists:get_value("Location", Hdrs),
    ok.


arg_rewrite_test_response() ->
    io:format("  response\n", []),
    Uri = "http://localhost:8006/response",
    ?line {ok, "200", Hdrs, Content} = ibrowse:send_req(Uri, [], get),
    ?line "text/plain" = proplists:get_value("Content-Type", Hdrs),
    ?line "Goodbye, Cruel World!" = Content,
    ok.



shaper_test() ->
    io:format("shaper_test\n", []),
    Uri = "http://localhost:8007/",
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, [], get),
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, [], get),
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, [], get),
    ?line {ok, "503", _, _} = ibrowse:send_req(Uri, [], get),
    ok.


%% used for appmod tests
%%
out(_A) ->
    %% add our special header to mark that we were here
    [{status, 200},
     {header, {?APPMOD_HEADER, "true"}}].
