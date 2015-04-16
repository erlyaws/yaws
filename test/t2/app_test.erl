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
    test_server_options(),
    test1(),
    test2(),
    test3(),
    test_appmod(),
    test_dispatchmod(),
    test_streamcontent(),
    sendfile_get(),
    test_json(),
    test_post(),
    test_flush(),
    test_te_trailer_and_extensions(),
    test_expires(),
    test_reentrant(),
    test_cgi_redirect(),
    test_php_handler(),
    test_arg_rewrite(),
    test_shaper(),
    test_sslaccept_timeout(),
    test_ssl_multipart_post(),
    test_throw(),
    test_too_many_headers(),
    test_index_files(),
    test_embedded_id_dir(),
    test_embedded_listen_ip(),
    test_chained_appmods(),
    test_cache_appmod(),
    test_multi_forwarded_for(),
    test_log_rotation(),
    test_exhtml(),
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
    {ok, erlang:list_to_integer(LenStr)}.


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

test_server_options() ->
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

test_appmod() ->
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

test_dispatchmod() ->
    io:format("dispatchmod test\n", []),
    Uri1 = "http://localhost:8011/done",
    ?line {ok, "204", Headers1, _} = ibrowse:send_req(Uri1, [], get),
    ?line "true" = proplists:get_value("X-DispatchMod", Headers1),
    Uri2 = "http://localhost:8011/closed",
    ?line {ok, "200", Headers2, _} = ibrowse:send_req(Uri2, [], get),
    ?line "close" = proplists:get_value("Connection", Headers2),
    Uri3 = "http://localhost:8011/index.yaws",
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri3, [], get),
    ok.

test_streamcontent() ->
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

test_json() ->
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
    UStr = "{ \"origfilename\":\"Acronyms \\u2013 April 2014.pptx\" }",
    ?line {ok, {struct,[{"origfilename",US}]}} = json2:decode_string(UStr),
    ?line iolist_to_binary(US),		% must not cause a badarg exception
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
            recv_hdrs(Sock, erlang:list_to_integer(LenStr));
        {http, Sock, {http_header, _, _, _, _}} ->
            recv_hdrs(Sock, Len);
        {http, Sock, {http_response, _, 200, "OK"}} ->
            recv_hdrs(Sock, Len);
        Other ->
            {error, {"unexpected message", Other}}
    end.


%% partial_post_size = 2048000
test_post() ->
    io:format("post_test\n",[]),
    small_post(),
    large_post(),
    small_chunked_post(),
    large_chunked_post(),
    ok.

small_post() ->
    io:format("  small post\n",[]),
    {ok, Bin} = file:read_file(?builddir ++ "/docroot-test/1000.txt"),
    Sz = size(Bin),
    Uri = "http://localhost:8006/posttest/" ++ erlang:integer_to_list(Sz),
    Hdrs = [{content_length, Sz}, {content_type, "binary/octet-stream"}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, Hdrs, post, Bin, []),
    ok.

large_post() ->
    io:format("  large post\n",[]),
    {ok, Bin} = file:read_file(?builddir ++ "/docroot-test/10000.txt"),
    Sz = size(Bin),
    Uri = "http://localhost:8006/posttest/" ++ erlang:integer_to_list(Sz),
    Hdrs = [{content_length, Sz}, {content_type, "binary/octet-stream"}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, Hdrs, post, Bin, []),
    ok.

small_chunked_post() ->
    io:format("  small chunked post\n",[]),
    {ok, Bin} = file:read_file(?builddir ++ "/docroot-test/3000.txt"),
    Sz = size(Bin),
    Uri = "http://localhost:8006/posttest/chunked/" ++ erlang:integer_to_list(Sz),
    Hdrs = [{content_type, "binary/octet-stream"}],
    Opts = [{transfer_encoding, {chunked, 1000*1000}}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, Hdrs, post, Bin, Opts),
    ok.

large_chunked_post() ->
    io:format("  large chunked post\n",[]),
    {ok, Bin} = file:read_file(?builddir ++ "/docroot-test/10000.txt"),
    Sz = size(Bin),
    Uri = "http://localhost:8006/posttest/chunked/" ++ erlang:integer_to_list(Sz),
    Hdrs = [{content_type, "binary/octet-stream"}],

    %% size of chunk _IS_NOT_ a multiple of partial_post_size
    Opts1 = [{transfer_encoding, {chunked, 4000*1000}}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, Hdrs, post, Bin, Opts1),

    %% size of chunk _IS_ a multiple of partial_post_size
    Opts2 = [{transfer_encoding, {chunked, 4000*1024}}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, Hdrs, post, Bin, Opts2),
    ok.

test_flush() ->
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
    {ok, Bin} = file:read_file(?builddir ++ "/docroot-test/1000.txt"),
    Sz = size(Bin),
    Uri1 = "http://localhost:8006/flushtest/" ++ erlang:integer_to_list(Sz),
    Uri2 = "http://localhost:8006/hello.txt",
    Hdrs = [{content_length, Sz}, {content_type, "binary/octet-stream"}],
    {ok, ConnPid} = ibrowse:spawn_worker_process("localhost", 8006),
    ?line {ok, "200", _, _} = ibrowse:send_req_direct(ConnPid, Uri1, Hdrs, post, Bin, []),
    ?line {ok, "200", _, _} = ibrowse:send_req_direct(ConnPid, Uri2, [], get, [], []),
    ibrowse:stop_worker_process(ConnPid),
    ok.

flush_large_post() ->
    io:format("  flush large post\n",[]),
    {ok, Bin} = file:read_file(?builddir ++ "/docroot-test/10000.txt"),
    Sz = size(Bin),
    Uri1 = "http://localhost:8006/flushtest/" ++ erlang:integer_to_list(Sz),
    Uri2 = "http://localhost:8006/hello.txt",
    Hdrs = [{content_length, Sz}, {content_type, "binary/octet-stream"}],
    {ok, ConnPid} = ibrowse:spawn_worker_process("localhost", 8006),
    ?line {ok, "200", _, _} = ibrowse:send_req_direct(ConnPid, Uri1, Hdrs, post, Bin, []),
    ?line {ok, "200", _, _} = ibrowse:send_req_direct(ConnPid, Uri2, [], get, [], []),
    ibrowse:stop_worker_process(ConnPid),
    ok.

flush_chunked_post() ->
    io:format("  flush chunked post\n",[]),
    {ok, Bin} = file:read_file(?builddir ++ "/docroot-test/10000.txt"),
    Sz = size(Bin),
    Uri1 = "http://localhost:8006/flushtest/chunked/" ++ erlang:integer_to_list(Sz),
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
    {ok, Bin} = file:read_file(?builddir ++ "/docroot-test/1000.txt"),
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
    {ok, Bin} = file:read_file(?builddir ++ "/docroot-test/10000.txt"),
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
    {ok, Bin} = file:read_file(?builddir ++ "/docroot-test/10000.txt"),
    Uri = "http://localhost:8006/hello.txt",
    Hdrs = [{content_type, "binary/octet-stream"}],
    Opts = [{transfer_encoding, {chunked, 4000*1000}}],
    {ok, ConnPid} = ibrowse:spawn_worker_process("localhost", 8006),
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, Hdrs, get, Bin, Opts),
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, [], get, [], []),
    ibrowse:stop_worker_process(ConnPid),
    ok.

test_te_trailer_and_extensions() ->
    io:format("te_trailer_and_extensions_test\n",[]),
    {ok, Sock} = gen_tcp:connect("localhost", 8006, [binary, {active, false}]),
    Data = ["This is the data in the first chunk\n",
            "and this is the second one\n",
            "con", "sequence"],
    Path = "/posttest/chunked/" ++ erlang:integer_to_list(length(lists:flatten(Data))),
    ?line gen_tcp:send(Sock, "POST "++Path++" HTTP/1.1\r\n"
                       "Host: localhost\r\n"
                       "Trailer: Content-Type\r\n"
                       "Trailer: Extra-Headers-WooHoo\r\n"
                       "Transfer-Encoding: Chunked\r\n\r\n"),
    Body = lists:flatten([[erlang:integer_to_list(length(X), 16),"; foo=bar\r\n",
                           X,"\r\n"] || X <- Data]),
    ?line gen_tcp:send(Sock, Body),
    ?line gen_tcp:send(Sock, "0\r\n"
                       "Extra-Headers-WooHoo: something\r\n"
                       "Content-Type: text/plain\r\n\r\n"),
    inet:setopts(Sock, [{packet, http}]),
    ?line {ok, _Len} = recv_hdrs(Sock),
    gen_tcp:close(Sock),
    ok.


test_expires() ->
    io:format("expires_test\n", []),
    Uri1 = "http://localhost:8006/hello.txt",
    ?line {ok, "200", Hdrs1, _} = ibrowse:send_req(Uri1, [], get),

    %% Test "text/plain" rule
    %%   - Retrieve max-age value to test Expires header
    ?line CCtrl1 = proplists:get_value("Cache-Control", Hdrs1),
    ?line {match, ["2592000"]} =
        re:run(CCtrl1, "max-age=(\\d+)", [{capture,all_but_first,list}]),

    %%   - Convert Date and Expires into datetime()
    ?line Date    = proplists:get_value("Date",    Hdrs1),
    ?line Expires = proplists:get_value("Expires", Hdrs1),
    Date_DT       = httpd_util:convert_request_date(Date),
    Expires_DT    = httpd_util:convert_request_date(Expires),

    %%   - Check if Expires value is equal to "Date + max-age"
    Val1 = calendar:datetime_to_gregorian_seconds(Date_DT) + 2592000,
    Val2 = calendar:datetime_to_gregorian_seconds(Expires_DT),
    ?line Val1 = Val2,

    %% Test "*/*" rule
    Uri2 = "http://localhost:8006/",
    ?line {ok, "200", Hdrs2, _} = ibrowse:send_req(Uri2, [], get),
    ?line CCtrl2 = proplists:get_value("Cache-Control", Hdrs2),
    ?line {match, ["0"]} =
        re:run(CCtrl2, "max-age=(\\d+)", [{capture,all_but_first,list}]),

    %% Test "image/*" rule
    Uri3 = "http://localhost:8006/yaws_head.gif",
    ?line {ok, "200", Hdrs3, _} = ibrowse:send_req(Uri3, [], get),
    ?line CCtrl3 = proplists:get_value("Cache-Control", Hdrs3),
    ?line {match, ["2592000"]} =
        re:run(CCtrl3, "max-age=(\\d+)", [{capture,all_but_first,list}]),
    ok.


test_reentrant() ->
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

test_cgi_redirect() ->
    io:format("cgi_redirect_test\n", []),
    Uri = "http://localhost:8008/cgi-bin/redirect_test.cgi",
    ?line {ok, "302", Hdrs, _} = ibrowse:send_req(Uri, [], get),
    ?line true = lists:any(fun({"Location", _}) -> true; (_) -> false end, Hdrs),
    ok.

test_php_handler() ->
    io:format("php_handler_test\n", []),
    Uri = "http://localhost:8006/test.php",
    {ok, Binary} = file:read_file(?srcdir ++ "/www/test.php"),
    Content = binary_to_list(Binary),
    ?line {ok, "200", _, Content} = ibrowse:send_req(Uri, [], get),
    ok.

test_arg_rewrite() ->
    io:format("arg_rewrite_test\n", []),
    arg_rewrite_test_rewrite(),
    arg_rewrite_test_redirect(),
    arg_rewrite_test_response(),
    ok.

arg_rewrite_test_rewrite() ->
    io:format("  rewrite\n", []),
    Uri = "http://localhost:8006/rewrite",
    ?line {ok, "200", Hdrs, _} = ibrowse:send_req(Uri, [], get),
    ?line "text/plain" = split_content_type(Hdrs),
    {ok, FI} = file:read_file_info(?srcdir ++ "/www/hello.txt"),
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
    ?line "text/plain" = split_content_type(Hdrs),
    ?line "Goodbye, Cruel World!" = Content,
    ok.

%% split content type away from any charset info
split_content_type(Hdrs) ->
    hd(string:tokens(proplists:get_value("Content-Type", Hdrs), " ;")).

test_shaper() ->
    io:format("shaper_test\n", []),
    Uri = "http://localhost:8007/",
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, [], get),
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, [], get),
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, [], get),
    ?line {ok, "503", _, _} = ibrowse:send_req(Uri, [], get),
    ok.



test_sslaccept_timeout() ->
    case erlang:system_info(version) of
        "5.9.3" ->
            io:format("sslaccept_tout_test (skipping due to R15B03 bug)\n", []);
        _ ->
            io:format("sslaccept_tout_test\n", []),
            {ok, Sock} = gen_tcp:connect("localhost", 8443, [binary, {active, true}]),
            ?line ok = receive
                           {tcp_closed, Sock} -> ok
                       after
                           %% keepalive_timeout is set to 10 secs. So, wait 15 secs
                           %% before returning an error
                           15000 -> error
                       end,
            gen_tcp:close(Sock)
    end,
    ok.

test_ssl_multipart_post() ->
    io:format("ssl_multipart_post_test\n", []),
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    Boundary = "----------------------------3e9876546ecf\r\n",
    {ok, Bin0} = file:read_file(?builddir ++ "/docroot-test/1000.txt"),
    Data = list_to_binary([Boundary, Bin0]),
    Size = size(Data),
    Headers = [
               {'Content-Type', "multipart/form-data; Boundary=" ++ Boundary},
               {'Content-Length', Size}
              ],
    Uri = "https://localhost:8443/test_upload_ssl.yaws",
    Options = [{is_ssl, true}, {ssl_options, [{verify, 0}]}],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, Headers, post, Data, Options),
    ok = application:stop(ssl),
    ok = application:stop(public_key),
    ok = application:stop(asn1),
    ok = application:stop(crypto),
    ok.


test_throw() ->
    io:format("throw test\n", []),
    Uri = "http://localhost:8009/",
    ?line {ok, "500", _, _} = ibrowse:send_req(Uri, [], get),
    ok.

test_too_many_headers() ->
    io:format("too many request headers test\n", []),
    Uri = "http://localhost:8009/",
    Hdrs = [{link, "<compact.css>; rel=\"stylesheet\"; title=\"compact\""} || _ <- lists:seq(0, 1001)],
    ?line {ok, "431", _, _} = ibrowse:send_req(Uri, Hdrs, get),
    ok.


test_index_files() ->
    io:format("index_files test\n", []),
    ?line {ok, Bin} = file:read_file(?srcdir ++ "/../..//www/testdir/index.html"),
    Content = binary_to_list(Bin),

    %% "/" should be redirected to "/testdir", then to "/testdir/" and finally
    %% get "/testdir/index.html"
    Uri0 = "http://localhost:8010/",
    ?line {ok, "302", Hdrs1, _} = ibrowse:send_req(Uri0, [], get),
    ?line Uri1 = proplists:get_value("Location", Hdrs1),
    ?line "http://localhost:8010/testdir" = Uri1,
    ?line {ok, "302", Hdrs2, _} = ibrowse:send_req(Uri1, [], get),
    ?line Uri2 = proplists:get_value("Location", Hdrs2),
    ?line "http://localhost:8010/testdir/" = Uri2,
    ?line {ok, "200", _, Content} = ibrowse:send_req(Uri2, [], get),

    %% Do the same thing but with a query-string
    Uri3 = "http://localhost:8010/?a=1&b=2",
    ?line {ok, "302", Hdrs3, _} = ibrowse:send_req(Uri3, [], get),
    ?line Uri4 = proplists:get_value("Location", Hdrs3),
    ?line "http://localhost:8010/testdir?a=1&b=2" = Uri4,
    ?line {ok, "302", Hdrs4, _} = ibrowse:send_req(Uri4, [], get),
    ?line Uri5 = proplists:get_value("Location", Hdrs4),
    ?line "http://localhost:8010/testdir/?a=1&b=2" = Uri5,
    ?line {ok, "200", _, Content} = ibrowse:send_req(Uri5, [], get),
    ok.


test_embedded_id_dir() ->
    io:format("test_embedded_id_dir\n", []),
    Id = "id_dir_test",
    GconfList = [{id, Id},
                 {logdir, "./logs"},
                 {ebin_dir, ["./ebin"]}],
    Docroot = yaws:tmpdir(),
    SconfList = [{port, 0},
                 {servername, Id},
                 {listen, {127,0,0,1}},
                 {docroot, Docroot}],
    {ok, _SCList, _GC, _ChildSpecs} = yaws_api:embedded_start_conf(
                                        Docroot, SconfList, GconfList, Id),
    try
        {ok,
         {file_info, _, directory, read_write, _, _, _, _, _, _, _, _, _, _}} =
            file:read_file_info(yaws:id_dir(Id)),
        ok
    after
        ok = file:del_dir(yaws:id_dir(Id))
    end.

test_embedded_listen_ip() ->
    %% make sure we can specify a listen address as either
    %% a list or a tuple
    lists:map(fun(IP) ->
		      DocRoot = ".",
		      Id = "embedded_listen",
		      SConf = [{servername, "embedded_listen:8000"},
			       {docroot, DocRoot},
			       {listen, IP},
			       {port, 0},
			       {appmods,[{"/", ?MODULE}]}],
		      GConf = [{id, Id}],
		      ok = yaws:start_embedded(DocRoot, SConf, GConf, Id),
		      yaws:stop()
	      end, [{0,0,0,0}, "0.0.0.0"]).

test_chained_appmods() ->
    io:format("test_chained_appmods\n", []),
    {ok, Bin} = file:read_file(?builddir ++ "/docroot-test/1000.txt"),
    Content = binary_to_list(Bin),
    Uri = "http://localhost:8012/",
    ?line {ok, "200", Hdrs, Content} = ibrowse:send_req(Uri, [], get),
    ?line "appmod1[/], appmod2[/appmod2], appmod1[/appmod1], appmod3[/1000.txt]" =
        proplists:get_value("X-AppMods", Hdrs),
    ok.

test_cache_appmod() ->
    io:format("test_cache_appmod\n", []),
    Uri1 = "http://localhost:8013/index.yaws?no-cache=1",
    Uri2 = "http://localhost:8013/index.yaws",

    %% call cache_appmod_test and disable page cache
    ?line {ok, "200", Hdrs1, _} = ibrowse:send_req(Uri1, [], get),
    ?line "cache_appmod_test" = proplists:get_value("X-Appmod", Hdrs1),

    %% check that index.yaws is not cached
    ?line {ok, "200", Hdrs2, _} = ibrowse:send_req(Uri2, [], get),
    ?line "cache_appmod_test" = proplists:get_value("X-Appmod", Hdrs2),

    %% retrieve index.yaws from the cache, so cache_appmod_test is not called
    ?line {ok, "200", Hdrs3, _} = ibrowse:send_req(Uri2, [], get),
    ?line undefined = proplists:get_value("X-Appmod", Hdrs3),

    ok.

test_multi_forwarded_for() ->
    io:format("test_multi_forwarded_for\n", []),
    %% apparently ibrowse can't handle sending two separate headers with
    %% the same name but different values, which is needed for this test
    ?line {ok, S} = gen_tcp:connect(localhost, 8014, [{active,false},
                                                      {packet,http}]),
    ok = gen_tcp:send(S, ["GET / HTTP/1.1\r\nHost: localhost\r\n"
                          "X-Forwarded-For: 192.168.1.1\r\n",
                          "X-Forwarded-For: 192.168.1.2\r\n\r\n"]),
    ?line ok = check_forwarded_for(S),
    ok.

check_forwarded_for(S) ->
    inet:setopts(S, [{active,once}]),
    receive
        {http, S, {http_response, _, Code, _}} ->
            gen_tcp:close(S),
            case Code of
                200 ->
                    ok;
                _ ->
                    Code
            end
    end.

test_log_rotation() ->
    io:format("test_log_rotation\n", []),
    %% Write 1M of data in .access and .auth log to check the log rotation
    ?line {ok, Fd1} = file:open(?builddir ++ "/logs/localhost:8000.access", [write]),
    ?line ok = file:write(Fd1, lists:duplicate(1000001, $a)),
    file:close(Fd1),
    file:sync(Fd1),

    ?line {ok, Fd2} = file:open(?builddir ++ "/logs/localhost:8000.auth", [write]),
    ?line ok = file:write(Fd2, lists:duplicate(1000001, $a)),
    file:close(Fd2),
    file:sync(Fd2),

    ?line {ok, "200", _, _} =
        ibrowse:send_req("http://localhost:8000/wrap_log", [], get),

    timer:sleep(500),

    ?line {ok, _} = file:read_file_info(?builddir ++ "/logs/localhost:8000.access.old"),
    ?line {ok, _} = file:read_file_info(?builddir ++ "/logs/localhost:8000.auth.old"),
    ?line {ok, _} = file:read_file_info(?builddir ++ "/logs/localhost:8000.access"),
    ?line {ok, _} = file:read_file_info(?builddir ++ "/logs/localhost:8000.auth"),

    ok.

test_exhtml() ->
    io:format("test_exhtml\n", []),
    %% See github issue #216
    ?line {ok, "200", _, Body} = ibrowse:send_req("http://localhost:8000/exhtml.yaws", [], get),
    Expected = "<p id=\"foo\">\n  bar\n</p>\n",
    Body = Expected,
    ok.

%% used for appmod tests
%%
out(_A) ->
    %% add our special header to mark that we were here
    [{status, 200},
     {header, {?APPMOD_HEADER, "true"}}].
