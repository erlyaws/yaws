-module(jsonrpc_SUITE).

-include("testsuite.hrl").
-include_lib("kernel/include/file.hrl").

-compile(export_all).

all() ->
    [
     param_array,
     encode_decode,
     param_object,
     notification,
     missing_method,
     invalid_json,
     invalid_request,
     invalid_params,
     invalid_batch_json,
     empty_batch,
     invalid_batch1,
     invalid_batch2,
     mixed_batch,
     all_notification_batch
    ].

group() ->
    [
    ].

%%====================================================================
init_per_suite(Config) ->
    Id    = "testsuite-server",
    YConf = filename:join(?tempdir(?MODULE), "yaws.conf"),
    inets:start(),
    application:load(yaws),
    application:set_env(yaws, id,   Id),
    application:set_env(yaws, conf, YConf),
    ok = yaws:start(),
    [{yaws_id, Id}, {yaws_config, YConf} | Config].

end_per_suite(_Config) ->
    ok = application:stop(yaws),
    ok = application:unload(yaws),
    inets:stop(),
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Test, Config) ->
    Config.

end_per_testcase(_Test, _Config) ->
    ok.

%%====================================================================
param_array(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/"),

    ?assertEqual(ok, do_json(Url, {struct, [{"jsonrpc", "2.0"},
                                            {"method", "subtract"},
                                            {"params", {array, [42, 23]}},
                                            {"id", 1}]},
                             {struct, [{"jsonrpc", "2.0"},
                                       {"result", 19},
                                       {"id", 1}]})),

    ?assertEqual(ok, do_json(Url, {struct, [{"jsonrpc", "2.0"},
                                            {"method", "subtract"},
                                            {"params", {array, [23, 42]}},
                                            {"id", 2}]},
                             {struct, [{"jsonrpc", "2.0"},
                                       {"result", -19},
                                       {"id", 2}]})),
    ok.

encode_decode(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/"),

    ?assertEqual({ok,{response,[19]}},
                 jsonrpc:call(Url, [], {call, "subtract", [42, 23]})),

    UStr = "{ \"origfilename\":\"Acronyms \\u2013 April 2014.pptx\" }",
    {ok, {struct,[{"origfilename", US}]}} = json2:decode_string(UStr),
    iolist_to_binary(US), % must not cause a badarg exception

    ok.

param_object(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/"),

    ?assertEqual(ok, do_json(Url, {struct, [{"jsonrpc", "2.0"},
                                            {"method", "subtract"},
                                            {"params", {struct, [{"subtrahend", 23},
                                                                 {"minuend", 42}]}},
                                            {"id", 3}]},
                             {struct, [{"jsonrpc", "2.0"},
                                       {"result", 19},
                                       {"id", 3}]})),

    ?assertEqual(ok, do_json(Url, {struct, [{"jsonrpc", "2.0"},
                                            {"method", "subtract"},
                                            {"params", {struct, [{"minuend", 42},
                                                                 {"subtrahend", 23}]}},
                                            {"id", 4}]},
                             {struct, [{"jsonrpc", "2.0"},
                                       {"result", 19},
                                       {"id", 4}]})),
    ok.

notification(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/"),

    ?assertEqual(ok, do_json(Url, {struct, [{"jsonrpc", "2.0"},
                                            {"method", "update"},
                                            {"params", {array, [1,2,3,4,5]}}]},
                             notification)),

    ?assertEqual(ok, do_json(Url, {struct, [{"jsonrpc", "2.0"},
                                            {"method", "foobar"}]},
                             notification)),

    ok.

missing_method(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/"),

    ?assertEqual(ok, do_json(Url, {struct, [{"jsonrpc", "2.0"},
                                            {"method", "foobar"},
                                            {"id", "1"}]},
                             {struct, [{"jsonrpc", "2.0"},
                                       {"id", "1"},
                                       {"error", {struct,
                                                  [{"code", -32601},
                                                   {"message", "method not found"}]}
                                       }]})),
    ok.

invalid_json(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/"),

    InvalidJson = "{\"jsonrpc\": \"2.0\", \"method\": \"foobar,"
        "\"params\": \"bar\", \"baz]",
    ?assertEqual(ok, do_json(Url, InvalidJson,
                             {struct, [{"jsonrpc", "2.0"},
                                       {"id", null},
                                       {"error", {struct,
                                                  [{"code", -32700},
                                                   {"message", "parse error"}]}
                                       }]},
                             no_encode)),
    ok.

invalid_request(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/"),

    InvalidReq = "{\"jsonrpc\": \"2.0\", \"method\": 1, \"params\": \"bar\"}",
    ?assertEqual(ok, do_json(Url, InvalidReq,
                             {struct, [{"jsonrpc", "2.0"},
                                       {"id", null},
                                       {"error", {struct,
                                                  [{"code", -32600},
                                                   {"message", "invalid request"}]}
                                       }]},
                             no_encode)),
    ok.

invalid_params(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/"),

    InvalidReq = "{\"jsonrpc\": \"2.0\", \"method\": \"x\",\"params\": \"bar\"}",
    ?assertEqual(ok, do_json(Url, InvalidReq,
                             {struct, [{"jsonrpc", "2.0"},
                                       {"id", null},
                                       {"error", {struct,
                                                  [{"code", -32602},
                                                   {"message", "invalid params"}]}
                                       }]},
                             no_encode)),
    ok.

invalid_batch_json(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/"),

    InvalidJsonBatch = "[ {\"jsonrpc\": \"2.0\", \"method\": \"sum\","
        "\"params\": [1,2,4],\"id\": \"1\"},{\"jsonrpc\": \"2.0\", \"method\" ]",
    ?assertEqual(ok, do_json(Url, InvalidJsonBatch,
                             {struct, [{"jsonrpc", "2.0"},
                                       {"id", null},
                                       {"error", {struct,
                                                  [{"code", -32700},
                                                   {"message", "parse error"}]}
                                       }]},
                             no_encode)),
    ok.

empty_batch(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/"),

    EmptyBatch = "[]",
    ?assertEqual(ok, do_json(Url, EmptyBatch,
                             {struct, [{"jsonrpc", "2.0"},
                                       {"id", null},
                                       {"error", {struct,
                                                  [{"code", -32600},
                                                   {"message", "invalid request"}]}
                                       }]},
                             no_encode)),
    ok.

invalid_batch1(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/"),

    BogusBatch = "[1]",
    ?assertEqual(ok, do_json(Url, BogusBatch,
                             {array,
                              [{struct, [{"jsonrpc", "2.0"},
                                         {"id", null},
                                         {"error", {struct,
                                                    [{"code", -32600},
                                                     {"message", "invalid request"}]}
                                         }]}]},
                             no_encode)),
    ok.

invalid_batch2(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/"),

    BogusBatch = "[1,2,3]",
    ?assertEqual(ok, do_json(Url, BogusBatch,
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
                             no_encode)),
    ok.

mixed_batch(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/"),

    MixedBatch = "
    [{\"jsonrpc\":\"2.0\",\"method\":\"sum\",\"params\": [1,2,4], \"id\": \"1\"},
     {\"jsonrpc\":\"2.0\",\"method\":\"notify_hello\", \"params\": [7]},
     {\"jsonrpc\":\"2.0\",\"method\":\"subtract\",\"params\":[42,23],
      \"id\":\"2\"},
     {\"foo\": \"boo\"},
     {\"jsonrpc\":\"2.0\",\"method\":\"foo.get\",
      \"params\":{\"name\": \"myself\"}, \"id\": \"5\"},
     {\"jsonrpc\": \"2.0\", \"method\": \"get_data\", \"id\": \"9\"}]",
    ?assertEqual(ok, do_json(Url, MixedBatch,
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
                             no_encode)),
    ok.

all_notification_batch(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/"),

    NotifBatch = "[
        {\"jsonrpc\": \"2.0\", \"method\": \"notify_sum\", \"params\": [1,2,4]},
        {\"jsonrpc\": \"2.0\", \"method\": \"notify_hello\", \"params\": [7]}]",
    ?assertEqual(ok, do_json(Url, NotifBatch, notification, no_encode)),
    ok.

%%====================================================================
do_json(Url, Req, Expected) ->
    do_json(Url, Req, Expected, encode).

do_json(Url, Req, notification, NeedEncode) ->
    {ok,  {{_,200,_}, Hdrs, Body}} = json_send(Url, Req, NeedEncode),
    ?assertEqual("application/json", proplists:get_value("content-type", Hdrs)),
    ?assertEqual(<<>>, Body),
    ok;
do_json(Url, Req, {struct, _}=Expected, NeedEncode) ->
    {ok, {{_,200,_}, Hdrs, Body}} = json_send(Url, Req, NeedEncode),
    ?assertEqual("application/json", proplists:get_value("content-type", Hdrs)),
    check_json(Expected, Body, true);
do_json(Url, Req, {array, Array}, NeedEncode) ->
    {ok, {{_,200,_}, Hdrs, Body}} = json_send(Url, Req, NeedEncode),
    ?assertEqual("application/json", proplists:get_value("content-type", Hdrs)),
    {ok, {array, GotArray}} = json2:decode_string(binary_to_list(Body)),
    lists:map(fun({Obj, Got}) ->
                      ?assertEqual(ok, check_json(Obj, Got, false))
              end, lists:zip(Array, GotArray)),
    ok.

check_json({struct, _}=Exp, Body, true) ->
    {ok, DecodedBody} = json2:decode_string(binary_to_list(Body)),
    check_json(Exp, DecodedBody);
check_json({struct, _}=Exp, Body, false) ->
    check_json(Exp, Body).

check_json({struct, Members}, DecodedBody) ->
    io:format("~p~n~p~n", [Members, DecodedBody]),
    lists:foreach(fun({Key, Val}) ->
                          ?assertEqual(Val, jsonrpc:s(DecodedBody, Key))
                  end, Members),
    ok.

%% json_send(Url, Req) ->
%%     json_send(Url, Req, encode).

json_send(Url, Req, encode) ->
    json_send(Url, lists:flatten(json2:encode(Req)), no_encode);
json_send(Url, Req, no_encode) ->
    testsuite:http_post(Url, {"application/json", Req}).
