-module(yaws_rpc_SUITE).

-include("testsuite.hrl").

-compile(export_all).

all() ->
    [
     unicode
    ].

groups() ->
    [
    ].

%%====================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
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
-define(PARAMS, ["åäö","abc"]).
-define(RESULT, "{\"result\":[\"åäö\",\"abc\"],\"id\":\"id\",\"jsonrpc\":\"2.0\"}").

unicode(_Config) ->
    Payload = json2:encode({struct,[{method,"foo"},{id,"id"},
                                    {params,?PARAMS}]}),
    Req = #http_request{method='POST', version={1,1}},
    Hdrs = #headers{},
    Arg = #arg{clidata=Payload,req=Req,headers=Hdrs},
    Res = yaws_rpc:handler(Arg, {?MODULE, handler}),
    ?assertEqual(200, proplists:get_value(status, Res)),
    {content, Type, Content} = lists:keyfind(content, 1, Res),
    ?assertEqual("application/json", Type),
    ?assertEqual(?RESULT, lists:flatten(Content)),
    ok.

handler(_, {call,foo,{array,?PARAMS}}) ->
    {false, {response, [{array, ?PARAMS}]}}.


