%% coding: utf-8
-module(yaws_rpc_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("yaws_api.hrl").
-include("tftest.hrl").

-define(PARAMS, ["åäö","abc"]).

%% ignore it
unicode_test() ->
    Payload = json2:encode({struct,[{method,"foo"},{id,"id"},
                                    {params,?PARAMS}]}),
    Req = #http_request{method='POST', version={1,1}},
    Hdrs = #headers{},
    Arg = #arg{clidata=Payload,req=Req,headers=Hdrs},
    yaws_rpc:handler(Arg, {?MODULE, handler}),
    timer:sleep(2000),
    ok.

handler(_, {call,foo,{array,?PARAMS}}) ->
    false.

