-module(jsontest).
-export([out/1, handler/3]).

out(Arg) ->
    yaws_rpc:handler_session(Arg, {?MODULE, handler}).

handler(_State, {call, subtract, Params}, _Session) ->
    {Minuend, Subtrahend} = case Params of
                                {array, [M, S]} ->
                                    {M, S};
                                Obj ->
                                    {jsonrpc:s(Obj, "minuend"),
                                     jsonrpc:s(Obj, "subtrahend")}
                            end,
    {true, undefined, undefined, {response, Minuend - Subtrahend}};
handler(_State, {notification, update, {array, [1,2,3,4,5]}}, _Session) ->
    false;
handler(_State, {notification, foobar, undefined}, _Session) ->
    false;
handler(_State, {call, sum, {array, Params}}, _Session) ->
    {true, undefined, undefined, {response, lists:sum(Params)}};
handler(_State, {notification, "notify_hello", {array, [7]}}, _Session) ->
    false;
handler(_State, {call, get_data, undefined}, _Session) ->
    {true, undefined, undefined, {response, {array, ["hello", 5]}}}.



