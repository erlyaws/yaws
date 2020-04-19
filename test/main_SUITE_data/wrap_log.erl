-module(wrap_log).
-export([out/1]).

out(_Arg) ->
    yaws_log ! minute10,
    {status, 200}.
