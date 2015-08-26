-module(max_connections).
-export([out/1]).

out(_Arg) ->
    timer:sleep(3000),
    [{status, 204}].
