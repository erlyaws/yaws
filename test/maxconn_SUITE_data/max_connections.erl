-module(max_connections).
-export([out/1]).

out(_Arg) ->
    [{status, 204}].
