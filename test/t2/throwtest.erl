-module(throwtest).
-export([out/1]).

out(_Arg) ->
    throw({status, 200}).
