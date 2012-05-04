-module(throwtest).
-export([out/1]).

out(Arg) ->
    throw({status, 200}).
