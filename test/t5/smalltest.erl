-module(smalltest).
-export([out/1]).

-include("../../include/yaws.hrl").
-include("../../include/yaws_api.hrl").

out(_Arg) ->
    [{status, 200}, {page, "/1000.txt"}].

