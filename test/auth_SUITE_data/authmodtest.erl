-module(authmodtest).
-export([auth/2]).

-include("../../include/yaws.hrl").
-include("../../include/yaws_api.hrl").


auth(#arg{}=Arg, #auth{}) ->
    H = Arg#arg.headers,
    case H#headers.authorization of
        {"foo", "bar", _} ->
            true;
        _ ->
            false
    end.
