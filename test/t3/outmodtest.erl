-module(outmodtest).
-export([auth/2, out401/3]).

-include("../../include/yaws.hrl").
-include("../../include/yaws_api.hrl").


auth(#arg{}, #auth{}) ->
    {appmod, ?MODULE}.

out401(#arg{}, #auth{}, Realm) ->
    [{status, 200}, {header, "X-Outmod-Test: true"},
     {content, "text/plain", Realm}].
