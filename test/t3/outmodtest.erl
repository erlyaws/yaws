-module(outmodtest).
-export([auth/2, out401/3]).

-include("../../include/yaws.hrl").
-include("../../include/yaws_api.hrl").


auth(_Arg, _Auth) ->
    {appmod, ?MODULE}.

out401(_Arg, _Auth, Realm) ->
    [{status, 200}, {header, "X-Outmod-Test: true"},
     {content, "text/plain", Realm}].
