-module(appmod_with_yssi).
-export([out/1]).

-include("yaws_api.hrl").

out(Arg) ->
    [Arg#arg{state=yssi},
     {yssi, "appmod_with_yssi.yaws"}].
