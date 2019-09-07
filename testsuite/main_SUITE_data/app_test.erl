-module(app_test).
-export([out/1]).

-include("yaws_api.hrl").

-define(APPMOD_HEADER, "Appmod-Called").

out(#arg{appmod_name=?MODULE}) ->
    %% add our special header to mark that we were here
    [{status, 200},
     {header, {?APPMOD_HEADER, "true"}}].
