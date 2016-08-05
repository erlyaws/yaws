-module(app_test).

-export([out/1]).

-define(APPMOD_HEADER, "Appmod-Called").

out(_A) ->
    %% add our special header to mark that we were here
    [{status, 200},
     {header, {?APPMOD_HEADER, "true"}}].
