-module(appmod2).
-export([out/1]).

out(Arg) ->
    Path  = yaws_api:arg_server_path(Arg),
    Stack = get(appmod_stack),
    put(appmod_stack, Stack++", appmod2["++Path++"]"),
    {page, "/appmod1"}.
