-module(appmod3).
-export([out/1]).

out(Arg) ->
    Path    = yaws_api:arg_server_path(Arg),
    Stack   = get(appmod_stack),
    Options = [{header, {"X-AppMods", Stack++", appmod3["++Path++"]"}}],
    {page, {Options, "/chained.txt"}}.
