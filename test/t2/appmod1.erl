-module(appmod1).
-export([out/1]).

out(Arg) ->
    case yaws_api:arg_server_path(Arg) of
        "/" ->
            put(appmod_stack, "appmod1[/]"),
            {page, "/appmod2"};
        Path ->
            Stack = get(appmod_stack),
            put(appmod_stack, Stack++", appmod1["++Path++"]"),
            {page, "/1000.txt"}
    end.
