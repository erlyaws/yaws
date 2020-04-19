-module(appmod_with_yssi_strip_undefined_bindings).
-export([out/1]).

out(_) ->
    [{bindings, [strip_undefined, {"HelloWorld", "hello world!"}]},
     {yssi, "appmod_with_yssi_strip_undefined_bindings.yaws"}].
