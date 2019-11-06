-module(appmod_strip_undefined_bindings).
-export([out/1]).

out(_) ->
    [{bindings, [{"HelloWorld", "hello world!"}]},
     {yssi, "appmod_with_yssi_strip_undefined_bindings.yaws"}].
