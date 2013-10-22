-module(srcdir_test).

-ifdef(SRCDIR_VERSION).

-export([out/1]).

-include("srcdir_test.hrl").

out(_Arg) ->
    Content = ?SRCDIR_VERSION,
    [{status, 200}, {content, "text/plain", Content}].

-endif. %% -ifdef(SRCDIR_VERSION).
