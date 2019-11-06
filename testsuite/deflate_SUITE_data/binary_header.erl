-module(binary_header).
-export([out/1]).

out(_Arg) ->
    %% Regression test for github issue 367, where returning a binary
    %% header with deflate on would cause a crash.
    %% https://github.com/klacke/yaws/issues/367
    [{status, 200},
     {header, {content_type, <<"application/octet">>}},
     {ehtml, {pre,[],<<"ABCDEFGHIJKLMNOPQRSTUVWXYZ\n">>}}].
