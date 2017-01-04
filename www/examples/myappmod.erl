-module(myappmod).

-include_lib("yaws/include/yaws_api.hrl").
-export([out/1]).

box(Str) ->
    {'pre',[{class,"code erlang"}], Str}.

out(A) ->
    {ehtml,
     [{p,[],
       box(io_lib:format("A#arg.pathinfo = ~p~n"
                         "A#arg.prepath = ~p~n"
                         "A#arg.querydata = ~p~n",
                         [A#arg.pathinfo,
                          A#arg.prepath,
                          A#arg.querydata]))}]}.
