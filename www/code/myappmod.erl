-module(myappmod).
-author('klacke@bluetail.com').

-include("../../include/yaws_api.hrl").
-compile(export_all).

box(Str) ->
    {'div',[{class,"box"}],
     {pre,[],Str}}.

out(A) ->
    {ehtml,
     [{p,[],
       box(io_lib:format("A#arg.appmoddata = ~p~n"
                         "A#arg.appmod_prepath = ~p~n"
                         "A#arg.querydata = ~p~n", 
                         [A#arg.appmoddata,
                          A#arg.appmod_prepath,
                          A#arg.querydata]))}]}.



