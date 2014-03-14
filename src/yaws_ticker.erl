-module(yaws_ticker).
-author('klacke@bluetail.com').

-export([ticker/3]).

%% Moved here from module yaws to ease purging of yaws.
%% cschultz

ticker(Time, To, Msg) ->
    receive
        {'EXIT', _, _} ->
            exit(normal)
    after Time ->
            To ! Msg
    end,
    ?MODULE:ticker(Time, To, Msg).
