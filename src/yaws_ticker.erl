-module(yaws_ticker).
-author('klacke@bluetail.com').

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include("yaws_debug.hrl").

-export([ticker/3]).

%% Moved here from module yaws to ease purging of yaws.
%% cschultz

ticker(Time, To, Msg) ->
    receive
	{'EXIT', _} ->
	    exit(normal)
    after Time ->
	    To ! Msg
    end,
    ?MODULE:ticker(Time, To, Msg).
