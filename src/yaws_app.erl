%%%----------------------------------------------------------------------
%%% File    : yaws_app.erl
%%% Author  : Claes Wikstrom <klacke@bluetail.com>
%%% Purpose : 
%%% Created : 16 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_app).
-author('klacke@hyber.org').


-behaviour(application).
-export([start/2,stop/1]).

%% start

start(Type, StartArgs) ->
  yaws_sup:start_link().

%% stop

stop(State) ->
  ok.
