%%%----------------------------------------------------------------------
%%% File    : yaws_ws_sup.erl
%%% Author  : Steve Vinoski <vinoski@ieee.org>
%%% Purpose : Supervise websocket callback processes
%%% Created : 22 Dec 2016 by Steve Vinoski <vinoski@ieee.org>
%%%----------------------------------------------------------------------

-module(yaws_ws_sup).
-author('vinoski@ieee.org').
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{yaws_websockets,
            {yaws_websockets, start_link, []},
            temporary, 2000, worker, [yaws_websockets]}]}}.
