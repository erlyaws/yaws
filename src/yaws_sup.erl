%%%----------------------------------------------------------------------
%%% File    : yaws_sup.erl
%%% Author  : Claes Wikstrom <klacke@bluetail.com>
%%% Purpose : 
%%% Created : 16 Jan 2002 by Claes Wikstrom <klacke@bluetail.com>
%%%----------------------------------------------------------------------

-module(yaws_sup).
-author('klacke@bluetail.com').


-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
init([]) ->

    Sess = {yaws_session_server, {yaws_session_server, start_link, []},
	       permanent, 5000, worker, [yaws_session_server]},

    YawsLog = {yaws_log, {yaws_log, start_link, []},
	       permanent, 5000, worker, [yaws_log]},

    YawsServ = {yaws_server, {yaws_server, start_link, []},
	       permanent, 5000, worker, [yaws_server]},

    {ok,{{one_for_all,0,300}, [YawsLog, YawsServ, Sess]}}.
