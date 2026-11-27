%%% File    : yaws_sup_restarts.erl
%%% Author  : Claes  Wikstrom <klacke@hyber.org>
%%% Description : Procs that can be restarted
%%% Created : 13 Jan 2009 by Claes  Wikstrom <klacke@hyber.org>

-module(yaws_sup_restarts).

-behaviour(supervisor).

%% External exports
-export([start_link/0, start_rss_link/0]).

%% supervisor callbacks
-export([init/1]).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_rss_link() ->
    supervisor:start_link({local, yaws_rss_sup}, ?MODULE, rss).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
init([]) ->
    Sess = {yaws_session_server, {yaws_session_server, start_link, []},
            permanent, 5000, worker, [yaws_session_server]},
    YawsEventManager = {yaws_event_manager,
                        {gen_event, start_link,[{local,yaws_event_manager}]},
                        permanent, 5000, worker, [gen_event]},
    {ok,{{one_for_one, 1, 60}, [Sess, YawsEventManager]}};
init(rss) ->
    YawsRSS = {yaws_rss, {yaws_rss, start_link, []},
               permanent, 5000, worker, [yaws_rss]},
    {ok,{{one_for_one, 1, 60}, [YawsRSS]}}.


