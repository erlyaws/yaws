%%%-------------------------------------------------------------------
%%% File    : yapp_sup.erl
%%% Author  : Mikael Karlsson <mikael@creado.se>
%%% Description : 
%%%
%%% Created :  1 Jun 2006 by Mikael Karlsson <mikael@creado.se>
%%%-------------------------------------------------------------------
-module(yapp_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(_) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    AllEnv = application:get_all_env(),
    YappRegistryModule = proplists:get_value(yapp_registry_impl, AllEnv, yapp_mnesia_server),
    YappRegName = YappRegistryModule,
    YappRegistry = {yapp_registry_worker,{YappRegistryModule, start_link,[YappRegName]},
                         permanent,2000,worker,[YappRegistryModule, yapp_registry]},
    YappHandler = {yapp_handler_worker,{yapp_server, start_link, [yapp_handler, YappRegName]},
                   permanent,2000,worker,[yapp_handler, yapp_server]},
    {ok,{{one_for_all,0,5}, [YappRegistry, YappHandler]}}.

%%====================================================================
%% Internal functions
%%====================================================================
