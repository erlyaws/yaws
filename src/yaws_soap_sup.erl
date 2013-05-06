%%%-------------------------------------------------------------------
%%% File    : yaws_soap_sup.erl
%%% Author  : Jan Nyström <JanHenryNystrom@gmail.com>
%%%           Andreas Hellström <andreas.hellstrom@teliasonera.com>
%%% Desc    : Supervisor for soap workers.
%%% Created : 4 Mar 2008 by Jan Nyström
%%%-------------------------------------------------------------------
-module(yaws_soap_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, child_spec/0, start_children/1, setup/1]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

setup(WsdlModel) ->
    lists:foreach(fun(Child) ->
                          yaws_soap_srv_worker:setup(Child, add, WsdlModel)
                  end,
                  children()).

start_children(Number) -> start_children(0, Number).

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
    {ok,{{simple_one_for_one, 1000, 3600},
         [child(worker, yaws_soap_srv_worker)]}}.

child_spec() -> child(supervisor, ?MODULE).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: child(Type, Module) -> ChildSpec
%% Description: Creates a child specification
%%--------------------------------------------------------------------
child(worker, Module) ->
    {Module, {Module, start_link, []}, permanent, 500, worker, [Module]};
child(supervisor, Module) ->
    {Module, {Module, start_link, []},
     permanent, infinity, supervisor, [Module]}.

%%--------------------------------------------------------------------
%% Function:
%% Description:
%%--------------------------------------------------------------------
children() ->
    lists:map(fun({_, Child, _, _}) -> Child end,
              supervisor:which_children(?MODULE)).

start_children(N, N) -> ok;
start_children(N, Limit) ->
    supervisor:start_child(?MODULE, []),
    start_children(N + 1, Limit).
