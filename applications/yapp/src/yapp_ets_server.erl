%%%-------------------------------------------------------------------
%%% File    : yapp_ets_server.erl
%%% @author Steve Vinoski <vinoski@ieee.org>
%%% @since 17 Apr 2009 by Steve Vinoski <vinoski@ieee.org>
%%% @see yapp_registry
%%% @doc Yapp registry implementation that uses ets.
%%% <p>This module is selected to be used by the yaws handler
%%% by setting the application environment property
%%% {yapp_registry_impl, yapp_ets_server} in the yapp.app file.
%%% It's useful for cases where yapp registration need not be
%%% persistent, such as for yapps that are bootstrapped at startup.</p>
%%% @end
%%%-------------------------------------------------------------------
-module(yapp_ets_server).
-behavior(gen_server).

%% API
-export([start_link/1, start/1]).
%% implements yapp_registry

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, { yapps }).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

start(Name) ->
    gen_server:start({local, Name}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{ yapps = ets:new(yapps, [private]) }}.

handle_call({yapp_registry, list}, _From, State=#state{yapps = Yapps}) ->
    Reply = ets:foldl(fun(Item, Acc) -> [Item | Acc] end, [], Yapps),
    {reply, Reply, State};
handle_call({yapp_registry, register, {SrvId, KeyValue={Yappurl, _AppName}}},
            _From, State = #state{yapps=Yapps}) ->
    NewKeyValue = case ets:lookup(Yapps, SrvId) of
                      [] ->
                          [KeyValue];
                      [{SrvId, UrlApps}] ->
                          case lists:keymember(Yappurl, 1, UrlApps) of
                              false ->
                                  [KeyValue | UrlApps];
                              true ->
                                  lists:keyreplace(Yappurl, 1,
                                                   UrlApps, KeyValue)
                          end
                  end,
    ets:insert(Yapps, {SrvId, NewKeyValue}),
    {reply, ok, State};
handle_call({yapp_registry, unregister, {SrvId, Yappurl}},
            _From, State = #state{yapps=Yapps}) ->
    Reply = case ets:lookup(Yapps, SrvId) of
                [] ->
                    {error, {srv_id_not_defined, SrvId}};
                [{SrvId, UrlApps}] ->
                    case lists:keymember(Yappurl, 1, UrlApps) of
                        false ->
                            ok;
                        true ->
                            NKv = lists:keydelete(Yappurl, 1, UrlApps),
                            ets:insert(Yapps, {SrvId, NKv})
                    end,
                    ok
            end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{yapps = Yapps}) ->
    ets:delete(Yapps),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
