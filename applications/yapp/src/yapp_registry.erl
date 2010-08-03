%%%-------------------------------------------------------------------
%%% File    : yapp_registry.erl
%%% @author Mikael Karlsson <mikael@creado.se>
%%% @since 1 June 2006 by Mikael Karlsson <mikael@creado.se>
%%% @doc Yapp registry api.
%%% <p>The default yapp registry (yapp_mnesia_server) uses mnesia, so a 
%%% prerequisite is that a schema is created. </p>

-module(yapp_registry).


%% API
-export([list/1, register/3, unregister/3]).

%% @spec list(YappRegistry::pid()) -> RegistryContent | exit()
%%   RegsitryContent = [YawsSrvContent]
%%   YawsSrvContent = {YawsServerId, Yapps}
%%   Yapps = [{URL, AppName}] | undefined
%%   URL= string()
%%   AppName = atom()
%% @doc Lists all registered Yapps.
list(YappRegistry) ->
    gen_server:call(YappRegistry,{?MODULE, list}).

%% @spec register(YappRegistry::pid(), SrvId::string(), {YappUrl::string(), AppName::atom()}) -> ok | exit()
%% @doc Register a Yapp. Registers in the virtual server with the opaque property 
%% yapp_server_id = SrvID. The YappUrl is the root path to the Yapp and the AppName is 
%% the Name of the application.
register(YappRegistry, SrvId, {_YappUrl, _AppName} = A) ->
    gen_server:call(YappRegistry,{?MODULE, register, {SrvId, A}}).
    

%% @spec unregister(YappRegistry::pid(), SrvId::string(), YappUrl::string()) -> ok | exit()
%% @doc Unregister a Yapp from Yaws. Unregisters in the virtual server with yapp_server_id = SrvID. 
%% The YappUrlOrName is either the root path to the Yapp or the name of it.
unregister(YappRegistry, SrvId, YappUrl) ->
    gen_server:call(YappRegistry,{?MODULE, unregister, {SrvId, YappUrl}}).
