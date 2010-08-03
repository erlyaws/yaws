%%%-------------------------------------------------------------------
%%% File    : yapp_handler.erl
%%% @author Mikael Karlsson <mikael@creado.se>
%%% @since 1 Jun 2006 by Mikael Karlsson <mikael@creado.se>
%%% @see yapp 
%%% @see yapp_registry
%%% @see yapp_server
%%% @doc Yaws applications handler. 
%%% <p>An easy way to deploy Yaws applications (Yapps) independently of
%%% each other. A Yapp is an Erlang application already installed, by for 
%%% instance erlmerge. 
%%% </p>
%%% <p>
%%% To register a Yapp on the virtual server uses the command (example):</p> <code>
%%% yapp_handler:add(yapp_handler, "myvirtserverid","/myYappPath", my_app_name)
%%% </code>
%%% <p>where "myvirtserverid" is the the "server id" set in the opaque variable
%%% yapp_server_id in yaws.conf:<pre>
%%%        &lt;opaque&gt;
%%%                yapp_server_id = myvirtserverid
%%%        &lt;/opaque&gt; </pre>
%%% </p>
%%% <p>If Yaws is running on a different node use:
%%% </p><code>
%%% yapp_handler:add({yapp_handler,'node@host'}, "myvirtserverid","/myYappPath", my_app_name)
%%% </code>
%%% <p>The default yapp_handler implementation is yapp_server.</p>
%%% <p>The yapp_handler uses the yapp module to change the configuration on Yaws, and the
%%% yapp_registry module implementation to store the configuration.</p>

-module(yapp_handler).


%% API
-export([list/2, add/3, add/4, remove/3, init_yapps/1]).

%% @spec list(YappServer::pid(), SrvId) -> Yapps | exit()
%%   SrvID = string() | all
%%   Yapps = {URL, AppName} | undefined
%%   URL= string()
%%   AppName = atom()
%% @doc Lists all registered Yapps.
list(YappServer, YawsServerId) ->
    gen_server:call(YappServer,{?MODULE, list, YawsServerId}).

%% @spec add(YappServer::pid(), SrvId::string(), YappUrl::string(), AppName::atom()) -> ok | exit()
%% @doc Add a Yapp. Adds in the virtual server with the opaque property 
%% yapp_server_id = SrvID. The YappUrl is the root path to the Yapp and the AppName is 
%% the Name of the application.
add(YappServer, SrvId, YappUrl, AppName) ->
    gen_server:call(YappServer,{?MODULE, add, {SrvId, YappUrl, AppName}}).
    
%% @spec add(YappServer::pid(), SrvId::string(),  AppName::atom()) -> ok | exit()
%% @doc Add a Yapp. Adds in the virtual server with the opaque property 
%% yapp_server_id = SrvID. The root URL will become  "/" ++ atom_to_list(AppName)
%% the Name of the application.
add(YappServer, SrvId, AppName) ->
    add(YappServer, SrvId, "/" ++ atom_to_list(AppName), AppName).


%% @spec remove(YappServer::pid(), SrvId::string(), YappUrlOrName::string()) -> ok | exit()
%%   YappUrlOrName = string() | atom()
%% @doc Remove a Yapp from Yaws. Removes in the virtual server with yapp_server_id = SrvID. 
%% The YappUrlOrName is either the root path to the Yapp or the name of it.
remove(YappServer, SrvId, YappUrlOrName) when is_list(YappUrlOrName)->
    gen_server:call(YappServer,{?MODULE, remove, {SrvId, YappUrlOrName}});
remove(YappServer, SrvId, YappUrlOrName) when is_atom(YappUrlOrName)->
    remove(YappServer, SrvId, "/" ++ atom_to_list(YappUrlOrName)).

%% @spec init_yapps(YappServer::pid()) -> ok | exit()
%% @doc Iinitalizes the Yaws Sconfs list with bootstrap_yapps and Yapps in the
%% the registry. The default name for YappServer is the atom yapp_handler.
init_yapps(YappServer) ->
    gen_server:call(YappServer,{?MODULE, init_yapps}).

