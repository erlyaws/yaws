%%%-------------------------------------------------------------------
%%% File    : yapp_mnesia_server.erl
%%% @author Mikael Karlsson <mikael@creado.se>
%%% @since 28 May 2006 by Mikael Karlsson <mikael@creado.se>
%%% @see yapp_registry 
%%% @doc Yapp registry implementation that uses mnesia.
%%% <p>This module is selected to be used by the yaws handler 
%%% by setting the application environment property
%%% {yapp_registry_impl, yapp_mnesia_server} in the yapp.app file.</p>
%%% @end
%%%-------------------------------------------------------------------
-module(yapp_mnesia_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).
%% implements yapp_registry

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {dummy}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    mnesia:start(),
    mnesia:wait_for_tables([yapp],500),
    init_y_registry(),   %% Create table if necessary
    init_yapp_reg(),     %% Create yapp register if necessary
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({yapp_registry, list}, _From, State) ->
    Reply = list_yapps(),
    {reply, Reply, State};
handle_call({yapp_registry, register,{SrvId,KeyValue}}, _From, State) ->
    Reply = register_yapp(SrvId,KeyValue),
    {reply, Reply, State};
handle_call({yapp_registry, unregister,{SrvId,Key}}, _From, State) ->
    Reply = unregister_yapp(SrvId,Key),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%% The yapp_reg is stored in Mnesia in the y_registry 
%% and contains a list of {yapp_server_id, yapps} tuples, where 
%% yapps is a list of {UrlPath, AppName} tuples
%% UrlPath = string()
%% AppName = atom()
-record(y_registry, {
          key,
          value
          }).

init_y_registry() ->
    case proplists:get_value(y_registry, mnesia:system_info(tables)) of
        true -> already_created;
        _    -> create_y_registry_table()
    end.

create_y_registry_table() ->
    Opts = [{type,set},{disc_copies, [node()]}],
    mnesia:create_table(y_registry,[{attributes, record_info(fields,y_registry)} | Opts ]).

init_yapp_reg() ->
    F = fun() ->
                case get_yapp_reg() of
                    { error, _ } ->
                        put_yapp_reg([]);
                    _ ->
                        do_nothing
                end
        end,
    {atomic, Value} = mnesia:transaction(F),
    Value.

get_yapp_reg() ->
    case mnesia:read({y_registry, yapp_reg}) of
        [] -> { error, no_yapp_reg };
        [{y_registry,yapp_reg,YappReg}] -> YappReg
    end.

put_yapp_reg(YappReg) ->    
    mnesia:write(#y_registry{ key=yapp_reg, value=YappReg}).
    

list_yapps() ->
    {atomic, Value} = mnesia:transaction(fun() -> get_yapp_reg() end),
    Value.

%% @spec register_yapp(SrvId::string(), {YappUrl::string(), AppName::atom()}) -> ok | exit()
%% @doc Register a Yapp. Registers in the virtual server with the opaque property 
%% yapp_server_id = SrvID. The YappUrl is the root path to the Yapp and the AppName is 
%% the Name of the application.
register_yapp(SrvId, KeyValue) ->
    {atomic, Value} = mnesia:transaction(fun() ->
                                              register2(SrvId,KeyValue)
                                      end),
    Value.

register2(SrvId, {YappUrl, AppName}) ->
    YR = get_yapp_reg(),
    YR2 = 
        case proplists:get_value(SrvId,YR) of
            undefined ->
                [{SrvId,[{YappUrl,AppName}]} | YR ];
            UrlApps ->
                UrlApps2 = insert_into_urlapps(YappUrl,AppName,UrlApps),
                lists:keyreplace(SrvId, 1, YR, {SrvId,UrlApps2})
        end,
    put_yapp_reg(YR2).


%% @spec unregister_yapp(SrvId::string(), YappUrl::string()) -> ok | exit()
%% @doc Unregister a Yapp. Unregisters in the virtual server with yapp_server_id = SrvID. 
%% The YappUrl is the root path to the Yapp.
unregister_yapp(SrvId, YappUrl) ->
    {atomic, Value} = mnesia:transaction(fun() ->
                                              unregister2(SrvId,YappUrl)
                                     end),
    Value.

unregister2(SrvId,YappUrl) ->
    YR = get_yapp_reg(),
    case proplists:get_value(SrvId,YR) of
        undefined ->
            {error, {srv_id_not_defined, SrvId}};
        UrlApps ->
            YR2 =
                case delete_from_urlapps(YappUrl, UrlApps) of
                    [] ->
                        lists:keydelete(SrvId,1,YR);
                    UrlApps2 ->
                        lists:keyreplace(SrvId, 1, YR, {SrvId,UrlApps2})
                end,
            put_yapp_reg(YR2)
    end.

insert_into_urlapps(YappUrl,AppName,UrlApps)->
    case lists:keymember(YappUrl, 1, UrlApps) of
        false ->
            [{YappUrl,AppName}|UrlApps];
        true ->
            lists:keyreplace(YappUrl, 1, UrlApps, {YappUrl, AppName})
    end.    

delete_from_urlapps(YappUrl, UrlApps) ->
    case lists:keymember(YappUrl, 1, UrlApps) of
        false ->
            UrlApps;
        true ->
            lists:keydelete(YappUrl, 1, UrlApps)
    end.
