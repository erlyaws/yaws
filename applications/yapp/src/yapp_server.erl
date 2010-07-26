%%%-------------------------------------------------------------------
%%% File    : yapp_server.erl
%%% Author  : Mikael Karlsson <mikael@creado.se>
%%% Description : 
%%%
%%% Created : 28 May 2006 by Mikael Karlsson <mikael@creado.se>
%%%-------------------------------------------------------------------
-module(yapp_server).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% implements yapp_handler

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {yapp_registry}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Name, YappRegistry) ->
    gen_server:start_link({local, Name}, ?MODULE, [YappRegistry], []).

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
    {ok, #state{}};

init([YappRegistry]) ->
    self() ! init_yapps,
    {ok, #state{yapp_registry = YappRegistry}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({yapp_handler, list, all}, _From, State) ->
    Reply = yapp_registry:list(State#state.yapp_registry),
    {reply, Reply, State};

handle_call({yapp_handler, list, SrvId}, _From, State) ->
    YR = yapp_registry:list(State#state.yapp_registry),
    Reply = proplists:get_value(SrvId,YR),
    {reply, Reply, State};

handle_call({yapp_handler, add, {SrvId,YappUrl, AppName}}, _From, State) ->
    yapp_registry:'register'(State#state.yapp_registry, SrvId,{YappUrl,AppName}),
    yapp:insert(SrvId, {YappUrl, AppName}),
    {reply, ok, State};

handle_call({yapp_handler, remove, {SrvId,YappUrl}}, _From, State) ->
    yapp_registry:'unregister'(State#state.yapp_registry, SrvId, YappUrl),
    yapp:remove(SrvId, YappUrl),
    {reply, ok, State};
    
handle_call({yapp_handler, init_yapps},_From, State) ->
    Reply = init_yapps(State#state.yapp_registry),
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
handle_info({setdep,{yapp_registry,P}} , State) ->
    {noreply, State#state{yapp_registry=P}};
handle_info(init_yapps, State) ->
    init_yapps(State#state.yapp_registry),
    {noreply, State};
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


init_yapps(YappRegistry) ->
    
    BYS = yapp:get_bootstrap_yapps(),
    Yapps = yapp_registry:list(YappRegistry),
    YB = BYS ++ Yapps,
    case YB of
        [] ->
            do_nothing;
        _ ->
            yapp:insert(YB)
    end,

    Handlers = gen_event:which_handlers(yaws_event_manager),
    case lists:member(yapp_event_handler, Handlers) of
	false -> 
	    ok = yapp_event_handler:add_handler(yaws_event_manager, yapp_handler);
	true -> 
	    do_nothing
    end.
