%%%-------------------------------------------------------------------
%%% File    : yapp_event_handler.erl
%%% @author Mikael Karlsson <mikael@creado.se>
%%% @since 3 Jun 2006 by Mikael Karlsson <mikael@creado.se>
%%% @see yapp_handler 
%%% @doc Yapp event handler. 
%%% <p>Listens for hup events from the Yaws event manager and makes
%%% sure that the yapp_handler reinitialise all yapps in case a hup
%%% (which resets the Yaws configuration from yaws.conf) occurs. 
%%% </p>

-module(yapp_event_handler).

-behaviour(gen_event).
%% API
-export([add_handler/2]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {yapp_server}).

%%--------------------------------------------------------------------
%% Function: add_handler(EventManager, YappServer) -> ok | {'EXIT',Reason} | term()
%% Description: Adds an event handler
%%--------------------------------------------------------------------
add_handler(EventManager, YappServer) ->
    gen_event:add_handler(EventManager, ?MODULE, [YappServer]).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
init([YappServer]) ->
    {ok, #state{yapp_server = YappServer}}.

%%--------------------------------------------------------------------
%% Function:  
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event. 
%%--------------------------------------------------------------------
handle_event({yaws_hupped, _Res}, State) ->
    io:format("yapp got hup event~n"),
    YappServ = State#state.yapp_server,
    YappServ ! init_yapps,
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1, 
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% Description: Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event 
%% handler to handle the request.
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% Description: This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description:Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and 
%% do any necessary cleaning up. 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState} 
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
