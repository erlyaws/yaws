%%%-------------------------------------------------------------------
%%% File    : yaws_stats.erl
%%% Author  : Olivier Girondel <olivier@biniou.info>
%%% Description : Statistics module for Yaws
%%%
%%% Created : 11 Apr 2009 by Olivier Girondel <olivier@biniou.info>
%%%-------------------------------------------------------------------
-module(yaws_stats).

-behaviour(gen_server).

-include("../include/yaws.hrl").

%% API
-export([start_link/0, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([hit/0, sent/1]).
-export([get/1]).

%% statistics
-record(stats, {
          hits = 0,
          sent = 0
         }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).


stop(Pid) ->
    gen_server:cast(Pid, {stop}).


hit() ->
    case get_stats() of
        undefined ->
            ok;
        Pid ->
            gen_server:cast(Pid, {hit})
    end.

sent({ok, Bytes}) ->
    sent(Bytes);
sent({error, _}) ->
    ignore;
sent(Bytes) ->
    case get_stats() of
        undefined ->
            ok;
        Pid ->
            gen_server:cast(Pid, {sent, Bytes})
    end.


get(Pid) ->
    case Pid of
        undefined ->
            undefined;
        Pid ->
            gen_server:call(Pid, {get})
    end.


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
    {ok, #stats{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get}, _From, State) ->
    Reply = {State#stats.hits, State#stats.sent},
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
handle_cast({hit}, Stats) ->
    NewHits = Stats#stats.hits+1,
    {noreply, Stats#stats{hits=NewHits}};


handle_cast({sent, Bytes}, Stats) ->
    NewSent = Stats#stats.sent+Bytes,
    {noreply, Stats#stats{sent=NewSent}};


handle_cast({stop}, Stats) ->
    {stop, normal, Stats};


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
get_stats() ->
    (erlang:get(sc))#sconf.stats.
