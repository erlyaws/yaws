%%%===========================================================
%%% compiled using erlc -I include src/basic_echo_callback.erl
%%%===========================================================

-module(basic_echo_callback_extended).

%% Export for websocket callbacks
-export([init/1, terminate/2, handle_open/2, handle_message/2]).

%% Export for apply
-export([say_hi/1]).

-record(state, {nb_texts=0, nb_bins=0}).

init([_Arg, Params]) ->
    io:format("Initalize ~p: ~p~n", [self(), Params]),
    {ok, #state{}}.

handle_open(WSState, State) ->
    yaws_websockets:send(WSState, {text, <<"Welcome !">>}),
    {ok, State}.

handle_message({text, <<"bye">>}, #state{nb_texts=N, nb_bins=M}) ->
    io:format("User said bye. ~p text / ~p binary messages echoed ~n", [N, M]),
    NbTexts = list_to_binary(integer_to_list(N)),
    NbBins  = list_to_binary(integer_to_list(M)),
    Messages = [
                {text, <<"Goodbye !">>},
                {text, <<NbTexts/binary, " text messages echoed">>},
                {text, <<NbBins/binary, " binary messages echoed">>}
               ],
    {close, Messages, {1000, <<"bye">>}};

handle_message({text, <<"something">>}, State) ->
    io:format("Some action without a reply~n", []),
    {noreply, State};

handle_message({text, <<"say hi later">>}, State) ->
    timer:apply_after(3000, ?MODULE, say_hi, [self()]),
    {noreply, State};

handle_message({text, Msg}, #state{nb_texts=N}=State) ->
    io:format("Receive text message (N=~p): ~p bytes~n", [N, byte_size(Msg)]),
    {reply, {text, Msg}, State#state{nb_texts=N+1}};

handle_message({binary, Msg}, #state{nb_bins=M}=State) ->
    io:format("Receive binary message (M=~p): ~p bytes~n", [M, byte_size(Msg)]),
    {reply, {text, Msg}, State#state{nb_bins=M+1}};

handle_message(timeout, State) ->
    io:format("process timed out~n", []),
    {reply, {text, <<"Anybody Else ?">>}, State};

handle_message({close, Status, Reason}, _) ->
    io:format("Close connection: ~p - ~p~n", [Status, Reason]),
    {close, Status}.


terminate(Reason, State) ->
    io:format("terminate ~p: ~p (state:~p)~n", [self(), Reason, State]),
    ok.

say_hi(Pid) ->
    io:format("asynchronous greeting~n", []),
    yaws_api:websocket_send(Pid, {text, <<"hi there!">>}).
