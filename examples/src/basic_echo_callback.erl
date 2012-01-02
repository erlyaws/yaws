%%%===========================================================
%%% compiled using erlc -I include src/basic_echo_callback.erl
%%%===========================================================

-module(basic_echo_callback).

%% Export for websocket callbacks
-export([handle_message/1]).

%% Export for apply
-export([say_hi/1]).

handle_message({text, <<"bye">>}) ->
    io:format("User said bye.~n", []),
    {close, normal};

handle_message({text, <<"something">>}) ->
    io:format("Some action without a reply~n", []),
    noreply;

handle_message({text, <<"say hi later">>}) ->
    io:format("saying hi in 3s.~n", []),
    timer:apply_after(3000, ?MODULE, say_hi, [self()]),
    {reply, {text, <<"I'll say hi in a bit...">>}};

handle_message({text, Message}) ->
    io:format("basic echo handler got ~p~n", [Message]),
    {reply, {text, <<Message/binary>>}};

handle_message({binary, Message}) ->
    {reply, {binary, Message}}.


say_hi(Pid) ->
    io:format("asynchronous greeting~n", []),
    yaws_api:websocket_send(Pid, {text, <<"hi there!">>}).
