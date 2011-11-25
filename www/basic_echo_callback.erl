%%%=====================================================
%%% compiled using erlc -I include www/basic_echo_callback.erl
%%%=====================================================

-module(basic_echo_callback).

-export([handle_message/1]).

handle_message({text, <<"bye">>}) ->
    io:format("User said bye.~n",
	      []),
    {close, normal};
handle_message({text, <<"something">>}) ->
    io:format("Some action without a reply~n",
	      []),
    {noreply};
handle_message({text, Message}) ->
    io:format("basic echo handler got ~p~n",
	      [Message]),
    {reply, {text, <<"yaws says ",Message/binary>>}}.
