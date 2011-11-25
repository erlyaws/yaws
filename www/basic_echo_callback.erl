%%%=====================================================
%%% compiled using erlc -I include www/basic_echo_callback.erl
%%%=====================================================

-module(basic_echo_callback).

-export([handle_message/1]).

handle_message({text, Message}) ->
    io:format("basic echo handler got ~p~n",
	      [Message]),
    {reply, <<"yaws says ",Message/binary>>}.
