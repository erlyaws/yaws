%%%=====================================================
%%% compiled using erlc -I include www/echo_callback.erl
%%%=====================================================

-module(advanced_echo_callback).

-export([handle_message/2]).

-include("yaws_api.hrl").


%% start of a fragmented message
handle_message( #ws_frame_info{ fin=0, 
				opcode=FragType, 
				data=Data },
		{none, <<>>}) ->
    {noreply, {FragType, Data}};

%% non-final continuation of a fragmented message
handle_message( #ws_frame_info{ fin=0,
				data=Data,
				opcode=continuation},
		{FragType, Acc}) ->
    {noreply, {FragType, <<Acc/binary,Data/binary>>}};

%% end of text fragmented message
handle_message( #ws_frame_info{ fin=1, 
				opcode=continuation, 
				data=Data},
		{text, Acc}) ->
    Unfragged = <<Acc/binary, Data/binary>>,
    {reply, {text, Unfragged}, {none, <<>>}};

%% one full non-fragmented message
handle_message( #ws_frame_info{opcode=text, data=Data}, AccStuff) ->
    {reply, {text, Data}, AccStuff};

%% end of binary fragmented message
handle_message( #ws_frame_info{ fin=1, 
				opcode=continuation, 
				data=Data },
		{binary, Acc}) ->
    Unfragged = <<Acc/binary, Data/binary>>,
    io:format("echoing back binary message~n",[]),
    {reply, {binary, Unfragged}, {none, <<>>}};

% one full non-fragmented binary message
handle_message( #ws_frame_info{ opcode=binary, 
				data=Data}, 
		AccStuff) ->
    io:format("echoing back binary message~n",[]),
    {reply, {binary, Data}, AccStuff};

handle_message( #ws_frame_info{ opcode=ping, 
				data=Data}, 
		AccStuff) ->
    io:format("replying pong to ping~n",[]),
    {reply, {pong, Data}, AccStuff};

handle_message(#ws_frame_info{opcode=pong}, AccStuff) ->
    % A response to an unsolicited pong frame is not expected.
    % http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-08#section-4
    io:format("ignoring unsolicited pong~n",[]),
    {noreply, AccStuff};

handle_message(FrameInfo=#ws_frame_info{}, AccStuff) ->
    io:format("WS Endpoint Unhandled message: ~p~n~p~n", [FrameInfo, AccStuff]),
    {close, {error, {unhandled_message, FrameInfo}}}.
