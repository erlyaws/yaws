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
    {FragType, Data};

%% non-final continuation of a fragmented message
handle_message( #ws_frame_info{ fin=0,
				data=Data,
				opcode=continuation},
		{FragType, Acc}) ->
    {FragType, <<Acc/binary,Data/binary>>};

%% end of text fragmented message
handle_message( #ws_frame_info{ fin=1, 
				opcode=continuation, 
				data=Data,
				ws_state=State},
		{text, Acc}) ->
    Unfragged = <<Acc/binary, Data/binary>>,
    echo_text(State, Unfragged),
    {none, <<>>};

handle_message( #ws_frame_info{opcode=text, data=Data, ws_state=State}, Acc) ->
    echo_text(State, Data),
    Acc;

%% end of binary fragmented message
handle_message( #ws_frame_info{ fin=1, 
				opcode=continuation, 
				data=Data,
				ws_state=State },
		{binary, Acc}) ->
    Unfragged = <<Acc/binary, Data/binary>>,
    io:format("echoing back binary message~n",[]),
    yaws_api:websocket_send(State, {binary, Unfragged}),
    {none, <<>>};

handle_message( #ws_frame_info{ opcode=binary, 
				data=Data,
				ws_state=State }, 
		Acc) ->
    io:format("echoing back binary message~n",[]),
    yaws_api:websocket_send(State, {binary, Data}), 
    Acc;

handle_message( #ws_frame_info{ opcode=ping, 
				data=Data,
				ws_state=WSState}, 
		Acc) ->
    io:format("replying pong to ping~n",[]),
    yaws_api:websocket_send(WSState, {pong, Data}),
    Acc;

handle_message(#ws_frame_info{opcode=pong, ws_state=WebSocket}, Acc) ->
    % A response to an unsolicited pong frame is not expected.
    % http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-08#section-4
    io:format("ignoring unsolicited pong~n",[]),
    Acc;

handle_message(FrameInfo=#ws_frame_info{}, Acc) ->
    io:format("WS Endpoint Unhandled message: ~p~n~p~n", [FrameInfo, Acc]),
%    fprof:trace(stop),
    exit(normal).

% for debugging
frame_info(Frame = <<Fin:1,Rsv1:1,Rsv2:1,Rsv3:1,Opcode:4,Masked:1,Len1:7,Rest1/binary>>) ->
    FrameInfo = [{fin, Fin},{rsv1,Rsv1},{rsv2,Rsv2},{rsv3,Rsv3},{opcode,Opcode},{masked,Masked},{len1,Len1}].


echo_text(WebSocket, Data) ->
    io:format("echoing back text message~n",[]),
%    CharCount = length(binary_to_list(Data)),
%    io:format("Text Chars Count = ~p~n", [CharCount]),
%    if
%	CharCount < 1000 ->
%	    io:format("Got data from Websocket: ~p~n", [Data]);
%	true ->
%	    io:format("Too many chars to print easily...~n",[])
%    end,

    yaws_api:websocket_send(WebSocket, {text, Data}).
