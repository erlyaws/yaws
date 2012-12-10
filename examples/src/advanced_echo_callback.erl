%%%==============================================================
%%% compiled using erlc -I include src/advanced_echo_callback.erl
%%%==============================================================

-module(advanced_echo_callback).

-export([handle_message/2]).

-include("yaws_api.hrl").

%% define callback state to accumulate a fragmented WS message
%% which we echo back when all fragments are in, returning to
%% initial state.
-record(state, {frag_type = none,               % fragment type
                acc = <<>>}).                   % accumulate fragment data


%% unfragmented text message
handle_message(#ws_frame_info{fin=1, opcode=text, data=Data},
               #state{frag_type=none, acc = <<>>}=State) ->
    case unicode:characters_to_binary(Data, utf8, utf8) of
        Data -> {reply, {text, Data}, State};
        _    -> {close, {1007, <<"invalid utf-8">>}}
    end;

%% start of a fragmented text message
handle_message(#ws_frame_info{fin=0, opcode=text, data=Data},
               #state{frag_type=none, acc = <<>>}) ->
    case unicode:characters_to_binary(Data, utf8, utf8) of
        Data ->
            {noreply, #state{frag_type=text, acc={[Data], <<>>}}};
        {incomplete, Dec, Rest} ->
            {noreply, #state{frag_type=text, acc={[Dec], Rest}}};
        _ ->
            {close, {1007, <<"invalid utf-8">>}}
    end;

%% non-final continuation of a fragmented text message
handle_message(#ws_frame_info{fin=0, data=Data, opcode=continuation},
               #state{frag_type=text, acc={Dec0, Rest0}}=State) ->
    Data1 = <<Rest0/binary, Data/binary>>,
    case unicode:characters_to_binary(Data1, utf8, utf8) of
        Data1 ->
            {noreply, State#state{acc={[Data1|Dec0], <<>>}}};
        {incomplete, Dec1, Rest1} ->
            {noreply, State#state{acc={[Dec1|Dec0],  Rest1}}};
        _ ->
            {close, {1007, <<"invalid utf-8">>}}
    end;

%% end of text fragmented message
handle_message(#ws_frame_info{fin=1, opcode=continuation, data=Data},
               #state{frag_type=text, acc={Dec, Rest}}) ->
    Data1 = <<Rest/binary, Data/binary>>,
    case unicode:characters_to_binary(Data1, utf8, utf8) of
        Data1 ->
            Msg = list_to_binary(lists:reverse([Data1|Dec])),
            {reply, {text, Msg}, #state{}};
        _ ->
            {close, {1007, <<"invalid utf-8">>}}
    end;

%% unfragmented binary message
handle_message(#ws_frame_info{fin=1, opcode=binary, data=Data},
               #state{frag_type=none, acc = <<>>}=State) ->
    {reply, {binary, Data}, State};

%% start of a fragmented binary message
handle_message(#ws_frame_info{fin=0, opcode=binary, data=Data},
               #state{frag_type=none, acc = <<>>}) ->
    {noreply, #state{frag_type=binary, acc=Data}};

%% non-final continuation of a fragmented binary message
handle_message(#ws_frame_info{fin=0, data=Data, opcode=continuation},
               #state{frag_type=binary, acc=FragAcc}=State) ->
    {noreply, State#state{acc = <<FragAcc/binary,Data/binary>>}};

%% end of binary fragmented message
handle_message(#ws_frame_info{fin=1, opcode=continuation, data=Data},
               #state{frag_type=binary, acc=FragAcc}) ->
    Unfragged = <<FragAcc/binary, Data/binary>>,
    {reply, {binary, Unfragged}, #state{}};


handle_message(#ws_frame_info{opcode=ping, data=Data}, State) ->
    io:format("replying pong to ping~n",[]),
    {reply, {pong, Data}, State};

handle_message(#ws_frame_info{opcode=pong}, State) ->
    %% A response to an unsolicited pong frame is not expected.
    %% http://tools.ietf.org/html/\
    %%        draft-ietf-hybi-thewebsocketprotocol-08#section-4
    io:format("ignoring unsolicited pong~n",[]),
    {noreply, State};

%% According to RFC 6455 section 5.4, control messages like close
%% MAY be injected in the middle of a fragmented message, which is
%% why we pass FragType and FragAcc along below. Whether any clients
%% actually do this in practice, I don't know.
handle_message(#ws_frame_info{opcode=close, length=Len,
                              data=Data, ws_state=WSState},
               _State) ->
    Reason = case Len of
                 0  -> {1000, <<>>};
                  1 -> {1002, <<"protocol error">>};
                  _ ->
                      <<Status:16/big, Msg/binary>> = Data,
                      case unicode:characters_to_binary(Msg, utf8, utf8) of
                          Msg -> {check_close_code(Status, WSState), Msg};
                          _   -> {1007, <<"invalid utf-8">>}
                      end
              end,
    io:format("got close. reply reason: ~p~n", [Reason]),
    {close, Reason};

handle_message(#ws_frame_info{}=FrameInfo, State) ->
    io:format("WS Endpoint Unhandled message: ~p~n~p~n", [FrameInfo, State]),
    {close, {1002, <<"protocol error">>}};

handle_message({fail_connection, Status, Msg}, State) ->
    io:format("Connection failure: ~p:~p~n~p~n", [Status, Msg, State]),
    {close, {Status, Msg}}.


%% The checks for close status codes here are based on RFC 6455 and on the
%% autobahn testsuite (http://autobahn.ws/testsuite).
check_close_code(Code, WSState) ->
    if
        Code >= 3000 andalso Code =< 4999 ->
            Code;
        Code < 1000 ->
            1002;
        Code == 1006 andalso WSState#ws_state.sock == undefined ->
            Code;
        Code >= 1004 andalso Code =< 1006 ->
            1002;
        Code > 1011 ->
            1002;
        true ->
            Code
    end.
