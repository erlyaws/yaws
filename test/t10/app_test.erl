-module(app_test).
-include("../include/tftest.hrl").
-compile(export_all).

-record(frame, {fin     = true,
                rsv     = 0,
                opcode,
                masked  = false,
                mask,
                payload = <<>>}).

-define(WS_OPCODE_CONTINUATION, 16#00).
-define(WS_OPCODE_TEXT,         16#01).
-define(WS_OPCODE_BINARY,       16#02).
-define(WS_OPCODE_CLOSE,        16#08).
-define(WS_OPCODE_PING,         16#09).
-define(WS_OPCODE_PONG,         16#0A).

-define(WS_STATUS_NORMAL,           1000).
-define(WS_STATUS_PROTO_ERROR,      1002).
-define(WS_STATUS_ABNORMAL_CLOSURE, 1006).
-define(WS_STATUS_INVALID_PAYLOAD,  1007).
-define(WS_STATUS_MSG_TOO_BIG,      1009).
-define(WS_STATUS_INTERNAL_ERROR,   1011).

%% Way to invoke just one test
start([F]) ->
    apply(app_test, F, []),
    ok.


start() ->
    io:format("~n ==== WEBSOCKET TESTS ==== ~n~n", []),

    test_valid_opening_handshake(),
    test_bad_version_handshake(),
    test_bad_origin_handshake(),
    test_noconnection_handshake(),
    test_bad_connection_handshake(),
    test_noupgrade_handshake(),
    test_bad_upgrade_handshake(),

    test_basic_unfragmented_text(0, all),
    test_basic_unfragmented_text(125, all),
    test_basic_unfragmented_text(126, all),
    test_basic_unfragmented_text(127, all),
    test_basic_unfragmented_text(128, all),
    test_basic_unfragmented_text(65535, all),
    test_basic_unfragmented_text(65536, all),
    test_basic_unfragmented_text(65536, 997),
    test_advanced_unfragmented_text(0, all),
    test_advanced_unfragmented_text(125, all),
    test_advanced_unfragmented_text(126, all),
    test_advanced_unfragmented_text(127, all),
    test_advanced_unfragmented_text(128, all),
    test_advanced_unfragmented_text(65535, all),
    test_advanced_unfragmented_text(65536, all),
    test_advanced_unfragmented_text(65536, 997),

    test_basic_unfragmented_binary(0, all),
    test_basic_unfragmented_binary(125, all),
    test_basic_unfragmented_binary(126, all),
    test_basic_unfragmented_binary(127, all),
    test_basic_unfragmented_binary(128, all),
    test_basic_unfragmented_binary(65535, all),
    test_basic_unfragmented_binary(65536, all),
    test_basic_unfragmented_binary(65536, 997),
    test_advanced_unfragmented_binary(0, all),
    test_advanced_unfragmented_binary(125, all),
    test_advanced_unfragmented_binary(126, all),
    test_advanced_unfragmented_binary(127, all),
    test_advanced_unfragmented_binary(128, all),
    test_advanced_unfragmented_binary(65535, all),
    test_advanced_unfragmented_binary(65536, all),
    test_advanced_unfragmented_binary(65536, 997),

    test_basic_ping_text(0, all),
    test_basic_ping_text(125, all),
    test_basic_ping_binary(125, all),
    test_basic_ping_binary(125, 1),
    test_advanced_ping_text(0, all),
    test_advanced_ping_text(125, all),
    test_advanced_ping_binary(125, all),
    test_advanced_ping_binary(125, 1),
    test_toolong_payload_ping(),
    test_basic_unsolicited_pong(0),
    test_basic_unsolicited_pong(125),
    test_advanced_unsolicited_pong(0),
    test_advanced_unsolicited_pong(125),
    test_basic_unsolicited_pong_ping_pong(),
    test_advanced_unsolicited_pong_ping_pong(),
    test_basic_10_pings(all),
    test_basic_10_pings(1),
    test_advanced_10_pings(all),
    test_advanced_10_pings(1),

    test_badrsv_text(),
    test_badrsv_binary(),
    test_badrsv_ping(),
    test_badrsv_close(),
    test_badrsv_complex(all),
    test_badrsv_complex(1),

    test_badopcodes(),

    test_basic_fragmented_empty(),
    test_basic_fragmented_text_1(),
    test_basic_fragmented_binary_1(),
    test_basic_fragmented_text_2(),
    test_basic_fragmented_binary_2(),
    test_basic_fragmented_ping(),
    test_basic_fragmented_pong(),
    test_basic_fragmented_close(),
    test_basic_fragmented_text_with_ping(),
    test_basic_fragmented_text_with_pong(),
    test_basic_badfragmented_1(),
    test_basic_badfragmented_2(),
    test_basic_badfragmented_nocontinuation(),
    test_advanced_fragmented_empty(),
    test_advanced_fragmented_text_1(),
    test_advanced_fragmented_binary_1(),
    test_advanced_fragmented_text_2(),
    test_advanced_fragmented_binary_2(),
    test_advanced_fragmented_ping(),
    test_advanced_fragmented_pong(),
    test_advanced_fragmented_close(),
    test_advanced_fragmented_text_with_ping(),
    test_advanced_fragmented_text_with_pong(),
    test_advanced_badfragmented_1(),
    test_advanced_badfragmented_2(),
    test_basic_badfragmented_nocontinuation(),

    test_basic_unfragmented_valid_utf8_text(all),
    test_basic_unfragmented_valid_utf8_text(1),
    test_basic_fragmented_valid_utf8_text(all),
    test_basic_fragmented_valid_utf8_text(1),
    test_advanced_unfragmented_valid_utf8_text(all),
    test_advanced_unfragmented_valid_utf8_text(1),
    test_advanced_fragmented_valid_utf8_text(all),
    test_advanced_fragmented_valid_utf8_text(1),

    test_basic_unfragmented_invalid_utf8_text(all),
    test_basic_unfragmented_invalid_utf8_text(1),
    test_basic_fragmented_invalid_utf8_text(all),
    test_basic_fragmented_invalid_utf8_text(1),
    test_advanced_unfragmented_invalid_utf8_text(all),
    test_advanced_unfragmented_invalid_utf8_text(1),
    test_advanced_fragmented_invalid_utf8_text(all),
    test_advanced_fragmented_invalid_utf8_text(1),

    test_basic_2_closes(),
    test_basic_close_ping(),
    test_basic_close_text(),
    test_basic_fragtext_close_fragtext(),
    test_basic_close_empty(),
    test_basic_close_toosmall(),
    test_basic_close_statusonly(),
    test_basic_close_with_reason(),
    test_basic_close_limit_size(),
    test_basic_close_toolong(),
    test_basic_close_invalid_utf8(),
    test_basic_close_valid_codes(),
    test_basic_close_invalid_codes(),
    test_advanced_2_closes(),
    test_advanced_close_ping(),
    test_advanced_close_text(),
    test_advanced_fragtext_close_fragtext(),
    test_advanced_close_empty(),
    test_advanced_close_toosmall(),
    test_advanced_close_statusonly(),
    test_advanced_close_with_reason(),
    test_advanced_close_limit_size(),
    test_advanced_close_toolong(),
    test_advanced_close_invalid_utf8(),
    test_advanced_close_valid_codes(),
    test_advanced_close_invalid_codes(),

    test_close_timeout(),
    test_keepalive_timeout(),
    test_too_big_frame(),
    test_too_big_message(),
    test_close_unmasked_frame(),
    test_secure_websocket(),
    ok.


%% ----
test_valid_opening_handshake() ->
    io:format("valid_opening_handshake~n",[]),
    WSPath = "/websockets_example_endpoint.yaws",
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",

    %% Send the handshake and retrieve the response
    ?line {ok, Sock}       = open("localhost", 8000),
    ?line {ok, {101, Hds}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    %% Check the server response
    ?line true = is_valid_handshake_hash(
                   Key, proplists:get_value("sec-websocket-accept", Hds)
                  ),
    ?line "websocket" = string:to_lower(proplists:get_value("upgrade",    Hds)),
    ?line "upgrade"   = string:to_lower(proplists:get_value("connection", Hds)),

    %% Close the webscoket and check the server reply
    ?line ok   = wsclose(Sock, ?WS_STATUS_NORMAL, <<>>),
    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_bad_version_handshake() ->
    io:format("bad_version_handshake~n",[]),
    WSPath = "/websockets_example_endpoint.yaws",
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",

    %% Send the handshake and retrieve the response
    ?line {ok, Sock}       = open("localhost", 8000),
    ?line {ok, {400, Hds}} = wsopen(Sock, Key, WSPath, "http://localhost", 15),

    %% Check the server response
    ?line "13, 8" = string:to_lower(
                      proplists:get_value("sec-websocket-version", Hds)
                     ),

    ?line ok = close(Sock),
    ok.


%% ----
test_bad_origin_handshake() ->
    io:format("bad_origin_handshake~n",[]),
    WSPath = "/websockets_example_endpoint.yaws",
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",

    %% Send the handshake and retrieve the response
    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {403, _}} = wsopen(Sock, Key, WSPath, "http://otherhost", 13),

    ?line ok = close(Sock),
    ok.


%% ----
test_noconnection_handshake() ->
    io:format("noconnection_handshake~n",[]),
    WSPath = "/websockets_example_endpoint.yaws",
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",

    %% Send the handshake and retrieve the response
    ?line {ok, Sock}     = open("localhost", 8000),

    Handshake = ["GET ", WSPath, " HTTP/1.1\r\n",
                 "Host: localhost\r\n",
                 "Upgrade: websocket\r\n",
                 "Sec-WebSocket-Key: ", Key, "\r\n",
                 "Origin: http://localhost\r\n",
                 "Sec-WebSocket-Version: 13\r\n",
                 "\r\n"],
    case yaws_api:get_sslsocket(Sock) of
        {ok, SslSock} -> ssl:send(SslSock, Handshake);
        undefined     -> gen_tcp:send(Sock, Handshake)
    end,
    ?line {ok, {400, _}} = read_handshake_response(Sock),

    ?line ok = close(Sock),
    ok.


%% ----
test_bad_connection_handshake() ->
    io:format("bad_connection_handshake~n",[]),
    WSPath = "/websockets_example_endpoint.yaws",
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",

    %% Send the handshake and retrieve the response
    ?line {ok, Sock}     = open("localhost", 8000),

    Handshake = ["GET ", WSPath, " HTTP/1.1\r\n",
                 "Host: localhost\r\n",
                 "Upgrade: websocket\r\n",
                 "Connection: Keep-Alive\r\n",
                 "Sec-WebSocket-Key: ", Key, "\r\n",
                 "Origin: http://localhost\r\n",
                 "Sec-WebSocket-Version: 13\r\n",
                 "\r\n"],
    case yaws_api:get_sslsocket(Sock) of
        {ok, SslSock} -> ssl:send(SslSock, Handshake);
        undefined     -> gen_tcp:send(Sock, Handshake)
    end,
    ?line {ok, {400, _}} = read_handshake_response(Sock),

    ?line ok = close(Sock),
    ok.


%% ----
test_noupgrade_handshake() ->
    io:format("noupgrade_handshake~n",[]),
    WSPath = "/websockets_example_endpoint.yaws",
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",

    %% Send the handshake and retrieve the response
    ?line {ok, Sock}     = open("localhost", 8000),

    Handshake = ["GET ", WSPath, " HTTP/1.1\r\n",
                 "Host: localhost\r\n",
                 "Connection: Upgrade\r\n",
                 "Sec-WebSocket-Key: ", Key, "\r\n",
                 "Origin: http://localhost\r\n",
                 "Sec-WebSocket-Version: 13\r\n",
                 "\r\n"],
    case yaws_api:get_sslsocket(Sock) of
        {ok, SslSock} -> ssl:send(SslSock, Handshake);
        undefined     -> gen_tcp:send(Sock, Handshake)
    end,
    ?line {ok, {400, _}} = read_handshake_response(Sock),

    ?line ok = close(Sock),
    ok.


%% ----
test_bad_upgrade_handshake() ->
    io:format("bad_upgrade_handshake~n",[]),
    WSPath = "/websockets_example_endpoint.yaws",
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",

    %% Send the handshake and retrieve the response
    ?line {ok, Sock}     = open("localhost", 8000),

    Handshake = ["GET ", WSPath, " HTTP/1.1\r\n",
                 "Host: localhost\r\n",
                 "Upgrade: TLS/1.0\r\n",
                 "Connection: Upgrade\r\n",
                 "Sec-WebSocket-Key: ", Key, "\r\n",
                 "Origin: http://localhost\r\n",
                 "Sec-WebSocket-Version: 13\r\n",
                 "\r\n"],
    case yaws_api:get_sslsocket(Sock) of
        {ok, SslSock} -> ssl:send(SslSock, Handshake);
        undefined     -> gen_tcp:send(Sock, Handshake)
    end,
    ?line {ok, {400, _}} = read_handshake_response(Sock),

    ?line ok = close(Sock),
    ok.



%% ----
test_basic_unfragmented_text(Sz, BlockSz) ->
    io:format("basic_unfragmented_text (len=~p, blocksize=~p)~n",
              [Sz, BlockSz]),
    Payload = list_to_binary(lists:duplicate(Sz, $*)),
    do_test_unfragmented_msg("/websockets_example_endpoint.yaws",
                             ?WS_OPCODE_TEXT, Payload, BlockSz).

test_advanced_unfragmented_text(Sz, BlockSz) ->
    io:format("advanced_unfragmented_text (len=~p, blocksize=~p)~n",
              [Sz, BlockSz]),
    Payload = list_to_binary(lists:duplicate(Sz, $*)),
    do_test_unfragmented_msg("/websockets_autobahn_endpoint.yaws",
                             ?WS_OPCODE_TEXT, Payload, BlockSz).

test_basic_unfragmented_binary(Sz, BlockSz) ->
    io:format("basic_unfragmented_binary (len=~p, blocksize=~p)~n",
              [Sz, BlockSz]),
    Payload = list_to_binary(lists:duplicate(Sz, 16#fe)),
    do_test_unfragmented_msg("/websockets_example_endpoint.yaws",
                             ?WS_OPCODE_BINARY, Payload, BlockSz).

test_advanced_unfragmented_binary(Sz, BlockSz) ->
    io:format("advanced_unfragmented_binary (len=~p, blocksize=~p)~n",
              [Sz, BlockSz]),
    Payload = list_to_binary(lists:duplicate(Sz, 16#fe)),
    do_test_unfragmented_msg("/websockets_autobahn_endpoint.yaws",
                             ?WS_OPCODE_BINARY, Payload, BlockSz).

do_test_unfragmented_msg(WSPath, Type, Payload, BlockSz) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    %% unmasked
    SndFrame1 = #frame{opcode=Type, payload=Payload},
    ?line ok = send_frame(Sock, SndFrame1, BlockSz),
    ?line {ok, RcvFrame1} = read_frame(Sock),
    ?line Type    = RcvFrame1#frame.opcode,
    ?line Payload = RcvFrame1#frame.payload,

    %% masked
    SndFrame2 = SndFrame1#frame{masked=true, mask = <<"abcd">>},
    ?line ok = send_frame(Sock, SndFrame2, BlockSz),
    ?line {ok, RcvFrame2} = read_frame(Sock),
    ?line Type    = RcvFrame2#frame.opcode,
    ?line Payload = RcvFrame2#frame.payload,

    ?line ok = wsclose(Sock, ?WS_STATUS_NORMAL, <<>>),
    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok   = close(Sock),
    ok.


%% ---
test_basic_ping_text(Sz, BlockSz) ->
    io:format("basic_ping_text (len=~p, blocksize=~p)~n", [Sz, BlockSz]),
    Payload = list_to_binary(lists:duplicate(Sz, $*)),
    do_test_ping_msg("/websockets_example_endpoint.yaws", Payload, BlockSz).

test_advanced_ping_text(Sz, BlockSz) ->
    io:format("advanced_ping_text (len=~p, blocksize=~p)~n", [Sz, BlockSz]),
    Payload = list_to_binary(lists:duplicate(Sz, $*)),
    do_test_ping_msg("/websockets_autobahn_endpoint.yaws", Payload, BlockSz).

test_basic_ping_binary(Sz, BlockSz) ->
    io:format("basic_ping_binary (len=~p, blocksize=~p)~n", [Sz, BlockSz]),
    Payload = list_to_binary(lists:duplicate(Sz, 16#fe)),
    do_test_ping_msg("/websockets_example_endpoint.yaws", Payload, BlockSz).

test_advanced_ping_binary(Sz, BlockSz) ->
    io:format("advanced_ping_binary (len=~p, blocksize=~p)~n", [Sz, BlockSz]),
    Payload = list_to_binary(lists:duplicate(Sz, 16#fe)),
    do_test_ping_msg("/websockets_autobahn_endpoint.yaws", Payload, BlockSz).


do_test_ping_msg(WSPath, Payload, BlockSz) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    %% unmasked
    SndFrame1 = #frame{opcode=?WS_OPCODE_PING, payload=Payload},
    ?line ok = send_frame(Sock, SndFrame1, BlockSz),
    ?line {ok, RcvFrame1} = read_frame(Sock),
    ?line ?WS_OPCODE_PONG = RcvFrame1#frame.opcode,
    ?line Payload = RcvFrame1#frame.payload,

    %% masked
    SndFrame2 = SndFrame1#frame{masked=true, mask = <<"abcd">>},
    ?line ok = send_frame(Sock, SndFrame2, BlockSz),
    ?line {ok, RcvFrame2} = read_frame(Sock),
    ?line ?WS_OPCODE_PONG = RcvFrame2#frame.opcode,
    ?line Payload = RcvFrame2#frame.payload,

    ?line ok = wsclose(Sock, ?WS_STATUS_NORMAL, <<>>),
    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_toolong_payload_ping() ->
    io:format("toolong_payload_ping ~n", []),
    WSPath  = "/websockets_example_endpoint.yaws",
    Key     = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload = list_to_binary(lists:duplicate(126, 16#fe)),

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    SndFrame = #frame{opcode=?WS_OPCODE_PING, payload=Payload},
    ?line ok = send_frame(Sock, SndFrame, all),

    ?line {ok, Frames} = wsflush(Sock, false),
    ?line true     = is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR]),
    ?line {ok, []} = wsflush(Sock, true),
    ?line ok       = close(Sock),
    ok.


%% ----
test_basic_unsolicited_pong(Sz) ->
    io:format("basic_unsolicited_pong (len=~p)~n", [Sz]),
    Payload = list_to_binary(lists:duplicate(Sz, 16#fe)),
    do_test_unsolicited_pong_msg("/websockets_example_endpoint.yaws", Payload).

test_advanced_unsolicited_pong(Sz) ->
    io:format("advanced_unsolicited_pong (len=~p)~n", [Sz]),
    Payload = list_to_binary(lists:duplicate(Sz, 16#fe)),
    do_test_unsolicited_pong_msg("/websockets_autobahn_endpoint.yaws", Payload).

do_test_unsolicited_pong_msg(WSPath, Payload) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    SndFrame = #frame{opcode=?WS_OPCODE_PONG, payload=Payload},
    ?line ok = send_frame(Sock, SndFrame, all),

    ?line ok            = wsclose(Sock, ?WS_STATUS_NORMAL, <<>>),
    ?line {ok, Frames}  = wsflush(Sock, true),
    ?line true          = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok            = close(Sock),
    ok.


%% ----
test_basic_unsolicited_pong_ping_pong() ->
    io:format("basic_unsolicited_pong_ping_pong~n", []),
    do_test_unsolicited_pong_ping_pong("/websockets_example_endpoint.yaws").

test_advanced_unsolicited_pong_ping_pong() ->
    io:format("advanced_unsolicited_pong_ping_pong~n", []),
    do_test_unsolicited_pong_ping_pong("/websockets_autobahn_endpoint.yaws").

do_test_unsolicited_pong_ping_pong(WSPath) ->
    Key      = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload1 = list_to_binary(lists:duplicate(125, $*)),
    Payload2 = <<"ping payload">>,

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    SndFrame1 = #frame{opcode=?WS_OPCODE_PONG, payload=Payload1},
    ?line ok = send_frame(Sock, SndFrame1, all),

    SndFrame2 = #frame{opcode=?WS_OPCODE_PING, payload=Payload2},
    ?line ok  = send_frame(Sock, SndFrame2, all),
    ?line {ok, RcvFrame2} = read_frame(Sock),
    ?line ?WS_OPCODE_PONG = RcvFrame2#frame.opcode,
    ?line Payload2        = RcvFrame2#frame.payload,

    ?line ok = wsclose(Sock, ?WS_STATUS_NORMAL, <<>>),
    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_basic_10_pings(BlockSz) ->
    io:format("basic_10_pings (blocksize=~p)~n", [BlockSz]),
    do_test_10_pings("/websockets_example_endpoint.yaws", BlockSz).

test_advanced_10_pings(BlockSz) ->
    io:format("advanced_10_pings (blocksize=~p)~n", [BlockSz]),
    do_test_10_pings("/websockets_autobahn_endpoint.yaws", BlockSz).

do_test_10_pings(WSPath, BlockSz) ->
    Key     = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload = <<"ping payload">>,

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    SndFrame = #frame{opcode=?WS_OPCODE_PING, payload=Payload},
    ?line ok = apply_loop(10, fun send_frame/3, [Sock, SndFrame, BlockSz], ok),

    ?line ok = wsclose(Sock, ?WS_STATUS_NORMAL, <<>>),
    ?line {ok, Frames}  = wsflush(Sock, true),
    {Frames1, Frames2}  = lists:split(10, Frames),
    ?line true = lists:all(fun(#frame{payload=P}) -> P == Payload end, Frames1),
    ?line true = is_valid_close_frame(Frames2, [?WS_STATUS_NORMAL]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_badrsv_text() ->
    io:format("badrsv_text (rsv=1)~n", []),
    do_test_badrsv("/websockets_example_endpoint.yaws", ?WS_OPCODE_TEXT, 1).

test_badrsv_binary() ->
    io:format("badrsv_binary (rsv=2)~n", []),
    do_test_badrsv("/websockets_example_endpoint.yaws", ?WS_OPCODE_BINARY, 2).

test_badrsv_ping() ->
    io:format("badrsv_ping (rsv=3)~n", []),
    do_test_badrsv("/websockets_example_endpoint.yaws", ?WS_OPCODE_PING, 3).

test_badrsv_close() ->
    io:format("badrsv_close (rsv=4)~n", []),
    do_test_badrsv("/websockets_example_endpoint.yaws", ?WS_OPCODE_CLOSE, 4).

do_test_badrsv(WSPath, Type, Rsv) ->
    Key     = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload = <<"small payload">>,

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    SndFrame = #frame{rsv=Rsv, opcode=Type, payload=Payload},
    ?line ok = send_frame(Sock, SndFrame, all),

    ?line {ok, Frames} = wsflush(Sock, false),
    ?line true     = is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR]),
    ?line {ok, []} = wsflush(Sock, true),
    ?line ok       = close(Sock),
    ok.


%% ----
test_badrsv_complex(BlockSz) ->
    io:format("badrsv_complex (rsv=5, blocksize=~p)~n", [BlockSz]),
    WSPath  = "/websockets_example_endpoint.yaws",
    Key     = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload = <<"small payload">>,

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    SndFrame1 = #frame{opcode=?WS_OPCODE_TEXT, payload=Payload},
    SndFrame2 = SndFrame1#frame{rsv=5},
    SndFrame3 = #frame{opcode=?WS_OPCODE_PING, payload=Payload},

    ?line ok = send_frame(Sock, SndFrame1, BlockSz),
    ?line ok = send_frame(Sock, SndFrame2, BlockSz),
    ?line ok = send_frame(Sock, SndFrame3, BlockSz),

    ?line {ok, [Frame1|Frames]} = wsflush(Sock, false),
    ?line ?WS_OPCODE_TEXT = Frame1#frame.opcode,
    ?line Payload  = Frame1#frame.payload,
    ?line true     = is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR]),
    ?line {ok, []} = wsflush(Sock, true),
    ?line ok       = close(Sock),
    ok.


%% ----
test_badopcodes() ->
    [test_badopcodes(O) || O <- [3,4,5,6,7,11,12,13,14,15]],
    ok.

test_badopcodes(Opcode) ->
    io:format("badopcodes (opcode=~p)~n", [Opcode]),
    WSPath  = "/websockets_example_endpoint.yaws",
    Key     = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    SndFrame = #frame{opcode=Opcode},
    ?line ok = send_frame(Sock, SndFrame, all),

    ?line {ok, Frames} = wsflush(Sock, false),
    ?line true     = is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR]),
    ?line {ok, []} = wsflush(Sock, true),
    ?line ok       = close(Sock),
    ok.


%% ----
test_basic_fragmented_empty() ->
    io:format("basic_fragmented_empty~n", []),
    do_test_fragmented_empty("/websockets_example_endpoint.yaws").

test_advanced_fragmented_empty() ->
    io:format("advanced_fragmented_empty~n", []),
    do_test_fragmented_empty("/websockets_autobahn_endpoint.yaws").


do_test_fragmented_empty(WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    SndFrame1 = #frame{fin=false, opcode=?WS_OPCODE_TEXT},
    SndFrame2 = #frame{fin=false, opcode=?WS_OPCODE_CONTINUATION},
    SndFrame3 = #frame{opcode=?WS_OPCODE_CONTINUATION},

    ?line ok = send_frame(Sock, SndFrame1, all),
    ?line ok = send_frame(Sock, SndFrame2, all),
    ?line ok = send_frame(Sock, SndFrame3, all),
    ?line ok = wsclose(Sock, ?WS_STATUS_NORMAL, <<>>),

    ?line {ok, [Frame1|Frames]} = wsflush(Sock, true),
    ?line ?WS_OPCODE_TEXT = Frame1#frame.opcode,
    ?line <<>> = Frame1#frame.payload,
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok   = close(Sock).


%% ----
test_basic_fragmented_text_1() ->
    io:format("basic_fragmented_text_1~n", []),
    do_test_valid_fragmented_1("/websockets_example_endpoint.yaws",
                               ?WS_OPCODE_TEXT).

test_advanced_fragmented_text_1() ->
    io:format("advanced_fragmented_text_1~n", []),
    do_test_valid_fragmented_2("/websockets_autobahn_endpoint.yaws",
                               ?WS_OPCODE_TEXT).

test_basic_fragmented_binary_1() ->
    io:format("basic_fragmented_binary_1~n", []),
    do_test_valid_fragmented_1("/websockets_example_endpoint.yaws",
                               ?WS_OPCODE_BINARY).

test_advanced_fragmented_binary_1() ->
    io:format("advanced_fragmented_binary_1~n", []),
    do_test_valid_fragmented_1("/websockets_autobahn_endpoint.yaws",
                               ?WS_OPCODE_BINARY).


do_test_valid_fragmented_1(WSPath, Type) ->
    Key      = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload1 = <<"fragment1">>,
    Payload2 = <<"fragment2">>,
    Payload3 = <<"fragment3">>,
    Payload  = <<Payload1/binary, Payload2/binary, Payload3/binary>>,

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    SndFrame1 = #frame{fin=false, opcode=Type, payload=Payload1},
    SndFrame2 = #frame{fin=false, opcode=?WS_OPCODE_CONTINUATION, payload=Payload2},
    SndFrame3 = #frame{opcode=?WS_OPCODE_CONTINUATION, payload=Payload3},

    ?line ok = send_frame(Sock, SndFrame1, all),
    ?line ok = send_frame(Sock, SndFrame2, all),
    ?line ok = send_frame(Sock, SndFrame3, all),
    ?line ok = wsclose(Sock, ?WS_STATUS_NORMAL, <<>>),

    ?line {ok, [Frame1|Frames]} = wsflush(Sock, true),
    ?line Type    = Frame1#frame.opcode,
    ?line Payload = Frame1#frame.payload,
    ?line true    = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok      = close(Sock).


%% ----
test_basic_fragmented_text_2() ->
    io:format("basic_fragmented_text_2~n", []),
    do_test_valid_fragmented_2("/websockets_example_endpoint.yaws",
                               ?WS_OPCODE_TEXT).

test_advanced_fragmented_text_2() ->
    io:format("advanced_fragmented_text_2~n", []),
    do_test_valid_fragmented_2("/websockets_autobahn_endpoint.yaws",
                               ?WS_OPCODE_TEXT).

test_basic_fragmented_binary_2() ->
    io:format("basic_fragmented_binary_2~n", []),
    do_test_valid_fragmented_2("/websockets_example_endpoint.yaws",
                               ?WS_OPCODE_BINARY).

test_advanced_fragmented_binary_2() ->
    io:format("advanced_fragmented_binary_2~n", []),
    do_test_valid_fragmented_2("/websockets_autobahn_endpoint.yaws",
                               ?WS_OPCODE_BINARY).


do_test_valid_fragmented_2(WSPath, Type) ->
    Key     = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload = <<"fragment">>,

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    SndFrame1 = #frame{fin=false, opcode=Type},
    SndFrame2 = #frame{fin=false, opcode=?WS_OPCODE_CONTINUATION, payload=Payload},
    SndFrame3 = #frame{opcode=?WS_OPCODE_CONTINUATION},

    ?line ok = send_frame(Sock, SndFrame1, all),
    ?line ok = send_frame(Sock, SndFrame2, all),
    ?line ok = send_frame(Sock, SndFrame3, all),
    ?line ok = wsclose(Sock, ?WS_STATUS_NORMAL, <<>>),

    ?line {ok, [Frame1|Frames]} = wsflush(Sock, true),
    ?line Type    = Frame1#frame.opcode,
    ?line Payload = Frame1#frame.payload,
    ?line true    = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok      = close(Sock).


%% ----
test_basic_fragmented_ping() ->
    io:format("basic_fragmented_ping~n", []),
    do_test_invalid_fragmented("/websockets_example_endpoint.yaws",
                               ?WS_OPCODE_PING).

test_advanced_fragmented_ping() ->
    io:format("advanced_fragmented_ping~n", []),
    do_test_invalid_fragmented("/websockets_autobahn_endpoint.yaws",
                               ?WS_OPCODE_PING).

test_basic_fragmented_pong() ->
    io:format("basic_fragmented_pong~n", []),
    do_test_invalid_fragmented("/websockets_example_endpoint.yaws",
                               ?WS_OPCODE_PONG).

test_advanced_fragmented_pong() ->
    io:format("advanced_fragmented_pong~n", []),
    do_test_invalid_fragmented("/websockets_autobahn_endpoint.yaws",
                               ?WS_OPCODE_PONG).

test_basic_fragmented_close() ->
    io:format("basic_fragmented_close~n", []),
    do_test_invalid_fragmented("/websockets_example_endpoint.yaws",
                               ?WS_OPCODE_CLOSE).

test_advanced_fragmented_close() ->
    io:format("advanced_fragmented_ping~n", []),
    do_test_invalid_fragmented("/websockets_autobahn_endpoint.yaws",
                               ?WS_OPCODE_CLOSE).


do_test_invalid_fragmented(WSPath, Type) ->
    Key      = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload1 = <<"fragment1">>,
    Payload2 = <<"fragment2">>,

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    SndFrame1 = #frame{fin=false, opcode=Type, payload=Payload1},
    SndFrame2 = #frame{opcode=?WS_OPCODE_CONTINUATION, payload=Payload2},

    ?line ok = send_frame(Sock, SndFrame1, all),
    ?line ok = send_frame(Sock, SndFrame2, all),

    ?line {ok, Frames} = wsflush(Sock, false),
    ?line true     = is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR]),
    ?line {ok, []} = wsflush(Sock, true),
    ?line ok       = close(Sock).


%% ----
test_basic_fragmented_text_with_ping() ->
    io:format("basic_fragmented_text_with_ping~n", []),
    do_test_fragmented_with_ping("/websockets_example_endpoint.yaws").

test_advanced_fragmented_text_with_ping() ->
    io:format("advanced_fragmented_text_with_ping~n", []),
    do_test_fragmented_with_ping("/websockets_autobahn_endpoint.yaws").

do_test_fragmented_with_ping(WSPath) ->
    Key      = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload1 = <<"fragment1">>,
    Payload2 = <<"fragment2">>,
    Payload  = <<Payload1/binary, Payload2/binary>>,

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    SndFrame1 = #frame{fin=false, opcode=?WS_OPCODE_TEXT, payload=Payload1},
    SndFrame2 = #frame{opcode=?WS_OPCODE_PING},
    SndFrame3 = #frame{opcode=?WS_OPCODE_CONTINUATION, payload=Payload2},

    ?line ok = send_frame(Sock, SndFrame1, all),
    ?line ok = send_frame(Sock, SndFrame2, all),
    ?line ok = send_frame(Sock, SndFrame3, all),
    ?line ok = wsclose(Sock, ?WS_STATUS_NORMAL, <<>>),

    ?line {ok, [Frame1, Frame2|Frames]} = wsflush(Sock, true),
    ?line ?WS_OPCODE_PONG = Frame1#frame.opcode,
    ?line ?WS_OPCODE_TEXT = Frame2#frame.opcode,
    ?line Payload = Frame2#frame.payload,
    ?line true    = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok      = close(Sock).


%% ---
test_basic_fragmented_text_with_pong() ->
    io:format("basic_fragmented_text_with_pong~n", []),
    do_test_fragmented_with_pong("/websockets_example_endpoint.yaws").

test_advanced_fragmented_text_with_pong() ->
    io:format("advanced_fragmented_text_with_pong~n", []),
    do_test_fragmented_with_pong("/websockets_autobahn_endpoint.yaws").

do_test_fragmented_with_pong(WSPath) ->
    Key      = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload1 = <<"fragment1">>,
    Payload2 = <<"fragment2">>,
    Payload  = <<Payload1/binary, Payload2/binary>>,

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    SndFrame1 = #frame{fin=false, opcode=?WS_OPCODE_TEXT, payload=Payload1},
    SndFrame2 = #frame{opcode=?WS_OPCODE_PONG},
    SndFrame3 = #frame{opcode=?WS_OPCODE_CONTINUATION, payload=Payload2},

    ?line ok = send_frame(Sock, SndFrame1, all),
    ?line ok = send_frame(Sock, SndFrame2, all),
    ?line ok = send_frame(Sock, SndFrame3, all),
    ?line ok = wsclose(Sock, ?WS_STATUS_NORMAL, <<>>),

    ?line {ok, [Frame1|Frames]} = wsflush(Sock, true),
    ?line ?WS_OPCODE_TEXT = Frame1#frame.opcode,
    ?line Payload = Frame1#frame.payload,
    ?line true    = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok      = close(Sock).


%% ----
test_basic_badfragmented_1() ->
    io:format("basic_badfragmented_1 (fin=true)~n", []),
    do_test_badfragmented("/websockets_example_endpoint.yaws", true).

test_basic_badfragmented_2() ->
    io:format("basic_badfragmented_2 (fin=false)~n", []),
    do_test_badfragmented("/websockets_example_endpoint.yaws", false).

test_advanced_badfragmented_1() ->
    io:format("advanced_badfragmented_1 (fin=true)~n", []),
    do_test_badfragmented("/websockets_autobahn_endpoint.yaws", true).

test_advanced_badfragmented_2() ->
    io:format("advanced_badfragmented_2 (fin=false)~n", []),
    do_test_badfragmented("/websockets_autobahn_endpoint.yaws", false).

do_test_badfragmented(WSPath, Fin) ->
    Key     = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload = <<"small payload">>,

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    SndFrame1 = #frame{fin=Fin, opcode=?WS_OPCODE_CONTINUATION, payload=Payload},
    SndFrame2 = #frame{opcode=?WS_OPCODE_TEXT, payload=Payload},

    ?line ok = send_frame(Sock, SndFrame1, all),
    ?line ok = send_frame(Sock, SndFrame2, all),

    ?line {ok, Frames} = wsflush(Sock, false),
    ?line true     = is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR]),
    ?line {ok, []} = wsflush(Sock, true),
    ?line ok       = close(Sock).


%% ---
test_basic_badfragmented_nocontinuation() ->
    io:format("basic_badfragmented_nocontinuation~n", []),
    do_test_badfragmented_nocontinuation("/websockets_example_endpoint.yaws").

test_advanced_badfragmented_nocontinuation() ->
    io:format("advanced_badfragmented_nocontinuation~n", []),
    do_test_badfragmented_nocontinuation("/websockets_autobahn_endpoint.yaws").

do_test_badfragmented_nocontinuation(WSPath) ->
    Key      = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload1 = <<"fragment1">>,
    Payload2 = <<"fragment2">>,

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    SndFrame1 = #frame{fin=false, opcode=?WS_OPCODE_TEXT, payload=Payload1},
    SndFrame2 = #frame{fin=false, opcode=?WS_OPCODE_TEXT, payload=Payload2},

    ?line ok = send_frame(Sock, SndFrame1, all),
    ?line ok = send_frame(Sock, SndFrame2, all),

    ?line {ok, Frames} = wsflush(Sock, false),
    ?line true     = is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR]),
    ?line {ok, []} = wsflush(Sock, true),
    ?line ok       = close(Sock).


%% ----
test_basic_unfragmented_valid_utf8_text(BlockSz) ->
    io:format("basic_unfragmented_valid_utf8_text (blocksize=~p)~n", [BlockSz]),
    do_test_unfragmented_valid_utf8("/websockets_example_endpoint.yaws",
                                    BlockSz).

test_advanced_unfragmented_valid_utf8_text(BlockSz) ->
    io:format("advanced_unfragmented_valid_utf8_text (blocksize=~p)~n",
              [BlockSz]),
    do_test_unfragmented_valid_utf8("/websockets_autobahn_endpoint.yaws",
                                    BlockSz).

do_test_unfragmented_valid_utf8(WSPath, BlockSz) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    Fun = fun(Payload) ->
                  SndFrame = #frame{opcode=?WS_OPCODE_TEXT, payload=Payload},
                  ?line ok = send_frame(Sock, SndFrame, BlockSz),
                  ?line {ok, RcvFrame}  = read_frame(Sock),
                  ?line ?WS_OPCODE_TEXT  = RcvFrame#frame.opcode,
                  ?line Payload         = RcvFrame#frame.payload
          end,

    Fun(<<16#ce,16#ba>>),
    Fun(<<16#ce,16#ba,16#e1,16#bd,16#b9>>),
    Fun(<<16#ce,16#ba,16#e1,16#bd,16#b9,16#cf,16#83>>),
    Fun(<<16#ce,16#ba,16#e1,16#bd,16#b9,16#cf,16#83,16#ce,16#bc>>),
    Fun(<<16#ce,16#ba,16#e1,16#bd,16#b9,16#cf,16#83,16#ce,16#bc,
          16#ce,16#b5>>),
    Fun(<<"Hello-",16#c2,16#b5,$@,16#c3,16#9f,16#c3,16#b6,16#c3,16#a4,
          16#c3,16#bc,16#c3,16#a0,16#c3,16#a1,"-UTF-8!!">>),
    Fun(<<16#00>>),
    Fun(<<16#c2,16#80>>),
    Fun(<<16#e0,16#a0,16#80>>),
    Fun(<<16#f0,16#90,16#80,16#80>>),
    Fun(<<16#7f>>),
    Fun(<<16#df,16#bf>>),
    Fun(<<16#ef,16#bf,16#bf>>),
    Fun(<<16#f4,16#8f,16#bf,16#bf>>),
    Fun(<<16#ed,16#9f,16#bf>>),
    Fun(<<16#ee,16#80,16#80>>),
    Fun(<<16#ef,16#bf,16#bd>>),
    Fun(<<16#f4,16#8f,16#bf,16#bf>>),

    ?line ok = wsclose(Sock, ?WS_STATUS_NORMAL, <<>>),
    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_basic_fragmented_valid_utf8_text(FragSz) ->
    io:format("basic_fragmented_valid_utf8_text (fragsize=~p)~n", [FragSz]),
    do_test_fragmented_valid_utf8("/websockets_example_endpoint.yaws",
                                  FragSz).

test_advanced_fragmented_valid_utf8_text(FragSz) ->
    io:format("advanced_fragmented_valid_utf8_text (fragsize=~p)~n",
              [FragSz]),
    do_test_fragmented_valid_utf8("/websockets_autobahn_endpoint.yaws",
                                  FragSz).

do_test_fragmented_valid_utf8(WSPath, FragSz) ->
    Key      = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload1 = <<"Hello-",16#c2,16#b5,$@,16#c3,16#9f,16#c3,16#b6,16#c3,16#a4>>,
    Payload2 = <<16#c3,16#bc,16#c3,16#a0,16#c3,16#a1,"-UTF-8!!">>,
    Payload  = <<Payload1/binary, Payload2/binary>>,

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    Payloads = fragment_payload(Payload1, FragSz) ++
        fragment_payload(Payload2, FragSz),

    [First|Rest0] = Payloads,
    [Last|Rest1]  = lists:reverse(Rest0),
    Middles       = lists:reverse(Rest1),
    FirstFrame    = #frame{fin=false, opcode=?WS_OPCODE_TEXT, payload=First},
    LastFrame     = #frame{opcode=?WS_OPCODE_CONTINUATION, payload=Last},
    MiddleFrames  = lists:map(fun(P) ->
                                      #frame{fin=false,
                                             opcode=?WS_OPCODE_CONTINUATION,
                                             payload=P}
                              end, Middles),

    ?line ok = send_frame(Sock, FirstFrame, all),
    lists:foreach(fun(F) -> ?line ok = send_frame(Sock, F, all) end,
                  MiddleFrames),
    ?line ok = send_frame(Sock, LastFrame, all),
    ?line ok = wsclose(Sock, ?WS_STATUS_NORMAL, <<>>),

    ?line {ok, [Frame1|Frames]} = wsflush(Sock, true),
    ?line ?WS_OPCODE_TEXT = Frame1#frame.opcode,
    ?line Payload = Frame1#frame.payload,
    ?line true    = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok      = close(Sock),
    ok.



%% ----
test_basic_unfragmented_invalid_utf8_text(BlockSz) ->
    io:format("basic_unfragmented_invalid_utf8_text (blocksize=~p)~n",
              [BlockSz]),
    do_test_unfragmented_invalid_utf8("/websockets_example_endpoint.yaws",
                                      BlockSz).

test_advanced_unfragmented_invalid_utf8_text(BlockSz) ->
    io:format("advanced_unfragmented_invalid_utf8_text (blocksize=~p)~n",
              [BlockSz]),
    do_test_unfragmented_invalid_utf8("/websockets_autobahn_endpoint.yaws",
                                      BlockSz).

do_test_unfragmented_invalid_utf8(WSPath, BlockSz) ->
    Key     = "dGhlIHNhbXBsZSBub25jZQ==",

    Fun = fun(Payload) ->
                  ?line {ok, Sock}     = open("localhost", 8000),
                  ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

                  SndFrame = #frame{opcode=?WS_OPCODE_TEXT, payload=Payload},
                  ?line ok = send_frame(Sock, SndFrame, BlockSz),

                  ?line {ok, Frames} = wsflush(Sock, false),
                  ?line true     = is_valid_close_frame(Frames, [?WS_STATUS_INVALID_PAYLOAD]),
                  ?line {ok, []} = wsflush(Sock, true),
                  ?line ok       = close(Sock)
          end,

    Fun(<<16#cd>>),
    Fun(<<16#ce,16#ba,16#e1>>),
    Fun(<<16#ce,16#ba,16#e1,16#bd>>),
    Fun(<<16#ce,16#ba,16#e1,16#bd,16#b9,16#cf>>),
    Fun(<<16#ce,16#ba,16#e1,16#bd,16#b9,16#cf,16#83,16#ce>>),
    Fun(<<16#ce,16#ba,16#e1,16#bd,16#b9,16#cf,16#83,16#ce,16#bc,16#ce>>),
    Fun(<<16#ce,16#ba,16#e1,16#bd,16#b9,16#cf,16#83,16#ce,16#bc,16#ce,
          16#b5,16#ed,16#a0,16#80,16#65,16#64,16#69,16#74,16#65,16#64>>),
    Fun(<<16#f8,16#88,16#80,16#80,16#80>>),
    Fun(<<16#fc,16#84,16#80,16#80,16#80,16#80>>),
    Fun(<<16#f7,16#bf,16#bf,16#bf>>),
    Fun(<<16#fb,16#bf,16#bf,16#bf,16#bf>>),
    Fun(<<16#fd,16#bf,16#bf,16#bf,16#bf,16#bf>>),
    Fun(<<16#f4,16#90,16#80,16#80>>),
    Fun(<<16#80>>),
    Fun(<<16#bf>>),
    Fun(<<16#80,16#bf>>),
    Fun(<<16#80,16#bf,16#80>>),
    Fun(<<16#80,16#bf,16#80,16#bf>>),
    Fun(<<16#80,16#bf,16#80,16#bf,16#80>>),
    Fun(<<16#80,16#bf,16#80,16#bf,16#80,16#bf>>),
    Fun(<<16#80,16#81,16#82,16#83,16#84,16#85,16#86,16#87,16#88,16#89,16#8a,
          16#8b,16#8c,16#8d,16#8e,16#8f,16#90,16#91,16#92,16#93,16#94,16#95,
          16#96,16#97,16#98,16#99,16#9a,16#9b,16#9c,16#9d,16#9e,16#9f,16#a0,
          16#a1,16#a2,16#a3,16#a4,16#a5,16#a6,16#a7,16#a8,16#a9,16#aa,16#ab,
          16#ac,16#ad,16#ae,16#af,16#b0,16#b1,16#b2,16#b3,16#b4,16#b5,16#b6,
          16#b7,16#b8,16#b9,16#ba,16#bb,16#bc,16#bd,16#be>>),
    ok.


%% ----
test_basic_fragmented_invalid_utf8_text(FragSz) ->
    io:format("basic_fragmented_invalid_utf8_text (fragsize=~p)~n", [FragSz]),
    do_test_fragmented_invalid_utf8("/websockets_example_endpoint.yaws",
                                    FragSz).

test_advanced_fragmented_invalid_utf8_text(FragSz) ->
    io:format("advanced_fragmented_invalid_utf8_text (fragsize=~p)~n",
              [FragSz]),
    do_test_fragmented_invalid_utf8("/websockets_autobahn_endpoint.yaws",
                                    FragSz).

do_test_fragmented_invalid_utf8(WSPath, FragSz) ->
    Key      = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload1 = <<16#ce,16#ba,16#e1,16#bd,16#b9,16#cf,16#83,16#ce,16#bc,16#ce>>,
    Payload2 = <<16#b5,16#ed,16#a0,16#80,16#65,16#64,16#69,16#74,16#65,16#64>>,

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    Payloads = fragment_payload(Payload1, FragSz) ++
        fragment_payload(Payload2, FragSz),

    [First|Rest0] = Payloads,
    [Last|Rest1]  = lists:reverse(Rest0),
    Middles       = lists:reverse(Rest1),
    FirstFrame    = #frame{fin=false, opcode=?WS_OPCODE_TEXT, payload=First},
    LastFrame     = #frame{opcode=?WS_OPCODE_CONTINUATION, payload=Last},
    MiddleFrames  = lists:map(fun(P) ->
                                      #frame{fin=false,
                                             opcode=?WS_OPCODE_CONTINUATION,
                                             payload=P}
                              end, Middles),

    ?line ok = send_frame(Sock, FirstFrame, all),
    lists:foreach(fun(F) -> ?line ok = send_frame(Sock, F, all) end,
                  MiddleFrames),
    ?line ok = send_frame(Sock, LastFrame, all),

    ?line {ok, Frames} = wsflush(Sock, false),
    ?line true     = is_valid_close_frame(Frames, [?WS_STATUS_INVALID_PAYLOAD]),
    ?line {ok, []} = wsflush(Sock, true),
    ?line ok       = close(Sock),
    ok.


%% ----
test_basic_2_closes() ->
    io:format("basic_2_closes~n", []),
    do_test_2_closes("/websockets_example_endpoint.yaws").

test_advanced_2_closes() ->
    io:format("advanced_2_closes~n", []),
    do_test_2_closes("/websockets_autobahn_endpoint.yaws").

do_test_2_closes(WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    ?line ok = wsclose(Sock, ?WS_STATUS_NORMAL, <<>>),
    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_basic_close_ping() ->
    io:format("basic_close_ping~n", []),
    do_test_close_ping("/websockets_example_endpoint.yaws").

test_advanced_close_ping() ->
    io:format("advanced_close_ping~n", []),
    do_test_close_ping("/websockets_autobahn_endpoint.yaws").

do_test_close_ping(WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    ?line ok = wsclose(Sock, ?WS_STATUS_NORMAL, <<>>),
    ?line ok = send_frame(Sock, #frame{opcode=?WS_OPCODE_PING}, all),

    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_basic_close_text() ->
    io:format("basic_close_text~n", []),
    do_test_close_text("/websockets_example_endpoint.yaws").

test_advanced_close_text() ->
    io:format("advanced_close_text~n", []),
    do_test_close_text("/websockets_autobahn_endpoint.yaws").

do_test_close_text(WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    ?line ok = wsclose(Sock, ?WS_STATUS_NORMAL, <<>>),
    ?line ok = send_frame(Sock, #frame{opcode=?WS_OPCODE_TEXT}, all),

    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_basic_fragtext_close_fragtext() ->
    io:format("basic_fragtext_close_fragtext~n", []),
    do_test_fragtext_close_fragtext("/websockets_example_endpoint.yaws").

test_advanced_fragtext_close_fragtext() ->
    io:format("advanced_fragtext_close_fragtext~n", []),
    do_test_fragtext_close_fragtext("/websockets_autobahn_endpoint.yaws").

do_test_fragtext_close_fragtext(WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    ?line ok = send_frame(Sock, #frame{fin=false, opcode=?WS_OPCODE_TEXT}, all),
    ?line ok = wsclose(Sock, ?WS_STATUS_NORMAL, <<>>),
    ?line ok = send_frame(Sock, #frame{opcode=?WS_OPCODE_CONTINUATION}, all),

    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_basic_close_empty() ->
    io:format("basic_close_empty~n", []),
    do_test_close_empty("/websockets_example_endpoint.yaws").

test_advanced_close_empty() ->
    io:format("advanced_close_empty~n", []),
    do_test_close_empty("/websockets_autobahn_endpoint.yaws").

do_test_close_empty(WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    ?line ok = gen_tcp:send(Sock, <<136,0>>),

    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_basic_close_toosmall() ->
    io:format("basic_close_toosmall~n", []),
    do_test_close_toosmall("/websockets_example_endpoint.yaws").

test_advanced_close_toosmall() ->
    io:format("advanced_close_toosmall~n", []),
    do_test_close_toosmall("/websockets_autobahn_endpoint.yaws").

do_test_close_toosmall(WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    ?line ok = gen_tcp:send(Sock, <<136,1,0>>),

    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_basic_close_statusonly() ->
    io:format("basic_close_statusonly~n", []),
    do_test_close_statusonly("/websockets_example_endpoint.yaws").

test_advanced_close_statusonly() ->
    io:format("advanced_close_statusonly~n", []),
    do_test_close_statusonly("/websockets_autobahn_endpoint.yaws").

do_test_close_statusonly(WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    ?line ok = gen_tcp:send(Sock, <<136,2,1000:16/big>>),

    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_basic_close_with_reason() ->
    io:format("basic_close_with_reason~n", []),
    do_test_close_with_reason("/websockets_example_endpoint.yaws").

test_advanced_close_with_reason() ->
    io:format("advanced_close_with_reason~n", []),
    do_test_close_with_reason("/websockets_autobahn_endpoint.yaws").

do_test_close_with_reason(WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    ?line ok = gen_tcp:send(Sock, <<136,4,1000:16/big,"Ok">>),

    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_basic_close_limit_size() ->
    io:format("basic_close_limit_size~n", []),
    do_test_close_limit_size("/websockets_example_endpoint.yaws").

test_advanced_close_limit_size() ->
    io:format("advanced_close_limit_size~n", []),
    do_test_close_limit_size("/websockets_autobahn_endpoint.yaws").

do_test_close_limit_size(WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    Reason = list_to_binary(lists:duplicate(123, $*)),
    ?line ok = gen_tcp:send(Sock, <<136,125,1000:16/big,Reason/binary>>),

    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_basic_close_toolong() ->
    io:format("basic_close_toolong~n", []),
    do_test_close_toolong("/websockets_example_endpoint.yaws").

test_advanced_close_toolong() ->
    io:format("advanced_close_toolong~n", []),
    do_test_close_toolong("/websockets_autobahn_endpoint.yaws").

do_test_close_toolong(WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    Reason = list_to_binary(lists:duplicate(124, $*)),
    ?line ok = gen_tcp:send(Sock, <<136,126,1000:16/big,Reason/binary>>),

    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_basic_close_invalid_utf8() ->
    io:format("basic_close_invalid_utf8~n", []),
    do_test_close_invalid_utf8("/websockets_example_endpoint.yaws").

test_advanced_close_invalid_utf8() ->
    io:format("advanced_close_invalid_utf8~n", []),
    do_test_close_invalid_utf8("/websockets_autobahn_endpoint.yaws").

do_test_close_invalid_utf8(WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    Reason = <<16#ce,16#ba,16#e1,16#bd,16#b9,16#cf,16#83,16#ce,16#bc,16#ce,
               16#b5,16#ed,16#a0,16#80,16#65,16#64,16#69,16#74,16#65,16#64>>,
    ?line ok = gen_tcp:send(Sock, <<136,22,1000:16/big,Reason/binary>>),

    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_INVALID_PAYLOAD]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_basic_close_valid_codes() ->
    lists:foreach(
      fun(Code) ->
              io:format("basic_close_valid_codes (code=~p)~n", [Code]),
              do_test_close_valid_code("/websockets_example_endpoint.yaws",
                                       Code)
      end, [1000,1001,1002,1003,1007,1008,1009,1010,1011,3000,3999,4000,4999]
     ).

test_advanced_close_valid_codes() ->
    lists:foreach(
      fun(Code) ->
              io:format("advanced_close_valid_codes (code=~p)~n", [Code]),
              do_test_close_valid_code("/websockets_autobahn_endpoint.yaws",
                                       Code)
      end, [1000,1001,1002,1003,1007,1008,1009,1010,1011,3000,3999,4000,4999]
     ).

do_test_close_valid_code(WSPath, Code) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    ?line ok = wsclose(Sock, Code, <<>>),
    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [Code]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_basic_close_invalid_codes() ->
    lists:foreach(
      fun(Code) ->
              io:format("basic_close_invalid_codes (code=~p)~n", [Code]),
              do_test_close_invalid_code("/websockets_example_endpoint.yaws",
                                         Code)
      end,
      [0,999,1004,1005,1006,1012,1013,1014,1015,1016,1100,2000,2999,5000,65536]
     ).

test_advanced_close_invalid_codes() ->
    lists:foreach(
      fun(Code) ->
              io:format("advanced_close_invalid_codes (code=~p)~n", [Code]),
              do_test_close_invalid_code("/websockets_autobahn_endpoint.yaws",
                                       Code)
      end,
      [0,999,1004,1005,1006,1012,1013,1014,1015,1016,1100,2000,2999,5000,65536]
     ).

do_test_close_invalid_code(WSPath, Code) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    ?line ok = wsclose(Sock, Code, <<>>),
    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_close_timeout() ->
    io:format("close_timeout (tout=5sec)~n",[]),
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",
    WSPath = "/websockets_example_endpoint.yaws?extversion=true",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    SndFrame = #frame{opcode=?WS_OPCODE_TEXT, payload = <<"bye">>},
    ?line ok = send_frame(Sock, SndFrame, all),
    ?line {ok, Frames} = wsflush(Sock, false),
    LastFrame  = lists:last(Frames),
    ?line true = is_valid_close_frame([LastFrame], [?WS_STATUS_NORMAL]),
    timer:sleep(5500), %% Waiting for the timeout
    ?line {error, closed} = gen_tcp:recv(Sock, 0),
    ?line ok = close(Sock),
    ok.


%% ----
test_keepalive_timeout() ->
    io:format("keepalive_timeout (timeout=5sec, grace_period=2sec, drop_on_timeout=true)~n",[]),
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",
    WSPath = "/websockets_example_endpoint.yaws?keepalive=true&timeout=5000&drop=true",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    timer:sleep(5500),
    ?line {ok, RcvFrame1} = read_frame(Sock),
    ?line ?WS_OPCODE_PING = RcvFrame1#frame.opcode,
    ?line ok = send_frame(Sock, #frame{opcode=?WS_OPCODE_PONG}, all),

    timer:sleep(5500),
    ?line {ok, RcvFrame2} = read_frame(Sock),
    ?line ?WS_OPCODE_PING = RcvFrame2#frame.opcode,

    timer:sleep(2000),
    ?line {error, closed} = gen_tcp:recv(Sock, 0),
    ?line ok = close(Sock),
    ok.


%% ----
test_too_big_frame() ->
    io:format("too_big_frame (> 16 Mo)~n",[]),
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",
    WSPath = "/websockets_example_endpoint.yaws",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    Payload1 = crypto:rand_bytes(16*1024*1024),
    SndFrame1 = #frame{opcode=?WS_OPCODE_BINARY, payload=Payload1},
    ?line ok  = send_frame(Sock, SndFrame1, all),
    ?line {ok, RcvFrame} = read_frame(Sock),
    ?line ?WS_OPCODE_BINARY = RcvFrame#frame.opcode,
    ?line Payload1 = RcvFrame#frame.payload,

    Payload2 = <<0, Payload1/binary>>,
    SndFrame2 = #frame{opcode=?WS_OPCODE_BINARY, payload=Payload2},
    ?line ok   = send_frame(Sock, SndFrame2, all),
    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_MSG_TOO_BIG]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_close_unmasked_frame() ->
    io:format("close_unmasked_frame~n",[]),
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",
    WSPath = "/websockets_example_endpoint.yaws?close_unmasked=true",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    %% unmasked
    SndFrame   = #frame{opcode=?WS_OPCODE_TEXT, payload = <<"unmasked">>},
    ?line ok   = send_frame(Sock, SndFrame, all),
    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_too_big_message() ->
    io:format("too_big_message (> 16 Mo)~n",[]),
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",
    WSPath = "/websockets_example_endpoint.yaws",

    ?line {ok, Sock}     = open("localhost", 8000),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    Payload1 = crypto:rand_bytes(16*1024*1024),
    <<Frag1:(4*1024)/binary, Frag2:(4*1024)/binary,
      Frag3:(4*1024)/binary, Frag4/binary>> = Payload1,
    SndFrame1 = #frame{fin=false, opcode=?WS_OPCODE_BINARY, payload=Frag1},
    SndFrame2 = #frame{fin=false, opcode=?WS_OPCODE_CONTINUATION, payload=Frag2},
    SndFrame3 = #frame{fin=false, opcode=?WS_OPCODE_CONTINUATION, payload=Frag3},
    SndFrame4 = #frame{opcode=?WS_OPCODE_CONTINUATION, payload=Frag4},
    ?line ok  = send_frame(Sock, SndFrame1, all),
    ?line ok  = send_frame(Sock, SndFrame2, all),
    ?line ok  = send_frame(Sock, SndFrame3, all),
    ?line ok  = send_frame(Sock, SndFrame4, all),
    ?line {ok, RcvFrame} = read_frame(Sock),
    ?line ?WS_OPCODE_BINARY = RcvFrame#frame.opcode,
    ?line Payload1 = RcvFrame#frame.payload,

    Payload2 = <<0, Payload1/binary>>,
    <<Frag5:(4*1024)/binary, Frag6:(4*1024)/binary,
      Frag7:(4*1024)/binary, Frag8/binary>> = Payload2,
    SndFrame5 = #frame{fin=false, opcode=?WS_OPCODE_BINARY, payload=Frag5},
    SndFrame6 = #frame{fin=false, opcode=?WS_OPCODE_CONTINUATION, payload=Frag6},
    SndFrame7 = #frame{fin=false, opcode=?WS_OPCODE_CONTINUATION, payload=Frag7},
    SndFrame8 = #frame{opcode=?WS_OPCODE_CONTINUATION, payload=Frag8},
    ?line ok  = send_frame(Sock, SndFrame5, all),
    ?line ok  = send_frame(Sock, SndFrame6, all),
    ?line ok  = send_frame(Sock, SndFrame7, all),
    ?line ok  = send_frame(Sock, SndFrame8, all),
    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_MSG_TOO_BIG]),
    ?line ok   = close(Sock),
    ok.


%% ----
test_secure_websocket() ->
    io:format("secure_websocket~n",[]),
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",
    WSPath = "/websockets_example_endpoint.yaws",

    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),

    ?line {ok, Sock}     = sslopen("localhost", 8443),
    ?line {ok, {101, _}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    SndFrame = #frame{opcode=?WS_OPCODE_TEXT, payload = <<"small payload">>},
    ?line ok = send_frame(Sock, SndFrame, all),
    ?line {ok, RcvFrame}      = read_frame(Sock),
    ?line ?WS_OPCODE_TEXT     = RcvFrame#frame.opcode,
    ?line <<"small payload">> = RcvFrame#frame.payload,

    ?line ok = wsclose(Sock, ?WS_STATUS_NORMAL, <<>>),
    ?line {ok, Frames} = wsflush(Sock, true),
    ?line true = is_valid_close_frame(Frames, [?WS_STATUS_NORMAL]),
    ?line ok   = close(Sock),

    ok = application:stop(ssl),
    ok = application:stop(public_key),
    ok = application:stop(crypto),
    ok.


%% =======================================================================
open(Host, Port) ->
    Opts = [{send_timeout, 2000}, binary, {packet, raw}, {active, false}],
    gen_tcp:connect(Host, Port, Opts).

sslopen(Host, Port) ->
    Opts = [{send_timeout, 2000}, binary, {packet, raw}, {active, false}],
    case ssl:connect(Host, Port, Opts) of
        {ok, Sock}      -> {ok, {ssl, Sock}};
        {error, Reason} -> {error, Reason}
    end.

close(Sock) ->
    case yaws_api:get_sslsocket(Sock) of
        {ok, SslSock} -> ssl:close(SslSock);
        undefined     -> gen_tcp:close(Sock)
    end.



%% ----
wsopen(Sock, Key, Path, Origin, Vsn) ->
    Handshake = ["GET ", Path, " HTTP/1.1\r\n",
                 "Host: localhost\r\n",
                 "Upgrade: websocket\r\n",
                 "Connection: Upgrade\r\n",
                 "Sec-WebSocket-Key: ", Key, "\r\n",
                 "Origin: ", Origin, "\r\n",
                 "Sec-WebSocket-Version: ", integer_to_list(Vsn), "\r\n",
                 "\r\n"],
    case yaws_api:get_sslsocket(Sock) of
        {ok, SslSock} -> ssl:send(SslSock, Handshake);
        undefined     -> gen_tcp:send(Sock, Handshake)
    end,
    read_handshake_response(Sock).

wsclose(Sock, Status, Msg) ->
    Fin    = 1,
    Rsv    = 0,
    Mask   = 0,
    Opcode = ?WS_OPCODE_CLOSE,
    Payload= <<Status:16/big, Msg/binary>>,
    Len    = byte_size(Payload),
    Frame = if
                Len < 126 ->
                    <<Fin:1,Rsv:3,Opcode:4,Mask:1,Len:7,Payload/binary>>;
                Len < 65536  ->
                    <<Fin:1,Rsv:3,Opcode:4,Mask:1,126:7,Len:16,Payload/binary>>;
                true  ->
                    <<Fin:1,Rsv:3,Opcode:4,Mask:1,127:7,Len:64,Payload/binary>>
            end,
    case yaws_api:get_sslsocket(Sock) of
        {ok, SslSock} -> ssl:send(SslSock, Frame);
        undefined     -> gen_tcp:send(Sock, Frame)
    end.


%% ----
wsflush(Sock, WithTcpClose) ->
    wsflush(Sock, WithTcpClose, []).

wsflush(Sock, WithTcpClose, Acc) ->
    case read_frame(Sock) of
        {ok, Frame} ->
            case Frame#frame.opcode of
                ?WS_OPCODE_CLOSE when WithTcpClose == false ->
                    {ok, lists:reverse([Frame|Acc])};
                _ ->
                    wsflush(Sock, WithTcpClose, [Frame|Acc])
            end;
        {error, closed} ->
            {ok, lists:reverse(Acc)};
        {error, Reason} ->
            {error, Reason}
    end.


%% ----
is_valid_handshake_hash(Key, Hash) ->
    Salted = Key ++ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11",
    HashBin = crypto:sha(Salted),
    Hash == base64:encode_to_string(HashBin).


%% ----
is_valid_close_frame([], _) ->
    io:format("    WARNING: Connection closed by server without Close frame~n"),
    true;
is_valid_close_frame([#frame{opcode=?WS_OPCODE_CLOSE, payload=Payload}|Rest],
                     Codes) ->
    case Rest of
        [] ->
            case Payload of
                <<>> -> lists:member(?WS_STATUS_NORMAL, Codes);
                <<Status:16/big, _/binary>> ->
                    case lists:member(Status, Codes) of
                        true ->
                            true;
                        false ->
                            io:format("    ERROR: Bad status code in close"
                                      " frame: status=~p~n", [Status]),
                            false
                    end
            end;
        _  ->
            io:format("    ERROR: Remaining frames after the Close frame~n")
    end;
is_valid_close_frame([#frame{opcode=OpCode}|_], _) ->
    io:format("    ERROR: Not a close frame: opcode=~p~n", [OpCode]),
    false.


%% ----
apply_loop(0, _Fun, _Args, ExpectedRes) ->
    ExpectedRes;
apply_loop(N, Fun, Args, ExpectedRes) ->
    ?line ExpectedRes = apply(Fun, Args),
    apply_loop(N-1, Fun, Args, ExpectedRes).


%% ----
fragment_payload(Payload, all) ->
    [Payload];
fragment_payload(<<>>, _) ->
    [];
fragment_payload(Payload, FragSz) ->
    case Payload of
        <<Frag:FragSz/binary, Rest/binary>> ->
            [Frag | fragment_payload(Rest, FragSz)];
        Rest ->
            [Rest]
    end.


%% ----
read_handshake_response(Sock) ->
    Res = case yaws_api:get_sslsocket(Sock) of
              {ok, SslSock} ->
                  ssl:setopts(SslSock, [{packet, http}, {packet_size, 16#4000}]),
                  ssl:recv(SslSock, 0, 2000);
              undefined ->
                  inet:setopts(Sock, [{packet, http}, {packet_size, 16#4000}]),
                  gen_tcp:recv(Sock, 0, 2000)
          end,
    case Res of
        {ok, {http_response, _, Status, _}} ->
            case yaws_api:get_sslsocket(Sock) of
                {ok, SslSock1} ->
                    ssl:setopts(SslSock1,[{packet,httph},{packet_size,16#4000}]);
                undefined ->
                    inet:setopts(Sock, [{packet,httph},{packet_size,16#4000}])
            end,

            Resp = read_handshake_response(Sock, Status, []),
            case yaws_api:get_sslsocket(Sock) of
                {ok, SslSock2} -> ssl:setopts(SslSock2,[binary, {packet, raw}]);
                undefined     -> inet:setopts(Sock,  [binary, {packet, raw}])
            end,
            Resp;
        {ok, Error} ->
            {error, Error};
        {error, Reason} ->
            {error, Reason}
    end.

read_handshake_response(Sock, Status, Acc) ->
    Res = case yaws_api:get_sslsocket(Sock) of
              {ok, SslSock} -> ssl:recv(SslSock, 0, 2000);
              undefined     -> gen_tcp:recv(Sock, 0, 2000)
          end,
    case Res of
        {ok, {http_header, _, Name, _, Value}} when is_atom(Name) ->
            Name1 = string:to_lower(atom_to_list(Name)),
            read_handshake_response(Sock, Status, [{Name1, Value}|Acc]);
        {ok, {http_header, _, Name, _, Value}} ->
            Name1 = string:to_lower(Name),
            read_handshake_response(Sock, Status, [{Name1, Value}|Acc]);
        {ok, http_eoh} ->
            {ok, {Status, Acc}};
        {ok, Error} ->
            {error, Error};
        {error, Reason} ->
            {error, Reason}
    end.


%% ----
read_frame(Sock) ->
    case read_frame_header(Sock) of
        {ok, #frame{mask=undefined}=Frame} ->
            {ok, Frame};
        {ok, Frame} ->
            Payload = mask(Frame#frame.mask, Frame#frame.payload),
            {ok, Frame#frame{payload=Payload}};
        {error, Reason} ->
            {error, Reason}
    end.

read_frame_header(Sock) ->
    case do_recv(Sock, 2) of
        {ok, <<Fin:1, Rsv:3, Opcode:4, MaskBit:1, Len:7>>} ->
            Frame = #frame{fin    = bit_to_boolean(Fin),
                           rsv    = Rsv,
                           opcode = Opcode,
                           masked = bit_to_boolean(MaskBit)},
            case read_frame_length(Sock, Len) of
                {ok, Length}    -> read_frame_payload(Sock, Frame, Length);
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


read_frame_length(Sock, 126) ->
    case do_recv(Sock, 2) of
        {ok, <<Length:16>>} -> {ok, Length};
        {error, Reason}     -> {error, Reason}
    end;
read_frame_length(Sock, 127) ->
    case do_recv(Sock, 8) of
        {ok, <<Length:64>>} -> {ok, Length};
        {error, Reason}     -> {error, Reason}
    end;
read_frame_length(_Sock, Length) ->
    {ok, Length}.


read_frame_mask(Sock) ->
    case do_recv(Sock, 4) of
        {ok, Mask}      -> {ok, Mask};
        {error, Reason} -> {error, Reason}
    end.

read_frame_payload(Sock, #frame{masked=true, mask=undefined}=Frame, Length) ->
    case read_frame_mask(Sock) of
        {ok, Mask} ->
            read_frame_payload(Sock, Frame#frame{mask=Mask}, Length);
        {error, Reason} ->
            {error, Reason}
    end;
read_frame_payload(Sock, Frame, Length) ->
    case do_recv(Sock, Length) of
        {ok, Payload}   -> {ok, Frame#frame{payload=Payload}};
        {error, Reason} -> {error, Reason}
    end.


%% ----
send_frame(Sock, Frame, BlockSz) ->
    Fin     = boolean_to_bit(Frame#frame.fin),
    Rsv     = Frame#frame.rsv,
    Opcode  = Frame#frame.opcode,
    MaskBit = boolean_to_bit(Frame#frame.masked),
    Mask    = case Frame#frame.mask of
                  undefined -> <<>>;
                  M         -> M
              end,
    Data    = mask(Mask, Frame#frame.payload),
    Len     = byte_size(Data),
    Packet  = if
                  Len < 126 ->
                      <<Fin:1,Rsv:3,Opcode:4,MaskBit:1,Len:7,
                        Mask/binary,Data/binary>>;
                  Len < 65536 ->
                      <<Fin:1,Rsv:3,Opcode:4,MaskBit:1,126:7,Len:16,
                        Mask/binary,Data/binary>>;
                  true ->
                      <<Fin:1,Rsv:3,Opcode:4,MaskBit:1,127:7,Len:64,
                        Mask/binary,Data/binary>>
              end,
    case BlockSz of
        all ->
            case yaws_api:get_sslsocket(Sock) of
                {ok, SslSock} -> ssl:send(SslSock, Packet);
                undefined     -> gen_tcp:send(Sock, Packet)
            end;
        _ ->
            do_send(Sock, Packet, BlockSz)
    end.

do_send(_Sock, <<>>, _BlockSz) ->
    ok;
do_send(Sock, Packet, BlockSz) ->
    case Packet of
        <<Block:BlockSz/binary, Rest/binary>> ->
            case yaws_api:get_sslsocket(Sock) of
                {ok, SslSock} -> ssl:send(SslSock, Block);
                undefined     -> gen_tcp:send(Sock, Block)
            end,
            do_send(Sock, Rest, BlockSz);
        _ ->
            case yaws_api:get_sslsocket(Sock) of
                {ok, SslSock} -> ssl:send(SslSock, Packet);
                undefined     -> gen_tcp:send(Sock, Packet)
            end
    end.


%% ----
mask(MaskBin, Data) ->
    list_to_binary(rmask(MaskBin, Data)).

rmask(_,<<>>) ->
    [<<>>];
rmask(<<>>, Data) ->
    [Data];
rmask(MaskBin = <<Mask:4/integer-unit:8>>,
      <<Data:4/integer-unit:8, Rest/binary>>) ->
    Masked = Mask bxor Data,
    [<<Masked:4/integer-unit:8>> | rmask(MaskBin, Rest)];
rmask(<<Mask:3/integer-unit:8, _Rest/binary>>, <<Data:3/integer-unit:8>>) ->
    Masked = Mask bxor Data,
    [<<Masked:3/integer-unit:8>>];
rmask(<<Mask:2/integer-unit:8, _Rest/binary>>, <<Data:2/integer-unit:8>>) ->
    Masked = Mask bxor Data,
    [<<Masked:2/integer-unit:8>>];
rmask(<<Mask:1/integer-unit:8, _Rest/binary>>, <<Data:1/integer-unit:8>>) ->
    Masked = Mask bxor Data,
    [<<Masked:1/integer-unit:8>>].


%% ----
bit_to_boolean(1) -> true;
bit_to_boolean(0) -> false.


boolean_to_bit(true)  -> 1;
boolean_to_bit(false) -> 0.


%% ----
do_recv(Sock, Sz) ->
    do_recv(Sock, Sz, []).

do_recv(_Sock, 0, Acc) ->
    {ok, list_to_binary(lists:reverse(Acc))};
do_recv(Sock, Sz, Acc) ->
    Res = case yaws_api:get_sslsocket(Sock) of
              {ok, SslSock} -> ssl:recv(SslSock, Sz, 1000);
              undefined     -> gen_tcp:recv(Sock, Sz, 1000)
          end,
    case Res of
        {ok, Bin}       -> do_recv(Sock, Sz - byte_size(Bin), [Bin|Acc]);
        {error, Reason} -> {error, Reason}
    end.

