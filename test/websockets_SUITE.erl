-module(websockets_SUITE).

-include("testsuite.hrl").

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

all() ->
    [
     {group, websocket_tests},
     {group, secure_websocket_tests}
    ].

groups() ->
    [
     {websocket_tests, [],
      [valid_opening_handshake,
       bad_version_handshake,
       bad_origin_handshake,
       noconnection_handshake,
       bad_connection_handshake,
       noupgrade_handshake,
       bad_upgrade_handshake,
       basic_unfragmented_text,
       advanced_unfragmented_text,
       basic_unfragmented_binary,
       advanced_unfragmented_binary,
       basic_ping_text,
       advanced_ping_text,
       basic_ping_binary,
       advanced_ping_binary,
       toolong_payload_ping,
       basic_unsolicited_pong,
       advanced_unsolicited_pong,
       basic_unsolicited_pong_ping_pong,
       advanced_unsolicited_pong_ping_pong,
       basic_10_pings,
       advanced_10_pings,
       badrsv_text,
       badrsv_binary,
       badrsv_ping,
       badrsv_close,
       badrsv_complex,
       badopcodes,
       basic_fragmented_empty,
       basic_fragmented_text_1,
       basic_fragmented_binary_1,
       basic_fragmented_text_2,
       basic_fragmented_binary_2,
       basic_fragmented_ping,
       basic_fragmented_pong,
       basic_fragmented_close,
       basic_fragmented_text_with_ping,
       basic_fragmented_text_with_pong,
       basic_badfragmented_1,
       basic_badfragmented_2,
       basic_badfragmented_nocontinuation,
       advanced_fragmented_empty,
       advanced_fragmented_text_1,
       advanced_fragmented_binary_1,
       advanced_fragmented_text_2,
       advanced_fragmented_binary_2,
       advanced_fragmented_ping,
       advanced_fragmented_pong,
       advanced_fragmented_close,
       advanced_fragmented_text_with_ping,
       advanced_fragmented_text_with_pong,
       advanced_badfragmented_1,
       advanced_badfragmented_2,
       advanced_badfragmented_nocontinuation,
       basic_unfragmented_valid_utf8_text,
       basic_fragmented_valid_utf8_text,
       advanced_unfragmented_valid_utf8_text,
       advanced_fragmented_valid_utf8_text,
       basic_unfragmented_invalid_utf8_text,
       basic_fragmented_invalid_utf8_text,
       advanced_unfragmented_invalid_utf8_text,
       advanced_fragmented_invalid_utf8_text,
       basic_2_closes,
       basic_close_ping,
       basic_close_text,
       basic_fragtext_close_fragtext,
       basic_close_empty,
       basic_close_toosmall,
       basic_close_statusonly,
       basic_close_with_reason,
       basic_close_limit_size,
       basic_close_toolong,
       basic_close_invalid_utf8,
       basic_close_valid_codes,
       basic_close_invalid_codes,
       advanced_2_closes,
       advanced_close_ping,
       advanced_close_text,
       advanced_fragtext_close_fragtext,
       advanced_close_empty,
       advanced_close_toosmall,
       advanced_close_statusonly,
       advanced_close_with_reason,
       advanced_close_limit_size,
       advanced_close_toolong,
       advanced_close_invalid_utf8,
       advanced_close_valid_codes,
       advanced_close_invalid_codes,
       close_timeout,
       keepalive_timeout,
       too_big_frame,
       too_big_message,
       close_unmasked_frame]},

     {secure_websocket_tests, [], [secure_websocket]}
    ].

%%====================================================================
init_per_suite(Config) ->
    Id      = "testsuite-server",
    GConf   = [
               {logdir,            ?config(priv_dir, Config)},
               {trace,             false},
               {flags,             [{copy_error_log, true}]},
               {keepalive_timeout, 10000},
               {acceptor_pool_size, 32}
              ],

    ok = yaws:start_embedded(?wwwdir, [], GConf, Id),
    [{yaws_id, Id} | Config].

end_per_suite(_Config) ->
    ok.

init_per_group(websocket_tests, Config) ->
    SConfHTTP = [
                 {docroot,    ?wwwdir},
                 {port,       testsuite:get_yaws_port(1, Config)},
                 {listen,     {127,0,0,1}},
                 {flags,      [{access_log, true}]},
                 {servername, "localhost"}
                ],
    {ok, _} = testsuite:add_yaws_server(?wwwdir, SConfHTTP),
    Config;
init_per_group(secure_websocket_tests, Config) ->
    SConfHTTPS = [
                  {docroot,    ?wwwdir},
                  {port,       testsuite:get_yaws_port(1, Config)},
                  {listen,     {127,0,0,1}},
                  {flags,      [{access_log, true}]},
                  {servername, "localhost"},
                  {ssl, [
                         {keyfile,  ?sslkeyfile},
                         {certfile, ?sslcertfile},
                         {depth,    0}
                        ]}
                 ],
    {ok, _} = testsuite:add_yaws_server(?wwwdir, SConfHTTPS),
    Config.

end_per_group(_Group, _Config) ->
    testsuite:reset_yaws_servers(),
    ok.

init_per_testcase(_Test, Config) ->
    Config.

end_per_testcase(_Test, _Config) ->
    ok.

%%====================================================================
valid_opening_handshake(Config) ->
    WSPath = "/websockets_example_endpoint.yaws",
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",

    %% Send the handshake and retrieve the response
    {ok, Sock}       = open("localhost", testsuite:get_yaws_port(1, Config)),
    {ok, {101, Hds}} = wsopen(Sock, Key, WSPath, "http://localhost", 13),

    %% Check the server response
    ?assert(is_valid_handshake_hash(
              Key, proplists:get_value("sec-websocket-accept", Hds)
             )),
    ?assertEqual("websocket", string:to_lower(proplists:get_value("upgrade",    Hds))),
    ?assertEqual("upgrade",   string:to_lower(proplists:get_value("connection", Hds))),

    %% Close the webscoket and check the server reply
    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),
    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

bad_version_handshake(Config) ->
    WSPath = "/websockets_example_endpoint.yaws",
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",

    %% Send the handshake and retrieve the response
    {ok, Sock}       = open("localhost", testsuite:get_yaws_port(1, Config)),
    {ok, {400, Hds}} = wsopen(Sock, Key, WSPath, "http://localhost", 15),

    %% Check the server response
    ?assertEqual("13, 8", string:to_lower(
                            proplists:get_value("sec-websocket-version", Hds)
                           )),

    ?assertEqual(ok, close(Sock)),
    ok.

bad_origin_handshake(Config) ->
    WSPath = "/websockets_example_endpoint.yaws",
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",

    %% Send the handshake and retrieve the response
    {ok, Sock}     = open("localhost", testsuite:get_yaws_port(1, Config)),
    {ok, {403, _}} = wsopen(Sock, Key, WSPath, "http://otherhost", 13),

    ?assertEqual(ok, close(Sock)),
    ok.

noconnection_handshake(Config) ->
    WSPath = "/websockets_example_endpoint.yaws",
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",

    %% Send the handshake and retrieve the response
    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),

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
    ?assertMatch({ok, {400, _}}, read_handshake_response(Sock)),

    ?assertEqual(ok, close(Sock)),
    ok.

bad_connection_handshake(Config) ->
    WSPath = "/websockets_example_endpoint.yaws",
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",

    %% Send the handshake and retrieve the response
    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),

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
    ?assertMatch({ok, {400, _}}, read_handshake_response(Sock)),

    ?assertEqual(ok, close(Sock)),
    ok.

noupgrade_handshake(Config) ->
    WSPath = "/websockets_example_endpoint.yaws",
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",

    %% Send the handshake and retrieve the response
    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),

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
    ?assertMatch({ok, {400, _}}, read_handshake_response(Sock)),

    ?assertEqual(ok, close(Sock)),
    ok.

bad_upgrade_handshake(Config) ->
    WSPath = "/websockets_example_endpoint.yaws",
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",

    %% Send the handshake and retrieve the response
    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),

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
    ?assertMatch({ok, {400, _}}, read_handshake_response(Sock)),

    ?assertEqual(ok, close(Sock)),
    ok.

basic_unfragmented_text(Config) ->
    basic_unfragmented_text(Config, 0,     all),
    basic_unfragmented_text(Config, 125,   all),
    basic_unfragmented_text(Config, 126,   all),
    basic_unfragmented_text(Config, 127,   all),
    basic_unfragmented_text(Config, 128,   all),
    basic_unfragmented_text(Config, 65535, all),
    basic_unfragmented_text(Config, 65536, all),
    basic_unfragmented_text(Config, 65536, 997),
    ok.

advanced_unfragmented_text(Config) ->
    advanced_unfragmented_text(Config, 0,     all),
    advanced_unfragmented_text(Config, 125,   all),
    advanced_unfragmented_text(Config, 126,   all),
    advanced_unfragmented_text(Config, 127,   all),
    advanced_unfragmented_text(Config, 128,   all),
    advanced_unfragmented_text(Config, 65535, all),
    advanced_unfragmented_text(Config, 65536, all),
    advanced_unfragmented_text(Config, 65536, 997),
    ok.

basic_unfragmented_binary(Config) ->
    basic_unfragmented_binary(Config, 0,     all),
    basic_unfragmented_binary(Config, 125,   all),
    basic_unfragmented_binary(Config, 126,   all),
    basic_unfragmented_binary(Config, 127,   all),
    basic_unfragmented_binary(Config, 128,   all),
    basic_unfragmented_binary(Config, 65535, all),
    basic_unfragmented_binary(Config, 65536, all),
    basic_unfragmented_binary(Config, 65536, 997),
    ok.

advanced_unfragmented_binary(Config) ->
    advanced_unfragmented_binary(Config, 0,     all),
    advanced_unfragmented_binary(Config, 125,   all),
    advanced_unfragmented_binary(Config, 126,   all),
    advanced_unfragmented_binary(Config, 127,   all),
    advanced_unfragmented_binary(Config, 128,   all),
    advanced_unfragmented_binary(Config, 65535, all),
    advanced_unfragmented_binary(Config, 65536, all),
    advanced_unfragmented_binary(Config, 65536, 997),
    ok.

basic_unfragmented_text(Config, Sz, BlockSz) ->
    Payload = list_to_binary(lists:duplicate(Sz, $*)),
    unfragmented_msg(Config, "/websockets_example_endpoint.yaws",
                     ?WS_OPCODE_TEXT, Payload, BlockSz).

advanced_unfragmented_text(Config, Sz, BlockSz) ->
    Payload = list_to_binary(lists:duplicate(Sz, $*)),
    unfragmented_msg(Config, "/websockets_autobahn_endpoint.yaws",
                     ?WS_OPCODE_TEXT, Payload, BlockSz).

basic_unfragmented_binary(Config, Sz, BlockSz) ->
    Payload = list_to_binary(lists:duplicate(Sz, 16#fe)),
    unfragmented_msg(Config, "/websockets_example_endpoint.yaws",
                     ?WS_OPCODE_BINARY, Payload, BlockSz).

advanced_unfragmented_binary(Config, Sz, BlockSz) ->
    Payload = list_to_binary(lists:duplicate(Sz, 16#fe)),
    unfragmented_msg(Config, "/websockets_autobahn_endpoint.yaws",
                     ?WS_OPCODE_BINARY, Payload, BlockSz).

unfragmented_msg(Config, WSPath, Type, Payload, BlockSz) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    %% unmasked
    SndFrame1 = #frame{opcode=Type, payload=Payload},
    ?assertEqual(ok, send_frame(Sock, SndFrame1, BlockSz)),
    {ok, RcvFrame1} = read_frame(Sock),
    ?assertEqual(Type, RcvFrame1#frame.opcode),
    ?assertEqual(Payload, RcvFrame1#frame.payload),

    %% masked
    SndFrame2 = SndFrame1#frame{masked=true, mask = <<"abcd">>},
    ?assertEqual(ok, send_frame(Sock, SndFrame2, BlockSz)),
    {ok, RcvFrame2} = read_frame(Sock),
    ?assertEqual(Type, RcvFrame2#frame.opcode),
    ?assertEqual(Payload, RcvFrame2#frame.payload),

    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),
    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_ping_text(Config) ->
    basic_ping_text(Config, 0, all),
    basic_ping_text(Config, 125, all),
    ok.

basic_ping_binary(Config) ->
    basic_ping_binary(Config, 125, all),
    basic_ping_binary(Config, 125, 1),
    ok.

advanced_ping_text(Config) ->
    advanced_ping_text(Config, 0, all),
    advanced_ping_text(Config, 125, all),
    ok.

advanced_ping_binary(Config) ->
    advanced_ping_binary(Config, 125, all),
    advanced_ping_binary(Config, 125, 1),
    ok.

basic_ping_text(Config, Sz, BlockSz) ->
    Payload = list_to_binary(lists:duplicate(Sz, $*)),
    ping_msg(Config, "/websockets_example_endpoint.yaws", Payload, BlockSz).

advanced_ping_text(Config, Sz, BlockSz) ->
    Payload = list_to_binary(lists:duplicate(Sz, $*)),
    ping_msg(Config, "/websockets_autobahn_endpoint.yaws", Payload, BlockSz).

basic_ping_binary(Config, Sz, BlockSz) ->
    Payload = list_to_binary(lists:duplicate(Sz, 16#fe)),
    ping_msg(Config, "/websockets_example_endpoint.yaws", Payload, BlockSz).

advanced_ping_binary(Config, Sz, BlockSz) ->
    Payload = list_to_binary(lists:duplicate(Sz, 16#fe)),
    ping_msg(Config, "/websockets_autobahn_endpoint.yaws", Payload, BlockSz).

ping_msg(Config, WSPath, Payload, BlockSz) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    %% unmasked
    SndFrame1 = #frame{opcode=?WS_OPCODE_PING, payload=Payload},
    ?assertEqual(ok, send_frame(Sock, SndFrame1, BlockSz)),
    {ok, RcvFrame1} = read_frame(Sock),
    ?assertEqual(?WS_OPCODE_PONG, RcvFrame1#frame.opcode),
    ?assertEqual(Payload, RcvFrame1#frame.payload),

    %% masked
    SndFrame2 = SndFrame1#frame{masked=true, mask = <<"abcd">>},
    ?assertEqual(ok, send_frame(Sock, SndFrame2, BlockSz)),
    {ok, RcvFrame2} = read_frame(Sock),
    ?assertEqual(?WS_OPCODE_PONG, RcvFrame2#frame.opcode),
    ?assertEqual(Payload, RcvFrame2#frame.payload),

    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),
    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

toolong_payload_ping(Config) ->
    WSPath  = "/websockets_example_endpoint.yaws",
    Key     = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload = list_to_binary(lists:duplicate(126, 16#fe)),

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    SndFrame = #frame{opcode=?WS_OPCODE_PING, payload=Payload},
    ?assertEqual(ok, send_frame(Sock, SndFrame, all)),

    {ok, Frames} = wsflush(Sock, false),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR])),
    ?assertEqual({ok, []}, wsflush(Sock, true)),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_unsolicited_pong(Config) ->
    basic_unsolicited_pong(Config, 0),
    basic_unsolicited_pong(Config, 125),
    ok.

advanced_unsolicited_pong(Config) ->
    advanced_unsolicited_pong(Config, 0),
    advanced_unsolicited_pong(Config, 125),
    ok.

basic_unsolicited_pong(Config, Sz) ->
    Payload = list_to_binary(lists:duplicate(Sz, 16#fe)),
    unsolicited_pong_msg(Config, "/websockets_example_endpoint.yaws", Payload).

advanced_unsolicited_pong(Config, Sz) ->
    Payload = list_to_binary(lists:duplicate(Sz, 16#fe)),
    unsolicited_pong_msg(Config, "/websockets_autobahn_endpoint.yaws", Payload).

unsolicited_pong_msg(Config, WSPath, Payload) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    SndFrame = #frame{opcode=?WS_OPCODE_PONG, payload=Payload},
    ?assertEqual(ok, send_frame(Sock, SndFrame, all)),

    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),
    {ok, Frames}  = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_unsolicited_pong_ping_pong(Config) ->
    unsolicited_pong_ping_pong(Config, "/websockets_example_endpoint.yaws").

advanced_unsolicited_pong_ping_pong(Config) ->
    unsolicited_pong_ping_pong(Config, "/websockets_autobahn_endpoint.yaws").

unsolicited_pong_ping_pong(Config, WSPath) ->
    Key      = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload1 = list_to_binary(lists:duplicate(125, $*)),
    Payload2 = <<"ping payload">>,

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    SndFrame1 = #frame{opcode=?WS_OPCODE_PONG, payload=Payload1},
    ?assertEqual(ok, send_frame(Sock, SndFrame1, all)),

    SndFrame2 = #frame{opcode=?WS_OPCODE_PING, payload=Payload2},
    ?assertEqual(ok, send_frame(Sock, SndFrame2, all)),
    {ok, RcvFrame2} = read_frame(Sock),
    ?assertEqual(?WS_OPCODE_PONG, RcvFrame2#frame.opcode),
    ?assertEqual(Payload2, RcvFrame2#frame.payload),

    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),
    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_10_pings(Config) ->
    basic_10_pings(Config, all),
    basic_10_pings(Config, 1),
    ok.

advanced_10_pings(Config) ->
    advanced_10_pings(Config, all),
    advanced_10_pings(Config, 1),
    ok.

basic_10_pings(Config, BlockSz) ->
    send_10_pings(Config, "/websockets_example_endpoint.yaws", BlockSz).

advanced_10_pings(Config, BlockSz) ->
    send_10_pings(Config, "/websockets_autobahn_endpoint.yaws", BlockSz).

send_10_pings(Config, WSPath, BlockSz) ->
    Key     = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload = <<"ping payload">>,

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    SndFrame = #frame{opcode=?WS_OPCODE_PING, payload=Payload},
    [begin
         ?assertEqual(ok, send_frame(Sock, SndFrame, BlockSz))
     end || _ <- lists:seq(1, 10)],

    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),
    {ok, Frames}  = wsflush(Sock, true),
    {Frames1, Frames2}  = lists:split(10, Frames),
    ?assert(lists:all(fun(#frame{payload=P}) -> P == Payload end, Frames1)),
    ?assert(is_valid_close_frame(Frames2, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

badrsv_text(Config) ->
    badrsv(Config, "/websockets_example_endpoint.yaws", ?WS_OPCODE_TEXT, 1).

badrsv_binary(Config) ->
    badrsv(Config, "/websockets_example_endpoint.yaws", ?WS_OPCODE_BINARY, 2).

badrsv_ping(Config) ->
    badrsv(Config, "/websockets_example_endpoint.yaws", ?WS_OPCODE_PING, 3).

badrsv_close(Config) ->
    badrsv(Config, "/websockets_example_endpoint.yaws", ?WS_OPCODE_CLOSE, 4).

badrsv(Config, WSPath, Type, Rsv) ->
    Key     = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload = <<"small payload">>,

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    SndFrame = #frame{rsv=Rsv, opcode=Type, payload=Payload},
    ?assertEqual(ok, send_frame(Sock, SndFrame, all)),

    {ok, Frames} = wsflush(Sock, false),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR])),
    ?assertEqual({ok, []}, wsflush(Sock, true)),
    ?assertEqual(ok, close(Sock)),
    ok.

badrsv_complex(Config) ->
    badrsv_complex(Config, all),
    badrsv_complex(Config, 1),
    ok.

badrsv_complex(Config, BlockSz) ->
    WSPath  = "/websockets_example_endpoint.yaws",
    Key     = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload = <<"small payload">>,

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    SndFrame1 = #frame{opcode=?WS_OPCODE_TEXT, payload=Payload},
    SndFrame2 = SndFrame1#frame{rsv=5},
    SndFrame3 = #frame{opcode=?WS_OPCODE_PING, payload=Payload},

    ?assertEqual(ok, send_frame(Sock, SndFrame1, BlockSz)),
    ?assertEqual(ok, send_frame(Sock, SndFrame2, BlockSz)),
    ?assertEqual(ok, send_frame(Sock, SndFrame3, BlockSz)),

    {ok, [Frame1|Frames]} = wsflush(Sock, false),
    ?assertEqual(?WS_OPCODE_TEXT, Frame1#frame.opcode),
    ?assertEqual(Payload, Frame1#frame.payload),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR])),
    ?assertEqual({ok, []}, wsflush(Sock, true)),
    ?assertEqual(ok, close(Sock)),
    ok.

badopcodes(Config) ->
    [badopcodes(Config, O) || O <- [3,4,5,6,7,11,12,13,14,15]],
    ok.

badopcodes(Config, Opcode) ->
    WSPath  = "/websockets_example_endpoint.yaws",
    Key     = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    SndFrame = #frame{opcode=Opcode},
    ?assertEqual(ok, send_frame(Sock, SndFrame, all)),

    {ok, Frames} = wsflush(Sock, false),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR])),
    ?assertEqual({ok, []}, wsflush(Sock, true)),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_fragmented_empty(Config) ->
    fragmented_empty(Config, "/websockets_example_endpoint.yaws").

advanced_fragmented_empty(Config) ->
    fragmented_empty(Config, "/websockets_autobahn_endpoint.yaws").

fragmented_empty(Config, WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    SndFrame1 = #frame{fin=false, opcode=?WS_OPCODE_TEXT},
    SndFrame2 = #frame{fin=false, opcode=?WS_OPCODE_CONTINUATION},
    SndFrame3 = #frame{opcode=?WS_OPCODE_CONTINUATION},

    ?assertEqual(ok, send_frame(Sock, SndFrame1, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame2, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame3, all)),
    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),

    {ok, [Frame1|Frames]} = wsflush(Sock, true),
    ?assertEqual(?WS_OPCODE_TEXT, Frame1#frame.opcode),
    ?assertEqual(<<>>, Frame1#frame.payload),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_fragmented_text_1(Config) ->
    valid_fragmented_1(Config, "/websockets_example_endpoint.yaws",
                       ?WS_OPCODE_TEXT).

advanced_fragmented_text_1(Config) ->
    valid_fragmented_1(Config, "/websockets_autobahn_endpoint.yaws",
                       ?WS_OPCODE_TEXT).

basic_fragmented_binary_1(Config) ->
    valid_fragmented_1(Config, "/websockets_example_endpoint.yaws",
                       ?WS_OPCODE_BINARY).

advanced_fragmented_binary_1(Config) ->
    valid_fragmented_1(Config, "/websockets_autobahn_endpoint.yaws",
                       ?WS_OPCODE_BINARY).

valid_fragmented_1(Config, WSPath, Type) ->
    Key      = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload1 = <<"fragment1">>,
    Payload2 = <<"fragment2">>,
    Payload3 = <<"fragment3">>,
    Payload  = <<Payload1/binary, Payload2/binary, Payload3/binary>>,

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    SndFrame1 = #frame{fin=false, opcode=Type, payload=Payload1},
    SndFrame2 = #frame{fin=false, opcode=?WS_OPCODE_CONTINUATION, payload=Payload2},
    SndFrame3 = #frame{opcode=?WS_OPCODE_CONTINUATION, payload=Payload3},

    ?assertEqual(ok, send_frame(Sock, SndFrame1, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame2, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame3, all)),
    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),

    {ok, [Frame1|Frames]} = wsflush(Sock, true),
    ?assertEqual(Type, Frame1#frame.opcode),
    ?assertEqual(Payload, Frame1#frame.payload),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_fragmented_text_2(Config) ->
    valid_fragmented_2(Config, "/websockets_example_endpoint.yaws",
                       ?WS_OPCODE_TEXT).

advanced_fragmented_text_2(Config) ->
    valid_fragmented_2(Config, "/websockets_autobahn_endpoint.yaws",
                       ?WS_OPCODE_TEXT).

basic_fragmented_binary_2(Config) ->
    valid_fragmented_2(Config, "/websockets_example_endpoint.yaws",
                       ?WS_OPCODE_BINARY).

advanced_fragmented_binary_2(Config) ->
    valid_fragmented_2(Config, "/websockets_autobahn_endpoint.yaws",
                       ?WS_OPCODE_BINARY).


valid_fragmented_2(Config, WSPath, Type) ->
    Key     = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload = <<"fragment">>,

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    SndFrame1 = #frame{fin=false, opcode=Type},
    SndFrame2 = #frame{fin=false, opcode=?WS_OPCODE_CONTINUATION, payload=Payload},
    SndFrame3 = #frame{opcode=?WS_OPCODE_CONTINUATION},

    ?assertEqual(ok, send_frame(Sock, SndFrame1, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame2, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame3, all)),
    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),

    {ok, [Frame1|Frames]} = wsflush(Sock, true),
    ?assertEqual(Type, Frame1#frame.opcode),
    ?assertEqual(Payload, Frame1#frame.payload),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_fragmented_ping(Config) ->
    invalid_fragmented(Config, "/websockets_example_endpoint.yaws",
                               ?WS_OPCODE_PING).

advanced_fragmented_ping(Config) ->
    invalid_fragmented(Config, "/websockets_autobahn_endpoint.yaws",
                       ?WS_OPCODE_PING).

basic_fragmented_pong(Config) ->
    invalid_fragmented(Config, "/websockets_example_endpoint.yaws",
                       ?WS_OPCODE_PONG).

advanced_fragmented_pong(Config) ->
    invalid_fragmented(Config, "/websockets_autobahn_endpoint.yaws",
                       ?WS_OPCODE_PONG).

basic_fragmented_close(Config) ->
    invalid_fragmented(Config, "/websockets_example_endpoint.yaws",
                       ?WS_OPCODE_CLOSE).

advanced_fragmented_close(Config) ->
    invalid_fragmented(Config, "/websockets_autobahn_endpoint.yaws",
                       ?WS_OPCODE_CLOSE).

invalid_fragmented(Config, WSPath, Type) ->
    Key      = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload1 = <<"fragment1">>,
    Payload2 = <<"fragment2">>,

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    SndFrame1 = #frame{fin=false, opcode=Type, payload=Payload1},
    SndFrame2 = #frame{opcode=?WS_OPCODE_CONTINUATION, payload=Payload2},

    ?assertEqual(ok, send_frame(Sock, SndFrame1, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame2, all)),

    {ok, Frames} = wsflush(Sock, false),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR])),
    ?assertEqual({ok, []}, wsflush(Sock, true)),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_fragmented_text_with_ping(Config) ->
    fragmented_with_ping(Config, "/websockets_example_endpoint.yaws").

advanced_fragmented_text_with_ping(Config) ->
    fragmented_with_ping(Config, "/websockets_autobahn_endpoint.yaws").

fragmented_with_ping(Config, WSPath) ->
    Key      = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload1 = <<"fragment1">>,
    Payload2 = <<"fragment2">>,
    Payload  = <<Payload1/binary, Payload2/binary>>,

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    SndFrame1 = #frame{fin=false, opcode=?WS_OPCODE_TEXT, payload=Payload1},
    SndFrame2 = #frame{opcode=?WS_OPCODE_PING},
    SndFrame3 = #frame{opcode=?WS_OPCODE_CONTINUATION, payload=Payload2},

    ?assertEqual(ok, send_frame(Sock, SndFrame1, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame2, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame3, all)),
    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),

    {ok, [Frame1, Frame2|Frames]} = wsflush(Sock, true),
    ?assertEqual(?WS_OPCODE_PONG, Frame1#frame.opcode),
    ?assertEqual(?WS_OPCODE_TEXT, Frame2#frame.opcode),
    ?assertEqual(Payload, Frame2#frame.payload),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_fragmented_text_with_pong(Config) ->
    fragmented_with_pong(Config, "/websockets_example_endpoint.yaws").

advanced_fragmented_text_with_pong(Config) ->
    fragmented_with_pong(Config, "/websockets_autobahn_endpoint.yaws").

fragmented_with_pong(Config, WSPath) ->
    Key      = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload1 = <<"fragment1">>,
    Payload2 = <<"fragment2">>,
    Payload  = <<Payload1/binary, Payload2/binary>>,

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    SndFrame1 = #frame{fin=false, opcode=?WS_OPCODE_TEXT, payload=Payload1},
    SndFrame2 = #frame{opcode=?WS_OPCODE_PONG},
    SndFrame3 = #frame{opcode=?WS_OPCODE_CONTINUATION, payload=Payload2},

    ?assertEqual(ok, send_frame(Sock, SndFrame1, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame2, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame3, all)),
    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),

    {ok, [Frame1|Frames]} = wsflush(Sock, true),
    ?assertEqual(?WS_OPCODE_TEXT, Frame1#frame.opcode),
    ?assertEqual(Payload, Frame1#frame.payload),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_badfragmented_1(Config) ->
    badfragmented(Config, "/websockets_example_endpoint.yaws", true).

basic_badfragmented_2(Config) ->
    badfragmented(Config, "/websockets_example_endpoint.yaws", false).

advanced_badfragmented_1(Config) ->
    badfragmented(Config, "/websockets_autobahn_endpoint.yaws", true).

advanced_badfragmented_2(Config) ->
    badfragmented(Config, "/websockets_autobahn_endpoint.yaws", false).

badfragmented(Config, WSPath, Fin) ->
    Key     = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload = <<"small payload">>,

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    SndFrame1 = #frame{fin=Fin, opcode=?WS_OPCODE_CONTINUATION, payload=Payload},
    SndFrame2 = #frame{opcode=?WS_OPCODE_TEXT, payload=Payload},

    ?assertEqual(ok, send_frame(Sock, SndFrame1, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame2, all)),

    {ok, Frames} = wsflush(Sock, false),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR])),
    ?assertEqual({ok, []}, wsflush(Sock, true)),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_badfragmented_nocontinuation(Config) ->
    badfragmented_nocontinuation(Config, "/websockets_example_endpoint.yaws").

advanced_badfragmented_nocontinuation(Config) ->
    badfragmented_nocontinuation(Config, "/websockets_autobahn_endpoint.yaws").

badfragmented_nocontinuation(Config, WSPath) ->
    Key      = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload1 = <<"fragment1">>,
    Payload2 = <<"fragment2">>,

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    SndFrame1 = #frame{fin=false, opcode=?WS_OPCODE_TEXT, payload=Payload1},
    SndFrame2 = #frame{fin=false, opcode=?WS_OPCODE_TEXT, payload=Payload2},

    ?assertEqual(ok, send_frame(Sock, SndFrame1, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame2, all)),

    {ok, Frames} = wsflush(Sock, false),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR])),
    ?assertEqual({ok, []}, wsflush(Sock, true)),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_unfragmented_valid_utf8_text(Config) ->
    unfragmented_valid_utf8(Config, "/websockets_example_endpoint.yaws", all),
    unfragmented_valid_utf8(Config, "/websockets_example_endpoint.yaws", 1),
    ok.

advanced_unfragmented_valid_utf8_text(Config) ->
    unfragmented_valid_utf8(Config, "/websockets_autobahn_endpoint.yaws", all),
    unfragmented_valid_utf8(Config, "/websockets_autobahn_endpoint.yaws", 1),
    ok.

unfragmented_valid_utf8(Config, WSPath, BlockSz) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    Fun = fun(Payload) ->
                  SndFrame = #frame{opcode=?WS_OPCODE_TEXT, payload=Payload},
                  ?assertEqual(ok, send_frame(Sock, SndFrame, BlockSz)),
                  {ok, RcvFrame}  = read_frame(Sock),
                  ?assertEqual(?WS_OPCODE_TEXT, RcvFrame#frame.opcode),
                  ?assertEqual(Payload, RcvFrame#frame.payload)
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

    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),
    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_fragmented_valid_utf8_text(Config) ->
    fragmented_valid_utf8(Config, "/websockets_example_endpoint.yaws", all),
    fragmented_valid_utf8(Config, "/websockets_example_endpoint.yaws", 1),
    ok.

advanced_fragmented_valid_utf8_text(Config) ->
    fragmented_valid_utf8(Config, "/websockets_autobahn_endpoint.yaws", all),
    fragmented_valid_utf8(Config, "/websockets_autobahn_endpoint.yaws", 1),
    ok.

fragmented_valid_utf8(Config, WSPath, FragSz) ->
    Key      = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload1 = <<"Hello-",16#c2,16#b5,$@,16#c3,16#9f,16#c3,16#b6,16#c3,16#a4>>,
    Payload2 = <<16#c3,16#bc,16#c3,16#a0,16#c3,16#a1,"-UTF-8!!">>,
    Payload  = <<Payload1/binary, Payload2/binary>>,

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

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

    ?assertEqual(ok, send_frame(Sock, FirstFrame, all)),
    lists:foreach(fun(F) ->
                          ?assertEqual(ok, send_frame(Sock, F, all))
                  end, MiddleFrames),
    ?assertEqual(ok, send_frame(Sock, LastFrame, all)),
    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),

    {ok, [Frame1|Frames]} = wsflush(Sock, true),
    ?assertEqual(?WS_OPCODE_TEXT, Frame1#frame.opcode),
    ?assertEqual(Payload, Frame1#frame.payload),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_unfragmented_invalid_utf8_text(Config) ->
    unfragmented_invalid_utf8(Config, "/websockets_example_endpoint.yaws", all),
    unfragmented_invalid_utf8(Config, "/websockets_example_endpoint.yaws", 1),
    ok.

advanced_unfragmented_invalid_utf8_text(Config) ->
    unfragmented_invalid_utf8(Config, "/websockets_autobahn_endpoint.yaws", all),
    unfragmented_invalid_utf8(Config, "/websockets_autobahn_endpoint.yaws", 1),
    ok.

unfragmented_invalid_utf8(Config, WSPath, BlockSz) ->
    Key     = "dGhlIHNhbXBsZSBub25jZQ==",

    Fun = fun(Payload) ->
                  {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
                  ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

                  SndFrame = #frame{opcode=?WS_OPCODE_TEXT, payload=Payload},
                  ?assertEqual(ok, send_frame(Sock, SndFrame, BlockSz)),

                  {ok, Frames} = wsflush(Sock, false),
                  ?assert(is_valid_close_frame(Frames, [?WS_STATUS_INVALID_PAYLOAD])),
                  ?assertEqual({ok, []}, wsflush(Sock, true)),
                  ?assertEqual(ok, close(Sock))
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

basic_fragmented_invalid_utf8_text(Config) ->
    fragmented_invalid_utf8(Config, "/websockets_example_endpoint.yaws", all),
    fragmented_invalid_utf8(Config, "/websockets_example_endpoint.yaws", 1),
    ok.

advanced_fragmented_invalid_utf8_text(Config) ->
    fragmented_invalid_utf8(Config, "/websockets_autobahn_endpoint.yaws", all),
    fragmented_invalid_utf8(Config, "/websockets_autobahn_endpoint.yaws", 1),
    ok.

fragmented_invalid_utf8(Config, WSPath, FragSz) ->
    Key      = "dGhlIHNhbXBsZSBub25jZQ==",
    Payload1 = <<16#ce,16#ba,16#e1,16#bd,16#b9,16#cf,16#83,16#ce,16#bc,16#ce>>,
    Payload2 = <<16#b5,16#ed,16#a0,16#80,16#65,16#64,16#69,16#74,16#65,16#64>>,

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

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

    ?assertEqual(ok, send_frame(Sock, FirstFrame, all)),
    lists:foreach(fun(F) ->
                          ?assertEqual(ok, send_frame(Sock, F, all))
                  end, MiddleFrames),
    ?assertEqual(ok, send_frame(Sock, LastFrame, all)),

    {ok, Frames} = wsflush(Sock, false),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_INVALID_PAYLOAD])),
    ?assertEqual({ok, []}, wsflush(Sock, true)),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_2_closes(Config) ->
    send_2_closes(Config, "/websockets_example_endpoint.yaws").

advanced_2_closes(Config) ->
    send_2_closes(Config, "/websockets_autobahn_endpoint.yaws").

send_2_closes(Config, WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),
    {ok, Frames} = wsflush(Sock, false),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),
    ?assertEqual({ok, []}, wsflush(Sock, true)),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_close_ping(Config) ->
    close_ping(Config, "/websockets_example_endpoint.yaws").

advanced_close_ping(Config) ->
    close_ping(Config, "/websockets_autobahn_endpoint.yaws").

close_ping(Config, WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),
    ?assertEqual(ok, send_frame(Sock, #frame{opcode=?WS_OPCODE_PING}, all)),

    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_close_text(Config) ->
    close_text(Config, "/websockets_example_endpoint.yaws").

advanced_close_text(Config) ->
    close_text(Config, "/websockets_autobahn_endpoint.yaws").

close_text(Config, WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),
    ?assertEqual(ok, send_frame(Sock, #frame{opcode=?WS_OPCODE_TEXT}, all)),

    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_fragtext_close_fragtext(Config) ->
    close_fragtext(Config, "/websockets_example_endpoint.yaws").

advanced_fragtext_close_fragtext(Config) ->
    close_fragtext(Config, "/websockets_autobahn_endpoint.yaws").

close_fragtext(Config, WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    ?assertEqual(ok, send_frame(Sock, #frame{fin=false, opcode=?WS_OPCODE_TEXT}, all)),
    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),
    ?assertEqual(ok, send_frame(Sock, #frame{opcode=?WS_OPCODE_CONTINUATION}, all)),

    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_close_empty(Config) ->
    close_empty(Config, "/websockets_example_endpoint.yaws").

advanced_close_empty(Config) ->
    close_empty(Config, "/websockets_autobahn_endpoint.yaws").

close_empty(Config, WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    ?assertEqual(ok, gen_tcp:send(Sock, <<136,0>>)),

    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_close_toosmall(Config) ->
    close_toosmall(Config, "/websockets_example_endpoint.yaws").

advanced_close_toosmall(Config) ->
    close_toosmall(Config, "/websockets_autobahn_endpoint.yaws").

close_toosmall(Config, WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    ?assertEqual(ok, gen_tcp:send(Sock, <<136,1,0>>)),

    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_close_statusonly(Config) ->
    close_statusonly(Config, "/websockets_example_endpoint.yaws").

advanced_close_statusonly(Config) ->
    close_statusonly(Config, "/websockets_autobahn_endpoint.yaws").

close_statusonly(Config, WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    ?assertEqual(ok, gen_tcp:send(Sock, <<136,2,1000:16/big>>)),

    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_close_with_reason(Config) ->
    close_with_reason(Config, "/websockets_example_endpoint.yaws").

advanced_close_with_reason(Config) ->
    close_with_reason(Config, "/websockets_autobahn_endpoint.yaws").

close_with_reason(Config, WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    ?assertEqual(ok, gen_tcp:send(Sock, <<136,4,1000:16/big,"Ok">>)),

    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_close_limit_size(Config) ->
    close_limit_size(Config, "/websockets_example_endpoint.yaws").

advanced_close_limit_size(Config) ->
    close_limit_size(Config, "/websockets_autobahn_endpoint.yaws").

close_limit_size(Config, WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    Reason = list_to_binary(lists:duplicate(123, $*)),
    ?assertEqual(ok, gen_tcp:send(Sock, <<136,125,1000:16/big,Reason/binary>>)),

    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_close_toolong(Config) ->
    close_toolong(Config, "/websockets_example_endpoint.yaws").

advanced_close_toolong(Config) ->
    close_toolong(Config, "/websockets_autobahn_endpoint.yaws").

close_toolong(Config, WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    Reason = list_to_binary(lists:duplicate(124, $*)),
    ?assertEqual(ok, gen_tcp:send(Sock, <<136,126,1000:16/big,Reason/binary>>)),

    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_close_invalid_utf8(Config) ->
    close_invalid_utf8(Config, "/websockets_example_endpoint.yaws").

advanced_close_invalid_utf8(Config) ->
    close_invalid_utf8(Config, "/websockets_autobahn_endpoint.yaws").

close_invalid_utf8(Config, WSPath) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    Reason = <<16#ce,16#ba,16#e1,16#bd,16#b9,16#cf,16#83,16#ce,16#bc,16#ce,
               16#b5,16#ed,16#a0,16#80,16#65,16#64,16#69,16#74,16#65,16#64>>,
    ?assertEqual(ok, gen_tcp:send(Sock, <<136,22,1000:16/big,Reason/binary>>)),

    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_INVALID_PAYLOAD])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_close_valid_codes(Config) ->
    lists:foreach(
      fun(Code) ->
              close_valid_code(Config, "/websockets_example_endpoint.yaws", Code)
      end,
      [1000,1001,1002,1003,1007,1008,1009,1010,1011,3000,3999,4000,4999]
     ).

advanced_close_valid_codes(Config) ->
    lists:foreach(
      fun(Code) ->
              close_valid_code(Config, "/websockets_autobahn_endpoint.yaws", Code)
      end,
      [1000,1001,1002,1003,1007,1008,1009,1010,1011,3000,3999,4000,4999]
     ).

close_valid_code(Config, WSPath, Code) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    ?assertEqual(ok, wsclose(Sock, Code, <<>>)),
    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [Code])),
    ?assertEqual(ok, close(Sock)),
    ok.

basic_close_invalid_codes(Config) ->
    lists:foreach(
      fun(Code) ->
              close_invalid_code(Config, "/websockets_example_endpoint.yaws", Code)
      end,
      [0,999,1004,1005,1006,1012,1013,1014,1015,1016,1100,2000,2999,5000,65536]
     ).

advanced_close_invalid_codes(Config) ->
    lists:foreach(
      fun(Code) ->
              close_invalid_code(Config, "/websockets_autobahn_endpoint.yaws", Code)
      end,
      [0,999,1004,1005,1006,1012,1013,1014,1015,1016,1100,2000,2999,5000,65536]
     ).

close_invalid_code(Config, WSPath, Code) ->
    Key = "dGhlIHNhbXBsZSBub25jZQ==",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    ?assertEqual(ok, wsclose(Sock, Code, <<>>)),
    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR])),
    ?assertEqual(ok, close(Sock)),
    ok.

close_timeout(Config) ->
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",
    WSPath = "/websockets_example_endpoint.yaws?extversion=true",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    SndFrame = #frame{opcode=?WS_OPCODE_TEXT, payload = <<"bye">>},
    ?assertEqual(ok, send_frame(Sock, SndFrame, all)),
    {ok, Frames} = wsflush(Sock, false),
    LastFrame  = lists:last(Frames),
    ?assert(is_valid_close_frame([LastFrame], [?WS_STATUS_NORMAL])),
    timer:sleep(5500), %% Waiting for the timeout
    ?assertEqual({error, closed}, gen_tcp:recv(Sock, 0)),
    ?assertEqual(ok, close(Sock)),
    ok.

keepalive_timeout(Config) ->
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",
    WSPath = "/websockets_example_endpoint.yaws?keepalive=true&timeout=5000&drop=true",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    timer:sleep(5500),
    {ok, RcvFrame1} = read_frame(Sock),
    ?assertEqual(?WS_OPCODE_PING, RcvFrame1#frame.opcode),
    ?assertEqual(ok, send_frame(Sock, #frame{opcode=?WS_OPCODE_PONG}, all)),

    timer:sleep(5500),
    {ok, RcvFrame2} = read_frame(Sock),
    ?assertEqual(?WS_OPCODE_PING, RcvFrame2#frame.opcode),

    timer:sleep(2000),
    ?assertEqual({error, closed}, gen_tcp:recv(Sock, 0)),
    ?assertEqual(ok, close(Sock)),
    ok.

too_big_frame(Config) ->
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",
    WSPath = "/websockets_example_endpoint.yaws",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    Payload1 = crypto:strong_rand_bytes(16*1024*1024),
    SndFrame1 = #frame{opcode=?WS_OPCODE_BINARY, payload=Payload1},
    ?assertEqual(ok, send_frame(Sock, SndFrame1, all)),
    {ok, RcvFrame} = read_frame(Sock),
    ?assertEqual(?WS_OPCODE_BINARY, RcvFrame#frame.opcode),
    ?assertEqual(Payload1, RcvFrame#frame.payload),

    Payload2 = <<0, Payload1/binary>>,
    SndFrame2 = #frame{opcode=?WS_OPCODE_BINARY, payload=Payload2},
    {ok, Closed} = case send_frame(Sock, SndFrame2, all) of
                       ok              -> {ok, false};
                       {error, closed} -> {ok, true}
                   end,
    {ok, Frames} = wsflush(Sock, true),
    ?assert(case Closed of
                false -> is_valid_close_frame(Frames, [?WS_STATUS_MSG_TOO_BIG]);
                true  -> true
            end),
    ?assertEqual(ok, close(Sock)),
    ok.

close_unmasked_frame(Config) ->
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",
    WSPath = "/websockets_example_endpoint.yaws?close_unmasked=true",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    %% unmasked
    SndFrame   = #frame{opcode=?WS_OPCODE_TEXT, payload = <<"unmasked">>},
    ?assertEqual(ok, send_frame(Sock, SndFrame, all)),
    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_PROTO_ERROR])),
    ?assertEqual(ok, close(Sock)),
    ok.

too_big_message(Config) ->
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",
    WSPath = "/websockets_example_endpoint.yaws",

    {ok, Sock} = open("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    Payload1 = crypto:strong_rand_bytes(16*1024*1024),
    <<Frag1:(4*1024)/binary, Frag2:(4*1024)/binary,
      Frag3:(4*1024)/binary, Frag4/binary>> = Payload1,
    SndFrame1 = #frame{fin=false, opcode=?WS_OPCODE_BINARY, payload=Frag1},
    SndFrame2 = #frame{fin=false, opcode=?WS_OPCODE_CONTINUATION, payload=Frag2},
    SndFrame3 = #frame{fin=false, opcode=?WS_OPCODE_CONTINUATION, payload=Frag3},
    SndFrame4 = #frame{opcode=?WS_OPCODE_CONTINUATION, payload=Frag4},
    ?assertEqual(ok, send_frame(Sock, SndFrame1, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame2, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame3, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame4, all)),
    {ok, RcvFrame} = read_frame(Sock),
    ?assertEqual(?WS_OPCODE_BINARY, RcvFrame#frame.opcode),
    ?assertEqual(Payload1, RcvFrame#frame.payload),

    Payload2 = <<0, Payload1/binary>>,
    <<Frag5:(4*1024)/binary, Frag6:(4*1024)/binary,
      Frag7:(4*1024)/binary, Frag8/binary>> = Payload2,
    SndFrame5 = #frame{fin=false, opcode=?WS_OPCODE_BINARY, payload=Frag5},
    SndFrame6 = #frame{fin=false, opcode=?WS_OPCODE_CONTINUATION, payload=Frag6},
    SndFrame7 = #frame{fin=false, opcode=?WS_OPCODE_CONTINUATION, payload=Frag7},
    SndFrame8 = #frame{opcode=?WS_OPCODE_CONTINUATION, payload=Frag8},
    ?assertEqual(ok, send_frame(Sock, SndFrame5, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame6, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame7, all)),
    ?assertEqual(ok, send_frame(Sock, SndFrame8, all)),
    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_MSG_TOO_BIG])),
    ?assertEqual(ok, close(Sock)),
    ok.


secure_websocket(Config) ->
    Key    = "dGhlIHNhbXBsZSBub25jZQ==",
    WSPath = "/websockets_example_endpoint.yaws",

    {ok, Sock} = sslopen("localhost", testsuite:get_yaws_port(1, Config)),
    ?assertMatch({ok, {101, _}}, wsopen(Sock, Key, WSPath, "http://localhost", 13)),

    SndFrame = #frame{opcode=?WS_OPCODE_TEXT, payload = <<"small payload">>},
    ?assertEqual(ok, send_frame(Sock, SndFrame, all)),
    {ok, RcvFrame}      = read_frame(Sock),
    ?assertEqual(?WS_OPCODE_TEXT, RcvFrame#frame.opcode),
    ?assertEqual(<<"small payload">>, RcvFrame#frame.payload),

    ?assertEqual(ok, wsclose(Sock, ?WS_STATUS_NORMAL, <<>>)),
    {ok, Frames} = wsflush(Sock, true),
    ?assert(is_valid_close_frame(Frames, [?WS_STATUS_NORMAL])),
    ?assertEqual(ok, close(Sock)),
    ok.

%%====================================================================
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
    HashBin = crypto:hash(sha, Salted),
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
                  ssl:recv(SslSock, 0, 5000);
              undefined ->
                  inet:setopts(Sock, [{packet, http}, {packet_size, 16#4000}]),
                  gen_tcp:recv(Sock, 0, 5000)
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
              {ok, SslSock} -> ssl:recv(SslSock, 0, 5000);
              undefined     -> gen_tcp:recv(Sock, 0, 5000)
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
