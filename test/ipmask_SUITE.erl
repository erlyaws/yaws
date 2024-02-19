-module(ipmask_SUITE).

-include("testsuite.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [
     parse_ipmask,
     match_ipmask
    ].

groups() ->
    [
    ].

%%====================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Test, Config) ->
    Config.

end_per_testcase(_Test, _Config) ->
    ok.

%%====================================================================
parse_ipmask(_Config) ->
    ?assertEqual({127,0,0,1},       yaws:parse_ipmask("127.0.0.1")),
    ?assertEqual({0,0,0,0,0,0,0,1}, yaws:parse_ipmask("::1")),
    ?assertThrow({error, einval},   yaws:parse_ipmask("invalid")),
    ?assertThrow({error, einval},   yaws:parse_ipmask(invalid)),
    ?assertThrow({error, einval},   yaws:parse_ipmask("127.0.0.500")),
    ?assertThrow({error, einval},   yaws:parse_ipmask("::ZZ")),

    ?assertEqual({127,0,0,1},       yaws:parse_ipmask("127.0.0.1/32")),
    ?assertEqual({0,0,0,0,0,0,0,1}, yaws:parse_ipmask("::1/128")),

    ?assertEqual({{127,0,0,1}, {127,0,0,254}},
                 yaws:parse_ipmask("127.0.0.1/24")),
    ?assertEqual({{127,0,0,1}, {127,0,0,254}},
                 yaws:parse_ipmask("127.0.0.1/255.255.255.0")),
    ?assertEqual({{127,0,0,1}, {127,255,255,254}},
                 yaws:parse_ipmask("127.0.0.0/8")),
    ?assertEqual({{192,168,0,1}, {192,171,255,254}},
                 yaws:parse_ipmask("192.168.4.2/14")),
    ?assertEqual({{192,168,4,1}, {192,168,4,126}},
                 yaws:parse_ipmask("192.168.4.2/25")),

    ?assertEqual({{0,0,0,0,0,0,0,0}, {0,0,0,0,0,0,0,16#ffff}},
                 yaws:parse_ipmask("::1/112")),
    ?assertEqual({{0,0,0,0,0,0,0,0}, {0,0,0,0,0,0,0,16#ffff}},
                 yaws:parse_ipmask("::1/ffff:ffff:ffff:ffff:ffff:ffff:ffff:0")),
    ?assertEqual({{0,0,0,0,0,0,0,0}, {0,0,0,0,16#ffff,16#ffff,16#ffff,16#ffff}},
                 yaws:parse_ipmask("::1/64")),
    ?assertEqual({{0,0,0,0,0,0,0,0}, {0,0,0,0,0,0,0,16#003f}},
                 yaws:parse_ipmask("::1/122")),

    ?assertThrow({error, einval}, yaws:parse_ipmask("127.0.0.1/36")),
    ?assertThrow({error, einval}, yaws:parse_ipmask("127.0.0.1/256.0.0.0")),
    ?assertThrow({error, einval}, yaws:parse_ipmask("::1/::fffg")),
    ok.


match_ipmask(_Config) ->
    ?assertEqual(true, yaws:match_ipmask({127,0,0,1}, {127,0,0,1})),
    ?assertEqual(true, yaws:match_ipmask({0,0,0,0,0,0,0,1}, {0,0,0,0,0,0,0,1})),
    ?assertEqual(false,yaws:match_ipmask({127,0,0,2}, {127,0,0,1})),
    ?assertEqual(false,yaws:match_ipmask(undefined, {127,0,0,1})),
    ?assertEqual(false,yaws:match_ipmask({127,0,0,1}, {0,0,0,0,0,0,0,1})),


    ?assertEqual(true, yaws:match_ipmask({192,168,1,10},
                                         {{192,168,1,10}, {192,168,1,20}})),
    ?assertEqual(false,yaws:match_ipmask({192,168,1,30},
                                         {{192,168,1,10}, {192,168,1,20}})),

    ?assertEqual(true, yaws:match_ipmask({192,168,1,30},
                                         {{192,168,1,0}, {192,168,2,0}})),
    ?assertEqual(false,yaws:match_ipmask({192,168,2,10},
                                         {{192,168,1,0}, {192,168,2,0}})),

    ?assertEqual(true, yaws:match_ipmask({192,168,1,10},
                                         {{192,168,0,0}, {192,169,0,0}})),
    ?assertEqual(true, yaws:match_ipmask({192,168,2,10},
                                         {{192,168,0,0}, {192,169,0,0}})),
    ?assertEqual(false,yaws:match_ipmask({192,169,2,10},
                                         {{192,168,0,0}, {192,169,0,0}})),

    ?assertEqual(true, yaws:match_ipmask({192,168,2,10},
                                         {{192,0,0,0}, {193,0,0,0}})),
    ?assertEqual(true, yaws:match_ipmask({192,169,2,10},
                                         {{192,0,0,0}, {193,0,0,0}})),
    ?assertEqual(false,yaws:match_ipmask({193,169,2,10},
                                         {{192,0,0,0}, {193,0,0,0}})),

    ?assertEqual(true, yaws:match_ipmask({0,0,0,0,0,0,0,1},
                                         {{0,0,0,0,0,0,0,1}, {0,0,0,0,0,0,0,2}})),
    ?assertEqual(false,yaws:match_ipmask({0,0,0,0,0,0,0,3},
                                         {{0,0,0,0,0,0,0,1}, {0,0,0,0,0,0,0,2}})),

    ?assertEqual(true, yaws:match_ipmask({0,0,0,0,0,0,1,0},
                                         {{0,0,0,0,0,0,1,0}, {0,0,0,0,0,0,2,0}})),
    ?assertEqual(true, yaws:match_ipmask({0,0,0,0,0,0,1,3},
                                         {{0,0,0,0,0,0,1,0}, {0,0,0,0,0,0,2,0}})),
    ?assertEqual(false,yaws:match_ipmask({0,0,0,0,0,0,3,0},
                                         {{0,0,0,0,0,0,1,0}, {0,0,0,0,0,0,2,0}})),

    ?assertEqual(true, yaws:match_ipmask({0,0,0,0,0,1,0,0},
                                         {{0,0,0,0,0,1,0,0}, {0,0,0,0,0,2,0,0}})),
    ?assertEqual(true, yaws:match_ipmask({0,0,0,0,0,1,3,0},
                                         {{0,0,0,0,0,1,0,0}, {0,0,0,0,0,2,0,0}})),
    ?assertEqual(false, yaws:match_ipmask({0,0,0,0,0,3,0,0},
                                          {{0,0,0,0,0,1,0,0}, {0,0,0,0,0,2,0,0}})),

    ?assertEqual(true, yaws:match_ipmask({0,0,0,0,1,0,0,0},
                                         {{0,0,0,0,1,0,0,0}, {0,0,0,0,2,0,0,0}})),
    ?assertEqual(true, yaws:match_ipmask({0,0,0,0,1,3,0,0},
                                         {{0,0,0,0,1,0,0,0}, {0,0,0,0,2,0,0,0}})),
    ?assertEqual(false,yaws:match_ipmask({0,0,0,0,3,0,0,0},
                                         {{0,0,0,0,1,0,0,0}, {0,0,0,0,2,0,0,0}})),

    ?assertEqual(true, yaws:match_ipmask({0,0,0,1,0,0,0,0},
                                         {{0,0,0,1,0,0,0,0}, {0,0,0,2,0,0,0,0}})),
    ?assertEqual(true, yaws:match_ipmask({0,0,0,1,3,0,0,0},
                                         {{0,0,0,1,0,0,0,0}, {0,0,0,2,0,0,0,0}})),
    ?assertEqual(false,yaws:match_ipmask({0,0,0,3,0,0,0,0},
                                         {{0,0,0,1,0,0,0,0}, {0,0,0,2,0,0,0,0}})),

    ?assertEqual(true, yaws:match_ipmask({0,0,1,0,0,0,0,0},
                                         {{0,0,1,0,0,0,0,0}, {0,0,2,0,0,0,0,0}})),
    ?assertEqual(true, yaws:match_ipmask({0,0,1,3,0,0,0,0},
                                         {{0,0,1,0,0,0,0,0}, {0,0,2,0,0,0,0,0}})),
    ?assertEqual(false,yaws:match_ipmask({0,0,3,0,0,0,0,0},
                                          {{0,0,1,0,0,0,0,0}, {0,0,2,0,0,0,0,0}})),

    ?assertEqual(true, yaws:match_ipmask({0,1,0,0,0,0,0,0},
                                         {{0,1,0,0,0,0,0,0}, {0,2,0,0,0,0,0,0}})),
    ?assertEqual(true, yaws:match_ipmask({0,1,3,0,0,0,0,0},
                                         {{0,1,0,0,0,0,0,0}, {0,2,0,0,0,0,0,0}})),
    ?assertEqual(false,yaws:match_ipmask({0,3,0,0,0,0,0,0},
                                         {{0,1,0,0,0,0,0,0}, {0,2,0,0,0,0,0,0}})),

    ?assertEqual(true, yaws:match_ipmask({1,0,0,0,0,0,0,0},
                                         {{1,0,0,0,0,0,0,0}, {2,0,0,0,0,0,0,0}})),
    ?assertEqual(true, yaws:match_ipmask({1,3,0,0,0,0,0,0},
                                         {{1,0,0,0,0,0,0,0}, {2,0,0,0,0,0,0,0}})),
    ?assertEqual(false,yaws:match_ipmask({3,0,0,0,0,0,0,0},
                                         {{1,0,0,0,0,0,0,0}, {2,0,0,0,0,0,0,0}})),
    ok.
