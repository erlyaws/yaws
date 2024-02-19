-module(yaws_ls_SUITE).

-include("testsuite.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [
     trim,
     parse_query
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
trim_helper(Expected, Input, Len) ->
    Enc = binary_to_list(unicode:characters_to_binary(Input)),
    ExpectedEnc = binary_to_list(unicode:characters_to_binary(Expected)),
    ExpectedEnc =:= yaws_ls:trim(Enc, Len).

trim(_Config) ->
    %% [19990,30028,26479] =:= "世界杯"
    %% [19990,30028,26479,36275,29699,36187] =:= "世界杯足球赛"
    %% Written in this way to support OTP < R16B.
    ?assert(trim_helper([19990,30028,26479|"..&gt;"],
                        [19990,30028,26479,36275,29699,36187],
                        11)),
    ?assert(trim_helper("xxxxxxxx..&gt;", "xxxxxxxxxxxx", 11)),
    ok.

parse_query(_Config) ->
    ?assertEqual({"/", 1, normal,  "/?z=x"}, yaws_ls:parse_query("/?z=x")),
    ?assertEqual({"/", 2, reverse, "/?m=r"}, yaws_ls:parse_query("/?m=r")),
    ?assertEqual({"/", 2, reverse, "/?M=r"}, yaws_ls:parse_query("/?M=r")),
    ?assertEqual({"/", 3, normal,  "/?s=n"}, yaws_ls:parse_query("/?s=n")),
    ?assertEqual({"/", 3, normal,  "/?S=n"}, yaws_ls:parse_query("/?S=n")),
    ?assertEqual({"/", 4, reverse, "/?d=r"}, yaws_ls:parse_query("/?d=r")),
    ?assertEqual({"/", 4, reverse, "/?D=r"}, yaws_ls:parse_query("/?D=r")).
