%% -*- coding: utf-8; -*-
-module(yaws_ls_tests).
-include_lib("eunit/include/eunit.hrl").

trim_helper(Expected, Input, Len) ->
    Enc = binary_to_list(unicode:characters_to_binary(Input)),
    ExpectedEnc = binary_to_list(unicode:characters_to_binary(Expected)),
    ExpectedEnc =:= yaws_ls:trim(Enc, Len).

trim_test_() ->
    %% [19990,30028,26479] =:= "世界杯"
    %% [19990,30028,26479,36275,29699,36187] =:= "世界杯足球赛"
    %% Written in this way to support OTP < R16B.
    [?_assert(trim_helper([19990,30028,26479|"..&gt;"],
                          [19990,30028,26479,36275,29699,36187],
                          11)),
     ?_assert(trim_helper("xxxxxxxx..&gt;", "xxxxxxxxxxxx", 11))].

parse_query_test_() ->
    [
        ?_assertEqual({"/", 1, normal,  "/?z=x"}, yaws_ls:parse_query("/?z=x")),
        ?_assertEqual({"/", 2, reverse, "/?m=r"}, yaws_ls:parse_query("/?m=r")),
        ?_assertEqual({"/", 2, reverse, "/?M=r"}, yaws_ls:parse_query("/?M=r")),
        ?_assertEqual({"/", 3, normal,  "/?s=n"}, yaws_ls:parse_query("/?s=n")),
        ?_assertEqual({"/", 3, normal,  "/?S=n"}, yaws_ls:parse_query("/?S=n")),
        ?_assertEqual({"/", 4, reverse, "/?d=r"}, yaws_ls:parse_query("/?d=r")),
        ?_assertEqual({"/", 4, reverse, "/?D=r"}, yaws_ls:parse_query("/?D=r"))
    ].
