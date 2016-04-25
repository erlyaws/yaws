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
