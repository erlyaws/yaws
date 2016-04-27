-module(json2_decode).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-include("yaws.hrl").
-include("tftest.hrl").


json2_decode_valid_test_() ->
    Dir = ?srcdir++"/json2_DATA/valid",
    Tests = filelib:wildcard(filename:join(Dir, "*.json")),
    [fun() ->
             {ok, Bin} = file:read_file(T),
             Str = unicode:characters_to_list(Bin),
             ?assertMatch({T, {ok, _}}, {T, json2:decode_string(Str)})
     end || T <- Tests].

json2_decode_invalid_test_() ->
    Dir = ?srcdir++"/json2_DATA/invalid",
    Tests = filelib:wildcard(filename:join(Dir, "*.json")),
    [fun() ->
             {ok, Bin} = file:read_file(T),
             Str = unicode:characters_to_list(Bin),
             ?assertMatch({T, {error, _}}, {T, json2:decode_string(Str)})
     end || T <- Tests].

json2_decode_invalid_utf8_test() ->
    Str1 = [$", 16#d848, $"], %% high surrogate only
    Str2 = [$", 16#dc49, $"],  %% low surrogate only
    [
     ?_assertMatch({Str1, {error, _}}, {Str1, json2:decode_string(Str1)}),
     ?_assertMatch({Str2, {error, _}}, {Str2, json2:decode_string(Str2)})
    ].
