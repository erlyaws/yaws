-module(json2_SUITE).

-include("testsuite.hrl").

-compile(export_all).

all() ->
    [
     json2_decode_valid,
     json2_decode_invalid,
     json2_decode_invalid_utf8
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
json2_decode_valid(_Config) ->
    Dir = filename:join(?srcdir, "json2_SUITE_data/valid"),
    Tests = filelib:wildcard(filename:join(Dir, "*.json")),
    [begin
         {ok, Bin} = file:read_file(T),
         Str = unicode:characters_to_list(Bin),
         ?assertMatch({T, {ok, _}}, {T, json2:decode_string(Str)})
     end || T <- Tests],
    ok.

json2_decode_invalid(_Config) ->
    Dir = filename:join(?srcdir, "json2_SUITE_data/invalid"),
    Tests = filelib:wildcard(filename:join(Dir, "*.json")),
    [begin
         {ok, Bin} = file:read_file(T),
         Str = unicode:characters_to_list(Bin),
         ?assertMatch({T, {error, _}}, {T, json2:decode_string(Str)})
     end || T <- Tests],
    ok.

json2_decode_invalid_utf8(_Config) ->
    Str1 = [$", 16#d848, $"], %% high surrogate only
    Str2 = [$", 16#dc49, $"],  %% low surrogate only
    ?assertMatch({Str1, {error, _}}, {Str1, json2:decode_string(Str1)}),
    ?assertMatch({Str2, {error, _}}, {Str2, json2:decode_string(Str2)}),
    ok.
