-module(json2_SUITE).

-include("testsuite.hrl").

-compile(export_all).

all() ->
    [
     json2_decode_valid,
     json2_decode_invalid,
     json2_decode_invalid_utf8,
     json2_obj_fetch
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
    Dir = filename:join(?data_srcdir(?MODULE), "valid"),
    Tests = filelib:wildcard(filename:join(Dir, "*.json")),
    ?assertMatch([_|_], Tests),
    [begin
         {ok, Bin} = file:read_file(T),
         Str = unicode:characters_to_list(Bin),
         ?assertMatch({T, {ok, _}}, {T, json2:decode_string(Str)})
     end || T <- Tests],
    ok.

json2_decode_invalid(_Config) ->
    Dir = filename:join(?data_srcdir(?MODULE), "invalid"),
    Tests = filelib:wildcard(filename:join(Dir, "*.json")),
    ?assertMatch([_|_], Tests),
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

json2_obj_fetch(_Config) ->
    F = filename:join(?data_srcdir(?MODULE), "valid/simple-object.json"),
    {ok, Bin} = file:read_file(F),
    Str = unicode:characters_to_list(Bin),
    {ok, Obj} = json2:decode_string(Str),
    ?assertEqual({array,[]}, json2:obj_fetch("a", Obj)),
    NotFound = try
                   json2:obj_fetch("b", Obj)
               catch
                   C:Exc ->
                       {C, Exc}
               end,
    ?assertEqual({exit, {struct_no_key, "b"}}, NotFound),
    ?assertEqual(foo, json2:obj_fetch("b", Obj, foo)),
    ok.
