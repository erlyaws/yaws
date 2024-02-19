-module(config_encoding_SUITE).

-include("testsuite.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [
     bad_encoding,
     good_encoding
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
bad_encoding(_Config) ->
    Enc = case file:native_name_encoding() of
              latin1 -> unicode;
              utf8   -> latin1
          end,
    Env = #env{debug    = false,
               encoding = Enc,
               conf     = {file, ?tempdir(?MODULE) ++ "/yaws_unicode.conf"}},
    {error, _} = yaws_config:load(Env),
    ok.

good_encoding(_Config) ->
    Enc = case file:native_name_encoding() of
              latin1 -> latin1;
              utf8   -> unicode
          end,
    Env = #env{debug    = false,
               encoding = Enc,
               conf     = {file, ?tempdir(?MODULE) ++ "/yaws_unicode.conf"}},
    {ok, _, _} = yaws_config:load(Env),
    ok.
