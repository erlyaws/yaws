-module(dynopts_SUITE).

-include("testsuite.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [
     compare_version,
     default_dynopts,
     generated_dynopts
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

end_per_group(_Test, _Config) ->
    ok.

init_per_testcase(_Test, Config) ->
    %% Be sure to purge generated module, if any
    code:purge(yaws_dynopts),
    code:delete(yaws_dynopts),
    Config.

end_per_testcase(_Test, _Config) ->
    ok.

%%====================================================================
compare_version(_Config) ->
    ?assert(yaws_dynopts:is_greater("2",         "1")),
    ?assert(yaws_dynopts:is_greater("2.0",       "1.0")),
    ?assert(yaws_dynopts:is_greater("1.1",       "1.0")),
    ?assert(yaws_dynopts:is_greater("1.1",       "1.0.0")),
    ?assert(yaws_dynopts:is_greater("1.1.0",     "1.0.1")),
    ?assert(yaws_dynopts:is_greater("1.0.2",     "1.0.1")),
    ?assert(yaws_dynopts:is_greater("1.0.2-rc1", "1.0.1")),
    ?assert(yaws_dynopts:is_greater("1.0.2",     "1.0.1-rc1")),
    ?assert(yaws_dynopts:is_greater("1.0.2-rc1", "1.0.1-rc1")),
    ?assert(yaws_dynopts:is_greater("1.0.2-rc1", "1.0-rc1")),
    ok.

default_dynopts(Config) ->
    ?assertNot(yaws_dynopts:is_generated()),
    ?assertEqual(ok, check_have_http_uri_parse(Config)),
    ok.

generated_dynopts(_Config) ->
    ?assertNot(yaws_dynopts:is_generated()),
    HaveHttpUriParse     = yaws_dynopts:have_http_uri_parse(),
    HaveSafeRelativePath = yaws_dynopts:have_safe_relative_path(),

    GC = yaws_config:make_default_gconf(false, "dummy_id"),
    ?assertEqual(ok, yaws_dynopts:generate(GC)),

    ?assert(yaws_dynopts:is_generated()),
    ?assertEqual(HaveHttpUriParse, yaws_dynopts:have_http_uri_parse()),
    ?assertEqual(HaveSafeRelativePath, yaws_dynopts:have_safe_relative_path()),
    ok.

%%====================================================================
check_have_http_uri_parse(_Config) ->
    ?assertEqual({ok,{http,[],"yaws.hyber.org",80,"/",[]}},
                 yaws_dynopts:http_uri_parse("http://yaws.hyber.org/")),
    case yaws_dynopts:have_http_uri_parse() of
        true ->
            {file, _} = code:is_loaded(http_uri),
            false = code:is_loaded(uri_string);
        false ->
            {file, _} = code:is_loaded(uri_string),
            false = code:is_loaded(http_uri)
    end,
    ok.

check_have_safe_relative_path(_Config) ->
    case yaws_dynopts:have_safe_relative_path() of
        true -> {file, _} = code:is_loaded(filelib);
        false -> false = code:is_loaded(filelib)
    end,
    ok.
