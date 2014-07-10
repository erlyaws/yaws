-module(subconfig).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-include("yaws.hrl").
-include("tftest.hrl").


-ifndef(HAVE_BAD_WILDCARD).

absolute_subconfig_test() ->
    Env = #env{debug = false,
               conf  = {file, ?builddir++"/subconfig_DATA/yaws_absolute.conf"}},
    {ok, _GC, SCs} = yaws_config:load(Env),
    check_sconfs(lists:flatten(SCs)).

relative_subconfig_test() ->
    Env = #env{debug = false,
               conf  = {file, ?srcdir++"/subconfig_DATA/yaws_relative.conf"}},
    {ok, _GC, SCs} = yaws_config:load(Env),
    check_sconfs(lists:flatten(SCs)).

check_sconfs(SCs) ->
    ?assertEqual(5, length(SCs)),
    ?assert(lists:keymember(80,   #sconf.port, SCs)),
    ?assert(lists:keymember(8001, #sconf.port, SCs)),
    ?assert(lists:keymember(8002, #sconf.port, SCs)),
    ?assert(lists:keymember(8003, #sconf.port, SCs)),
    ?assert(lists:keymember(8004, #sconf.port, SCs)),
    ok.

-else.

absolute_subconfig_test() ->
    Env = #env{debug = false,
               conf  = {file, ?builddir++"/subconfig_DATA/yaws_absolute_no_wildcard.conf"}},
    {ok, _GC, SCs} = yaws_config:load(Env),
    check_sconfs(lists:flatten(SCs)).

relative_subconfig_test() ->
    Env = #env{debug = false,
               conf  = {file, ?srcdir++"/subconfig_DATA/yaws_relative_no_wildcard.conf"}},
    {ok, _GC, SCs} = yaws_config:load(Env),
    check_sconfs(lists:flatten(SCs)).

unsupported_wildcard_test() ->
    Env1 = #env{debug = false,
                conf  = {file, ?builddir++"/subconfig_DATA/yaws_absolute.conf"}},
    {error, _} = yaws_config:load(Env1),

    Env2 = #env{debug = false,
               conf  = {file, ?srcdir++"/subconfig_DATA/yaws_relative.conf"}},
    {error, _} = yaws_config:load(Env2),
    ok.


check_sconfs(SCs) ->
    ?assertEqual(3, length(SCs)),
    ?assert(lists:keymember(80,   #sconf.port, SCs)),
    ?assert(lists:keymember(8003, #sconf.port, SCs)),
    ?assert(lists:keymember(8004, #sconf.port, SCs)),
    ok.

-endif.
