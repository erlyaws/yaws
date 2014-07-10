-module(subconfig).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-include("yaws.hrl").
-include("tftest.hrl").

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

