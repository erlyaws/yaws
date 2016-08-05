-module(subconfig_SUITE).

-include("testsuite.hrl").

-compile(export_all).

all() ->
    [
     bad_global_subconfig,
     bad_server_subconfig,
     absolute_subconfig,
     relative_subconfig
    ].

group() ->
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
bad_global_subconfig(_Config) ->
    Env1 = #env{debug = false,
                conf  = {file, ?tempdir(?MODULE) ++ "/yaws_global_subconfig_notfound1.conf"}},
    {error, _} = yaws_config:load(Env1),

    Env2 = #env{debug = false,
                conf  = {file, ?tempdir(?MODULE) ++ "/yaws_global_subconfig_notfound2.conf"}},
    {error, _} = yaws_config:load(Env2),

    Env3 = #env{debug = false,
                conf  = {file, ?tempdir(?MODULE) ++ "/yaws_global_subconfig_notfound3.conf"}},
    {error, _} = yaws_config:load(Env3),

    ok.

bad_server_subconfig(_Config) ->
    Env1 = #env{debug = false,
                conf  = {file, ?tempdir(?MODULE) ++ "/yaws_server_subconfig_notfound1.conf"}},
    {error, _} = yaws_config:load(Env1),

    Env2 = #env{debug = false,
                conf  = {file, ?tempdir(?MODULE) ++ "/yaws_server_subconfig_notfound2.conf"}},
    {error, _} = yaws_config:load(Env2),

    Env3 = #env{debug = false,
                conf  = {file, ?tempdir(?MODULE) ++ "/yaws_server_subconfig_notfound3.conf"}},
    {error, _} = yaws_config:load(Env3),

    Env4 = #env{debug = false,
                conf  = {file, ?tempdir(?MODULE) ++ "/yaws_server_subconfig_closing_tag.conf"}},
    {error, _} = yaws_config:load(Env4),

    ok.

absolute_subconfig(_Config) ->
    F = case yaws_dynopts:have_bad_wildcard() of
            true  -> ?tempdir(?MODULE) ++ "/yaws_absolute_no_wildcard.conf";
            false -> ?tempdir(?MODULE) ++ "/yaws_absolute.conf"
        end,
    Env = #env{debug=false, conf={file,F}},
    {ok, _GC, SCs} = yaws_config:load(Env),
    check_sconfs(lists:flatten(SCs)).

relative_subconfig(_Config) ->
    F = case yaws_dynopts:have_bad_wildcard() of
            true  -> ?tempdir(?MODULE) ++ "/yaws_relative_no_wildcard.conf";
            false -> ?tempdir(?MODULE) ++ "/yaws_relative.conf"
        end,
    Env = #env{debug=false, conf={file,F}},
    {ok, _GC, SCs} = yaws_config:load(Env),
    check_sconfs(lists:flatten(SCs)).

check_sconfs(SCs) ->
    case yaws_dynopts:have_bad_wildcard() of
        true ->
            ?assertEqual(3, length(SCs)),
            ?assert(lists:keymember(8000, #sconf.port, SCs)),
            ?assert(lists:keymember(8003, #sconf.port, SCs)),
            ?assert(lists:keymember(8004, #sconf.port, SCs)),

            SC_8000 = lists:keyfind(8000, #sconf.port, SCs),
            ?assertEqual(3, length(SC_8000#sconf.serveralias)),
            ?assert(lists:member("server_subconfig",     SC_8000#sconf.serveralias)),
            ?assert(lists:member("server_subconfigdir1", SC_8000#sconf.serveralias)),
            ?assert(lists:member("server_subconfigdir2", SC_8000#sconf.serveralias));
        false ->
            ?assertEqual(5, length(SCs)),
            ?assert(lists:keymember(8000, #sconf.port, SCs)),
            ?assert(lists:keymember(8001, #sconf.port, SCs)),
            ?assert(lists:keymember(8002, #sconf.port, SCs)),
            ?assert(lists:keymember(8003, #sconf.port, SCs)),
            ?assert(lists:keymember(8004, #sconf.port, SCs)),

            SC_8000 = lists:keyfind(8000, #sconf.port, SCs),
            ?assertEqual(5, length(SC_8000#sconf.serveralias)),
            ?assert(lists:member("server_subconfig",     SC_8000#sconf.serveralias)),
            ?assert(lists:member("server_subconfig1",    SC_8000#sconf.serveralias)),
            ?assert(lists:member("server_subconfig2",    SC_8000#sconf.serveralias)),
            ?assert(lists:member("server_subconfigdir1", SC_8000#sconf.serveralias)),
            ?assert(lists:member("server_subconfigdir2", SC_8000#sconf.serveralias))
    end,
    ok.
