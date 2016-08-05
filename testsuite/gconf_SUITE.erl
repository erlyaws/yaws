-module(gconf_SUITE).

-include("testsuite.hrl").

-compile(export_all).

all() ->
    [
     setup_default_gconf,
     set_gc_flags,
     setup_mime_types_info
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
setup_default_gconf(_Config) ->
    YawsDir = real_dir_path(yaws_dir()),
    EbinDir = filename:join(YawsDir, "examples/ebin"),
    IncDir  = filename:join(YawsDir, "examples/include"),

    GC = yaws_config:make_default_gconf(false, "test"),
    GC = yaws:create_gconf([], "test"),

    DefFlags = ?GC_FAIL_ON_BIND_ERR bor
        ?GC_PICK_FIRST_VIRTHOST_ON_NOMATCH bor
        ?GC_COPY_ERRLOG,
    Flags = case yaws_sendfile:have_sendfile() of
                true  -> ?GC_USE_YAWS_SENDFILE bor DefFlags;
                false -> DefFlags
            end,

    {yaws_dir,    YDir} = get_gconf_attr(yaws_dir,    GC),
    {ebin_dir,    EDir} = get_gconf_attr(ebin_dir,    GC),
    {include_dir, IDir} = get_gconf_attr(include_dir, GC),
    io:format("~p~n", [code:where_is_file("yaws.beam")]),
    io:format("~p~n~p~n~p~n", [YawsDir, EbinDir, IncDir]),
    io:format("~p~n~p~n~p~n", [YDir, EDir, IDir]),
    YDir1 = real_dir_path(YDir),
    EDir1 = real_dir_path(EDir),
    IDir1 = real_dir_path(IDir),

    ?assertEqual(YDir1, YawsDir),
    ?assertEqual(EDir1, EbinDir),
    ?assertEqual(IDir1, IncDir),

    ?assertEqual({trace, false},  get_gconf_attr(trace, GC)),
    ?assertEqual({flags, Flags},  get_gconf_attr(flags, GC)),
    ?assertEqual({id,    "test"}, get_gconf_attr(id,    GC)),

    ok.

set_gc_flags(_Config) ->
    GC = yaws:create_gconf([], "test"),
    ?assertEqual({tty_trace, true},
                 check_gc_flags(tty_trace, ?GC_TTY_TRACE, GC)),
    ?assertEqual({debug, true},
                 check_gc_flags(debug, ?GC_DEBUG, GC)),
    ?assertEqual({copy_errlog, true},
                 check_gc_flags(copy_errlog, ?GC_COPY_ERRLOG, GC)),
    ?assertEqual({backwards_compat_parse, true},
                 check_gc_flags(backwards_compat_parse, ?GC_BACKWARDS_COMPAT_PARSE, GC)),
    ?assertEqual({fail_on_bind_err, true},
                 check_gc_flags(fail_on_bind_err, ?GC_FAIL_ON_BIND_ERR, GC)),
    ?assertEqual({log_resolve_hostname, true},
                 check_gc_flags(log_resolve_hostname,
                                ?GC_LOG_RESOLVE_HOSTNAME, GC)),
    ?assertEqual({pick_first_virthost_on_nomatch, true},
                 check_gc_flags(pick_first_virthost_on_nomatch,
                                ?GC_PICK_FIRST_VIRTHOST_ON_NOMATCH, GC)),
    ok.


setup_mime_types_info(_Config) ->
    GC0 = yaws:create_gconf([], "test"),
    ?assertEqual(undefined, yaws:gconf_mime_types_info(GC0)),

    GC1 = yaws:create_gconf([{mime_types_info, []}], "test"),
    MI1 = yaws:new_mime_types_info(),
    MI1 = yaws:gconf_mime_types_info(GC1),

    MI2 = yaws:mime_types_info_mime_types_file(MI1, "/etc/mime.types"),
    GC2 = yaws:create_gconf([{mime_types_info, MI2}], "test"),

    MI  = yaws:mime_types_info_mime_types_file(yaws:gconf_mime_types_info(GC2)),

    ?assertEqual("/etc/mime.types", MI),
    ok.

%% =======================================================================
get_gconf_attr(Name, GConf) ->
    Fun = list_to_atom("gconf_" ++ atom_to_list(Name)),
    {Name, yaws:Fun(GConf)}.


check_gc_flags(Flag, Id, GConf0) ->
    Flags0 = yaws:gconf_flags(GConf0),
    Val0   = (Flags0 band Id) /= 0,

    GConf1 = yaws:create_gconf([{flags, [{Flag, not Val0}]}], "test"),
    Flags1 = yaws:gconf_flags(GConf1),
    Flags2 = yaws:flag(Flags1, Id, Val0),

    {Flag,
     (not Val0 == ((Flags1 band Id) /= 0) andalso Flags2 == Flags0)}.

yaws_dir() ->
    filename:dirname(   %% yaws_dir/
      filename:dirname( %% yaws_dir/testsuite
        filename:absname(code:which(?MODULE))
       )
     ).

real_dir_path(Path) ->
    {ok, CurCwd} = file:get_cwd(),
    ok = file:set_cwd(Path),
    {ok, RealPath} = file:get_cwd(),
    ok = file:set_cwd(CurCwd),
    filename:absname(RealPath).
