-module(gconf).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-include("yaws.hrl").
-include("tftest.hrl").

setup_default_gconf_test() ->
    Dir     = yaws_dir(),
    EbinDir = Dir++"/examples/ebin",
    IncDir  = Dir++"/examples/include",

    GC = yaws_config:make_default_gconf(false, "test"),
    GC = yaws:create_gconf([], "test"),

    DefFlags = ?GC_FAIL_ON_BIND_ERR bor
        ?GC_PICK_FIRST_VIRTHOST_ON_NOMATCH bor
        ?GC_COPY_ERRLOG,
    Flags = case yaws_sendfile:have_sendfile() of
                true  -> ?GC_USE_YAWS_SENDFILE bor DefFlags;
                false -> DefFlags
            end,

    true = is_same_path(Dir,     get_gconf_attr(yaws_dir,    GC)),
    true = is_same_path(EbinDir, get_gconf_attr(ebin_dir,    GC)),
    true = is_same_path(IncDir,  get_gconf_attr(include_dir, GC)),

    {trace, false}  = get_gconf_attr(trace, GC),
    {flags, Flags}  = get_gconf_attr(flags, GC),
    {id,    "test"} = get_gconf_attr(id,    GC),

    ok.

set_gc_flags_test() ->
    GC = yaws:create_gconf([], "test"),
    {tty_trace, true} =
        check_gc_flags(tty_trace, ?GC_TTY_TRACE, GC),
    {debug, true} =
        check_gc_flags(debug, ?GC_DEBUG, GC),
    {copy_errlog, true} =
        check_gc_flags(copy_errlog, ?GC_COPY_ERRLOG, GC),
    {backwards_compat_parse, true} =
        check_gc_flags(backwards_compat_parse, ?GC_BACKWARDS_COMPAT_PARSE, GC),
    {fail_on_bind_err, true} =
        check_gc_flags(fail_on_bind_err, ?GC_FAIL_ON_BIND_ERR, GC),
    {log_resolve_hostname, true} =
        check_gc_flags(log_resolve_hostname, ?GC_LOG_RESOLVE_HOSTNAME, GC),
    {pick_first_virthost_on_nomatch, true} =
        check_gc_flags(pick_first_virthost_on_nomatch,
                       ?GC_PICK_FIRST_VIRTHOST_ON_NOMATCH, GC),
    ok.


setup_mime_types_info_test() ->
    GC0 = yaws:create_gconf([], "test"),
    undefined = yaws:gconf_mime_types_info(GC0),

    GC1 = yaws:create_gconf([{mime_types_info, []}], "test"),
    MI1 = yaws:new_mime_types_info(),
    MI1 = yaws:gconf_mime_types_info(GC1),

    MI2 = yaws:mime_types_info_mime_types_file(MI1, "/etc/mime.types"),
    GC2 = yaws:create_gconf([{mime_types_info, MI2}], "test"),
    "/etc/mime.types" = yaws:mime_types_info_mime_types_file
                          (yaws:gconf_mime_types_info(GC2)
                          ),
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

is_same_path(Dir1, {_, Dir2}) ->
    (real_dir_path(Dir1) == real_dir_path(Dir2)).

yaws_dir() ->
    filename:dirname(     %% yaws_dir/
      filename:dirname(   %% yaws_dir/test/
        filename:dirname( %% yaws_dir/test/eunit
          filename:absname(code:which(?MODULE))
         )
       )
     ).

real_dir_path(Path) ->
    {ok, CurCwd} = file:get_cwd(),
    ok = file:set_cwd(Path),
    {ok, RealPath} = file:get_cwd(),
    ok = file:set_cwd(CurCwd),
    filename:absname(RealPath).
