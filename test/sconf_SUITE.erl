-module(sconf_SUITE).

-include("testsuite.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [
     setup_default_sconf,
     set_sc_flags,
     setup_ssl,
     setup_authdirs,
     setup_deflate,
     setup_mime_types_info,
     comp_sname,
     wildcomp_salias,
     ssl_renegotiation_config
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

init_per_testcase(ssl_renegotiation_config, Config) ->
    case tlsv13_supported() of
        true ->
            ok = prepare_docroots(),
            Id = "testsuite-server",
            YConf = filename:join(?tempdir(?MODULE), "yaws.conf"),
            application:load(yaws),
            application:set_env(yaws, id,   Id),
            application:set_env(yaws, conf, YConf),
            ok = yaws:start(),
            [{yaws_id, Id}, {yaws_config, YConf} | Config];
        false ->
            Config
    end;
init_per_testcase(_Test, Config) ->
    Config.

end_per_testcase(ssl_renegotiation_config, _Config) ->
    case tlsv13_supported() of
        true ->
            ok = application:stop(yaws),
            ok = application:unload(yaws);
        false ->
            ok
    end;
end_per_testcase(_Test, _Config) ->
    ok.

%%====================================================================
setup_default_sconf(_Config) ->
    Dir = filename:absname("."),

    SC1 = yaws_config:make_default_sconf(".", undefined, undefined),
    SC2 = yaws:create_sconf(".", []),
    ?assertEqual(SC1, SC2),
    ?assertEqual({docroot, Dir},         get_sconf_attr(docroot, SC1)),
    ?assertEqual({listen,  {127,0,0,1}}, get_sconf_attr(listen,  SC1)),
    ?assertEqual({port,    8000},        get_sconf_attr(port,    SC1)),


    SC3 = yaws_config:make_default_sconf(".", undefined, 8080),
    SC4 = yaws:create_sconf(".", [{port, 8080}]),
    ?assertEqual(SC3, SC4),
    ?assertEqual({port, 8080}, get_sconf_attr(port, SC3)),

    SC5 = #sconf{port=8080, docroot="/tmp"},
    ?assertEqual({listen,  {127,0,0,1}}, get_sconf_attr(listen,  SC5)),

    ok.


set_sc_flags(_Config) ->
    SC = yaws:create_sconf(".", []),
    ?assertEqual({access_log, true},
                 check_sc_flags(access_log, ?SC_ACCESS_LOG, SC)),
    ?assertEqual({auth_log, true},
                 check_sc_flags(auth_log, ?SC_AUTH_LOG, SC)),
    ?assertEqual({add_port, true},
                 check_sc_flags(add_port, ?SC_ADD_PORT, SC)),
    ?assertEqual({statistics, true},
                 check_sc_flags(statistics, ?SC_STATISTICS, SC)),
    ?assertEqual({tilde_expand, true},
                 check_sc_flags(tilde_expand, ?SC_TILDE_EXPAND, SC)),
    ?assertEqual({dir_listings, true},
                 check_sc_flags(dir_listings, ?SC_DIR_LISTINGS, SC)),
    ?assertEqual({deflate, true},
                 check_sc_flags(deflate, ?SC_DEFLATE, SC)),
    ?assertEqual({dir_all_zip, true},
                 check_sc_flags(dir_all_zip, ?SC_DIR_ALL_ZIP, SC)),
    ?assertEqual({dav, true},
                 check_sc_flags(dav, ?SC_DAV, SC)),
    ?assertEqual({fcgi_trace_protocol, true},
                 check_sc_flags(fcgi_trace_protocol, ?SC_FCGI_TRACE_PROTOCOL, SC)),
    ?assertEqual({fcgi_log_app_error, true},
                 check_sc_flags(fcgi_log_app_error, ?SC_FCGI_LOG_APP_ERROR, SC)),
    ?assertEqual({forward_proxy, true},
                 check_sc_flags(forward_proxy, ?SC_FORWARD_PROXY, SC)),
    ?assertEqual({auth_skip_docroot, true},
                 check_sc_flags(auth_skip_docroot, ?SC_AUTH_SKIP_DOCROOT, SC)),
    ok.


setup_ssl(_Config) ->
    SC0 = yaws:create_sconf(".", []),
    ?assertEqual(undefined, yaws:sconf_ssl(SC0)),

    SC1  = yaws:create_sconf(".", [{ssl, []}]),
    SSL1 = yaws:new_ssl(),
    SSL2 = yaws:sconf_ssl(SC1),
    ?assertEqual(SSL1, SSL2),

    SSL3 = yaws:ssl_keyfile(SSL1, "/tmp/yaws-key.pem"),
    SC2  = yaws:create_sconf(".", [{ssl, SSL3}]),
    Key  = yaws:ssl_keyfile(yaws:sconf_ssl(SC2)),
    ?assertEqual("/tmp/yaws-key.pem", Key),

    PVs = ['tlsv1.3', 'tlsv1.2', 'tlsv1.1', tlsv1],
    SC3 = yaws:create_sconf(".", [{ssl, [{protocol_version, PVs}]}]),
    ?assertMatch(#ssl{protocol_version=PVs}, yaws:sconf_ssl(SC3)),
    ok.


setup_authdirs(_Config) ->
    SC0 = yaws:create_sconf(".", []),
    ?assertEqual([], yaws:sconf_authdirs(SC0)),

    SC1     = yaws:create_sconf(".", [{auth, []}]),
    Auth1   = yaws:new_auth(),
    Auths   = yaws:sconf_authdirs(SC1),
    ?assertEqual([Auth1], Auths),

    Auth2 = yaws:auth_dir(Auth1, "/"),
    SC2   = yaws:create_sconf(".", [{auth, Auth2}]),
    AuthDir = yaws:auth_dir(hd(yaws:sconf_authdirs(SC2))),
    ?assertEqual("/", AuthDir),
    ok.


setup_deflate(_Config) ->
    SC0 = yaws:create_sconf(".", []),
    ?assertEqual(undefined, yaws:sconf_deflate_options(SC0)),

    SC1    = yaws:create_sconf(".", [{deflate_options, []}]),
    DOpts1 = yaws:new_deflate(),
    DOpts2 = yaws:sconf_deflate_options(SC1),
    ?assertEqual(DOpts1, DOpts2),

    DOpts3 = yaws:deflate_compression_level(DOpts1, best_speed),
    SC2    = yaws:create_sconf(".", [{deflate_options, DOpts3}]),
    CmpLvl = yaws:deflate_compression_level(yaws:sconf_deflate_options(SC2)),
    ?assertEqual(best_speed, CmpLvl),
    ok.


setup_mime_types_info(_Config) ->
    SC0 = yaws:create_sconf(".", []),
    ?assertEqual(undefined, yaws:sconf_mime_types_info(SC0)),

    SC1 = yaws:create_sconf(".", [{mime_types_info, []}]),
    MI1 = yaws:new_mime_types_info(),
    MI2 = yaws:sconf_mime_types_info(SC1),
    ?assertEqual(MI1, MI2),

    MI3 = yaws:mime_types_info_mime_types_file(MI1, "/etc/mime.types"),
    SC2 = yaws:create_sconf(".", [{mime_types_info, MI3}]),
    MTFile = yaws:mime_types_info_mime_types_file(
               yaws:sconf_mime_types_info(SC2)
              ),
    ?assertEqual("/etc/mime.types", MTFile),
    ok.


comp_sname(_Config) ->
    ?assert(yaws_server:comp_sname("www.example.org",     "www.example.org")),
    ?assert(yaws_server:comp_sname("www.example.org",     "WWW.EXAMPLE.ORG")),
    ?assert(yaws_server:comp_sname("www.example.org:80",  "www.example.org")),
    ?assert(yaws_server:comp_sname("www.example.org",     "www.example.org:80")),
    ?assert(yaws_server:comp_sname("www.example.org:443", "www.example.org:80")),

    ?assertNot(yaws_server:comp_sname("www.example.org",     "www.example.com")),
    ?assertNot(yaws_server:comp_sname("www.example.org:80",  "www.example.org.bad")),
    ?assertNot(yaws_server:comp_sname("www.example.org.bad", "www.example.org:80")),
    ?assertNot(yaws_server:comp_sname("www.example.org",     "www.example.org.bad")),
    ?assertNot(yaws_server:comp_sname("www.example.org.bad", "www.example.org")),
    ok.

wildcomp_salias(_Config) ->
    ?assert(yaws_server:wildcomp_salias("www.example.org",     "www.example.org")),
    ?assert(yaws_server:wildcomp_salias("www.example.org",     "WWW.EXAMPLE.ORG")),
    ?assert(yaws_server:wildcomp_salias("www.example.org:80",  "www.example.org")),
    ?assert(yaws_server:wildcomp_salias("www.example.org",     "www.example.org:80")),
    ?assert(yaws_server:wildcomp_salias("www.example.org:443", "www.example.org:80")),

    ?assertNot(yaws_server:wildcomp_salias("www.example.org",     "www.example.com")),
    ?assertNot(yaws_server:wildcomp_salias("www.example.org:80",  "www.example.org.bad")),
    ?assertNot(yaws_server:wildcomp_salias("www.example.org.bad", "www.example.org:80")),
    ?assertNot(yaws_server:wildcomp_salias("www.example.org",     "www.example.org.bad")),
    ?assertNot(yaws_server:wildcomp_salias("www.example.org.bad", "www.example.org")),

    ?assert(yaws_server:wildcomp_salias("www.example.org",       "*.example.org")),
    ?assert(yaws_server:wildcomp_salias("www.example.hyber.org", "*.hyber.org")),
    ?assert(yaws_server:wildcomp_salias("www.example.org",       "www.*.org")),
    ?assert(yaws_server:wildcomp_salias("www.example.hyber.org", "www.*.org")),
    ?assert(yaws_server:wildcomp_salias("www.example.org.org",   "www.*.org")),
    ?assert(yaws_server:wildcomp_salias("www.example.org",       "www.example.*")),
    ?assert(yaws_server:wildcomp_salias("www.example.hyber.org", "www.example.*")),
    ?assert(yaws_server:wildcomp_salias("www.example.org.org",   "www.example.*")),
    ?assert(yaws_server:wildcomp_salias("www.example.org",       "*.example.*")),
    ?assert(yaws_server:wildcomp_salias("www.example.hyber.org", "*.example.*")),
    ?assert(yaws_server:wildcomp_salias("erlang.example.org",     "*.example.*")),
    ?assert(yaws_server:wildcomp_salias("www.example.org",       "*.*.org")),
    ?assert(yaws_server:wildcomp_salias("www.example.hyber.org", "*.*.org")),
    ?assert(yaws_server:wildcomp_salias("..org",                "*.*.org")),
    ?assert(yaws_server:wildcomp_salias("example.org",            "*example.org")),
    ?assert(yaws_server:wildcomp_salias("yaws-example.org",       "*example.org")),

    ?assertNot(yaws_server:wildcomp_salias("www.example.com",       "*.example.org")),
    ?assertNot(yaws_server:wildcomp_salias("www.example.hyber.com", "*.example.org")),
    ?assertNot(yaws_server:wildcomp_salias("www.example.org.org",   "*.example.org")),
    ?assertNot(yaws_server:wildcomp_salias("www.example.org.com",   "*.example.org")),
    ?assertNot(yaws_server:wildcomp_salias("erlang.hyber.org",     "www.*.org")),
    ?assertNot(yaws_server:wildcomp_salias("www.example.com",       "www.*.org")),
    ?assertNot(yaws_server:wildcomp_salias("www.example.hyber.com", "www.*.org")),
    ?assertNot(yaws_server:wildcomp_salias("www.example.org.com",   "www.*.org")),
    ?assertNot(yaws_server:wildcomp_salias("erlang.example.com",     "www.example.*")),
    ?assertNot(yaws_server:wildcomp_salias("yaws.www.example.org",  "www.example.*")),

    ?assert(yaws_server:wildcomp_salias("www.example.org", "?ww.example.org")),
    ?assert(yaws_server:wildcomp_salias("www.example.org", "???.example.org")),
    ?assert(yaws_server:wildcomp_salias("www-example.org", "www?example.org")),

    ?assertNot(yaws_server:wildcomp_salias("www.example.com", "?ww.example.org")),
    ?assertNot(yaws_server:wildcomp_salias("ww.example.org",  "???.example.org")),
    ?assertNot(yaws_server:wildcomp_salias("www.example.org", "??.example.org")),
    ?assertNot(yaws_server:wildcomp_salias("www.example.org", "www?example.org")),

    ?assert(yaws_server:wildcomp_salias("www.example.example.org", "www.*.example.???")),
    ?assert(yaws_server:wildcomp_salias("www.example.example.com", "www.*.example.???")),

    ?assertNot(yaws_server:wildcomp_salias("www.example.example.fr", "www.*.example.???")),
    ok.

ssl_renegotiation_config(_Config) ->
    case tlsv13_supported() of
        true ->
            {ok, _, [[SC]]} = yaws_api:getconf(),
            Ssl = yaws:sconf_ssl(SC),
            ?assertEqual(Ssl#ssl.protocol_version, ['tlsv1.3']),
            ?assertEqual(Ssl#ssl.secure_renegotiate, undefined),
            ?assertEqual(Ssl#ssl.client_renegotiation, undefined),
            ok;
        false ->
            {skip, "tlsv1.3 not available"}
    end.

%% =======================================================================
tlsv13_supported() ->
    case lists:keyfind(available, 1, ssl:versions()) of
        {available, Available} ->
            lists:member('tlsv1.3', Available);
        false ->
            false
    end.

prepare_docroots() ->
    WWW = filename:join(?tempdir(?MODULE), "www"),
    ok = testsuite:create_dir(WWW),
    ok.

get_sconf_attr(Name, SConf) ->
    Fun = list_to_atom("sconf_" ++ atom_to_list(Name)),
    {Name, yaws:Fun(SConf)}.


check_sc_flags(Flag, Id, SConf0) ->
    Flags0 = yaws:sconf_flags(SConf0),
    Val0   = (Flags0 band Id) /= 0,

    SConf1 = yaws:create_sconf(".", [{flags, [{Flag, not Val0}]}]),
    Flags1 = yaws:sconf_flags(SConf1),
    Flags2 = yaws:flag(Flags1, Id, Val0),

    {Flag,
     (not Val0 == ((Flags1 band Id) /= 0) andalso Flags2 == Flags0)}.
