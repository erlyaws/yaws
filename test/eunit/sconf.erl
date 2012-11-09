-module(sconf).
-compile(export_all).
-include("../../include/yaws.hrl").
-include_lib("eunit/include/eunit.hrl").


setup_default_sconf_test() ->
    Dir = filename:absname("."),

    SC1 = yaws_config:make_default_sconf(".", undefined),
    SC1 = yaws:create_sconf(".", []),
    {docroot, Dir}         = get_sconf_attr(docroot, SC1),
    {listen,  {127,0,0,1}} = get_sconf_attr(listen,  SC1),
    {port,    8000}        = get_sconf_attr(port,    SC1),


    SC2 = yaws_config:make_default_sconf(".", 8080),
    SC2 = yaws:create_sconf(".", [{port, 8080}]),
    {port, 8080} = get_sconf_attr(port,    SC2),

    ok.


set_sc_flags_test() ->
    SC = yaws:create_sconf(".", []),
    {access_log, true} =
        check_sc_flags(access_log, ?SC_ACCESS_LOG, SC),
    {auth_log, true} =
        check_sc_flags(auth_log, ?SC_AUTH_LOG, SC),
    {add_port, true} =
        check_sc_flags(add_port, ?SC_ADD_PORT, SC),
    {statistics, true} =
        check_sc_flags(statistics, ?SC_STATISTICS, SC),
    {tilde_expand, true} =
        check_sc_flags(tilde_expand, ?SC_TILDE_EXPAND, SC),
    {dir_listings, true} =
        check_sc_flags(dir_listings, ?SC_DIR_LISTINGS, SC),
    {deflate, true} =
        check_sc_flags(deflate, ?SC_DEFLATE, SC),
    {dir_all_zip, true} =
        check_sc_flags(dir_all_zip, ?SC_DIR_ALL_ZIP, SC),
    {dav, true} =
        check_sc_flags(dav, ?SC_DAV, SC),
    {fcgi_trace_protocol, true} =
        check_sc_flags(fcgi_trace_protocol, ?SC_FCGI_TRACE_PROTOCOL, SC),
    {fcgi_log_app_error, true} =
        check_sc_flags(fcgi_log_app_error, ?SC_FCGI_LOG_APP_ERROR, SC),
    {forward_proxy, true} =
        check_sc_flags(forward_proxy, ?SC_FORWARD_PROXY, SC),
    {auth_skip_docroot, true} =
        check_sc_flags(auth_skip_docroot, ?SC_AUTH_SKIP_DOCROOT, SC),
    ok.


setup_ssl_test() ->
    SC0 = yaws:create_sconf(".", []),
    undefined = yaws:sconf_ssl(SC0),

    SC1  = yaws:create_sconf(".", [{ssl, []}]),
    SSL1 = yaws:new_ssl(),
    SSL1 = yaws:sconf_ssl(SC1),

    SSL2 = yaws:ssl_keyfile(SSL1, "/tmp/yaws-key.pem"),
    SC2  = yaws:create_sconf(".", [{ssl, SSL2}]),
    "/tmp/yaws-key.pem" = yaws:ssl_keyfile(
                            yaws:sconf_ssl(SC2)
                           ),
    ok.


setup_authdirs_test() ->
    SC0 = yaws:create_sconf(".", []),
    []  = yaws:sconf_authdirs(SC0),

    SC1     = yaws:create_sconf(".", [{auth, []}]),
    Auth1   = yaws:new_auth(),
    [Auth1] = yaws:sconf_authdirs(SC1),

    Auth2 = yaws:auth_dir(Auth1, "/"),
    SC2   = yaws:create_sconf(".", [{auth, Auth2}]),
    "/"   = yaws:auth_dir(
              hd(yaws:sconf_authdirs(SC2))
             ),
    ok.


setup_deflate_test() ->
    SC0 = yaws:create_sconf(".", []),
    undefined = yaws:sconf_deflate_options(SC0),

    SC1    = yaws:create_sconf(".", [{deflate_options, []}]),
    DOpts1 = yaws:new_deflate(),
    DOpts1 = yaws:sconf_deflate_options(SC1),

    DOpts2 = yaws:deflate_compression_level(DOpts1, best_speed),
    SC2    = yaws:create_sconf(".", [{deflate_options, DOpts2}]),
    best_speed = yaws:deflate_compression_level(
                   yaws:sconf_deflate_options(SC2)
                  ),
    ok.


setup_mime_types_info_test() ->
    SC0 = yaws:create_sconf(".", []),
    undefined = yaws:sconf_mime_types_info(SC0),

    SC1 = yaws:create_sconf(".", [{mime_types_info, []}]),
    MI1 = yaws:new_mime_types_info(),
    MI1 = yaws:sconf_mime_types_info(SC1),

    MI2 = yaws:mime_types_info_mime_types_file(MI1, "/etc/mime.types"),
    SC2 = yaws:create_sconf(".", [{mime_types_info, MI2}]),
    "/etc/mime.types" = yaws:mime_types_info_mime_types_file(
                          yaws:sconf_mime_types_info(SC2)
                         ),
    ok.



%% =======================================================================
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
