%%%----------------------------------------------------------------------
%%% File    : yaws.erl
%%% Author  : Claes Wikstrom <klacke@bluetail.com>
%%% Purpose :
%%% Created : 16 Jan 2002 by Claes Wikstrom <klacke@bluetail.com>
%%%----------------------------------------------------------------------

-module(yaws).
-author('klacke@bluetail.com').

-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_appdeps.hrl").
-include("yaws_debug.hrl").

-include_lib("kernel/include/file.hrl").
-export([start/0, stop/0, hup/0, hup/1, restart/0, modules/0, load/0]).
-export([start_embedded/1, start_embedded/2, start_embedded/3, start_embedded/4,
         add_server/2, create_gconf/2, create_sconf/2, setup_sconf/2]).

-export([gconf_yaws_dir/1, gconf_trace/1, gconf_flags/1, gconf_logdir/1,
         gconf_ebin_dir/1, gconf_src_dir/1, gconf_runmods/1,
         gconf_keepalive_timeout/1, gconf_keepalive_maxuses/1,
         gconf_max_num_cached_files/1, gconf_max_num_cached_bytes/1,
         gconf_max_size_cached_file/1, gconf_max_connections/1,
         gconf_process_options/1, gconf_large_file_chunk_size/1,
         gconf_mnesia_dir/1, gconf_log_wrap_size/1, gconf_cache_refresh_secs/1,
         gconf_include_dir/1, gconf_phpexe/1, gconf_yaws/1, gconf_id/1,
         gconf_enable_soap/1, gconf_soap_srv_mods/1, gconf_ysession_mod/1,
         gconf_acceptor_pool_size/1, gconf_mime_types_info/1,
         gconf_nslookup_pref/1,
         gconf_ysession_idle_timeout/1, gconf_ysession_long_timeout/1,
         gconf_sni/1]).

-export([gconf_yaws_dir/2, gconf_trace/2, gconf_flags/2, gconf_logdir/2,
         gconf_ebin_dir/2, gconf_src_dir/2, gconf_runmods/2,
         gconf_keepalive_timeout/2, gconf_keepalive_maxuses/2,
         gconf_max_num_cached_files/2, gconf_max_num_cached_bytes/2,
         gconf_max_size_cached_file/2, gconf_max_connections/2,
         gconf_process_options/2, gconf_large_file_chunk_size/2,
         gconf_mnesia_dir/2, gconf_log_wrap_size/2, gconf_cache_refresh_secs/2,
         gconf_include_dir/2, gconf_phpexe/2, gconf_yaws/2, gconf_id/2,
         gconf_enable_soap/2, gconf_soap_srv_mods/2, gconf_ysession_mod/2,
         gconf_acceptor_pool_size/2, gconf_mime_types_info/2,
         gconf_nslookup_pref/2,
         gconf_ysession_idle_timeout/2, gconf_ysession_long_timeout/2,
         gconf_sni/2]).

-export([sconf_port/1, sconf_flags/1, sconf_redirect_map/1, sconf_rhost/1,
         sconf_rmethod/1, sconf_docroot/1, sconf_xtra_docroots/1,
         sconf_listen/1, sconf_servername/1, sconf_serveralias/1, sconf_yaws/1,
         sconf_ets/1, sconf_ssl/1, sconf_authdirs/1, sconf_partial_post_size/1,
         sconf_appmods/1, sconf_expires/1, sconf_errormod_401/1,
         sconf_errormod_404/1, sconf_arg_rewrite_mode/1, sconf_logger_mod/1,
         sconf_opaque/1, sconf_start_mod/1, sconf_allowed_scripts/1,
         sconf_tilde_allowed_scripts/1, sconf_index_files/1, sconf_revproxy/1,
         sconf_spotions/1, sconf_extra_cgi_vars/1, sconf_stats/1,
         sconf_fcgi_app_server/1, sconf_php_handler/1, sconf_shaper/1,
         sconf_deflate_options/1, sconf_mime_types_info/1,
         sconf_dispatch_mod/1]).

-export([sconf_port/2, sconf_flags/2, sconf_redirect_map/2, sconf_rhost/2,
         sconf_rmethod/2, sconf_docroot/2, sconf_xtra_docroots/2,
         sconf_listen/2, sconf_servername/2, sconf_serveralias/2, sconf_yaws/2,
         sconf_ets/2, sconf_ssl/2, sconf_authdirs/2, sconf_partial_post_size/2,
         sconf_appmods/2, sconf_expires/2, sconf_errormod_401/2,
         sconf_errormod_404/2, sconf_arg_rewrite_mode/2, sconf_logger_mod/2,
         sconf_opaque/2, sconf_start_mod/2, sconf_allowed_scripts/2,
         sconf_tilde_allowed_scripts/2, sconf_index_files/2, sconf_revproxy/2,
         sconf_spotions/2, sconf_extra_cgi_vars/2, sconf_stats/2,
         sconf_fcgi_app_server/2, sconf_php_handler/2, sconf_shaper/2,
         sconf_deflate_options/2, sconf_mime_types_info/2,
         sconf_dispatch_mod/2]).

-export([new_auth/0,
         auth_dir/1, auth_dir/2,
         auth_docroot/1, auth_docroot/2,
         auth_files/1, auth_files/2,
         auth_realm/1, auth_realm/2,
         auth_type/1, auth_type/2,
         auth_headers/1, auth_headers/2,
         auth_users/1, auth_users/2,
         auth_acl/1, auth_acl/2,
         auth_mod/1, auth_mod/2,
         auth_outmod/1, auth_outmod/2,
         auth_pam/1, auth_pam/2]).

-export([new_ssl/0,
         ssl_keyfile/1, ssl_keyfile/2,
         ssl_certfile/1, ssl_certfile/2,
         ssl_verify/1, ssl_verify/2,
         ssl_fail_if_no_peer_cert/1, ssl_fail_if_no_peer_cert/2,
         ssl_depth/1, ssl_depth/2,
         ssl_password/1, ssl_password/2,
         ssl_cacertfile/1, ssl_cacertfile/2,
         ssl_dhfile/1, ssl_dhfile/2,
         ssl_ciphers/1, ssl_ciphers/2,
         ssl_cachetimeout/1, ssl_cachetimeout/2,
         ssl_secure_renegotiate/1, ssl_secure_renegotiate/2,
         ssl_client_renegotiation/1, ssl_client_renegotiation/2,
         ssl_protocol_version/1, ssl_protocol_version/2,
         ssl_honor_cipher_order/1, ssl_honor_cipher_order/2,
         ssl_require_sni/1, ssl_require_sni/2]).

-export([new_deflate/0,
         deflate_min_compress_size/1, deflate_min_compress_size/2,
         deflate_compression_level/1, deflate_compression_level/2,
         deflate_window_size/1, deflate_window_size/2,
         deflate_mem_level/1, deflate_mem_level/2,
         deflate_strategy/1, deflate_strategy/2,
         deflate_use_gzip_static/1, deflate_use_gzip_static/2,
         deflate_mime_types/1, deflate_mime_types/2]).

-export([new_mime_types_info/0,
         mime_types_info_mime_types_file/1, mime_types_info_mime_types_file/2,
         mime_types_info_types/1, mime_types_info_types/2,
         mime_types_info_charsets/1, mime_types_info_charsets/2,
         mime_types_info_default_type/1, mime_types_info_default_type/2,
         mime_types_info_default_charset/1, mime_types_info_default_charset/2]).

-export([first/2, elog/2, filesize/1, upto/2, to_string/1, to_list/1,
         integer_to_hex/1, hex_to_integer/1, string_to_hex/1, hex_to_string/1,
         is_modified_p/2, flag/3, dohup/1, is_ssl/1, address/0, is_space/1,
         setopts/3, eat_crnl/2, get_chunk_num/2, get_chunk_header/2,
         get_chunk/4, get_chunk_trailer/2, list_to_uue/1, uue_to_list/1,
         printversion/0, strip_spaces/1, strip_spaces/2,
         month/1, mk2/1, home/0, arg_rewrite/1, to_lowerchar/1, to_lower/1,
         funreverse/2, is_prefix/2, split_sep/2, join_sep/2, accepts_gzip/2,
         upto_char/2, deepmap/2, ticker/2, ticker/3, unique_triple/0, get_time_tuple/0,
         parse_qvalue/1, parse_auth/1]).

-export([outh_set_status_code/1,
         outh_set_non_cacheable/1,
         outh_set_content_type/1,
         outh_set_content_encoding/1,
         outh_set_cookie/1,
         outh_set_static_headers/3, outh_set_static_headers/4,
         outh_set_304_headers/3,
         outh_set_dyn_headers/3,
         outh_set_connection/1,
         outh_set_content_length/1,
         outh_set_dcc/2,
         outh_set_transfer_encoding_off/0,
         outh_set_auth/1,
         outh_set_vary/1,
         outh_clear_headers/0,
         outh_fix_doclose/0,
         dcc/2]).

-export([make_allow_header/0, make_allow_header/1,
         make_server_header/0,
         make_last_modified_header/1,
         make_location_header/1,
         make_etag_header/1,
         make_content_range_header/1,
         make_content_length_header/1,
         make_content_encoding_header/1,
         make_connection_close_header/1,
         make_transfer_encoding_chunked_header/1,
         make_www_authenticate_header/1,
         make_etag/1,
         make_content_type_header/1,
         make_date_header/0,
         make_vary_header/1]).

-export([outh_get_status_code/0,
         outh_get_contlen/0,
         outh_get_act_contlen/0,
         outh_inc_act_contlen/1,
         outh_get_doclose/0,
         outh_get_chunked/0,
         outh_get_content_encoding/0,
         outh_get_content_encoding_header/0,
         outh_get_content_type/0,
         outh_get_vary_fields/0,
         outh_serialize/0]).

-export([accumulate_header/1, headers_to_str/1,
         getuid/0,
         user_to_home/1,
         uid_to_name/1,
         exists/1,
         mkdir/1]).

-export([tcp_connect/3, tcp_connect/4, ssl_connect/3, ssl_connect/4]).

-export([do_recv/3, do_recv/4, cli_recv/3,
         gen_tcp_send/2,
         http_get_headers/2]).

-export([sconf_to_srvstr/1,
         redirect_host/2, redirect_port/1,
         redirect_scheme_port/1, redirect_scheme/1,
         tmpdir/0, tmpdir/1, mktemp/1, split_at/2, insert_at/3,
         id_dir/1, ctl_file/1]).

-export([parse_ipmask/1, match_ipmask/2, find_private_port/0]).

-export([get_app_dir/0, get_ebin_dir/0, get_priv_dir/0,
         get_inc_dir/0]).

%% Internal
-export([local_time_as_gmt_string/1, universal_time_as_string/1,
         stringdate_to_datetime/1]).

start() ->
    ok = start_app_deps(),
    application:start(yaws, permanent).

stop() ->
    application:stop(yaws).


%%% Quick and easy way of starting Yaws in embedded mode.  No need for any
%%% start-script switches and no dependencies to Yaws header files. Just call
%%% start_embedded/N and you are in the air.
start_embedded(DocRoot) ->
    start_embedded(DocRoot, []).

start_embedded(DocRoot, SL) when is_list(DocRoot),is_list(SL) ->
    start_embedded(DocRoot, SL, []).

start_embedded(DocRoot, SL, GL) when is_list(DocRoot),is_list(SL),is_list(GL) ->
    start_embedded(DocRoot, SL, GL, "default").
start_embedded(DocRoot, SL, GL, Id)
  when is_list(DocRoot), is_list(SL), is_list(GL) ->
    ok = start_app_deps(),
    {ok, SCList, GC, _} = yaws_api:embedded_start_conf(DocRoot, SL, GL, Id),
    ok = application:start(yaws, permanent),
    yaws_config:add_yaws_soap_srv(GC),
    yaws_api:setconf(GC, SCList),
    ok.

add_server(DocRoot, SL) when is_list(DocRoot),is_list(SL) ->
    SC  = create_sconf(DocRoot, SL),
    %% Change #auth in authdirs to {Dir, #auth} if needed
    Fun = fun
              (A = #auth{dir = [Dir]}, Acc) -> [{Dir, A}| Acc];
              (A, Acc)                      -> [A| Acc]
          end,
    Authdirs = lists:foldr(Fun, [], SC#sconf.authdirs),
    SC1 = yaws_config:add_yaws_auth(SC#sconf{authdirs = Authdirs}),
    yaws_config:add_sconf(SC1).

create_gconf(GL, Id) when is_list(GL) ->
    Debug = case application:get_env(yaws, debug) of
                undefined -> false;
                {ok, D}   -> D
            end,
    setup_gconf(GL, yaws_config:make_default_gconf(Debug, Id)).

create_sconf(DocRoot, SL) when is_list(DocRoot), is_list(SL) ->
    SC = yaws_config:make_default_sconf(DocRoot,
                                        lkup(servername, SL, undefined),
                                        lkup(port, SL, undefined)),
    SL1 = lists:keydelete(port, 1, lists:keydelete(servername, 1, SL)),
    setup_sconf(SL1, SC).

start_app_deps() ->
    Deps = split_sep(?YAWS_APPDEPS, $,),
    catch lists:foldl(fun(App0, Acc) ->
                              App = list_to_existing_atom(App0),
                              case application:start(App, permanent) of
                                  ok -> Acc;
                                  {error,{already_started,App}} -> Acc;
                                  Else -> throw(Else)
                              end
                      end, ok, Deps).

%%% Access functions for the GCONF and SCONF records.
%% Getters
gconf_yaws_dir             (#gconf{yaws_dir              = X}) -> X.
gconf_trace                (#gconf{trace                 = X}) -> X.
gconf_flags                (#gconf{flags                 = X}) -> X.
gconf_logdir               (#gconf{logdir                = X}) -> X.
gconf_ebin_dir             (#gconf{ebin_dir              = X}) -> X.
gconf_src_dir              (#gconf{src_dir               = X}) -> X.
gconf_runmods              (#gconf{runmods               = X}) -> X.
gconf_keepalive_timeout    (#gconf{keepalive_timeout     = X}) -> X.
gconf_keepalive_maxuses    (#gconf{keepalive_maxuses     = X}) -> X.
gconf_max_num_cached_files (#gconf{max_num_cached_files  = X}) -> X.
gconf_max_num_cached_bytes (#gconf{max_num_cached_bytes  = X}) -> X.
gconf_max_size_cached_file (#gconf{max_size_cached_file  = X}) -> X.
gconf_max_connections      (#gconf{max_connections       = X}) -> X.
gconf_process_options      (#gconf{process_options       = X}) -> X.
gconf_large_file_chunk_size(#gconf{large_file_chunk_size = X}) -> X.
gconf_mnesia_dir           (#gconf{mnesia_dir            = X}) -> X.
gconf_log_wrap_size        (#gconf{log_wrap_size         = X}) -> X.
gconf_cache_refresh_secs   (#gconf{cache_refresh_secs    = X}) -> X.
gconf_include_dir          (#gconf{include_dir           = X}) -> X.
gconf_phpexe               (#gconf{phpexe                = X}) -> X.
gconf_yaws                 (#gconf{yaws                  = X}) -> X.
gconf_id                   (#gconf{id                    = X}) -> X.
gconf_enable_soap          (#gconf{enable_soap           = X}) -> X.
gconf_soap_srv_mods        (#gconf{soap_srv_mods         = X}) -> X.
gconf_ysession_mod         (#gconf{ysession_mod          = X}) -> X.
gconf_acceptor_pool_size   (#gconf{acceptor_pool_size    = X}) -> X.
gconf_mime_types_info      (#gconf{mime_types_info       = X}) -> X.
gconf_nslookup_pref        (#gconf{nslookup_pref         = X}) -> X.
gconf_ysession_idle_timeout(#gconf{ysession_idle_timeout = X}) -> X.
gconf_ysession_long_timeout(#gconf{ysession_long_timeout = X}) -> X.
gconf_sni                  (#gconf{sni                   = X}) -> X.

%% Setters
gconf_yaws_dir             (S, X) -> S#gconf{yaws_dir              = X}.
gconf_trace                (S, X) -> S#gconf{trace                 = X}.
gconf_flags                (S, X) -> S#gconf{flags                 = X}.
gconf_logdir               (S, X) -> S#gconf{logdir                = X}.
gconf_ebin_dir             (S, X) -> S#gconf{ebin_dir              = X}.
gconf_src_dir              (S, X) -> S#gconf{src_dir               = X}.
gconf_runmods              (S, X) -> S#gconf{runmods               = X}.
gconf_keepalive_timeout    (S, X) -> S#gconf{keepalive_timeout     = X}.
gconf_keepalive_maxuses    (S, X) -> S#gconf{keepalive_maxuses     = X}.
gconf_max_num_cached_files (S, X) -> S#gconf{max_num_cached_files  = X}.
gconf_max_num_cached_bytes (S, X) -> S#gconf{max_num_cached_bytes  = X}.
gconf_max_size_cached_file (S, X) -> S#gconf{max_size_cached_file  = X}.
gconf_max_connections      (S, X) -> S#gconf{max_connections       = X}.
gconf_process_options      (S, X) -> S#gconf{process_options       = X}.
gconf_large_file_chunk_size(S, X) -> S#gconf{large_file_chunk_size = X}.
gconf_mnesia_dir           (S, X) -> S#gconf{mnesia_dir            = X}.
gconf_log_wrap_size        (S, X) -> S#gconf{log_wrap_size         = X}.
gconf_cache_refresh_secs   (S, X) -> S#gconf{cache_refresh_secs    = X}.
gconf_include_dir          (S, X) -> S#gconf{include_dir           = X}.
gconf_phpexe               (S, X) -> S#gconf{phpexe                = X}.
gconf_yaws                 (S, X) -> S#gconf{yaws                  = X}.
gconf_id                   (S, X) -> S#gconf{id                    = X}.
gconf_enable_soap          (S, X) -> S#gconf{enable_soap           = X}.
gconf_soap_srv_mods        (S, X) -> S#gconf{soap_srv_mods         = X}.
gconf_ysession_mod         (S, X) -> S#gconf{ysession_mod          = X}.
gconf_acceptor_pool_size   (S, X) -> S#gconf{acceptor_pool_size    = X}.
gconf_mime_types_info      (S, X) -> S#gconf{mime_types_info       = X}.
gconf_nslookup_pref        (S, X) -> S#gconf{nslookup_pref         = X}.
gconf_ysession_idle_timeout(S, X) -> S#gconf{ysession_idle_timeout = X}.
gconf_ysession_long_timeout(S, X) -> S#gconf{ysession_long_timeout = X}.
gconf_sni                  (S, X) -> S#gconf{sni                   = X}.

%% Getters
sconf_port                 (#sconf{port                  = X}) -> X.
sconf_flags                (#sconf{flags                 = X}) -> X.
sconf_redirect_map         (#sconf{redirect_map          = X}) -> X.
sconf_rhost                (#sconf{rhost                 = X}) -> X.
sconf_rmethod              (#sconf{rmethod               = X}) -> X.
sconf_docroot              (#sconf{docroot               = X}) -> X.
sconf_xtra_docroots        (#sconf{xtra_docroots         = X}) -> X.
sconf_listen               (#sconf{listen                = X}) -> X.
sconf_servername           (#sconf{servername            = X}) -> X.
sconf_serveralias          (#sconf{serveralias           = X}) -> X.
sconf_yaws                 (#sconf{yaws                  = X}) -> X.
sconf_ets                  (#sconf{ets                   = X}) -> X.
sconf_ssl                  (#sconf{ssl                   = X}) -> X.
sconf_authdirs             (#sconf{authdirs              = X}) -> X.
sconf_partial_post_size    (#sconf{partial_post_size     = X}) -> X.
sconf_appmods              (#sconf{appmods               = X}) -> X.
sconf_expires              (#sconf{expires               = X}) -> X.
sconf_errormod_401         (#sconf{errormod_401          = X}) -> X.
sconf_errormod_404         (#sconf{errormod_404          = X}) -> X.
sconf_arg_rewrite_mode     (#sconf{arg_rewrite_mod       = X}) -> X.
sconf_logger_mod           (#sconf{logger_mod            = X}) -> X.
sconf_opaque               (#sconf{opaque                = X}) -> X.
sconf_start_mod            (#sconf{start_mod             = X}) -> X.
sconf_allowed_scripts      (#sconf{allowed_scripts       = X}) -> X.
sconf_tilde_allowed_scripts(#sconf{tilde_allowed_scripts = X}) -> X.
sconf_index_files          (#sconf{index_files           = X}) -> X.
sconf_revproxy             (#sconf{revproxy              = X}) -> X.
sconf_spotions             (#sconf{soptions              = X}) -> X.
sconf_extra_cgi_vars       (#sconf{extra_cgi_vars        = X}) -> X.
sconf_stats                (#sconf{stats                 = X}) -> X.
sconf_fcgi_app_server      (#sconf{fcgi_app_server       = X}) -> X.
sconf_php_handler          (#sconf{php_handler           = X}) -> X.
sconf_shaper               (#sconf{shaper                = X}) -> X.
sconf_deflate_options      (#sconf{deflate_options       = X}) -> X.
sconf_mime_types_info      (#sconf{mime_types_info       = X}) -> X.
sconf_dispatch_mod         (#sconf{dispatch_mod          = X}) -> X.

%% Setters
sconf_port                 (S, X) -> S#sconf{port                  = X}.
sconf_flags                (S, X) -> S#sconf{flags                 = X}.
sconf_redirect_map         (S, X) -> S#sconf{redirect_map          = X}.
sconf_rhost                (S, X) -> S#sconf{rhost                 = X}.
sconf_rmethod              (S, X) -> S#sconf{rmethod               = X}.
sconf_docroot              (S, X) -> S#sconf{docroot               = X}.
sconf_xtra_docroots        (S, X) -> S#sconf{xtra_docroots         = X}.
sconf_listen               (S, X) -> S#sconf{listen                = X}.
sconf_servername           (S, X) -> S#sconf{servername            = X}.
sconf_serveralias          (S, X) -> S#sconf{serveralias           = X}.
sconf_yaws                 (S, X) -> S#sconf{yaws                  = X}.
sconf_ets                  (S, X) -> S#sconf{ets                   = X}.
sconf_ssl                  (S, X) -> S#sconf{ssl                   = X}.
sconf_authdirs             (S, X) -> S#sconf{authdirs              = X}.
sconf_partial_post_size    (S, X) -> S#sconf{partial_post_size     = X}.
sconf_appmods              (S, X) -> S#sconf{appmods               = X}.
sconf_expires              (S, X) -> S#sconf{expires               = X}.
sconf_errormod_401         (S, X) -> S#sconf{errormod_401          = X}.
sconf_errormod_404         (S, X) -> S#sconf{errormod_404          = X}.
sconf_arg_rewrite_mode     (S, X) -> S#sconf{arg_rewrite_mod       = X}.
sconf_logger_mod           (S, X) -> S#sconf{logger_mod            = X}.
sconf_opaque               (S, X) -> S#sconf{opaque                = X}.
sconf_start_mod            (S, X) -> S#sconf{start_mod             = X}.
sconf_allowed_scripts      (S, X) -> S#sconf{allowed_scripts       = X}.
sconf_tilde_allowed_scripts(S, X) -> S#sconf{tilde_allowed_scripts = X}.
sconf_index_files          (S, X) -> S#sconf{index_files           = X}.
sconf_revproxy             (S, X) -> S#sconf{revproxy              = X}.
sconf_spotions             (S, X) -> S#sconf{soptions              = X}.
sconf_extra_cgi_vars       (S, X) -> S#sconf{extra_cgi_vars        = X}.
sconf_stats                (S, X) -> S#sconf{stats                 = X}.
sconf_fcgi_app_server      (S, X) -> S#sconf{fcgi_app_server       = X}.
sconf_php_handler          (S, X) -> S#sconf{php_handler           = X}.
sconf_shaper               (S, X) -> S#sconf{shaper                = X}.
sconf_deflate_options      (S, X) -> S#sconf{deflate_options       = X}.
sconf_mime_types_info      (S, X) -> S#sconf{mime_types_info       = X}.
sconf_dispatch_mod         (S, X) -> S#sconf{dispatch_mod          = X}.


%% Access functions for the AUTH record.
new_auth() -> #auth{}.

auth_dir    (#auth{dir     = X}) -> X.
auth_docroot(#auth{docroot = X}) -> X.
auth_files  (#auth{files   = X}) -> X.
auth_realm  (#auth{realm   = X}) -> X.
auth_type   (#auth{type    = X}) -> X.
auth_headers(#auth{headers = X}) -> X.
auth_users  (#auth{users   = X}) -> X.
auth_acl    (#auth{acl     = X}) -> X.
auth_mod    (#auth{mod     = X}) -> X.
auth_outmod (#auth{outmod  = X}) -> X.
auth_pam    (#auth{pam     = X}) -> X.

auth_dir    (A, Dir)     -> A#auth{dir     = Dir}.
auth_docroot(A, DocRoot) -> A#auth{docroot = DocRoot}.
auth_files  (A, Files)   -> A#auth{files   = Files}.
auth_realm  (A, Realm)   -> A#auth{realm   = Realm}.
auth_type   (A, Type)    -> A#auth{type    = Type}.
auth_headers(A, Headers) -> A#auth{headers = Headers}.
auth_users  (A, Users)   -> A#auth{users   = Users}.
auth_acl    (A, Acl)     -> A#auth{acl     = Acl}.
auth_mod    (A, Mod)     -> A#auth{mod     = Mod}.
auth_outmod (A, Outmod)  -> A#auth{outmod  = Outmod}.
auth_pam    (A, Pam)     -> A#auth{pam     = Pam}.


setup_authdirs(SL, DefaultAuthDirs) ->
    case [A || {auth, A} <- SL] of
        [] -> DefaultAuthDirs;
        As -> [setup_auth(A) || A <- As]
    end.

setup_auth(#auth{}=Auth) ->
    Auth;
setup_auth(AuthProps) ->
    Auth = #auth{},
    #auth{dir     = lkup(dir,     AuthProps, Auth#auth.dir),
          docroot = lkup(docroot, AuthProps, Auth#auth.docroot),
          files   = lkup(files,   AuthProps, Auth#auth.files),
          realm   = lkup(realm,   AuthProps, Auth#auth.realm),
          type    = lkup(type,    AuthProps, Auth#auth.type),
          headers = lkup(headers, AuthProps, Auth#auth.headers),
          users   = lkup(users,   AuthProps, Auth#auth.users),
          acl     = lkup(acl,     AuthProps, Auth#auth.acl),
          mod     = lkup(mod,     AuthProps, Auth#auth.mod),
          outmod  = lkup(outmod,  AuthProps, Auth#auth.outmod),
          pam     = lkup(pam,     AuthProps, Auth#auth.pam)}.


%% Access functions for the SSL record.
new_ssl() -> #ssl{}.

ssl_keyfile             (#ssl{keyfile              = X}) -> X.
ssl_certfile            (#ssl{certfile             = X}) -> X.
ssl_verify              (#ssl{verify               = X}) -> X.
ssl_fail_if_no_peer_cert(#ssl{fail_if_no_peer_cert = X}) -> X.
ssl_depth               (#ssl{depth                = X}) -> X.
ssl_password            (#ssl{password             = X}) -> X.
ssl_cacertfile          (#ssl{cacertfile           = X}) -> X.
ssl_dhfile              (#ssl{dhfile               = X}) -> X.
ssl_ciphers             (#ssl{ciphers              = X}) -> X.
ssl_cachetimeout        (#ssl{cachetimeout         = X}) -> X.
ssl_secure_renegotiate  (#ssl{secure_renegotiate   = X}) -> X.
ssl_client_renegotiation(#ssl{client_renegotiation = X}) -> X.
ssl_protocol_version    (#ssl{protocol_version     = X}) -> X.
ssl_honor_cipher_order  (#ssl{honor_cipher_order   = X}) -> X.
ssl_require_sni         (#ssl{require_sni          = X}) -> X.

ssl_keyfile             (S, File)    -> S#ssl{keyfile              = File}.
ssl_certfile            (S, File)    -> S#ssl{certfile             = File}.
ssl_verify              (S, Verify)  -> S#ssl{verify               = Verify}.
ssl_fail_if_no_peer_cert(S, Bool)    -> S#ssl{fail_if_no_peer_cert = Bool}.
ssl_depth               (S, Depth)   -> S#ssl{depth                = Depth}.
ssl_password            (S, Pass)    -> S#ssl{password             = Pass}.
ssl_cacertfile          (S, File)    -> S#ssl{cacertfile           = File}.
ssl_dhfile              (S, File)    -> S#ssl{dhfile               = File}.
ssl_ciphers             (S, Ciphers) -> S#ssl{ciphers              = Ciphers}.
ssl_cachetimeout        (S, Timeout) -> S#ssl{cachetimeout         = Timeout}.
ssl_secure_renegotiate  (S, Bool)    -> S#ssl{secure_renegotiate   = Bool}.
ssl_protocol_version    (S, Vsns)    -> S#ssl{protocol_version     = Vsns}.
ssl_require_sni         (S, Bool)    -> S#ssl{require_sni          = Bool}.
ssl_honor_cipher_order  (S, Bool) ->
    case yaws_dynopts:have_ssl_honor_cipher_order() of
        true  -> S#ssl{honor_cipher_order   = Bool};
        false -> S
    end.
ssl_client_renegotiation(S, Bool) ->
    case yaws_dynopts:have_ssl_client_renegotiation() of
        true  -> S#ssl{client_renegotiation = Bool};
        false -> S
    end.

setup_ssl(SL, DefaultSSL) ->
    case lkup(ssl, SL, undefined) of
        undefined ->
            DefaultSSL;
        SSL when is_record(SSL, ssl) ->
            SSL;
        SSLProps when is_list(SSLProps) ->
            SSL = #ssl{},
            #ssl{keyfile              = lkup(keyfile, SSLProps,
                                             SSL#ssl.keyfile),
                 certfile             = lkup(certfile, SSLProps,
                                             SSL#ssl.certfile),
                 verify               = lkup(verify, SSLProps, SSL#ssl.verify),
                 fail_if_no_peer_cert = lkup(fail_if_no_peer_cert, SSLProps,
                                             SSL#ssl.fail_if_no_peer_cert),
                 depth                = lkup(depth, SSLProps, SSL#ssl.depth),
                 password             = lkup(password, SSLProps,
                                             SSL#ssl.password),
                 cacertfile           = lkup(cacertfile, SSLProps,
                                             SSL#ssl.cacertfile),
                 dhfile               = lkup(dhfile, SSLProps,
                                             SSL#ssl.dhfile),
                 ciphers              = lkup(ciphers, SSLProps,
                                             SSL#ssl.ciphers),
                 cachetimeout         = lkup(cachetimeout, SSLProps,
                                             SSL#ssl.cachetimeout),
                 secure_renegotiate   = lkup(secure_renegotiate, SSLProps,
                                             SSL#ssl.secure_renegotiate),
                 client_renegotiation = lkup(client_renegotiation, SSLProps,
                                             SSL#ssl.client_renegotiation),
                 honor_cipher_order   = lkup(honor_cipher_order, SSLProps,
                                             SSL#ssl.honor_cipher_order),
                 protocol_version     = lkup(protocol_version, SSLProps,
                                             undefined),
                 require_sni          = lkup(require_sni, SSLProps,
                                             SSL#ssl.require_sni)}
    end.


%% Access functions for the DEFLATE record.
new_deflate() -> #deflate{}.

deflate_min_compress_size(#deflate{min_compress_size = X}) -> X.
deflate_compression_level(#deflate{compression_level = X}) -> X.
deflate_window_size      (#deflate{window_size       = X}) -> X.
deflate_mem_level        (#deflate{mem_level         = X}) -> X.
deflate_strategy         (#deflate{strategy          = X}) -> X.
deflate_use_gzip_static  (#deflate{use_gzip_static   = X}) -> X.
deflate_mime_types       (#deflate{mime_types        = X}) -> X.

deflate_min_compress_size(D, Min)   -> D#deflate{min_compress_size = Min}.
deflate_compression_level(D, Level) -> D#deflate{compression_level = Level}.
deflate_window_size      (D, Size)  -> D#deflate{window_size       = Size}.
deflate_mem_level        (D, Level) -> D#deflate{mem_level         = Level}.
deflate_strategy         (D, Strat) -> D#deflate{strategy          = Strat}.
deflate_use_gzip_static  (D, Bool)  -> D#deflate{use_gzip_static   = Bool}.
deflate_mime_types       (D, Types) -> D#deflate{mime_types        = Types}.


setup_deflate(SL, DefaultDeflate) ->
    case lkup(deflate_options, SL, undefined) of
        undefined ->
            DefaultDeflate;
        D when is_record(D, deflate) ->
            D;
        DProps when is_list(DProps) ->
            D = #deflate{},
            #deflate{min_compress_size = lkup(min_compress_size, DProps,
                                              D#deflate.min_compress_size),
                     compression_level = lkup(compression_level, DProps,
                                              D#deflate.compression_level),
                     window_size       = lkup(window_size, DProps,
                                              D#deflate.window_size),
                     mem_level         = lkup(mem_level, DProps,
                                              D#deflate.mem_level),
                     strategy          = lkup(strategy, DProps,
                                              D#deflate.strategy),
                     use_gzip_static   = lkup(use_gzip_static, DProps,
                                              D#deflate.use_gzip_static),
                     mime_types        = lkup(mime_types, DProps,
                                              D#deflate.mime_types)}
    end.

%% Access functions to MIME_TYPES_INFO record.
new_mime_types_info() -> #mime_types_info{}.

mime_types_info_mime_types_file(#mime_types_info{mime_types_file = X}) -> X.
mime_types_info_types          (#mime_types_info{types           = X}) -> X.
mime_types_info_charsets       (#mime_types_info{charsets        = X}) -> X.
mime_types_info_default_type   (#mime_types_info{default_type    = X}) -> X.
mime_types_info_default_charset(#mime_types_info{default_charset = X}) -> X.

mime_types_info_mime_types_file(M, File) ->
    M#mime_types_info{mime_types_file = File}.
mime_types_info_types(M, Types) ->
    M#mime_types_info{types = Types}.
mime_types_info_charsets(M, Charsets) ->
    M#mime_types_info{charsets = Charsets}.
mime_types_info_default_type(M, Type) ->
    M#mime_types_info{default_type = Type}.
mime_types_info_default_charset(M, Charset) ->
    M#mime_types_info{default_charset = Charset}.


setup_mime_types_info(SL, DefaultMTI) ->
    case lkup(mime_types_info, SL, undefined) of
        undefined ->
            DefaultMTI;
        M when is_record(M, mime_types_info) ->
            M;
        MProps when is_list(MProps) ->
            M = #mime_types_info{},
            #mime_types_info{mime_types_file =
                                 lkup(mime_types_file, MProps,
                                      M#mime_types_info.mime_types_file),
                             types           = lkup(types, MProps,
                                                    M#mime_types_info.types),
                             charsets        = lkup(charsets, MProps,
                                                    M#mime_types_info.charsets),
                             default_type    =
                                 lkup(default_type, MProps,
                                      M#mime_types_info.default_type),
                             default_charset =
                                 lkup(default_charset, MProps,
                                      M#mime_types_info.default_charset)}
    end.


%% Setup global configuration
setup_gconf([], GC) -> GC;
setup_gconf(GL, GC) ->
    #gconf{yaws_dir              = lkup(yaws_dir, GL, GC#gconf.yaws_dir),
           trace                 = lkup(trace, GL, GC#gconf.trace),
           flags                 = set_gc_flags(lkup(flags, GL, []),
                                                GC#gconf.flags),
           logdir                = lkup(logdir, GL, GC#gconf.logdir),
           ebin_dir              = lkup(ebin_dir, GL, GC#gconf.ebin_dir),
           src_dir               = lkup(src_dir, GL, GC#gconf.src_dir),
           runmods               = lkup(runmods, GL, GC#gconf.runmods),
           keepalive_timeout     = lkup(keepalive_timeout, GL,
                                        GC#gconf.keepalive_timeout),
           keepalive_maxuses     = lkup(keepalive_maxuses, GL,
                                        GC#gconf.keepalive_maxuses),
           max_num_cached_files  = lkup(max_num_cached_files, GL,
                                        GC#gconf.max_num_cached_files),
           max_num_cached_bytes  = lkup(max_num_cached_bytes, GL,
                                        GC#gconf.max_num_cached_bytes),
           max_size_cached_file  = lkup(max_size_cached_file, GL,
                                        GC#gconf.max_size_cached_file),
           max_connections       = lkup(max_connections, GL,
                                        GC#gconf.max_connections),
           process_options       = lkup(process_options, GL,
                                        GC#gconf.process_options),
           large_file_chunk_size = lkup(large_file_chunk_size, GL,
                                        GC#gconf.large_file_chunk_size),
           mnesia_dir            = lkup(mnesia_dir, GL, GC#gconf.mnesia_dir),
           log_wrap_size         = lkup(log_wrap_size, GL,
                                        GC#gconf.log_wrap_size),
           cache_refresh_secs    = lkup(cache_refresh_secs, GL,
                                        GC#gconf.cache_refresh_secs),
           include_dir           = lkup(include_dir, GL, GC#gconf.include_dir),
           phpexe                = lkup(phpexe, GL, GC#gconf.phpexe),
           yaws                  = lkup(yaws, GL, GC#gconf.yaws),
           id                    = lkup(id, GL, GC#gconf.id),
           enable_soap           = lkup(enable_soap, GL, GC#gconf.enable_soap),
           soap_srv_mods         = lkup(soap_srv_mods, GL,
                                        GC#gconf.soap_srv_mods),
           ysession_mod          = lkup(ysession_mod, GL,
                                        GC#gconf.ysession_mod),
           acceptor_pool_size    = lkup(acceptor_pool_size, GL,
                                        GC#gconf.acceptor_pool_size),
           mime_types_info       = setup_mime_types_info(
                                     GL, GC#gconf.mime_types_info
                                    ),
           nslookup_pref         = lkup(nslookup_pref, GL,
                                        GC#gconf.nslookup_pref),
           sni                   = lkup(sni, GL, GC#gconf.sni)
          }.

set_gc_flags([{tty_trace, Bool}|T], Flags) ->
    set_gc_flags(T, flag(Flags,?GC_TTY_TRACE, Bool));
set_gc_flags([{debug, Bool}|T], Flags) ->
    set_gc_flags(T, flag(Flags, ?GC_DEBUG, Bool));
set_gc_flags([{copy_errlog, Bool}|T], Flags) ->
    set_gc_flags(T, flag(Flags, ?GC_COPY_ERRLOG, Bool));
set_gc_flags([{copy_error_log, Bool}|T], Flags) ->
    set_gc_flags(T, flag(Flags, ?GC_COPY_ERRLOG, Bool));
set_gc_flags([{backwards_compat_parse, Bool}|T], Flags) ->
    set_gc_flags(T, flag(Flags, ?GC_BACKWARDS_COMPAT_PARSE, Bool));
set_gc_flags([{log_resolve_hostname, Bool}|T], Flags) ->
    set_gc_flags(T, flag(Flags, ?GC_LOG_RESOLVE_HOSTNAME, Bool));
set_gc_flags([{fail_on_bind_err, Bool}|T], Flags) ->
    set_gc_flags(T, flag(Flags,?GC_FAIL_ON_BIND_ERR,Bool));
set_gc_flags([{pick_first_virthost_on_nomatch, Bool}|T], Flags) ->
    set_gc_flags(T, flag(Flags, ?GC_PICK_FIRST_VIRTHOST_ON_NOMATCH,Bool));
set_gc_flags([{use_erlang_sendfile, Bool}|T], Flags) ->
    set_gc_flags(T, flag(Flags,?GC_USE_ERLANG_SENDFILE,Bool));
set_gc_flags([{use_yaws_sendfile, Bool}|T], Flags) ->
    set_gc_flags(T, flag(Flags,?GC_USE_YAWS_SENDFILE,Bool));
set_gc_flags([_|T], Flags) ->
    set_gc_flags(T, Flags);
set_gc_flags([], Flags) ->
    Flags.


%% Setup vhost configuration
setup_sconf(SL, SC) ->
    #sconf{port                  = lkup(port, SL, SC#sconf.port),
           flags                 = set_sc_flags(lkup(flags, SL, []),
                                                SC#sconf.flags),
           redirect_map          = lkup(redirect_map, SL,
                                        SC#sconf.redirect_map),
           rhost                 = lkup(rhost, SL, SC#sconf.rhost),
           rmethod               = lkup(rmethod, SL, SC#sconf.rmethod),
           docroot               = lkup(docroot, SL, SC#sconf.docroot),
           xtra_docroots         = lkup(xtra_docroots, SL,
                                        SC#sconf.xtra_docroots),
           listen                = lkup(listen, SL, SC#sconf.listen),
           servername            = lkup(servername, SL, SC#sconf.servername),
           serveralias           = lkup(serveralias, SL, SC#sconf.serveralias),
           yaws                  = lkup(yaws, SL, SC#sconf.yaws),
           ets                   = lkup(ets, SL, SC#sconf.ets),
           ssl                   = setup_ssl(SL, SC#sconf.ssl),
           authdirs              = setup_authdirs(SL, SC#sconf.authdirs),
           partial_post_size     = lkup(partial_post_size, SL,
                                        SC#sconf.partial_post_size),
           appmods               = lkup(appmods, SL, SC#sconf.appmods),
           expires               = lkup(expires, SL, SC#sconf.expires),
           errormod_401          = lkup(errormod_401, SL,
                                        SC#sconf.errormod_401),
           errormod_404          = lkup(errormod_404, SL,
                                        SC#sconf.errormod_404),
           errormod_crash        = lkup(errormod_crash, SL,
                                        SC#sconf.errormod_crash),
           arg_rewrite_mod       = lkup(arg_rewrite_mod, SL,
                                        SC#sconf.arg_rewrite_mod),
           logger_mod            = lkup(logger_mod, SL, SC#sconf.logger_mod),
           opaque                = lkup(opaque, SL, SC#sconf.opaque),
           start_mod             = lkup(start_mod, SL, SC#sconf.start_mod),
           allowed_scripts       = lkup(allowed_scripts, SL,
                                        SC#sconf.allowed_scripts),
           tilde_allowed_scripts = lkup(tilde_allowed_scripts, SL,
                                        SC#sconf.tilde_allowed_scripts),
           index_files           = lkup(index_files, SL, SC#sconf.index_files),
           revproxy              = lkup(revproxy, SL, SC#sconf.revproxy),
           soptions              = lkup(soptions, SL, SC#sconf.soptions),
           extra_cgi_vars        = lkup(extra_cgi_vars, SL,
                                        SC#sconf.extra_cgi_vars),
           stats                 = lkup(stats, SL, SC#sconf.stats),
           fcgi_app_server       = lkup(fcgi_app_server, SL,
                                        SC#sconf.fcgi_app_server),
           php_handler           = lkup(php_handler, SL, SC#sconf.php_handler),
           shaper                = lkup(shaper, SL, SC#sconf.shaper),
           deflate_options       = setup_deflate(SL, SC#sconf.deflate_options),
           mime_types_info       = setup_mime_types_info(
                                     SL, SC#sconf.mime_types_info
                                    ),
           dispatch_mod          = lkup(dispatchmod, SL, SC#sconf.dispatch_mod)
          }.

set_sc_flags([{access_log, Bool}|T], Flags) ->
    set_sc_flags(T, flag(Flags, ?SC_ACCESS_LOG, Bool));
set_sc_flags([{auth_log, Bool}|T], Flags) ->
    set_sc_flags(T, flag(Flags, ?SC_AUTH_LOG, Bool));
set_sc_flags([{add_port, Bool}|T], Flags) ->
    set_sc_flags(T, flag(Flags, ?SC_ADD_PORT, Bool));
set_sc_flags([{statistics, Bool}|T], Flags) ->
    set_sc_flags(T, flag(Flags, ?SC_STATISTICS, Bool));
set_sc_flags([{tilde_expand, Bool}|T], Flags) ->
    set_sc_flags(T, flag(Flags, ?SC_TILDE_EXPAND, Bool));
set_sc_flags([{dir_listings, Bool}|T], Flags) ->
    set_sc_flags(T, flag(Flags, ?SC_DIR_LISTINGS, Bool));
set_sc_flags([{deflate, Bool}|T], Flags) ->
    set_sc_flags(T, flag(Flags, ?SC_DEFLATE, Bool));
set_sc_flags([{dir_all_zip, Bool}|T], Flags) ->
    set_sc_flags(T, flag(Flags, ?SC_DIR_ALL_ZIP, Bool));
set_sc_flags([{dav, Bool}|T], Flags) ->
    set_sc_flags(T, flag(Flags, ?SC_DAV, Bool));
set_sc_flags([{fcgi_trace_protocol, Bool}|T], Flags) ->
    set_sc_flags(T, flag(Flags, ?SC_FCGI_TRACE_PROTOCOL, Bool));
set_sc_flags([{fcgi_log_app_error, Bool}|T], Flags) ->
    set_sc_flags(T, flag(Flags, ?SC_FCGI_LOG_APP_ERROR, Bool));
set_sc_flags([{forward_proxy, Bool}|T], Flags) ->
    set_sc_flags(T, flag(Flags, ?SC_FORWARD_PROXY, Bool));
set_sc_flags([{auth_skip_docroot, Bool}|T], Flags) ->
    set_sc_flags(T, flag(Flags, ?SC_AUTH_SKIP_DOCROOT, Bool));
set_sc_flags([_Unknown|T], Flags) ->
    error_logger:format("Unknown and unhandled flag ~p~n", [_Unknown]),
    set_sc_flags(T, Flags);
set_sc_flags([], Flags) ->
    Flags.

lkup(Key, List, Def) ->
    case lists:keyfind(Key, 1, List) of
        {_,Value} -> Value;
        _         -> Def
    end.



hup() ->
    dohup(undefined).

hup(Sock) ->
    spawn(fun() ->
                  group_leader(whereis(user), self()),
                  dohup(Sock)
          end).

dohup(Sock) ->
    Env = yaws_sup:get_app_args(),
    Res = try yaws_config:load(Env) of
              {ok, Gconf, Sconfs} -> yaws_api:setconf(Gconf, Sconfs);
              Err                 -> Err
          catch
              _:X ->
                  X
          end,
    gen_event:notify(yaws_event_manager, {yaws_hupped, Res}),
    yaws_log:rotate(Res),
    case Sock of
        undefined ->
            {yaws_hupped, Res};
        _  ->
            gen_tcp:send(Sock, io_lib:format("hupped: ~p~n", [Res])),
            gen_tcp:close(Sock)
    end.



%%% misc funcs
first(_F, []) ->
    false;
first(F, [H|T]) ->
    case F(H) of
        {ok, Val} -> {ok, Val, H};
        ok        -> {ok, ok, H};
        _         -> first(F, T)
    end.


elog(F, As) ->
    error_logger:format(F, As).


filesize(Fname) ->
    case file:read_file_info(Fname) of
        {ok, FI} when FI#file_info.type == regular ->
            {ok, FI#file_info.size};
        {ok, FI} ->
            {error,  FI#file_info.type};
        Err ->
            Err
    end.


upto(_I, [])    -> [];
upto(0,  _)     -> " ....";
upto(_I, [0|_]) -> " ....";
upto(I,  [H|T]) -> [H|upto(I-1, T)].


to_string(X) when is_float(X)   -> io_lib:format("~.2.0f",[X]);
to_string(X) when is_integer(X) -> erlang:integer_to_list(X);
to_string(X) when is_atom(X)    -> atom_to_list(X);
to_string(X)                    -> lists:concat([X]).


to_list(L) when is_list(L) -> L;
to_list(A) when is_atom(A) -> atom_to_list(A).


integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
        {'EXIT', _} -> old_integer_to_hex(I);
        Int         -> Int
    end.


old_integer_to_hex(I) when I < 10 ->
    integer_to_list(I);
old_integer_to_hex(I) when I < 16 ->
    [I-10+$A];
old_integer_to_hex(I) when I >= 16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).


hex_to_integer(Hex) ->
    erlang:list_to_integer(Hex, 16).


string_to_hex(String) ->
    HEXC = fun (D) when D > 9 -> $a + D - 10;
               (D)            -> $0 + D
           end,
    lists:foldr(fun(E, Acc) -> [HEXC(E div 16),HEXC(E rem 16)|Acc] end,
                [], String).


hex_to_string(Hex) ->
    DEHEX = fun (H) when H >= $a -> H - $a + 10;
                (H) when H >= $A -> H - $A + 10;
                (H) ->              H - $0
            end,
    {String, _} =
        lists:foldr(fun (E, {Acc, nolow}) -> {Acc, DEHEX(E)};
                        (E, {Acc, LO})    -> {[DEHEX(E)*16+LO|Acc], nolow}
                    end, {[], nolow}, Hex),
    String.



universal_time_as_string() ->
    universal_time_as_string(calendar:universal_time()).
universal_time_as_string(UTime) ->
    time_to_string(UTime, "GMT").
local_time_as_gmt_string(LocalTime) ->
    time_to_string(erlang:localtime_to_universaltime(LocalTime), "GMT").


time_to_string({{Year, Month, Day}, {Hour, Min, Sec}}, Zone) ->
    [day(Year, Month, Day), ", ",
     mk2(Day), " ", month(Month), " ", erlang:integer_to_list(Year), " ",
     mk2(Hour), ":", mk2(Min), ":", mk2(Sec), " ", Zone].

mk2(I) when I < 10 -> [$0 | erlang:integer_to_list(I)];
mk2(I)             -> erlang:integer_to_list(I).

day(Year, Month, Day) ->
    int_to_wd(calendar:day_of_the_week(Year, Month, Day)).

int_to_wd(1) -> "Mon";
int_to_wd(2) -> "Tue";
int_to_wd(3) -> "Wed";
int_to_wd(4) -> "Thu";
int_to_wd(5) -> "Fri";
int_to_wd(6) -> "Sat";
int_to_wd(7) -> "Sun".

month(1)  -> "Jan";
month(2)  -> "Feb";
month(3)  -> "Mar";
month(4)  -> "Apr";
month(5)  -> "May";
month(6)  -> "Jun";
month(7)  -> "Jul";
month(8)  -> "Aug";
month(9)  -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

month_str_to_int("Jan") -> 1;
month_str_to_int("Feb") -> 2;
month_str_to_int("Mar") -> 3;
month_str_to_int("Apr") -> 4;
month_str_to_int("May") -> 5;
month_str_to_int("Jun") -> 6;
month_str_to_int("Jul") -> 7;
month_str_to_int("Aug") -> 8;
month_str_to_int("Sep") -> 9;
month_str_to_int("Oct") -> 10;
month_str_to_int("Nov") -> 11;
month_str_to_int("Dec") -> 12.


stringdate_to_datetime([$ |T]) ->
    stringdate_to_datetime(T);
stringdate_to_datetime([_D1, _D2, _D3, $\,, $ |Tail]) ->
    stringdate_to_datetime1(Tail).

stringdate_to_datetime1([A, B, $\s |T]) ->
    stringdate_to_datetime2(T, erlang:list_to_integer([A,B]));
stringdate_to_datetime1([A, $\s |T]) ->
    stringdate_to_datetime2(T, erlang:list_to_integer([A])).

stringdate_to_datetime2([M1, M2, M3, $\s , Y1, Y2, Y3, Y4, $\s,
                         H1, H2, $:, Min1, Min2,$:,
                         S1, S2,$\s ,$G, $M, $T|_], Day) ->
    {{erlang:list_to_integer([Y1,Y2,Y3,Y4]),
      month_str_to_int([M1, M2, M3]), Day},
     {erlang:list_to_integer([H1, H2]),
      erlang:list_to_integer([Min1, Min2]),
      erlang:list_to_integer([S1, S2])}}.


%% used by If-Modified-Since header code
is_modified_p(FI, UTC_string) ->
    case catch stringdate_to_datetime(UTC_string) of
        {'EXIT', _ } ->
            true;
        UTC ->
            MtimeUTC = erlang:localtime_to_universaltime(FI#file_info.mtime),
            (MtimeUTC > UTC)
    end.


ticker(Time, Msg) ->
    ticker(Time, self(), Msg).
ticker(Time, To, Msg ) ->
    spawn_link(fun() ->
                       process_flag(trap_exit, true),
                       yaws_ticker:ticker(Time, To, Msg)
               end).


address() ->
    Sc = get(sc),
    ?F("<address> ~s Server at ~s </address>",
       [case Sc#sconf.yaws of
            undefined -> (get(gc))#gconf.yaws;
            Signature -> Signature
        end, Sc#sconf.servername]).


is_space($\s) -> true;
is_space($\r) -> true;
is_space($\n) -> true;
is_space($\t) -> true;
is_space(_)   -> false.


strip_spaces(String) ->
    strip_spaces(String, both).

strip_spaces(String, left) ->
    drop_spaces(String);
strip_spaces(String, right) ->
    lists:reverse(drop_spaces(lists:reverse(String)));
strip_spaces(String, both) ->
    strip_spaces(drop_spaces(String), right).

drop_spaces([]) ->
    [];
drop_spaces(YS=[X|XS]) ->
    case is_space(X) of
        true  -> drop_spaces(XS);
        false -> YS
    end.


%%% basic uuencode and decode functionality
list_to_uue(L) -> list_to_uue(L, []).

list_to_uue([], Out) ->
    lists:reverse([$\n,enc(0)|Out]);
list_to_uue(L, Out) ->
    {L45, L1} = get_45(L),
    Encoded = encode_line(L45),
    list_to_uue(L1, lists:reverse(Encoded, Out)).

uue_to_list(L) ->
    uue_to_list(L, []).

uue_to_list([], Out) ->
    lists:reverse(Out);
uue_to_list(L, Out) ->
    {Decoded, L1} = decode_line(L),
    uue_to_list(L1, lists:reverse(Decoded, Out)).

encode_line(L) ->
    [enc(length(L))|encode_line1(L)].

encode_line1([C0, C1, C2|T]) ->
    Char1 = enc(C0 bsr 2),
    Char2 = enc((C0 bsl 4) band 8#60 bor (C1 bsr 4) band 8#17),
    Char3 = enc((C1 bsl 2) band 8#74 bor (C2 bsr 6) band 8#3),
    Char4 = enc(C2 band 8#77),
    [Char1,Char2,Char3,Char4|encode_line1(T)];
encode_line1([C1, C2]) ->
    encode_line1([C1, C2, 0]);
encode_line1([C]) ->
    encode_line1([C,0,0]);
encode_line1([]) ->
    [$\n].

decode_line([H|T]) ->
    case dec(H) of
        0   -> {[], []};
        Len -> decode_line(T, Len, [])
    end.

decode_line([P0,P1,P2,P3|T], N, Out) when N >= 3->
    Char1 = 16#FF band ((dec(P0) bsl 2) bor (dec(P1) bsr 4)),
    Char2 = 16#FF band ((dec(P1) bsl 4) bor (dec(P2) bsr 2)),
    Char3 = 16#FF band ((dec(P2) bsl 6) bor dec(P3)),
    decode_line(T, N-3, [Char3,Char2,Char1|Out]);
decode_line([P0,P1,P2,_|T], 2, Out) ->
    Char1  = 16#FF band ((dec(P0) bsl 2) bor (dec(P1) bsr 4)),
    Char2  = 16#FF band ((dec(P1) bsl 4) bor (dec(P2) bsr 2)),
    {lists:reverse([Char2,Char1|Out]), tl(T)};
decode_line([P0,P1,_,_|T], 1, Out) ->
    Char1  = 16#FF band ((dec(P0) bsl 2) bor (dec(P1) bsr 4)),
    {lists:reverse([Char1|Out]), tl(T)};
decode_line(T, 0, Out) ->
    {lists:reverse(Out), tl(T)}.

get_45(L) -> get_45(L, 45, []).

get_45(L, 0, F)     -> {lists:reverse(F), L};
get_45([], _N, L)   -> {lists:reverse(L), []};
get_45([H|T], N, L) -> get_45(T, N-1, [H|L]).


%% enc/1 is the basic 1 character encoding function to make a char printing
%% dec/1 is the inverse
enc(0) -> $`;
enc(C) -> (C band 8#77) + $ .

dec(Char) -> (Char - $ ) band 8#77.


printversion() ->
    io:format("Yaws ~s~n", [yaws_generated:version()]),
    init:stop().

%% our default arg rewriter does of course nothing
arg_rewrite(A) ->
    A.

is_ssl(#sconf{ssl = undefined})                -> nossl;
is_ssl(#sconf{ssl = S}) when is_record(S, ssl) -> ssl.


to_lowerchar(C) when C >= $A, C =< $Z -> C+($a-$A);
to_lowerchar(C)                       -> C.

to_lower([])                           -> [];
to_lower([C|Cs]) when C >= $A, C =< $Z -> [C+($a-$A)|to_lower(Cs)];
to_lower([C|Cs])                       -> [C|to_lower(Cs)];
to_lower(A) when is_atom(A)            -> to_lower(atom_to_list(A)).


funreverse(List, Fun) ->
    funreverse(List, Fun, []).

funreverse([H|T], Fun, Ack) -> funreverse(T, Fun, [Fun(H)|Ack]);
funreverse([], _Fun, Ack)   -> Ack.

%% is arg1 a prefix of arg2
is_prefix([H|T1], [H|T2]) -> is_prefix(T1, T2);
is_prefix([], T)          -> {true, T};
is_prefix(_,_)            -> false.


%% Split a string of words separated by Sep into a list of words and
%% strip off white space.
%%
%% HTML semantics are used, such that empty words are omitted.
split_sep(undefined, _Sep) ->
    [];
split_sep(L, Sep) ->
    case drop_spaces(L) of
        []      -> [];
        [Sep|T] -> split_sep(T, Sep);
        [C|T]   -> split_sep(T, Sep, [C], [])
    end.

split_sep([], _Sep, AccL) ->
    lists:reverse(AccL);
split_sep([Sep|T], Sep, AccL) ->
    split_sep(T, Sep, AccL);
split_sep([C|T], Sep, AccL) ->
    split_sep(T, Sep, [C], AccL).

split_sep([], _Sep, AccW, AccL) ->
    lists:reverse([lists:reverse(drop_spaces(AccW))|AccL]);
split_sep([Sep|Tail], Sep, AccW, AccL) ->
    split_sep(drop_spaces(Tail), Sep, [lists:reverse(drop_spaces(AccW))|AccL]);
split_sep([C|Tail], Sep, AccW, AccL) ->
    split_sep(Tail, Sep, [C|AccW], AccL).


%% Join strings with separator. Same as string:join in later
%% versions of Erlang. Separator is expected to be a list.
join_sep([], Sep) when is_list(Sep) ->
    [];
join_sep([H|T], Sep) ->
    H ++ lists:append([Sep ++ X || X <- T]).

%% Provide a unique 3-tuple of positive integers.
unique_triple() -> yaws_dynopts:unique_triple().

%% Get a current time 3-tuple.
get_time_tuple() -> yaws_dynopts:get_time_tuple().

%% header parsing
parse_qval(S) ->
    parse_qval([], S).

parse_qval(A, ";q="++Q) -> {lists:reverse(A), parse_qvalue(Q)};
parse_qval(A, "")       -> {lists:reverse(A), 1000};
parse_qval(A, [C|T])    -> parse_qval([C|A], T).

parse_qvalue("0")              -> 0;
parse_qvalue("0.")             -> 0;
parse_qvalue("1")              -> 1000;
parse_qvalue("1.")             -> 1000;
parse_qvalue("1.0")            -> 1000;
parse_qvalue("1.00")           -> 1000;
parse_qvalue("1.000")          -> 1000;
parse_qvalue("0."++[D1])       -> three_digits_to_integer(D1,$0,$0);
parse_qvalue("0."++[D1,D2])    -> three_digits_to_integer(D1,D2,$0);
parse_qvalue("0."++[D1,D2,D3]) -> three_digits_to_integer(D1,D2,D3);
parse_qvalue(_)                -> 0. %% error

three_digits_to_integer(D1, D2, D3) ->
    100*(D1-$0)+10*(D2-$0)+D3-$0.


%% Gzip encoding
accepts_gzip(H, Mime) ->
    case [Val || {_,_,'Accept-Encoding',_,Val}<- H#headers.other] of
        [] ->
            false;
        [_|_]=AcceptEncoding0 ->
            AcceptEncoding = join_sep(AcceptEncoding0, ","),
            EncList = [parse_qval(X) || X <- split_sep(AcceptEncoding, $,)],
            case [Q || {"gzip",Q} <- EncList] ++ [Q || {"*",Q} <- EncList] of
                [] ->
                    false;
                [Q|_] ->
                    (Q > 100) %% just for fun
                        and not has_buggy_gzip(H#headers.user_agent, Mime)
            end
    end.

%%% Advice partly taken from Apache's documentation of `mod_deflate'.

%% Only Netscape 4.06-4.08 is really broken.
has_buggy_gzip("Mozilla/4.06"++_, _) ->
    true;
has_buggy_gzip("Mozilla/4.07"++_, _) ->
    true;
has_buggy_gzip("Mozilla/4.08"++_, _) ->
    true;

%% Everything else handles at least HTML.
has_buggy_gzip(_, "text/html") ->
    false;
has_buggy_gzip(UserAgent, Mime) ->
    UA = parse_ua(UserAgent),
    in_ua(fun("Mozilla/4"++_) ->
                  %% Netscape 4.x may choke on anything not HTML.
                  case Mime of
                      %% IE doesn't, but some versions are said to have issues
                      %% with plugins.
                      "application/pdf" ->
                          true;
                      _ -> not in_comment(
                                 fun("MSIE"++_) -> true;
                                    (_)         -> false
                                 end, UA)
                  end;
             ("w3m"++_) ->
                  %% W3m does not decompress when saving.
                  true;
             ("Opera") ->
                  %% Opera 6 does not uncompress downloads.
                  in_ua(fun("6."++_) -> true;
                           (_)       -> false
                        end, UA);
             ("Opera/6."++_) ->
                  true;
             (_) ->
                  false
          end, UA).


%%% Parsing of User-Agent header.
%%% Yes, this looks a bit like overkill.
tokenize_ua([], Acc) ->
    lists:reverse(Acc);
tokenize_ua([$\\ , C|T], Acc) ->
    tokenize_ua(T, [C|Acc]);
tokenize_ua([$(|T], Acc) ->
    tokenize_ua(T, [popen | Acc]);
tokenize_ua([$)|T], Acc) ->
    tokenize_ua(T, [pclose | Acc]);
tokenize_ua([C|T], Acc) ->
    tokenize_ua(T, [C|Acc]).

parse_ua(Line) ->
    case catch parse_ua_l(tokenize_ua(Line, [])) of
        {'EXIT', _} -> [];
        Res         -> Res
    end.

parse_ua_l(Line) ->
    case drop_spaces(Line) of
        [] ->
            [];
        [popen|T] ->
            {Comment, Tail} = parse_comment(T),
            [Comment | parse_ua_l(Tail)];
        [pclose|T] ->
            %% Error, ignore
            parse_ua_l(T);
        L ->
            {UA, Tail} = parse_ua1(L),
            [UA | parse_ua_l(Tail)]
    end.

parse_comment(L) ->
    parse_comment(L, [], []).

parse_comment([], _, _) ->
    %% Error
    {error, []};
parse_comment([pclose|T], CAcc, CsAcc) ->
    {{comment, lists:reverse([lists:reverse(CAcc)|CsAcc])}, T};
parse_comment([popen|T], CAcc, CsAcc) ->
    {Comment, Tail} = parse_comment(T),
    parse_comment(drop_spaces(Tail), [], [Comment, lists:reverse(CAcc)|CsAcc]);
parse_comment([$;|T], CAcc, CsAcc) ->
    parse_comment(drop_spaces(T), [], [lists:reverse(CAcc)|CsAcc]);
parse_comment([C|T], CAcc, CsAcc) ->
    parse_comment(T, [C|CAcc], CsAcc).


parse_ua1(L) ->
    parse_ua1(L, []).

parse_ua1([], Acc) ->
    {{ua,lists:reverse(Acc)}, []};
parse_ua1([popen|T], Acc) ->
    {{ua, lists:reverse(Acc)}, [popen|T]};
parse_ua1([pclose|T], _Acc) ->
    {error, T};
parse_ua1([$ |T], Acc) ->
    {{ua, lists:reverse(Acc)}, T};
parse_ua1([C|T], Acc) ->
    parse_ua1(T, [C|Acc]).


in_ua(Pred, L) ->
    lists:any(fun({ua, UA}) -> Pred(UA);
                 (_)        -> false
              end, L).

in_comment(_Pred, []) ->
    false;
in_comment(Pred, [{comment, Cs}|T]) ->
    case in_comment_l(Pred, Cs) of
        true  -> true;
        false -> in_comment(Pred, T)
    end;
in_comment(Pred, [_|T]) ->
    in_comment(Pred, T).


in_comment_l(Pred, Cs) ->
    lists:any(fun({comment, Cs1}) -> in_comment_l(Pred, Cs1);
                 (error)          -> false;
                 (L)              -> Pred(L)
              end, Cs).


%% imperative out header management
outh_set_status_code(Code) ->
    put(outh, (get(outh))#outh{status = Code}),
    ok.

outh_set_non_cacheable(_Version) ->
    put(outh, (get(outh))#outh{cache_control = "Cache-Control: no-cache\r\n"}),
    ok.

outh_set_content_type(Mime) ->
    put(outh, (get(outh))#outh{content_type = make_content_type_header(Mime)}),
    ok.

outh_set_content_encoding(Encoding) ->
    ContentEncoding = case Encoding of
                          identity -> undefined;
                          deflate  -> make_content_encoding_header(Encoding)
                      end,
    put(outh, (get(outh))#outh{encoding         = Encoding,
                               content_encoding = ContentEncoding}),
    ok.

outh_set_cookie(C) ->
    put(outh, (get(outh))#outh{set_cookie = ["Set-Cookie: ", C, "\r\n"]}),
    ok.


outh_clear_headers() ->
    H = get(outh),
    put(outh, #outh{status     = H#outh.status,
                    doclose    = true,
                    chunked    = false,
                    connection = make_connection_close_header(true)}),
    ok.


outh_set_static_headers(Req, UT, Headers) ->
    outh_set_static_headers(Req, UT, Headers, all).

outh_set_static_headers(Req, UT, Headers, Range) ->
    H = get(outh),
    FIL = (UT#urltype.finfo)#file_info.size,
    {DoClose0, Chunked0} = dcc(Req, Headers),
    {DoDeflate, Length}
        = case Range of
              all ->
                  case UT#urltype.deflate of
                      DB when is_binary(DB) -> % cached
                          %% Remove charset
                          [Mime|_] = yaws:split_sep(UT#urltype.mime, $;),
                          case accepts_gzip(Headers, Mime) of
                              true  -> {true, size(DB)};
                              false -> {false, FIL}
                          end;
                      undefined ->
                          {false, FIL};
                      dynamic ->
                          %% Remove charset
                          [Mime|_] = yaws:split_sep(UT#urltype.mime, $;),
                          case accepts_gzip(Headers, Mime) of
                              true  -> {true, undefined};
                              false -> {false, FIL}
                          end
                  end;
              {fromto, From, To, _} ->
                  {false, To - From + 1}
          end,
    Encoding = case DoDeflate of
                   true  -> decide;
                   false -> identity
               end,
    Chunked = Chunked0 and (Length == undefined),
    DoClose = if
                  DoClose0 == true ->
                      true;
                  ((Length == undefined) and not Chunked) ->
                      %% We cannot keep the connection alive, because the client
                      %% has no way of knowing the end of the content data.
                      true;
                  DoClose0 == keep_alive ->
                      keep_alive;
                  true ->
                      DoClose0
              end,

    H2 = H#outh{
           status            = case Range of
                                   all               -> 200;
                                   {fromto, _, _, _} -> 206
                               end,
           chunked           = Chunked,
           encoding          = Encoding,
           date              = make_date_header(),
           server            = make_server_header(),
           last_modified     = make_last_modified_header(UT#urltype.finfo),
           etag              = make_etag_header(UT#urltype.finfo),
           content_range     = make_content_range_header(Range),
           content_length    = make_content_length_header(Length),
           content_type      = make_content_type_header(UT#urltype.mime),
           content_encoding  = make_content_encoding_header(Encoding),
           transfer_encoding = make_transfer_encoding_chunked_header(Chunked),
           connection        = make_connection_close_header(DoClose),
           doclose           = DoClose,
           contlen           = Length
          },
    %% store finfo to set last_modified, expires and cache_control headers
    %% during #outh{} serialization.
    put(file_info, UT#urltype.finfo),
    put(outh, H2).

outh_set_304_headers(Req, UT, Headers) ->
    H = get(outh),
    {DoClose, _Chunked} = dcc(Req, Headers),
    H2 = H#outh{
           status         = 304,
           chunked        = false,
           date           = make_date_header(),
           server         = make_server_header(),
           last_modified  = make_last_modified_header(UT#urltype.finfo),
           etag           = make_etag_header(UT#urltype.finfo),
           content_length = make_content_length_header(0),
           connection     = make_connection_close_header(DoClose),
           doclose        = DoClose,
           contlen        = 0
          },
    %% store finfo to set last_modified, expires and cache_control headers
    %% during #outh{} serialization.
    put(file_info, UT#urltype.finfo),
    put(outh, H2).

outh_set_dyn_headers(Req, Headers, UT) ->
    H = get(outh),
    {DoClose, Chunked} = dcc(Req, Headers),
    H2 = H#outh{
           status            = 200,
           date              = make_date_header(),
           server            = make_server_header(),
           connection        = make_connection_close_header(DoClose),
           content_type      = make_content_type_header(UT#urltype.mime),
           doclose           = DoClose,
           chunked           = Chunked,
           transfer_encoding = make_transfer_encoding_chunked_header(Chunked)},
    %% store finfo to set last_modified, expires and cache_control headers
    %% during #outh{} serialization.
    put(file_info, UT#urltype.finfo),
    put(outh, H2).


outh_set_connection(What) ->
    H = get(outh),
    H2 = H#outh{connection = make_connection_close_header(What),
                doclose    = What},
    put(outh, H2),
    ok.


outh_set_content_length(Int) ->
    H  = get(outh),
    H2 = H#outh{
           content_length = make_content_length_header(Int),
           contlen        = Int
          },
    put(outh, H2).



outh_set_dcc(Req, Headers) ->
    H = get(outh),
    {DoClose, Chunked} = dcc(Req, Headers),
    H2 = H#outh{
           connection        = make_connection_close_header(DoClose),
           doclose           = DoClose,
           chunked           = Chunked,
           transfer_encoding = make_transfer_encoding_chunked_header(Chunked)
          },
    put(outh, H2).


%% can only turn if off, not on.
%% if it allready is off, it's off because the cli headers forced us.
outh_set_transfer_encoding_off() ->
    H  = get(outh),
    H2 = H#outh{
           chunked           = false,
           transfer_encoding = make_transfer_encoding_chunked_header(false)
          },
    put(outh, H2).

outh_set_auth([]) ->
    ok;

outh_set_auth(Headers) ->
    H  = get(outh),
    H2 = case H#outh.www_authenticate of
             undefined ->
                 H#outh{www_authenticate = Headers};
             _ ->
                 H#outh{www_authenticate = H#outh.www_authenticate ++ Headers}
         end,
    put(outh, H2).

outh_set_vary(Fields) ->
    put(outh, (get(outh))#outh{vary = make_vary_header(Fields)}),
    ok.

outh_fix_doclose() ->
    H = get(outh),
    if
        (H#outh.doclose /= true)    andalso
        (H#outh.contlen==undefined) andalso
        (H#outh.chunked == false) ->
            put(outh, H#outh{doclose    = true,
                             connection = make_connection_close_header(true)});
        true ->
            ok
    end.


dcc(Req, Headers) ->
    H = get(outh),
    DoClose = case Req#http_request.version of
                  _ when H#outh.exceedmaxuses == true ->
                      true; %% too many keepalives
                  {1, 0} ->
                      case Headers#headers.connection of
                          "close"      -> true;
                          "Keep-Alive" -> keep_alive;
                          _            -> true
                      end;
                  {1, 1} ->
                      Headers#headers.connection == "close";
                  {0,9} ->
                      true
              end,
    Chunked = case Req#http_request.version of
                  {1, 0} -> false;
                  {1,1}  -> true;
                  {0,9}  ->  false
              end,
    {DoClose, Chunked}.





%%
%% The following all make_ function return an actual header string
%%
make_allow_header() ->
    make_allow_header([]).
make_allow_header(Options) ->
    case Options of
        [] ->
            ["Allow: GET, POST, OPTIONS, HEAD\r\n"];
        _ ->
            ["Allow: ",
             lists:foldl(fun(M, "") -> atom_to_list(M);
                            (M, Acc) -> atom_to_list(M) ++ ", " ++ Acc
                         end, "", lists:reverse(Options)),
             "\r\n"]
    end.
make_server_header() ->
    Sc = get(sc),
    Signature = case Sc#sconf.yaws of
                    undefined -> (get(gc))#gconf.yaws;
                    S         -> S
                end,
    case Signature of
        "" ->
            [];
        _ ->
            ["Server: ", Signature, "\r\n"]
    end.

make_last_modified_header(FI) ->
    Then = FI#file_info.mtime,
    ["Last-Modified: ", local_time_as_gmt_string(Then), "\r\n"].


make_expires_header(all, FI) ->
    SC = get(sc),
    case lists:keyfind(all, 1, SC#sconf.expires) of
        {_, EType, TTL} -> make_expires_header(EType, TTL, FI);
        false           -> {undefined, undefined}
    end;
make_expires_header({Type,all}, FI) ->
    SC = get(sc),
    case lists:keyfind({Type,all}, 1, SC#sconf.expires) of
        {_, EType, TTL} -> make_expires_header(EType, TTL, FI);
        false           -> make_expires_header(all, FI)
    end;
make_expires_header({Type,SubType}, FI) ->
    SC = get(sc),
    case lists:keyfind({Type,SubType}, 1, SC#sconf.expires) of
        {_, EType, TTL} -> make_expires_header(EType, TTL, FI);
        false           -> make_expires_header({Type,all}, FI)
    end;
make_expires_header(MT0, FI) ->
    SC = get(sc),
    %% Use split_sep to remove charset
    case yaws:split_sep(MT0, $;) of
        [] -> {undefined, undefined};
        [MT1|_] ->
            case lists:keyfind(MT1, 1, SC#sconf.expires) of
                {_, EType, TTL} ->
                    make_expires_header(EType, TTL, FI);
                false ->
                    case split_sep(MT1, $/) of
                        [Type, SubType] ->
                            make_expires_header({Type,SubType}, FI);
                        false ->
                            make_expires_header(all, FI)
                    end
            end
    end.


make_expires_header(always, _TTL, _FI) ->
    {["Expires: ", "Thu, 01 Jan 1970 00:00:00 GMT\r\n"],
     ["Cache-Control: ", "private, no-cache, no-store, must-revalidate, max-age=0, proxy-revalidate, s-maxage=0\r\n"]};
make_expires_header(access, TTL, _FI) ->
    Secs = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    ExpireTime = calendar:gregorian_seconds_to_datetime(Secs+TTL),
    {["Expires: ", universal_time_as_string(ExpireTime), "\r\n"],
     ["Cache-Control: ", "max-age=", erlang:integer_to_list(TTL), "\r\n"]};
make_expires_header(modify, TTL, FI) ->
    %% mtime is local here
    Secs1 = calendar:datetime_to_gregorian_seconds(FI#file_info.mtime),
    Secs2 = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
    ExpireTime = calendar:gregorian_seconds_to_datetime(Secs1+TTL),
    MaxAge     = erlang:max(0, TTL - (Secs2 - Secs1)),
    {["Expires: ", local_time_as_gmt_string(ExpireTime), "\r\n"],
     ["Cache-Control: ", "max-age=", erlang:integer_to_list(MaxAge), "\r\n"]}.


make_location_header(Where) ->
    ["Location: ", Where, "\r\n"].


make_etag_header(FI) ->
    ETag = make_etag(FI),
    ["Etag: ", ETag, "\r\n"].

make_etag(FI) ->
    Stamp = {FI#file_info.size, FI#file_info.mtime},
    ETag = integer_to_list(erlang:phash2(Stamp, 16#100000000), 19),
    lists:flatten([$", ETag, $"]).


make_content_type_header(no_content_type) ->
    undefined;
make_content_type_header(MimeType) ->
    ["Content-Type: ", MimeType, "\r\n"].


make_content_range_header(all) ->
    undefined;
make_content_range_header({fromto, From, To, Tot}) ->
    ["Content-Range: bytes ",
     erlang:integer_to_list(From), $-, erlang:integer_to_list(To),
     $/, erlang:integer_to_list(Tot), $\r, $\n].

make_content_length_header(Size) when is_integer(Size) ->
    ["Content-Length: ", erlang:integer_to_list(Size), "\r\n"];
make_content_length_header(FI) when is_record(FI, file_info) ->
    Size = FI#file_info.size,
    ["Content-Length: ", erlang:integer_to_list(Size), "\r\n"];
make_content_length_header(_) ->
    undefined.

make_content_encoding_header(deflate) ->
    "Content-Encoding: gzip\r\n";
make_content_encoding_header(_) ->
    undefined.

make_connection_close_header(true) ->
    "Connection: close\r\n";
make_connection_close_header(false) ->
    undefined;
make_connection_close_header(keep_alive) ->
    "Connection: Keep-Alive\r\n".

make_transfer_encoding_chunked_header(true) ->
    "Transfer-Encoding: chunked\r\n";
make_transfer_encoding_chunked_header(false) ->
    undefined.

make_www_authenticate_header({realm, Realm}) ->
    ["WWW-Authenticate: Basic realm=\"", Realm, ["\"\r\n"]];

make_www_authenticate_header(Method) ->
    ["WWW-Authenticate: ", Method, ["\r\n"]].

make_date_header() ->
    N = element(2, os:timestamp()),
    case get(date_header) of
        {_Str, Secs} when (Secs+10) < N ->
            H = ["Date: ", universal_time_as_string(), "\r\n"],
            put(date_header, {H, N}),
            H;
        {Str, _Secs} ->
            Str;
        undefined ->
            H = ["Date: ", universal_time_as_string(), "\r\n"],
            put(date_header, {H, N}),
            H
    end.

make_vary_header(Fields) ->
    case lists:member("*", Fields) of
        true  -> ["Vary: ", "*", "\r\n"];
        false -> ["Vary: ", join_sep(Fields, ", "), "\r\n"]
    end.



%% access functions into the outh record
outh_get_status_code() ->
    (get(outh))#outh.status.

outh_get_contlen() ->
    (get(outh))#outh.contlen.

outh_get_act_contlen() ->
    (get(outh))#outh.act_contlen.

outh_inc_act_contlen(Int) ->
    O = get(outh),
    L = case O#outh.act_contlen of
            undefined -> Int;
            Len       -> Len+Int
        end,
    put(outh, O#outh{act_contlen = L}),
    L.

outh_get_doclose() ->
    (get(outh))#outh.doclose.

outh_get_chunked() ->
    (get(outh))#outh.chunked.

outh_get_content_encoding() ->
    (get(outh))#outh.encoding.

outh_get_content_encoding_header() ->
    (get(outh))#outh.content_encoding.

outh_get_content_type() ->
    case (get(outh))#outh.content_type of
        undefined    -> undefined;
        [_, Mime, _] -> Mime
    end.

outh_get_vary_fields() ->
    case (get(outh))#outh.vary of
        undefined      -> [];
        [_, Fields, _] -> split_sep(Fields, $,)
    end.

outh_serialize() ->
    H = get(outh),
    Code = case H#outh.status of
               undefined -> 200;
               Int       -> Int
           end,
    StatusLine = ["HTTP/1.1 ", erlang:integer_to_list(Code), " ",
                  yaws_api:code_to_phrase(Code), "\r\n"],
    GC=get(gc),
    if ?gc_has_debug(GC) -> yaws_debug:check_headers(H);
       true              -> ok
    end,
    ContentEnc = case H#outh.content_encoding of
                     undefined -> make_content_encoding_header(H#outh.encoding);
                     CE        -> CE
                 end,
    {Expires, CacheControl} =
        case erase(file_info) of
            undefined ->
                {H#outh.expires, H#outh.cache_control};
            FI ->
                {E, CC} = case {H#outh.expires, H#outh.cache_control} of
                              {undefined, undefined} ->
                                  CT = outh_get_content_type(),
                                  make_expires_header(CT, FI);
                              _ ->
                                  {H#outh.expires, H#outh.cache_control}
                          end,
                {E, CC}
        end,

    %% Add 'Accept-Encoding' in the 'Vary:' header if the compression is enabled
    %% or if the response is compressed _AND_ if the response has a non-empty
    %% body.
    Vary = case get(sc) of
               undefined -> undefined;
               SC ->
                   case (?sc_has_deflate(SC) orelse H#outh.encoding == deflate) of
                       true when H#outh.contlen /= undefined, H#outh.contlen /= 0;
                                 H#outh.act_contlen /= undefined,
                                 H#outh.act_contlen /= 0 ->
                           Fields = outh_get_vary_fields(),
                           Fun    = fun("*") -> true;
                                       (F)   -> (to_lower(F) == "accept-encoding")
                                    end,
                           case lists:any(Fun, Fields) of
                               true  -> H#outh.vary;
                               false -> make_vary_header(["Accept-Encoding"|Fields])
                           end;
                       _ ->
                           H#outh.vary
                   end
           end,

    Headers = [noundef(H#outh.connection),
               noundef(H#outh.server),
               noundef(H#outh.location),
               noundef(H#outh.date),
               noundef(H#outh.allow),
               noundef(H#outh.last_modified),
               noundef(Expires),
               noundef(CacheControl),
               noundef(H#outh.etag),
               noundef(H#outh.content_range),
               noundef(H#outh.content_length),
               noundef(H#outh.content_type),
               noundef(ContentEnc),
               noundef(H#outh.set_cookie),
               noundef(H#outh.transfer_encoding),
               noundef(H#outh.www_authenticate),
               noundef(Vary),
               noundef(H#outh.other)],
    {StatusLine, Headers}.


noundef(undefined) -> [];
noundef(Str)       -> Str.



accumulate_header({X, erase}) when is_atom(X) ->
    erase_header(X);


%% special headers
accumulate_header({connection, What}) ->
    DC = case What of
             "close" -> true;
             _       -> false
         end,
    H = get(outh),
    put(outh, H#outh{connection = ["Connection: ", What, "\r\n"],
                     doclose    = DC});
accumulate_header({"Connection", What}) ->
    accumulate_header({connection, What});

accumulate_header({server, What}) ->
    put(outh, (get(outh))#outh{server = ["Server: ", What, "\r\n"]});
accumulate_header({"Server", What}) ->
    accumulate_header({server, What});

accumulate_header({location, What}) ->
    put(outh, (get(outh))#outh{location = ["Location: ", What, "\r\n"]});
accumulate_header({"Location", What}) ->
    accumulate_header({location, What});

accumulate_header({cache_control, What}) ->
    put(outh, (get(outh))#outh{cache_control = ["Cache-Control: ", What,
                                                "\r\n"]});
accumulate_header({"Cache-Control", What}) ->
    accumulate_header({cache_control, What});

accumulate_header({expires, What}) ->
    put(outh, (get(outh))#outh{expires = ["Expires: ", What, "\r\n"]});
accumulate_header({"Expires", What}) ->
    accumulate_header({expires, What});

accumulate_header({date, What}) ->
    put(outh, (get(outh))#outh{date = ["Date: ", What, "\r\n"]});
accumulate_header({"Date", What}) ->
    accumulate_header({date, What});

accumulate_header({allow, What}) ->
    put(outh, (get(outh))#outh{date = ["Allow: ", What, "\r\n"]});
accumulate_header({"Allow", What}) ->
    accumulate_header({allow, What});

accumulate_header({last_modified, What}) ->
    put(outh, (get(outh))#outh{last_modified = ["Last-Modified: ", What,
                                                "\r\n"]});
accumulate_header({"Last-Modified", What}) ->
    accumulate_header({last_modified, What});

accumulate_header({etag, What}) ->
    put(outh, (get(outh))#outh{etag = ["Etag: ", What, "\r\n"]});
accumulate_header({"Etag", What}) ->
    accumulate_header({etag, What});

accumulate_header({set_cookie, What}) ->
    O = get(outh),
    Old = case O#outh.set_cookie of
              undefined -> "";
              X         -> X
          end,
    put(outh, O#outh{set_cookie = ["Set-Cookie: ", What, "\r\n"|Old]});
accumulate_header({"Set-Cookie", What}) ->
    accumulate_header({set_cookie, What});

accumulate_header({content_range, What}) ->
    put(outh, (get(outh))#outh{content_range = ["Content-Range: ", What,
                                                "\r\n"]});
accumulate_header({"Content-Range", What}) ->
    accumulate_header({content_range, What});

accumulate_header({content_type, What}) ->
    put(outh, (get(outh))#outh{content_type = ["Content-Type: ", What,
                                               "\r\n"]});
accumulate_header({"Content-Type", What}) ->
    accumulate_header({content_type, What});

accumulate_header({content_encoding, What}) ->
    case What of
        "identity" ->
            put(outh, (get(outh))#outh{encoding         = identity,
                                       content_encoding = undefined});
        _ ->
            put(outh, (get(outh))#outh{encoding         = deflate,
                                       content_encoding = ["Content-Encoding: ",
                                                           What, "\r\n"]})
    end;
accumulate_header({"Content-Encoding", What}) ->
    accumulate_header({content_encoding, What});

accumulate_header({content_length, Len}) when is_integer(Len) ->
    H = get(outh),
    put(outh, H#outh{
                chunked           = false,
                transfer_encoding = undefined,
                contlen           = Len,
                act_contlen       = 0,
                content_length    = make_content_length_header(Len)});
accumulate_header({"Content-Length", Len}) ->
    case Len of
        I when is_integer(I) ->
            accumulate_header({content_length, I});
        L when is_list(L) ->
            accumulate_header({content_length, erlang:list_to_integer(L)})
    end;

accumulate_header({transfer_encoding, What}) ->
    put(outh, (get(outh))#outh{chunked           = true,
                               contlen           = 0,
                               transfer_encoding = ["Transfer-Encoding: ", What,
                                                    "\r\n"]});
accumulate_header({"Transfer-Encoding", What}) ->
    accumulate_header({transfer_encoding, What});

accumulate_header({www_authenticate, What}) ->
    put(outh, (get(outh))#outh{www_authenticate = ["WWW-Authenticate: ", What,
                                                   "\r\n"]});
accumulate_header({"WWW-Authenticate", What}) ->
    accumulate_header({www_authenticate, What});

accumulate_header({vary, What}) ->
    put(outh, (get(outh))#outh{vary = ["Vary: ", What, "\r\n"]});
accumulate_header({"Vary", What}) ->
    accumulate_header({vary, What});

%% non-special headers (which may be special in a future Yaws version)
accumulate_header({Name, What}) when is_list(Name) ->
    H = get(outh),
    Old = case H#outh.other of
              undefined -> [];
              V         -> V
          end,
    H2 = H#outh{other = [Name, ": ", What, "\r\n", Old]},
    put(outh, H2);



%% backwards compatible clause
accumulate_header(Data) when is_list(Data) ->
    Str = lists:flatten(Data),
    accumulate_header(split_header(Str)).

split_header(Str)           ->
    split_header(Str, []).

split_header([], A)         -> {lists:reverse(A), ""};
split_header([$:, $ |W], A) -> {lists:reverse(A), W};
split_header([$:|W], A)     -> {lists:reverse(A), W};
split_header([C|S], A)      -> split_header(S, [C|A]).


erase_header(connection) ->
    put(outh, (get(outh))#outh{connection=undefined, doclose=false});
erase_header(server) ->
    put(outh, (get(outh))#outh{server=undefined});
erase_header(cache_control) ->
    put(outh, (get(outh))#outh{cache_control=undefined});
erase_header(expires) ->
    put(outh, (get(outh))#outh{expires=undefined});
erase_header(date) ->
    put(outh, (get(outh))#outh{date=undefined});
erase_header(allow) ->
    put(outh, (get(outh))#outh{allow=undefined});
erase_header(last_modified) ->
    put(outh, (get(outh))#outh{last_modified=undefined});
erase_header(etag) ->
    put(outh, (get(outh))#outh{etag=undefined});
erase_header(set_cookie) ->
    put(outh, (get(outh))#outh{set_cookie=undefined});
erase_header(content_range) ->
    put(outh, (get(outh))#outh{content_range=undefined});
erase_header(content_length) ->
    put(outh, (get(outh))#outh{contlen=0, content_length=undefined});
erase_header(content_type) ->
    put(outh, (get(outh))#outh{content_type=undefined});
erase_header(content_encoding) ->
    put(outh, (get(outh))#outh{encoding=decide, content_encoding=undefined});
erase_header(transfer_encoding) ->
    put(outh, (get(outh))#outh{chunked           = false,
                               act_contlen       = 0,
                               transfer_encoding = undefined});
erase_header(www_authenticate) ->
    put(outh, (get(outh))#outh{www_authenticate=undefined});
erase_header(location) ->
    put(outh, (get(outh))#outh{location=undefined});
erase_header(vary) ->
    put(outh, (get(outh))#outh{vary=undefined}).

getuid() ->
    case os:type() of
        {win32, _} ->
            {ok, "0"};
        _ ->
            load_setuid_drv(),
            P = open_port({spawn, "setuid_drv g"},[]),
            receive
                {P, {data, "ok " ++ IntList}} ->
                    {ok, IntList}
            end
    end.

user_to_home(User) ->
    case os:type() of
        {win32, _} ->
            ".";
        _ ->
            load_setuid_drv(),
            P = open_port({spawn, "setuid_drv " ++ [$h|User]}, []),
            receive
                {P, {data, "ok " ++ Home}} ->
                    Home
            end
    end.


uid_to_name(Uid) ->
    load_setuid_drv(),
    P = open_port({spawn, "setuid_drv " ++
                       [$n|erlang:integer_to_list(Uid)]}, []),
    receive
        {P, {data, "ok " ++ Name}} ->
            Name
    end.

load_setuid_drv() ->
    Path = filename:join(get_priv_dir(), "lib"),
    case erl_ddll:load_driver(Path, "setuid_drv") of
        ok ->
            ok;
        {error, Reason} ->
            error_logger:format("Failed to load setuid_drv (from ~p) : ~p",
                                [Path, erl_ddll:format_error(Reason)]),
            exit(normal)
    end.

exists(F) ->
    case file:open(F, [read, raw]) of
        {ok, Fd} ->
            file:close(Fd),
            ok;
        _ ->
            false
    end.


mkdir(Path) ->
    [Hd|Parts] = filename:split(Path),
    mkdir([Hd], Parts).
mkdir(Ack, []) ->
    ensure_exist(filename:join(Ack));
mkdir(Ack, [H|T]) ->
    ensure_exist(filename:join(Ack ++ [H])),
    mkdir(Ack ++ [H], T).

ensure_exist(Path) ->
    case file:read_file_info(Path) of
        {ok, _} ->
            ok;
        _ ->
            case file:make_dir(Path) of
                ok ->
                    ok;
                ERR ->
                    error_logger:format("Failed to mkdir ~p: ~p~n", [Path, ERR])
            end
    end.

%%
%%
%% TCP/SSL connection with a configurable IPv4/IPv6 preference on NS lookup.
%%
%%

tcp_connect(Host, Port, Options) ->
    tcp_connect(Host, Port, Options, infinity).

tcp_connect(Host, Port, Options, Timeout) ->
    parse_ipaddr_and_connect(tcp, Host, Port, Options, Timeout).

ssl_connect(Host, Port, Options) ->
    ssl_connect(Host, Port, Options, infinity).

ssl_connect(Host, Port, Options, Timeout) ->
    parse_ipaddr_and_connect(ssl, Host, Port, Options, Timeout).

parse_ipaddr_and_connect(Proto, IP, Port, Options, Timeout)
when is_tuple(IP) ->
    %% The caller handled name resolution himself.
    filter_tcpoptions_and_connect(Proto, undefined,
      IP, Port, Options, Timeout);
parse_ipaddr_and_connect(Proto, [$[ | Rest], Port, Options, Timeout) ->
    %% yaws_api:parse_url/1 keep the "[...]" enclosing an IPv6 address.
    %% Remove them now, and parse the address.
    IP = string:strip(Rest, right, $]),
    parse_ipaddr_and_connect(Proto, IP, Port, Options, Timeout);
parse_ipaddr_and_connect(Proto, Host, Port, Options, Timeout) ->
    %% First, try to parse an IP address, because inet:getaddr/2 could
    %% return nxdomain if the family doesn't match the IP address
    %% format.
    case inet:parse_strict_address(Host) of
        {ok, IP} ->
            filter_tcpoptions_and_connect(Proto, undefined,
                                          IP, Port, Options, Timeout);
        {error, einval} ->
            NsLookupPref = get_nslookup_pref(Options),
            filter_tcpoptions_and_connect(Proto, NsLookupPref,
                                          Host, Port, Options, Timeout)
    end.

filter_tcpoptions_and_connect(Proto, NsLookupPref,
  Host, Port, Options, Timeout) ->
    %% Now that we have IP addresses, remove family from the TCP options,
    %% because calling gen_tcp:connect/3 with {127,0,0,1} and [inet6]
    %% would return {error, nxdomain otherwise}.
    OptionsWithoutFamily = lists:filter(fun
          (inet)  -> false;
          (inet6) -> false;
          (_)     -> true
      end, Options),
    resolve_and_connect(Proto, NsLookupPref, Host, Port, OptionsWithoutFamily, Timeout).

resolve_and_connect(Proto, _, IP, Port, Options, Timeout)
when is_tuple(IP) ->
    do_connect(Proto, IP, Port, Options, Timeout);
resolve_and_connect(Proto, [Family | Rest], Host, Port, Options, Timeout) ->
    Result = case inet:getaddr(Host, Family) of
        {ok, IP} -> do_connect(Proto, IP, Port, Options, Timeout);
        R        -> R
    end,
    case Result of
        {ok, Socket} ->
            {ok, Socket};
        {error, _} when length(Rest) >= 1 ->
            %% If the connection fails here, ignore the error and
            %% continue with the next address family.
            resolve_and_connect(Proto, Rest, Host, Port, Options, Timeout);
        {error, Reason} ->
            %% This was the last IP address in the list, return the
            %% connection error.
            {error, Reason}
    end.

do_connect(Proto, IP, Port, Options, Timeout) ->
    case Proto of
        tcp -> gen_tcp:connect(IP, Port, Options, Timeout);
        ssl -> ssl:connect(IP, Port, Options, Timeout)
    end.

%% If the caller specified inet or inet6 in the TCP options, prefer
%% this to the global nslookup_pref parameter.
%%
%% This can be used in processes which can't use get(gc) to get the
%% global conf: if they are given the global conf, they can get
%% nslookup_pref value and add it the TCP options.
%%
%% If neither TCP options specify the family, nor the global conf is
%% accessible, use default value declared in #gconf definition.
get_nslookup_pref(TcpOptions) ->
    get_nslookup_pref(TcpOptions, []).

get_nslookup_pref([inet | Rest], Result) ->
    get_nslookup_pref(Rest, [inet | Result]);
get_nslookup_pref([inet6 | Rest], Result) ->
    get_nslookup_pref(Rest, [inet6 | Result]);
get_nslookup_pref([_ | Rest], Result) ->
    get_nslookup_pref(Rest, Result);
get_nslookup_pref([], []) ->
    case get(gc) of
        undefined -> gconf_nslookup_pref(#gconf{});
        GC        -> gconf_nslookup_pref(GC)
    end;
get_nslookup_pref([], Result) ->
    lists:reverse(Result).

%%
%%
%% http/tcp send receive functions
%%
%%
do_recv(Sock, Num, nossl) ->
    gen_tcp:recv(Sock, Num, (get(gc))#gconf.keepalive_timeout);
do_recv(Sock, Num, ssl) ->
    ssl:recv(Sock, Num, (get(gc))#gconf.keepalive_timeout).
do_recv(Sock, Num, nossl, Timeout) ->
    gen_tcp:recv(Sock, Num, Timeout);
do_recv(Sock, Num, ssl, Timeout) ->
    ssl:recv(Sock, Num, Timeout).

cli_recv(S, Num, SslBool) ->
    Res = do_recv(S, Num, SslBool),
    cli_recv_trace(yaws_trace:get_type(get(gc)), Res),
    Res.

cli_recv_trace(undefined, _) ->
    ok;
cli_recv_trace(Trace, Res) ->
    case Res of
        {ok, Val} when is_tuple(Val) ->
            yaws_trace:write(from_client, ?F("~p~n", [Val]));
        {error, What} ->
            yaws_trace:write(from_client, ?F("~p~n", [What]));
        {ok, http_eoh} ->
            ok;
        {ok, Val} when Trace == traffic ->
            yaws_trace:write(from_client, Val);
        _ ->
            ok
    end.



gen_tcp_send(S, Data) ->
    SC = get(sc),
    Res = case SC of
              undefined ->
                  case catch ssl:sockname(S) of
                      {ok, _} -> ssl:send(S, Data);
                      _ -> gen_tcp:send(S, Data)
                  end;
              _ ->
                  case SC#sconf.ssl of
                      undefined -> gen_tcp:send(S, Data);
                      _SSL      -> ssl:send(S, Data)
                  end
          end,
    case ?gc_has_debug((get(gc))) of
        false ->
            case Res of
                ok ->
                    case SC of
                        undefined -> ok;
                        _ ->
                            yaws_stats:sent(iolist_size(Data))
                    end,
                    ok;
                _Err ->
                    exit(normal)   %% keep quiet
            end;
        true ->
            case Res of
                ok ->
                    case SC of
                        undefined -> ok;
                        _ ->
                            yaws_stats:sent(iolist_size(Data))
                    end,
                    ?Debug("Sent ~p~n", [yaws_debug:nobin(Data)]),
                    ok;
                Err ->
                    {B2, Size} = strip(Data),
                    yaws_debug:derror("Failed to send ~w bytes:~n~p "
                                      "on socket ~p: ~p~n~p~n",
                                      [Size, B2, S, Err,
                                       yaws_debug:nobin(Data)]),
                    erlang:error(Err)
            end
    end.


strip(Data) ->
    L = list_to_binary([Data]),
    case L of
        <<Head:50/binary, _/binary>> ->
            {binary_to_list(<<Head/binary, ".....">>), size(L)};
        _ ->
            {binary_to_list(L), size(L)}
    end.



%% This is the api function
%% return {Req, Headers}
%%     or closed
http_get_headers(CliSock, SSL) ->
    do_http_get_headers(CliSock, SSL).


headers_to_str(Headers) ->
    lists:map(fun(H) -> [H, "\r\n"] end, yaws_api:reformat_header(Headers)).


setopts(Sock, Opts, nossl) ->
    ok = inet:setopts(Sock, Opts);
setopts(Sock, Opts, ssl) ->
    ok = ssl:setopts(Sock, Opts).

do_http_get_headers(CliSock, SSL) ->
    case http_recv_request(CliSock,SSL) of
        bad_request ->
            {#http_request{method=bad_request, version={0,9}}, #headers{}};
        closed ->
            closed;
        R ->
            %% Http request received. Store the current time. it will be usefull
            %% to get the time taken to serve the request.
            put(request_start_time, os:timestamp()),
            case http_collect_headers(CliSock, R,  #headers{}, SSL, 0) of
                {error, _}=Error ->
                    Error;
                H ->
                    {R, H}
            end
    end.


http_recv_request(CliSock, SSL) ->
    setopts(CliSock, [{packet, http}, {packet_size, 16#4000}], SSL),
    case do_recv(CliSock, 0,  SSL) of
        {ok, R} when is_record(R, http_request) ->
            R;
        {ok, R} when is_record(R, http_response) ->
            R;
        {_, {http_error, "\r\n"}} ->
            http_recv_request(CliSock, SSL);
        {_, {http_error, "\n"}} ->
            http_recv_request(CliSock,SSL);
        {_, {http_error, _}} ->
            bad_request;
        {error, closed} ->
            closed;
        {error, timeout} ->
            closed;
        _Other ->
            error_logger:format("Unhandled reply fr. do_recv() ~p~n", [_Other]),
            exit(normal)
    end.

http_collect_headers(CliSock, Req, H, SSL, Count) when Count < 1000 ->
    setopts(CliSock, [{packet, httph}, {packet_size, 16#4000}], SSL),
    Recv = do_recv(CliSock, 0, SSL),
    case Recv of
        {ok, {http_header,  _Num, 'Host', _, Host}} ->
            NewHostH = case H#headers.host of
                           undefined ->
                               H#headers{host = Host};
                           {Hosts} ->
                               H#headers{host = {[Host | Hosts]}};
                           CurrentHost ->
                               H#headers{host = {[Host, CurrentHost]}}
                       end,
            http_collect_headers(CliSock, Req, NewHostH, SSL, Count+1);
        {ok, {http_header, _Num, 'Connection', _, Conn}} ->
            http_collect_headers(CliSock, Req,
                                 H#headers{connection = Conn},SSL, Count+1);
        {ok, {http_header, _Num, 'Accept', _, Accept}} ->
            http_collect_headers(CliSock, Req, H#headers{accept = Accept},
                                 SSL, Count+1);
        {ok, {http_header, _Num, 'If-Modified-Since', _, X}} ->
            http_collect_headers(CliSock, Req,
                                 H#headers{if_modified_since = X},SSL, Count+1);
        {ok, {http_header, _Num, 'If-Match', _, X}} ->
            http_collect_headers(CliSock, Req, H#headers{if_match = X},
                                 SSL, Count+1);
        {ok, {http_header, _Num, 'If-None-Match', _, X}} ->
            http_collect_headers(CliSock, Req,
                                 H#headers{if_none_match = X},SSL, Count+1);
        {ok, {http_header, _Num, 'If-Range', _, X}} ->
            http_collect_headers(CliSock, Req, H#headers{if_range = X},
                                 SSL, Count+1);
        {ok, {http_header, _Num, 'If-Unmodified-Since', _, X}} ->
            http_collect_headers(CliSock, Req,
                                 H#headers{if_unmodified_since = X},SSL,
                                 Count+1);
        {ok, {http_header, _Num, 'Range', _, X}} ->
            http_collect_headers(CliSock, Req, H#headers{range = X},
                                 SSL, Count+1);
        {ok, {http_header, _Num, 'Referer',_, X}} ->
            http_collect_headers(CliSock, Req, H#headers{referer = X},
                                 SSL, Count+1);
        {ok, {http_header, _Num, 'User-Agent', _, X}} ->
            http_collect_headers(CliSock, Req, H#headers{user_agent = X},
                                 SSL, Count+1);
        {ok, {http_header, _Num, 'Accept-Ranges', _, X}} ->
            http_collect_headers(CliSock, Req,
                                 H#headers{accept_ranges = X},SSL, Count+1);
        {ok, {http_header, _Num, 'Cookie', _, X}} ->
            http_collect_headers(CliSock, Req,
                                 H#headers{cookie = [X|H#headers.cookie]},
                                 SSL, Count+1);
        {ok, {http_header, _Num, 'Keep-Alive', _, X}} ->
            http_collect_headers(CliSock, Req, H#headers{keep_alive = X},
                                 SSL, Count+1);
        {ok, {http_header, _Num, 'Content-Length', _, X}} ->
            http_collect_headers(CliSock, Req,
                                 H#headers{content_length = X},SSL,
                                 Count+1);
        {ok, {http_header, _Num, 'Content-Type', _, X}} ->
            http_collect_headers(CliSock, Req,
                                 H#headers{content_type = X},SSL, Count+1);
        {ok, {http_header, _Num, 'Content-Encoding', _, X}} ->
            http_collect_headers(CliSock, Req,
                                 H#headers{content_encoding = X},SSL, Count+1);
        {ok, {http_header, _Num, 'Transfer-Encoding', _, X}} ->
            http_collect_headers(CliSock, Req,
                                 H#headers{transfer_encoding=X},SSL, Count+1);
        {ok, {http_header, _Num, 'Location', _, X}} ->
            http_collect_headers(CliSock, Req, H#headers{location=X},
                                 SSL, Count+1);
        {ok, {http_header, _Num, 'Authorization', _, X}} ->
            http_collect_headers(CliSock, Req,
                                 H#headers{authorization = parse_auth(X)},
                                 SSL, Count+1);
        {ok, {http_header, _Num, 'X-Forwarded-For', _, X}} ->
            case H#headers.x_forwarded_for of
                undefined ->
                    http_collect_headers(CliSock, Req, H#headers{x_forwarded_for=X},
                                         SSL, Count+1);
                PrevXF ->
                    NewXF = join_sep([PrevXF,X], ", "),
                    http_collect_headers(CliSock, Req, H#headers{x_forwarded_for=NewXF},
                                         SSL, Count+1)
            end;
        {ok, http_eoh} ->
            H;

        %% these are here to be a little forgiving to
        %% bad (typically test script) clients
        {_, {http_error, "\r\n"}} ->
            http_collect_headers(CliSock, Req, H,SSL, Count+1);
        {_, {http_error, "\n"}} ->
            http_collect_headers(CliSock, Req, H,SSL, Count+1);

        %% auxiliary headers we don't have builtin support for
        {ok, X} ->
            ?Debug("OTHER header ~p~n", [X]),
            http_collect_headers(CliSock, Req,
                                 H#headers{other=[X|H#headers.other]},
                                 SSL, Count+1);
        _Err ->
            exit(normal)

    end;
http_collect_headers(_CliSock, Req, _H, _SSL, _Count)  ->
    {error, {too_many_headers, Req}}.



parse_auth(Orig = "Basic " ++ Auth64) ->
    case decode_base64(Auth64) of
        {error, _Err} ->
            {undefined, undefined, Orig};
        Auth ->
            case string:tokens(Auth, ":") of
                [User, Pass ] ->
                    {User, Pass, Orig};
                [User, Pass0 | Extra] ->
                    %% password can contain :
                    Pass = join_sep([Pass0 | Extra], ":"),
                    {User, Pass, Orig};
                _ ->
                    {undefined, undefined, Orig}
            end
    end;
parse_auth(Orig = "Negotiate " ++ _Auth64) ->
    {undefined, undefined, Orig};
parse_auth(Orig) ->
    {undefined, undefined, Orig}.


decode_base64([]) ->
    [];
decode_base64(Auth64) ->
    decode_base64(Auth64, []).
decode_base64([], Acc) ->
    lists:reverse(Acc);
decode_base64([Sextet1,Sextet2,$=,$=|Rest], Acc) ->
    Bits2x6 =
        (d(Sextet1) bsl 18) bor
        (d(Sextet2) bsl 12),
    Octet1 = Bits2x6 bsr 16,
    decode_base64(Rest, [Octet1|Acc]);
decode_base64([Sextet1,Sextet2,Sextet3,$=|Rest], Acc) ->
    Bits3x6 =
        (d(Sextet1) bsl 18) bor
        (d(Sextet2) bsl 12) bor
        (d(Sextet3) bsl 6),
    Octet1 = Bits3x6 bsr 16,
    Octet2 = (Bits3x6 bsr 8) band 16#ff,
    decode_base64(Rest, [Octet2,Octet1|Acc]);
decode_base64([Sextet1,Sextet2,Sextet3,Sextet4|Rest], Acc) ->
    Bits4x6 =
        (d(Sextet1) bsl 18) bor
        (d(Sextet2) bsl 12) bor
        (d(Sextet3) bsl 6) bor
        d(Sextet4),
    Octet1 = Bits4x6 bsr 16,
    Octet2 = (Bits4x6 bsr 8) band 16#ff,
    Octet3 = Bits4x6 band 16#ff,
    decode_base64(Rest, [Octet3,Octet2,Octet1|Acc]);
decode_base64(_CatchAll, _Acc) ->
    {error, bad_base64}.

d(X) when X >= $A, X =<$Z -> X-65;
d(X) when X >= $a, X =<$z -> X-71;
d(X) when X >= $0, X =<$9 -> X+4;
d($+)                     -> 62;
d($/)                     -> 63;
d(_)                      -> 63.


flag(CurFlag, Bit, true)  -> CurFlag bor Bit;
flag(CurFlag, Bit, false) -> CurFlag band (bnot Bit).


%% misc debug funcs .... use from cli only
restart() ->
    stop(),
    load(),
    start().


modules() ->
    application:load(yaws),
    M = case application:get_all_key(yaws) of
            {ok, L} ->
                case lists:keysearch(modules, 1, L) of
                    {value, {modules, Mods}} -> Mods;
                    _                        -> []
                end;
            _ ->
                []
        end,
    M.


load() ->
    load(modules()).
load(M) ->
    lists:foreach(fun(Mod) ->
                          ?Debug("Load ~p~n", [Mod]),
                          c:l(Mod)
                  end, M).



upto_char(Char, [Char|_]) ->
    [];
upto_char(Char, [H|T]) when is_integer(H) ->
    [H|upto_char(Char, T)];
upto_char(_, []) ->
    [];
%% deep lists
upto_char(Char, [H|T]) when is_list(H) ->
    case lists:member(Char ,H) of
        true  -> upto_char(Char, H);
        false -> [H, upto_char(Char, T)]
    end.


%% map over deep list and maintain
%% list structure as is
deepmap(Fun, [H|T]) when is_list(H) ->
    [deepmap(Fun, H) | deepmap(Fun, T)];
deepmap(Fun, [H|T]) ->
    [Fun(H) | deepmap(Fun,T)];
deepmap(_Fun, []) ->
    [].


sconf_to_srvstr(SC) ->
    redirect_scheme(SC) ++ redirect_host(SC,undefined).

redirect_scheme(SC) ->
    case {SC#sconf.ssl,SC#sconf.rmethod} of
        {_, Method} when is_list(Method) -> Method++"://";
        {undefined, _}                   -> "http://";
        {_SSl, _}                        -> "https://"
    end.

redirect_host(SC, HostHdr) ->
    case SC#sconf.rhost of
        undefined ->
            if HostHdr == undefined ->
                    ServerName  = SC#sconf.servername,
                    SnameNoPort = case string:chr(ServerName, $:) of
                                      0 -> ServerName;
                                      N -> lists:sublist(ServerName, N-1)
                                  end,
                    SnameNoPort ++ redirect_port(SC);
               true ->
                    HostHdr
            end;
        _ ->
            SC#sconf.rhost
    end.

redirect_port(SC) ->
    case {SC#sconf.rmethod, SC#sconf.ssl, SC#sconf.port} of
        {"https", _, 443}    -> "";
        {"http", _, 80}      -> "";
        {_, undefined, 80}   -> "";
        {_, undefined, Port} -> [$:|erlang:integer_to_list(Port)];
        {_, _SSL, 443}       -> "";
        {_, _SSL, Port}      -> [$:|erlang:integer_to_list(Port)]
    end.

redirect_scheme_port(SC) ->
    Scheme   = redirect_scheme(SC),
    PortPart = redirect_port(SC),
    {Scheme, PortPart}.

tmpdir() ->
    tmpdir(filename:join([home(), ".yaws"])).
tmpdir(DefaultTmpDir) ->
    case os:type() of
        {win32,_} ->
            case os:getenv("TEMP") of
                false ->
                    case os:getenv("TMP") of
                        %%
                        %% No temporary path set?
                        %% Then try standard paths.
                        %%
                        false ->
                            case file:read_file_info("C:/WINNT/Temp") of
                                {error, _} -> "C:/WINDOWS/Temp";
                                {ok, _}    -> "C:/WINNT/Temp"
                            end;
                        PathTMP ->
                            PathTMP
                    end;
                PathTEMP ->
                    PathTEMP
            end;
        _ ->
            DefaultTmpDir
    end.

%% mktemp function borrowed from Klacke's misc module
%% Modified to use tmpdir/1 so it works on Windows too.
%% Note that mktemp/2 could be exported too, but no Yaws
%% code needs it, yet anyway.
mktemp(Template) ->
    mktemp(Template, file).

mktemp(Template, Ret) ->
    Tdir = tmpdir("/tmp"),
    Max = 1000,
    mktemp(Tdir, Template, Ret, 0, Max, "").

mktemp(Dir, Template, Ret, I, Max, Suffix) when I < Max ->
    {X,Y,Z} = unique_triple(),
    PostFix = erlang:integer_to_list(X) ++ "-" ++
        erlang:integer_to_list(Y) ++ "-" ++
        erlang:integer_to_list(Z),
    F = filename:join(Dir, Template ++ [$_ | PostFix] ++ Suffix),
    filelib:ensure_dir(F),
    case file:open(F, [read, raw]) of
        {error, enoent} when Ret == file ->
            {ok, F};
        {error, enoent} when Ret == fd ->
            case file:open(F, [read, write, raw]) of
                {ok, Fd} ->
                    file:delete(F),
                    {ok, Fd};
                Err ->
                    Err
            end;
        {error, enoent} when Ret == binfd ->
            case file:open(F, [read, write, raw, binary]) of
                {ok, Fd} ->
                    file:delete(F),
                    {ok, Fd};
                Err ->
                    Err
            end;
        {ok, Fd} ->
            file:close(Fd),
            mktemp(Dir, Template, Ret, I+1, Max, Suffix);
        _Err ->
            mktemp(Dir, Template, Ret, I+1, Max, Suffix)
    end;
mktemp(_Dir, _Template, _Ret, _I, _Max, _Suffix) ->
    {error, too_many}.


%% This feature is usable together with
%% privbind and authbind on linux
home() ->
    case os:getenv("YAWSHOME") of
        false -> os:getenv("HOME");
        DIR   -> DIR
    end.

id_dir(Id) ->
    filename:join([tmpdir(), "yaws", to_list(Id)]).

ctl_file(Id) ->
    filename:join([id_dir(Id), "CTL"]).


eat_crnl(Fd,SSL) ->
    setopts(Fd, [{packet, line}],SSL),
    case do_recv(Fd,0, SSL) of
        {ok, <<13,10>>} -> ok;
        {ok, [13,10]}   -> ok;
        _               -> exit(normal)
    end.


get_chunk_num(Fd, SSL) ->
    {N, _} = get_chunk_header(Fd, SSL),
    N.

get_chunk_header(Fd, SSL) ->
    case do_recv(Fd, 0, SSL) of
        {ok, Data} ->
            Line = if is_binary(Data) -> binary_to_list(Data);
                      true            -> Data
                   end,
            ?Debug("Get chunk num from line ~p~n",[Line]),
            {N, Exts} = split_at(Line, $;),
            {erlang:list_to_integer(strip_spaces(N),16), strip_spaces(Exts)};
        {error, _Rsn} ->
            exit(normal)
    end.


get_chunk(_Fd, N, N, _) ->
    [];
get_chunk(Fd, N, Asz,SSL) ->
    case do_recv(Fd, N, SSL) of
        {ok, Bin} ->
            SZ = size(Bin),
            [Bin|get_chunk(Fd, N, SZ+Asz,SSL)];
        _ ->
            exit(normal)
    end.

get_chunk_trailer(Fd, SSL) ->
    Hdrs = #headers{},
    case http_collect_headers(Fd, undefined, Hdrs, SSL, 0) of
        {error,_} -> exit(normal);
        Hdrs      -> <<>>;
        NewHdrs   -> {<<>>, NewHdrs}
    end.

%% split inputstring at first occurrence of Char
split_at(String, Char) ->
    split_at(String, Char, []).
split_at([H|T], H, Ack) ->
    {lists:reverse(Ack), T};
split_at([H|T], Char, Ack) ->
    split_at(T, Char, [H|Ack]);
split_at([], _Char, Ack) ->
    {lists:reverse(Ack), []}.

%% insert an elemant at a given position into a list
insert_at(Elm, 0, Ls) ->
    Ls ++ [Elm];
insert_at(Elm, Pos, Ls) ->
    insert_at(Elm, Pos, Ls, []).

insert_at(Elm, _, [], Res) ->
    lists:reverse([Elm|Res]);
insert_at(Elm, 1, Ls, Res) ->
    lists:reverse([Elm|Res]) ++ Ls;
insert_at(Elm, Pos, [H|T], Res) ->
    insert_at(Elm, Pos-1, T, [H|Res]).



%% Parse an Ip address or an Ip address range
%% Return Ip || {IpMin, IpMax} where:
%%     Ip, IpMin, IpMax ::= ip_address()
parse_ipmask(Str) when is_list(Str) ->
    case string:tokens(Str, [$/]) of
        [IpStr] ->
            case inet_parse:address(IpStr) of
                {ok, Ip}        -> Ip;
                {error, Reason} -> throw({error, Reason})
            end;
        [IpStr, NetMask] ->
            {Type, IpInt} = ip_to_integer(IpStr),
            MaskInt       = netmask_to_integer(Type, NetMask),
            case netmask_to_wildcard(Type, MaskInt) of
                0 ->
                    integer_to_ip(Type, IpInt);
                Wildcard when Type =:= ipv4 ->
                    NetAddr   = (IpInt band MaskInt),
                    Broadcast = NetAddr + Wildcard,
                    IpMin     = NetAddr + 1,
                    IpMax     = Broadcast - 1,
                    {integer_to_ip(ipv4, IpMin), integer_to_ip(ipv4, IpMax)};
                Wildcard when Type =:= ipv6 ->
                    NetAddr   = (IpInt band MaskInt),
                    IpMin = NetAddr,
                    IpMax = NetAddr + Wildcard,
                    {integer_to_ip(ipv6, IpMin), integer_to_ip(ipv6, IpMax)}
            end;
        _ ->
            throw({error, einval})
    end;
parse_ipmask(_) ->
    throw({error, einval}).


-define(MAXBITS_IPV4, 32).
-define(MASK_IPV4,    16#FFFFFFFF).
-define(MAXBITS_IPV6, 128).
-define(MASK_IPV6,    16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).

ip_to_integer(Str) when is_list(Str) ->
    case inet_parse:address(Str) of
        {ok, Ip}        -> ip_to_integer(Ip);
        {error, Reason} -> throw({error, Reason})
    end;
ip_to_integer({N1,N2,N3,N4}) ->
    <<Int:32>> = <<N1:8, N2:8, N3:8, N4:8>>,
    if
        (Int bsr ?MAXBITS_IPV4) == 0 -> {ipv4, Int};
        true -> throw({error, einval})
    end;
ip_to_integer({N1,N2,N3,N4,N5,N6,N7,N8}) ->
    <<Int:128>> = <<N1:16, N2:16, N3:16, N4:16, N5:16, N6:16, N7:16, N8:16>>,
    if
        (Int bsr ?MAXBITS_IPV6) == 0 -> {ipv6, Int};
        true -> throw({error, einval})
    end;
ip_to_integer(_) ->
    throw({error, einval}).

integer_to_ip(ipv4, I) when is_integer(I), I =< ?MASK_IPV4 ->
    <<N1:8, N2:8, N3:8, N4:8>> = <<I:32>>,
    {N1, N2, N3, N4};
integer_to_ip(ipv6, I) when is_integer(I), I =< ?MASK_IPV6 ->
    <<N1:16, N2:16, N3:16, N4:16, N5:16, N6:16, N7:16, N8:16>> = <<I:128>>,
    {N1, N2, N3, N4, N5, N6, N7, N8};
integer_to_ip(_, _) ->
    throw({error, einval}).

netmask_to_integer(Type, NetMask) ->
    case catch erlang:list_to_integer(NetMask) of
        I when is_integer(I) ->
            case Type of
                ipv4 -> (1 bsl ?MAXBITS_IPV4) - (1 bsl (?MAXBITS_IPV4 - I));
                ipv6 -> (1 bsl ?MAXBITS_IPV6) - (1 bsl (?MAXBITS_IPV6 - I))
            end;
        _ ->
            case ip_to_integer(NetMask) of
                {Type, MaskInt} -> MaskInt;
                _               -> throw({error, einval})
            end
    end.

netmask_to_wildcard(ipv4, Mask) -> ((1 bsl ?MAXBITS_IPV4) - 1) bxor Mask;
netmask_to_wildcard(ipv6, Mask) -> ((1 bsl ?MAXBITS_IPV6) - 1) bxor Mask.


%% Compare an ip to another ip or a range of ips
match_ipmask(Ip, Ip) ->
    true;
match_ipmask(Ip, {IpMin, IpMax}) ->
    case compare_ips(Ip, IpMin) of
        error -> false;
        less  -> false;
        _ ->
            case compare_ips(Ip, IpMax) of
                error   -> false;
                greater -> false;
                _       -> true
            end
    end;
match_ipmask(_, _) ->
    false.

compare_ips({A,B,C,D},          {A,B,C,D})                       -> equal;
compare_ips({A,B,C,D,E,F,G,H},  {A,B,C,D,E,F,G,H})               -> equal;
compare_ips({A,B,C,D1},         {A,B,C,D2})         when D1 < D2 -> less;
compare_ips({A,B,C,D1},         {A,B,C,D2})         when D1 > D2 -> greater;
compare_ips({A,B,C1,_},         {A,B,C2,_})         when C1 < C2 -> less;
compare_ips({A,B,C1,_},         {A,B,C2,_})         when C1 > C2 -> greater;
compare_ips({A,B1,_,_},         {A,B2,_,_})         when B1 < B2 -> less;
compare_ips({A,B1,_,_},         {A,B2,_,_})         when B1 > B2 -> greater;
compare_ips({A1,_,_,_},         {A2,_,_,_})         when A1 < A2 -> less;
compare_ips({A1,_,_,_},         {A2,_,_,_})         when A1 > A2 -> greater;
compare_ips({A,B,C,D,E,F,G,H1}, {A,B,C,D,E,F,G,H2}) when H1 < H2 -> less;
compare_ips({A,B,C,D,E,F,G,H1}, {A,B,C,D,E,F,G,H2}) when H1 > H2 -> greater;
compare_ips({A,B,C,D,E,F,G1,_}, {A,B,C,D,E,F,G2,_}) when G1 < G2 -> less;
compare_ips({A,B,C,D,E,F,G1,_}, {A,B,C,D,E,F,G2,_}) when G1 > G2 -> greater;
compare_ips({A,B,C,D,E,F1,_,_}, {A,B,C,D,E,F2,_,_}) when F1 < F2 -> less;
compare_ips({A,B,C,D,E,F1,_,_}, {A,B,C,D,E,F2,_,_}) when F1 > F2 -> greater;
compare_ips({A,B,C,D,E1,_,_,_}, {A,B,C,D,E2,_,_,_}) when E1 < E2 -> less;
compare_ips({A,B,C,D,E1,_,_,_}, {A,B,C,D,E2,_,_,_}) when E1 > E2 -> greater;
compare_ips({A,B,C,D1,_,_,_,_}, {A,B,C,D2,_,_,_,_}) when D1 < D2 -> less;
compare_ips({A,B,C,D1,_,_,_,_}, {A,B,C,D2,_,_,_,_}) when D1 > D2 -> greater;
compare_ips({A,B,C1,_,_,_,_,_}, {A,B,C2,_,_,_,_,_}) when C1 < C2 -> less;
compare_ips({A,B,C1,_,_,_,_,_}, {A,B,C2,_,_,_,_,_}) when C1 > C2 -> greater;
compare_ips({A,B1,_,_,_,_,_,_}, {A,B2,_,_,_,_,_,_}) when B1 < B2 -> less;
compare_ips({A,B1,_,_,_,_,_,_}, {A,B2,_,_,_,_,_,_}) when B1 > B2 -> greater;
compare_ips({A1,_,_,_,_,_,_,_}, {A2,_,_,_,_,_,_,_}) when A1 < A2 -> less;
compare_ips({A1,_,_,_,_,_,_,_}, {A2,_,_,_,_,_,_,_}) when A1 > A2 -> greater;
compare_ips(_,                  _)                               -> error.


%% Use  IANA range for dynamic or private ports (49152 -> 65535)
find_private_port() ->
    Start = case application:get_env(kernel, next_yaws_private_port) of
                {ok, Port} -> Port;
                undefined  -> 49152
            end,
    End = 65535,
    find_private_port(Start, End).

find_private_port(Port, End) when Port > End ->
    {error, notfound};
find_private_port(Port, End) ->
    case gen_tcp:listen(Port, [{ip, {0,0,0,0}}]) of
        {ok, Sock} ->
            gen_tcp:close(Sock),
            application:set_env(kernel, next_yaws_private_port, Port+1),
            {ok, Port};
        {error, _} ->
            find_private_port(Port+1, End)
    end.

%% ----
get_app_subdir(SubDir) when is_atom(SubDir) ->
    filename:join(get_app_dir(), atom_to_list(SubDir)).

get_app_dir() ->
    case application:get_env(yaws, app_dir) of
        {ok, AppDir} ->
            AppDir;
        undefined ->
            Path = case code:which(?MODULE) of
                       cover_compiled -> code:where_is_file("yaws.beam");
                       Dir            -> Dir
                   end,
            AppDir = filename:absname(filename:dirname(filename:dirname(Path))),
            application:set_env(yaws, app_dir, AppDir),
            AppDir
    end.

get_ebin_dir() ->
    get_app_subdir(ebin).

get_priv_dir() ->
    get_app_subdir(priv).

get_inc_dir() ->
    get_app_subdir(include).
