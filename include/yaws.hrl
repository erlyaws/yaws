%%%----------------------------------------------------------------------
%%% File    : yaws.hrl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose :
%%% Created : 16 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-author('klacke@hyber.org').



%% flags for gconfs
-define(GC_TTY_TRACE,                        1).
-define(GC_DEBUG,                            2).
-define(GC_COPY_ERRLOG,                      4).
-define(GC_BACKWARDS_COMPAT_PARSE,           8).
-define(GC_LOG_RESOLVE_HOSTNAME,            16).
-define(GC_FAIL_ON_BIND_ERR,                32).
-define(GC_PICK_FIRST_VIRTHOST_ON_NOMATCH,  64).
-define(GC_USE_FDSRV,                      128).
-define(GC_USE_ERLANG_SENDFILE,            256).


-define(GC_DEF, ?GC_FAIL_ON_BIND_ERR).

-define(gc_has_tty_trace(GC),
        ((GC#gconf.flags band ?GC_TTY_TRACE) /= 0)).
-define(gc_has_debug(GC),
        ((GC#gconf.flags band ?GC_DEBUG) /= 0)).
-define(gc_has_copy_errlog(GC),
        ((GC#gconf.flags band ?GC_COPY_ERRLOG) /= 0)).
-define(gc_log_has_resolve_hostname(GC),
        ((GC#gconf.flags band ?GC_LOG_RESOLVE_HOSTNAME) /= 0)).
-define(gc_fail_on_bind_err(GC),
        ((GC#gconf.flags band ?GC_FAIL_ON_BIND_ERR) /= 0)).
-define(gc_pick_first_virthost_on_nomatch(GC),
        ((GC#gconf.flags band ?GC_PICK_FIRST_VIRTHOST_ON_NOMATCH) /= 0)).
-define(gc_use_erlang_sendfile(GC),
        ((GC#gconf.flags band ?GC_USE_ERLANG_SENDFILE) /= 0)).

-define(gc_set_tty_trace(GC, Bool),
        GC#gconf{flags = yaws:flag(GC#gconf.flags,?GC_TTY_TRACE, Bool)}).
-define(gc_set_debug(GC, Bool),
        GC#gconf{flags = yaws:flag(GC#gconf.flags, ?GC_DEBUG, Bool)}).
-define(gc_set_copy_errlog(GC, Bool),
        GC#gconf{flags = yaws:flag(GC#gconf.flags, ?GC_COPY_ERRLOG, Bool)}).
-define(gc_log_set_resolve_hostname(GC, Bool),
        GC#gconf{flags = yaws:flag(GC#gconf.flags,
                                   ?GC_LOG_RESOLVE_HOSTNAME, Bool)}).
-define(gc_set_fail_on_bind_err(GC, Bool),
        GC#gconf{flags = yaws:flag(GC#gconf.flags,?GC_FAIL_ON_BIND_ERR,Bool)}).
-define(gc_set_pick_first_virthost_on_nomatch(GC, Bool),
        GC#gconf{flags = yaws:flag(GC#gconf.flags,
                                   ?GC_PICK_FIRST_VIRTHOST_ON_NOMATCH,Bool)}).
-define(gc_set_use_erlang_sendfile(GC, Bool),
        GC#gconf{flags = yaws:flag(GC#gconf.flags,?GC_USE_ERLANG_SENDFILE,Bool)}).

%% global conf
-record(gconf,{
          yaws_dir,                       % topdir of Yaws installation
          trace,                          % false | {true,http} | {true,traffic}
          flags = ?GC_DEF,                % boolean flags
          logdir,
          ebin_dir = [],
          src_dir  = [],
          runmods  = [],                  % runmods for entire server
          keepalive_timeout    = 30000,
          keepalive_maxuses    = nolimit, % nolimit or non negative integer
          max_num_cached_files = 400,
          max_num_cached_bytes = 1000000, % 1 MEG
          max_size_cached_file = 8000,
          max_connections      = nolimit, % max number of TCP connections

          %% Override default connection handler processes spawn options for
          %% performance/memory tuning.
          %% [] | [{fullsweep_after,Number}, {min_heap_size, Size}]
          %% other options such as monitor, link are ignored.
          process_options = [],

          large_file_chunk_size = 10240,
          mnesia_dir            = [],
          log_wrap_size         = 1000000,  % wrap logs after 1M
          cache_refresh_secs    = 30,       % seconds  (auto zero when debug)
          include_dir           = [],       % list of inc dirs for .yaws files
          phpexe = "/usr/bin/php-cgi",      % cgi capable php executable

          yaws,                % server string
          id = "default",      % string identifying this instance of yaws

          enable_soap = false, % start yaws_soap_srv iff true

          %% a list of
          %% {{Mod, Func}, WsdlFile, Prefix} | {{Mod, Func}, WsdlFile}
          %% automatically setup in yaws_soap_srv init.
          soap_srv_mods = [],

          acceptor_pool_size = 8,             % size of acceptor proc pool

          mime_types_info,                    % undefined | #mime_types_info{}
          nslookup_pref = [inet],             % [inet | inet6]
          ysession_mod = yaws_session_server, % storage module for ysession
          ysession_cookiegen,                 % ysession cookie generation module
          ysession_idle_timeout = 2*60*1000,  % default 2 minutes
          ysession_long_timeout = 60*60*1000, % default 1 hour

          sni = disable % disable | enable | strict
         }).

-record(ssl, {
          keyfile,
          certfile,
          verify = verify_none,
          fail_if_no_peer_cert,
          depth = 1,
          password,
          cacertfile,
          dhfile,
          ciphers,
          cachetimeout,
          secure_renegotiate = false,
          client_renegotiation = case yaws_dynopts:have_ssl_client_renegotiation() of
                                     true  -> true;
                                     false -> undefined
                                 end,
          honor_cipher_order = case yaws_dynopts:have_ssl_honor_cipher_order() of
                                   true  -> true;
                                   false -> undefined
                               end,
          protocol_version,
          require_sni = false,
          eccs
         }).


%% flags for sconfs
-define(SC_ACCESS_LOG,              (1 bsl 0)).
-define(SC_AUTH_LOG,                (1 bsl 1)).
-define(SC_ADD_PORT,                (1 bsl 2)).
-define(SC_STATISTICS,              (1 bsl 3)).
-define(SC_TILDE_EXPAND,            (1 bsl 4)).
-define(SC_DIR_LISTINGS,            (1 bsl 5)).
-define(SC_DEFLATE,                 (1 bsl 6)).
-define(SC_DIR_ALL_ZIP,             (1 bsl 7)).
-define(SC_DAV,                     (1 bsl 8)).
-define(SC_FCGI_TRACE_PROTOCOL,     (1 bsl 9)).
-define(SC_FCGI_LOG_APP_ERROR,      (1 bsl 10)).
-define(SC_FORWARD_PROXY,           (1 bsl 11)).
-define(SC_AUTH_SKIP_DOCROOT,       (1 bsl 12)).
-define(SC_STRIP_UNDEF_BINDINGS,    (1 bsl 13)).

-define(SC_DEF, ?SC_ACCESS_LOG bor ?SC_ADD_PORT bor ?SC_AUTH_LOG).

-define(sc_has_access_log(SC),
        (((SC)#sconf.flags band ?SC_ACCESS_LOG) /= 0)).
-define(sc_has_auth_log(SC),
        (((SC)#sconf.flags band ?SC_AUTH_LOG) /= 0)).
-define(sc_has_add_port(SC),
        (((SC)#sconf.flags band ?SC_ADD_PORT) /= 0)).
-define(sc_has_statistics(SC),
        (((SC)#sconf.flags band ?SC_STATISTICS) /= 0)).
-define(sc_has_tilde_expand(SC),
        (((SC)#sconf.flags band ?SC_TILDE_EXPAND) /= 0)).
-define(sc_has_dir_listings(SC),
        (((SC)#sconf.flags band ?SC_DIR_LISTINGS) /= 0)).
-define(sc_has_deflate(SC),
        (((SC)#sconf.flags band ?SC_DEFLATE) /= 0)).
-define(sc_has_dir_all_zip(SC),
        (((SC)#sconf.flags band ?SC_DIR_ALL_ZIP) /= 0)).
-define(sc_has_dav(SC),
        (((SC)#sconf.flags band ?SC_DAV) /= 0)).
-define(sc_fcgi_trace_protocol(SC),
        (((SC)#sconf.flags band ?SC_FCGI_TRACE_PROTOCOL) /= 0)).
-define(sc_fcgi_log_app_error(SC),
        (((SC)#sconf.flags band ?SC_FCGI_LOG_APP_ERROR) /= 0)).
-define(sc_forward_proxy(SC),
        (((SC)#sconf.flags band ?SC_FORWARD_PROXY) /= 0)).
-define(sc_auth_skip_docroot(SC),
        (((SC)#sconf.flags band ?SC_AUTH_SKIP_DOCROOT) /= 0)).
-define(sc_strip_undef_bindings(SC),
        (((SC)#sconf.flags band ?SC_STRIP_UNDEF_BINDINGS) /= 0)).

-define(sc_set_access_log(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_ACCESS_LOG, Bool)}).
-define(sc_set_auth_log(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_AUTH_LOG, Bool)}).
-define(sc_set_add_port(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_ADD_PORT, Bool)}).
-define(sc_set_statistics(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_STATISTICS, Bool)}).
-define(sc_set_tilde_expand(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_TILDE_EXPAND, Bool)}).
-define(sc_set_dir_listings(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_DIR_LISTINGS, Bool)}).
-define(sc_set_deflate(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_DEFLATE, Bool)}).
-define(sc_set_dir_all_zip(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_DIR_ALL_ZIP, Bool)}).
-define(sc_set_dav(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_DAV, Bool)}).
-define(sc_set_fcgi_trace_protocol(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_FCGI_TRACE_PROTOCOL,
                                   Bool)}).
-define(sc_set_fcgi_log_app_error(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_FCGI_LOG_APP_ERROR,
                                   Bool)}).
-define(sc_set_forward_proxy(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_FORWARD_PROXY, Bool)}).
-define(sc_set_auth_skip_docroot(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_AUTH_SKIP_DOCROOT,
                                   Bool)}).
-define(sc_set_strip_undef_bindings(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_STRIP_UNDEF_BINDINGS,
                                   Bool)}).


%% server conf
%% we cannot compare sconfs directly due to the ets field in #sconf{} use
%% yaws_config:eq_sconfs/2
-record(sconf, {
          port = 8000,                  % which port is this server listening to
          flags = ?SC_DEF,
          redirect_map=[],              % a list of
                                        % {Prefix, #url{}, append|noappend}
                                        % #url{} can be partially populated

          rhost,                        % forced redirect host (+ optional port)
          rmethod,                      % forced redirect method
          docroot,                      % path to the docs
          xtra_docroots = [],           % if we have additional pseudo docroots
          listen = [{127,0,0,1}],       % bind to this IP, {0,0,0,0} is possible
          servername = "localhost",     % servername is what Host: header is
          serveralias = [],             % Alternate names for this vhost
          yaws,                         % server string for this vhost
          ets,                          % local store for this server
          ssl,                          % undefined | #ssl{}
          authdirs = [],                % [{docroot, [#auth{}]}]
          partial_post_size = 10240,

          %% An item in the appmods list  can be either of the
          %% following, this is all due to backwards compat issues.
          %% 1.  an atom - this is the equivalent to {atom, atom}
          %% 2 . A two tuple {Path, Mod}
          %% 3 A three tuple {Path, Mod, [ExcludeDir ....]}
          appmods = [],

          expires = [],
          errormod_401 = yaws_outmod,   % the default 401 error module
          errormod_404 = yaws_outmod,   % the default 404 error module
          errormod_crash = yaws_outmod, % use the same module for crashes
          arg_rewrite_mod = yaws,
          logger_mod = yaws_log,        % access/auth logging module
          opaque = [],                  % useful in embedded mode
          start_mod,                    % user provided module to be started
          allowed_scripts = [yaws,php,cgi,fcgi],
          tilde_allowed_scripts = [],
          index_files = ["index.yaws", "index.html", "index.php"],
          revproxy = [],
          soptions = [{listen_opts, [{backlog, 1024}]}],
          extra_cgi_vars = [],
          stats,                        % raw traffic statistics
          fcgi_app_server,              % FastCGI application server {host,port}
          php_handler = {cgi, "/usr/bin/php-cgi"},
          shaper,
          deflate_options,              % undefined | #deflate{}
          mime_types_info,              % undefined | #mime_types_info{}
                                        % if undefined, global config is used
          dispatch_mod,                 % custom dispatch module
          extra_response_headers = []   % configured extra response headers
         }).


%% Auth conf - from server conf and .yaws_auth
-record(auth, {
          dir     = [],
          docroot = [],
          files   = [],
          realm   = "",
          type    = "Basic",
          headers = [],    % headers to send on 401
          users   = [],    % list of {User, Password} tuples
          acl     = none,  % list of allowed/denies IPs or none
          mod     = [],    % authentication module callback
          outmod  = [],    % module to handles 401 unauthorized messages
          pam     = false  % should we use pam to auth a user
         }).


%% Macro used to list default compressible mime-types
-define(DEFAULT_COMPRESSIBLE_MIME_TYPES, [
                                          {"text", all},
                                          {"application", "rtf"},
                                          {"application", "msword"},
                                          {"application", "postscript"},
                                          {"application", "pdf"},
                                          {"application", "x-dvi"},
                                          {"application", "javascript"}
                                         ]).

%% Internal record used to initialize a zlib stream for compression
-record(deflate, {
          min_compress_size = nolimit, % nolimit or non negative integer
                                       % (in bytes)
          compression_level = default, % none | default | best_compression |
                                       % best_speed | 0..9
          window_size       = -15,     % -15..-9
          mem_level         = 8,       % 1..9
          strategy          = default, % default | filtered | huffman_only
          use_gzip_static   = false,

          %% [{Type, undefined|SubType}] | all
          mime_types = ?DEFAULT_COMPRESSIBLE_MIME_TYPES
         }).


%% Internal record used to set information about mime-types
-record(mime_types_info, {
          mime_types_file, % an absolute filename path
          types    = [],   % a map between mime-types and extensions
          charsets = [],   % a map between charsets and extensions
          default_type = "text/plain",
          default_charset
         }).


%% this internal record is used and returned by the URL path parser
-record(urltype, {
          type,          % error | yaws | regular | directory | forbidden |
                         % appmod
          finfo,
          path     = [],
          fullpath = [], % deep list (WHY?)
          dir      = [], % relative dir where the path leads to
                         % flat | unflat need flat for authentication
          data,          % type-specific
                         % e.g: Binary | FileDescriptor | DirListing | undefined
          deflate,       % undefined | Binary | dynamic
          mime = "text/html",
          getpath,       % as GET'ed by client
          pathinfo
         }).




%% this record is constructed as we build up the outgoing headers
-record(outh, {
          status,              % int status code

          doclose,             % bool
          chunked,             % bool
          exceedmaxuses=false, % bool, true if hit keep-alive max uses
          encoding=decide,     % decide, identity, deflate
          contlen,             % integer
          act_contlen,         % actual content length for dynamic pages
                               % and the total set of out headers we can have as
                               % actual strings
          connection,
          server,
          location,
          cache_control,
          expires,
          date,
          allow,
          last_modified,
          etag,
          set_cookie,
          content_range,
          content_length,
          content_type,
          content_encoding,
          transfer_encoding,
          www_authenticate,
          vary,
          accept_ranges,
          other                % misc other headers
         }).


%% forward and reverse proxy config info
-record(proxy_cfg, {
          prefix,
          url,
          intercept_mod
         }).


%% as read by application:get_env()
-record(env, {
          debug,
          trace,
          traceoutput,
          conf,
          runmod,
          embedded,
          id,
          encoding=latin1
         }).

%% Typically used in error printouts as in:
%% error_logger:format("Err ~p at ~p~n", [Reason, ?stack()])
-ifdef(OTP_RELEASE).
-define(stack(), try throw(1) catch _:_:ST -> ST end).
-define(MAKE_ST(CATCH,STVAR,BODY), CATCH:STVAR -> BODY).
-else.
-define(stack(), try throw(1) catch _:_ -> (fun erlang:get_stacktrace/0)() end).
-define(MAKE_ST(CATCH,STVAR,BODY),
        CATCH -> STVAR = (fun erlang:get_stacktrace/0)(), BODY).
-endif.


%%% The following is for emacs, do not remove
%%% Local Variables:
%%% comment-column: 36
%%% End:
