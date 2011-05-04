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
-define(GC_USE_OLD_SSL,                    256).



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
-define(gc_use_old_ssl(GC),
        ((GC#gconf.flags band ?GC_USE_OLD_SSL) /= 0)).

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
-define(gc_set_use_old_ssl(GC, Bool),
        GC#gconf{flags = yaws:flag(GC#gconf.flags,?GC_USE_OLD_SSL,Bool)}).



%% global conf
-record(gconf,{yaws_dir,           %% topdir of Yaws installation
               trace,              %% false | {true,http}|{true,traffic}
               flags = ?GC_DEF,    %% boolean flags
               logdir,
               ebin_dir = [],
               runmods = [],       %% runmods for entire server
               keepalive_timeout = 30000,
               keepalive_maxuses = nolimit, %% nolimit or non negative integer
               max_num_cached_files = 400,
               max_num_cached_bytes = 1000000,  %% 1 MEG
               max_size_cached_file = 8000,
               max_connections = nolimit, % max number of TCP connections
               process_options = [],  % Override default connection handler
                                    % processes spawn options for
                                    % performance/memory tuning.  [] |
                                    % [{fullsweep_after,Number},
                                    % {min_heap_size, Size}] other options such
                                    % as monitor, link are ignored.
               large_file_chunk_size = 10240,
               mnesia_dir = [],
               log_wrap_size = 10000000,  % wrap logs after 10M
               cache_refresh_secs = 30,  % seconds  (auto zero when debug)
               include_dir = [],    %% list of inc dirs for .yaws files
               phpexe = "/usr/bin/php-cgi",  %% cgi capable php executable
               x_forwarded_for_log_proxy_whitelist = [], % list of proxy server
                                    % ips we will replace with the last element
                                    % of the list in the X-Forwarded-For http
                                    % header in logs
               yaws,                %% server string
               id = "default",      %% string identifying this instance of yaws
               enable_soap = false, %% start yaws_soap_srv iff true
               soap_srv_mods = [],  %% a list of
                                    %% {{Mod, Func}, WsdlFile, Prefix } |
                                    %%        {{Mod, Func}, WsdlFile}
                                    %% automatically setup in yaws_soap_srv init.
               ysession_mod = yaws_session_server %% storage module for ysession
              }).



-record(ssl,
        {
         keyfile,
         certfile,
         verify = 0,
         fail_if_no_peer_cert,
         depth = 1,
         password,
         cacertfile,
         ciphers,
         cachetimeout}).


%% flags for sconfs
-define(SC_ACCESS_LOG,          1).
-define(SC_AUTH_LOG,            2).
-define(SC_ADD_PORT,            4).
-define(SC_STATISTICS,          8).
-define(SC_TILDE_EXPAND,        16).
-define(SC_DIR_LISTINGS,        32).
-define(SC_DEFLATE,             64).
-define(SC_DIR_ALL_ZIP,         128).
-define(SC_DAV,                 512).
-define(SC_FCGI_TRACE_PROTOCOL, 1024).
-define(SC_FCGI_LOG_APP_ERROR,  2048).
-define(SC_FORWARD_PROXY,       4096).
-define(SC_AUTH_SKIP_DOCROOT,   8192).



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


-define(sc_set_access_log(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_ACCESS_LOG, Bool)}).
-define(sc_set_auth_log(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_AUTH_LOG, Bool)}).
-define(sc_set_add_port(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_ADD_PORT, Bool)}).
-define(sc_set_statistics(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_STATISTICS, Bool)}).
-define(sc_set_ssl(SC, Bool),
        SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_SSL, Bool)}).
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
        SC#sconf{flags = yaws:flag(SC#sconf.flags,?SC_AUTH_SKIP_DOCROOT,Bool)}).



%% server conf
-record(sconf,
        {port = 8000,                %% which port is this server listening to
         flags = ?SC_DEF,
         redirect_map=[],            %% a list of
                                     %% {Prefix, #url{}, append|noappend}
                                     %% #url{} can be partially populated

         rhost,                      %% forced redirect host (+ optional port)
         rmethod,                    %% forced redirect method
         docroot,                    %% path to the docs
         xtra_docroots = [],         %% if we have additional pseudo docroots
         listen = [{127,0,0,1}],     %% bind to this IP, {0,0,0,0} is possible
         servername = "localhost",   %% servername is what Host: header is
         ets,                        %% local store for this server
         ssl,                        %% undefined | #ssl{}
         authdirs = [],
         partial_post_size = 10240,
         appmods = [],
         %%  An item in the appmods list  can be either of the
         %% following, this is all due to backwards compat issues.
         %% 1.  an atom - this is the equivalent to {atom, atom}
         %% 2 . A two tuple {Path, Mod}
         %% 3 A three tuple {Path, Mod, [ExcludeDir ....]}

         expires = [],
         errormod_401 = yaws_outmod,     %% the default 401 error module
         errormod_404 = yaws_outmod,     %% the default 404 error module
         errormod_crash = yaws_outmod,   %% use the same module for crashes
         arg_rewrite_mod = yaws,
         logger_mod = yaws_log,          %% access/auth logging module
         opaque = [],                    %% useful in embedded mode
         start_mod,                      %% user provided module to be started
         allowed_scripts = [yaws,php,cgi,fcgi],
         tilde_allowed_scripts = [],
         revproxy = [],
         soptions = [],
         extra_cgi_vars = [],
	 stats,                       %% raw traffic statistics
         fcgi_app_server,             %% FastCGI application server {host,port}
         php_handler = {cgi, "/usr/bin/php-cgi"},
         shaper
        }).

%% we cannot compare sconfs directly due to the ets
%% field in #sconf{} use yaws_config:eq_sconfs/2


% Auth conf - from server conf and .yaws_auth
-record(auth,
         {dir = [],
          docroot = [],
          files = [],
          realm = "",
          type = "Basic",
          headers = [],  %% headers to send on 401
          users = [],   %% list of {User, Password} tuples
          acl = none,    %% list of allowed/denies IPs or none
          mod = [],     %% authentication module callback
          outmod = [],   %% module to handles 401 unauthorized messages
          pam = false   %% should we use pam to auth a user
         }).



%% this internal record is used and returned by the URL path parser


-record(urltype, {type,   %% error | yaws | regular | directory |
                          %% forbidden | appmod
                  finfo,
                  path = [],
                  fullpath = [], %% deep list (WHY?)
                  dir = [],      %% relative dir where the path leads to
                                 %% flat | unflat need flat for authentication
                  data,          %% type-specific e.g: Binary | FileDescriptor
                                 %% | DirListing | undefined
                  deflate,       %% undefined | Binary | dynamic
                  mime = "text/html",    %% MIME type
                  getpath,       %% as GET'ed by client
                  pathinfo
                 }).




%% this record is constructed as we build up
%% the outgoing headers

-record(outh, {
          status,        %% int status code

          doclose,       %% bool
          chunked,       %% bool
          exceedmaxuses=false,
                         %% bool, true if hit keep-alive max uses
          encoding=identity,
                         %% identity, deflate
          contlen,       %% integer
          act_contlen,   %% actual content length for dynamic pages


                         %% and the total set of out headers we can have
                         %% as actual strings
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
          other     %% misc other headers
}).






%% as read by application:get_env()
-record(env, {debug,
              trace,
              traceoutput,
              conf,
              runmod,
              embedded,
              id
             }).

%% Typically used in error printouts as in:
%% error_logger:format("Err ~p at ~p~n", [Reason, ?stack()])
-define(stack(), try throw(1) catch _:_ -> erlang:get_stacktrace() end).


%%% The following is for emacs, do not remove
%%% Local Variables:
%%% comment-column: 36
%%% End:
