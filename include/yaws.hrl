%%%----------------------------------------------------------------------
%%% File    : yaws.hrl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 16 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-author('klacke@hyber.org').



%% flags for gconfs 
-define(GC_TTY_TRACE,   1).
-define(GC_DEBUG,       2).
-define(GC_AUTH_LOG,    4).
-define(GC_COPY_ERRLOG, 8).
-define(GC_BACKWARDS_COMPAT_PARSE, 16).

-define(GC_DEF, ?GC_AUTH_LOG).

-define(gc_has_tty_trace(GC), 
	((GC#gconf.flags band ?GC_TTY_TRACE) /= 0)).
-define(gc_has_debug(GC), 
	((GC#gconf.flags band ?GC_DEBUG) /= 0)).
-define(gc_has_auth_log(GC), 
	((GC#gconf.flags band ?GC_AUTH_LOG) /= 0)).
-define(gc_has_copy_errlog(GC), 
	((GC#gconf.flags band ?GC_COPY_ERRLOG) /= 0)).
-define(gc_has_backwards_compat_parse(GC), 
	((GC#gconf.flags band ?GC_BACKWARDS_COMPAT_PARSE) /= 0)).

-define(gc_set_tty_trace(GC, Bool), 
	GC#gconf{flags = yaws:flag(GC#gconf.flags,?GC_TTY_TRACE, Bool)}).
-define(gc_set_debug(GC, Bool), 
	GC#gconf{flags = yaws:flag(GC#gconf.flags, ?GC_DEBUG, Bool)}).
-define(gc_set_auth_log(GC, Bool), 
	GC#gconf{flags = yaws:flag(GC#gconf.flags, ?GC_AUTH_LOG, Bool)}).
-define(gc_set_copy_errlog(GC, Bool), 
	GC#gconf{flags = yaws:flag(GC#gconf.flags, ?GC_COPY_ERRLOG, Bool)}).
-define(gc_set_backwards_compat_parse(GC, Bool), 
	GC#gconf{flags = yaws:flag(GC#gconf.flags, 
				   ?GC_BACKWARDS_COMPAT_PARSE, Bool)}).



%% global conf
-record(gconf,{yaws_dir,
	       trace,
	       flags = ?GC_DEF,
	       logdir,
	       ebin_dir = [],
	       runmods = [],
	       keepalive_timeout = 15000,
	       max_num_cached_files = 400,
	       max_num_cached_bytes = 1000000,  %% 1 MEG
	       max_size_cached_file = 8000,
	       large_file_chunk_size = 10240,
	       cache_refresh_secs = 30,  % seconds  (auto zero when debug)
	       default_type = "text/html",
	       include_dir = [],
	       yaws,                %% server string
	       username,            %% maybe run as a different user than root
	       uid,                 %% unix uid of user that started yaws
	       id = "default"       %% string identifying this instance of yaws
	      }).  



-record(ssl, 
	{
	 keyfile,
	 certfile,
	 verify = 0,
	 depth = 1,
	 password,
	 cacertfile,
	 ciphers,
	 cachetimeout}).


%% flags for sconfs
-define(SC_ACCESS_LOG,   1).
-define(SC_ADD_PORT,     2).
-define(SC_TILDE_EXPAND, 8).
-define(SC_DIR_LISTINGS, 16).
-define(SC_DEFLATE,      32).


-define(SC_DEF, ?SC_ACCESS_LOG bor ?SC_ADD_PORT).

-define(sc_has_access_log(SC), 
	((SC#sconf.flags band ?SC_ACCESS_LOG) /= 0)).
-define(sc_has_add_port(SC), 
	((SC#sconf.flags band ?SC_ADD_PORT) /= 0)).
-define(sc_has_tilde_expand(SC),
	((SC#sconf.flags band ?SC_TILDE_EXPAND) /= 0)).
-define(sc_has_dir_listings(SC),
	((SC#sconf.flags band ?SC_DIR_LISTINGS) /= 0)).
-define(sc_has_deflate(SC), 
	((SC#sconf.flags band ?SC_DEFLATE) /= 0)).


-define(sc_set_access_log(SC, Bool), 
	SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_ACCESS_LOG, Bool)}).
-define(sc_set_add_port(SC, Bool), 
	SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_ADD_PORT, Bool)}).
-define(sc_set_ssl(SC, Bool), 
	SC#sconf{flags = yaws:flag(SC#sconf.flags , ?SC_SSL, Bool)}).
-define(sc_set_tilde_expand(SC, Bool), 
	SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_TILDE_EXPAND, Bool)}).
-define(sc_set_dir_listings(SC, Bool), 
	SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_DIR_LISTINGS, Bool)}).
-define(sc_set_deflate(SC, Bool), 
	SC#sconf{flags = yaws:flag(SC#sconf.flags, ?SC_DEFLATE, Bool)}).



%% server conf
-record(sconf,
	{port = 8000,                %% which port is this server listening to
	 flags = ?SC_DEF,
	 rhost,                      %% forced redirect host (+ optional port)
	 rmethod,                    %% forced redirect method
	 docroot,                    %% path to the docs
	 listen = {127,0,0,1},       %% bind to this IP, {0,0,0,0} is possible
	 servername = "localhost",   %% servername is what Host: header is
	 ets,                        %% local store for this server
	 ssl,
	 authdirs = [],
	 partial_post_size = nolimit,
	 appmods = [],                %% list of modules for this app
	 errormod_404 = yaws_404,     %% the default 404 error module 
	 errormod_crash = yaws_404,   %% use the same module for crashes
	 arg_rewrite_mod = yaws,
	 opaque = [],                 %% useful in embedded mode
	 start_mod,                   %% user provided module to be started
	 allowed_scripts = [yaws],
	 phpexe = "php",
	 revproxy = []
	}).


% Auth conf - from server conf and .yaws_auth
-record(auth,
         {dir = [],
          realm = "",
          type = "Basic",
          users = []
         }).



%% this internal record is used and returned by the URL path parser

-record(urltype, {type,   %% error | yaws | regular | directory | 
                          %% forbidden | appmod
		  finfo,
		  path,
		  fullpath, %% deep list
		  dir,      %% relative dir where the path leads to
		            %% flat | unflat need flat for authentication
		  data,     %% Binary | FileDescriptor | DirListing | undefined
		  deflate,  %% undefined | Binary | dynamic
		  mime = "text/html",    %% MIME type
		  getpath,  %% as GET'ed by client
		  pathinfo
		 }).




%% this record is constructed as we build up
%% the outgoing headers

-record(outh, {
	  status,        %% int status code

	  doclose,       %% bool
	  chunked,       %% bool
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


	  
-define(READ_TIMEOUT, 30000).




-record(appmodspec, {
	  type,  %% atom, pair or absolute
	  data}).

