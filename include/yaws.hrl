%%%----------------------------------------------------------------------
%%% File    : yaws.hrl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 16 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-author('klacke@hyber.org').


%% global conf
-record(gconf,{file,
	       yaws_dir,
	       tty_trace = false,
	       trace,
	       debug,
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
	       timeout = 30000,
	       include_dir = [],
	       yaws,                %% server string
	       username,            %% maybe run as a different user than root
	       uid                  %% unix uid of user that started yaws
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




%% a list of lists of #sconfs
%% one list of #sconf's per listen ip


%% server conf
-record(sconf,
	{port = 8000,                %% which port is this server listening to
	 rhost,                      %% forced redirect host (+ optional port)
	 rmethod,                    %% forced redirect method
	 docroot,                    %% path to the docs
	 access_log = true,          %% log acces 
	 listen = {127,0,0,1},       %% bind to this IP, {0,0,0,0} is possible
	 servername = "localhost",   %% servername is what Host: header is
         add_port = true,            %% add port after reading config
	 ets,                        %% local store for this server
	 ssl,
	 authdirs = [],
	 partial_post_size = nolimit,
	 appmods = [],                %% list of modules for this app
	 errormod_404 = yaws_404,     %% the default 404 error module 
	 errormod_crash = yaws_404,   %% use the same module for crashes
	 arg_rewrite_mod = yaws,
	 tilde_expand = false,        %% allow public_html user dirs
	 dir_listings = false,        %% allow dir listings
	 opaque = [],                 %% useful in embedded mode
	 start_mod,                   %% user provided module to be started
	 allowed_scripts = [yaws],
	 revproxy = [{"/tmp/", yaws_api:parse_url("http://localhost/")}]
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
		  fullpath,
		  dir,     %% relative dir where the path leads to
		           %% flat | unflat need flat for authentication
		  data,    %% Binary | FileDescriptor | DirListing | undefined
		  mime = "text/html",    %% MIME type
		  pathinfo
		 }).




%% this record is constructed as we build up
%% the outgoing headers

-record(outh, {
	  status,        %% int status code

	  doclose,       %% bool
	  chunked,       %% bool
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
	  transfer_encoding,
	  www_authenticate,
	  other     %% misc other headers
}).


	  
