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
	       trace,
	       debug,
	       logdir,
	       ebin_dir = [],
	       keepalive_timeout = 15000,
	       max_num_cached_files = 400,
	       max_num_cached_bytes = 1000000,  %% 1 MEG
	       max_size_cached_file = 8000,
	       large_file_chunk_size = 10240,
	       cache_refresh_secs = 30,  % seconds
	       default_type = "text/html",
	       timeout = 30000,
	       include_dir = [],
	       yaws,
	       uid                  %% unix uid of user running yaws
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
	{port = 8000,                %% which port is this server lsitenintg to
	 docroot,                    %% path to the docs
	 access_log = true,          %% log acces 
	 listen = {127,0,0,1},       %% bind to this IP, {0,0,0,0} is possible
	 servername = "localhost",   %% servername is what Host: header is
	 ets,                        %% local store for this server
	 ssl,
	 authdirs = [],
	 partial_post_size = nolimit,
	 appmods = [],                %% list of modules for this app
	 tilde_expand = true          %% allow public_html user dirs
	}).


% Auth conf - from server conf and .yaws_auth
-record(auth,
         {dir = [],
          realm = "",
          type = "Basic",
          users = []
         }).


-record(dcc, {
	  doclose = true,
	  chunked = false}).
