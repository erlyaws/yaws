%%%----------------------------------------------------------------------
%%% File    : yaws.hrl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 16 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-author('klacke@hyber.org').
-include_lib("yaws/include/yaws_api.hrl").
-include("yaws_debug.hrl").

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
	       cache_refresh_secs = 30,  % seconds
	       default_type = "text/html",
	       timeout = 2000,
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
	{port = 8000,
	 docroot,
	 access_log = true,
	 listen = {127,0,0,1},
	 servername = "localhost",
	 ets,
	 ssl,
	 authdirs = [],
	 partial_post_size = nolimit
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
