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
	       default_type = "text/html",
	       timeout = 2000,
	       include_dir = [],
	       yaws   %% FIXME add version here 
	         }).  %% a list of lists of #sconfs
                      %% one list of #sconf's per listen ip


%% server conf
-record(sconf,
	{port = 8000,
	 ssl = off,
	 ssl_port = 443,
	 docroot,
	 access_log = true,
	 listen = {127,0,0,1},
	 servername = "localhost",
	 default_server_on_this_ip = true,
	 ets,
	 authdirs = []
	}).









