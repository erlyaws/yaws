%%%----------------------------------------------------------------------
%%% File    : yaws_api.hrl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 24 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-author('klacke@hyber.org').


-record(arg, {
	  clisock,        %% the socket leading to the peer client
	  headers,        %% headers
	  req,            %% request
	  clidata,        %% The client data (as a binary in POST requests)
	  querydata,      %% Was the URL on the form of ...?query (GET reqs)
	  appmoddata,     %% the remainder of the path leading up to the querey
	  docroot,        %% where's the data
	  fullpath,       %% full path to yaws file
	  cont,		  %% Continuation for chunked multipart uploads
	  state,          %% State for use by users of the out/1 callback
	  pid,            %% pid of the yaws worker process
	  opaque,         %% useful to pass static data
	  appmod_prepath  %% path in front of: <appmod><appmoddata>
	 }).              


-record(http_request, {method,
		       path,
		       version}).

	    
-record(headers, {
	  connection,
	  accept,
	  host,
	  if_modified_since,
	  if_match,
	  if_none_match,
	  if_range,
	  if_unmodified_since,
	  range,
	  referer,
	  user_agent,
	  accept_ranges,
	  cookie = [],
	  keep_alive,
	  content_length,
	  content_type,
	  authorization,
	  other = []   %% misc other headers
	 }).




-record(url,
	{scheme,
	 host, 
	 port,            %% undefined means not set
	 path = [],
	 querypart = []}).
