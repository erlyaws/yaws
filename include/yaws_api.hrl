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
	  querydata,      %% Was the URL on the form of ....?query (GET reqs)
	  docroot         %% where's the data

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
	  authorization,
	  other = []   %% misc other headers
	 }).





