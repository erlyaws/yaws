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
	  server_path,    %% The normalized server path
	  querydata,      %% Was the URL on the form of ...?query (GET reqs)
	  appmoddata,     %% the remainder of the path leading up to the querey
	  docroot,        %% where's the data
	  fullpath,       %% full path to yaws file
	  cont,		  %% Continuation for chunked multipart uploads
	  state,          %% State for use by users of the out/1 callback
	  pid,            %% pid of the yaws worker process
	  opaque,         %% useful to pass static data
	  appmod_prepath, %% path in front of: <appmod><appmoddata>
	  pathinfo        %% Set to 'd/e' when calling c.yaws for the request
                          %% http://some.host/a/b/c.yaws/d/e
	 }).              


-record(http_request, {method,
		       path,
		       version}).

-record(http_response, {version,
			status,
			phrase}).

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
	  location,
	  content_length,
	  content_type,
	  authorization,
	  transfer_encoding,
	  other = []   %% misc other headers
	 }).




-record(url,
	{scheme,
	 host, 
	 port,            %% undefined means not set
	 path = [],
	 querypart = []}).


-record(setcookie,{
	    key,           
	    value,         
	    quoted,        
	    comment,
	    comment_url,
	    discard,
	    domain,
	    max_age,
	    expires,
	    path,
	    port,
	    secure,
	    version}).


%% Search a var, find it or crash

-define(VAR(__ARG, __Key),
	if (__ARG#arg.req)#http_request.method == 'GET' ->
		{value,{_,__Val}}=lists:keysearch(__Key,1,yaws_api:parse_query(__ARG)), __Val;
	   (__ARG#arg.req)#http_request.method == 'POST' ->
		{value,{_,__Val}}=lists:keysearch(__Key,1,yaws_api:parse_post_data(__ARG)), __Val
	end).


%% Search a var: return {ok, Val} | false
-define(SVAR(__ARG, __Key),
	if (__ARG#arg.req)#http_request.method == 'GET' ->
		case lists:keysearch(__Key,1,yaws_api:parse_query(__ARG)) of
		    {value,{_,__Val}} -> {ok, __Val};
		    false -> false
		end;
	   (__ARG#arg.req)#http_request.method == 'POST' ->
		case lists:keysearch(__Key,1,yaws_api:parse_post_data(__ARG)) of
		    {value,{_,__Val}} ->
			{ok, __Val};
		    false ->
			false
		end
	end).

	   
