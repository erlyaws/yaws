%%%----------------------------------------------------------------------
%%% File    : webmail.hrl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 25 Jun 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-author('klacke@hyber.org').


-record(s, {user,
	    cookie,
	    pops,
	    logintime,
	    idletimer}).


-record(mail, {popnum,
	       size,
	       return_path,
	       received,
	       date,
	       from,
	       to,
	       cc,
	       subject = "none",
	       message_id,
	       mime_version,
	       content_type,
	       content_disposition,
	       user_agent,
	       content_transfer_encoding,
	       other_h = [],
	       content}).
