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


