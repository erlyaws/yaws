%%%-------------------------------------------------------------------
%%% File    : attachment.erl
%%% Author  :  <klacke@hyber.org>
%%% Description : 
%%%
%%% Created :  4 Feb 2004 by  <klacke@hyber.org>
%%%-------------------------------------------------------------------
-module(attachment).
-compile(export_all).

out(A) ->
    case mail:check_session(A) of
	{ok, Session} ->
	    L = yaws_api:parse_query(A),
	    {value, {_,Nr}} = lists:keysearch(nr,1,L),
	    mail:send_attachment(Session, list_to_integer(Nr));
	Error ->
	    Error
    end.



