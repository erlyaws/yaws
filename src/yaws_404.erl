%%%----------------------------------------------------------------------
%%% File    : yaws_404.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created :  4 Nov 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_404).
-author('klacke@hyber.org').


-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").

-export([out404/3,
	 crashmsg/3]).


%% The default error 404 error delivery module
%% This function can be used to generate
%% a special page on 404's (it doesn't even have to be a 404)


out404(Arg, GC, SC) ->
    yaws_api:set_status_code(404),
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    B = not_found_body(Path, GC, SC),
    {html, B}.



not_found_body(Path, GC, SC) ->
    L = ["<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">"
	 "<HTML><HEAD>"
	 "<TITLE>404 Not Found</TITLE>"
	 "</HEAD><BODY>"
	 "<H1>Not Found</H1>"
	 "The requested URL ", 
	 Path, 
	 " was not found on this server.<P>"
	 "<HR>",
	 yaws:address(GC, SC),
	 "  </BODY></HTML>"
	],
    list_to_binary(L).




%% possibility to customize crash messages, 

% while developing
%% it's extremely convenient to get the crash messages in the browser,
%% however not in production :-)

crashmsg(A, SC, L) ->
    {ehtml,
     [{h2, [], "Internal error, yaws code crashed"},
      {br},
      {hr},
      {pre, [], L},
      {hr}]}.
