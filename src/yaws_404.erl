%%%----------------------------------------------------------------------
%%% File    : yaws_404.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created :  4 Nov 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_404).
-author('klacke@hyber.org').

-compile(export_all).
-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").

%% The default error 404 error delivery module

out(Arg, GC, SC) ->
    yaws_api:set_status_code(404),
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    B = not_found_body(Path, GC, SC),
    io:format("In 404 \n",[]),
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



