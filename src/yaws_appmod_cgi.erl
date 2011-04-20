%%% File    : yaws_appmod_cgi.erl
%%% Author  : Claes  Wikstrom <klacke@hyber.org>
%%% Description :
%%% Created : 10 Mar 2008 by Claes  Wikstrom <klacke@hyber.org>

-module(yaws_appmod_cgi).
-export([out/1]).
-include("../include/yaws_api.hrl").

out(Arg) ->
    yaws_cgi:call_cgi(Arg,  lists:flatten(Arg#arg.fullpath)).



