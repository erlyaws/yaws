%%% File        : yaws_appmod_fcgi.erl
%%% Author      : Bruno Rijsman <brunorijsman@hotmail.com>
%%% Description : Application module for FastCGI virtual paths.
%%% Created     : 9 Jul 2009

-module(yaws_appmod_fcgi).
-export([out/1]).
-include("../include/yaws_api.hrl").

out(Arg) ->
    yaws_cgi:call_fcgi_responder(Arg).



