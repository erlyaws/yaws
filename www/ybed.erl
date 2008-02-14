%%%-------------------------------------------------------------------
%%% File    : ybed.erl
%%% Author  :  <klacke@hyber.org>
%%% Description : Small embedded yaws example
%%%
%%% Created : 25 Nov 2003 by  <klacke@hyber.org>
%%%-------------------------------------------------------------------
-module(ybed).
-compile(export_all).

-include("/usr/local/lib/yaws/include/yaws.hrl").

start() ->
    application:start(yaws),
    GC = yaws_config:make_default_gconf(false),
    SC = #sconf{port = 8888,
                servername = "foobar",
                listen = {0,0,0,0},
                docroot = "/tmp"},
    yaws_api:setconf(GC, [[SC]]).

                

    

