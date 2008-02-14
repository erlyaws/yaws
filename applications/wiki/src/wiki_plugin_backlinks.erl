%%% File    : wiki_plugin_backlinks.erl
%%% Author  : Mickael Remond <mickael.remond@erlang-fr.org>
%%% Description : This plugin can show the list of backlinks inline
%%% Created : 20 Oct 2003 by Mickael Remond
%%%                          <mickael.remond@erlang-fr.org>

-module(wiki_plugin_backlinks).

-export([run/2]).

-include("yaws.hrl").   %% Needed only if you want to manipulate
                        %% Yaws configuration

run(Page, ArgList) ->
    %% TODO: Fixme
    %% This is working if there is only one virtual server.
    %% A way to handle this cleanly is needed.
    {ok, Gconf, [[Sconf|Others]]} = yaws_api:getconf(),
    Root = Sconf#sconf.docroot,

    AllRefs = wiki_utils:getallrefs(Page, Root),
    
    lists:map(fun(F) -> 
                [wiki_to_html:format_link(F, Root),"<br>"] end, 
        AllRefs).

