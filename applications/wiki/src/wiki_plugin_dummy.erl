%%% File    : wiki_plugin_dummy.erl
%%% Author  : Mickael Remond <mickael.remond@erlang-fr.org>
%%% Description : Example "dummy" wiki plugin
%%% Created : 20 Oct 2003 by Mickael Remond
%%%                          <mickael.remond@erlang-fr.org>

-module(wiki_plugin_dummy).

-export([run/2]).

run(Page, ArgList) ->
    "<p>Using Dummy Wiki Plugin.</p>".
