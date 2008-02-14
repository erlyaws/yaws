-module(wiki_plugin_menu).

%%% File    : wiki_plugin_menu.erl
%%% Author  : Mickal Remond <mickael.remond@erlang-fr.org>
%%% Description : Allows the selection and display of a menu Links are
%%%               selected base on first word of the page name (Menu,
%%%               Category, ...). The other part of the title is the
%%%               name of the menu entry.
%%% Created : 22 Oct 2003 by Mickael Remond
%%%                          <mickael.remond@erlang-fr.org>

-export([run/2]).

-include("yaws.hrl").   %% Needed only if you want to manipulate
                        %% Yaws configuration

run(_Page, ArgList) ->
    %% TODO: Fixme
    %% This is working if there is only one virtual server.
    %% A way to handle this cleanly is needed.
    {ok, Gconf, [[Sconf|Others]]} = yaws_api:getconf(),
    Root = Sconf#sconf.docroot,

    Prefix = get_prefix(ArgList),

    %% Get all page starting with a given word
    Pages = wiki_utils:getpages_by_prefix(Prefix, Root),
    
    lists:map(fun(F) -> 
                [wiki_to_html:format_menu_link(Prefix, F, Root),"<br>"] end, 
        Pages).


%% Get the category to use in the menu If not passed as parameter, use
%% "Category"
%% Be careful: plugin syntax is for the moment case sensitive
get_prefix(ArgList) ->
    case lists:keysearch("prefix", 1, ArgList) of
        {value, {"prefix", Prefix}} ->
            Prefix;
        _Other ->
            "Category"
    end.

%% TODO: is it relevant to be able to handle several category in
%% the menu ?
