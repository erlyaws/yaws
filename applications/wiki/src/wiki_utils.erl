-module(wiki_utils).

%% File    : wiki_utils.erl
%% Author  : Joe Armstrong (joe@bluetail.com)
%%         : Johan Bevemyr (jb@bluetail.com)
%%         : Mickael Remond (mickael.remond@erlang-fr.org)
%% Purpose : Wiki web utilities
%%         : Find zombie pages
%%         : Find all references to a given page

-export([findallrefsto/2, zombies/1]).
-export([getallrefs/2]).
-export([getpages_by_prefix/2]).

-import(lists,  [filter/2, member/2, reverse/1, sort/1, map/2]).
-import(wiki, [p/1, h1/1, show/1]).
-import(wiki_templates, [template2/5]).

%% HTML structure of the backlink list
findallrefsto(Page, Root) ->
    Pages = getallrefs(Page, Root),
    template2(Root, "References",  "References",
         ["<p>The following pages contain references to ",
          wiki_to_html:format_link(Page, Root),".",
          "<ul>",
          map(fun(F) -> 
                      [wiki_to_html:format_link(F, Root),"<br>"] end, 
              Pages),
          "</ul>"], false). 

%% Backlinks list
getallrefs(Page, Root) ->
    All = wiki:ls(Root),
    Pages = filter(fun(I) ->
                           case wiki:read_page(I, Root) of
                               {ok, Str} ->
                                   Links = get_links(Str, []),
                                   member(Page, Links);
                               error ->
                                   false
                           end
                   end, All),
    sort(Pages).

zombies(Root) -> 
    All = wiki:ls(Root),
    {Reached, Missing} = gc(["home"], [], [], Root),
    %% Missing = Pages refered to but do not exists at all
    %% This is not an error
    NotReached = sort(All -- Reached),
    template2(Root, "Zombies", "Zombies",
         [p("These pages have no links to them."),
          "<ul>",
          map(fun(F) -> 
                      [wiki_to_html:format_link(F, Root),"<br>"] end, 
              NotReached),
          "</ul>"], false).

%% Return page name that match a specific prefix
getpages_by_prefix(Prefix, Root) ->
    Files = utils:fold_files(Root, Prefix ++ ".*\.wob", false,
                         fun(F, AccIn)-> [F|AccIn] end, []),
    Pages = lists:map(fun(I) -> filename:basename(I, ".wob") end,
                      Files),
    sort(Pages).                      

    
gc([H|T], Visited, Missing, Root) ->
    case member(H, Visited) or member(H, Missing) of
        true ->
            gc(T, Visited, Missing, Root);
        false ->
            case wiki:read_page(H, Root) of
                {ok, Str} ->
                    Links = get_links(Str, []),
                    gc(Links ++ T, [H|Visited], Missing, Root);
                error ->
                    gc(T, Visited, [H|Missing], Root)
            end
    end;
gc([], Visited, Missing, _Root) ->
    {Visited, Missing}.

get_links([$\\,C|T], L) -> get_links(T, L);
get_links([$~|T], L) ->
    {Link, T1} = wiki_format_txt:collect_wiki_link(T),
    get_links(T1, [Link|L]);
get_links([_|T], L) ->
    get_links(T, L);
get_links([], L) ->
    L.



