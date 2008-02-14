-module(wiki_split).

%% File    : wiki_format_txt.erl
%% Author  : Joe Armstrong (joe@bluetail.com)
%%         : Johan Bevemyr, minor modifications
%% Purpose : Wiki formatting engine
%%
%% Split the text. Looking for blocks
%%      <         is represented as {open,Tag,string()}
%%        ...
%%      >
%%      <         is represented as {write_append,Tag,string()}
%%        ...
%%      >
%%
%%      Everything else is represented as {txt, Tag, string()}

%% +type str2wiki(string())                             -> wikiText().
%% +type wiki2str(wikiText())                           -> string().
%% +type getRegion(tag(), wikiText())                   -> string().
%% +type putRegion(tag(), wikiText(), string())         -> wikiText().
%% +type writeAppendRegion(tag(), wikiText(), string()) -> wikiText().

%% +deftype wikiText() = {wik, [{text,tag(),string()} |
%%                              {open,tag(),string()} |
%%                              {write_append, tag(), string()}]}
%% +deftype tag() = int(). 

-export([str2wiki/1, wiki2str/1, 
         getRegion/2, putRegion/3, writeAppendRegion/3]).

-import(lists, [reverse/1]).

str2wiki(Str) ->
    Blocks = str2wiki(Str, []),
    {wik, number_blocks(Blocks, 1)}.

number_blocks([{txt,[]}|T], N)  -> number_blocks(T, N);
number_blocks([{Tag,Str}|T], N) -> [{Tag,N,Str}|number_blocks(T, N+1)];
number_blocks([], _)            -> [].

str2wiki(Str, L) ->
    {Before, Stuff} = collect_str(Str),
    case Stuff of
        "<<\n" ++ T -> 
            {In, Str3} = collect_write_append([$\n|T], []),
            str2wiki(Str3, [{write_append,In},{txt,Before}|L]);
        "<\n" ++ T -> 
            {In, Str3} = collect_open_region([$\n|T], []),
            str2wiki(Str3, [{open,In},{txt,Before}|L]);
        [] ->
            reverse([{txt,Before}|L])
    end.

%% collect_str(Str) -> {Str1, Str2}
%% where Str2 == [], "<" ++ _ | "<<" ++ _
    
collect_str(Str) -> collect_after_nl(Str, []).

collect_after_nl(S = "<<\n" ++ _, L) -> {reverse(L), S};
collect_after_nl(S = "<\n" ++ _, L)  -> {reverse(L), S};
collect_after_nl(X, L)               -> collect_str(X, L).

collect_str([$\n|T], L) -> collect_after_nl(T, [$\n|L]);
collect_str([H|T], L)   -> collect_str(T, [H|L]);
collect_str([], L)      -> {reverse(L), []}.

collect_write_append("\n>>\n" ++ T, L) -> {reverse([$\n|L]), [$\n|T]};
collect_write_append([H|T], L)         -> collect_write_append(T, [H|L]);
collect_write_append([], L)            -> {reverse(L), []}.

collect_open_region("\n>\n" ++ T, L) -> {reverse([$\n|L]), [$\n|T]};
collect_open_region([H|T], L)        -> collect_open_region(T, [H|L]);
collect_open_region([], L)           -> {reverse(L), []}.

%% wiki2str.

wiki2str({wik,L})                  -> sneaky_flatten(wiki2str1(L)).

wiki2str1([{txt,_,Str}|T])          -> [Str|wiki2str1(T)];
wiki2str1([{open,_,Str}|T])         -> ["<\n",Str,"\n>"|wiki2str1(T)];
wiki2str1([{write_append,_,Str}|T]) -> ["<<\n",Str,"\n>>"|wiki2str1(T)];
wiki2str1([])                       -> [].
    
sneaky_flatten(L) ->
    binary_to_list(list_to_binary(L)).

getRegion(Tag, {wik, L}) -> getRegion1(Tag, L).

getRegion1(Tag, [{Type,Tag,Str}|_]) -> {Type, Str};
getRegion1(Tag, [_|T])              -> getRegion1(Tag, T).
    
putRegion(Tag, {wik, L}, Str1) -> {wik, putRegion1(Tag, L, Str1)}.

putRegion1(Tag, [{Type,Tag,_}|T], New) -> [{Type,Tag,New}|T];
putRegion1(Tag, [H|T], New)            -> [H|putRegion1(Tag, T, New)].

writeAppendRegion(Tag, Wik, Str) ->
    Str1 = getRegion(Tag, Wik),
    putRegion(Tag, Wik, Str ++ Str1).








