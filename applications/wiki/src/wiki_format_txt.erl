-module(wiki_format_txt).

%% File    : wiki_format_txt.erl
%% Author  : Joe Armstrong (joe@bluetail.com)
%%         : Johan Bevemyr, minor modifications (jb@bevemyr.com)
%% Purpose : Wiki formatting engine
%%
%% Rules:  
%% Inline annotations: 
%%                      *              Bold
%%                      ''             Italic
%%                      '''            Code
%%                      ~[a-zA-Z1-9]+  Wiki link
%%                      http://...     URL
%%
%% Blocks (denoted by special characters in column 1):
%%                      [              Note
%%                        ...
%%                      ]
%%                      {              Preformatted
%%                        ...
%%                      }
%%                      {{             Embedded HTML
%%                        ...
%%                      }}
%%                      <              Writable region within a 
%%                        ...          locked page
%%                      >
%%                      <<             Write append region within
%%                        ...          a locked page
%%                      >>
%%
%% Bullets: (Column1)
%%
%%                      - * Text         Then - in column1 is the tab
%%                                       level. The number of tabs =
%%                                       the number of -'s
%%                                       * is the bullet. Then the text
%%                      - [Header] Text  Is a displayed list.
%%          
%%                                       Blanks or tabs between the - mark
%%                                       and the * (or Header) and Text
%%                                       are not significant.

-export([format/2, collect_wiki_link/1]).
-compile(export_all).

-import(lists, [member/2, map/2, reverse/1, reverse/2]).

format(Str, F) ->
    Env = {F, false, false, false, 0, false},
    Str1 = case Str of [$\n|_] -> Str; _ -> [$\n|Str] end,
    format_txt(Str1, Env, []).

blank_line(S=[$\n|_]) -> {yes, S};
blank_line([$\t|T])   -> blank_line(T);
blank_line([$  |T])   -> blank_line(T);
blank_line(_)         -> no.

format_txt([$\n|T], Env, L) ->
    case blank_line(T) of
	{yes, T1} ->
	    {Env1, L1} = clear_line(Env, reverse("<p>\n", L)),
	    format_txt(T1, Env1, L1);
	no ->
	    after_nl(T, Env, [$\n|L])
    end;
format_txt([$\\,H|T], Env, L) ->
    format_txt(T, Env, [H|L]);
format_txt([$*|T], Env, L) ->
    {Env1, L1} = char_style(b, Env, L),
    format_txt(T, Env1, L1);
format_txt([${,${|T], Env, L) ->
    emb(T,Env,L);
format_txt("'''" ++ T, Env, L) ->
    {Env1, L1} = char_style(tt, Env, L),
    format_txt(T, Env1, L1);
format_txt("''" ++ T, Env, L) ->
    {Env1, L1} = char_style(i, Env, L),
    format_txt(T, Env1, L1);
format_txt("~" ++ T, Env, L) ->
    {Word, T1} = collect_wiki_link(T),
    Link = format_wiki_word(Word, Env),
    format_txt(T1, Env, reverse(Link, L));
format_txt("http://" ++ T, Env, L) ->
    {Url, T1} = collect_url(T, []),
    Txt = format_external_url(Url),
    format_txt(T1, Env, reverse(Txt, L));
format_txt("mailto:" ++ T, Env, L) ->
    {X, T1} = collect_url(T, []),
    Txt = "<a href='mailto:" ++ X ++ "'>" ++ X ++ "</a>",
    format_txt(T1, Env, reverse(Txt, L));
format_txt([H|T], Env, L) ->
    format_txt(T, Env, [H|L]);
format_txt([], Env, L) ->
    {_, L1} = clear_line(Env, L),
    reverse(L1).

format_wiki_word(Str, {F,F1,F2,F3,Lev,Dl}) ->
    F({wikiLink, Str}).

collect_url(S=[$ |_], L)      -> {reverse(L), S};
collect_url(S=[$,|_], L)      -> {reverse(L), S};
collect_url(S=[$)|_], L)      -> {reverse(L), S};
collect_url(S=[$.,$ |_], L)   -> {reverse(L), S};
collect_url(S=[$.,$\n|_], L)  -> {reverse(L), S};
collect_url(S=[$.,$\r|_], L)  -> {reverse(L), S};
collect_url(S=[$.,$\t|_], L)  -> {reverse(L), S};
collect_url(S=[$\n|_], L)     -> {reverse(L), S};
collect_url([H|T], L)         -> collect_url(T, [H|L]);
collect_url([], L)            -> {reverse(L), []}.  

format_url(Url, {_,_,_,F}) ->  F(Url).

format_external_url(F) ->
    F1 = "http://" ++ F,
    case is_graphic(F) of
	true ->
	    "<img src=\"" ++  F1 ++ "\">";
	false ->
	    "<a href=\"" ++  F1 ++ "\">" ++ F1 ++ "</a> "
    end.

is_graphic(F) ->
    member(filename:extension(F), [".gif", ".GIF", ".jpg", ".JPG"]).

after_nl([${,$\n|T], Env, L)  -> pre(T, Env, L);
after_nl([${,${|T], Env, L)   -> emb(T, Env, L);
after_nl([${|T], Env, L)      -> pre(T, Env, L);
after_nl([$[|T], Env, L)      -> note(T, Env, L);
after_nl("____" ++ T, Env, L) -> hr(T, Env, L);
after_nl(S=[$-|T], Env, L)    -> mk_list(S, Env, L);
after_nl(T, Env, L)           -> format_txt(T, Env, L).

hr(T, Env, L) ->
    {Env1, L1} = clear_line(Env, L),
    L2 = reverse("<hr>\n", L1),
    format_txt(T, Env1, L2).

pre(T, Env, L) ->
    {Env1, L1} = clear_line(Env, L),
    L2 = reverse("<pre>\n", L1),
    pre1(T, Env1, L2).

pre1([$\r,$}|T], Env, L) ->
    L1 = reverse("\n</pre>\n", L),
    format_txt(T, Env, L1);
pre1([$\n,$}|T], Env, L) ->
    L1 = reverse("\n</pre>\n", L),
    format_txt(T, Env, L1);
pre1([H|T], Env, L) ->
    pre1(T, Env, [H|L]);
pre1([], Env, L) ->
    pre1([$\n,$}], Env, L).

emb([$},$}|T], Env, L) ->
    format_txt(T, Env, L);
emb([H|T], Env, L) ->
    emb(T, Env, [H|L]);
emb([], Env, L) ->
    emb([$},$}], Env, L).

note(T, Env, L) ->
    {Env1, L1} = clear_line(Env, L),
    L2 = reverse(note_start(), L1),
    note1(T, Env1, L2).

note1([$\n,$]|T], Env, L) ->
    L1 = reverse(note_end(), L),
    format_txt(T, Env, L1);
note1([H|T], Env, L) ->
    note1(T, Env, [H|L]);
note1([], Env, L) ->
    note1([$\n,$]], Env, L).

note_start() ->
    "<p><table cellpadding=20>
     <tr><td width=\"75%\"></td>
     <td bgcolor=\"yellow\"><font size=\"-1\">".

note_end() -> "</font></td></tr></table><p>\n".
	      
mk_list(T, Env, L) ->
    {Lev, T1} = count_indent_levels(T, 0),
    {Env1, L1} = adjust_indents(Env, Lev, L),
    T2 = skip_blanks(T1),
    case T2 of
	[$*|T3] ->
	    format_txt(T3,Env1,reverse("<li>", L1));
	[$[|T4] ->
	    {Env2, L2} = open_dl(Env1, L1),
            add_dl(T4, Env2, reverse("<dt>", L2));
	_ ->
	   format_txt(T2,Env1,L1)
    end.

skip_blanks([$ |T])  -> skip_blanks(T);
skip_blanks([$\n|T]) -> skip_blanks(T);
skip_blanks([$\t|T]) -> skip_blanks(T);
skip_blanks(X)       -> X.

open_dl({F,F1,F2,F3,N,false}, L) -> {{F,F1,F2,F3,N,true}, reverse("<dl>", L)};
open_dl(Env, L)                  -> {Env, L}.

add_dl([$]|T], Env, L)  -> format_txt(T, Env, reverse("</dt><dd>", L));
add_dl([$\n|T], Env, L) -> format_txt(T, Env, reverse("</dt><dd>", L));
add_dl([H|T], Env, L)   -> add_dl(T, Env, [H|L]);
add_dl([], Env, L)      -> format_txt([], Env, reverse("</dt>", L)).
    
count_indent_levels([$-|T], N) -> count_indent_levels(T, N+1);
count_indent_levels(T, N)      -> {N, T}.

adjust_indents(Env={F,F1,F2,F3,K,Dl}, K, L) ->
    {Env, L};
adjust_indents(Env={F,F1,F2,F3,N,Dl}, K, L) when N > K ->
    adjust_indents({F,F1,F2,F3,N-1,Dl}, K, reverse("</ul>", L));
adjust_indents(Env={F,F1,F2,F3,N,Dl}, K, L) when K > N ->
    adjust_indents({F,F1,F2,F3,N+1,Dl}, K, reverse("<ul>", L)).

clear_line({F, true, F2, F3, Lev, Dl}, L) ->
    clear_line({F, false, F2,F3,Lev,Dl}, reverse("</b>", L));
clear_line({F, F1, true, F3, Lev, Dl}, L) ->
    clear_line({F,F1,false,F3,Lev,Dl}, reverse("</i>", L));
clear_line({F, F1, F2, true, Lev,Dl}, L) ->
    clear_line({F, F1,F2,false,Lev,Dl}, reverse("</tt>", L));
clear_line({F, F1, F2, F3, N,true}, L) ->
    clear_line({F, F1,F2,F3,N,false}, reverse("</dl>", L));
clear_line(Env={F, F1, F2, F3, N, false}, L) when N /= 0 ->
    {Env1, L1} = adjust_indents(Env,0,L),
    clear_line(Env1, L1);
clear_line(Env, L) ->
    {Env, L}.

char_style(b, {F,false,F2,F3,Lev,Dl}, L) ->
    {{F,true,F2,F3,Lev,Dl},reverse("<b>", L)};
char_style(b, {F,true,F2,F3,Lev,Dl}, L) ->
    {{F,false,F2,F3,Lev,Dl},reverse("</b>", L)};
char_style(i, {F,F1,false,F3,Lev,Dl}, L) ->
    {{F,F1,true,F3,Lev,Dl},reverse("<i>", L)};
char_style(i, {F,F1,true,F3,Lev,Dl}, L) ->
    {{F,F1,false,F3,Lev,Dl},reverse("</i>", L)};
char_style(tt, {F,F1,F2,false,Lev,Dl}, L) ->
    {{F,F1,F2,true,Lev,Dl},reverse("<tt>", L)};
char_style(tt, {F,F1,F2,true,Lev,Dl}, L) ->
    {{F,F1,F2,false,Lev,Dl},reverse("</tt>", L)}.

collect_wiki_link([$"|X]) ->
    collect_wiki_link(X, [], true);
collect_wiki_link(X) ->
    collect_wiki_link(X, [], false).

collect_wiki_link([$"|T], L, true) ->
    {reverse(L), T};
collect_wiki_link([Any|T], L, true) ->
    collect_wiki_link(T, [Any|L], true);
collect_wiki_link([H|T], L, Quoted) when $A =< H, H =< $Z ->
    collect_wiki_link(T, [H|L], Quoted);
collect_wiki_link([H|T], L, Quoted) when $a =< H, H =< $z ->
    collect_wiki_link(T, [H|L], Quoted);
collect_wiki_link([H|T], L, Quoted) when $0 =< H, H =< $9 ->
    collect_wiki_link(T, [H|L], Quoted);
collect_wiki_link(S=[H|T], L, Quoted) ->
    case member(H, "äÄöÖåÅ") of
	true ->
	    collect_wiki_link(T, [H|L], Quoted);
	false ->
	    {reverse(L), S}
    end;
collect_wiki_link(T, L, Quoted) ->
    {reverse(L), T}.



