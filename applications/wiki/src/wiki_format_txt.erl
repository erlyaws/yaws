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

-export([format/3, collect_wiki_link/1]).
-compile(export_all).

-import(lists, [member/2, map/2, reverse/1, reverse/2]).

-record(env, {node,
	      f,
	      f1 = false,
	      f2 = false,
	      f3 = false,
	      n = 0, 
	      dl = false
	     }).

format(Str, F, Node) ->
    Env = #env{node=Node,f=F},
    Str1 = case Str of [$\n|_] -> Str; _ -> [$\n|Str] end,
    format_txt(Str1, Env, [], Str1).

blank_line(S=[$\n|_]) -> {yes, S};
blank_line([$\t|T])   -> blank_line(T);
blank_line([$  |T])   -> blank_line(T);
blank_line(_)         -> no.

format_txt([$\n|T], Env, L, Doc) ->
    case blank_line(T) of
	{yes, T1} ->
	    {Env1, L1} = clear_line(Env, reverse("<p>\n", L)),
	    format_txt(T1, Env1, L1, Doc);
	no ->
	    after_nl(T, Env, [$\n|L], Doc)
    end;
format_txt([$\\,H|T], Env, L, Doc) ->
    format_txt(T, Env, [H|L], Doc);
format_txt([$*|T], Env, L, Doc) ->
    {Env1, L1} = char_style(b, Env, L),
    format_txt(T, Env1, L1, Doc);
format_txt([${,${|T], Env, L, Doc) ->
    emb(T,Env,L, Doc);
format_txt("'''" ++ T, Env, L, Doc) ->
    {Env1, L1} = char_style(tt, Env, L),
    format_txt(T, Env1, L1, Doc);
format_txt("''" ++ T, Env, L, Doc) ->
    {Env1, L1} = char_style(i, Env, L),
    format_txt(T, Env1, L1, Doc);
format_txt("~" ++ T, Env, L, Doc) ->
    {Word, T1} = collect_wiki_link(T),
    Link = format_wiki_word(Word, Env),
    format_txt(T1, Env, reverse(Link, L), Doc);
format_txt("http://" ++ T, Env, L, Doc) ->
    {Url, T1} = collect_url(T, []),
    Txt = format_external_url(Url),
    format_txt(T1, Env, reverse(Txt, L), Doc);
format_txt("slideshow:" ++ T, Env, L, Doc) ->
    {X, T1} = collect_wiki_link(T),
    Txt = "<a href='slideShow.yaws?node="++Env#env.node++
	"&index=1'>"++X++ "</a>",
    format_txt(T1, Env, reverse(Txt, L), Doc);
format_txt("mailto:" ++ T, Env, L, Doc) ->
    {X, T1} = collect_mail(T, []),
    Txt = "<a href='mailto:" ++ X ++ "'>" ++ X ++ "</a>",
    format_txt(T1, Env, reverse(Txt, L), Doc);
format_txt("mailtoall:" ++ T, Env, L, Doc) ->
    {Name, T1} = collect_wiki_link(T),
    case get_mailto(Doc, []) of
	[] ->
	    format_txt(T, Env, L, Doc);
	[F|Rs] ->
	    Recipients = [F | [[$,|R] || R <- Rs]],
	    Txt = "<a href='mailto:" ++ Recipients ++ "'>" ++ Name ++ "</a>",
	    format_txt(T1, Env, reverse(Txt, L), Doc)
    end;
format_txt([H|T], Env, L, Doc) ->
    format_txt(T, Env, [H|L], Doc);
format_txt([], Env, L, Doc) ->
    {_, L1} = clear_line(Env, L),
    reverse(L1).

format_wiki_word(Str, Env) ->
    F = Env#env.f,
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

collect_mail(S=[$ |_], L)      -> {reverse(L), S};
collect_mail(S=[$)|_], L)      -> {reverse(L), S};
collect_mail(S=[$<|_], L)      -> {reverse(L), S};
collect_mail(S=[$>|_], L)      -> {reverse(L), S};
collect_mail(S=[$.,$ |_], L)   -> {reverse(L), S};
collect_mail(S=[$.,$\n|_], L)  -> {reverse(L), S};
collect_mail(S=[$.,$\r|_], L)  -> {reverse(L), S};
collect_mail(S=[$.,$\t|_], L)  -> {reverse(L), S};
collect_mail(S=[$\n|_], L)     -> {reverse(L), S};
collect_mail([H|T], L)         -> collect_mail(T, [H|L]);
collect_mail([], L)            -> {reverse(L), []}.  

get_mailto([$\\,C|T], L) ->
    get_mailto(T, L);
get_mailto("mailto:"++T, L) ->
    {Link, T1} = collect_mail(T, []),
    get_mailto(T1, [Link|L]);
get_mailto([_|T], L) ->
    get_mailto(T, L);
get_mailto([], L) ->
    L.

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

after_nl([${,$\n|T], Env, L, Doc)  -> pre(T, Env, L, Doc);
after_nl([${,${|T], Env, L, Doc)   -> emb(T, Env, L, Doc);
after_nl([${|T], Env, L, Doc)      -> pre(T, Env, L, Doc);
after_nl([$[|T], Env, L, Doc)      -> note(T, Env, L, Doc);
after_nl("____" ++ T, Env, L, Doc) -> hr(T, Env, L, Doc);
after_nl(S=[$-|T], Env, L, Doc)    -> mk_list(S, Env, L, Doc);
after_nl(T, Env, L, Doc)           -> format_txt(T, Env, L, Doc).

hr(T, Env, L, Doc) ->
    {Env1, L1} = clear_line(Env, L),
    L2 = reverse("<hr>\n", L1),
    format_txt(T, Env1, L2, Doc).

pre(T, Env, L, Doc) ->
    {Env1, L1} = clear_line(Env, L),
    L2 = reverse("<pre>\n", L1),
    pre1(T, Env1, L2, Doc).

pre1([$\r,$}|T], Env, L, Doc) ->
    L1 = reverse("\n</pre>\n", L),
    format_txt(T, Env, L1, Doc);
pre1([$\n,$}|T], Env, L, Doc) ->
    L1 = reverse("\n</pre>\n", L),
    format_txt(T, Env, L1, Doc);
pre1([$<|T], Env, L, Doc) ->
    L1 = reverse("&lt;", L),
    pre1(T, Env, L1, Doc);
pre1([$>|T], Env, L, Doc) ->
    L1 = reverse("&gt;", L),
    pre1(T, Env, L1, Doc);
pre1([H|T], Env, L, Doc) ->
    pre1(T, Env, [H|L], Doc);
pre1([], Env, L, Doc) ->
    pre1([$\n,$}], Env, L, Doc).

emb([$},$}|T], Env, L, Doc) ->
    format_txt(T, Env, L, Doc);
emb([H|T], Env, L, Doc) ->
    emb(T, Env, [H|L], Doc);
emb([], Env, L, Doc) ->
    emb([$},$}], Env, L, Doc).

note(T, Env, L, Doc) ->
    {Env1, L1} = clear_line(Env, L),
    L2 = reverse(note_start(), L1),
    note1(T, Env1, L2, Doc).

note1([$\n,$]|T], Env, L, Doc) ->
    L1 = reverse(note_end(), L),
    format_txt(T, Env, L1, Doc);
note1([H|T], Env, L, Doc) ->
    note1(T, Env, [H|L], Doc);
note1([], Env, L, Doc) ->
    note1([$\n,$]], Env, L, Doc).

note_start() ->
    "<p><table cellpadding=20>
     <tr><td width=\"75%\"></td>
     <td bgcolor=\"yellow\"><font size=\"-1\">".

note_end() -> "</font></td></tr></table><p>\n".
	      
mk_list(T, Env, L, Doc) ->
    {Lev, T1} = count_indent_levels(T, 0),
    {Env1, L1} = adjust_indents(Env, Lev, L),
    T2 = skip_blanks(T1),
    case T2 of
	[$*|T3] ->
	    format_txt(T3,Env1,reverse("<li>", L1), Doc);
	[$[|T4] ->
	    {Env2, L2} = open_dl(Env1, L1),
            add_dl(T4, Env2, reverse("<dt>", L2), Doc);
	_ ->
	   format_txt(T2,Env1,L1, Doc)
    end.

skip_blanks([$ |T])  -> skip_blanks(T);
skip_blanks([$\n|T]) -> skip_blanks(T);
skip_blanks([$\t|T]) -> skip_blanks(T);
skip_blanks(X)       -> X.

open_dl(Env, L) when Env#env.dl == false ->
    {Env#env{dl=true}, reverse("<dl>", L)};
open_dl(Env, L) -> {Env, L}.

add_dl([$]|T], Env, L, Doc) ->
    format_txt(T, Env, reverse("</dt><dd>", L), Doc);
add_dl([$\n|T], Env, L, Doc) ->
    format_txt(T, Env, reverse("</dt><dd>", L), Doc);
add_dl([H|T], Env, L, Doc) ->
    add_dl(T, Env, [H|L], Doc);
add_dl([], Env, L, Doc) ->
    format_txt([], Env, reverse("</dt>", L), Doc).
    
count_indent_levels([$-|T], N) -> count_indent_levels(T, N+1);
count_indent_levels(T, N)      -> {N, T}.

adjust_indents(Env, K, L) when Env#env.n == K ->
    {Env, L};
adjust_indents(Env, K, L) when Env#env.n > K ->
    adjust_indents(Env#env{n=Env#env.n-1}, K, reverse("</ul>", L));
adjust_indents(Env, K, L) when K > Env#env.n ->
    adjust_indents(Env#env{n=Env#env.n+1}, K, reverse("<ul>", L)).

clear_line(Env, L) when Env#env.f1==true ->
    clear_line(Env#env{f1=false}, reverse("</b>", L));
clear_line(Env,  L) when Env#env.f2==true ->
    clear_line(Env#env{f2=false}, reverse("</i>", L));
clear_line(Env,  L) when Env#env.f3==true ->
    clear_line(Env#env{f3=false}, reverse("</tt>", L));
clear_line(Env, L) when Env#env.dl==true->
    clear_line(Env#env{dl=false}, reverse("</dl>", L));
clear_line(Env, L) when Env#env.n /= 0 ->
    {Env1, L1} = adjust_indents(Env,0,L),
    clear_line(Env1, L1);
clear_line(Env, L) ->
    {Env, L}.

char_style(b, Env, L) when Env#env.f1 == false ->
    {Env#env{f1=true},reverse("<b>", L)};
char_style(b, Env, L) when Env#env.f1 == true ->
    {Env#env{f1=false},reverse("</b>", L)};
char_style(i, Env, L) when Env#env.f2 == false ->
    {Env#env{f2=true},reverse("<i>", L)};
char_style(i, Env, L) when Env#env.f2 == true ->
    {Env#env{f2=false},reverse("</i>", L)};
char_style(tt, Env, L) when Env#env.f3==false ->
    {Env#env{f3=true},reverse("<tt>", L)};
char_style(tt, Env, L) when Env#env.f3==true ->
    {Env#env{f3=false},reverse("</tt>", L)}.

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



