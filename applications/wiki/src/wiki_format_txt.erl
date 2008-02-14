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
%%                      [expires:Date  expire tagged region
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
              h1 = false,
              h2 = false,
              h3 = false,
              u = false,
              n = 0, 
              dl = false
             }).

format(Str, F, Node) ->
    Env = #env{node=Node,f=F},
    Str1 = case Str of [$\n|_] -> Str; _ -> [$\n|Str] end,
    {Env1, Txt} = format_txt(Str1, Env, [], Str1),
    Txt.

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
format_txt([$_|T], Env, L, Doc) ->
    {Env1, L1} = char_style(u, Env, L),
    format_txt(T, Env1, L1, Doc);
format_txt([$=,$=,$=,$=|T], Env, L, Doc) ->
    {Env1, L1} = char_style(h1, Env, L),
    format_txt(T, Env1, L1, Doc);
format_txt([$=,$=,$=|T], Env, L, Doc) ->
    {Env1, L1} = char_style(h2, Env, L),
    format_txt(T, Env1, L1, Doc);
format_txt([$=,$=|T], Env, L, Doc) ->
    {Env1, L1} = char_style(h3, Env, L),
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
format_txt("https://" ++ T, Env, L, Doc) ->
    {Url, T1} = collect_url(T, []),
    Txt = format_external_url(Url, "https://"),
    format_txt(T1, Env, reverse(Txt, L), Doc);
format_txt("ftp://" ++ T, Env, L, Doc) ->
    {Url, T1} = collect_url(T, []),
    Txt = format_external_url(Url, "ftp://"),
    format_txt(T1, Env, reverse(Txt, L), Doc);
format_txt("slideshow:" ++ T, Env, L, Doc) ->
    {X, T1} = collect_wiki_link(T),
    Txt = "<a href='slideShow.yaws?node="++wiki:str2urlencoded(Env#env.node)++
        "&next=1'>"++X++ "</a>",
    format_txt(T1, Env, reverse(Txt, L), Doc);
format_txt("mailto:" ++ T, Env, L, Doc) ->
    {X, T1} = collect_mail(T, []),
    Txt = "<a href='mailto:" ++ X ++ "'>" ++ 
        "<img border=0 src='WikiPreferences.files/mailto.png'>"
        ++ X ++ "</a>",
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
format_txt("<?plugin " ++ T, Env, L, Doc) ->
    Page = Env#env.node,
    {Txt, T1} = plugin(T, Page),
    format_txt(T1, Env, reverse(Txt, L), Doc);
format_txt([H|T], Env, L, Doc) ->
    format_txt(T, Env, [H|L], Doc);
format_txt([], Env, L, Doc) ->
    {_, L1} = clear_line(Env, L),
    {Env, reverse(L1)}.

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
    format_external_url(F, "http://").
format_external_url(F, Scheme) ->
    F1 = Scheme ++ F,
    case is_graphic(F) of
        true ->
            "<img src=\"" ++  F1 ++ "\">";
        false ->
            "<a href=\"" ++  F1 ++ "\">" ++ 
            "<img border=0 src='WikiPreferences.files/http.png'>"
            ++ F1 ++ "</a> "
    end.

is_graphic(F) ->
    member(filename:extension(F), [".gif", ".GIF", ".jpg", ".JPG"]).

after_nl([${,$\n|T], Env, L, Doc)  -> pre(T, Env, L, Doc);
after_nl([${,${|T], Env, L, Doc)   -> emb(T, Env, L, Doc);
after_nl([${|T], Env, L, Doc)      -> pre(T, Env, L, Doc);
after_nl("[expires:"++T, Env, L, Doc) -> eregion(T, Env, L, Doc);
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

eregion(T0, Env, L, Doc) ->
    {DateStr,T1} = collect_wiki_link(T0),
    Date = parse_date(DateStr),
    Expired = date_less(Date, {date(),time()}),
    {Region, T2} = collect_region($], T1, []),
    case Expired of
        error ->
            L1 = reverse("ERROR: bad expires date entry - "++DateStr++". "
                         "The date should be on the form \"3 Jan 2003 "
                         "00:00:00\".", L),
            format_txt(T2, Env, L1, Doc);
        true ->
            format_txt(T2, Env, L, Doc);
        false ->
            {Env1, RTxt} = format_txt(Region, Env, [], Doc),
            L1 = reverse(RTxt, L),
            format_txt(T2, Env1, L1, Doc)
    end.

collect_region(_, [], Acc) ->
    {lists:reverse(Acc), []};
collect_region(End, [$\n,End|T], Acc) ->
    {lists:reverse(Acc), T};
collect_region(End, [C|T], Acc) ->
    collect_region(End, T, [C|Acc]).

date_less(error,_) -> error;
date_less(_,error) -> error;
date_less(D1,D2) ->
    Ds1 = calendar:datetime_to_gregorian_seconds(D1),
    Ds2 = calendar:datetime_to_gregorian_seconds(D2),
    Ds1 < Ds2.

-record(date,
        {
          year,
          month,
          day,
          hours, 
          minutes,
          seconds
          }).

parse_date(Date) ->
    parse_date(Date, #date{}).

parse_date([], D) ->
    Entries = tl(tuple_to_list(D)),
    AllDone = lists:all(fun(X) -> if integer(X) -> true;
                                     true -> false
                                  end
                        end, Entries),
    if
        AllDone ->
            {{D#date.year,D#date.month,D#date.day},
             {D#date.hours,D#date.minutes,D#date.seconds}};
        true ->
            error
    end;
parse_date([D|Ds], Date) ->
    case char_type(D) of
        space -> parse_date(Ds, Date);
        alpha when Date#date.month == undefined ->
            case is_month(lowercase([D|Ds])) of
                false ->
                    parse_date(Ds, Date);
                {true, M, Rest} ->
                    parse_date(Rest, Date#date{month=M})
            end;
        alpha ->
            parse_date(Ds, Date);
        digit ->
            case parse_time([D|Ds]) of
                error ->
                    {Number,Rest} = get_number([D|Ds], 0),
                    if
                        Number < 32, Date#date.day == undefined ->
                            parse_date(Rest, Date#date{day=Number});
                        Number < 50, Date#date.year == undefined ->
                            parse_date(Rest, Date#date{year=Number+2000});
                        Number < 100, Date#date.year == undefined ->
                            parse_date(Rest, Date#date{year=Number+1900});
                        Number > 1900, Date#date.year == undefined ->
                            parse_date(Rest, Date#date{year=Number});
                        true ->
                            parse_date(Rest, Date)
                    end;
                {Hours, Minutes, Seconds, Rest} ->
                    parse_date(Rest, Date#date{hours=Hours,
                                               minutes=Minutes,
                                               seconds=Seconds})
            end;
        _ ->
            parse_date(Ds, Date)
    end.

lowercase([C|S]) -> [lowercase(C)|lowercase(S)];
lowercase(C) when C>=$A, C=<$Z -> C+32;
lowercase(C) -> C.

is_month("jan"++Rest) -> {true, 1, Rest};
is_month("feb"++Rest) -> {true, 2, Rest};
is_month("mar"++Rest) -> {true, 3, Rest};
is_month("apr"++Rest) -> {true, 4, Rest};
is_month("may"++Rest) -> {true, 5, Rest};
is_month("jun"++Rest) -> {true, 6, Rest};
is_month("jul"++Rest) -> {true, 7, Rest};
is_month("aug"++Rest) -> {true, 8, Rest};
is_month("sep"++Rest) -> {true, 9, Rest};
is_month("oct"++Rest) -> {true, 10, Rest};
is_month("nov"++Rest) -> {true, 11, Rest};
is_month("dec"++Rest) -> {true, 12, Rest};
is_month(_) -> false.

enc_month(1) -> "Jan";
enc_month(2) -> "Feb";
enc_month(3) -> "Mar";
enc_month(4) -> "Apr";
enc_month(5) -> "May";
enc_month(6) -> "Jun";
enc_month(7) -> "Jul";
enc_month(8) -> "Aug";
enc_month(9) -> "Sep";
enc_month(10) -> "Oct";
enc_month(11) -> "Nov";
enc_month(12) -> "Dec".

enc_day(1) -> "Mon";
enc_day(2) -> "Tue";
enc_day(3) -> "Wed";
enc_day(4) -> "Thu";
enc_day(5) -> "Fri";
enc_day(6) -> "Sat";
enc_day(7) -> "Sun".

char_type(D) when D>=$a, D=<$z -> alpha;
char_type(D) when D>=$A, D=<$Z -> alpha;
char_type(D) when D>=$0, D=<$9 -> digit;
char_type($\ ) -> space;
char_type($\n) -> space;
char_type($\t) -> space;
char_type($\v) -> space;
char_type(_) -> unknown.

get_number([D|Ds], N) when D>=$0, D=<$9 ->
    get_number(Ds, N*10+(D-$0));
get_number(Rest, N) -> {N, Rest}.

parse_time(Time) ->
    F = fun() ->
                {Hour,[$:|R1]}    = get_number(Time, 0),
                {Minutes,[$:|R2]} = get_number(R1, 0),
                {Seconds,R3}      = get_number(R2, 0),
                {Hour, Minutes, Seconds, R3}
        end,
    case catch F() of
        {Hour, Minutes, Seconds, Rest} when integer(Hour),
                                      integer(Minutes),
                                      integer(Seconds) ->
            {Hour, Minutes, Seconds, Rest};
        _ -> error
    end.

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

char_style(u, Env, L) when Env#env.u == false ->
    {Env#env{u=true},reverse("<u>", L)};
char_style(u, Env, L) when Env#env.u == true ->
    {Env#env{u=false},reverse("</u>", L)};
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
    {Env#env{f3=false},reverse("</tt>", L)};
char_style(h1, Env, L) when Env#env.h1==true ->
    {Env#env{h1=false},reverse("</h1>", L)};
char_style(h2, Env, L) when Env#env.h2==true ->
    {Env#env{h2=false},reverse("</h2>", L)};
char_style(h3, Env, L) when Env#env.h3==true ->
    {Env#env{h3=false},reverse("</h3>", L)};
char_style(h1, Env, L) when Env#env.h1==false ->
    {Env#env{h1=true},reverse("<h1>", L)};
char_style(h2, Env, L) when Env#env.h2==false ->
    {Env#env{h2=true},reverse("<h2>", L)};
char_style(h3, Env, L) when Env#env.h3==false ->
    {Env#env{h3=true},reverse("<h3>", L)}.

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

%% Plugin implementation.
%% The plugin is a special syntaxe in Wiki pages:
%% <?plugin name arg1=Value1 ... ?>
%% When such a syntax is used, the following function is called:
%% wiki_plugin_name:run([{arg1, Value1}, ...]).
plugin(Data, Page) ->
   case string:str(Data, "?>") of
       0 -> %% Broken plugin syntax
           {"", "<?plugin " ++ Data};
       EndPluginIndex ->
           PluginData = string:sub_string(Data, 1, EndPluginIndex -1),
           [PluginName| ArgStrings] = string:tokens(PluginData, " "),
           Result  = exec_plugin(PluginName, Page, ArgStrings),
           Rest    = string:sub_string(Data, EndPluginIndex + 2),
           {Result, Rest}
   end.

exec_plugin(Name, Page, ArgStrings) ->
   exec_plugin(Name, Page, ArgStrings, []).
exec_plugin(Name, Page, [], ArgsList) ->
   case catch apply(list_to_atom("wiki_plugin_" ++ Name),run, [Page, ArgsList]) of
       {'EXIT', Reason} -> io_lib:format("Plugin error: ~p", [Reason]);
       Result -> Result
   end;
exec_plugin(Name, Page, [ArgString|ArgStrings], Acc) ->
   [Key,Val] = string:tokens(ArgString, "="),
   exec_plugin(Name, Page, ArgStrings, [{Key,Val}|Acc]).
