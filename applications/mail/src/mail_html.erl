%    -*- Erlang -*- 
%    File:        mail_html.erl
%    Author:        Johan Bevemyr
%    Created:        Sat Jun 19 15:13:49 2004
%    Purpose:   Transform HTML to text

-module('mail_html').
-author('jb@mor.bevemyr.com').

-export([html_to_text/1]).

html_to_text(Input) ->
    Tokens = tokenize(lists:flatten(Input), [], [], 1),
    Ehtml = parse(Tokens),
    RevText = ehtml_to_text(Ehtml, []),
    lists:reverse(RevText).

ehtml_to_text([], Acc) ->
    Acc;
ehtml_to_text([{Tag, Opts}|Rest], Acc) ->
    Acc2 = add_tag_space(Tag, Acc),
    ehtml_to_text(Rest, Acc2);
ehtml_to_text([{script, Opts, Body}|Rest], Acc) ->
    ehtml_to_text(Rest, Acc);
ehtml_to_text([{Tag, Opts, Body}|Rest], Acc) ->
    Acc1 = add_tag_space(Tag, Acc),
    Acc2 = ehtml_to_text(Body, Acc1),
    ehtml_to_text(Rest, Acc2);
ehtml_to_text([Text|Rest], Acc) ->
    Text2 = text_reformat(Text, []),
    ehtml_to_text(Rest, [Text2|Acc]).

add_tag_space(p, Acc) ->
    [$\n,$\r|Acc];
add_tag_space(br, Acc) ->
    [$\n,$\r|Acc];
add_tag_space(hr, Acc) ->
    [$\n,$\r|Acc];
add_tag_space(_, Acc) ->
    Acc.

text_reformat([], Acc) ->
    lists:reverse(Acc);
text_reformat([$\n|R], [$ |Acc]) ->
    text_reformat(R, Acc);
text_reformat([$\n|R], Acc) ->
    text_reformat(R, [$ |Acc]);
text_reformat([$\r|R], Acc) ->
    text_reformat(R, Acc);
text_reformat([C|R], Acc) ->
    text_reformat(R, [C|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Alternative parser, recursive as hell
%%

parse(Tokens) ->
    parse(Tokens, []).

parse([], Acc) -> lists:reverse(Acc);

parse([{begin_tag, T, A, L}|Rest], Acc) ->
    case tag_type(T) of
        leaf ->
            parse(Rest, [{T,A}|Acc]);
        node ->
            case find_body(T, Rest, []) of
                {error, Reason} ->
                    %% no body found, assume leaf
                    %% io:format("Error: ~s on line ~p\n", [Reason, L]),
                    parse(Rest, [{T,A}|Acc]);
                {Body,Rest2} ->
                    ParsedBody = parse(Body),
                    parse(Rest2, [{T,A,ParsedBody}|Acc])
            end
    end;
parse([{end_tag, T, A, L}|Rest], Acc) ->
    %% errounous end tag, ignore
    parse(Rest, Acc);
parse([{data, Data, L}|Rest], Acc) ->
    parse(Rest, [Data|Acc]).

find_body(Tag, [], Acc) ->
    {error, "Missing end tag for "++atom_to_list(Tag)};
find_body(Tag, [{end_tag,Tag,_,_}|Rest], Acc) ->
    {lists:reverse(Acc),Rest};
find_body(Tag, [{begin_tag, Tag, A, L}|Rest], Acc) ->
    case find_body(Tag, Rest, []) of
        {error, Reason} ->
            %% no body found
            {error, Reason};
        {Body, Rest1} ->
            find_body(Tag, Rest1,
                      [{end_tag, Tag, [], -1}|lists:reverse(Body)++
                       [{begin_tag, Tag, A, L}|Acc]])
    end;
find_body(Tag, [X|Rest], Acc) ->
    find_body(Tag, Rest, [X|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%tag_type(option)       -> leaf;
tag_type(p)          -> leaf;
tag_type(hr)         -> leaf;
tag_type(input)      -> leaf;
tag_type(base)       -> leaf;
tag_type(img)        -> leaf;
tag_type('!doctype') -> leaf;
tag_type(meta)       -> leaf;
tag_type(link)       -> leaf;
tag_type(br)         -> leaf;
tag_type(param)      -> leaf;
tag_type(_)          -> node.

% tokenize(Input, DataAcc, TokenAcc, LineNr)

tokenize([], [], Tokens, _Line) ->
    lists:reverse(Tokens);
tokenize([], Acc, Tokens, Line) ->
    lists:reverse([{data, lists:reverse(Acc), Line}|Tokens]);
tokenize([$<,$!,$-,$-|R0], Acc, Tokens, L0) ->
    {R1, L1} = skip_comment(R0,L0),
    tokenize(R1, Acc, Tokens, L1);
tokenize([$<|R0], Acc, Tokens, L0) ->
    {Tag,R1,L1} = scan_tag(R0,L0),
    if 
        Acc == [] ->
            next_token(Tag, R1, [Tag|Tokens], L1);
        true ->
            Data = {data,lists:reverse(Acc),L0},
            next_token(Tag, R1, [Tag,Data|Tokens], L1)
    end;
tokenize([C=$\n|R0], Acc, Tokens, L) ->
    tokenize(R0, [C|Acc], Tokens, L+1);
tokenize([C=$\r|R0], Acc, Tokens, L) ->
    tokenize(R0, [C|Acc], Tokens, L+1);
tokenize([C|R0], Acc, Tokens, L) ->
    tokenize(R0, [C|Acc], Tokens, L).

%

next_token({begin_tag, script, _, _}, R, Tokens, L) ->
    {Data, R1, L1} = scan_endtag(R, "script", L),
    tokenize(R1, [], [{data, Data, L}|Tokens], L1);
next_token({begin_tag, style, _, _}, R, Tokens, L) ->
    {Data, R1, L1} = scan_endtag(R, "style", L),
    tokenize(R1, [], [{data, Data, L}|Tokens], L1);
next_token(_Tag, R, Tokens, L) ->
    tokenize(R, [], Tokens, L).

%% '<' <id> <sp>+ [<id><sp>*['='<val>]]* ['/'] '>'

scan_tag([$/|I], L) ->
    {R0,L0} = skip_space(I, L),
    {Name,R1,L1} = scan_tag_name(I, L0),
    {R2,L2} = skip_space(R1, L1),
    {Args,R3,L3} = scan_tag_args(R2, L2),
    {{end_tag,list_to_atom(lowercase(Name)),Args,L0}, R3, L3};
scan_tag(I, L) ->
    {R0,L0} = skip_space(I, L),
    {Name,R1,L1} = scan_tag_name(I, L0),
    {R2,L2} = skip_space(R1, L1),
    {Args,R3,L3} = scan_tag_args(R2, L2),
    {{begin_tag,list_to_atom(lowercase(Name)),Args,L0}, R3, L3}.

%

scan_tag_name(I, L) ->
    scan_token(I, [], L).

%

scan_tag_args(I, L) ->
    scan_tag_args(I, [], L).

scan_tag_args([], Acc, L) ->
    {lists:reverse(Acc), [], L};
scan_tag_args([$>|R], Acc, L) ->
    {lists:reverse(Acc), R, L};
scan_tag_args(R=[$<|_], Acc, L) ->  %% bad html
    {lists:reverse(Acc), R, L};
scan_tag_args(R0, Acc, L0) ->
    {Name,R1,L1} = scan_value(R0, L0),
    {R2, L2} = skip_space(R1, L1),
    case R2 of
        [$=|R3] ->
            {R4,L4} = skip_space(R3, L2),
            {Value,R5,L5} = scan_value(R4, L4),
            {R6,L6} = skip_space(R5, L5),
            OptName = list_to_atom(lowercase(Name)),
            scan_tag_args(R6, [{OptName,Value}|Acc], L6);
        _ ->
            scan_tag_args(R2, [Name|Acc], L2)
    end.

%

scan_value([$"|R], L) ->
    scan_quote(R, [], $", L);
scan_value([$'|R], L) ->
    scan_quote(R, [], $', L);
scan_value(R, L) ->
    scan_token(R, [], L).

%

scan_token([], Acc, L) ->
    {lists:reverse(Acc), [], L};
scan_token(R=[$>|_], Acc, L) ->
    {lists:reverse(Acc), R, L};
scan_token(R=[$<|_], Acc, L) ->  %% bad html
    {lists:reverse(Acc), R, L};
scan_token(R=[$=|_], Acc, L) ->  %% bad html
    {lists:reverse(Acc), R, L};
scan_token([C|R], Acc, L0) ->
    case char_class(C) of
        space ->
            {lists:reverse(Acc), R, L0};
        nl ->
            {lists:reverse(Acc), R, L0+1};
        _ ->
            scan_token(R, [C|Acc], L0)
    end.

%

scan_quote([], Acc, _Q, L) ->
    {lists:reverse(Acc), [], L};
scan_quote([Q|R], Acc, Q, L) ->
    {lists:reverse(Acc), R, L};
scan_quote([C=$\n|R], Acc, Q, L) ->
    scan_quote(R, [C|Acc], Q, L+1);
scan_quote([C=$\r|R], Acc, Q, L) ->
    scan_quote(R, [C|Acc], Q, L+1);
scan_quote([C|R], Acc, Q, L) ->
    scan_quote(R, [C|Acc], Q, L).
            
%

scan_endtag(R, Tag, L) ->
    scan_endtag(R, Tag, [], L).

scan_endtag([], _Tag, Acc, L) ->
    {lists:reverse(Acc), [], L};
scan_endtag(R=[$<,$/|R0], Tag, Acc, L0) ->
    case casecmp(Tag, R0) of
        {true, R1} ->
            {R2,_} = skip_space(R1,L0),
            if hd(R2) == $> ->
                    {lists:reverse(Acc), R, L0};
               true ->
                    scan_endtag(R0, Tag, Acc, L0)
            end;
        false ->
            scan_endtag(R0, Tag, Acc, L0)
    end;
scan_endtag([C=$\n|R], Tag, Acc, L) ->
    scan_endtag(R, Tag, [C|Acc], L+1);
scan_endtag([C=$\r|R], Tag, Acc, L) ->
    scan_endtag(R, Tag, [C|Acc], L+1);
scan_endtag([C|R], Tag, Acc, L) ->
    scan_endtag(R, Tag, [C|Acc], L).

%

casecmp([], R) -> {true, R};
casecmp([C1|T1], [C2|T2]) ->
    C2low = lowercase_ch(C2),
    if C1 == C2low -> casecmp(T1,T2);
       true        -> false
    end.

%

char_class($\n) -> nl;
char_class($\r) -> nl;
char_class($ )  -> space;
char_class($\t) -> space;
char_class(C) when C >= $a, C =< $z -> alpha;
char_class(C) when C >= $A, C =< $Z -> alpha;
char_class(C) when C >= $0, C =< $9 -> digit;
char_class(C)   -> other.

%

skip_space([], L) ->
    {[], L};
skip_space(R = [C|R0], L) ->
    case char_class(C) of
        nl ->
            skip_space(R0, L+1);
        space ->
            skip_space(R0, L);
        _ ->
            {R, L}
    end.

%

skip_comment([], L) ->          {[], L};
skip_comment([$-,$-,$>|R],L) -> {R,L};
skip_comment([$\n|R],L) ->      skip_comment(R,L+1);
skip_comment([$\r|R],L) ->      skip_comment(R,L+1);
skip_comment([C|R],L) ->        skip_comment(R,L).

%

lowercase(Str) ->
    [lowercase_ch(S) || S <- Str].

lowercase_ch(C) when C>=$A, C=<$Z -> C + 32;
lowercase_ch(C) -> C.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


