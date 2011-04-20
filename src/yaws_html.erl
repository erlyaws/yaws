%%    -*- Erlang -*-
%%    File:        parse_html.erl
%%    Author:        Johan Bevemyr
%%    Created:        Tue Nov 25 20:53:36 2003
%%    Purpose:   Transform html to an erlang represention (ehtml)

-module('yaws_html').
-author('jb@bevemyr.com').

-export([parse/1,parse/2,h2e/1]).

parse(Name) ->
    {ok, B} = file:read_file(Name),
    h2e(binary_to_list(B)).

parse(Name,Out) ->
    {ok, B} = file:read_file(Name),
    case h2e(binary_to_list(B)) of
        {ehtml, [], Ehtml} ->
            Cont = io_lib:format("~p", [{ehtml, Ehtml}]),
            file:write_file(Out, Cont);
        Error ->
            Error
    end.

h2e(Input) ->
    Tokens = tokenize(Input, [], [], 1),
    parse(Tokens, {ehtml,[],0}, [], []).

%% parse(Tokens, Stack, Acc)

parse([], {T,A,_L}, [], Acc) ->
    {T, A, lists:reverse(Acc)};
parse([], {T,A,L}, [{CTag,CAcc}|Stack], Acc) ->
    io:format("Unterminated tag '~p' at line ~p\n", [T,L]),
    parse([], CTag, Stack, [{T,A,lists:reverse(Acc)}|CAcc]);
parse([{begin_tag,T,A,L}|Tokens], CTag, Stack, Acc) ->
    case tag_type(T) of
        leaf ->
            parse(Tokens, CTag, Stack, [{T,A}|Acc]);
        node ->
            parse(Tokens, {T,A,L}, [{CTag,Acc}|Stack],[])
    end;

parse([{end_tag,T,[],_L}|Tokens], {T,A,_}, [{CTag,CAcc}|Stack], Acc) ->
    E = case Acc of
            [Single] ->
                {T,A,Single};
            _ ->
                {T,A,lists:reverse(Acc)}
        end,
    parse(Tokens, CTag, Stack, [E|CAcc]);

parse([{end_tag,T1,[],L1}|Tokens], CTag = {T2,_A,L2}, Stack, Acc) ->
    case tag_type(T1) of
        leaf -> %% ignore
            parse(Tokens, CTag, Stack, Acc);
        node ->
            Msg = lists:flatten(io_lib:format(
                                  "expected '</~p>'  on line ~p, start "
                                  "tag at line: ~p", [T2,L1,L2])),
            {error, Msg}
    end;

parse([{data, Data, _Line}|Tokens], CTag, Stack, Acc) ->
    case skip_space(Data, 0) of
        {[], _} ->
            parse(Tokens, CTag, Stack, Acc);
        _ ->
            parse(Tokens, CTag, Stack, [Data|Acc])
    end.
%%

tag_type(p)          -> leaf;
tag_type(hr)         -> leaf;
tag_type(input)      -> leaf;
tag_type(base)       -> leaf;
tag_type(img)        -> leaf;
tag_type('!doctype') -> leaf;
tag_type(meta)       -> leaf;
tag_type(link)       -> leaf;
tag_type(br)         -> leaf;
tag_type(_)          -> node.

%% tokenize(Input, DataAcc, TokenAcc, LineNr)

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

%%

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
    {_R0,L0} = skip_space(I, L),
    {Name,R1,L1} = scan_tag_name(I, L0),
    {R2,L2} = skip_space(R1, L1),
    {Args,R3,L3} = scan_tag_args(R2, L2),
    {{end_tag,list_to_atom(lowercase(Name)),Args,L0}, R3, L3};
scan_tag(I, L) ->
    {_R0,L0} = skip_space(I, L),
    {Name,R1,L1} = scan_tag_name(I, L0),
    {R2,L2} = skip_space(R1, L1),
    {Args,R3,L3} = scan_tag_args(R2, L2),
    {{begin_tag,list_to_atom(lowercase(Name)),Args,L0}, R3, L3}.

%%

scan_tag_name(I, L) ->
    scan_token(I, [], L).

%%

scan_tag_args(I, L) ->
    scan_tag_args(I, [], L).

scan_tag_args([], Acc, L) ->
    {lists:reverse(Acc), [], L};
scan_tag_args([$>|R], Acc, L) ->
    {lists:reverse(Acc), R, L};
scan_tag_args(R=[$<|_], Acc, L) ->  %%%% bad html
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

%%

scan_value([$"|R], L) ->
    scan_quote(R, [], $", L);
scan_value([$'|R], L) ->
    scan_quote(R, [], $', L);
scan_value(R, L) ->
    scan_token(R, [], L).

%%

scan_token([], Acc, L) ->
    {lists:reverse(Acc), [], L};
scan_token(R=[$>|_], Acc, L) ->
    {lists:reverse(Acc), R, L};
scan_token(R=[$<|_], Acc, L) ->  %%% bad html
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

%%

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

%%

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

%%

casecmp([], R) -> {true, R};
casecmp([C1|T1], [C2|T2]) ->
    C2low = lowercase_ch(C2),
    if C1 == C2low -> casecmp(T1,T2);
       true        -> false
    end.

%%

char_class($\n) -> nl;
char_class($\r) -> nl;
char_class($ )  -> space;
char_class($\t) -> space;
char_class(C) when C >= $a, C =< $z -> alpha;
char_class(C) when C >= $A, C =< $Z -> alpha;
char_class(C) when C >= $0, C =< $9 -> digit;
char_class(_C)   -> other.

%%

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

%%

skip_comment([], L) ->          {[], L};
skip_comment([$-,$-,$>|R],L) -> {R,L};
skip_comment([$\n|R],L) ->      skip_comment(R,L+1);
skip_comment([$\r|R],L) ->      skip_comment(R,L+1);
skip_comment([_C|R],L) ->        skip_comment(R,L).

%%

lowercase(Str) ->
    [lowercase_ch(S) || S <- Str].

lowercase_ch(C) when C>=$A, C=<$Z -> C + 32;
lowercase_ch(C) -> C.
