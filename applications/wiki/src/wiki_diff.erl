-module(wiki_diff).

%% File    : diff.erl
%% Author  : Joe Armstrong (joe@bluetail.com)
%% Purpose : Diff of two files (like Diff and patch).

%% diff(New, Old) -> Patch
%% patch(New, Patch) -> Old
%% patchL(New, [Patch]) -> Old'

%% -compile(export_all).

-export([diff/2, diff_files/2, patch/2, patchL/2]).
-export([test/0]).

-import(lists, [foldl/3, reverse/1]).

test() ->
    diff_files("diff.erl", "diff.erl.old").

diff_files(F1, F2) ->
    {ok, B1} = file:read_file(F1),
    {ok, B2} = file:read_file(F2),
    diff(binary_to_list(B1), binary_to_list(B2)).

diff(New, Old) ->
    Patch = diff(str2lines(Old), str2lines(New), []),
    %% io:format("Patch size=~p~n",[size(Patch)]),
    check(Patch, New, Old),
    Patch.

check(Patch, New, Old) ->
    case patch(New, Patch) of
        Old ->
            true;
        _ ->
            exit(oops)
    end.

patchL(New, Patches) ->
    foldl(fun(Patch, N) -> patch(N, Patch) end,        New, Patches).

patch(New, Patch) ->
    sneaky_flatten(patch1(binary_to_term(Patch), str2lines(New))).

patch1([{L1,L2}|T], New) -> [get_lines(L1, L2, New)|patch1(T, New)];
patch1([H|T], New)       -> [H|patch1(T, New)];
patch1([], _)            -> [].

get_lines(_, L2, [{L2,S}|_])  -> S;
get_lines(L1, L2, [{L1,S}|T]) -> [S|get_lines(L1+1, L2, T)];
get_lines(L1, L2, [_|T])      -> get_lines(L1, L2, T).

sneaky_flatten(L) ->
    binary_to_list(list_to_binary(L)).

diff([], _, Patch) ->
    term_to_binary(reverse(Patch));
diff(Old = [{_,Str}|T], New, Patch) ->
    case match(Old, New) of
        {yes, Ln, Ln, Old1} ->
            case Str of
                "\n" ->
                    diff(Old1, New, [Str|Patch]);
                _ ->
                    diff(Old1, New, [{Ln,Ln}|Patch])
            end;
        {yes, L1, L2, Old1} ->
            diff(Old1, New, [{L1,L2}|Patch]);
        no ->
            diff(T, New, [Str|Patch])
    end.

match(X=[{Ln,Str}|T], [{L1,Str}|T1]) -> extend_match(T, T1, L1, L1);
match(X, [_|T])                      -> match(X, T);
match(X, [])                         -> no.

extend_match([{_,S}|T1], [{L2,S}|T2], L1, _) -> extend_match(T1, T2, L1, L2);
extend_match(X, _, L1, L2)                   -> {yes, L1, L2, X}.

str2lines(L) -> str2lines(L, 1, [], []).
 
str2lines([H|T], Line, C, L) ->
    case H of
        $\n -> str2lines(T, Line+1,[],[{Line,reverse([$\n|C])}|L]);
        _   -> str2lines(T, Line,  [H|C], L)
    end;
str2lines([], Line, [], L) ->
    reverse(L);
str2lines([], Line, C, L) ->
    reverse([{Line,reverse(C)}|L]).
                                   
