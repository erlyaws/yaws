-module(utils).

%%% File    : utils.erl
%%% Author  : Mickael Remond <mickael.remond@erlang-fr.org>
%%% Description : Various general purpose helper functions
%%% Created : 22 Oct 2003 by Mickael Remond
%%%                          <mickael.remond@erlang-fr.org>

-export([time_to_string/1]).
-export([fold_files/3, fold_files/5]).

%% Time (as return by calendar:local_time() to string conversion.
time_to_string( {{Y,Mo,D},{H,Mi,S}} ) ->
    String = io_lib:format( "~4.4w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
                            [Y,Mo,D,H,Mi,S] ),
    lists:flatten(String).


%% Fold_files is a rewrite by Chris Pressey of the broken
%% implementation in Erlang R9C

%% @spec fold_files(dir(), fun(), term()) -> term()
%% @doc Folds the function Fun(F, IsDir, Acc) -> {Recurse, Acc1} over
%% all files F in Dir that match the regular expression RegExp.
%% If Recurse is true all sub-directories of F are processed.
%% (This function is a modified version of that from filelib.erl)

fold_files(Dir, Fun, Acc) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            fold_files0(Files, Dir, Fun, Acc);
        {error, _} ->
            Acc
    end.

fold_files0([File | Tail], Dir, Fun, Acc) ->
    FullName = filename:join([Dir, File]),
    IsDir = filelib:is_dir(FullName),
    {Recurse, NewAcc} = Fun(FullName, IsDir, Acc),
    fold_files0(FullName, Tail, Dir, Fun, IsDir, Recurse, NewAcc);
fold_files0([], Dir, Fun, Acc) ->
    Acc.

fold_files0(FullName, Tail, Dir, Fun, true, true, Acc) ->
    NewAcc = fold_files(FullName, Fun, Acc),
    fold_files0(Tail, Dir, Fun, NewAcc);
fold_files0(FullName, Tail, Dir, Fun, _, _, Acc) ->
    fold_files0(Tail, Dir, Fun, Acc).

%% @spec fold_files(dir(), regexp(), bool(), fun(), term()) -> term()
%% Wrapper for the original fold_files/5 behaviour.

fold_files(Dir, RegExp, Recursive, Fun, InitialAcc) ->
    {ok, CompiledRegExp} = regexp:parse(RegExp),
    Wrapper = fun
                  (FullName, false, Acc) ->
                      NewAcc = case regexp:match(FullName, CompiledRegExp) of
                                   {match, _, _}  -> 
                                       Fun(FullName, Acc);
                                   _ ->
                                       Acc
                               end,
                      {Recursive, NewAcc};
                  (_, true, Acc) ->
                      {Recursive, Acc}
              end,
    fold_files(Dir, Wrapper, InitialAcc).
