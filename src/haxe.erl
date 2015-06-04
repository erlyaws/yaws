%%% Copyright (c) 2005-2006, A2Z Development USA, Inc.  All Rights Reserved.
%%%
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Initial Developer of the Original Code is A2Z Development USA, Inc.
%%% All Rights Reserved.

%% This code was originally created for serializing/deserializing
%% Erlang terms in JSON format. It was hacked to handle haXe
%% (http://www.haxe.org) serialization/deserialization.
%%
%% Modified by Yariv Sadan (yarivvv@gmail.com)

-module(haxe).
-export([encode/1, encode/2, decode_string/1, decode_string/2,
         decode/1, decode/2, decode_next/1]).
-export([get_left_over_chars/1]).
-export([is_obj/1, obj_new/0, obj_fetch/2, obj_find/2, obj_is_key/2]).
-export([test/0]).
-export([obj_store/3, obj_from_list/1, obj_fold/3]).

-author("Jim Larson <jalarson@amazon.com>, Robert Wai-Chi Chu <robchu@amazon.com>").
-author("Gaspar Chilingarov <nm@web.am>, Gurgen Tumanyan <barbarian@armkb.com>").
-author("Yariv Sadan <yarivvv@gmail.com>").

%%% This module translates haXe types into the following Erlang types:
%%%
%%%     haXe                         Erlang
%%%     ----                         ------
%%%     int, float                   number
%%%     string (ascii, utf8)         string
%%%     string (base64)              binary
%%%     array                        {array, ElementList}
%%%     true, false, null            atoms 't', 'f', and 'n'
%%%     Number.NaN                   atom 'nan'
%%%     Number.NEGATIVE_INFINITY     atom 'neg_infinity'
%%%     Number.POSITIVE_INFINITY     atom 'infinity'
%%%     object                       tagged proplist with string (or atom) keys
%%%                                  (e.g. {struct, [{foo, "bar"}, {baz, 4}]} )
%%%     class object                 NOT SUPPORTED
%%%     enum                         NOT SUPPORTED
%%%     reference                    {ref, Idx}, Idx -> int()
%%%     exception                    {exception, Obj}, Obj is any
%%%                                  of the haXe types above
%%%
%%% Classs objects and Enums are currently not supported
%%% because simulating such types in Erlang is quite cumbersome, plus
%%% anonymous objects should be sufficient for most RPC needs.
%%% (If you strongly believe otherwise, contact me at yarivvv@gmail.com
%%% and I will consider adding class/enum support in a future version).
%%%
%%% References are handled transparently by the encoding/decoding
%%% functions. The decoding functions automatically expand
%%% references, and the encoding functions automatically serialize
%%% a string as a reference if an equal string has already been
%%% serialized (note: the haXe serializer can reference strings,
%%% arrays and objects, but the Erlang encoder only references
%%% strings due to the lack of address comparison in Erlang).
%%%
%%% If you wish to avoid expensive string comparisons
%%% in the encoder, or to have references to non-string objects,
%%% you can define a reference explicitly as a
%%% {ref, Idx} tuple. However, this usage is confusing and
%%% error prone, so it's better to avoid it except in extreme
%%% cases.

-define(L(Obj), io:format("~s:~w: ~w\n", [?MODULE, ?LINE, Obj])).

encode({ref, _Idx} = Ref) -> encode_basic(Ref);


encode(Obj) -> encode(Obj, true, 2).

%% The second parameter is a list of options.
%% The following options are available:
%%    {use_cache, boolean()}      -- default = true
%%    {haxe_version, Vsn}         -- default = 2, can be 1 or 2
%%
%% The haxe_version is a rough indication of what haXe version we
%% should encode to. This does not really capture the different
%% encodings used in the different haXe 1.x versions, see the
%% token_identifier function in this module for details.
%%
%% There second parameter can also be a boolean for backwards compatibility,
%% in which case the latest haxe_version (2) is assumed.
encode(Obj, Opts) when is_list(Opts) ->
    UseCache = proplists:get_value(use_cache, Opts, true),
    Vsn = proplists:get_value(haxe_version, Opts, 2),
    encode(Obj, UseCache, Vsn);
% function clauses for backwards compatibility:
encode(Obj, true=UseCache)  -> encode(Obj, UseCache, 2);
encode(Obj, false=UseCache) -> encode(Obj, UseCache, 2).


%% The second parameter, if a boolean, indicates whether the cache should
%% be used. By default, it's turned on (this follows the default configuration
%% in haXe).
encode(Obj, true, Vsn) ->
    {Result, _Cache} = encode(Obj, dict:new(), Vsn),
    Result;
encode(Obj, false, Vsn) ->
    {Result, _Cache} = encode(Obj, undefined, Vsn),
    Result;
encode(L, Cache, _Vsn=1) when is_list(L) ->
    case is_string(L) of
        yes -> encode_string1(L, $s, Cache);
        unicode -> encode_string1(xmerl_ucs:to_utf8(L), $j, Cache);
        no -> exit({error, {haxe_encode, {not_string, L}}})
    end;
encode(L, Cache, _Vsn=2) when is_list(L) ->
    case is_string(L) of
        no -> exit({error, {haxe_encode, {not_string, L}}});
        _  -> L2 = yaws_api:url_encode(xmerl_ucs:to_utf8(L)),
              encode_string1(L2, $y, Cache)
    end;
encode({array, Props}, Cache, Vsn) -> encode_array(Props, Cache, Vsn);
encode({struct, Props}, Cache, Vsn) -> encode_object(Props, Cache, Vsn);
encode({exception, E}, Cache, Vsn) ->
    {Result, Cache2} = encode(E, Cache, Vsn),
    {[$x | Result], Cache2};
encode(Term, Cache, _Vsn) ->
    case encode_basic(Term) of
        {error, _Err} = Err ->
            exit(Err);
        Result ->
            {Result, Cache}
    end.

encode_basic(Term) ->
    case Term of
        true -> "t";
        false -> "f";
        null -> "n";
        undefined -> "n";
        {ref, Idx} -> [$r | integer_to_list(Idx)];
        nan -> "k";
        infinity -> "p";
        neg_infinity -> "m";
        0 -> "z";
        I when is_integer(I) -> [$i | integer_to_list(I)];
        F when is_float(F) -> [$d | io_lib:format("~g", [F])];
        B when is_binary(B) -> S = binary_to_list(base64:encode(B)),
                               [$s, integer_to_list(length(S)), $: | S];
        _ -> {error, {bad_term, Term}}
    end.


%% Find a string in the list of previously encoded string
%% end return {true, {ref, Idx}}, if the search succeeded,
%% where Idx is the value associated with the string.
%% Otherwise, return 'false'.
find_ref(_Str, undefined) ->
    false;
find_ref(Str, CacheDict) ->
    case dict:find(Str, CacheDict) of
        error ->
            false;
        {ok, Idx} ->
            {true, Idx}
    end.

%% Encode an Erlang string to haXe.
encode_string(Str, Cache, 1=_Vsn) -> encode_string1(Str, $s, Cache);
encode_string(Str, Cache, 2=_Vsn) -> encode_string1(Str, $y, Cache).

encode_string1(Str, FirstChar, Cache) ->
    case find_ref(Str, Cache) of
        false ->
            {ok, Result} = encode_string2(Str, FirstChar),
            NewCache =
                case Cache of
                    undefined ->
                        undefined;
                    _ ->
                        dict:store(Str, dict:size(Cache), Cache)
                end,
            {Result, NewCache};
        {true, Idx} ->
            {[$R | integer_to_list(Idx)], Cache}
    end.
encode_string2(S, $y) ->
    {ok, [$y, integer_to_list(length(S)), $: | S]};
encode_string2(S, FirstChar) ->
    encode_string2(S, FirstChar, []).
encode_string2([], FirstChar, Acc) ->
    Str = lists:reverse(Acc),
    Len = integer_to_list(length(Str)),
    {ok, [FirstChar, Len, $: | Str]};
encode_string2([C | Cs], FirstChar, Acc) ->
    case C of
        $\\ -> encode_string2(Cs, FirstChar, [$\\, $\\ | Acc]);
        $\n -> encode_string2(Cs, FirstChar, [$n, $\\ | Acc]);
        $\r -> encode_string2(Cs, FirstChar, [$r, $\\ | Acc]);
        C when C =< 16#FFFF -> encode_string2(Cs, FirstChar, [C | Acc]);
        _ -> exit({error, {haxe_encode, {bad_char, C}}})
    end.

encode_object(Props, Cache, Vsn) ->
    {Result, Cache2} = encode_object_rest(Props, Cache, Vsn),
    {[$o | Result], Cache2}.
encode_object_rest(Props, Cache, Vsn) ->
    {EncodedProps, Cache1} =
        lists:foldl(
          fun({Key, Value}, {Acc, Cache2}) ->
                  {EncodedKey, Cache3} =
                      case Key of
                          L when is_list(L) ->
                              encode_string(L, Cache2, Vsn);
                          A when is_atom(A) ->
                              encode_string(atom_to_list(A), Cache2, Vsn);
                          _ ->
                              exit({error, {haxe_encode, {bad_key, Key}}})
                      end,
                  {EncodedVal, Cache4} = encode(Value, Cache3, Vsn),
                  case Acc of
                      [] ->
                          {[[EncodedKey, EncodedVal]], Cache4};
                      _  ->
                          {[[EncodedKey, EncodedVal] | Acc], Cache4}
                  end
          end,

          {[], Cache},
          Props),

    Result = [lists:reverse(EncodedProps), $g],
    {Result, Cache1}.

encode_array(Props, Cache, Vsn) ->
    {NullCount, Arr, Cache1} = lists:foldl(
          fun
              (Elem, {NullCount, Arr, Cache2}) when Elem == null;
                                                    Elem == undefined ->
                  {NullCount+1, Arr, Cache2};
              (Elem, {0, Arr, Cache2}) ->
                  {Encoded, Cache3} = encode(Elem, Cache2, Vsn),
                  {0, [Encoded | Arr], Cache3};
              (Elem, {NullCount, Arr, Cache2}) ->
                  {Encoded, Cache3}  = encode(Elem, Cache2, Vsn),
                  {0, [Encoded, encode_nulls(NullCount) | Arr], Cache3}
          end,
          {0, [$a], Cache}, Props),
    {lists:reverse([$h , encode_nulls(NullCount) | Arr]), Cache1}.

encode_nulls(0) -> [];
encode_nulls(1) -> [$n];
encode_nulls(Num) -> [$u | integer_to_list(Num)].

%%% SCANNING
%%%
%%% Scanning funs return either:
%%%    {done, Result, LeftOverChars}
%%% if a complete token is recognized, or
%%%    {more, Continuation}
%%% if more input is needed.
%%% Result is {ok, Term}, 'eof', or {error, Reason}.
%%% Here, the Continuation is a simple Erlang string.
%%%
%%% Currently, error handling is rather crude - errors are recognized
%%% by match failures.  EOF is handled only by number scanning, where
%%% it can delimit a number, and otherwise causes a match failure.
%%%

token_vsn1([]) -> {more, []};
token_vsn1(eof) -> {done, eof, []};
token_vsn1([C | Rest]) ->
    case token_identifier(1, C) of
        int -> scan_int(Rest);
        float -> scan_float(Rest);
        string -> scan_string(Rest);
        utf8 -> scan_utf8(Rest);
        error -> {done, {error, invalid_token}, Rest};
        Token -> {done, {ok, Token}, Rest}
    end.

token_vsn2([]) -> {more, []};
token_vsn2(eof) -> {done, eof, []};
token_vsn2([C | Rest]) ->
    case token_identifier(2, C) of
        int -> scan_int(Rest);
        float -> scan_float(Rest);
        string -> scan_string(Rest);
        url_encoded_string -> scan_url_encoded_string(Rest);
        base64_bytes -> scan_base64_bytes(Rest);
        error -> {done, {error, invalid_token}, Rest};
        Token -> {done, {ok, Token}, Rest}
    end.

token_identifier(Vsn, C) ->
    case C of
        $i -> int;
        $d -> float;
        $s -> case Vsn of
                  1 -> string;
                  2 -> base64_bytes %% redefined to bytes (base64) in haXe 2.0
              end;
        $j -> case Vsn of
                  1 -> utf8;
                  2 -> enum  %% redefined from utf8 to enum in haXe 1.16
              end;
        $n -> null;
        $t -> true;
        $f -> false;
        $z -> 0;
        $k -> nan;
        $p -> infinity;
        $m -> neg_infinity;
        $a -> array;
        $h -> array_end;
        $o -> obj;
        $c -> class;
        $g -> obj_end;
        $r -> ref;
        $R -> str_ref;
        $x -> exception;
        $y -> case Vsn of
                  1 -> error;
                  2 -> url_encoded_string %% utf8; added/defined in haXe 1.11
              end;

        %% this token is only valid as an element
        %% of an array
        $u -> null_seq;
        _ -> error
    end.

scan_utf8(Chars) ->
    scan_chars(Chars, utf8).

scan_string(Chars) ->
    scan_chars(Chars, string).

scan_url_encoded_string(Chars) ->
    scan_chars(Chars, url_encoded).

scan_base64_bytes(Chars) ->
    scan_chars(Chars, base64_bytes).

scan_chars(Chars, Type) ->
    case scan_int(Chars) of
        {done, {ok, _NumBytes}, []} ->
            {more, Chars};
        {done, {ok, _NumBytes}, [C | _Rest]} when C /= $: ->
            {done, {error, bad_char, C}, Chars};
        {done, {ok, NumBytes}, [_ | Rest]} when length(Rest) >= NumBytes ->
            case Type of
                utf8 ->
                    NewStr = xmerl_ucs:from_utf8(lists:sublist(Rest, NumBytes)),
                    scan_chars(NewStr, [], length(NewStr));
                string ->
                    scan_chars(Rest, [], NumBytes);
                url_encoded ->
                    {S, Rest2} = split_str_after(Rest, NumBytes),
                    S2 = xmerl_ucs:from_utf8(yaws_api:url_decode_with_encoding(S, latin1)),
                    {done, {ok, S2}, Rest2};
                base64_bytes ->
                    {S, Rest2} = split_str_after(Rest, NumBytes),
                    {done, {ok, base64:decode(S)}, Rest2}
            end;
        {done, {ok, _NumBytes}, _Rest} ->
            {more, Chars};
        Other ->
            Other
    end.

scan_chars(Rest, A, 0) ->
    {done, {ok, lists:reverse(A)}, Rest};
scan_chars([$\\] = Chars, _A, 0) ->
    {done, {error, missing_escape_character}, Chars};
scan_chars([$\\, C | Rest], A, NumLeft) ->
    scan_chars(Rest, [esc_to_char(C) | A], NumLeft - 2);
scan_chars([C | Rest], A, NumLeft) ->
    scan_chars(Rest, [C | A], NumLeft - 1).

esc_to_char(C) ->
    case C of
        $n -> $\n;
        $r -> $\r;
        $\\ -> $\\
    end.

split_str_after(S, AfterNumBytes) ->
    {string:substr(S, 1, AfterNumBytes), string:substr(S, AfterNumBytes + 1)}.


scan_float(Chars) ->
    scan_number(Chars, float).

scan_int(Chars) ->
    scan_number(Chars, int).

scan_number([], _Type) -> {more, []};
scan_number(eof, _Type) -> {done, {error, incomplete_number}, []};
scan_number([$-, $- | _Rest] = Input, _Type) ->
    {done, {error, invalid_number}, Input};
scan_number([$- | Ds] = Input, Type) ->
    case scan_number(Ds, Type) of
        {more, _Cont} -> {more, Input};
        {done, {ok, N}, CharList} -> {done, {ok, -1 * N}, CharList};
        {done, Other, Chars} -> {done, Other, Chars}
    end;
scan_number([D | Ds] = Input, Type) when D >= $0, D =< $9 ->
    scan_number(Ds, D - $0, Input, Type).

%% Numbers don't have a terminator, so stop at the first non-digit,
%% and ask for more if we run out.

scan_number([], _Num, X, _Type) -> {more, X};
scan_number(eof, Num, _X, _Type) -> {done, {ok, Num}, eof};
scan_number([$.], _Num, X, float) -> {more, X};
scan_number([$., D | Ds], Num, X, float) when D >= $0, D =< $9 ->
    scan_fraction([D | Ds], Num, X);
scan_number([D | Ds], Num, X, Type) when Num > 0, D >= $0, D =< $9 ->
    % Note that nonzero numbers can't start with "0".
    scan_number(Ds, 10 * Num + (D - $0), X, Type);
scan_number([D | Ds], Num, X, float) when D == $E; D == $e ->
    scan_exponent_begin(Ds, float(Num), X);
scan_number([D | _] = Ds, Num, _X, _Type) when D < $0; D > $9 ->
    {done, {ok, Num}, Ds}.

scan_fraction(Ds, I, X) -> scan_fraction(Ds, [], I, X).
scan_fraction([], _Fs, _I, X) -> {more, X};
scan_fraction(eof, Fs, I, _X) ->
    R = I + list_to_float("0." ++ lists:reverse(Fs)),
    {done, {ok, R}, eof};
scan_fraction([D | Ds], Fs, I, X) when D >= $0, D =< $9 ->
    scan_fraction(Ds, [D | Fs], I, X);
scan_fraction([D | Ds], Fs, I, X) when D == $E; D == $e ->
    R = I + list_to_float("0." ++ lists:reverse(Fs)),
    scan_exponent_begin(Ds, R, X);
scan_fraction(Rest, Fs, I, _X) ->
    R = I + list_to_float("0." ++ lists:reverse(Fs)),
    {done, {ok, R}, Rest}.

scan_exponent_begin(Ds, R, X) ->
    scan_exponent_begin(Ds, [], R, X).
scan_exponent_begin([], _Es, _R, X) -> {more, X};
scan_exponent_begin(eof, _Es, _R, X) -> {done, {error, missing_exponent}, X};
scan_exponent_begin([D | Ds], Es, R, X) when D == $-;
                                             D == $+;
                                             D >= $0, D =< $9 ->
    scan_exponent(Ds, [D | Es], R, X).

scan_exponent([], _Es, _R, X) -> {more, X};
scan_exponent(eof, Es, R, _X) ->
    X = R * math:pow(10, list_to_integer(lists:reverse(Es))),
    {done, {ok, X}, eof};
scan_exponent([D | Ds], Es, R, X) when D >= $0, D =< $9 ->
    scan_exponent(Ds, [D | Es], R, X);
scan_exponent(Rest, Es, R, _X) ->
    X = R * math:pow(10, list_to_integer(lists:reverse(Es))),
    {done, {ok, X}, Rest}.

%%% PARSING
%%%
%%% The decode function takes a char list as input, but
%%% interprets the end of the list as only an end to the available
%%% input, and returns a "continuation" requesting more input.
%%% When additional characters are available, they, and the
%%% continuation, are fed into decode/2.  You can use the atom 'eof'
%%% as a character to signal a true end to the input stream, and
%%% possibly flush out an unfinished number.  The decode_string/1
%%% function appends 'eof' to its input and calls decode/1.
%%%
%%% Parsing and scanning errors are handled only by match failures.
%%% The external caller must take care to wrap the call in a "catch"
%%% or "try" if better error-handling is desired.  Eventually parse
%%% or scan errors will be returned explicitly with a description,
%%% and someday with line numbers too.
%%%
%%% The parsing code uses a continuation-passing style to allow
%%% for the parsing to suspend at any point and be resumed when
%%% more input is available.
%%% See http://en.wikipedia.org/wiki/Continuation_passing_style


%% Return the first haXe value decoded from the input string.
%% The string must contain at least one complete haXe value.
%%
%% The second (optional) parameter can be either:
%% - a list of options:
%%     {use_cache, boolean()}   -- default = true
%%     {haxe_version, Vsn}      -- default = 2, can be 1 or 2
%% - a continuation, if {more,Continuation} was previously
%%   returned and you now have more characters
%% - a boolean specifying whether to use the cache,
%%   for backwards compatibility
%%
%% The haxe_version is a rough indication of what haXe version we
%% should expect to decode. This does not really capture the different
%% encodings used in the different haXe 1.x versions, see the
%% token_identifier function in this module for details.
%%
%% There second parameter can also be a boolean for backwards compatibility,
%% in which case the latest haxe_version (2) is assumed.
%%
%% Caching is on by default, but it can be turned
%% off for performance and lower memory consumption depending
%% on the content of the messages.
decode_string(CharList) ->
    decode_string(CharList, [{use_cache, true}]).
decode_string(CharList, Opts) ->
    {done, V, _} = decode(CharList ++ [eof], Opts),
    V.

%% Attempt to decode a haXe value from the input string
%% and continuation, using an empty initial continuation.
%% Return {done, Result, Continuation} if a value is recognized,
%% or {more, Continuation} if more input characters are needed.
%% The Result can be {ok, Value}, eof, or {error, Reason}.
%% The Continuation is then fed as an argument to decode/2 when
%% more input is available.
%% Use the atom 'eof' instead of a char list to signal
%% a true end to the input, and may flush a final number.
%%
%% Use the get_left_over_chars/1 function to retrieve any non-consumed
%% characters in the continuation.

decode(CharList) ->
    decode_o(CharList, [{use_cache, true}]).

-record(cont,{chars,
              cache,
              tokenizer}).

decode(CharList, Opts) when is_list(Opts) ->
    decode_o(CharList, Opts);
decode(CharList, Cont) when is_record(Cont, cont) -> %% Continuation case
    #cont{chars = Chars, cache = Cache, tokenizer = Tokenizer} = Cont,
    decode2(Chars ++ CharList, Cache, Tokenizer);
decode(CharList, true) ->  %% function clause for backwards compatibility
    decode_o(CharList, [{use_cache, true}]);
decode(CharList, false) -> %% function clause for backwards compatibility
    decode_o(CharList, [{use_cache, false}]).

decode_o(CharList, Opts) ->
    %% Note: our Cache for decoding is the tuple
    %% {ObjCache::list(), StringCache::dict()}
    Cache = case proplists:get_value(use_cache, Opts, true) of
                true  -> {[], dict:new()};
                false -> undefined
            end,
    Tokenizer = case proplists:get_value(haxe_version, Opts, 2) of
                    2 -> fun token_vsn2/1;
                    1 -> fun token_vsn1/1 %% haxe uses "old_serializer"
                end,
    decode2(CharList, Cache, Tokenizer).


decode2(CharList, Cache, Tokenizer) ->
    Cont = #cont{chars = CharList,
                 cache = Cache,
                 tokenizer = Tokenizer},
    get_token(Cont, fun first_cont_fun/2).


decode_next(Cont) ->
    get_token(Cont, fun first_cont_fun/2).

get_left_over_chars(#cont{chars=Cs}) ->
    Cs.


first_cont_fun(eof, Cs) -> {done, eof, Cs};
first_cont_fun(T, Cs) ->
    parse_value(T, Cs,
                fun(V, C2) ->
                        {done, {ok, V}, C2}
                end).

%% Continuation Kt must accept (TokenOrEof, {Chars, Cache, Tokenizer})

get_token(C = #cont{chars=Chars, tokenizer=Tokenizer}, Kt) ->
    case Tokenizer(Chars) of
        {done, {ok, T}, Rest} -> Kt(T, C#cont{chars=Rest});
        {done, eof, Rest} -> Kt(eof, C#cont{chars=Rest});
        {done, {error, Reason}, Rest} -> {done, {error, Reason},
                                          C#cont{chars=Rest}};
        {more, X} -> {more, C#cont{chars=X}}
    end.

%% Continuation Kv must accept (Value, {Chars, Cache, Tokenizer})

parse_value(Token, C, Kv) ->
    parse_value(Token, C, Kv, false).
parse_value(Token, #cont{cache=Cache} = C, Kv, AcceptNullSeq)->
    case Token of
        eof -> {done, {error, premature_eof}, C};
        T when T == null; T == true; T == false; T == nan;
               T == infinity; T == neg_infinity ->
            Kv(T, C);
        obj -> parse_object(C, Kv);
        class -> parse_class(C, Kv);
        array -> parse_array(C, Kv);
        enum -> parse_enum(C, Kv);
        exception -> parse_exception(C, Kv);
        ref -> parse_ref(C, Kv);
        str_ref -> parse_ref(C, Kv, true);
        null_seq when AcceptNullSeq -> parse_null_seq(C, Kv);
        Str when is_list(Str) -> Kv(Str, C#cont{cache=put_str(Str, Cache)});
        Num when is_number(Num) -> Kv(Num, C);
        B   when is_binary(B) -> Kv(B, C#cont{cache=put_str(B, Cache)});
        X -> {done, {error, syntax_error,X}, C}
    end.

parse_class(Chars, _Kv) ->
    {done, {error, class_objects_not_supported}, Chars}.

parse_object(Chars, Kv) ->
    parse_object(Chars, Kv, obj_new()).

parse_object(C, Kv, Obj) ->
    get_token(C,
              fun(T, C2=#cont{cache=Cache}) ->
                      case T of
                          obj_end ->
                              NewCache =
                                  put_obj(Obj, Cache),
                              Kv(Obj, C2#cont{cache=NewCache});
                          _ ->
                              NewCache =
                                  put_obj(placeholder, Cache),
                              parse_object_field(
                                Obj, T,
                                C2#cont{cache=NewCache}, Cache, Kv)
                      end
              end).

put_obj(_xObj, undefined) ->
    undefined;
put_obj(Obj, {ObjCache, StrCache}) ->
    {[Obj | ObjCache], StrCache}.

put_str(_Str, undefined) ->
    undefined;
put_str(Str, {ObjCache, StrCache}) ->
    {ObjCache, dict:store(dict:size(StrCache), Str, StrCache)}.

parse_object_field(_Obj, eof, C, _OrigCache, _Kv) ->
    {done, {error, premature_eof}, C};

%% if the key is a reference, we deference it and continue
parse_object_field(Obj, RefType, C, OrigCache, Kv)
  when RefType == ref; RefType == str_ref ->
    parse_ref(C,
              fun(Key, C1) ->
                      parse_object_val(Obj, Key, C1, OrigCache, Kv)
              end,
             RefType == str_ref);

%% if the key is a string, we put it in the cache and continue
parse_object_field(Obj, Field, C=#cont{cache=Cache}, OrigCache, Kv)
  when is_list(Field) ->
    NewCache = put_str(Field, Cache),
    parse_object_val(Obj, Field, C#cont{cache=NewCache}, OrigCache, Kv).

parse_object_val(Obj, Field, C, OrigCache, Kv)
  when is_list(Field) ->
    get_token(C,
              fun(T, C2) ->
                      parse_value
                        (T, C2,
                         fun(Val, C3) ->
                                 Obj2 = obj_store(Field, Val, Obj),
                                 parse_object_next(Obj2, C3, OrigCache, Kv)
                         end)
              end);
parse_object_val(_Obj, Field, C, _OrigCache, _Kv) ->
    {done, {error, {member_name_not_string, Field}}, C}.

parse_object_next({struct, Props} = Obj, C, OrigCache, Kv) ->
    get_token(C,
              fun
                  (obj_end, C1 = #cont{cache=Cache}) ->
                      Obj1 = {struct, lists:reverse(Props)},
                      Cache1 = append_new_elems(
                                    Cache, Obj1, OrigCache),
                      Kv(Obj1, C1#cont{cache=Cache1});
                  (eof, C1) ->
                      {done, {error, premature_eof}, C1};
                  (T, C1) ->
                      parse_object_field(Obj, T, C1, OrigCache, Kv)
              end).


append_new_elems(undefined, _Obj, _Cache) ->
    undefined;
append_new_elems({TempObjCache, NewStrCache}, Obj,
                 {ObjCache, _OrigStrCache}) ->
    ObjCache1 = [Obj | ObjCache],
    NumNewElts = length(TempObjCache) - length(ObjCache1),
    NewElts = lists:sublist(TempObjCache, NumNewElts),
    {NewElts ++ ObjCache1, NewStrCache}.


parse_array(C = #cont{cache=Cache}, Kv) ->

    %% We need to put a temporary placeholder in the cache
    %% to comply with haXe's indexing scheme, which assumes
    %% the array is put in the cache *before* its members.
    %% When we finish parsing the array, we collect the new
    %% cache entries and put them in the old cache after
    %% first inserting the fully parsed array.
    %%
    %% If it sounds backwards, well, it is! :)

    parse_array([], C#cont{cache=put_obj(placeholder, Cache)},
                Cache, Kv).

parse_array(Elems, C, OrigCache, Kv) ->
    get_token(C,
              fun
                  (eof, C1) -> {done, {error, premature_eof}, C1};
                  (array_end, C1=#cont{cache=Cache1}) ->
                      Arr = {array, lists:reverse(Elems)},
                      Cache2 = append_new_elems(
                                 Cache1, Arr, OrigCache),
                      Kv(Arr, C1#cont{cache=Cache2});
                  (T, C1) ->
                      parse_array_tok(Elems, T, C1, OrigCache, Kv)
              end).

parse_array_tok(Elems, T, Cont, OrigCache, Kv) ->
    parse_value(T, Cont,
                fun({null_seq, Nulls}, C1) ->
                        parse_array(Nulls ++ Elems, C1, OrigCache, Kv);
                   (V, C1) ->
                        parse_array([V | Elems], C1, OrigCache, Kv)
                end,
               true).

parse_enum(Chars, _Kv) ->
    {done, {error, enums_not_supported}, Chars}.

%% it's safe to assume we'll never have to parse exceptions
%% on the server side, but this function is here for completeness
parse_exception(Cont, Kv) ->
    get_token(Cont,
              fun(T, C1) ->
                      parse_value(T, C1,
                                  fun(Val, C2) ->
                                          Kv({exception, Val}, C2)
                                  end)
              end).


%%% The next three functions help with storing references
%%% to deserialized objects for future lookup during decoding

parse_ref(Cont, Kv) ->
    parse_ref(Cont, Kv, false).

parse_ref(C=#cont{chars=Chars}, Kv, IsStrRef) ->
    case scan_int(Chars) of
        {done, {ok, Idx}, Chars1} ->
            parse_ref2(IsStrRef, C#cont{chars=Chars1}, Idx, Kv);
        Other ->
            Other
    end.

parse_ref2(_IsStrRef, #cont{cache=undefined} = Cont, _Idx, _Kv) ->
    {done, {error, references_disabled}, Cont};

parse_ref2(false, #cont{cache={ObjCache, _StrCache}} = Cont, Idx, Kv) ->
    if Idx > length(ObjCache) ->
            {done, {error, {ref_idx_out_of_bounds, Idx}}, Cont};
       true ->
            Val = lists:nth(length(ObjCache) - Idx, ObjCache),
            if
                Val == placeholder ->
                    {done, {error, {illegal_ref, Idx}}, Cont};
                true ->
                    Kv(Val, Cont)
            end
    end;

parse_ref2(true, #cont{cache={_ObjCache, StrCache}} = Cont, Idx, Kv) ->
    Cond = Idx + 1 > dict:size(StrCache),
    if Cond ->
            {done, {error, {str_ref_idx_out_of_bounds, Idx}}, Cont};
       true ->
            {ok, Val} = dict:find(Idx, StrCache),
            Kv(Val, Cont)
    end.



parse_null_seq(Cont = #cont{chars=Chars}, Kv) ->
    case scan_int(Chars) of
        {done, {ok, Num}, C1} when Num > 0 ->
            Cont1 = Cont#cont{chars=C1},
            Kv({null_seq, lists:duplicate(Num, null)}, Cont1);
        Other ->
            Other
    end.

%%% OBJECTS
%%%
%%% We'll use tagged property lists as the internal representation
%%% of haXe objects.  Unordered lists perform worse than trees for
%%% lookup and modification of members, but we expect objects to be
%%% have only a few members.  Lists also print better.

is_obj(_) ->
    false.

%% create a simple haXe object
obj_new() ->
    {struct, []}.

%% Fetch an object member's value, expecting it to be in the object.
%% Return value, runtime error if no member found with that name.
obj_fetch(Key, {struct, Props}) ->
    case proplists:get_value(Key, Props) of
        undefined ->
            exit({struct_no_key, Key});
        Value ->
            Value
    end.

%% Fetch an object member's value, or indicate that there is no such member.
%% Return {ok, Value} or 'error'.

obj_find(Key, {struct, Props}) ->
    case proplists:get_value(Key, Props) of
        undefined ->
            error;
        Value ->
            {ok, Value}
    end.

obj_is_key(Key, {struct, Props}) ->
    proplists:is_defined(Key, Props).

%% Store a new member in an object.  Returns a new object.

obj_store(KeyStr, Value, {struct, Props}) ->
    Key = list_to_atom(KeyStr),
    NewProps = [{Key, Value} | proplists:delete(Key, Props)],
    {struct, NewProps}.

%% Create an object from a list of Key/Value pairs.

obj_from_list(Props) ->
    {struct, {Props}}.

%% Fold Fun across object, with initial accumulator Acc.
%% Fun should take (Value, Acc) as arguments and return Acc.

obj_fold(Fun, Acc, {struct, Props}) ->
    lists:foldl(Fun, Acc, Props).

is_string([]) -> yes;
is_string(List) -> is_string(List, non_unicode).

is_string([C|Rest], non_unicode) when C >= 0, C =< 255 ->
    is_string(Rest, non_unicode);
is_string([C|Rest], _) when C =< 65000 -> is_string(Rest, unicode);
is_string([], non_unicode) -> yes;
is_string([], unicode) -> unicode;
is_string(_, _) -> no.


test() ->
    Tests = [
             {1, "i1"},
             {1.1, "d1.10000"},
             {"foo", "y3:foo"},
             %% todo test utf8
             {null, "n"},
             {true, "t"},
             {false, "f"},
             {0, "z"},
             {nan, "k"},
             {infinity, "p"},
             {neg_infinity, "m"},
             {{array, [1,2,3]}, "ai1i2i3h"},
             {{array, [null]}, "anh"},
             {{array, [null, null]}, "au2h"},
             {{array, [3, 4, null, null, null, 5, null]}, "ai3i4u3i5nh"},
             {{struct, [{foo, "bar"}, {baz, "boing"}]},
              "oy3:fooy3:bary3:bazy5:boingg"},
             {{array, ["foo", "bar", "foo",
                       {struct, [{bar, "baz"}, {foo, 123}]}]},
              "ay3:fooy3:bary3:foooy3:bary3:bazy3:fooi123gh"},
             {{exception, "bad"}, "xy3:bad"}
            ],

    {Passed, Failed} = run_tests(Tests, false),

    Tests1 = [
              {{array, ["foo", "bar", "foo"]}, "ay3:fooy3:barR0h"},
              {{struct, [{foo, "bar"}, {bar, "foo"}]}, "oy3:fooy3:barR1R0g"},
              {{array, ["foo", "bar", "foo",
                        {struct, [{bar, "baz"}, {foo, 123}]}]},
               "ay3:fooy3:barR0oR1y3:bazR0i123gh"}
             ],
    {Passed1, Failed1} = run_tests(Tests1, true),

    Tests2 = [{"abc"++[16#e5,16#e4,16#f6],   %% a-ring,a-diaeresis,o-diaeresis
               "y21:abc%C3%A5%C3%A4%C3%B6"}, %% url-encoded utf8
              {<<"abc">>,
               "s4:YWJj"}],
    {Passed2, Failed2} = run_tests(Tests2, [{haxe_version,2}]),

    %% version1 encode/decode (simple utf8)
    %% This is mostly based on looking at the code of the haxe
    %% Serializer.hx and Unserializer.hx (as of cvs-tag v1-10)
    Tests3 = [{"abc\n"++[16#1e5,16#1e4,16#1f6],
	       "j11:abc\\n"++[16#c7,16#a5,16#c7,16#a4,16#c7,16#b6]},
              {"abc",
               "s3:abc"}],
    {Passed3, Failed3} = run_tests(Tests3, [{haxe_version,1}]),

    io:format("passed: ~w, failed: ~w\n",
              [Passed + Passed1 + Passed2 + Passed3,
               Failed + Failed1 + Failed2 + Failed3]).

run_tests(Tests, EnableReferences) ->
    lists:foldl(
      fun({Term, Str}, Agg) ->
              Encoded = lists:flatten(encode(Term,EnableReferences)),
              {ok, Decoded} = decode_string(Str,EnableReferences),
              Check = fun(Val1, Val2, {P, F}) ->
                              case Val1 == Val2 of
                                  true -> {P + 1, F};
                                  _ -> {P, F + 1}
                              end
                      end,
              Agg1 = Check(Str, Encoded, Agg),
              Agg2 = Check(Term, Decoded, Agg1),
              io:format("~s == ~s\n~w\n~w == ~w\n~w\n\n",
                        [Str, Encoded, Str == Encoded,
                         Term, Decoded, Term == Decoded]),
              Agg2
      end,
      {0, 0}, Tests).
