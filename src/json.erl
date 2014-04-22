%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WARNING DEPRECATED WARNING DEPRECATED WARNING DEPRECATED WARNING DEPRECATED
%%% WARNING DEPRECATED WARNING DEPRECATED WARNING DEPRECATED WARNING DEPRECATED
%%%
%%% Use module json2.erl instead
%%%
%%% This module is deprecated. It uses list_to_atom and so could potentially
%%% fill the atom table. It also fails to pass its own internal tests due to
%%% changes made years ago outside the context of Yaws.
%%%
%%% Do not report problems with this module, as they will not be fixed. You
%%% should instead convert your code to use the json2 module.
%%%
%%% WARNING DEPRECATED WARNING DEPRECATED WARNING DEPRECATED WARNING DEPRECATED
%%% WARNING DEPRECATED WARNING DEPRECATED WARNING DEPRECATED WARNING DEPRECATED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%(%% Copyright (c) 2005-2006, A2Z Development USA, Inc.  All Rights Reserved.
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

-module(json).
-deprecated(module).
-export([encode/1, decode_string/1, decode/2]).
-export([is_obj/1, obj_new/0, obj_fetch/2, obj_find/2, obj_is_key/2]).
-export([obj_store/3, obj_from_list/1, obj_fold/3]).
-export([test/0]).
-author("Jim Larson <jalarson@amazon.com>, Robert Wai-Chi Chu <robchu@amazon.com>").
-author("Gaspar Chilingarov <nm@web.am>, Gurgen Tumanyan <barbarian@armkb.com>").

%%% JavaScript Object Notation ("JSON", http://www.json.org) is a simple
%%% data syntax meant as a lightweight alternative to other representations,
%%% such as XML.  JSON is natively supported by JavaScript, but many
%%% other languages have conversion libraries available.
%%%
%%% This module translates JSON types into the following Erlang types:
%%%
%%%     JSON                    Erlang
%%%     ----                    ------
%%%     number                  number
%%%     string                  string
%%%     array                   {array, ElementList}
%%%     object                  tagged proplist with string (or atom) keys (i.e. {struct, PropList} )
%%%     true, false, null       atoms 'true', 'false', and 'null'
%%%
%%% Character Sets: the external representation, and the internal
%%% representation of strings, are lists of UTF-16 code units.
%%% The encoding of supplementary characters, as well as
%%% transcoding to other schemes, such as UTF-8, can be provided
%%% by other modules.  (See discussion at
%%% http://groups.yahoo.com/group/json/message/52)
%%%
%%%######################################################################
%%% UPD by Gaspar: for this moment utf-8 encoding inplemented by default
%%%                if incoming character list have symbols with codes
%%%                > 255
%%%######################################################################
%%%
%%% Numbers: Thanks to Erlang's bignums, JSON-encoded integers of any
%%% size can be parsed.  Conversely, extremely large integers may
%%% be JSON-encoded.  This may cause problems for interoperability
%%% with JSON parsers which can't handle arbitrary-sized integers.
%%% Erlang's floats are of fixed precision and limited range, so
%%% syntactically valid JSON floating-point numbers could silently
%%% lose precision or noisily cause an overflow.  However, most
%%% other JSON libraries are likely to behave in the same way.
%%%
%%% Strings: If we represented JSON string data as Erlang binaries,
%%% we would have to choose a particular unicode format.  Instead,
%%% we use lists of UTF-16 code units, which applications may then
%%% change to binaries in their application-preferred manner.
%%%
%%% Arrays: Because of the string decision above, and Erlang's
%%% lack of a distinguished string datatype, JSON arrays map
%%% to Erlang tuples.  Consider utilities like tuple_fold/3
%%% to deal with tuples in their native form.
%%%######################################################################
%%% UPD by Gaspar: array changed to {array, ArrayElementList}
%%%                ArrayElementList -> list
%%%                to provide compatibility to xmlrpc module
%%%######################################################################
%%%
%%% Objects: Though not explicitly stated in the JSON "spec",
%%% JSON's JavaScript heritage mandates that member names must
%%% be unique within an object.  The object/tuple ambiguity is
%%% not a problem, since the atom 'struct' is not an
%%% allowable value.  Object keys may be atoms or strings on
%%% encoding but are always decoded as strings.
%%%
%%%######################################################################
%%% UPD by Gaspar: struct changed to {array, PropList}
%%%                object keys always decoded to atoms to
%%%                provide full compatility with xmlrpc module
%%%######################################################################
%%%

%%% ENCODING

%% Encode an erlang number, string, tuple, or object to JSON syntax, as a
%% possibly deep list of UTF-16 code units, throwing a runtime error in the
%% case of un-convertible input.
%% Note: object keys may be either strings or atoms.

encode(true) -> "true";
encode(false) -> "false";
encode(null) -> "null";
encode(undefined) -> "null";
encode(B) when is_binary(B) -> encode_string(B);
encode(I) when is_integer(I) -> integer_to_list(I);
encode(F) when is_float(F) -> float_to_list(F);
encode(L) when is_list(L) ->
    case is_string(L) of
        yes -> encode_string(L);
        unicode -> encode_string(xmerl_ucs:to_utf8(L));
        no -> encode({array, L})
    end;
encode({array, Props}) when is_list(Props) -> encode_array(Props);
encode({struct, Props} = T) when is_list(Props) -> encode_object(T);
encode(Bad) -> exit({json_encode, {bad_term, Bad}}).

%% Encode an Erlang string to JSON.
%% Accumulate strings in reverse.

encode_string(B) when is_binary(B) -> encode_string(binary_to_list(B));
encode_string(S) -> encode_string(S, [$"]).

encode_string([], Acc) -> lists:reverse([$" | Acc]);
encode_string([C | Cs], Acc) ->
    case C of
        $" -> encode_string(Cs, [$", $\\ | Acc]);
        % (don't escape solidus on encode)
        $\\ -> encode_string(Cs, [$\\, $\\ | Acc]);
        $\b -> encode_string(Cs, [$b, $\\ | Acc]);      % note missing \
        $\f -> encode_string(Cs, [$f, $\\ | Acc]);
        $\n -> encode_string(Cs, [$n, $\\ | Acc]);
        $\r -> encode_string(Cs, [$r, $\\ | Acc]);
        $\t -> encode_string(Cs, [$t, $\\ | Acc]);
        C when C >= 0, C < $\s ->
            % Control characters must be unicode-encoded.
            Hex = lists:flatten(io_lib:format("~4.16.0b", [C])),
            encode_string(Cs, lists:reverse(Hex) ++ "u\\" ++ Acc); % "
        C when C =< 16#FFFF -> encode_string(Cs, [C | Acc]);
        _ -> exit({json_encode, {bad_char, C}})
    end.

%% Encode an Erlang object as a JSON object, allowing string or atom keys.
%% Note that order is irrelevant in both internal and external object
%% representations.  Nevertheless, the output will respect the order
%% of the input.

encode_object({struct, _Props} = Obj) ->
    M = obj_fold(fun({Key, Value}, Acc) ->
        S = case Key of
                B when is_binary(B) -> encode_string(B);
                L when is_list(L) ->
                    case is_string(L) of
                        yes -> encode_string(L);
                        unicode -> encode_string(xmerl_ucs:to_utf8(L));
                        no -> exit({json_encode, {bad_key, Key}})
                    end;
                A when is_atom(A) -> encode_string(atom_to_list(A));
                _ -> exit({json_encode, {bad_key, Key}})
            end,
        V = encode(Value),
        case Acc of
            [] -> [S, $:, V];
            _ -> [Acc, $,, S, $:, V]
        end
    end, [], Obj),
    [${, M, $}].

%% Encode an Erlang tuple as a JSON array.
%% Order *is* significant in a JSON array!

encode_array(T) ->
    M = lists:foldl(fun(E, Acc) ->
        V = encode(E),
        case Acc of
            [] -> V;
            _ -> [Acc, $,, V]
        end
    end, [], T),
    [$[, M, $]].

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
%%% Tokens are one of the following
%%% JSON string -> erlang string
%%% JSON number -> erlang number
%%% true, false, null -> erlang atoms
%%% { } [ ] : , -> lcbrace rcbrace lsbrace rsbrace colon comma

token([]) -> {more, []};
token(eof) -> {done, eof, []};

token("true" ++ Rest) -> {done, {ok, true}, Rest};
token("tru")    -> {more, "tru"};
token("tr")     -> {more, "tr"};
token("t")      -> {more, "t"};

token("false" ++ Rest) -> {done, {ok, false}, Rest};
token("fals")   -> {more, "fals"};
token("fal")    -> {more, "fal"};
token("fa")     -> {more, "fa"};
token("f")      -> {more, "f"};

token("null" ++ Rest) -> {done, {ok, null}, Rest};
token("nul")    -> {more, "nul"};
token("nu")     -> {more, "nu"};
token("n")      -> {more, "n"};

token([C | Cs] = Input) ->
    case C of
        $\s -> token(Cs);       % eat whitespace
        $\t -> token(Cs);       % eat whitespace
        $\n -> token(Cs);       % eat whitespace
        $\r -> token(Cs);       % eat whitespace
        $" -> scan_string(Input);
        $- -> scan_number(Input);
        D when D >= $0, D =< $9-> scan_number(Input);
        ${ -> {done, {ok, lcbrace}, Cs};
        $} -> {done, {ok, rcbrace}, Cs};
        $[ -> {done, {ok, lsbrace}, Cs};
        $] -> {done, {ok, rsbrace}, Cs};
        $: -> {done, {ok, colon}, Cs};
        $, -> {done, {ok, comma}, Cs};
        $/ -> case scan_comment(Cs) of
            {more, X} -> {more, X};
            {done, _, Chars} -> token(Chars)
        end;
        _ -> {done, {error, {bad_char, C}}, Cs}
    end.

scan_string([$" | Cs] = Input) ->
    scan_string(Cs, [], Input).

%% Accumulate in reverse order, save original start-of-string for continuation.

scan_string([], _, X) -> {more, X};
scan_string(eof, _, X) -> {done, {error, missing_close_quote}, X};
scan_string([$" | Rest], A, _) -> {done, {ok, lists:reverse(A)}, Rest};
scan_string([$\\], _, X) -> {more, X};
scan_string([$\\, $u, U1, U2, U3, U4 | Rest], A, X) ->
    scan_string(Rest, [uni_char([U1, U2, U3, U4]) | A], X);
scan_string([$\\, $u | _], _, X) -> {more, X};
scan_string([$\\, C | Rest], A, X) ->
    scan_string(Rest, [esc_to_char(C) | A], X);
scan_string([C | Rest], A, X) ->
    scan_string(Rest, [C | A], X).

%% Given a list of hex characters, convert to the corresponding integer.

uni_char(HexList) ->
    erlang:list_to_integer(HexList, 16).

esc_to_char($") -> $";
esc_to_char($/) -> $/;
esc_to_char($\\) -> $\\;
esc_to_char($b) -> $\b;
esc_to_char($f) -> $\f;
esc_to_char($n) -> $\n;
esc_to_char($r) -> $\r;
esc_to_char($t) -> $\t.

scan_number([]) -> {more, []};
scan_number(eof) -> {done, {error, incomplete_number}, []};
scan_number([$-, $- | _Ds]) -> {done, {error, invalid_number}, []};
scan_number([$- | Ds] = Input) ->
    case scan_number(Ds) of
        {more, _Cont} -> {more, Input};
        {done, {ok, N}, CharList} -> {done, {ok, -1 * N}, CharList};
        {done, Other, Chars} -> {done, Other, Chars}
    end;
scan_number([D | Ds] = Input) when D >= $0, D =< $9 ->
    scan_number(Ds, D - $0, Input).

%% Numbers don't have a terminator, so stop at the first non-digit,
%% and ask for more if we run out.

scan_number([], _A, X) -> {more, X};
scan_number(eof, A, _X) -> {done, {ok, A}, eof};
scan_number([$.], _A, X) -> {more, X};
scan_number([$., D | Ds], A, X) when D >= $0, D =< $9 ->
    scan_fraction([D | Ds], A, X);
scan_number([D | Ds], A, X) when A > 0, D >= $0, D =< $9 ->
    % Note that nonzero numbers can't start with "0".
    scan_number(Ds, 10 * A + (D - $0), X);
scan_number([D | Ds], A, X) when D == $E; D == $e ->
    scan_exponent_begin(Ds, integer_to_list(A) ++ ".0", X);
scan_number([D | _] = Ds, A, _X) when D < $0; D > $9 ->
    {done, {ok, A}, Ds}.

scan_fraction(Ds, I, X) -> scan_fraction(Ds, [], I, X).

scan_fraction([], _Fs, _I, X) -> {more, X};
scan_fraction(eof, Fs, I, _X) ->
    R = list_to_float(lists:append([integer_to_list(I), ".",
                                    lists:reverse(Fs)])),
    {done, {ok, R}, eof};
scan_fraction([D | Ds], Fs, I, X) when D >= $0, D =< $9 ->
    scan_fraction(Ds, [D | Fs], I, X);
scan_fraction([D | Ds], Fs, I, X) when D == $E; D == $e ->
    R = lists:append([integer_to_list(I), ".", lists:reverse(Fs)]),
    scan_exponent_begin(Ds, R, X);
scan_fraction(Rest, Fs, I, _X) ->
    R = list_to_float(lists:append([integer_to_list(I), ".",
                                    lists:reverse(Fs)])),
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
    X = list_to_float(lists:append([R, "e", lists:reverse(Es)])),
    {done, {ok, X}, eof};
scan_exponent([D | Ds], Es, R, X) when D >= $0, D =< $9 ->
    scan_exponent(Ds, [D | Es], R, X);
scan_exponent(Rest, Es, R, _X) ->
    X = list_to_float(lists:append([R, "e", lists:reverse(Es)])),
    {done, {ok, X}, Rest}.

scan_comment([]) -> {more, "/"};
scan_comment(eof) -> {done, eof, []};
scan_comment([$/ | Rest]) -> scan_cpp_comment(Rest);
scan_comment([$* | Rest]) -> scan_c_comment(Rest).

%% Ignore up to next CR or LF.  If the line ends in CRLF,
%% the LF will be treated as separate whitespace, which is
%% okay since it will also be ignored.

scan_cpp_comment([]) -> {more, "//"};
scan_cpp_comment(eof) -> {done, eof, []};
scan_cpp_comment([$\r | Rest]) -> {done, [], Rest};
scan_cpp_comment([$\n | Rest]) -> {done, [], Rest};
scan_cpp_comment([_ | Rest]) -> scan_cpp_comment(Rest).

scan_c_comment([]) -> {more, "/*"};
scan_c_comment(eof) -> {done, eof, []};
scan_c_comment([$*]) -> {more, "/**"};
scan_c_comment([$*, $/ | Rest]) -> {done, [], Rest};
scan_c_comment([_ | Rest]) -> scan_c_comment(Rest).

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

%% Return the first JSON value decoded from the input string.
%% The string must contain at least one complete JSON value.

decode_string(CharList) ->
    {done, V, _} = decode([], CharList ++ eof),
    V.

%% Attempt to decode a JSON value from the input string
%% and continuation, using empty list for the initial continuation.
%% Return {done, Result, LeftoverChars} if a value is recognized,
%% or {more, Continuation} if more input characters are needed.
%% The Result can be {ok, Value}, eof, or {error, Reason}.
%% The Continuation is then fed as an argument to decode/2 when
%% more input is available.
%% Use the atom 'eof' instead of a char list to signal
%% a true end to the input, and may flush a final number.

decode([], CharList) ->
    decode(first_continuation(), CharList);

decode(Continuation, CharList) ->
    {OldChars, Kt} = Continuation,
    get_token(OldChars ++ CharList, Kt).

first_continuation() ->
    {[], fun
        (eof, Cs) ->
                {done, eof, Cs};
        (T, Cs) ->
            parse_value(T, Cs, fun(V, C2) ->
                {done, {ok, V}, C2}
            end)
    end}.

%% Continuation Kt must accept (TokenOrEof, Chars)

get_token(Chars, Kt) ->
    case token(Chars) of
        {done, {ok, T}, Rest} -> Kt(T, Rest);
        {done, eof, Rest} -> Kt(eof, Rest);
        {done, {error, Reason}, Rest} -> {done, {error, Reason}, Rest};
        {more, X} -> {more, {X, Kt}}
    end.

%% Continuation Kv must accept (Value, Chars)

parse_value(eof, C, _Kv) -> {done, {error, premature_eof}, C};
parse_value(true, C, Kv) -> Kv(true, C);
parse_value(false, C, Kv) -> Kv(false, C);
parse_value(null, C, Kv) -> Kv(null, C);
parse_value(S, C, Kv) when is_list(S) -> Kv(S, C);
parse_value(N, C, Kv) when is_number(N) -> Kv(N, C);
parse_value(lcbrace, C, Kv) -> parse_object(C, Kv);
parse_value(lsbrace, C, Kv) -> parse_array(C, Kv);
parse_value(_, C, _Kv) -> {done, {error, syntax_error}, C}.

%% Continuation Kv must accept (Value, Chars)

parse_object(Chars, Kv) ->
    get_token(Chars, fun(T, C2) ->
        Obj = obj_new(),
        case T of
            rcbrace -> Kv(Obj, C2);             % empty object
            _ -> parse_object(Obj, T, C2, Kv)   % token must be string
        end
    end).

parse_object(_Obj, eof, C, _Kv) ->
    {done, {error, premature_eof}, C};

parse_object(Obj, S, C, Kv) when is_list(S) ->    % S is member name
    get_token(C, fun
        (colon, C2) ->
            parse_object2(Obj, S, C2, Kv);
        (T, C2) ->
            {done, {error, {expecting_colon, T}}, C2}
    end);

parse_object(_Obj, M, C, _Kv) ->
    {done, {error, {member_name_not_string, M}}, C}.

parse_object2(Obj, S, C, Kv) ->
    get_token(C, fun
        (eof, C2) ->
            {done, {error, premature_eof}, C2};
        (T, C2) ->
            parse_value(T, C2, fun(V, C3) ->    % V is member value
                Obj2 = obj_store(S, V, Obj),
                get_token(C3, fun
                    (rcbrace, C4) ->    % "}" end of object
                                                {struct, PropList1} = Obj2,
                        Kv({struct, lists:reverse(PropList1)}, C4);
                    (comma, C4) ->              % "," another member follows
                        get_token(C4, fun(T3, C5) ->
                            parse_object(Obj2, T3, C5, Kv)
                        end);
                    (eof, C4) ->
                        {done, {error, premature_eof}, C4};
                    (T2, C4) ->
                        {done, {error, {expecting_comma_or_curly, T2}}, C4}
                end)
            end)
    end).

%% Continuation Kv must accept (Value, Chars)

parse_array(C, Kv) ->
    get_token(C, fun
        (eof, C2) -> {done, {error, premature_eof}, C2};
        (rsbrace, C2) -> Kv({array, []}, C2);  % empty array
        (T, C2) -> parse_array([], T, C2, Kv)
    end).

parse_array(E, T, C, Kv) ->
    parse_value(T, C, fun(V, C2) ->
        E2 = [V | E],
        get_token(C2, fun
            (rsbrace, C3) ->        % "]" end of array
                Kv({array, lists:reverse(E2)}, C3);

            (comma, C3) ->          % "," another value follows
                get_token(C3, fun(T3, C4) ->
                    parse_array(E2, T3, C4, Kv)
                end);
            (eof, C3) ->
                {done, {error, premature_eof}, C3};
            (T2, C3) ->
                {done, {error, {expecting_comma_or_close_array, T2}}, C3}
        end)
    end).

%%% OBJECTS
%%%
%%% We'll use tagged property lists as the internal representation
%%% of JSON objects.  Unordered lists perform worse than trees for
%%% lookup and modification of members, but we expect objects to be
%%% have only a few members.  Lists also print better.

%% Is this a proper JSON object representation?

is_obj({struct, Props}) when is_list(Props) ->
    lists:all(fun
        ({Member, _Value}) when is_atom(Member); is_list(Member) -> true;
        (_) -> false
    end, Props);

is_obj(_) ->
    false.

%% Create a new, empty object.

obj_new() ->
    {struct, []}.

%% Fetch an object member's value, expecting it to be in the object.
%% Return value, runtime error if no member found with that name.

obj_fetch(Key, {struct, Props}) when is_list(Props) ->
    case proplists:get_value(Key, Props) of
        undefined ->
            exit({struct_no_key, Key});
        Value ->
            Value
    end.

%% Fetch an object member's value, or indicate that there is no such member.
%% Return {ok, Value} or 'error'.

obj_find(Key, {struct, Props}) when is_list(Props) ->
    case proplists:get_value(Key, Props) of
        undefined ->
            error;
        Value ->
            {ok, Value}
    end.

obj_is_key(Key, {struct, Props}) ->
    proplists:is_defined(Key, Props).

%% Store a new member in an object.  Returns a new object.

obj_store(KeyList, Value, {struct, Props}) when is_list(Props) ->
        Key = try list_to_atom(KeyList)
              catch error:badarg -> KeyList
              end,
    {struct, [{Key, Value} | proplists:delete(Key, Props)]}.

%% Create an object from a list of Key/Value pairs.

obj_from_list(Props) ->
    Obj = {struct, Props},
    case is_obj(Obj) of
        true -> Obj;
        false -> exit(json_bad_object)
    end.

%% Fold Fun across object, with initial accumulator Acc.
%% Fun should take (Value, Acc) as arguments and return Acc.

obj_fold(Fun, Acc, {struct, Props}) ->
    lists:foldl(Fun, Acc, Props).

is_string([]) -> yes;
is_string(List) -> is_string(List, non_unicode).

is_string([C|Rest], non_unicode) when is_integer(C), C >= 0, C =< 255 ->
    is_string(Rest, non_unicode);
is_string([C|Rest], _) when is_integer(C), C>= 0, C =< 65000 ->
    is_string(Rest, unicode);
is_string([], non_unicode) -> yes;
is_string([], unicode) -> unicode;
is_string(_, _) -> no.


%%% TESTING
%%%
%%% We can't expect to round-trip from JSON -> Erlang -> JSON,
%%% due to the degrees of freedom in the JSON syntax: whitespace,
%%% and ordering of object members.  We can, however, expect to
%%% round-trip from Erlang -> JSON -> Erlang, so the JSON parsing
%%% tests will in fact test the Erlang equivalence of the
%%% JSON -> Erlang -> JSON -> Erlang coding chain.

%% Test driver.  Return 'ok' or {failed, Failures}.

test() ->
    E2Js = e2j_test_vec(),
    Failures = lists:foldl(fun({E, J}, Fs) ->
        case (catch test_e2j(E, J)) of
            ok ->
                case (catch round_trip(E)) of
                    ok ->
                        case (catch round_trip_one_char(E)) of
                            ok -> Fs;
                            Reason -> [{round_trip_one_char, E, Reason} | Fs]
                        end;
                    Reason ->
                        [{round_trip, E, Reason} | Fs]
                end;
            Reason ->
                [{erlang_to_json, E, J, Reason} | Fs]
        end;
    (end_of_tests, Fs) -> Fs end, [], E2Js),
    case Failures of
        [] -> ok;
        _ -> {failed, Failures}
    end.

%% Test for conversion from Erlang to JSON.  Note that unequal strings
%% may represent equal JSON data, due to discretionary whitespace,
%% object member order, trailing zeroes in floating point, etc.
%% Legitimate changes to the encoding routines may require tweaks to
%% the reference JSON strings in e2j_test_vec().

test_e2j(E, J) ->
    J2 = lists:flatten(encode(E)),
    J = J2,                                     % raises error if unequal
    ok.

%% Test that Erlang -> JSON -> Erlang round-trip yields equivalent term.

round_trip(E) ->
    J2 = lists:flatten(encode(E)),
    {ok, E2} = decode_string(J2),
    true = equiv(E, E2),                        % raises error if false
    ok.

%% Round-trip with one character at a time to test all continuations.

round_trip_one_char(E) ->
    J = lists:flatten(encode(E)),
    {done, {ok, E2}, _} = lists:foldl(fun(C, Ret) ->
        case Ret of
            {done, _, _} -> Ret;
            {more, Cont} -> decode(Cont, [C])
        end
    end, {more, first_continuation()}, J ++ [eof]),
    true = equiv(E, E2),                        % raises error if false
    ok.

%% Test for equivalence of Erlang terms.
%% Due to arbitrary order of construction, equivalent objects might
%% compare unequal as erlang terms, so we need to carefully recurse
%% through aggregates (tuples and objects).

equiv({struct, Props1}, {struct, Props2}) ->
    equiv_object(Props1, Props2);
equiv(T1, T2) when is_tuple(T1), is_tuple(T2) ->
    equiv_tuple(T1, T2);
equiv(N1, N2) when is_number(N1), is_number(N2) -> N1 == N2;
equiv(S1, S2) when is_list(S1), is_list(S2)     -> S1 == S2;
equiv(true, true) -> true;
equiv(false, false) -> true;
equiv(null, null) -> true.

%% Object representation and traversal order is unknown.
%% Use the sledgehammer and sort property lists.

equiv_object(Props1, Props2) ->
    L1 = lists:keysort(1, Props1),
    L2 = lists:keysort(1, Props2),
    Pairs = lists:zip(L1, L2),
    true = lists:all(fun({{K1, V1}, {K2, V2}}) ->
        equiv(K1, K2) and equiv(V1, V2)
    end, Pairs).

%% Recursively compare tuple elements for equivalence.

equiv_tuple({}, {}) ->
    true;
equiv_tuple(T1, T2) when size(T1) == size(T2) ->
    S = size(T1),
    lists:all(fun(I) ->
        equiv(element(I, T1), element(I, T2))
    end, lists:seq(1, S)).

e2j_test_vec() -> [
    {1, "1"},
    {3.1416, "3.14160"}, % text representation may truncate, trail zeroes
    {-1, "-1"},
    {-3.1416, "-3.14160"},
    {12.0e10, "1.20000e+11"},
    {1.234E+10, "1.23400e+10"},
    {-1.234E-10, "-1.23400e-10"},
    {"foo", "\"foo\""},
    {"foo" ++ [500] ++ "bar", [$", $f, $o, $o, 500, $b, $a, $r, $"]},
    {"foo" ++ [5] ++ "bar", "\"foo\\u0005bar\""},
    {"", "\"\""},
    {[], "\"\""},
    {"\n\n\n", "\"\\n\\n\\n\""},
    {obj_new(), "{}"},
    {obj_from_list([{"foo", "bar"}]), "{\"foo\":\"bar\"}"},
    {obj_from_list([{"foo", "bar"}, {"baz", 123}]),
     "{\"foo\":\"bar\",\"baz\":123}"},
    {{}, "[]"},
    {{{}}, "[[]]"},
    {{1, "foo"}, "[1,\"foo\"]"},

    % json array in a json object
    {obj_from_list([{"foo", {123}}]),
     "{\"foo\":[123]}"},

    % json object in a json object
    {obj_from_list([{"foo", obj_from_list([{"bar", true}])}]),
     "{\"foo\":{\"bar\":true}}"},

    % fold evaluation order
    {obj_from_list([{"foo", {}},
                     {"bar", obj_from_list([{"baz", true}])},
                     {"alice", "bob"}]),
     "{\"foo\":[],\"bar\":{\"baz\":true},\"alice\":\"bob\"}"},

    % json object in a json array
    {{-123, "foo", obj_from_list([{"bar", {}}]), null},
     "[-123,\"foo\",{\"bar\":[]},null]"},

    end_of_tests
].

%%% TODO:
%%%
%%% Measure the overhead of the CPS-based parser by writing a conventional
%%% scanner-parser that expects all input to be available.
%%%
%%% JSON has dropped comments - disable their parsing.
%%%
%%% Allow a compile-time option to decode object member names as atoms,
%%% to reduce the internal representation overheads when communicating
%%% with trusted peers.
