%% -*- coding: latin-1 -*-
%%%----------------------------------------------------------------------
%%% File    : exhtml.erl
%%% Author  : Joakim Grebenö <jocke@tail-f.com>
%%% Purpose : Format ehtml as xhtml code with optional indentation support.
%%% Created : 24 Apr 2006 by Joakim Grebenö <jocke@tail-f.com>
%%%----------------------------------------------------------------------

-module(yaws_exhtml).
-export([check_xhtml/1]).
-export([fformat/0, sformat/0]). % test
-export([format/1, format/2, format/3]).
-export([sformat/1, sformat/2]).
-export([count_trailing_spaces/1]).

-define(INDENT_LEVEL, 2).

fformat() -> % test
    HTML =
        format(
          [{table,
            [{tr,
              [{td,
                ["foo ", {em, ["bar"]}, " now.",
                 {hr},
                 {p, "foo"}]}]},
             {tr,
              [{td,
                ["foo ", {em, ["bar"]}, " now."]}]}]}]),
    io:format("~s~n", [lists:flatten(HTML)]).

sformat() -> % test
    HTML =
        sformat(
          [{table,
            [{tr,
              [{td,
                ["foo ", {em, ["bar"]}, " now.",
                 {hr},
                 {p, "foo"}]}]},
             {tr,
              [{td,
                ["foo ", {em, ["bar"]}, " now."]}]}]}]),
    io:format("~s~n", [lists:flatten(HTML)]).

check_xhtml(XHTMLContent) when is_list(XHTMLContent) ->
    check_xhtml(list_to_binary(XHTMLContent));
check_xhtml(XHTMLContent) when is_binary(XHTMLContent) ->
    {ok, Filename} = yaws:mktemp("confd"),
    ok = file:write_file(Filename, XHTMLContent),
    DTD = filename:join(code:priv_dir(webgui), "xhtml1-strict.dtd"),
    Cmd = "xmllint --dtdvalid "++DTD++" -noout -nonet "++Filename++" 2>&1",
    case os:cmd(Cmd) of
        "" ->
            file:delete(Filename),
            ok;
        Reason ->
            file:delete(Filename),
            {error, Reason}
    end.

format(Data) ->
    tl(format(block, Data, fun value2string/1, 0, [])).

format(Data, N) ->
    tl(format(block, Data,  fun value2string/1, N, lists:duplicate(N, $ ))).

format(Data, N, Value2StringF) ->
    tl(format(block, Data, Value2StringF, N, lists:duplicate(N, $ ))).

format(Mode, Data, Value2StringF, N, Indent) when is_tuple(Data) ->
    format(Mode, [Data], Value2StringF, N, Indent);
format(_Mode, [], _Value2StringF, _N, _Indent) -> [];
format(Mode, [{'$html', HTML}|Rest], Value2StringF, N, Indent) ->
    [HTML|format(Mode, Rest, Value2StringF, N, Indent)];
format(Mode, [{Tag}|Rest], Value2StringF, N, Indent) ->
    format(Mode, [{Tag, [], []}|Rest], Value2StringF, N, Indent);
format(Mode, [{Tag, Attrs}|Rest], Value2StringF, N, Indent) ->
    format(Mode, [{Tag, Attrs, []}|Rest], Value2StringF, N, Indent);
format(Mode, [{Tag, Attrs, Body}|Rest], Value2StringF, N, Indent) ->
    TagString = lowercase(tag_string(Tag)),
    case {Mode, block_level(TagString), Body} of
        {first_in_block, no, []} ->
            [$\n, Indent, $<, TagString,
             format_attrs(Value2StringF, Attrs), $>, $<, $/,
             TagString, $>|format(Mode, Rest, Value2StringF, N, Indent)];
        {block, no, []} ->
            [$<, TagString, format_attrs(Value2StringF, Attrs), $>, $<, $/,
             TagString, $>|format(Mode, Rest, Value2StringF, N, Indent)];
        {first_in_block, yes, _} ->
            NextLevel = lists:duplicate(?INDENT_LEVEL, $ ),
            [$\n, Indent, $<, TagString,
             format_attrs(Value2StringF, Attrs), $>,
             format(first_in_block, Body, Value2StringF, N+?INDENT_LEVEL,
                    [NextLevel, Indent]),
             $\n, Indent, $<, $/, TagString, $>|
             format(block, Rest, Value2StringF, N, Indent)];
        %% Block element in a block element.
        {block, yes, _} ->
            NextLevel = lists:duplicate(?INDENT_LEVEL, $ ),
            [$\n, Indent, $<, TagString,
             format_attrs(Value2StringF, Attrs), $>,
             format(first_in_block, Body, Value2StringF, N+?INDENT_LEVEL,
                    [NextLevel, Indent]),
             $\n, Indent, $<, $/, TagString, $>|
             format(block, Rest, Value2StringF, N, Indent)];
        %% Inline element first in a block element.
        {first_in_block, no, _} ->
            [$\n, Indent, $<, TagString, format_attrs(Value2StringF, Attrs),
             $>,
             format(inline, Body, Value2StringF, N, Indent),
             $<, $/, TagString, $>|
             format(block, Rest, Value2StringF, N, Indent)];
        %% Inline element in a block or an inline element.
        {_, no, _} ->
            [$<, TagString, format_attrs(Value2StringF, Attrs), $>,
             format(inline, Body, Value2StringF, N, Indent),
             $<, $/, TagString, $>|
             format(Mode, Rest, Value2StringF, N, Indent)]
    end;
%% Inline data first in a block element.
format(first_in_block, [String|Rest], Value2StringF, N, Indent)
  when is_list(String) ->
    [$\n, Indent, String|format(block, Rest, Value2StringF, N, Indent)];
%% Inline data in a block/inline element.
format(Mode, [String|Rest], Value2StringF, N, Indent) when is_list(String) ->
    [String|format(Mode, Rest, Value2StringF, N, Indent)];
%% PCDATA in first a block element.
format(first_in_block, Value, Value2StringF, _N, Indent) ->
    [$\n, Indent, Value2StringF(Value)];
%% PCDATA in a block element.
format(block, Value, Value2StringF, _N, _Indent) ->
    [Value2StringF(Value)];
%% PCDATA in an inline element.
format(inline, Value, Value2StringF, _N, _Indent) ->
    Value2StringF(Value).

tag_string(TagAtom) when is_atom(TagAtom) -> atom_to_list(TagAtom);
tag_string(TagString) -> TagString.

lowercase(String) -> lowercase(String, []).

lowercase([C|Cs], Acc) when C >= $A, C =< $Z ->
    lowercase(Cs, [C+($a-$A)| Acc]);
lowercase([C|Cs], Acc) -> lowercase(Cs, [C| Acc]);
lowercase([], Acc) -> lists:reverse(Acc).

%% The following are defined as block-level elements:
block_level("address") -> yes;
block_level("blockquote") -> yes;
block_level("center") -> yes;
block_level("dir") -> yes;
block_level("div") -> yes;
block_level("dl") -> yes;
block_level("fieldset") -> yes;
block_level("form") -> yes;
block_level("h1") -> yes;
block_level("h2") -> yes;
block_level("h3") -> yes;
block_level("h4") -> yes;
block_level("h5") -> yes;
block_level("h6") -> yes;
block_level("hr") -> yes;
block_level("input") -> yes;
block_level("isindex") -> yes;
block_level("menu") -> yes;
block_level("noframes") -> yes;
block_level("noscript") -> yes;
block_level("ol") -> yes;
block_level("p") -> yes;
block_level("pre") -> yes;
block_level("table") -> yes;
block_level("textarea") -> no;
block_level("tbody") -> yes;
block_level("ul") -> yes;
block_level("select") -> yes;
%% The following elements may also be considered block-level elements since
%% they may contain block-level elements:
block_level("dd") -> yes;
block_level("dt") -> yes;
block_level("frameset") -> yes;
block_level("li") -> yes;
block_level("td") -> yes;
block_level("tfoot") -> yes;
block_level("th") -> yes;
block_level("thead") -> yes;
block_level("tr") -> yes;
%% The following elements may be used as either block-level elements or
%% inline elements. If used as inline elements (e.g., within another inline
%% element or a P), these elements should not contain any block-level
%% elements.
block_level("applet") -> yes;
block_level("button") -> yes;
block_level("del") -> yes;
block_level("iframe") -> yes;
block_level("ins") -> yes;
block_level("map") -> yes;
block_level("object") -> yes;
block_level("script") -> yes;
%% All else are defined as inline elements:
block_level(_) -> no.

format_attrs(_Value2StringF, []) -> [];
format_attrs(Value2StringF, [{Name, Value}|Rest]) ->
    [$ , lowercase(tag_string(Name)), $=, $\", Value2StringF(Value), $\"|
     format_attrs(Value2StringF, Rest)].

value2string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
value2string(Integer) when is_integer(Integer) -> integer_to_list(Integer);
value2string(Float) when is_float(Float) -> float_to_list(Float);
value2string(Binary) when is_binary(Binary) -> Binary;
value2string(String) when is_list(String) -> String.

sformat(Data) -> sformat(Data, fun value2string/1).

sformat(Data, Value2StringF) when is_tuple(Data) ->
    sformat([Data], Value2StringF);
sformat([], _Value2StringF) -> [];
sformat([{Tag}|Rest], Value2StringF) ->
    sformat([{Tag, [], []}|Rest], Value2StringF);
sformat([{Tag, Body}|Rest], Value2StringF) ->
    sformat([{Tag, [], Body}|Rest], Value2StringF);
sformat([{Tag, Attrs, []}|Rest], Value2StringF) ->
    TagString = lowercase(tag_string(Tag)),
    [$<, TagString, format_attrs(Value2StringF, Attrs), $/, $>|
     sformat(Rest, Value2StringF)];
sformat([{Tag, Attrs, Body}|Rest], Value2StringF) ->
    TagString = lowercase(tag_string(Tag)),
    [$<, TagString, format_attrs(Value2StringF, Attrs), $>,
     sformat(Body, Value2StringF),
     $<, $/, TagString, $>|
     sformat(Rest, Value2StringF)];
sformat([String|Rest], Value2StringF) when is_list(String) ->
    [String|sformat(Rest, Value2StringF)];
sformat(Value, Value2StringF) ->
    Value2StringF(Value).

-define(SZ, 16).

count_trailing_spaces(<<>>) ->
    0;
count_trailing_spaces(Bin) ->
    count_trailing_spaces(Bin, size(Bin), 0).

count_trailing_spaces(Bin, Stop, N) ->
    Start = if Stop =< ?SZ ->
                    1;
               true ->
                    Stop - ?SZ + 1
            end,
    L = binary_to_list(Bin, Start, Stop),
    case spaces_in_list(L) of
        ?SZ when Start == 1 ->
            N + ?SZ;
        ?SZ ->
            %% keep going
            count_trailing_spaces(Bin, Start-1, N + ?SZ);
        M ->
            N + M
    end.

spaces_in_list(L) ->
    spaces_in_list(lists:reverse(L), 0).

spaces_in_list([$\s | T], N) ->
    spaces_in_list(T, N+1);
spaces_in_list(_, N) ->
    N.
