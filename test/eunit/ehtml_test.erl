-module(ehtml_test).

-include_lib("eunit/include/eunit.hrl").

-include("tftest.hrl").

-export([mfa_fun/1, nested_mfa_fun/1, nested_mfa_attr_fun/1]).

void_element_test() ->
    %% No end tag (</tag>) for void elements in HTML5.
    {ehtml, E1} = {ehtml, [{img, [{src, "foo.png"}, {alt, "foo"}]}]},
    Img = "<img src=\"foo.png\" alt=\"foo\" />",
    Img = lists:flatten(yaws_api:ehtml_expand(E1)),
    {ehtml, E2} = {ehtml, [{br}]},
    Br = "<br />",
    Br = lists:flatten(yaws_api:ehtml_expand(E2)).

non_void_element_test() ->
    %% No self-closing syntax (/>) for non-void elements in HTML5.
    {ehtml, E} = {ehtml, [{p}]},
    P = "<p></p>",
    P = lists:flatten(yaws_api:ehtml_expand(E)).

attributes_test() ->
    {ehtml, E} = {ehtml, [{img, [{check, src, "quote\".png"},
                                 {check, width, 10},
                                 {height, 20},
                                 {check, alt, "quote\""}]}]},
    Img = "<img src='quote\".png' width=\"10\" height=\"20\" alt='quote\"' />",
    Img = lists:flatten(yaws_api:ehtml_expand(E)).

get_title() ->
    "Funtest Title".

simple_fun_test() ->
    {ehtml, E} = {ehtml, [{title, [], fun get_title/0}]},
    Title = "\n<title>" ++ get_title() ++ "</title>",
    Title = lists:flatten(yaws_api:ehtml_expand(E)).

mfa_fun(Args) ->
    Args.

mfa_test() ->
    Args = ["another ", "string"],
    {ehtml, E} = {ehtml, [{p, [], {?MODULE, mfa_fun, Args}}]},
    P = lists:flatten(["\n<p>", Args, "</p>"]),
    P = lists:flatten(yaws_api:ehtml_expand(E)).

nested_fun_test() ->
    Value = "paragraph",
    {ehtml, E} = {ehtml, [fun() -> [{p, [], Value}] end]},
    P = lists:flatten(["\n<p>", Value, "</p>"]),
    P = lists:flatten(yaws_api:ehtml_expand(E)).

nested_mfa_fun(Args) ->
    fun() -> [{p, [], fun() -> Args end}] end.

nested_mfa_test() ->
    Args = ["another ", "string"],
    {ehtml, E} = {ehtml, [{?MODULE, nested_mfa_fun, Args}]},
    P = lists:flatten(["\n<p>", Args, "</p>"]),
    P = lists:flatten(yaws_api:ehtml_expand(E)).

get_link() ->
    "http://yaws.hyber.org/".

simple_attr_test() ->
    Link = "link",
    {ehtml, E} = {ehtml, [{a, [{href, fun get_link/0}], Link}]},
    A = lists:flatten(["<a href=\"", get_link(), "\">", Link, "</a>"]),
    A = lists:flatten(yaws_api:ehtml_expand(E)).

mfa_attr_test() ->
    Args = [get_link()],
    Link = "link",
    {ehtml, E} = {ehtml, [{a, [{href, {?MODULE, mfa_fun, Args}}], Link}]},
    A = lists:flatten(["<a href=\"", Args, "\">", Link, "</a>"]),
    A = lists:flatten(yaws_api:ehtml_expand(E)).

nested_attr_test() ->
    Link = "link",
    {ehtml, E} = {ehtml, [{a, [{href, fun() -> fun get_link/0 end}], Link}]},
    A = lists:flatten(["<a href=\"", get_link(), "\">", Link, "</a>"]),
    A = lists:flatten(yaws_api:ehtml_expand(E)).

nested_mfa_attr_fun(Args) ->
    {?MODULE, mfa_fun, Args}.

nested_mfa_attr_test() ->
    Args = [get_link()],
    Link = "link",
    {ehtml, E} = {ehtml,
                  [{a, [{href, {?MODULE, nested_mfa_attr_fun, Args}}], Link}]},
    A = lists:flatten(["<a href=\"", Args, "\">", Link, "</a>"]),
    A = lists:flatten(yaws_api:ehtml_expand(E)).
