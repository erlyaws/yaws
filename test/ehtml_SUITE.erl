-module(ehtml_SUITE).

-include("testsuite.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [
     void_element,
     non_void_element,
     attributes,
     simple_fun,
     mfa,
     nested_fun,
     nested_mfa,
     simple_attr,
     mfa_attr,
     nested_attr,
     nested_mfa_attr
    ].

groups() ->
[
].

%%====================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Test, Config) ->
    Config.

end_per_testcase(_Test, _Config) ->
    ok.

%%====================================================================
void_element(_Config) ->
    %% No end tag (</tag>) for void elements in HTML5.
    {ehtml, E1} = {ehtml, [{img, [{src, "foo.png"}, {alt, "foo"}]}]},
    Img = "<img src=\"foo.png\" alt=\"foo\" />",
    ?assertEqual(Img, lists:flatten(yaws_api:ehtml_expand(E1))),
    {ehtml, E2} = {ehtml, [{br}]},
    Br = "<br />",
    ?assertEqual(Br,lists:flatten(yaws_api:ehtml_expand(E2))),
    ok.

non_void_element(_Config) ->
    %% No self-closing syntax (/>) for non-void elements in HTML5.
    {ehtml, E} = {ehtml, [{p}]},
    P = "<p></p>",
    ?assertEqual(P, lists:flatten(yaws_api:ehtml_expand(E))),
    ok.

attributes(_Config) ->
    {ehtml, E1} = {ehtml, [{img, [{check, src, <<"quote\".png">>},
                                  {check, width, 10},
                                  {height, 20},
                                  {check, alt, "quote\""}]}]},
    Img = <<"<img src='quote\".png' width=\"10\" height=\"20\" alt='quote\"' />">>,
    ?assertEqual(Img, iolist_to_binary(yaws_api:ehtml_expand(E1))),
    {ehtml, E2} = {ehtml, [{a, [{href, <<"test">>}], <<"test link">>}]},
    A = <<"<a href=\"test\">test link</a>">>,
    ?assertEqual(A, iolist_to_binary(yaws_api:ehtml_expand(E2))),
    ok.

get_title() ->
    "Funtest Title".

simple_fun(_Config) ->
    {ehtml, E} = {ehtml, [{title, [], fun get_title/0}]},
    Title = "\n<title>" ++ get_title() ++ "</title>",
    ?assertEqual(Title, lists:flatten(yaws_api:ehtml_expand(E))),
    ok.

mfa_fun(Args) ->
    Args.

mfa(_Config) ->
    Args = ["another ", "string"],
    {ehtml, E} = {ehtml, [{p, [], {?MODULE, mfa_fun, Args}}]},
    P = lists:flatten(["\n<p>", Args, "</p>"]),
    ?assertEqual(P, lists:flatten(yaws_api:ehtml_expand(E))),
    ok.

nested_fun(_Config) ->
    Value = "paragraph",
    {ehtml, E} = {ehtml, [fun() -> [{p, [], Value}] end]},
    P = lists:flatten(["\n<p>", Value, "</p>"]),
    ?assertEqual(P, lists:flatten(yaws_api:ehtml_expand(E))),
    ok.

nested_mfa_fun(Args) ->
    fun() -> [{p, [], fun() -> Args end}] end.

nested_mfa(_Config) ->
    Args = ["another ", "string"],
    {ehtml, E} = {ehtml, [{?MODULE, nested_mfa_fun, Args}]},
    P = lists:flatten(["\n<p>", Args, "</p>"]),
    ?assertEqual(P, lists:flatten(yaws_api:ehtml_expand(E))),
    ok.

get_link() ->
    "http://www.example.org/".

simple_attr(_Config) ->
    Link = "link",
    {ehtml, E} = {ehtml, [{a, [{href, fun get_link/0}], Link}]},
    A = lists:flatten(["<a href=\"", get_link(), "\">", Link, "</a>"]),
    ?assertEqual(A, lists:flatten(yaws_api:ehtml_expand(E))),
    ok.

mfa_attr(_Config) ->
    Args = [get_link()],
    Link = "link",
    {ehtml, E} = {ehtml, [{a, [{href, {?MODULE, mfa_fun, Args}}], Link}]},
    A = lists:flatten(["<a href=\"", Args, "\">", Link, "</a>"]),
    ?assertEqual(A, lists:flatten(yaws_api:ehtml_expand(E))),
    ok.

nested_attr(_Config) ->
    Link = "link",
    {ehtml, E} = {ehtml, [{a, [{href, fun() -> fun get_link/0 end}], Link}]},
    A = lists:flatten(["<a href=\"", get_link(), "\">", Link, "</a>"]),
    ?assertEqual(A, lists:flatten(yaws_api:ehtml_expand(E))),
    ok.

nested_mfa_attr_fun(Args) ->
    {?MODULE, mfa_fun, Args}.

nested_mfa_attr(_Config) ->
    Args = [get_link()],
    Link = "link",
    {ehtml, E} = {ehtml,
                  [{a, [{href, {?MODULE, nested_mfa_attr_fun, Args}}], Link}]},
    A = lists:flatten(["<a href=\"", Args, "\">", Link, "</a>"]),
    ?assertEqual(A, lists:flatten(yaws_api:ehtml_expand(E))),
    ok.
