-module(cookies).
-compile(export_all).
-include("../../include/yaws_api.hrl").
-include_lib("eunit/include/eunit.hrl").

parse_set_cookies_test() ->
    %% Set-Cookie: value
    ?assertEqual(
       #setcookie{key="value"},
       yaws_api:parse_set_cookie("value")
      ),

    %% Set-Cookie: name=value
    ?assertEqual(
       #setcookie{key="name", value="value"},
       yaws_api:parse_set_cookie("name=value")
      ),

    %% Set-Cookie: name="value [quoted]"
    ?assertEqual(
       #setcookie{key="name", value="value [quoted]", quoted=true},
       yaws_api:parse_set_cookie("name=\"value [quoted]\"")
      ),

    %% Set-Cookie: name=value; path=/
    ?assertEqual(
       #setcookie{key="name", value="value", path="/"},
       yaws_api:parse_set_cookie("name=value; Path=/")
      ),

    %% Set-Cookie: name=value; domain=test.com
    ?assertEqual(
       #setcookie{key="name", value="value", domain="test.com"},
       yaws_api:parse_set_cookie("name=value; Domain=test.com")
      ),

    %% Set-Cookie: name=value; max-age=86400
    ?assertEqual(
       #setcookie{key="name", value="value", max_age="86400"},
       yaws_api:parse_set_cookie("name=value; Max-Age=86400")
      ),

    %% Set-Cookie: name=value; expires="Sat, 02 May 2009 23:38:25 GMT"
    ?assertEqual(
       #setcookie{key="name", value="value", expires="Sat, 02 May 2009 23:38:25 GMT"},
       yaws_api:parse_set_cookie("name=value; Expires=Sat, 02 May 2009 23:38:25 GMT")
      ),

    %% Set-Cookie: name=value; secure
    ?assertEqual(
       #setcookie{key="name", value="value", secure=true},
       yaws_api:parse_set_cookie("name=value; Secure")
      ),

    %% Set-Cookie: name=value; HttpOnly
    ?assertEqual(
       #setcookie{key="name", value="value", http_only=true},
       yaws_api:parse_set_cookie("name=value; HttpOnly")
      ),



    %% Set-Cookie: name=value; comment="This is a comment"
    ?assertEqual(
       #setcookie{key="name", value="value",
                  extensions=[{"comment", "This is a comment", true}]},
       yaws_api:parse_set_cookie("name=value; Comment=\"This is a comment\"")
      ),

    %% Set-Cookie: name=value; discard
    ?assertEqual(
       #setcookie{key="name", value="value",
                  extensions=[{"discard", undefined, false}]},
       yaws_api:parse_set_cookie("name=value; discard")
      ),

    %% Set-Cookie: name=value; ext1=val1; ext2="val2"; ext3
    ?assertEqual(
       #setcookie{key="name", value="value",
                  extensions=[{"ext1", "val1", false}, {"ext2", "val2", true},
                              {"ext3", undefined, false}]},
       yaws_api:parse_set_cookie("name=value; ext1=val1; ext2=\"val2\"; ext3")
      ),

    %% Set-Cookie: value1, value2, name=value
    ?assertEqual(
       [#setcookie{key="value1"}, #setcookie{key="value2"},
        #setcookie{key="name", value="value"}],
       yaws_api:parse_set_cookie("value1 , value2, name=value")
      ),
    ok.

parse_cookies_test() ->
    %% Cookie: value
    ?assertEqual(
       [#cookie{key="value"}],
       yaws_api:parse_cookie("value")
      ),

    %% Cookie: name=value
    ?assertEqual(
       [#cookie{key="name", value="value"}],
       yaws_api:parse_cookie("name=value")
      ),

    %% Cookie: name="value [quoted]"
    ?assertEqual(
       [#cookie{key="name", value="value [quoted]", quoted=true}],
       yaws_api:parse_cookie("name=\"value [quoted]\"")
      ),

    %% Cookie: name=value; $path=/
    ?assertEqual(
       [#cookie{key="name", value="value"},
        #cookie{key="$path", value="/"}],
       yaws_api:parse_cookie("name=value; $path=/")
      ),

    %% Cookie: name=value; $port=443
    ?assertEqual(
       [#cookie{key="name", value="value"},
        #cookie{key="$port", value="443"}],
       yaws_api:parse_cookie("name=value; $port=443")
      ),

    %% Cookie: name=value; $domain=test.com
    ?assertEqual(
       [#cookie{key="name", value="value"},
        #cookie{key="$domain", value="test.com"}],
       yaws_api:parse_cookie("name=value; $domain=test.com")
        ),

    %% Cookie: $version=1, value1, value2; name=value
    ?assertEqual(
       [#cookie{key="$version", value="1"}, #cookie{key="value1"},
        #cookie{key="value2"}, #cookie{key="name", value="value"}],
       yaws_api:parse_cookie("$version=1, value1, value2; name=value")
      ),
    ok.

parse_invalid_set_cookies_test() ->
    ?assertEqual([], yaws_api:parse_set_cookie("name=value [quoted]")),
    ?assertEqual([], yaws_api:parse_set_cookie("name1=value1 name2=value2")),
    ?assertEqual([], yaws_api:parse_set_cookie("name1=value1 domain=test.com path=/")),
    ?assertEqual([], yaws_api:parse_set_cookie("name=value; secure=1")),
    ok.

parse_invalid_cookies_test() ->
    ?assertEqual([], yaws_api:parse_cookie("name=value [quoted]")),
    ?assertEqual([], yaws_api:parse_cookie("name1=value1 name2=value2")),
    ok.

format_set_cookies_test() ->

    %% Set-Cookie: value
    ?assertEqual(
       "value",
       lists:flatten(
         yaws_api:format_set_cookie(
           #setcookie{key="value"}
          )
        )
      ),

    %% Set-Cookie: name=value
    ?assertEqual(
       "name=value",
       lists:flatten(
         yaws_api:format_set_cookie(
           #setcookie{key="name", value="value"}
          )
        )
      ),

    %% Set-Cookie: name="value [quoted]"
    ?assertEqual(
       "name=\"value [quoted]\"",
       lists:flatten(
         yaws_api:format_set_cookie(
           #setcookie{key="name", value="value [quoted]", quoted=true}
          )
        )
      ),

    %% Set-Cookie: name=value; Domain=test.com; Max-Age=86400;
    %%                         Expires=Sat, 02 May 2009 23:38:25 GMT;
    %%                         Secure; HttpOnly; Path=/; Port=443;
    %%                         Comment="This is a comment";
    %%                         CommentURL="http://localhost"
    ?assertEqual(
       "name=value; Domain=test.com; Max-Age=86400; "
       "Expires=Sat, 02 May 2009 23:38:25 GMT; Secure; HttpOnly; "
       "path=/; port=443; "
       "comment=\"This is a comment\"; commenturl=\"http://localhost\"",
       lists:flatten(
         yaws_api:format_set_cookie(
           #setcookie{key="name", value="value", domain="test.com",
                      max_age="86400", expires="Sat, 02 May 2009 23:38:25 GMT",
                      secure=true, http_only=true,
                      extensions=[{"path", "/", false}, {"port", "443", false},
                                  {"comment", "This is a comment", true},
                                  {"commenturl", "http://localhost", true}]}
          )
        )
      ),
    ok.

format_cookies_test() ->
    %% Cookie: value
    ?assertEqual(
       "value",
       lists:flatten(
         yaws_api:format_cookie(#cookie{key="value"})
        )
      ),

    %% Cookie: name=value
    ?assertEqual(
       "name=value",
       lists:flatten(
         yaws_api:format_cookie(#cookie{key="name", value="value"})
        )
      ),

    %% Cookie: name="value [quoted]"
    ?assertEqual(
       "name=\"value [quoted]\"",
       lists:flatten(
         yaws_api:format_cookie(
           #cookie{key="name", value="value [quoted]", quoted=true}
          )
        )
      ),

    %% Cookie: value1; value2; name=value
    ?assertEqual(
       "value1; value2; name=value",
       lists:flatten(
         yaws_api:format_cookie([#cookie{key="value1"}, #cookie{key="value2"},
                                 #cookie{key="name", value="value"}])
        )
      ),
    ok.


get_multiple_cookies_test() ->
    ?assertEqual("1234", yaws_api:find_cookie_val("abc", ["abc=1234;def=5678"])),
    ?assertEqual("5678", yaws_api:find_cookie_val("def", ["abc=1234;def=5678"])),
    ?assertEqual([], yaws_api:find_cookie_val("ghij", ["abc=1234;def=5678"])),
    ok.

cookie_key_case_insensitive_test() ->
    ?assertEqual("1234", yaws_api:find_cookie_val("abc", ["abc=1234"])),
    ?assertEqual("1234", yaws_api:find_cookie_val("abc", ["ABC=1234"])),
    ?assertEqual("1234", yaws_api:find_cookie_val("AbC", ["aBc=1234"])),
    ok.


set_cookie_expires_test() ->
    %% rfc1123 date format
    ?assertEqual(
       #setcookie{key="a", value="b", expires="Sat, 02 May 2009 23:38:25 GMT"},
       yaws_api:parse_set_cookie("a=b; Expires=Sat, 02 May 2009 23:38:25 GMT")
      ),

    %% rfc1123 date format with hyphens
    ?assertEqual(
       #setcookie{key="a", value="b", expires="Sat, 02-May-2009 23:38:25 GMT"},
       yaws_api:parse_set_cookie("a=b; Expires=Sat, 02-May-2009 23:38:25 GMT")
      ),

    %% rfc1036 date format
    ?assertEqual(
       #setcookie{key="a", value="b", expires="Saturday, 02-May-2009 23:38:25 GMT"},
       yaws_api:parse_set_cookie("a=b; Expires=Saturday, 02-May-2009 23:38:25 GMT")
      ),

    %% AINSI C's astime format: must be in double quotes
    ?assertEqual(
       #setcookie{key="a", value="b", expires="Sat May  2 23:38:25 2009"},
       yaws_api:parse_set_cookie("a=b; Expires=\"Sat May  2 23:38:25 2009\"")
      ),

    ok.

real_cookies_test() ->
    {ok, Io} = case file:open("cookies.dump", read) of
                   {error, _} ->
                       %% handle eunit testing under rebar
                       file:open("../test/eunit/cookies.dump", read);
                   Else ->
                       Else
               end,
    ?assertEqual(ok, parse_cookies(Io, file:read_line(Io), 1)),
    ok.

real_setcookies_test() ->
    {ok, Io} = case file:open("setcookies.dump", read) of
                   {error, _} ->
                       %% handle eunit testing under rebar
                       file:open("../test/eunit/setcookies.dump", read);
                   Else ->
                       Else
               end,
    ?assertEqual(ok, parse_set_cookies(Io, file:read_line(Io), 1)),
    ok.


parse_cookies(Io, eof, _) ->
    file:close(Io),
    ok;
parse_cookies(Io, {error, Reason}, LNo) ->
    file:close(Io),
    {error, Reason, LNo};
parse_cookies(Io, {ok, Line0}, LNo) ->
    Line = string:strip(Line0, right, $\n),
    Cookies = yaws_api:parse_cookie(Line),
    ?assertMatch({LNo, [C|_]} when is_record(C, cookie), {LNo, Cookies}),
    ?assertEqual(
       string:to_lower(Line),
       string:to_lower(lists:flatten(yaws_api:format_cookie(Cookies)))
      ),
    parse_cookies(Io, file:read_line(Io), LNo+1).


parse_set_cookies(Io, eof, _) ->
    file:close(Io),
    ok;
parse_set_cookies(Io, {error, Reason}, LNo) ->
    file:close(Io),
    {error, Reason, LNo};
parse_set_cookies(Io, {ok, Line0}, LNo) ->
    Line = string:strip(Line0, right, $\n),
    SetCookie = yaws_api:parse_set_cookie(Line),
    ?assertMatch({LNo, C} when is_record(C, setcookie), {LNo, SetCookie}),
    ?assertEqual(
       string:to_lower(string:substr(Line, 1, string:chr(Line, $=)-1)),
       SetCookie#setcookie.key
      ),
    parse_set_cookies(Io, file:read_line(Io), LNo+1).
