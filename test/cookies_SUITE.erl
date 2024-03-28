-module(cookies_SUITE).

-include("testsuite.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [
     parse_set_cookies,
     parse_cookies,
     parse_invalid_set_cookies,
     parse_invalid_cookies,
     format_cookies,
     get_multiple_cookies,
     cookie_key_case_insensitive,
     set_cookie_expires,
     real_cookies,
     real_setcookies,
     set_cookie,
     set_cookie_duplicates
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
parse_set_cookies(_Config) ->
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

parse_cookies(_Config) ->
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

    %% Cookies with trailing spaces
    ?assertEqual(
       [#cookie{key="value"}],
       yaws_api:parse_cookie("value ")
      ),
    ?assertEqual(
       [#cookie{key="name", value="value"}],
       yaws_api:parse_cookie("name=value  ")
      ),
    ?assertEqual(
       [#cookie{key="name", value="value"}],
       yaws_api:parse_cookie("name=value;  ")
      ),
    ?assertEqual(
       [#cookie{key="name", value="value [quoted]", quoted=true}],
       yaws_api:parse_cookie("name=\"value [quoted]\" \n")
      ),
    ?assertEqual(
       [#cookie{key="$version", value="1"}, #cookie{key="value1"},
        #cookie{key="value2"}, #cookie{key="name", value="value"}],
       yaws_api:parse_cookie("  $version  =  1\n ,   value1\t  ,   value2\r\n; name  =\n\t  value  ;  ")
      ),
    ok.

parse_invalid_set_cookies(_Config) ->
    ?assertEqual([], yaws_api:parse_set_cookie("name=value [quoted]")),
    ?assertEqual([], yaws_api:parse_set_cookie("name1=value1 name2=value2")),
    ?assertEqual([], yaws_api:parse_set_cookie("name1=value1 domain=test.com path=/")),
    ?assertEqual([], yaws_api:parse_set_cookie("name=value; secure=1")),
    ok.

parse_invalid_cookies(_Config) ->
    ?assertEqual([], yaws_api:parse_cookie("name=value [quoted]")),
    ?assertEqual([], yaws_api:parse_cookie("name1=value1 name2=value2")),
    ok.

format_cookies(_Config) ->
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

get_multiple_cookies(_Config) ->
    Cookies = ["abc=1234;def=5678"],
    Headers = #headers{cookie = Cookies},
    Arg = #arg{headers = Headers},
    %% Headers list as input
    ?assertEqual("1234", yaws_api:find_cookie_val("abc", Cookies)),
    ?assertEqual("5678", yaws_api:find_cookie_val("def", Cookies)),
    ?assertEqual([], yaws_api:find_cookie_val("ghij", Cookies)),
    %% #headers{} record as input
    ?assertEqual("1234", yaws_api:find_cookie_val("abc", Headers)),
    ?assertEqual("5678", yaws_api:find_cookie_val("def", Headers)),
    ?assertEqual([], yaws_api:find_cookie_val("ghij", Headers)),
    %% #arg{} record as input
    ?assertEqual("1234", yaws_api:find_cookie_val("abc", Arg)),
    ?assertEqual("5678", yaws_api:find_cookie_val("def", Arg)),
    ?assertEqual([], yaws_api:find_cookie_val("ghij", Arg)),
    ok.

cookie_key_case_insensitive(_Config) ->
    Cookies1 = ["abc=1234"],
    Cookies2 = ["ABC=1234"],
    Cookies3 = ["aBc=1234"],
    %% Headers list as input
    ?assertEqual("1234", yaws_api:find_cookie_val("abc", Cookies1)),
    ?assertEqual("1234", yaws_api:find_cookie_val("abc", Cookies2)),
    ?assertEqual("1234", yaws_api:find_cookie_val("AbC", Cookies3)),
    %% #headers{} record as input
    ?assertEqual("1234",
                 yaws_api:find_cookie_val("abc", #headers{cookie = Cookies1})),
    ?assertEqual("1234",
                 yaws_api:find_cookie_val("abc", #headers{cookie = Cookies2})),
    ?assertEqual("1234",
                 yaws_api:find_cookie_val("AbC", #headers{cookie = Cookies3})),
    %% #arg{} record as input
    Arg1 = #arg{headers = #headers{cookie = Cookies1}},
    Arg2 = #arg{headers = #headers{cookie = Cookies2}},
    Arg3 = #arg{headers = #headers{cookie = Cookies3}},
    ?assertEqual("1234", yaws_api:find_cookie_val("abc", Arg1)),
    ?assertEqual("1234", yaws_api:find_cookie_val("abc", Arg2)),
    ?assertEqual("1234", yaws_api:find_cookie_val("AbC", Arg3)),
    ok.


set_cookie_expires(_Config) ->
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

real_cookies(_Config) ->
    File = filename:join(?data_srcdir(?MODULE), "cookies.dump"),
    {ok, Io} = file:open(File, read),
    ?assertEqual(ok, parse_cookies(Io, file:read_line(Io), 1)),
    ok.

real_setcookies(_Config) ->
    File = filename:join(?data_srcdir(?MODULE), "setcookies.dump"),
    {ok, Io} = file:open(File, read),
    ?assertEqual(ok, parse_set_cookies(Io, file:read_line(Io), 1)),
    ok.

set_cookie(_Config) ->
    ?assertEqual(
        "a=bcd; Comment=OK; Domain=g.com; Path=/; SameSite=Lax; Max-Age=1; "
        "Expires=Tue, 03 Jan 2012 10:00:05 GMT; HttpOnly; Secure",
        begin
            {header, {set_cookie, L}} = yaws_api:set_cookie("a", "bcd",
                [{expires, {{2012,1,3},{10,0,5}}},
                 {max_age, 1}, secure, http_only, {same_site, lax},
                 {path, "/"}, {domain, "g.com"}, {comment, "OK"}]),
            lists:flatten(L)
        end),
    ?assertEqual(
        "a=bcd; Path=/home",
        begin
            {header, {set_cookie, L}} =
                yaws_api:set_cookie("a", "bcd", [{path, "/home"}]),
            lists:flatten(L)
        end),
    ?assertEqual(
        "a=bcd",
        begin
            {header, {set_cookie, L}} = yaws_api:set_cookie("a", "bcd", []),
            lists:flatten(L)
        end).

set_cookie_duplicates(_Config) ->
    Cookie = "Cookie",
    put(outh, #outh{}),
    yaws:accumulate_header({set_cookie, Cookie}),
    yaws:accumulate_header({"Set-Cookie", Cookie}),
    yaws:accumulate_header({set_cookie, {multi, [Cookie, Cookie, Cookie]}}),
    yaws:accumulate_header({"Set-Cookie", {multi, [Cookie, Cookie, Cookie]}}),
    ?assertEqual([["Set-Cookie: ", Cookie, "\r\n"]], (get(outh))#outh.set_cookie).

%%====================================================================
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
