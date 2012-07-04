-module(cookies).
-compile(export_all).
-include("../../include/yaws_api.hrl").
-include_lib("eunit/include/eunit.hrl").

parse_set_cookies_test() ->
    %% Set-Cookie: value
    ?assertEqual(
       [#setcookie{key="value", quoted=false}],
       yaws_api:parse_set_cookie("value")
      ),

    %% Set-Cookie: name=value
    ?assertEqual(
       [#setcookie{key="name", value="value", quoted=false}],
       yaws_api:parse_set_cookie("name=value")
      ),

    %% Set-Cookie: name="value [quoted]"
    ?assertEqual(
       [#setcookie{key="name", value="value [quoted]", quoted=true}],
       yaws_api:parse_set_cookie("name=\"value [quoted]\"")
      ),

    %% Set-Cookie: name=value; path=/
    ?assertEqual(
       [#setcookie{key="name", value="value", quoted=false, path="/"}],
       yaws_api:parse_set_cookie("name=value; path=/")
      ),

    %% Set-Cookie: name=value; domain=test.com
    ?assertEqual(
       [#setcookie{key="name", value="value", quoted=false, domain="test.com"}],
       yaws_api:parse_set_cookie("name=value; domain=test.com")
      ),

    %% Set-Cookie: name=value; comment="This is a comment"
    ?assertEqual(
       [#setcookie{key="name", value="value", quoted=false, comment="This is a comment"}],
       yaws_api:parse_set_cookie("name=value; comment=\"This is a comment\"")
      ),

    %% Set-Cookie: name=value; comment_url="http://localhost"
    ?assertEqual(
       [#setcookie{key="name", value="value", quoted=false, comment_url="http://localhost"}],
       yaws_api:parse_set_cookie("name=value; commentURL=\"http://localhost\"")
      ),

    %% Set-Cookie: name=value; discard
    ?assertEqual(
       [#setcookie{key="name", value="value", quoted=false, discard=true}],
       yaws_api:parse_set_cookie("name=value; discard")
      ),

    %% Set-Cookie: name=value; secure
    ?assertEqual(
       [#setcookie{key="name", value="value", quoted=false, secure=true}],
       yaws_api:parse_set_cookie("name=value; secure")
      ),

    %% Set-Cookie: name=value; max-age=86400
    ?assertEqual(
       [#setcookie{key="name", value="value", quoted=false, max_age="86400"}],
       yaws_api:parse_set_cookie("name=value; max-age=86400")
      ),

    %% Set-Cookie: name=value; expires="Sat, 02 May 2009 23:38:25 GMT"
    ?assertEqual(
       [#setcookie{key="name", value="value", quoted=false, expires="Sat, 02 May 2009 23:38:25 GMT"}],
       yaws_api:parse_set_cookie("name=value; expires=\"Sat, 02 May 2009 23:38:25 GMT\"")
      ),

    %% Set-Cookie: name=value; port=443
    ?assertEqual(
       [#setcookie{key="name", value="value", quoted=false, port="443"}],
       yaws_api:parse_set_cookie("name=value; port=443")
      ),

    %% Set-Cookie: name=value; version=2
    ?assertEqual(
       [#setcookie{key="name", value="value", quoted=false, version="2"}],
       yaws_api:parse_set_cookie("name=value; version=2")
      ),

    %% Set-Cookie: name=value; unkown-opt=skipped
    ?assertEqual(
       [#setcookie{key="name", value="value", quoted=false}],
       yaws_api:parse_set_cookie("name=value; unkown-opt=skipped")
      ),

    %% Set-Cookie: value1, value2, name=value
    ?assertEqual(
       [#setcookie{key="value1", quoted=false},
        #setcookie{key="value2", quoted=false},
        #setcookie{key="name", value="value", quoted=false}],
       yaws_api:parse_set_cookie("value1 , value2, name=value")
      ),

    %% Set-Cookie: value1; comment="c1", value2; comment="c2", name=value
    ?assertEqual(
       [#setcookie{key="value1", quoted=false, comment="c1"},
        #setcookie{key="value2", quoted=false, comment="c2"},
        #setcookie{key="name", value="value", quoted=false}],
       yaws_api:parse_set_cookie("value1; comment=\"c1\", value2; comment=\"c2\", name=value")
      ),
    ok.

parse_cookies_test() ->
    %% Cookie: value
    ?assertEqual(
       [#cookie{key="value", quoted=false}],
       yaws_api:parse_cookie("value")
      ),

    %% Cookie: name=value
    ?assertEqual(
       [#cookie{key="name", value="value", quoted=false}],
       yaws_api:parse_cookie("name=value")
      ),

    %% Cookie: name="value [quoted]"
    ?assertEqual(
       [#cookie{key="name", value="value [quoted]", quoted=true}],
       yaws_api:parse_cookie("name=\"value [quoted]\"")
      ),

    %% Cookie: name=value; $path=/
    ?assertEqual(
       [#cookie{key="name", value="value", quoted=false, path="/"}],
       yaws_api:parse_cookie("name=value; $path=/")
      ),

    %% Cookie: name=value; $port=443
    ?assertEqual(
       [#cookie{key="name", value="value", quoted=false, port="443"}],
       yaws_api:parse_cookie("name=value; $port=443")
      ),

    %% Cookie: name=value; $domain=test.com
    ?assertEqual(
       [#cookie{key="name", value="value", quoted=false, domain="test.com"}],
       yaws_api:parse_cookie("name=value; $domain=test.com")
      ),

    %% Cookie: name=value, $path=/
    ?assertEqual(
       [#cookie{key="name", value="value", quoted=false, path="/"}],
       yaws_api:parse_cookie("name=value, $path=/")
      ),

    %% Cookie: $version=1, name=value; $path=/
    ?assertEqual(
       [#cookie{key="name", value="value", quoted=false, version="1", path="/"}],
       yaws_api:parse_cookie("$version=1, name=value; $path=/")
      ),

    %% Cookie: $version=1, value1, value2; name=value
    ?assertEqual(
       [#cookie{key="value1", quoted=false, version="1"},
        #cookie{key="value2", quoted=false, version="1"},
        #cookie{key="name", value="value", quoted=false, version="1"}],
       yaws_api:parse_cookie("$version=1, value1, value2; name=value")
      ),
    ok.

parse_invalid_set_cookies_test() ->
    ?assertEqual([], yaws_api:parse_set_cookie("name=value [quoted]")),
    ?assertEqual([], yaws_api:parse_set_cookie("name1=value1 name2=value2")),
    ?assertEqual([], yaws_api:parse_set_cookie("name1=value1 domain=test.com path=/")),
    ?assertEqual([], yaws_api:parse_set_cookie("name=value; secure=1")),
    ?assertEqual([], yaws_api:parse_set_cookie("name=value; discard=1")),
    ok.

parse_invalid_cookies_test() ->
    ?assertEqual([], yaws_api:parse_cookie("name=value [quoted]")),
    ?assertEqual([], yaws_api:parse_cookie("name1=value1 name2=value2")),
    ?assertEqual([], yaws_api:parse_cookie("$version")),
    ?assertEqual([], yaws_api:parse_cookie("$version; name=value")),
    ok.

format_set_cookies_test() ->

    %% Set-Cookie: value
    ?assertEqual(
       "value",
       lists:flatten(
         yaws_api:format_set_cookie(
           #setcookie{key="value", quoted=false}
          )
        )
      ),

    %% Set-Cookie: name=value
    ?assertEqual(
       "name=value",
       lists:flatten(
         yaws_api:format_set_cookie(
           #setcookie{key="name", value="value", quoted=false}
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

    %% Set-Cookie: name=value; Path="/"; Port="443"; Domain="test.com";
    %%                         Secure; Expires="Sat, 02 May 2009 23:38:25 GMT";
    %%                         Max-Age="86400"; Discard; Comment="This is a comment";
    %%                         CommentURL="http://localhost"; Version="1"
    ?assertEqual(
       "name=value; Path=\"/\"; Port=\"443\"; Domain=\"test.com\"; "
       "Secure; Expires=\"Sat, 02 May 2009 23:38:25 GMT\"; Max-Age=\"86400\"; "
       "Discard; Comment=\"This is a comment\"; CommentURL=\"http://localhost\"; "
       "Version=\"1\"",
       lists:flatten(
         yaws_api:format_set_cookie(
           #setcookie{key="name", value="value", quoted=false, comment="This is a comment",
                      comment_url="http://localhost", discard=true, domain="test.com",
                      max_age="86400", expires="Sat, 02 May 2009 23:38:25 GMT",
                      path="/", port="443", secure=true, version="1"}
          )
        )
      ),
    ok.

format_cookies_test() ->
    %% Cookie: $Version=0; value
    ?assertEqual(
       "$Version=0; value",
       lists:flatten(
         yaws_api:format_cookie(
           #cookie{key="value", quoted=false}
          )
        )
      ),

    %% Cookie: $Version=0; name=value
    ?assertEqual(
       "$Version=0; name=value",
       lists:flatten(
         yaws_api:format_cookie(
           #cookie{key="name", value="value", quoted=false}
          )
        )
      ),

    %% Cookie: $Version=0; name="value [quoted]"
    ?assertEqual(
       "$Version=0; name=\"value [quoted]\"",
       lists:flatten(
         yaws_api:format_cookie(
           #cookie{key="name", value="value [quoted]", quoted=true}
          )
        )
      ),

    %% Cookie: $Version=1; name=value; $Path="/"; $Domain="test.com"; $Port="443"
    ?assertEqual(
       "$Version=0; name=value; $Path=\"/\"; $Domain=\"test.com\"; $Port=\"443\"",
       lists:flatten(
         yaws_api:format_cookie(
           #cookie{key="name", value="value", quoted=false, path="/",
                  domain="test.com", port="443"}
          )
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
