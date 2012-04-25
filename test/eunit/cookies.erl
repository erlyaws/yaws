-module(cookies).
-compile(export_all).
-include("../../include/yaws_api.hrl").
-include_lib("eunit/include/eunit.hrl").

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
