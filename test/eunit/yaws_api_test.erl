-module(yaws_api_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("yaws.hrl").
-include("tftest.hrl").

url_encode_decode_test_() ->
    Tests = [
             {"file", "file"},
             {"\r\n", "%0D%0A"},
             {"one & two", "one%20%26%20two"},

             %% Reserved characters (/ and : are not encoded, see yaws_api.erl#L892)
             {"!*'();:@&=+$,/?%#[]", "%21%2A%27%28%29%3B:%40%26%3D%2B%24%2C/%3F%25%23%5B%5D"},

             %% UTF-8 characters (/d/你好)
             {[47,100,47,20320,22909], "/d/%E4%BD%A0%E5%A5%BD"}
            ],
    [fun() ->
             From1 = case file:native_name_encoding() of
                         latin1 -> binary_to_list(unicode:characters_to_binary(From));
                         utf8   -> From
                     end,
             ?assertEqual(To,   yaws_api:url_encode(From1)),
             ?assertEqual(From1, yaws_api:url_decode(To))
     end || {From, To} <- Tests].
