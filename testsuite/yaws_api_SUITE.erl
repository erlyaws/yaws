-module(yaws_api_SUITE).

-include("testsuite.hrl").

-export([
    url_encode_decode/1,
    request_url/1
]).
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

all() ->
    [
     url_encode_decode,
     request_url
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
url_encode_decode(_Config) ->
    Tests = [
             {"file", "file"},
             {"\r\n", "%0D%0A"},
             {"one & two", "one%20%26%20two"},

             %% Reserved characters (/ and : are not encoded, see yaws_api.erl#L892)
             {"!*'();:@&=+$,/?%#[]", "%21%2A%27%28%29%3B:%40%26%3D%2B%24%2C/%3F%25%23%5B%5D"},

             %% UTF-8 characters (/d/你好)
             {[47,100,47,20320,22909], "/d/%E4%BD%A0%E5%A5%BD"}
            ],
    [begin
         From1 = case file:native_name_encoding() of
                     latin1 -> binary_to_list(unicode:characters_to_binary(From));
                     utf8   -> From
                 end,
         ?assertEqual(To,    yaws_api:url_encode(From1)),
         ?assertEqual(From1, yaws_api:url_decode(To))
     end || {From, To} <- Tests],
    ok.

%% ----
request_url(_Config) ->
    ?assertEqual(request_url1(), ok),
    ?assertEqual(request_url2(), ok),
    ?assertEqual(request_url3(), ok),
    ?assertEqual(request_url4(), ok),
    ?assertEqual(request_url5(), ok),
    ?assertEqual(request_url6(), ok),
    ?assertEqual(request_url7(), ok),
    ?assertEqual(request_url8(), ok),
    ?assertEqual(request_url9(), ok),
    ?assertEqual(request_url10(), ok),
    ok.

request_url1() ->
    SC = #sconf{servername="example.com", ssl=undefined},
    Arg = #arg{req     = #http_request{path={abs_patch, "/page.yaws?query-string=1"}},
               headers = #headers{}},
    put(sc, SC),
    Req = yaws_api:request_url(Arg),
    ?assertEqual(Req#url.scheme,   "http"),
    ?assertEqual(Req#url.host,     "example.com"),
    ?assertEqual(Req#url.port,     undefined),
    ?assertEqual(Req#url.path,     "/page.yaws"),
    ?assertEqual(Req#url.querypart, "query-string=1"),
    ok.

request_url2() ->
    SC = #sconf{servername="example.com:8000", ssl=undefined},
    Arg = #arg{req     = #http_request{path={abs_patch, "/page.yaws?query-string=1"}},
               headers = #headers{}},
    put(sc, SC),
    Req = yaws_api:request_url(Arg),
    ?assertEqual(Req#url.scheme,   "http"),
    ?assertEqual(Req#url.host,     "example.com"),
    ?assertEqual(Req#url.port,     8000),
    ?assertEqual(Req#url.path,     "/page.yaws"),
    ?assertEqual(Req#url.querypart, "query-string=1"),
    ok.

request_url3() ->
    SC = #sconf{servername="example.com", ssl=placeholder},
    Arg = #arg{req     = #http_request{path={abs_patch, "/page.yaws?query-string=1"}},
               headers = #headers{}},
    put(sc, SC),
    Req = yaws_api:request_url(Arg),
    ?assertEqual(Req#url.scheme,   "https"),
    ?assertEqual(Req#url.host,     "example.com"),
    ?assertEqual(Req#url.port,     undefined),
    ?assertEqual(Req#url.path,     "/page.yaws"),
    ?assertEqual(Req#url.querypart, "query-string=1"),
    ok.

request_url4() ->
    SC = #sconf{servername="example.com:4443", ssl=placeholder},
    Arg = #arg{req     = #http_request{path={abs_patch, "/page.yaws?query-string=1"}},
               headers = #headers{}},
    put(sc, SC),
    Req = yaws_api:request_url(Arg),
    ?assertEqual(Req#url.scheme,   "https"),
    ?assertEqual(Req#url.host,     "example.com"),
    ?assertEqual(Req#url.port,     4443),
    ?assertEqual(Req#url.path,     "/page.yaws"),
    ?assertEqual(Req#url.querypart, "query-string=1"),
    ok.

request_url5() ->
    SC = #sconf{servername="example.com", ssl=undefined},
    Arg = #arg{req     = #http_request{path={abs_patch, "/page.yaws?query-string=1"}},
               headers = #headers{host="www.example.com"}},
    put(sc, SC),
    Req = yaws_api:request_url(Arg),
    ?assertEqual(Req#url.scheme,   "http"),
    ?assertEqual(Req#url.host,     "www.example.com"),
    ?assertEqual(Req#url.port,     undefined),
    ?assertEqual(Req#url.path,     "/page.yaws"),
    ?assertEqual(Req#url.querypart, "query-string=1"),
    ok.

request_url6() ->
    SC = #sconf{servername="example.com", ssl=undefined},
    Arg = #arg{req     = #http_request{path={abs_patch, "/page.yaws?query-string=1"}},
               headers = #headers{host="www.example.com:8000"}},
    put(sc, SC),
    Req = yaws_api:request_url(Arg),
    ?assertEqual(Req#url.scheme,   "http"),
    ?assertEqual(Req#url.host,     "www.example.com"),
    ?assertEqual(Req#url.port,     8000),
    ?assertEqual(Req#url.path,     "/page.yaws"),
    ?assertEqual(Req#url.querypart, "query-string=1"),
    ok.

request_url7() ->
    SC = #sconf{servername="example.com", ssl=placeholder},
    Arg = #arg{req     = #http_request{path={abs_patch, "/page.yaws?query-string=1"}},
               headers = #headers{host="www.example.com"}},
    put(sc, SC),
    Req = yaws_api:request_url(Arg),
    ?assertEqual(Req#url.scheme,   "https"),
    ?assertEqual(Req#url.host,     "www.example.com"),
    ?assertEqual(Req#url.port,     undefined),
    ?assertEqual(Req#url.path,     "/page.yaws"),
    ?assertEqual(Req#url.querypart, "query-string=1"),
    ok.

request_url8() ->
    SC = #sconf{servername="example.com", ssl=placeholder},
    Arg = #arg{req     = #http_request{path={abs_patch, "/page.yaws?query-string=1"}},
               headers = #headers{host="www.example.com:4443"}},
    put(sc, SC),
    Req = yaws_api:request_url(Arg),
    ?assertEqual(Req#url.scheme,   "https"),
    ?assertEqual(Req#url.host,     "www.example.com"),
    ?assertEqual(Req#url.port,     4443),
    ?assertEqual(Req#url.path,     "/page.yaws"),
    ?assertEqual(Req#url.querypart, "query-string=1"),
    ok.

request_url9() ->
    SC = #sconf{servername="example.com", ssl=undegined},
    Arg = #arg{req     = #http_request{path={abs_patch, "/page.yaws?query-string=1"}},
               headers = #headers{host="127.0.0.1:8000"}},
    put(sc, SC),
    Req = yaws_api:request_url(Arg),
    ?assertEqual(Req#url.scheme,   "https"),
    ?assertEqual(Req#url.host,     "127.0.0.1"),
    ?assertEqual(Req#url.port,     8000),
    ?assertEqual(Req#url.path,     "/page.yaws"),
    ?assertEqual(Req#url.querypart, "query-string=1"),
    ok.

request_url10() ->
    SC = #sconf{servername="example.com", ssl=placeholder},
    Arg = #arg{req     = #http_request{path={abs_patch, "/page.yaws?query-string=1"}},
               headers = #headers{host="[2001:610:240:22::c100:68b]:4443"}},
    put(sc, SC),
    Req = yaws_api:request_url(Arg),
    ?assertEqual(Req#url.scheme,   "https"),
    ?assertEqual(Req#url.host,     "[2001:610:240:22::c100:68b]"),
    ?assertEqual(Req#url.port,     4443),
    ?assertEqual(Req#url.path,     "/page.yaws"),
    ?assertEqual(Req#url.querypart, "query-string=1"),
    ok.
