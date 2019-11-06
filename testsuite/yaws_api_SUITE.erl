-module(yaws_api_SUITE).

-include("testsuite.hrl").

-compile(export_all).

all() ->
    [
     url_encode_decode,
     url_decode_with_encoding,
     parse_query,
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
             {list_to_binary("one & two"), "one%20%26%20two"},

             %% Reserved characters
             {"!*'();:@&=+$,/?%#[]", "%21%2A%27%28%29%3B%3A%40%26%3D%2B%24%2C%2F%3F%25%23%5B%5D"},

             %% UTF-8 characters ("/d/你好")
             {[47,100,47,20320,22909], "%2Fd%2F%E4%BD%A0%E5%A5%BD"},
             {unicode:characters_to_binary([47,100,47,20320,22909]), "%2Fd%2F%E4%BD%A0%E5%A5%BD"}
            ],
    [begin
         From1 = case From of
                     _ when is_list(From) ->
                         case io_lib:latin1_char_list(From) of
                             true  -> binary_to_list(unicode:characters_to_binary(From));
                             false -> From
                         end;
                     _  -> From
                 end,
         ?assertEqual(To,    yaws_api:url_encode(From1)),
         ?assertEqual(From1, case yaws_api:url_decode(To) of
                                 Decoded when is_binary(From1) ->
                                     unicode:characters_to_binary(Decoded);
                                 Decoded -> Decoded
                             end)
     end || {From, To} <- Tests],
    ok.

url_decode_with_encoding(_Config) ->
    %% USTF-8 characters "디지털기기"
    Input = [46356,51648,53560,44592,44592],
    Result = yaws_api:url_decode_with_encoding(Input, utf8),
    ?assertEqual(Input, Result).

parse_query(_Config) ->
    Arg1 = #arg{querydata = "",
                req = #http_request{method = 'GET',
                                    path = {abs_path, "/foo"},
                                    version = {1,1}}},
    ?assertEqual(yaws_api:parse_query(Arg1), []),
    ParamName = "param",
    ParamVal = "42",
    Param = ParamName ++ "=" ++ ParamVal,
    Arg2 = #arg{querydata = Param,
                req = #http_request{method = 'GET',
                                    path = {abs_path,
                                            "/foo?" ++ Param},
                                    version = {1,1}}},
    ?assertEqual(yaws_api:parse_query(Arg2), [{ParamName,ParamVal}]),
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
