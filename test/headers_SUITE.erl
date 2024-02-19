-module(headers_SUITE).

-include("testsuite.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [
     set_headers,
     get_headers,
     delete_headers,
     merge_headers,
     reformat_headers
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
set_headers(_Config) ->
    Value = "test value",
    lists:foreach(
      fun({Hdr, StrHdr, Fun}) ->
              NewHdrs = yaws_api:set_header(#headers{}, Hdr, Value),
              ?assertEqual(Value, yaws_api:Fun(NewHdrs)),
              ?assertEqual(Value, yaws_api:get_header(NewHdrs, StrHdr)),
              ?assertEqual(0, length(NewHdrs#headers.other))
      end,
      field_headers()
     ),
    Hdrs = yaws_api:set_header(create_headers(99), age, "123"),
    ?assertEqual(100, length(Hdrs#headers.other)),
    ok.

get_headers(_Config) ->
    ?assertEqual(undefined, yaws_api:get_header(#headers{}, accept)),
    ?assertEqual(undefined, yaws_api:get_header(#headers{}, "Connection")),
    ?assertEqual(none,      yaws_api:get_header(#headers{}, range, none)),
    ?assertEqual(none,      yaws_api:get_header(#headers{}, "No-Such-Header", none)),
    Hdrs = create_headers(100),
    lists:foreach(
      fun(I) ->
              L = integer_to_list(I),
              Val = "value"++L,
              Hdr = "X-HEADER-"++L,
              ?assertEqual(Val, yaws_api:get_header(Hdrs, Hdr))
      end,
      lists:seq(1,100)
     ),
    ok.

delete_headers(_Config) ->
    Headers = create_headers(10),
    Res = lists:foldl(
            fun(I, {Size, Hdrs}) ->
                    ?assertEqual(Size, length(Hdrs#headers.other)),
                    L = integer_to_list(I),
                    Hdr = "X-Header-"++L,
                    NHdrs = yaws_api:delete_header(Hdrs, Hdr),
                    NewSize = Size - 1,
                    ?assertEqual(NewSize, length(NHdrs#headers.other)),
                    {NewSize, NHdrs}
            end,
            {10, Headers},
            lists:seq(1,10)
           ),
    ?assertEqual({0, #headers{}}, Res),
    ok.

merge_headers(_Config) ->
    Hdrs0 = create_headers(10),
    Hdrs1 = yaws_api:merge_header(Hdrs0, <<"x-header-7">>, <<"another-value">>),
    Val1 = yaws_api:get_header(Hdrs1, 'x-header-7'),
    Expected1 = lists:sort(["value7", "another-value"]),
    ?assertEqual(Expected1, lists:sort(string:tokens(Val1, ", "))),
    Hdrs2 = yaws_api:set_header(Hdrs1, "set-cookie", "user=joe"),
    Hdrs3 = yaws_api:merge_header(Hdrs2, "set-cookie", "domain=erlang.org"),
    Val2 = yaws_api:get_header(Hdrs3, "set-cookie"),
    Expected2 = {multi, ["user=joe", "domain=erlang.org"]},
    ?assertEqual(Expected2, Val2),
    ok.

reformat_headers(_Config) ->
    Hdrs0 = create_headers(10),
    Hdrs1 = yaws_api:set_header(Hdrs0, "set-cookie", "user=joe"),
    Hdrs2 = yaws_api:set_header(Hdrs1, "Connection", "close"),
    Hdrs3 = yaws_api:set_header(Hdrs2, 'X-Atom', 'an atom header/value'),
    Hdrs4 = yaws_api:merge_header(Hdrs3, "set-cookie", "domain=erlang.org"),
    Hdrs5 = yaws_api:reformat_header(Hdrs4),
    ?assert(lists:all(fun(T) -> T end, [lists:flatten(H) == H || H <- Hdrs5])),
    %% Verify that a user-defined FormatFun that doesn't handle
    %% {multi, ValueList} as documented in yaws_api.5 still works
    Hdrs6 = yaws_api:reformat_header(Hdrs4,
                                     fun(H, {multi, Vals}) ->
                                             %% return a list of strings
                                             [lists:flatten(
                                                io_lib:format("~s: ~s", [H, V])) ||
                                                    V <- Vals];
                                        (H, V) ->
                                             %% return a binary
                                             list_to_binary(
                                               io_lib:format("~s: ~s", [H, V]))
                                     end),
    %% Only one element of the Hdrs5 list should be a list: the one
    %% returned for the {multi, ValueList} tuple.
    Expected = [["Set-Cookie: user=joe", "Set-Cookie: domain=erlang.org"]],
    ?assertEqual(Expected, lists:filter(fun(V) -> is_list(V) end, Hdrs6)),
    %% Verify reformat_header options
    Hdrs7 = yaws_api:reformat_header(Hdrs4,
                                     fun(H, {multi, Vals}) when is_binary(H) ->
                                             lists:all(fun erlang:is_binary/1, Vals);
                                        (H, V) when is_binary(H), is_binary(V) ->
                                             true;
                                        (_, _) -> false
                                     end,
                                     [binary, string]),
    ?assert(lists:all(fun(X) -> X end, Hdrs7)),
    ok.

create_headers(N) ->
    lists:foldl(fun({Hdr,Val}, Hdrs) ->
                        yaws_api:set_header(Hdrs, Hdr, Val)
                end,
                #headers{},
                [begin
                     L = integer_to_list(I),
                     {"X-Header-"++L, "value"++L}
                 end || I <- lists:seq(1,N)]).

%% these headers are the fields in the #headers{} record
field_headers() ->
    [{connection, "Connection", headers_connection},
     {accept, "Accept", headers_accept},
     {host, "Host", headers_host},
     {if_modified_since,"If-Modified-Since", headers_if_modified_since},
     {if_match, "If-Match", headers_if_match},
     {if_none_match, "If-None-Match", headers_if_none_match},
     {if_range, "if-Range", headers_if_range},
     {if_unmodified_since, "If-Unmodified-Since", headers_if_unmodified_since},
     {range, "Range", headers_range},
     {referer, "Referer", headers_referer},
     {user_agent, "User-Agent", headers_user_agent},
     {accept_ranges, "Accept-Ranges", headers_accept_ranges},
     {cookie, "Cookie", headers_cookie},
     {keep_alive, "Keep-Alive", headers_keep_alive},
     {location, "Location", headers_location},
     {content_length, "Content-Length", headers_content_length},
     {content_type, "Content-Type", headers_content_type},
     {content_encoding, "Content-Encoding", headers_content_encoding},
     {authorization, "Authorization", headers_authorization},
     {transfer_encoding, "Transfer-Encoding", headers_transfer_encoding},
     {x_forwarded_for, "X-Forwarded-For", headers_x_forwarded_for}].
