-module(headers).
-compile(export_all).
-include("../../include/yaws_api.hrl").
-include_lib("eunit/include/eunit.hrl").

set_headers_test() ->
    Value = "test value",
    lists:foreach(fun({Hdr, StrHdr, Fun}) ->
                          NewHdrs = yaws_api:set_header(#headers{}, Hdr, Value),
                          Value = yaws_api:Fun(NewHdrs),
                          Value = yaws_api:get_header(NewHdrs, StrHdr),
                          0 = length(NewHdrs#headers.other)
                  end, field_headers()),
    Hdrs = yaws_api:set_header(create_headers(99), age, "123"),
    100 = length(Hdrs#headers.other),
    ok.

get_headers_test() ->
    undefined = yaws_api:get_header(#headers{}, accept),
    undefined = yaws_api:get_header(#headers{}, "Connection"),
    none = yaws_api:get_header(#headers{}, range, none),
    none = yaws_api:get_header(#headers{}, "No-Such-Header", none),
    Hdrs = create_headers(100),
    lists:foreach(fun(I) ->
                          L = integer_to_list(I),
                          Val = "value"++L,
                          Hdr = "X-HEADER-"++L,
                          Val = yaws_api:get_header(Hdrs, Hdr)
                  end, lists:seq(1,100)),
    ok.

delete_headers_test() ->
    Headers = create_headers(10),
    {0, #headers{}} = lists:foldl(
                        fun(I, {Size, Hdrs}) ->
                                Size = length(Hdrs#headers.other),
                                L = integer_to_list(I),
                                Hdr = "X-Header-"++L,
                                NHdrs = yaws_api:delete_header(Hdrs, Hdr),
                                NewSize = Size - 1,
                                NewSize = length(NHdrs#headers.other),
                                {NewSize, NHdrs}
                        end, {10, Headers}, lists:seq(1,10)),
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
