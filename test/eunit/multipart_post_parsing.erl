-module(multipart_post_parsing).
-compile(export_all).
-include("../../include/yaws_api.hrl").
-include_lib("eunit/include/eunit.hrl").

data_to_parse() ->
    list_to_binary(
      ["--!!!\r\n",
       "Content-Disposition: form-data; name=\"abc123\"; "
       ++ "filename=\"abc123\"\r\n"
       ++ "Content-Type: text/plain\r\n"
       ++ "Test-Header: sampledata\r\n\r\n",
       "sometext\n\r\n--!!!--\r\n"]).

complete_parse(Opt) ->
    yaws_api:parse_multipart_post(mk_arg(data_to_parse()), [Opt]).

test_complete_parse(Opt) ->
    {result, Params} = complete_parse(Opt),
    2 = length(Params),
    {"abc123", HeadParams} = proplists:get_value(head, Params),
    4 = length(HeadParams),
    "abc123" = proplists:get_value("name", HeadParams),
    "abc123" = proplists:get_value("filename", HeadParams),
    "text/plain" = proplists:get_value(content_type, HeadParams),
    "sampledata" = proplists:get_value("test-header", HeadParams),
    proplists:get_value(body, Params).

complete_parse_list_test() ->
    "sometext\n" = test_complete_parse(list),
    ok.

complete_parse_binary_test() ->
    <<"sometext\n">> = test_complete_parse(binary),
    ok.

test_incomplete_body(Opt) ->
    Data1 = list_to_binary(
	      ["--!!!\r\n",
	       "Content-Disposition: form-data; name=\"abc123\"; "
	       ++ "filename=\"abc123\"\r\n\r\n",
	       "someincomplete"]),
    A1 = mk_arg(Data1),
    {cont, Cont, Res1} = yaws_api:parse_multipart_post(A1, [Opt]),
    Data2 = list_to_binary(
	      ["text\n\r\n--!!!\r\n",
	       "Content-Disposition: form-data; name=\"def456\"; "
	       ++ "filename=\"def456\"\r\n\r\n",
	       "sometext\n\r\n--!!!--\r\n"]),
    A2 = A1#arg{cont = Cont, clidata = Data2},
    {result, Res2} = yaws_api:parse_multipart_post(A2, [Opt]),
    2 = length(Res1),
    {"abc123", HeadParams1} = proplists:get_value(head, Res1),
    PB = proplists:get_value(part_body, Res1),
    2 = length(HeadParams1),
    "abc123" = proplists:get_value("filename", HeadParams1),
    "abc123" = proplists:get_value("name", HeadParams1),
    3 = length(Res2),
    {"def456", HeadParams2} = proplists:get_value(head, Res2),
    BL = lists:sort([B || {K, _}=B <- Res2, K =:= body]),
    2 = length(HeadParams2),
    "def456" = proplists:get_value("filename", HeadParams2),
    "def456" = proplists:get_value("name", HeadParams2),
    {PB, BL}.

incomplete_body_list_test() ->
    {"someinc", [{body, "ompletetext\n"}, {body, "sometext\n"}]} =
        test_incomplete_body(list).

incomplete_body_binary_test() ->
    {<<"someinc">>, [{body, <<"ompletetext\n">>}, {body, <<"sometext\n">>}]} =
        test_incomplete_body(binary).

test_incomplete_head_list(Opt) ->
    Data1 = list_to_binary(
	      ["--!!!\r\n",
	       "Content-Disposition: form-data; name=\"abc123\"; "
	       ++ "filename=\"abc123\"\r\n\r\n",
	       "sometext1\n\r\n--!!!\r\n",
	       "Content-Disposition: form-data; name=\"ghi"]),
    A1 = mk_arg(Data1),
    {cont, Cont, Res1} = yaws_api:parse_multipart_post(A1, [Opt]),
    Data2 = list_to_binary(
	      ["789\"; "
	       ++ "filename=\"ghi789\"\r\n\r\n",
	       "sometext2\n\r\n--!!!--\r\n"]),
    A2 = A1#arg{cont = Cont, clidata = Data2},
    {result, Res2} = yaws_api:parse_multipart_post(A2),
    2 = length(Res1),
    {"abc123", HeadParams1} = proplists:get_value(head, Res1),
    Body1 = proplists:get_value(body, Res1),
    2 = length(HeadParams1),
    "abc123" = proplists:get_value("filename", HeadParams1),
    "abc123" = proplists:get_value("name", HeadParams1),
    2 = length(Res2),
    {"ghi789", HeadParams2} = proplists:get_value(head, Res2),
    Body2 = proplists:get_value(body, Res2),
    2 = length(HeadParams1),
    "ghi789" = proplists:get_value("filename", HeadParams2),
    "ghi789" = proplists:get_value("name", HeadParams2),
    {Body1, Body2}.

incomplete_head_list_test() ->
    {"sometext1\n", "sometext2\n"} = test_incomplete_head_list(list),
    ok.

incomplete_head_binary_test() ->
    {<<"sometext1\n">>, <<"sometext2\n">>} = test_incomplete_head_list(binary),
    ok.

test_incomplete_boundary_list(Opt) ->
    Data1 = list_to_binary(
              ["--!!!\r\n",
               "Content-Disposition: form-data; name=\"abc123\"; "
               ++ "filename=\"abc123\"\r\n\r\n",
               "sometext1\n\r\n--!!!"]),
    A1 = mk_arg(Data1),
    {cont, Cont1, Res1} = yaws_api:parse_multipart_post(A1, [Opt]),
    Data2 = list_to_binary(
              ["\r\nContent-Disposition: form-data; name=\"ghi789\"; "
               ++ "filename=\"ghi789\"\r\n\r\n",
               "sometext2\n\r\n--!!!"]),
    A2 = A1#arg{cont = Cont1, clidata = Data2},
    {cont, Cont2, Res2} = yaws_api:parse_multipart_post(A2),
    Data3 = <<"--\r\n">>,
    A3 = A2#arg{cont = Cont2, clidata = Data3},
    {result, []} = yaws_api:parse_multipart_post(A3),
    2 = length(Res1),
    {"abc123", HeadParams1} = proplists:get_value(head, Res1),
    Body1 = proplists:get_value(body, Res1),
    2 = length(HeadParams1),
    "abc123" = proplists:get_value("filename", HeadParams1),
    "abc123" = proplists:get_value("name", HeadParams1),
    2 = length(Res2),
    {"ghi789", HeadParams2} = proplists:get_value(head, Res2),
    Body2 = proplists:get_value(body, Res2),
    2 = length(HeadParams1),
    "ghi789" = proplists:get_value("filename", HeadParams2),
    "ghi789" = proplists:get_value("name", HeadParams2),
    {Body1, Body2}.

incomplete_boundary_list_test() ->
    {"sometext1\n", "sometext2\n"} = test_incomplete_boundary_list(list),
    ok.

incomplete_boundary_binary_test() ->
    {<<"sometext1\n">>, <<"sometext2\n">>} = test_incomplete_boundary_list(binary),
    ok.

read_multipart_form_base(Opt) ->
    {done, Dict} = yaws_multipart:read_multipart_form(mk_arg(data_to_parse()),
                                                      [no_temp_file, Opt]),
    {ok, Params} = dict:find("abc123", Dict),
    "abc123" = proplists:get_value("filename", Params),
    "text/plain" = proplists:get_value(content_type, Params),
    "sampledata" = proplists:get_value("test-header", Params),
    proplists:get_value(value, Params).

read_multipart_form_list_test() ->
    "sometext\n" = read_multipart_form_base(list),
    ok.

read_multipart_form_binary_test() ->
    <<"sometext\n">> = read_multipart_form_base(binary),
    ok.

malformed_multipart_form_test() ->
    Data1 = list_to_binary(
              ["--!!!\r\n",
               "Content-Disposition: form-data; name=\"abc123\"; "
               ++ "filename=\"abc123\"\r\n\r\n",
               "sometext\n\r\n--!!!Oops"]),
    A1 = mk_arg(Data1),
    {error, malformed_multipart_post} = yaws_api:parse_multipart_post(A1),
    Data2 = list_to_binary(
              ["--!!!\r\n",
               "Content-Disposition: form-data; name=\"abc123\"; "
               ++ "filename=\"abc123\"\r\n",
               "Invalid-Header\r\n\r\n"
               "sometext\n\r\n--!!!"]),
    A2 = mk_arg(Data2),
    {error, malformed_multipart_post} = yaws_api:parse_multipart_post(A2),
    Req1  = #http_request{method = 'GET'},
    Req2  = #http_request{method = 'POST'},
    Hdrs1 = #headers{},
    Hdrs2 = #headers{content_type = "text/plain"},
    A3 = #arg{headers=Hdrs1, req=Req1},
    {error, bad_method} = yaws_api:parse_multipart_post(A3),
    A4 = #arg{headers=Hdrs1, req=Req2},
    {error, no_content_type} = yaws_api:parse_multipart_post(A4),
    A5 = #arg{headers=Hdrs2, req=Req2},
    {error, no_multipart_form_data} = yaws_api:parse_multipart_post(A5),
    ok.

mk_arg(Data) ->
    ContentType = "multipart/form-data; boundary=!!!",
    Req = #http_request{method = 'POST'},
    Headers = #headers{content_type = ContentType},
    #arg{headers = Headers, req = Req, clidata = Data}.
