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

complete_parse() ->
    yaws_api:parse_multipart_post(mk_arg(data_to_parse())).

complete_parse_test() ->
    {result, Params} = complete_parse(),
    2 = length(Params),
    {"abc123", HeadParams} = proplists:get_value(head, Params),
    "sometext\n" = proplists:get_value(body, Params),
    4 = length(HeadParams),
    "abc123" = proplists:get_value(name, HeadParams),
    "abc123" = proplists:get_value(filename, HeadParams),
    "text/plain" = proplists:get_value(content_type, HeadParams),
    "sampledata" = proplists:get_value("test-header", HeadParams),
    ok.

incomplete_body_test() ->
    Data1 = list_to_binary(
	      ["--!!!\r\n",
	       "Content-Disposition: form-data; name=\"abc123\"; "
	       ++ "filename=\"abc123\"\r\n\r\n",
	       "some"]),
    A1 = mk_arg(Data1),
    {cont, Cont, Res1} = yaws_api:parse_multipart_post(A1),
    Data2 = list_to_binary(
	      ["text\n\r\n--!!!\r\n",
	       "Content-Disposition: form-data; name=\"def456\"; "
	       ++ "filename=\"def456\"\r\n\r\n",
	       "sometext\n\r\n--!!!--\r\n"]),
    A2 = A1#arg{cont = Cont, clidata = Data2},
    {result, Res2} = yaws_api:parse_multipart_post(A2),
    2 = length(Res1),
    {"abc123", HeadParams1} = proplists:get_value(head, Res1),
    "some" = proplists:get_value(part_body, Res1),
    2 = length(HeadParams1),
    "abc123" = proplists:get_value(filename, HeadParams1),
    "abc123" = proplists:get_value(name, HeadParams1),
    3 = length(Res2),
    {"def456", HeadParams2} = proplists:get_value(head, Res2),
    [{body, "sometext\n"},
     {body, "text\n"}] = lists:sort([B || {K, _}=B <- Res2, K =:= body]),
    2 = length(HeadParams2),
    "def456" = proplists:get_value(filename, HeadParams2),
    "def456" = proplists:get_value(name, HeadParams2),
    ok.

incomplete_head_test() ->
    Data1 = list_to_binary(
	      ["--!!!\r\n",
	       "Content-Disposition: form-data; name=\"abc123\"; "
	       ++ "filename=\"abc123\"\r\n\r\n",
	       "sometext\n\r\n--!!!\r\n",
	       "Content-Disposition: form-data; name=\"ghi"]),
    A1 = mk_arg(Data1),
    {cont, Cont, Res1} = yaws_api:parse_multipart_post(A1),
    Data2 = list_to_binary(
	      ["789\"; "
	       ++ "filename=\"ghi789\"\r\n\r\n",
	       "sometext\n\r\n--!!!--\r\n"]),
    A2 = A1#arg{cont = Cont, clidata = Data2},
    {result, Res2} = yaws_api:parse_multipart_post(A2),
    2 = length(Res1),
    {"abc123", HeadParams1} = proplists:get_value(head, Res1),
    "sometext\n" = proplists:get_value(body, Res1),
    2 = length(HeadParams1),
    "abc123" = proplists:get_value(filename, HeadParams1),
    "abc123" = proplists:get_value(name, HeadParams1),
    2 = length(Res2),
    {"ghi789", HeadParams2} = proplists:get_value(head, Res2),
    "sometext\n" = proplists:get_value(body, Res2),
    2 = length(HeadParams1),
    "ghi789" = proplists:get_value(filename, HeadParams2),
    "ghi789" = proplists:get_value(name, HeadParams2),
    ok.

read_multipart_form_test() ->
    {done, Dict} = yaws_multipart:read_multipart_form(mk_arg(data_to_parse()),
                                                      [no_temp_file]),
    {ok, Params} = dict:find("abc123", Dict),
    "abc123" = proplists:get_value(filename, Params),
    "sometext\n" = proplists:get_value(value, Params),
    "text/plain" = proplists:get_value(content_type, Params),
    "sampledata" = proplists:get_value("test-header", Params).

mk_arg(Data) ->
    ContentType = "multipart/form-data; boundary=!!!",
    Req = #http_request{method = 'POST'},
    Headers = #headers{content_type = ContentType},
    #arg{headers = Headers, req = Req, clidata = Data}.
