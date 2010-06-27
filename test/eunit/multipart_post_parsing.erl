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
    {result,[{head,{"abc123", [{filename,"abc123"},{name,"abc123"},
                               {content_type,"text/plain"},
                               {"test-header","sampledata"}]}},
             {body,"sometext\n"}]} = complete_parse().


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
    {[{head,{"abc123",[{filename,"abc123"},{name,"abc123"}]}},
      {part_body,"some"}],
     [{body,"text\n"},
      {head,{"def456",[{filename,"def456"},{name,"def456"}]}},
      {body,"sometext\n"}]} = {Res1, Res2}.

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
    {[{head,{"abc123",[{filename,"abc123"},{name,"abc123"}]}},
      {body,"sometext\n"}],
     [{head,{"ghi789",[{filename,"ghi789"},{name,"ghi789"}]}},
      {body,"sometext\n"}]} = {Res1, Res2}.

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
