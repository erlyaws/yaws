-module(multipart_post_parsing_SUITE).

-include("testsuite.hrl").

-compile(export_all).

all() ->
    [
     complete_parse_list,
     complete_parse_binary,
     incomplete_body_list,
     incomplete_body_binary,
     incomplete_head_list,
     incomplete_head_binary,
     boundary_markers,
     incomplete_boundary_list,
     incomplete_boundary_binary,
     read_multipart_form_list,
     read_multipart_form_binary,
     malformed_multipart_form,
     escaped_parse,
     return_error_file_path
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
    ?assertEqual(2, length(Params)),
    {"abc123", HeadParams} = proplists:get_value(head, Params),
    ?assertEqual(4, length(HeadParams)),
    ?assertEqual("abc123", proplists:get_value("name", HeadParams)),
    ?assertEqual("abc123", proplists:get_value("filename", HeadParams)),
    ?assertEqual("text/plain", proplists:get_value(content_type, HeadParams)),
    ?assertEqual("sampledata", proplists:get_value("test-header", HeadParams)),
    proplists:get_value(body, Params).

complete_parse_list(_Config) ->
    ?assertEqual("sometext\n", test_complete_parse(list)),
    ok.

complete_parse_binary(_config) ->
    ?assertEqual(<<"sometext\n">>, test_complete_parse(binary)),
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
    ?assertEqual(2, length(Res1)),
    {"abc123", HeadParams1} = proplists:get_value(head, Res1),
    PB = proplists:get_value(part_body, Res1),
    ?assertEqual(2, length(HeadParams1)),
    ?assertEqual("abc123", proplists:get_value("filename", HeadParams1)),
    ?assertEqual("abc123", proplists:get_value("name", HeadParams1)),
    ?assertEqual(3, length(Res2)),
    {"def456", HeadParams2} = proplists:get_value(head, Res2),
    BL = lists:sort([B || {K, _}=B <- Res2, K =:= body]),
    ?assertEqual(2, length(HeadParams2)),
    ?assertEqual("def456", proplists:get_value("filename", HeadParams2)),
    ?assertEqual("def456", proplists:get_value("name", HeadParams2)),
    {PB, BL}.

incomplete_body_list(_config) ->
    ?assertEqual({"someinc", [{body, "ompletetext\n"}, {body, "sometext\n"}]},
                 test_incomplete_body(list)),
    ok.

incomplete_body_binary(_Config) ->
    ?assertEqual({<<"someinc">>, [{body, <<"ompletetext\n">>}, {body, <<"sometext\n">>}]},
                 test_incomplete_body(binary)),
    ok.

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
    ?assertEqual(2, length(Res1)),
    {"abc123", HeadParams1} = proplists:get_value(head, Res1),
    Body1 = proplists:get_value(body, Res1),
    ?assertEqual(2, length(HeadParams1)),
    ?assertEqual("abc123", proplists:get_value("filename", HeadParams1)),
    ?assertEqual("abc123", proplists:get_value("name", HeadParams1)),
    ?assertEqual(2, length(Res2)),
    {"ghi789", HeadParams2} = proplists:get_value(head, Res2),
    Body2 = proplists:get_value(body, Res2),
    ?assertEqual(2, length(HeadParams1)),
    ?assertEqual("ghi789", proplists:get_value("filename", HeadParams2)),
    ?assertEqual("ghi789", proplists:get_value("name", HeadParams2)),
    {Body1, Body2}.

incomplete_head_list(_Config) ->
    ?assertEqual({"sometext1\n", "sometext2\n"},
                 test_incomplete_head_list(list)),
    ok.

incomplete_head_binary(_Config) ->
    ?assertEqual({<<"sometext1\n">>, <<"sometext2\n">>},
                 test_incomplete_head_list(binary)),
    ok.

boundary_markers(_Config) ->
    Body = <<"------WebKitFormBoundaryUkx0KS47IKKfcF4z\r\n",
             "Content-Disposition: form-data; name=upfile; filename=test.txt\r\n",
             "Content-Type: text/plain\r\n",
             "\r\n",
             "Hello world\n",
             "\r\n",
             "------WebKitFormBoundaryUkx0KS47IKKfcF4z\r\n",
             "Content-Disposition: form-data; name=note\r\n",
             "\r\n",
             "test\r\n",
             "------WebKitFormBoundaryUkx0KS47IKKfcF4z--\r\n">>,
    Results = lists:foldl(fun (N, {Ok, Errors, Done}) ->
                                  <<BodyPart:N/binary, _/binary>> = Body,
                                  case boundary_marker_parse(BodyPart) of
                                      {cont, _, _} ->
                                          {Ok+1, Errors, Done};
                                      {result, _} ->
                                          {Ok+1, Errors, Done+1};
                                      _ ->
                                          {Ok, Errors+1, Done}
                                  end
                          end, {0, 0, 0}, lists:seq(1, size(Body))),
    ?assertMatch({_, 0, 1}, Results),
    ok.

boundary_marker_parse(Body) ->
    Arg = #arg{headers=#headers{content_type="multipart/form-data; boundary=----WebKitFormBoundaryUkx0KS47IKKfcF4z"},
               req=#http_request{method='POST'},
               clidata=Body},
    yaws_api:parse_multipart_post(Arg).

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
    ?assertMatch({result, []}, yaws_api:parse_multipart_post(A3)),
    ?assertEqual(2, length(Res1)),
    {"abc123", HeadParams1} = proplists:get_value(head, Res1),
    Body1 = proplists:get_value(body, Res1),
    ?assertEqual(2, length(HeadParams1)),
    ?assertEqual("abc123", proplists:get_value("filename", HeadParams1)),
    ?assertEqual("abc123", proplists:get_value("name", HeadParams1)),
    ?assertEqual(2, length(Res2)),
    {"ghi789", HeadParams2} = proplists:get_value(head, Res2),
    Body2 = proplists:get_value(body, Res2),
    ?assertEqual(2, length(HeadParams1)),
    ?assertEqual("ghi789", proplists:get_value("filename", HeadParams2)),
    ?assertEqual("ghi789", proplists:get_value("name", HeadParams2)),
    {Body1, Body2}.

incomplete_boundary_list(_Config) ->
    ?assertEqual({"sometext1\n", "sometext2\n"},
                 test_incomplete_boundary_list(list)),
    ok.

incomplete_boundary_binary(_Config) ->
    ?assertEqual({<<"sometext1\n">>, <<"sometext2\n">>},
                 test_incomplete_boundary_list(binary)),
    ok.

read_multipart_form_base(Opt) ->
    {done, Dict} = yaws_multipart:read_multipart_form(mk_arg(data_to_parse()),
                                                      [no_temp_file, Opt]),
    {ok, Params} = dict:find("abc123", Dict),
    ?assertEqual("abc123", proplists:get_value("filename", Params)),
    ?assertEqual("text/plain", proplists:get_value(content_type, Params)),
    ?assertEqual("sampledata", proplists:get_value("test-header", Params)),
    proplists:get_value(value, Params).

read_multipart_form_list(_Config) ->
    ?assertEqual("sometext\n", read_multipart_form_base(list)),
    ok.

read_multipart_form_binary(_Config) ->
    ?assertEqual(<<"sometext\n">>, read_multipart_form_base(binary)),
    ok.

malformed_multipart_form(_Config) ->
    Data1 = list_to_binary(
              ["--!!!\r\n",
               "Content-Disposition: form-data; name=\"abc123\"; "
               ++ "filename=\"abc123\"\r\n\r\n",
               "sometext\n\r\n--!!!Oops"]),
    A1 = mk_arg(Data1),
    ?assertEqual({error, malformed_multipart_post},
                 yaws_api:parse_multipart_post(A1)),
    Data2 = list_to_binary(
              ["--!!!\r\n",
               "Content-Disposition: form-data; name=\"abc123\"; "
               ++ "filename=\"abc123\"\r\n",
               "Invalid-Header\r\n\r\n"
               "sometext\n\r\n--!!!"]),
    A2 = mk_arg(Data2),
    ?assertEqual({error, malformed_multipart_post},
                 yaws_api:parse_multipart_post(A2)),
    Req   = #http_request{method = 'POST'},
    Hdrs1 = #headers{},
    Hdrs2 = #headers{content_type = "text/plain"},
    A3 = #arg{headers=Hdrs1, req=Req},
    ?assertEqual({error, no_content_type}, yaws_api:parse_multipart_post(A3)),
    A4 = #arg{headers=Hdrs2, req=Req},
    ?assertEqual({error, no_multipart_form_data}, yaws_api:parse_multipart_post(A4)),
    ok.

escaped_data_to_parse(Name) ->
    list_to_binary(
      ["--!!!\r\n",
       "Content-Disposition: form-data; name=\"" ++ Name ++ "\"\r\n\r\n"
       "sometext\n\r\n--!!!--\r\n"]).

get_unescaped_name(RawName) ->
    Data = escaped_data_to_parse(RawName),
    {result, Params} = yaws_api:parse_multipart_post(mk_arg(Data)),
    ?assertEqual(2, length(Params)),
    {Name, HeadParams} = proplists:get_value(head, Params),
    ?assertEqual([{"name", Name}], HeadParams),
    Name.

return_error_file_path(_)->
    Data = <<"Hello world12313123132133423421341dda2341234123412341241241223412421adsfsdfasdfaaaaaaaaa\n">>,
    Body = <<"------WebKitFormBoundaryUkx0KS47IKKfcF4z\r\n",
             "Content-Disposition: form-data; name=upfile; filename=test.txt\r\n",
             "\r\n",
	     Data/binary,
             "\r\n",
             "------WebKitFormBoundaryUkx0KS47IKKfcF4z\r\n",
             "Content-Disposition: form-data; name=note\r\n",
             "\r\n",
             "testing\r\n",
             "------WebKitFormBoundaryUkx0KS47IKKfcF4z--\r\n">>,
    Arg = #arg{headers=#headers{content_type="multipart/form-data; boundary=----WebKitFormBoundaryUkx0KS47IKKfcF4z"},
               req=#http_request{method='POST'}, clidata=Body},
    MaxFileSize = byte_size(Data) div 2,
    {error, Error} = yaws_multipart:read_multipart_form(Arg, [list, return_error_file_path,
							      {max_file_size, MaxFileSize}]),
    {max_file_size_exceeded, _, FilePath} = Error,
    FileSize = filelib:file_size(FilePath),
    ?assert(FileSize < MaxFileSize).

escaped_parse(_Config) ->
    %% Support both escaped (Firefox, Opera) and unescaped (Konqueror)
    %% quotation mark.
    ?assertEqual("a\"b", get_unescaped_name("a\\\"b")),
    ?assertEqual("a\"b", get_unescaped_name("a\"b")),
    %% Do not decode "%22" (IE, Chrome), user must deal with ambiguity
    %% himself.
    ?assertEqual("a%22b", get_unescaped_name("a%22b")),
    %% Support unescaped backslash (Firefox, Chrome, Konqueror, IE).
    ?assertEqual("a\\b", get_unescaped_name("a\\b")),
    ?assertEqual("a\\\\b", get_unescaped_name("a\\\\b")),
    %% Support backslash at the end of name (for simple form values).
    ?assertEqual("a\\", get_unescaped_name("a\\")),
    ok.

mk_arg(Data) ->
    ContentType = "multipart/form-data; boundary=!!!",
    Req = #http_request{method = 'POST'},
    Headers = #headers{content_type = ContentType},
    #arg{headers = Headers, req = Req, clidata = Data}.
