-module(yaws_compile_SUITE).

-include("testsuite.hrl").

-compile(export_all).

all() ->
    [
     {group, compile_tests},
     {group, request_tests}
    ].

groups() ->
    [
     {compile_tests, [], [compile_www_scripts,
                          compile_erl_tag,
                          compile_verbatim_tag,
                          compile_bindings,
                          compile_unknown,
                          compile_empty_erl,
                          compile_empty_verbatim,
                          compile_bad_yaws_dir,
                          compile_not_out_fun,
                          compile_compilation_error,
                          compile_bad_module_name,
                          compile_erl_comments]},
     {request_tests, [], [request_www_scripts,
                          request_erl_tag,
                          request_verbatim_tag,
                          request_bindings,
                          request_empty_erl,
                          request_empty_verbatim,
                          request_bad_yaws_dir,
                          request_not_out_fun,
                          request_compilation_error,
                          request_bad_module_name,
                          request_erl_comments]}
    ].

%%====================================================================
init_per_suite(Config) ->
    Id       = "testsuite-server",
    YConf    = filename:join(?tempdir(?MODULE), "yaws.conf"),
    YawsHome = ?tempdir(?MODULE),
    os:putenv("YAWSHOME", YawsHome),
    application:load(yaws),
    application:set_env(yaws, id,   Id),
    application:set_env(yaws, conf, YConf),
    ok = yaws:start(),
    [{yaws_id, Id}, {yaws_config, YConf}|Config].

end_per_suite(_Config) ->
    ok = application:stop(yaws),
    ok = application:unload(yaws),
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(compile_bad_yaws_dir, Config) ->
    os:putenv("YAWSHOME", "/unknown"),
    Config;
init_per_testcase(request_bad_yaws_dir, Config) ->
    os:putenv("YAWSHOME", "/unknown"),
    Config;
init_per_testcase(_Test, Config) ->
    Config.

end_per_testcase(compile_bad_yaws_dir, _Config) ->
    YawsHome = ?tempdir(?MODULE),
    os:putenv("YAWSHOME", YawsHome),
    ok;
end_per_testcase(request_bad_yaws_dir, _Config) ->
    YawsHome = ?tempdir(?MODULE),
    os:putenv("YAWSHOME", YawsHome),
    ok;
end_per_testcase(_Test, _Config) ->
    ok.

%%====================================================================
compile_www_scripts(Config) ->
    [begin
         ?assertMatch({ok, 0, _}, compile_script(Config, S))
     end || S <- get_scripts()],
    ok.

compile_erl_tag(Config) ->
    ?assertMatch({ok, 0, _}, compile_script(Config, "erl_tag.yaws")),
    ok.

compile_verbatim_tag(Config) ->
    ?assertMatch({ok, 0, _}, compile_script(Config, "verbatim_tag.yaws")),
    ok.

compile_bindings(Config) ->
    ?assertMatch({ok, 0, _}, compile_script(Config, "var_bindings.yaws")),
    ok.

compile_unknown(Config) ->
    ?assertMatch({ok, 1, [{error, 0, _}]}, compile_script(Config, "no_script.yaws")),
    ok.

compile_empty_erl(Config) ->
    ?assertMatch({ok, 0, [{data, _}]}, compile_script(Config, "empty_erl.yaws")),
    ok.

compile_empty_verbatim(Config) ->
    ?assertMatch({ok, 0, [{data, _}]}, compile_script(Config, "empty_verbatim.yaws")),
    ok.

compile_bad_yaws_dir(Config) ->
    ?assertMatch({ok, 1, [{error, _, _}|_]}, compile_script(Config, "hello.yaws")),
    ok.

compile_not_out_fun(Config) ->
    ?assertMatch({ok, 1, [{error, _, _}|_]}, compile_script(Config, "no_out_fun1.yaws")),
    ?assertMatch({ok, 1, [{error, _, _}|_]}, compile_script(Config, "no_out_fun2.yaws")),
    ok.

compile_compilation_error(Config) ->
    ?assertMatch({ok, 1, [{error, _, _}|_]}, compile_script(Config, "compilation_error.yaws")),
    ok.

compile_bad_module_name(Config) ->
    ?assertMatch({ok, 0, _}, compile_script(Config, "hello.yaws")),
    ?assertMatch({ok, 1, [{error, _, _}|_]}, compile_script(Config, "bad_module1.yaws")),
    ?assertMatch({ok, 1, [{error, _, _}|_]}, compile_script(Config, "bad_module2.yaws")),
    ?assertMatch({ok, 1, [{error, _, _}|_]}, compile_script(Config, "bad_module3.yaws")),
    ok.

compile_erl_comments(Config) ->
    ?assertMatch({ok, 0, _}, compile_script(Config, "comments.yaws")),
    ok.

request_www_scripts(Config) ->
    [begin
         Path = "/"++filename:basename(S),
         ?assertMatch({ok, {{_,200,_}, _, _}}, request_script(Config, Path))
     end || S <- get_scripts()],
    ok.

request_erl_tag(Config) ->
    Res = <<"erl_tag#test1\n\n",
            "erl_tag#test2\n\n",
            "erl_tag#test3\n\n",
            "<b>erl_tag#test4</b>\n\n",
            "\n", %% erl_tag_test5 module definition
            "erl_tag#test5\n\n",
            "erl_tag#test6\n\n",
            "\n", %% erl_tag_test7 module definition
            "erl_tag#test7\n\n",
            "&lt;erl&gt;&quot;erl_tag#test8&quot;&lt;/erl&gt;\n\n",
            "&lt;erl&gt;'erl_tag#test9'&lt;/erl&gt;\n\n",
            "erl_tag#test10">>,
    ?assertMatch({ok, {{_,200,_}, _, Res}}, request_script(Config, "/erl_tag.yaws")),
    ok.

request_verbatim_tag(Config) ->
    Res = <<"\n",
            "<pre>\n&lt;verbatim_tag#test1&gt;\n</pre>\n\n",
            "<pre>&lt;verbatim_tag#test2&gt;</pre>\n\n",
            "<pre>&lt;verbatim_tag#test3&gt;\n</pre>\n\n",
            "<b><pre>\n&lt;verbatim_tag#test4&gt;\n</pre></b>\n\n",
            "<pre \n  >\n&lt;verbatim_tag#test5&gt;\n</pre>\n\n",
            "<pre \n  class = \"code\" id=\"test6\" >\n&lt;verbatim_tag#test6&gt;\n</pre>\n">>,
    ?assertMatch({ok, {{_,200,_}, _, Res}}, request_script(Config, "/verbatim_tag.yaws")),
    ok.

request_bindings(Config) ->
    Res = <<"\n\n",
            "%%%%<b>binding_value</b>%%%%\n\n",
            "%% %%not_a_binding %%\n">>,
    ?assertMatch({ok, {{_,200,_}, _, Res}}, request_script(Config, "/var_bindings.yaws")),
    ok.

request_empty_erl(Config) ->
    Res = <<"Empty erl: <erl/> <erl />\n">>,
    ?assertMatch({ok, {{_,200,_}, _, Res}}, request_script(Config, "/empty_erl.yaws")),
    ok.

request_empty_verbatim(Config) ->
    Res = <<"Empty verbatim: <verbatim/> <verbatim />\n">>,
    ?assertMatch({ok, {{_,200,_}, _, Res}}, request_script(Config, "/empty_verbatim.yaws")),
    ok.

request_bad_yaws_dir(Config) ->
    {ok, {{_,200,_}, _, Error}} = request_script(Config, "/hello.yaws"),
    ?assertEqual(match, re:run(Error, <<"Failed to create temp file">>,
                               [caseless, {capture, none}])),
    ok.

request_not_out_fun(Config) ->
    {ok, {{_,200,_}, _, Error1}} = request_script(Config, "/no_out_fun1.yaws"),
    {ok, {{_,200,_}, _, Error2}} = request_script(Config, "/no_out_fun2.yaws"),

    ?assertEqual(match, re:run(Error1, <<"Dynamic compile error">>,
                               [caseless, {capture, none}])),
    ?assertEqual(match, re:run(Error1, <<"function out/1 undefined">>,
                               [caseless, {capture, none}])),

    ?assertEqual(match, re:run(Error2, <<"Dynamic compile error">>,
                               [caseless, {capture, none}])),
    ?assertEqual(match, re:run(Error2, <<"function out/1 undefined">>,
                               [caseless, {capture, none}])),
    ok.

request_compilation_error(Config) ->
    {ok, {{_,200,_}, _, Error}} = request_script(Config, "/compilation_error.yaws"),
    ?assertEqual(match, re:run(Error, <<"Dynamic compile error">>,
                               [caseless, {capture, none}])),
    ?assertEqual(match, re:run(Error, <<"function function_not_found/0 undefined">>,
                               [caseless, {capture, none}])),
    ok.

request_bad_module_name(Config) ->
    ?assertMatch({ok, {{_,200,_}, _, _}}, request_script(Config, "/hello.yaws")),
    {ok, {{_,200,_}, _, Error1}} = request_script(Config, "/bad_module1.yaws"),
    {ok, {{_,200,_}, _, Error2}} = request_script(Config, "/bad_module2.yaws"),
    {ok, {{_,200,_}, _, Error3}} = request_script(Config, "/bad_module3.yaws"),
    ?assertEqual(match, re:run(Error1, <<"Cannot create generated module 'yaws'">>,
                               [caseless, {capture, none}])),
    ?assertEqual(match, re:run(Error1, <<"try to override existing module">>,
                               [caseless, {capture, none}])),

    ?assertEqual(match, re:run(Error2, <<"Cannot create generated module 'hello'">>,
                               [caseless, {capture, none}])),
    ?assertEqual(match, re:run(Error2, <<"try to override generated module owned by script">>,
                               [caseless, {capture, none}])),

    ?assertEqual(match, re:run(Error3, <<"Cannot create generated module ''">>,
                               [caseless, {capture, none}])),
    ?assertEqual(match, re:run(Error3, <<"empty module name">>,
                               [caseless, {capture, none}])),
    ok.

request_erl_comments(Config) ->
    Res = <<"Hello World! % this is not a comment %\n">>,
    ?assertMatch({ok, {{_,200,_}, _, Res}}, request_script(Config, "/comments.yaws")),
    ok.


%%====================================================================
compile_script(_Config, File) ->
    {ok, GC, _} = yaws_server:getconf(),
    put(gc, GC),
    Script = filename:join([?data_srcdir(?MODULE), File]),
    yaws_compile:compile_file(Script).

request_script(Config, Path) ->
    Port = testsuite:get_yaws_port(1, Config),
    {ok, GC, _} = yaws_server:getconf(),
    put(gc, GC),
    Url = testsuite:make_url(http, "127.0.0.1", Port, Path),
    testsuite:http_get(Url).

get_scripts() ->
    TabFile = filename:join([?wwwdir, "TAB.inc"]),
    {ok, Content} = file:read_file(TabFile),
    {match, Scripts} = re:run(Content, "href=\"([^\"]+\\.yaws)\"",
                              [global, {capture, all_but_first, list}]),
    [case S of
         [$/|_] -> ?wwwdir ++ S;
         _      -> filename:join([?wwwdir, S])
     end || [S] <- Scripts].
