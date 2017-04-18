-module(host_header_SUITE).

-include("testsuite.hrl").
-include_lib("kernel/include/file.hrl").

-export([
    missing_host_header/1,
    multiple_host_headers/1,
    wrong_host_header/1
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
     missing_host_header,
     multiple_host_headers,
     wrong_host_header
    ].

groups() ->
    [
    ].

%%====================================================================
init_per_suite(Config) ->
    Id    = "testsuite-server",
    YConf = filename:join(?tempdir(?MODULE), "yaws.conf"),
    application:load(yaws),
    application:set_env(yaws, id,   Id),
    application:set_env(yaws, conf, YConf),
    ok = yaws:start(),
    [{yaws_id, Id}, {yaws_config, YConf} | Config].

end_per_suite(_Config) ->
    ok = application:stop(yaws),
    ok = application:unload(yaws),
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
missing_host_header(Config) ->
    Port = testsuite:get_yaws_port(1, Config),

    {ok, S} = gen_tcp:connect("127.0.0.1", Port, [binary, {active,false}]),
    ?assertEqual(ok, testsuite:send_http_request(S, {get, "/", "HTTP/1.1"}, [])),
    ?assertMatch({ok, {{_,400,_}, _, _}}, testsuite:receive_http_response(S)),
    ?assertEqual(ok, gen_tcp:close(S)),
    ok.

multiple_host_headers(Config) ->
    Port = testsuite:get_yaws_port(1, Config),

    {ok, S} = gen_tcp:connect("127.0.0.1", Port, [binary, {active,false}]),
    ?assertEqual(ok,
                 testsuite:send_http_request(S, {get, "/", "HTTP/1.1"},
                                             [{"Host", "localhost"},
                                              {"Host", "foo"}])),
    ?assertMatch({ok, {{_,400,_}, _, _}}, testsuite:receive_http_response(S)),
    ?assertEqual(ok, gen_tcp:close(S)),
    ok.

wrong_host_header(Config) ->
    Port = testsuite:get_yaws_port(1, Config),

    {ok, S} = gen_tcp:connect("127.0.0.1", Port, [binary, {active,false}]),
    ?assertEqual(ok,
                 testsuite:send_http_request(S, {get, "/", "HTTP/1.1"},
                                             [{"Host", "foo"}])),
    ?assertMatch({ok, {{_,400,_}, _, _}}, testsuite:receive_http_response(S)),
    ?assertEqual(ok, gen_tcp:close(S)),
    ok.
