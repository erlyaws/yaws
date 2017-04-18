-module(maxconn_SUITE).

-include("testsuite.hrl").
-include_lib("kernel/include/file.hrl").

-export([
    max_connections/1
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
     max_connections
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
max_connections(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/"),
    ConnHdr = {"Connection", "close"},

    ?assertMatch({ok, {{_,204,_}, _, _}}, testsuite:http_get(Url, [ConnHdr])),

    {ok, Sock1} = gen_tcp:connect("127.0.0.1", Port, []),
    ?assertMatch({ok, {{_,204,_}, _, _}}, testsuite:http_get(Url, [ConnHdr])),

    {ok, Sock2} = gen_tcp:connect("127.0.0.1", Port, []),
    {ok, Sock3} = gen_tcp:connect("127.0.0.1", Port, [{active, once}]),
    Res = receive
              {tcp_closed, Sock3} -> ok
          after
              1000 -> {error, timeout}
          end,
    ?assertEqual(Res, ok),

    ?assertEqual(ok, gen_tcp:close(Sock1)),
    ?assertMatch({ok, {{_,204,_}, _, _}}, testsuite:http_get(Url, [ConnHdr])),

    ?assertEqual(ok, gen_tcp:close(Sock2)),
    ?assertMatch({ok, {{_,204,_}, _, _}}, testsuite:http_get(Url, [ConnHdr])),
    ok.
