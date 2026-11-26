-module(maxconn_SUITE).

-include("testsuite.hrl").
-include_lib("kernel/include/file.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [
     max_connections,
     max_connections_after_reconfig
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

%% Reconfiguring a running server (here a soft setconf, i.e. update_sconf)
%% must not leak connections. Two sequential requests leave a
%% reused acceptor as the current one (Last), which is the one stop_ready/2
%% has to terminate on reconfiguration. If it survives as an unlinked
%% acceptor, the connection it serves afterwards is never decremented and,
%% with max_connections = 2, the server refuses all connections after a few
%% reconfigurations.
max_connections_after_reconfig(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url  = testsuite:make_url(http, "127.0.0.1", Port, "/"),
    ConnHdr = {"Connection", "close"},
    {ok, GC, Groups0} = yaws_api:getconf(),

    lists:foreach(
      fun(I) ->
              ?assertMatch({ok, {{_,204,_}, _, _}},
                           testsuite:http_get(Url, [ConnHdr])),
              ?assertMatch({ok, {{_,204,_}, _, _}},
                           testsuite:http_get(Url, [ConnHdr])),
              %% A changed opaque value is enough for a soft reconfiguration
              %% (update_sconf) of the running server.
              Groups = [[SC#sconf{opaque = [{reconfig, I}]} || SC <- G]
                        || G <- Groups0],
              ?assertEqual(ok, yaws_api:setconf(GC, Groups))
      end, lists:seq(1, 10)),

    ?assertEqual(ok, wait_for_connections(Port, 0, 50)),
    ?assertMatch({ok, {{_,204,_}, _, _}}, testsuite:http_get(Url, [ConnHdr])),

    ?assertEqual(ok, yaws_api:setconf(GC, Groups0)),
    ok.

wait_for_connections(Port, Expected, 0) ->
    {error, {connections, current_connections(Port), expected, Expected}};
wait_for_connections(Port, Expected, Retries) ->
    case current_connections(Port) of
        Expected -> ok;
        _ ->
            timer:sleep(100),
            wait_for_connections(Port, Expected, Retries - 1)
    end.

current_connections(Port) ->
    {_, Stats} = yaws_server:stats(),
    hd([Conns || {_, P, Conns, _, _} <- Stats, P == Port]).
