-module(rss_startup_SUITE).

-include("testsuite.hrl").

-export([all/0, suite/0, rss_startup_and_restart/1]).
-export([init_backend/1, stop_backend/0, insert/1, lookup/1]).

all() ->
    [rss_startup_and_restart].

suite() ->
    [{timetrap, {seconds, 15}}].

rss_startup_and_restart(Config) ->
    Dir = ?config(priv_dir, Config),
    Conf = write_config(Dir),
    OldHome = os:getenv("YAWSHOME"),
    ok = application:load(yaws),
    try
        true = os:putenv("YAWSHOME", Dir),
        ok = application:set_env(yaws, conf, Conf),
        ok = application:set_env(yaws, embedded, false),
        ok = application:set_env(yaws, id, atom_to_list(?MODULE)),
        {ok, _} = application:ensure_all_started(yaws),

        {ok, _, [[SC]]} = yaws_api:getconf(),
        Port = yaws_api:get_listen_port(SC),
        ?assert(Port > 0 andalso is_integer(Port)),
        assert_http_response(Port),

        ok = yaws_rss:insert(test_feed, news, "Startup item", "/item", "RSS"),
        {ok, Items} = yaws_rss:retrieve(test_feed, news),
        ?assertMatch({_, _}, binary:match(iolist_to_binary(Items),
                                          <<"<title>Startup item</title>">>)),

        Server = whereis(yaws_server),
        Sessions = whereis(yaws_session_server),
        ?assert(is_pid(Sessions)),
        ?assertEqual(Sessions, ets:info(?MODULE, owner)),
        Cookie = yaws_api:new_cookie_session(startup_session),
        ?assertEqual({ok, startup_session}, yaws_api:cookieval_to_opaque(Cookie)),

        RSS = whereis(yaws_rss),
        Ref = erlang:monitor(process, RSS),
        exit(RSS, kill),
        receive
            {'DOWN', Ref, process, RSS, killed} -> ok
        after 2000 ->
            ct:fail(rss_worker_did_not_exit)
        end,
        NewRSS = wait_for_rss_restart(RSS, 100),
        _ = sys:get_state(NewRSS, 2000),
        ?assertEqual(Server, whereis(yaws_server)),
        ?assertEqual(Sessions, whereis(yaws_session_server)),
        ?assertEqual({ok, startup_session}, yaws_api:cookieval_to_opaque(Cookie)),
        assert_http_response(Port)
    after
        application:stop(yaws),
        application:unload(yaws),
        case OldHome of
            false -> os:unsetenv("YAWSHOME");
            _ -> os:putenv("YAWSHOME", OldHome)
        end
    end.

write_config(Dir) ->
    Docroot = filename:join(Dir, "www"),
    Index = filename:join(Docroot, "index.html"),
    ok = filelib:ensure_dir(Index),
    ok = file:write_file(Index, <<"Yaws is ready">>),
    RSSDir = filename:join(Dir, "rss"),
    DBFile = filename:join(RSSDir, "yaws_rss.dets"),
    ok = filelib:ensure_dir(DBFile),
    {ok, DB} = dets:open_file(rss_startup_fixture, [{file, DBFile}]),
    try
        ok = dets:insert(DB, {counter, 0})
    after
        ok = dets:close(DB)
    end,
    Conf = filename:join(Dir, "yaws.conf"),
    ok = file:write_file(Conf, io_lib:format(
           "logdir = ~s~nysession_mod = ~s~n"
           "<server localhost>~n"
           "    port = 0~n    listen = 127.0.0.1~n    docroot = ~s~n"
           "    <rss>~n        rss_id = test_feed~n        rss_dir = ~s~n"
           "    </rss>~n</server>~n", [Dir, ?MODULE, Docroot, RSSDir])),
    Conf.

assert_http_response(Port) ->
    {ok, Sock} = gen_tcp:connect({127, 0, 0, 1}, Port,
                                 [binary, {active, false}], 2000),
    try
        ok = testsuite:send_http_request(Sock, {get, "/index.html", "HTTP/1.1"},
                                         [{"Host", "localhost"}]),
        {ok, {{_, 200, _}, _, <<"Yaws is ready">>}} =
            testsuite:receive_http_response(get, Sock, 2000),
        ok
    after
        gen_tcp:close(Sock)
    end.

wait_for_rss_restart(_OldRSS, 0) ->
    ct:fail(rss_worker_did_not_restart);
wait_for_rss_restart(OldRSS, Attempts) ->
    case whereis(yaws_rss) of
        Pid when is_pid(Pid), Pid =/= OldRSS ->
            Pid;
        _ ->
            timer:sleep(20),
            wait_for_rss_restart(OldRSS, Attempts - 1)
    end.

init_backend([cookie | _]) ->
    ets:new(?MODULE, [named_table, {keypos, 2}]).

stop_backend() ->
    ets:delete(?MODULE).

insert(Session) ->
    ets:insert(?MODULE, Session).

lookup(Cookie) ->
    ets:lookup(?MODULE, Cookie).
