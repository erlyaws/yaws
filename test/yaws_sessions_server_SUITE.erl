-module(yaws_sessions_server_SUITE).

-include("testsuite.hrl").

-compile(export_all).

all() ->
    [
     {group, basic_tests},
     {group, cookiegen_tests}
    ].

groups() ->
    [
     {basic_tests, [], [init,
                        new_session_and_list,
                        replace_session,
                        replace_session_cleanup,
                        cookieval_to_opaque,
                        delete_session,
                        timeout]},
     {cookiegen_tests, [], [new_cookiegen_session]}
    ].

%%====================================================================
init_per_suite(Config) ->
    application:load(yaws),
    Config.

end_per_suite(_Config) ->
    application:unload(yaws),
    ok.

init_per_group(cookiegen_tests, Config) ->
    Id       = "testsuite-server",
    YConf    = filename:join(?tempdir(?MODULE), "yaws.conf"),
    YawsHome = ?tempdir(?MODULE),
    os:putenv("YAWSHOME", YawsHome),
    application:load(yaws),
    application:set_env(yaws, id,   Id),
    application:set_env(yaws, conf, YConf),
    ok = yaws:start(),
    {ok, GConf, SCs} = yaws_api:getconf(),
    yaws_api:setconf(yaws:gconf_ysession_cookiegen(GConf, ?MODULE), SCs),
    Config;
init_per_group(_Group, Config) ->
    Config.

end_per_group(cookiegen_tests, _Config) ->
    ok = application:stop(yaws);
end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Test, Config) ->
    Config.

end_per_testcase(_Test, _Config) ->
    ok.

%%====================================================================
%% Change this macro to test another backend storage module
%%-define(BACKEND, ?MODULE).
-define(BACKEND, yaws_session_server).

start() ->
    %% starting crypto is required for these tests to pass on R13
    {ok, Pid} = gen_server:start({local, yaws_session_server},
                                 yaws_session_server, ?BACKEND, []),
    ?BACKEND:cleanup(),
    {ok, Pid}.

init(_Config) ->
    {ok, _} = start(),
    ?assertEqual([], ?BACKEND:list()),
    yaws_session_server:stop(),
    ok.

new_session_and_list(_Config) ->
    {ok, _} = start(),
    ?assertMatch([], ?BACKEND:list()),
    Cookie1 = yaws_session_server:new_session({opaque, 1}),
    Cookie2 = yaws_session_server:new_session({opaque, 2}, 1, self()),
    Cookie3 = yaws_session_server:new_session({opaque, 3}, 1, self(), "cookie"),
    ?assert(Cookie1 /= Cookie2),
    CPrefix = atom_to_list(node()) ++ "-",
    ?assert(lists:prefix(CPrefix, Cookie1)),
    ?assert(lists:prefix(CPrefix, Cookie2)),
    ?assertEqual("cookie", Cookie3),
    ?assertMatch([_, _, _], ?BACKEND:list()),
    yaws_session_server:stop(),
    ok.

replace_session(_Config) ->
    {ok, _} = start(),
    Opaque = {opaque, 1},
    Cookie1 = yaws_session_server:new_session(Opaque),
    [Session] = ?BACKEND:list(),
    Opaque2 = {opaque, 2},
    ?assert(yaws_session_server:replace_session(Cookie1, Opaque2)),
    [Session_updated] = ?BACKEND:list(),
    ?assert(Session /= Session_updated),
    yaws_session_server:stop(),
    ok.

replace_session_cleanup(_Config) ->
    {ok, _} = start(),
    Parent = self(),
    Cleanup1 = spawn(fun() ->
                             receive
                                 stop -> ok;
                                 Msg  -> Parent ! {cleanup1, Msg}
                             end
                     end),
    Opaque = {opaque, 1},
    Cookie = yaws_session_server:new_session(Opaque, 60, Cleanup1),
    Cleanup2 = spawn(fun() ->
                             receive
                                 stop -> ok;
                                 Msg  -> Parent ! {cleanup2, Msg}
                             end
                     end),
    Opaque2 = {opaque, 2},
    ?assert(yaws_session_server:replace_session(Cookie, Opaque2, Cleanup2)),
    ?assert(is_process_alive(Cleanup1)),
    ?assert(is_process_alive(Cleanup2)),
    yaws_session_server:delete_session(Cookie),
    Res = receive
              {cleanup2, {yaws_session_end, normal, _, Opaque2}} ->
                  ?assertNot(is_process_alive(Cleanup2)),
                  ok
          after
              5000 ->
                  {error, cleanup_failure}
          end,
    ?assertEqual(ok, Res),
    ?assert(is_process_alive(Cleanup1)),
    Cleanup1 ! stop,
    yaws_session_server:stop(),
    ok.

cookieval_to_opaque(_Config) ->
    {ok, _} = start(),
    Opaque = {opaque, 1},
    Cookie1 = yaws_session_server:new_session(Opaque),
    [Session] = ?BACKEND:list(),
    timer:sleep(1000), %% ensure cookie TS is updated of at least 1 second
    {ok, Result} = yaws_session_server:cookieval_to_opaque(Cookie1),
    ?assertEqual(Opaque, Result),
    [Session_updated] = ?BACKEND:list(),
    ?assert(Session /= Session_updated),
    yaws_session_server:stop(),
    ok.

delete_session(_Config) ->
    {ok, _} = start(),
    Opaque = {opaque, 1},
    Cookie1 = yaws_session_server:new_session(Opaque),
    Cookie2 = yaws_session_server:new_session(Opaque, 10, self()),
    ?assertMatch([_, _], ?BACKEND:list()),
    Expected1 = nocleanup,
    ?assertEqual(Expected1, yaws_session_server:delete_session(Cookie1)),
    Delete_notif = {yaws_session_end, normal, Cookie2, Opaque},
    ?assertEqual(Delete_notif, yaws_session_server:delete_session(Cookie2)),
    ?assertEqual([], ?BACKEND:list()),
    Res = receive
              Delete_notif -> ok
          after
              500 -> cleanup_timeout
          end,
    ?assertEqual(ok, Res),
    yaws_session_server:stop(),
    ok.

timeout(_Config) ->
    {ok, Pid} = start(),
    Opaque = {opaque, 1},
    Cookie1 = yaws_session_server:new_session(Opaque, 0, self()),
    Opaque2 = {opaque, 1},
    _Cookie2 = yaws_session_server:new_session(Opaque2, 10, self()),
    ?assertMatch([_, _], ?BACKEND:list()),
    Pid ! timeout,
    Timeout_notif = {yaws_session_end,timeout, Cookie1, Opaque},
    Res =
        receive
            Timeout_notif -> ok
        after
            500 -> cleanup_timeout
        end,
    ?assertEqual(ok, Res),
    ?assertMatch([_], ?BACKEND:list()),
    yaws_session_server:stop(),
    ok.

%% Our test callbacks
init_backend(_) ->
    proc_lib:start (?MODULE, mock_session_server, []),
    ok.

insert(Session) ->
    ?MODULE ! {insert, Session},
    true.

lookup(Cookie) ->
    ?MODULE ! {{lookup, Cookie}, self()},
    receive {ok, Opaque} -> [Opaque]
    after 1000 -> list_timeout
    end.

list() ->
    ?MODULE ! {list, self()},
    receive {ok, Sessions} -> Sessions
    after 1000 -> list_timeout
    end.

delete(Cookie) ->
    ?MODULE ! {delete, Cookie},
    true.

cleanup() ->
    Cleaner =
        fun(Session) ->
                Cookie = yaws_session_server:cookie(Session),
                delete(Cookie)
        end,
    lists:foreach(Cleaner, list()).

traverse(Gnow) ->
    Timeouter =
        fun (Session) ->
                case yaws_session_server:has_timedout(Session, Gnow) of
                    false ->
                        ok;
                    true ->
                        yaws_session_server:report_timedout_sess(Session),
                        Cookie = yaws_session_server:cookie(Session),
                        delete(Cookie)
                end
        end,
    lists:foreach(Timeouter, list()).

stop_backend() ->
    ?MODULE ! stop,
    ok.

%% Mock Internals
mock_session_server() ->
    register(?MODULE, self()),
    proc_lib:init_ack({ok, self()}),
    mock_loop().

mock_loop() ->
    receive
        {list, Pid} ->
            Pid ! {ok, sessions()},
            mock_loop();
        {insert, Session} ->
            Cookie = element(2, Session),
            put({session, Cookie}, Session),
            mock_loop();
        {{lookup, Cookie}, Pid} ->
            Session = get({session, Cookie}),
            Pid ! {ok, Session},
            mock_loop();
        {delete, Cookie} ->
            erase({session, Cookie}),
            mock_loop();
        stop ->
            ok;
        Other ->
            ?debugFmt("Unexpected: ~p~n", [Other])
    end.

sessions() ->
    lists:foldl(fun({{session, _}, Session}, Acc) -> [Session | Acc];
                    (_, Acc) -> Acc
                 end, [], get()).

%%====================================================================

-define(SPECIAL_COOKIE, "my special cookie").
new_cookie() ->
    ?SPECIAL_COOKIE.

new_cookiegen_session(_Config) ->
    Cookie1 = yaws_session_server:new_session({opaque, 1}),
    Cookie2 = yaws_session_server:new_session({opaque, 2}, 1, self()),
    Cookie3 = yaws_session_server:new_session({opaque, 3}, 1, self(), "cookie"),
    ?assertEqual(?SPECIAL_COOKIE, Cookie1),
    ?assertEqual(Cookie1, Cookie2),
    ?assertEqual("cookie", Cookie3),
    ok.
