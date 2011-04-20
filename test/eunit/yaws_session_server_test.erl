-module (yaws_session_server_test).

-include_lib("eunit/include/eunit.hrl").

-export ([mock_session_server/0]).

%% Our test backend exports
-export ([init_backend/1, stop_backend/0,
          insert/1, list/0, lookup/1, delete/1,
          traverse/1, cleanup/0]).

%% Change this macro to test another backend storage module
%%-define(BACKEND, ?MODULE).
-define(BACKEND, yaws_session_server).

start() ->
    application:load(yaws),
    Result = gen_server:start({local, yaws_session_server},
                              yaws_session_server, ?BACKEND, []),
    ?BACKEND:cleanup(),
    Result.

init_test() ->
    {ok, _} = start(),
    ?assertEqual([], ?BACKEND:list()),
    yaws_session_server:stop().

new_session_and_list_test() ->
    {ok, _} = start(),
    ?assertMatch([], ?BACKEND:list()),
    Cookie1 = yaws_session_server:new_session({opaque, 1}),
    Cookie2 = yaws_session_server:new_session({opaque, 2}, 1, self()),
    Cookie3 = yaws_session_server:new_session({opaque, 3}, 1, self(), "cookie"),
    ?assert(Cookie1 /= Cookie2),
    "nonode@nohost-" ++ _ = Cookie1,
    "nonode@nohost-" ++ _ = Cookie2,
    "cookie" = Cookie3,
    ?assertMatch([_, _, _], ?BACKEND:list()),
    yaws_session_server:stop().

cookieval_to_opaque_test() ->
    {ok, _} = start(),
    Opaque = {opaque, 1},
    Cookie1 = yaws_session_server:new_session(Opaque),
    [Session] = ?BACKEND:list(),
    Opaque2 = {opaque, 2},
    true = yaws_session_server:replace_session(Cookie1, Opaque2),
    [Session_updated] = ?BACKEND:list(),
    ?assert(Session /= Session_updated),
    yaws_session_server:stop().

replace_session_test() ->
    {ok, _} = start(),
    Opaque = {opaque, 1},
    Cookie1 = yaws_session_server:new_session(Opaque),
    [Session] = ?BACKEND:list(),
    timer:sleep(1000), %% ensure cookie TS is updated of at least 1 second
    {ok, Result} = yaws_session_server:cookieval_to_opaque(Cookie1),
    ?assertEqual(Opaque, Result),
    [Session_updated] = ?BACKEND:list(),
    ?assert(Session /= Session_updated),
    yaws_session_server:stop().

delete_session_test() ->
    {ok, _} = start(),
    Opaque = {opaque, 1},
    Cookie1 = yaws_session_server:new_session(Opaque),
    Cookie2 = yaws_session_server:new_session(Opaque, 10, self()),
    [_, _] = ?BACKEND:list(),
    Expected1 = nocleanup,
    ?assertEqual(Expected1, yaws_session_server:delete_session(Cookie1)),
    Delete_notif = {yaws_session_end,normal,Cookie2,Opaque},
    ?assertEqual(Delete_notif, yaws_session_server:delete_session(Cookie2)),
    [] = ?BACKEND:list(),
    receive Delete_notif -> ok
    after 500 -> exit (cleanup_timeout)
    end,
    yaws_session_server:stop().

timeout_test () ->
    {ok, Pid} = start(),
    Opaque = {opaque, 1},
    Cookie1 = yaws_session_server:new_session(Opaque, 0, self()),
    Opaque2 = {opaque, 1},
    _Cookie2 = yaws_session_server:new_session(Opaque2, 10, self()),
    ?assertMatch([_, _], ?BACKEND:list()),
    Pid ! timeout,
    Timeout_notif = {yaws_session_end,timeout,Cookie1,Opaque},
    receive Timeout_notif -> ok
    after 500 -> exit (cleanup_timeout)
    end,
    ?assertMatch([_], ?BACKEND:list()),
    yaws_session_server:stop().

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




