-module(ymnesia_SUITE).

-include("testsuite.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, blank_filters/1]).

all() ->
    [blank_filters].

init_per_suite(Config) ->
    case code:which(mnesia) of
        non_existing ->
            {skip, "Mnesia is not available"};
        _ ->
            Source = filename:join([?top_srcdir, "contrib", "ymnesia.erl"]),
            {ok, ymnesia, Binary} = compile:file(Source, [binary]),
            {module, ymnesia} = code:load_binary(ymnesia, Source, Binary),
            {ok, Apps} = application:ensure_all_started(mnesia),
            [{started_apps, Apps} | Config]
    end.

end_per_suite(Config) ->
    lists:foreach(fun application:stop/1, lists:reverse(?config(started_apps, Config))),
    code:delete(ymnesia),
    code:purge(ymnesia),
    ok.

blank_filters(_Config) ->
    {atomic, ok} = mnesia:create_table(ymnesia_test, [{attributes, [id, name]}]),
    try
        ok = mnesia:dirty_write({ymnesia_test, 1, alice}),
        ok = mnesia:dirty_write({ymnesia_test, 2, bob}),
        ?assertEqual([["1", "alice"], ["2", "bob"]],
                     table_rows(<<"tablename=ymnesia_test&id=&name=">>)),
        ?assertEqual([["1", "alice"]],
                     table_rows(<<"tablename=ymnesia_test&id=1&name=">>))
    after
        mnesia:delete_table(ymnesia_test)
    end.

table_rows(FormData) ->
    erase(post_parse),
    Arg = #arg{req = #http_request{method = 'POST'},
               headers = #headers{content_type = "application/x-www-form-urlencoded"},
               appmoddata = "table.yaws",
               clidata = FormData},
    {ehtml, [_, {body, _, [_, _, {'div', _, [{table, _, [_ | Rows]}]}]}]} =
        ymnesia:out(Arg),
    [[Cell || {td, _, Cell} <- Cells] || {tr, _, Cells} <- Rows].
