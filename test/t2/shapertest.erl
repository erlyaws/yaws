-module(shapertest).
-behaviour(yaws_shaper).

-export([
         check/1,
         update/3
        ]).

-include("../../include/yaws.hrl").
-include("../../include/yaws_api.hrl").

-define(SHAPER_DB, shaper_db).

check(Ip) ->
    try_create(),
    case ets:lookup(?SHAPER_DB, Ip) of
        [{Ip, NbHit}] when NbHit > 2 ->
            {deny, 503, "Request quota exceeded"};
        _ ->
            allow
    end.

update(Ip, Hits, _Bytes) ->
    try_create(),
    case ets:member(?SHAPER_DB, Ip) of
        true  -> ets:update_counter(?SHAPER_DB, Ip, Hits);
        false -> ets:insert(?SHAPER_DB, {Ip, Hits})
    end.




try_create() ->
    case ets:info(?SHAPER_DB) of
        undefined ->
            ets:new(?SHAPER_DB, [set, public, named_table,
                                 {heir, whereis(yaws_server), []}]);
        _ ->
            ok
    end.
