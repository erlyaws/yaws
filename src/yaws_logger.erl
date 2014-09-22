%%%----------------------------------------------------------------------
%%% File    : yaws_logger.erl
%%% Author  : Christopher Faulet <christopher@yakaz.com>
%%% Purpose :
%%% Created : 14 Dec 2010 by Christopher Faulet <christopher@yakaz.com>
%%%----------------------------------------------------------------------

-module(yaws_logger).
-author('christopher@yakaz.com').
-include_lib("kernel/include/file.hrl").


-export([behaviour_info/1]).

%% API
-export([
         open_log/3,
         close_log/2,
         close_logs/0,
         rotate/1,

         accesslog/6,
         authlog/4
        ]).


-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").


-record(log, {id, amod, data}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{open_log,3}, {close_log,3}, {wrap_log,4}, {write_log,4}];
behaviour_info(_Other) ->
    undefined.


open_log(SConf, auth, Dir) when ?sc_has_auth_log(SConf) ->
    do_open_log(SConf, auth, Dir);
open_log(SConf, access, Dir) when ?sc_has_access_log(SConf) ->
    do_open_log(SConf, access, Dir);
open_log(_, _, _) ->
    false.


close_log(SConf, Type) ->
    case ets:lookup(yaws_log, {Type, SConf#sconf.servername}) of
        [AL] ->
            do_close_log(AL),
            ets:delete(yaws_log, {Type, SConf#sconf.servername}),
            ok;
        [] ->
            ok
    end.


close_logs() ->
    do_close_logs(ets:first(yaws_log)),
    ets:delete_all_objects(yaws_log),
    ok.


rotate(LogWrapSize) ->
    do_rotate(ets:first(yaws_log), LogWrapSize).


accesslog(#sconf{servername=Srv}, Ip, Req, InH, OutH, Time) ->
    case ets:lookup(yaws_log, {access, Srv}) of
        [#log{amod=Mod, data=Data}] ->
            catch Mod:write_log(Srv, access, Data, {Ip, Req, InH, OutH, Time});
        _ ->
            ok
    end.

authlog(#sconf{servername=Srv}, IP, Path, Item) ->
    case ets:lookup(yaws_log, {auth, Srv}) of
        [#log{amod=Mod, data=Data}] ->
            catch Mod:write_log(Srv, auth, Data, {IP, Path, Item});
        _ ->
            ok
    end.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

do_open_log(#sconf{servername=Srv, logger_mod=Mod}, Type, Dir) ->
    Id = {Type, Srv},
    case ets:lookup(yaws_log, Id) of
        [] ->
            case catch Mod:open_log(Srv, Type, Dir) of
                {true, Data} ->
                    AL = #log{id={Type, Srv}, amod=Mod, data=Data},
                    ets:insert(yaws_log, AL),
                    true;
                _ ->
                    false
            end;
        _ ->
            %% Already exists. Might be the case that both http and https
            %% has been enabled and we don't want to open the same log twice.
            true
    end.


do_close_log(#log{id={Type, Srv}, amod=Mod, data=Data}) ->
    catch Mod:close_log(Srv, Type, Data).


do_close_logs('$end_of_table') ->
    ok;
do_close_logs(Id) ->
    [AL] = ets:lookup(yaws_log, Id),
    do_close_log(AL),
    do_close_logs(ets:next(yaws_log, Id)).


do_rotate('$end_of_table', _) ->
    ok;
do_rotate(Id, LogWrapSize) ->
    [#log{id={Type, Srv}, amod=Mod, data=Data}=AL] = ets:lookup(yaws_log, Id),
    Data1 = Mod:wrap_log(Srv, Type, Data, LogWrapSize),
    ets:insert(yaws_log, AL#log{data=Data1}),
    do_rotate(ets:next(yaws_log, Id), LogWrapSize).
