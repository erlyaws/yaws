%%%----------------------------------------------------------------------
%%% File    : yaws_mnesia_session.erl
%%% Author  : Nicolas Thauvin <nicolas@corporama.com>
%%% Purpose : mnesia storage callbacks for yaws_session_server
%%%----------------------------------------------------------------------

%% Warning !!!
%% This module may init mnesia in a way that is not suitable for your
%% application !

%% Quick steps:
%% 1) Compile this module
%% 2) Drop the .beam in your code path
%% 3) Run Yaws with the 

-module (yaws_mnesia_session).

-export ([init_backend/1, stop_backend/0]).
-export ([list/0, insert/1, lookup/1, delete/1]).
-export ([traverse/1, cleanup/0]).

-define(TABLE, ysession).

init_backend(Fields) ->
    case net_kernel:get_net_ticktime() of
        ignored ->
            Message = "mnesia backend needs distribution (a node name)",
            Error = {?MODULE, Message},
            error_logger:error_msg("~p~n", [Error]),
            exit (Error);
        _ ->
            ok
    end,
    case catch mnesia:table_info(schema, where_to_write) of
        Nodes when is_list(Nodes) ->
            case catch mnesia:table_info(?TABLE, where_to_write) of
                {'EXIT', {aborted, {no_exists, ?TABLE, where_to_write}}} ->
                    Nodes = mnesia:system_info(running_db_nodes),
                    Options = [{disc_copies, Nodes}, {type, set},
                               {attributes, Fields}],
                    {atomic, ok} = mnesia: create_table (?TABLE, Options),
                    ok;
                List when is_list (List) ->
                    ok = mnesia:wait_for_tables([?TABLE], 60000)
            end;
        {'EXIT',{aborted,{no_exists,schema,where_to_write}}} ->
            application:stop(mnesia),
            mnesia:create_schema([node()]),
            ok = application:start(mnesia),
            init_backend(Fields)
    end.

stop_backend() ->
    ok.

insert(Session) ->
    Fun = fun () -> mnesia:write(Session) end,
    {atomic, ok} = mnesia:transaction(Fun),
    true.

lookup(Key) ->            
    Fun = fun () -> mnesia:read(?TABLE, Key) end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result.

delete(Key) ->
    Fun = fun () -> mnesia:delete({?TABLE, Key}) end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result.

list() ->
    Fold = fun (Session, Acc) -> [Session | Acc] end,
    Fun = fun () -> mnesia:foldl(Fold, [], ?TABLE) end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result.

cleanup() ->
    mnesia:clear_table(?TABLE).

traverse(Gnow) ->
    Fun = fun() -> tr_traverse(Gnow) end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result.

tr_traverse(Gnow) ->
    tr_traverse(Gnow, mnesia:first(?TABLE)).

tr_traverse(_N, '$end_of_table') ->
    ok;
tr_traverse(N, Key) ->
    case mnesia:read(?TABLE, Key) of
        [Y] ->
            case yaws_session_server:has_timedout(Y, N) of
                false ->
                    tr_traverse(N, mnesia:next(?TABLE, Key));
                true ->
                    yaws_session_server:report_timedout_sess(Y),
                    Next = mnesia:next(?TABLE, Key),
                    mnesia:delete({?TABLE, Key}),
                    tr_traverse(N, Next)                    
            end;
        [] ->
           tr_traverse(N, mnesia:next(?MODULE, Key))
    end.


