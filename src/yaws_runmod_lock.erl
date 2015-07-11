%%----------------------------------------------------------------------
%%% File    : yaws_runmod_lock.erl
%%% Created : 11 Nov 2012 by tjeerd <tjeerd@yolt.nl>
%%% Purpose : Generic locking server
%%%           - lock a resource identified by its path
%%%           - this path uses a forward slash ("/") as separator
%%%           - use unique path for the resource (i.e. filesystem path)
%%%           - unique: server wide, locking functionality can be shared
%%%           - always use an absolute path starting with a slash
%%%           - lock scope can be exclusive (default) or shared
%%%           - depth can be 0 or infinity (default)
%%%---------------------------------------------------------------------

-module(yaws_runmod_lock).

-define(DEBUG(X), io:format(X)).
-define(DEBUG(X,Y), io:format(X,Y)).


-include("../include/yaws_lock.hrl").
-include("../include/yaws_api.hrl").
-include("../include/yaws.hrl").
-include("yaws_debug.hrl").
-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).

%% start/stop manually
-export([start/0,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% API - main use
%%
%% lock(Path,Lock) -> {ok,Id}|{error,Reason}
%% unlock(Path,Id) -> ok|{error,Reason}
%% locked(Path) -> true|false
%% check(Path,Id) -> ok|{error,Reason}
%% discover(Path) -> [Lock]
%% started() -> true|false
%%
-export([lock/2, unlock/2, locked/1, check/2, discover/1, clear/0, started/0]).

%% API - console debugging
%%
%% report() -> Report
%% dump() -> Dump
%% clear_manual() -> ok
%%
-export([report/0, dump/0, cleanup_manual/0]).

start() -> gen_server:start_link({local,?MODULE},?MODULE,[],[]).
stop() ->
    gen_server:call(?MODULE,stop).

lock(Path,Lock) ->
    gen_server:call(?MODULE,{lock,Path,Lock}).
unlock(Path,Id) ->
    gen_server:call(?MODULE,{unlock,Path,Id}).
locked(Path) ->
    gen_server:call(?MODULE,{locked,Path}).
check(Path,Id) ->
    gen_server:call(?MODULE,{check,Path,Id}).
discover(Path) ->
    gen_server:call(?MODULE,{discover,Path}).
report() ->
    gen_server:call(?MODULE,report).
dump() ->
    gen_server:call(?MODULE,dump).
clear() ->
    gen_server:call(?MODULE,clear).
cleanup_manual() ->
    erlang:send(?MODULE,cleanup).
started() ->
    case erlang:whereis(?MODULE) of
        undefined -> false;
        _ -> true
    end.

%% init/1
init([]) ->
    % Table is a tree consisting of {Name, Locks, Children} tuples
    error_logger:info_msg("Initializing resource locking server ...~n"),
    Table = [],
    erlang:send_after(?CLEANUP_INTERVAL*1000, self(), cleanup),
    {ok, Table}.

%% handle_call/3
handle_call({lock,Path,Lock}, _From, Table) ->
    try
        T0 = yaws:get_time_tuple(),
        Id = case Lock#lock.id of
                 undefined -> locktoken();
                 _ -> Lock#lock.id
             end,
        %?DEBUG("create lock ~p for ~p~n",[Id,Path]),
        Lock1 = Lock#lock{path=Path,id=Id,timestamp=T0},
        Path1 = filename:split(["/",Path]),
        Table1 = do_lock(Path1,Lock1,Table),
        {reply, {ok,Id}, Table1}
    catch
        Status -> {reply, {error, Status}, Table};
        _Error:Reason ->
            error_logger:error_msg("Unexpected error: ~p~n~p~n",
                                   [Reason,erlang:get_stacktrace()]),
            {reply, {error, Reason}, Table}
    end;
handle_call({unlock,Path,Id}, _From, Table) ->
    % even if the lock is not found, its removal is succesfull
    Path1 = filename:split(["/",Path]),
    {Status,Table1} = do_unlock(Path1,Id,Table),
    {reply, Status, Table1};
handle_call({locked,Path}, _From, Table) ->
    L = filename:split(["/",Path]),
    Lock = do_locked(L,Table),
    {reply, Lock, Table};
handle_call({check,Path,Id}, _From, Table) ->
    L = filename:split(["/",Path]),
    Lock = do_check(L,Id,Table),
    {reply, Lock, Table};
handle_call({discover,Path}, _From, Table) ->
    L = filename:split(["/",Path]),
    Lock = do_discover(L,Table),
    {reply, Lock, Table};
handle_call(report, _From, Table) ->
    do_report(Table),
    {reply, ok, Table};
handle_call(dump, _From, Table) ->
    {reply, Table, Table};
handle_call(stop, _From, Table) ->
    {stop, normal, stopped, Table};
handle_call(clear, _From, _Table) ->
    {reply, ok, []}.

%% handle_cast/2
handle_cast(_Msg, State) ->
    {noreply, State}.

%% handle_info/2
handle_info(cleanup, Table) ->
    erlang:send_after(?CLEANUP_INTERVAL*1000, self(), cleanup),
    Table1 = do_cleanup(Table),
    {noreply, Table1}.

%% terminate/2
terminate(_Reason, _State) ->
    ok.

%% code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% do_lock(Lock,Path,Table) -> Table
%%
do_lock([H],Lock,Table) ->
    case lists:keysearch(H,1,Table) of
        {value,{H,Locks,Children}} ->
            case do_lock_check(Locks,Lock#lock.id) of
                this ->
                    % refresh lock only when same resource
                    Locks1 = do_lock_refresh(Locks,Lock),
                    lists:keyreplace(H,1,Table,{H,Locks1,Children});
                {shared,_} when Lock#lock.scope == shared ->
                    lists:keyreplace(H,1,Table,{H,[Lock|Locks],Children});
                unlocked ->
                    lists:keyreplace(H,1,Table,{H,[Lock],Children});
                _ ->
                    throw(locked)
            end;
        false ->
            lists:keystore(H,1,Table,{H,[Lock],[]})
    end;
do_lock([H|T],Lock,Table) ->
    case lists:keysearch(H,1,Table) of
        {value,{H,Locks,Children}} -> %%%% hier gebleven
            case do_lock_check(Locks,Lock#lock.id) of
                {_,infinity} when Lock#lock.scope == exclusive ->
                    throw(locked);
                _ ->
                    lists:keyreplace(H,1,Table,{H,Locks,
                                                do_lock(T,Lock,Children)})
            end;
        false ->
            lists:keystore(H,1,Table,{H,[],do_lock(T,Lock,[])})
    end.

%% do_lock_check/2 returns
%% - unlocked when no lock found or
%% - {scope,depth} when locked
%% - this when Id matches
do_lock_check(Locks,Id) ->
    do_lock_check(Locks,Id,unlocked).
do_lock_check([],_Id,Result) ->
    Result;
do_lock_check([H|_T],Id,_Result) when H#lock.id == Id ->
    this;
do_lock_check([H|T],Id,_Result) ->
    case H#lock.depth of
        infinity -> {H#lock.scope,infinity};
        0 -> do_lock_check(T,Id,{H#lock.scope,0})
    end.

do_lock_refresh(Locks,Lock) ->
    do_lock_refresh(Locks,Lock,[]).
do_lock_refresh([],_Lock,Result) ->
    Result;
do_lock_refresh([H|T],Lock,Result) when H#lock.id == Lock#lock.id ->
    do_lock_refresh(T,Lock,[Lock|Result]);
do_lock_refresh([H|T],Lock,Result) ->
    do_lock_refresh(T,Lock,[H|Result]).

%%----------------------------------------------------------------------
%% do_unlock(Path,Id,Table) -> Table
%%
do_unlock([H],Id,Table) ->
    case lists:keysearch(H,1,Table) of
        {value,{H,Locks,Children}} ->
            {Status,Locks1} = do_unlock_id(Locks,Id),
            case {Locks1,Children} of
                {[],[]} ->
                    {_,_,Return} = lists:keytake(H,1,Table),
                    {Status,Return};
                _ ->
                    Result = lists:keyreplace(H,1,Table,{H,Locks1,Children}),
                    {Status,Result}
            end;
        false ->
            {not_found,Table}
    end;
do_unlock([H|T],Id,Table) ->
    case lists:keysearch(H,1,Table) of
        {value,{H,Locks,Children}} ->
            {Status,Children1} = do_unlock(T,Id,Children),
            case {Locks,Children1} of
                {[],[]} ->
                    {_,_,Return} = lists:keytake(H,1,Table),
                    {Status,Return};
                _ ->
                    Result = lists:keyreplace(H,1,Table,{H,Locks,Children1}),
                    {Status,Result}
            end;
        false ->
            {not_found,Table}
    end.

do_unlock_id([],_Id) ->
    {not_found,[]};
do_unlock_id([H|T],Id) ->
    case H#lock.id of
        Id ->
            {ok,T};
        _ ->
            {Status,Result} = do_unlock_id(T,Id),
            {Status,[H|Result]}
    end.

%%----------------------------------------------------------------------
%% do_locked(Path,Table) -> Table
%%
do_locked([H],Table) ->
    case lists:keysearch(H,1,Table) of
        {value,{H,Locks,_}} when length(Locks)>0 -> true;
        _ -> false
    end;
do_locked([H|T],Table) ->
    case lists:keysearch(H,1,Table) of
        {value,{H,_Locks,Children}} -> do_locked(T,Children);
        _ -> false
    end.

%%----------------------------------------------------------------------
%% do_check(Path,Id,Table) -> Table
%%
do_check([H|T],Id,Table) ->
    case lists:keysearch(H,1,Table) of
        {value,{H,Locks,_}} when length(T)==0 -> do_check_locks(Locks,Id);
        {value,{H,_Lock,Children}} -> do_check(T,Id,Children);
        false -> {error,not_found}
    end.

do_check_locks([],_Id) ->
    {error,not_found};
do_check_locks([H|T],Id) ->
    case H#lock.id of
        Id -> ok;
        _ -> do_check_locks(T,Id)
    end.

%%----------------------------------------------------------------------
%% do_discover(Path,Table) -> Locks
%%
do_discover([H|T],Table) ->
    case lists:keysearch(H,1,Table) of
        {value,{H,Locks,_}} when length(T)==0 ->
            Locks;
        {value,{H,Locks,Children}} ->
            do_discover_depth_infinity(Locks)++do_discover(T,Children);
        false ->
            []
    end.

do_discover_depth_infinity([]) ->
    [];
do_discover_depth_infinity([H|T]) ->
    Take = case H#lock.depth of
               infinity -> [H];
               _ -> []
           end,
    Take ++ do_discover_depth_infinity(T).

%%----------------------------------------------------------------------
%% do_report(Path) -> Report
%%
do_report(Table) ->
    case Table of
        [] -> io:format("No locks.~n",[]);
        _ -> do_report([],Table)
    end.
do_report(_Path,[]) ->
    ok;
do_report(Path,[{Name,Locks,Children}|T]) ->
    Resource = filename:join(Path,Name),
    if
        length(Locks)>0 ->
            io:format("~p~n",[Resource]),
            do_report_locks(Locks);
        true ->
            []
    end,
    do_report(Resource,Children),
    do_report(Path,T).

do_report_locks([]) ->
    ok;
do_report_locks([H|T]) ->
    io:format("... ~p lock with token ~p, scope ~p, depth ~p~n",
              [H#lock.type,H#lock.id,H#lock.scope,H#lock.depth]),
    do_report_locks(T).

%%----------------------------------------------------------------------
%% do_cleanup(Table) -> Table
%%
do_cleanup([]) ->
    [];
do_cleanup([{Name,Locks,Children}|T]) ->
    Locks1 = do_cleanup_locks(Locks),
    Children1 = do_cleanup(Children),
    if
        (length(Locks1)==0) and (length(Children1)==0) ->
            do_cleanup(T);
        true ->
            [{Name,Locks1,Children1}|do_cleanup(T)]
    end.

do_cleanup_locks([]) ->
    [];
do_cleanup_locks([H|T]) ->
    T0 = H#lock.timestamp,
    T1 = yaws:get_time_tuple(),
    Delta = timer:now_diff(T1,T0),
    if
        Delta > (H#lock.timeout*1000000) ->
            error_logger:info_msg("discarded lock ~p~n",[H#lock.id]),
            do_cleanup_locks(T);
        true ->
            [H|do_cleanup_locks(T)]
    end.

%%----------------------------------------------------------------------
locktoken() ->
    % RFC4122 section 3 based UUID
    Version = 1,
    Variant = 2#10,
    Now = {_, _, Micro} = yaws:get_time_tuple(),
    Nowish = calendar:now_to_universal_time(Now),
    Timestamp = calendar:datetime_to_gregorian_seconds(Nowish) * 1000000000,
    <<TimeHi:12, TimeMid:16, TimeLow:32>> = <<Timestamp:60>>,
    Clocksequence = <<Micro:14>>,
    <<ClockseqHi:6, ClockseqLow:8>> = Clocksequence,
    Node = get_hwaddr(),
    UUID = <<TimeLow:32, TimeMid:16, Version:4, TimeHi:12,
      Variant:2, ClockseqLow:8, ClockseqHi:6, Node/binary>>,
    <<U0:32, U1:16, U2:16, U3:16, U4:48>> = UUID,
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b"
                                ,[U0,U1,U2,U3,U4])).


get_hwaddr() ->
    get_hwaddr(erlang:function_exported(inet, getifaddrs, 0)).
get_hwaddr(true) ->
    {ok,Ifs} = inet:getifaddrs(),
    Addrs = [ lists:keysearch(hwaddr,1,Attr) || {_If,Attr} <- Ifs ],
    Addr = lists:max([ A || {value,{hwaddr,A}} <- Addrs ]),
    list_to_binary(Addr);
get_hwaddr(false) ->
    %% this clause is for backward compatibility to R13
    {ok, Ifs} = inet:getiflist(),
    Addrs = lists:foldl(
              fun([{hwaddr, HW}], Acc) -> [HW|Acc];
                 ([], Acc) -> Acc
              end, [],
              [begin {ok, HW} =inet:ifget(If, [hwaddr]), HW end || If <- Ifs]),
    HWAddrs = case Addrs of
                  [] ->
                      %% hwaddr doesn't work on Mac on R13.
                      %% Fall back to ifconfig :(
                      Ifconfig = os:cmd("/sbin/ifconfig -a"),
                      {ok, Pat} = re:compile("ether\s+([0-9a-fA-F:]+)"),
                      {match, Matches} =
                          re:run(Ifconfig, Pat, [global, {capture, [1]}]),
                      HWs = [string:substr(Ifconfig, At+1, Len) ||
                                [{At,Len}] <- Matches],
                      [[erlang:list_to_integer(V, 16) ||
                           V <- string:tokens(S, ":")] || S <- HWs];
                  _ ->
                      Addrs
              end,
    list_to_binary(lists:max(HWAddrs)).

