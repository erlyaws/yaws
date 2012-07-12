%%----------------------------------------------------------------------
%%% File    : yaws_davlock.erl
%%% Author  : tjeerd <tjeerd@yolt.nl>
%%% Purpose :
%%% Created : 25 Jul 2012 by tjeerd <tjeerd@yolt.nl>
%%%----------------------------------------------------------------------

-module(yaws_davlock).
-author('tjeerd@yolt.nl').
-include("../include/yaws_dav.hrl").
-include("../include/yaws_api.hrl").
-include("../include/yaws.hrl").
-include("yaws_debug.hrl").
-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).

%% start/stop manually
-export([start_link/0,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% API
%
% lock(Path,Lock) -> {ok,Id}|{error,Reason}
% unlock(Path,Id) -> ok|{error,Reason}
% locked(Path) -> true|false
% check(Path,Id) -> ok|{error,Reason}
% discover(Path) -> [Lock]
% report() -> Report
% dump() -> Dump
%
-export([lock/2, unlock/2, locked/1, check/2, discover/1, report/0, dump/0, clear/0]).
-export([cleanup_manual/0]).

start_link() -> gen_server:start_link({local,?MODULE},?MODULE,[],[]).
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

%% init/1
init([]) ->
    %Table is a tree consisting of {Name, Locks, Children} tuples
    Table = [],
    erlang:send_after(?CLEANUP_INTERVAL*1000, self(), cleanup),
    {ok, Table}.

%% handle_call/3
handle_call({lock,Path,Lock}, _From, Table) ->
    try
        T0 = erlang:now(),
        Id = locktoken(),
        %?elog("create lock ~p for ~p~n",[Id,Path]),
        Lock1 = Lock#davlock{path=Path,id=Id,timestamp=T0},
        Path1 = filename:split(Path),
        Table1 = do_lock(Path1,Lock1,Table),
        {reply, {ok,Id}, Table1}
    catch
        Status -> {reply, {error, Status}, Table};
        _Error:Reason ->
            ?elog("Unexpected error: ~p~n~p~n",[Reason,erlang:get_stacktrace()]),
            {reply, {error, Reason}, Table}
    end;
handle_call({unlock,Path,Id}, _From, Table) ->
    % even if the lock is not found, its removal is succesfull
    %?elog("remove lock ~p for ~p~n",[Id,Path]),
    Path1 = filename:split(Path),
    Table1 = do_unlock(Path1,Id,Table),
    {reply, ok, Table1};
handle_call({locked,Path}, _From, Table) ->
    L = filename:split(Path),
    Lock = do_locked(L,Table),
    {reply, Lock, Table};
handle_call({check,Path,Id}, _From, Table) ->
    L = filename:split(Path),
    Lock = do_check(L,Id,Table),
    {reply, Lock, Table};
handle_call({discover,Path}, _From, Table) ->
    L = filename:split(Path),
    Lock = do_discover(L,Table),
    {reply, Lock, Table};
handle_call(report, _From, Table) ->
    R = do_report(Table),
    Report = [ {Lock,format(Name)} || {Lock,Name} <- R],
    {reply, Report, Table};
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
do_lock([],_Lock,Table) ->
    % fileaname:split/1 can return empty list
    Table;
do_lock([H],Lock,Table) ->
    case lists:keysearch(H,1,Table) of
        {value,{H,Locks,Children}} ->
            case do_lock_check(Locks) of
                {shared,_} when Lock#davlock.scope == shared ->
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
        {value,{H,Locks,Children}} ->
            case do_lock_check(Locks) of
                {_,infinity} when Lock#davlock.scope == exclusive ->
                    throw(locked);
                _ ->
                    lists:keyreplace(H,1,Table,{H,Locks,do_lock(T,Lock,Children)})
            end;
        false ->
            lists:keystore(H,1,Table,{H,[],do_lock(T,Lock,[])})
    end.

do_lock_check(Locks) ->
    do_lock_check(Locks,unlocked).
do_lock_check([],Result) ->
    Result;
do_lock_check([H|T],_Result) ->
    case H#davlock.depth of
        infinity -> {H#davlock.scope,infinity};
        0 -> do_lock_check(T,{H#davlock.scope,0})
    end.

%%----------------------------------------------------------------------
%% do_unlock(Path,Id,Table) -> Table
%%
do_unlock([],_Id,Table) ->
    Table;
do_unlock([_H|_T],_Id,[]) ->
    [];
do_unlock([H],Id,Table) ->
    case lists:keysearch(H,1,Table) of
        {value,{H,Locks,Children}} ->
            Locks1 = do_unlock_id(Locks,Id),
            case {Locks1,Children} of
                {[],[]} ->
                    {_,_,Return} = lists:keytake(H,1,Table),
                    Return;
                _ ->
                    lists:keyreplace(H,1,Table,{H,Locks1,Children})
            end;
        false ->
            Table
    end;
do_unlock([H|T],Id,Table) ->
    case lists:keysearch(H,1,Table) of
        {value,{H,Locks,Children}} ->
            Children1 = do_unlock(T,Id,Children),
            case {Locks,Children1} of
                {[],[]} ->
                    {_,_,Return} = lists:keytake(H,1,Table),
                    Return;
                _ ->
                    lists:keyreplace(H,1,Table,{H,Locks,Children1})
            end;
        false ->
            Table
    end.

do_unlock_id([],_Id) ->
    [];
do_unlock_id([H|T],Id) ->
    case H#davlock.id of
        Id -> T;
        _ -> [H|do_unlock_id(T,Id)]
    end.

%%----------------------------------------------------------------------
%% do_locked(Path,Table) -> Table
%%
do_locked([],_Table) ->
    false;
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
do_check([],_Id,_Table) ->
    {error,not_found};
do_check([H],Id,Table) ->
    case lists:keysearch(H,1,Table) of
        {value,{H,Locks,_}} -> do_check_locks(Locks,Id);
        false -> {error,not_found}
    end;
do_check([H|T],Id,Table) ->
    case lists:keysearch(H,1,Table) of
        {value,{H,_Lock,Children}} -> do_check(T,Id,Children);
        false -> {error,not_found}
    end.

do_check_locks([],_Id) ->
    {error,not_found};
do_check_locks([H|T],Id) ->
    case H#davlock.id of
        Id -> ok;
        _ -> do_check_locks(T,Id)
    end.

%%----------------------------------------------------------------------
%% do_discover(Path,Table) -> Locks
%%
do_discover([],_Table) ->
    [];
do_discover([H],Table) ->
    case lists:keysearch(H,1,Table) of
        {value,{H,Locks,_}} -> Locks;
        false -> []
    end;
do_discover([H|T],Table) ->
    case lists:keysearch(H,1,Table) of
        {value,{H,Locks,Children}} ->
            do_discover_depth_infinity(Locks)++do_discover(T,Children);
        false -> []
    end.

do_discover_depth_infinity([]) ->
    [];
do_discover_depth_infinity([H|T]) ->
    Take = case H#davlock.depth of
               infinity -> [H];
               _ -> []
           end,
    Take ++ do_discover_depth_infinity(T).

%%----------------------------------------------------------------------
%% do_report(Path) -> Report
%%
do_report([]) ->
    [];
do_report([{Name,Lock,Children}|T]) ->
    Rest = do_report(T),
    case Lock of
        unlocked ->
            [ {locked,[Name|N]} || {locked,N} <- do_report(Children)]++Rest;
        _ ->
            _Owner = Lock#davlock.owner,
            _Scope = Lock#davlock.scope,
            _Type = Lock#davlock.type,
            [{locked,[Name]}]++Rest++[ {locked,[Name|N]} || {locked,N} <- do_report(Children)]
    end.

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
    T0 = H#davlock.timestamp,
    T1 = erlang:now(),
    Delta = timer:now_diff(T1,T0),
    if
        Delta > (H#davlock.timeout*1000000) ->
            ?elog("discarded lock ~p~n",[H#davlock.id]),
            do_cleanup_locks(T);
        true ->
            [H|do_cleanup_locks(T)]
    end.

%%----------------------------------------------------------------------
locktoken() ->
    % RFC4122 section 3 based UUID
    Version = 1,
    Variant = 2#10,
    Now = {_, _, Micro} = now(),
    Nowish = calendar:now_to_universal_time(Now),
    Timestamp = calendar:datetime_to_gregorian_seconds(Nowish) * 1000000000,
    <<TimeHi:12, TimeMid:16, TimeLow:32>> = <<Timestamp:60>>,
    Clocksequence = <<Micro:14>>,
    <<ClockseqHi:6, ClockseqLow:8>> = Clocksequence,
    {ok,Ifs} = inet:getifaddrs(),
    Addrs = [ lists:keysearch(hwaddr,1,Attr) || {_If,Attr} <- Ifs ],
    Addr = lists:max([ A || {value,{hwaddr,A}} <- Addrs ]),
    Node = list_to_binary(Addr),
    UUID = <<TimeLow:32, TimeMid:16, Version:4, TimeHi:12,
      Variant:2, ClockseqLow:8, ClockseqHi:6, Node/binary>>,
    <<U0:32, U1:16, U2:16, U3:16, U4:48>> = UUID,
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",[U0,U1,U2,U3,U4])).

format([]) ->
    [];
format(["/"|T]) ->
    format(T);
format([H|T]) ->
    lists:flatten(["/",H|format(T)]).
