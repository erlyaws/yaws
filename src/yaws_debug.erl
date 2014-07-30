%%%----------------------------------------------------------------------
%%% File    : yaws_debug.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose :
%%% Created :  7 Feb 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_debug).
-author('klacke@hyber.org').

-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").
-export([typecheck/3,
         format_record/3,
         assert/4,
         format/2,format/3,
         derror/2,
         dinfo/2,
         mktags/0,
         xref/1,
         pids/0,
         eprof/0,
         check_headers/1, nobin/1,
         do_debug_dump/1
        ]).



typecheck([{record, Rec, X} | Tail], File, Line) when is_atom(X),
                                                      element(1, Rec) == X ->
    typecheck(Tail, File, Line);
typecheck([{int, Int} |Tail], File, Line) when is_integer(Int) ->
    typecheck(Tail, File, Line);
typecheck([Err|_], File, Line) ->
    debug_format(user, "TC ERROR ~s:~w:~n~p",
              [File, Line, Err]),
    erlang:error(tcerr);
typecheck([], _,_) ->
    ok.


%% returns {record, RecName, [Field1, Val1} .....]
format_record(Record, Name, Fields) ->
    case tuple_to_list(Record) of
        [Name | Rest] ->
            io_lib:format("record ~w\n~s", [Name,
                                            format_record(Rest, Fields)]);
        _X ->
            ?Debug("Bad record ~p is not ~p~n", [_X, Name]),
            "badrecord"
    end.

format_record([], []) ->
    [];
format_record([Val|Vals], [F|Fs]) when is_integer(Val);
                                       Val == [];
                                       is_atom(Val);
                                       is_float(Val)->
    [io_lib:format("     ~w = ~w\n", [F,Val]),
     format_record(Vals, Fs)];
format_record([Val|Vals], [F|Fs]) ->
    case is_string(Val) of
        true ->
            [io_lib:format("     ~w = \"~s\"\n", [F,Val]),
             format_record(Vals, Fs)];
        false ->
            [io_lib:format("     ~w = ~p~n", [F, nobin(Val)]),
             format_record(Vals, Fs)]
    end.

is_string(L) when is_list(L) ->
    lists:filter(fun(X) when is_integer(X),
                             $A < X, X < $z ->
                         false;
                    (_) ->
                         true
                 end,L) == [];
is_string(_) ->
    false.



assert(equal,X,Y,_) when X==Y ->
    ok;
assert(neq,X,Y,_) when X/=Y ->
    ok;
assert(integer,X,_,_) when is_integer(X) ->
    ok;
assert(list,X,_,_) when is_list(X) ->
    ok;
assert({list,length,equal},X,Y,_) when is_list(X), length(X)==Y ->
    ok;
assert(greater,X,Y,_) when is_integer(X), is_integer(Y), X>Y ->
    ok;
assert(min,X,Y,_) when is_integer(X), is_integer(Y), X>=Y ->
    ok;
assert(lesser,X,Y,_) when is_integer(X), is_integer(Y), X<Y ->
    ok;
assert(max,X,Y,_) when is_integer(X), is_integer(Y), X=<Y ->
    ok;
assert(interval,X,{Min,Max},_) when is_integer(X), is_integer(Min),
                                    is_integer(Max),
                                    X>=Min, Max>=X ->
    ok;
assert('fun', Fun, _, Failure) ->
    case catch Fun() of
        true -> ok;
        _Other -> fail(Failure)
    end;

assert(in,X,L,Failure) when is_list(L) ->
    case lists:member(X,L) of
        true -> ok;
        _ -> fail(Failure)
    end;

assert(_,_,_,Failure) ->
    fail(Failure).

fail({assert,File,Line,Message}) ->
    debug_format(user, "Assertion FAILED ~p:~p, pid ~w exiting: ~p~n",
              [File, Line, self(), Message]),
    erlang:error(assertion_failed);
fail({alert,File,Line,Message}) ->
    debug_format(user, "Assert WARNING ~p:~p, pid ~w: ~p~n",
              [File, Line, self(), Message]),
    ok;
fail({{debug,Fstr}, File,Line,Fmt, Args}) ->
    Str = lists:flatten(
            io_lib:format("~s <~p> ~s:~p, pid ~w: ~n",
                          [Fstr, node(), filename:basename(File),
                           Line, self()])),

    case (catch debug_format(user, Str ++ Fmt ++ "~n", Args)) of
        ok -> ok;
        _ -> debug_format(user, "ERROR ~p:~p: Pid ~w: (bad format)~n~p,~p~n",
                       [File, Line, self(), Fmt, Args]),

             ok
    end;

fail({format, File,Line,Fmt,Args}) ->
    case (catch debug_format(user, Fmt,Args)) of
        ok -> ok;
        _ ->
            debug_format(user, "ERROR ~p:~p: Pid ~w: (bad format)~n~p,~p~n",
                      [File, Line, self(), Fmt, Args]),

            ok
    end.

debug_format(_, F, D) ->
    debug_format(F, D).

debug_format(F, A) ->
    Str = case catch io_lib:format("yaws debug: " ++ F, A) of
        {'EXIT', Reason} ->
            io_lib:format("yaws debug: F=~s A=~p (failed to format: ~p)",
                [F, A, Reason]);
        Ok -> Ok
    end,
    error_logger:info_msg(Str),
    catch io:format(F, A),
    ok.

format(F, A) ->
    format(get(gc), F, A).
format(GC, F, A) ->
    case ?gc_has_debug(GC) of
        true ->
            error_logger:info_msg("yaws debug:" ++ F, A);
        false ->
            ok
    end.

derror(F, A) ->
    case ?gc_has_debug((get(gc))) of
        true ->
            error_logger:error_msg("yaws:" ++ F, A);
        false ->
            ok
    end.

dinfo(F, A) ->
    case ?gc_has_debug((get(gc))) of
        true ->
            error_logger:info_msg("yaws:" ++ F, A);
        false ->
            ok
    end.


mktags() ->
    tags:dirs(["."]),
    init:stop().



xref([Dir]) ->
    debug_format("~p~n", [xref:d(Dir)]),
    init:stop().


pids() ->
    lists:zf(
      fun(P) ->
              case process_info(P) of
                  L when is_list(L) ->
                      {value, {_, {M1, _,_}}} =
                          lists:keysearch(current_function, 1, L),
                      {value, {_, {M2, _,_}}} =
                          lists:keysearch(initial_call, 1, L),
                      S1= atom_to_list(M1),
                      S2 = atom_to_list(M2),
                      case {S1, S2} of
                          {"yaws" ++ _, _} ->
                              {true, P};
                          {_, "yaws"++_} ->
                              {true, P};
                          _ ->
                              false
                      end;
                  _ ->
                      false
              end
      end,
      processes()).



eprof() ->
    eprof:start(),
    eprof:profile(pids()),
    debug_format("Ok run some traffic \n", []).



-define(h_check(H, Field),
        f_check(H#outh.Field, Field)).

f_check(undefined, _Field) ->
    ok;
f_check(Str, Field) ->
    case lists:reverse(lists:flatten(Str)) of
        [$\n, $\r , H | _Tail] ->
            case lists:member(H, [$\n, $\r]) of
                true ->
                    error_logger:format("Bad <~p> header:~n"
                                        "  ~p~nToo many newlines",
                                        [Field, Str]),
                    exit(normal);
                false ->
                    ok
            end;
        _Other ->
            error_logger:format("Bad <~p> header:~n"
                                "~p~nNot ending with CRNL~n",
                                [Field, Str]),
            exit(normal)
    end.

check_headers(H) ->
    ?h_check(H, connection),
    ?h_check(H, server),
    ?h_check(H, location),
    ?h_check(H, cache_control),
    ?h_check(H, date),
    ?h_check(H, allow),
    ?h_check(H, last_modified),
    ?h_check(H, etag),
    ?h_check(H, content_range),
    ?h_check(H, content_length),
    ?h_check(H, content_encoding),
    ?h_check(H, set_cookie),
    ?h_check(H, transfer_encoding),
    ?h_check(H, www_authenticate),
    check_other(H#outh.other).


check_other(undefined) ->
    ok;
check_other(L0) ->
    L = lists:flatten(L0),
    case lists:dropwhile(fun(X) -> not lists:member(X, ["\r\n"]) end, L) of
        [] ->
            ok;
        [$\r, $\n, H | _Tail] ->
            case lists:member(H, [$\n, $\r]) of
                true ->
                    bad_other(L);
                false ->
                    ok
            end;
        _Other ->
            bad_other(L)
    end.


bad_other(L) ->
    Bad = lists:takewhile(
            fun(X) -> not lists:member(X, ["\r\n"]) end, L),
    error_logger:format("Bad header:~p~n"
                        "Too many newlines",
                        [Bad]),
    exit(normal).




nobin(X) ->
    case catch xnobin(X) of
        {'EXIT', Reason} ->
            error_logger:format("~p~n~p~n", [X, Reason]),
            erlang:error(Reason);
        Res ->
            Res
    end.


xnobin(B) when is_binary(B) ->
    lists:flatten(io_lib:format("#Bin(~w)", [size(B)]));
xnobin(L) when is_list(L) ->
    lists:map(fun(X) -> xnobin(X) end, L);
xnobin(T) when is_tuple(T) ->
    list_to_tuple(xnobin(tuple_to_list(T)));
xnobin(X) ->
    X.



%%%%%%%%%%%%%%% debug dump %%%%%%%%%%%%%%%%%%%%%%%


do_debug_dump(Socket) ->
    gen_version(Socket),
    gen_sep(Socket),
    %% keep proc status last, to report on hangs for the others
    CollectOS = gen_os(Socket),
    Collect = lists:foldl(fun({F, Str}, Acc) ->
                                  Ret = collect(F, Socket, Str),
                                  gen_sep(Socket),
                                  [Ret|Acc]
                          end,
                          CollectOS,
                          [{fun send_status/1, "Yaws status"},
                           {fun send_inet/1,   "Inet status"},
                           {proc_status_fun(), "process status"}]),
    lists:foreach(fun(ok) ->
                          ok;
                     ({pid, Pid}) ->
                          exit(Pid, shutdown)
                  end, Collect),
    ok.


gen_version(Socket) ->
    sock_format(Socket, "Yawsvsn: ~p~n", [yaws_generated:version()]).

gen_os(Socket) ->
    OSType = os:type(),
    [gen_oscmd(Socket, "uname -a"),
     gen_oscmd(Socket, "ifconfig -a"),
     gen_oscmd(Socket, top_cmd(OSType)),
     gen_oscmd(Socket, netstat_cmd(OSType))].

gen_oscmd(Socket, Cmd) ->
    F = fun(Sock) ->
                sock_format(Sock, "~s:~n~s~n", [Cmd, os:cmd(Cmd)])
        end,
    Ret = collect(F, Socket, Cmd),
    gen_sep(Socket),
    Ret.

%% FIXME The 'top -b -n 1' invocation is actually for version 3.2.x
%% typically(?) found on Linux, while the 'top -b' is for (e.g.) version
%% 3.5.x typically(?) found on *BSD. For the latter, '-b' itself means
%% "run only once", '-n' is an alias for '-b', and '1' means show only
%% one process. Obviously there is a problem if 3.2.x or equivalent ends
%% up getting invoked w/o '-n 1', since it will loop forever...
top_cmd({unix, linux}) -> "top -b -n 1";
top_cmd({unix, sunos}) -> "top -b -d 2 -s 1 || /usr/ucb/ps -auxww";
top_cmd({unix, qnx})   -> "pidin times; pidin pmem";
top_cmd({unix, darwin}) -> "top -o cpu -l 1";
top_cmd(_)             -> "top -b".

netstat_cmd({unix, linux})   -> "netstat -ant";
netstat_cmd({unix, freebsd}) -> "netstat -an -p tcp";
netstat_cmd({unix, sunos})   -> "netstat -an -P tcp";
netstat_cmd(_)               -> "netstat -an".

gen_sep(Socket) ->
    sock_format(Socket,"~n~s~n", [lists:duplicate(40, $*)]).


proc_status_fun() ->
    fun(Fd) ->
            sock_format(Fd, "Process status:~n", []),
            i1(Fd, processes())
    end.

i1(Fd, Ps) ->
    Alive = lists:filter(fun palive/1, Ps),
    i2(Fd, Alive),
    case lists:filter(fun pzombie/1, Ps) of
        [] ->
            ok;
        Zombies ->
            %% Zombies is not the same as Ps-Alive, since the remote
            %% process that fetched Ps is included among Alive, but has
            %% exited (for ni/0).
            sock_format(Fd, "\nDead processes:\n", []),
            i2(Fd, Zombies)
    end.

i2(Fd, Ps) ->
    iformat(Fd, "Pid", "Initial Call", "Current Function", "Reds", "Msgs",
            "Heap", "Stack"),
    {Reds,Msgs,Heap,Stack,Susp1,Susp2,MemSusp,_} =
        lists:foldl(fun display_info/2, {0,0,0,0,[],[],[],Fd}, Ps),
    iformat(Fd, "Total", "", "", io_lib:write(Reds), io_lib:write(Msgs),
            io_lib:write(Heap), io_lib:write(Stack)),
    lists:foreach(fun(Susp) -> display_susp1(Fd, Susp) end, Susp1),
    lists:foreach(fun(Susp) -> display_susp2(Fd, Susp) end, Susp2),
    lists:foreach(fun(Susp) -> display_susp3(Fd, Susp) end, MemSusp).


palive(Pid) ->
    case process_info(Pid, status) of
        undefined         -> false;
        {status, exiting} -> false;
        _                 -> true
    end.

pzombie(Pid) ->
    case process_info(Pid, status) of
        undefined         -> false;
        {status, exiting} -> true;
        _                 -> false
    end.


-define(MEM_LARGE, 40000).

display_info(Pid, {R,M,H,St,S1,S2,S3,Fd}) ->
    case process_info(Pid) of
        undefined ->
            {R, M};
        Info ->
            Call = initial_call(Info),
            Curr = fetch(current_function, Info),
            Reds = fetch(reductions, Info),
            LM = fetch(message_queue_len, Info),
            Heap = fetch(heap_size, Info),
            Stack = fetch(stack_size, Info),
            Mem = case process_info(Pid, memory) of
                      undefined -> 0;
                      {memory, Int} -> Int
                  end,
            iformat(Fd,
                    io_lib:write(Pid),
                    mfa_string(Call),
                    mfa_string(Curr),
                    io_lib:write(Reds),
                    io_lib:write(LM),
                    io_lib:write(Heap),
                    io_lib:write(Stack)),
            %% if it got msgs, it's suspicios
            NS1 = if LM > 0 -> [{Pid, Reds, LM} | S1];
                     true -> S1
                  end,
            %% if it's in gen:wait_resp* it's suspicios
            NS2 = case Curr of
                      {gen, wait_resp, _} -> [{Pid, Reds} | S2];
                      {gen, wait_resp_mon, _} -> [{Pid, Reds} | S2];
                      _ -> S2
                  end,
            %% If it is large .. it is suspicios
            NS3 = if Mem > ?MEM_LARGE ->
                          [{Pid, Mem} | S3];
                     true ->
                          S3
                  end,
            {R+Reds, M+LM, H+Heap,St+Stack, NS1, NS2, NS3, Fd}
    end.

display_susp1(Fd, {Pid, Reds0, LM0}) ->
    case process_info(Pid) of
        undefined ->
            ok;
        Info ->
            Reds1 = fetch(reductions, Info),
            LM1 = fetch(message_queue_len, Info),
            Msgs = fetch(messages, Info),
            Bt = case process_info(Pid, backtrace) of
                     {backtrace, Bin} ->
                         binary_to_list(Bin);
                     _ ->
                         []
                 end,
            if LM1 > 0 ->
                    %% still suspicious
                    sock_format(Fd,
                                "*** Suspicious *** : ~-12w, Qlen = ~4w/~-4w, "
                                "Reds = ~12w/~-12w\n",
                                [Pid, LM0, LM1, Reds0, Reds1]),
                    lists:foreach(
                      fun(Msg) -> sock_format(Fd, "  ~p\n",[Msg]) end,
                      Msgs),
                    gen_sep(Fd),
                    sock_format(Fd, "\n\n\n\n*** Backtrace *** for ~w\n~s\n",
                                [Pid,Bt]);
               true ->
                    ok
            end
    end.

display_susp2(Fd, {Pid, Reds0}) ->
    case process_info(Pid, reductions) of
        undefined ->
            ok;
        {reductions, Reds0} ->
            %% it hasn't done any work... print bt
            case process_info(Pid, backtrace) of
                {backtrace, Bin} ->
                    gen_sep(Fd),
                    sock_format(Fd, "\n\n\n\n*** Backtrace (gen_wait) "
                                "*** for ~w\n~s\n",
                                [Pid, binary_to_list(Bin)]);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

display_susp3(Fd, {Pid, _Mem}) ->
    case {process_info(Pid, memory), process_info(Pid, current_function)} of
        {undefined, _} ->
            ok;
        {_, {current_function,{yaws_debug,display_susp3,2}}} ->
            ok;
        {{memory, Mem2}, _} when Mem2 > ?MEM_LARGE ->
            %% it's still too big
            case process_info(Pid, backtrace) of
                {backtrace, Bin} ->
                    gen_sep(Fd),
                    sock_format(Fd,
                                "\n\n\n\n*** Backtrace (mem=~p) "
                                "*** for ~w\n~p~n~s\n",
                                [Mem2, Pid, process_info(Pid),
                                 binary_to_list(Bin)]);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.



initial_call(Info)  ->
    case fetch(initial_call, Info) of
        {proc_lib, init_p, 5} ->
            proc_lib:translate_initial_call(Info);
        ICall ->
            ICall
    end.

mfa_string({M, F, A}) ->
    io_lib:format("~w:~w/~w", [M, F, A]);
mfa_string(X) ->
    io_lib:write(X).

fetch(Key, Info) ->
    case lists:keysearch(Key, 1, Info) of
        {value, {_, Val}} -> Val;
        false -> 0
    end.

iformat(Fd, A1, A2, A3, A4, A5, A6, A7) ->
    sock_format(Fd, "~-12s ~-23s ~-23s ~12s ~4s ~12s ~10s\n",
                [A1,A2,A3,A4,A5,A6,A7]).

sock_format(Sock, Fmt, Args) ->
    gen_tcp:send(Sock, io_lib:format(Fmt, Args)).

-define(COLLECT_TIMEOUT, 10000).


%% purpose of this collect function is to not hang, remember a probable
%% reason for running debug-dump is that the system is in a
%% corrupt state.

collect(F, Sock, User) ->
    SELF = self(),
    Pid = spawn(fun() ->
                        F(Sock),
                        SELF ! {self(), ok},
                        timer:sleep(infinity)
                end),
    Ref = erlang:monitor(process, Pid),
    receive
        {Pid, ok} ->
            erlang:demonitor(Ref),
            exit(Pid, shutdown),
            ok;
        Down = {'DOWN', Ref, _,_,_} ->
            sock_format(Sock, "*** Failed to collect ~s: ~p~n", [User, Down]),
            ok
    after ?COLLECT_TIMEOUT ->
            erlang:demonitor(Ref),
            sock_format(Sock, "*** Failed to collect ~s: timeout~n", [User]),
            {pid, Pid}  % Let it hang for proc status, exit after
    end.

send_status(Sock) ->
    {InitStatus, _} = init:get_status(),
    sock_format(Sock, "vsn: ~s\n", [yaws_generated:version()]),
    sock_format(Sock, "status: ~p\n", [InitStatus]),
    ok.

send_inet(Sock) ->
    Chars = capture_io(fun() -> inet:i() end),
    sock_format(Sock, "inet:i() output ~n~s~n", [Chars]),
    ok.


%% This function runs a Fun that is producing IO through
%% io:format() and collects the IO and retuns the IO as a char list
%% Returns io_list() | {timeout, io_list()}
%%
capture_io(Fun) ->
    do_capture_io(Fun).

%% capture_io(Fun, MilliSecTimeout) ->
%%    {ok, Tref} = timer:send_after(MilliSecTimeout, capio_timeout),
%%    Chars = do_capture_io(Fun),
%%    timer:cancel(Tref),
%%    Chars.

do_capture_io(Fun) ->
    Pid = spawn(fun() ->
                        receive run -> ok end,
                        Fun()
                end),
    Mref = erlang:monitor(process,Pid),
    group_leader(self(), Pid),
    Pid ! run,
    collect_io(Pid, Mref, []).

collect_io(Pid, Mref, Ack) ->
    receive
        {'DOWN', Mref, _,_,_} ->
            Ack;
        {io_request, From, Me, {put_chars, M, F, A}} ->
            From ! {io_reply, Me, ok},
            collect_io(Pid, Mref, [Ack, apply(M,F, A)]);
        {io_request, From, Me, {put_chars, unicode, M, F, A}} ->
            From ! {io_reply, Me, ok},
            collect_io(Pid, Mref, [Ack, apply(M,F, A)]);
        capio_timeout ->
            {timeout, Ack}
    end.






