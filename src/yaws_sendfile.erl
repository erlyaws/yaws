%%% File    : yaws_sendfile.erl
%%% Author  : Steve Vinoski <vinoski@ieee.org>
%%% Description : interface to sendfile linked-in driver for Yaws
%%% Created :  9 Nov 2008 by Steve Vinoski <vinoski@ieee.org>

-module(yaws_sendfile).
-author('vinoski@ieee.org').

-include("../include/yaws.hrl").
-include_lib("kernel/include/file.hrl").

-export([send/2, send/3, send/4]).
-export([have_sendfile/0, have_erlang_sendfile/0, check_gc_flags/1]).

%% export bytes_to_transfer to avoid warning when sendfile is disabled (or not
%% supported)
-export([bytes_to_transfer/3]).


-ifdef(HAVE_SENDFILE).

-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([start_link/0, stop/0]).

have_sendfile() -> true.

-else.

have_sendfile() -> false.

-endif.

have_erlang_sendfile() -> yaws_dynopts:have_erlang_sendfile().

check_gc_flags(GC) ->
    %% below, ignore dialyzer warning:
    %% The pattern depends on the macro HAVE_ERLANG_SENDFILE
    case have_erlang_sendfile() of
        false when ?gc_use_erlang_sendfile(GC) ->
            error_logger:error_msg("Cannot use file:sendfile/5: not supported, "
                                   "gen_tcp:send/2 will be used instead.~n",
                                   []);
        _ ->
            ok
    end,

    %% below, ignore dialyzer warning:
    %% The pattern depends on the macro HAVE_SENDFILE
    case have_sendfile() of
        false when ?gc_use_yaws_sendfile(GC) ->
            error_logger:error_msg("Cannot use Yaws sendfile linked-in driver:"
                                   " not supported, gen_tcp:send/2 will be used"
                                   " instead.~n",
                                   []);
        _ ->
            ok
    end.


send(Out, Filename) ->
    send(Out, Filename, 0, all).
send(Out, Filename, Offset) ->
    send(Out, Filename, Offset, all).

send(Out, Filename, Offset, Count) ->
    GC             = get(gc),
    ChunkSize      = GC#gconf.large_file_chunk_size,
    ErlangSendFile = ?gc_use_erlang_sendfile(GC),
    YawsSendFile   = ?gc_use_yaws_sendfile(GC),
    if
        ErlangSendFile ->
            erlang_sendfile(Out, Filename, Offset, Count, ChunkSize);
        YawsSendFile ->
            yaws_sendfile(Out, Filename, Offset, Count, ChunkSize);
        true ->
            compat_send(Out, Filename, Offset, Count, ChunkSize)
    end.


bytes_to_transfer(Filename, Offset, Count) ->
    case Count of
        all ->
            case file:read_file_info(Filename) of
                {ok, #file_info{size = Size}} -> Size - Offset;
                Error -> Error
            end;
        Count when is_integer(Count) ->
            Count;
        _ ->
            {error, badarg}
    end.


erlang_sendfile(Out, Filename, Offset, Count, ChunkSize) ->
    case have_erlang_sendfile() of
        true ->
            Count1 = bytes_to_transfer(Filename, Offset, Count),
            case Count1 of
                {error, _}=Error1 ->
                    Error1;
                _ ->
                    case file:open(Filename, [raw, read, binary]) of
                        {ok, RawFile} ->
                            Res = file:sendfile(RawFile, Out, Offset, Count1,
                                                [{chunk_size, ChunkSize}]),
                            ok = file:close(RawFile),
                            Res;
                        Error2 ->
                            Error2
                    end
            end;
        false ->
            compat_send(Out, Filename, Offset, Count, ChunkSize)
     end.


-ifdef(HAVE_SENDFILE).

yaws_sendfile(Out, Filename, Offset, Count, ChunkSize) ->
    Count1 = bytes_to_transfer(Filename, Offset, Count),
    case Count1 of
        {error, _}=Error ->
            Error;
        _ ->
            case prim_inet:getfd(Out) of
                {ok, SocketFd} ->
                    do_send(Out, SocketFd, Filename, Offset, Count1, ChunkSize);
                Error2 ->
                    Error2
            end
    end.


-record(state, {
          port,                    % driver port
          caller_tbl               % table mapping socket fd to caller
         }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init([]) ->
    process_flag(trap_exit, true),
    Shlib = "yaws_sendfile_drv",
    Dir   = filename:join(yaws:get_priv_dir(), "lib"),
    case erl_ddll:load_driver(Dir, Shlib) of
        ok -> ok;
        {error, already_loaded} -> ok;
        _ -> exit({error, "could not load driver " ++ Shlib})
    end,
    Port = open_port({spawn, Shlib}, [binary]),
    CallerTable = ets:new(yaws_sendfile, []),
    {ok, #state{port = Port, caller_tbl = CallerTable}}.

handle_call({send, SocketFd, Msg}, From, State) ->
    true = erlang:port_command(State#state.port, Msg),
    true = ets:insert(State#state.caller_tbl, {SocketFd, From}),
    {noreply, State};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_info({_, {data, <<Cnt:64, SocketFd:32, Res:8, Err/binary>>}}, State) ->
    Reply = case Res of
                1 ->
                    {ok, Cnt};
                0 ->
                    {error,
                     list_to_atom(
                       lists:takewhile(fun(El) -> El =/= 0 end,
                                       binary_to_list(Err)))}
            end,
    CallerTable = State#state.caller_tbl,
    [{SocketFd, From}] = ets:lookup(CallerTable, SocketFd),
    gen_server:reply(From, Reply),
    ets:delete(CallerTable, SocketFd),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, State};
handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port, caller_tbl = CallerTable}) ->
    erlang:port_close(Port),
    receive {'EXIT', Port, _Reason} -> ok
    after 0 -> ok
    end,
    ets:delete(CallerTable),
    ok.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

do_send(_Out, _SocketFd, _Filename, _Offset, Count, _) when Count =< 0 ->
    {ok, 0};
do_send(Out, SocketFd, Filename0, Offset, Count, ChunkSize) ->
    Filename = case file:native_name_encoding() of
                   latin1 -> Filename0;
                   utf8 -> unicode:characters_to_binary(Filename0)
               end,
    Call = list_to_binary([<<Offset:64, Count:64, SocketFd:32>>,
                           Filename, <<0:8>>]),
    case gen_server:call(?MODULE, {send, SocketFd, Call}, infinity) of
        {error, eoverflow} ->
            compat_send(Out, Filename, Offset, Count, ChunkSize);
        Else ->
            Else
    end.

-else.

yaws_sendfile(Out, Filename, Offset, Count, ChunkSize) ->
    compat_send(Out, Filename, Offset, Count, ChunkSize).

-endif.



compat_send(Out, Filename, Offset, Count0, ChunkSize) ->
    Count = case Count0 of
                0 -> all;
                _ -> Count0
            end,
    case file:open(Filename, [read, binary, raw]) of
        {ok, Fd} ->
            file:position(Fd, {bof, Offset}),
            Ret = loop_send(Fd, ChunkSize, file:read(Fd, ChunkSize), Out,
                            Count, 0),
            file:close(Fd),
            Ret;
        Err ->
            Err
    end.

loop_send(Fd, ChunkSize, {ok, Bin}, Out, all, BytesSent) ->
    case gen_tcp:send(Out, Bin) of
        ok ->
            loop_send(Fd, ChunkSize, file:read(Fd, ChunkSize), Out, all,
                      BytesSent+size(Bin));
        Err ->
            Err
    end;
loop_send(_Fd, _ChunkSize, eof, _Out, _, BytesSent) ->
    {ok, BytesSent};
loop_send(Fd, ChunkSize, {ok, Bin}, Out, Count, BytesSent) ->
    Sz = size(Bin),
    if Sz < Count ->
            case gen_tcp:send(Out, Bin) of
                ok ->
                    loop_send(Fd, ChunkSize, file:read(Fd, ChunkSize),
                              Out, Count-Sz, BytesSent+Sz);
                Err ->
                    Err
            end;
       Sz == Count ->
            case gen_tcp:send(Out, Bin) of
                ok  -> {ok, BytesSent+Sz};
                Err -> Err
            end;
       Sz > Count ->
            <<Deliver:Count/binary , _/binary>> = Bin,
            case gen_tcp:send(Out, Deliver) of
                ok  -> {ok, BytesSent+Count};
                Err -> Err
            end
    end;
loop_send(_Fd, _, Err, _, _, _) ->
    Err.
