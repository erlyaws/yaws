%%% File    : yaws_sendfile.erl
%%% Author  : Steve Vinoski <vinoski@ieee.org>
%%% Description : interface to sendfile linked-in driver for Yaws
%%% Created :  9 Nov 2008 by Steve Vinoski <vinoski@ieee.org>

-module(yaws_sendfile).
-author('vinoski@ieee.org').

-export([start_link/0, start/0, stop/0,
         enabled/0, send/2, send/3, send/4]).

-include("yaws_configure.hrl").
-include("../include/yaws.hrl").

-ifdef(HAVE_SENDFILE).

-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include_lib("kernel/include/file.hrl").

-endif.

send(Out, Filename) ->
    send(Out, Filename, 0, all).
send(Out, Filename, Offset) ->
    send(Out, Filename, Offset, all).

-ifdef(HAVE_SENDFILE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

enabled() ->
    true.

send(Out, Filename, Offset, Count) ->
    Count1 = case Count of
                 all ->
                     case file:read_file_info(Filename) of
                         {ok, #file_info{size = Size}} ->
                             Size - Offset;
                         Error ->
                             Error
                     end;
                 Count when is_integer(Count) ->
                     Count;
                 _ ->
                     {error, badarg}
             end,
    case Count1 of
        {error, _}=Error2 ->
            Error2;
        _ ->
            case prim_inet:getfd(Out) of
                {ok, SocketFd} ->
                    do_send(Out, SocketFd, Filename, Offset, Count1);
                Error3 ->
                    Error3
            end
    end.

stop() ->
    gen_server:cast(?MODULE, stop).

-record(state, {
          port,                    % driver port
          caller_tbl               % table mapping socket fd to caller
         }).

init([]) ->
    process_flag(trap_exit, true),
    Shlib = "yaws_sendfile_drv",
    Dir = case yaws_generated:is_local_install() of
	      true ->
		  filename:dirname(code:which(?MODULE)) ++ "/../priv/lib";
              false ->
		  %% ignore dialyzer on this one
		  PrivDir = code:priv_dir(yaws),
		  filename:join(PrivDir,"lib")
	  end,
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

do_send(_Out, _SocketFd, _Filename, _Offset, Count) when Count =< 0 ->
    {ok, 0};
do_send(Out, SocketFd, Filename, Offset, Count) ->
    Call = list_to_binary([<<Offset:64, Count:64, SocketFd:32>>,
                           Filename, <<0:8>>]),
    case gen_server:call(?MODULE, {send, SocketFd, Call}, infinity) of
        {error, eoverflow} ->
            compat_send(Out, Filename, Offset, Count);
        Else ->
            Else
    end.

-else.

enabled() ->
    false.
start_link() ->
    ignore.
start() ->
    ignore.
stop() ->
    ok.
send(Out, Filename, Offset, Count) ->
    compat_send(Out, Filename, Offset, Count).

-endif.

compat_send(Out, Filename, Offset, Count0) ->
    Count = case Count0 of
                0 -> all;
                _ -> Count0
            end,
    case file:open(Filename, [read, binary, raw]) of
        {ok, Fd} ->
            file:position(Fd, {bof, Offset}),
            ChunkSize = (get(gc))#gconf.large_file_chunk_size,
            Ret = loop_send(Fd, ChunkSize, file:read(Fd, ChunkSize), Out, Count),
            file:close(Fd),
            Ret;
        Err ->
            Err
    end.

loop_send(Fd, ChunkSize, {ok, Bin}, Out, all) ->
    case gen_tcp:send(Out, Bin) of
        ok ->
            loop_send(Fd, ChunkSize, file:read(Fd, ChunkSize), Out, all);
        Err ->
            Err
    end;
loop_send(_Fd, _ChunkSize, eof, _Out, _) ->
    ok;
loop_send(Fd, ChunkSize, {ok, Bin}, Out, Count) ->
    Sz = size(Bin),
    if Sz < Count ->
            case gen_tcp:send(Out, Bin) of
                ok ->
                    loop_send(Fd, ChunkSize, file:read(Fd, ChunkSize),
                              Out, Count-Sz);
                Err ->
                    Err
            end;
       Sz == Count ->
            gen_tcp:send(Out, Bin);
       Sz > Count ->
            <<Deliver:Count/binary , _/binary>> = Bin,
            gen_tcp:send(Out, Deliver)
    end;
loop_send(_Fd, _, Err, _,_) ->
    Err.
