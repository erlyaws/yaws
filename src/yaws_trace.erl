-module(yaws_trace).
-author('christopher@yakaz.com').

-behaviour(gen_server).

-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").

%% API
-export([
         start_link/0,
         setup/1,
         set_tty_trace/1,
         get_type/1,
         open/0, open/1,
         write/2, write/3,
         disable_trace/1, enable_trace/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {tracedir,
                type      = false,
                traces    = dict:new(),
                tty_trace = false}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


setup(GC) ->
    gen_server:cast(?MODULE, {setup, GC}).


set_tty_trace(Bool) ->
    gen_server:cast(?MODULE, {tty_trace, Bool}).


get_type(#gconf{trace={true, Type}}) ->
    Type;
get_type(_) ->
    undefined.



open() ->
    open(self()).
open(Owner) ->
    gen_server:cast(?MODULE, {open, Owner}).


write(Tag, Data) ->
    write(self(), Tag, Data).
write(Pid, Tag, Data) ->
    gen_server:cast(?MODULE, {write, Pid, Tag, Data}).


disable_trace(infinity) ->
    {ok, GC, Groups} = yaws_server:getconf(),
    case GC#gconf.trace of
        false ->
            error_logger:info_msg("Trace already disabled~n", []);
        {true, _} ->
            yaws_api:setconf(GC#gconf{trace=false}, Groups),
            error_logger:info_msg("Trace disabled~n", [])
    end;
disable_trace(Time) ->
    {ok, GC, Groups} = yaws_server:getconf(),
    case GC#gconf.trace of
        false ->
            error_logger:info_msg("Trace already disabled~n", []);
        {true, CurType} ->
            yaws_api:setconf(GC#gconf{trace=false}, Groups),
            timer:apply_after(Time, ?MODULE, enable_trace, [CurType, infinity]),
            error_logger:info_msg("Trace disabled for ~w milliseconds~n",[Time])
    end.


enable_trace(Type, infinity) when Type =:= http; Type =:= traffic ->
    {ok, GC, Groups} = yaws_server:getconf(),
    case GC#gconf.trace of
        {true, Type} ->
            error_logger:info_msg("~p trace already enabled~n", [Type]);
        {true, _} ->
            yaws_api:setconf(GC#gconf{trace={true, Type}}, Groups),
            error_logger:info_msg("~p trace enabled~n", [Type]);
        false ->
            yaws_api:setconf(GC#gconf{trace={true, Type}}, Groups),
            error_logger:info_msg("~p trace enabled~n", [Type])
    end;
enable_trace(Type, Time) when Type =:= http; Type =:= traffic ->
    {ok, GC, Groups} = yaws_server:getconf(),
    case GC#gconf.trace of
        {true, Type} ->
            error_logger:info_msg("~p trace already enabled~n", [Type]);
        {true, CurType} ->
            yaws_api:setconf(GC#gconf{trace={true, Type}}, Groups),
            timer:apply_after(Time, ?MODULE, enable_trace, [CurType, infinity]),
            error_logger:info_msg("~p trace enabled for ~w milliseconds~n",
                                  [Type, Time]);
        false ->
            yaws_api:setconf(GC#gconf{trace={true, Type}}, Groups),
            timer:apply_after(Time, ?MODULE, disable_trace, [infinity]),
            error_logger:info_msg("~p trace enabled for ~w milliseconds~n",
                                  [Type, Time])
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.


%% ----
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% ----
handle_cast({setup, #gconf{trace=false}=GC}, State) ->
    Bool = ?gc_has_tty_trace(GC),
    {noreply, State#state{type=false, tty_trace=Bool, tracedir=undefined}};
handle_cast({setup, GC}, State) ->
    {true, Type} = GC#gconf.trace,
    Bool = ?gc_has_tty_trace(GC),
    Dir  = filename:join(GC#gconf.logdir, subdir()),
    case file:make_dir(Dir) of
        ok ->
            error_logger:info_msg("Trace directory ~p created~n", [Dir]),
            {noreply, State#state{type=Type, tty_trace=Bool, tracedir=Dir}};
        {error, Reason} ->
            error_logger:error_msg("Failed to create  trace directory ~p: ~p~n",
                                   [Dir, file:format_error(Reason)]),
            {noreply, State#state{type=false, tty_trace=Bool}}
    end;

handle_cast({tty_trace, Bool}, State) ->
    {noreply, State#state{tty_trace=Bool}};

handle_cast({open, _}, #state{type=false}=State) ->
    {noreply, State};
handle_cast({open, Owner}, State) ->
    Type = State#state.type,
    case dict:find(Owner, State#state.traces) of
        {ok, {Type, _Fd}} ->
            {noreply, State};
        {ok, {_OtherType, OldFd}} ->
            file:close(OldFd),
            case open_trace(State#state.tracedir, filename(Type, Owner)) of
                {ok, NewFd} ->
                    erlang:monitor(process, Owner),
                    D = dict:store(Owner, {Type,NewFd}, State#state.traces),
                    {noreply, State#state{traces=D}};
                error ->
                    D = dict:erase(Owner, State#state.traces),
                    {noreply, State#state{traces=D}}
            end;
        error ->
            case open_trace(State#state.tracedir, filename(Type, Owner)) of
                {ok, NewFd} ->
                    erlang:monitor(process, Owner),
                    D = dict:store(Owner, {Type,NewFd}, State#state.traces),
                    {noreply, State#state{traces=D}};
                error ->
                    {noreply, State}
            end
    end;

handle_cast({write, _, _, _}, #state{type=false}=State) ->
    {noreply, State};
handle_cast({write, Pid, Tag, Data}, State) ->
    TTYTrace = State#state.tty_trace,
    case dict:find(Pid, State#state.traces) of
        {ok, {_Type, Fd}} -> write_trace(Pid, Fd, Tag, Data, TTYTrace);
        error             -> ok
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


%% ----
handle_info({'DOWN', _MRef, _Type, Pid, _Info}, State) ->
    case dict:find(Pid, State#state.traces) of
        {ok, {_, Fd}} ->
            file:close(Fd),
            D = dict:erase(Pid, State#state.traces),
            {noreply, State#state{traces=D}};
        error ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.


%% ----
terminate(_Reason, State) ->
    [file:close(Fd) || {_Owner, Fd} <- dict:to_list(State#state.traces)],
    ok.


%% ----
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================
subdir() ->
    {{Y,M,D}, {HH,MM,SS}} = erlang:localtime(),
    lists:flatten(
      io_lib:format("trace_~4.10.0B~2.10.0B~2.10.0B_~2.10.0B~2.10.0B~2.10.0B",
                    [Y,M,D,HH,MM,SS])
     ).

filename(Type, Owner) ->
    lists:concat(["trace.", pid_to_list(Owner), ".", Type]).


open_trace(Dir, File) ->
    case file:open(filename:join([Dir, File]), [write, raw]) of
        {ok, Fd} ->
            Date = iso_8601_fmt(),
            Str  = ["=====\n===== TRACE STARTED ", Date, "\n=====\n\n"],
            file:write(Fd, Str),
            {ok, Fd};
        {error, Reason} ->
            error_logger:error_msg("Failed to open trace ~p: ~p~n",
                                   [File, file:format_error(Reason)]),
            error
    end.


write_trace(Pid, Fd, from_server, Data, TTYTrace) ->
    Date = iso_8601_fmt(),
    Head = io_lib:format("~n[~s] ===== SRV -> CLI =====~n", [Date]),
    write_trace(Pid, Fd, [Head, Data], TTYTrace);
write_trace(Pid, Fd, from_client, Data, TTYTrace) ->
    Date = iso_8601_fmt(),
    Head = io_lib:format("~n[~s] ===== CLI -> SRV =====~n", [Date]),
    write_trace(Pid, Fd, [Head, Data], TTYTrace).


write_trace(Pid, Fd, Str, true) ->
    file:write(Fd, Str),
    io:format("Worker: ~p ~s~n", [Pid, flatten([Str])]);
write_trace(_Pid, Fd, Str, false) ->
    file:write(Fd, Str).


iso_8601_fmt() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
    {_,_,MicroSec} = now(),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B.~w",
                  [Year, Month, Day, Hour, Min, Sec, (MicroSec div 1000)]).


flatten(L) ->
    binary_to_list(iolist_to_binary(L)).
