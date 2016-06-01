%%----------------------------------------------------------------------
%%% File    : yaws_log.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose :
%%% Created : 26 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_log).
-author('klacke@hyber.org').
-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/inet.hrl").


-behaviour(gen_server).

%% External exports
-export([start_link/0, reopen_logs/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% API
-export([accesslog/6,
         setup/2,
         authlog/4,
         rotate/1,
         add_sconf/1,
         del_sconf/1]).

%% yaws_logger callbacks
-export([
         open_log/3,
         close_log/3,
         wrap_log/4,
         write_log/4
        ]).


-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").

%% 1 meg log we wrap
-define(WRAP_LOG_SIZE, 1000000).


-record(state, {running,
                dir,
                now,
                log_wrap_size = ?WRAP_LOG_SIZE,
                copy_errlog,
                resolve_hostnames = false}).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, yaws_log}, yaws_log, [], []).

setup(GC, Sconfs) ->
    gen_server:call(?MODULE, {setup, GC, Sconfs}, infinity).

add_sconf(SConf) ->
    gen_server:call(yaws_log, {soft_add_sc, SConf}, infinity).

del_sconf(SConf) ->
    gen_server:call(yaws_log, {soft_del_sc, SConf}, infinity).

accesslog(SConf, Ip, Req, InH, OutH, Time) ->
    catch yaws_logger:accesslog(SConf, Ip, Req, InH, OutH, Time).

authlog(SConf, IP, Path, Item) ->
    catch yaws_logger:authlog(SConf, IP, Path, Item).

rotate(Res) ->
    gen_server:cast(?MODULE, {yaws_hupped, Res}).

%% Useful for embeddded yaws when we don't want yaws to
%% automatically wrap the logs.
reopen_logs() ->
    {ok, _GC, SCs} = yaws_api:getconf(),
    gen_server:call(?MODULE, {reopen, SCs}).


%%%----------------------------------------------------------------------
%%% Callback functions from yaws_logger
%%%----------------------------------------------------------------------
open_log(ServerName, Type, Dir) ->
    FileName = case os:type() of
                   {win32,_ } ->
                       lists:map(fun($:) -> $.;
                                    (C ) -> C
                                 end, ServerName);
                   _ ->
                       ServerName
               end,
    A = filename:join([Dir, FileName ++ "." ++ atom_to_list(Type)]),
    case file:open(A, [write, raw, append]) of
        {ok, Fd} ->
            {true, {Fd, A}};
        _Err ->
            error_logger:format("Cannot open ~p",[A]),
            false
    end.

close_log(_ServerName, _Type, {Fd, _FileName}) ->
    file:close(Fd).

wrap_log(_ServerName, _Type, {Fd, FileName}, LogWrapSize) ->
    case wrap_p(FileName, LogWrapSize) of
        true ->
            file:close(Fd),
            Old = [FileName, ".old"],
            file:delete(Old),
            file:rename(FileName, Old),
            {ok, Fd2} = file:open(FileName, [write, raw]),
            {Fd2, FileName};
        false ->
            {Fd, FileName};
        enoent ->
            %% Logfile disappeared,
            error_logger:format("Logfile ~p disappeared - we reopen it",
                                [FileName]),
            file:close(Fd),
            {ok, Fd2} = file:open(FileName, [write, raw]),
            {Fd2, FileName}
    end.

write_log(ServerName, Type, {Fd, _FileName}, Infos) ->
    gen_server:cast(yaws_log, {ServerName, Type, Fd, Infos}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    ets:new(yaws_log, [named_table, set, protected, {keypos, 2}]),
    {ok, #state{running = false, now = fmtnow()}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({setup, GC, Sconfs}, _From, State)
  when State#state.running == false ->
    Dir = GC#gconf.logdir,
    ?Debug("setup ~s~n", [Dir]),
    ElogFile = filename:join([Dir, "report.log"]),
    Copy = if ?gc_has_copy_errlog(GC) ->
                   gen_event:add_handler(error_logger, yaws_log_file_h,
                                         ElogFile),
                   true;
              true ->
                   false
           end,
    SCs = lists:flatten(Sconfs),
    lists:foreach(fun(SC) ->
                          yaws_logger:open_log(SC, auth, Dir),
                          yaws_logger:open_log(SC, access, Dir)
                  end, SCs),

    S2 = State#state{running = true,
                     dir  = Dir,
                     now = fmtnow(),
                     log_wrap_size = GC#gconf.log_wrap_size,
                     copy_errlog = Copy,
                     resolve_hostnames = ?gc_log_has_resolve_hostname(GC)},

    yaws:ticker(3000, secs3),

    if is_integer(GC#gconf.log_wrap_size) ->
            yaws:ticker(10 * 60 * 1000, minute10);
       true ->
            ok
    end,

    {reply, ok, S2};



%% We can't ever change logdir, we can however
%% change logging opts for various servers

handle_call({setup, GC, Sconfs}, _From, State)
  when State#state.running == true ->

    Dir = State#state.dir,
    ElogFile = filename:join([Dir, "report.log"]),
    Copy = if ?gc_has_copy_errlog(GC), State#state.copy_errlog == false->
                   gen_event:add_handler(error_logger, yaws_log_file_h,
                                         ElogFile),
                   true;
              ?gc_has_copy_errlog(GC) ->
                   true;
              State#state.copy_errlog == true ->
                   gen_event:delete_handler(error_logger, yaws_log_file_h,
                                            normal),
                   false;
              true ->
                   false
           end,

    %% close all files
    yaws_logger:close_logs(),

    %% reopen logfiles
    SCs = lists:flatten(Sconfs),
    lists:foreach(fun(SC) ->
                          yaws_logger:open_log(SC, auth, Dir),
                          yaws_logger:open_log(SC, access, Dir)
                  end, SCs),

    S2 = State#state{running = true,
                     dir  = Dir,
                     now = fmtnow(),
                     log_wrap_size = GC#gconf.log_wrap_size,
                     copy_errlog = Copy,
                     resolve_hostnames = ?gc_log_has_resolve_hostname(GC)},

    if
        not is_integer(State#state.log_wrap_size),
        is_integer(GC#gconf.log_wrap_size) ->
            yaws:ticker(10 * 60 * 1000, minute10);
       true ->
            ok
    end,
    {reply, ok, S2};


%% a virt server has been added
handle_call({soft_add_sc, SC}, _From, State) ->
    yaws_logger:open_log(SC, auth, State#state.dir),
    yaws_logger:open_log(SC, access, State#state.dir),
    {reply, ok, State};

%% a virt server has been deleted
handle_call({soft_del_sc, SC}, _From, State) ->
    yaws_logger:close_log(SC, auth),
    yaws_logger:close_log(SC, access),
    {reply, ok, State};


handle_call(state, _From, State) ->
    {reply, State, State};

handle_call({reopen, Sconfs}, _From, State) ->
    Dir = State#state.dir,
    %% close all files
    yaws_logger:close_logs(),

    %% reopen logfiles
    SCs = lists:flatten(Sconfs),
    lists:foreach(fun(SC) ->
                          yaws_logger:open_log(SC, auth, Dir),
                          yaws_logger:open_log(SC, access, Dir)
                  end, SCs),
    {reply, ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast({_ServerName, access, Fd, {Ip, Req, InH, OutH, _}}, State) ->
    case State#state.running of
        true ->
            Status = case OutH#outh.status of
                         undefined -> "-";
                         S         -> integer_to_list(S)
                     end,
            Len = case Req#http_request.method of
                      'HEAD' ->
                          "-";
                      _ ->
                          case OutH#outh.contlen of
                              undefined ->
                                  case OutH#outh.act_contlen of
                                      undefined -> "-";
                                      L         -> integer_to_list(L)
                                  end;
                              L ->
                                  integer_to_list(L)
                          end
                  end,
            Ver = case Req#http_request.version of
                      {1,0} -> "HTTP/1.0";
                      {1,1} -> "HTTP/1.1";
                      {0,9} -> "HTTP/0.9";
                      _     -> "HTTP/X.X"
                  end,

            Path      = yaws_server:safe_path(Req#http_request.path),
            Meth      = yaws:to_list(Req#http_request.method),
            Referer   = optional_header(InH#headers.referer),
            UserAgent = optional_header(InH#headers.user_agent),
            User      = case InH#headers.authorization of
                            {U, _P, _OStr} -> U;
                            _              -> "-"
                        end,

            Msg = fmt_access_log(State#state.now, fmt_ip(Ip, State), User,
                                 [Meth, $\s, Path, $\s, Ver],
                                 Status,  Len, Referer, UserAgent),
            file:write(Fd, safe_log_data(Msg)),
            {noreply, State};
        false ->
            {noreply, State}
    end;

handle_cast({ServerName, auth, Fd, {Ip, Path, Item}}, State) ->
    case State#state.running of
        true ->
            Host = fmt_ip(Ip, State),
            Msg  = [Host, " ", State#state.now, " ", ServerName, " " ,
                    "\"", Path,"\"",
                   case Item of
                       {ok, User}       -> [" OK user=", User];
                       403              -> [" 403"];
                       {401, Realm}     -> [" 401 realm=", Realm];
                       {401, User, PWD} -> [" 401 user=", User, " badpwd=",PWD];
                       _                -> ""
                   end, "\n"],
            file:write(Fd, safe_log_data(Msg)),
            {noreply, State};
        false ->
            {noreply,State}
    end;

handle_cast({yaws_hupped, _}, State) ->
    handle_info(minute10, State).


%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(secs3, State) ->
    {noreply, State#state{now = fmtnow()}};

%% once every 10 minutes, check log sizes
handle_info(minute10, State) ->
    yaws_logger:rotate(State#state.log_wrap_size),

    case gen_event:call(error_logger, yaws_log_file_h, size, infinity) of
        {ok, Size} when  State#state.log_wrap_size > 0,
                       Size > State#state.log_wrap_size ->
            gen_event:call(error_logger, yaws_log_file_h, wrap, infinity);
        {error, enoent} ->
            gen_event:call(error_logger, yaws_log_file_h, reopen, infinity);
        _ ->
            ok
    end,
    {noreply, State};
handle_info({'EXIT', _, _}, State) ->
    {noreply, State}.



wrap_p(Filename, LogWrapSize) ->
    case file:read_file_info(Filename) of
        {ok, FI} when FI#file_info.size > LogWrapSize, LogWrapSize > 0 ->
            true;
        {ok, _FI} ->
            false;
        {error, enoent} ->
            enoent;
        _ ->
            false
    end.



%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    gen_event:delete_handler(error_logger, yaws_log_file_h, normal),
    yaws_logger:close_logs(),
    ok.


%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Handle upgrade
%% Returns: new State data
%%----------------------------------------------------------------------
code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
optional_header(Item) ->
    case Item of
        undefined -> "-";
        Item -> Item
    end.

fmt_access_log(Time, Host, User, Req, Status,  Length, Referrer, UserAgent) ->
    [Host, " - ", User, [$\s], Time, [$\s, $\"], no_ctl(Req), [$\",$\s],
     Status, [$\s], Length, [$\s,$"], Referrer, [$",$\s,$"], UserAgent,
     [$",$\n]].


%% Odd security advisory that only affects webservers where users are
%% somehow allowed to upload files that later can be downloaded.

no_ctl([H|T]) when H < 32 ->
    no_ctl(T);
no_ctl([H|T]) ->
    [H|no_ctl(T)];
no_ctl([]) ->
    [].


fmt_ip(IP, State) when is_tuple(IP) ->
    case State#state.resolve_hostnames of
        true ->
            case catch inet:gethostbyaddr(IP) of
                {ok, HE} ->
                    HE#hostent.h_name;
                _ ->
                    case catch inet_parse:ntoa(IP) of
                        {'EXIT', _} -> "unknownip";
                        Addr        -> Addr
                    end
            end;
        false ->
            case catch inet_parse:ntoa(IP) of
                {'EXIT', _} -> "unknownip";
                Addr        -> Addr
            end
    end;
fmt_ip(unknown, _) ->
    "unknownip";
fmt_ip(undefined, _) ->
    "0.0.0.0";
fmt_ip(HostName, _) ->
    HostName.


fmtnow() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:now_to_local_time(yaws:get_time_tuple()),
    ["[",fill_zero(Day,2),"/",yaws:month(Month),"/",integer_to_list(Year),":",
     fill_zero(Hour,2),":",fill_zero(Min,2),":",
     fill_zero(Sec,2)," ",zone(),"]"].


zone() ->
    Time = erlang:universaltime(),
    LocalTime = calendar:universal_time_to_local_time(Time),
    DiffSecs = calendar:datetime_to_gregorian_seconds(LocalTime) -
        calendar:datetime_to_gregorian_seconds(Time),
    zone(DiffSecs div 3600, (DiffSecs rem 3600) div 60).



zone(Hr, Min) when Hr < 0; Min < 0 ->
    [$-, fill_zero(abs(Hr), 2), fill_zero(abs(Min), 2)];
zone(Hr, Min) when Hr >= 0, Min >= 0 ->
    [$+, fill_zero(abs(Hr), 2), fill_zero(abs(Min), 2)].

fill_zero(N, Width) ->
    left_fill(N, Width, $0).

left_fill(N, Width, Fill) when is_integer(N) ->
    left_fill(integer_to_list(N), Width, Fill);
left_fill(N, Width, _Fill) when length(N) >= Width ->
    N;
left_fill(N, Width, Fill) ->
    left_fill([Fill|N], Width, Fill).

safe_log_data(Elements) ->
    [ yaws:to_string(E) || E <- Elements ].



