%%----------------------------------------------------------------------
%%% File    : yaws_log.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 26 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_log).
-author('klacke@hyber.org').
-include_lib("kernel/include/file.hrl").


-behaviour(gen_server).

%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
         handle_event/2]).
-export([accesslog/5,
         accesslog/7,
         accesslog/8,
         setdir/2,
         open_trace/1,
         trace_traffic/2,
         authlog/4,
         actl_trace/1]).


-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").

%% 1 meg log we wrap
-define(WRAP_LOG_SIZE, 1000000).


-record(state, {
          running,
          dir,
          now,
          tracefd,
          log_wrap_size = ?WRAP_LOG_SIZE,
          tty_trace = false,
          copy_errlog,
          auth_log,
          alogs =  []}).


-record(alog, {fd,
               servername,
               filename}).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, yaws_log}, yaws_log, [], []).

accesslog(ServerName, Ip, Req, Status, Length) ->
    accesslog(ServerName, Ip, "-", Req, Status, Length, "-", "-").

accesslog(ServerName, Ip, Req, Status, Length, Referrer, UserAgent) ->
    accesslog(ServerName, Ip, "-", Req, Status, Length, Referrer, UserAgent).

accesslog(ServerName, Ip, User, Req, Status, Length, Referrer, UserAgent) ->
    gen_server:cast(?MODULE, {access, ServerName, Ip, User, Req, 
                              Status, Length, Referrer, UserAgent}).
setdir(GC, Sconfs) ->
    gen_server:call(?MODULE, {setdir, GC, Sconfs}, infinity).

open_trace(What) ->
    gen_server:call(?MODULE, {open_trace, What}, infinity).
trace_traffic(ServerOrClient , Data) ->
    gen_server:cast(?MODULE, {trace, ServerOrClient, Data}).

authlog(ServerName, IP, Path, Item) ->
    gen_server:cast(?MODULE, {auth, ServerName, IP, Path, Item}).

%% from external ctl prog
actl_trace(What) ->
    gen_server:call(?MODULE, {actl_trace, What}, infinity).




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
handle_call({setdir, GC, Sconfs}, _From, State) 
  when State#state.running == false ->
    Dir = GC#gconf.logdir,
    ?Debug("setdir ~s~n", [Dir]),
    ElogFile = filename:join([Dir, "report.log"]),
    Copy = if ?gc_has_copy_errlog(GC) ->
                   gen_event:add_handler(error_logger, yaws_log_file_h, 
                                         ElogFile),
                   true;
              true ->
                   false
           end,
    SCs = lists:flatten(Sconfs),
    L = lists:zf(
          fun(SC) when ?sc_has_access_log(SC)  ->
                  FileName = case os:type() of
                                 {win32,_ } ->
                                     lists:map(fun($:) -> $.;
                                                              (C ) -> C
                                                       end,
                                               SC#sconf.servername);
                                 _ ->
                                     SC#sconf.servername
                             end,
                  A = filename:join([Dir, FileName ++ ".access"]),
                  case file:open(A, [write, raw, append]) of
                      {ok, Fd} ->
                          {true, #alog{servername = SC#sconf.servername,
                                       fd = Fd,
                                       filename = A}};
                      _Err ->
                          error_logger:format("Cannot open ~p~n",[A]),
                          false
                  end;
             (_) ->
                  false
          end, SCs),

    AuthLogFileName = filename:join([Dir, "auth.log"]),
    AuthLog = 
        if ?gc_has_auth_log(GC) ->
                case file:open(AuthLogFileName, [write, raw, append]) of
                    {ok, AFd} ->
                        #alog{fd = AFd,
                              servername = undefined,
                              filename = AuthLogFileName};
                    _Err ->
                        error_logger:format("Cannot open ~s~n", 
                                            [AuthLogFileName]),
                        undefined
                end;
           true ->
                undefined
        end,
    S2 = State#state{running = true,
                     dir  = Dir,
                     now = fmtnow(),
                     log_wrap_size = GC#gconf.log_wrap_size,
                     copy_errlog = Copy,
                     auth_log = AuthLog,
                     alogs = L},

    yaws:ticker(3000, secs3),
    yaws:ticker(10 * 60 * 1000, minute10),

    {reply, ok, S2};



%% We can't ever change logdir, we can however
%% change logging opts for various servers

handle_call({setdir, GC, Sconfs}, _From, State) 
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
    lists:map(
      fun(AL) ->
              file:close(AL#alog.fd)
      end, State#state.alogs),
    SCs = lists:flatten(Sconfs),

    %% reopen logfiles
    L = lists:zf(fun(SC) -> open_alog(Dir, SC) end,
                 SCs),
    S2 = State#state{running = true,
                     dir  = Dir,
                     now = fmtnow(),
                     copy_errlog = Copy,
                     alogs = L},

    {reply, ok, S2};


%% a virt server has been added
handle_call({soft_add_sc, SC}, _From, State) ->
    case open_alog(State#state.dir, SC) of
        {true, A} ->
            {reply, ok, State#state{alogs = [A|State#state.alogs]}};
        false ->
            {reply, ok, State}
    end;

%% a virt server has been deleted
handle_call({soft_del_sc, SC}, _From, State) ->
    case lists:keysearch(SC#sconf.servername, #alog.servername,
                         State#state.alogs) of
        {value, A} ->
            file:close(A#alog.fd),
            {reply,ok, State#state{alogs = State#state.alogs -- [A]}};
        false ->
            {reply,ok,State}
    end;


handle_call({open_trace, What}, _From, State) ->
    case State#state.tracefd of
        undefined ->
            ok;
        Fd0 ->
            file:close(Fd0)
    end,
    F = lists:concat(["trace.", What]),
    case file:open(filename:join([State#state.dir, F]),[write, raw]) of
        {ok, Fd} ->
            {reply, ok, State#state{tracefd = Fd}};
        Err ->
            {reply,  Err, State}
    end;


handle_call({trace_tty, What}, _From, State) ->
    {reply, ok, State#state{tty_trace = What}};
handle_call(state, _From, State) ->
    {reply, State, State}.


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast({access, ServerName, Ip, User, Req, Status, 
             Length, Referrer, UserAgent}, State) ->
    case State#state.running of 
        true ->
            do_alog(ServerName, Ip, User, Req, Status, Length, 
                    Referrer, UserAgent, State),
            {noreply, State};
        false ->
            {noreply, State}
    end;

handle_cast({auth, ServerName, IP, Path, Item}, State) ->
    case State#state.running of 
        true when State#state.auth_log /= undefined ->
            do_auth_log(ServerName, IP, Path, Item, State),
            {noreply, State};
        false ->
            {noreply,State}
    end;

handle_cast({trace, from_server, Data}, State) ->
    Str = ["*** SRV -> CLI *** ", Data],
    file:write(State#state.tracefd, Str),
    tty_trace(Str, State),
    {noreply,  State};
handle_cast({trace, from_client, Data}, State) ->
    Str = ["*** CLI -> SRV *** ", Data],
    file:write(State#state.tracefd, Str),
    tty_trace(Str, State),
    {noreply, State}.


open_alog(Dir, SC) when ?sc_has_access_log(SC) ->
    A = filename:join([Dir, SC#sconf.servername ++ ".access"]),
    case file:open(A, [write, raw, append]) of
        {ok, Fd} ->
            {true, #alog{servername = SC#sconf.servername,
                         fd = Fd,
                         filename = A}};
        _Err ->
            error_logger:format("Cannot open ~p",[A]),
            false
    end;
open_alog(_,_) ->
    false.



do_alog(ServerName, Ip, User, Req, Status, Length, Referrer,
        UserAgent, State) ->
    case lists:keysearch(ServerName, #alog.servername, State#state.alogs) of
        {value, AL} ->
            I = fmt_alog(State#state.now, Ip, User, Req, Status,  Length, 
                         Referrer, UserAgent),
            file:write(AL#alog.fd, I);
        _ ->
            false
    end.

do_auth_log(ServerName, IP, Path, Item, State) ->
    AL = State#state.auth_log,
    IpStr = case catch inet_parse:ntoa(IP) of
                {'EXIT', _} -> "unknownip";
                Val -> Val
            end,
    I = [IpStr,
         " ", State#state.now,
         " ", ServerName, " " ,"\"", Path,"\"",
         case Item of
             {ok, User} ->
                 [" OK user=", User];
             403 ->
                 [" 403"];
             {401, Realm} ->
                 [" 401 realm=", Realm];
             {401, User, PWD} ->
                 [" 401 user=", User, " badpwd=", PWD]
         end, "\n"],
    file:write(AL#alog.fd, I).



tty_trace(Str, State) ->
    case State#state.tty_trace of
        false ->
            ok;
        true ->
            io:format("~s", [binary_to_list(list_to_binary([Str]))])
    end.

%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: 
%%----------------------------------------------------------------------
handle_event({yaws_hupped, _Res}, State) ->
    handle_info(minute10,State),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.




%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(secs3, State) ->
    {noreply, State#state{now = fmtnow()}};

handle_info({notify,{yaws_hupped,_}}, State) ->
    handle_info(minute10,State);
%% once every 10  minute, check log sizes
handle_info(minute10, State) ->
    L = lists:map(
          fun(AL) ->
                  wrap(AL, State)
          end, State#state.alogs),

    Dir = State#state.dir,
    E = filename:join([Dir, "report.log"]),
    case file:read_file_info(E) of
        {ok, FI} when  State#state.log_wrap_size > 0, 
        FI#file_info.size > State#state.log_wrap_size, 
        State#state.copy_errlog == true ->
            gen_event:call(error_logger, yaws_log_file_h, wrap, infinity);
        {error,enoent} ->
            gen_event:call(error_logger, yaws_log_file_h, reopen, infinity);
        _ ->
            ok
    end,
    {noreply, State#state{alogs= L,
                          auth_log = wrap(State#state.auth_log, State)}}.



wrap_p(AL, State) when record(AL, alog) ->
    case file:read_file_info(AL#alog.filename) of
        {ok, FI} when FI#file_info.size > State#state.log_wrap_size,
        State#state.log_wrap_size > 0 ->
            true;
        {ok, _FI} ->
            false;
        {error, enoent} ->
            enoent;
        _ ->
            false
    end;
wrap_p(_,_) ->
    false.



wrap(AL, State) ->
    case wrap_p(AL, State) of
        true ->
            file:close(AL#alog.fd),
            Old = [AL#alog.filename, ".old"],
            file:delete(Old),
            file:rename(AL#alog.filename, Old),
            {ok, Fd2} = file:open(AL#alog.filename, [write, raw]),
            error_logger:info_msg("Wrap log ~p",[AL#alog.filename]), 
            AL#alog{fd = Fd2};
        false ->
            AL;
        enoent ->
            %% Logfile disapeared, 
            error_logger:format("Logfile ~p disapeared - we reopen it",
                                [AL#alog.filename]),
	    file:close(AL#alog.fd),
            {ok, Fd2} = file:open(AL#alog.filename, [write, raw]),
            AL#alog{fd = Fd2}
    end.





%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------


fmt_alog(Time, Ip, User, Req, Status,  Length, Referrer, UserAgent) ->
    [fmt_ip(Ip), " - ", User, [$\s], Time, [$\s, $"], Req, [$",$\s], 
     Status, [$\s], Length, [$\s,$"], Referrer, [$",$\s,$"], UserAgent, [$",$\n]].


fmt_ip(IP) when tuple(IP) ->
    inet_parse:ntoa(IP);
fmt_ip(undefined) ->
    "0.0.0.0";
fmt_ip(HostName) ->
    HostName.


fmtnow() ->
    {{Year, Month, Date}, {Hour, Min, Sec}} = calendar:local_time(),
    io_lib:format("[~2..0w/~s/~4..0w:~2..0w:~2..0w:~2..0w ~s]",
                  [Date,yaws:month(Month),Year, Hour, Min, Sec, zone()]).

zone() ->
    Time = erlang:universaltime(),
    LocalTime = calendar:universal_time_to_local_time(Time),
    DiffSecs = calendar:datetime_to_gregorian_seconds(LocalTime) - 
        calendar:datetime_to_gregorian_seconds(Time),
    zone((DiffSecs/3600)*100).

%% Ugly reformatting code to get times like +0000 and -1300

zone(Val) when Val < 0 ->
    io_lib:format("-~4..0w", [trunc(abs(Val))]);
zone(Val) when Val >= 0 ->
    io_lib:format("+~4..0w", [trunc(abs(Val))]).
