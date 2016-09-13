%%%----------------------------------------------------------------------
%%% File    : yaws_ctl.erl
%%% Author  : Claes Wikstrom <klacke@bluetail.com>
%%% Purpose :
%%% Created : 29 Apr 2002 by Claes Wikstrom <klacke@bluetail.com>
%%%----------------------------------------------------------------------


%% some code to remoteley control a running yaws server

-module(yaws_ctl).
-author('klacke@bluetail.com').

-include_lib("kernel/include/file.hrl").
-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").

-export([start/2, actl_trace/1]).
-export([ls/1,hup/1,stop/1,status/1,load/1,
         check/1,trace/1, debug_dump/1, stats/1, running_config/1,
         configtest/1, auth/1]).
%% internal
-export([run/1, aloop/3, handle_a/3]).


%% assumes the appropriate file structures
%% are already created with the right perms

start(_GC, FirstTime) when FirstTime == false ->
    ok;
start(GC, true) ->
    proc_lib:start_link(?MODULE, run, [GC]).


run(GC) ->
    %% First check if there is already a Yaws system running
    %% with the same sid.
    case connect(GC#gconf.id) of
        {ok, Sock, _Key} ->
            %% Not good,
            gen_tcp:close(Sock),
            e("There is already a yaws system running with the same ~n"
              " id <~p> on this computer and this user, ~n"
              " set another id in the yaws conf file ~n",
              [GC#gconf.id]);
        {error, eacces} ->
            %% We're not allowed to open the ctl file
            e("Error reading ~s, you don't have access rights to read it",
              [yaws:ctl_file(GC#gconf.id)]);
        {error, _} ->
            %% Fine, this should be the case
            run_listen(GC)
    end.

rand() ->
    case os:type() of
        {win32, _} ->
            {A1, A2, A3}=yaws:get_time_tuple(),
            yaws_dynopts:random_seed(A1, A2, A3),
            yaws_dynopts:random_uniform(1 bsl 64);
        _ ->
            try
                crypto:start(),
                crypto:rand_uniform(0, 1 bsl 64)
            catch
                _:_ ->
                    error_logger:warning_msg("Running without crypto app\n"),
                    {A1, A2, A3}=yaws:get_time_tuple(),
                    yaws_dynopts:random_seed(A1, A2, A3),
                    yaws_dynopts:random_uniform(1 bsl 64)
            end
    end.



ctl_args() ->
    [{packet, 2},
     {active, false},
     binary,
     {ip, {127,0,0,1}},
     {reuseaddr, true}].

run_listen(GC) ->
    case gen_tcp:listen(0, ctl_args()) of
        {ok,  L} ->
            case inet:sockname(L) of
                {ok, {_, Port}} ->
                    Key = rand(),
                    case w_ctl_file(GC#gconf.id, Port, Key) of
                        ok ->
                            proc_lib:init_ack(ok),
                            aloop(L, GC, Key);
                        error ->
                            e(
                              "Failed to create/manipulate the ctlfile ~n"
                              "called ~s~n"
                              "Either problems with permissions or "
                              " earlier runs of yaws ~nwith the same id "
                              " <~p> as this, check dir for perms~n",
                              [yaws:ctl_file(GC#gconf.id), GC#gconf.id])
                    end;
                Err ->
                    e("Cannot get sockname for ctlsock: ~p",[Err] )
            end;
        Err ->
            e("Cannot listen on ctl socket, fatal: ~p", [Err])
    end.


e(Fmt, Args) ->
    proc_lib:init_ack({error, io_lib:format(Fmt, Args)}),
    exit(normal).



%% write the control file, set perms of the file
%% so that only this user can read the file
%% That way we're making sure different users
%% cannot manipulate each others webservers
w_ctl_file(Sid, Port, Key) ->
    case catch
             begin
                 F = yaws:ctl_file(Sid),
                 error_logger:info_msg("Ctlfile : ~s~n", [F]),
                 file:write_file(F, io_lib:format("~w.", [{Port,Key}])),
                 {ok, FI} = file:read_file_info(F),
                 ok = file:write_file_info(F, FI#file_info{mode = 8#00600})
             end of
             {'EXIT', _} ->
                 error;
             _ ->
                 ok
         end.



aloop(L, GC, Key) ->
    case gen_tcp:accept(L) of
        {ok, A} ->
            proc_lib:spawn (?MODULE, handle_a, [A, GC, Key]);
        Err ->
            error_logger:format("yaws_ctl failed to accept: ~p~n",
                                [Err]),
            timer:sleep(2000),
            ignore
    end,
    ?MODULE:aloop(L, GC, Key).

handle_a(A, GC, Key) ->
    case gen_tcp:recv(A, 0) of
        {ok, Data} ->
            case catch binary_to_term(Data) of
                {hup, Key} ->
                    Res = yaws:dohup(A),
                    Res;
                {stop, Key} ->
                    error_logger:info_msg("Stopping yaws\n",[]),
                    gen_tcp:send(A, io_lib:format(
                                      "stopping yaws with id=~p\n",
                                      [GC#gconf.id])),
                    file:delete(yaws:ctl_file(GC#gconf.id)),
                    init:stop();
                {{trace, What}, Key} ->
                    Res = actl_trace(What),
                    gen_tcp:send(A, Res),
                    gen_tcp:close(A);
                {status, Key} ->
                    a_status(A),
                    gen_tcp:close(A);
                {{load, Mods}, Key} ->
                    a_load(A, Mods),
                    gen_tcp:close(A);
                {id, Key} ->
                    a_id(A),
                    gen_tcp:close(A);
                {debug_dump, Key} ->
                    a_debug_dump(A),
                    gen_tcp:close(A);
                {stats, Key} ->
                    a_stats(A),
                    gen_tcp:close(A);
                {running_config, Key} ->
                    a_running_config(A),
                    gen_tcp:close(A);
                {Other, Key} ->
                    gen_tcp:send(A, io_lib:format("Other: ~p~n", [Other])),
                    gen_tcp:close(A);
                _Other ->
                    gen_tcp:close(A)

            end;
        {error, _} ->
            gen_tcp:close(A)
    end.


%% We implement this by reloading a patched config
actl_trace(What) ->
    case lists:member(What, [traffic, http, off]) of
        true ->
            {ok, GC, SCs} = yaws_api:getconf(),
            case GC#gconf.trace of
                false when What /= off ->
                    yaws_api:setconf(GC#gconf{trace = {true, What}},SCs),
                    io_lib:format(
                      "Turning on trace of ~p to directory ~s~n",
                      [What,
                       filename:join([GC#gconf.logdir,
                                      yaws_trace:get_tracedir()])]);
                false when What == off ->
                    io_lib:format("Tracing is already turned off ~n",[]);
                {true, _} when What == off ->
                    yaws_api:setconf(GC#gconf{trace = false},SCs),
                    "Turning trace off \n";
                {true, What} ->
                    io_lib:format("Trace of ~p is already turned on, use 'off' "
                                  "to turn off~n", [What]);
                {true, _Other} ->
                    yaws_api:setconf(GC#gconf{trace = {true, What}},SCs),
                    io_lib:format(
                      "Turning on trace of ~p to directory ~s~n",
                      [What,
                       filename:join([GC#gconf.logdir,
                                      yaws_trace:get_tracedir()])])
            end;
        false ->
            "error: need one of http | traffic | off as argument\n"
    end.



f(Fmt, As) ->
    io_lib:format(Fmt, As).


a_id(Sock) ->
    ID = gen_server:call(yaws_server, id, infinity),
    gen_tcp:send(Sock, ID),
    ok.

a_status(Sock) ->
    gen_tcp:send(Sock, a_status()).
a_status() ->
    try
        {UpTime, L} = yaws_server:stats(),
        {Days, {Hours, Minutes, _Secs}} = UpTime,
        UpStr = f("~n Uptime: ~w Days, ~w Hours, ~w Minutes  ~n",
                  [Days, Hours, Minutes]),

        Header = f("IP Port Connections Sessions Requests~n", []),
        Lines  = lists:map(fun({IP0, Port, Conns, Sess, Reqs}) ->
                                   IP = format_ip(IP0),
                                   f("~s ~p ~p ~p ~p~n",
                                     [IP, Port, Conns, Sess, Reqs])
                           end, L),
        [Header, Lines, UpStr]
    catch
        _:Err ->
            io_lib:format("Cannot get status ~p~n", [Err])
    end.


a_debug_dump(Sock) ->
    gen_tcp:send(Sock, a_status()),
    yaws_debug:do_debug_dump(Sock).


vsn(IP) when size(IP) =:= 4 ->
    "(ipv4)";
vsn(IP) when size(IP) =:= 8 ->
    "(ipv6)".

format_ip(IP) ->
    inet_parse:ntoa(IP).

a_running_config(Sock) ->
    gen_tcp:send(Sock, a_running_config()).
a_running_config() ->
    {ok, GC, Groups} = yaws_server:getconf(),
    GcStr = ?format_record(GC, gconf),
    L = lists:map(fun(Group) ->
                          ["** GROUP ** \n",
                           lists:map(
                             fun(SC) ->
                                     ?format_record(SC, sconf)
                             end,
                             Group)
                          ]
                  end, Groups),
    ["** GLOBAL CONF ** \n", GcStr, L].

a_stats(Sock) ->
    gen_tcp:send(Sock, a_stats()).
a_stats() ->
    {ok, _GC, Servers0} = yaws_server:getconf(),
    Servers1 = lists:flatten(Servers0),
    %% io:format("~p~n", [Servers1]),
    Servers2 = parse(Servers1),

    case Servers2 of
        [] ->
            f("No statistics available~n", []);

        Servers2 ->
            Stats = fstats(Servers2),
            Header = f("Host IP Port Hits Sent~n", []),

            Lines = lists:map(fun({Host, IP0, Port, {Hits, Sent}}) ->
                                      %% we don't use inet_parse:ntoa/1
                                      %% since it's not documented
                                      IP = format_ip(IP0),
                                      IPVsn = vsn(IP0),
                                      f("~s~s ~s ~p ~p ~p~n",
                                        [Host, IPVsn, IP, Port, Hits, Sent])
                              end, Stats),
            [Header, Lines]
    end.

parse(V) ->
    parse(V, []).
parse([], Acc) ->
    Acc;
parse([#sconf{stats=undefined}|Tail], Acc) ->
    parse(Tail, Acc);
parse([#sconf{listen=IP, port=Port, servername=Servername,
              stats=Stats}|Tail], Acc) ->
    Host = {Servername, IP, Port, Stats},
    parse(Tail, [Host|Acc]).

fstats(S) ->
    fstats(S, []).
fstats([], Acc) ->
    lists:keysort(1, Acc);
fstats([{IP, Port, Server, Stats}|Tail], Acc) ->
    S = {IP, Port, Server, yaws_stats:get(Stats)},
    fstats(Tail, [S|Acc]).


a_load(A, Mods) ->
    case purge(Mods) of
        ok ->
            gen_tcp:send(A, f("~p~n", [loadm(Mods)]));
        Err ->
            gen_tcp:send(A, f("~p~n", [Err]))
    end.

loadm([]) ->
    [];
loadm([M|Ms]) ->
    [code:load_file(M)|loadm(Ms)].

purge(Ms) ->
    case purge(Ms, []) of
        [] -> ok;
        L -> {cannot_purge, L}
    end.

purge([], Ack) ->
    Ack;
purge([M|Ms], Ack) ->
    case code:soft_purge(M) of
        true ->
            purge(Ms, Ack);
        false ->
            purge(Ms, [M|Ack])
    end.

connect(Sid) ->
    connect_file(yaws:ctl_file(Sid)).


%% The ctl file contains the port number the yaws server
%% is listening at and secret key string.

connect_file(CtlFile) ->
    case file:consult(CtlFile) of
        {ok, [{Port, Key}]} ->
            case gen_tcp:connect({127,0,0,1}, Port,
                                 [{active, false},
                                  {reuseaddr, true},
                                  binary,
                                  {packet, 2}], 2000) of
                {ok, Socket} ->
                    case inet:port(Socket) of
                        {ok,Port} ->
                            {error, erefused};
                        _X ->
                            {ok, Socket, Key}
                    end;
                Err ->
                    Err
            end;
        {ok, Terms} ->
            {error, {content, Terms}};
        Err ->
            Err
    end.



actl(SID, Term) ->
    case connect(SID) of
        {error, eacces} ->
            io:format("Another user is using the yaws sid <~p>, ~n"
                      "You are not allowd to read the file <~s>, ~n"
                      "specify by <-I id> which yaws system you want"
                      " to control~n",
                      [SID, yaws:ctl_file(SID)]),
            timer:sleep(10),
            erlang:halt(1);
        {error, econnrefused} ->
            io:format("No yaws system responds~n",[]),
            timer:sleep(10),
            erlang:halt(2);
        {error, {content,Terms}} ->
            io:format("The ctlfile ~s is readable but its content~n"
                      "~p~n"
                      "isn't in YAWS control file format~n",
                      [yaws:ctl_file(SID),Terms]),
            timer:sleep(10),
            erlang:halt(2);
        {error, Reason} ->
            io:format("You failed to read the ctlfile ~s~n"
                      "error was: <~p>~n"
                      "specify by <-I id> which yaws system you want"
                      " to control~n",
                      [yaws:ctl_file(SID), Reason]),
            timer:sleep(10),
            erlang:halt(3);
        {ok, Socket, Key} ->
            gen_tcp:send(Socket, term_to_binary({Term, Key})),
            Ret = s_cmd(Socket, SID, 0),
            timer:sleep(40), %% sucks bigtime, we have no good way to flush io
            case Ret of
                ok when Term == stop ->
                    %% wait for Yaws node to truly stop.
                    case gen_tcp:recv(Socket, 0) of
                        {error, closed} ->
                            erlang:halt(0);
                        Other ->
                            io:format("Stopping yaws: ~p~n", [Other]),
                            erlang:halt(3)
                    end;
                ok ->
                    erlang:halt(0);
                error ->
                    erlang:halt(4)
            end
    end.


s_cmd(Fd, SID, Count) ->
    case gen_tcp:recv(Fd, 0) of
        {ok, Bin} ->
            io:format("~s", [binary_to_list(Bin)]),
            s_cmd(Fd, SID, Count+1);
        {error, closed} when Count > 0 ->
            gen_tcp:close(Fd);
        Err ->
            io_lib:format("yaws server for yaws id <~p> not "
                          "responding: ~p ~n", [SID, Err]),
            error
    end.



%% List existing yaws nodes on this machine for this user
ls(_) ->
    Dir = filename:join([yaws:tmpdir(), "yaws"]),
    case file:list_dir(Dir) of
        {ok, List} ->
            io:format("~-15s~-10s~-10s~n",
                      ["Id", "Status", "Owner"]),
            io:format("-------------------------------------~n",[]),
            lists:foreach(
              fun(IdDir) ->
                      lls(IdDir)
              end, List);
        _ ->
            ok
    end,
    init:stop().


lls(IdDir) ->
    CtlFile = yaws:ctl_file(IdDir),
    case {file:read_file_info(CtlFile),
          file:read_file(CtlFile)} of
        {{ok, FI}, {error, eacces}} ->
            User = yaws:uid_to_name(FI#file_info.uid),
            io:format("~-15s~-10s~-10s~n",
                      [IdDir, "noaccess", User]);
        {{ok, FI}, {ok, _Bin}} ->
            Running = case connect(IdDir) of
                          {ok, Sock, _Key} ->
                              gen_tcp:close(Sock),
                              "running";
                          {error, timeout} ->
                              "hanging??";
                          {error, eacces} ->
                              "noaccess";
                          _Err ->
                              "stopped"
                      end,
            User = yaws:uid_to_name(FI#file_info.uid),
            io:format("~-15s~-10s~-10s~n",
                      [IdDir, Running, User]);
        _ ->
            ok
    end.



%% send a hup (kindof) to the yaws server to make it
%% reload its configuration and clear its caches

hup([SID]) ->
    actl(SID, hup).


%% stop a daemon
stop([SID]) ->
    actl(SID, stop).

%% query a daemon for status/stats
status([SID]) ->
    actl(SID, status).

load(X) ->
    [SID | Modules] = lists:reverse(X),
    actl(SID, {load, Modules}).

check([Id, File| IncludeDirs]) ->
    GC = yaws_config:make_default_gconf(false, atom_to_list(Id)),
    GC2 = GC#gconf{include_dir = lists:map(fun(X) -> atom_to_list(X) end,
                                           IncludeDirs)},
    put(gc, GC2),
    put(check_yaws_script, true),
    case yaws_compile:compile_file(atom_to_list(File)) of
        {ok, 0, _Spec} ->
            timer:sleep(100),erlang:halt(0);
        _Other ->
            timer:sleep(100),erlang:halt(1)
    end.

%% control a daemon http/traffic tracer
trace([What, SID]) ->
    actl(SID, {trace, What}).

debug_dump([SID]) ->
    actl(SID, debug_dump).

stats([SID]) ->
    actl(SID, stats).
running_config([SID]) ->
    actl(SID, running_config).

configtest([File]) ->
    Env = #env{debug = false, conf  = {file, File}},
    case catch yaws_config:load(Env) of
        {ok, _GC, _SCs} ->
            io:format("Syntax OK~n"),
            timer:sleep(100),erlang:halt(0);
        {error, Error} ->
            io:format("Syntax error in file ~p:~n~s~n", [File, Error]),
            timer:sleep(100),erlang:halt(1);
        Other ->
            io:format("Syntax error in file ~p:~n~p~n", [File, Other]),
            timer:sleep(100),erlang:halt(1)
    end.

auth([User, Algo, Passwd]) ->
    if
        Algo == md5    orelse Algo == sha    orelse
        Algo == sha224 orelse Algo == sha256 orelse
        Algo == sha384 orelse Algo == sha512 orelse
        Algo == ripemd160 ->
            Salt    = yaws_dynopts:rand_bytes(32),
            B64Salt = base64:encode(Salt),
            Hash    = crypto:hash(Algo, [Salt, atom_to_list(Passwd)]),
            B64Hash = base64:encode(Hash),
            io:format("~nUser's credential successfully generated:~n", []),
            io:format("\tPut this line in your Yaws config (in <auth> section):"
                      " user = \"~s:{~s}$~s$~s\"~n~n",
                      [atom_to_list(User),atom_to_list(Algo),B64Salt,B64Hash]),
            io:format("\tOr in a .yaws_auth file:"
                      " {\"~s\", \"~s\", \"~s\", \"~s\"}.~n",
                      [atom_to_list(User),atom_to_list(Algo),B64Salt,B64Hash]),
            timer:sleep(100),erlang:halt(0);
        true ->
            io:format("Unsupported Hash algorithm ~p~n"
                      "\tUse: md5 | ripemd160 | sha | sha224 | sha256 | sha384 | sha512 ~n", [Algo]),
            timer:sleep(100),erlang:halt(1)
    end.
