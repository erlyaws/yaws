-module(testsuite).

-include("testsuite.hrl").

%% Testsuite API
-export([
         create_dir/1,

         receive_http_headers/1,
         receive_http_body/2, receive_http_chunked_body/1,
         receive_http_response/1, receive_http_response/2,
         receive_http_response/3,

         send_http_headers/3,
         send_http_body/2,
         send_http_request/3, send_http_request/4,

         http_get/1,  http_get/2,  http_get/3,  http_get/4,
         http_post/2, http_post/3, http_post/4, http_post/5,
         http_req/2,  http_req/3,  http_req/4,  http_req/5, http_req/6,

         post_file/1,

         add_yaws_server/2, delete_yaws_server/1, reset_yaws_servers/0,
         yaws_servers/0, make_url/4, get_yaws_port/2,

         sock_connect/5, sock_close/1
        ]).


%% CT_HOOKS callbacks
-export([
         init/2, terminate/1,
         pre_init_per_suite/3,    post_end_per_suite/4,
         pre_init_per_group/3,    post_end_per_group/4,
         pre_init_per_testcase/3, post_init_per_testcase/4, post_end_per_testcase/4
        ]).


-record(suite_state,
        {
          name          :: atom(),
          passed  = 0   :: non_neg_integer(),
          failed  = 0   :: non_neg_integer(),
          skipped = 0   :: non_neg_integer()
         }).

-record(ct_state,
        {
          suites = []        :: [#suite_state{}],
          current_suite      :: #suite_state{},
          private_ports = [] :: [pos_integer()]
        }).


-define(PRINT_START_TC(Name, Config),
        begin
            SepLine   = lists:duplicate(74, $=),
            TCTitle   = lists:flatten(io_lib:format("[ Testcase ~p ]", [Name])),
            TCLogFile = ?config(tc_logfile, Config),
            TCPrivDir = ?config(priv_dir, Config),
            ct:print("~n\t~s~n\t~s~n\t~s~n~n~n"
                     "Testcase log file      : ~s~n"
                     "Private log directory  : ~s~n"
                     "Testcase configuration : ~p~n",
                     [SepLine, string:centre(TCTitle,74,$=), SepLine,
                      TCLogFile, TCPrivDir, Config])
        end).

-define(PRINT_TC_RESULT(Res, Error),
        begin
            SepLine  = lists:duplicate(74, $=),
            case Error of
                none -> ct:print("~n\t\t\tRESULT: ~s~n\t~s~n", [Res,SepLine]);
                _ -> ct:print("~nError: ~p~n~n~n\t\t\tRESULT: ~s~n\t~s~n", [Error,Res,SepLine])
            end
        end).


%%====================================================================
%% TESTSUITE API
%%====================================================================
add_yaws_server(Docroot, SL) ->
    yaws:add_server(Docroot, SL).

delete_yaws_server(Sconf) ->
    yaws_config:delete_sconf(Sconf).

reset_yaws_servers() ->
    {ok, GConf, _} = yaws_api:getconf(),
    yaws_api:setconf(GConf, []).

yaws_servers() ->
    {ok, _, Groups} = yaws_api:getconf(),
    F = fun(#sconf{listen=Ip, port=Port, servername=Srv}=SC) ->
                [VHost|_] = string:tokens(Srv, ":"),
                {Ip, Port, VHost, SC}
        end,
    lists:flatten([lists:map(F, SCs) || SCs <- Groups]).

%% ----
make_url(http,   Host, Port, Path) ->
    make_url("http", Host, Port, Path);
make_url(https,  Host, Port, Path) ->
    make_url("https", Host, Port, Path);
make_url(Scheme, Host, Port, Path) ->
    lists:append([Scheme, "://", Host, ":", integer_to_list(Port), Path]).

%% ----
get_yaws_port(N, Config) ->
    lists:nth(N, ?config(yaws_ports, Config)).

%% ----
create_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            ok;
        false ->
            ok = filelib:ensure_dir(Dir),
            ok = file:make_dir(Dir)
    end.

%% ----
sock_setopts(Sock, Opts) when is_port(Sock) -> inet:setopts(Sock, Opts);
sock_setopts(Sock, Opts) -> ssl:setopts(Sock, Opts).

sock_connect(http, Host, Port, Opts, Tout) ->
    gen_tcp:connect(Host, Port, Opts, Tout);
sock_connect(https, Host, Port, Opts, Tout) ->
    ssl:connect(Host, Port, Opts, Tout).

sock_close(Sock) when is_port(Sock) -> gen_tcp:close(Sock);
sock_close(Sock) -> ssl:close(Sock).

sock_recv(Sock, Length) ->
    case get(request_timer) of
        undefined -> sock_recv(Sock, Length, infinity);
        Ref ->
            case erlang:read_timer(Ref) of
                false ->
                    receive
                        {timeout, Ref, Msg} -> {error, Msg}
                    after 0 -> sock_recv(Sock, Length, infinity)
                    end;
                Time ->
                    sock_recv(Sock, Length, Time)
            end
    end.

sock_recv(Sock, Length, Tout) when is_port(Sock) -> gen_tcp:recv(Sock, Length, Tout);
sock_recv(Sock, Length, Tout) -> ssl:recv(Sock, Length, Tout).

sock_send(Sock, Data) when is_port(Sock) -> gen_tcp:send(Sock, Data);
sock_send(Sock, Data) -> ssl:send(Sock, Data).


receive_http_headers(Sock) ->
    sock_setopts(Sock, [list, {packet, http}, {packet_size, 16#4000}]),
    recv_hdrs(Sock, {undefined, []}).

receive_http_body(Sock, undefined) ->
    sock_setopts(Sock, [binary, {packet, raw}, {active, false}]),
    recv_body_until_close(Sock, []);
receive_http_body(Sock, Length) ->
    sock_setopts(Sock, [binary, {packet, raw}, {active, false}]),
    sock_recv(Sock, Length).

receive_http_chunked_body(Sock) ->
    recv_chunked_body(Sock, []).

receive_http_response(Sock) ->
    receive_http_response(unknown, Sock, infinity).
receive_http_response(Meth, Sock) ->
    receive_http_response(Meth, Sock, infinity).
receive_http_response(Meth, Sock, Tout) ->
    ReqRef = case Tout of
                 infinity ->
                     erase(request_timer),
                     make_ref();
                 _ ->
                     Ref = erlang:start_timer(Tout, self(), timeout),
                     put(request_timer, Ref),
                     Ref
             end,
    try
        case receive_http_headers(Sock) of
            {ok, {StatusLine, Hdrs}} when Meth == head ->
                {ok, {StatusLine, Hdrs, <<>>}};
            {ok, {StatusLine, Hdrs}} ->
                case {proplists:get_value("transfer-encoding", Hdrs),
                      proplists:get_value("content-length", Hdrs)} of
                    {"chunked", _} ->
                        case receive_http_chunked_body(Sock) of
                            {ok, Body}       -> {ok, {StatusLine,Hdrs,Body}};
                            {ok, Body, Tlrs} -> {ok, {StatusLine,Hdrs,{Body,Tlrs}}};
                            Error            -> Error
                        end;
                    {_, undefined} ->
                        case receive_http_body(Sock, undefined) of
                            {ok, Data} -> {ok, {StatusLine, Hdrs, Data}};
                            Error      -> Error
                        end;
                    {_, "0"} ->
                        {ok, {StatusLine, Hdrs, <<>>}};
                    {_, Len} ->
                        case receive_http_body(Sock, list_to_integer(Len)) of
                            {ok, Data} -> {ok, {StatusLine, Hdrs, Data}};
                            Error      -> Error
                        end
                end;
            Error ->
                Error
        end
    after
        erlang:cancel_timer(ReqRef),
        receive
            {timeout, ReqRef, _} -> ok
        after 0 -> ok
        end
    end.

recv_hdrs(Sock, {StatusLine, Hdrs}) ->
    case sock_recv(Sock, 0) of
        {ok, http_eoh} ->
            {ok, {StatusLine, lists:reverse(Hdrs)}};
        {ok, {http_error, "\r\n"}} ->
            recv_hdrs(Sock, {StatusLine, Hdrs});
        {ok, {http_error, "\n"}} ->
            recv_hdrs(Sock, {StatusLine, Hdrs});
        {ok, {http_error, Error}} ->
            {error, Error};
        {ok, {http_header, _, Name, _, Value}} ->
            Hdr = if
                     is_atom(Name) -> atom_to_list(Name);
                     true          -> Name
                 end,
            recv_hdrs(Sock, {StatusLine, [{string:to_lower(Hdr), Value}|Hdrs]});
        {ok, {http_response, {MajVsn,MinVsn}, Code, Reason}} ->
            Vsn = "HTTP/"++integer_to_list(MajVsn)++"."++integer_to_list(MinVsn),
            recv_hdrs(Sock, {{Vsn,Code,Reason}, Hdrs});
        Error ->
            Error
    end.

recv_body_until_close(Sock, Acc) ->
    case sock_recv(Sock, 0) of
        {ok, Bin}       -> recv_body_until_close(Sock, [Bin|Acc]);
        {error, closed} -> {ok, iolist_to_binary(lists:reverse(Acc))};
        Error           -> Error
    end.

get_chunk_size(Line) ->
    Parts = binary:split(Line, [<<";">>, <<" ">>, <<"\r">>, <<"\n">>, <<"\t">>],
                         [global, trim]),
    [N|_] = [P || P <- Parts, P /= <<>>],
    list_to_integer(binary_to_list(N), 16).

recv_chunk_size(Sock) ->
    sock_setopts(Sock, [binary, {packet, line}]),
    case sock_recv(Sock, 0) of
        {ok, Line} -> {ok, get_chunk_size(Line)};
        Error      -> Error
    end.
recv_chunk_crnl(Sock) ->
    sock_setopts(Sock, [binary, {packet, line}]),
    case sock_recv(Sock, 0) of
        {ok, <<"\r\n">>} -> ok;
        Error            -> Error
    end.

recv_chunk_trailers(Sock) ->
    sock_setopts(Sock, [list, {packet, httph}, {packet_size, 16#4000}]),
    recv_chunk_trailers(Sock, []).

recv_chunk_trailers(Sock, Tlrs) ->
    case sock_recv(Sock, 0) of
        {ok, http_eoh} ->
            {ok, lists:reverse(Tlrs)};
        {ok, {http_error, "\r\n"}} ->
            recv_chunk_trailers(Sock, Tlrs);
        {ok, {http_error, "\n"}} ->
            recv_chunk_trailers(Sock, Tlrs);
        {ok, {http_error, Error}} ->
            {error, Error};
        {ok, {http_header, _, Name, _, Value}} ->
            Hdr = if
                      is_atom(Name) -> atom_to_list(Name);
                      true          -> Name
                  end,
            recv_chunk_trailers(Sock, [{string:to_lower(Hdr), Value}|Tlrs]);
        Other ->
            {error, {"unexpected message", Other}}
    end.

recv_chunked_body(Sock, Acc) ->
    case recv_chunk_size(Sock) of
        {ok, 0} ->
            case recv_chunk_trailers(Sock) of
                {ok, []}   -> {ok, iolist_to_binary(lists:reverse(Acc))};
                {ok, Tlrs} -> {ok, iolist_to_binary(lists:reverse(Acc)), Tlrs};
                Error      -> Error
            end;
        {ok, Sz} ->
            sock_setopts(Sock, [binary, {packet, raw}]),
            case sock_recv(Sock, Sz) of
                {ok, Chunk} ->
                    case recv_chunk_crnl(Sock) of
                        ok    -> recv_chunked_body(Sock, [Chunk|Acc]);
                        Error -> Error
                    end;
                Error -> Error
            end;
        Error ->
            Error
    end.

%% ----
send_http_headers(Sock, Req, Headers) ->
    sock_setopts(Sock, [binary, {active, false}]),
    ReqStr  = case Req of
                  {Meth, Path, Vsn} ->
                      [string:to_upper(atom_to_list(Meth)), $\s, Path, $\s, Vsn, "\r\n"];
                  _ ->
                      [Req, "\r\n"]
              end,
    HdrsStr = [ [N, ": ", V, "\r\n"] || {N,V} <-Headers ],
    sock_send(Sock, [ReqStr, HdrsStr, "\r\n"]).

send_http_body(Sock, {Process,Acc}) when is_function(Process, 1) ->
    sock_setopts(Sock, [binary, {active, false}]),
    case Process(Acc) of
        {ok, Data, NewAcc} ->
            case sock_send(Sock, Data) of
                ok    -> send_http_body(Sock, {Process,NewAcc});
                Error -> Error
            end;
        eof ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end;
send_http_body(Sock, {chunkify,Process,Acc}) when is_function(Process, 1) ->
    sock_setopts(Sock, [binary, {active, false}]),
    case Process(Acc) of
        {ok, Data, NewAcc} ->
		    Chunk = [integer_to_list(iolist_size(Data), 16),"\r\n",
                     Data, "\r\n"],
            case sock_send(Sock, Chunk) of
                ok    -> send_http_body(Sock, {chunkify,Process,NewAcc});
                Error -> Error
            end;
        eof ->
            sock_send(Sock, <<"0\r\n\r\n">>);
        {error, Reason} ->
            {error, Reason}
    end;
send_http_body(Sock, {chunks,Body}) ->
    send_http_body(Sock, Body);
send_http_body(Sock, Body) when is_list(Body) orelse is_binary(Body) ->
    sock_setopts(Sock, [binary, {active, false}]),
    sock_send(Sock, Body).

send_http_request(Sock, Req, Headers) ->
    send_http_request(Sock, Req, Headers, <<>>).

send_http_request(Sock, Req, Headers, Body) ->
    {Headers1, Body1} =
        case Body of
            {CT, Data} when is_binary(Data) orelse is_list(Data) ->
                Sz  = integer_to_list(iolist_size(Data)),
                Hs0 = lists:keystore("Content-Type", 1, Headers, {"Content-Type", CT}),
                Hs1 = lists:keystore("Content-Length", 1, Hs0, {"Content-Length", Sz}),
                Hs2 = lists:keydelete("Transfer-Encoding", 1, Hs1),
                {Hs2, Data};
            {CT, {chunkify, _, _}=Data} ->
                Hs0 = lists:keystore("Content-Type", 1, Headers, {"Content-Type", CT}),
                Hs1 = lists:keystore("Transfer-Encoding", 1, Hs0, {"Transfer-Encoding", "chunked"}),
                Hs2 = lists:keydelete("Content-Length", 1, Hs1),
                {Hs2, Data};
            {CT, {chunks, Data}} ->
                Hs0 = lists:keystore("Content-Type", 1, Headers, {"Content-Type", CT}),
                Hs1 = lists:keystore("Transfer-Encoding", 1, Hs0, {"Transfer-Encoding", "chunked"}),
                Hs2 = lists:keydelete("Content-Length", 1, Hs1),
                {Hs2, Data};
            {CT, Data} ->
                Hs = lists:keystore("Content-Type", 1, Headers, {"Content-Type", CT}),
                {Hs, Data};
            Data when is_binary(Data) orelse is_list(Data) ->
                Sz  = integer_to_list(iolist_size(Data)),
                Hs0 = lists:keystore("Content-Length", 1, Headers, {"Content-Length", Sz}),
                Hs1 = lists:keydelete("Transfer-Encoding", 1, Hs0),
                {Hs1, Data}
        end,
    case send_http_headers(Sock, Req, Headers1) of
        ok    -> send_http_body(Sock, Body1);
        Error -> Error
    end.

%% ----
http_get(Url) ->
    http_get(Url, [], [], []).
http_get(Url, Headers) ->
    http_get(Url, Headers, [], []).
http_get(Url, Headers, HttpOpts) ->
    http_get(Url, Headers, HttpOpts, []).
http_get(Url, Headers, HttpOpts, SockOpts) ->
    http_req(get, Url, Headers, <<>>, HttpOpts, SockOpts).

http_post(Url, {CT, Body}) ->
    http_post(Url, [], {CT, Body}, [], []).
http_post(Url, Headers, {CT, Body}) ->
    http_post(Url, Headers, {CT, Body}, [], []).
http_post(Url, Headers, {CT, Body}, HttpOpts) ->
    http_post(Url, Headers, {CT, Body}, HttpOpts, []).
http_post(Url, Headers, {CT, Body}, HttpOpts, SockOpts) ->
    http_req(post, Url, Headers, {CT, Body}, HttpOpts, SockOpts).

%% ----
http_req(Meth, Url) ->
    do_http_req(Meth, Url, [], <<>>, [], []).
http_req(Meth, Url, Headers) ->
    do_http_req(Meth, Url, Headers, <<>>, [], []).
http_req(Meth, Url, Headers, Body) ->
    do_http_req(Meth, Url, Headers, Body, [], []).
http_req(Meth, Url, Headers, Body, HttpOpts) ->
    do_http_req(Meth, Url, Headers, Body, HttpOpts, []).
http_req(Meth, Url, Headers, Body, HttpOpts, SockOpts) ->
    do_http_req(Meth, Url, Headers, Body, HttpOpts, SockOpts).

do_http_req(Meth, Url, Headers, undefined, HttpOpts, SockOpts) ->
    do_http_req(Meth, Url, Headers, <<>>, HttpOpts, SockOpts);
do_http_req(Meth, Url, Headers, Body, HttpOpts, SockOpts) ->
    {Scheme, Host, Port, Query} = parse_url(Url, HttpOpts),
    SockOpts1 = set_default_sockopts(Scheme, SockOpts),
    ConTout = case lists:keyfind(connect_timeout, 1, HttpOpts) of
                  {connect_timeout, T1} -> T1;
                  false                -> infinity
              end,
    case sock_connect(Scheme, Host, Port, SockOpts1, ConTout) of
        {ok, Sock} ->
            try
                NewHeaders = set_default_headers(Scheme, Host, Port, Headers),
                Req = make_http_request(Meth, Query, HttpOpts),
                ok = send_http_request(Sock, Req, NewHeaders, Body),
                ReqTout = case lists:keyfind(timeout, 1, HttpOpts) of
                              {timeout, T2} -> T2;
                              false        -> infinity
                          end,
                receive_http_response(Meth, Sock, ReqTout)
            after
                sock_close(Sock)
            end;
        Error ->
            Error
    end.

set_default_sockopts(https, SockOpts) ->
    SockOpts1 = case lists:keyfind(verify, 1, SockOpts) of
                    {verify, _} -> SockOpts;
                    false       -> [{verify, verify_none}|SockOpts]
                end,
    set_default_sockopts(http, SockOpts1);
set_default_sockopts(http, SockOpts) ->
    [binary, {active, false}|SockOpts].


parse_url(Url, HttpOpts) ->
    {ok, {Scheme, _, Host, Port, Path, QS}} = yaws_dynopts:http_uri_parse(Url),
    case lists:keyfind(proxy, 1, HttpOpts) of
        {proxy, {PHost, PPort}} -> {Scheme, PHost, PPort, Url};
        false                   -> {Scheme, Host,  Port,  Path++QS}
    end.

make_http_request(Meth, Query, HttpOpts) ->
    Vsn = case lists:keyfind(version, 1, HttpOpts) of
              {version, V} -> V;
              false        -> "HTTP/1.1"
          end,
    {Meth, Query, Vsn}.

set_default_headers(Scheme, Host, Port, Headers0) ->
    Headers1 = case lists:keyfind("Host", 1, Headers0) of
                   {"Host", _} -> Headers0;
                   false       -> [header_host(Scheme, Host, Port)|Headers0]
               end,
    Headers2 = case lists:keyfind("User-Agent", 1, Headers1) of
                   {"User-Agent", _} -> Headers1;
                   false             -> [header_user_agent()|Headers1]
               end,
    Headers2.

header_host(https, Host, 443)-> {"Host", Host};
header_host(http, Host, 80)  -> {"Host", Host};
header_host(_, Host, Port)   -> {"Host", Host ++ ":" ++ integer_to_list(Port)}.

header_user_agent() -> {"User-Agent", "Yaws HTTP client"}.

post_file({File,MaxSz}) ->
    case file:open(File, [read,binary]) of
        {ok, FD} ->
            post_file({File,FD,MaxSz});
        {error, Reason} ->
            ct:log(error, "Failed to open file ~p: ~p~n", [File,Reason]),
            eof
    end;
post_file({File,FD,MaxSz}) ->
    case file:read(FD, MaxSz) of
        {ok, Data} ->
            {ok, Data, {File,FD,MaxSz}};
        eof ->
            file:close(FD),
            eof;
        {error, Reason} ->
            ct:log(error, "Failed to read next ~p bytes from ~p: ~p~n",
                   [MaxSz,File,Reason]),
            file:close(FD),
            eof
    end.

%%====================================================================
init(_Id, Opts) ->
    eunit:start(),
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),

    dbg:tracer(process, {fun dbg_trace_handler/2, []}),
    dbg:p(all,[c,sos,sol]),

    MatchSpec = [{'_',[],[message,{return_trace},{exception_trace}]}],
    [case Trace of
         {Mod,Fun,Artity} -> dbg:tpl(Mod,Fun,Artity,MatchSpec);
         {Mod,Fun}        -> dbg:tpl(Mod,Fun,MatchSpec);
         Mod              -> dbg:tpl(Mod,MatchSpec)
     end || Trace <- proplists:get_value(traces, Opts, [])],

    PvPorts = lists:map(fun(_) -> {ok, P} = yaws:find_private_port(), P end,
                        lists:seq(1, 20)),

    {ok, #ct_state{private_ports=PvPorts}}.

terminate(State) ->
    dbg:ctpl(),
    dbg:stop(),

    application:stop(ssl),
    application:stop(public_key),
    application:stop(asn1),
    application:stop(crypto),
    eunit:stop(),

    Bold  = ?GET_ENV("BOLD_COLOR"),
    Red   = ?GET_ENV("RED_COLOR"),
    Green = ?GET_ENV("GREEN_COLOR"),
    Std   = ?GET_ENV("STD_COLOR"),
    {Passed,Skipped,Failed} = lists:foldl(fun(St, {P,S,F}) ->
                                                  {P+St#suite_state.passed,
                                                   S+St#suite_state.skipped,
                                                   F+St#suite_state.failed}
                                          end, {0,0,0}, State#ct_state.suites),
    ?LOG("~n~n~sRESULT:~s ", [Bold,Std]),
    if
        Failed > 0 -> ?LOG("~sFAILED~s ", [Red,Std]);
        true       -> ?LOG("~sPASS~s ",   [Green,Std])
    end,
    ?LOG("\t[ Failed: ~s~-5b~sSkipped: ~-5bPassed: ~s~b~s ]~n",
         [Red,Failed,Std,Skipped,Green,Passed,Std]),
    ok.

pre_init_per_suite(SuiteName, Config, State) ->
    Bold  = ?GET_ENV("BOLD_COLOR"),
    Std   = ?GET_ENV("STD_COLOR"),
    ?LOG("~n~n  ~s~p~s...~n", [Bold, SuiteName, Std]),

    Config1 = [{yaws_ports, State#ct_state.private_ports}|Config],

    ?assertEqual(ok, create_tmpdir(SuiteName)),
    ?assertEqual(ok, parse_templates(SuiteName, Config1)),

    {Config1, State#ct_state{current_suite=#suite_state{name=SuiteName}}}.

post_end_per_suite(SuiteName, Config, Return, State) ->
    %% Be sure Yaws is stopped and unloaded
    (catch application:stop(yaws)),
    (catch application:unload(yaws)),
    unload_suite_modules(SuiteName),

    Suite = State#ct_state.current_suite,
    Red   = ?GET_ENV("RED_COLOR"),
    Green = ?GET_ENV("GREEN_COLOR"),
    Std   = ?GET_ENV("STD_COLOR"),
    ?LOG("    ------------------------------------------~n"),
    ?LOG("    Failed: ~s~-5b~sSkipped: ~-5bPassed: ~s~-5b~s~n",
         [Red, Suite#suite_state.failed, Std, Suite#suite_state.skipped,
          Green, Suite#suite_state.passed, Std]),
    if
        Suite#suite_state.failed > 0 ->
            Logfile = filename:dirname(?config(tc_logfile, Config)),
            SuiteLog = filename:join(Logfile, "suite.log"),
            ?LOG("Error details: ~s~n", [SuiteLog]);
        true ->
            ok
    end,
    NewSuites = [Suite|State#ct_state.suites],
    {Return, State#ct_state{current_suite=undefined, suites=NewSuites}}.

pre_init_per_group(GroupName, Config, State) ->
    Bold  = ?GET_ENV("BOLD_COLOR"),
    Std   = ?GET_ENV("STD_COLOR"),
    ?LOG("    [~s~p~s]~n", [Bold, GroupName, Std]),
    {Config, State}.


post_end_per_group(_GroupName, _Config, Return, State) ->
    {Return, State}.

pre_init_per_testcase(TestcaseName, Config, State) ->
    ?PRINT_START_TC(TestcaseName, Config),
    ?LOG("        ~-50s", [io_lib:format("~p...", [TestcaseName])]),
    {Config, State}.

post_init_per_testcase(_TestcaseName, Config, Return, State) ->
    Suite = State#ct_state.current_suite,
    Red   = ?GET_ENV("RED_COLOR"),
    Std   = ?GET_ENV("STD_COLOR"),
    NewSuite = case Return of
                   {error,Error} ->
                       ?PRINT_TC_RESULT("Failed", Error),
                       ?LOG("[ ~sKO~s ]~n", [Red, Std]),
                       Suite#suite_state{failed=Suite#suite_state.failed+1};
                   {fail,Reason} ->
                       ?PRINT_TC_RESULT("Failed", Reason),
                       ?LOG("[ ~sKO~s ]~n", [Red, Std]),
                       Suite#suite_state{failed=Suite#suite_state.failed+1};
                   {skip,_} ->
                       ?PRINT_TC_RESULT("Skipped", none),
                       ?LOG("[ ~sSKIPPED~s ]~n", [Std, Std]),
                       Suite#suite_state{skipped=Suite#suite_state.skipped+1};
                   _ ->
                       Suite
               end,
    {Config, State#ct_state{current_suite=NewSuite}}.


post_end_per_testcase(_TestcaseName, _Config, Return, State) ->
    Suite = State#ct_state.current_suite,
    Red   = ?GET_ENV("RED_COLOR"),
    Green = ?GET_ENV("GREEN_COLOR"),
    Std   = ?GET_ENV("STD_COLOR"),
    NewSuite = case Return of
                   {error,Error} ->
                       ?PRINT_TC_RESULT("Failed", Error),
                       ?LOG("[ ~sKO~s ]~n", [Red, Std]),
                       Suite#suite_state{failed=Suite#suite_state.failed+1};
                   {fail,Reason} ->
                       ?PRINT_TC_RESULT("Failed", Reason),
                       ?LOG("[ ~sKO~s ]~n", [Red, Std]),
                       Suite#suite_state{failed=Suite#suite_state.failed+1};
                   {skip,_} ->
                       ?PRINT_TC_RESULT("Skipped", none),
                       ?LOG("[ ~sSKIPPED~s ]~n", [Std, Std]),
                       Suite#suite_state{skipped=Suite#suite_state.skipped+1};
                   _ ->
                       ?PRINT_TC_RESULT("Passed", none),
                       ?LOG("[ ~sOK~s ]~n", [Green, Std]),
                       Suite#suite_state{passed=Suite#suite_state.passed+1}
               end,
    {Return, State#ct_state{current_suite=NewSuite}}.


%%====================================================================
create_tmpdir(SuiteName) ->
    create_dir(?tempdir(SuiteName)).

parse_templates(SuiteName, Config) ->
    Tmpldir = ?templatedir(SuiteName),
    Tmpdir  = ?tempdir(SuiteName),
    case filelib:is_dir(Tmpldir) of
        true->
            Templates = filelib:fold_files(Tmpldir, "", true,
                                           fun(F, Acc) -> [F|Acc] end, []),
            parse_templates(SuiteName, Templates, Tmpldir, Tmpdir, Config);
        false ->
            ok
    end.

parse_templates(_SuiteName, [], _Tmpldir, _Tmpdir, _Config) ->
    ok;
parse_templates(SuiteName, [SrcFile|Rest], Tmpldir, Tmpdir, Config) ->
    DstFile = Tmpdir ++ lists:subtract(SrcFile, Tmpldir),
    {ok, Content} = file:read_file(SrcFile),
    ReplacementMap = [
                      {<<"$top_srcdir$">>,    list_to_binary(?top_srcdir)},
                      {<<"$top_builddir$">>,  list_to_binary(?top_builddir)},
                      {<<"$srcdir$">>,        list_to_binary(?srcdir)},
                      {<<"$ebindir$">>,       list_to_binary(?ebindir)},
                      {<<"$ts_srcdir$">>,     list_to_binary(?ts_srcdir)},
                      {<<"$ts_builddir$">>,   list_to_binary(?ts_builddir)},
                      {<<"$wwwdir$">>,        list_to_binary(?wwwdir)},
                      {<<"$ssldir$">>,        list_to_binary(?ssldir)},
                      {<<"$sslkeyfile$">>,    list_to_binary(?sslkeyfile)},
                      {<<"$sslcertfile$">>,   list_to_binary(?sslcertfile)},
                      {<<"$data_srcdir$">>,   list_to_binary(?data_srcdir(SuiteName))},
                      {<<"$data_builddir$">>, list_to_binary(?data_builddir(SuiteName))},
                      {<<"$templatedir$">>,   list_to_binary(?templatedir(SuiteName))},
                      {<<"$tempdir$">>,       list_to_binary(?tempdir(SuiteName))},
                      {<<"$logdir$">>,        list_to_binary(?config(priv_dir, Config))}
                     ] ++
        lists:foldl(fun(P, Acc) ->
                            Id = list_to_binary(integer_to_list(length(Acc)+1)),
                            [{<<"$yaws_port",Id/binary,"$">>,
                              list_to_binary(integer_to_list(P))}|Acc]
                    end, [], ?config(yaws_ports, Config)),

    NewContent = lists:foldl(fun({Pat, Repl}, Bin) ->
                                     binary:replace(Bin, Pat, Repl, [global])
                             end, Content, ReplacementMap),
    ok = filelib:ensure_dir(DstFile),
    ok = file:write_file(DstFile, NewContent, [write]),
    parse_templates(SuiteName, Rest, Tmpldir, Tmpdir, Config).


unload_suite_modules(SuiteName) ->
    Dir = ?data_builddir(SuiteName),
    lists:foreach(fun(P) ->
                          case lists:prefix(Dir, filename:absname(P)) of
                              true  -> code:del_path(P);
                              false -> ok
                          end
                  end, code:get_path()),

    lists:foreach(fun({_, preloaded}) -> ok;
                     ({_, cover_compiled}) -> ok;
                     ({Mod,File}) ->
                          case lists:prefix(Dir, filename:absname(File)) of
                              true  -> code:purge(Mod), code:delete(Mod);
                              false -> ok
                          end
                  end, code:all_loaded()),
    ok.


%%====================================================================
%% Print dbg trace (from dbg.erl)
dbg_trace_handler(end_of_trace, Out) ->
    Out;
dbg_trace_handler(Trace, Out) when element(1, Trace) == trace,
                                   tuple_size(Trace) >= 3 ->
    dbg_trace_handler1(Trace, tuple_size(Trace), Out);
dbg_trace_handler(Trace, Out) when element(1, Trace) == trace_ts,
                                   tuple_size(Trace) >= 4 ->
    dbg_trace_handler1(Trace, tuple_size(Trace)-1,
                       element(tuple_size(Trace),Trace), Out);
dbg_trace_handler(Trace, Out) when element(1, Trace) == drop,
                                   tuple_size(Trace) =:= 2 ->
    ct:log("*** Dropped ~p messages.~n", [element(2,Trace)]),
    Out;
dbg_trace_handler(Trace, Out) when element(1, Trace) == seq_trace,
                                   tuple_size(Trace) >= 3 ->
    SeqTraceInfo = case Trace of
                       {seq_trace, Lbl, STI, TS} ->
                           ct:log("SeqTrace ~p [~p]: ", [TS, Lbl]),
                           STI;
                       {seq_trace, Lbl, STI} ->
                           ct:log("SeqTrace [~p]: ", [Lbl]),
                           STI
                   end,
    case SeqTraceInfo of
        {send, Ser, Fr, To, Mes} ->
            ct:log("(~p) ~p ! ~p [Serial: ~p]~n", [Fr, To, Mes, Ser]);
        {'receive', Ser, Fr, To, Mes} ->
            ct:log("(~p) << ~p [Serial: ~p, From: ~p]~n", [To, Mes, Ser, Fr]);
        {print, Ser, Fr, _, Info} ->
            ct:log("-> ~p [Serial: ~p, From: ~p]~n", [Info, Ser, Fr]);
        Else ->
            ct:log("~p~n", [Else])
    end,
    Out;
dbg_trace_handler(_Trace, Out) ->
    Out.

dbg_trace_handler1(Trace, Size, Out) ->
    From = element(2, Trace),
    case element(3, Trace) of
        'receive' ->
            case element(4, Trace) of
                {dbg,ok} -> ok;
                Message  -> ct:log("(~p) << ~p~n", [From,Message])
            end;
        'send' ->
            Message = element(4, Trace),
            To = element(5, Trace),
            ct:log("(~p) ~p ! ~p~n", [From,To,Message]);
        call ->
            case element(4, Trace) of
                MFA when Size == 5 ->
                    Message = element(5, Trace),
                    ct:log("(~p) call ~s (~p)~n", [From,ffunc(MFA),Message]);
                MFA ->
                    ct:log("(~p) call ~s~n", [From,ffunc(MFA)])
            end;
        return_from ->
            MFA = element(4, Trace),
            Ret = element(5, Trace),
            ct:log("(~p) returned from ~s -> ~p~n", [From,ffunc(MFA),Ret]);
        return_to ->
            MFA = element(4, Trace),
            ct:log("(~p) returning to ~s~n", [From,ffunc(MFA)]);
        spawn when Size == 5 ->
            Pid = element(4, Trace),
            MFA = element(5, Trace),
            ct:log("(~p) spawn ~p as ~s~n", [From,Pid,ffunc(MFA)]);
        Op ->
            ct:log("(~p) ~p ~s~n", [From,Op,ftup(Trace,4,Size)])
    end,
    Out.

dbg_trace_handler1(Trace, Size, TS, Out) ->
    From = element(2, Trace),
    case element(3, Trace) of
        'receive' ->
            case element(4, Trace) of
                {dbg,ok} -> ok;
                Message ->
                    ct:log("(~p) << ~p (Timestamp: ~p)~n", [From,Message,TS])
            end;
        'send' ->
            Message = element(4, Trace),
            To = element(5, Trace),
            ct:log("(~p) ~p ! ~p (Timestamp: ~p)~n", [From,To,Message,TS]);
        call ->
            case element(4, Trace) of
                MFA when Size == 5 ->
                    Message = element(5, Trace),
                    ct:log("(~p) call ~s (~p) (Timestamp: ~p)~n",
                           [From,ffunc(MFA),Message,TS]);
                MFA ->
                    ct:log("(~p) call ~s (Timestamp: ~p)~n",
                           [From,ffunc(MFA),TS])
            end;
        return_from ->
            MFA = element(4, Trace),
            Ret = element(5, Trace),
            ct:log("(~p) returned from ~s -> ~p (Timestamp: ~p)~n",
                   [From,ffunc(MFA),Ret,TS]);
        return_to ->
            MFA = element(4, Trace),
            ct:log("(~p) returning to ~s (Timestamp: ~p)~n",
                   [From,ffunc(MFA),TS]);
        spawn when Size == 5 ->
            Pid = element(4, Trace),
            MFA = element(5, Trace),
            ct:log("(~p) spawn ~p as ~s (Timestamp: ~p)~n",
                   [From,Pid,ffunc(MFA),TS]);
        Op ->
            ct:log("(~p) ~p ~s (Timestamp: ~p)~n",
                   [From,Op,ftup(Trace,4,Size),TS])
    end,
    Out.

ffunc({M,F,Argl}) when is_list(Argl) ->
    io_lib:format("~p:~p(~s)", [M, F, fargs(Argl)]);
ffunc({M,F,Arity}) ->
    io_lib:format("~p:~p/~p", [M,F,Arity]);
ffunc(X) ->
    io_lib:format("~p", [X]).

fargs(Arity) when is_integer(Arity) ->
    integer_to_list(Arity);
fargs([]) ->
    [];
fargs([A]) ->
    io_lib:format("~p", [A]);  %% last arg
fargs([A|Args]) ->
    [io_lib:format("~p,", [A]) | fargs(Args)];
fargs(A) ->
    io_lib:format("~p", [A]). % last or only arg

ftup(Trace, Index, Index) ->
    io_lib:format("~p", [element(Index,Trace)]);
ftup(Trace, Index, Size) ->
    [io_lib:format("~p ", [element(Index,Trace)]) | ftup(Trace, Index+1, Size)].
