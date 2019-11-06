-module(yaws_cgi).
-author('carsten@codimi.de').
-author('brunorijsman@hotmail.com').         %% Added support for FastCGI

-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").
-include("../include/yaws.hrl").

%%% Returns Out (i.e. same return values as out/1).
%%%
-export([call_cgi/5, call_cgi/4, call_cgi/3, call_cgi/2]).
-export([call_fcgi_responder/2, call_fcgi_responder/1]).

%%% Returns {allowed, Out} or {denied, Out}.
%%%
-export([call_fcgi_authorizer/2, call_fcgi_authorizer/1]).

%%% Returns [{VariableName, VariableValue}].
%%%
-export([fcgi_extract_variables/1]).

%%% TODO: Implement FastCGI filter role.

-export([cgi_worker/7, fcgi_worker/6]).

%%%=====================================================================
%%% Code shared between CGI and FastCGI
%%%=====================================================================

-define(ASCII_NEW_LINE, 10).
-define(ASCII_CARRIAGE_RETURN, 13).


handle_clidata(Arg, WorkerPid) ->
    case Arg#arg.clidata of
        undefined ->
            end_of_clidata(Arg, WorkerPid);
        {partial, Data} ->
            send_clidata(WorkerPid, Data),
            {get_more, cgicont, {cgistate, WorkerPid}};
        Data when is_binary(Data) ->
            send_clidata(WorkerPid, Data),
            end_of_clidata(Arg, WorkerPid)
    end.


end_of_clidata(Arg, WorkerPid) ->
    WorkerPid ! {self(), end_of_clidata},
    get_from_worker(Arg, WorkerPid).


send_clidata(WorkerPid, Data) ->
    WorkerPid ! {self(), clidata, Data},
    receive
        {WorkerPid, clidata_receipt} -> ok
    end.


get_from_worker(Arg, WorkerPid) ->
    case get_resp(WorkerPid) of
        {failure, Reason} ->
            [{status, 500}, {html, io_lib:format("CGI failure: ~p", [Reason])}];
        {Headers, Data} ->
            AllResps = lists:map(fun(X)-> do_header(Arg, X, Data) end, Headers),
            %%
            %% The CGI 1.1 spec (RFC 3875) requires a worker response
            %% consisting of only a location header and optional extension
            %% headers to be augmented with a 302 status code. Any other
            %% worker response with a location header is handled normally.
            %% Technically a response of the latter type MUST have a status
            %% code in it, but we don't enforce that.
            %%
            {LocHdr, _ExtHdrs, TheRest} =
                lists:foldl(
                  fun({header, Line}=Hdr, {Loc, Ext, Rest}) ->
                          {HdrLower, _HdrVal} = do_lower_header(Line),
                          case HdrLower of
                              "location" ->
                                  {[Hdr], Ext, Rest};
                              "x-cgi-"++_ ->
                                  {Loc, [Hdr|Ext], Rest};
                              _ ->
                                  {Loc, Ext, [Hdr|Rest]}
                          end;
                     (Hdr, {Loc, Ext, Rest}) ->
                          {Loc, Ext, [Hdr|Rest]}
                  end, {[], [], []}, AllResps),
            Next = case LocHdr of
                       [] ->
                           normal;
                       [{header, _Location}] ->
                           case TheRest of
                               [] ->
                                   location_add_302;
                               _ ->
                                   normal
                           end
                   end,
            case Next of
                normal ->
                    {ContentResps, NotCtnt} =
                        filter2(fun iscontent/1, AllResps),
                    {RedirResps, Others} = filter2(fun isredirect/1, NotCtnt),
                    case RedirResps of
                        [R|_] ->
                            WorkerPid ! {self(), no_data},
                            Others ++ [R];
                        [] ->
                            case ContentResps of
                                [C={streamcontent, _, _}|_] ->
                                    WorkerPid ! {self(), stream_data},
                                    Others++[C];
                                [C={content, _, _}|_] ->
                                    WorkerPid ! {self(), no_data},
                                    Others++[C];
                                [] ->
                                    WorkerPid ! {self(), no_data},
                                    Others
                            end
                    end;
                location_add_302 ->
                    WorkerPid ! {self(), no_data},
                    AllResps++[{status, 302}]
            end
    end.


filter2(Pred, Xs) ->
    filter2(Pred, Xs, [], []).

filter2(_Pred, [], Ts, Fs) ->
    {lists:reverse(Ts), lists:reverse(Fs)};
filter2(Pred, [X|Xs], Ts, Fs) ->
    case Pred(X) of
        true ->
            filter2(Pred, Xs, [X|Ts], Fs);
        false ->
            filter2(Pred, Xs, Ts, [X|Fs])
    end.


iscontent({content, _, _}) ->
    true;
iscontent({streamcontent, _, _}) ->
    true;
iscontent(_) ->
    false.

isredirect({status, I}) when is_integer(I) , I > 301, I < 304; I =:= 307 ->
    true;
isredirect(_) ->
    false.

checkdef(undefined) ->
    "";
checkdef(L) ->
    L.


deep_drop_prefix([], L) ->
    L;
deep_drop_prefix([X|Xs], [X|Ys]) when is_integer(X) ->
    deep_drop_prefix(Xs, Ys);
deep_drop_prefix([X|Xs], Ys) when is_list(X) ->
    deep_drop_prefix(X++Xs, Ys);
deep_drop_prefix(Xs, [Y|Ys]) when is_list(Y) ->
    deep_drop_prefix(Xs, Y++Ys);
deep_drop_prefix(_, _) ->
    false.


get_socket_peername({ssl, SslSocket}) ->
    {ok, {IP, _Port}}=ssl:peername(SslSocket),
    inet_parse:ntoa(IP);
get_socket_peername(Socket) ->
    {ok, {IP, _Port}}=inet:peername(Socket),
    inet_parse:ntoa(IP).


get_socket_sockname({ssl, SslSocket}) ->
    {ok, {IP, _Port}}=ssl:sockname(SslSocket),
    inet_parse:ntoa(IP);
get_socket_sockname(Socket) ->
    {ok, {IP, _Port}}=inet:sockname(Socket),
    inet_parse:ntoa(IP).


build_env(Arg, Scriptfilename, Pathinfo, ExtraEnv, SC) ->
    H       = Arg#arg.headers,
    Req     = Arg#arg.req,
    OrigReq = Arg#arg.orig_req,

    %% Use the original request to set REQUEST_URI
    case OrigReq#http_request.path of
        {abs_path, RequestURI} -> ok;
        _ -> RequestURI = undefined
    end,
    {Maj,Min} = Req#http_request.version,
    {Hostname, Hosttail}=lists:splitwith(fun(X)->X /= $: end,
                                         checkdef(H#headers.host)),
    Hostport = case Hosttail of
                   [$: | P] -> P;
                   [] -> integer_to_list(SC#sconf.port)
               end,
    PeerAddr = get_socket_peername(Arg#arg.clisock),
    LocalAddr = get_socket_sockname(Arg#arg.clisock),

    Scheme = (catch yaws:redirect_scheme(SC)),
    %% Needed by trac, for redirs after POST
    HttpsEnv  = case Scheme of
                    "https://" -> [{"HTTPS", "1"}];
                    _ ->[]
                end,


    %%Scriptname = deep_drop_prefix(Arg#arg.docroot, Arg#arg.fullpath),
    %%SCRIPT_NAME is the path of the script relative to the root of the website.
    %%just dropping docroot from the fullpath does not give the full SCRIPT_NAME
    %% path if a 'vdir' is involved.
    UriTail = deep_drop_prefix(Arg#arg.docroot, Arg#arg.fullpath),
    case Arg#arg.docroot_mount of
        "/" ->
            %%no arg.docroot_mount means that arg.docroot
            %% corresponds to the URI-root of the request "/"
            Scriptname = UriTail;
        Vdir ->
            Scriptname = Vdir ++ string:strip(UriTail,left,$/)
    end,

    Pathinfo2 = checkdef(Pathinfo),
    case Pathinfo2 of
        "" ->
            PathTranslated = "";
        _ ->
            %%determine what physical path the server would map Pathinfo2
            %%to if it had received just Pathinfo2 in the request.
            PathTranslated = yaws_server:mappath(SC,Arg,Pathinfo2)
    end,


    %%Pass auth info in environment - yes - including password in plain text.
    %%REMOTE_USER always = AUTH_USER
    %%!todo - LOGON_USER - same as AUTH_USER unless some auth filter has mapped
    %%the user to another username under which to run the request.
    case H#headers.authorization of
        undefined ->
            AuthEnv = [];
        {undefined, _, _} ->
            AuthEnv = [];
        {User, Password, "Basic " ++ Auth64} ->
            AuthEnv = [
                       {"HTTP_AUTHORIZATION", "Basic " ++ Auth64},
                       {"AUTH_TYPE", "Basic"},
                       {"AUTH_USER", User},
                       {"REMOTE_USER", User},
                       {"LOGON_USER", User},
                       {"AUTH_PASSWORD", Password}
                      ];
        {_User, _Password, _OrigString} ->
            %%not attempting to pass through any auth info for
            %% auth schemes that we don't yet handle
            AuthEnv = []
    end,

    Extra_CGI_Vars = lists:flatmap(fun({Dir, Vars}) ->
                                           case lists:prefix(Dir, Scriptname) of
                                               true -> Vars;
                                               false -> []
                                           end
                                   end,
                                   SC#sconf.extra_cgi_vars),

    %% Some versions of erlang:open_port can't handle query strings that
    %% end with an equal sign. This is because the broken versions treat
    %% environment variable strings ending with '=' as environment variable
    %% names intended to be deleted from the environment, i.e. as if they
    %% have no value. The result is that no QUERY_STRING environment
    %% variable gets set for these cases. We work around this by appending
    %% a & character to any query string that ends in =.
    QueryString = case checkdef(Arg#arg.querydata) of
                      "" ->
                          "";
                      QS ->
                          case lists:reverse(QS) of
                              [$= | _] ->
                                  QS ++ "&";
                              _ ->
                                  QS
                          end
                  end,

    %%todo - review. should AuthEnv entries be overridable by ExtraEnv or not?
    %% we should define policy here rather than let through dupes.

    ExtraEnv ++
        HttpsEnv ++
        AuthEnv ++
        lists:filter(
          fun({K, L}) when is_list(L) ->
                  case lists:keysearch(K, 1, ExtraEnv) of
                      false ->
                          true;
                      _ ->
                          %% we have override in extraenv
                          false
                  end;
             (_) ->
                  false
          end,
          ([
            {"SERVER_SOFTWARE", "Yaws/"++yaws_generated:version()},
            {"SERVER_NAME", Hostname},
            {"HTTP_HOST", checkdef(H#headers.host)},
            {"GATEWAY_INTERFACE", "CGI/1.1"},
            {"SERVER_PROTOCOL", "HTTP/" ++ integer_to_list(Maj) ++
             "." ++ integer_to_list(Min)},
            {"SERVER_PORT", Hostport},
            {"REQUEST_METHOD", yaws:to_list(Req#http_request.method)},
            {"REQUEST_URI", RequestURI},
            {"DOCUMENT_ROOT",         Arg#arg.docroot},
            {"DOCUMENT_ROOT_MOUNT", Arg#arg.docroot_mount},
            %% SCRIPT_FILENAME is for PHP 4.3.2 and higher
            %% see http://bugs.php.net/bug.php?id=28227
            %% (Sergei Golovan).
            {"SCRIPT_FILENAME", Scriptfilename},
            %% {"SCRIPT_TRANSLATED", Scriptfilename},   %IIS6+
            {"PATH_INFO",                Pathinfo2},
            {"PATH_TRANSLATED",        PathTranslated},
            %% <JMN_2007-02>
            %%  CGI/1.1 spec says PATH_TRANSLATED should be NULL or unset
            %% if PATH_INFO is NULL
            %%  This is in contrast to IIS behaviour - and may break some apps.
            %%  broken apps that expect it to always correspond to path of
            %% script
            %%  should be modified to use SCRIPT_FILENAME instead - or wrapped.
            %% </JMN_2007-02>
            %% --------------------
            %%  <pre_2007-02_comments>
            %%  This seems not to
            %%  correspond to the
            %%  documentation I have
            %%  read, but it works
            %%  with PHP.
            %%
            %%  (Not with PHP 4.3.10-16) from
            %%  Debian sarge (Sergei Golovan).
            %%  </pre_2007-02_comments>
            %% ---------------------
            {"SCRIPT_NAME", Scriptname},
            {"REMOTE_ADDR", PeerAddr},
            {"REMOTE_HOST", PeerAddr},  %%  We SHOULD send this
            %%  Resolving DNS not practical for performance reasons
            %%  - at least on 1st contact from a particular host.
            %%  we could do background lookup so that it's available
            %% for subsequent invocations,
            %%  but it hardly seems worthwhile. We are permitted by the
            %% CGI/1.1 spec to substitute REMOTE_ADDR
            {"SERVER_ADDR", LocalAddr},   %% Apache compat
            {"LOCAL_ADDR", LocalAddr},    %% IIS compat
            {"QUERY_STRING", QueryString},
            {"CONTENT_TYPE", H#headers.content_type},
            {"CONTENT_LENGTH", H#headers.content_length},
            {"HTTP_ACCEPT", H#headers.accept},
            {"HTTP_USER_AGENT", H#headers.user_agent},
            {"HTTP_REFERER", H#headers.referer},
            {"HTTP_IF_MODIFIED_SINCE", H#headers.if_modified_since},
            {"HTTP_IF_MATCH", H#headers.if_match},
            {"HTTP_IF_NONE_MATCH", H#headers.if_none_match},
            {"HTTP_IF_UNMODIFIED_SINCE", H#headers.if_unmodified_since},
            {"HTTP_COOKIE", flatten_val(make_cookie_val(H#headers.cookie))}
           ]++ other_headers(H#headers.other)
          )) ++
        Extra_CGI_Vars.

other_headers(Headers) ->
    lists:zf(fun({http_header,_,Var,_,Val}) ->
                     case tohttp(Var) of
                         "HTTP_PROXY" ->
                             %% See http://httpoxy.org/
                             false;
                         HTTP ->
                             {true, {HTTP,Val}}
                     end
             end, Headers).

tohttp(X) ->
    "HTTP_"++lists:map(fun tohttp_c/1, yaws:to_list(X)).


tohttp_c($-) ->
    $_;

tohttp_c(C) when C >= $a , C =< $z ->
    C - $a + $A;

tohttp_c(C) ->
    C.


make_cookie_val([]) ->
    undefined;
make_cookie_val([C]) ->
    C;
make_cookie_val([C|CS]) ->
    [make_cookie_val(CS), $; | C].


%%% Seems not to be necessary, but open_port documentation says that
%%% value has to be a string.

flatten_val(L) when is_list(L) ->
    lists:flatten(L);
flatten_val(X) ->
    X.


notslash($/) ->
    false;
notslash(_) ->
    true.


pathof(F) ->
    case lists:dropwhile(fun notslash/1, lists:reverse(F)) of
        "/" ->
            "/";
        [$/ | Tail] -> lists:reverse(Tail)
    end.


exeof(F) ->
    [$\., $/|lists:reverse(lists:takewhile(fun notslash/1, lists:reverse(F)))].


do_header(_Arg, "HTTP/1."++[_,_,N1,N2,N3|_], _) ->
    {status, list_to_integer([N1,N2,N3])};
do_header(Arg, Header, Data) when is_list(Header) ->
    {HdrLower, HdrVal} = do_lower_header(Header),
    do_header(Arg, {HdrLower, yaws:join_sep(HdrVal, ":"), Header}, Data);
do_header(_Arg, {"content-type", CT, _}, {partial_data, Data}) ->
    {streamcontent, CT, Data};
do_header(_Arg, {"content-type", CT, _}, {all_data, Data}) ->
    {content, CT, Data};
do_header(_Arg, {"status", [N1,N2,N3|_], _}, _) ->
    {status, list_to_integer([N1,N2,N3])};
do_header(_Arg, {_, _, Line}, _) ->
    {header, Line}.

do_lower_header(Header) ->
    [HdrName | HdrVal] = yaws:split_sep(Header, $:),
    HdrNmParts = [yaws:to_lower(H) || H <- yaws:split_sep(HdrName, $-)],
    {yaws:join_sep(HdrNmParts, "-"), HdrVal}.

get_resp(WorkerPid) ->
    get_resp([], WorkerPid).

get_resp(Hs, WorkerPid) ->
    receive
        {WorkerPid, header, H} ->
            ?Debug("~p~n", [{WorkerPid, header, H}]),
            get_resp([H|Hs], WorkerPid);
        {WorkerPid, all_data, Data} ->
            ?Debug("~p~n", [{WorkerPid, all_data, Data}]),
            {Hs, {all_data, Data}};
        {WorkerPid, partial_data, Data} ->
            ?Debug("~p~n", [{WorkerPid, partial_data, binary_to_list(Data)}]),
            {Hs, {partial_data, Data}};
        {WorkerPid, failure, Reason} ->
            ?Debug("~p~n", [{WorkerPid, failure, Reason}]),
            {failure, Reason};
        _Other ->
            ?Debug("~p~n", [_Other]),
            get_resp(Hs, WorkerPid)
    end.


get_opt(Key, List, Default) ->
    case lists:keysearch(Key, 1, List) of
        {value, {_Key, Val}} -> Val;
        _ -> Default
    end.


%%%==========================================================================
%%% Code specific to CGI
%%%==========================================================================

%%%  TO DO:  Handle failure and timeouts.

%%%  call_cgi calls the script `Scriptfilename' (full path).
%%%  If `Exefilename' is given, it is the executable to handle this,
%%%  otherwise `Scriptfilame' is assumed to be executable itself.
%%%
%%%  Corresponding to a URI of
%%%     `http://somehost/some/dir/script.cgi/path/info',
%%%  `Pathinfo' should be set to `/path/info'.

%%%  These functions can be used from a `.yaws' file.
%%%  Note however, that they usually generate stream content.

call_cgi(Arg, Scriptfilename) ->
    call_cgi(Arg, undefined, Scriptfilename, undefined, []).

call_cgi(Arg, Exefilename, Scriptfilename) ->
    call_cgi(Arg, Exefilename, Scriptfilename, undefined, []).

call_cgi(Arg, Exefilename, Scriptfilename, Pathinfo) ->
    call_cgi(Arg, Exefilename, Scriptfilename, Pathinfo, []).

call_cgi(Arg, Exefilename, Scriptfilename, Pathinfo, ExtraEnv) ->
    case Arg#arg.state of
        {cgistate, WorkerPid} ->
            case Arg#arg.cont of
                cgicont ->
                    handle_clidata(Arg, WorkerPid);
                undefined ->
                    ?Debug("Error while reading clidata: ~p~n",
                           [Arg#arg.clidata]),
                    %%  Error, what to do?
                    exit(normal)
            end;
        _ ->
            WorkerPid = cgi_start_worker(Arg, Exefilename, Scriptfilename,
                                      Pathinfo, ExtraEnv, get(sc)),
            handle_clidata(Arg, WorkerPid)
    end.


cgi_start_worker(Arg, Exefilename, Scriptfilename, Pathinfo, ExtraEnv, SC) ->
    ExeFN = case Exefilename of
                undefined -> exeof(Scriptfilename);
                "" -> exeof(Scriptfilename);
                FN -> FN
            end,
    PI = case Pathinfo of
             undefined -> Arg#arg.pathinfo;
             OK -> OK
         end,
    WorkerPid = proc_lib:spawn(?MODULE, cgi_worker,
                               [self(), Arg, ExeFN, Scriptfilename,
                                PI, ExtraEnv, SC]),
    WorkerPid.



cgi_worker(Parent, Arg, Exefilename, Scriptfilename, Pathinfo, ExtraEnv0, SC) ->
    ExtraEnv = lists:map(fun({K,V}) when is_binary(K), is_binary(V) ->
                                 {binary_to_list(K), binary_to_list(V)};
                            ({K,V}) when is_binary(K) ->
                                 {binary_to_list(K), V};
                            ({K,V}) when is_binary(V) ->
                                 {K, binary_to_list(V)};
                            (KV) -> KV
                         end, ExtraEnv0),
    Env = build_env(Arg, Scriptfilename, Pathinfo, ExtraEnv, SC),
    ?Debug("~p~n", [Env]),
    CGIPort = open_port({spawn, Exefilename},
                        [{env, Env},
                         {cd, pathof(Scriptfilename)},
                         exit_status,
                         binary]),
    cgi_pass_through_clidata(Parent, CGIPort),
    cgi_do_work(Parent, Arg, CGIPort).


cgi_pass_through_clidata(Parent, CGIPort) ->
    receive
        {Parent, clidata, Clidata} ->
            ?Debug("Got clidata ~p~n", [binary_to_list(Clidata)]),
            Parent ! {self(), clidata_receipt},
            CGIPort ! {self(), {command, Clidata}},
            cgi_pass_through_clidata(Parent, CGIPort);
        {Parent, end_of_clidata} ->
            ?Debug("End of clidata~n", []),
            ok
    end.


cgi_do_work(Parent, Arg, Port) ->
    cgi_header_loop(Parent, Arg, {start, Port}).


cgi_header_loop(Parent, Arg, S) ->
    Line = cgi_get_line(S),
    ?Debug("Line = ~p~n", [Line]),
    case Line of
        {failure, F} ->
            Parent ! {self(), failure, F};
        {[], T} ->
            case T of
                {middle, Data, Port} ->
                    Parent ! {self(), partial_data, Data},
                    receive
                        {Parent, stream_data} ->
                            cgi_data_loop(Arg#arg.pid, Port);
                        {Parent, no_data} ->
                            ok
                    end;
                {ending, Data, _} ->
                    Parent ! {self(), all_data, Data},
                    receive
                        {Parent, stream_data} ->
                            yaws_api:stream_chunk_end(Arg#arg.pid);
                        {Parent, no_data} ->
                            ok
                    end
            end;
        {H, T} ->
            Parent ! {self(), header, H},
            cgi_header_loop(Parent, Arg, T)
    end.


cgi_data_loop(Pid, Port) ->
    receive
        {Port, {data,Data}} ->
            ?Debug("~p~n", [{data, binary_to_list(Data)}]),
            yaws_api:stream_chunk_deliver_blocking(Pid, Data),
            cgi_data_loop(Pid, Port);
        {Port, {exit_status, _Status}} ->
            ?Debug("~p~n", [{exit_status, _Status}]),
            yaws_api:stream_chunk_end(Pid);
        _Other ->
            ?Debug("~p~n", [_Other]),
            cgi_data_loop(Pid, Port)
    end.



cgi_get_line({start, Port}) ->
    receive
        {Port, {data,Data}} ->
            cgi_get_line([], {middle, Data, Port});
        {Port, {exit_status, 0}} ->
            ?Debug("~p~n", [{exit_status, 0}]),
            cgi_get_line([], {ending, <<>>, Port});
        {Port, {exit_status, Status}} when Status /=0 ->
            ?Debug("~p~n", [{exit_status, Status}]),
            {failure, {exit_status, Status}};
        _Other ->
            ?Debug("~p~n", [_Other]),
            cgi_get_line({start, Port})
    end;
cgi_get_line(State) ->
    cgi_get_line([], State).

cgi_get_line(Acc, {S, <<?ASCII_NEW_LINE, Tail/binary>>, Port}) ->
    {lists:reverse(Acc), {S, Tail, Port}};
cgi_get_line(Acc, {S, <<?ASCII_CARRIAGE_RETURN, ?ASCII_NEW_LINE, Tail/binary>>,
                   Port}) ->
    {lists:reverse(Acc), {S, Tail, Port}};
cgi_get_line(Acc, {middle, <<>>, Port}) ->
    cgi_get_line(Acc, cgi_add_resp(<<>>, Port));
cgi_get_line(Acc, {middle, <<?ASCII_CARRIAGE_RETURN>>, Port}) ->
    %% We SHOULD test for CRLF.
    %% Would be easier without.
    cgi_get_line(Acc, cgi_add_resp(<<?ASCII_CARRIAGE_RETURN>>, Port));
cgi_get_line(Acc, {ending, <<>>, Port}) ->
    {lists:reverse(Acc), {ending, <<>>, Port}};
cgi_get_line(Acc, {S, <<C, Tail/binary>>, Port}) ->
    cgi_get_line([C|Acc], {S, Tail, Port}).


cgi_add_resp(Bin, Port) ->
    receive
        {Port, {data,Data}} ->
            {middle, <<Bin/binary, Data/binary>>, Port};
        {Port, {exit_status, _Status}} ->
            ?Debug("~p~n", [{exit_status, _Status}]),
            {ending, Bin, Port};
        _Other ->
            ?Debug("~p~n", [_Other]),
            cgi_add_resp(Bin, Port)
    end.


%%%===========================================================================
%%% Code specific to FastCGI
%%%===========================================================================

-define(FCGI_VERSION_1, 1).

-define(FCGI_TYPE_BEGIN_REQUEST, 1).
%%% Not needed yet
%%%-define(FCGI_TYPE_ABORT_REQUEST, 2).
-define(FCGI_TYPE_END_REQUEST, 3).
-define(FCGI_TYPE_PARAMS, 4).
-define(FCGI_TYPE_STDIN, 5).
-define(FCGI_TYPE_STDOUT, 6).
-define(FCGI_TYPE_STDERR, 7).
%%% Not needed yet
%%%-define(FCGI_TYPE_DATA, 8).
%%%-define(FCGI_TYPE_GET_VALUES, 9).
%%%-define(FCGI_TYPE_GET_VALUES_RESULT, 10).
-define(FCGI_TYPE_UNKNOWN_TYPE, 11).

fcgi_type_name(?FCGI_TYPE_BEGIN_REQUEST) -> "begin-request";
%%% Not needed yet
%%%fcgi_type_name(?FCGI_TYPE_ABORT_REQUEST) -> "abort-request";
fcgi_type_name(?FCGI_TYPE_END_REQUEST) -> "end-request";
fcgi_type_name(?FCGI_TYPE_PARAMS) -> "params";
fcgi_type_name(?FCGI_TYPE_STDIN) -> "stdin";
fcgi_type_name(?FCGI_TYPE_STDOUT) -> "stdout";
fcgi_type_name(?FCGI_TYPE_STDERR) -> "stderr";
%%% Not needed yet
%%%fcgi_type_name(?FCGI_TYPE_DATA) -> "data";
%%%fcgi_type_name(?FCGI_TYPE_GET_VALUES) -> "get_values";
%%%fcgi_type_name(?FCGI_TYPE_GET_VALUES_RESULT) -> "get_values_result";
fcgi_type_name(?FCGI_TYPE_UNKNOWN_TYPE) -> "unknown-type".

%%% The FCGI implementation does not support handling concurrent requests
%%% over a connection; it creates a separate connection for each
%%% request. Hence, all application records have the same request-id,
%%% namely 1.
%%%
-define(FCGI_REQUEST_ID_MANAGEMENT, 0).
-define(FCGI_REQUEST_ID_APPLICATION, 1).

-define(FCGI_DONT_KEEP_CONN, 0).
-define(FCGI_KEEP_CONN, 1).

-define(FCGI_ROLE_RESPONDER, 1).
-define(FCGI_ROLE_AUTHORIZER, 2).
-define(FCGI_ROLE_FILTER, 3).

-ifdef(debug).   % To avoid compile warning if debug is disabled.
fcgi_role_name(?FCGI_ROLE_RESPONDER) -> "responder";
fcgi_role_name(?FCGI_ROLE_AUTHORIZER) -> "authorizer";
fcgi_role_name(?FCGI_ROLE_FILTER) -> "filter";
fcgi_role_name(_) -> "?".
-endif.

-define(FCGI_STATUS_REQUEST_COMPLETE, 0).
-define(FCGI_STATUS_CANT_MPX_CONN, 1).
-define(FCGI_STATUS_OVERLOADED, 2).
-define(FCGI_STATUS_UNKNOWN_ROLE, 3).

fcgi_status_name(?FCGI_STATUS_REQUEST_COMPLETE) -> "request-complete";
fcgi_status_name(?FCGI_STATUS_CANT_MPX_CONN) -> "cannot-multiple-connection";
fcgi_status_name(?FCGI_STATUS_OVERLOADED) -> "overloaded";
fcgi_status_name(?FCGI_STATUS_UNKNOWN_ROLE) -> "unknown-role";
fcgi_status_name(_) -> "?".

%%% Amount of time (in milliseconds) allowed to connect to the application
%%% server.
%%%
-define(FCGI_CONNECT_TIMEOUT_MSECS, 10000).

%%% Amount of time (in milliseconds) allowed for data to arrive when
%%% reading the TCP connection to the application server.
%%%
-define(FCGI_READ_TIMEOUT_MSECS, 10000).

%%% TODO: Implement a configurable timeout which applies to the whole
%%% operation (as oposed to individual socket reads).

-record(fcgi_worker_state, {
            app_server_host,            % The hostname or IP address of
                                        % the application server
            app_server_port,            % The TCP port number of the
                                        % application server
            path_info,                  % The path info
            env,                        % All environment variables to be passed
                                        % to the application (incl the extras)
            keep_connection,            % Delegate close authority to the
                                        % application?
            trace_protocol,             % If true, log info messages for sent
                                        % and received FastCGI messages
            log_app_error,              % If true, log error messages for
                                        % application errors (stderr and
                                        % non-zero exit)
            role,                       % The role of the worker
                                        % (responder, authorizer, filter)
            parent_pid,                 % The PID of the parent process = the
                                        % Yaws worker process
            yaws_worker_pid,            % When doing chunked output, stream to
                                        % this Yaws worker.
            app_server_socket,          % The TCP socket to the FastCGI
                                        % application server
            stream_to_socket            % The TCP socket to the web browser
                                        % (stream chunked delivery to
                                        %  this socket)
        }).


call_fcgi_responder(Arg) ->
    call_fcgi_responder(Arg, []).

call_fcgi_responder(Arg, Options) ->
    call_fcgi(?FCGI_ROLE_RESPONDER, Arg, Options).


call_fcgi_authorizer(Arg) ->
    call_fcgi_authorizer(Arg, []).

call_fcgi_authorizer(Arg, Options) ->
    Out = call_fcgi(?FCGI_ROLE_AUTHORIZER, Arg, Options),
    case fcgi_is_access_allowed(Out) of
        true ->
            StrippedOut = strip_content_from_out(Out),
            {allowed, StrippedOut};
        false ->
            {denied, Out}
    end.


call_fcgi(Role, Arg, Options) ->
    case Arg#arg.state of
        {cgistate, WorkerPid} ->
            case Arg#arg.cont of
                cgicont ->
                    ?Debug("Call FastCGI: continuation~n", []),
                    handle_clidata(Arg, WorkerPid)
            end;
        _ ->
            ?Debug("Call FastCGI:~n"
                   "  Role = ~p (~s)~n"
                   "  Options = ~p~n"
                   "  Arg = ~p~n",
                   [Role, fcgi_role_name(Role),
                    Options,
                    Arg]),
            GlobalConf = get(gc),
            ServerConf = get(sc),
            WorkerPid = fcgi_start_worker(Role, Arg, GlobalConf, ServerConf,
              Options),
            handle_clidata(Arg, WorkerPid)
    end.


is_not_content({content, _MimeType, _Content}) -> false;
is_not_content({streamcontent, _MimeType, _Content}) -> false;
is_not_content(_) -> true.


strip_content_from_out(Out) ->
    lists:filter(fun is_not_content/1, Out).


fcgi_worker_fail(WorkerState, Reason) ->
    ParentPid = WorkerState#fcgi_worker_state.parent_pid,
    ParentPid ! {self(), failure, Reason},
    error_logger:error_msg("FastCGI failure: ~p~n", [Reason]),
    %% exit normally to avoid filling log with crash messages
    exit(normal).

fcgi_worker_fail_if(true, WorkerState, Reason) ->
    fcgi_worker_fail(WorkerState, Reason);
fcgi_worker_fail_if(_Condition, _WorkerState, _Reason) ->
    ok.

fcgi_start_worker(Role, Arg, GlobalConf, ServerConf, Options) ->
    proc_lib:spawn(?MODULE, fcgi_worker,
                   [self(), Role, Arg, GlobalConf, ServerConf, Options]).


fcgi_worker(ParentPid, Role, Arg, GlobalConf, ServerConf, Options) ->
    {DefaultSvrHost, DefaultSvrPort} =
        case ServerConf#sconf.fcgi_app_server of
            undefined ->
                {undefined, undefined};
            Else ->
                Else
        end,
    AppServerHost = get_opt(app_server_host, Options, DefaultSvrHost),
    AppServerPort = get_opt(app_server_port, Options, DefaultSvrPort),
    PreliminaryWorkerState = #fcgi_worker_state{parent_pid = ParentPid},
    fcgi_worker_fail_if(AppServerHost == undefined, PreliminaryWorkerState,
                        "app server host must be configured"),
    fcgi_worker_fail_if(AppServerPort == undefined, PreliminaryWorkerState,
                        "app server port must be configured"),
    PathInfo = get_opt(path_info, Options, Arg#arg.pathinfo),
    ScriptFileName = Arg#arg.fullpath,
    ExtraEnv = get_opt(extra_env, Options, []),
    Env = build_env(Arg, ScriptFileName, PathInfo, ExtraEnv, ServerConf),
    TraceProtocol = get_opt(trace_protocol, Options,
                            ?sc_fcgi_trace_protocol(ServerConf)),
    LogAppError = get_opt(log_app_error, Options,
                          ?sc_fcgi_log_app_error(ServerConf)),
    TcpOptions = yaws:gconf_nslookup_pref(GlobalConf),
    AppServerSocket =
        fcgi_connect_to_application_server(PreliminaryWorkerState,
                                           AppServerHost, AppServerPort,
                                           TcpOptions),
    ?Debug("Start FastCGI worker:~n"
           "  Role = ~p (~s)~n"
           "  AppServerHost = ~p~n"
           "  AppServerPort = ~p~n"
           "  PathInfo = ~p~n"
           "  ExtraEnv = ~p~n"
           "  TraceProtocol = ~p~n"
           "  LogAppStderr = ~p~n",
           [Role, fcgi_role_name(Role),
            AppServerHost,
            AppServerPort,
            PathInfo,
            ExtraEnv,
            TraceProtocol,
            LogAppError]),
    WorkerState = #fcgi_worker_state{
      app_server_host = AppServerHost,
      app_server_port = AppServerPort,
      path_info = PathInfo,
      env = Env,
      keep_connection = false,                % Currently hard-coded; make
                                              % configurable in the future?
      trace_protocol = TraceProtocol,
      log_app_error = LogAppError,
      role = Role,
      parent_pid = ParentPid,
      yaws_worker_pid = Arg#arg.pid,
      app_server_socket = AppServerSocket
     },
    fcgi_send_begin_request(WorkerState),
    fcgi_send_params(WorkerState, Env),
    fcgi_send_params(WorkerState, []),
    fcgi_pass_through_client_data(WorkerState),
    fcgi_header_loop(WorkerState),
    gen_tcp:close(AppServerSocket),
    ok.


fcgi_pass_through_client_data(WorkerState) ->
    ParentPid = WorkerState#fcgi_worker_state.parent_pid,
    receive
        {ParentPid, clidata, <<>>} ->
            ParentPid ! {self(), clidata_receipt},
            fcgi_pass_through_client_data(WorkerState);
        {ParentPid, clidata, ClientData} ->
            ParentPid ! {self(), clidata_receipt},
            fcgi_send_stdin(WorkerState, ClientData),
            fcgi_pass_through_client_data(WorkerState);
        {ParentPid, end_of_clidata} ->
            fcgi_send_stdin(WorkerState, <<>>)
    end.


fcgi_connect_to_application_server(WorkerState, Host, Port, TcpOptions) ->
    Options = [binary, {packet, 0}, {active, false}, {nodelay, true} |
      TcpOptions],
    case yaws:tcp_connect(Host, Port, Options, ?FCGI_CONNECT_TIMEOUT_MSECS) of
        {error, Reason} ->
            fcgi_worker_fail(WorkerState,
                             {"connect to application server failed", Reason});
        {ok, Socket} ->
            Socket
    end.


fcgi_send_begin_request(WorkerState) ->
    %% Not needed yet -- keep_connection is currently hard-coded to false
    %%KeepConnection = WorkerState#fcgi_worker_state.keep_connection,
    %%Flags = case KeepConnection of
    %%            true -> ?FCGI_KEEP_CONN;
    %%            false -> ?FCGI_DONT_KEEP_CONN
    %%        end,
    Flags = ?FCGI_DONT_KEEP_CONN,
    Role = WorkerState#fcgi_worker_state.role,
    fcgi_send_record(WorkerState, ?FCGI_TYPE_BEGIN_REQUEST,
                     ?FCGI_REQUEST_ID_APPLICATION, <<Role:16, Flags:8, 0:40>>).


fcgi_send_params(WorkerState, NameValueList) ->
    fcgi_send_record(WorkerState, ?FCGI_TYPE_PARAMS,
                     ?FCGI_REQUEST_ID_APPLICATION, NameValueList).


fcgi_send_stdin(WorkerState, Data) ->
    fcgi_send_record(WorkerState, ?FCGI_TYPE_STDIN,
                     ?FCGI_REQUEST_ID_APPLICATION, Data).


%%% Not needed yet
%%%
%%% fcgi_send_data(ParentPid, Socket, Data) ->
%%%     fcgi_send_record(ParentPid, Socket, ?FCGI_TYPE_DATA,
%%%                      ?FCGI_REQUEST_ID_APPLICATION, Data).


%%% Not needed yet
%%%
%%% fcgi_send_abort_request(ParentPid, Socket) ->
%%%     fcgi_send_record(ParentPid, Socket, ?FCGI_TYPE_ABORT_REQUEST,
%%%                      ?FCGI_REQUEST_ID_APPLICATION, <<>>).


fcgi_data_to_string(Data) ->
    fcgi_data_to_string("", 0, "", "", Data).

fcgi_data_to_string(LinesStr, Count, CharStr, HexStr, <<>>) ->
    if
        Count == 0 ->
            LinesStr;
        true ->
            Padding = lists:duplicate(16 - Count, $ ),
            LinesStr ++ "\n    " ++ CharStr ++ Padding ++ "  " ++ HexStr
    end;
fcgi_data_to_string(LinesStr, Count, CharStr, HexStr,
                    <<Byte:8, MoreData/binary>>) ->
    Char = if
        (Byte >= $!) and (Byte =< $~) ->
            Byte;
        true ->
            $.
    end,
    Hex = io_lib:format("~2.16.0b ", [Byte]),
    if
        Count == 16 ->
            fcgi_data_to_string(LinesStr ++ "\n    " ++ CharStr ++ "  " ++
                                HexStr, 1, [Char], Hex, MoreData);
        true ->
            fcgi_data_to_string(LinesStr, Count + 1, CharStr ++ [Char],
                                HexStr ++ Hex, MoreData)
    end.


fcgi_trace_protocol(WorkerState, Action, Version, Type, RequestId,
                    ContentLength, PaddingLength, Reserved, ContentData,
                    PaddingData) ->
    Trace = WorkerState#fcgi_worker_state.trace_protocol,
    if
        Trace ->
            error_logger:info_msg(
                "~s FastCGI record:~n"
                "  version = ~p~n"
                "  type = ~p (~s)~n"
                "  request-id = ~p~n"
                "  content-length = ~p~n"
                "  padding-length = ~p~n"
                "  reserved = ~p~n"
                "  content-data = ~s~n"
                "  padding-data = ~s~n",
                [Action,
                 Version,
                 Type, fcgi_type_name(Type),
                 RequestId,
                 ContentLength,
                 PaddingLength,
                 Reserved,
                 fcgi_data_to_string(ContentData),
                 fcgi_data_to_string(PaddingData)]);
        true ->
            ok
    end.


fcgi_send_record(WorkerState, Type, RequestId, NameValueList) ->
    EncodedRecord = fcgi_encode_record(WorkerState, Type, RequestId,
                                       NameValueList),
    AppServerSocket = WorkerState#fcgi_worker_state.app_server_socket,
    case gen_tcp:send(AppServerSocket, EncodedRecord) of
        {error, Reason} ->
            fcgi_worker_fail(WorkerState,
                             {"send to application server failed", Reason});
        ok ->
            ok
    end.


fcgi_encode_record(WorkerState, Type, RequestId, NameValueList)
  when is_list(NameValueList) ->
    fcgi_encode_record(WorkerState, Type, RequestId,
                       fcgi_encode_name_value_list(NameValueList));

fcgi_encode_record(WorkerState, Type, RequestId, ContentData)
  when is_binary(ContentData), size(ContentData) > 65535  ->
    <<Bin:65535/binary, Rest/binary>> = ContentData,
    [fcgi_encode_record(WorkerState, Type, RequestId, Bin),
     fcgi_encode_record(WorkerState, Type, RequestId, Rest)];

fcgi_encode_record(WorkerState, Type, RequestId, ContentData)
  when is_binary(ContentData) ->
    Version = 1,
    ContentLength = size(ContentData),
    %% Add padding bytes (if needed) to content bytes to make
    %% content plus padding a multiple of 8 bytes.
    PaddingLength = if
                        ContentLength rem 8 == 0 ->
                            0;
                        true ->
                            8 - (ContentLength rem 8)
                    end,
    PaddingData = <<0:(PaddingLength * 8)>>,
    Reserved = 0,
    fcgi_trace_protocol(WorkerState, "Send", Version, Type, RequestId,
                        ContentLength, PaddingLength, Reserved,
                        ContentData, PaddingData),
    <<Version:8,
      Type:8,
      RequestId:16,
      ContentLength:16,
      PaddingLength:8,
      Reserved:8,
      ContentData/binary,
      PaddingData/binary>>.


fcgi_encode_name_value_list(_NameValueList = []) ->
    <<>>;
fcgi_encode_name_value_list(_NameValueList = [{Name, Value} | Tail]) ->
    <<(fcgi_encode_name_value(Name,Value))/binary,
      (fcgi_encode_name_value_list(Tail))/binary>>.


fcgi_encode_name_value(Name, _Value = undefined) ->
    fcgi_encode_name_value(Name, "");
fcgi_encode_name_value(Name0, Value0) ->
    Name = unicode:characters_to_binary(Name0),
    Value = unicode:characters_to_binary(Value0),
    NameSize = byte_size(Name),
    %% If name size is < 128, encode it as one byte with the high bit clear.
    %% If the name size >= 128, encoded it as 4 bytes with the high bit set.
    NameSizeData = if
                       NameSize < 128 ->
                           <<NameSize:8>>;
                       true ->
                           <<(NameSize bor 16#80000000):32>>
                   end,
    %% Same encoding for the value size.
    ValueSize = byte_size(Value),
    ValueSizeData = if
                        ValueSize < 128 ->
                            <<ValueSize:8>>;
                        true ->
                            <<(ValueSize bor 16#80000000):32>>
                    end,
    list_to_binary([<<NameSizeData/binary, ValueSizeData/binary>>, Name, Value]).


fcgi_header_loop(WorkerState) ->
    fcgi_header_loop(WorkerState, start).

fcgi_header_loop(WorkerState, LineState) ->
    Line = fcgi_get_line(WorkerState, LineState),
    ParentPid = WorkerState#fcgi_worker_state.parent_pid,
    case Line of
        {failure, Reason} ->
            ParentPid ! {self(), failure, Reason};
        {_EmptyLine = [], NewLineState} ->
            case NewLineState of
                {middle, Data} ->
                    case WorkerState#fcgi_worker_state.role of
                        ?FCGI_ROLE_AUTHORIZER ->
                            % For authorization we never stream to the client
                            fcgi_collect_all_data_loop(WorkerState, Data);
                        _ ->
                            ParentPid ! {self(), partial_data, Data},
                            receive
                                {ParentPid, stream_data} ->
                                    fcgi_stream_data_loop(WorkerState);
                                {ParentPid, no_data} ->
                                    ok
                            end
                    end;
                {ending, Data} ->
                    ParentPid ! {self(), all_data, Data},
                    receive
                        {ParentPid, stream_data} ->
                            yaws_api:stream_chunk_end(
                              WorkerState#fcgi_worker_state.yaws_worker_pid);
                        {ParentPid, no_data} ->
                            ok
                    end
            end;
        {Header, NewLineState} ->
            ParentPid ! {self(), header, Header},
            fcgi_header_loop(WorkerState, NewLineState)
    end.


fcgi_get_line(WorkerState, start) ->
    case fcgi_get_output(WorkerState) of
        {data, Data} ->
            fcgi_get_line(WorkerState, [], {middle, Data});
        {exit_status, 0} ->
            fcgi_get_line(WorkerState, [], {ending, <<>>});
        {exit_status, Status} when Status /=0 ->
            {failure, {exit_status, Status}}
    end;
fcgi_get_line(WorkerState, LineState) ->
    fcgi_get_line(WorkerState, [], LineState).

fcgi_get_line(_WorkerState, Acc, {State, <<?ASCII_NEW_LINE, Tail/binary>>}) ->
    {lists:reverse(Acc), {State, Tail}};
fcgi_get_line(_WorkerState, Acc, {State, <<?ASCII_CARRIAGE_RETURN,
                                           ?ASCII_NEW_LINE, Tail/binary>>}) ->
    {lists:reverse(Acc), {State, Tail}};
fcgi_get_line(WorkerState, Acc, {middle, <<>>}) ->
    fcgi_get_line(WorkerState, Acc, fcgi_add_resp(WorkerState, <<>>));
fcgi_get_line(WorkerState, Acc, {middle, <<?ASCII_CARRIAGE_RETURN>>}) ->
    fcgi_get_line(WorkerState, Acc, fcgi_add_resp(WorkerState,
                                                  <<?ASCII_CARRIAGE_RETURN>>));
fcgi_get_line(_WorkerState, Acc, {ending, <<>>}) ->
    {lists:reverse(Acc), {ending, <<>>}};
fcgi_get_line(WorkerState, Acc, {State, <<Char, Tail/binary>>}) ->
    fcgi_get_line(WorkerState, [Char | Acc], {State, Tail}).


fcgi_add_resp(WorkerState, OldData) ->
    case fcgi_get_output(WorkerState) of
        {data, NewData} ->
            {middle, <<OldData/binary, NewData/binary>>};
        {exit_status, _Status} ->
            {ending, OldData}
    end.


fcgi_stream_data_loop(WorkerState) ->
    YawsWorkerPid = WorkerState#fcgi_worker_state.yaws_worker_pid,
    case catch fcgi_get_output(WorkerState) of
        {data, Data} ->
            yaws_api:stream_chunk_deliver_blocking(YawsWorkerPid, Data),
            fcgi_stream_data_loop(WorkerState);
        {exit_status, _Status} ->
            yaws_api:stream_chunk_end(YawsWorkerPid);
        {'EXIT', _Reason} ->
            yaws_api:stream_chunk_end(YawsWorkerPid)
    end.


fcgi_collect_all_data_loop(WorkerState, Data) ->
    YawsWorkerPid = WorkerState#fcgi_worker_state.yaws_worker_pid,
    case fcgi_get_output(WorkerState) of
        {data, MoreData} ->
            NewData = <<Data/binary, MoreData/binary>>,
            fcgi_collect_all_data_loop(WorkerState, NewData);
        {exit_status, _Status} ->
            ParentPid = WorkerState#fcgi_worker_state.parent_pid,
            ParentPid ! {self(), all_data, Data},
            receive
                {ParentPid, stream_data} ->
                    yaws_api:stream_chunk_end(YawsWorkerPid);
                {ParentPid, no_data} ->
                    ok
            end
    end.


fcgi_get_output(WorkerState) ->
    {Type, ContentData} = fcgi_receive_record(WorkerState),
    case Type of
        ?FCGI_TYPE_END_REQUEST ->
            <<AppStatus:32/signed, ProtStatus:8, _Reserved:24>> = ContentData,
            fcgi_worker_fail_if(ProtStatus < ?FCGI_STATUS_REQUEST_COMPLETE,
                                WorkerState,
                                {"received unknown protocol status",
                                 ProtStatus}),
            fcgi_worker_fail_if(ProtStatus > ?FCGI_STATUS_UNKNOWN_ROLE,
                                WorkerState,
                                {"received unknown protocol status",
                                 ProtStatus}),
            if
                ProtStatus /= ?FCGI_STATUS_REQUEST_COMPLETE ->
                    error_logger:error_msg("FastCGI protocol error: ~p (~s)~n",
                                           [ProtStatus,
                                            fcgi_status_name(ProtStatus)]);
                true ->
                    ok
            end,
            if
                (AppStatus /= 0),
                (WorkerState#fcgi_worker_state.log_app_error) ->
                    error_logger:error_msg(
                      "FastCGI application non-zero exit status: ~p~n",
                      [AppStatus]);
                true ->
                    ok
            end,
            {exit_status, AppStatus};
        ?FCGI_TYPE_STDOUT ->
            {data, ContentData};
        ?FCGI_TYPE_STDERR ->
            if
                (ContentData /= <<>>),
                (WorkerState#fcgi_worker_state.log_app_error) ->
                    error_logger:error_msg(
                      "FastCGI application stderr output:~s~n",
                      [fcgi_data_to_string(ContentData)]);
                true ->
                    ok
            end,
            fcgi_get_output(WorkerState);
        ?FCGI_TYPE_UNKNOWN_TYPE ->
            <<UnknownType:8, _Reserved:56>> = ContentData,
            fcgi_worker_fail(
              WorkerState,
              {"application did not understand record type we sent",
               UnknownType})
    end.


fcgi_receive_record(WorkerState) ->
    Header = fcgi_receive_binary(WorkerState, 8, ?FCGI_READ_TIMEOUT_MSECS),
    <<Version:8, Type:8, RequestId:16, ContentLength:16,
      PaddingLength:8, Reserved:8>> = Header,
    fcgi_worker_fail_if(Version /= 1, WorkerState,
                        {"received unsupported version", Version}),
    case Type of
        ?FCGI_TYPE_END_REQUEST ->
            fcgi_worker_fail_if(RequestId /= ?FCGI_REQUEST_ID_APPLICATION,
                                WorkerState,
                                {"unexpected request id", RequestId}),
            fcgi_worker_fail_if(ContentLength /= 8, WorkerState,
                                {"incorrect content length for end request",
                                 ContentLength}),
            ok;
        ?FCGI_TYPE_STDOUT ->
            fcgi_worker_fail_if(RequestId /= ?FCGI_REQUEST_ID_APPLICATION,
                                WorkerState,
                                {"unexpected request id", RequestId}),
            ok;
        ?FCGI_TYPE_STDERR ->
            fcgi_worker_fail_if(RequestId /= ?FCGI_REQUEST_ID_APPLICATION,
                                WorkerState,
                                {"unexpected request id", RequestId}),
            ok;
        ?FCGI_TYPE_UNKNOWN_TYPE ->
            fcgi_worker_fail_if(RequestId /= ?FCGI_REQUEST_ID_MANAGEMENT,
                                WorkerState,
                                {"unexpected request id", RequestId}),
            fcgi_worker_fail_if(ContentLength /= 8, WorkerState,
                                {"incorrect content length for unknown type",
                                 ContentLength}),
            ok;
        OtherType ->
            throw({"received unexpected type", OtherType})
    end,
    ContentData = case ContentLength of
                      0 ->
                          <<>>;
                      _ ->
                          fcgi_receive_binary(WorkerState, ContentLength,
                                              ?FCGI_READ_TIMEOUT_MSECS)
                  end,
    case PaddingLength of
        0 ->
            {Type, ContentData};
        _ ->
            PaddingData = fcgi_receive_binary(WorkerState, PaddingLength,
                                              ?FCGI_READ_TIMEOUT_MSECS),
            fcgi_trace_protocol(WorkerState, "Receive",
                                Version, Type, RequestId,
                                ContentLength, PaddingLength,
                                Reserved, ContentData,
                                PaddingData),
            {Type, ContentData}
    end.


fcgi_receive_binary(_WorkerState, Length, _Timeout) when Length == 0 ->
    <<>>;
fcgi_receive_binary(WorkerState, Length, Timeout) ->
    AppServerSocket = WorkerState#fcgi_worker_state.app_server_socket,
    case gen_tcp:recv(AppServerSocket, Length, Timeout) of
        {error, Reason} ->
            fcgi_worker_fail(WorkerState,
                             {"recv from application server failed", Reason});
        {ok, Data} ->
            Data
    end.


%%% Access is allowed if, and only if, the resonse from the authorizer
%%% running on the application server contains a 200 OK status. Any other
%%% status or absence of a status means access is denied.
%%%
fcgi_is_access_allowed([Head | Tail]) ->
    fcgi_is_access_allowed(Head) orelse fcgi_is_access_allowed(Tail);
fcgi_is_access_allowed({status, 200}) ->
    true;
fcgi_is_access_allowed(_AnythingElse) ->
    false.


%%% Look for headers of the form "Variable-VAR_NAME: var value"
%%%
fcgi_extract_variables([Head | Tail]) ->
    fcgi_extract_variables(Head) ++ fcgi_extract_variables(Tail);
fcgi_extract_variables({header, "Variable-" ++ Rest}) ->
    [fcgi_split_header(Rest)];
fcgi_extract_variables(_AnythingElse) ->
    [].


fcgi_split_header(Header) ->
    fcgi_split_header(name, [], [], Header).

fcgi_split_header(_, NameAcc, ValueAcc, "") ->
    {string:strip(lists:reverse(NameAcc)),
     string:strip(lists:reverse(ValueAcc))};
fcgi_split_header(name, NameAcc, ValueAcc, [$: | MoreStr]) ->
    fcgi_split_header(value, NameAcc, ValueAcc, MoreStr);
fcgi_split_header(name, NameAcc, ValueAcc, [Char | MoreStr]) ->
    fcgi_split_header(name, [Char | NameAcc], ValueAcc, MoreStr);
fcgi_split_header(value, NameAcc, ValueAcc, [Char | MoreStr]) ->
    fcgi_split_header(value, NameAcc, [Char | ValueAcc], MoreStr).
