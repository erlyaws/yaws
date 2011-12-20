%%%-------------------------------------------------------------------
%%% File    : yaws_revproxy.erl
%%% Author  :  <klacke@hyber.org>
%%% Description : reverse proxy
%%%
%%% Created :  3 Dec 2003 by  <klacke@hyber.org>
%%%-------------------------------------------------------------------
-module(yaws_revproxy).

-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").
-export([init/6, ploop/5]).


%% reverse proxy implementation.

%% TODO: Activate proxy keep-alive with a new option ?
-define(proxy_keepalive, false).

%% Proxy socket definition
-record(psock, {s,      %% the socket
                mode,   %% chunked,len,undefined,expectheaders,expectchunked
                prefix, %% The prefix to strip and add
                url,    %% The url we're proxying to
                type,   %% client | server
                r_req,  %% if'we're server, what req method are we processing
                r_host, %% and value of Host: for the cli request
                httpconnection="keep-alive", %% Do we need to keep the
                                             %% connection open
                                             %% ("keep-alive" | "close")
                state}).%% various depending on mode, ......

%% MREMOND: TODO: Check if redirection works properly (this is
%% fundamental as most of the time cookie based authentication works
%% with redirection).
%% I did not yet check that.


%% This code is still buggy - we cannot revproxy from https->http
%% this is nontrivial to fix. OTP ssl app cannot deal with what we
%% need to do. Maybe this will work once the ssl native in OTP works.

init(CliSock, ARG, _DecPath, _QueryPart, {Prefix, URL}, N) ->
    GC=get(gc), SC=get(sc),
    %% Connect to the backend server
    case connect_url(URL, SC) of
        {ok, Ssock} ->
            Headers0 = ARG#arg.headers,
            KeepAlive = Headers0#headers.connection,
            ?Debug("Keep-alive: ~p~n",[KeepAlive]),
            Cli = sockmode(Headers0, ARG#arg.req,
                           #psock{s = CliSock, prefix = Prefix,
                                  url = URL, type = client}),
            ?Debug("CLI: ~s~n",[?format_record(Cli, psock)]),
            Headers = rewrite_headers(Cli, Headers0),
            ReqStr = yaws_api:reformat_request(
                       rewrite_path(ARG#arg.req, Prefix)),
            Hstr = yaws:headers_to_str(Headers),

            %% Sending received data from the client to the proxied server:
            %%  MREMOND: TODO: Refactor.
            yaws:gen_tcp_send(Ssock, [ReqStr, "\r\n", Hstr, "\r\n"]),
            RemainingData = if
                                N /= 0 ->
                                    %% Send data to the server:
                                    ClientData=ARG#arg.clidata,
                                    yaws:gen_tcp_send(Ssock,ClientData),
                                    N - size(ClientData);
                                true ->
                                    %% MREMOND: Check this (If N == 0, it might
                                    %% be because, we do not know the size of
                                    %% the data to receive. For now we will get
                                    %% back in http mode, expecting headers
                                    %% (new request)
                                    0
                            end,

            Cli2 = case RemainingData of
                       0      ->
                           %% Sockmode will be changed in the ploop function
                           Cli#psock{mode = expectheaders, state = undefined};
                       _Other -> Cli
                   end,

            Srv = #psock{s = Ssock,
                         prefix = Prefix,
                         url = URL,
                         r_req = (ARG#arg.req)#http_request.method,
                         r_host = (ARG#arg.headers)#headers.host,
                         mode = expectheaders, type = server,
                         httpconnection= if KeepAlive == undefined ->
                                                 "keep-alive";
                                            true ->
                                                 yaws:to_lower(KeepAlive)
                                         end
                        },
            %% Need to check if we could close the connection after serveranswer
            %% Now we _must_ spawn a process here, because we
            %% can't use {active, once} due to the inefficencies
            %% that would occur with chunked encodings
            P1 = proc_lib:spawn_link(?MODULE, ploop,
                                     [Cli2, Srv, GC, SC, self()]),
            ?Debug("Client=~p, Srv=~p", [P1, self()]),
            ploop(Srv, Cli2, GC, SC, P1);
        _ERR ->
            yaws:outh_set_dyn_headers(ARG#arg.req, ARG#arg.headers,
                                      #urltype{}),
            yaws_server:deliver_dyn_part(
              CliSock,
              0, "404",
              0,
              ARG, no_UT_defined,
              fun(A)->(SC#sconf.errormod_404):out404(A,get(gc),get(sc))
              end,
              fun(A)->yaws_server:finish_up_dyn_file(A, CliSock)
              end
             )
    end.



connect_url(URL, _SC) ->
    Port = if
               URL#url.port == undefined -> 80;
               true -> URL#url.port
           end,
    %% FIXME, do ssl:connect here if is_ssl(SC)
    %% as it is now, SSL doesn't work at all here
    gen_tcp:connect(URL#url.host, Port, [{active, false},
                                         {packet, http},
                                         {packet_size, 16#4000}]).



rewrite_path(Req, Pref) ->
    {abs_path, P} = Req#http_request.path,
    New = Req#http_request{path = {abs_path,strip_prefix(P, Pref)}},
    ?Debug("Rewritten path=~p~nP=~p Pref=~p", [New,P,Pref]),
    New.


strip_prefix("","") ->
    "/";
strip_prefix(P,"") ->
    P;
strip_prefix(P,"/") ->
    P;
strip_prefix([H|T1],[H|T2]) ->
    strip_prefix(T1,T2).



%% Once we have read the headers, what comes after
%% the headers,
%% This is applicable both for cli and srv sockets

sockmode(H,Req,Psock) ->
    case Psock#psock.type of
        server ->
            s_sockmode(H, Req, Psock);
        client ->
            cont_len_check(H,Psock)
    end.

s_sockmode(H,Resp,Psock) ->
    if Psock#psock.r_req == 'HEAD' ->
            %% we're replying to a HEAD
            %% no body
            Psock#psock{mode = expectheaders, state = undefined};

       true ->
            case lists:member(Resp#http_response.status,
                              [100,204,205,304,406]) of
                true ->
                    %% no body, illegal
                    Psock#psock{mode = expectheaders, state = undefined};
                false ->
                    cont_len_check(H, Psock)
            end
    end.

cont_len_check(H,Psock) when H#headers.transfer_encoding == "chunked" ->
    Psock#psock{mode = expectchunked, state = init};
cont_len_check(H,Psock) ->
    case H#headers.content_length of
        undefined ->
            case H#headers.transfer_encoding of
                undefined when Psock#psock.type == client ->
                    Psock#psock{mode = expectheaders, state = undefined};
                undefined when Psock#psock.type == server ->
                    Psock#psock{mode = undefined, state = 0}
            end;
        Int when is_integer(Int) ->
            Psock#psock{mode = len,
                        state = Int};
        List when is_list(List) ->
            Psock#psock{mode = len,
                        state = list_to_integer(List)}

    end.



set_sock_mode(PS) ->
    S = PS#psock.s,
    case PS#psock.mode of
        expectheaders ->
            inet:setopts(S, [{packet, http}, {packet_size, 16#4000}]);
        expectchunked ->
            inet:setopts(S, [binary, {packet, line}]);
        expect_nl_before_chunked ->
            inet:setopts(S, [binary, {packet, line}]);
        _ ->
            inet:setopts(S, [binary, {packet, raw}])
    end.



ploop(From0, To, GC, SC, Pid) ->
    put(sc, SC),
    put(gc, GC),
    put(ssl, case SC#sconf.ssl of
                 undefined -> nossl;
                 true -> ssl
             end),
    ploop(From0, To, Pid).


ploop(From0, To, Pid) ->
    From = receive
               {cli2srv, Method, Host} ->
                   From0#psock{r_req = Method,
                               r_host = Host}
           after 0 ->
                   From0
           end,
    set_sock_mode(From),
    TS = To#psock.s,
    case From#psock.mode of
        expectheaders ->
            case yaws:http_get_headers(From#psock.s, get(ssl)) of
                {R, H0} ->
                    ?Debug("R = ~p~n",[R]),
                    RStr =
                        if
                            %% FIXME handle bad_request here
                            is_record(R, http_response) ->
                                yaws_api:reformat_response(R);
                            is_record(R, http_request) ->
                                Pid ! {cli2srv, R#http_request.method,
                                       H0#headers.host},
                                yaws_api:reformat_request(
                                  rewrite_path(R, From#psock.prefix))
                        end,
                    Hstr = yaws:headers_to_str(H = rewrite_headers(From, H0)),
                    yaws:gen_tcp_send(TS, [RStr, "\r\n", Hstr]),
                    From2 = sockmode(H, R, From),
                    yaws:gen_tcp_send(TS,"\r\n"),
                    ploop(From2, To, Pid);
                closed ->
                    done
            end;
        expectchunked ->
            %% read the chunk number, we're in line mode
            N = yaws:get_chunk_num(From#psock.s, get(ssl)),
            if N == 0 ->
                    ok=yaws:eat_crnl(From#psock.s, get(ssl)),
                    yaws:gen_tcp_send(TS,["0\r\n\r\n"]),
                    ?Debug("SEND final 0 ",[]),
                    ploop_keepalive(From#psock{mode = expectheaders,
                                               state = undefined},To, Pid);
               true ->
                    ploop(From#psock{mode = chunk,
                                     state = N},To, Pid)
            end;
        chunk ->
            CG = yaws:get_chunk(From#psock.s,From#psock.state, 0, get(ssl)),
            SZ = From#psock.state,
            Data2 = [yaws:integer_to_hex(SZ),"\r\n", CG, "\r\n"],
            yaws:gen_tcp_send(TS, Data2),
            ok = yaws:eat_crnl(From#psock.s, get(ssl)),
            ploop(From#psock{mode = expectchunked,
                             state = undefined}, To, Pid);
        len when From#psock.state == 0 ->
            ploop_keepalive(From#psock{mode = expectheaders,
                                       state = undefined},To, Pid);
        len ->
            case yaws:do_recv(From#psock.s, From#psock.state, get(ssl)) of
                {ok, Bin} ->
                    SZ = size(Bin),
                    ?Debug("Read ~p bytes~n", [SZ]),
                    yaws:gen_tcp_send(TS, Bin),
                    ploop(From#psock{state = From#psock.state - SZ},
                          To, Pid);
                _Rsn ->
                    ?Debug("Failed to read :~p~n", [_Rsn]),
                    exit(normal)
            end;
        undefined ->
            case yaws:do_recv(From#psock.s, From#psock.state, get(ssl)) of
                {ok, Bin} ->
                    yaws:gen_tcp_send(TS, Bin),
                    ploop(From, To, Pid);
                _ ->
                    exit(normal)
            end
    end.

%% Before reentering the ploop in expect_header mode (new request/reply),
%% We must check the if we need to keep the connection alive
%% or if we must close it.
ploop_keepalive(_From = #psock{httpconnection="close"}, _To, _Pid) ->
    ?Debug("Connection closed by proxy: No keep-alive~n",[]),
    done;    %%  Close the connection
ploop_keepalive(From, To, Pid) ->
    %% Check server reverse proxy keep-alive setting
    %% TODO: We should get this value from the config file
    case check_server_keepalive() of
        false -> done; %% Close the connection: Server config do not
        %%  allow proxy keep-alive
        true -> ploop(From, To, Pid) %% Try keeping the connection
                %% alive => Wait for headers
    end.

%% TODO: Get proxy keepalive value in SC record
check_server_keepalive() ->
    ?proxy_keepalive.

%% On the way from the client to the server, we need
%% rewrite the Host header

rewrite_headers(PS, H) when PS#psock.type == client ->
    Host =
        if H#headers.host == undefined ->
                undefined;
           true ->
                U = PS#psock.url,
                [U#url.host,
                 if
                     U#url.port == undefined ->
                         [];
                     true ->
                         [$: | integer_to_list(U#url.port)]
                 end]
        end,

    ?Debug("New Host hdr: ~p~n", [Host]),
    H#headers{host = Host};



%% And on the way from the server to the client we
%% need to rewrite the Location header, and the
%% Set-Cookie header

rewrite_headers(PS, H) when PS#psock.type == server ->
    ?Debug("Location header to rewrite:  ~p~n", [H#headers.location]),
    Loc = if
              H#headers.location == undefined ->
                  undefined;
              true ->
                  ?Debug("parse_url(~p)~n", [H#headers.location]),
                  LocUrl = (catch yaws_api:parse_url(H#headers.location)),
                  ProxyUrl = PS#psock.url,
                  if
                      LocUrl#url.host == ProxyUrl#url.host,
                      LocUrl#url.port == ProxyUrl#url.port,
                      LocUrl#url.scheme == ProxyUrl#url.scheme ->
                          rewrite_loc_url(LocUrl, PS);

                      element(1, LocUrl) == 'EXIT' ->
                          rewrite_loc_rel(PS, H#headers.location);
                      true ->
                          ?Debug("Not rew ~p~n~p~n",
                                 [LocUrl, ProxyUrl]),
                          H#headers.location
                  end
          end,
    ?Debug("New loc=~p~n", [Loc]),

    %% And we also should do cookies here ... FIXME

    H#headers{location = Loc}.


%% Rewrite a properly formatted location redir
rewrite_loc_url(LocUrl, PS) ->
    SC=get(sc),
    Scheme = yaws:redirect_scheme(SC),
    RedirHost = yaws:redirect_host(SC, PS#psock.r_host),
    _RealPath = LocUrl#url.path,
    [Scheme, RedirHost, slash_append(PS#psock.prefix, LocUrl#url.path)].


%% This is the case for broken webservers that reply with
%% Location: /path
%% or even worse, Location: path

rewrite_loc_rel(PS, Loc) ->
    SC=get(sc),
    Scheme = yaws:redirect_scheme(SC),
    RedirHost = yaws:redirect_host(SC, PS#psock.r_host),
    [Scheme, RedirHost,Loc].



slash_append("/", [$/|T]) ->
    [$/|T];
slash_append("/", T) ->
    [$/|T];
slash_append([], [$/|T]) ->
    [$/|T];
slash_append([], T) ->
    [$/|T];
slash_append([H|T], X) ->
    [H | slash_append(T,X)].
















