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
-export([out/1]).


%% reverse proxy implementation.

%% the revproxy internal state
-record(revproxy, {srvsock,         %% the socket opened on the backend server
                   type,            %% the socket type: ssl | nossl

                   cliconn_status,  %% "Connection:" header value:
                   srvconn_status,  %%   "keep-alive' or "close"

                   state,           %% revproxy state:
                                    %%   sendheaders | sendcontent | sendchunk |
                                    %%   recvheaders | recvcontent | recvchunk |
                                    %%   terminate
                   prefix,          %% The prefix to strip and add
                   url,             %% the url we're proxying to
                   r_meth,          %% what req method are we processing
                   r_host,          %%   and value of Host: for the cli request

                   resp,            %% response received from the server
                   headers,         %%   and associated headers
                   srvdata,         %% the server data
                   is_chunked,      %% true if the response is chunked
                   intercept_mod   %% revproxy request/response intercept module
                  }).


%% TODO: Activate proxy keep-alive with a new option ?
-define(proxy_keepalive, false).


%% Initialize the connection to the backend server. If an error occurred, return
%% an error 404.
out(#arg{req=Req, headers=Hdrs, state=#proxy_cfg{url=URL}=State}=Arg) ->
    case connect(URL) of
        {ok, Sock, Type} ->
            ?Debug("Connection established on ~p: Socket=~p, Type=~p~n",
                   [URL, Sock, Type]),
            RPState = #revproxy{srvsock       = Sock,
                                type          = Type,
                                state         = sendheaders,
                                prefix        = State#proxy_cfg.prefix,
                                url           = URL,
                                r_meth        = Req#http_request.method,
                                r_host        = Hdrs#headers.host,
                                intercept_mod = State#proxy_cfg.intercept_mod},
            out(Arg#arg{state=RPState});
        _ERR ->
            ?Debug("Connection failed: ~p~n", [_ERR]),
            out404(Arg)
    end;


%% Send the client request to the server then check if the request content is
%% chunked or not
out(#arg{state=#revproxy{}=RPState}=Arg)
  when RPState#revproxy.state == sendheaders ->
    ?Debug("Send request headers to backend server: ~n"
           " - ~s~n", [?format_record(Arg#arg.req, http_request)]),

    Req     = rewrite_request(RPState,  Arg#arg.req),
    Hdrs0   = Arg#arg.headers,
    Hdrs    = rewrite_client_headers(RPState, Hdrs0),
    {NewReq, NewHdrs} = case RPState#revproxy.intercept_mod of
                            undefined ->
                                {Req, Hdrs};
                            InterceptMod ->
                                case catch InterceptMod:rewrite_request(
                                             Req, Hdrs) of
                                    {ok, NewReq0, NewHdrs0} ->
                                        {NewReq0, NewHdrs0};
                                    InterceptError ->
                                        error_logger:error_msg(
                                          "revproxy intercept module ~p:"
                                          "rewrite_request failed: ~p~n",
                                          [InterceptMod, InterceptError]),
                                        exit({error, intercept_mod})
                                end
                        end,
    ReqStr  = yaws_api:reformat_request(NewReq),
    HdrsStr = yaws:headers_to_str(NewHdrs),
    case send(RPState, [ReqStr, "\r\n", HdrsStr, "\r\n"]) of
        ok ->
            case yaws:to_lower(Hdrs#headers.transfer_encoding) of
                "chunked" ->
                    ?Debug("Request content is chunked~n", []),
                    out(Arg#arg{state=RPState#revproxy{state=sendchunk}});
                _ ->
                    out(Arg#arg{state=RPState#revproxy{state=sendcontent}})
            end;

        {error, Reason} ->
            ?Debug("TCP error: ~p~n", [Reason]),
            case Reason of
                closed -> ok;
                _      -> close(RPState)
            end,
            outXXX(500, Arg)
    end;


%% Send the request content to the server. Here the content is not chunked. But
%% it can be split because of 'partial_post_size' value.
out(#arg{state=RPState}=Arg) when RPState#revproxy.state == sendcontent ->
    case Arg#arg.clidata of
        {partial, Bin} ->
            ?Debug("Send partial content to backend server: ~p bytes~n",
                   [size(Bin)]),
            case send(RPState, Bin) of
                ok ->
                    {get_more, undefined, RPState};
                {error, Reason} ->
                    ?Debug("TCP error: ~p~n", [Reason]),
                    case Reason of
                        closed -> ok;
                        _      -> close(RPState)
                    end,
                    outXXX(500, Arg)
            end;

        Bin when is_binary(Bin), Bin /= <<>> ->
            ?Debug("Send content to backend server: ~p bytes~n", [size(Bin)]),
            case send(RPState, Bin) of
                ok ->
                    RPState1 = RPState#revproxy{state=recvheaders},
                    out(Arg#arg{state=RPState1});
                {error, Reason} ->
                    ?Debug("TCP error: ~p~n", [Reason]),
                    case Reason of
                        closed -> ok;
                        _      -> close(RPState)
                    end,
                    outXXX(500, Arg)
            end;

        _ ->
            ?Debug("no content found~n", []),
            RPState1 = RPState#revproxy{state=recvheaders},
            out(Arg#arg{state=RPState1})
    end;


%% Send the request content to the server. Here the content is chunked, so we
%% must rebuild the chunk before sending it. Chunks can have different size than
%% the original request because of 'partial_post_size' value.
out(#arg{state=RPState}=Arg) when RPState#revproxy.state == sendchunk ->
    case Arg#arg.clidata of
        {partial, Bin} ->
            ?Debug("Send chunked content to backend server: ~p bytes~n",
                   [size(Bin)]),
            Res = send(RPState,
                       [yaws:integer_to_hex(size(Bin)),"\r\n",Bin,"\r\n"]),
            case Res of
                ok ->
                    {get_more, undefined, RPState};
                {error, Reason} ->
                    ?Debug("TCP error: ~p~n", [Reason]),
                    case Reason of
                        closed -> ok;
                        _      -> close(RPState)
                    end,
                    outXXX(500, Arg)
            end;

        <<>> ->
            ?Debug("Send last chunk to backend server~n", []),
            case send(RPState, "0\r\n\r\n") of
                ok ->
                    RPState1 = RPState#revproxy{state=recvheaders},
                    out(Arg#arg{state=RPState1});
                {error, Reason} ->
                    ?Debug("TCP error: ~p~n", [Reason]),
                    case Reason of
                        closed -> ok;
                        _      -> close(RPState)
                    end,
                    outXXX(500, Arg)
            end
    end;


%% The request and its content were sent. Now, we try to read the response
%% headers. Then we check if the response content is chunked or not.
out(#arg{state=RPState}=Arg) when RPState#revproxy.state == recvheaders ->
    Res = yaws:http_get_headers(RPState#revproxy.srvsock,
                                RPState#revproxy.type),
    case Res of
        {error, {too_many_headers, _Resp}} ->
            ?Debug("Response headers too large from backend server~n", []),
            close(RPState),
            outXXX(500, Arg);

        {Resp0, RespHdrs0} when is_record(Resp0, http_response) ->
            ?Debug("Response headers received from backend server:~n"
                   " - ~s~n - ~s~n", [?format_record(Resp0, http_response),
                                      ?format_record(RespHdrs0, headers)]),

            {Resp, RespHdrs} =
                case RPState#revproxy.intercept_mod of
                    undefined ->
                        {Resp0, RespHdrs0};
                    InterceptMod ->
                        case catch InterceptMod:rewrite_response(
                                     Resp0, RespHdrs0) of
                            {ok, NewResp, NewRespHdrs} ->
                                {NewResp, NewRespHdrs};
                            InterceptError ->
                                error_logger:error_msg(
                                  "revproxy intercept module ~p:"
                                  "rewrite_response failure: ~p~n",
                                  [InterceptMod, InterceptError]),
                                exit({error, intercept_mod})
                        end
                end,

            {CliConn, SrvConn} = get_connection_status(
                                   (Arg#arg.req)#http_request.version,
                                   Arg#arg.headers, RespHdrs
                                  ),
            RPState1 = RPState#revproxy{cliconn_status = CliConn,
                                        srvconn_status = SrvConn,
                                        resp           = Resp,
                                        headers        = RespHdrs},
            if
                RPState1#revproxy.r_meth =:= 'HEAD' ->
                    RPState2 = RPState1#revproxy{state=terminate},
                    out(Arg#arg{state=RPState2});

                Resp#http_response.status =:= 100 orelse
                Resp#http_response.status =:= 204 orelse
                Resp#http_response.status =:= 205 orelse
                Resp#http_response.status =:= 304 orelse
                Resp#http_response.status =:= 406 ->
                    RPState2 = RPState1#revproxy{state=terminate},
                    out(Arg#arg{state=RPState2});

                true ->
                    RPState2 =
                        case {yaws:to_lower(RespHdrs#headers.transfer_encoding),
                              RespHdrs#headers.content_length} of
                            {"chunked", _} ->
                                RPState1#revproxy{state=recvchunk};
                            {_, undefined} ->
                                RPState1#revproxy{
                                  cliconn_status="close",
                                  srvconn_status="close",
                                  state=recvcontent};
                            _ ->
                                RPState1#revproxy{state=recvcontent}
                        end,
                    out(Arg#arg{state=RPState2})
            end;

        {_R, _H} ->
            %% bad_request
            ?Debug("Bad response received from backend server: ~p~n", [_R]),
            close(RPState),
            outXXX(500, Arg);

        closed ->
            ?Debug("TCP error: ~p~n", [closed]),
            outXXX(500, Arg)
    end;


%% The response content is not chunked.
out(#arg{state=RPState}=Arg) when RPState#revproxy.state == recvcontent ->
    Len = case (RPState#revproxy.headers)#headers.content_length of
              undefined -> undefined;
              CLen      -> list_to_integer(CLen)
          end,
    SC=get(sc),
    if
        is_integer(Len) andalso Len =< SC#sconf.partial_post_size ->
            case read(RPState, Len) of
                {ok, Data} ->
                    ?Debug("Response content received from the backend server: "
                           "~p bytes~n", [size(Data)]),
                    RPState1 = RPState#revproxy{state      = terminate,
                                                is_chunked = false,
                                                srvdata    = {content, Data}},
                    out(Arg#arg{state=RPState1});
                {error, Reason} ->
                    ?Debug("TCP error: ~p~n", [Reason]),
                    case Reason of
                        closed -> ok;
                        _      -> close(RPState)
                    end,
                    outXXX(500, Arg)
            end;

        is_integer(Len) ->
            %% Here partial_post_size is always an integer
            BlockSize  = SC#sconf.partial_post_size,
            BlockCount = Len div BlockSize,
            LastBlock  = Len rem BlockSize,
            SrvData    = {block, BlockCount, BlockSize, LastBlock},
            RPState1   = RPState#revproxy{state      = terminate,
                                          is_chunked = true,
                                          srvdata    = SrvData},
            out(Arg#arg{state=RPState1});

        true ->
            SrvData  = {block, undefined, undefined, undefined},
            RPState1 = RPState#revproxy{state      = terminate,
                                        is_chunked = true,
                                        srvdata    = SrvData},
            out(Arg#arg{state=RPState1})
    end;

%% The response content is chunked. Read the first chunk here and spawn a
%% process to read others.
out(#arg{state=RPState}=Arg) when RPState#revproxy.state == recvchunk ->
    case read_chunk(RPState) of
        {ok, Data} ->
            ?Debug("First chunk received from the backend server : "
                   "~p bytes~n", [size(Data)]),
            RPState1 = RPState#revproxy{state      = terminate,
                                        is_chunked = (Data /= <<>>),
                                        srvdata    = {stream, Data}},
            out(Arg#arg{state=RPState1});
        {error, Reason} ->
            ?Debug("TCP error: ~p~n", [Reason]),
            case Reason of
                closed -> ok;
                _      -> close(RPState)
            end,
            outXXX(500, Arg)
    end;


%% Now, we return the result and we let yaws_server deals with it. If it is
%% possible, we try to cache the connection.
out(#arg{state=RPState}=Arg) when RPState#revproxy.state == terminate ->
    case RPState#revproxy.srvconn_status of
        "close" when RPState#revproxy.is_chunked == false -> close(RPState);
        "close" -> ok;
        _  -> cache_connection(RPState)
    end,

    AllHdrs = [{header, H} || H <- yaws_api:reformat_header(
                                     rewrite_server_headers(RPState)
                                    )],
    ?Debug("~p~n", [AllHdrs]),

    Res = [
           {status, (RPState#revproxy.resp)#http_response.status},
           {allheaders, AllHdrs}
          ],
    case RPState#revproxy.srvdata of
        {content, <<>>} ->
            Res;
        {content, Data} ->
            MimeType = (RPState#revproxy.headers)#headers.content_type,
            Res ++ [{content, MimeType, Data}];

        {stream, <<>>} ->
            %% Chunked response with only the last empty chunk: do not spawn a
            %% process to manage chunks
            yaws_api:stream_chunk_end(self()),
            MimeType = (RPState#revproxy.headers)#headers.content_type,
            Res ++ [{streamcontent, MimeType, <<>>}];

        {stream, Chunk} ->
            Self = self(),
            GC   = get(gc),
            spawn(fun() -> put(gc, GC), recv_next_chunk(Self, Arg) end),
            MimeType = (RPState#revproxy.headers)#headers.content_type,
            Res ++ [{streamcontent, MimeType, Chunk}];

        {block, BlockCnt, BlockSz, LastBlock} ->
            GC  = get(gc),
            Pid = spawn(fun() ->
                                put(gc, GC),
                                receive
                                    {ok, YawsPid} ->
                                        recv_blocks(YawsPid, Arg, BlockCnt,
                                                    BlockSz, LastBlock);
                                    {discard, YawsPid} ->
                                        recv_blocks(YawsPid, Arg, 0, BlockSz, 0)
                                end
                        end),
            MimeType = (RPState#revproxy.headers)#headers.content_type,
            Res ++ [{streamcontent_from_pid, MimeType, Pid}];

        _ ->
            Res
    end;


%% Catch unexpected state by sending an error 500
out(#arg{state=RPState}=Arg) ->
    ?Debug("Unexpected revproxy state:~n - ~s~n",
           [?format_record(RPState, revproxy)]),
    case RPState#revproxy.srvsock of
        undefined -> ok;
        _         -> close(RPState)
    end,
    outXXX(500, Arg).


%%==========================================================================
out404(Arg) ->
    SC=get(sc),
    (SC#sconf.errormod_404):out404(Arg,get(gc),SC).


outXXX(Code, _Arg) ->
    Content = ["<html><h1>", integer_to_list(Code), $\ ,
               yaws_api:code_to_phrase(Code), "</h1></html>"],
    [
     {status, Code},
     {header, {connection, "close"}},
     {content, "text/html", Content}
    ].


%%==========================================================================
%% This function is used to read a chunk and to stream it to the client.
recv_next_chunk(YawsPid, #arg{state=RPState}=Arg) ->
    case read_chunk(RPState) of
        {ok, <<>>} ->
            ?Debug("Last chunk received from the backend server~n", []),
            yaws_api:stream_chunk_end(YawsPid),
            case RPState#revproxy.srvconn_status of
                "close" -> close(RPState);
                _       -> ok %% Cached by the main process
            end;
        {ok, Data} ->
            ?Debug("Next chunk received from the backend server : "
                   "~p bytes~n", [size(Data)]),
            yaws_api:stream_chunk_deliver(YawsPid, Data),
            recv_next_chunk(YawsPid, Arg);
        {error, Reason} ->
            ?Debug("TCP error: ~p~n", [Reason]),
            yaws_api:stream_chunk_end(YawsPid),
            case Reason of
                closed -> ok;
                _      -> close(RPState)
            end
    end.

%%==========================================================================
%% This function reads blocks from the server and streams them to the client.
recv_blocks(YawsPid, #arg{state=RPState}=Arg,
            undefined, undefined, undefined) ->
    case read(RPState) of
        {ok, <<>>} ->
            %% no data, wait 100 msec to avoid time-consuming loop and retry
            timer:sleep(100),
            recv_blocks(YawsPid, Arg, undefined, undefined, undefined);
        {ok, Data} ->
            ?Debug("Response content received from the backend server : "
                   "~p bytes~n", [size(Data)]),
            ok = yaws_api:stream_process_deliver(Arg#arg.clisock, Data),
            recv_blocks(YawsPid, Arg, undefined, undefined, undefined);
        {error, closed} ->
            yaws_api:stream_process_end(closed, YawsPid);
        {error, _Reason} ->
            ?Debug("TCP error: ~p~n", [_Reason]),
            yaws_api:stream_process_end(closed, YawsPid),
            close(RPState)
    end;
recv_blocks(YawsPid, #arg{state=RPState}=Arg, 0, _, 0) ->
    yaws_api:stream_process_end(Arg#arg.clisock, YawsPid),
    case RPState#revproxy.srvconn_status of
        "close" -> close(RPState);
        _       -> ok %% Cached by the main process
    end;
recv_blocks(YawsPid, #arg{state=RPState}=Arg, 0, _, LastBlock) ->
    Sock = Arg#arg.clisock,
    case read(RPState, LastBlock) of
        {ok, Data} ->
            ?Debug("Response content received from the backend server : "
                   "~p bytes~n", [size(Data)]),
            ok = yaws_api:stream_process_deliver(Sock, Data),
            yaws_api:stream_process_end(Sock, YawsPid),
            case RPState#revproxy.srvconn_status of
                "close" -> close(RPState);
                _       -> ok %% Cached by the main process
            end;
        {error, Reason} ->
            ?Debug("TCP error: ~p~n", [Reason]),
            yaws_api:stream_process_end(closed, YawsPid),
            case Reason of
                closed -> ok;
                _      -> close(RPState)
            end
    end;
recv_blocks(YawsPid, #arg{state=RPState}=Arg, BlockCnt, BlockSz, LastBlock) ->
    case read(RPState, BlockSz) of
        {ok, Data} ->
            ?Debug("Response content received from the backend server : "
                   "~p bytes~n", [size(Data)]),
            ok = yaws_api:stream_process_deliver(Arg#arg.clisock, Data),
            recv_blocks(YawsPid, Arg, BlockCnt-1, BlockSz, LastBlock);
        {error, Reason} ->
            ?Debug("TCP error: ~p~n", [Reason]),
            yaws_api:stream_process_end(closed, YawsPid),
            case Reason of
                closed -> ok;
                _      -> close(RPState)
            end
    end.

%%==========================================================================
%% TODO: find a better way to cache connections to backend servers. Here we can
%% have 1 connection per gserv process for each backend server.
get_cached_connection(URL) ->
    Key = lists:flatten(yaws_api:reformat_url(URL)),
    case erase(Key) of
        undefined ->
            undefined;
        {Sock, nossl} ->
            case gen_tcp:recv(Sock, 0, 1) of
                {error, closed} ->
                    ?Debug("Invalid cached connection~n", []),
                    undefined;
                _ ->
                    ?Debug("Found cached connection to ~s~n", [Key]),
                    {ok, Sock, nossl}
            end;
        {Sock, ssl} ->
            case ssl:recv(Sock, 0, 1) of
                {error, closed} ->
                    ?Debug("Invalid cached connection~n", []),
                    undefined;
                _ ->
                    ?Debug("Found cached connection to ~s~n", [Key]),
                    {ok, Sock, ssl}
            end
    end.

cache_connection(RPState) ->
    Key = lists:flatten(yaws_api:reformat_url(RPState#revproxy.url)),
    ?Debug("Cache connection to ~s~n", [Key]),
    InitDB0 = get(init_db),
    InitDB1 = lists:keystore(
                Key, 1, InitDB0,
                {Key, {RPState#revproxy.srvsock, RPState#revproxy.type}}
               ),
    put(init_db, InitDB1),
    ok.


%%==========================================================================
connect(URL) ->
    case get_cached_connection(URL) of
        {ok, Sock, Type} -> {ok, Sock, Type};
        undefined        -> do_connect(URL)
    end.

do_connect(URL) ->
    Opts = [
            binary,
            {packet,    raw},
            {active,    false},
            {reuseaddr, true}
           ],
    case URL#url.scheme of
        http  ->
            Port = case URL#url.port of
                       undefined -> 80;
                       P         -> P
                   end,
            case yaws:tcp_connect(URL#url.host, Port, Opts) of
                {ok, S} -> {ok, S, nossl};
                Err     -> Err
            end;
        https ->
            Port = case URL#url.port of
                       undefined -> 443;
                       P         -> P
                   end,
            case yaws:ssl_connect(URL#url.host, Port, Opts) of
                {ok, S} -> {ok, S, ssl};
                Err     -> Err
            end;
        _ ->
            {error, unsupported_protocol}
    end.

send(#revproxy{srvsock=Sock, type=ssl}, Data) ->
    ssl:send(Sock, Data);
send(#revproxy{srvsock=Sock, type=nossl}, Data) ->
    gen_tcp:send(Sock, Data).


read(#revproxy{srvsock=Sock, type=Type}) ->
    yaws:setopts(Sock, [{packet, raw}, binary], Type),
    yaws:do_recv(Sock, 0, Type).

read(RPState, Len) ->
    yaws:setopts(RPState#revproxy.srvsock, [{packet, raw}, binary],
                 RPState#revproxy.type),
    read(RPState, Len, []).

read(_, 0, Data) ->
    {ok, iolist_to_binary(lists:reverse(Data))};
read(RPState = #revproxy{srvsock=Sock, type=Type}, Len, Data) ->
    case yaws:do_recv(Sock, Len, Type) of
        {ok, Bin}       -> read(RPState, Len-size(Bin), [Bin|Data]);
        {error, Reason} -> {error, Reason}
    end.

read_chunk(#revproxy{srvsock=Sock, type=Type}) ->
    try
        yaws:setopts(Sock, [binary, {packet, line}], Type),
        %% Ignore chunk extentions
        {Len, _Exts} = yaws:get_chunk_header(Sock, Type),
        yaws:setopts(Sock, [binary, {packet, raw}], Type),
        if
            Len == 0 ->
                %% Ignore chunk trailer
                yaws:get_chunk_trailer(Sock, Type),
                {ok, <<>>};
            true ->
                B = yaws:get_chunk(Sock, Len, 0, Type),
                ok = yaws:eat_crnl(Sock, Type),
                {ok, iolist_to_binary(B)}
        end
    catch
        _:Reason ->
            {error, Reason}
    end.


close(#revproxy{srvsock=Sock, type=ssl}) ->
    ssl:close(Sock);
close(#revproxy{srvsock=Sock, type=nossl}) ->
    gen_tcp:close(Sock).


get_connection_status(Version, ReqHdrs, RespHdrs) ->
    CliConn = case Version of
                  {0,9} ->
                      "close";
                  {1, 0} ->
                      case ReqHdrs#headers.connection of
                          undefined -> "close";
                          C1        -> yaws:to_lower(C1)
                      end;
                  {1, 1} ->
                      case ReqHdrs#headers.connection of
                          undefined -> "keep-alive";
                          C1        -> yaws:to_lower(C1)
                      end
              end,
    ?Debug("Client Connection header: ~p~n", [CliConn]),

    %% below, ignore dialyzer warning:
    %% "The pattern 'true' can never match the type 'false'"
    SrvConn = case ?proxy_keepalive of
                  true ->
                      case RespHdrs#headers.connection of
                          undefined -> CliConn;
                          C2        -> yaws:to_lower(C2)
                      end;
                  false ->
                      "close"
              end,
    ?Debug("Server Connection header: ~p~n", [SrvConn]),
    {CliConn, SrvConn}.

%%==========================================================================
rewrite_request(RPState, Req) ->
    ?Debug("Request path to rewrite:  ~p~n", [Req#http_request.path]),
    {abs_path, Path} = Req#http_request.path,
    NewPath = strip_prefix(Path, RPState#revproxy.prefix),
    ?Debug("New Request path: ~p~n", [NewPath]),
    Req#http_request{path = {abs_path, NewPath}}.


rewrite_client_headers(RPState, Hdrs) ->
    ?Debug("Host header to rewrite:  ~p~n", [Hdrs#headers.host]),
    Host = case Hdrs#headers.host of
               undefined ->
                   undefined;
               _ ->
                   ProxyUrl = RPState#revproxy.url,
                   [ProxyUrl#url.host,
                    case ProxyUrl#url.port of
                        undefined -> [];
                        P         -> [$:|integer_to_list(P)]
                    end]
           end,
    ?Debug("New Host header: ~p~n", [Host]),
    Hdrs#headers{host = Host}.


rewrite_server_headers(RPState) ->
    Hdrs = RPState#revproxy.headers,
    ?Debug("Location header to rewrite:  ~p~n", [Hdrs#headers.location]),
    Loc = case Hdrs#headers.location of
              undefined ->
                  undefined;
              L ->
                  ?Debug("parse_url(~p)~n", [L]),
                  LocUrl   = (catch yaws_api:parse_url(L)),
                  ProxyUrl = RPState#revproxy.url,
                  if
                      LocUrl#url.scheme == ProxyUrl#url.scheme andalso
                      LocUrl#url.host   == ProxyUrl#url.host   andalso
                      LocUrl#url.port   == ProxyUrl#url.port ->
                          rewrite_loc_url(RPState, LocUrl);

                      element(1, L) == 'EXIT' ->
                          rewrite_loc_rel(RPState, L);

                      true ->
                          L
                  end
          end,
    ?Debug("New Location header: ~p~n", [Loc]),

    %% FIXME: And we also should do cookies here ...

    Hdrs#headers{location = Loc, connection = RPState#revproxy.cliconn_status}.


%% Rewrite a properly formatted location redir
rewrite_loc_url(RPState, LocUrl) ->
    SC=get(sc),
    Scheme    = yaws:redirect_scheme(SC),
    RedirHost = yaws:redirect_host(SC, RPState#revproxy.r_host),
    [Scheme, RedirHost, slash_append(RPState#revproxy.prefix, LocUrl#url.path)].


%% This is the case for broken webservers that reply with
%% Location: /path
%% or even worse, Location: path
rewrite_loc_rel(RPState, Loc) ->
    SC=get(sc),
    Scheme    = yaws:redirect_scheme(SC),
    RedirHost = yaws:redirect_host(SC, RPState#revproxy.r_host),
    [Scheme, RedirHost, Loc].



strip_prefix("", "") ->
    "/";
strip_prefix(P, "") ->
    P;
strip_prefix(P, "/") ->
    P;
strip_prefix([H|T1], [H|T2]) ->
    strip_prefix(T1, T2).


slash_append("/", [$/|T]) ->
    [$/|T];
slash_append("/", T) ->
    [$/|T];
slash_append([], [$/|T]) ->
    [$/|T];
slash_append([], T) ->
    [$/|T];
slash_append([H|T], X) ->
    [H | slash_append(T, X)].
