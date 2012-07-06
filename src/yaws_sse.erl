%%%----------------------------------------------------------------------
%%% File    : yaws_sse.erl
%%% Author  : Steve Vinoski <vinoski@ieee.org>
%%% Purpose : Support for Server-Sent Events
%%% Created : 31 May 2012 by Steve Vinoski <vinoski@ieee.org>
%%%----------------------------------------------------------------------
-module(yaws_sse).
-author('vinoski@ieee.org').

-export([headers/1,
         event/0, event/1,
         data/0, data/1, data/2,
         id/0, id/1,
         retry/0, retry/1,
         comment/1,
         send_events/2, send_events/3
        ]).

headers(StreamPid) ->
    [{status, 200},
     {header, {"Cache-Control", "no-cache"}},
     {header, {connection, "close"}},
     {header, {transfer_encoding, erase}},
     {streamcontent_from_pid, "text/event-stream", StreamPid}].

event() ->
    <<"event\n">>.
event(EventName) ->
    [<<"event:">>, EventName, <<"\n">>].

data() ->
    <<"data\n">>.
data(Data) ->
    [<<"data:">>, Data, <<"\n">>].
%% The version below trims out all embedded newlines. If you send data
%% containing newlines using the version above, your events will be
%% misinterpreted at the client. If the result of trimming includes
%% any empty strings or binaries, they are dropped and not sent.
data(Data0, [trim]) ->
    Bin = iolist_to_binary(Data0),
    Tokens = case catch binary:split(Bin, <<"\n">>, [global, trim]) of
                 {'EXIT', {undef, [{binary,split,__}|_]}} ->
                     %% handle older releases of Erlang
                     Lst = binary_to_list(Bin),
                     [Tok || Tok <- string:tokens(Lst, "\n")];
                 Bins ->
                     [B || B <- Bins, B /= <<>>]
             end,
    [data(Data) || Data <- Tokens].

id() ->
    <<"id\n">>.
id(Id) when is_integer(Id) ->
    [<<"id:">>, integer_to_list(Id), <<"\n">>];
id(Id) ->
    [<<"id:">>, Id, <<"\n">>].

retry() ->
    <<"retry\n">>.
retry(ReconnectionTime) ->
    [<<"retry:">>, integer_to_list(ReconnectionTime), <<"\n">>].

comment(Comment) ->
    [<<":">>, Comment, <<"\n">>].

send_events(Socket, Events) ->
    send_events(Socket, Events, fun yaws_api:stream_process_deliver/2).
send_events(Socket, Events, SendFun) ->
    SendFun(Socket, [Events, <<"\n">>]).
