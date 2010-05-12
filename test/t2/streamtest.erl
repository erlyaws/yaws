-module(streamtest).
-export([out/1, streamer/1, close_streamer/2]).
-include("../../include/yaws_api.hrl").


out(Arg) ->
    Sock = Arg#arg.clisock,
    Url = yaws_api:request_url(Arg),
    case lists:reverse(string:tokens(Url#url.path, "/")) of
        ["1"|_] ->
            Pid1 = spawn(?MODULE, streamer, [Sock]),
            {streamcontent_from_pid, "text/plain", Pid1};
        ["2"|_] ->
            Msg = "closing the socket",
            Pid2 = spawn(?MODULE, close_streamer, [Sock, Msg]),
            [{header, {content_length, length(Msg)}},
             {streamcontent_from_pid, "text/plain", Pid2}]
    end.

streamer(Sock) ->
    receive
        {discard, YawsPid} ->
            yaws_api:stream_process_end(Sock, YawsPid);
        {ok, YawsPid} ->
            yaws_api:stream_process_deliver_final_chunk(
              Sock,
              ["this", " is", <<" an">>, <<" iolist">>]
             ),
            yaws_api:stream_process_end(Sock, YawsPid)
    end.

close_streamer(Sock, Msg) ->
    receive
        {discard, YawsPid} ->
            yaws_api:stream_process_end(Sock, YawsPid);
        {ok, YawsPid} ->
            gen_tcp:send(Sock, Msg),
            timer:sleep(3000),
            gen_tcp:close(Sock),
            yaws_api:stream_process_end(closed, YawsPid)
    end.
