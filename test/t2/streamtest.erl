-module(streamtest).
-export([out/1, streamer/1]).
-include("../../include/yaws_api.hrl").


out(Arg) ->
    Sock = Arg#arg.clisock,
    Pid = spawn(?MODULE, streamer, [Sock]),
    {streamcontent_from_pid, "text/plain", Pid}.

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
