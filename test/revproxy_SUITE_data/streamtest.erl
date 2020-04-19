-module(streamtest).
-export([out/1]).

-include("../../include/yaws.hrl").
-include("../../include/yaws_api.hrl").

out(_Arg) ->
    yaws_api:stream_chunk_deliver(self(), "and this is the second one\n"),
    yaws_api:stream_chunk_deliver(self(), "con"),
    yaws_api:stream_chunk_deliver(self(), "sequence"),
    yaws_api:stream_chunk_end(self()),
    {streamcontent, "text/plain", "This is the data in the first chunk\n"}.
