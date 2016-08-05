-module(nolengthtest).
-export([out/1]).

-include("../../include/yaws.hrl").
-include("../../include/yaws_api.hrl").

out(_Arg) ->
    yaws_api:stream_chunk_deliver(self(), lists:duplicate(256, $A)),
    yaws_api:stream_chunk_end(self()),
    [
     {header, {transfer_encoding, erase}},
     {header, {content_length, erase}},
     {header, {connection, "close"}},
     {streamcontent, "text/plain", lists:duplicate(256, $A)}
    ].
