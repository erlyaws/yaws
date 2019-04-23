-module(status).
-export([out/1]).

out(Arg) ->
    [{"code", Status}] = yaws_api:parse_query(Arg),
    {status, list_to_integer(Status)}.
