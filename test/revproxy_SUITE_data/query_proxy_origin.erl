-module(query_proxy_origin).
-export([out/1]).

-include("yaws_api.hrl").

out(#arg{req = #http_request{method = 'OPTIONS'}}) ->
    [{status, 200}, {header, {allow, "OPTIONS, QUERY"}}, accept_query()];
out(#arg{req = #http_request{method = 'GET', path = {abs_path, Target}}}) ->
    {content, "application/octet-stream", ["GET ", Target]};
out(#arg{clidata = {partial, Bytes}, cont = Cont}) ->
    {get_more, [Bytes | fragments(Cont)], undefined};
out(#arg{req = #http_request{method = Method, path = {abs_path, Target}},
         server_path = Path, headers = Headers, clisock = Sock,
         clidata = Bytes, cont = Cont}) ->
    Body = iolist_to_binary([lists:reverse(fragments(Cont)), Bytes]),
    case Path of
        "/jobs/continue" ->
            Interim = "HTTP/1.1 100 Continue\r\n\r\n",
            ok = gen_tcp:send(Sock, [Interim, Interim]),
            echo(Method, Target, Headers, Body);
        "/jobs/conditional" ->
            case yaws_api:get_header(Headers, if_match) of
                "\"stale\"" -> [{status, 412}, accept_query()];
                _ -> {status, 200}
            end;
        "/jobs/unsupported" ->
            [{status, 415}, accept_query()];
        "/jobs/relative" ->
            [{status, 303}, {header, {location, "../result?view=a%2Bb+1&limit=2"}}];
        "/jobs/absolute" ->
            [{status, 303}, {header, {location, absolute_result(Headers)}}];
        "/jobs/query-only" ->
            [{status, 303}, {header, {location, "?view=new%20value"}}];
        "/jobs/content" ->
            content_result("/result?view=content%2Fref", "/result?view=content%2Fref");
        "/jobs/content-absolute" ->
            content_result(absolute_result(Headers), "/result?view=absolute%26ref");
        "/jobs/external" ->
            [{status, 303}, {header, {location, "https://example.invalid/result?view=elsewhere"}},
             {header, {"Content-Location", "//example.invalid/result?view=elsewhere"}}];
        _ ->
            echo(Method, Target, Headers, Body)
    end.

echo(Method, Target, Headers, Body) ->
    Names = [content_type, content_encoding, if_match, if_none_match,
             content_length, transfer_encoding, {lower, "expect"}],
    Values = [yaws_api:get_header(Headers, Name) || Name <- Names],
    {content, "application/octet-stream", term_to_binary({Method, Target, Values, Body})}.

absolute_result(Headers) ->
    Host = string:uppercase(yaws_api:get_header(Headers, host)),
    "HTTP://" ++ Host ++ "/result?view=absolute%26ref".

content_result(Reference, Target) ->
    [{header, {"Content-Location", Reference}},
     {content, "application/octet-stream", ["GET ", Target]}].

accept_query() ->
    {header, {"Accept-Query", "application/vnd.example.query"}}.

fragments(undefined) -> [];
fragments(Cont) -> Cont.
