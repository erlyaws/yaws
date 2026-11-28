-module(query_appmod_test).
-export([arg_rewrite/1, dispatch/1, out/1]).

-include("yaws_api.hrl").

-define(QUERY_TYPE, "application/octet-stream").

arg_rewrite(#arg{req = #http_request{method = Method}} = Arg) ->
    put(query_rewrite_method, Method),
    Arg.

dispatch(#arg{req = #http_request{method = 'QUERY', path = {abs_path, "/query/dispatch-probe"}},
              clidata = undefined, clisock = Sock}) ->
    ok = gen_tcp:send(Sock, <<"HTTP/1.1 204 No Content\r\nConnection: close\r\n\r\n">>),
    gen_tcp:close(Sock),
    closed;
dispatch(#arg{req = #http_request{method = Method}, clidata = undefined}) ->
    put(query_dispatch_methods, {erase(query_rewrite_method), Method}),
    continue.

out(#arg{req = #http_request{method = 'OPTIONS'}}) ->
    [{status, 200}, {header, {allow, "OPTIONS, QUERY"}}, accept_query()];
out(#arg{clidata = {partial, Bytes}, cont = Cont}) ->
    {get_more, [Bytes | fragments(Cont)], undefined};
out(#arg{req = #http_request{method = Method}, clidata = Bytes,
         headers = Headers, querydata = Query, cont = Cont} = Arg) ->
    Parts = lists:reverse(fragments(Cont)),
    Body = iolist_to_binary([Parts, Bytes]),
    Type = yaws_api:headers_content_type(Headers),
    Seen = erase(query_dispatch_methods),
    case {Method, Type, Body} of
        {'QUERY', "text/plain", _} -> [{status, 415}, accept_query()];
        {'QUERY', ?QUERY_TYPE, <<"!">>} -> {status, 422};
        _ ->
            Payload = {Method, Seen, Type, lists:map(fun byte_size/1, Parts),
                       Query, yaws_api:parse_query(Arg), Body},
            {content, "application/octet-stream", term_to_binary(Payload)}
    end.

fragments(undefined) -> [];
fragments(Cont) -> Cont.

accept_query() ->
    {header, {"Accept-Query", ?QUERY_TYPE}}.
