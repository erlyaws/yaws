-module(dispatchmod_tester).
-export([dispatch/1]).
-include("../../include/yaws_api.hrl").

dispatch(#arg{clisock=Sock}=A) ->
    {abs_path, Path} = (A#arg.req)#http_request.path,
    case Path of
        "/done" ->
            ok = gen_tcp:send(Sock, <<"HTTP/1.1 204 No Content\r\nX-DispatchMod: true\r\n\r\n">>),
            done;
        "/closed" ->
            ok = gen_tcp:send(Sock, <<"HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n">>),
            gen_tcp:close(Sock),
            closed;
        _ ->
            continue
    end.
