-module(reentranttest).
-export([out/1]).

-include("../../include/yaws.hrl").
-include("../../include/yaws_api.hrl").

out(Arg) ->
    Url = yaws_api:request_url(Arg),
    case Url#url.path of
        "/reentranttest/status" ->
            {page, {[{status, 201}], "/icons/yaws.gif"}};
        "/reentranttest/delayed_headers" ->
            Hdrs = [{header, {cache_control, "no-cache"}},
                    {header, "Etag: static-tag"},
                    {header, "X-Delayed-Header: true"}],
            {page, {Hdrs, "/icons/yaws.gif"}};
        _ ->
            Reason = "unknown path: " ++ Url#url.path,
            [{status, 500}, {html, Reason}]
    end.

