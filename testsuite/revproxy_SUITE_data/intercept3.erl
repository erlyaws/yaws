-module(intercept3).
-export([rewrite_request/2, rewrite_response/2]).

-include("../../include/yaws_api.hrl").

rewrite_request(Req, Hdrs) ->
    {ok, Req, yaws_api:set_header(Hdrs, accept, "text/plain")}.

rewrite_response(Resp, Hdrs) ->
    {ok, Resp, yaws_api:set_header(Hdrs, "X-Test-Interception", "true")}.
