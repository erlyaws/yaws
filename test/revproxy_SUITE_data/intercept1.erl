-module(intercept1).
-export([rewrite_request/2, rewrite_response/2]).

%% Test that returning an incorrect return value from rewrite_request
%% is handled correctly by the reverse proxy.
rewrite_request(_, _) ->
    error.

rewrite_response(Resp, Hdrs) ->
    {ok, Resp, Hdrs}.
