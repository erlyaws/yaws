-module(intercept2).
-export([rewrite_request/2, rewrite_response/2]).

rewrite_request(Req, Hdrs) ->
    {ok, Req, Hdrs}.

%% Test that returning an incorrect return value from rewrite_request
%% is handled correctly by the reverse proxy.
rewrite_response(_, _) ->
    error.
