-module(header_order).
-export([out/1]).

%% This test verifies that Yaws doesn't unnecessarily reorder headers
%% returned by an appmod. We use a header known to Yaws in the #outh
%% record, Cache-Control, along with fabricated user headers, which
%% are stored in the #outh "other" field, to verify ordering between
%% "known" and "other" and also ordering among "other" headers. See
%% main_SUITE:header_order/1.
out(_) ->
    [{header, {"Cache-Control", "no-cache"}},
     {header, {"X-Test-Header1", "1"}},
     {header, {"X-Test-Header2", "2"}}].
