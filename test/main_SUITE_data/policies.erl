-module(policies).
-export([out/1]).

-include("yaws_api.hrl").

out(Arg) ->
    {abs_path, OrigReqPath} = Arg#arg.orig_req#http_request.path,
    {abs_path, ReqPath} = Arg#arg.req#http_request.path,
    [{status, 200},
     {header, {"X-OrigReqPath", OrigReqPath}},
     {header, {"X-ReqPath", ReqPath}},
     {header, {"X-ServerPath", Arg#arg.server_path}},
     {header, {"X-Pathinfo", Arg#arg.pathinfo}}].
