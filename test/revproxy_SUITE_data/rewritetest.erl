-module(rewritetest).
-export([arg_rewrite/1]).

-include("../../include/yaws.hrl").
-include("../../include/yaws_api.hrl").


arg_rewrite(Arg) ->
    Url = yaws_api:request_url(Arg),
    case Url#url.path of
        "/rewrite" ++ Rest ->
            Req0 = Arg#arg.req,
            Req1 = Req0#http_request{path={abs_path,Rest}},
            Arg#arg{req=Req1};
        _ ->
            Arg
    end.
