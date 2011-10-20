-module(rewritetest).
-export([arg_rewrite/1]).

-include("../../include/yaws.hrl").
-include("../../include/yaws_api.hrl").


arg_rewrite(Arg) ->
    Url = yaws_api:request_url(Arg),
    case Url#url.path of
        "/rewrite" ->
            Req0 = Arg#arg.req,
            Req1 = Req0#http_request{path={abs_path,"/hello.txt"}},
            Arg#arg{req=Req1};
        "/redirect" ->
            L = "http://www.yakaz.com",
            H = [{header, {location, L}}],
            RwResp = #rewrite_response{status=301, headers=H},
            Arg#arg{state=RwResp};
        "/response" ->
            H = [{header, {content_type, "text/plain"}}],
            C = <<"Goodbye, Cruel World!">>,
            RwResp = #rewrite_response{status=200, headers=H, content=C},
            Arg#arg{state=RwResp};
        _ ->
            Arg
    end.
