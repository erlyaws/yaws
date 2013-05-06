-module(rewritetest).
-export([arg_rewrite/1]).

-include("../../include/yaws_api.hrl").


arg_rewrite(Arg) ->
    Url = yaws_api:request_url(Arg),
    case Url#url.path of
        "/test11_1/" ++ Rest ->
            Req0 = Arg#arg.req,
            Req1 = Req0#http_request{path={abs_path,"/test1/" ++ Rest}},
            Arg#arg{req=Req1};
        "/test11_2/" ++ Rest ->
            Req0 = Arg#arg.req,
            Req1 = Req0#http_request{path={abs_path,"/test1/" ++ Rest}},
            Arg#arg{req=Req1};
        "/test11_3/" ++ Rest ->
            Req0 = Arg#arg.req,
            Req1 = Req0#http_request{path={abs_path,"/test1/" ++ Rest}},
            Arg#arg{req=Req1};
        "/test11_4/" ++ Rest ->
            Req0 = Arg#arg.req,
            Req1 = Req0#http_request{path={abs_path,"/test7/" ++ Rest}},
            Arg#arg{req=Req1};
        _ ->
            Arg
    end.
