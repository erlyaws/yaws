-module(cache_appmod_test).
-export([out/1]).

-include("../../include/yaws_api.hrl").

out(Arg) ->
    {abs_path, Path} = (Arg#arg.req)#http_request.path,
    Opts0 = case yaws_api:queryvar(Arg, "no-cache") of
               {ok, "1"} -> [{disable_cache, true}];
               _         -> []
            end,
    Opts1 = [{header, {"X-Appmod", "cache_appmod_test"}}|Opts0],
    {page, {Opts1, Path}}.

