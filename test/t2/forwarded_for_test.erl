-module(forwarded_for_test).
-export([out/1]).

-include("yaws_api.hrl").

out(Arg) ->
    Fwd = yaws_api:headers_x_forwarded_for(Arg#arg.headers),
    {status,
     case string:tokens(Fwd, ", ") of
         ["192.168.1.1","192.168.1.2"] ->
             200;
         _ ->
             400
     end}.

