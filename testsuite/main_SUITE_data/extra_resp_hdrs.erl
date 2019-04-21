-module(extra_resp_hdrs).
-export([extra_response_headers/1]).

-include("yaws.hrl").

extra_response_headers(#outh{}) ->
    [{"X-ExtraMod", atom_to_list(?MODULE)}].
