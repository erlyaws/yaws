-module(extra_resp_hdrs).
-export([extra_response_headers/3]).

-include("yaws_api.hrl").

%% This test adds extra headers to the response. The addition of
%% Set-Cookie verifies that the response to the client contains
%% multiple instances of the Set-Cookie header in the order specified
%% here. The addition of X-Multi verifies that only on such header is
%% in the client response, with a comma-separated value consisting of
%% the two values here.
%%
%% The #arg{} record in the function head verifies that the
%% appmod_name field is set to one used by the test client.
extra_response_headers(OutHdrs, #arg{appmod_name=AppmodName}, _Status) ->
    true = AppmodName =:= appmod_strip_undefined_bindings orelse
        AppmodName =:= status orelse
        AppmodName =:= undefined,
    OutHdrs#{"X-ExtraMod" => atom_to_list(?MODULE),
             "Set-Cookie" => {multi, ["cookie1=ABCDEFG",
                                      "cookie2=1234567"]},
             "X-Multi" => {multi, ["value1", "value2"]}}.
