-module(app_test).
-compile(export_all).

-include("yaws.hrl").
-include_lib("ibrowse/include/ibrowse.hrl").
-include("tftest.hrl").


%% Way to invoke just one test
start([F]) ->
    ?line {ok, _} = ibrowse:start_link(),
    apply(app_test, F, []),
    ibrowse:stop().

start() ->
    io:format("\n ==== DHFILE TESTS ==== \n\n", []),
    ?line {ok, _} = ibrowse:start_link(),
    test_ssl_with_valid_dhfile(),
    test_ssl_with_invalid_dhfile(),
    ibrowse:stop().

start_ssl() ->
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok.

stop_ssl() ->
    ok = application:stop(ssl),
    ok = application:stop(public_key),
    ok = application:stop(asn1),
    ok = application:stop(crypto),
    ok.

test_ssl_with_valid_dhfile() ->
    io:format("ssl with valid dhfile\n", []),
    start_ssl(),

    %% The server has its own Diffie-Hellman group. Try connecting
    %% with ephemeral DH and see if it works.
    Uri = "https://localhost:8443/index.yaws",
    Options = [ {is_ssl, true}
              , {ssl_options, [ {verify, 0}
                              , {ciphers, [C || {dhe_rsa, _, _}=C
                                                    <- ssl:cipher_suites()]}
                              ] }
              ],
    ?line {ok, "200", _, _} = ibrowse:send_req(Uri, [], post, <<>>, Options),

    stop_ssl().

test_ssl_with_invalid_dhfile() ->
    io:format("ssl with invalid dhfile\n", []),
    start_ssl(),

    %% ssl:listen/2 succeeds even when an invalid dhfile is given, and
    %% then fails on ssl:ssl_accept/2. This sounds like a bug in
    %% ssl:listen/2 but that's how it works anyway.
    Uri = "https://localhost:8444/index.yaws",
    Options = [ {is_ssl, true}
              , {ssl_options, [ {verify, 0}
                              , {ciphers, [C || {dhe_rsa, _, _}=C
                                                    <- ssl:cipher_suites()]}
                              ] }
              ],
    ?line {error, {conn_failed, _}}
        = ibrowse:send_req(Uri, [], post, <<>>, Options),

    stop_ssl().
