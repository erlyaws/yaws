-module(dynopts).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("yaws.hrl").
-include("tftest.hrl").

default_dynopts_test() ->
    ?assertEqual(ok, start_ssl_app()),
    ?assertNot(yaws_dynopts:is_generated()),
    ?assertEqual(ok, check_bad_unicode()),
    ?assertEqual(ok, check_bad_wildcard()),
    ?assertEqual(ok, check_ssl_honor_cipher_order()),
    ?assertEqual(ok, check_ssl_client_renegotiation()),
    ?assertEqual(ok, check_ssl_sni()),
    ?assertEqual(ok, check_ssl_log_alert()),
    ?assertEqual(ok, check_erlang_sendfile()),
    ?assertEqual(ok, check_crypto_hash()),
    ?assertEqual(ok, check_inet_parse_strict_address()),
    ?assertEqual(ok, check_erlang_now()),
    ?assertEqual(ok, stop_ssl_app()),
    ok.

generated_dynopts_test() ->
    ?assertNot(yaws_dynopts:is_generated()),
    RefVal = [yaws_dynopts:have_bad_unicode(),
              yaws_dynopts:have_bad_wildcard(),
              yaws_dynopts:have_ssl_honor_cipher_order(),
              yaws_dynopts:have_ssl_client_renegotiation(),
              yaws_dynopts:have_ssl_sni(),
              yaws_dynopts:have_ssl_log_alert(),
              yaws_dynopts:have_erlang_sendfile(),
              yaws_dynopts:have_crypto_hash(),
              yaws_dynopts:have_inet_parse_strict_address(),
              yaws_dynopts:have_erlang_now()],
    GC = yaws_config:make_default_gconf(false, "dummy_id"),
    ?assertEqual(ok, yaws_dynopts:generate(GC)),

    ?assert(yaws_dynopts:is_generated()),
    ?assertEqual(RefVal, [yaws_dynopts:have_bad_unicode(),
                          yaws_dynopts:have_bad_wildcard(),
                          yaws_dynopts:have_ssl_honor_cipher_order(),
                          yaws_dynopts:have_ssl_client_renegotiation(),
                          yaws_dynopts:have_ssl_sni(),
                          yaws_dynopts:have_ssl_log_alert(),
                          yaws_dynopts:have_erlang_sendfile(),
                          yaws_dynopts:have_crypto_hash(),
                          yaws_dynopts:have_inet_parse_strict_address(),
                          yaws_dynopts:have_erlang_now()]),
    ok.


start_ssl_app() ->
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok.

stop_ssl_app() ->
    ok = application:stop(ssl),
    ok = application:stop(public_key),
    ok = application:stop(asn1),
    ok = application:stop(crypto),
    ok.

check_bad_unicode() ->
    %% TODO
    case yaws_dynopts:have_bad_unicode() of
        true ->
            ok;
        false ->
            ok
    end,
    ok.

check_bad_wildcard() ->
    File = filename:basename(code:which(?MODULE)),
    Dir  = filename:dirname(code:which(?MODULE)),
    Path = filename:dirname(Dir),
    Pattern = filename:join(filename:basename(Dir), "*.beam"),
    case yaws_dynopts:have_bad_wildcard() of
        true ->
            [] = filelib:wildcard(Pattern, Path);
        false ->
            Res = filelib:wildcard(Pattern, Path),
            true = (length(Res) > 0),
            true = lists:member(filename:join(filename:basename(Dir), File), Res)
    end,
    ok.

check_ssl_honor_cipher_order() ->
    case yaws_dynopts:have_ssl_honor_cipher_order() of
        true ->
            {ok, Sock} = ssl:listen(8443, [{honor_cipher_order, true}]),
            ok = ssl:close(Sock);
        false ->
            {'EXIT', badarg} =
                (catch ssl:listen(8443, [{honor_cipher_order, true}]))
    end,
    ok.

check_ssl_client_renegotiation() ->
    case yaws_dynopts:have_ssl_client_renegotiation() of
        true ->
            {ok, Sock} = ssl:listen(8443, [{client_renegotiation, true}]),
            ok = ssl:close(Sock);
        false ->
            {'EXIT',badarg} =
                (catch ssl:listen(8443, [{client_renegotiation, true}]))
    end,
    ok.

check_ssl_sni() ->
    case yaws_dynopts:have_ssl_sni() of
        true ->
            {ok, Sock} = ssl:listen(8443, [{sni_fun, fun(_) -> [] end}]),
            ok = ssl:close(Sock);
        false ->
            {'EXIT', badarg} =
                (catch ssl:listen(8443, [{sni_fun, fun(_) -> [] end}]))
    end,
    ok.

check_ssl_log_alert() ->
    case yaws_dynopts:have_ssl_log_alert() of
        true ->
            {ok, Sock} = ssl:listen(8443, [{log_alert, true}]),
            ok = ssl:close(Sock);
        false ->
            {'EXIT', badarg} =
                (catch ssl:listen(8443, [{log_alert, true}]))
    end,
    ok.

check_erlang_sendfile() ->
    Funs = file:module_info(exports),
    case yaws_dynopts:have_erlang_sendfile() of
        true ->
            true = lists:member({sendfile, 5}, Funs),
            true = ($R /= hd(erlang:system_info(otp_release)));
        false ->
            case lists:member({sendfile, 5}, Funs) of
                true  -> true = ($R == hd(erlang:system_info(otp_release)));
                false -> ok
            end
    end,
    ok.

check_crypto_hash() ->
    Funs = crypto:module_info(exports),
    case yaws_dynopts:have_crypto_hash() of
        true  -> true  = lists:member({hash, 2}, Funs);
        false -> false = lists:member({hash, 2}, Funs)
    end,
    ok.

check_inet_parse_strict_address() ->
    Funs = inet:module_info(exports),
    case yaws_dynopts:have_inet_parse_strict_address() of
        true  -> true  = lists:member({parse_strict_address, 1}, Funs);
        false -> false = lists:member({parse_strict_address, 1}, Funs)
    end,
    ok.

check_erlang_now() ->
    Funs = erlang:module_info(exports),
    case yaws_dynopts:have_erlang_now() of
        true  -> false = lists:member({unique_integer, 1}, Funs);
        false -> true  = lists:member({unique_integer, 1}, Funs)
    end,
    ok.
