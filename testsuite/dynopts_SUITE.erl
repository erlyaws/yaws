-module(dynopts_SUITE).

-include("testsuite.hrl").

-compile(export_all).

all() ->
    [
     compare_version,
     default_dynopts,
     generated_dynopts
    ].

groups() ->
    [
    ].

%%====================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Test, _Config) ->
    ok.

init_per_testcase(_Test, Config) ->
    %% Be sure to purge generated module, if any
    code:purge(yaws_dynopts),
    code:delete(yaws_dynopts),
    Config.

end_per_testcase(_Test, _Config) ->
    ok.

%%====================================================================
compare_version(_Config) ->
    ?assert(yaws_dynopts:is_greater("2",         "1")),
    ?assert(yaws_dynopts:is_greater("2.0",       "1.0")),
    ?assert(yaws_dynopts:is_greater("1.1",       "1.0")),
    ?assert(yaws_dynopts:is_greater("1.1",       "1.0.0")),
    ?assert(yaws_dynopts:is_greater("1.1.0",     "1.0.1")),
    ?assert(yaws_dynopts:is_greater("1.0.2",     "1.0.1")),
    ?assert(yaws_dynopts:is_greater("1.0.2-rc1", "1.0.1")),
    ?assert(yaws_dynopts:is_greater("1.0.2",     "1.0.1-rc1")),
    ?assert(yaws_dynopts:is_greater("1.0.2-rc1", "1.0.1-rc1")),
    ?assert(yaws_dynopts:is_greater("1.0.2-rc1", "1.0-rc1")),
    ok.

default_dynopts(Config) ->
    ?assertNot(yaws_dynopts:is_generated()),
    ?assertEqual(ok, check_ssl_honor_cipher_order(Config)),
    ?assertEqual(ok, check_ssl_client_renegotiation(Config)),
    ?assertEqual(ok, check_ssl_sni(Config)),
    ?assertEqual(ok, check_ssl_log_alert(Config)),
    ?assertEqual(ok, check_erlang_sendfile(Config)),
    ?assertEqual(ok, check_erlang_now(Config)),
    ?assertEqual(ok, check_rand(Config)),
    ok.

generated_dynopts(_Config) ->
    ?assertNot(yaws_dynopts:is_generated()),
    SSLHonorCipherOrder = yaws_dynopts:have_ssl_honor_cipher_order(),
    SSLCliReneg         = yaws_dynopts:have_ssl_client_renegotiation(),
    SSLSni              = yaws_dynopts:have_ssl_sni(),
    SSLLogAlert         = yaws_dynopts:have_ssl_log_alert(),
    ErlSendfile         = yaws_dynopts:have_erlang_sendfile(),
    ErlNow              = yaws_dynopts:have_erlang_now(),
    Rand                = yaws_dynopts:have_rand(),

    GC = yaws_config:make_default_gconf(false, "dummy_id"),
    ?assertEqual(ok, yaws_dynopts:generate(GC)),

    ?assert(yaws_dynopts:is_generated()),
    ?assertEqual(SSLHonorCipherOrder, yaws_dynopts:have_ssl_honor_cipher_order()),
    ?assertEqual(SSLCliReneg,         yaws_dynopts:have_ssl_client_renegotiation()),
    ?assertEqual(SSLSni,              yaws_dynopts:have_ssl_sni()),
    ?assertEqual(SSLLogAlert,         yaws_dynopts:have_ssl_log_alert()),
    ?assertEqual(ErlSendfile,         yaws_dynopts:have_erlang_sendfile()),
    ?assertEqual(ErlNow,              yaws_dynopts:have_erlang_now()),
    ?assertEqual(Rand,                yaws_dynopts:have_rand()),
    ok.

%%====================================================================
check_ssl_honor_cipher_order(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    case yaws_dynopts:have_ssl_honor_cipher_order() of
        true ->
            {ok, Sock} = ssl:listen(Port, [{reuseaddr, true}, {honor_cipher_order, true}]),
            ok = ssl:close(Sock);
        false ->
            {'EXIT', badarg} =
                (catch ssl:listen(Port, [{reuseaddr, true}, {honor_cipher_order, true}]))
    end,
    ok.

check_ssl_client_renegotiation(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    case yaws_dynopts:have_ssl_client_renegotiation() of
        true ->
            {ok, Sock} = ssl:listen(Port, [{reuseaddr, true}, {client_renegotiation, true}]),
            ok = ssl:close(Sock);
        false ->
            {'EXIT',badarg} =
                (catch ssl:listen(Port, [{reuseaddr, true}, {client_renegotiation, true}]))
    end,
    ok.

check_ssl_sni(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    case yaws_dynopts:have_ssl_sni() of
        true ->
            {ok, Sock} = ssl:listen(Port, [{reuseaddr, true}, {sni_fun, fun(_) -> [] end}]),
            ok = ssl:close(Sock);
        false ->
            {'EXIT', badarg} =
                (catch ssl:listen(Port, [{reuseaddr, true}, {sni_fun, fun(_) -> [] end}]))
    end,
    ok.

check_ssl_log_alert(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    case yaws_dynopts:have_ssl_log_alert() of
        true ->
            {ok, Sock} = ssl:listen(Port, [{reuseaddr, true}, {log_alert, true}]),
            ok = ssl:close(Sock);
        false ->
            {'EXIT', badarg} =
                (catch ssl:listen(Port, [{reuseaddr, true}, {log_alert, true}]))
    end,
    ok.

check_erlang_sendfile(_Config) ->
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

check_erlang_now(_Config) ->
    Funs = erlang:module_info(exports),
    case yaws_dynopts:have_erlang_now() of
        true  -> false = lists:member({unique_integer, 1}, Funs);
        false -> true  = lists:member({unique_integer, 1}, Funs)
    end,
    ok.

check_rand(_Config) ->
    case yaws_dynopts:have_rand() of
        true  -> {module, rand} = code:ensure_loaded(rand);
        false -> {error, _}     = code:ensure_loaded(rand)
    end,
    ok.
