-module(dhfile_SUITE).

-include("testsuite.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(SSL_OTP26_DHFILE_FIX, [11, 1, 2]).
-define(SSL_VALIDATE_DHFILE_AT_LISTEN, [11, 4]).

all() ->
    [
     ssl_with_valid_dhfile,
     ssl_with_invalid_dhfile
    ].

groups() ->
    [
    ].

init_per_suite(Config) ->
    {ok, SslVsn} = application:get_key(ssl, vsn),
    SslVersion = [list_to_integer(Part) || Part <- string:tokens(SslVsn, ".")],
    case ?OTP_RELEASE =:= 26 andalso SslVersion < ?SSL_OTP26_DHFILE_FIX of
        true ->
            {skip, otp26_broken_dhfile};
        false ->
            [{ssl_version, SslVersion} | Config]
    end.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(Test, Config) ->
    ConfigFile = case Test of
                     ssl_with_valid_dhfile -> "yaws.conf";
                     ssl_with_invalid_dhfile -> "yaws_invalid_dhfile.conf"
                 end,
    ok = application:load(yaws),
    ok = application:set_env(yaws, id, "testsuite-server"),
    ok = application:set_env(yaws, conf,
                             filename:join(?tempdir(?MODULE), ConfigFile)),
    Config.

end_per_testcase(_Test, _Config) ->
    case application:stop(yaws) of
        ok -> ok;
        {error, {not_started, yaws}} -> ok
    end,
    ok = application:unload(yaws).

%%====================================================================
ssl_with_valid_dhfile(Config) ->
    {ok, _} = application:ensure_all_started(yaws, temporary),
    Port = testsuite:get_yaws_port(1, Config),
    Url = testsuite:make_url(https, "127.0.0.1", Port, "/index.yaws"),
    HttpOpts = [{connect_timeout, 5000}, {timeout, 5000}],
    ?assertMatch({ok, {{_, 200, _}, _, _}},
                 testsuite:http_get(Url, [], HttpOpts, dhe_client_options())).

ssl_with_invalid_dhfile(Config) ->
    StartResult = application:ensure_all_started(yaws, temporary),
    case ?config(ssl_version, Config) >= ?SSL_VALIDATE_DHFILE_AT_LISTEN of
        true ->
            ?assertMatch({error, {yaws,
                                  {{shutdown, {failed_to_start_child,
                                               yaws_server, {badbind, _}}},
                                   {yaws_app, start, _}}}}, StartResult);
        false ->
            ?assertMatch({ok, _}, StartResult),
            Port = testsuite:get_yaws_port(1, Config),
            case ssl:connect("127.0.0.1", Port, dhe_client_options(), 5000) of
                {error, closed} -> ok;
                {error, {tls_alert, _}} -> ok;
                Result -> ct:fail({expected_tls_handshake_failure, Result})
            end
    end.

dhe_client_options() ->
    Ciphers = [Cipher || #{key_exchange := dhe_rsa} = Cipher
                             <- ssl:cipher_suites(all, 'tlsv1.2')],
    [{versions, ['tlsv1.2']}, {ciphers, Ciphers}, {verify, verify_none}].
