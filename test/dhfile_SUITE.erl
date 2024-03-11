-module(dhfile_SUITE).

-include("testsuite.hrl").
-include_lib("kernel/include/file.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [
     ssl_with_valid_dhfile,
     ssl_with_invalid_dhfile
    ].

groups() ->
    [
    ].

%% Skip dhfile tests on broken OTP-26 releases.
%% The fix was released in OTP-26.2.3.
-define(OTP26_FIXED_DHFILE, <<"26.2.3">>).

-ifdef(OTP_RELEASE).
  -if(?OTP_RELEASE == 26).
otp26_broken_dhfile() ->
    case otp_release_version("26") of
        {ok, OtpVsn}
          when OtpVsn =< ?OTP26_FIXED_DHFILE ->
            true;
       _ ->
            false
    end.
  -else.
otp26_broken_dhfile() ->
    false.
  -endif.
-endif.

otp_release_version(Rel) ->
    try
        {ok, _OtpVsn} =
            file:read_file(filename:join([code:root_dir(), "releases", Rel,
                                          "OTP_VERSION"]))
    catch
        error:{badmatch, _} ->
            file:read_file(filename:join([code:root_dir(), "OTP_VERSION"]))
    end.

%%====================================================================
init_per_suite(Config) ->
    case otp26_broken_dhfile() of
        true ->
            {skip, otp26_broken_dhfile};
        false ->
            Id    = "testsuite-server",
            YConf = filename:join(?tempdir(?MODULE), "yaws.conf"),
            application:load(yaws),
            application:set_env(yaws, id,   Id),
            application:set_env(yaws, conf, YConf),
            ok = yaws:start(),
            [{yaws_id, Id}, {yaws_config, YConf} | Config]
    end.

end_per_suite(_Config) ->
    ok = application:stop(yaws),
    ok = application:unload(yaws),
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Test, Config) ->
    Config.

end_per_testcase(_Test, _Config) ->
    ok.

%%====================================================================
ssl_with_valid_dhfile(Config) ->
    Port = testsuite:get_yaws_port(1, Config),
    Url = testsuite:make_url(https, "127.0.0.1", Port, "/index.yaws"),

    %% The server has its own Diffie-Hellman group. Try connecting with
    %% ephemeral DH and see if it works.
    SslOpts = [{versions, ['tlsv1.2']}|
               [{ciphers,[C || #{key_exchange := dhe_rsa}=C
                                <- ssl:cipher_suites(all, 'tlsv1.2')]}]],

    ?assertMatch({ok, {{_,200,_}, _, _}}, testsuite:http_get(Url, [], [], SslOpts)),
    ok.

ssl_with_invalid_dhfile(Config) ->
    Port = testsuite:get_yaws_port(2, Config),
    Url = testsuite:make_url(https, "127.0.0.1", Port, "/index.yaws"),

    %% ssl:listen/2 succeeds even when an invalid dhfile is given, and then
    %% fails on ssl:ssl_accept/2. This sounds like a bug in ssl:listen/2 but
    %% that's how it works anyway.
    SslOpts = [{versions, ['tlsv1.2']}|
               [{ciphers, [C || #{key_exchange := dhe_rsa}=C
                                 <- ssl:cipher_suites(all, 'tlsv1.2')]}]],

    ?assertMatch({error, _}, testsuite:http_get(Url, [], [], SslOpts)),
    ok.
