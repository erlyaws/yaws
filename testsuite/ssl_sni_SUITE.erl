-module(ssl_sni_SUITE).

-include("testsuite.hrl").
-include_lib("kernel/include/file.hrl").

-export([
    sni_disabled/1,
    sni_enabled/1,
    sni_strict/1,
    sni_required_on_vhost/1,
    sni_not_enabled/1,
    sni_without_tls/1,
    sni_not_available/1
]).
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

all() ->
    [
     sni_disabled,
     sni_enabled,
     sni_strict,
     sni_required_on_vhost,
     sni_not_enabled,
     sni_without_tls,
     sni_not_available
    ].

groups() ->
    [
    ].

-include_lib("public_key/include/public_key.hrl").

-define(YAWS_CERT_ISSUER,
        "C=SE, ST=Gondwanaland, L=Yawstown, O=Hyber Inc., OU=Janitorial section, CN=localhost/nobody@nowhere.gondwanaland").

-define(ALICE_SNI_CERT_ISSUER,
        "C=SE, ST=Gondwanaland, L=Yawstown, O=Hyber Inc., OU=Janitorial section, CN=alice.sni.example.com/nobody@nowhere.gondwanaland").

-define(YAWS_SNI_CERT_ISSUER,
        "C=SE, ST=Gondwanaland, L=Yawstown, O=Hyber Inc., OU=Janitorial section, CN=yaws.sni.example.com/nobody@nowhere.gondwanaland").

%%====================================================================
init_per_suite(Config) ->
    Id    = "testsuite-server",
    YConf = filename:join(?tempdir(?MODULE), "yaws.conf"),
    application:load(yaws),
    application:set_env(yaws, id,   Id),
    application:set_env(yaws, conf, YConf),
    ok = yaws:start(),
    [{yaws_id, Id}, {yaws_config, YConf} | Config].

end_per_suite(_Config) ->
    ok = application:stop(yaws),
    ok = application:unload(yaws),
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(sni_disabled, Config) ->
    restart_yaws("yaws_sni_disabled.conf", Config);
init_per_testcase(sni_enabled, Config) ->
    case yaws_dynopts:have_ssl_sni() of
        true  -> restart_yaws("yaws_sni_enabled.conf", Config);
        false -> {skip, "sni not supported on this Erlang/OTP release"}
    end;
init_per_testcase(sni_strict, Config) ->
    case yaws_dynopts:have_ssl_sni() of
        true  -> restart_yaws("yaws_sni_strict.conf", Config);
        false -> {skip, "sni not supported on this Erlang/OTP release"}
    end;
init_per_testcase(sni_required_on_vhost, Config) ->
    case yaws_dynopts:have_ssl_sni() of
        true  -> restart_yaws("yaws_sni_required_on_vhost.conf", Config);
        false -> {skip, "sni not supported on this Erlang/OTP release"}
    end;
init_per_testcase(sni_without_tls, Config) ->
    case yaws_dynopts:have_ssl_sni() of
        true  -> Config;
        false -> {skip, "sni not supported on this Erlang/OTP release"}
    end;
init_per_testcase(sni_not_available, Config) ->
    case yaws_dynopts:have_ssl_sni() of
        true  -> {skip, "sni supported on this Erlang/OTP release"};
        false -> Config
    end;
init_per_testcase(_Test, Config) ->
    Config.

end_per_testcase(_Test, _Config) ->
    ok.

%%====================================================================
sni_disabled(Config) ->
    Port  = testsuite:get_yaws_port(1, Config),
    Url   = testsuite:make_url(https, "127.0.0.1", Port, "/index.yaws"),
    SPort = integer_to_list(Port),
    case yaws_dynopts:have_ssl_sni() of
        true ->
            ?assertEqual(ok, check_good_request(Url, "localhost:"++SPort, undefined, "localhost:"++SPort)),
            ?assertEqual(ok, check_good_request(Url, "yaws.example.com:"++SPort, undefined, "yaws.example.com:"++SPort)),
            ?assertEqual(ok, check_good_request(Url, "localhost:"++SPort, "alice.example.com", "localhost:"++SPort)),
            ?assertEqual(ok, check_good_request(Url, "nomatch_servername:"++SPort, undefined, "localhost:"++SPort)),
            ok;
        false ->
            ?assertEqual(ok, check_good_request(Url, "localhost:"++SPort, "localhost:"++SPort)),
            ?assertEqual(ok, check_good_request(Url, "yaws.example.com:"++SPort, "yaws.example.com:"++SPort)),
            ?assertEqual(ok, check_good_request(Url, "localhost:"++SPort, "localhost:"++SPort)),
            ?assertEqual(ok, check_good_request(Url, "nomatch_servername:"++SPort, "localhost:"++SPort)),
            ok
    end.


sni_enabled(Config) ->
    Port  = testsuite:get_yaws_port(1, Config),
    Url   = testsuite:make_url(https, "127.0.0.1", Port, "/index.yaws"),
    SPort = integer_to_list(Port),
    ?assertEqual(ok, check_good_request(Url, "localhost:"++SPort, undefined, "localhost:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "localhost:"++SPort, "localhost", "localhost:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "nomatch_servername:"++SPort, undefined, "localhost:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "nomatch_servername:"++SPort, "nomatch_servername", "localhost:"++SPort)),

    ?assertEqual(ok, check_good_request(Url, "alice.sni.example.com:"++SPort, undefined, "alice.sni.example.com:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "alice.sni.example.com:"++SPort, "alice.sni.example.com", "alice.sni.example.com:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "bob.sni.example.com:"++SPort, undefined, "alice.sni.example.com:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "bob.sni.example.com:"++SPort, "bob.sni.example.com", "alice.sni.example.com:"++SPort)),

    ?assertEqual(ok, check_good_request(Url, "yaws.sni.example.com:"++SPort, undefined, "yaws.sni.example.com:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "yaws.sni.example.com:"++SPort, "yaws.sni.example.com", "yaws.sni.example.com:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "sni.example.com:"++SPort, undefined, "yaws.sni.example.com:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "sni.example.com:"++SPort, "sni.example.com", "yaws.sni.example.com:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "test.sni.example.com:"++SPort, undefined, "yaws.sni.example.com:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "test.sni.example.com:"++SPort, "test.sni.example.com", "yaws.sni.example.com:"++SPort)),

    ?assertEqual(ok, check_bad_request(Url, "alice.sni.example.com:"++SPort, "localhost")),

    ?assertEqual(ok, check_ssl_cert(Port, "localhost",             ?YAWS_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, "nomatch_servername",    ?YAWS_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, undefined,               ?YAWS_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, "alice.sni.example.com", ?ALICE_SNI_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, "bob.sni.example.com",   ?ALICE_SNI_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, "yaws.sni.example.com",  ?YAWS_SNI_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, "sni.example.com",       ?YAWS_SNI_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, "test.sni.example.com",  ?YAWS_SNI_CERT_ISSUER)),
    ok.

sni_strict(Config) ->
    Port  = testsuite:get_yaws_port(1, Config),
    Url   = testsuite:make_url(https, "127.0.0.1", Port, "/index.yaws"),
    SPort = integer_to_list(Port),
    ?assertEqual(ok, check_good_request(Url, "localhost:"++SPort, "localhost", "localhost:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "nomatch_servername:"++SPort, "nomatch_servername", "localhost:"++SPort)),

    ?assertEqual(ok, check_good_request(Url, "alice.sni.example.com:"++SPort, "alice.sni.example.com", "alice.sni.example.com:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "bob.sni.example.com:"++SPort, "bob.sni.example.com", "alice.sni.example.com:"++SPort)),

    ?assertEqual(ok, check_good_request(Url, "yaws.sni.example.com:"++SPort, "yaws.sni.example.com", "yaws.sni.example.com:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "sni.example.com:"++SPort, "sni.example.com", "yaws.sni.example.com:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "test.sni.example.com:"++SPort, "test.sni.example.com", "yaws.sni.example.com:"++SPort)),

    ?assertEqual(ok, check_bad_request(Url, "alice.sni.example.com:"++SPort, "localhost")),
    ?assertEqual(ok, check_bad_request(Url, "localhost:"++SPort, undefined)),
    ?assertEqual(ok, check_bad_request(Url, "nomatch_servername:"++SPort, undefined)),
    ?assertEqual(ok, check_bad_request(Url, "alice.sni.example.com:"++SPort, undefined)),
    ?assertEqual(ok, check_bad_request(Url, "bob.sni.example.com:"++SPort, undefined)),
    ?assertEqual(ok, check_bad_request(Url, "yaws.sni.example.com:"++SPort, undefined)),
    ?assertEqual(ok, check_bad_request(Url, "sni.example.com:"++SPort, undefined)),
    ?assertEqual(ok, check_bad_request(Url, "test.sni.example.com:"++SPort, undefined)),

    ?assertEqual(ok, check_ssl_cert(Port, "localhost",             ?YAWS_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, "nomatch_servername",    ?YAWS_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, undefined,               ?YAWS_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, "alice.sni.example.com", ?ALICE_SNI_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, "bob.sni.example.com",   ?ALICE_SNI_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, "yaws.sni.example.com",  ?YAWS_SNI_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, "sni.example.com",       ?YAWS_SNI_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, "test.sni.example.com",  ?YAWS_SNI_CERT_ISSUER)),
    ok.

sni_required_on_vhost(Config) ->
    Port  = testsuite:get_yaws_port(1, Config),
    Url   = testsuite:make_url(https, "127.0.0.1", Port, "/index.yaws"),
    SPort = integer_to_list(Port),
    ?assertEqual(ok, check_good_request(Url, "localhost:"++SPort, "localhost", "localhost:"++SPort)),

    ?assertEqual(ok, check_good_request(Url, "alice.sni.example.com:"++SPort, "alice.sni.example.com", "alice.sni.example.com:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "bob.sni.example.com:"++SPort, "bob.sni.example.com", "alice.sni.example.com:"++SPort)),

    ?assertEqual(ok, check_good_request(Url, "yaws.sni.example.com:"++SPort, undefined, "yaws.sni.example.com:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "yaws.sni.example.com:"++SPort, "yaws.sni.example.com", "yaws.sni.example.com:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "sni.example.com:"++SPort, undefined, "yaws.sni.example.com:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "sni.example.com:"++SPort, "sni.example.com", "yaws.sni.example.com:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "test.sni.example.com:"++SPort, undefined, "yaws.sni.example.com:"++SPort)),
    ?assertEqual(ok, check_good_request(Url, "test.sni.example.com:"++SPort, "test.sni.example.com", "yaws.sni.example.com:"++SPort)),

    ?assertEqual(ok, check_bad_request(Url, "alice.sni.example.com:"++SPort, "localhost")),
    ?assertEqual(ok, check_bad_request(Url, "localhost:"++SPort, undefined)),
    ?assertEqual(ok, check_bad_request(Url, "nomatch_servername:"++SPort, undefined)),
    ?assertEqual(ok, check_bad_request(Url, "alice.sni.example.com:"++SPort, undefined)),
    ?assertEqual(ok, check_bad_request(Url, "bob.sni.example.com:"++SPort, undefined)),

    ?assertEqual(ok, check_ssl_cert(Port, "localhost",             ?YAWS_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, "nomatch_servername",    ?YAWS_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, undefined,               ?YAWS_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, "alice.sni.example.com", ?ALICE_SNI_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, "bob.sni.example.com",   ?ALICE_SNI_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, "yaws.sni.example.com",  ?YAWS_SNI_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, "sni.example.com",       ?YAWS_SNI_CERT_ISSUER)),
    ?assertEqual(ok, check_ssl_cert(Port, "test.sni.example.com",  ?YAWS_SNI_CERT_ISSUER)),
    ok.


sni_not_enabled(_Config) ->
    Env = #env{debug=false, conf={file, ?tempdir(?MODULE) ++ "/yaws_sni_not_enabled.conf"}},
    ?assertMatch({error, _}, yaws_config:load(Env)),
    ok.

sni_without_tls(_Config) ->
    Env = #env{debug=false, conf={file, ?tempdir(?MODULE) ++ "/yaws_sni_without_tls.conf"}},
    ?assertMatch({error, _}, yaws_config:load(Env)),
    ok.

sni_not_available(_Config) ->
    Env = #env{debug=false, conf={file, ?tempdir(?MODULE) ++ "/yaws_sni_not_available.conf"}},
    ?assertMatch({error, _}, yaws_config:load(Env)),
    ok.

%%====================================================================
restart_yaws(File, Config)->
    YConf = filename:join(?tempdir(?MODULE), File),
    application:set_env(yaws, conf, YConf),
    ok = yaws:stop(),
    ok = yaws:start(),
    [{yaws_ssl_config, YConf} | Config].

%%====================================================================
%% non SNI version
check_good_request(Url, HttpHost, SN) ->
    Res  = iolist_to_binary([<<"servername: ">>, SN, $\n]),

    ConnHdr = {"Connection", "close"},
    HostHdr = {"Host", HttpHost},

    ?assertMatch({ok, {{_,200,_}, _, Res}}, testsuite:http_get(Url, [HostHdr, ConnHdr])),
    ok.

%% SNI version
check_good_request(Url, HttpHost, SniHost, SN) ->
    Res  = iolist_to_binary([<<"servername: ">>, SN, $\n]),

    ConnHdr = {"Connection", "close"},
    HostHdr = {"Host", HttpHost},
    SSLOpts = case SniHost of
                  undefined -> [{server_name_indication, disable}];
                  H         -> [{server_name_indication, H}]
              end,

    ?assertMatch({ok, {{_,200,_}, _, Res}}, testsuite:http_get(Url, [HostHdr, ConnHdr], [], SSLOpts)),
    ok.

check_bad_request(Url, HttpHost, SniHost) ->
    ConnHdr = {"Connection", "close"},
    HostHdr = {"Host", HttpHost},
    SSLOpts = case SniHost of
                  undefined -> [{server_name_indication, disable}];
                  H         -> [{server_name_indication, H}]
              end,
    ?assertMatch({ok, {{_,400,_}, _, _}}, testsuite:http_get(Url, [HostHdr, ConnHdr], [], SSLOpts)),
    ok.

check_ssl_cert(Port, SniHost, Issuer) ->
    SSLOpts = case SniHost of
                  undefined -> [{verify, verify_none}, {server_name_indication, disable}];
                  H         -> [{verify, verify_none}, {server_name_indication, H}]
              end,
    {ok, Sock} = ssl:connect("127.0.0.1", Port, SSLOpts),
    ?assertEqual(Issuer, get_issuer(Sock)),
    ?assertEqual(ok, ssl:close(Sock)),
    ok.

get_issuer(Sock) ->
    {ok, Cert} = ssl:peercert(Sock),
    OTPCert = public_key:pkix_decode_cert(Cert, otp),
    C  = get_attr_value(OTPCert, ?'id-at-countryName'),
    ST = get_attr_value(OTPCert, ?'id-at-stateOrProvinceName'),
    L  = get_attr_value(OTPCert, ?'id-at-localityName'),
    O  = get_attr_value(OTPCert, ?'id-at-organizationName'),
    OU = get_attr_value(OTPCert, ?'id-at-organizationalUnitName'),
    CN = get_attr_value(OTPCert, ?'id-at-commonName'),
    E  = get_attr_value(OTPCert, ?'id-emailAddress'),
    lists:flatten(
      io_lib:format("C=~s, ST=~s, L=~s, O=~s, OU=~s, CN=~s/~s",
                    [C, ST, L, O, OU, CN, E])
     ).

get_attr_value(OTPCert, Id) ->
    {rdnSequence, Subject} = OTPCert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subject,
    case [ATV#'AttributeTypeAndValue'.value
          || [ATV] <- Subject, ATV#'AttributeTypeAndValue'.type == Id] of
        [Att] ->
            case Att of
                {teletexString,   Str}  -> Str;
                {printableString, Str}  -> Str;
                {utf8String,      Bin}  -> binary_to_list(Bin);
                Str when is_list(Str)   -> Str;
                Bin when is_binary(Bin) -> binary_to_list(Bin);
                _                       -> "unexpected"
            end;
        _ ->
            "unknown"
    end.
