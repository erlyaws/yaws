-module(app_test).
-compile(export_all).

-include_lib("ibrowse/include/ibrowse.hrl").
-include("tftest.hrl").

%% Way to invoke just one test
start([F]) ->
    ?line ok = start_ssl_app(),
    ?line {ok, _} = ibrowse:start_link(),
    apply(app_test, F, []),
    ibrowse:stop(),
    stop_ssl_app().

start() ->
    %% This testsuite should not be called without a specific test
    %% (corresponding to a specific yaws configuration)
    io:format("SKIPPED\n", []),
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

sni_disabled() ->
    io:format("sni_disabled\n", []),

    ?line ok = check_good_request("localhost:8443", undefined, "localhost:8443"),
    ?line ok = check_good_request("yaws.example.com:8443", undefined, "yaws.example.com:8443"),
    ?line ok = check_good_request("localhost:8443", "alice.example.com", "localhost:8443"),
    ?line ok = check_good_request("nomatch_servername:8443", undefined, "localhost:8443"),
    ok.


-ifdef(HAVE_SSL_SNI).

-include_lib("public_key/include/public_key.hrl").

-define(YAWS_CERT_ISSUER,
        "C=SE, ST=Gondwanaland, L=Yawstown, O=Hyber Inc., OU=Janitorial section, CN=localhost/nobody@nowhere.gondwanaland").

-define(ALICE_SNI_CERT_ISSUER,
        "C=SE, ST=Gondwanaland, L=Yawstown, O=Hyber Inc., OU=Janitorial section, CN=alice.sni.example.com/nobody@nowhere.gondwanaland").

-define(YAWS_SNI_CERT_ISSUER,
        "C=SE, ST=Gondwanaland, L=Yawstown, O=Hyber Inc., OU=Janitorial section, CN=yaws.sni.example.com/nobody@nowhere.gondwanaland").


sni_enabled() ->
    io:format("sni_enabled\n", []),
    ?line ok = check_good_request("localhost:8443", undefined, "localhost:8443"),
    ?line ok = check_good_request("localhost:8443", "localhost", "localhost:8443"),
    ?line ok = check_good_request("nomatch_servername:8443", undefined, "localhost:8443"),
    ?line ok = check_good_request("nomatch_servername:8443", "nomatch_servername", "localhost:8443"),

    ?line ok = check_good_request("alice.sni.example.com:8443", undefined, "alice.sni.example.com:8443"),
    ?line ok = check_good_request("alice.sni.example.com:8443", "alice.sni.example.com", "alice.sni.example.com:8443"),
    ?line ok = check_good_request("bob.sni.example.com:8443", undefined, "alice.sni.example.com:8443"),
    ?line ok = check_good_request("bob.sni.example.com:8443", "bob.sni.example.com", "alice.sni.example.com:8443"),

    ?line ok = check_good_request("yaws.sni.example.com:8443", undefined, "yaws.sni.example.com:8443"),
    ?line ok = check_good_request("yaws.sni.example.com:8443", "yaws.sni.example.com", "yaws.sni.example.com:8443"),
    ?line ok = check_good_request("sni.example.com:8443", undefined, "yaws.sni.example.com:8443"),
    ?line ok = check_good_request("sni.example.com:8443", "sni.example.com", "yaws.sni.example.com:8443"),
    ?line ok = check_good_request("test.sni.example.com:8443", undefined, "yaws.sni.example.com:8443"),
    ?line ok = check_good_request("test.sni.example.com:8443", "test.sni.example.com", "yaws.sni.example.com:8443"),

    ?line ok = check_bad_request("alice.sni.example.com:8443", "localhost"),

    ?line ok = check_ssl_cert("localhost",             ?YAWS_CERT_ISSUER),
    ?line ok = check_ssl_cert("nomatch_servername",    ?YAWS_CERT_ISSUER),
    ?line ok = check_ssl_cert(undefined,               ?YAWS_CERT_ISSUER),
    ?line ok = check_ssl_cert("alice.sni.example.com", ?ALICE_SNI_CERT_ISSUER),
    ?line ok = check_ssl_cert("bob.sni.example.com",   ?ALICE_SNI_CERT_ISSUER),
    ?line ok = check_ssl_cert("yaws.sni.example.com",  ?YAWS_SNI_CERT_ISSUER),
    ?line ok = check_ssl_cert("sni.example.com",       ?YAWS_SNI_CERT_ISSUER),
    ?line ok = check_ssl_cert("test.sni.example.com",  ?YAWS_SNI_CERT_ISSUER),
    ok.

sni_strict() ->
    io:format("sni_strict\n", []),
    ?line ok = check_good_request("localhost:8443", "localhost", "localhost:8443"),
    ?line ok = check_good_request("nomatch_servername:8443", "nomatch_servername", "localhost:8443"),

    ?line ok = check_good_request("alice.sni.example.com:8443", "alice.sni.example.com", "alice.sni.example.com:8443"),
    ?line ok = check_good_request("bob.sni.example.com:8443", "bob.sni.example.com", "alice.sni.example.com:8443"),

    ?line ok = check_good_request("yaws.sni.example.com:8443", "yaws.sni.example.com", "yaws.sni.example.com:8443"),
    ?line ok = check_good_request("sni.example.com:8443", "sni.example.com", "yaws.sni.example.com:8443"),
    ?line ok = check_good_request("test.sni.example.com:8443", "test.sni.example.com", "yaws.sni.example.com:8443"),

    ?line ok = check_bad_request("alice.sni.example.com:8443", "localhost"),
    ?line ok = check_bad_request("localhost:8443", undefined),
    ?line ok = check_bad_request("nomatch_servername:8443", undefined),
    ?line ok = check_bad_request("alice.sni.example.com:8443", undefined),
    ?line ok = check_bad_request("bob.sni.example.com:8443", undefined),
    ?line ok = check_bad_request("yaws.sni.example.com:8443", undefined),
    ?line ok = check_bad_request("sni.example.com:8443", undefined),
    ?line ok = check_bad_request("test.sni.example.com:8443", undefined),


    ?line ok = check_ssl_cert("localhost",             ?YAWS_CERT_ISSUER),
    ?line ok = check_ssl_cert("nomatch_servername",    ?YAWS_CERT_ISSUER),
    ?line ok = check_ssl_cert(undefined,               ?YAWS_CERT_ISSUER),
    ?line ok = check_ssl_cert("alice.sni.example.com", ?ALICE_SNI_CERT_ISSUER),
    ?line ok = check_ssl_cert("bob.sni.example.com",   ?ALICE_SNI_CERT_ISSUER),
    ?line ok = check_ssl_cert("yaws.sni.example.com",  ?YAWS_SNI_CERT_ISSUER),
    ?line ok = check_ssl_cert("sni.example.com",       ?YAWS_SNI_CERT_ISSUER),
    ?line ok = check_ssl_cert("test.sni.example.com",  ?YAWS_SNI_CERT_ISSUER),
    ok.

sni_required_on_vhost() ->
    io:format("sni_required_on_vhost\n", []),

    ?line ok = check_good_request("localhost:8443", "localhost", "localhost:8443"),

    ?line ok = check_good_request("alice.sni.example.com:8443", "alice.sni.example.com", "alice.sni.example.com:8443"),
    ?line ok = check_good_request("bob.sni.example.com:8443", "bob.sni.example.com", "alice.sni.example.com:8443"),

    ?line ok = check_good_request("yaws.sni.example.com:8443", undefined, "yaws.sni.example.com:8443"),
    ?line ok = check_good_request("yaws.sni.example.com:8443", "yaws.sni.example.com", "yaws.sni.example.com:8443"),
    ?line ok = check_good_request("sni.example.com:8443", undefined, "yaws.sni.example.com:8443"),
    ?line ok = check_good_request("sni.example.com:8443", "sni.example.com", "yaws.sni.example.com:8443"),
    ?line ok = check_good_request("test.sni.example.com:8443", undefined, "yaws.sni.example.com:8443"),
    ?line ok = check_good_request("test.sni.example.com:8443", "test.sni.example.com", "yaws.sni.example.com:8443"),

    ?line ok = check_bad_request("alice.sni.example.com:8443", "localhost"),
    ?line ok = check_bad_request("localhost:8443", undefined),
    ?line ok = check_bad_request("nomatch_servername:8443", undefined),
    ?line ok = check_bad_request("alice.sni.example.com:8443", undefined),
    ?line ok = check_bad_request("bob.sni.example.com:8443", undefined),

    ?line ok = check_ssl_cert("localhost",             ?YAWS_CERT_ISSUER),
    ?line ok = check_ssl_cert("nomatch_servername",    ?YAWS_CERT_ISSUER),
    ?line ok = check_ssl_cert(undefined,               ?YAWS_CERT_ISSUER),
    ?line ok = check_ssl_cert("alice.sni.example.com", ?ALICE_SNI_CERT_ISSUER),
    ?line ok = check_ssl_cert("bob.sni.example.com",   ?ALICE_SNI_CERT_ISSUER),
    ?line ok = check_ssl_cert("yaws.sni.example.com",  ?YAWS_SNI_CERT_ISSUER),
    ?line ok = check_ssl_cert("sni.example.com",       ?YAWS_SNI_CERT_ISSUER),
    ?line ok = check_ssl_cert("test.sni.example.com",  ?YAWS_SNI_CERT_ISSUER),
    ok.

check_good_request(HttpHost, SniHost, SN) ->
    Uri = "https://localhost:8443/index.yaws",
    Hdrs = [{connection, "close"}],
    SSLOpts = case SniHost of
                  undefined -> [{verify, 0}, {server_name_indication, disable}];
                  H         -> [{verify, 0}, {server_name_indication, H}]
              end,
    Opts = [{host_header, HttpHost}, {is_ssl, true}, {ssl_options, SSLOpts}],
    ?line {ok, "200", _, Body} = ibrowse:send_req(Uri, Hdrs, get, <<>>, Opts),
    ?line Body = "servername: "++SN++"\n",
    ok.

check_bad_request(HttpHost, SniHost) ->
    Uri = "https://localhost:8443/index.yaws",
    Hdrs = [{connection, "close"}],
    SSLOpts = case SniHost of
                  undefined -> [{verify, 0}, {server_name_indication, disable}];
                  H         -> [{verify, 0}, {server_name_indication, H}]
              end,
    Opts = [{host_header, HttpHost}, {is_ssl, true}, {ssl_options, SSLOpts}],
    ?line {ok, "400", _, _} = ibrowse:send_req(Uri, Hdrs, get, <<>>, Opts),
    ok.

check_ssl_cert(SniHost, Issuer) ->
    SSLOpts = case SniHost of
                  undefined -> [{verify, 0}, {server_name_indication, disable}];
                  H         -> [{verify, 0}, {server_name_indication, H}]
              end,
    ?line {ok, Sock} = ssl:connect("localhost", 8443, SSLOpts),
    ?line Issuer = get_issuer(Sock),
    ?line ok = ssl:close(Sock),
    ok.

get_issuer(Sock) ->
    ?line {ok, Cert} = ssl:peercert(Sock),
    ?line OTPCert = public_key:pkix_decode_cert(Cert, otp),
    ?line C  = get_attr_value(OTPCert, ?'id-at-countryName'),
    ?line ST = get_attr_value(OTPCert, ?'id-at-stateOrProvinceName'),
    ?line L  = get_attr_value(OTPCert, ?'id-at-localityName'),
    ?line O  = get_attr_value(OTPCert, ?'id-at-organizationName'),
    ?line OU = get_attr_value(OTPCert, ?'id-at-organizationalUnitName'),
    ?line CN = get_attr_value(OTPCert, ?'id-at-commonName'),
    ?line E  = get_attr_value(OTPCert, ?'id-emailAddress'),
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

-else.
check_good_request(HttpHost, _SniHost, SN) ->
    Uri = "https://localhost:8443/index.yaws",
    Hdrs = [{connection, "close"}],
    SSLOpts = [{verify, 0}],
    Opts = [{host_header, HttpHost}, {is_ssl, true}, {ssl_options, SSLOpts}],
    ?line {ok, "200", _, Body} = ibrowse:send_req(Uri, Hdrs, get, <<>>, Opts),
    ?line Body = "servername: "++SN++"\n",
    ok.
-endif.
