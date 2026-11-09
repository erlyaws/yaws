-module(netns_vrf_ns_SUITE).

%% Needs CAP_NET_ADMIN in its own network namespace, without real root:
%%
%%   YAWS_NETNS_TESTS=1 unshare -Urnm make -C test common_test \
%%       SUITES=netns_vrf_ns_SUITE
%%
%% Skipped unless YAWS_NETNS_TESTS is set and the privilege probe succeeds,
%% so it never touches the host network configuration by accident.

-include("testsuite.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(NS1, "yaws_t_ns1").
-define(NS2, "yaws_t_ns2").
-define(PORT, 18080).

all() ->
    [
     %%%
     %%% UNCOMMENT MANUALLY TO RUN THESE TESTS
     %%%
     %listen_same_port_in_different_netns,
     %listen_same_addr_in_different_vrf
    ].

groups() ->
    [
    ].

%%====================================================================
init_per_suite(Config) ->
    case os:getenv("YAWS_NETNS_TESTS") of
        false ->
            {skip, "YAWS_NETNS_TESTS not set"};
        _ ->
            case sh("ip link add yawsprobe type dummy") of
                {0, _} ->
                    sh("ip link del yawsprobe"),
                    setup(Config);
                {_, Out} ->
                    {skip, "no CAP_NET_ADMIN (try unshare -Urnm): " ++ Out}
            end
    end.

setup(Config) ->
    sh("ip link set lo up"),
    Steps = ["ip netns add " ?NS1,
             "ip netns add " ?NS2,
             "ip netns exec " ?NS1 " ip link set lo up",
             "ip netns exec " ?NS2 " ip link set lo up"]
        ++ lists:append([vrf_setup(V, T) || {V, T} <- [{"yawsvrf1", 101},
                                                       {"yawsvrf2", 102}]]),
    case [R || S <- Steps, {C, _}=R <- [sh(S)], C /= 0] of
        [] ->
            Config;
        [{_, Out}|_] ->
            end_per_suite(Config),
            {skip, "namespace setup failed: " ++ Out}
    end.

%% Each vrf gets a dummy device carrying the same address
vrf_setup(Vrf, Table) ->
    Dev = Vrf ++ "d",
    [io_lib:format("ip link add ~s type vrf table ~w", [Vrf, Table]),
     "ip link set " ++ Vrf ++ " up",
     "ip link add " ++ Dev ++ " type dummy",
     "ip link set " ++ Dev ++ " master " ++ Vrf,
     "ip addr add 10.99.0.1/24 dev " ++ Dev,
     "ip link set " ++ Dev ++ " up"].

end_per_suite(_Config) ->
    [sh(C) || C <- ["ip netns del " ?NS1, "ip netns del " ?NS2,
                    "ip link del yawsvrf1", "ip link del yawsvrf2",
                    "ip link del yawsvrf1d", "ip link del yawsvrf2d"]],
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
sh(Cmd) ->
    Out = os:cmd(lists:flatten(Cmd) ++ " 2>&1; echo \"\n$?\""),
    Lines = string:split(string:trim(Out), "\n", trailing),
    [Code|Rest] = lists:reverse(Lines),
    {list_to_integer(Code), string:join(lists:reverse(Rest), "\n")}.

%% listen_opts as yaws would pass them on, taken from the sconf soptions
lopts(SC) ->
    proplists:get_value(listen_opts, yaws:sconf_soptions(SC)).

sconf(Addr, LOpts) ->
    yaws:create_sconf(".", [{listen, Addr}, {port, ?PORT},
                            {soptions, [{listen_opts, LOpts}]}]).

listen(SC) ->
    gen_tcp:listen(?PORT, [{ip, yaws:sconf_listen(SC)}, {reuseaddr, true}
                           | lopts(SC)]).

listen_same_port_in_different_netns(_Config) ->
    Lo = {127,0,0,1},
    SCs = [sconf(Lo, []),
           sconf(Lo, [{netns, "/run/netns/" ?NS1}]),
           sconf(Lo, [{netns, "/run/netns/" ?NS2}])],
    %% three listeners on the same ip:port, one per namespace
    Socks = [listen(SC) || SC <- SCs],
    [{ok, Default}|_] = Socks,

    %% a connect from this namespace reaches the default listener only
    {ok, C} = gen_tcp:connect(Lo, ?PORT, [binary, {active, false}]),
    ?assertMatch({ok, _}, gen_tcp:accept(Default, 2000)),
    gen_tcp:close(C),
    [gen_tcp:close(S) || {ok, S} <- Socks],
    ok.

listen_same_addr_in_different_vrf(_Config) ->
    Addr = {10,99,0,1},
    SCs = [sconf(Addr, [{bind_to_device, <<"yawsvrf1">>}]),
           sconf(Addr, [{bind_to_device, <<"yawsvrf2">>}])],
    Socks = [?assertMatch({ok, _}, listen(SC)) || SC <- SCs],
    [gen_tcp:close(S) || {ok, S} <- Socks],
    ok.
