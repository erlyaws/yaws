-module(netns_vrf_SUITE).

-include("testsuite.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [
     get_netns_vrf,
     fmt_netns_vrf,
     eq_sconfs_netns_vrf,
     search_sconf_netns_vrf,
     search_group_netns_vrf,
     update_delete_sconf_netns_vrf,
     same_virt_srv_ssl_netns_vrf,
     validate_cs_groups_netns_vrf
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

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Test, Config) ->
    Config.

end_per_testcase(_Test, _Config) ->
    ok.

%%====================================================================
sconf(LOpts) ->
    yaws:create_sconf(".", [{listen, {127,0,0,1}}, {port, 8080},
                            {soptions, [{listen_opts, LOpts}]}]).

get_netns_vrf(_Config) ->
    ?assertEqual([], yaws:get_netns_vrf(yaws:create_sconf(".", []))),
    ?assertEqual([], yaws:get_netns_vrf(sconf([]))),
    ?assertEqual([], yaws:get_netns_vrf(sconf([{backlog, 10},
                                               {nodelay, true}]))),

    %% only netns and bind_to_device are kept, sorted
    SC = sconf([{netns, "/run/netns/a"}, {backlog, 10},
                {bind_to_device, <<"vrf1">>}]),
    ?assertEqual([{bind_to_device, <<"vrf1">>}, {netns, "/run/netns/a"}],
                 yaws:get_netns_vrf(SC)),

    %% option order is irrelevant
    SC2 = sconf([{bind_to_device, <<"vrf1">>}, {netns, "/run/netns/a"}]),
    ?assertEqual(yaws:get_netns_vrf(SC), yaws:get_netns_vrf(SC2)),
    ok.


fmt_netns_vrf(_Config) ->
    Plain = sconf([]),
    ?assertEqual([], yaws:fmt_netns(Plain)),
    ?assertEqual([], yaws:fmt_vrf(Plain)),

    NS = sconf([{netns, "/run/netns/blue"}]),
    ?assertEqual(" in /run/netns/blue", yaws:fmt_netns(NS)),
    ?assertEqual([], yaws:fmt_vrf(NS)),

    VRF = sconf([{bind_to_device, <<"vrf-red">>}]),
    ?assertEqual([], yaws:fmt_netns(VRF)),
    ?assertEqual(" on vrf-red", yaws:fmt_vrf(VRF)),

    %% empty values are ignored
    Empty = sconf([{netns, ""}, {bind_to_device, <<>>}]),
    ?assertEqual([], yaws:fmt_netns(Empty)),
    ?assertEqual([], yaws:fmt_vrf(Empty)),

    Both = sconf([{netns, "ns1"}, {bind_to_device, <<"vrf1">>}]),
    ?assertEqual(" in ns1", yaws:fmt_netns(Both)),
    ?assertEqual(" on vrf1", yaws:fmt_vrf(Both)),
    ok.


eq_sconfs_netns_vrf(_Config) ->
    A  = sconf([{netns, "ns1"}, {bind_to_device, <<"vrf1">>}]),
    A2 = sconf([{netns, "ns1"}, {bind_to_device, <<"vrf1">>}]),
    ?assert(yaws_config:eq_sconfs(A, A2)),
    ?assert(yaws_config:eq_sconfs(sconf([]), sconf([]))),
    ?assert(yaws_config:eq_sconfs(sconf([{netns, "ns1"}]),
                                  sconf([{netns, "ns1"}]))),
    ?assert(yaws_config:eq_sconfs(sconf([{bind_to_device, <<"vrf1">>}]),
                                  sconf([{bind_to_device, <<"vrf1">>}]))),

    DiffNs  = sconf([{netns, "ns2"}, {bind_to_device, <<"vrf1">>}]),
    DiffVrf = sconf([{netns, "ns1"}, {bind_to_device, <<"vrf2">>}]),
    NoNs    = sconf([{bind_to_device, <<"vrf1">>}]),
    NoVrf   = sconf([{netns, "ns1"}]),
    Plain   = sconf([]),
    [?assertNot(yaws_config:eq_sconfs(A, X))
     || X <- [DiffNs, DiffVrf, NoNs, NoVrf, Plain]],
    ok.


%% Same ip:port is the same virtual server group only if netns and vrf match.
search_sconf_netns_vrf(_Config) ->
    GC = yaws_config:make_default_gconf(false, "netns_vrf_SUITE"),
    Ns1  = sconf([{netns, "ns1"}]),
    Ns2  = sconf([{netns, "ns2"}]),
    Vrf1 = sconf([{bind_to_device, <<"vrf1">>}]),
    Vrf2 = sconf([{bind_to_device, <<"vrf2">>}]),
    Ns1Vrf2 = sconf([{netns, "ns1"}, {bind_to_device, <<"vrf2">>}]),
    Ns2Vrf2 = sconf([{netns, "ns2"}, {bind_to_device, <<"vrf2">>}]),
    None = sconf([]),
    Pairs = [{p_ns1, [Ns1]}, {p_vrf1, [Vrf1]}, {p_none, [None]},
             {p_ns2vrf2, [Ns2Vrf2]}],

    ?assertMatch({p_ns1, _, _},  yaws_config:search_sconf(GC, Ns1, Pairs)),
    ?assertMatch({p_vrf1, _, _}, yaws_config:search_sconf(GC, Vrf1, Pairs)),
    ?assertMatch({p_none, _, _}, yaws_config:search_sconf(GC, None, Pairs)),
    ?assertMatch({p_ns2vrf2, _, _},
                 yaws_config:search_sconf(GC, Ns2Vrf2, Pairs)),
    ?assertEqual(false, yaws_config:search_sconf(GC, Ns2, Pairs)),
    ?assertEqual(false, yaws_config:search_sconf(GC, Vrf2, Pairs)),
    ?assertEqual(false, yaws_config:search_sconf(GC, Ns1Vrf2, Pairs)),
    ok.


%% search_group finds the group a new sconf would join, by ip, port, netns, vrf
search_group_netns_vrf(_Config) ->
    GC = yaws_config:make_default_gconf(false, "netns_vrf_SUITE"),
    Ns1  = sconf([{netns, "ns1"}]),
    Ns2  = sconf([{netns, "ns2"}]),
    Vrf1 = sconf([{bind_to_device, <<"vrf1">>}]),
    Ns2Vrf2 = sconf([{netns, "ns2"}, {bind_to_device, <<"vrf2">>}]),
    None = sconf([]),
    Pairs = [{p_ns1, [Ns1]}, {p_vrf1, [Vrf1]}, {p_none, [None]},
             {p_ns2vrf2, [Ns2Vrf2]}],

    ?assertMatch([{p_ns1, [_]}],  yaws_config:search_group(GC, Ns1, Pairs)),
    ?assertMatch([{p_vrf1, [_]}], yaws_config:search_group(GC, Vrf1, Pairs)),
    ?assertMatch([{p_none, [_]}], yaws_config:search_group(GC, None, Pairs)),
    ?assertMatch([{p_ns2vrf2, [_]}],
                 yaws_config:search_group(GC, Ns2Vrf2, Pairs)),
    ?assertEqual([], yaws_config:search_group(GC, Ns2, Pairs)),

    %% a different port never matches, whatever netns/vrf
    OtherPort = yaws:create_sconf(".", [{listen, {127,0,0,1}}, {port, 8081},
                                        {soptions, [{listen_opts,
                                                     [{netns, "ns1"}]}]}]),
    ?assertEqual([], yaws_config:search_group(GC, OtherPort, Pairs)),
    ok.


%% update_sconf and delete_sconf must only touch the group with the same
%% netns and vrf
update_delete_sconf_netns_vrf(_Config) ->
    GC = yaws_config:make_default_gconf(false, "netns_vrf_SUITE"),
    Ns1  = sconf([{netns, "ns1"}]),
    Ns2  = sconf([{netns, "ns2"}]),
    Ns3  = sconf([{netns, "ns3"}]),
    Vrf1 = sconf([{bind_to_device, <<"vrf1">>}]),
    Ns2Vrf2 = sconf([{netns, "ns2"}, {bind_to_device, <<"vrf2">>}]),
    None = sconf([]),
    Pairs = [{p_ns1, [Ns1]}, {p_ns2, [Ns2]}, {p_vrf1, [Vrf1]}, {p_none, [None]},
             {p_ns2vrf2, [Ns2Vrf2]}],

    Ns1b = yaws:sconf_docroot(Ns1, "/other"),
    Updated = yaws_config:update_sconf(GC, Ns1b, 1, Pairs),
    ?assertMatch([{p_ns1, [_]}, {p_ns2, [_]}, {p_vrf1, [_]}, {p_none, [_]},
                  {p_ns2vrf2, [_]}], Updated),
    [{p_ns1, [U1]}, {p_ns2, [U2]} | _] = Updated,
    ?assertEqual("/other", yaws:sconf_docroot(U1)),
    ?assertEqual(yaws:sconf_docroot(Ns2), yaws:sconf_docroot(U2)),

    ?assertEqual(Pairs, yaws_config:update_sconf(GC, Ns3, 1, Pairs)),

    ?assertMatch([{p_ns1, []}, {p_ns2, [_]}, {p_vrf1, [_]}, {p_none, [_]},
                  {p_ns2vrf2, [_]}], yaws_config:delete_sconf(GC, Ns1, Pairs)),
    ?assertMatch([{p_ns1, [_]}, {p_ns2, []}, {p_vrf1, [_]}, {p_none, [_]},
                  {p_ns2vrf2, [_]}], yaws_config:delete_sconf(GC, Ns2, Pairs)),
    ?assertMatch([{p_ns1, [_]}, {p_ns2, [_]}, {p_vrf1, []}, {p_none, [_]},
                  {p_ns2vrf2, [_]}], yaws_config:delete_sconf(GC, Vrf1, Pairs)),
    ?assertMatch([{p_ns1, [_]}, {p_ns2, [_]}, {p_vrf1, [_]}, {p_none, []},
                  {p_ns2vrf2, [_]}], yaws_config:delete_sconf(GC, None, Pairs)),
    ?assertMatch([{p_ns1, [_]}, {p_ns2, [_]}, {p_vrf1, [_]}, {p_none, [_]},
                  {p_ns2vrf2, []}],
                 yaws_config:delete_sconf(GC, Ns2Vrf2, Pairs)),
    ?assertEqual(Pairs, yaws_config:delete_sconf(GC, Ns3, Pairs)),
    ok.


%% netns/vrf must also separate groups in the SSL/SNI branches
same_virt_srv_ssl_netns_vrf(_Config) ->
    Ssl = #ssl{},
    S = fun(LOpts, SSL) -> yaws:sconf_ssl(sconf(LOpts), SSL) end,
    GC0 = yaws_config:make_default_gconf(false, "netns_vrf_SUITE"),
    GCSni = GC0#gconf{sni = enable},
    GCNoSni = GCSni#gconf{sni = disable},

    lists:foreach(
      fun(GC) ->
              %% same ssl, different netns/vrf: separate groups
              Pairs = [{p, [S([{netns, "ns1"}], Ssl)]}],
              ?assertEqual([], yaws_config:search_group(GC,
                                                        S([{netns, "ns2"}],
                                                          Ssl),
                                                        Pairs)),
              ?assertEqual([], yaws_config:search_group(GC,
                                                        S([{bind_to_device,
                                                            <<"v">>}],
                                                          Ssl),
                                                        Pairs)),
              %% same ssl, same netns: same group
              ?assertMatch([{p, _}],
                           yaws_config:search_group(GC,
                                                    S([{netns, "ns1"}], Ssl),
                                                    Pairs))
      end, [GCSni, GCNoSni]),

    %% with sni, a plain and an ssl server in the same netns share a group
    %% only if sni is enabled and both have ssl
    PairsPlain = [{p, [S([{netns, "ns1"}], undefined)]}],
    ?assertEqual([], yaws_config:search_group(GCSni, S([{netns, "ns2"}], Ssl),
                                              PairsPlain)),
    ?assertMatch([{p, _}], yaws_config:search_group(GCSni,
                                                    S([{netns, "ns1"}],
                                                      undefined),
                                                    PairsPlain)),
    ok.


%% validate_cs partitions sconfs into server groups by ip, port, netns and vrf
validate_cs_groups_netns_vrf(_Config) ->
    GC = yaws_config:make_default_gconf(false, "netns_vrf_SUITE"),
    Named = fun(Name, LOpts) ->
                    yaws:sconf_servername(sconf(LOpts), Name)
            end,
    Plain = Named("plain", []),
    Ns1   = Named("ns1", [{netns, "ns1"}]),
    Ns1b  = Named("ns1b", [{netns, "ns1"}]),
    Ns2   = Named("ns2", [{netns, "ns2"}]),
    Vrf1  = Named("vrf1", [{bind_to_device, <<"vrf1">>}]),
    Vrf1b = Named("vrf1b", [{bind_to_device, <<"vrf1">>}]),
    Both  = Named("both", [{netns, "ns1"}, {bind_to_device, <<"vrf1">>}]),
    Both2 = Named("both2", [{netns, "ns1"}, {bind_to_device, <<"vrf1">>}]),

    Names =
        fun(Groups) ->
                L = [lists:sort([hd(string:split(yaws:sconf_servername(S), ":"))
                                 || S <- G])
                     || G <- Groups],
                lists:sort(L)
            end,
    {ok, _, Groups} =
        yaws_config:validate_cs(GC, [Plain, Ns1, Vrf1, Ns2, Ns1b, Both, Both2,
                                     Vrf1b]),
    ?assertEqual([["both", "both2"], ["ns1", "ns1b"], ["ns2"], ["plain"],
                  ["vrf1", "vrf1b"]],
                 Names(Groups)),
    ok.
