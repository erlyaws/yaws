%%%----------------------------------------------------------------------
%%% File    : yaws_config.erl
%%% Author  : Claes Wikstrom <klacke@bluetail.com>
%%% Purpose :
%%% Created : 16 Jan 2002 by Claes Wikstrom <klacke@bluetail.com>
%%%----------------------------------------------------------------------

-module(yaws_config).
-author('klacke@bluetail.com').


-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").


-include_lib("kernel/include/file.hrl").

-export([load/1,
         make_default_gconf/2, make_default_sconf/0,
         add_sconf/1,
         add_yaws_auth/1,
         add_yaws_soap_srv/1, add_yaws_soap_srv/2,
         search_sconf/2, search_group/2,
         update_sconf/2, delete_sconf/2,
         eq_sconfs/2, soft_setconf/4, hard_setconf/2,
         can_hard_gc/2, can_soft_setconf/4,
         can_soft_gc/2, verify_upgrade_args/2, toks/2]).

%% where to look for yaws.conf
paths() ->
    case application:get_env(yaws, config) of
        undefined ->
            case yaws:getuid() of
                {ok, "0"} ->    %% root
                    [yaws_generated:etcdir() ++ "/yaws/yaws.conf"];
                _ -> %% developer
                    [filename:join([yaws:home(), "yaws.conf"]),
                     "./yaws.conf",
                     yaws_generated:etcdir() ++ "/yaws/yaws.conf"]
            end;
        {ok, File} ->
            [File]
    end.



%% load the config

load(E = #env{conf = false}) ->
    case yaws:first(fun(F) -> yaws:exists(F) end, paths()) of
        false ->
            {error, "Can't find config file "};
        {ok, _, File} ->
            load(E#env{conf = {file, File}})
    end;
load(E) ->
    {file, File} = E#env.conf,
    error_logger:info_msg("Yaws: Using config file ~s~n", [File]),
    case file:open(File, [read]) of
        {ok, FD} ->
            GC = make_default_gconf(E#env.debug, E#env.id),
            GC2 = if E#env.traceoutput == undefined ->
                          GC;
                     true ->
                          ?gc_set_tty_trace(GC, E#env.traceoutput)
                  end,
            GC3 =  ?gc_set_debug(GC2, E#env.debug),
            GC4 = GC3#gconf{trace = E#env.trace},
            R = (catch fload(FD, globals, GC4, undefined,
                             [], 1, io:get_line(FD, ''))),
            ?Debug("FLOAD: ~p", [R]),
            case R of
                {ok, GC5, Cs} ->
                    yaws:mkdir(yaws:tmpdir()),
                    Cs2 = add_yaws_auth(Cs),
                    add_yaws_soap_srv(GC5),
                    validate_cs(GC5, Cs2);
                Err ->
                    Err
            end;
        _ ->
            {error, "Can't open config file " ++ File}
    end.


add_yaws_soap_srv(GC) when GC#gconf.enable_soap == true ->
    add_yaws_soap_srv(GC, true);
add_yaws_soap_srv(_GC) ->
    [].
add_yaws_soap_srv(GC, false) when GC#gconf.enable_soap == true ->
    [{yaws_soap_srv, {yaws_soap_srv, start_link, [GC#gconf.soap_srv_mods]},
      permanent, 5000, worker, [yaws_soap_srv]}];
add_yaws_soap_srv(GC, true) when GC#gconf.enable_soap == true ->
    Spec = add_yaws_soap_srv(GC, false),
    case whereis(yaws_soap_srv) of
        undefined ->
            spawn(fun() -> supervisor:start_child(yaws_sup, hd(Spec)) end);
        _ ->
            ok
    end,
    Spec;
add_yaws_soap_srv(_GC, _Start) ->
    [].


add_yaws_auth(SCs) ->
    [SC#sconf{authdirs = setup_auth(SC)} || SC <- SCs].


%% We search and setup www authenticate for each directory
%% specified as an auth directory or containing a .yaws_auth file.
%% These are merged with server conf.
setup_auth(#sconf{docroot = Docroot, xtra_docroots = XtraDocroots,
                  authdirs = Authdirs}=SC) ->
    [begin
         Authdirs1 = load_yaws_auth_from_docroot(D, ?sc_auth_skip_docroot(SC)),
         Authdirs2 = load_yaws_auth_from_authdirs(Authdirs, D, []),
         Authdirs3 = [A || A <- Authdirs1,
                           not lists:keymember(A#auth.dir,#auth.dir,Authdirs2)],
         Authdirs4 = ensure_auth_headers(Authdirs3 ++ Authdirs2),
         start_pam(Authdirs4),
         {D, Authdirs4}
     end || D <- [Docroot|XtraDocroots] ].


load_yaws_auth_from_docroot(_, true) ->
    [];
load_yaws_auth_from_docroot(undefined, _) ->
    [];
load_yaws_auth_from_docroot(Docroot, _) ->
    Fun = fun (Path, Acc) ->
                  %% Strip Docroot and then filename
                  SP  = string:sub_string(Path, length(Docroot)+1),
                  Dir = filename:dirname(SP),
                  A = #auth{docroot=Docroot, dir=Dir},
                  case catch load_yaws_auth_file(Path, A) of
                      {ok, L} -> L ++ Acc;
                      _Other  -> Acc
                  end
          end,
    filelib:fold_files(Docroot, "^.yaws_auth$", true, Fun, []).


load_yaws_auth_from_authdirs([], _, Acc) ->
    lists:reverse(Acc);
load_yaws_auth_from_authdirs([Auth = #auth{dir=Dir}| Rest], Docroot, Acc) ->
    if
        Auth#auth.docroot /= [] andalso Auth#auth.docroot /= Docroot ->
            load_yaws_auth_from_authdirs(Rest, Docroot, Acc);
        Auth#auth.docroot == [] ->
            Auth1 = Auth#auth{dir=filename:nativename(Dir)},
            F = fun(A) ->
                        (A#auth.docroot == Docroot andalso
                         A#auth.dir == Auth1#auth.dir)
                end,
            case lists:any(F, Acc) of
                true ->
                    load_yaws_auth_from_authdirs(Rest, Docroot, Acc);
                false ->
                    Acc1 = Acc ++ load_yaws_auth_from_authdir(Docroot, Auth1),
                    load_yaws_auth_from_authdirs(Rest, Docroot, Acc1)
            end;
        true -> %% #auth.docroot == Docroot
            Auth1 = Auth#auth{docroot=Docroot, dir=filename:nativename(Dir)},
            F = fun(A) ->
                        not (A#auth.docroot == [] andalso
                             A#auth.dir == Auth1#auth.dir)
                end,
            Acc1 = lists:filter(F, Acc),
            Acc2 = Acc1 ++ load_yaws_auth_from_authdir(Docroot, Auth1),
            load_yaws_auth_from_authdirs(Rest, Docroot, Acc2)
    end;
load_yaws_auth_from_authdirs([{Docroot, Auths}|_], Docroot, Acc) ->
    load_yaws_auth_from_authdirs(Auths, Docroot, Acc);
load_yaws_auth_from_authdirs([_| Rest], Docroot, Acc) ->
    load_yaws_auth_from_authdirs(Rest, Docroot, Acc).


load_yaws_auth_from_authdir(Docroot, Auth) ->
    Dir = case Auth#auth.dir of
              "/" ++ R -> R;
              _        -> Auth#auth.dir
          end,
    Path = filename:join([Docroot, Dir, ".yaws_auth"]),
    case catch load_yaws_auth_file(Path, Auth) of
        {ok, Auths} -> Auths;
        _           -> [Auth]
    end.


load_yaws_auth_file(Path, Auth) ->
    case file:consult(Path) of
	{ok, TermList} ->
	    error_logger:info_msg("Reading .yaws_auth ~s~n", [Path]),
	    parse_yaws_auth_file(TermList, Auth);
	{error, enoent} ->
	    {error, enoent};
	Error ->
	    error_logger:format("Bad .yaws_auth file ~s ~p~n", [Path, Error]),
	    Error
    end.


ensure_auth_headers(Authdirs) ->
    [add_auth_headers(Auth) || Auth <- Authdirs].

add_auth_headers(Auth = #auth{headers = []}) ->
    %% Headers needs to be set
    Realm   = Auth#auth.realm,
    Headers = yaws:make_www_authenticate_header({realm, Realm}),
    Auth#auth{headers = Headers};
add_auth_headers(Auth) ->
    Auth.


start_pam([]) ->
    ok;
start_pam([#auth{pam = false}|T]) ->
    start_pam(T);
start_pam([A|T]) ->
    case whereis(yaws_pam) of
	undefined ->	% pam not started
            Spec = {yaws_pam, {yaws_pam, start_link,
                               [yaws:to_list(A#auth.pam),undefined,undefined]},
                    permanent, 5000, worker, [yaws_pam]},
	    spawn(fun() -> supervisor:start_child(yaws_sup, Spec) end);
	_ ->
	    start_pam(T)
    end.


parse_yaws_auth_file([], Auth=#auth{files=[]}) ->
    {ok, [Auth]};
parse_yaws_auth_file([], Auth=#auth{dir=Dir, files=Files}) ->
    {ok, [Auth#auth{dir=filename:join(Dir, F), files=[F]} || F <- Files]};

parse_yaws_auth_file([{realm, Realm}|T], Auth0) ->
    parse_yaws_auth_file(T, Auth0#auth{realm = Realm});

parse_yaws_auth_file([{pam, Pam}|T], Auth0)
  when is_atom(Pam) ->
    parse_yaws_auth_file(T, Auth0#auth{pam = Pam});

parse_yaws_auth_file([{authmod, Authmod0}|T], Auth0)
  when is_atom(Authmod0)->
    Headers = try
                  Authmod0:get_header() ++ Auth0#auth.headers
              catch
                  _:_ ->
                      error_logger:format("Failed to ~p:get_header() \n",
                                          [Authmod0]),
                      Auth0#auth.headers
              end,
    parse_yaws_auth_file(T, Auth0#auth{mod = Authmod0, headers = Headers});

parse_yaws_auth_file([{file, File}|T], Auth0) ->
    Files = case File of
                "/" ++ F -> [F|Auth0#auth.files];
                _        -> [File|Auth0#auth.files]
            end,
    parse_yaws_auth_file(T, Auth0#auth{files=Files});

parse_yaws_auth_file([{User, Password}|T], Auth0)
  when is_list(User), is_list(Password) ->
    Users = case lists:member({User,Password}, Auth0#auth.users) of
                true  -> Auth0#auth.users;
                false -> [{User, Password} | Auth0#auth.users]
            end,
    parse_yaws_auth_file(T, Auth0#auth{users = Users});

parse_yaws_auth_file([{allow, all}|T], Auth0) ->
    Auth1 = case Auth0#auth.acl of
                none    -> Auth0#auth{acl={all, [], deny_allow}};
                {_,D,O} -> Auth0#auth{acl={all, D, O}}
            end,
    parse_yaws_auth_file(T, Auth1);

parse_yaws_auth_file([{allow, IPs}|T], Auth0) when is_list(IPs) ->
    Auth1 = case Auth0#auth.acl of
                none ->
                    AllowIPs = parse_auth_ips(IPs, []),
                    Auth0#auth{acl={AllowIPs, [], deny_allow}};
                {all, _, _} ->
                    Auth0;
                {AllowIPs, DenyIPs, Order} ->
                    AllowIPs2 = parse_auth_ips(IPs, []) ++ AllowIPs,
                    Auth0#auth{acl={AllowIPs2, DenyIPs, Order}}
            end,
    parse_yaws_auth_file(T, Auth1);

parse_yaws_auth_file([{deny, all}|T], Auth0) ->
    Auth1 = case Auth0#auth.acl of
                none    -> Auth0#auth{acl={[], all, deny_allow}};
                {A,_,O} -> Auth0#auth{acl={A, all, O}}
            end,
    parse_yaws_auth_file(T, Auth1);

parse_yaws_auth_file([{deny, IPs}|T], Auth0) when is_list(IPs) ->
    Auth1 = case Auth0#auth.acl of
                none ->
                    DenyIPs = parse_auth_ips(IPs, []),
                    Auth0#auth{acl={[], DenyIPs, deny_allow}};
                {_, all, _} ->
                    Auth0;
                {AllowIPs, DenyIPs, Order} ->
                    DenyIPs2 = parse_auth_ips(IPs, []) ++ DenyIPs,
                    Auth0#auth{acl={AllowIPs, DenyIPs2, Order}}
            end,
    parse_yaws_auth_file(T, Auth1);

parse_yaws_auth_file([{order, O}|T], Auth0)
  when O == allow_deny; O == deny_allow ->
    Auth1 = case Auth0#auth.acl of
                none    -> Auth0#auth{acl={[], [], O}};
                {A,D,_} -> Auth0#auth{acl={A, D, O}}
            end,
    parse_yaws_auth_file(T, Auth1).



%% This is the function that arranges sconfs into
%% different server groups
validate_cs(GC, Cs) ->
    L = lists:map(fun(SC) -> {{SC#sconf.listen, SC#sconf.port}, SC} end, Cs),
    L2 = lists:map(fun(X) -> element(2, X) end, lists:keysort(1,L)),
    L3 = arrange(L2, start, [], []),
    case validate_groups(L3) of
        ok ->
            {ok, GC, L3};
        Err ->
            Err
    end.


validate_groups([]) ->
    ok;
validate_groups([H|T]) ->
    case (catch validate_group(H)) of
        ok ->
            validate_groups(T);
        Err ->
            Err
    end.

validate_group(List) ->
    %% all ssl servers with the same IP must share the same ssl  configuration
    %% check if one ssl server in the group
    case lists:any(fun(SC) -> SC#sconf.ssl  /= undefined end,List) of
	true ->
            [SC0|SCs] = List,
            %% check if all servers in the group have the same ssl configuration
            case lists:filter(
                   fun(SC) -> SC#sconf.ssl /= SC0#sconf.ssl end,SCs) of
                L when length(L) > 1 ->
                    throw({error, ?F("SSL server per IP must share the "
                                     "same certificate : ~p",
                                     [(hd(L))#sconf.servername])});
                _ ->
                    ok
            end;
	_ ->
            ok
    end,
    %% second all servernames in a group must be unique
    SN = lists:sort([yaws:to_lower(X#sconf.servername) || X <- List]),
    no_two_same(SN).

no_two_same([H,H|_]) ->
    throw({error,
           ?F("Two servers in the same group cannot have same name ~p",[H])});
no_two_same([_H|T]) ->
    no_two_same(T);
no_two_same([]) ->
    ok.



arrange([C|Tail], start, [], B) ->
    arrange(Tail, {in, C}, [set_server(C)], B);
arrange([], _, [], B) ->
    B;
arrange([], _, A, B) ->
    [A | B];
arrange([C1|Tail], {in, C0}, A, B) ->
    if
        C1#sconf.listen == C0#sconf.listen,
        C1#sconf.port == C0#sconf.port ->
            arrange(Tail, {in, C0}, [set_server(C1)|A], B);
        true ->
            arrange(Tail, {in, C1}, [set_server(C1)], [A|B])
    end.


set_server(SC) ->
    case {SC#sconf.ssl, SC#sconf.port, ?sc_has_add_port(SC)} of
        {undefined, 80, _} ->
            SC;
        {undefined, Port, true} ->
            add_port(SC, Port);
        {_SSL, 443, _} ->
            SC;
        {_SSL, Port, true} ->
            add_port(SC, Port);
        {_,_,_} ->
            SC
    end.


add_port(SC, Port) ->
    case string:tokens(SC#sconf.servername, ":") of
        [Srv, Prt] ->
            case (catch list_to_integer(Prt)) of
                {'EXIT', _} ->
                    SC#sconf{servername =
                                 Srv ++ [$:|integer_to_list(Port)]};
                _Int ->
                    SC
            end;
        [Srv] ->
            SC#sconf{servername =   Srv ++ [$:|integer_to_list(Port)]}
    end.


make_default_gconf(Debug, Id) ->
    Y = yaws_dir(),
    #gconf{yaws_dir = Y,
           ebin_dir = [filename:join([Y, "examples/ebin"])],
           include_dir = [filename:join([Y, "examples/include"])],
           trace = false,
           logdir = ".",
           cache_refresh_secs = if
                                    Debug == true ->
                                        0;
                                    true ->
                                        30
                                end,
           flags = if Debug ->
                           ?GC_DEBUG bor ?GC_COPY_ERRLOG
                               bor ?GC_FAIL_ON_BIND_ERR bor
                               ?GC_PICK_FIRST_VIRTHOST_ON_NOMATCH;

                      true ->
                           ?GC_COPY_ERRLOG bor ?GC_FAIL_ON_BIND_ERR bor
                               ?GC_PICK_FIRST_VIRTHOST_ON_NOMATCH
                   end,

           yaws = "Yaws " ++ yaws_generated:version(),
           id = Id
          }.

make_default_sconf() ->
    Y = yaws_dir(),
    #sconf{docroot = filename:join([Y, "www"])}.

yaws_dir() ->
    %% below, ignore dialyzer warning:
    %% "The pattern 'false' can never match the type 'true'"
    case  yaws_generated:is_local_install() of
        true ->
            P = filename:split(code:which(?MODULE)),
            P1 = del_tail(P),
            filename:join(P1);
        false ->
            code:lib_dir(yaws)
    end.

del_tail(Parts) ->
    del_tail(Parts,[]).
%% Initial ".." should be preserved
del_tail([".." |Tail], Acc) ->
    del_tail(Tail, [".."|Acc]);
del_tail(Parts, Acc) ->
    del_tail2(Parts, Acc).

%% Embedded ".." should be removed together with preceding dir
del_tail2([_H, ".." |Tail], Acc) ->
    del_tail2(Tail, Acc);
del_tail2([".." |Tail], [_P|Acc]) ->
    del_tail2(Tail, Acc);
del_tail2([_X, _Y], Acc) ->
    lists:reverse(Acc);
del_tail2([H|T], Acc) ->
    del_tail2(T, [H|Acc]).


string_to_host_and_port(String) ->
    case string:tokens(String, ":") of
	[Host, Port] ->
	    case string:to_integer(Port) of
		{Integer, []} when Integer >= 0, Integer =< 65535 ->
		    {ok, Host, Integer};
		_Else ->
                    {error, ?F("~p is not a valid port number", [Port])}
	    end;
	_Else ->
            {error, ?F("bad host and port specifier, expected HOST:PORT", [])}
    end.

string_to_node_mod_fun(String) ->
    case string:tokens(String, ":") of
        [Node, Mod, Fun] ->
            {ok, list_to_atom(Node), list_to_atom(Mod), list_to_atom(Fun)};
        [Mod, Fun] ->
            {ok, list_to_atom(Mod), list_to_atom(Fun)};
        _ ->
            {error, ?F("bad external module specifier, "
                       "expected NODE:MODULE:FUNCTION or MODULE:FUNCTION", [])}
    end.



%% two states, global, server
fload(FD, globals, GC, _C, Cs, _Lno, eof) ->
    file:close(FD),
    {ok, GC, Cs};


fload(FD, _,  _GC, _C, _Cs, Lno, eof) ->
    file:close(FD),
    {error, ?F("Unexpected end-of-file at line ~w", [Lno])};

fload(FD, globals, GC, C, Cs, Lno, Chars) ->
    Next = io:get_line(FD, ''),
    case toks(Lno, Chars) of
        [] ->
            fload(FD, globals, GC, C, Cs, Lno+1, Next);

        ["subconfig", '=', Name] ->
            File = filename:absname(Name),
            case is_file(File) of
                true ->
                    error_logger:info_msg(
                      "Yaws: Using subconfig file ~s~n", [File]),
                    case file:open(File, [read]) of
                        {ok, FD1} ->
                            R = (catch fload(FD1, globals, GC, undefined,
                                             Cs, 1, io:get_line(FD1, ''))),
                            ?Debug("FLOAD: ~p", [R]),
                            case R of
                                {ok, GC1, Cs1} ->
                                    fload(FD, globals, GC1, C, Cs1, Lno+1,Next);
                                Err ->
                                    Err
                            end;
                        _ ->
                            {error, "Can't open config file " ++ File}
                    end;
                false ->
                    {error, ?F("Expect filename at line ~w", [Lno])}
            end;

        ["subconfigdir", '=', Name] ->
            Dir = filename:absname(Name),
            case is_dir(Dir) of
                true ->
                    case file:list_dir(Dir) of
                        {ok, Names} ->
                            Paths = lists:map(
                                      fun(N) ->
                                              filename:absname(N, Dir)
                                      end, Names),
                            Fold = lists:foldl(
                                     fun subconfigdir_fold/2,
                                     {ok, GC, Cs}, Paths),
                            case Fold of
                                {ok, GC1, Cs1} ->
                                    fload(FD, globals, GC1, C, Cs1, Lno+1, Next);
                                Err ->
                                    Err
                            end;
                        {error, Error} ->
                            {error, ?F("Directory ~s is not readable: ~s",
                                       [Name, Error])}
                    end;
                false ->
                    {error, ?F("Expect directory at line ~w (subconfdir: ~s)",
                               [Lno, Dir])}
            end;

        ["trace", '=', Bstr] when GC#gconf.trace == false ->
            case Bstr of
                "traffic" ->
                    fload(FD, globals, GC#gconf{trace = {true, traffic}},
                          C, Cs, Lno+1, Next);
                "http" ->
                    fload(FD, globals, GC#gconf{trace = {true, http}},
                          C, Cs, Lno+1, Next);
                "false" ->
                    fload(FD, globals, GC#gconf{trace = false},
                          C, Cs, Lno+1, Next);
                _ ->
                    {error, ?F("Expect false|http|traffic at line ~w",[Lno])}
            end;
        ["trace", '=', _Bstr] ->
            %% don't overwrite setting from commandline
            fload(FD, globals, GC, C, Cs, Lno+1, Next);


        ["logdir", '=', Logdir] ->
            Dir = filename:absname(Logdir),
            case is_dir(Dir) of
                true ->
                    put(logdir, Dir),
                    fload(FD, globals, GC#gconf{logdir = Dir},
                          C, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect directory at line ~w (logdir ~s)", [Lno, Dir])}
            end;

        ["ebin_dir", '=', Ebindir] ->
            Dir = filename:absname(Ebindir),
            case warn_dir("ebin_dir", Dir) of
                true ->
                    fload(FD, globals, GC#gconf{ebin_dir =
                                                    [Dir|GC#gconf.ebin_dir]},
                          C, Cs, Lno+1, Next);
                false ->
                    fload(FD, globals, GC, C, Cs, Lno+1, Next)

            end;

        ["runmod", '=', Mod0] ->
            Mod = list_to_atom(Mod0),
            fload(FD, globals, GC#gconf{runmods = [Mod|GC#gconf.runmods]},
                  C, Cs, Lno+1, Next);

        ["enable_soap", '=', Bool] ->
            if (Bool == "true") ->
                    fload(FD, globals, GC#gconf{enable_soap = true},
                          C, Cs, Lno+1, Next);
               true ->
                    fload(FD, globals, GC#gconf{enable_soap = false},
                          C, Cs, Lno+1, Next)
            end;

        ["soap_srv_mods", '=' | SoapSrvMods] ->
            case parse_soap_srv_mods(SoapSrvMods, []) of
                {ok, L} ->
                    fload(FD, globals, GC#gconf{soap_srv_mods = L},
                          C, Cs, Lno+1, Next);
                {error, Str} ->
                    {error, ?F("~s at line ~w", [Str, Lno])}
            end;

        ["max_connections", '=', Int] ->
            case (catch list_to_integer(Int)) of
                I when is_integer(I) ->
                    fload(FD, globals, GC#gconf{max_connections = I},
                          C, Cs, Lno+1, Next);
                _ when Int == "nolimit" ->
                    fload(FD, globals, GC, C, Cs, Lno+1, Next);
                _ ->
                    {error, ?F("Expect integer at line ~w", [Lno])}
            end;

        ["process_options", '=', POpts] ->
            case parse_process_options(POpts) of
                {ok, ProcList} ->
                    fload(FD, globals, GC#gconf{process_options=ProcList},
                          C, Cs, Lno+1, Next);
                {error, Str} ->
                    {error, ?F("~s at line ~w", [Str, Lno])}
            end;

        ["acceptor_pool_size", '=', Int] ->
            case catch list_to_integer(Int) of
                I when is_integer(I), I >= 0 ->
                    fload(FD, globals, GC#gconf{acceptor_pool_size = I},
                          C, Cs, Lno+1, Next);
                _ ->
                    {error, ?F("Expect integer >= 0 at line ~w", [Lno])}
            end;

        ["log_wrap_size", '=', Int] ->
            case (catch list_to_integer(Int)) of
                I when is_integer(I) ->
                    fload(FD, globals, GC#gconf{log_wrap_size = I},
                          C, Cs, Lno+1, Next);
                _ ->
                    {error, ?F("Expect integer at line ~w", [Lno])}
            end;

        ["log_resolve_hostname", '=',  Bool] ->
            case is_bool(Bool) of
                {true, Val} ->
                    fload(FD, globals, ?gc_log_set_resolve_hostname(GC, Val),
                          C, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect true|false at line ~w", [Lno])}
            end;

        ["fail_on_bind_err", '=',  Bool] ->
            case is_bool(Bool) of
                {true, Val} ->
                    fload(FD, globals, ?gc_set_fail_on_bind_err(GC, Val),
                          C, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect true|false at line ~w", [Lno])}
            end;


        ["include_dir", '=', Incdir] ->
            Dir = filename:absname(Incdir),
            case warn_dir("include_dir", Dir) of
                true ->
                    fload(FD, globals, GC#gconf{include_dir=
                                                    [Dir|GC#gconf.include_dir]},
                          C, Cs, Lno+1, Next);
                false ->
                    fload(FD, globals, GC, C, Cs, Lno+1, Next)

            end;

        ["mnesia_dir", '=', Mnesiadir] ->
            Dir = filename:absname(Mnesiadir),
            case is_dir(Dir) of
                true ->
                    put(mnesiadir, Dir),
                    fload(FD, globals, GC#gconf{mnesia_dir = Dir},
                          C, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect mnesia directory at line ~w", [Lno])}
            end;

        ["tmpdir", '=', _TmpDir] ->
            %% ignore
            error_logger:format(
              "tmpdir in yaws.conf is no longer supported - ignoring\n",[]),
            fload(FD, globals, GC,C, Cs, Lno+1, Next);

        %% keep this bugger for backward compat for a while
        ["keepalive_timeout", '=', Val] ->
            case (catch list_to_integer(Val)) of
                I when is_integer(I) ->
                    fload(FD, globals, GC#gconf{keepalive_timeout = I},
                          C, Cs, Lno+1, Next);
                _ when Val == "infinity" ->
                    fload(FD, globals, GC#gconf{keepalive_timeout = infinity},
                          C, Cs, Lno+1, Next);
                _ ->
                    {error, ?F("Expect integer at line ~w", [Lno])}
            end;

        ["keepalive_maxuses", '=', Int] ->
            case (catch list_to_integer(Int)) of
                I when is_integer(I) ->
                    fload(FD, globals, GC#gconf{keepalive_maxuses = I},
                          C, Cs, Lno+1, Next);
                _ when Int == "nolimit" ->
                    %% nolimit is the default
                    fload(FD, globals, GC, C, Cs, Lno+1, Next);
                _ ->
                    {error, ?F("Expect integer at line ~w", [Lno])}
            end;

        ["php_exe_path", '=' , PhpPath] ->
            error_logger:format(
              "'php_exe_path' is deprecated, use 'php_handler' instead\n",
              []),
            case is_file(PhpPath) of
                true ->
                    fload(FD, globals, GC#gconf{phpexe = PhpPath},
                          C, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect executable file at line ~w", [Lno])}
            end;

        %% deprected, don't use
        ["read_timeout", '=', _Val] ->
            fload(FD, globals, GC, C, Cs, Lno+1, Next);

        ["max_num_cached_files", '=', Val] ->
            case (catch list_to_integer(Val)) of
                I when is_integer(I) ->
                    fload(FD, globals, GC#gconf{max_num_cached_files = I},
                          C, Cs, Lno+1, Next);
                _ ->
                    {error, ?F("Expect integer at line ~w", [Lno])}
            end;


        ["max_num_cached_bytes", '=', Val] ->
            case (catch list_to_integer(Val)) of
                I when is_integer(I) ->
                    fload(FD, globals, GC#gconf{max_num_cached_bytes = I},
                          C, Cs, Lno+1, Next);
                _ ->
                    {error, ?F("Expect integer at line ~w", [Lno])}
            end;


        ["max_size_cached_file", '=', Val] ->
            case (catch list_to_integer(Val)) of
                I when is_integer(I) ->
                    fload(FD, globals, GC#gconf{max_size_cached_file = I},
                          C, Cs, Lno+1, Next);
                _ ->
                    {error, ?F("Expect integer at line ~w", [Lno])}
            end;

        ["cache_refresh_secs", '=', Val] ->
            case (catch list_to_integer(Val)) of
                I when is_integer(I), I >= 0 ->
                    fload(FD, globals, GC#gconf{cache_refresh_secs = I},
                          C, Cs, Lno+1, Next);
                _ ->
                    {error, ?F("Expect 0 or positive integer at line ~w",[Lno])}
            end;


        ["copy_error_log", '=', Bool] ->
            case is_bool(Bool) of
                {true, Val} ->
                    fload(FD, globals, ?gc_set_copy_errlog(GC, Val),
                          C, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect true|false at line ~w", [Lno])}
            end;


        ["auth_log", '=', Bool] ->
            error_logger:format(
              "'auth_log' global variable is deprecated and ignored."
              " it is now a per-server variable", []),
            case is_bool(Bool) of
                {true, _Val} ->
                    fload(FD, globals, GC, C, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect true|false at line ~w", [Lno])}
            end;
        ["id", '=', String] when GC#gconf.id == undefined;
                                 GC#gconf.id == "default" ->
            fload(FD, globals, GC#gconf{id=String},C, Cs, Lno+1, Next);
        ["id", '=', String]  ->
            error_logger:format("Ignoring 'id = ~p' setting at line ~p~n",
                                [String,Lno]),
            fload(FD, globals, GC, C, Cs, Lno+1, Next);
        ["pick_first_virthost_on_nomatch", '=',  Bool] ->
            case is_bool(Bool) of
                {true, Val} ->
                    fload(FD, globals,
                          ?gc_set_pick_first_virthost_on_nomatch(GC,Val),
                          C, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect true|false at line ~w", [Lno])}
            end;
        ["use_fdsrv", '=',  _Bool] ->
            %% feature removed
            fload(FD, globals, GC, C, Cs, Lno+1, Next);
        ["use_old_ssl", '=',  Bool] ->
            case is_bool(Bool) of
                {true, Val} ->
                    fload(FD, globals,
                          ?gc_set_use_old_ssl(GC,Val),
                          C, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect true|false at line ~w", [Lno])}
            end;
        ["use_large_ssl_pool", '=',  _Bool] ->
            %% just ignore - not relevant any longer
            fload(FD, globals, GC,
                  C, Cs, Lno+1, Next);
        ["x_forwarded_for_log_proxy_whitelist", '=' | _] ->
            error_logger:info_msg("Warning, x_forwarded_for_log_proxy_whitelist"
                                  " is deprecated and ignored~n", []),
            fload(FD, globals, GC, C, Cs, Lno+1, Next);
        ["ysession_mod", '=', Mod_str] ->
            Ysession_mod = list_to_atom(Mod_str),
            fload(FD, globals, GC#gconf{ysession_mod = Ysession_mod},
                  C, Cs, Lno+1, Next);
        ["server_signature", '=', Signature] ->
            fload(FD, globals, GC#gconf{yaws=Signature},C, Cs, Lno+1, Next);
        ['<', "server", Server, '>'] ->  %% first server
            PhpHandler = {cgi, GC#gconf.phpexe},
            fload(FD, server, GC,
                  #sconf{servername = Server, php_handler = PhpHandler,
                         listen = []},
                  Cs, Lno+1, Next);

        [H|_] ->
            {error, ?F("Unexpected tokens ~p at line ~w", [H, Lno])};
        Err ->
            Err
    end;


fload(FD, server, GC, C, Cs, Lno, Chars) ->
    Next = io:get_line(FD, ''),
    case toks(Lno, Chars) of
        [] ->
            fload(FD, server, GC, C, Cs, Lno+1, Next);

        ["server_signature", '=', Signature] ->
            fload(FD, server, GC, C#sconf{yaws=Signature}, Cs, Lno+1, Next);
        ["access_log", '=', Bool] ->
            case is_bool(Bool) of
                {true, Val} ->
                    C2 = ?sc_set_access_log(C, Val),
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect true|false at line ~w", [Lno])}
            end;
        ["auth_log", '=', Bool] ->
            case is_bool(Bool) of
                {true, Val} ->
                    C2 = ?sc_set_auth_log(C, Val),
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect true|false at line ~w", [Lno])}
            end;
        ["logger_mod", '=', Module] ->
            C2 = C#sconf{logger_mod = list_to_atom(Module)},
            fload(FD, server, GC, C2, Cs, Lno+1, Next);
        ["dir_listings", '=', StrVal] ->
            case StrVal of
                "true" ->
                    C2 = ?sc_set_dir_listings(C, true),
                    C3 = ?sc_set_dir_all_zip(C2, true),
                    C4 = C3#sconf{appmods = [ {"all.zip", yaws_ls},
                                              {"all.tgz", yaws_ls},
                                              {"all.tbz2", yaws_ls}|
                                              C3#sconf.appmods]},
                    fload(FD, server, GC, C4, Cs, Lno+1, Next);
                "true_nozip" ->
                    C2 = ?sc_set_dir_listings(C, true),
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                "false" ->
                    C2 = ?sc_set_dir_listings(C, false),
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                _ ->
                    {error, ?F("Expect true|true_nozip|false at line ~w",[Lno])}
            end;
        ["deflate", '=', Bool] ->
            case is_bool(Bool) of
                {true, Val} ->
                    C1 = C#sconf{deflate_options=#deflate{}},
                    C2 = ?sc_set_deflate(C1, Val),
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect true|false at line ~w", [Lno])}
            end;
        ["auth_skip_docroot",'=',Bool] ->
            case is_bool(Bool) of
                {true,Val} ->
                    C2 = ?sc_set_auth_skip_docroot(C, Val),
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect true|false at line ~w", [Lno])}
            end;
        ["dav", '=', Bool] ->
            case is_bool(Bool) of
                {true, Val} ->
                    C2 = ?sc_set_dav(C, Val),
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect true|false at line ~w", [Lno])}
            end;
        ["port", '=', Val] ->
            case (catch list_to_integer(Val)) of
                I when is_integer(I) ->
                    C2 = C#sconf{port = I},
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                _ ->
                    {error, ?F("Expect integer at line ~w", [Lno])}
            end;
        ["rmethod", '=', Val] ->
            case Val of
                "http" ->
                    C2 = C#sconf{rmethod = Val},
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                "https" ->
                    C2 = C#sconf{rmethod = Val},
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                _ ->
                    {error, ?F("Expect http or https at line ~w", [Lno])}
            end;
        ["rhost", '=', Val] ->
            C2 = C#sconf{rhost = Val},
            fload(FD, server, GC, C2, Cs, Lno+1, Next);
        ["listen", '=', IP] ->
            case inet_parse:address(IP) of
                {error, _} ->
                    {error, ?F("Expect IP address at line ~w:", [Lno])};
                {ok,Addr} ->
                    Lstn = C#sconf.listen,
                    C2 = if
                             is_list(Lstn) ->
                                 case lists:member(Addr, Lstn) of
                                     false ->
                                         C#sconf{listen = [Addr|Lstn]};
                                     true ->
                                         C
                                 end;
                             true ->
                                 C#sconf{listen = [Addr, Lstn]}
                         end,
                    fload(FD, server, GC, C2, Cs, Lno+1, Next)
            end;
        ["listen_backlog", '=', Val] ->
            case (catch list_to_integer(Val)) of
                B when is_integer(B) ->
                    C2 = case proplists:get_value(listen_opts,
                                                  C#sconf.soptions) of
                             undefined ->
                                 C#sconf{soptions =
                                             [{listen_opts, [{backlog, B}]} |
                                              C#sconf.soptions]};
                             Opts ->
                                 C#sconf{soptions =
                                             [{listen_opts, [{backlog, B} | Opts]} |
                                              C#sconf.soptions]}
                         end,
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                _ ->
                    {error, ?F("Expect integer at line ~w", [Lno])}
            end;
        ["servername", '=', Name] ->
            C2 = ?sc_set_add_port((C#sconf{servername = Name}),false),
            fload(FD, server, GC, C2, Cs, Lno+1, Next);

        ["docroot", '=', Rootdir | XtraDirs] ->
            RootDirs = lists:map(fun(R) -> filename:absname(R) end,
                                 [Rootdir | XtraDirs]),
            case lists:filter(fun(R) -> not is_dir(R) end, RootDirs) of
                [] when C#sconf.docroot =:= undefined ->
                    C2 = C#sconf{docroot = hd(RootDirs),
                                 xtra_docroots = tl(RootDirs)},
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                [] ->
                    XtraDocroots = RootDirs ++ C#sconf.xtra_docroots,
                    C2 = C#sconf{xtra_docroots = XtraDocroots},
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                NoDirs ->
                    error_logger:info_msg("Warning, Skip invalid docroots"
                                          " at line ~w : ~s~n",
                                          [Lno, string:join(NoDirs, ", ")]),
                    case lists:subtract(RootDirs, NoDirs) of
                        [] ->
                            fload(FD, server, GC, C, Cs, Lno+1, Next);
                        [H|T] when C#sconf.docroot =:= undefined ->
                            C2 = C#sconf{docroot = H, xtra_docroots = T},
                            fload(FD, server, GC, C2, Cs, Lno+1, Next);
                        Ds ->
                            XtraDocroots = Ds ++ C#sconf.xtra_docroots,
                            C2 = C#sconf{xtra_docroots = XtraDocroots},
                            fload(FD, server, GC, C2, Cs, Lno+1, Next)
                    end
            end;

        ["partial_post_size",'=',Size] ->
            case Size of
                "nolimit" ->
                    C2 = C#sconf{partial_post_size = nolimit},
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                Val ->
                    case (catch list_to_integer(Val)) of
                        I when is_integer(I) ->
                            C2 = C#sconf{partial_post_size = I},
                            fload(FD, server, GC, C2, Cs, Lno+1, Next);
                        _ ->
                            {error,
                             ?F("Expect integer or 'nolimit' at line ~w",
                                [Lno])}
                    end
            end;
        ['<', "auth", '>'] ->
            fload(FD, server_auth, GC, C, Cs, Lno+1, Next, #auth{});

        ['<', "redirect", '>'] ->
            fload(FD, server_redirect, GC, C, Cs, Lno+1, Next, []);

        ['<', "deflate", '>'] ->
            fload(FD, server_deflate, GC, C, Cs, Lno+1, Next,
                  #deflate{mime_types=[]});

        %% noop
        ["default_server_on_this_ip", '=', _Bool] ->
            fload(FD, server, GC, C, Cs, Lno+1, Next);

        [ '<', "ssl", '>'] ->
            ssl_start(),
            fload(FD, ssl, GC, C#sconf{ssl = #ssl{}}, Cs, Lno+1, Next);

        ["appmods", '=' | AppMods] ->
            case parse_appmods(AppMods, []) of
                {ok, L} ->
                    C2 = C#sconf{appmods = L ++ C#sconf.appmods},
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                {error, Str} ->
                    {error, ?F("~s at line ~w", [Str, Lno])}
            end;
        ["expires", '=' | Expires] ->
            case parse_expires(Expires, []) of
                {ok, L} ->
                    C2 = C#sconf{expires = L ++ C#sconf.expires},
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                {error, Str} ->
                    {error, ?F("~s at line ~w", [Str, Lno])}
            end;
        ["errormod_404", '=' , Module] ->
            C2 = C#sconf{errormod_404 = list_to_atom(Module)},
            fload(FD, server, GC, C2, Cs, Lno+1, Next);

        ["errormod_crash", '=', Module] ->
            C2 = C#sconf{errormod_crash = list_to_atom(Module)},
            fload(FD, server, GC, C2, Cs, Lno+1, Next);

        ["errormod_401", '=' , Module] ->
            C2 = C#sconf{errormod_401 = list_to_atom(Module)},
            fload(FD, server, GC, C2, Cs, Lno+1, Next);

        ["arg_rewrite_mod", '=', Module] ->
            C2 = C#sconf{arg_rewrite_mod = list_to_atom(Module)},
            fload(FD, server, GC, C2, Cs, Lno+1, Next);

        ["tilde_expand", '=', Bool] ->
            case is_bool(Bool) of
                {true, Val} ->
                    C2 = ?sc_set_tilde_expand(C,Val),
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect true|false at line ~w", [Lno])}
            end;

        ['<', "/server", '>'] when C#sconf.docroot =:= undefined ->
            {error,
             ?F("No valid docroot configured for virthost '~s' (port: ~w)",
                [C#sconf.servername, C#sconf.port])};
        ['<', "/server", '>'] ->
            case C#sconf.listen of
                [] ->
                    C2 = C#sconf{listen = {127,0,0,1}},
                    fload(FD, globals, GC, undefined, [C2|Cs], Lno+1, Next);
                Ls ->
                    Cs2 = [C#sconf{listen=L} || L <- Ls] ++ Cs,
                    fload(FD, globals, GC, undefined, Cs2, Lno+1, Next)
            end;

        ['<', "opaque", '>'] ->
            fload(FD, opaque, GC, C, Cs, Lno+1, Next);

        ["start_mod", '=' , Module] ->
            C2 = C#sconf{start_mod = list_to_atom(Module)},
            fload(FD, server, GC, C2, Cs, Lno+1, Next);

        ['<', "rss", '>'] ->
            erase(rss_id),
            put(rss, []),
            fload(FD, rss, GC, C, Cs, Lno+1, Next);

        ["tilde_allowed_scripts", '=' | Suffixes] ->
            C2 = C#sconf{tilde_allowed_scripts =
                             lists:map(fun(X)->element(1,mime_types:t(X)) end,
                                       Suffixes)},
            fload(FD, server, GC, C2, Cs, Lno+1, Next);

        ["allowed_scripts", '=' | Suffixes] ->
            C2 = C#sconf{allowed_scripts =
                             lists:map(fun(X)->element(1,mime_types:t(X)) end,
                                       Suffixes)},
            fload(FD, server, GC, C2, Cs, Lno+1, Next);

        ["revproxy", '=' | Tail] ->
            case parse_revproxy(Tail) of
                {ok, RevProxy} ->
                    C2 = C#sconf{revproxy = [RevProxy | C#sconf.revproxy]},
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                {error, url} ->
                    {error, ?F("Bad url at line ~p",[Lno])};
                {error, syntax} ->
                    {error, ?F("Bad revproxy syntax at line ~p",[Lno])};
                Error ->
                    Error
            end;

        ["fwdproxy", '=', Bool] ->
            case is_bool(Bool) of
                {true, Val} ->
                    C2 = ?sc_set_forward_proxy(C, Val),
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect true|false at line ~w", [Lno])}
            end;

        ['<', "extra_cgi_vars", "dir", '=', Dir, '>'] ->
            fload(FD, extra_cgi_vars, GC, C, Cs, Lno+1, Next, {Dir, []});

        ["statistics", '=', Bool] ->
            case is_bool(Bool) of
                {true, Val} ->
                    C2 = ?sc_set_statistics(C, Val),
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect true|false at line ~w", [Lno])}
            end;

        ["fcgi_app_server", '=', Val] ->
            case string_to_host_and_port(Val) of
                {ok, Host, Port} ->
                    C2 = C#sconf{fcgi_app_server = {Host, Port}},
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                {error, Reason} ->
                    {error, ?F("Invalid fcgi_app_server ~p at line ~w: ~s",
                               [Val, Lno, Reason])}
            end;

        ["fcgi_trace_protocol", '=', Bool] ->
            case is_bool(Bool) of
                {true, Val} ->
                    C2 = ?sc_set_fcgi_trace_protocol(C, Val),
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect true|false at line ~w", [Lno])}
            end;

        ["fcgi_log_app_error", '=', Bool] ->
            case is_bool(Bool) of
                {true, Val} ->
                    C2 = ?sc_set_fcgi_log_app_error(C, Val),
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                false ->
                    {error, ?F("Expect true|false at line ~w", [Lno])}
            end;

	["phpfcgi", '=', HostPortSpec] ->
            error_logger:format(
              "'phpfcgi' is deprecated, use 'php_handler' instead\n", []),
	    case string_to_host_and_port(HostPortSpec) of
		{ok, Host, Port} ->
                C2 = C#sconf{php_handler = {fcgi, {Host, Port}}},
		    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                {error, Reason} ->
                    {error,
                     ?F("Invalid php fcgi server ~p at line ~w: ~s",
                        [HostPortSpec, Lno, Reason])}
            end;

        ["php_handler", '=' | PhpMod] ->
            case parse_phpmod(PhpMod, GC#gconf.phpexe) of
                {ok, I} ->
                    C2 = C#sconf{php_handler = I},
                    fload(FD, server, GC, C2, Cs, Lno+1, Next);
                {error, Reason} ->
                    {error,
                     ?F("Invalide php_handler configuration at line ~w: ~s",
                        [Lno, Reason])}
            end;

        ["shaper", '=', Module] ->
            C2 = C#sconf{shaper = list_to_atom(Module)},
            fload(FD, server, GC, C2, Cs, Lno+1, Next);

        [H|T] ->
            {error, ?F("Unexpected input ~p at line ~w", [[H|T], Lno])};
        Err ->
            Err
    end;



fload(FD, ssl, GC, C, Cs, Lno, Chars) ->
    Next = io:get_line(FD, ''),
    case toks(Lno, Chars) of
        [] ->
            fload(FD, ssl, GC, C, Cs, Lno+1, Next);

        %% A bunch of ssl options

        ["keyfile", '=', Val] ->
            case is_file(Val) of
                true when  is_record(C#sconf.ssl, ssl) ->
                    C2 = C#sconf{ssl = (C#sconf.ssl)#ssl{keyfile = Val}},
                    fload(FD, ssl, GC, C2, Cs, Lno+1, Next);
                true ->
                    {error, ?F("Need to set option ssl to true before line ~w",
                               [Lno])};
                _ ->
                    {error, ?F("Expect existing file at line ~w", [Lno])}
            end;
        ["certfile", '=', Val] ->
            case is_file(Val) of
                true when  is_record(C#sconf.ssl, ssl) ->
                    C2 = C#sconf{ssl = (C#sconf.ssl)#ssl{certfile = Val}},
                    fload(FD, ssl, GC, C2, Cs, Lno+1, Next);
                true ->
                    {error, ?F("Need to set option ssl to true before line ~w",
                               [Lno])};
                _ ->
                    {error, ?F("Expect existing file at line ~w", [Lno])}
            end;
        ["cacertfile", '=', Val] ->
            case is_file(Val) of
                true when  is_record(C#sconf.ssl, ssl) ->
                    C2 = C#sconf{ssl = (C#sconf.ssl)#ssl{cacertfile = Val}},
                    fload(FD, ssl, GC, C2, Cs, Lno+1, Next);
                true ->
                    {error, ?F("Need to set option ssl to true before line ~w",
                               [Lno])};
                _ ->
                    {error, ?F("Expect existing file at line ~w", [Lno])}
            end;
        ["verify", '=', Val0] ->
            Val =
                try
                    list_to_integer(Val0)
                catch error:badarg ->
                        list_to_atom(Val0)
                end,
            case lists:member(Val, [0,1,2,verify_peer,verify_none]) of
                true when  is_record(C#sconf.ssl, ssl) ->
                    C2 = C#sconf{ssl = (C#sconf.ssl)#ssl{verify = Val}},
                    fload(FD, ssl, GC, C2, Cs, Lno+1, Next);
                true ->
                    {error, ?F("Need to set option ssl to true before line ~w",
                               [Lno])};
                _ ->
                    {error, ?F("Expect integer or verify_none, "
                               "verify_peer at line ~w", [Lno])}
            end;
        ["fail_if_no_peer_cert", '=', Val0] ->
            Val = (catch list_to_atom(Val0)),
            if
                is_record(C#sconf.ssl, ssl) ->
                    C2 = C#sconf{ssl = (C#sconf.ssl)#ssl{
                                         fail_if_no_peer_cert = Val}},
                    fload(FD, ssl, GC, C2, Cs, Lno+1, Next);
                true ->
                    {error, ?F("Need to set option fail_if_no_peer_cert "
                               "to true before line ~w",
                               [Lno])}
            end;
        ["depth", '=', Val0] ->
            Val = (catch list_to_integer(Val0)),
            case lists:member(Val, [0, 1,2,3,4,5,6,7]) of
                true when  is_record(C#sconf.ssl, ssl) ->
                    C2 = C#sconf{ssl = (C#sconf.ssl)#ssl{depth = Val}},
                    fload(FD, ssl, GC, C2, Cs, Lno+1, Next);
                true ->
                    {error, ?F("Need to set option ssl to true before line ~w",
                               [Lno])};
                _ ->
                    {error, ?F("Expect integer 0..7 at line ~w", [Lno])}
            end;
        ["password", '=', Val] ->
            if
                is_record(C#sconf.ssl, ssl) ->
                    C2 = C#sconf{ssl = (C#sconf.ssl)#ssl{password = Val}},
                    fload(FD, ssl, GC, C2, Cs, Lno+1, Next);
                true ->
                    {error, ?F("Need to set option ssl to true before line ~w",
                               [Lno])}
            end;
        ["ciphers", '=', Val] ->
            if
                is_record(C#sconf.ssl, ssl) ->
                    C2 = C#sconf{ssl = (C#sconf.ssl)#ssl{ciphers = Val}},
                    fload(FD, ssl, GC, C2, Cs, Lno+1, Next);
                true ->
                    {error, ?F("Need to set option ssl to true before line ~w",
                               [Lno])}
            end;
        ['<', "/ssl", '>'] ->
            fload(FD, server, GC, C, Cs, Lno+1, Next);

        [H|T] ->
            {error, ?F("Unexpected input ~p at line ~w", [[H|T], Lno])};
        Err ->
            Err
    end;

fload(FD, rss, GC, C, Cs, Lno, Chars) ->
    %%?Debug("Chars: ~s", [Chars]),
    Next = io:get_line(FD, ''),
    case toks(Lno, Chars) of
        [] ->
            fload(FD, rss, GC, C, Cs, Lno+1, Next);
        ['<', "/rss", '>'] ->
            case get(rss_id) of
                undefined ->
                    {error, ?F("No rss_id specified at line ~w", [Lno])};
                RSSid ->
                    yaws_rss:open(RSSid, get(rss)),
                    fload(FD, server, GC, C, Cs, Lno+1, Next)
            end;
        ["rss_id", '=', Value] ->   % mandatory !!
            put(rss_id, list_to_atom(Value)),
            fload(FD, rss, GC, C, Cs, Lno+1, Next);
        ["rss_dir", '=', Value] ->   % mandatory !!
            put(rss, [{db_dir, Value} | get(rss)]),
            fload(FD, rss, GC, C, Cs, Lno+1, Next);
        ["rss_expire", '=', Value] ->
            put(rss, [{expire, Value} | get(rss)]),
            fload(FD, rss, GC, C, Cs, Lno+1, Next);
        ["rss_days", '=', Value] ->
            put(rss, [{days, Value} | get(rss)]),
            fload(FD, rss, GC, C, Cs, Lno+1, Next);
        ["rss_rm_exp", '=', Value] ->
            put(rss, [{rm_exp, Value} | get(rss)]),
            fload(FD, rss, GC, C, Cs, Lno+1, Next);
        ["rss_max", '=', Value] ->
            put(rss, [{rm_max, Value} | get(rss)]),
            fload(FD, rss, GC, C, Cs, Lno+1, Next);
        [H|T] ->
            {error, ?F("Unexpected input ~p at line ~w", [[H|T], Lno])};
        Err ->
            Err
    end;

fload(FD, opaque, GC, C, Cs, Lno, Chars) ->
    %%?Debug("Chars: ~s", [Chars]),
    Next = io:get_line(FD, ''),
    case toks(Lno, Chars) of
        [] ->
            fload(FD, opaque, GC, C, Cs, Lno+1, Next);
        ['<', "/opaque", '>'] ->
            fload(FD, server, GC, C, Cs, Lno+1, Next);
        [Key, '=', Value] ->
            C2 = C#sconf{opaque = [{Key,Value} | C#sconf.opaque]},
            fload(FD, opaque, GC, C2, Cs, Lno+1, Next);
        [Key, '='| Value] ->
            String_value = lists:flatten(
                             lists:map(
                               fun(Item) when is_atom(Item) ->
                                       atom_to_list(Item);
                                  (Item) ->
                                       Item
                               end, Value)),
            C2 = C#sconf{opaque = [{Key, String_value} | C#sconf.opaque]},
            fload(FD, opaque, GC, C2, Cs, Lno+1, Next);
        [H|T] ->
            {error, ?F("Unexpected input ~p at line ~w", [[H|T], Lno])};
        Err ->
            Err
    end.


fload(FD, server_auth, _GC, _C, _Cs, Lno, eof, _Auth) ->
    file:close(FD),
    {error, ?F("Unexpected end of file at line ~w", [Lno])};

fload(FD, server_auth, GC, C, Cs, Lno, Chars, Auth) ->
    %%?Debug("Chars: ~s", [Chars]),
    Next = io:get_line(FD, ''),
    case toks(Lno, Chars) of
        [] ->
            fload(FD, server_auth, GC, C, Cs, Lno+1, Next, Auth);
        ["docroot", '=', Docroot] ->
            A2 = Auth#auth{docroot = filename:absname(Docroot)},
            fload(FD, server_auth, GC, C, Cs, Lno+1, Next, A2);
        ["dir", '=', Authdir] ->
            case file:list_dir(Authdir) of
                {ok,_} when Authdir /= "/" ->
                    error_logger:info_msg("Warning, authdir must be set "
                                          "relative docroot ~n",[]);
                _ ->
                    ok
            end,
            Dir = yaws_api:path_norm(Authdir),
            A2 = Auth#auth{dir = [Dir | Auth#auth.dir]},
            fload(FD, server_auth, GC, C, Cs, Lno+1, Next, A2);
        ["realm", '=', Realm] ->
            A2 = Auth#auth{realm = Realm},
            fload(FD, server_auth, GC, C, Cs, Lno+1, Next, A2);
        ["authmod", '=', Mod] ->
            Mod2 = list_to_atom(Mod),
            code:ensure_loaded(Mod2),
            %% Add the auth header for the mod
	    H = try
                    Mod2:get_header() ++ Auth#auth.headers
                catch _:_ ->
                        error_logger:format("Failed to ~p:get_header() \n",
                                            [Mod2]),
                        Auth#auth.headers
                end,
            A2 = Auth#auth{mod = Mod2, headers = H},
            fload(FD, server_auth, GC, C, Cs, Lno+1, Next, A2);
        ["user", '=', User] ->
            case (catch string:tokens(User, ":")) of
                [Name, Passwd] ->
                    A2 = Auth#auth{users = [{Name, Passwd}|Auth#auth.users]},
                    fload(FD, server_auth, GC, C, Cs, Lno+1, Next, A2);
                _ ->
                    {error, ?F("Invalid user at line ~w", [Lno])}
            end;
        ["allow", '=', "all"] ->
            A2 = case Auth#auth.acl of
                     none    -> Auth#auth{acl={all, [], deny_allow}};
                     {_,D,O} -> Auth#auth{acl={all, D, O}}
                 end,
            fload(FD, server_auth, GC, C, Cs, Lno+1, Next, A2);
        ["allow", '=' | IPs] ->
            A2 = case Auth#auth.acl of
                     none ->
                         AllowIPs = parse_auth_ips(IPs, []),
                         Auth#auth{acl={AllowIPs, [], deny_allow}};
                     {all, _, _} ->
                         Auth;
                     {AllowIPs, DenyIPs, Order} ->
                         AllowIPs2 = parse_auth_ips(IPs, []) ++ AllowIPs,
                         Auth#auth{acl={AllowIPs2, DenyIPs, Order}}
                 end,
            fload(FD, server_auth, GC, C, Cs, Lno+1, Next, A2);
        ["deny", '=', "all"] ->
            A2 = case Auth#auth.acl of
                     none    -> Auth#auth{acl={[], all, deny_allow}};
                     {A,_,O} -> Auth#auth{acl={A, all, O}}
                 end,
            fload(FD, server_auth, GC, C, Cs, Lno+1, Next, A2);
        ["deny", '=' | IPs] ->
            A2 = case Auth#auth.acl of
                     none ->
                         DenyIPs = parse_auth_ips(IPs, []),
                         Auth#auth{acl={[], DenyIPs, deny_allow}};
                     {_, all, _} ->
                         Auth;
                     {AllowIPs, DenyIPs, Order} ->
                         DenyIPs2 = parse_auth_ips(IPs, []) ++ DenyIPs,
                         Auth#auth{acl={AllowIPs, DenyIPs2, Order}}
                 end,
            fload(FD, server_auth, GC, C, Cs, Lno+1, Next, A2);
        ["order", '=', "allow", ',', "deny"] ->
            A2 = case Auth#auth.acl of
                     none    -> Auth#auth{acl={[], [], allow_deny}};
                     {A,D,_} -> Auth#auth{acl={A, D, allow_deny}}
                 end,
            fload(FD, server_auth, GC, C, Cs, Lno+1, Next, A2);
        ["order", '=', "deny", ',', "allow"] ->
            A2 = case Auth#auth.acl of
                     none    -> Auth#auth{acl={[], [], deny_allow}};
                     {A,D,_} -> Auth#auth{acl={A, D, deny_allow}}
                 end,
            fload(FD, server_auth, GC, C, Cs, Lno+1, Next, A2);
        ["pam", "service", '=', Serv] ->
            A2 = Auth#auth{pam=Serv},
            fload(FD, server_auth, GC, C, Cs, Lno+1, Next, A2);
        ['<', "/auth", '>'] ->
            Pam = Auth#auth.pam,
            Users = Auth#auth.users,
            Realm = Auth#auth.realm,
            A2 =  case {Pam, Users} of
                      {false, []} ->
                          Auth;
                      _ ->
                          H = Auth#auth.headers ++
                              yaws:make_www_authenticate_header({realm, Realm}),
                          Auth#auth{headers = H}
                  end,
            Authdirs = case A2#auth.dir of
                           [] -> [A2#auth{dir="/"}];
                           Ds -> [A2#auth{dir=D} || D <- Ds]
                       end,
            C2 = C#sconf{authdirs = Authdirs ++ C#sconf.authdirs},
            fload(FD, server, GC, C2, Cs, Lno+1, Next);
        [H|T] ->
            {error, ?F("Unexpected input ~p at line ~w", [[H|T], Lno])};
        Err ->
            Err
    end;

fload(FD, server_redirect, _GC, _C, _Cs, Lno, eof, _RedirMap) ->
    file:close(FD),
    {error, ?F("Unexpected end of file at line ~w", [Lno])};

fload(FD, server_redirect, GC, C, Cs, Lno, Chars, RedirMap) ->
    %%?Debug("Chars: ~s", [Chars]),
    Next = io:get_line(FD, ''),
    Toks = toks(Lno, Chars),
    case Toks of
        [] ->
            fload(FD, server_redirect, GC, C, Cs, Lno+1, Next, RedirMap);
        [Path, '=', URL] ->
            try yaws_api:parse_url(URL, sloppy) of
                U when is_record(U, url) ->
                    fload(FD, server_redirect, GC, C, Cs, Lno+1, Next,
                          [{Path, U, append}|RedirMap])
            catch _:_ ->
                    {error, ?F("bad redir ~p at line ~w", [URL, Lno])}
            end;
        [Path, '=', '=', URL] ->
            try yaws_api:parse_url(URL, sloppy) of
                U when is_record(U, url) ->
                    fload(FD, server_redirect, GC, C, Cs, Lno+1, Next,
                          [{Path, U, noappend}|RedirMap])
            catch _:_ ->
                    {error, ?F("Bad redir ~p at line ~w", [URL, Lno])}
            end;
        ['<', "/redirect", '>'] ->
            C2 = C#sconf{redirect_map = lists:reverse(RedirMap)},
            fload(FD, server, GC, C2, Cs, Lno+1, Next);
        [H|T] ->
            {error, ?F("Unexpected input ~p at line ~w", [[H|T], Lno])};
        Err ->
            Err
    end;

fload(FD, server_deflate, _GC, _C, _Cs, Lno, eof, _Deflate) ->
    file:close(FD),
    {error, ?F("Unexpected end of file at line ~w", [Lno])};

fload(FD, server_deflate, GC, C, Cs, Lno, Chars, Deflate) ->
    Next = io:get_line(FD, ''),
    case toks(Lno, Chars) of
        [] ->
            fload(FD, server_deflate, GC, C, Cs, Lno+1, Next, Deflate);
        ["min_compress_size", '=', CSize] ->
            case (catch list_to_integer(CSize)) of
                I when is_integer(I), I > 0 ->
                    D2 = Deflate#deflate{min_compress_size=I},
                    fload(FD, server_deflate, GC, C, Cs, Lno+1, Next, D2);
                _ when CSize == "nolimit" ->
                    D2 = Deflate#deflate{min_compress_size=nolimit},
                    fload(FD, server_deflate, GC, C, Cs, Lno+1, Next, D2);
                _ ->
                    {error, ?F("Expect integer > 0 at line ~w", [Lno])}
            end;
        ["mime_types", '=' | MimeTypes] ->
            case parse_compressible_mime_types(MimeTypes,
                                               Deflate#deflate.mime_types) of
                {ok, L} ->
                    D2 = Deflate#deflate{mime_types=L},
                    fload(FD, server_deflate, GC, C, Cs, Lno+1, Next, D2);
                {error, Str} ->
                    {error, ?F("~s at line ~w", [Str, Lno])}
            end;
        ["compression_level", '=', CLevel] ->
            L = try
                    list_to_integer(CLevel)
                catch error:badarg ->
                        list_to_atom(CLevel)
                end,
            if
                L =:= none; L =:= default;
                L =:= best_compression; L =:= best_speed ->
                    D2 = Deflate#deflate{compression_level=L},
                    fload(FD, server_deflate, GC, C, Cs, Lno+1, Next, D2);
                is_integer(L), L >= 0, L =< 9 ->
                    D2 = Deflate#deflate{compression_level=L},
                    fload(FD, server_deflate, GC, C, Cs, Lno+1, Next, D2);
                true ->
                    {error, ?F("Bad compression level at line ~w", [Lno])}
            end;
        ["window_size", '=', WSize] ->
            case (catch list_to_integer(WSize)) of
                I when is_integer(I), I > 8, I < 16 ->
                    D2 = Deflate#deflate{window_size=I * -1},
                    fload(FD, server_deflate, GC, C, Cs, Lno+1, Next, D2);
                _ ->
                    {error,
                     ?F("Expect integer between 9..15 at line ~w",
                        [Lno])}
            end;
        ["mem_level", '=', MLevel] ->
            case (catch list_to_integer(MLevel)) of
                I when is_integer(I), I >= 1, I =< 9 ->
                    D2 = Deflate#deflate{mem_level=I},
                    fload(FD, server_deflate, GC, C, Cs, Lno+1, Next, D2);
                _ ->
                    {error, ?F("Expect integer between 1..9 at line ~w", [Lno])}
            end;
        ["strategy", '=', Strategy] ->
            if
                Strategy =:= "default";
                Strategy =:= "filtered";
                Strategy =:= "huffman_only" ->
                    D2 = Deflate#deflate{strategy=list_to_atom(Strategy)},
                    fload(FD, server_deflate, GC, C, Cs, Lno+1, Next, D2);
                true ->
                    {error,
                     ?F("Unknown strategy ~p at line ~w", [Strategy, Lno])}
            end;
        ["use_gzip_static", '=', Bool] ->
            case is_bool(Bool) of
                {true, Val} ->
                    D2 = Deflate#deflate{use_gzip_static=Val},
                    fload(FD, server_deflate, GC, C, Cs, Lno+1, Next, D2);
                false ->
                    {error, ?F("Expect true|false at line ~w", [Lno])}
            end;
        ['<', "/deflate", '>'] ->
            D2 = case Deflate#deflate.mime_types of
                     [] ->
                         Deflate#deflate{
                           mime_types = ?DEFAULT_COMPRESSIBLE_MIME_TYPES
                          };
                     _ ->
                         Deflate
                 end,
            C2 = C#sconf{deflate_options = D2},
            fload(FD, server, GC, C2, Cs, Lno+1, Next);
        [H|T] ->
            {error, ?F("Unexpected input ~p at line ~w", [[H|T], Lno])};
        Err ->
            Err
    end;

fload(FD, extra_cgi_vars, _GC, _C, _Cs, Lno, eof, _EVars) ->
    file:close(FD),
    {error, ?F("Unexpected end of file at line ~w", [Lno])};

fload(FD, extra_cgi_vars, GC, C, Cs, Lno, Chars, EVars = {Dir, Vars}) ->
    Next = io:get_line(FD, ''),
    case toks(Lno, Chars) of
        [] ->
            fload(FD, extra_cgi_vars, GC, C, Cs, Lno+1, Next, EVars);
        [Var, '=', Val] ->
	    fload(FD, extra_cgi_vars, GC, C, Cs, Lno+1, Next,
                  {Dir, [{Var, Val} | Vars]});
        ['<', "/extra_cgi_vars", '>'] ->
            C2 = C#sconf{extra_cgi_vars = [EVars | C#sconf.extra_cgi_vars]},
            fload(FD, server, GC, C2, Cs, Lno+1, Next);
        [H|T] ->
            {error, ?F("Unexpected input ~p at line ~w", [[H|T], Lno])};
        Err ->
            Err
    end.


is_bool("true") ->
    {true, true};
is_bool("false") ->
    {true, false};
is_bool(_) ->
    false.


warn_dir(Type, Dir) ->
    case is_dir(Dir) of
        true ->
            true;
        false ->
            error_logger:format("Config Warning: Directory ~s "
                                "for ~s doesn't exist~n",
                                [Dir, Type]),
            false
    end.

is_dir(Val) ->
    case file:read_file_info(Val) of
        {ok, FI} when FI#file_info.type == directory ->
            true;
        _ ->
            false
    end.


is_file(Val) ->
    case file:read_file_info(Val) of
        {ok, FI} when FI#file_info.type == regular ->
            true;
        _ ->
            false
    end.


%% tokenizer
toks(Lno, Chars) ->
    toks(Lno, Chars, free, [], []). % two accumulators

toks(Lno, [$#|_T], Mode, Ack, Tack) ->
    toks(Lno, [], Mode, Ack, Tack);

toks(Lno, [H|T], free, Ack, Tack) ->
    %%?Debug("Char=~p", [H]),
    case {is_quote(H), is_string_char([H|T]),is_special(H), yaws:is_space(H)} of
        {_,_, _, true} ->
            toks(Lno, T, free, Ack, Tack);
        {_,_, true, _} ->
            toks(Lno, T, free, [], [list_to_atom([H]) | Tack]);
        {_,true, _,_} ->
            toks(Lno, T, string, [H], Tack);
        {_,utf8, _,_} ->
            toks(Lno, tl(T), string, [H, hd(T)], Tack);
        {true,_, _,_} ->
            toks(Lno, T, quote, [], Tack);
        {false, false, false, false} ->
            {error, ?F("Unexpected character  <~p / ~c> at line ~w",
                       [H,H, Lno])}
    end;
toks(Lno, [C|T], string, Ack, Tack) ->
    case {is_backquote(C), is_quote(C), is_string_char([C|T]), is_special(C),
          yaws:is_space(C)} of
        {true, _, _, _,_} ->
            toks(Lno, T, [backquote,string], Ack, Tack);
        {_, _, true, _,_} ->
            toks(Lno, T, string, [C|Ack], Tack);
        {_, _, utf8, _,_} ->
            toks(Lno, tl(T), string, [C, hd(T)|Ack], Tack);
        {_, _, _, true, _} ->
            toks(Lno, T, free, [], [list_to_atom([C]),lists:reverse(Ack)|Tack]);
        {_, true, _, _, _} ->
            toks(Lno, T, quote, [], [lists:reverse(Ack)|Tack]);
        {_, _, _, _, true} ->
            toks(Lno, T, free, [], [lists:reverse(Ack)|Tack]);
        {false, false, false, false, false} ->
            {error, ?F("Unexpected character  <~p / ~c> at line ~w",
                       [C, C, Lno])}
    end;
toks(Lno, [C|T], quote, Ack, Tack) ->
    case {is_quote(C), is_backquote(C)} of
        {true, _} ->
            toks(Lno, T, free, [], [lists:reverse(Ack)|Tack]);
        {_, true} ->
            toks(Lno, T, [backquote,quote], [C|Ack], Tack);
        {false, false} ->
            toks(Lno, T, quote, [C|Ack], Tack)
    end;
toks(Lno, [C|T], [backquote,Mode], Ack, Tack) ->
    toks(Lno, T, Mode, [C|Ack], Tack);
toks(_Lno, [], string, Ack, Tack) ->
    lists:reverse([lists:reverse(Ack) | Tack]);
toks(_Lno, [], free, _,Tack) ->
    lists:reverse(Tack).

is_quote(34) -> true ;  %% $" but emacs mode can't handle it
is_quote(_)  -> false.

is_backquote($\\) -> true ;
is_backquote(_)  -> false.

is_string_char([C|T]) ->
    if
        $a =< C, C =< $z ->
            true;
        $A =< C, C =< $Z ->
            true;
        $0 =< C, C =< $9 ->
            true;
        C == 195 , T /= [] ->
            %% FIXME check that [C, hd(T)] really is a char ?? how
            utf8;
        true ->
            lists:member(C, [$., $/, $:, $_, $-, $+, $~, $@, $*])
    end.

is_special(C) ->
    lists:member(C, [$=, $<, $>, $,]).

%% parse the argument string PLString which can either be the undefined
%% atom or a proplist. Currently the only supported keys are
%% fullsweep_after, min_heap_size, and min_bin_vheap_size. Any other
%% key/values are ignored.
parse_process_options(PLString) ->
    case erl_scan:string(PLString ++ ".") of
        {ok, PLTokens, _} ->
            case erl_parse:parse_term(PLTokens) of
                {ok, undefined} ->
                    {ok, []};
                {ok, []} ->
                    {ok, []};
                {ok, [Hd|_Tl]=PList} when is_atom(Hd); is_tuple(Hd) ->
                    %% create new safe proplist of desired options
                    {ok, proplists_int_copy([], PList, [fullsweep_after,
                                                        min_heap_size,
                                                        min_bin_vheap_size])};
                _ ->
                    {error, "Expect undefined or proplist"}
            end;
        _ ->
            {error, "Expect undefined or proplist"}
    end.

%% copy proplist integer values for the given keys from the
%% Src proplist to the Dest proplist. Ignored keys that are not
%% found or have non-integer values. Returns the new Dest proplist.
proplists_int_copy(Dest, _Src, []) ->
    Dest;
proplists_int_copy(Dest, Src, [Key|NextKeys]) ->
    case proplists:get_value(Key, Src) of
        Val when is_integer(Val) ->
            proplists_int_copy([{Key, Val}|Dest], Src, NextKeys);
        _ ->
            proplists_int_copy(Dest, Src, NextKeys)
    end.

parse_soap_srv_mods(['<', Module, ',' , Handler, ',', WsdlFile, '>' | Tail],
                    Ack) ->
    case is_file(WsdlFile) of
        true ->
            S = { {list_to_atom(Module), list_to_atom(Handler)}, WsdlFile},
            parse_soap_srv_mods(Tail, [S |Ack]);
        false ->
            {error, ?F("Bad wsdl file ~p", [WsdlFile])}
    end;

parse_soap_srv_mods(['<', Module, ',' , Handler, ',', WsdlFile, ',',
                     Prefix, '>' | Tail], Ack) ->
    case is_file(WsdlFile) of
        true ->
            S = { {list_to_atom(Module), list_to_atom(Handler)},
                  WsdlFile, Prefix},
            parse_soap_srv_mods(Tail, [S |Ack]);
        false ->
            {error, ?F("Bad wsdl file ~p", [WsdlFile])}
    end;

parse_soap_srv_mods([ SoapSrvMod | _Tail], _Ack) ->
    {error, ?F("Bad soap_srv_mods syntax: ~p", [SoapSrvMod])};

parse_soap_srv_mods([], Ack) ->
    {ok, Ack}.

parse_appmods(['<', PathElem, ',' , AppMod, '>' | Tail], Ack) ->
    S = {PathElem , list_to_atom(AppMod)},
    parse_appmods(Tail, [S |Ack]);

parse_appmods(['<', PathElem, ',' , AppMod, "exclude_paths" |Tail], Ack)->
    Paths = lists:takewhile(fun(X) -> X /= '>' end,
                            Tail),
    Tail2 = lists:dropwhile(fun(X) -> X /= '>' end,
                            Tail),
    Tail3 = tl(Tail2),

    S = {PathElem , list_to_atom(AppMod), lists:map(
                                            fun(Str) ->
                                                    string:tokens(Str, "/")
                                            end, Paths)},
    parse_appmods(Tail3, [S |Ack]);


parse_appmods([AppMod | Tail], Ack) ->
    %% just some simpleminded test to catch syntax errors in the config
    case AppMod of
        [Char] ->
            case is_special(Char) of
                true ->
                    {error, "Bad appmod syntax"};
                false ->
                    S = {AppMod, list_to_atom(AppMod)},
                    parse_appmods(Tail, [S | Ack])
            end;
        _ ->
            S = {AppMod, list_to_atom(AppMod)},
            parse_appmods(Tail, [S | Ack])
    end;

parse_appmods([], Ack) ->
    {ok, Ack}.


parse_revproxy([Prefix, Url]) ->
    parse_revproxy_url(Prefix, Url);
parse_revproxy([Prefix, Url, "intercept_mod", InterceptMod]) ->
    case parse_revproxy_url(Prefix, Url) of
        {ok, RP} ->
            {ok, RP#proxy_cfg{intercept_mod = list_to_atom(InterceptMod)}};
        Error ->
            Error
    end;
parse_revproxy(_Other) ->
    {error, syntax}.

parse_revproxy_url(Prefix, Url) ->
    case (catch yaws_api:parse_url(Url)) of
        {'EXIT', _} ->
            {error, url};
        URL when URL#url.path == "/" ->
            P = case lists:reverse(Prefix) of
                    [$/|_Tail] ->
                        Prefix;
                    Other ->
                        lists:reverse(Other)
                end,
            {ok, #proxy_cfg{prefix=P, url=URL}};
        _URL ->
            {error, "Can't revproxy to a URL with a path "}
    end.


parse_expires(['<', MimeType, ',' , Expire, '>' | Tail], Ack) ->
    {Type, Value} =
        case string:tokens(Expire, "+") of
            [Secs] ->
                {access, (catch list_to_integer(Secs))};
            ["access", Secs] ->
                {access, (catch list_to_integer(Secs))};
            ["modify", Secs] ->
                {modify, (catch list_to_integer(Secs))};
            _ ->
                {error, "Bad expires syntax"}
        end,
    if
        Type =:= error ->
            {Type, Value};
        not is_integer(Value) ->
            {error, "Bad expires syntax"};
        true ->
            E = {MimeType, Type, Value},
            parse_expires(Tail, [E |Ack])
    end;
parse_expires([], Ack)->
    {ok, Ack}.


parse_phpmod(['<', "cgi", ',', DefaultPhpPath, '>'], DefaultPhpPath) ->
    {ok, {cgi, DefaultPhpPath}};
parse_phpmod(['<', "cgi", ',', PhpPath, '>'], _) ->
    case is_file(PhpPath) of
        true ->
            {ok, {cgi, PhpPath}};
        false ->
            {error, ?F("~s is not a regular file", [PhpPath])}
    end;
parse_phpmod(['<', "fcgi", ',', HostPortSpec, '>'], _) ->
    case string_to_host_and_port(HostPortSpec) of
        {ok, Host, Port} ->
            {ok, {fcgi, {Host, Port}}};
        {error, Reason} ->
            {error, Reason}
    end;
parse_phpmod(['<', "extern", ',', NodeModFunSpec, '>'], _) ->
    case string_to_node_mod_fun(NodeModFunSpec) of
        {ok, Node, Mod, Fun} ->
            {ok, {extern, {Node,Mod,Fun}}};
        {ok, Mod, Fun} ->
            {ok, {extern, {Mod,Fun}}};
        {error, Reason} ->
            {error, Reason}
    end.


parse_compressible_mime_types(_, all) ->
    {ok, all};
parse_compressible_mime_types(["all"|_], _Acc) ->
    {ok, all};
parse_compressible_mime_types(["*/*"|_], _Acc) ->
    {ok, all};
parse_compressible_mime_types(["defaults"|Rest], Acc) ->
    parse_compressible_mime_types(Rest, ?DEFAULT_COMPRESSIBLE_MIME_TYPES++Acc);
parse_compressible_mime_types([',' | Rest], Acc) ->
    parse_compressible_mime_types(Rest, Acc);
parse_compressible_mime_types([MimeType | Rest], Acc) ->
    Res = re:run(MimeType, "^([-\\w\+]+)/([-\\w\+\.]+|\\*)$",
                 [{capture, all_but_first, list}]),
    case Res of
        {match, [Type,"*"]} ->
            parse_compressible_mime_types(Rest, [{Type, all}|Acc]);
        {match, [Type,SubType]} ->
            parse_compressible_mime_types(Rest, [{Type, SubType}|Acc]);
        nomatch ->
            {error, "Invalid MimeType"}
    end;
parse_compressible_mime_types([], Acc) ->
    {ok, Acc}.


ssl_start() ->
    case catch ssl:start() of
        ok ->
            ok;
        {error,{already_started,ssl}} ->
            ok;
        Err ->
            error_logger:format("Failed to start ssl: ~p~n", [Err])
    end.



%% search for an SC within Pairs that have the same, listen,port,ssl,severname
%% Return {Pid, SC} or false
%% Pairs is the pairs in yaws_server #state{}
search_sconf(NewSC, Pairs) ->
    case lists:zf(
           fun({Pid, Scs = [SC|_]}) ->
                   case same_virt_srv(NewSC, SC) of
                       true ->
                           case lists:keysearch(NewSC#sconf.servername,
                                                #sconf.servername, Scs) of
                               {value, Found} ->
                                   {true, {Pid, Found, Scs}};
                               false ->
                                   false
                           end;
                       false ->
                           false
                   end
           end, Pairs) of
        [] ->
            false;
        [{Pid, Found, Scs}] ->
            {Pid, Found, Scs};
        _Other ->
            error_logger:format("Fatal error, no two sconfs should "
                                " ever be considered equal ..",[]),
            erlang:error(fatal_conf)
    end.

%% find the group a new SC would belong to
search_group(SC, Pairs) ->
    Fun =  fun({Pid, [S|Ss]}) ->
                   case same_virt_srv(S, SC) of
                       true ->
                           {true, {Pid, [S|Ss]}};
                       false ->
                           false
                   end
           end,

    lists:zf(Fun, Pairs).


%% Return a new Pairs list with one SC updated
update_sconf(NewSC, Pairs) ->
    lists:map(
      fun({Pid, Scs}) ->
              {Pid,
               lists:map(fun(SC) ->
                                 case same_sconf(SC, NewSC) of
                                     true ->
                                         NewSC;
                                     false ->
                                         SC
                                 end
                         end, Scs)
              }
      end, Pairs).


%% return a new pairs list with SC removed
delete_sconf(OldSc, Pairs) ->
    lists:zf(
      fun({Pid, Scs}) ->
              case same_virt_srv(hd(Scs), OldSc) of
                  true ->
                      L2 = lists:keydelete(OldSc#sconf.servername,
                                           #sconf.servername, Scs),
                      {true, {Pid, L2}};
                  false ->
                      {true, {Pid, Scs}}
              end

      end, Pairs).



same_virt_srv(S, NewSc) when
      S#sconf.listen == NewSc#sconf.listen,
      S#sconf.port == NewSc#sconf.port,
      S#sconf.ssl == NewSc#sconf.ssl ->
    true;
same_virt_srv(_,_) ->
    false.


same_sconf(S, NewSc) ->
    same_virt_srv(S, NewSc) andalso
        S#sconf.servername == NewSc#sconf.servername.





eq_sconfs(S1,S2) ->
    (S1#sconf.port == S2#sconf.port andalso
     S1#sconf.flags == S2#sconf.flags andalso
     S1#sconf.rhost == S2#sconf.rhost andalso
     S1#sconf.rmethod == S2#sconf.rmethod andalso
     S1#sconf.docroot == S2#sconf.docroot andalso
     S1#sconf.listen == S2#sconf.listen andalso
     S1#sconf.servername == S2#sconf.servername andalso
     S1#sconf.ssl == S2#sconf.ssl andalso
     S1#sconf.authdirs == S2#sconf.authdirs andalso
     S1#sconf.appmods == S2#sconf.appmods andalso
     S1#sconf.partial_post_size == S2#sconf.partial_post_size andalso
     S1#sconf.errormod_404 == S2#sconf.errormod_404 andalso
     S1#sconf.errormod_crash == S2#sconf.errormod_crash andalso
     S1#sconf.arg_rewrite_mod == S2#sconf.arg_rewrite_mod andalso
     S1#sconf.opaque == S2#sconf.opaque andalso
     S1#sconf.start_mod == S2#sconf.start_mod andalso
     S1#sconf.allowed_scripts == S2#sconf.allowed_scripts andalso
     S1#sconf.revproxy == S2#sconf.revproxy andalso
     S1#sconf.soptions == S2#sconf.soptions andalso
     S1#sconf.fcgi_app_server == S2#sconf.fcgi_app_server).




%% This the version of setconf that perform a
%% soft reconfig, it requires the args to be checked.
soft_setconf(GC, Groups, OLDGC, OldGroups) ->
    if
        GC /= OLDGC ->
            yaws_trace:setup(GC),
            update_gconf(GC);
        true ->
            ok
    end,
    Rems = remove_old_scs(lists:flatten(OldGroups), Groups),
    Adds = soft_setconf_scs(lists:flatten(Groups), OldGroups),
    lists:foreach(
      fun({delete_sconf, SC}) ->
              delete_sconf(SC);
         ({add_sconf, SC}) ->
              add_sconf(SC);
         ({update_sconf, SC}) ->
              update_sconf(SC)
      end, Rems ++ Adds).



hard_setconf(GC, Groups) ->
    case gen_server:call(yaws_server,{setconf, GC, Groups},infinity) of
        ok ->
            yaws_log:setup(GC, Groups),
            yaws_trace:setup(GC);
        E ->
            erlang:error(E)
    end.



remove_old_scs([Sc|Scs], NewGroups) ->
    case find_group(Sc, NewGroups) of
        false ->
            [{delete_sconf, Sc} |remove_old_scs(Scs, NewGroups)];
        {true, G} ->
            case find_sc(Sc, G) of
                false ->
                    [{delete_sconf, Sc} | remove_old_scs(Scs, NewGroups)];
                _ ->
                    remove_old_scs(Scs, NewGroups)
            end
    end;
remove_old_scs([],_) ->
    [].

soft_setconf_scs([Sc|Scs], OldGroups) ->
    case find_group(Sc, OldGroups) of
        false ->
            [{add_sconf,Sc} | soft_setconf_scs(Scs, OldGroups)];
        {true, G} ->
            case find_sc(Sc, G) of
                false ->
                    [{add_sconf, Sc} | soft_setconf_scs(Scs, OldGroups)];
                {true, _OldSc} ->
                    [{update_sconf,Sc} | soft_setconf_scs(Scs, OldGroups)]
            end
    end;
soft_setconf_scs([],_) ->
    [].


%% checking code

can_hard_gc(New, Old) ->
    if
        Old == undefined ->
            true;
        New#gconf.yaws_dir == Old#gconf.yaws_dir,
        New#gconf.runmods == Old#gconf.runmods,
        New#gconf.logdir == Old#gconf.logdir ->
            true;
        true ->
            false
    end.



can_soft_setconf(NEWGC, NewGroups, OLDGC, OldGroups) ->
    can_soft_gc(NEWGC, OLDGC) andalso
        can_soft_sconf(lists:flatten(NewGroups), OldGroups).

can_soft_gc(G1, G2) ->
    if
        G1#gconf.flags == G2#gconf.flags,
        G1#gconf.logdir == G2#gconf.logdir,
        G1#gconf.log_wrap_size == G2#gconf.log_wrap_size,
        G1#gconf.id == G2#gconf.id ->
            true;
        true ->
            false
    end.


can_soft_sconf([Sc|Scs], OldGroups) ->
    case find_group(Sc, OldGroups) of
        false ->
            can_soft_sconf(Scs, OldGroups);
        {true, G} ->
            case find_sc(Sc, G) of
                false ->
                    can_soft_sconf(Scs, OldGroups);
                {true, Old} when Old#sconf.start_mod /= Sc#sconf.start_mod ->
                    false;
                {true, Old} ->
                    case
                        {proplists:get_value(listen_opts, Old#sconf.soptions),
                         proplists:get_value(listen_opts, Sc#sconf.soptions)} of
                        {Opts, Opts} ->
                            can_soft_sconf(Scs, OldGroups);
                        _ ->
                            false
                    end
            end
    end;
can_soft_sconf([], _) ->
    true.


find_group(SC, [G|Gs]) ->
    case same_virt_srv(SC, hd(G)) of
        true ->
            {true, G};
        false ->
            find_group(SC, Gs)
    end;
find_group(_,[]) ->
    false.

find_sc(SC, [S|Ss]) ->
    if SC#sconf.servername  == S#sconf.servername  ->
            {true, S};
       true ->
            find_sc(SC, Ss)
    end;
find_sc(_SC,[]) ->
    false.


verify_upgrade_args(GC, Groups0) when is_record(GC, gconf) ->
    case is_groups(Groups0) of
        true ->
            %% Embedded code may give appmods as a list of strings, or
            %% appmods can be {StringPathElem,ModAtom} or
            %% {StringPathElem,ModAtom,ExcludePathsList} tuples. Handle
            %% all possible variants here.
            Groups = yaws:deepmap(
                       fun(SC) ->
                               SC#sconf{appmods =
                                            lists:map(
                                              fun({PE, Mod}) ->
                                                      {PE, Mod};
                                                 ({PE,Mod,Ex}) ->
                                                      {PE,Mod,Ex};
                                                 (AM) when is_list(AM) ->
                                                      {AM,list_to_atom(AM)};
                                                 (AM) when is_atom(AM) ->
                                                      {atom_to_list(AM), AM}
                                              end,
                                              SC#sconf.appmods)}
                       end, Groups0),
            {GC, Groups};
        _ ->
            erlang:error(badgroups)
    end.


%% verify args to setconf
is_groups([H|T]) ->
    case is_list_of_scs(H) of
        true ->
            is_groups(T);
        false ->
            false
    end;
is_groups([]) ->
    true.

is_list_of_scs([H|T]) when is_record(H, sconf) ->
    is_list_of_scs(T);
is_list_of_scs([]) ->
    true;
is_list_of_scs(_) ->
    false.

add_sconf(SC) ->
    ok= gen_server:call(yaws_server, {add_sconf, SC}, infinity),
    ok = yaws_log:add_sconf(SC).

update_sconf(SC) ->
    ok = gen_server:call(yaws_server, {update_sconf, SC}, infinity).

delete_sconf(SC) ->
    ok = gen_server:call(yaws_server, {delete_sconf, SC}, infinity),
    ok = yaws_log:del_sconf(SC).

update_gconf(GC) ->
    ok = gen_server:call(yaws_server, {update_gconf, GC}, infinity).


-define(MAXBITS_IPV4, 32).
-define(MAXBITS_IPV6, 128).

parse_auth_ips([], Result) ->
    Result;
parse_auth_ips([Str|Rest], Result) ->
    try
        parse_auth_ips(Rest, [parse_ip(Str)|Result])
    catch
        _:_ -> parse_auth_ips(Rest, Result)
    end.

parse_ip(Str) when is_list(Str) ->
    [Ip|Rest] = string:tokens(Str, [$/]),
    {Type, IpInt}  = ip_to_integer(Ip),
    case Rest of
        [] ->
            {IpInt, IpInt};
        [NetMask] ->
            MaskInt  = netmask_to_integer(Type, NetMask),
            WildCard = netmask_to_wildcard(Type, MaskInt),
            NetAddr  = IpInt band MaskInt,
            case Type of
                ipv4 -> {NetAddr + 1, NetAddr + WildCard - 1};
                ipv6 -> {NetAddr, NetAddr + WildCard}
            end;
        _ ->
            throw({error, einval})
    end;
parse_ip(_) ->
    throw({error, einval}).

ip_to_integer(Str) when is_list(Str) ->
    case inet_parse:ipv6_address(Str) of
        {ok, {0,0,0,0,0,16#FFFF,N1,N2}} ->
            {ipv4, (N1 bsl 16) bor N2};
        {ok, Ip} ->
            ip_to_integer(Ip);
        {error, Reason} ->
            throw({error, Reason})
    end;
ip_to_integer({N1,N2,N3,N4}) ->
    Int = (N1 bsl 24) bor (N2 bsl 16) bor (N3 bsl 8) bor N4,
    if
        (Int bsr ?MAXBITS_IPV4) == 0 -> {ipv4, Int};
        true -> throw({error, einval})
    end;
ip_to_integer({N1,N2,N3,N4,N5,N6,N7,N8}) ->
    Int = (N1 bsl 112) bor (N2 bsl 96) bor (N3 bsl 80) bor (N4 bsl 64) bor
        (N5 bsl 48) bor (N6 bsl 32) bor (N7 bsl 16) bor N8,
    if
        (Int bsr ?MAXBITS_IPV6) == 0 -> {ipv6, Int};
        true -> throw({error, einval})
    end;
ip_to_integer(_) ->
    throw({error, einval}).


netmask_to_integer(Type, NetMask) ->
    case catch list_to_integer(NetMask) of
        I when is_integer(I) ->
            case Type of
                ipv4 -> (1 bsl ?MAXBITS_IPV4) - (1 bsl (?MAXBITS_IPV4 - I));
                ipv6 -> (1 bsl ?MAXBITS_IPV6) - (1 bsl (?MAXBITS_IPV6 - I))
            end;
        _ ->
            case ip_to_integer(NetMask) of
                {Type, MaskInt} -> MaskInt;
                _               -> throw({error, einval})
            end
    end.

netmask_to_wildcard(ipv4, Mask) -> ((1 bsl ?MAXBITS_IPV4) - 1) bxor Mask;
netmask_to_wildcard(ipv6, Mask) -> ((1 bsl ?MAXBITS_IPV6) - 1) bxor Mask.

subconfigdir_fold(_File, {error, _Err}=Acc) ->
    Acc;
subconfigdir_fold(File, {ok, GCp, Csp}=Acc) ->
    case is_file(File) of
        true ->
            error_logger:info_msg("Yaws: Using subconfig file ~s~n", [File]),
            case file:open(File, [read]) of
                {ok, FD1} ->
                    R = (catch fload(FD1,
                                     globals,
                                     GCp,
                                     undefined,
                                     Csp,
                                     1,
                                     io:get_line(FD1, ''))),
                    ?Debug("FLOAD: ~p", [R]),
                    R;
                _ ->
                    {error, "Can't open config file " ++ File}
            end;
        false ->
            %% Ignore subdirectories
            Acc
    end.
