%%% File        : yapp.erl
%%% @author Mikael Karlsson <mikael@creado.se>
%%% @since 11 Dec 2005 by Mikael Karlsson <mikael@creado.se>
%%% @see yapp_handler
%%% @doc Yaws applications handler. 
%%% <p>An easy way to deploy Yaws applications independently of
%%% each other.</p>
%%% <p>This module implements Yaws runmod and arg_rewrite_mod interfaces.
%%% It has functions to insert and remove Yapps to the Yaws sconfs records. 
%%% It is used by the yapp_handler implementation. The yapp module will make Yaws 
%%% temporarily switch the docroot
%%% to the applications priv/docroot directory when it encounters the registered
%%% URL path of the application. One can set another docroot than the default
%%%  (priv/docroot) by setting the environment variable yapp_docroot in the 
%%% .app file for the application.
%%% The application may also have own appmods which are put in the
%%% application environment variable yapp_appmods like: </p>
%%% <code>  {env, [
%%%         {yapp_appmods,[{"ctrl",enityme_controller}]}
%%%        ]}, </code>
%%% <p>
%%% In order to include the yapp module, runmod shall be set to yapp.
%%% The arg_rewrite_mod shall also be set to yapp and the opaque variable 
%%% yapp_server_id shall be set to an unique string for every virtual server 
%%% using yapp, like: </p>
%%% <pre>
%%% .
%%% ebin_dir = /usr/local/yaws/lib/yapp/ebin
%%% .
%%% runmod = yapp
%%% .
%%% &lt;server aspen4&gt; 
%%%        port = 8001
%%%        listen = 0.0.0.0
%%%        dir_listings = true
%%%        arg_rewrite_mod = yapp        
%%%        docroot = /home/yaws/scripts/../www 
%%%        &lt;opaque&gt;
%%%                yapp_server_id = aspeninternal
%%%                bootstrap_yapps = yapp
%%%        &lt;/opaque&gt;
%%% &lt;/server&gt;
%%% </pre>
%%%
%%% <p><em>Note:</em> The yapp application is in itself a "Yapp" with a small admin console.
%%% So by adding the line <code>bootstrap_yapps = yapp</code> as above you will get access
%%% to the admin console on http://yourservername/yapp/index.html</p>
%%% <p><em>Note2:</em> The current available registry implementation uses Mnesia so you need
%%% to create a mnesia schema (if not already done) for the node(s) you are running Yaws on.
%%% <code>mnesia:create_schema([node()]).</code></p>
%%%
%%% @type yawsArg() = term(). This is the #arg record as defined
%%% by yaws_api.hrl.
%%%
%%% @type yawsSconf() = term(). This is the #sconf record as defined
%%% by yaws.hrl.
%%%
%%% @type yawsGconf() = term(). This is the #gconf record as defined
%%% by yaws.hrl.

-module(yapp).
-author('mikael@creado.se').

-include("yaws_api.hrl").
-include("yaws.hrl").
-export([arg_rewrite/1, start/0, prepath/1, insert/1, insert/2, remove/2,
         log/3,
         reset_yaws_conf/0, srv_id/1, 
         get_bootstrap_yapps/0, get_yapps/0, get_server_ids/0]).
 %%     reload_yaws/0,

-define(prepath, yapp_prepath).
-define(srv_id, "yapp_server_id").
-define(bootstrap_yapps, "bootstrap_yapps"). %% Opaque key for "bootstrap yapps"
-define(erlyweb_appname,"appname").
-define(yapp_list, yapp_list).

-define(priv_docroot, "priv/docroot").

%% This record is stored in the sconf opaque property yapp_list
-record(yapp, {
          urlpath,
          docroot = "",
          appname = "",
          appmods = [],
          opaque = []
         }).

%% The yapp_reg is stored in Mnesia in the y_registry 
%% and contains a list of {yapp_server_id, yapps} tuples, where 
%% yapps is a list of {UrlPath, AppName} tuples
%% UrlPath = string()
%% Interface arg_rewrite_mod
%% @spec arg_rewrite(Arg::yawsArg()) -> yawsArg()
%% @doc Interface function for a Yaws arg_rewrite_mod. If
%% it finds a registered Yapp it will strip of the
%% path up to the Yapp and redirect the docroot.
arg_rewrite(Arg) ->
    case find_registered_yapps(Arg) of
        undefined ->
            Arg;
        {#yapp{urlpath = YappPath, docroot = Docroot, 
               appmods = YappMods, opaque = YOpaque }, _Rest} ->

            DocMount =
                case string:right(YappPath,1) of
                    "/" -> YappPath;
                    _ -> YappPath ++ "/"
                end,

            VDir = {"vdir", DocMount ++ " " ++ Docroot},

            AddOpaque = [ VDir | YOpaque], 
            
            %% Add Yapp appmods, Yaws uses process dictionary.
            SC = get(sc),
            AppMods = SC#sconf.appmods,
            Opaque = SC#sconf.opaque,
            SC2 = SC#sconf{docroot=Docroot, appmods = AppMods ++ YappMods, 
                           opaque = AddOpaque ++ Opaque},
            put(sc, SC2),

            Opaque2 = Arg#arg.opaque,
            Arg#arg{docroot=Docroot, docroot_mount=DocMount,
                    opaque = AddOpaque ++ Opaque2}
    end.


%% Interface run_mod
%% @spec start() -> void()
%% @doc Interface function for Yaws run_mod.
%% This fun is spawned from Yaws so no return val is expected.
%% All Yapps can expect mnesia to be started, so mnesia is ensured
%% to be started.
%% For every server id that has Yapps the yapp module is registered
%% as an arg_rewrite_mod in #sconf.
%% Configuration data for mapping Yapp paths to applications is looked
%% up in a mnesia registry table and stored in Yaws #sconf.opaque record
%% for the each server id. 
start() ->
    case wait_for_yaws() of
	ok ->
	    log(info, "Starting yapp~n",[]),
	    application:start(yapp);
	Error ->
	    log(error, "Failed waiting for Yaws to start when starting Yapp: ~p~n",[Error])
    end.
    
wait_for_yaws() ->  wait_for_yaws(20).

wait_for_yaws(0) -> {error, timeout};
wait_for_yaws(N) ->
    WA = application:which_applications(),
    case lists:keysearch(yaws, 1, WA) of
	false ->
	    log(info, "Yapp starting but Yaws not ready - waiting 500 ms",[]),
	    receive after 500 -> ok end, %% Give Yaws some time to settle own things
	    wait_for_yaws(N-1);
	_ -> 
	    ok
    end.
     

%% @spec prepath(Arg::yawsArg()) -> Path::string()
%% @doc Get the Yapp root-path. Can be called from a Yapp erl/1
%% fun or an erl section in a .yaws file to get the Yapp root
%% path. 
prepath(Arg) ->
    Arg#arg.docroot_mount.


%% @spec log(Level, FormatStr::string(), Args) -> void()
%%   Level = error | warning | info | debug
%%   Args = [term()]
%% @doc Yapp interface to the error_logger.
log(debug, FormatStr, Args) ->
    gen_event:notify(error_logger, {debug_msg, group_leader(), {self(), FormatStr, Args}});
log(info, FormatStr, Args) ->
    error_logger:info_msg(FormatStr, Args);
log(warning, FormatStr, Args) ->
    error_logger:warning_msg(FormatStr, Args);
log(error, FormatStr, Args) ->
    error_logger:error_msg(FormatStr, Args);
log(Level, FormatStr, Args) ->
    error_logger:error_msg("Unknown logging level ~p  ," ++ FormatStr,[Level|Args]).

%% Utility function

find_registered_yapps(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    A = proplists:get_value(?yapp_list, Arg#arg.opaque,[]),
    find_registered(A, Path).

find_registered([],_Path) ->
    undefined;
find_registered([ #yapp{urlpath = RegPath} = Y | T ], Path) ->
    case string:str(Path, RegPath) of  
        1 ->
            case string:substr(Path,1+length(RegPath)) of
                [] -> {Y,[]};
                [$/ |_] = Rest -> {Y,Rest};
                _ -> find_registered(T,Path)
            end;
        _ ->
            find_registered(T, Path)
    end.

%% @hidden
insert([]) -> 
    ok;
insert(Yapps) when is_list(Yapps) ->
    {ok, Gconf, Sconfs} = get_conf(),
    NewSconfs = (catch insert_yapps_in_sconfs(Yapps, Sconfs)),
    yaws_api:setconf(Gconf, NewSconfs).
%% @hidden
insert(SrvId, Yapp)->
    {ok, Gconf, SconfGroups} = get_conf(),
    NewSconfGroups = insert_yapp_in_sconfgroups(SrvId, Yapp, SconfGroups),
    yaws_api:setconf(Gconf, NewSconfGroups).
    

insert_yapps_in_sconfs([], SconfGroups) ->
    SconfGroups;
insert_yapps_in_sconfs([{_SrvId,[]}|T], SconfGroups) ->
    insert_yapps_in_sconfs(T, SconfGroups);
insert_yapps_in_sconfs([{SrvId,[Y|YS]}|T], SconfGroups) ->
    NewSconfGroups = insert_yapp_in_sconfgroups(SrvId,Y,SconfGroups),
    insert_yapps_in_sconfs([{SrvId,YS}|T], NewSconfGroups).

insert_yapp_in_sconfgroups(_SrvId, _Yapp,[]) ->
    [];
insert_yapp_in_sconfgroups(SrvId, Yapp, [SCG|T]) ->
    [insert_yapp_in_sconfgroup(SrvId,Yapp, SCG) | insert_yapp_in_sconfgroups(SrvId,Yapp,T)].

insert_yapp_in_sconfgroup(_SrvId, _Yapp, []) ->
    [];
insert_yapp_in_sconfgroup(SrvId, Yapp, [#sconf{}=SC|SCG]) ->
    case srv_id(SC) of
        SrvId ->
            case insert_yapp_in_sconf(Yapp,SC) of
                no_app ->
                    [SC | SCG];
                NewSC ->
                    [NewSC | SCG]
            end;
        _ ->
            [ SC | insert_yapp_in_sconfgroup(SrvId,Yapp,SCG)]
    end.

%% the yapp application itself maybe a yapp, treated as special case
insert_yapp_in_sconf({UrlPath, yapp}, SC) ->
    insert_yapp_in_sconf0({UrlPath, yapp}, SC);
insert_yapp_in_sconf({UrlPath, AppName}, SC) ->
    case start_app([AppName]) of
        ok -> 
            insert_yapp_in_sconf0({UrlPath, AppName}, SC);
        Error ->
            log(error, "yapp:insert_yapp_in_sconf - Error loading Yapp ~p, ~p", 
                [AppName, Error]),
            no_app
    end.

start_app([AppName|T]) ->
    log(info, "Starting app ~p" , [AppName]),
    case application:start(AppName) of
       {error,{not_started,RequiredApp}} ->
           start_app([RequiredApp,AppName|T]);
       {error,{already_started,AppName}} ->
           start_app(T);
       ok ->
           start_app(T);
       Error -> Error
    end;
start_app([]) -> ok;
start_app(Error) -> Error.

insert_yapp_in_sconf0({UrlPath, AppName}, #sconf{opaque = OP} = SC) ->
    log(info,"Inserting App ~p in Url ~p~n", [AppName, UrlPath]),
    AppEnv = application:get_all_env(AppName),
    DocSubRoot = proplists:get_value(yapp_docroot, AppEnv, ?priv_docroot),
    YAppMods   = proplists:get_value(yapp_appmods, AppEnv, []), 
    YOpaque    = proplists:get_value(yapp_opaque,  AppEnv, []), 
    Y = #yapp{urlpath= UrlPath,
              docroot = code:lib_dir(AppName) ++ "/" ++ DocSubRoot,
              appname = AppName,
              appmods = YAppMods,
              opaque  = YOpaque },

    OP2 = case proplists:get_value(?yapp_list, OP) of
              undefined ->
                  [{?yapp_list,[Y]} | OP];
              YR ->
                  YR2 = insert_yapp_in_yapp_list(Y, YR),
                  lists:keyreplace(?yapp_list, 1, OP, {?yapp_list, YR2})
          end,
    SC#sconf{opaque = OP2}.

insert_yapp_in_yapp_list(#yapp{} = Y, []) -> 
    [Y];
insert_yapp_in_yapp_list(#yapp{urlpath= UP} = Y, [#yapp{urlpath=UP} | T])  ->
    [Y|T];
insert_yapp_in_yapp_list(Y, [H | T])  ->
    [ H | insert_yapp_in_yapp_list(Y,T) ].

%% @hidden
remove(SrvId, RegPath) ->
    {ok, Gconf, Sconfs} = yaws_api:getconf(),
    NewSconfs = remove_yapp_from_sconfgroups(SrvId, RegPath, Sconfs),
    yaws_api:setconf(Gconf, NewSconfs).

remove_yapp_from_sconfgroups(_SrvId, _RegPath, []) ->
    [];
remove_yapp_from_sconfgroups(SrvId, RegPath, [SCG|T]) ->
    [remove_yapp_from_sconfgroup(SrvId, RegPath,SCG) | 
     remove_yapp_from_sconfgroups(SrvId, RegPath, T)]. 

remove_yapp_from_sconfgroup(_SrvId, _RegPath, []) ->
    [];
remove_yapp_from_sconfgroup(SrvId, RegPath, [ H | T ]) ->
    case srv_id(H) of 
        SrvId ->
            [ remove_yapp_from_sconf(RegPath, H) | T ];
        _ ->
            [ H | remove_yapp_from_sconfgroup(SrvId, RegPath,  T ) ]
    end.

remove_yapp_from_sconf(RegPath, #sconf{opaque = OP} = SC) ->
    OP2 = case proplists:get_value(?yapp_list, OP) of
              undefined ->
                  OP;
              YR ->
                  YR2 = remove_yapp_from_yapp_list(RegPath, YR),
                  lists:keyreplace(?yapp_list, 1, OP, {?yapp_list, YR2})
          end,
    SC#sconf{opaque = OP2}.

remove_yapp_from_yapp_list(_, [] ) -> 
    [];
remove_yapp_from_yapp_list(RegPath, [ #yapp{urlpath = RegPath} | T ] ) -> 
    T;
remove_yapp_from_yapp_list(RegPath, [H | T]) ->
    [H | remove_yapp_from_yapp_list(RegPath, T)].

%% by tobbe@tornkvist.org
reset_yaws_conf() ->
    case catch yaws_config:load(yaws_sup:get_app_args()) of
        {ok, Gconf, Sconfs} ->
            yaws_api:setconf(Gconf, Sconfs);
        Err ->
            Err
    end.

%% @spec get_conf() -> {ok,yawsGconf(), Sconfs}
%% Sconfs = [ yawsSconf() ]
get_conf() ->
    yaws_api:getconf().
%    yaws_config:load(yaws_sup:get_app_args()).
    
%% @spec srv_id(Sconf) -> string() | undefined
%%  Sconf = yawsSconf()
%% @doc Get the server id from an Sconf if available
srv_id(#sconf{opaque = OP}) -> 
    proplists:get_value(?srv_id, OP).

%% @spec get_bootstrap_yapps() -> [{ ServerId, [ {Path, ApplicationName}]}]
%%   ServerId = string()
%%   Path = string()
%%   Applicationame = atom()
%% @doc Gets the Yapps defined in each opaque 
%% "bootstrap_yapps = appname1, appname2" for every server id. (If available). 
%% Bootstrap yapps will get the same pathname as their application name
%% and are "static" meaning that they can not be removed from the server
%% unless yaws.conf is changed (or if embedded yaws - yaws:setconf/2 is used).
get_bootstrap_yapps() ->

    {ok, _Gconf, Sconfs} = get_conf(),

    YL = [ {proplists:get_value(?srv_id, OP), 
       make_yapp_tuples(proplists:get_value(?bootstrap_yapps, OP))} 
      || #sconf{opaque=OP} <- lists:flatten(Sconfs) ],

    [{SrvId, Yapps} || 
        {SrvId, Yapps} <- YL, SrvId =/= undefined, Yapps =/= []].

                              

make_yapp_tuples(undefined) ->
    [];
make_yapp_tuples(BootStrapYapps) ->
    [ make_yapp_tuple(A) || A <- string:tokens(BootStrapYapps,",")].

make_yapp_tuple(A) ->
    B = string:strip(A),
    {"/" ++ B, list_to_atom(B)}. 

%% @spec get_yapps() -> [{ServId,[{yapp, Urlpath, Docroot, Appname , Appmods}]}]
%%  Urlpath = string()
%%  Docroot = string()
%%  Appname = atom()
%%  Appmods = [atom()]
%% @doc Gets all Yapps that are configured for the Yaws server.
get_yapps() ->
    {ok, _Gconf, Sconfs} = yaws_api:getconf(),
    Yapps1 = [{proplists:get_value("yapp_server_id", OP),
               proplists:get_value(yapp_list, OP)} || 
                 #sconf{opaque=OP} <- lists:flatten(Sconfs)],
    [{S,Y} || {S,Y} <- Yapps1, Y =/= undefined, S =/= undefined].

%% @spec get_server_ids() -> [string()]
%% @doc Lists all server ids.
get_server_ids() ->
    {ok, _Gconf, Sconfs} = get_conf(),
    SrvIds1 = [proplists:get_value("yapp_server_id", OP) ||
                  #sconf{opaque=OP} <- lists:flatten(Sconfs)],
    [S|| S <- SrvIds1,  S =/= undefined].

