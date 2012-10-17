-module(yaws_dav).
%%%-------------------------------------------------------------------
%%% Created : 15 May 2005 by Tobbet <tobbe@tornkvist.org>
%%% Modified: 21 Nov 2005 by <mbj@tail-f.com>
%%% Modified: 28 Jun 2012 by <tjeerd@yolt.nl>
%%% Desc.   : WebDav specifics.
%%%           RFC4918 class 1, 2, 3 compliant
%%%           To use, add a line dav = true in the <server>.
%%%
%%%-------------------------------------------------------------------
-export([lock/1, unlock/1, propfind/1, proppatch/1, delete/1, put/2,
         mkcol/1, move/1, copy/1]).

-include("../include/yaws_dav.hrl").
-include("../include/yaws_api.hrl").
-include("../include/yaws.hrl").
-include("yaws_debug.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/file.hrl").

%%------------------------------------------------------
%% methods
%%

lock(A) ->
    try
        Name = davname(A),
        Path = davpath(A),
        Locks = yaws_davlock:discover(Path),
        If = h_if(Name,A,Locks),
        verify_protected(Locks,If),
        R = case file:read_file_info(Path) of
                {ok, F} when (F#file_info.type == directory) or (F#file_info.type == regular) ->
                    #resource{ name = Name, info = F};
                {error,enoent} ->
                    case string:right(A#arg.server_path,1) of
                        "/" ->
                            ok = file:make_dir(Path);
                        _ ->
                            ok = file:write_file(Path,"")
                    end,
                    {ok, F} = file:read_file_info(Path),
                    #resource{ name = Name, info = F};
                {error,_} -> throw(409)
            end,
        Req = binary_to_list(A#arg.clidata),
        L = parse_lockinfo(Req),
        Id = h_locktoken(A),
        Timeout = h_timeout(A),
        Depth = h_depth(A),
        case yaws_davlock:lock(R#resource.name,L#davlock{id=Id,timeout=Timeout,depth=Depth}) of
            {ok,Id1} ->
                {200,Result} = prop_get({'DAV:',lockdiscovery},A,R),
                Response = [{'D:prop', [{'xmlns:D',"DAV:"}], [Result]}],
                status(200,[{"Lock-Token","<opaquelocktoken:"++Id1++">"}],Response);
            {error,locked} ->
                status(423);
            _ ->
                throw(501)
        end
    catch
        Status ->
            status(Status);
        _Error:Reason ->
            ?elog("unexpected error: ~p~n~p~n",[Reason,erlang:get_stacktrace()]),
            status(500,[{'D:error',[{'xmlns:D',"DAV:"}],[Reason]}])
    end.

unlock(A) ->
    try
        R = davresource0(A),
        Id = h_locktoken(A),
        yaws_davlock:unlock(R#resource.name,Id),
        status(204)
    catch
        Status ->
            status(Status);
        _Error:Reason ->
            ?elog("unexpected error: ~p~n~p~n",[Reason,erlang:get_stacktrace()]),
            status(500,[{'D:error',[{'xmlns:D',"DAV:"}],[Reason]}])
    end.


delete(A) ->
    try
        R = davresource0(A),
        case yaws_davlock:lock(R#resource.name,#davlock{depth=infinity,scope=exclusive}) of
            {ok,Id} ->
                rmrf(A#arg.docroot++R#resource.name),
                yaws_davlock:unlock(R#resource.name,Id);
            _ -> throw(423)
        end
    catch
        Status ->
            status(Status);
        _Error:Reason ->
            ?elog("unexpected error: ~p~n~p~n",[Reason,erlang:get_stacktrace()]),
            status(500,[{'D:error',[{'xmlns:D',"DAV:"}],[Reason]}])
    end.

put(SC, ARG) ->
    try
        Name = davname(ARG),
        FName = davpath(ARG),
        Locks = yaws_davlock:discover(Name),
        If = h_if(FName,ARG,Locks),
        verify_protected(Locks,If),
        IsDir = filelib:is_dir(FName),
        H = ARG#arg.headers,
        PPS = SC#sconf.partial_post_size,
        CT = case yaws:to_lower(H#headers.content_type) of
                 "multipart/form-data"++_ -> multipart;
                 _ -> urlencoded
             end,
        if
            IsDir-> throw(405);
            true -> ok
        end,
        SSL = yaws:is_ssl(SC),
        CliSock = case yaws_api:get_sslsocket(ARG#arg.clisock) of
                      {ok, SslSock} -> SslSock;
                      undefined -> ARG#arg.clisock
                  end,
        TmpName = temp_name(FName),
        case file:open(TmpName, [raw,write]) of
            {ok, Fd} ->
                case H#headers.content_length of
                    undefined ->
                        Chunked = yaws:to_lower(H#headers.transfer_encoding) == "chunked",
                        case H#headers.connection of
                            "close" when Chunked == false->
                                store_client_data(Fd, CliSock, all, SSL);
                            _ when Chunked == true ->
                                store_chunked_client_data(Fd, CliSock, SSL);
                            _ ->
                                %store_client_data(Fd, CliSock, all, SSL)
                                ok
                        end;
                    Len when is_integer(PPS) ->
                        Int_len = list_to_integer(Len),
                        if
                            Int_len == 0 ->
                                ok;
                            PPS < Int_len, CT == multipart ->
                                %% FIXME: handle this
                                %% {partial,
                                store_client_data(Fd,CliSock, PPS, SSL); % };
                            true ->
                                store_client_data(Fd, CliSock, Int_len, SSL)
                        end;
                    Len when PPS == nolimit ->
                        Int_len = list_to_integer(Len),
                        if
                            Int_len == 0 ->
                                ok;
                            true ->
                                store_client_data(Fd, CliSock, Int_len, SSL)
                        end
                end,
                file:close(Fd),
                case file:rename(TmpName, FName) of
                    ok ->
                        status(200);
                    _ ->
                        status(409)
                end;
            {error,eexist} -> throw(405);
            {error,enoent} -> throw(409);
            {error,eisdir} -> throw(409);
            {error,enospace} -> throw(507);
            _ -> status(500)
        end
    catch
        exit:normal -> exit(normal);
        Status ->
            status(Status);
        _Error:Reason ->
            ?elog("unexpected error: ~p~n~p~n",[Reason,erlang:get_stacktrace()]),
            status(500,[{'D:error',[{'xmlns:D',"DAV:"}],[Reason]}])
    end.

mkcol(A) ->
    Path = davpath(A),
    Name = davname(A),
    try
        Locks = yaws_davlock:discover(Name),
        If = h_if(Path,A,Locks),
        verify_protected(Locks,If),
        file_do(make_dir,[Path]),
        status(201)
    catch
        Status -> status(Status);
        Error:Reason ->
            ?elog("create directory ~p failed: ~p with reason ~p~n", [Path,Error,Reason]),
            status(500,[{'D:error',[{'xmlns:D',"DAV:"}],[Reason]}])
    end.

copy(A) ->
    copy_move(A, fun do_copy/2).

move(A) ->
    copy_move(A, fun do_move/2).

copy_move(A, OpF) ->
    From = davpath(A),
    try
        To = h_destination(A),
        Locks = yaws_davlock:discover(To),
        If = h_if(To,A,Locks),
        verify_protected(Locks,If),
        DoOverwrite = h_overwrite(A),
        ToExists = exists(To),
        if
            DoOverwrite == false, ToExists == true ->
                status(412);
            true  ->
                if ToExists == true ->
                        rmrf(To);
                   true ->
                        ok
                end,
                OpF(From, To)
        end
    catch
        Status -> status(Status);
        Error:Reason ->
            ?elog("copy/move ~p failed: ~p with reason ~p~n", [From,Error,Reason]),
            status(500,[{'D:error',[{'xmlns:D',"DAV:"}],[Reason]}])
    end.

do_move(From, To) ->
    case file:rename(From, To) of
        ok ->
            status(201);
        _ ->
            case file:copy(From, To) of
                {ok,_} ->
                    file:delete(From),
                    status(201);
                {error, Reason} ->
                    ?elog("move from ~p to ~p failed: ~p\n",
                          [From, To, Reason]),
                    status(409,[{'D:error', [{'xmlns:D',"DAV:"}],[Reason]}])
            end
    end.

do_copy(From, To) ->
    case file:copy(From, To) of
        {ok, _} ->
            status(201);
        {error, Reason} ->
            ?elog("move from ~p to ~p failed: ~p\n",
                  [From, To, Reason]),
            status(409)
    end.

exists(Path) ->
    case file:read_file_info(Path) of
        {ok, _} -> true;
        _ -> false
    end.

temp_name(F) ->
    {A,B,C} = erlang:now(),
    Path = filename:dirname(F),
    File = filename:basename(F),
    T0 = io_lib:format("~s/.~s.~p-~p-~p",[Path,File,A,B,C]),
    lists:flatten(T0).

propfind(A) ->
    try
        Req = binary_to_list(A#arg.clidata),
        Props = parse_propfind(Req),
        R = davresource0(A),
        case h_depth(A) of
            0 ->
                %?elog("PROPFIND ~p (Depth=0)~n", [R#resource.name]),
                Response = {'D:response', [], propfind_response(Props,A,R)},
                MultiStatus = [{'D:multistatus', [{'xmlns:D',"DAV:"}], [Response]}],
                status(207,MultiStatus);
            1 ->
                R1 = davresource1(A),
                %?elog("PROPFIND ~p (Depth=1, entries=~p)~n", [R#resource.name,length(R1)]),
                Response = {'D:response', [], propfind_response(Props,A,R)},
                Responses = [{'D:response', [], propfind_response(Props,A,Rx)} || Rx <- R1],
                MultiStatus = [{'D:multistatus', [{'xmlns:D',"DAV:"}], [Response|Responses]}],
                status(207,MultiStatus);
            infinity ->
                %?elog("PROPFIND ~p (Depth=infinity)~n", [R#resource.name]),
                Response = [{'D:error', [{'xmlns:D',"DAV:"}],[{'propfind-finite-depth',[],[]}]}],
                status(403,Response)
        end
    catch
        {Status,Precondition} ->
            Response1 = [{'D:error', [{'xmlns:D',"DAV:"}],[{Precondition,[],[]}]}],
            status(Status,Response1);
        Status ->
            status(Status);
        _Error:Reason ->
            ?elog("unexpected error: ~p~n~p~n",[Reason,erlang:get_stacktrace()]),
            status(500,[{'D:error',[{'xmlns:D',"DAV:"}],[Reason]}])
    end.

propfind_response(Props,A,R) ->
    Url = yaws_api:url_encode(R#resource.name),
    %Url = R#resource.name,
    case Props of
        [allprop] ->
            AllProp = [ prop_get(N,A,R) || N <- allprops(R) ],
            AllSorted = prop_sort(AllProp),
            {200, Results} = lists:keyfind(200,1,AllSorted),
            [{'D:href', [], [Url]},
             {'D:propstat', [], [
                {'D:prop', [], Results},{status, [],["HTTP/1.1 200 OK"]}
            ]}];
        [propname] ->
            Results = [ case NS of
                        'DAV:' -> {list_to_atom("D:"++atom_to_list(P)),[],[]};
                        _ -> {P,[{'xmlns',NS}],[]}
                        end
                      || {NS,P} <-allprops(R) ],
            [{'D:href', [], [Url]},
             {'D:propstat', [], [
                {'D:prop', [], Results},{status, [],["HTTP/1.1 200 OK"]}
            ]}];
        PropsRequested ->
            Results = [ prop_get(N,A,R) || {N,_} <- PropsRequested ],
            ResultsSorted = prop_sort(Results),
            [{'D:href', [], [Url]}|
             [{'D:propstat', [], [
                {'D:prop', [], PropsFound},prop_status(Status)
             ]} || {Status,PropsFound} <- ResultsSorted ]
            ]
    end.

proppatch(A) ->
    try
        Req = binary_to_list(A#arg.clidata),
        R = davresource0(A),
        Update = parse_proppatch(Req),
        Response = {'D:response',[],proppatch_response(Update,A,R)},
        MultiStatus = [{'D:multistatus', [{'xmlns:D',"DAV:"}], [Response]}],
        status(207,MultiStatus)
    catch
        {Status,Precondition} ->
            Response1 = [{'D:error', [{'xmlns:D',"DAV:"}],[{Precondition,[],[]}]}],
            status(Status,Response1);
        Status ->
            status(Status);
        _Error:Reason ->
            ?elog("unexpected error: ~p~n~p~n",[Reason,erlang:get_stacktrace()]),
            status(500,[{'D:error',[{'xmlns:D',"DAV:"}],[Reason]}])
    end.

proppatch_response(Update,A,R) ->
    Url = yaws_api:url_encode(R#resource.name),
    %Url = R#resource.name,
    Results = proppatch_response(Update,A,R,[]),
    ResultsSorted = prop_sort(lists:flatten(Results)),
    [{'D:href', [], [Url]}|
     [{'D:propstat', [], [
        {'D:prop', [], PropsFound},prop_status(Status)
     ]} || {Status,PropsFound} <- ResultsSorted ]
    ].
proppatch_response([H|T],A,R,Results) ->
    Result = case H of
                 {set,Props} -> [ prop_set(P,A,R,V) || {P,V} <- Props];
                 {remove,Props} -> [ prop_remove(P,A,R) || {P,_V} <- Props]
             end,
    proppatch_response(T,A,R,[Result|Results]);
proppatch_response([],_A,_R,Results) ->
    Results.

prop_sort(L) -> prop_sort(L,[]).
prop_sort([H|T],R) ->
    {Status,Prop} = H,
    R1 = case lists:keyfind(Status,1,R) of
        {Status, Props} -> lists:keystore(Status,1,R,{Status,[Prop|Props]});
        false -> lists:keystore(Status,1,R,{Status,[Prop]})
    end,
    prop_sort(T,R1);
prop_sort([],R) -> R.


prop_status(Status) ->
    {'D:status',[],["HTTP/1.1 " ++ integer_to_list(Status) ++ " " ++
                        yaws_api:code_to_phrase(Status)]}.

%----------------------------------------------------
% Available props include namespace
% Available props can differ per resource
% For proposed Microsoft extensions see: draft-hopmann-collection-props-00.txt
%
allprops(R) ->
    F = R#resource.info,
    P1 = case F#file_info.type of
          directory -> [
            {'DAV:',childcount} % Microsoft extension
                ];
          _ -> [
            {'http://apache.org/dav/props/',executable} % Apache extension
                ]
        end,
    P2 = [
           %{'http://yaws.hyber.org/',access},  % sample Yaws extension
            {'DAV:',creationdate},
            {'DAV:',displayname},
           %{'DAV:',getcontentlanguage},        % not supported in GET
                                                % so omitted here as well
            {'DAV:',getcontentlength},
            {'DAV:',getcontenttype},
            {'DAV:',getetag},
            {'DAV:',getlastmodified},
            {'DAV:',isfolder}, % Microsoft extension
            {'DAV:',ishidden}, % Microsoft extension
            {'DAV:',lockdiscovery}, % class 2 compliancy
           %{'DAV:','quota-avialable-bytes'} % RFC4331
           %{'DAV:','quota-used-bytes'} % RFC4331
            {'DAV:',resourcetype},
            {'DAV:',supportedlock} % class 2 compliancy
        ],
    P1 ++ P2.

prop_get({'http://yaws.hyber.org/',access},_A,R) ->
    F = R#resource.info,
    A = F#file_info.access,
    P = {access, [{xmlns,'http://yaws.hyber.org/'}], [atom_to_list(A)]},
    {200, P};
prop_get({'DAV:',childcount},A,_R) ->
    Path=davpath(A),
    L = case file:list_dir(Path) of
            {ok, Files} -> length(Files);
            _ -> 0
        end,
    P = {'D:childcount', [], [integer_to_list(L)]},
    {200, P};
prop_get({'DAV:',creationdate},_A,R) ->
    F = R#resource.info,
    D = F#file_info.ctime,
    T = yaws:local_time_as_gmt_string(D),
    P = {'D:creationdate', [], [lists:flatten(T)]},
    {200, P};
prop_get({'DAV:',displayname},_A,R) ->
    Name = filename:basename(R#resource.name),
    P = {'D:displayname', [], [Name]},
    {200, P};
prop_get({'http://apache.org/dav/props/',executable},_A,R) ->
    F = R#resource.info,
    case F#file_info.type of
        directory -> {404,{executable, [{'xmlns',"http://apache.org/dav/props/"}], []}};
        _ -> {200, {executable, [{'xmlns',"http://apache.org/dav/props/"}], ["F"]}}
    end;
prop_get({'DAV:',getcontentlanguage},_A,_R) ->
    P = {'D:getcontentlanguage', [], []},
    {200, P};
prop_get({'DAV:',getcontentlength},_A,R) ->
    F = R#resource.info,
    P = {'D:getcontentlength', [], [integer_to_list(F#file_info.size)]},
    {200, P};
prop_get({'DAV:',getcontenttype},_A,R) ->
    F = R#resource.info,
    Mediatype = case F#file_info.type of
          directory ->
              "text/html"; % this represents the mediatype of a GET on a collection
          _ ->
              Name = R#resource.name,
              Ext = filename:extension(Name),
              Ext1 = case Ext of
                         [] -> "";
                         _ -> tl(Ext)
                     end,
              {_Kind,Mimetype} = mime_types:t(Ext1),
              Mimetype
        end,
    P = {'D:getcontenttype', [], [Mediatype]},
    {200, P};
prop_get({'DAV:',getetag},_A,R) ->
    F = R#resource.info,
    E = yaws:make_etag(F),
    P = {'D:getetag', [], [E]},
    {200, P};
prop_get({'DAV:',getlastmodified},_A,R) ->
    F = R#resource.info,
    D = F#file_info.mtime,
    T = yaws:local_time_as_gmt_string(D),
    P = {'D:getlastmodified', [], [lists:flatten(T)]},
    {200, P};
prop_get({'DAV:',isfolder},_A,R) ->
    F = R#resource.info,
    D = case F#file_info.type of
            directory -> "1";
            _ -> "0"
        end,
    P = {'D:isfolder', [], [D]},
    {200, P};
prop_get({'DAV:',ishidden},_A,R) ->
    N = filename:basename(R#resource.name),
    H = case N of
            "."++_Rest -> "1"; % dotted file
            _ -> "0"
        end,
    P = {'D:ishidden', [], [H]},
    {200, P};
prop_get({'DAV:',resourcetype},_A,R) ->
    F = R#resource.info,
    P = case F#file_info.type of
            directory -> {'D:resourcetype', [], [{'D:collection',[],[]}]};
            _ -> {'D:resourcetype', [], []}
        end,
    {200, P};
prop_get({'DAV:',lockdiscovery},_A,R) ->
    Name = R#resource.name,
    Locks = yaws_davlock:discover(Name),
    case Locks of
        [] ->
            {404,{'D:lockdiscovery',[],[]}};
        _ ->
            ActiveLocks = [
                        {'D:activelock',[],[
                            {'D:lockscope',[],[prop_get_format(scope,Lock#davlock.scope)]},
                            {'D:locktype',[],[prop_get_format(type,Lock#davlock.type)]},
                            {'D:depth',[],[prop_get_format(depth,Lock#davlock.depth)]},
                            %{'D:owner',[],[prop_get_format(owner,Lock#davlock.owner)]}, % kept secret
                            {'D:timeout',[],[prop_get_format(timeout,Lock#davlock.timeout)]},
                            {'D:locktoken',[],[prop_get_format(locktoken,Lock#davlock.id)]},
                            {'D:lockroot',[],[prop_get_format(lockroot,Lock#davlock.path)]}
                        ]}
                    || Lock <- Locks ],
            {200, {'D:lockdiscovery',[],ActiveLocks}}
    end;
prop_get({'DAV:',supportedlock},_A,_R) ->
    P = {'D:supportedlock',[],[
            {'D:lockentry',[],[
                {'D:lockscope',[],[{'D:exclusive',[],[]}]},
                {'D:locktype',[],[{'D:write',[],[]}]}
            ]},
            {'D:lockentry',[],[
                {'D:lockscope',[],[{'D:shared',[],[]}]},
                {'D:locktype',[],[{'D:write',[],[]}]}
            ]}
        ]},
    {200, P};
prop_get({NS,P},_A,_R) ->
    {404,{P,[{'xmlns',NS}],[]}}.


prop_set({'DAV:',creationdate},A,_R,V) ->
    Path=davpath(A),
    P = {'D:creationdate', [], []},
    case file:read_file_info(Path) of
        {ok,F0} ->
            T = yaws:stringdate_to_datetime(V),
            F1 = F0#file_info{ctime=T},
            case file:write_file_info(Path,F1) of
                ok ->
                    {200, P};
                {error,_} ->
                    {409, P}
            end;
        {error,_} ->
            {409, P}
    end;
prop_set({'DAV:',getlastmodified},A,_R,V) ->
    Path=davpath(A),
    P = {'D:getlastmodified', [], []},
    case file:read_file_info(Path) of
        {ok,F0} ->
            T = yaws:stringdate_to_datetime(V),
            F1 = F0#file_info{mtime=T},
            case file:write_file_info(Path,F1) of
                ok ->
                    {200, P};
                {error,_} ->
                    {409, P}
            end;
        {error,_} ->
            {409, P}
    end;
prop_set({'DAV:',getetag},_A,_R,_V) ->
    {403,{'D:getetag',[],[{'cannot-modify-protected-property',[],[]}]}};
prop_set({'DAV:',lockdiscovery},_A,_R,_V) ->
    {403,{'D:lockdiscovery',[],[{'cannot-modify-protected-property',[],[]}]}};
prop_set({'DAV:',resourcetype},_A,_R,_V) ->
    {403,{'D:resourcetype',[],[{'cannot-modify-protected-property',[],[]}]}};
prop_set({NS,P},_A,_R,_V) ->
    {404,{P,[{'xmlns',NS}],[]}}.

prop_remove({P,NS},_A,_R) ->
    {403,{P,[{'xmlns',NS}],[]}}.


prop_get_format(type,write) ->
    {'D:write',[],[]};
prop_get_format(scope,exclusive) ->
    {'D:exclusive',[],[]};
prop_get_format(scope,_) ->
    {'D:shared',[],[]};
prop_get_format(depth,infinity) ->
    "infinity";
prop_get_format(depth,Depth) ->
    integer_to_list(Depth);
prop_get_format(timeout,Timeout) ->
    lists:flatten(io_lib:format("Second-~p",[Timeout]));
prop_get_format(locktoken,Id) ->
    {'D:href',[],["opaquelocktoken:"++Id]};
prop_get_format(lockroot,Ref) ->
    {'D:href',[],[Ref]};
prop_get_format(owner,Owner) ->
    Owner;
prop_get_format(_,_) ->
    throw(500).

%% --------------------------------------------------------
%% Resource mapping
%%

davname(A) ->
    A#arg.server_path.

davpath(A) ->
    A#arg.docroot ++ A#arg.server_path.

davroot(A) ->
    Method = case yaws_api:get_sslsocket(A#arg.clisock) of
                 {ok, _SslSock} -> "https";
                 undefined -> "http"
             end,
    Host = (A#arg.headers)#headers.host,
    Method ++ "://" ++ Host.

%% davresource0/1 - get resources with depth 0
davresource0(A) ->
    Name = davname(A),
    Path = davpath(A),
    case file:read_file_info(Path) of
        {ok, F} when (F#file_info.type == directory) or (F#file_info.type == regular) ->
            #resource{ name = Name, info = F};
        {error,_} -> throw(404)
    end.
%% davresource1/1 - get additional resources for depth 1
davresource1(A) ->
    Coll = davname(A),
    Path = davpath(A),
    case file:read_file_info(Path) of
        {ok, Dir} when Dir#file_info.type == directory ->
            {ok, L} = file:list_dir(Path),
            davresource1(A,Path,Coll,L,[]);
        {ok, _Else} ->
            []
    end.
davresource1(_A,_Path,_Coll,[],Result) ->
    Result;
davresource1(_A,Path,Coll,[Name|Rest],Result) ->
    File = filename:join(Path,Name),
    Ref = filename:join(Coll,Name),
    {ok, Info} = file:read_file_info(File),
    if
        (Info#file_info.type == regular) or (Info#file_info.type == directory) ->
            Resource = #resource {name = Ref, info = Info},
            davresource1(_A,Path,Coll,Rest,[Resource|Result]);
        true ->
            davresource1(_A,Path,Coll,Rest,Result)
    end.

%% --------------------------------------------------------
%% Check if resource is protected by locks
%% is_protected(Locks,If) -> true|false

verify_protected(Locks,false) when length(Locks)>0 -> throw(412);
verify_protected(Locks,undefined) when length(Locks)>0 -> throw(423);
verify_protected(_Lock,_If) -> ok.

%% --------------------------------------------------------
%% Parse additional HTTP headers
%%

h_depth(A) ->
    Hs = (A#arg.headers)#headers.other,
    case lists:keysearch("Depth", 3, Hs) of
        {value, {_,_,"Depth",_,Depth}} ->
            h_depth_interpret(Depth);
        _ ->
            infinity
    end.
h_depth_interpret("infinity") -> infinity;
h_depth_interpret("1") -> 1;
h_depth_interpret(_) -> 0.

h_destination(A) ->
    Hs = (A#arg.headers)#headers.other,
    case lists:keysearch("Destination", 3, Hs) of
        {value, {http_header,_,_,_,Dest}} ->
            Url = yaws_api:parse_url(Dest),
            {Path,_} = yaws_api:url_decode_q_split(Url#url.path),
            A#arg.docroot ++ "/" ++ Path;
        _ ->
            throw(501)
    end.

h_overwrite(A) ->
    Hs = (A#arg.headers)#headers.other,
    case lists:keysearch("Overwrite", 3, Hs) of
        {value, {http_header, _ , _, _, "T"}} ->
            true;
        _ ->
            false
    end.

h_timeout(A) ->
    Hs = (A#arg.headers)#headers.other,
    case lists:keysearch("Timeout", 3, Hs) of
        {value, {_,_,"Timeout",_,T}} ->
            case T of
                "Second-"++TimeoutVal ->
                    Val = case catch list_to_integer(TimeoutVal) of
                              I when is_integer(I) -> I;
                              _ -> ?LOCK_LIFETIME
                          end,
                    min(Val,?LOCK_LIFETIME);
                _ -> ?LOCK_LIFETIME
            end;
        _ ->
            ?LOCK_LIFETIME
    end.

h_locktoken(A) ->
    Hs = (A#arg.headers)#headers.other,
    case lists:keysearch("Lock-Token", 3, Hs) of
        {value, {_,_,"Lock-Token",_,URL}} ->
            case URL of
                "<opaquelocktoken:"++Token -> string:left(Token,36);
                _ -> URL
            end;
        _ ->
            undefined
    end.

h_if(_Path,A,Locks) ->
    Hs = (A#arg.headers)#headers.other,
    case lists:keysearch("If", 3, Hs) of
        {value, {_,_,"If",_,If}} ->
            List = if_parse(If,untagged),
            Q = if_eval(A,Locks,List),
            %?elog("If-header ~p evaluated to ~p~n",[List,Q]),
            Q;
        _ ->
            undefined
    end.

if_parse([],_Resource) ->
    [];
if_parse(Line,Resource) when hd(Line)==32 ->
    if_parse(tl(Line),Resource);
if_parse(Line,untagged) when hd(Line)==60 -> % <
    {Url,Rest} = if_parse_token(tl(Line),""),
    if_parse(Rest,Url);
if_parse(Line,Resource) when hd(Line)==40 -> % (
    {Condition,Rest} = if_parse_condition(tl(Line),[],true),
    [{Resource,Condition}|if_parse(Rest,untagged)].

if_parse_condition(Line,List,_Bool) when hd(Line)==41 -> % )
    {List,tl(Line)};
if_parse_condition(Line,List,Bool) when hd(Line)==32 -> % whitespace
    if_parse_condition(tl(Line),List,Bool);
if_parse_condition("Not"++Line,List,_Bool) -> % negate
    if_parse_condition(tl(Line),List,false);
if_parse_condition(Line,List,Bool) when hd(Line)==60 -> % <
    {Token,Rest} = if_parse_token(tl(Line),""),
    if_parse_condition(Rest,[{Bool,state,Token}|List],true);
if_parse_condition(Line,List,Bool) when hd(Line)==91 -> % [
    {Etag,Rest} = if_parse_etag(tl(Line),""),
    if_parse_condition(Rest,[{Bool,etag,Etag}|List],true).

if_parse_token(Line,Buffer) when hd(Line)==62 -> % >
    Uri = lists:reverse(Buffer),
    Token1 = case Uri of
                 "opaquelocktoken:"++Token -> Token;
                 _ -> Uri
             end,
    {Token1,tl(Line)};
if_parse_token([H|T],Buffer) ->
    if_parse_token(T,[H|Buffer]).

if_parse_etag(Line,Buffer) when hd(Line)==93 -> % ]
    {lists:reverse(Buffer),tl(Line)};
if_parse_etag([H|T],Buffer) ->
    if_parse_etag(T,[H|Buffer]).

%% if_eval(A,RequestPath,Conditions)
if_eval(_A,_Locks,[]) ->
    false;
if_eval(A,Locks,[{Resource,AndList}|More]) ->
    Target = case Resource of
                 untagged -> davname(A);
                 _ -> Resource -- davroot(A)
             end,
    if_eval_condition(AndList,A,Target,Locks) orelse if_eval(A,Locks,More).

if_eval_condition(AndList,A,Target,Locks) ->
    if_eval_condition(AndList,true,false,A,Target,Locks).

if_eval_condition([],Result,Valid,_A,_Target,_Locks) ->
    Result and Valid;
if_eval_condition([{false,Kind,Ref}|T],Result,Valid,A,Target,Locks) ->
    not if_eval_condition([{true,Kind,Ref}|T],Result,Valid,A,Target,Locks);
if_eval_condition([{true,state,Ref}|T],_Result,_Valid,A,Target,Locks) ->
    Result1 = if_eval_locktoken(Target,Ref,Locks),
    Valid1 = true,
    Result1 andalso if_eval_condition(T,Result1,Valid1,A,Target,Locks);
if_eval_condition([{true,etag,Ref}|T],_Result,Valid,A,Target,Locks) ->
    F = file:read_info(A#arg.docroot++Target),
    E = yaws:make_etag(F),
    Result1 = (E==Ref),
    Valid1 = Valid,
    Result1 andalso if_eval_condition(T,Result1,Valid1,A,Target,Locks).

%% if_eval_locktoken(Target,Token,Locktokens) -> true|false
if_eval_locktoken(_Target,_Token,[]) ->
    false;
if_eval_locktoken(Target,Token,[H|T]) ->
    ((H#davlock.path == Target) and (H#davlock.id == Token)) orelse if_eval_locktoken(Target,Token,T).


%% --------------------------------------------------------
%% XML elements of RFC4918
%%
%% activelock
-define(IS_ALLPROP(X), #xmlElement{expanded_name = {'DAV:',allprop}} = X).
%% collection
%% depth
%% error
-define(IS_EXCLUSIVE(X), #xmlElement{expanded_name = {'DAV:',exclusive}} = X).
-define(IS_HREF(X), #xmlElement{expanded_name = {'DAV:',href}} = X).
%% include % TODO: add this tag
%% location
%% lockentry
-define(IS_LOCKINFO(X), #xmlElement{expanded_name = {'DAV:',lockinfo}} = X).
%% lockroot
-define(IS_LOCKSCOPE(X), #xmlElement{expanded_name = {'DAV:',lockscope}} = X).
%% locktoken
-define(IS_LOCKTYPE(X), #xmlElement{expanded_name = {'DAV:',locktype}} = X).
%% multistatus
-define(IS_OWNER(X), #xmlElement{expanded_name = {'DAV:',owner}} = X).
-define(IS_PROP(X), #xmlElement{expanded_name = {'DAV:',prop}} = X).
-define(IS_PROPERTYUPDATE(X), #xmlElement{expanded_name = {'DAV:',propertyupdate}} = X).
-define(IS_PROPFIND(X), #xmlElement{expanded_name = {'DAV:',propfind}} = X).
-define(IS_PROPNAME(X), #xmlElement{expanded_name = {'DAV:',propname}} = X).
%% propstat
-define(IS_REMOVE(X), #xmlElement{expanded_name = {'DAV:',remove}} = X).
%% response
%% responsedescription
-define(IS_SET(X), #xmlElement{expanded_name = {'DAV:',set}} = X).
-define(IS_SHARED(X), #xmlElement{expanded_name = {'DAV:',shared}} = X).
%% status
%% timeout
-define(IS_WRITE(X), #xmlElement{expanded_name = {'DAV:',write}} = X).

-define(CONTENT(X), X#xmlElement.content).

%% Parameter is always list
parse_propfind([]) -> [allprop]; % RFC4918: no body then allprop, is [] no body?
parse_propfind(L) ->
    case catch xmerl_scan:string(L, [{namespace_conformant, true}]) of
        {?IS_PROPFIND(X),_} ->
            parse_propfind(?CONTENT(X),[]);
        _Z ->
            throw(400)
    end.
parse_propfind([?IS_PROPNAME(_H)|_T], _R) ->
    [propname];
parse_propfind([?IS_ALLPROP(_H)|_T], _R) ->
    [allprop]; %% TODO add include tag
parse_propfind([?IS_PROP(H)|T], _R) ->
    Props = parse_prop(?CONTENT(H)),
    parse_propfind(T, Props);
parse_propfind([_H|T], R) ->
    %% skip #xmlText, #xmlComment, etc.
    parse_propfind(T, R);
parse_propfind([], R) ->
    R.

parse_proppatch(L) ->
    case catch xmerl_scan:string(L, [{namespace_conformant, true}]) of
        {?IS_PROPERTYUPDATE(X),_} ->
            parse_proppatch(?CONTENT(X),[]);
        _Z ->
            throw(400)
    end.
parse_proppatch([?IS_SET(H)|T],R) ->
    Props = parse_setremove(?CONTENT(H)),
    parse_proppatch(T,[{set,Props}|R]);
parse_proppatch([?IS_REMOVE(H)|T],R) ->
    Props = parse_setremove(?CONTENT(H)),
    parse_proppatch(T,[{remove,Props}|R]);
parse_proppatch([_H|T], R) ->
    %% skip #xmlText, #xmlComment, etc.
    parse_proppatch(T, R);
parse_proppatch([],R) ->
    lists:reverse(R). % MUST proces in document order

parse_setremove([?IS_PROP(X)]) ->
    parse_prop(?CONTENT(X)).

parse_prop(L) ->
    parse_prop(L, []).

parse_prop([H|T],L) ->
    case H of
        H when is_record(H,xmlElement) ->
            Value = case H#xmlElement.content of
                        [C] when is_record(C,xmlText) -> C#xmlText.value;
                        _ -> ""
                    end,
            parse_prop(T,[{H#xmlElement.expanded_name,Value}|L]);
        _ ->
            parse_prop(T,L)
    end;
parse_prop([], L) ->
    lists:reverse(L).  % preserve order for PROPPATCH

parse_lockinfo(L) ->
    case catch xmerl_scan:string(L, [{namespace_conformant, true}]) of
        {?IS_LOCKINFO(X),_} ->
            parse_lockinfo(?CONTENT(X),#davlock{});
        _Z ->
            throw(400)
    end.
parse_lockinfo([?IS_LOCKSCOPE(H)|T], D) ->
    X = parse_lockscope(?CONTENT(H)),
    parse_lockinfo(T,D#davlock{scope=X});
parse_lockinfo([?IS_LOCKTYPE(H)|T], D) ->
    X = parse_locktype(?CONTENT(H)),
    parse_lockinfo(T,D#davlock{type=X});
parse_lockinfo([?IS_OWNER(H)|T], D) ->
    X = parse_owner(?CONTENT(H)),
    parse_lockinfo(T,D#davlock{owner=X});
parse_lockinfo([_H|T],D) ->
    parse_lockinfo(T,D); % skip spaces and comments, etc.
parse_lockinfo([], D) ->
    D.

parse_lockscope([?IS_EXCLUSIVE(_H)|_T]) ->
    exclusive;
parse_lockscope([?IS_SHARED(_H)|_T]) ->
    shared;
parse_lockscope(_X) ->
    throw(400).

parse_locktype([?IS_WRITE(_H)|_T]) ->
    write;
parse_locktype(_) ->
    throw(400).

parse_owner(X) ->
    Xml = xmerl:export_simple_content(X,xmerl_xml),
    lists:flatten(Xml).

%% --------------------------------------------------------
%% Status output
%%

status(Status) ->
    [{status, Status}].
status(Status,Response) ->
    Xml = xml_expand(Response),
    [{status, Status},
     {content, "application/xml; charset=\"utf-8\"", Xml}
    ].
status(Status,Headers,Response) ->
    Xml = xml_expand(Response),
    Hdrs = [ {header,H} || H <- Headers],
    [{status, Status} | Hdrs] ++ [{content, "application/xml; charset=\"utf-8\"", Xml}].

xml_expand(L) ->
    xml_expand(L, "utf-8").

xml_expand(L, Cset) ->
    Prolog = ["<?xml version=\"1.0\" encoding=\""++Cset++"\" ?>"],
    Xml = xmerl:export_simple(L,xmerl_xml,[{prolog,Prolog}]),
    % MS requires \r\n at end of every XML response
    [Xml|"\r\n"].

%% --------------------------------------------------------
%% File functions
%%

file_do(Func,Params) ->
    Result = erlang:apply(file,Func,Params),
    case Result of
        ok -> ok;
        {ok,X} -> {ok,X};
        {ok,X1,X2} -> {ok,X1,X2};
        eof -> eof;
        {error,eexist} -> throw(405);
        {error,enoent} -> throw(409);
        {error,eisdir} -> throw(409);
        {error,enospace} -> throw(507);
        _Error -> ?elog("file function returned ~p~n",[_Error]),throw(500)
    end.

rmrf(Path) ->
    {ok, F} = file_do(read_file_info,[Path]),
    case F#file_info.type of
        directory ->
            {ok, Dir} = file_do(list_dir,[Path]),
            [ rmrf(filename:join(Path,File)) || File <- Dir ],
            file_do(del_dir,[Path]);
        _ ->
            file:delete(Path)
    end.


store_client_data(Fd, CliSock, all, SSlBool) ->
    store_client_data_all(Fd, CliSock, SSlBool);

store_client_data(Fd, CliSock, Len, SSlBool) ->
    store_client_data_len(Fd, CliSock, Len, SSlBool).


%% not nice to support this for ssl sockets
store_chunked_client_data(Fd, CliSock, SSL) ->
    yaws:setopts(CliSock, [binary, {packet, line}], SSL),
    %% Ignore chunk extentions
    {N, _Exts} = yaws:get_chunk_header(CliSock, SSL),
    yaws:setopts(CliSock, [binary, {packet, raw}], SSL),
    if
        N == 0 ->
            %% Ignore chunk trailer
            yaws:get_chunk_trailer(CliSock, SSL),
            ok;
        true ->
            B = yaws:get_chunk(CliSock, N, 0,SSL),
            yaws:eat_crnl(CliSock,SSL),
            ok = file:write(Fd, B),
            store_chunked_client_data(Fd, CliSock, SSL)
    end.

-define(MAX_PART, 65536).

store_client_data_len(_Fd, _CliSock, 0, _SSlBool) ->
    ok;
store_client_data_len(Fd, CliSock, Len, SSlBool) ->
    L = if Len<?MAX_PART -> Len; true -> ?MAX_PART end,
    case yaws:cli_recv(CliSock, L, SSlBool) of
        {ok, B} ->
            ok = file:write(Fd, B),
            store_client_data_len(Fd, CliSock, Len-size(B), SSlBool);
        _Other ->
            exit(normal)
    end.

store_client_data_all(Fd, CliSock, SSlBool) ->
    case yaws:cli_recv(CliSock, 4000, SSlBool) of
        {ok, B} ->
            ok = file:write(Fd, B),
            store_client_data_all(Fd, CliSock, SSlBool);
        eof ->
            ok;
        _Other ->
            ?Debug("store_client_data_all: ~p~n", [_Other]),
            exit(normal)
    end.
