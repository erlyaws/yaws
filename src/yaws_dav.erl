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
-export([propfind/1, proppatch/1, delete/1, put/2, mkcol/1, move/1, copy/1]).
-compile(export_all).

-include("../include/yaws_dav.hrl").
-include("../include/yaws_api.hrl").
-include("../include/yaws.hrl").
-include("yaws_debug.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/file.hrl").

%%------------------------------------------------------
%% from where to call init?
%%

init() ->
    %% lock table must be ordered to check parent/child locks
    ets:new(davlocks,[ordered_set,named_table]).

%%------------------------------------------------------
%% methods
%%

lock(A) ->
    try
        R = davresource0(A),
        Req = binary_to_list(A#arg.clidata),
        L = parse_lockinfo(Req),
        Id = h_locktoken(A),
        _If = h_if(A),
        Timeout = h_timeout(A),
        Depth = h_depth(A),
        L1 = L#davlock{id=Id,timeout=Timeout,depth=Depth},
        Rx = yaws_davlock:lock(R#resource.name,L1),
        %%case yaws_davlock:lock(R#resource.name,L#davlock{id=Id,timeout=Timeout,depth=Depth}) of
        case Rx of
            {ok,Id1} ->
                {200,Result} = prop_get({'DAV:',lockdiscovery},A,R),
                Response = [{'D:prop', [{'xmlns:D',"DAV:"}], [Result]}],
                status(200,[{"Lock-Token","<urn:uuid:"++Id1++">"}],Response);
            {error,locked} ->
                status(423);
            _ ->
                throw(501)
        end
    catch
        Status ->
            case Status of
                404 ->
                    status(501); % not implemented
                    %% TODO create and lock the resource
                _ ->
                    ?elog("Status: ~p~n~p~n",[Status,erlang:get_stacktrace()]),
                    status(Status)
            end;
        _Error:Reason ->
            ?elog("Unexpected error: ~p~n~p~n",[Reason,erlang:get_stacktrace()]),
            status(500,[{'D:error',[{'xmlns:D',"DAV:"}],[Reason]}])
    end.

unlock(A) ->
    try
        R = davresource0(A),
        Id = h_locktoken(A),
        _If = h_if(A),
        case yaws_davlock:unlock(R#resource.name,Id) of
            ok ->
                status(204)
        end
    catch
        Status -> status(Status);
        _Error:Reason ->
            ?elog("Unexpected error: ~p~n~p~n",[Reason,erlang:get_stacktrace()]),
            status(500,[{'D:error',[{'xmlns:D',"DAV:"}],[Reason]}])
    end.


delete(A) ->
    Path = davpath(A),
    ?elog("DELETE Path=~p~n", [Path]),
    case rmrf(Path) of
        ok -> status(200);
        _  -> status(403)
    end.

put(SC, ARG) ->
    %% FIXME Check if allowed to PUT this resource
    H = ARG#arg.headers,
    PPS = SC#sconf.partial_post_size,
    CT =
        case yaws:to_lower(H#headers.content_type) of
            "multipart/form-data"++_ -> multipart;
            _ -> urlencoded
        end,
    SSL = yaws:is_ssl(SC),
    FName = davpath(ARG),
    CliSock = ARG#arg.clisock,
    TmpName = FName ++ ".tmp",
    %% FIXME: first check if we can write to original file??
    case file:open(TmpName, [raw,write]) of
        {ok, Fd} ->
            try
                case H#headers.content_length of
                    undefined ->
                        Chunked = H#headers.transfer_encoding == "chunked",
                        case H#headers.connection of
                            "close" when Chunked == false->
                                store_client_data(Fd, CliSock, all, SSL);
                            _ when Chunked == true ->
                                store_chunked_client_data(Fd, CliSock, SSL);
                            _ ->
                                store_client_data(Fd, CliSock, all, SSL)
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
                    Error ->
                        throw(Error)
                        %% FIXME status(409)?
                end
            catch
                _:_Err ->
                    ?Debug("PUT error ~p\n", [_Err, TmpName]),
                    file:close(Fd),
                    file:delete(TmpName),
                    status(409)
            end;
        _Error ->
            ?Debug("PUT error ~p ~p\n", [_Error, TmpName]),
            status(409)
    end.

mkcol(A) ->
    Path = davpath(A),
    try
        file_do(make_dir,[Path]),
        ?elog("MKCOL ~p~n", [Path]),
        status(201)
    catch
        Status -> status(Status);
        Error:Reason ->
            ?elog("Create dir ~p failed: ~p with reason ~p~n", [Path,Error,Reason]),
            status(500,[{'D:error',[{'xmlns:D',"DAV:"}],[Reason]}])
    end.

copy(A) ->
    copy_move(A, fun do_copy/2).

move(A) ->
    copy_move(A, fun do_move/2).

copy_move(A, OpF) ->
    case lists:keysearch("Destination", 3, (A#arg.headers)#headers.other) of
        {value, {http_header, _, _, _, Url}} ->
            %% FIXME: check for weird paths
            {Url1, _} = yaws_api:url_decode_q_split(Url),
            Path = Url1 -- davroot(A),
            From = davpath(A),
            To = A#arg.docroot ++ "/" ++ Path,
            ?elog("move from ~p to ~p (~p)\n", [From, To, Url]),
            DoOverwrite = get_overwrite(A),
            IsSame = is_same(From, To),
            ToExsist = exists(To),
            if IsSame == true ->
                    status(403);
               DoOverwrite == false,
               ToExsist == true ->
                    status(412);
               true ->
                    if DoOverwrite == true ->
                            rmrf(To);
                       true ->
                            ok
                    end,
                    OpF(From, To)
            end;
        _ ->
            status(501)
    end.

do_move(From, To) ->
    case file:rename(From, To) of
        ok ->
            status(201);
        _ ->
            case file:copy(From, To) of
                {ok,_} ->
                    ok = file:delete(From),
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
        Error ->
            ?elog("move from ~p to ~p failed: ~p\n",
                  [From, To, Error]),
            status(409)
    end.

get_name("/") -> "/";
get_name("")  -> "/";
get_name(L) ->
    [Rname|_] = string:tokens(lists:reverse(L), "/"),
    lists:reverse(Rname).

file_name("/") -> ".";
file_name("")  -> ".";
file_name(L) ->
    [Rname|_] = string:tokens(lists:reverse(L), "/"),
    lists:reverse(Rname).

get_overwrite(A) ->
    case lists:keysearch("Overwrite", 3, (A#arg.headers)#headers.other) of
        {value, {http_header, _, _, _, "T"}} -> true;
        _ -> false
    end.

exists(Path) ->
    case file:read_file_info(Path) of
        {ok, _} -> true;
        _ -> false
    end.

%% FIXME: how to do this in a portable way?  on unix we could check inode...
is_same(A, B) ->
    A == B.

propfind(A) ->
    try
        Req = binary_to_list(A#arg.clidata),
        Props = parse_propfind(Req),
        R = davresource0(A),
        case h_depth(A) of
            0 ->
                ?elog("PROPFIND ~p (Depth=0) ~p~n", [R#resource.name,Props]),
                Response = {'D:response', [], propfind_response(Props,A,R)},
                MultiStatus = [{'D:multistatus', [{'xmlns:D',"DAV:"}], [Response]}],
                status(207,MultiStatus);
            1 ->
                R1 = davresource1(A),
                ?elog("PROPFIND ~p (Depth=1, entries=~p) ~p~n", [R#resource.name,length(R1),Props]),
                Response = {'D:response', [], propfind_response(Props,A,R)},
                Responses = [{'D:response', [], propfind_response(Props,A,Rx)} || Rx <- R1],
                MultiStatus = [{'D:multistatus', [{'xmlns:D',"DAV:"}], [Response|Responses]}],
                status(207,MultiStatus);
            infinity ->
                ?elog("PROPFIND ~p (Depth=infinity)~n", [R#resource.name]),
                Response = [{'D:error', [{'xmlns:D',"DAV:"}],[{'propfind-finite-depth'}]}],
                status(403,Response)
        end
    catch
        Status -> status(Status);
        _Error:Reason ->
            ?elog("Unexpected error: ~p~n~p~n",[Reason,erlang:get_stacktrace()]),
            status(500,[{'D:error',[{'xmlns:D',"DAV:"}],[Reason]}])
    end.

propfind_response(Props,A,R) ->
    Url = yaws_api:url_encode(R#resource.name),
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
            Results = [ prop_get(N,A,R) || N <- PropsRequested ],
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
        Response = proppatch_response(Update,A,R),
        MultiStatus = [{'D:multistatus', [{'xmlns:D',"DAV:"}], [Response]}],
        status(207,MultiStatus)
    catch
        Status -> status(Status);
        _Error:Reason ->
            ?elog("Unexpected error: ~p~n~p~n",[Reason,erlang:get_stacktrace()]),
            status(500,[{'D:error',[{'xmlns:D',"DAV:"}],[Reason]}])
    end.

proppatch_response(Update,A,R) ->
    Url = yaws_api:url_encode(R#resource.name),
    Results = proppatch_response(Update,A,R,[]),
    ResultsSorted = prop_sort(lists:flatten(Results)),
    [{'D:href', [], [Url]}|
     [{'D:propstat', [], [
        {'D:prop', [], PropsFound},prop_status(Status)
     ]} || {Status,PropsFound} <- ResultsSorted ]
    ].
proppatch_response([H|T],A,R,Results) ->
    Result = case H of
                 {set,Props} -> [ prop_set(P,A,R) || P <- Props];
                 {remove,Props} -> [ prop_remove(P,A,R) || P <- Props]
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


prop_status(200) -> {'D:status', [],["HTTP/1.1 200 OK"]};
prop_status(403) -> {'D:status', [],["HTTP/1.1 403 Forbidden"]};
prop_status(404) -> {'D:status', [],["HTTP/1.1 404 Not Found"]};
prop_status(409) -> {'D:status', [],["HTTP/1.1 409 Conflict"]};
prop_status(424) -> {'D:status', [],["HTTP/1.1 424 Failed Dependency"]};
prop_status(507) -> {'D:status', [],["HTTP/1.1 507 Insufficient Storage"]}.

%%----------------------------------------------------
%%
%% Available props include namespace
%% TODO Available props can differ per resource
%% For Microsoft extensions see: draft-hopmann-collection-props-00.txt
%%
allprops(R) ->
    F = R#resource.info,
    P1 = case F#file_info.type of
          directory -> [
            {'http://apache.org/dav/props/',executable} % Apache extension
                ];
          _ -> []
        end,
    P2 = [
            {'http://yaws.hyber.org/',access}, % Yaws sample extension
          %%{'DAV:',childcount}, % Microsoft extension
            {'DAV:',creationdate},
            {'DAV:',displayname},
            {'http://apache.org/dav/props/',executable}, % Apache extension
            {'DAV:',getcontentlanguage},
            {'DAV:',getcontentlength},
            {'DAV:',getcontenttype},
            {'DAV:',getetag},
            {'DAV:',getlastmodified},
          %%{'DAV:',isfolder}, % Microsoft extension
            {'DAV:',ishidden}, % Microsoft extension
            {'DAV:',lockdiscovery}, % class 2 compliancy
          %%{'DAV:','quota-available-bytes'} % RFC4331
          %%{'DAV:','quota-used-bytes'} % RFC4331
            {'DAV:',resourcetype},
            {'DAV:',supportedlock} % class 2 compliancy
        ],
    P1 ++ P2.

prop_get({'http://yaws.hyber.org/',access},_A,R) ->
    F = R#resource.info,
    A = F#file_info.access,
    P = {access, [{xmlns,'http://yaws.hyber.org/'}], [atom_to_list(A)]},
    {200, P};
prop_get({'DAV:',displayname},_A,R) ->
    Name = filename:basename(R#resource.name),
    P = {'D:displayname', [], [Name]},
    {200, P};
prop_get({'DAV:',creationdate},_A,R) ->
    F = R#resource.info,
    D = F#file_info.ctime,
    T = yaws:local_time_as_gmt_string(D),
    P = {'D:creationdate', [], [lists:flatten(T)]},
    {200, P};
prop_get({'DAV:',getlastmodified},_A,R) ->
    F = R#resource.info,
    D = F#file_info.mtime,
    T = yaws:local_time_as_gmt_string(D),
    P = {'D:getlastmodified', [], [lists:flatten(T)]},
    {200, P};
prop_get({'DAV:',getcontenttype},_A,R) ->
    Name = R#resource.name,
    P = {'D:getcontenttype', [], [mediatype(Name)]},
    {200, P};
prop_get({'DAV:',getcontentlength},_A,R) ->
    F = R#resource.info,
    P = {'D:getcontentlength', [], [integer_to_list(F#file_info.size)]},
    {200, P};
prop_get({'DAV:',getetag},_A,R) ->
    F = R#resource.info,
    E = yaws:make_etag(F),
    %%?elog("ETAG: ~p~n",[E]),
    P = {'D:getetag', [], [E]},
    {200, P};
prop_get({'DAV:',ishidden},_A,R) ->
    N = filename:basename(R#resource.name),
    H = case hd(N) of
            46 -> "1";
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
                            {'D:lockscope',[],[format(scope,Lock#davlock.scope)]},
                            {'D:locktype',[],[format(type,Lock#davlock.type)]},
                            {'D:depth',[],[format(depth,Lock#davlock.depth)]},
                            %%{'D:owner',[],[format(owner,Lock#davlock.owner)]},
                            {'D:timeout',[],[format(timeout,Lock#davlock.timeout)]},
                            {'D:locktoken',[],[format(locktoken,Lock#davlock.id)]},
                            {'D:lockroot',[],[format(lockroot,R#resource.name)]}
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
prop_get({'http://apache.org/dav/props/',executable},_A,R) ->
    F = R#resource.info,
    case F#file_info.type of
        directory -> {404,{executable, [{'xmlns',"http://apache.org/dav/props/"}], []}};
        _ -> {200, {executable, [{'xmlns',"http://apache.org/dav/props/"}], ["F"]}}
    end;
prop_get({NS,P},_A,_R) ->
    {404,{P,[{'xmlns',NS}],[]}}.

prop_set({'DAV:',getetag},_A,_R) ->
    P = {'D:getetag',[],[]},
    {403,P}; % add precondition 'cannot-modify-protected-property'
prop_set({P,NS},_A,_R) ->
    {403,{P,[{'xmlns',NS}],[]}}.

prop_remove({P,NS},_A,_R) ->
    {403,{P,[{'xmlns',NS}],[]}}.


format(type,write) ->
    {'D:write',[],[]};
format(scope,Scope) ->
    case Scope of
        exclusive -> {'D:exclusive',[],[]};
        _ -> {'D:shared',[],[]}
    end;
format(depth,Depth) ->
    case Depth of
        infinity -> "infinity";
        _ -> integer_to_list(Depth)
    end;
format(timeout,_Timeout) ->
    lists:flatten(io_lib:format("Second-~p",[?LOCK_LIFETIME]));
format(locktoken,Id) ->
    {'D:href',[],["urn:uuid:"++Id]};
format(lockroot,Ref) ->
    {'D:href',[],[Ref]};
format(owner,Owner) ->
    Owner;
format(_,_) ->
    throw(500).

%% Former path routines, replace?

davpath(A) ->
    A#arg.docroot ++ A#arg.server_path.

davurl(A) ->
    davroot(A) ++ A#arg.server_path ++ "/".

davroot(A) ->
    Method = case A#arg.clisock of
                 {sslsocket,_,_} -> "https";
                 _ -> "http"
             end,
    Host = (A#arg.headers)#headers.host,
    Method ++ "://" ++ Host.

%% davresource0/1 - get resources with depth 0
davresource0(A) ->
    Name = normalize(A#arg.server_path),
    Path = normalize(A#arg.docroot) ++ Name,
    case file:read_file_info(Path) of
        {ok, F} ->
            case F#file_info.type of
                directory ->
                    #resource{ name = Name ++ "/", info = F};
                regular ->
                    #resource{ name = Name, info = F};
                _ -> throw(404)
            end;
        {error,_} -> throw(404)
    end.
%% davresource1/1 - get additional resources for depth 1
davresource1(A) ->
    Coll = normalize(A#arg.server_path),
    Path = normalize(A#arg.docroot) ++ Coll,
    case file:read_file_info(Path) of
        {ok, Dir} when Dir#file_info.type == directory ->
            {ok, L} = file:list_dir(Path),
            davresource1(A,Path,Coll,L,[]);
        {ok, _Else} ->
            %% TODO: not a collection, error?
            []
    end.
davresource1(_A,_Path,_Coll,[],Result) ->
    Result;
davresource1(_A,Path,Coll,[Name|Rest],Result) ->
%%    if
%%        hd(Name) == 46 -> % dotted files
%%            davresource1(_A,Path,Coll,Rest,Result); % skip
%%        true ->
            {ok, Info} = file:read_file_info(Path++"/"++Name),
            if
                Info#file_info.type == regular ; Info#file_info.type == directory ->
                    Resource = #resource {name = Coll++"/"++Name, info = Info},
                    davresource1(_A,Path,Coll,Rest,[Resource|Result]);
                true ->
                    davresource1(_A,Path,Coll,Rest,Result) % skip
%%            end
    end.

%% Additional function to normalize a Path in order to handle . and .. correctly
%% A normalized path never ends with a / (root is an empty string)
normalize(Path) ->
    Tree = filename:split(Path),
    Walk = normalize(Tree,[]),
    NPath = lists:reverse(Walk),
    lists:flatten([ ["/",Dir] || Dir <- NPath ]).
normalize(["/"|T],R) ->  normalize(T,R);
normalize([],R) -> R;
normalize(["."|T],R) -> normalize(T,R);
normalize([".."|T],[]) -> normalize(T,[]);
normalize([".."|T1],[_H2|T2]) -> normalize(T1,T2);
normalize([H|T],R) -> normalize(T,[H|R]).



is_collection(R) ->
    F = R#resource.info,
    case F#file_info.type of
        directory -> true;
        _ -> false
    end.

%% --------------------------------------------------------
%% http header
%%
%%

h_depth(A) ->
    %%
    %% Look for: {http_header,  _Num, 'Depth', _, Depth}
    %%
    Hs = (A#arg.headers)#headers.other,
    case lists:keysearch("Depth", 3, Hs) of
        {value, {_,_,"Depth",_,Depth}} ->
            h_depth_interpret(Depth);
        _ ->
            %% RFC4918 strict is that if not found 'infinity' is assumed
            infinity
    end.
h_depth_interpret("infinity") -> infinity;
h_depth_interpret(L) ->
    case catch list_to_integer(L) of
        I when is_integer(I) -> I;
        _                 -> 0
    end.

h_destination(A) ->
    Hs = (A#arg.headers)#headers.other,
    case lists:keysearch("Destination", 3, Hs) of
        {value, {http_header,_,_,_,Dest}} ->
            davresource0(Dest);
        _ ->
            throw(501)
    end.

h_overwrite(A) ->
    Hs = (A#arg.headers)#headers.other,
    case lists:keysearch("Overwrite", 3, Hs) of
        {value, {_,_,"Overwrite",_,YesNo}} ->
            YesNo;
        _ ->
            %% RFC4918 strict is that if not found 'false' is assumed
            false
    end.

h_timeout(A) ->
    Hs = (A#arg.headers)#headers.other,
    case lists:keysearch("Timeout", 3, Hs) of
        {value, {_,_,"Timeout",_,T}} ->
            %% TODO Parse timeout
            T;
        _ ->
            undefined
    end.

h_locktoken(A) ->
    Hs = (A#arg.headers)#headers.other,
    case lists:keysearch("Lock-Token", 3, Hs) of
        {value, {_,_,"Lock-Token",_,T}} ->
            %% TODO Parse and check for Lock-Token
            T;
        _ ->
            undefined
    end.

h_if(A) ->
    Hs = (A#arg.headers)#headers.other,
    case lists:keysearch("If", 3, Hs) of
        {value, {_,_,"If",_,If}} ->
            %% TODO Check if If header is evaluated correctly
            If;
        _ ->
            undefined
    end.

if_parse([],_Resource) ->
    [];
if_parse(Line,Resource) when hd(Line)==32 ->
    if_parse(tl(Line),Resource);
if_parse(Line,untagged) when hd(Line)==60 -> % <
    {Resource,Rest} = if_parse_token(tl(Line),""),
    if_parse(Rest,Resource);
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
    {lists:reverse(Buffer),tl(Line)};
if_parse_token([H|T],Buffer) ->
    if_parse_token(T,[H|Buffer]).

if_parse_etag(Line,Buffer) when hd(Line)==93 -> % ]
    {lists:reverse(Buffer),tl(Line)};
if_parse_etag([H|T],Buffer) ->
    if_parse_etag(T,[H|Buffer]).

if_eval(_R,[]) ->
    false;
if_eval(R,[{Resource,AndList}|T]) ->
    Target = case Resource of
                 untagged -> R;
                 _ ->
                    Url = yaws_api:parse_url(Resource),
                    try
                        davresource0(Url#url.path)
                    catch
                        _ -> throw(412)
                    end
             end,
    if_eval_condition(Target,AndList) orelse if_eval(R,T).

if_eval_condition(_R,[]) ->
    true;
if_eval_condition(R,[{Negate,Kind,Ref}|T]) ->
    Check = case Kind of
                state ->
                    davlock:check(R#resource.name,Ref);
                etag ->
                    F = R#resource.info,
                    E = yaws:make_etag(F),
                    E==Ref
            end,
    This = if Negate -> Check; true -> not Check end,
    This andalso if_eval_condition(R,T).

%% XML elements of RFC4918
%%
%% activelock
-define(IS_ALLPROP(X), #xmlElement{expanded_name = {'DAV:',allprop}} = X).
%% collection
%% depth
%% error
-define(IS_EXCLUSIVE(X), #xmlElement{expanded_name = {'DAV:',exclusive}} = X).
-define(IS_HREF(X), #xmlElement{expanded_name = {'DAV:',href}} = X).
%% include TODO!!
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
    [allprop]; %% TODO include
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
            parse_prop(T,[H#xmlElement.expanded_name|L]);
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
    X.

%% ----------------------
%% output functions
%%

status(Status) ->
    %%?elog("STATUS: ~p~n",[Status]),
    {status, Status}.
status(Status,Response) ->
    %%?elog("~nSTATUS: ~p~nOUTPUT: ~p~n",[Status,Response]),
    Xml = xml_expand(Response),
    [{status, Status},
     {content, "application/xml; charset=\"utf-8\"", Xml}
    ].
status(Status,Headers,Response) ->
    %%?elog("~nSTATUS: ~p~nOUTPUT: ~p~n",[Status,Response]),
    Xml = xml_expand(Response),
    Hdrs = [ {header,H} || H <- Headers],
    [{status, Status} | Hdrs] ++ [{content, "application/xml; charset=\"utf-8\"", Xml}].

xml_expand(L) ->
    xml_expand(L, "utf-8").

xml_expand(L, Cset) ->
    Prolog = ["<?xml version=\"1.0\" encoding=\""++Cset++"\" ?>"],
    xmerl:export_simple(L,xmerl_xml,[{prolog,Prolog}]).

%%-------------------------------------------
%% Functions needed within methods
%%

file_do(Func,Params) ->
    Result = erlang:apply(file,Func,Params),
    case Result of
        ok -> Result;
        {error,eexist} -> throw(405);
        {error,enoent} -> throw(409);
        {error,enospace} -> throw(507);
        _ -> throw(500)
    end.

rmrf(Path) ->
    case file:read_file_info(Path) of
        {ok, F} when F#file_info.type == directory ->
            case file:list_dir(Path) of
                {ok, Fs} ->
                    case rmrf(Path, Fs) of
                        ok ->
                            file:del_dir(Path);
                        _Err ->
                            ok
                    end;
                Err ->
                    Err
            end;
        {ok, _} ->
            file:delete(Path);
        Err ->
            Err
    end.

rmrf(_Dir, []) ->
    ok;
rmrf(Dir, [H|T]) ->
    F = filename:join(Dir, H),
    case rmrf(F) of
        ok ->
            rmrf(Dir, T);
        Err ->
            Err
    end.


store_client_data(Fd, CliSock, all, SSlBool) ->
    store_client_data_all(Fd, CliSock, SSlBool);

store_client_data(Fd, CliSock, Len, SSlBool) ->
    store_client_data_len(Fd, CliSock, Len, SSlBool).


%% not nice to support this for ssl sockets
store_chunked_client_data(Fd, CliSock, SSL) ->
    yaws:setopts(CliSock, [binary, {packet, line}], SSL),
    N = yaws:get_chunk_num(CliSock, SSL),
    yaws:setopts(CliSock, [binary, {packet, raw}], SSL),
    if
        N == 0 ->
            _Tmp=yaws:do_recv(CliSock, 2, SSL);%% flush last crnl
        true ->
            B = yaws:get_chunk(CliSock, N, 0,SSL),
            yaws:eat_crnl(CliSock,SSL),
            ok = file:write(Fd, B),
            store_chunked_client_data(Fd, CliSock, SSL)
    end.

store_client_data_len(_Fd, _CliSock, 0, _SSlBool) ->
    ok;
store_client_data_len(Fd, CliSock, Len, SSlBool) ->
    case yaws:cli_recv(CliSock, Len, SSlBool) of
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

%%------------------------------------------------------------------------------------
%% These functions are already part of yaws.erl but not exported
%% and therefore repeated here
%%
%% universal_time_as_string/0, universal_time_as_string/1
%% local_time_as_gmt_string/1
%% time_to_string/3
%% mediatype/1

%%universal_time_as_string() ->
%%    universal_time_as_string(calendar:universal_time()).
%%universal_time_as_string(UTime) ->
%%    time_to_string(UTime, "GMT").
%%local_time_as_gmt_string(LocalTime) ->
%%    time_to_string(erlang:localtime_to_universaltime(LocalTime),"GMT").
%%
%%time_to_string({{Year, Month, Day}, {Hour, Min, Sec}}, Zone) ->
%%    [day(Year, Month, Day), ", ",
%%     mk2(Day), " ", month(Month), " ", integer_to_list(Year), " ",
%%     mk2(Hour), ":", mk2(Min), ":", mk2(Sec), " ", Zone].
%%
%%
%%mk2(I) when I < 10 ->
%%    [$0 | integer_to_list(I)];
%%mk2(I) ->
%%    integer_to_list(I).
%%
%%day(Year, Month, Day) ->
%%    int_to_wd(calendar:day_of_the_week(Year, Month, Day)).
%%
%%month(1)  -> "Jan";
%%month(2)  -> "Feb";
%%month(3)  -> "Mar";
%%month(4)  -> "Apr";
%%month(5)  -> "May";
%%month(6)  -> "Jun";
%%month(7)  -> "Jul";
%%month(8)  -> "Aug";
%%month(9)  -> "Sep";
%%month(10) -> "Oct";
%%month(11) -> "Nov";
%%month(12) -> "Dec".
%%
%%int_to_wd(1) -> "Mon";
%%int_to_wd(2) -> "Tue";
%%int_to_wd(3) -> "Wed";
%%int_to_wd(4) -> "Thu";
%%int_to_wd(5) -> "Fri";
%%int_to_wd(6) -> "Sat";
%%int_to_wd(7) -> "Sun".

%% RFC4288
%% use mime_types:t/1 instead?
mediatype(Filename) ->
    Extension = filename:extension(Filename),
    Extension1 = string:to_lower(Extension),
    Basename = filename:basename(Filename),
    Basename1 = string:to_lower(Basename),
    mediatype(Basename1,Extension1).
mediatype(_,".abw") -> "application/abiword";
mediatype(_,".avi") -> "video/x-msvideo";
mediatype(_,".asf") -> "video/x-ms-asf";
mediatype(_,".bmp") -> "image/bmp";
mediatype(_,".css") -> "text/css";
mediatype(_,".csv") -> "text/csv";
mediatype(_,".desktop") -> "application/x-desktop";
mediatype(_,".doc") -> "application/msword";
mediatype(_,".docx") -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document";
mediatype(_,".eml") -> "message";
mediatype(_,".eps") -> "image-x-eps";
mediatype(_,".erl") -> "text/x-erlang";
mediatype(_,".gif") -> "image/gif";
mediatype(_,".gz") -> "application/x-tarz";
mediatype(_,".html") -> "text/html";
mediatype(_,".java") -> "application/x-java";
mediatype(_,".jpeg") -> "image/jpeg";
mediatype(_,".jpg") -> "image/jpeg";
mediatype(_,".js") -> "application/javascript";
mediatype(_,".log") -> "text/x-log";
mediatype(_,".odf") -> "application/vnd.oasis.opendocument.formula";
mediatype(_,".odg") -> "application/vnd.oasis.opendocument.graphics";
mediatype(_,".odi") -> "application/vnd.oasis.opendocument.image";
mediatype(_,".odp") -> "application/vnd.oasis.opendocument.presentation";
mediatype(_,".ods") -> "application/vnd.oasis.opendocument.spreadsheet";
mediatype(_,".odt") -> "application/vnd.oasis.opendocument.text";
mediatype(_,".pdf") -> "application/pdf";
mediatype(_,".php") -> "application/x-php";
mediatype(_,".png") -> "image/png";
mediatype(_,".ppt") -> "application/vnd.ms-powerpoint";
mediatype(_,".ps") -> "application/postscript";
mediatype(_,".rtf") -> "application/rtf";
mediatype(_,".sql") -> "text/x-sql";
mediatype(_,".tgz") -> "application/x-compressed-tar";
mediatype(_,".txt") -> "text/plain";
mediatype(_,".wmv") -> "video/x-ms-wmv";
mediatype(_,".xls") -> "application/vnd.ms-excel";
mediatype(_,".xml") -> "text/xml";
mediatype(_,".xpm") -> "image/x-pixmap";
mediatype(_,".zip") -> "application/zip";
mediatype("install",_) -> "text-x-install";
mediatype("makefile",_) -> "text-x-makefile";
mediatype("readme",_) -> "text-x-readme";
mediatype(_,_) -> "text/plain".
