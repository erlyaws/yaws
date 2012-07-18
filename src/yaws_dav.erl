-module(yaws_dav).
%%%-------------------------------------------------------------------
%%% Created : 15 May 2005 by Tobbet <tobbe@tornkvist.org>
%%% Modified: 21 Nov 2005 by <mbj@tail-f.com>
%%% Modified: 28 Jun 2012 by <tjeerd@yolt.nl>
%%% Desc.   : WebDav specifics.
%%%           RFC4918 class 1, 3 compliant
%%%           To use, add a line dav = true in the <server>.
%%% TODO: add locking (class 2 compliancy) using an ETS table
%%%   
%%%-------------------------------------------------------------------
-export([propfind/1, proppatch/1, delete/1, put/2, mkcol/1, move/1, copy/1]).
%-export([parse_propfind/1, xml_expand/1, xml_expand/2]).
-compile(export_all).

-include("../include/yaws_dav.hrl").
-include("../include/yaws_api.hrl").
-include("../include/yaws.hrl").
-include("yaws_debug.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/file.hrl").
-record(resource,{name,info}).

-define(elog(X,Y), error_logger:info_msg("*elog ~p:~p: " X,
                                         [?MODULE, ?LINE | Y])).

delete(A) ->
    Path = davpath(A),
    ?elog("DELETE Path=~p~n", [Path]),
    case rmrf(Path) of
        ok -> status(200);
        _  -> status(403)
    end.

put(SC, ARG) ->
    % FIXME Check if allowed to PUT this resource
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
                        % FIXME status(409)?
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
        case depth(A) of
            0 ->
                ?elog("propfind: Depth=0~n", []),
                R = davresource0(A),
                Response = {'D:response', [], propfind_response(Props,A,R)},
                MultiStatus = [{'D:multistatus', [{'xmlns:D',"DAV:"}], [Response]}],
                status(207,MultiStatus);
            1 ->
                R = davresource0(A),
                Response = {'D:response', [], propfind_response(Props,A,R)},
                R1 = davresource1(A),
                ?elog("propfind: Depth=1, entries=~p~n", [length(R1)]),
                Responses = [{'D:response', [], propfind_response(Props,A,Rx)} || Rx <- R1],
                MultiStatus = [{'D:multistatus', [{'xmlns:D',"DAV:"}], [Response|Responses]}],
                status(207,MultiStatus);
            infinity ->
                ?elog("propfind: Depth=infinity~n", []),
                Response = [{'D:error', [{'xmlns:D',"DAV:"}],[{'propfind-finite-depth'}]}],
                status(403,Response)
        end
    catch
        Status -> status(Status);
        Error:Reason ->
            io:format("catched ~p: ~p~n~p~n",[Error,Reason,erlang:get_stacktrace()]),
            status(500,[{'D:error',[{'xmlns:D',"DAV:"}],[Reason]}])
    end.

propfind_response(Props,A,R) ->
    Url = R#resource.name,
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
io:format("~nparse_proppatch(~p) <- ~p~n" ,[R,Req]),
        Update = parse_proppatch(Req),
io:format("~nproppatch_response() <- ~p~n" ,[Update]),
        Response = proppatch_response(Update,A,R),
io:format("~nproppatch_response() -> ~p~n" ,[Response]),
        MultiStatus = [{'D:multistatus', [{'xmlns:D',"DAV:"}], [Response]}],
        status(207,MultiStatus)
    catch
        Status -> status(Status);
        Error:Reason ->
            io:format("catched ~p: ~p~n~p~n",[Error,Reason,erlang:get_stacktrace()]),
            status(500,[{'D:error',[{'xmlns:D',"DAV:"}],[Reason]}])
    end.

proppatch_response(Update,A,R) -> 
    Url = R#resource.name,
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

%----------------------------------------------------
%
% Available props include namespace
% TODO Available props can differ per resource
% For Microsoft extensions see: draft-hopmann-collection-props-00.txt
%
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
           %{'DAV:',childcount}, % Microsoft extension
            {'DAV:',creationdate},
            {'DAV:',displayname},
            {'DAV:',getcontentlanguage},
            {'DAV:',getcontentlength},
            {'DAV:',getcontenttype},
            {'DAV:',getetag},
            {'DAV:',getlastmodified},
           %{'DAV:',isfolder}, % Microsoft extension
            {'DAV:',ishidden}, % Microsoft extension
           %{'DAV:',lockdiscovery}, % class 2 compliancy
           %{'DAV:','quota-avialable-bytes'} % RFC4331
           %{'DAV:','quota-used-bytes'} % RFC4331
            {'DAV:',resourcetype}
           %{'DAV:',supportedlock}, % class 2 compliancy
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
    T = local_time_as_gmt_string(D),
    P = {'D:creationdate', [], [lists:flatten(T)]},
    {200, P};
prop_get({'DAV:',getlastmodified},_A,R) ->
    F = R#resource.info,
    D = F#file_info.mtime,
    T = local_time_as_gmt_string(D),
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
    %?elog("ETAG: ~p~n",[E]),
    P = {'D:getetag', [], [E]},
    {200, P}; 
prop_get({'DAV:',ishidden},_A,R) ->
    N = filename:basename(R#resource.name),
io:format(N),
    H = case hd(N) of
            46 -> "1";
            _ -> "0"
        end,
    P = {'D:ishidden', [], [H]},
    {200, P};
prop_get({'DAV:',resourcetype},_A,R) ->
    F = R#resource.info,
    P = case F#file_info.type of
            directory -> {'D:resourcetype', [], [{'D:collection', [],[]}]};
            _ -> {'D:resourcetype', [], []}
        end,
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
            % TODO: not a collection, error?
            []
    end.
davresource1(_A,_Path,_Coll,[],Result) ->
    Result;
davresource1(_A,Path,Coll,[Name|Rest],Result) ->
%    if
%        hd(Name) == 46 -> % dotted files
%            davresource1(_A,Path,Coll,Rest,Result); % skip 
%        true ->
            {ok, Info} = file:read_file_info(Path++"/"++Name),
            if 
                Info#file_info.type == regular ; Info#file_info.type == directory ->
                    Resource = #resource {name = Coll++"/"++Name, info = Info},
                    davresource1(_A,Path,Coll,Rest,[Resource|Result]);
                true ->
                    davresource1(_A,Path,Coll,Rest,Result) % skip 
%            end
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

depth(A) ->
    %%
    %% Look for: {http_header,  _Num, 'Depth', _, Depth}
    %%
    Hs = (A#arg.headers)#headers.other,
    case lists:keysearch("Depth", 3, Hs) of
        {value, {_,_,"Depth",_,Depth}} ->
            to_depth(Depth);
        _ ->
           	% was 0, RFC4918 strict is that if not found infinity is assumed
            infinity
    end.

to_depth("infinity") -> infinity;
to_depth(L) ->
    case catch list_to_integer(L) of
        I when is_integer(I) -> I;
        _                 -> 0
    end.

xml_expand(L) ->
    xml_expand(L, "utf-8").

xml_expand(L, Cset) ->
    Prolog = ["<?xml version=\"1.0\" encoding=\""++Cset++"\" ?>"],
    xmerl:export_simple(L,xmerl_xml,[{prolog,Prolog}]).


-define(CONTENT(X), X#xmlElement.content).

%-define(IS_TEXT(X), #xmlText{} = X).
-define(IS_PROPFIND(X), #xmlElement{expanded_name = {'DAV:',propfind}} = X).
-define(IS_PROP(X), #xmlElement{expanded_name = {'DAV:',prop}} = X).
-define(IS_PROPNAME(X), #xmlElement{expanded_name = {'DAV:',propname}} = X).
-define(IS_ALLPROP(X), #xmlElement{expanded_name = {'DAV:',allprop}} = X).
-define(IS_PROPERTYUPDATE(X), #xmlElement{expanded_name = {'DAV:',propertyupdate}} = X).
-define(IS_PROPSET(X), #xmlElement{expanded_name = {'DAV:',set}} = X).
-define(IS_PROPREMOVE(X), #xmlElement{expanded_name = {'DAV:',remove}} = X).
%-define(IS_NAME(X), #xmlElement{expanded_name = {'DAV:',name}} = X).
%-define(IS_PARENTNAME(X), #xmlElement{expanded_name = {'DAV:',parentname}} = X).
%-define(IS_HREF(X), #xmlElement{expanded_name = {'DAV:',href}} = X).
%-define(IS_ISHIDDEN(X), #xmlElement{expanded_name = {'DAV:',ishidden}} = X).
%-define(IS_ISCOLLECTION(X), #xmlElement{expanded_name = {'DAV:',iscollection}} = X).
%-define(IS_ISREADONLY(X), #xmlElement{expanded_name = {'DAV:',isreadonly}} = X).
%-define(IS_GETCONTENTTYPE(X), #xmlElement{expanded_name = {'DAV:',getcontenttype}} = X).
%-define(IS_CONTENTCLASS(X), #xmlElement{expanded_name = {'DAV:',contentclass}} = X).
%-define(IS_GETCONTENTLANGUAGE(X), #xmlElement{expanded_name = {'DAV:',getcontentlanguage}} = X).
%-define(IS_CREATIONDATE(X), #xmlElement{expanded_name = {'DAV:',creationdate}} = X).
%-define(IS_LASTACCESSED(X), #xmlElement{expanded_name = {'DAV:',lastaccessed}} = X).
%-define(IS_GETLASTMODIFIED(X), #xmlElement{expanded_name = {'DAV:',getlastmodified}} = X).
%-define(IS_GETCONTENTLENGTH(X), #xmlElement{expanded_name = {'DAV:',getcontentlength}} = X).
%-define(IS_RESOURCETYPE(X), #xmlElement{expanded_name = {'DAV:',resourcetype}} = X).
%-define(IS_ISSTRUCTUREDDOCUMENT(X), #xmlElement{expanded_name = {'DAV:',isstructureddocument}} = X).
%-define(IS_DEFAULTDOCUMENT(X), #xmlElement{expanded_name = {'DAV:',defaultdocument}} = X).
%-define(IS_DISPLAYNAME(X), #xmlElement{expanded_name = {'DAV:',displayname}} = X).
%-define(IS_ISROOT(X), #xmlElement{expanded_name = {'DAV:',isroot}} = X).

% Parameter is always list
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
parse_proppatch([?IS_PROPSET(H)|T],R) ->
    Props = parse_propsetremove(?CONTENT(H)),
    parse_proppatch(T,[{set,Props}|R]);
parse_proppatch([?IS_PROPREMOVE(H)|T],R) ->
    Props = parse_propsetremove(?CONTENT(H)),
    parse_proppatch(T,[{remove,Props}|R]);
parse_proppatch([_H|T], R) ->
    %% skip #xmlText, #xmlComment, etc.
    parse_proppatch(T, R);
parse_proppatch([],R) ->
    lists:reverse(R). % MUST proces in document order

parse_propsetremove([?IS_PROP(X)]) ->
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

%% ----------------------
%% output functions
%%

status(Status) -> 
    %?elog("STATUS: ~p~n",[Status]),
    [{status, Status}].
status(Status,Response) ->
    %?elog("~nSTATUS: ~p~nOUTPUT: ~p~n",[Status,Response]),
    Xml = xml_expand(Response),
    [{status, Status},
     {content, "application/xml; charset=\"utf-8\"", Xml}
    ].

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

universal_time_as_string() ->
    universal_time_as_string(calendar:universal_time()).
universal_time_as_string(UTime) ->
    time_to_string(UTime, "GMT").
local_time_as_gmt_string(LocalTime) ->
    time_to_string(erlang:localtime_to_universaltime(LocalTime),"GMT").
    
time_to_string({{Year, Month, Day}, {Hour, Min, Sec}}, Zone) ->
    [day(Year, Month, Day), ", ",
     mk2(Day), " ", month(Month), " ", integer_to_list(Year), " ",
     mk2(Hour), ":", mk2(Min), ":", mk2(Sec), " ", Zone].
 

mk2(I) when I < 10 ->
    [$0 | integer_to_list(I)];
mk2(I) ->
    integer_to_list(I).

day(Year, Month, Day) ->
    int_to_wd(calendar:day_of_the_week(Year, Month, Day)).


month(1)  -> "Jan";
month(2)  -> "Feb";
month(3)  -> "Mar";
month(4)  -> "Apr";
month(5)  -> "May";
month(6)  -> "Jun";
month(7)  -> "Jul";
month(8)  -> "Aug";
month(9)  -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

int_to_wd(1) -> "Mon";
int_to_wd(2) -> "Tue";
int_to_wd(3) -> "Wed";
int_to_wd(4) -> "Thu";
int_to_wd(5) -> "Fri";
int_to_wd(6) -> "Sat";
int_to_wd(7) -> "Sun".

% RFC4288
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



