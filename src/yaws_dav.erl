%%%-------------------------------------------------------------------
%%% Created : 15 May 2005 by Tobbet <tobbe@tornkvist.org>
%%% Modified: 21 Nov 2005 by <mbj@tail-f.com>
%%% Modified: 28 Jun 2012 by <tjeerd@yolt.nl>
%%% Desc.   : WebDav specifics.
%%%           RFC4918 class 1, 3 compliant
%%%           To use, add a line dav = true in the <server>.
%%% TODO: add locking (class 2 compliancy) using a DETS table
%%%
%%%-------------------------------------------------------------------
-module(yaws_dav).
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
                                store_chunked_client_data(Fd, CliSock, SSL)
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
    case file:make_dir(Path) of
        ok ->
            status(201);
        {error, Reason} ->
            ?elog("failed to create dir: ~p , reason: ~p~n", [Path, Reason]),
            case Reason of
                enoent -> status(409);
                eexist -> status(405);
                enospace -> status(507);
                _ -> status(500)
            end
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
                    status(409,[{error, [{'xmlns',"DAV:"}],[Reason]}])
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
    %% Depth:
    %%  If '0', then no members should be returned.
    %%  If '1', then members one level down should be included in the reply.
    %%  If 'infinity', then all members, recursively, should be included but is allowed to return 403.
    Req = binary_to_list(A#arg.clidata),
    Props = parse_propfind(Req),
    case depth(A) of
        0 ->
            ?elog("propfind: Depth=0~n", []),
            R = davresource0(A),
            case R of
                404 ->
                    status(404);
                _ ->
                    Response = {response, [{'xmlns',"DAV:"}], propfind_response(Props,A,R)},
                    MultiStatus = [{multistatus, [{'xmlns',"DAV:"}], [Response]}],
                    status(207,MultiStatus)
            end;
        1 ->
            R = davresource0(A),
            case R of
                404 ->
                    status(404);
                _ ->
                    Response = {response, [{'xmlns',"DAV:"}], propfind_response(Props,A,R)},
                    R1 = davresource1(A),
                    ?elog("propfind: Depth=1 , entries=~p~n",
                          [length(R1)]),
                    Responses = [{response, [{'xmlns',"DAV:"}], propfind_response(Props,A,Rx)} || Rx <- R1],
                    MultiStatus = [{multistatus, [{'xmlns',"DAV:"}], [Response|Responses]}],
                    status(207,MultiStatus)
            end;
        infinity ->
            ?elog("propfind: Depth=infinity~n", []),
            Response = [{error, [{'xmlns',"DAV:"}],[{'propfind-finite-depth'}]}],
            status(403,Response)
    end.

propfind_response(Props,A,R) ->
    Url = R#resource.name,
    case Props of
        [allprop] ->
            AllProp = [ get_prop(N,A,R) || N <- allprops(R) ],
            AllSorted = prop_sort(AllProp),
            {200, Results} = lists:keyfind(200,1,AllSorted),
            [{href, [{'xmlns',"DAV:"}], [Url]},
             {propstat, [{'xmlns',"DAV:"}], [
                {prop, [{'xmlns',"DAV:"}], Results},{status, [],["HTTP/1.1 200 OK"]}
            ]}];
        [propname] ->
            Results = [ {P,[{'xmlns',NS}],[]} || {NS,P} <-allprops(R) ],
            [{href, [{'xmlns',"DAV:"}], [Url]},
             {propstat, [{'xmlns',"DAV:"}], [
                {prop, [{'xmlns',"DAV:"}], Results},{status, [],["HTTP/1.1 200 OK"]}
            ]}];
        PropsRequested ->
            Results = [ get_prop(N,A,R) || N <- PropsRequested ],
            ResultsSorted = prop_sort(Results),
            [{href, [{'xmlns',"DAV:"}], [Url]}|
             [{propstat, [{'xmlns',"DAV:"}], [
                {prop, [{'xmlns',"DAV:"}], PropsFound},prop_status(Status)
             ]} || {Status,PropsFound} <- ResultsSorted ]
            ]
    end.

proppatch(_A) ->
%%    Req = binary_to_list(A#arg.clidata),
%%    Props = parse_proppatch(Req),
%% FIXME Finish this
    status(501).

prop_sort(L) -> prop_sort(L,[]).
prop_sort([H|T],R) ->
    {Status,Prop} = H,
    R1 = case lists:keyfind(Status,1,R) of
        {Status, Props} -> lists:keystore(Status,1,R,{Status,[Prop|Props]});
        false -> lists:keystore(Status,1,R,{Status,[Prop]})
    end,
    prop_sort(T,R1);
prop_sort([],R) -> R.


prop_status(200) -> {status, [{'xmlns',"DAV:"}],["HTTP/1.1 200 OK"]};
prop_status(403) -> {status, [{'xmlns',"DAV:"}],["HTTP/1.1 403 Forbidden"]};
prop_status(404) -> {status, [{'xmlns',"DAV:"}],["HTTP/1.1 404 Not Found"]};
prop_status(409) -> {status, [{'xmlns',"DAV:"}],["HTTP/1.1 409 Conflict"]};
prop_status(424) -> {status, [{'xmlns',"DAV:"}],["HTTP/1.1 424 Failed Dependency"]};
prop_status(507) -> {status, [{'xmlns',"DAV:"}],["HTTP/1.1 507 Insufficient Storage"]}.

%%----------------------------------------------------
%%
%% Available props include namespace
%% TODO Available props can differ per resource
allprops(_R) ->
    [{'DAV:',creationdate},
     {'DAV:',displayname},
     {'DAV:',getcontentlanguage},
     {'DAV:',getcontentlength},
     {'DAV:',getcontenttype},
     {'DAV:',getetag},
     {'DAV:',getlastmodified},
     %%{'DAV:',lockdiscovery},
     {'DAV:',resourcetype}
     %%{'DAV:',supportedlock},
    ].

get_prop({'DAV:',displayname},_A,R) ->
    Name = filename:basename(R#resource.name),
    P = {displayname, [{'xmlns',"DAV:"}], [Name]},
    {200, P};
get_prop({'DAV:',creationdate},_A,R) ->
    F = R#resource.info,
    D = F#file_info.ctime,
    T = local_time_as_gmt_string(D),
    P = {creationdate, [{'xmlns',"DAV:"}], [lists:flatten(T)]},
    {200, P};
get_prop({'DAV:',getlastmodified},_A,R) ->
    F = R#resource.info,
    D = F#file_info.mtime,
    T = local_time_as_gmt_string(D),
    P = {getlastmodified, [{'xmlns',"DAV:"}], [lists:flatten(T)]},
    {200, P};
get_prop({'DAV:',getcontenttype},_A,R) ->
    Name = R#resource.name,
    P = {getcontenttype, [{'xmlns',"DAV:"}], [mediatype(Name)]},
    {200, P};
get_prop({'DAV:',getcontentlength},_A,R) ->
    F = R#resource.info,
    P = {getcontentlength, [{'xmlns',"DAV:"}], [integer_to_list(F#file_info.size)]},
    {200, P};
get_prop({'DAV:',getetag},_A,R) ->
    F = R#resource.info,
    E = yaws:make_etag(F),
    %%?elog("ETAG: ~p~n",[E]),
    P = {getetag, [{'xmlns',"DAV:"}], [E]},
    {200, P};
get_prop({'DAV:',resourcetype},_A,R) ->
    F = R#resource.info,
    P = case F#file_info.type of
            directory -> {resourcetype, [{'xmlns',"DAV:"}], [{collection, [],[]}]};
            _ -> {resourcetype, [{'xmlns',"DAV:"}], []}
        end,
    {200, P};
get_prop({NS,P},_A,_R) ->
    {404,{P,[{'xmlns',NS}],[]}}.

prop_set(P) ->
    {403,{P,[],[]}}.

prop_update(P) ->
    {403,{P,[],[]}}.

prop_remove(P) ->
    {403,{P,[],[]}}.

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
                _ -> 404
            end;
        {error,_} -> 404
    end.
%% davresource0/1 - get additional resources for depth 1
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
    if
        hd(Name) == 46 -> % dotted files
            davresource1(_A,Path,Coll,Rest,Result); % skip
        true ->
            {ok, Info} = file:read_file_info(Path++"/"++Name),
            if
                Info#file_info.type == regular ; Info#file_info.type == directory ->
                    Resource = #resource {name = Coll++"/"++Name, info = Info},
                    davresource1(_A,Path,Coll,Rest,[Resource|Result]);
                true ->
                    davresource1(_A,Path,Coll,Rest,Result) % skip
            end
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


depth(A) ->
    %%
    %% Look for: {http_header,  _Num, 'Depth', _, Depth}
    %%
    Hs = (A#arg.headers)#headers.other,
    case lists:keysearch("Depth", 3, Hs) of
        {value, {_,_,"Depth",_,Depth}} ->
            to_depth(Depth);
        _ ->
            %% was 0, RFC4918 strict is that if not found infinity is assumed
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


parse_propfind([]) -> [allprops]; % RFC4918: no body then allprops
parse_propfind(L) when is_list(L) ->
    case catch xmerl_scan:string(L, [{namespace_conformant, true}]) of
        {X,_} when is_record(X, xmlElement) ->
            parse_dav(X);
        _Z ->
            ?elog("to_xml: error ~p~n", [_Z]),
            {error, "xml scanner failed"}
    end.

-define(CONTENT(X), X#xmlElement.content).

%%-define(IS_TEXT(X), #xmlText{} = X).
-define(IS_PROPFIND(X), #xmlElement{expanded_name = {'DAV:',propfind}} = X).
-define(IS_PROP(X), #xmlElement{expanded_name = {'DAV:',prop}} = X).
-define(IS_PROPNAME(X), #xmlElement{expanded_name = {'DAV:',propname}} = X).
-define(IS_ALLPROP(X), #xmlElement{expanded_name = {'DAV:',allprop}} = X).
%%-define(IS_NAME(X), #xmlElement{expanded_name = {'DAV:',name}} = X).
%%-define(IS_PARENTNAME(X), #xmlElement{expanded_name = {'DAV:',parentname}} = X).
%%-define(IS_HREF(X), #xmlElement{expanded_name = {'DAV:',href}} = X).
%%-define(IS_ISHIDDEN(X), #xmlElement{expanded_name = {'DAV:',ishidden}} = X).
%%-define(IS_ISCOLLECTION(X), #xmlElement{expanded_name = {'DAV:',iscollection}} = X).
%%-define(IS_ISREADONLY(X), #xmlElement{expanded_name = {'DAV:',isreadonly}} = X).
%%-define(IS_GETCONTENTTYPE(X), #xmlElement{expanded_name = {'DAV:',getcontenttype}} = X).
%%-define(IS_CONTENTCLASS(X), #xmlElement{expanded_name = {'DAV:',contentclass}} = X).
%%-define(IS_GETCONTENTLANGUAGE(X), #xmlElement{expanded_name = {'DAV:',getcontentlanguage}} = X).
%%-define(IS_CREATIONDATE(X), #xmlElement{expanded_name = {'DAV:',creationdate}} = X).
%%-define(IS_LASTACCESSED(X), #xmlElement{expanded_name = {'DAV:',lastaccessed}} = X).
%%-define(IS_GETLASTMODIFIED(X), #xmlElement{expanded_name = {'DAV:',getlastmodified}} = X).
%%-define(IS_GETCONTENTLENGTH(X), #xmlElement{expanded_name = {'DAV:',getcontentlength}} = X).
%%-define(IS_RESOURCETYPE(X), #xmlElement{expanded_name = {'DAV:',resourcetype}} = X).
%%-define(IS_ISSTRUCTUREDDOCUMENT(X), #xmlElement{expanded_name = {'DAV:',isstructureddocument}} = X).
%%-define(IS_DEFAULTDOCUMENT(X), #xmlElement{expanded_name = {'DAV:',defaultdocument}} = X).
%%-define(IS_DISPLAYNAME(X), #xmlElement{expanded_name = {'DAV:',displayname}} = X).
%%-define(IS_ISROOT(X), #xmlElement{expanded_name = {'DAV:',isroot}} = X).


parse_dav(?IS_PROPFIND(X)) ->
    parse_propfind(?CONTENT(X), #propfind{});
parse_dav(_X) ->
    %%?elog("parse_dav: GOT ~p~n", [_X]),
    {error, "parse_dav"}.  % FIXME , webdav (tobbe)

parse_propfind([?IS_PROPNAME(_H)|_T], _R) ->
    [propname];
parse_propfind([?IS_ALLPROP(_H)|_T], _R) ->
    [allprop];
parse_propfind([?IS_PROP(H)|T], _R) ->
    Props = parse_prop(?CONTENT(H)),
    parse_propfind(T, Props);
parse_propfind([_H|T], R) ->
    %%?elog("parse_propfind: ~p~n",[_H]),
    parse_propfind(T, R);
parse_propfind([], R) ->
    R.

%%parse_proppatch()


parse_prop(L) ->
    parse_prop(L, []).

parse_prop([H|T],L) ->
    case H of
        H when is_record(H,xmlElement) ->
            parse_prop(T,[H#xmlElement.expanded_name|L]);
        _ ->
            %% skip
            parse_prop(T,L)
    end;


%%parse_prop([?IS_TEXT(_H)|T], L) ->
%%    parse_prop(T, L);
%%parse_prop([?IS_NAME(_H)|T], L) ->
%%    parse_prop(T, [name | L]);
%%parse_prop([?IS_PARENTNAME(_H)|T], L) ->
%%    parse_prop(T, [parentname | L]);
%%parse_prop([?IS_HREF(_H)|T], L) ->
%%    parse_prop(T, [href | L]);
%%parse_prop([?IS_ISHIDDEN(_H)|T], L) ->
%%    parse_prop(T, [ishidden | L]);
%%parse_prop([?IS_ISCOLLECTION(_H)|T], L) ->
%%    parse_prop(T, [iscollection | L]);
%%parse_prop([?IS_ISREADONLY(_H)|T], L) ->
%%    parse_prop(T, [isreadonly | L]);
%%parse_prop([?IS_GETCONTENTTYPE(_H)|T], L) ->
%%    parse_prop(T, [getcontenttype | L]);
%%parse_prop([?IS_CONTENTCLASS(_H)|T], L) ->
%%    parse_prop(T, [contentclass | L]);
%%parse_prop([?IS_GETCONTENTLANGUAGE(_H)|T], L) ->
%%    parse_prop(T, [getcontentlanguage | L]);
%%parse_prop([?IS_CREATIONDATE(_H)|T], L) ->
%%    parse_prop(T, [creationdate | L]);
%%parse_prop([?IS_LASTACCESSED(_H)|T], L) ->
%%    parse_prop(T, [lastaccessed | L]);
%%parse_prop([?IS_GETLASTMODIFIED(_H)|T], L) ->
%%    parse_prop(T, [getlastmodified | L]);
%%parse_prop([?IS_GETCONTENTLENGTH(_H)|T], L) ->
%%    parse_prop(T, [getcontentlength | L]);
%%parse_prop([?IS_RESOURCETYPE(_H)|T], L) ->
%%    parse_prop(T, [resourcetype | L]);
%%parse_prop([?IS_ISSTRUCTUREDDOCUMENT(_H)|T], L) ->
%%    parse_prop(T, [isstructureddocument | L]);
%%parse_prop([?IS_DEFAULTDOCUMENT(_H)|T], L) ->
%%    parse_prop(T, [defaultdocument | L]);
%%parse_prop([?IS_DISPLAYNAME(_H)|T], L) ->
%%    parse_prop(T, [displayname | L]);
%%parse_prop([?IS_ISROOT(_H)|T], L) ->
%%    parse_prop(T, [isroot | L]);
%%parse_prop([H|T], L) ->
%%    %%?elog("parse_propfind: NYI ~p~n",[H]),  % FIXME , webdav
%%    X = H#xmlElement.expanded_name,
%%    parse_prop(T, [{404,X}|L]);
parse_prop([], L) ->
%%    L1 = lists:flatten(L), % maybe not nescessary anymore when bug 2 lines above is removed
    lists:reverse(L).  % preserve order?

%% ----------------------
%% output functions
%% TODO Replace outXXX functions with status/1 or status/2

status(Status) ->
    %%?elog("STATUS: ~p~n",[Status]),
    [{status, Status}].
status(Status,Response) ->
    %%?elog("~nSTATUS: ~p~nOUTPUT: ~p~n",[Status,Response]),
    Xml = xml_expand(Response),
    [{status, Status},
     {content, "application/xml; charset=\"utf-8\"", Xml}
    ].

%%outXXX(XXX, L) ->
%%    [{status, XXX},
%%     {header, {content_type, "text/xml; charset=\"utf-8\""}},
%%     {html, L}].


%%-------------------------------------------
%% Functions needed within methods
%%

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
