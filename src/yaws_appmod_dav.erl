%%%-------------------------------------------------------------------
%%% File    : yaws_appmod_dav.erl
%%% Created : 11 Nov 2012
%%% Purpose : WebDav module, RFC4918 class 1, 2, 3 compliant
%%%
%%%           To use, add the following configuration:
%%%
%%%               <server>
%%%                   ...
%%%                   dav = true
%%%               </server>
%%%
%%%           This configuration is short for:
%%%
%%%               runmod = yaws_runmod_lock
%%%               ...
%%%               <server>
%%%                   ...
%%%                   appmods = </, yaws_appmod_dav>
%%%               <server>
%%%
%%% Todo    : 1) Add header handling (most of the time not used by DAV
%%%              clients):
%%%              If-Match, If-Modified-Since, If-None-Match,
%%%              If-Range, If-Unmodified-Since, TE
%%%           2) POST on collections
%%%-------------------------------------------------------------------

-module(yaws_appmod_dav).

%% for appmod:
-export([out/1]).

%% for replacement xmerl_xml:
-export([export/1]).
-import(xmerl_lib, [markup/3, empty_tag/2, export_text/1]).


-ifdef(debug).
-define(DEBUG(X), io:format(X)).
-define(DEBUG(X,Y), io:format(X,Y)).
-else.
-define(DEBUG(X), true).
-define(DEBUG(X,Y), true).
-endif.

-include("../include/yaws_dav.hrl").
-include("../include/yaws_lock.hrl").
-include("../include/yaws_api.hrl").
-include("../include/yaws.hrl").
-include("yaws_debug.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/file.hrl").

out(A) ->
    try
        h_litmus(A),
        %% handle the request
        R = A#arg.req,
        Method = R#http_request.method,
        H = A#arg.headers,
        C = if
                H#headers.user_agent == undefined -> standard;
                true ->
                    Ms = string:str(H#headers.user_agent,"Microsoft"),
                    if
                        Ms > 0 -> microsoft;
                        true -> standard
                    end
            end,
        put(compatibility,C),
        handle(Method,A)
    catch
        {Status,Precondition} ->
            Response1 = [{'D:error', [{'xmlns:D',"DAV:"}],
                          [{Precondition,[],[]}]}],
            status(Status,{xml,Response1});
        Status ->
            status(Status);
        _Error:{noproc,{gen_server,call,[yaws_runmod_lock|_Whatever]}} ->
            Msg = "Lock server not started. See documentation.~n",
            error_logger:error_msg(Msg),
            Response = [{'D:error',[{'xmlns:D',"DAV:"}],[Msg]}],
            status(500,{xml,Response});
        _Error:Reason ->
            error_logger:info_msg("unexpected error: ~p~n~p~n",
                                  [Reason,erlang:get_stacktrace()]),
            Response = [{'D:error',[{'xmlns:D',"DAV:"}],[Reason]}],
            status(500,{xml,Response})
    end.

%%------------------------------------------------------
%% handle methods
%%
handle('OPTIONS',A) ->
    ?DEBUG("OPTIONS"),
    R = davresource0(A),
    F = R#resource.info,
    T = case F#file_info.type of
            directory -> "httpd/unix-directory";
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
    Headers = [{header,{"Allow",
                        "GET, POST, OPTIONS, HEAD, PUT, DELETE, "
                        "PROPFIND, PROPPATCH, LOCK, UNLOCK, "
                        "MKCOL, MOVE, COPY"}}],
    status(200,[{header,{"Content-Type",T}}|Headers],[]);
handle('HEAD',A) ->
    ?DEBUG("HEAD ~p",[A#arg.server_path]),
    Path = davpath(A),
    case file:read_file_info(Path) of
        {ok, F} when (F#file_info.type == regular) ->
            E = yaws:make_etag(F),
            Name = A#arg.server_path,
            Ext = filename:extension(Name),
            Ext1 = case Ext of
                       [] -> "";
                       _ -> tl(Ext)
                   end,
            {_Kind,T} = mime_types:t(Ext1),
            status(200,[{header,{"Etag",E}},{header,{"Content-Type",T}}]);
        {ok, F} when (F#file_info.type == directory) ->
            status(200,[{header,{"Content-Type","httpd/unix-directory"}}]);
        _ -> throw(404)
    end;
handle('GET',A) ->
    ?DEBUG("GET ~p",[A#arg.server_path]),
    Name = A#arg.server_path,
    Path = A#arg.fullpath,
    Pid = self(),
    SC=get(sc),
    PPS = SC#sconf.partial_post_size,
    case file:read_file_info(Path) of
        {ok,F} when F#file_info.type==directory ->
            {ok,Dir} = file:list_dir(Path),
            Listing = lists:foldl(
                        fun(Fname,L) ->
                            {ok,Finfo} = file:read_file_info(
                                           filename:join(Path,Fname)),
                            Ftype = case Finfo#file_info.type of
                                        directory -> "Coll:";
                                        _         -> "     "
                                    end,
                            Fsize = integer_to_list(Finfo#file_info.size),
                            Ftime = yaws:local_time_as_gmt_string(
                                      Finfo#file_info.mtime),
                            Entry = io_lib:format("~s ~-20s ~10s  ~s~n",
                                                  [Ftype,Fname,Fsize,Ftime]),
                            L++Entry
                        end,"",lists:sort(Dir)),
            Response = {ehtml,[
                {html,[],[
                    {head,[],[
                        {title,[],Name}]
                    },
                    {body,[],[
                        {h2,[],"Index of "++Name},
                        {pre,[],Listing}]
                    }]
                }]
            },
            status(200,Response);
        {ok,F} ->
            Size = integer_to_list(F#file_info.size),
            Ext = filename:extension(Name),
            Ext1 = case Ext of
                       [] -> "";
                       _ -> tl(Ext)
                   end,
            {_Kind,Mimetype} = mime_types:t(Ext1),
            H = [{header,{"Content-Length",Size}}],
            {ok,Fd} = file:open(Path,[read,binary]),
            case file:read(Fd,PPS) of
                {ok,Data} when size(Data)<PPS ->
                    ?DEBUG("only chunk~n"),
                    status(200,H,{content,Mimetype,Data});
                {ok,Data} ->
                    ?DEBUG("first chunk~n"),
                    spawn(fun() -> deliver_rest(Pid,Fd) end),
                    status(200,H,{streamcontent,Mimetype,Data});
                eof ->
                    status(200,{content,"application/octet-stream",<<>>});
                {error,Reason} ->
                    Response = [{'D:error',[{'xmlns:D',"DAV:"}],[Reason]}],
                    status(500,{xml,Response})
            end;
        {error,enoent} ->
            status(404);
        _Other ->
            status(500)
    end;
handle('POST',A) ->
    ?DEBUG("POST ~p",[A#arg.server_path]),
    _Path = davpath(A),
    %% TODO POST for collections: RFC5995
    status(501);
handle("LOCK",A) ->
    %% TODO Multi resource lock (lock on collection) returns 207
    %%      (multi-status) when failing
    ?DEBUG("LOCK ~p",[A#arg.server_path]),
    Name = A#arg.server_path,
    Path = davpath(A),
    %% check if file/collection exists and create if not so
    %% RFC4918 - 9.10.4
    {Status,R} = case file:read_file_info(Path) of
            {ok, F} when (F#file_info.type == directory) or
                         (F#file_info.type == regular) ->
                {200,#resource{ name = Name, info = F}};
            {error,enoent} ->
                case string:right(A#arg.server_path,1) of
                    "/" ->
                        ok = file:make_dir(Path);
                    _ ->
                        ok = file:write_file(Path,"")
                end,
                {ok, F} = file:read_file_info(Path),
                {201,#resource{ name = Name, info = F}};
            {error,_} -> throw(409)
        end,
    Req = binary_to_list(A#arg.clidata),
    L = parse_lockinfo(Req),
    %%Id = h_locktoken(A),
    Timeout = h_timeout(A),
    Depth = h_depth(A),
    Id = h_if_refresh(A,Path),
    case yaws_runmod_lock:lock(Path,L#lock{path=Path,id=Id,timeout=Timeout,
                                           depth=Depth}) of
        {ok,Id1} ->
            {200,Result} = prop_get({'DAV:',lockdiscovery},A,R),
            Response = [{'D:prop', [{'xmlns:D',"DAV:"}], [Result]}],
            status(Status,[{header,
                            {"Lock-Token","<opaquelocktoken:"++Id1++">"}}],
                   {xml,Response});
        {error,locked} ->
            status(423);
        _ ->
            throw(501)
    end;
handle("UNLOCK",A) ->
    ?DEBUG("UNLOCK ~p",[A#arg.server_path]),
    Path = davpath(A),
    Id = h_locktoken(A),
    %%?DEBUG(" Id=~p",[Id]),
    case yaws_runmod_lock:unlock(Path,Id) of
        ok -> status(204);
        not_found ->
            Response = [{'D:error', [{'xmlns:D',"DAV:"}],
                         [{'lock-token-matches-request-uri',[],[]}]}],
            status(409,{xml,Response})
    end;
handle('DELETE',A) ->
    ?DEBUG("DELETE ~p",[A#arg.server_path]),
    Path = davpath(A),
    h_if(A,Path),
    R = davresource0(A),
    %% use internal locking to be safe
    case yaws_runmod_lock:lock(R#resource.name,
                               #lock{depth=infinity,scope=exclusive}) of
        {ok,Id} ->
            F = filename:join(A#arg.docroot,["./",R#resource.name]),
            fs_rmrf(F),
            yaws_runmod_lock:unlock(R#resource.name,Id),
            status(200);
        _ -> throw(423)
    end;
handle('PUT',A) when A#arg.state == undefined ->
    ?DEBUG("PUT ~p",[A#arg.server_path]),
    Path = davpath(A),
    h_if(A,Path),
    case filelib:is_dir(Path) of
        true ->
            throw(405);
        false ->
            TmpName = temp_name(Path),
            X = file:open(TmpName, [raw,write]),
            case X  of
                {ok, Fd} ->
                    State = #upload{fd=Fd, filename=Path, tempname=TmpName},
                    case A#arg.clidata of
                        {partial,Bin} ->
                            file:write(Fd,Bin),
                            {get_more,<<>>,State};
                        Bin ->
                            file:write(Fd,Bin),
                            file:close(Fd),
                            case file:rename(TmpName, Path) of
                                ok -> status(201);
                                _ -> status(409)
                            end
                    end;
                {error,eexist} ->
                    throw(405);
                {error,enoent} -> throw(409);
                {error,eisdir} -> throw(409);
                {error,enospace} -> throw(507);
                _ -> status(500)
            end
    end;
handle('PUT',A) ->
    State = A#arg.state,
    Fd = State#upload.fd,
    case A#arg.clidata of
        {partial,Bin} ->
            file:write(Fd,Bin),
            {get_more,<<>>,State};
        Bin ->
            file:write(Fd,Bin),
            file:close(Fd),
            FName = State#upload.filename,
            TmpName = State#upload.tempname,
            case file:rename(TmpName, FName) of
                ok -> status(201);
                _ -> status(409) % TODO delete temp file here?
            end
    end;
handle("MKCOL",A) ->
    ?DEBUG("MKCOL ~p",[A#arg.server_path]),
    Path = davpath(A),
    if
        %% RFC2518, 8.3.1
        size(A#arg.clidata) > 0 -> throw(415);
        true -> ok
    end,
    h_if(A,Path),
    file_do(make_dir,[Path]),
    status(201);
handle("COPY",A) ->
    ?DEBUG("COPY ~p",[A#arg.server_path]),
    From = A#arg.fullpath,
    Dest = h_destination(A),
    To = case {string:right(Dest,1),string:right(Dest,1)} of
             {"/",_} -> Dest;
             {_,"/"} -> filename:join(Dest,filename:basename(From));
             _ -> Dest
         end,
    DoOverwrite = h_overwrite(A),
    ToExists = exists(To),
    if
        DoOverwrite == false, ToExists == true ->
            status(412);
        true  ->
            if ToExists == true ->
                    h_if(A,To),
                    fs_rmrf(To),
                    fs_cp(From,To),
                    status(204);
               true ->
                    fs_cp(From,To),
                    status(201)
                    %%status(201,[{'Location',To}],[])
            end
    end;
handle("MOVE",A) ->
    ?DEBUG("MOVE ~p",[A#arg.server_path]),
    From = davpath(A),
    h_if(A,From),
    Dest = h_destination(A),
    To = case {string:right(Dest,1),string:right(Dest,1)} of
             {"/",_} -> Dest;
             {_,"/"} -> filename:join(Dest,filename:basename(From));
             _ -> Dest
         end,
    DoOverwrite = h_overwrite(A),
    ToExists = exists(To),
    if
        DoOverwrite == false, ToExists == true ->
            status(412);
        true  ->
            if
                ToExists == true ->
                    h_if(A,To),
                    fs_rmrf(To);
                true ->
                    ok
            end,
            case file:rename(From, To) of
                ok when ToExists == true ->
                    status(204);
                ok ->
                    status(201);
                _ ->
                    try
                        fs_cp(From, To),
                        fs_rmrf(From),
                        status(201)
                    catch
                        throw:Status ->
                            ?DEBUG(" move from ~p to ~p failed: ~p\n",
                                   [From, To, Status]),
                            Response = [{'D:error', [{'xmlns:D',"DAV:"}],
                                         [Status]}],
                            status(Status,{xml,Response})
                    end
            end
    end;
handle("PROPFIND",A) ->
    ?DEBUG("PROPFIND ~p",[A#arg.server_path]),
    Req = binary_to_list(A#arg.clidata),
    Props = parse_propfind(Req),
    R = davresource0(A),
    case h_depth(A) of
        0 ->
            Response = {'D:response', [], propfind_response(Props,A,R)},
            MultiStatus = [{'D:multistatus', [{'xmlns:D',"DAV:"}], [Response]}],
            status(207,{xml,MultiStatus});
        1 ->
            R1 = davresource1(A),
            Response = {'D:response', [], propfind_response(Props,A,R)},
            Responses = [{'D:response', [],
                          propfind_response(Props,A,Rx)} || Rx <- R1],
            MultiStatus = [{'D:multistatus', [{'xmlns:D',"DAV:"}],
                            [Response|Responses]}],
            status(207,{xml,MultiStatus});
        infinity ->
            Response = [{'D:error', [{'xmlns:D',"DAV:"}],
                         [{'propfind-finite-depth',[],[]}]}],
            status(403,{xml,Response})
    end;
handle("PROPPATCH",A) ->
    ?DEBUG("PROPPATCH ~p",[A#arg.server_path]),
    Path = davpath(A),
    h_if(A,Path),
    Req = binary_to_list(A#arg.clidata),
    R = davresource0(A),
    Update = parse_proppatch(Req),
    Response = {'D:response',[],proppatch_response(Update,A,R)},
    MultiStatus = [{'D:multistatus', [{'xmlns:D',"DAV:"}], [Response]}],
    status(207,{xml,MultiStatus});
handle(_Other,_A) ->
    status(405).


%% --------------------------------------------------------
%% File functions
%%

%% deliver chunked data
deliver_rest(Pid,Fd) ->
    case file:read(Fd,10240) of
        {ok, Data} ->
            yaws_api:stream_chunk_deliver(Pid,Data),
            deliver_rest(Pid,Fd);
        eof ->
            yaws_api:stream_chunk_end(Pid),
            file:close(Fd);
        {error,Reason} ->
            exit(Reason)
    end.

%% do a recoverable/catchable file function
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
        _Error -> ?DEBUG("file function returned ~p~n",[_Error]),throw(500)
    end.

%% recursive remove, equivalent of rm -rf
fs_rmrf(Path) ->
    {ok, F} = file_do(read_file_info,[Path]),
    case F#file_info.type of
        directory ->
            {ok, Dir} = file_do(list_dir,[Path]),
            [ fs_rmrf(filename:join(Path,File)) || File <- Dir ],
            ok = file:del_dir(Path);
            %%file_do(del_dir,[Path]);
        _ ->
            ok = file:delete(Path)
            %%file_do(delete,[Path])
    end.

%% recursive copy, equivalent of cp
fs_cp(From,To) ->
    %% All checks on existence of the destination have to be done before
    %% so destination should not exist
    {ok, F} = file:read_file_info(From),
    case F#file_info.type of
        directory ->
            file_do(make_dir,[To]),
            {ok, Dir} = file:list_dir(From),
            [ fs_cp(filename:join(From,File),filename:join(To,File)) ||
                File <- Dir ];
        _ ->
            file_do(copy,[From,To])
    end.

%% check existence
exists(Path) ->
    case file:read_file_info(Path) of
        {ok, _} -> true;
        _ -> false
    end.

%% generate a temporary filename as a dotted file with a timestamp
temp_name(F) ->
    {A,B,C} = yaws:get_time_tuple(),
    Path = filename:dirname(F),
    File = filename:basename(F),
    T0 = io_lib:format("~s/.~s.~p-~p-~p",[Path,File,A,B,C]),
    lists:flatten(T0).

%% --------------------------------------------------------
%% Property functions
%%

%% propfind response
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
            Results = [ prop_get(N,A,R) || {N,_} <- PropsRequested ],
            ResultsSorted = prop_sort(Results),
            [{'D:href', [], [Url]}|
             [{'D:propstat', [], [
                {'D:prop', [], PropsFound},prop_status(Status)
             ]} || {Status,PropsFound} <- ResultsSorted ]
            ]
    end.

%% proppatch response/
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


%% Available props include namespace
%% Available props can differ per resource
%% For proposed Microsoft extensions see: draft-hopmann-collection-props-00.txt
%%
allprops(R) ->
    F = R#resource.info,
    C = get(compatibility),
    %% default property set
    P1 = [
          {'http://yaws.hyber.org/',access},    % sample Yaws extension
          {'DAV:',creationdate},
          %%{'DAV:',getcontentlanguage},        % not supported in GET
                                                % so omitted here as well
          {'DAV:',getcontentlength},
          {'DAV:',getcontenttype},
          {'DAV:',getetag},
          {'DAV:',getlastmodified},
          {'DAV:',lockdiscovery}, % class 2 compliancy
          %%{'DAV:','quota-avialable-bytes'}    % RFC4331
                                         %{'DAV:','quota-used-bytes'} % RFC4331
          {'DAV:',resourcetype},
          {'DAV:',supportedlock} % class 2 compliancy
         ],
    %% properties depending on file type
    P2 = case F#file_info.type of
             directory when C==windows ->
                 [
                   {'DAV:',childcount} % Microsoft extension
                 ];
             %% The executable property is only shown for regular files
             regular ->
                 [
                  {'http://apache.org/dav/props/',executable} % Apache extension
                 ];
             _ -> [
                  ]
         end,
    %% compatibility properties
    P3 = case C of
             microsoft -> [
                           %%{'DAV:',iscollection},
                           {'DAV:',isfolder},
                           {'DAV:',ishidden}
                           %%{'DAV:',isreadonly},
                           %%{'DAV:',isroot},
                           %%{'DAV:',name},
                          ];
             _ -> [
                   {'DAV:',displayname}
                  ]
         end,
    P1++P2++P3.

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
    case get(compatibility) of
        microsoft ->
            {404,{'D:displayname',[],[]}};
        _ ->
            Name = filename:basename(R#resource.name),
            Xml = #xmlText{type=cdata,value=Name},
            P = {'D:displayname', [], [Xml]},
            {200, P}
    end;
prop_get({'http://apache.org/dav/props/',executable},_A,R) ->
    F = R#resource.info,
    case F#file_info.type of
        directory -> {404,{executable,
                           [{'xmlns',"http://apache.org/dav/props/"}], []}};
        _ ->
            %% TODO check on extension for Windows?
            X = case F of
                    #file_info{mode=Mode} when Mode band 8#111 =/= 0 -> "T";
                    _ -> "F"
                end,
            {200, {executable, [{'xmlns',"http://apache.org/dav/props/"}], X}}
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
              "httpd/unix-directory";
              %%"text/html";
              %% this should represent the mediatype of a GET on a collection
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
    X = lists:flatten(T),
    C = get(compatibility),
    P = case C of
            microsoft ->
                {'getlastmodified',
                 [{'xmlns:b',
                   "urn:uuid:c2f41010-65b3-11d1-a29f-00aa00c14882/"},
                  {'b:dt',"dateTime.rfc1123"}],[X]};
            _ -> {'D:getlastmodified',[],[X]}
        end,
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
prop_get({'DAV:',lockdiscovery},A,_R) ->
    Path = davpath(A),
    Locks = yaws_runmod_lock:discover(Path),
    case Locks of
        [] ->
            {404,{'D:lockdiscovery',[],[]}};
        _ ->
            ActiveLocks =
                [{'D:activelock',[],
                  [{'D:lockscope',[],[prop_get_format(scope,Lock#lock.scope)]},
                   {'D:locktype',[],[prop_get_format(type,Lock#lock.type)]},
                   {'D:depth',[],[prop_get_format(depth,Lock#lock.depth)]},
                   {'D:owner',[],[prop_get_format(owner,Lock#lock.owner)]},
                   {'D:timeout',[],[prop_get_format(timeout,Lock#lock.timeout)]},
                   {'D:locktoken',[],[prop_get_format(locktoken,Lock#lock.id)]},
                   {'D:lockroot',[],[prop_get_format(lockroot,Lock#lock.path)]}
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
prop_get({'',_P},_A,_R) ->
    throw(400);
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
%%prop_set({'http://apache.org/dav/props/',executable},_A,R,_V) ->
%%   {501,{P,[{'xmlns',NS}],[]}}; % Not yet implemented
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
prop_get_format(_Something,_) ->
    ?DEBUG(" did not expect ~p here ~n",[_Something]),
    throw(500).


%% --------------------------------------------------------
%% Resource mapping
%%

davname(A) ->
    A#arg.server_path.

davpath(A) ->
    filename:join(A#arg.docroot,["./",A#arg.server_path]).

%% davresource0/1 - get resources with depth 0
davresource0(A) ->
    Name = davname(A),
    Path = davpath(A),
    case file:read_file_info(Path) of
        {ok, F} when (F#file_info.type == directory) or
                     (F#file_info.type == regular) ->
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
        (Info#file_info.type == regular) or
        (Info#file_info.type == directory) ->
            Resource = #resource {name = Ref, info = Info},
            davresource1(_A,Path,Coll,Rest,[Resource|Result]);
        true ->
            davresource1(_A,Path,Coll,Rest,Result)
    end.


%% --------------------------------------------------------
%% Parse additional HTTP headers
%%

h_litmus(A) ->
    Hs = (A#arg.headers)#headers.other,
    case lists:keysearch("X-Litmus", 3, Hs) of
        {value, {_,_,"X-Litmus",_,_Test}} ->
            ?DEBUG("~s - ",[_Test]);
        _ ->
            ok
    end.

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
            ?DEBUG(" TO ~p",[Path]),
            filename:join(A#arg.docroot,["./",Path]);
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
                    erlang:min(Val,?LOCK_LIFETIME);
                _ -> ?LOCK_LIFETIME
            end;
        _ ->
            ?LOCK_LIFETIME
    end.

h_locktoken(A) ->
    Hs = (A#arg.headers)#headers.other,
    C = get(compatibility),
    case lists:keysearch("Lock-Token", 3, Hs) of
        {value, {_,_,"Lock-Token",_,URL}} ->
            case URL of
                %%"<opaquelocktoken:"++Token -> string:left(Token,36);
                "<opaquelocktoken:"++Token ->
                    T = parse_locktoken(Token),
                    check_locktoken_format(T),
                    T;
                "opaquelocktoken:"++Token when C==microsoft -> Token;
                _ -> URL
            end;
        _ ->
            undefined
    end.
parse_locktoken([]) ->
    [];
parse_locktoken([H|_T]) when H==62 ->
    [];
parse_locktoken([H|T]) ->
    [H|parse_locktoken(T)].

check_locktoken_format("DAV:no-lock") ->
    ok;
check_locktoken_format(Token) when length(Token)==36 ->
    ok;
check_locktoken_format(_Token) ->
    throw(423).

%%h_if_match(A,Path) ->
%%    Hs = (A#arg.headers)#headers.other,
%%    case lists:keysearch("If-Match", 3, Hs) of
%%        {value, {_,_,"If-Match",_,Tag}} ->
%%            F = file:read_file_info(Path),
%%            case yaws:make_etag(F) of
%%                Tag -> ok;
%%                _ -> throw(412)
%%            end;
%%        _ ->
%%            ok
%%    end.

h_if_refresh(A,Path) ->
    ?DEBUG(" If(~p)",[Path]),
    _Locks = yaws_runmod_lock:discover(Path),
    Hs = (A#arg.headers)#headers.other,
    case lists:keysearch("If", 3, Hs) of
        {value, {_,_,"If",_,If}} ->
            List = if_parse(If,untagged),
            %%?DEBUG(" ~p",[List]),
            case List of
                [{_Resource,[{true,state,Locktoken}]}] -> Locktoken;
                _ -> throw(412)
            end;
        _ ->
            undefined
    end.

h_if(A,Path) ->
    ?DEBUG(" If(~p)",[Path]),
    Locks = yaws_runmod_lock:discover(Path),
    Hs = (A#arg.headers)#headers.other,
    {_L,I} = case lists:keysearch("If", 3, Hs) of
            {value, {_,_,"If",_,If}} ->
                List = if_parse(If,untagged),
                Value = if_eval(A,Locks,List),
                {List,Value};
            _ ->
                {[],undefined}
        end,
    ?DEBUG(" -> ~p (~p)",[I,length(Locks)]),
    ?DEBUG(" If-header ~p (~p) evaluated to ~p~n",[_L,length(Locks),I]),
    case I of
        undefined when length(Locks)>0 -> throw(423);
        %%false when length(Locks)>0 -> throw(412);
        false -> throw(412);
        _ -> ok
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
                 "opaquelocktoken:"++Token ->
                    check_locktoken_format(Token),
                    Token;
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
                 untagged -> davpath(A);
                 _ ->
                    Url = yaws_api:parse_url(Resource),
                    filename:join(A#arg.docroot,["./",Url#url.path])
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
    {ok, F} = file:read_file_info(filename:join(A#arg.docroot,Target)),
    E = yaws:make_etag(F),
    Result1 = (E==Ref),
    Valid1 = Valid,
    Result1 andalso if_eval_condition(T,Result1,Valid1,A,Target,Locks).

%% if_eval_locktoken(Target,Token,Locktokens) -> true|false
if_eval_locktoken(_Target,_Token,[]) ->
    false;
%%if_eval_locktoken(_Target,"DAV:no-lock",[]) ->
%%    true;
if_eval_locktoken(Target,Token,[H|T]) ->
    ((H#lock.path == Target) and (H#lock.id == Token))
        orelse if_eval_locktoken(Target,Token,T).


%% --------------------------------------------------------
%% Parsing of XML elements (RFC4918)
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
-define(IS_PROPERTYUPDATE(X),
        #xmlElement{expanded_name = {'DAV:',propertyupdate}} = X).
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
    [allprop];
parse_propfind([?IS_PROP(H)|_T], _R) when length(?CONTENT(H))==0 ->
    [allprop]; % NetDrive uses empty <prop> element instead of <allprop>
parse_propfind([?IS_PROP(H)|T], _R) ->
    Props = parse_prop(?CONTENT(H)),
    parse_propfind(T, Props);
parse_propfind([_H|T], R) ->
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
            %% check on supported namespaces:
            %% - http://www.w3.org/TR/RC-xml-names#dt-prefix
            %% - although strict, not very forgiving towards clients
            %%NS = H#xmlElement.namespace,
            %%case NS#xmlNamespace.default of
            %%    "" ->
            %%        throw(400);
            %%    _ -> ok
            %%end,
            Value = case H#xmlElement.content of
                        [C] when is_record(C,xmlText) -> C#xmlText.value;
                        _ -> ""
                    end,
            parse_prop(T,[{H#xmlElement.expanded_name,Value}|L]);
        _ ->
            parse_prop(T,L)
    end;
parse_prop([], L) ->
    L.

parse_lockinfo([]) ->
    #lock{};
parse_lockinfo(L) ->
    case catch xmerl_scan:string(L, [{namespace_conformant, true}]) of
        {?IS_LOCKINFO(X),_} ->
            parse_lockinfo(?CONTENT(X),#lock{});
        _Z ->
            throw(400)
    end.
parse_lockinfo([?IS_LOCKSCOPE(H)|T], D) ->
    X = parse_lockscope(?CONTENT(H)),
    parse_lockinfo(T,D#lock{scope=X});
parse_lockinfo([?IS_LOCKTYPE(H)|T], D) ->
    X = parse_locktype(?CONTENT(H)),
    parse_lockinfo(T,D#lock{type=X});
parse_lockinfo([?IS_OWNER(H)|T], D) ->
    X = parse_owner(?CONTENT(H)),
    parse_lockinfo(T,D#lock{owner=X});
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
    status(Status,[],[]).
status(Status,Response) ->
    status(Status,[],Response).
status(Status,Headers,{xml,Response}) ->
    Xml = xml_expand(Response),
    status(Status,Headers,{content, "application/xml; charset=\"utf-8\"", Xml});
status(Status,Headers,Response) ->
    ?DEBUG(" -> ~p~n",[Status]),
    H = case get(compatibility) of
            microsoft -> [{header,{"MS-Author-Via","DAV"}}|Headers];
            _ -> Headers
        end,
    [{status, Status},{header,{"DAV","1, 2, 3"}}|H] ++ [Response].

xml_expand(L) ->
    xml_expand(L, "utf-8").
xml_expand(L, Cset) ->
    Prolog = ["<?xml version=\"1.0\" encoding=\"",Cset,"\" ?>"],
    %%Xml = xmerl:export_simple(L,xmerl_xml,[{prolog,Prolog}]),
    %% MS requires \r\n at end of every XML response
    case get(compatibility) of
        microsoft ->
            [Prolog,yaws_appmod_dav:export(L),"\r\n"];
        _ ->
            [Prolog,yaws_appmod_dav:export(L)]
    end.

%% --------------------------------------------------------
%% XML output (xmlerl_xml does not support CDATA)
%%

export([]) ->
    [];
export([#xmlComment{}|T]) -> % for now I skip comments
    export(T);
export([#xmlText{type=text, value=Text}|T]) ->
    [export_text(Text),export(T)];
export([#xmlText{type=cdata, value=Text}|T]) ->
    ["<![CDATA[",Text,"]]>",export(T)];
export([#xmlElement{name=Name,attributes=Attrs,content=Content}|T]) ->
    export([{Name,Attrs,Content}|T]);
export([{Name,Attrs,Content}|T]) when is_atom(Name)->
    Tag = atom_to_list(Name),
    export([{Tag,Attrs,Content}|T]);
export([{Tag,Attrs,[]}|T]) when is_list(Tag)->
    ["<",Tag,export_attrs(Attrs)," />",export(T)];
export([{Tag,Attrs,Content}|T]) when is_list(Tag)->
    ["<",Tag,export_attrs(Attrs),">",
     export_content(Content),"</",Tag,">",export(T)].

export_content([]) ->
    "";
export_content([H|T]) when is_tuple(H) -> % tuples are XML records
    export([H|T]);
export_content([H]) when is_number(H) ->
    integer_to_list(H);
export_content([H]) when is_atom(H) ->
    atom_to_list(H);
export_content(L) ->
    L.

export_attrs([]) ->
    [];
export_attrs([{Name,Value}|T]) ->
    [" ",export_id(Name),"=\"",export_id(Value),"\"",export_attrs(T)];
export_attrs([Attr|T]) ->
    [" \"",export_id(Attr),"\"",export_attrs(T)].

export_id(Id) when is_atom(Id) ->
    atom_to_list(Id);
export_id(Id) when is_number(Id) ->
    integer_to_list(Id);
export_id(Id) when is_list(Id) ->
    Id.


