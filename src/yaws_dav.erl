-module(yaws_dav).
%%%-------------------------------------------------------------------
%%% Created : 15 May 2005 by Tobbet <tobbe@tornkvist.org>
%%% Modified: 21 Nov 2005 by <mbj@tail-f.com>
%%% Desc.   : WebDav specifics.
%%%   To use, add a line dav = true in the <server>.
%%% TODO: fix more fine-grained permissions
%%%-------------------------------------------------------------------
-export([propfind/1, delete/1, put/2, mkcol/1, move/1, copy/1]).
-export([parse_xml/1, xml_expand/1, xml_expand/2]).

-include("../include/yaws_dav.hrl").
-include("../include/yaws_api.hrl").
-include("../include/yaws.hrl").
-include("yaws_debug.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/file.hrl").


-define(elog(X,Y), error_logger:info_msg("*elog ~p:~p: " X,
                                         [?MODULE, ?LINE | Y])).

delete(A) ->
    Path = davpath(A),
    ?elog("DELETE Path=~p~n", [Path]),
    case rmrf(Path) of
        ok -> out200();
        _  -> out403()
    end.

put(SC, ARG) ->
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
                    Len when integer(PPS) ->
                        Int_len = list_to_integer(Len),
                        if 
                            Int_len == 0 ->
                                ok;
                            PPS < Int_len, CT == multipart ->
                                %% FIXME: handle this
                                {partial,
                                 store_client_data(Fd,CliSock, PPS, SSL)};
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
                        out200();
                    Error ->
                        throw(Error)
                end
            catch
                _:_Err ->
                    ?Debug("PUT error ~p\n", [_Err, TmpName]),
                    file:close(Fd),
                    file:delete(TmpName),
                    out409()
            end;
        _Error ->
            ?Debug("PUT error ~p ~p\n", [_Error, TmpName]),
            out409()
    end.

mkcol(A) ->
    Path = davpath(A),
    case file:make_dir(Path) of
        ok ->
            out201();
        {error, Reason} ->
            ?elog("failed to create dir: ~p , reason: ~p~n", [Path, Reason]),
            out403()
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
                    [{status, 403}];
               DoOverwrite == false,
               ToExsist == true ->
                    [{status, 412}];
               true ->
                    if DoOverwrite == true ->
                            rmrf(To);
                       true ->
                            ok
                    end,
                    OpF(From, To)
            end;
        _ ->
            [{status, 501}]
    end.

do_move(From, To) ->
    case file:rename(From, To) of
        ok ->
            out201();
        _ ->
            case file:copy(From, To) of
                {ok,_} ->
                    ok = file:delete(From),
                    out201();
                {error, Reason} ->
                    ?elog("move from ~p to ~p failed: ~p\n",
                          [From, To, Reason]),
                    out409()
            end
    end.

do_copy(From, To) ->
    case file:copy(From, To) of
        {ok, _} ->
            out201();
        Error ->
            ?elog("move from ~p to ~p failed: ~p\n",
                  [From, To, Error]),
            out409()
    end.

propfind(A) ->
    %% Depth:
    %%  If '0', then no members should be returned.
    %%  If '1', then members one level down should be included in the reply.
    %%  If 'infinity', then all members, recursively, should be included.
    case depth(A) of
        0 ->
            ?elog("propfind: Depth=0~n", []),
            Response = depth_zero(A),
            MultiStatus = [{multistatus, [{'xmlns',"DAV:"}], Response}],
            B = yaws_dav:xml_expand(MultiStatus),
            out207(B);
        1 ->
            Entries = get_entries(A),
            ?elog("propfind: Depth=1 , length(Entries)=~p~n",
                  [length(Entries)]),
            Url = davurl(A),
            F = fun(Finfo) -> response_entry(Finfo, Url) end,
            Responses = lists:map(F, Entries),
            MultiStatus = [{multistatus, [{'xmlns',"DAV:"}], Responses}],
            B = yaws_dav:xml_expand(MultiStatus),
            out207(B)
    end.


date_string({{Y,M,D}, {Hr,Min,Sec}}) ->
    lists:concat([D, " ", month(M), " ", Y, " ", Hr, ":", Min, ":", Sec]).

get_entries(A) ->
    Path = davpath(A),
    case file:read_file_info(Path) of
        {ok, Dir} when Dir#file_info.type == directory ->
            {ok, L} = file:list_dir(Path),
            [{Name, element(2, file:read_file_info(Path ++ "/" ++ Name))} ||
                Name <- L];
        {ok, Else} ->
            [{get_name(Path),Else}]
    end.

%%% FIXME should get a proper file_info entry here
%%
response_entry({Name, F}, Url) when F#file_info.type == directory  -> % Dir
    {response, [],
     [{href, [], [Url ++ Name]},
      {propstat, [], 
       [{prop, [],
         [{name, [], [Name]},
          {creationdate, [], [date_string(F#file_info.ctime)]}, 
          {getlastmodified, [], [date_string(F#file_info.mtime)]},
          {getcontentlength, [], [integer_to_list(F#file_info.size)]},
          {resourcetype, [], 
           [{collection, [], []}]}
          %%{ishidden, [], [bool2lnum(F#file.is_hidden)]}]},
         ]},
        {status, [],    % Status 1
         ["HTTP/1.1 200 OK"]}]}]};
%%
response_entry({Name, F}, Url) when F#file_info.type == regular ->  % File
    {response, [],
     [{href, [], [Url ++ Name]},
      {propstat, [], 
       [{prop, [],
         [{name, [], [Name]},
          {creationdate, [], [date_string(F#file_info.ctime)]}, 
          {getlastmodified, [], [date_string(F#file_info.mtime)]},
          {getcontentlength, [], [integer_to_list(F#file_info.size)]},
          {resourcetype, [], []}
          %%{ishidden, [], [bool2lnum(F#file.is_hidden)]}]},
         ]},
        {status, [],    % Status 1
         ["HTTP/1.1 200 OK"]}]}]};
%%
response_entry(F, _Url) ->
    ?elog("ignoring file: ~p~n", [F]),
    [].


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


depth_zero(A) ->
    Path = davpath(A),
    Url = davurl(A),
    Name = file_name(Path),
    {ok, F} =  file:read_file_info(Path),            % FIXME
    ?elog("server_path=~p~n", [A#arg.server_path]),
    [{response, [],
      [{href, [], [Url]},
       {propstat, [], 
        [{prop, [],
          [{name, [], [Name]},
           {creationdate, [], [date_string(F#file_info.ctime)]}, 
           {getlastmodified, [], [date_string(F#file_info.mtime)]},
           {getcontentlength, [], [integer_to_list(F#file_info.size)]},
           {resourcetype, [], 
            is_collection(F)}
           %%{ishidden, [], [bool2lnum(F#file.is_hidden)]}]},
          ]},
         {status, [],   
          ["HTTP/1.1 200 OK"]}]}]}].

is_collection(F) when F#file_info.type == directory ->
    [{collection, [], []}];
is_collection(_) ->
    [].

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

depth(A) ->
    %%
    %% Look for: {http_header,  _Num, 'Depth', _, Depth}
    %%
    Hs = (A#arg.headers)#headers.other,
    case lists:keysearch("Depth", 3, Hs) of
        {value, {_,_,"Depth",_,Depth}} ->
            to_depth(Depth);
        _ ->
            0
    end.

to_depth("infinity") -> infinity;
to_depth(L) ->
    case catch list_to_integer(L) of
        I when integer(I) -> I;
        _                 -> 0
    end.

xml_expand(L) ->
    xml_expand(L, "utf-8").

xml_expand(L, Cset) ->
    Prolog = ["<?xml version=\"1.0\" encoding=\""++Cset++"\" ?>"],
    xmerl:export_simple(L,xmerl_xml,[{prolog,Prolog}]).


parse_xml([]) -> [];
parse_xml(L) when list(L) ->
    case catch xmerl_scan:string(L, [{namespace_conformant, true}]) of
        {X,_} when record(X, xmlElement) ->
            parse_dav(X);
        _Z ->
            ?elog("to_xml: error ~p~n", [_Z]),
            {error, "xml scanner failed"}
    end.

-define(CONTENT(X), X#xmlElement.content).

-define(IS_PROPFIND(X), #xmlElement{expanded_name = {'DAV:',propfind}} = X).
-define(IS_PROP(X), #xmlElement{expanded_name = {'DAV:',prop}} = X).
-define(IS_NAME(X), #xmlElement{expanded_name = {'DAV:',name}} = X).
-define(IS_PARENTNAME(X), #xmlElement{expanded_name = {'DAV:',parentname}} = X).
-define(IS_HREF(X), #xmlElement{expanded_name = {'DAV:',href}} = X).
-define(IS_ISHIDDEN(X), #xmlElement{expanded_name = {'DAV:',ishidden}} = X).
-define(IS_ISCOLLECTION(X), #xmlElement{expanded_name = {'DAV:',iscollection}} = X).
-define(IS_ISREADONLY(X), #xmlElement{expanded_name = {'DAV:',isreadonly}} = X).
-define(IS_GETCONTENTTYPE(X), #xmlElement{expanded_name = {'DAV:',getcontenttype}} = X).
-define(IS_CONTENTCLASS(X), #xmlElement{expanded_name = {'DAV:',contentclass}} = X).
-define(IS_GETCONTENTLANGUAGE(X), #xmlElement{expanded_name = {'DAV:',getcontentlanguage}} = X).
-define(IS_CREATIONDATE(X), #xmlElement{expanded_name = {'DAV:',creationdate}} = X).
-define(IS_LASTACCESSED(X), #xmlElement{expanded_name = {'DAV:',lastaccessed}} = X).
-define(IS_GETLASTMODIFIED(X), #xmlElement{expanded_name = {'DAV:',getlastmodified}} = X).
-define(IS_GETCONTENTLENGTH(X), #xmlElement{expanded_name = {'DAV:',getcontentlength}} = X).
-define(IS_RESOURCETYPE(X), #xmlElement{expanded_name = {'DAV:',resourcetype}} = X).
-define(IS_ISSTRUCTUREDDOCUMENT(X), #xmlElement{expanded_name = {'DAV:',isstructureddocument}} = X).
-define(IS_DEFAULTDOCUMENT(X), #xmlElement{expanded_name = {'DAV:',defaultdocument}} = X).
-define(IS_DISPLAYNAME(X), #xmlElement{expanded_name = {'DAV:',displayname}} = X).
-define(IS_ISROOT(X), #xmlElement{expanded_name = {'DAV:',isroot}} = X).


parse_dav(?IS_PROPFIND(X)) ->
    parse_propfind(?CONTENT(X), #propfind{});
parse_dav(_X) ->
    %%?elog("parse_dav: GOT ~p~n", [_X]),
    {error, "parse_dav"}.  % FIXME , webdav (tobbe)


parse_propfind([?IS_PROP(H)|T], R) ->
    Prop = parse_prop(?CONTENT(H)),
    parse_propfind(T, R#propfind{prop = Prop});
parse_propfind([_H|T], R) ->
    %%?elog("parse_propfind: ~p~n",[_H]),
    parse_propfind(T, R);
parse_propfind([], R) ->
    R.

parse_prop(L) ->
    parse_prop(L, []).

parse_prop([?IS_NAME(_H)|T], L) ->
    parse_prop(T, [name | L]);
parse_prop([?IS_PARENTNAME(_H)|T], L) ->
    parse_prop(T, [parentname | L]);
parse_prop([?IS_HREF(_H)|T], L) ->
    parse_prop(T, [href | L]);
parse_prop([?IS_ISHIDDEN(_H)|T], L) ->
    parse_prop(T, [ishidden | L]);
parse_prop([?IS_ISCOLLECTION(_H)|T], L) ->
    parse_prop(T, [iscollection | L]);
parse_prop([?IS_ISREADONLY(_H)|T], L) ->
    parse_prop(T, [isreadonly | L]);
parse_prop([?IS_GETCONTENTTYPE(_H)|T], L) ->
    parse_prop(T, [getcontenttype | L]);
parse_prop([?IS_CONTENTCLASS(_H)|T], L) ->
    parse_prop(T, [contentclass | L]);
parse_prop([?IS_GETCONTENTLANGUAGE(_H)|T], L) ->
    parse_prop(T, [getcontentlanguage | L]);
parse_prop([?IS_CREATIONDATE(_H)|T], L) ->
    parse_prop(T, [creationdate | L]);
parse_prop([?IS_LASTACCESSED(_H)|T], L) ->
    parse_prop(T, [lastaccessed | L]);
parse_prop([?IS_GETLASTMODIFIED(_H)|T], L) ->
    parse_prop(T, [getlastmodified | L]);
parse_prop([?IS_GETCONTENTLENGTH(_H)|T], L) ->
    parse_prop(T, [getcontentlength | L]);
parse_prop([?IS_RESOURCETYPE(_H)|T], L) ->
    parse_prop(T, [resourcetype | L]);
parse_prop([?IS_ISSTRUCTUREDDOCUMENT(_H)|T], L) ->
    parse_prop(T, [isstructureddocument | L]);
parse_prop([?IS_DEFAULTDOCUMENT(_H)|T], L) ->
    parse_prop(T, [defaultdocument | L]);
parse_prop([?IS_DISPLAYNAME(_H)|T], L) ->
    parse_prop(T, [displayname | L]);
parse_prop([?IS_ISROOT(_H)|T], L) ->
    parse_prop(T, [isroot | L]);
parse_prop([H|T], L) ->
    ?elog("parse_propfind: NYI ~p~n",[H]),  % FIXME , webdav
    parse_prop(T, L);
parse_prop([], L) ->
    lists:reverse(L).  % preserve order!


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

out207(L) ->
    outXXX(207, L).

outXXX(XXX, L) ->
    [{status, XXX},
     {header, {content_type, "text/xml; charset=\"utf-8\""}},
     {html, L}].

out200() ->
    [{status, 200}].

out201() ->
    [{status, 201}].

out403() ->
    [{status, 403}].

out409() ->
    [{status, 409}].


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
    N = yaws_revproxy:store_chunk_num(CliSock, SSL),
    yaws:setopts(CliSock, [binary, {packet, raw}], SSL),
    if
        N == 0 ->
            _Tmp=yaws:do_recv(CliSock, 2, SSL);%% flush last crnl
        true ->
            B = yaws_revproxy:store_chunk(CliSock, N, 0,SSL),
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
            ?Debug("store_client_data_len: ~p~n", [_Other]),
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

