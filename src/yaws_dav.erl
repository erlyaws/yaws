-module(yaws_dav).
%%%-------------------------------------------------------------------
%%% Created : 15 May 2005 by Tobbet <tobbe@tornkvist.org>
%%% Desc.   : WebDav specifics.
%%%-------------------------------------------------------------------
-export([parse_xml/1, xml_expand/1, xml_expand/2]).

-include("../include/yaws_dav.hrl").
-include_lib("xmerl/include/xmerl.hrl").


-define(elog(X,Y), error_logger:info_msg("*elog ~p:~p: " X,
					[?MODULE, ?LINE | Y])).


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
parse_dav(X) ->
    %%?elog("parse_dav: GOT ~p~n", [X]),
    {error, "parse_dav"}.  % FIXME , webdav (tobbe)


parse_propfind([?IS_PROP(H)|T], R) ->
    Prop = parse_prop(?CONTENT(H)),
    parse_propfind(T, R#propfind{prop = Prop});
parse_propfind([H|T], R) ->
    %%?elog("parse_propfind: ~p~n",[H]),
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


