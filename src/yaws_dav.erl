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
parse_prop([H|T], L) ->
    %%?elog("parse_propfind: ~p~n",[H]),  % FIXME , webdav
    parse_prop(T, L);
parse_prop([], L) ->
    lists:reverse(L).  % preserve order!


