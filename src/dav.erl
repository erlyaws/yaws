-module(dav).
%%%-------------------------------------------------------------------
%%% Created : 17 May 2005 by Torbjorn Tornkvist <tobbe@struts.tornkvist.org>
%%% Desc.   : WebDav-appmod file system interface.
%%%
%%% Test it, example: cadaver http://struts:8000/dav
%%%
%%%
%%% NOTE: This is work in heavy progress!!
%%%
%%%-------------------------------------------------------------------
-export([out/1]).

-include("../include/yaws_api.hrl").
-include("../include/yaws_dav.hrl").

-define(elog(X,Y), error_logger:info_msg("*elog ~p:~p: " X,
					[?MODULE, ?LINE | Y])).

-define(IS_PROPFIND(A), (A#arg.req)#http_request.method == "PROPFIND").
-define(IS_MKCOL(A), (A#arg.req)#http_request.method == "MKCOL").


out(A) when ?IS_PROPFIND(A) ->
    propfind(A);
out(A) when ?IS_MKCOL(A) ->
    create_directory(A);
out(A) ->
    out403().

propfind(A) ->
    ?elog("propfind: appmoddata=~p~n", [A#arg.appmoddata]),
    %% Depth:
    %%  If '0', then no members should be returned.
    %%  If '1', then members one level down should be included in the reply.
    %%  If 'infinity', then all members, recursively, should be included.
    case depth(A) of
	0 ->
	    ?elog("propfind: Depth=0~n", []),
	    B = depth_zero(A),
	    out207(B);
	1 ->
	    %% FIXME should get root dir from yaws.conf
	    L = string:tokens(os:cmd("ls /home/tobbe/docroot"), "\n"), 
	    ?elog("propfind: Depth=1 , L=~p~n", [L]),
	    Url = "http://struts/dav/",                                % FIXME
	    F = fun(File) -> response_entry(File, Url) end,
	    Responses = lists:map(F, L),
	    MultiStatus = [{multistatus, [{'xmlns',"DAV:"}], Responses}],
	    B = yaws_dav:xml_expand(MultiStatus),
	    out207(B)
    end.

%%% FIXME should get a proper file_info entry here
response_entry(F, Url) ->  % File
    {response, [],
     [{href, [], [Url ++ F]},
      {propstat, [], 
       [{prop, [],
	 [{name, [], [F]},
	  %%{creationdate, [], [(F#file.date)#date.string]},    % FIXME
	  %%{getlastmodified, [], [(F#file.date)#date.string]}, % FIXME
	  {getcontentlength, [], ["123"]},
	  {resourcetype, [], []}
	  %%{ishidden, [], [bool2lnum(F#file.is_hidden)]}]},
	  ]},
	{status, [],    % Status 1
	 ["HTTP/1.1 200 OK"]}]}]};
%%
response_entry(F, Url)  -> % Dir
    {response, [],
     [{href, [], [Url ++ F ++ "/"]},
      {propstat, [], 
       [{prop, [],
	 [{name, [], [F]},
	  %%{creationdate, [], [(F#file.date)#date.string]},    % FIXME
	  %%{getlastmodified, [], [(F#file.date)#date.string]}, % FIXME
	  {resourcetype, [], 
	   [{collection, [], []}]}
	  %%{ishidden, [], [bool2lnum(F#file.is_hidden)]}]},
	  ]},
	{status, [],    % Status 1
	 ["HTTP/1.1 200 OK"]}]}]}.


create_directory(A) ->
    tbd.


depth_zero(A) ->
    Url = "http://struts/dav" ++ A#arg.appmoddata,
    ["<?xml version=\"1.0\" encoding=\"utf-8\" ?>",
     "  <D:multistatus xmlns:D=\"DAV:\">",
     "    <D:response>",
     "      <D:href>", Url, "</D:href>",
     "        <D:propstat>",
     "          <D:prop>",
     "            <D:getcontentlength>0</D:getcontentlength>",
     "            <D:getlastmodified>17 May 2005</D:getlastmodified>",
     "            <D:resourcetype>",
     "              <D:collection/>",
     "            </D:resourcetype>",
     "          </D:prop>",
     "          <D:status>HTTP/1.1 200 MultiStatus</D:status>",
     "        </D:propstat>",
     "    </D:response>",
     "  </D:multistatus>"].



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



out207(L) ->
    outXXX(207, L).

outXXX(XXX, L) ->
    [{status, XXX},
     {header, {content_type, "text/xml; charset=\"utf-8\""}},
     {html, L}].

    
out403() ->
    [{status, 403}].
