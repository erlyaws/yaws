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
-include_lib("kernel/include/file.hrl").


-define(elog(X,Y), error_logger:info_msg("*elog ~p:~p: " X,
					[?MODULE, ?LINE | Y])).


-define(IS_PROPFIND(A), (A#arg.req)#http_request.method == "PROPFIND").
-define(IS_MKCOL(A), (A#arg.req)#http_request.method == "MKCOL").
-define(IS_GET(A), (A#arg.req)#http_request.method == "GET").


out(A) when ?IS_PROPFIND(A) ->
    propfind(A);
out(A) when ?IS_GET(A) ->
    "/dav"++Path = A#arg.appmoddata,
    [{redirect_local, Path}];
out(A) when ?IS_MKCOL(A) ->
    create_directory(A);
out(_A) ->
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
	    Response = depth_zero(A),
	    MultiStatus = [{multistatus, [{'xmlns',"DAV:"}], Response}],
	    B = yaws_dav:xml_expand(MultiStatus),
	    out207(B);
	1 ->
	    %% FIXME should get root dir from yaws.conf
	    Entries = get_entries(A),
	    ?elog("propfind: Depth=1 , length(Entries)=~p~n", [length(Entries)]),
	    Url = "http://struts/dav/",                                % FIXME
	    F = fun(Finfo) -> response_entry(Finfo, Url) end,
	    Responses = lists:map(F, Entries),
	    MultiStatus = [{multistatus, [{'xmlns',"DAV:"}], Responses}],
	    B = yaws_dav:xml_expand(MultiStatus),
	    out207(B)
    end.

date_string({{Y,M,D}, {Hr,Min,Sec}}) ->
    lists:concat([D, " ", month(M), " ", Y, " ", Hr, ":", Min, ":", Sec]).



get_entries(A) ->
    Path = A#arg.docroot ++ A#arg.appmoddata,
    case file:read_file_info(Path) of
	{ok, Dir} when Dir#file_info.type == directory ->
	    {ok, L} = file:list_dir(Path),
	    [{Name,element(2,file:read_file_info(Path++"/"++Name))} || Name <- L];
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

create_directory(A) ->
    tbd.


depth_zero(A) ->
    Path = A#arg.docroot ++ A#arg.appmoddata,
    Url = "http://struts/dav" ++ A#arg.appmoddata,
    Name = file_name(Path),
    {ok, F} =  file:read_file_info(Path),
    ?elog("server_parh=~p~n", [A#arg.server_path]),
    [{response, [],
      [{href, [], [Url]},
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
	 {status, [],   
	  ["HTTP/1.1 200 OK"]}]}]}].


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
