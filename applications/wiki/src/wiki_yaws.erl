%    -*- Erlang -*- 
%    File:	wiki_yaws.erl  (~jb/work/wiki/src/wiki_yaws.erl)
%    Author:	Johan Bevemyr
%    Created:	Thu Jun 27 22:26:49 2002
%    Purpose:   Yaws support utilities

-module('wiki_yaws').
-author('jb@son.bevemyr.com').

-export([get_path_prefix/1, parse_post_data/1, parse_post/2,
	 call_with_data/3]).

-include("../../../include/yaws_api.hrl").


get_path_prefix(UrlPath) ->
    %% search for initial path part
    case string:rchr(UrlPath, $/) of
	0 ->
	    UrlPath;
	N ->
	    lists:sublist(UrlPath, N)
    end.

 
parse_post_data(Arg) ->
    case yaws_api:parse_post_data(Arg) of
	{result, PostList} ->
	    {done, parse_post(PostList,[])};
	{cont, Cont, Res} when Arg#arg.state == undefined ->
	    {get_more, Cont, Res};
	{cont, Cont, Res} ->
	    {get_more, Cont, Arg#arg.state ++ Res};
	Post ->
	    {done, [{N,V,[]} || {N,V} <- Post]}
    end.

parse_post([], Acc) -> Acc;
parse_post([{head, {Name, Opts}}|Rest], Acc) ->
    parse_post(Rest, [{Name, "", Opts}|Acc]);
parse_post([{body, Value}|Rest], [{Name, _, Opts}|Acc]) ->
    parse_post(Rest, [{Name, Value, Opts}|Acc]);
parse_post([{body_part, Data}|Rest], [{Name, Value, Opts}|Acc]) ->
    parse_post(Rest, [{Name, Value++Data, Opts}|Acc]);
parse_post([{Name, Value}|Rest], Acc) ->
    parse_post(Rest, [{Name, Value, []}|Acc]).


call_with_data(M, F, Arg) ->
    case wiki_yaws:parse_post_data(Arg) of
	{done, Params} ->
	    WikiRoot      = filename:dirname(Arg#arg.fullpath),
	    {abs_path, P} = (Arg#arg.req)#http_request.path,
	    Path          = yaws_api:url_decode(P),
	    Prefix        = wiki_yaws:get_path_prefix(Path),
	    M:F(Params, WikiRoot, Prefix);
	{more, Return} ->
	    Return
    end.
