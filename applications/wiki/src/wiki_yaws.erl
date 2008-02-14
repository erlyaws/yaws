%    -*- Erlang -*- 
%    File:        wiki_yaws.erl  (~jb/work/wiki/src/wiki_yaws.erl)
%    Author:        Johan Bevemyr
%    Created:        Thu Jun 27 22:26:49 2002
%    Purpose:   Yaws support utilities

-module('wiki_yaws').
-author('jb@son.bevemyr.com').

-export([get_path_prefix/1, parse_multipost/1, parse_post/2,
         call_with_multi/3, call_with_query/3, call_with_post/3,
         call_wiki/3, call_with_multiquery/3]).

-include("../../../include/yaws_api.hrl").


get_path_prefix(UrlPath) ->
    %% search for initial path part
    case string:rchr(UrlPath, $/) of
        0 ->
            UrlPath;
        N ->
            lists:sublist(UrlPath, N)
    end.

 
parse_multipost(Arg) ->
    case yaws_api:parse_multipart_post(Arg) of
        {result, PostList} when Arg#arg.state == undefined->
            {done, parse_post(PostList,[])};
        {result, PostList} ->
            Params = Arg#arg.state++PostList,
            {done, parse_post(Params,[])};
        {cont, Cont, Res} when Arg#arg.state == undefined ->
            {get_more, Cont, Res};
        {cont, Cont, Res} ->
            {get_more, Cont, Arg#arg.state ++ Res}
    end.

parse_post([], Acc) -> Acc;
parse_post([{head, {Name, Opts}}|Rest], Acc) ->
    parse_post(Rest, [{to_string(Name), "", Opts}|Acc]);
parse_post([{body, Data}|Rest], [{Name, Value, Opts}|Acc]) ->
    parse_post(Rest, [{to_string(Name), Value++Data, Opts}|Acc]);
parse_post([{part_body, Data}|Rest], [{Name, Value, Opts}|Acc]) ->
    parse_post(Rest, [{to_string(Name), Value++Data, Opts}|Acc]);
parse_post([{Name, Value}|Rest], Acc) ->
    parse_post(Rest, [{to_string(Name), Value, []}|Acc]).

to_string(Atom) when atom(Atom) ->
    atom_to_list(Atom);
to_string(String) ->
    String.

call_with_multi(M, F, Arg) ->
    case parse_multipost(Arg) of
        {done, Params} ->
            WikiRoot      = filename:dirname(Arg#arg.fullpath),
            {abs_path, P} = (Arg#arg.req)#http_request.path,
            Path          = yaws_api:url_decode(P),
            Prefix        = wiki_yaws:get_path_prefix(Path),
            M:F(Params, WikiRoot, Prefix);
        {get_more, Cont, State} ->
            {get_more, Cont, State}
    end.

call_with_multiquery(M, F, Arg) ->
    case parse_multipost(Arg) of
        {done, Params} ->
            WikiRoot      = filename:dirname(Arg#arg.fullpath),
            {abs_path, P} = (Arg#arg.req)#http_request.path,
            Path          = yaws_api:url_decode(P),
            Prefix        = wiki_yaws:get_path_prefix(Path),
            QueryArgs     = yaws_api:parse_query(Arg),
            QParams       = [{N,V,[]} || {N,V} <- QueryArgs],
            M:F(QParams++Params, WikiRoot, Prefix);
        {get_more, Cont, State} ->
            {get_more, Cont, State}
    end.

call_with_post(M, F, Arg) ->
    QueryArgs     = yaws_api:parse_post(Arg),
    Params        = [{N,V,[]} || {N,V} <- QueryArgs],
    WikiRoot      = filename:dirname(Arg#arg.fullpath),
    {abs_path, P} = (Arg#arg.req)#http_request.path,
    Path          = yaws_api:url_decode(P),
    Prefix        = wiki_yaws:get_path_prefix(Path),
    M:F(Params, WikiRoot, Prefix).

call_with_query(M, F, Arg) ->
    QueryArgs     = yaws_api:parse_query(Arg),
    Params        = [{N,V,[]} || {N,V} <- QueryArgs],
    WikiRoot      = filename:dirname(Arg#arg.fullpath),
    {abs_path, P} = (Arg#arg.req)#http_request.path,
    Path          = yaws_api:url_decode(P),
    Prefix        = wiki_yaws:get_path_prefix(Path),
    M:F(Params, WikiRoot, Prefix).

call_wiki(M, F, Arg) ->
    WikiRoot      = filename:dirname(Arg#arg.fullpath),
    {abs_path, P} = (Arg#arg.req)#http_request.path,
    Path          = yaws_api:url_decode(P),
    Prefix        = wiki_yaws:get_path_prefix(Path),
    M:F([], WikiRoot, Prefix).
