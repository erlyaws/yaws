%%%----------------------------------------------------------------------
%%% File    : yaws_api.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 24 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_api).
-author('klacke@hyber.org').

-compile(export_all).
-include("yaws.hrl").

%% these are a bunch of function that are useful inside
%% yaws scripts




parse_post_data(Arg) ->
    Headers = Arg#arg.headers,
    case lists:keysearch('Content-Type', 3, Headers#headers.other) of
	{value, {_,_,_,_,"multipart/form-data"++Line}} ->
	    LineArgs = parse_arg_line(Line),
	    {value, {_, Boundary}} = lists:keysearch(boundary, 1, LineArgs),
	    parse_multipart(Boundary, binary_to_list(Arg#arg.clidata));
	_ ->
	    parse_post_data_urlencoded(Arg#arg.querydata)
    end.
	    
	    
%

parse_arg_line(Line) ->
    parse_arg_line(Line, []).

parse_arg_line([],Acc) -> Acc;
parse_arg_line([$ |Line], Acc) ->
    parse_arg_line(Line, Acc);
parse_arg_line([$;|Line], Acc) ->
    {KV,Rest} = parse_arg_key(Line, [], []),
    parse_arg_line(Rest, [KV|Acc]).

%

parse_arg_key([], Key, Value) ->
    make_parse_line_reply(Key, Value, []);
parse_arg_key([$;|Line], Key, Value) ->
    make_parse_line_reply(Key, Value, [$;|Line]);
parse_arg_key([$ |Line], Key, Value) ->
    parse_arg_key(Line, Key, Value);
parse_arg_key([$=|Line], Key, Value) ->
    parse_arg_value(Line, Key, Value, false, false);
parse_arg_key([C|Line], Key, Value) ->
    parse_arg_key(Line, [C|Key], Value).
    
%
% We need to deal with quotes and initial spaces here.
% parse_arg_value(String, Key, ValueAcc, InQuoteBool, InValueBool)
%

parse_arg_value([], Key, Value, _, _) ->
    make_parse_line_reply(Key, Value, []);
parse_arg_value([$\\,$"|Line], Key, Value, Quote, Begun) ->
    parse_arg_value(Line, Key, [$"|Value], Quote, Begun);
parse_arg_value([$"|Line], Key, Value, false, _) ->
    parse_arg_value(Line, Key, Value, true, true);
parse_arg_value([$"|Line], Key, Value, true, _) ->
    make_parse_line_reply(Key, Value, Line);
parse_arg_value([$;|Line], Key, Value, false, _) ->
    make_parse_line_reply(Key, Value, [$;|Line]);
parse_arg_value([$ |Line], Key, Value, false, true) ->
    make_parse_line_reply(Key, Value, Line);
parse_arg_value([$ |Line], Key, Value, false, false) ->
    parse_arg_value(Line, Key, Value, false, false);
parse_arg_value([C|Line], Key, Value, Quote, _) ->
    parse_arg_value(Line, Key, [C|Value], Quote, true).


%

make_parse_line_reply(Key, Value, Rest) ->
    {{list_to_atom(httpd_util:to_lower(lists:reverse(Key))),
      lists:reverse(Value)}, Rest}.


%

isolate_arg(Str) -> isolate_arg(Str, []).

isolate_arg([$:,$ |T], L) -> {httpd_util:to_lower(lists:reverse(L)), T};
isolate_arg([H|T], L)     -> isolate_arg(T, [H|L]).



%

parse_multipart(Boundary, Args) ->
    Parts = split_boundary(Boundary, [$\r,$\n|Args]),
    Parsed = parse_parts(Parts),
    F = fun({Header,Body}) ->
		{value, {_,"form-data"++Line}} =
		    lists:keysearch("content-disposition", 1, Header),
		Parameters = parse_arg_line(Line),
		{value, {_,Name}} = lists:keysearch(name, 1, Parameters),
		{list_to_atom(Name), Body}
	end,
    lists:map(F, Parsed).

parse_parts([]) ->   [];
parse_parts([[]|Ps]) ->
    parse_parts(Ps);
parse_parts([P|Ps]) ->
    {Head, Body} = split_head_body(P, []),
    {ok, Fields} = regexp:split(Head, "\r\n"),
    Header = lists:map(fun isolate_arg/1, Fields),
    [{Header, Body} | parse_parts(Ps)].


split_head_body([], Acc) ->
    {lists:reverse(Acc), []};
split_head_body("\r\n\r\n"++Body, Acc) ->
    {lists:reverse(Acc), Body};
split_head_body([C|P], Acc) ->
    split_head_body(P, [C|Acc]).


%

split_boundary(Boundary, Line) ->    
    case string:str(Line, "\r\n--"++Boundary) of
 	0 ->
 	    [Line];
 	N ->
 	    Entry = string:substr(Line, 1, N-1),
 	    Rest = string:substr(Line, N+4+length(Boundary)),
 	    case Rest of
 		"--"++_ ->
 		    [Entry];  
 		"\r\n"++Next ->
 		    [Entry|split_boundary(Boundary,Next)]
 	    end
     end.


%% parse POST data when ENCTYPE is unset or
%% Content-type: application/x-www-form-urlencoded
%% Bin is the content of ARG#arg.clidata
%% the alternative is
%% Content-type: multipart/form-data; boundary=-------------------7cd1d6371ec
%% which is used for file upload

parse_post_data_urlencoded(Bin) ->
    parse_post_data_urlencoded(Bin, ['ALLSTRINGS']).

parse_post_data_urlencoded(Bin, Spec) ->
    do_parse_spec(Bin, Spec, nokey, [], key).

%% Spec is a typelist of the types we expect
%% acceptable types are

%% int
%% float
%% string
%% ip
%% binary
%% checkbox
%% 'ALLSTRINGS' 

%% special value ['ALLSTRINGS'] can be used in order to denote that
%% the remainder of the args are all strings

%% It will return a [{Key, Value}] list from the post data
%% with the same length as the Spec or EXIT
%% special value undefined is reserverd for non set fields
%% Key wil always be a regular atom.


do_parse_spec(<<$%, Hi:8, Lo:8, Tail/binary>>, Spec, Last, Cur, State) ->
    Hex = yaws:hex_to_integer([Hi, Lo]),
    do_parse_spec(Tail, Spec, Last, [ Hex | Cur],  State);

do_parse_spec(<<$&, Tail/binary>>, Spec, _Last , Cur,  key) ->
    [{mkkey(Cur), undefined} |
     do_parse_spec(Tail, Spec, nokey, [], key)];  %% cont keymode

do_parse_spec(<<$&, Tail/binary>>, Spec, Last, Cur, value) ->
    [S|Ss] = tail_spec(Spec),
    V = {Last, coerce_type(S, Cur)},
    [V | do_parse_spec(Tail, Ss, nokey, [], key)];

do_parse_spec(<<$+, Tail/binary>>, Spec, Last, Cur,  State) ->
    do_parse_spec(Tail, Spec, Last, [$\s|Cur], State);

do_parse_spec(<<$=, Tail/binary>>, Spec, _Last, Cur, key) ->
    do_parse_spec(Tail, Spec, mkkey(Cur), [], value); %% change mode

do_parse_spec(<<H:8, Tail/binary>>, Spec, Last, Cur, State) ->
    do_parse_spec(Tail, Spec, Last, [H|Cur], State);
do_parse_spec(<<>>, Spec, Last, Cur, _State) ->
    [S|_Ss] = tail_spec(Spec),
    [{Last, coerce_type(S, Cur)}];
do_parse_spec(undefined,_,_,_,_) ->
    [];
do_parse_spec(QueryList, Spec, Last, Cur, State) when list(QueryList) ->
    do_parse_spec(list_to_binary(QueryList), Spec, Last, Cur, State).




tail_spec(['ALLSTRINGS']) ->
    [string, 'ALLSTRINGS'];
tail_spec(L) ->
     L.

coerce_type(_, []) ->
    undefined;
coerce_type(int, Str) ->
    list_to_integer(lists:reverse(Str));
coerce_type(float, Str) ->
    list_to_float(lists:reverse(Str));
coerce_type(string, Str) ->
    lists:reverse(Str);
coerce_type(checkbox, "no") ->
    on;
coerce_type(checkbox, Str) ->
    io:format("XX ~s~n", [Str]),
    off;
coerce_type(ip, _Str) ->
    exit(nyi_ip);
coerce_type(binary, Str) ->
    list_to_binary(lists:reverse(Str)).

mkkey(S) ->
    list_to_atom(lists:reverse(S)).









code_to_phrase(100) -> "Continue";
code_to_phrase(101) -> "Switching Protocols ";
code_to_phrase(200) -> "OK";
code_to_phrase(201) -> "Created";
code_to_phrase(202) -> "Accepted";
code_to_phrase(203) -> "Non-Authoritative Information";
code_to_phrase(204) -> "No Content";
code_to_phrase(205) -> "Reset Content";
code_to_phrase(206) -> "Partial Content";
code_to_phrase(300) -> "Multiple Choices";
code_to_phrase(301) -> "Moved Permanently";
code_to_phrase(302) -> "Found";
code_to_phrase(303) -> "See Other";
code_to_phrase(304) -> "Not Modified";
code_to_phrase(305) -> "Use Proxy";
code_to_phrase(306) -> "(Unused)";
code_to_phrase(307) -> "Temporary Redirect";
code_to_phrase(400) -> "Bad Request";
code_to_phrase(401) -> "Unauthorized";
code_to_phrase(402) -> "Payment Required";
code_to_phrase(403) -> "Forbidden";
code_to_phrase(404) -> "Not Found";
code_to_phrase(405) -> "Method Not Allowed";
code_to_phrase(406) -> "Not Acceptable";
code_to_phrase(407) -> "Proxy Authentication Required";
code_to_phrase(408) -> "Request Timeout";
code_to_phrase(409) -> "Conflict";
code_to_phrase(410) -> "Gone";
code_to_phrase(411) -> "Length Required";
code_to_phrase(412) -> "Precondition Failed";
code_to_phrase(413) -> "Request Entity Too Large";
code_to_phrase(414) -> "Request-URI Too Long";
code_to_phrase(415) -> "Unsupported Media Type";
code_to_phrase(416) -> "Requested Range Not Satisfiable";
code_to_phrase(417) -> "Expectation Failed";
code_to_phrase(500) -> "Internal Server Error";
code_to_phrase(501) -> "Not Implemented";
code_to_phrase(502) -> "Bad Gateway";
code_to_phrase(503) -> "Service Unavailable";
code_to_phrase(504) -> "Gateway Timeout";
code_to_phrase(505) -> "HTTP Version Not Supported".



%%
%% server side include
%%

ssi(DocRoot, Files) ->
    L = lists:map(fun(F) ->
			  case file:read_file([DocRoot ++ [$/|F]]) of
			      {ok, Bin} ->
				  Bin;
			      {error, Reason} ->
				  io_lib:format("Cannot include file ~p: ~p", 
						[F, Reason])
			  end
		  end, Files),
    {html, L}.


%% include pre
pre_ssi_files(DocRoot, Files) ->
    {html, L} = ssi(DocRoot, Files),
    pre_ssi_string(L).

pre_ssi_string(Str) ->
    pre_ssi_string(Str, "box").

pre_ssi_string(Str, Class) ->
    {html, ["<br><br>\n<div class=\"", Class, "\"> <pre>", 
	    htmlize_l(Str),
	    "</pre></div>\n<br>\n\n"]}.
    
    
%% convenience

f(Fmt, Args) ->
    io_lib:format(Fmt, Args).


fl([Fmt, Arg | Tail]) ->
    [f(Fmt, Arg) | fl(Tail)];
fl([]) ->
    [].



%% htmlize  
htmlize(<<Char, Tail/binary>>) ->
    case htmlize_char(Char) of
	Char ->
	    <<Char, (htmlize(Tail))/binary>>;
        Bin ->		
            <<Bin/binary, (htmlize(Tail))/binary>>
    end;
htmlize(<<>>) ->			 
    <<>>.

htmlize_char($>) ->
    <<"&gt;">>;
htmlize_char($<) ->
    <<"&lt;">>;
htmlize_char($&) ->
    <<"&amp;">>;
htmlize_char(X) ->
    X.


%% htmlize list (usually much more efficient than above)
htmlize_l(List) ->
    htmlize_l(List, []).

htmlize_l([], Acc) -> lists:reverse(Acc);
htmlize_l([$>|Tail], Acc) ->
    htmlize_l(Tail, [$;,$t,$g,$&|Acc]);
htmlize_l([$<|Tail], Acc) ->
    htmlize_l(Tail, [$;,$t,$l,$&|Acc]);
htmlize_l([$&|Tail], Acc) ->
    htmlize_l(Tail, [$;,$p,$m,$a,$&|Acc]);
htmlize_l([X|Tail], Acc) ->
    htmlize_l(Tail, [X|Acc]).


secs() ->
    {MS, S, _} = now(),
    (MS * 1000000) + S.



setcookie(Name, Value) ->
    {header, f("Set-Cookie: ~s=~s;", [Name, Value])}.

setcookie(Name, Value, Path) ->
     {header, f("Set-Cookie: ~s=~s; path=~s", [Name, Value, Path])}.

setcookie(Name, Value, Path, Expire) ->
    setcookie(Name, Value, Path,  Expire, [], []).

setcookie(Name, Value, Path, Expire, Domain) ->
    setcookie(Name, Value, Path, Expire, Domain,[]).

setcookie(_Name, _Value, _Path, _Expire, _Domain, _Secure) ->
    exit(nyi).

	

	    


%% This function can be passed the cookie we get in the Arg#arg.cookies
%% to search for a specific cookie 
%% return [] if not found
%%        Str if found
%% if serveral cookies with the same name are passed fron the browser,
%% only the first match is returned


find_cookie_val(_Cookie, []) ->
    [];
find_cookie_val(Cookie, [FullCookie | FullCookieList]) ->
    case find_cookie_val2(Cookie, FullCookie) of
	[] ->
	    find_cookie_val(Cookie, FullCookieList);
	Val ->
	    Val
    end.

find_cookie_val2(Cookie, FullCookie) ->    
    case lists:prefix(Cookie, FullCookie) of
	false when FullCookie == [] ->
	    [];
	false ->
	    find_cookie_val2(Cookie, tl(FullCookie));
	true ->
	    case lists:dropwhile(fun(X) -> X /= $= end, FullCookie) of
		[] ->
		    [];
		List ->
		    find_cookie_val3(tl(List),[])
	    end
    end.


find_cookie_val3([], Ack) ->
    lists:reverse(Ack);
find_cookie_val3([$;|_], Ack) ->
    lists:reverse(Ack);
find_cookie_val3([H|T], Ack) ->
    find_cookie_val3(T, [H|Ack]).


%% This one needs to deal with Microsoft style encoding.
%% 
%% FIXME: This scheme is derived from what MSIE generates. I've
%% not tried to find the proper RFC.

url_decode([$%, $C, $2, $%, Hi, Lo | Tail]) ->
    Hex = yaws:hex_to_integer([Hi, Lo]),
    [Hex | url_decode(Tail)];
url_decode([$%, $C, $3, $%, Hi, Lo | Tail]) when Hi > $9 ->
    Hex = yaws:hex_to_integer([Hi+4, Lo]),
    [Hex | url_decode(Tail)];
url_decode([$%, $C, $3, $%, Hi, Lo | Tail]) when Hi < $A ->
    Hex = yaws:hex_to_integer([Hi+4+7, Lo]),
    [Hex | url_decode(Tail)];
url_decode([$%, Hi, Lo | Tail]) ->
    Hex = yaws:hex_to_integer([Hi, Lo]),
    [Hex | url_decode(Tail)];
url_decode([H|T]) ->
    [H |url_decode(T)];
url_decode([]) ->
    [].

redirect(Url) -> [{redirect, Url}].

