%%----------------------------------------------------------------------
%%% File    : yaws_api.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 24 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_api).
-author('klacke@hyber.org').

%% -compile(export_all).


-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include("yaws_debug.hrl").


-export([parse_post_data/1]).


-export([parse_query/1, parse_post/1, parse_multipart_post/1]).
-export([code_to_phrase/1, ssi/2, redirect/1]).
-export([setcookie/2, setcookie/3, setcookie/4, setcookie/5]).
-export([pre_ssi_files/2,  pre_ssi_string/1, pre_ssi_string/2,
	 set_content_type/1,
	 htmlize/1, htmlize_char/1, f/2, fl/1]).
-export([find_cookie_val/2, secs/0, 
	 url_decode/1, 
	 url_encode/1, parse_url/1, parse_url/2, format_url/1]).
-export([get_line/1, mime_type/1]).
-export([stream_chunk_deliver/2, stream_chunk_deliver_blocking/2,
	 stream_chunk_end/1]).
-export([new_cookie_session/1,
	 cookieval_to_opaque/1,
	 print_cookie_sessions/0,
	 replace_cookie_session/2, delete_cookie_session/1]).
-export([getconf/0, setconf/2, set_status_code/1, reformat_header/1]).

-export([set_trace/1,
	 set_tty_trace/1,
	 set_access_log/1]).

-export([ehtml_expand/1, ehtml_expander/1, ehtml_apply/2,
	ehtml_expander_test/0]).

-import(lists, [map/2, flatten/1, reverse/1]).

%% these are a bunch of function that are useful inside
%% yaws scripts


parse_post_data(Arg) ->

    yaws_log:infolog("Warning Warning !!!! function "
		    "yaws_api:parse_post_data will be removed ", []),

    
    Headers = Arg#arg.headers,
    Req = Arg#arg.req,
    case lists:keysearch('Content-Type', 3, Headers#headers.other) of
	{value, {_,_,_,_,"multipart/form-data"++Line}} ->
	    case Arg#arg.cont of
		{cont, Cont} ->
		    parse_multipart(
		      binary_to_list(un_partial(Arg#arg.clidata)),
		      {cont, Cont});
		undefined ->
		    LineArgs = parse_arg_line(Line),
		    {value, {_, Boundary}} =
			lists:keysearch(boundary, 1, LineArgs),
		    parse_multipart(
		      binary_to_list(un_partial(Arg#arg.clidata)), Boundary)
	    end;
	_ ->
	    case Req#http_request.method of
		'POST' ->
		    parse_post_data_urlencoded(un_partial(Arg#arg.clidata));
		_ ->
		    %% kinda weird default bahaviour here
		    parse_post_data_urlencoded(Arg#arg.querydata)
	    end
    end.
	    




%% parse the command line query data
parse_query(Arg) ->
    D = Arg#arg.querydata,
    if
	D == [] ->
	    yaws_log:errlog("Tried to parse_query with "
			    "no query data ",[]),
	    [];
	true ->
	    parse_post_data_urlencoded(D)
    end.


%% parse url encoded POST data
parse_post(Arg) ->
    D = Arg#arg.clidata,
    Req = Arg#arg.req,
    case Req#http_request.method of
	'POST' ->
	    if
		D == [] ->
		    yaws_log:errlog("Tried to parse_post with "
				   "no POST data ",[]),
		    [];
		true ->
		    parse_post_data_urlencoded(D)
	    end;
	Other ->
	    yaws_log:errlog("Can't parse post body if get a ~p",[Other]),
	    []
    end.

	    


%
% Changed implementation of multipart form data. There is a new config
% parameter called
%
%      partial_post_size
%
% which if set to an integer value
% will cause the content of the post content to be sent to the out/1
% function in chunks of this size.
%
% It is possible to get the server to maintain a state on behalf of the
% out/1 user by returning {get_more, Cont, State}.
%
% 
% yaws_api:parse_multipart_post/1 will return either:
% 
% {cont, Cont, Res} where Res is new result(s) from this segment. This
% indicates that there is more data to come and the out/1 function
% should return {get_more, Cont, User_state} where User_state might
% usefully be a File Descriptor.
%
% or {result, Res} if this is the last (or only) segment.
% 
% Res is a list of {header, Header} | {part_body, Binary} | {body, Binary}
% 
% Example usage could be:
% 
% <erl>
% 
% out(A) ->
%        case yaws_api:parse_multipart_post(A) of
%             {cont, Cont, Res} ->
%                    St = handle_res(A, Res),
%                    {get_more, Cont, St};
%             {result, Res} ->
%                    handle_res(A, Res),
%                    {html, f("<pre>Done </pre>",[])}
%        end.
% 
% handle_res(A, [{head, Name}|T]) ->
%      io:format("head:~p~n",[Name]),
%      handle_res(A, T);
% handle_res(A, [{part_body, Data}|T]) ->
%      io:format("part_body:~p~n",[Data]),
%      handle_res(A, T);
% handle_res(A, [{body, Data}|T]) ->
%      io:format("body:~p~n",[Data]),
%      handle_res(A, T);
% handle_res(A, []) ->
%      io:format("End_res~n").
% 
% </erl>



parse_multipart_post(Arg) ->
    H = Arg#arg.headers,
    CT = H#headers.content_type,
    Req = Arg#arg.req,
    case Req#http_request.method of
	'POST' ->
	    case CT of
		undefined ->
		    yaws_log:errlog("Can't parse multipart if we "
				   "have no Content-Type header",[]),
		    [];
		"multipart/form-data"++Line ->
		    case Arg#arg.cont of
			{cont, Cont} ->
			    parse_multipart(
			      binary_to_list(un_partial(Arg#arg.clidata)),
			      {cont, Cont});
			undefined ->
			    LineArgs = parse_arg_line(Line),
			    {value, {_, Boundary}} =
				lists:keysearch(boundary, 1, LineArgs),
			    parse_multipart(
			      binary_to_list(un_partial(Arg#arg.clidata)), 
			      Boundary)
		    end;
		Other ->
		    yaws_log:errlog("Can't parse multipart if we "
				   "find no multipart/form-data",[]),
		    []
	    end;
	Other ->
	    yaws_log:errlog("Can't parse multipart if get a ~p",[Other]),
	    []
    end.

		



un_partial({partial, Bin}) ->
    Bin;
un_partial(Bin) ->
    Bin.



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
     {{list_to_atom(yaws:funreverse(Key, {yaws, to_lowerchar})),
       lists:reverse(Value)}, Rest}.


%

isolate_arg(Str) -> isolate_arg(Str, []).

isolate_arg([$:,$ |T], L) -> {yaws:funreverse(L, {yaws, to_lowerchar}), T};
isolate_arg([H|T], L)     -> isolate_arg(T, [H|L]).



%
%% Stateful parser of multipart data - allows easy re-entry
% States are header|body|boundary|is_end

parse_multipart(Data, St) ->
    case parse_multi(Data, St) of
	{cont, St2, Res} ->
	    {cont, {cont, St2}, lists:reverse(Res)};
	{result, Res} ->
	    {result, lists:reverse(Res)}
    end.

% Re-entry
parse_multi(Data, {cont, {boundary, Start_data, PartBoundary, Acc, {Possible,Boundary}}}) ->
    parse_multi(boundary, Start_data++Data, PartBoundary, Acc, [], {Possible++Data,Boundary});
parse_multi(Data, {cont, {State, Start_data, Boundary, Acc, Tmp}}) ->
    parse_multi(State, Start_data++Data, Boundary, Acc, [], Tmp);

% Initial entry point
parse_multi(Data, Boundary) ->
    B1 = "\r\n--"++Boundary,
    D1 = "\r\n"++Data,
    parse_multi(boundary, D1, B1, start, [], {D1, B1}).

parse_multi(header, "\r\n\r\n"++Body, Boundary, Acc, Res, Tmp) ->
    Header = do_header(lists:reverse(Acc)),
    parse_multi(body, Body, Boundary, [], [{head, Header}|Res], Tmp);
parse_multi(header, "\r\n\r", Boundary, Acc, Res, Tmp) ->
    {cont, {header, "\r\n\r", Boundary, Acc, Tmp}, Res};
parse_multi(header, "\r\n", Boundary, Acc, Res, Tmp) ->
    {cont, {header, "\r\n", Boundary, Acc, Tmp}, Res};
parse_multi(header, "\r", Boundary, Acc, Res, Tmp) ->
    {cont, {header, "\r", Boundary, Acc, Tmp}, Res};
parse_multi(header, [H|T], Boundary, Acc, Res, Tmp) ->
    parse_multi(header, T, Boundary, [H|Acc], Res, Tmp);
parse_multi(header, [], Boundary, Acc, Res, Tmp) ->
    {cont, {header, [], Boundary, Acc, Tmp}, Res};

parse_multi(body, [B|T], [B|T1], Acc, Res, _Tmp) ->
    parse_multi(boundary, T, T1, Acc, Res, {[B|T], [B|T1]}); % store in case no match
parse_multi(body, [H|T], Boundary, Acc, Res, Tmp) ->
    parse_multi(body, T, Boundary, [H|Acc], Res, Tmp);
parse_multi(body, [], Boundary, [], Res, Tmp) ->  % would be empty partial body result
    {cont, {body, [], Boundary, [], Tmp}, Res};
parse_multi(body, [], Boundary, Acc, Res, Tmp) ->	% make a partial body result
    {cont, {body, [], Boundary, [], Tmp}, [{part_body, lists:reverse(Acc)}|Res]};

parse_multi(boundary, [B|T], [B|T1], Acc, Res, Tmp) ->
    parse_multi(boundary, T, T1, Acc, Res, Tmp);
parse_multi(boundary, [_H|_T], [_B|_T1], start, Res, {[D|T2], Bound}) -> % false alarm
    parse_multi(body, T2, Bound, [D], Res, []);
parse_multi(boundary, [_H|_T], [_B|_T1], Acc, Res, {[D|T2], Bound}) -> % false alarm
    parse_multi(body, T2, Bound, [D|Acc], Res, []);
parse_multi(boundary, [], [B|T1], Acc, Res, Tmp) -> % run out of body
    {cont, {boundary, [], [B|T1], Acc, Tmp}, Res};
parse_multi(boundary, [], [], start, Res, {_, Bound}) ->
    {cont, {is_end, [], Bound, [], []}, Res};
parse_multi(boundary, [], [], Acc, Res, {_, Bound}) ->
    {cont, {is_end, [], Bound, [], []}, [{body, lists:reverse(Acc)}|Res]};
parse_multi(boundary, [H|T], [], start, Res, {_, Bound}) -> % matched whole boundary!
    parse_multi(is_end, [H|T], Bound, [], Res, []);
parse_multi(boundary, [H|T], [], Acc, Res, {_, Bound}) -> % matched whole boundary!
    parse_multi(is_end, [H|T], Bound, [], [{body, lists:reverse(Acc)}|Res], []);

parse_multi(is_end, "--"++_, _Boundary, _Acc, Res, _Tmp) ->
    {result, Res};
parse_multi(is_end, "-", Boundary, Acc, Res, Tmp) ->
    {cont, {is_end, "-", Boundary, Acc, Tmp}, Res};
parse_multi(is_end, "\r\n"++Next, Boundary, _Acc, Res, _Tmp) ->
    parse_multi(header, Next, Boundary, [], Res, []);
parse_multi(is_end, "\r", Boundary, Acc, Res, Tmp) ->
    {cont, {is_end, "\r", Boundary, Acc, Tmp}, Res}.


do_header(Head) ->
    {ok, Fields} = regexp:split(Head, "\r\n"),
    Header = lists:map(fun isolate_arg/1, Fields),
    {value, {_,"form-data"++Line}} =
	lists:keysearch("content-disposition", 1, Header),
    Parameters = parse_arg_line(Line),
    {value, {_,Name}} = lists:keysearch(name, 1, Parameters),
    {list_to_atom(Name), Parameters}.



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
    {html, ["<br><br>\n<div class=\"", Class, "\"> <pre>\n", 
	    htmlize_l(Str),
	    "\n</pre></div>\n<br>\n\n"]}.
    
    
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
    <<>>;
htmlize(List) when list(List) ->
    htmlize_l(List).

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
htmlize_l([X|Tail], Acc) when integer(X) ->
    htmlize_l(Tail, [X|Acc]);
htmlize_l([X|Tail], Acc) when binary(X) ->
    X2 = htmlize_l(binary_to_list(X)),
    htmlize_l(Tail, [X2|Acc]);
htmlize_l([X|Tail], Ack) when list(X) ->
    X2 = htmlize_l(X),
    htmlize_l(Tail, [X2|Ack]).



secs() ->
    {MS, S, _} = now(),
    (MS * 1000000) + S.



setcookie(Name, Value) ->
    {header, {set_cookie, f("~s=~s;", [Name, Value])}}.

setcookie(Name, Value, Path) ->
     {header, {set_cookie, f("~s=~s; path=~s", [Name, Value, Path])}}.

setcookie(Name, Value, Path, Expire) ->
    setcookie(Name, Value, Path,  Expire, [], []).

setcookie(Name, Value, Path, Expire, Domain) ->
    setcookie(Name, Value, Path, Expire, Domain,[]).

setcookie(Name, Value, Path, Expire, Domain, _Secure) ->
    SetDomain = if Domain == [] -> "";
	           true -> "Domain="++Domain++";"
	        end,
    SetExpire = if Expire == [] -> "";
	           true -> "Expires=\""++Expire++"\";"
                end,
    SetPath = if Path == [] -> "/";
                 true -> Path
              end,
    {header, {set_cookie, f("~s=~s; Version=\"1\";~s~sPath=~s",
			    [Name,Value,SetDomain,SetExpire,SetPath])}}.


%% This function can be passed the cookie we get in the Arg#arg.headers.cookies
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
%%
%% Does not unescape the query string - that has to wait until it is
%% parsed, otherwise we can unescape '&', '=', etc too early and make
%% it unparsable. -luke

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
url_decode([$?|T]) ->
    %% Don't decode the query string here, that is parsed separately.
    [$?|T];
url_decode([H|T]) ->
    [H |url_decode(T)];
url_decode([]) ->
    [].



url_encode([H|T]) ->
    if
	H >= $a, $z >= H ->
	    [H|url_encode(T)];
	H >= $A, $Z >= H ->
	    [H|url_encode(T)];
	H >= $0, $9 >= H ->
	    [H|url_encode(T)];
	H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
	    [H|url_encode(T)];
	true ->
	    case yaws:integer_to_hex(H) of
		[X, Y] ->
		    [$%, X, Y | url_encode(T)];
		[X] ->
		    [$%, 0, X | url_encode(T)]
	    end
    end;

url_encode([]) ->
    [].



redirect(Url) -> [{redirect, Url}].

is_nb_space(X) ->
    lists:member(X, [$\s, $\t]).
    

% ret: {line, Line, Trail} | {lastline, Line, Trail}

get_line(L) ->    
    get_line(L, []).
get_line("\r\n\r\n" ++ Tail, Cur) ->
    {lastline, lists:reverse(Cur), Tail};
get_line("\r\n" ++ Tail, Cur) ->
    case is_nb_space(hd(Tail)) of
	true ->  %% multiline ... continue 
	    get_line(Tail, [$\n, $\r | Cur]);
	false ->
	    {line, lists:reverse(Cur), Tail}
    end;
get_line([H|T], Cur) ->
    get_line(T, [H|Cur]).



mime_type(FileName) ->
    {_, MT} = mime_types:t(filename:extension(FileName)),
    MT.


%% Asynchronously delivery
stream_chunk_deliver(YawsPid, Data) ->
    YawsPid  ! {streamcontent, Data}.

%% Synchronous (on ultimate gen_tcp:send) delivery
%% Returns: ok | {error, Rsn}
stream_chunk_deliver_blocking(YawsPid, Data) ->
    Ref = erlang:monitor(process, YawsPid),
    YawsPid  ! {streamcontent_with_ack, self(), Data},
    receive
	{YawsPid, streamcontent_ack} ->
	    erlang:demonitor(Ref),
	    %% flush incase a DOWN message was sent before the demonitor call
	    receive
		{'DOWN', Ref, _, _, _} ->
		    ok
	    after 0 ->
		    ok
	    end;
	{'DOWN', Ref, _, _, Info} ->
	    {error, {ypid_crash, Info}}
    end.

stream_chunk_end(YawsPid) ->
    YawsPid ! endofstreamcontent.



%% Return new cookie string
new_cookie_session(Opaque) ->
    yaws_session_server:new_session(Opaque).

%% as returned in #ysession.cookie
cookieval_to_opaque(CookieVal) ->
    yaws_session_server:cookieval_to_opaque(CookieVal).

print_cookie_sessions() ->
    yaws_session_server:print_sessions().

replace_cookie_session(Cookie, NewOpaque) ->
    yaws_session_server:replace_session(Cookie, NewOpaque).

delete_cookie_session(Cookie) ->
    yaws_session_server:delete_session(Cookie).


%% to be used in embedded mode, make it possible
%% to pass a config to yaws from another data source
%% than /etc/yaws.conf, for example from a database

setconf(GC, Groups) ->
    case gen_server:call(yaws_server, {setconf, GC, Groups}) of
	ok ->
	    yaws_log:setdir(GC#gconf.logdir, Groups),
	    case GC#gconf.trace of
		false ->
		    ok;
		{true, What} ->
		     yaws_log:open_trace(What)
	    end;
	E ->
	    E
    end.

%% return {ok, GC, Groups}.
getconf() ->
    gen_server:call(yaws_server, getconf).


lmap(F, [H|T]) ->
    [lists:map(F, H) | lmap(F, T)];
lmap(_, []) ->
    [].


%% interactively turn on|off tracing
set_trace(Val) ->
    case lists:member(Val, [traffic, http, false]) of
	true ->
	    {ok, GC, Groups} = getconf(),
	    Tval = case Val of
		       http ->
			   {true, http};
		       traffic ->
			   {true, traffic};
		       false ->
			   false
		   end,
	    setconf(GC#gconf{trace = Tval}, Groups);
	_ ->
	    io:format(
	      "Usage: set_trace(true | false, traffic | http | access)",[])
    end.



set_access_log(Bool) ->
    {ok, GC, Groups} = getconf(),
    Groups2 = lmap(fun(SC) ->
			   SC#sconf{access_log = Bool}
		   end, Groups),
    setconf(GC, Groups2).




%% interactively turn on|off tracing to the tty (as well)
%% typically useful in embedded mode
set_tty_trace(Bool) ->
    gen_server:call(yaws_log, {trace_tty, Bool}).



set_status_code(Code) ->
    {status, Code}.




%% returns [ Header1, Header2 .....]
reformat_header(H) ->
    lists:zf(fun({Hname, Str}) ->
		     I =  lists:flatten(io_lib:format("~s: ~s",[Hname, Str])),
		     {true, I};
		(undefined) ->
		     false
	     end, 
	     [
	      if H#headers.connection == undefined ->
		      undefined;
		 true ->
		      {"Connection", H#headers.connection}
	      end,
	      
	      if H#headers.accept == undefined ->
		      undefined;
		 true ->
		      {"Accept", H#headers.accept}
	      end,
	      if H#headers.host == undefined ->
		      undefined;
		 true ->
		      {"Host", H#headers.host}
	      end,
	      if H#headers.if_modified_since == undefined ->
		      undefined;
		 true ->
		      {"If-Modified-Since", H#headers.if_modified_since}
	      end,
	      if H#headers.if_match == undefined ->
		      undefined;
		 true ->
		      {"If-Match", H#headers.if_match}
	      end,
	      if H#headers.if_none_match == undefined ->
		      undefined;
		 true ->
		      {"If-None-Match", H#headers.if_none_match}
	      end,
	      
	      
	      if H#headers.if_range == undefined ->
		      undefined;
		 true ->
		      {"If-Range", H#headers.if_range}
	      end,
	      if H#headers.if_unmodified_since == undefined ->
		      undefined;
		 true ->
		      {"If-Unmodified-Since", H#headers.if_unmodified_since}
	      end,
	      if H#headers.range == undefined ->
		      undefined;
		 true ->
		      {"Range", H#headers.range}
	      end,
	      if H#headers.referer == undefined ->
		      undefined;
		 true ->
		      {"Referer", H#headers.referer}
	      end,
	      if H#headers.user_agent == undefined ->
		      undefined;
		 true ->
		      {"User-Agent", H#headers.user_agent}
	      end,
	      if H#headers.accept_ranges == undefined ->
		      undefined;
		 true ->
		      {"Accept-Ranges", H#headers.accept_ranges}
	      end,
	      if H#headers.cookie == undefined ->
		      undefined;
		 true ->
		      {"Cookie", H#headers.cookie}
	      end,
	      if H#headers.keep_alive == undefined ->
		      undefined;
		 true ->
		      {"Keep-Alive", H#headers.keep_alive}
	      end,
	      if H#headers.content_length == undefined ->
		      undefined;
		 true ->
		      {"Content-Length", H#headers.content_length}
	      end,
	      if H#headers.content_type == undefined ->
		      undefined;
		 true ->
		      {"Content-Type", H#headers.content_type}
	      end,
	      
	      if H#headers.authorization == undefined ->
		      undefined;
		 true ->
		      {"Authorization", H#headers.authorization}
	      end]
	     ) ++
     lists:map(
       fun({http_header,_,K,_,V}) ->
	       lists:flatten(io_lib:format("~s: ~s",[K,V]))
       end, H#headers.other).




set_content_type(MimeType) ->
    {header, {content_type, MimeType}}.


%% returns a #url{} record
parse_url(Str) ->
    parse_url(Str, strict).

parse_url(Str, Strict) ->
    case Str of
	"http://" ++ Rest ->
	    parse_url(host, Strict, #url{scheme = http}, Rest, []);
	"https://" ++ Rest ->
	    parse_url(host, Strict, #url{scheme = https}, Rest, []);	
	"ftp://" ++ Rest ->
	    parse_url(host, Strict, #url{scheme = ftp}, Rest, []);
	"file://" ++ Rest ->
	    parse_url(host, Strict, #url{scheme = file}, Rest, []);
	_ when Strict == sloppy ->
	    parse_url(host, Strict, #url{scheme = http}, Str, [])
    end.


parse_url(host, Strict, U, Str, Ack) ->
    case Str of
	[] ->
	    U#url{host = lists:reverse(Ack), 
		  path = "/"
		 };
	[$/|Tail] ->
	    U2 = U#url{host = lists:reverse(Ack)},
	    parse_url(path, Strict, U2, Tail,"/");
	[$:|T] ->
	    U2 = U#url{host = lists:reverse(Ack)},
	    parse_url(port, Strict, U2, T,[]);
	[H|T] ->
	    parse_url(host, Strict, U, T, [H|Ack])
    end;
parse_url(port, Strict, U, Str, Ack) ->
    case Str of
	[] ->
	    U#url{port = list_to_integer(lists:reverse(Ack)),
		  path = "/"};
	[$/|T] ->
	    U2 = U#url{port = list_to_integer(lists:reverse(Ack))},
	    parse_url(path, Strict, U2, T,"/");
	[H|T] ->
	    parse_url(port, Strict, U,T,[H|Ack])
    end;
parse_url(path, Strict, U, Str, Ack) ->
    case Str of
	[] ->
	    U#url{path = lists:reverse(Ack)};
	[$?|T] ->
	    U#url{path = lists:reverse(Ack),
		  querypart = T};
	[H|T] ->
	    parse_url(path, Strict, U, T, [H|Ack])
    end.

		
format_url(Url) when record(Url, url) ->
    [
     atom_to_list(Url#url.scheme), "://",
     Url#url.host,
     if
	 Url#url.port == undefined ->
	     [];
	 true  ->
	     [$: | integer_to_list(Url#url.port)]
     end,
     Url#url.path,
     if
	 Url#url.querypart == [] ->
	     [];
	 true ->
	     [$?|Url#url.querypart]
     end
    ].




%% ------------------------------------------------------------
%% simple erlang term representation of HTML:
%% EHTML = [EHTML] | {Tag, Attrs, Body} | {Tag, Attrs} | {Tag} |
%%         binary() | character()
%% Tag 	 = atom()
%% Attrs = [{Key, Value}]  or {EventTag, {jscall, FunName, [Args]}}
%% Key 	 = atom()
%% Value = string()
%% Body  = EHTML
ehtml_expand(Ch) when Ch >= 0, Ch =< 255 ->
    yaws_api:htmlize_char(Ch);
ehtml_expand(Bin) when binary(Bin) ->
    yaws_api:htmlize(Bin);
ehtml_expand({Tag}) ->
    ehtml_expand({Tag,[]});
ehtml_expand({pre_html, X}) ->
    X;
ehtml_expand({Tag, Attrs}) ->
    ["<", atom_to_list(Tag), ehtml_attrs(Attrs), ">\n"];
ehtml_expand({Tag, Attrs, Body}) when atom(Tag) ->
    Ts = atom_to_list(Tag),
    ["<",Ts, ehtml_attrs(Attrs),">",
     ehtml_expand(Body),
     "</", Ts, ">\n"];
ehtml_expand([H|T]) ->
    [ehtml_expand(H) | ehtml_expand(T)];
ehtml_expand([]) ->
    [].


ehtml_attrs([]) ->
    [];
ehtml_attrs([{Name, Value} | Tail]) ->
    ValueString = if atom(Value) -> [$",atom_to_list(Value),$"];
		     list(Value) -> [$",Value,$"]
		  end,
    [[$ |atom_to_list(Name)], [$=|ValueString]| ehtml_attrs(Tail)].


%% ------------------------------------------------------------
%% ehtml_expander/1: an EHTML optimizer
%%
%% This is an optimization for generating the same EHTML multiple times with
%% only small differences, by using fast re-usable templates that contain
%% variables. The variables are atoms starting with a dollar sign, like
%% '$myvar'. There are two functions: ehtml_expander/1 to create an optimized
%% EHTML template, then ehtml_apply/2 takes a template and a dictionary of
%% variable values and generates the actual HTML.
%%
%% If you are spending a lot of time regenerating similar EHTML fragments then
%% this is for you.
%%
%% Variables can appear in three places:
%% - As a body element, where you would normally have a tag. The values of
%%   these variables are expanded as EHTML.
%% - As the name or value of an attribute. The values of these variables are
%%   strings.
%% - As the CDR of an attribute list. The values of these variables are
%%   key-value lists of more attributes.
%%
%% See ehtml_expander_test/0 for an example.
%%
%% The approach is inspired by the way that Yaws already treats .yaws files,
%% and the article ``A Hacker's Introduction To Partial Evaluation'' by Darius
%% Bacon (cool guy), http://www.lisp-p.org/htdocs/peval/peval.cgi
%%
%% (For now I flatter myself that this is some kind of partial evaluator, but
%% I don't really know :-) -luke)

ehtml_expander(X) ->
    ehtml_expander_compress(flatten(ehtml_expander(X, [], [])), []).

%% Returns a deep list of text and variable references (atoms)

%% Text
ehtml_expander(Ch, Before, After) when Ch >= 0, Ch =< 255 ->
    ehtml_expander_done(yaws_api:htmlize_char(Ch), Before, After);
ehtml_expander(Bin, Before, After) when binary(Bin) ->
    ehtml_expander_done(yaws_api:htmlize(Bin), Before, After);
ehtml_expander({pre_html, X}, Before, After) ->
    ehtml_expander_done(X, Before, After);
%% Tags
ehtml_expander({Tag}, Before, After) ->
    ehtml_expander({Tag, []}, Before, After);
ehtml_expander({Tag, Attrs}, Before, After) ->
    ehtml_expander_done(["<",atom_to_list(Tag),
			 ehtml_attrs_expander(Attrs),">\n"],
			Before,
			After);
ehtml_expander({Tag, Attrs, Body}, Before, After) ->
    ehtml_expander(Body,
		   [["<",atom_to_list(Tag), ehtml_attrs_expander(Attrs), ">"] |
		    Before],
		   ["</",atom_to_list(Tag),">\n" | After]);
%% Variable references
ehtml_expander(Var, Before, After) when atom(Var) ->
    [reverse(Before), {ehtml, ehtml_var_name(Var)}, After];
%% Lists
ehtml_expander([H|T], Before, After) ->
    ehtml_expander(T, [ehtml_expander(H, [], []) | Before], After);
ehtml_expander([], Before, After) ->
    ehtml_expander_done("", Before, After).


%% Expander for attributes. The attribute name and value can each be a
%% variable reference.
ehtml_attrs_expander([]) ->
    "";
ehtml_attrs_expander([{Var,Val}|T]) ->
    [[" ",
      ehtml_attr_part_expander(Var),
      "=",
      "\"", ehtml_attr_part_expander(Val), "\""]
     | ehtml_attrs_expander(T)];
ehtml_attrs_expander(Var) when atom(Var) ->
    %% Var in the cdr of an attribute list
    [{ehtml_attrs, ehtml_var_name(Var)}].

ehtml_attr_part_expander(A) when atom(A) ->
    case ehtml_var_p(A) of
	true  -> {preformatted, ehtml_var_name(A)};
	false -> atom_to_list(A)
    end;
ehtml_attr_part_expander(S) when list(S) ->
    S.

ehtml_expander_done(X, Before, After) ->
    [reverse([X|Before]), After].

%% Compress an EHTML expander, converting all adjacent bits of text into
%% binaries.
%% Returns: [binary() | {ehtml, Var} | {preformatted, Var}, {ehtml_attrs, Var}]
%% Var = atom()
ehtml_expander_compress([Tag|T], Acc) when tuple(Tag) ->
    [list_to_binary(reverse(Acc)), Tag | ehtml_expander_compress(T, [])];
ehtml_expander_compress([], Acc) ->
    [list_to_binary(reverse(Acc))];
ehtml_expander_compress([H|T], Acc) when integer(H) ->
    ehtml_expander_compress(T, [H|Acc]).

%% Apply an expander with the variable bindings in Env.  Env is a list of
%% {VarName, Value} tuples, where VarName is an atom and Value is an ehtml
%% term.
ehtml_apply(Expander, Env) ->
    [ehtml_eval(X, Env) || X <- Expander].

ehtml_eval(Bin, Env) when binary(Bin) ->
    Bin;
ehtml_eval({Type, Var}, Env) ->
    case lists:keysearch(Var, 1, Env) of
	false ->
	    exit({ehtml_unbound, Var});
	{value, {Var, Val}} ->
	    case Type of
		ehtml ->
		    ehtml_expand(Val);
		preformatted ->
		    Val;
		ehtml_attrs ->
		    ehtml_attrs(Val)
	    end
    end.

%% Get the name part of a variable reference.
%% e.g. ehtml_var_name('$foo') -> foo.
ehtml_var_name(A) when atom(A) ->
    case ehtml_var_p(A) of
	true ->
	    list_to_atom(tl(atom_to_list(A)));
	false ->
	    exit({bad_ehtml_var_name, A})
    end.

%% Is X a variable reference? Variable references are atoms starting with $.
ehtml_var_p(X) when atom(X) ->
    hd(atom_to_list(X)) == $$;
ehtml_var_p(_) ->
    false.

ehtml_expander_test() ->
    %% Expr is a template containing variables.
    Expr = {html, [{title, '$title'}],
	    {body, [],
	     [{h1, [], '$heading'},
	      '$text']}},
    %% Expand is an expander that can be used to quickly generate the HTML
    %% specified in Expr.
    Expand = ehtml_expander(Expr),
    %% Bs{1,2} are lists of variable bindings to fill in the gaps in the
    %% template. We can reuse the template on many sets of bindings, and this
    %% is much faster than doing a full ehtml of the whole page each time.
    Bs1 = [{title, "First page"},
	  {heading, "Heading"},
	  {text, {pre_html, "<b>My text!</b>"}}],
    Bs2 = [{title, "Second page"},
	  {heading, "Foobar"},
	  {text, {b, [], "My text again!"}}],
    %% Page1 and Page2 are generated from the template. They are I/O lists
    %% (i.e. deep lists of strings and binaries, ready to ship)
    Page1 = ehtml_apply(Expand, Bs1),
    Page2 = ehtml_apply(Expand, Bs2),
    %% We return the two pages as strings, plus the actual expander (which is
    %% an "opaque" data structure, but maybe interesting to see.)
    {binary_to_list(list_to_binary(Page1)),
     binary_to_list(list_to_binary(Page2)),
     Expand}.

