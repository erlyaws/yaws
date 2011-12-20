%%----------------------------------------------------------------------
%%% File    : yaws_api.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose :
%%% Created : 24 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_api).
-author('klacke@hyber.org').

%% -compile(export_all).


-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").



-export([parse_query/1, parse_post/1,
         parse_multipart_post/1, parse_multipart_post/2,
         parse_multipart/2, parse_multipart/3]).
-export([code_to_phrase/1, ssi/2, redirect/1]).
-export([setcookie/2, setcookie/3, setcookie/4, setcookie/5, setcookie/6]).
-export([pre_ssi_files/2,  pre_ssi_string/1, pre_ssi_string/2,
         set_content_type/1,
         htmlize/1, htmlize_char/1, f/2, fl/1]).
-export([find_cookie_val/2, secs/0,
         url_decode/1, url_decode_q_split/1,
         url_encode/1, parse_url/1, parse_url/2, format_url/1,
         format_partial_url/2]).
-export([is_absolute_URI/1]).
-export([path_norm/1, path_norm_reverse/1,
         sanitize_file_name/1]).
-export([get_line/1, mime_type/1]).
-export([stream_chunk_deliver/2, stream_chunk_deliver_blocking/2,
         stream_chunk_end/1]).
-export([stream_process_deliver/2, stream_process_deliver_chunk/2,
         stream_process_deliver_final_chunk/2, stream_process_end/2]).
-export([websocket_send/2]).
-export([new_cookie_session/1, new_cookie_session/2, new_cookie_session/3,
         cookieval_to_opaque/1, request_url/1,
         print_cookie_sessions/0,
         replace_cookie_session/2, delete_cookie_session/1]).

-export([getconf/0,
         setconf/2,
         embedded_start_conf/1, embedded_start_conf/2,
         embedded_start_conf/3, embedded_start_conf/4]).

-export([set_status_code/1, reformat_header/1,
         reformat_request/1, reformat_response/1, reformat_url/1]).

-export([set_trace/1,
         set_tty_trace/1,
         set_access_log/1]).

-export([call_cgi/2, call_cgi/3]).

-export([call_fcgi_responder/1, call_fcgi_responder/2,
         call_fcgi_authorizer/1, call_fcgi_authorizer/2]).

-export([ehtml_expand/1, ehtml_expander/1, ehtml_apply/2,
         ehtml_expander_test/0]).

-export([parse_set_cookie/1, format_set_cookie/1,
         postvar/2, queryvar/2, getvar/2]).

-export([binding/1,binding_exists/1,
         dir_listing/1, dir_listing/2, redirect_self/1]).

-export([arg_clisock/1
         , arg_client_ip_port/1
         , arg_headers/1
         , arg_req/1
         , arg_clidata/1
         , arg_server_path/1
         , arg_querydata/1
         , arg_appmoddata/1
         , arg_docroot/1
         , arg_docroot_mount/1
         , arg_fullpath/1
         , arg_cont/1
         , arg_state/1
         , arg_pid/1
         , arg_opaque/1
         , arg_appmod_prepath/1
         , arg_prepath/1
         , arg_pathinfo/1

         , http_request_method/1
         , http_request_path/1
         , http_request_version/1

         , http_response_version/1
         , http_response_status/1
         , http_response_phrase/1

         , headers_connection/1
         , headers_accept/1
         , headers_host/1
         , headers_if_modified_since/1
         , headers_if_match/1
         , headers_if_none_match/1
         , headers_if_range/1
         , headers_if_unmodified_since/1
         , headers_range/1
         , headers_referer/1
         , headers_user_agent/1
         , headers_accept_ranges/1
         , headers_cookie/1
         , headers_keep_alive/1
         , headers_location/1
         , headers_content_length/1
         , headers_content_type/1
         , headers_content_encoding/1
         , headers_authorization/1
         , headers_transfer_encoding/1
         , headers_x_forwarded_for/1
         , headers_other/1

        ]).


-import(lists, [map/2, flatten/1, reverse/1]).

%% these are a bunch of function that are useful inside
%% yaws scripts

arg_clisock(#arg{clisock = X}) -> X.
arg_client_ip_port(#arg{client_ip_port = X}) -> X.
arg_headers(#arg{headers = X}) -> X.
arg_req(#arg{req = X}) -> X.
arg_clidata(#arg{clidata = X}) -> X.
arg_server_path(#arg{server_path = X}) -> X.
arg_querydata(#arg{querydata = X}) -> X.
arg_appmoddata(#arg{appmoddata = X}) -> X.
arg_docroot(#arg{docroot = X}) -> X.
arg_docroot_mount(#arg{docroot_mount = X}) -> X.
arg_fullpath(#arg{fullpath = X}) -> X.
arg_cont(#arg{cont = X}) -> X.
arg_state(#arg{state = X}) -> X.
arg_pid(#arg{pid = X}) -> X.
arg_opaque(#arg{opaque = X}) -> X.
arg_appmod_prepath(#arg{appmod_prepath = X}) -> X.
arg_prepath(#arg{prepath = X}) -> X.
arg_pathinfo(#arg{pathinfo = X}) ->  X.

http_request_method(#http_request{method = X}) -> X.
http_request_path(#http_request{path = X}) -> X.
http_request_version(#http_request{version = X}) -> X.

http_response_version(#http_response{version = X}) -> X.
http_response_status(#http_response{status = X}) -> X.
http_response_phrase(#http_response{phrase = X}) -> X.

headers_connection(#headers{connection = X}) -> X.
headers_accept(#headers{accept = X}) -> X.
headers_host(#headers{host = X}) -> X.
headers_if_modified_since(#headers{if_modified_since = X}) -> X.
headers_if_match(#headers{if_match = X}) -> X.
headers_if_none_match(#headers{if_none_match = X}) -> X.
headers_if_range(#headers{if_range = X}) -> X.
headers_if_unmodified_since(#headers{if_unmodified_since = X}) -> X.
headers_range(#headers{range = X}) -> X.
headers_referer(#headers{referer = X}) -> X.
headers_user_agent(#headers{user_agent = X}) -> X.
headers_accept_ranges(#headers{accept_ranges = X}) -> X.
headers_cookie(#headers{cookie = X}) -> X.
headers_keep_alive(#headers{keep_alive = X}) -> X.
headers_location(#headers{location = X}) -> X.
headers_content_length(#headers{content_length = X}) -> X.
headers_content_type(#headers{content_type = X}) -> X.
headers_content_encoding(#headers{content_encoding = X}) -> X.
headers_authorization(#headers{authorization = X}) -> X.
headers_transfer_encoding(#headers{transfer_encoding = X}) -> X.
headers_x_forwarded_for(#headers{x_forwarded_for = X}) -> X.
headers_other(#headers{other = X}) -> X.


%% parse the command line query data
parse_query(Arg) ->
    D = Arg#arg.querydata,
    if
        D == [] ->
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
            case D of
                [] -> [];
                _ ->
                    parse_post_data_urlencoded(D)
            end;
        Other ->
            error_logger:error_msg(
              "ERROR: Can't parse post body for ~p requests: URL: ~p",
              [Other, Arg#arg.fullpath]),
            []
    end.


%%
%% Changed implementation of multipart form data. There is a new config
%% parameter called
%%
%%      partial_post_size
%%
%% which if set to an integer value
%% will cause the content of the post content to be sent to the out/1
%% function in chunks of this size.
%%
%% It is possible to get the server to maintain a state on behalf of the
%% out/1 user by returning {get_more, Cont, State}.
%%
%%
%% yaws_api:parse_multipart_post/1 will return either:
%%
%% {cont, Cont, Res} where Res is new result(s) from this segment. This
%% indicates that there is more data to come and the out/1 function
%% should return {get_more, Cont, User_state} where User_state might
%% usefully be a File Descriptor.
%%
%% or {result, Res} if this is the last (or only) segment.
%%
%% Res is a list of {header, Header} | {part_body, Binary} | {body, Binary}
%%
%% Example usage could be:
%%
%% <erl>
%%
%% out(A) ->
%%        case yaws_api:parse_multipart_post(A) of
%%             {cont, Cont, Res} ->
%%                    St = handle_res(A, Res),
%%                    {get_more, Cont, St};
%%             {result, Res} ->
%%                    handle_res(A, Res),
%%                    {html, f("<pre>Done </pre>",[])}
%%        end.
%%
%% handle_res(A, [{head, Name}|T]) ->
%%      io:format("head:~p~n",[Name]),
%%      handle_res(A, T);
%% handle_res(A, [{part_body, Data}|T]) ->
%%      io:format("part_body:~p~n",[Data]),
%%      handle_res(A, T);
%% handle_res(A, [{body, Data}|T]) ->
%%      io:format("body:~p~n",[Data]),
%%      handle_res(A, T);
%% handle_res(A, []) ->
%%      io:format("End_res~n").
%%
%% </erl>

parse_multipart_post(Arg) ->
    parse_multipart_post(Arg, [list]).
parse_multipart_post(Arg, Options) ->
    H = Arg#arg.headers,
    CT = H#headers.content_type,
    Req = Arg#arg.req,
    case Req#http_request.method of
        'POST' ->
            case CT of
                undefined ->
                    error_logger:error_msg("Can't parse multipart if we "
                                           "have no Content-Type header",[]),
                    [];
                "multipart/form-data"++Line ->
                    case Arg#arg.cont of
                        {cont, Cont} ->
                            parse_multipart(
                              un_partial(Arg#arg.clidata),
                              {cont, Cont});
                        undefined ->
                            LineArgs = parse_arg_line(Line),
                            {value, {_, Boundary}} =
                                lists:keysearch(boundary, 1, LineArgs),
                            parse_multipart(
                              un_partial(Arg#arg.clidata),
                              Boundary, Options)
                    end;
                _Other ->
                    error_logger:error_msg("Can't parse multipart if we "
                                           "find no multipart/form-data",[]),
                    []
            end;
        Other ->
            error_logger:error_msg("Can't parse multipart if get a ~p",
                                   [Other]),
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

%%

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

%%
%% We need to deal with quotes and initial spaces here.
%% parse_arg_value(String, Key, ValueAcc, InQuoteBool, InValueBool)
%%

parse_arg_value([], Key, Value, _, _) ->
    make_parse_line_reply(Key, Value, []);
parse_arg_value([$\\,$"|Line], Key, Value, Quote, Begun) ->
    parse_arg_value(Line, Key, [$"|Value], Quote, Begun);
parse_arg_value([$"|Line], Key, Value, false, _) ->
    parse_arg_value(Line, Key, Value, true, true);
parse_arg_value([$"], Key, Value, true, _) ->
    make_parse_line_reply(Key, Value, []);
parse_arg_value([$",$;|Line], Key, Value, true, _) ->
    make_parse_line_reply(Key, Value, [$;|Line]);
parse_arg_value([$;|Line], Key, Value, false, _) ->
    make_parse_line_reply(Key, Value, [$;|Line]);
parse_arg_value([$ |Line], Key, Value, false, true) ->
    make_parse_line_reply(Key, Value, Line);
parse_arg_value([$ |Line], Key, Value, false, false) ->
    parse_arg_value(Line, Key, Value, false, false);
parse_arg_value([C|Line], Key, Value, Quote, _) ->
    parse_arg_value(Line, Key, [C|Value], Quote, true).


%%

make_parse_line_reply(Key, Value, Rest) ->
    {{list_to_atom(yaws:funreverse(Key, fun yaws:to_lowerchar/1)),
      lists:reverse(Value)}, Rest}.


-record(mp_parse_state, {
          state,
          boundary_ctx,
          hdr_end_ctx,
          old_data,
          data_type
         }).

%% Stateful parser of multipart data - allows easy re-entry
parse_multipart(Data, St) ->
    parse_multipart(Data, St, [list]).
parse_multipart(Data, St, Options) ->
    case parse_multi(Data, St, Options) of
        {cont, St2, Res} ->
            {cont, {cont, St2}, lists:reverse(Res)};
        {result, Res} ->
            {result, lists:reverse(Res)}
    end.

%% Reentry point
parse_multi(Data, {cont, #mp_parse_state{old_data = OldData}=ParseState}, _) ->
    NData = <<OldData/binary, Data/binary>>,
    parse_multi(NData, ParseState, []);

parse_multi(<<"--\r\n", Data/binary>>,
            #mp_parse_state{state=boundary}=ParseState, Acc) ->
    parse_multi(Data, ParseState, Acc);
parse_multi(Data, #mp_parse_state{state=boundary}=ParseState, Acc) ->
    #mp_parse_state{boundary_ctx = BoundaryCtx} = ParseState,
    case bm_find(Data, BoundaryCtx) of
        {0, Len} ->
            LenPlusCRLF = Len+2,
            <<_:LenPlusCRLF/binary, Rest/binary>> = Data,
            NParseState = ParseState#mp_parse_state{state = start_header},
            parse_multi(Rest, NParseState, Acc);
        {_Pos, _Len} ->
            {NParseState, NData} = case Data of
                                       <<"\r\n", Rest/binary>> ->
                                           {ParseState#mp_parse_state{
                                              state = start_header},
                                            Rest};
                                       _ ->
                                           {ParseState#mp_parse_state{
                                              state = body}, Data}
                                   end,
            parse_multi(NData, NParseState, Acc);
        nomatch ->
            case Data of
                <<>> ->
                    {result, Acc};
                <<"\r\n">> ->
                    {result, Acc};
                _ ->
                    NParseState = ParseState#mp_parse_state{old_data = Data},
                    {cont, NParseState, Acc}
            end
    end;
parse_multi(Data, #mp_parse_state{state=start_header}=ParseState, Acc) ->
    NParseState = ParseState#mp_parse_state{state = header},
    parse_multi(Data, NParseState, Acc, [], []);
parse_multi(Data, #mp_parse_state{state=body}=ParseState, Acc) ->
    #mp_parse_state{boundary_ctx = BoundaryCtx} = ParseState,
    case bm_find(Data, BoundaryCtx) of
        {Pos, Len} ->
            <<Body:Pos/binary, _:Len/binary, Rest/binary>> = Data,
            BodyData = case ParseState#mp_parse_state.data_type of
                           list ->
                               binary_to_list(Body);
                           binary ->
                               Body
                       end,
            NAcc = [{body, BodyData}|Acc],
            NParseState = ParseState#mp_parse_state{state = boundary},
            parse_multi(Rest, NParseState, NAcc);
        nomatch ->
            NParseState = ParseState#mp_parse_state{
                            state = body,
                            old_data = <<>>
                           },
            BodyData = case ParseState#mp_parse_state.data_type of
                           list ->
                               binary_to_list(Data);
                           binary ->
                               Data
                       end,
            NAcc = [{part_body, BodyData}|Acc],
            {cont, NParseState, NAcc}
    end;
%% Initial entry point
parse_multi(Data, Boundary, Options) ->
    B1 = "\r\n--"++Boundary,
    D1 = <<"\r\n", Data/binary>>,
    BoundaryCtx = bm_start(B1),
    HdrEndCtx = bm_start("\r\n\r\n"),
    DataType = lists:foldl(fun(_, list) ->
                                   list;
                              (list, _) ->
                                   list;
                              (binary, undefined) ->
                                   binary;
                              (_, Acc) ->
                                   Acc
                           end, undefined, Options),
    ParseState = #mp_parse_state{state = boundary,
                                 boundary_ctx = BoundaryCtx,
                                 hdr_end_ctx = HdrEndCtx,
                                 data_type = DataType},
    parse_multi(D1, ParseState, []).

parse_multi(Data, #mp_parse_state{state=start_header}=ParseState, Acc, [], []) ->
    #mp_parse_state{hdr_end_ctx = HdrEndCtx} = ParseState,
    case bm_find(Data, HdrEndCtx) of
        nomatch ->
            {cont, ParseState#mp_parse_state{old_data = Data}, Acc};
        _ ->
            NParseState = ParseState#mp_parse_state{state = header},
            parse_multi(Data, NParseState, Acc, [], [])
    end;
parse_multi(Data, #mp_parse_state{state=header}=ParseState, Acc, Name, Hdrs) ->
    case erlang:decode_packet(httph_bin, Data, [{packet_size, 16#4000}]) of
        {ok, http_eoh, Rest} ->
            Head = case Name of
                       [] ->
                           lists:reverse(Hdrs);
                       _ ->
                           {Name, lists:reverse(Hdrs)}
                   end,
            NParseState = ParseState#mp_parse_state{state = body},
            parse_multi(Rest, NParseState, [{head, Head}|Acc]);
        {ok, {http_header, _, Hdr, _, HdrVal}, Rest} when is_atom(Hdr) ->
            Header = {case Hdr of
                          'Content-Type' ->
                              content_type;
                          Else ->
                              Else
                      end,
                      binary_to_list(HdrVal)},
            parse_multi(Rest, ParseState, Acc, Name, [Header|Hdrs]);
        {ok, {http_header, _, Hdr, _, HdrVal}, Rest} ->
            HdrValStr = binary_to_list(HdrVal),
            case yaws:to_lower(binary_to_list(Hdr)) of
                "content-disposition" ->
                    "form-data"++Params = HdrValStr,
                    Parameters = parse_arg_line(Params),
                    {value, {_, NewName}} = lists:keysearch(name, 1, Parameters),
                    parse_multi(Rest, ParseState, Acc,
                                NewName, Parameters++Hdrs);
                LowerHdr ->
                    parse_multi(Rest, ParseState, Acc,
                                Name, [{LowerHdr, HdrValStr}|Hdrs])
            end;
        {error, _Reason}=Error ->
            Error
    end.

%% parse POST data when ENCTYPE is unset or
%% Content-type: application/x-www-form-urlencoded
%% Bin is the content of ARG#arg.clidata
%% the alternative is
%% Content-type: multipart/form-data; boundary=-------------------7cd1d6371ec
%% which is used for file upload

parse_post_data_urlencoded(Bin) ->
    do_parse_spec(Bin, nokey, [], key).


%% It will return a [{Key, Value}] list from the post data

do_parse_spec(<<$%, Hi:8, Lo:8, Tail/binary>>, Last, Cur, State)
    when Hi /= $u ->
    Hex = yaws:hex_to_integer([Hi, Lo]),
    do_parse_spec(Tail, Last, [ Hex | Cur],  State);

do_parse_spec(<<$&, Tail/binary>>, _Last , Cur,  key) ->
    [{lists:reverse(Cur), undefined} |
     do_parse_spec(Tail, nokey, [], key)];  %% cont keymode

do_parse_spec(<<$&, Tail/binary>>, Last, Cur, value) ->
    V = {Last, lists:reverse(Cur)},
    [V | do_parse_spec(Tail, nokey, [], key)];

do_parse_spec(<<$+, Tail/binary>>, Last, Cur,  State) ->
    do_parse_spec(Tail, Last, [$\s|Cur], State);

do_parse_spec(<<$=, Tail/binary>>, _Last, Cur, key) ->
    do_parse_spec(Tail, lists:reverse(Cur), [], value); %% change mode

do_parse_spec(<<$%, $u, A:8, B:8,C:8,D:8, Tail/binary>>,
	       Last, Cur, State) ->
    %% non-standard encoding for Unicode characters: %uxxxx,
    Hex = yaws:hex_to_integer([A,B,C,D]),
    do_parse_spec(Tail, Last, [ Hex | Cur],  State);

do_parse_spec(<<H:8, Tail/binary>>, Last, Cur, State) ->
    do_parse_spec(Tail, Last, [H|Cur], State);
do_parse_spec(<<>>, nokey, Cur, _State) ->
    [{lists:reverse(Cur), undefined}];
do_parse_spec(<<>>, Last, Cur, _State) ->
    [{Last, lists:reverse(Cur)}];
do_parse_spec(undefined,_,_,_) ->
    [];
do_parse_spec(QueryList, Last, Cur, State) when is_list(QueryList) ->
    do_parse_spec(list_to_binary(QueryList), Last, Cur, State).


code_to_phrase(100) -> "Continue";
code_to_phrase(101) -> "Switching Protocols ";
code_to_phrase(200) -> "OK";
code_to_phrase(201) -> "Created";
code_to_phrase(202) -> "Accepted";
code_to_phrase(203) -> "Non-Authoritative Information";
code_to_phrase(204) -> "No Content";
code_to_phrase(205) -> "Reset Content";
code_to_phrase(206) -> "Partial Content";
code_to_phrase(207) -> "Multi Status";
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
code_to_phrase(505) -> "HTTP Version Not Supported";

%% Below are some non-HTTP status codes from other protocol standards that
%% we've seen used with HTTP in the wild, so we include them here. HTTP 1.1
%% section 6.1.1 allows for this sort of extensibility, but we recommend
%% sticking with the HTTP status codes above for maximal portability and
%% interoperability.
%%
code_to_phrase(451) -> "Requested Action Aborted";   % from FTP (RFC 959)
code_to_phrase(452) -> "Insufficient Storage Space"; % from FTP (RFC 959)
code_to_phrase(453) -> "Not Enough Bandwidth".       % from RTSP (RFC 2326)



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
htmlize(Bin) when is_binary(Bin) ->
    list_to_binary(htmlize_l(binary_to_list(Bin)));
htmlize(List) when is_list(List) ->
    htmlize_l(List).



htmlize_char($>) ->
    <<"&gt;">>;
htmlize_char($<) ->
    <<"&lt;">>;
htmlize_char($&) ->
    <<"&amp;">>;
htmlize_char($") ->
    <<"&quot;">>;
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
htmlize_l([$"|Tail], Acc) ->
    htmlize_l(Tail, [$; , $t, $o,  $u,  $q  ,$&|Acc]);

htmlize_l([X|Tail], Acc) when is_integer(X) ->
    htmlize_l(Tail, [X|Acc]);
htmlize_l([X|Tail], Acc) when is_binary(X) ->
    X2 = htmlize_l(binary_to_list(X)),
    htmlize_l(Tail, [X2|Acc]);
htmlize_l([X|Tail], Ack) when is_list(X) ->
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

setcookie(Name, Value, Path, Expire, Domain, Secure) ->
    SetDomain = if Domain == [] -> "";
                   true -> " Domain="++Domain++";"
                end,
    SetExpire = if Expire == [] -> "";
                   true -> " Expires="++Expire++";"
                end,
    SetPath = if Path == [] -> "/";
                 true -> Path
              end,
    SetSecure = if Secure == on -> " secure;";
                   true -> ""
                end,
    {header, {set_cookie, f("~s=~s;~s~s~s Path=~s",
                            [Name,Value,SetDomain,SetExpire,
                             SetSecure, SetPath])}}.


%% This function can be passed the cookie we get in the Arg#arg.headers.cookies
%% to search for a specific cookie
%% return [] if not found
%%        Str if found
%% if serveral cookies with the same name are passed fron the browser,
%% only the first match is returned


find_cookie_val(Cookie, A) when is_record(A, arg) ->
    find_cookie_val(Cookie,  (A#arg.headers)#headers.cookie);
%%
find_cookie_val(_Cookie, []) ->
    [];
find_cookie_val(Cookie, [FullCookie | FullCookieList]) ->
    case eat_cookie(Cookie, FullCookie) of
        [] ->
            find_cookie_val(Cookie, FullCookieList);
        Val ->
            Val
    end.

%% Remove leading spaces before eating.
eat_cookie([], _)           -> [];
eat_cookie([$\s|T], Str)    -> eat_cookie(T, Str);
eat_cookie(_, [])           -> [];
eat_cookie(Cookie, [$\s|T]) -> eat_cookie(Cookie, T);
eat_cookie(Cookie, Str) when is_list(Cookie),is_list(Str) ->
    try
        eat_cookie2(Cookie++"=", Str, Cookie)
    catch
        _:_ -> []
    end.

%% Look for the Cookie and extract its value.
eat_cookie2(_, [], _)    ->
    throw("not found");
eat_cookie2([H|T], [H|R], C) ->
    eat_cookie2(T, R, C);
eat_cookie2([H|_], [X|R], C) when H =/= X ->
    {_,Rest} = eat_until(R, $;),
    eat_cookie(C, Rest);
eat_cookie2([], L, _) ->
    {Meat,_} = eat_until(L, $;),
    Meat.

eat_until(L, U) ->
    eat_until(L, U, []).

eat_until([H|T], H, Acc)              -> {lists:reverse(Acc), T};
eat_until([H|T], U, Acc) when H =/= U -> eat_until(T, U, [H|Acc]);
eat_until([], _, Acc)                 -> {lists:reverse(Acc), []}.



url_decode([$%, Hi, Lo | Tail]) ->
            Hex = yaws:hex_to_integer([Hi, Lo]),
            [Hex | url_decode(Tail)];
            url_decode([$?|T]) ->
                   %% Don't decode the query string here, that is
                   %% parsed separately.
                   [$?|T];
            url_decode([H|T]) when is_integer(H) ->
                   [H |url_decode(T)];
            url_decode([]) ->
                   [];
            %% deep lists
            url_decode([H|T]) when is_list(H) ->
                   [url_decode(H) | url_decode(T)].


path_norm(Path) ->
    path_norm_reverse(lists:reverse(Path)).

path_norm_reverse("/" ++ T) -> start_dir(0, "/", T);
path_norm_reverse(       T) -> start_dir(0,  "", T).

start_dir(N, Path, [$\\|T]     ) -> start_dir(N, Path, [$/|T]);
start_dir(N, Path, ".."        ) -> rest_dir(N, Path, "");
start_dir(N, Path, "/"    ++ T ) -> start_dir(N    , Path, T);
start_dir(N, Path, "./"   ++ T ) -> start_dir(N    , Path, T);
start_dir(N, Path, ".\\"  ++ T ) -> start_dir(N    , Path, T);
start_dir(N, Path, "../"  ++ T ) -> start_dir(N + 1, Path, T);
start_dir(N, Path, "..\\" ++ T ) -> start_dir(N + 1, Path, T);
start_dir(N, Path,           T ) -> rest_dir (N    , Path, T).

rest_dir (_N, Path, []         ) -> case Path of
                                        [] -> "/";
                                        _  -> Path
                                    end;
rest_dir (0, Path, [ $/ | T ] ) -> start_dir(0    , [ $/ | Path ], T);
rest_dir (N, Path, [ $/ | T ] ) -> start_dir(N - 1,        Path  , T);
rest_dir (N, Path, [ $\\ | T ] ) -> rest_dir(N, Path, [$/|T]);
rest_dir (0, Path, [  H | T ] ) -> rest_dir (0    , [  H | Path ], T);
rest_dir (N, Path, [  _H | T ] ) -> rest_dir (N    ,        Path  , T).

%% url decode the path and return {Path, QueryPart}

url_decode_q_split(Path) ->
    url_decode_q_split(Path, []).

url_decode_q_split([$%, Hi, Lo | Tail], Ack) ->
    Hex = yaws:hex_to_integer([Hi, Lo]),
    if Hex  == 0 -> exit(badurl);
       true -> ok
    end,
    url_decode_q_split(Tail, [Hex|Ack]);
url_decode_q_split([$?|T], Ack) ->
    %% Don't decode the query string here,
    %% that is parsed separately.
    {path_norm_reverse(Ack), T};
url_decode_q_split([H|T], Ack) when H /= 0 ->
    url_decode_q_split(T, [H|Ack]);
url_decode_q_split([], Ack) ->
    {path_norm_reverse(Ack), []}.



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
                    [$%, $0, X | url_encode(T)]
            end
     end;

url_encode([]) ->
    [].



redirect(Url) -> [{redirect, Url}].

is_nb_space(X) ->
    lists:member(X, [$\s, $\t]).

%% ret: {line, Line, Trail} | {lastline, Line, Trail} | need_more

get_line(L) ->
    get_line(L, []).
get_line("\r\n\r\n" ++ Tail, Cur) ->
    {lastline, lists:reverse(Cur), Tail};
get_line("\r\n" ++ Tail, Cur) when Tail /= []  ->
    case is_nb_space(hd(Tail)) of
        true ->  %% multiline ... continue
            get_line(Tail, [$\n, $\r | Cur]);
        false ->
            {line, lists:reverse(Cur), Tail}
    end;
get_line("\r\n", Cur)   ->
    {line, lists:reverse(Cur), []};
get_line([H|T], Cur) ->
    get_line(T, [H|Cur]);
get_line([], _) ->
    need_more.



mime_type(FileName) ->
    case filename:extension(FileName) of
        [_|T] ->
            element(2, mime_types:t(T));
        [] ->
            element(2, mime_types:t([]))
    end.


%% Asynchronously delivery
stream_chunk_deliver(YawsPid, Data) ->
    YawsPid  ! {streamcontent, Data}.


%% Use timeout here to guard against bug in the SSL application
%% that apparently does not close the socket in between
%% ssl_esock and erlang (FIN_WAIT2 resp. CLOSE_WAIT).
%% Thus, the stream process hangs forever...
-define(STREAM_GARBAGE_TIMEOUT, 3600000). % 1 hour

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
    after ?STREAM_GARBAGE_TIMEOUT ->
            %% Killing (unless this function is caught) process tree but
            %% NOTE that as this is probably due to the OTP SSL application
            %% not managing to close the socket (FIN_WAIT2
            %% resp. CLOSE_WAIT) the SSL process is not killed (it traps
            %% exit signals) and thus we will leak one file descriptor.
	    error_logger:error_msg(
	      "~p:stream_chunk_deliver_blocking/2 STREAM_GARBAGE_TIMEOUT "
	      "(default 1 hour). Killing ~p", [?MODULE, YawsPid]),
	    erlang:error(stream_garbage_timeout, [YawsPid, Data])
    end.

stream_chunk_end(YawsPid) ->
    YawsPid ! endofstreamcontent.

stream_process_deliver(Sock={sslsocket,_,_}, IoList) ->
    ssl:send(Sock, IoList);
stream_process_deliver(Sock, IoList) ->
    gen_tcp:send(Sock, IoList).

stream_process_deliver_chunk(Sock, IoList) ->
    Chunk = case erlang:iolist_size(IoList) of
                0 ->
                    stream_process_deliver_final_chunk(Sock, IoList);
                S ->
                    [yaws:integer_to_hex(S), "\r\n", IoList, "\r\n"]
            end,
    stream_process_deliver(Sock, Chunk).

stream_process_deliver_final_chunk(Sock, IoList) ->
    Chunk = case erlang:iolist_size(IoList) of
                0 ->
                    <<"0\r\n\r\n">>;
                S ->
                    [yaws:integer_to_hex(S), "\r\n", IoList, "\r\n0\r\n\r\n"]
            end,
    stream_process_deliver(Sock, Chunk).

stream_process_end(closed, YawsPid) ->
    YawsPid ! {endofstreamcontent, closed};
stream_process_end(Sock={sslsocket,_,_}, YawsPid) ->
    ssl:controlling_process(Sock, YawsPid),
    YawsPid ! endofstreamcontent;
stream_process_end(Sock, YawsPid) ->
    gen_tcp:controlling_process(Sock, YawsPid),
    YawsPid ! endofstreamcontent.


%% Pid must the the process in control of the websocket connection.
websocket_send(Pid, {Type, Data}) ->
    yaws_websockets:send(Pid, {Type, Data}).


%% Return new cookie string
new_cookie_session(Opaque) ->
    yaws_session_server:new_session(Opaque).

new_cookie_session(Opaque, TTL) ->
    yaws_session_server:new_session(Opaque, TTL).

new_cookie_session(Opaque, TTL, Cleanup) ->
    yaws_session_server:new_session(Opaque, TTL, Cleanup).

%% as returned in #ysession.cookie
cookieval_to_opaque(CookieVal) ->
    yaws_session_server:cookieval_to_opaque(CookieVal).

print_cookie_sessions() ->
    yaws_session_server:print_sessions().

replace_cookie_session(Cookie, NewOpaque) ->
    yaws_session_server:replace_session(Cookie, NewOpaque).

delete_cookie_session(Cookie) ->
    yaws_session_server:delete_session(Cookie).


lmap(F, [H|T]) ->
    [lists:map(F, H) | lmap(F, T)];
lmap(_, []) ->
    [].


%% interactively turn on|off tracing
set_trace(Val) ->
    Str = yaws_ctl:actl_trace(Val),
    io:format("~s", [Str]).


set_access_log(Bool) ->
    {ok, GC, Groups} = getconf(),
    Groups2 = lmap(fun(SC) ->
                           ?sc_set_access_log(SC, Bool)
                   end, Groups),
    setconf(GC, Groups2).


%% interactively turn on|off tracing to the tty (as well)
%% typically useful in embedded mode
set_tty_trace(Bool) ->
    yaws_log:trace_tty(Bool).



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
              if H#headers.cookie == [] ->
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
                      {"Authorization", element(3, H#headers.authorization)}
              end,
              if H#headers.transfer_encoding == undefined ->
                      undefined;
                 true ->
                      {"Transfer-Encoding", H#headers.transfer_encoding}
              end,
              if H#headers.location == undefined ->
                      undefined;
                 true ->
                      {"Location", H#headers.location}
              end

             ]
            ) ++
        lists:map(
          fun({http_header,_,K,_,V}) ->
                  lists:flatten(io_lib:format("~s: ~s",[K,V]))
          end, H#headers.other).




reformat_request(#http_request{method = bad_request}) ->
    ["Bad request"];
reformat_request(Req) ->
    Path = case Req#http_request.path of
               {abs_path, AbsPath} ->
                   AbsPath;
               {absoluteURI, _Scheme, _Host0, _Port, RawPath} ->
                   RawPath
           end,
    {Maj, Min} = Req#http_request.version,
    [yaws:to_list(Req#http_request.method), " ", Path," HTTP/",
     integer_to_list(Maj),".", integer_to_list(Min)].


reformat_response(Resp) ->
    {Maj,Min} = Resp#http_response.version,
    ["HTTP/",integer_to_list(Maj),".", integer_to_list(Min),
     " ", integer_to_list(Resp#http_response.status),
     " ", Resp#http_response.phrase].



%% stringify the scheme://host[:port] part of a #url
reformat_url(U) ->
    [yaws:to_string(U#url.scheme),
     "://",
     U#url.host,
     if
         U#url.port == undefined ->
             [];
         true ->
             [$: | integer_to_list(U#url.port)]
     end].

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
            parse_url(host, Strict, #url{scheme = undefined}, Str, [])
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
        [$[|T] ->
            parse_url(ipv6, Strict, U, T, [$[]);
        [H|T] ->
            parse_url(host, Strict, U, T, [H|Ack])
    end;
parse_url(ipv6, Strict, U, Str, Ack) ->
    case Str of
        [$]] ->
            U#url{host = lists:reverse([$]|Ack]),
                  path = "/"
                 };
        [$], $/|T] ->
            U2 = U#url{host = lists:reverse([$]|Ack])},
            parse_url(path, Strict, U2, T,"/");
        [$], $:|T] ->
            U2 = U#url{host = lists:reverse([$]|Ack])},
            parse_url(port, Strict, U2, T,[]);
        [H|T] ->
            parse_url(ipv6, Strict, U, T, [H|Ack])
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


%% used to construct redir headers from partial URLs such
%% as e.g. /foo/bar

format_partial_url(Url, SC) ->
    [if
         Url#url.scheme == undefined ->
             yaws:redirect_scheme(SC);
         true ->
             yaws:to_string(Url#url.scheme) ++ "://"
     end,
     if
         Url#url.host == undefined ->
             yaws:redirect_host(SC, undefined);
         true ->
             Url#url.host
     end,
     if
         Url#url.port == undefined ->
             yaws:redirect_port(SC);
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


format_url(Url) when is_record(Url, url) ->
    [
     if
         Url#url.scheme == undefined ->
             "http://";
         true ->
             yaws:to_string(Url#url.scheme) ++ "://"
     end,
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

is_absolute_URI([C|T]) when ((C>=$a) and (C=<$z)) or ((C>=$A) and (C=<$Z))->
    is_abs_URI1(T);
is_absolute_URI(_) ->
    false.

is_abs_URI1([$:|_]) ->
    true;
is_abs_URI1([C|T]) when
((C>=$a) and (C=<$z))
or ((C>=$A) and (C=<$Z))
or ((C>=$0) and (C=<$9))
or (C==$+) or (C==$-) or (C==$.) ->
    is_abs_URI1(T);
is_abs_URI1(_) ->
    false.


%% ------------------------------------------------------------
%% simple erlang term representation of HTML:
%% EHTML = [EHTML] | {Tag, Attrs, Body} | {Tag, Attrs} | {Tag} |
%%         {Module, Fun, [Args]} | fun/0
%%         binary() | character()
%% Tag   = atom()
%% Attrs = [{Key, Value}]  or {EventTag, {jscall, FunName, [Args]}}
%% Key   = atom()
%% Value = string() | {Module, Fun, [Args]} | fun/0
%% Body  = EHTML

ehtml_expand(Ch) when Ch >= 0, Ch =< 255 -> Ch; %yaws_api:htmlize_char(Ch);
ehtml_expand(Bin) when is_binary(Bin) -> Bin; % yaws_api:htmlize(Bin);

ehtml_expand({ssi,File, Del, Bs}) ->
    case yaws_server:ssi(File, Del, Bs) of
        {error, Rsn} ->
            io_lib:format("ERROR: ~p~n",[Rsn]);
        X ->
            X
    end;

%%!todo (low priority) - investigate whether tail-recursion would be of any
%% benefit here instead of the current ehtml_expand(Body) recursion.
%%                - provide a tail_recursive version & add a file in the
%% benchmarks folder to measure it.
                                                %
ehtml_expand({Tag}) ->
    ["<", atom_to_list(Tag), " />"];
ehtml_expand({pre_html, X}) -> X;
ehtml_expand({Mod, Fun, Args})
  when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ehtml_expand(Mod:Fun(Args));
ehtml_expand({Tag, Attrs}) ->
    NL = ehtml_nl(Tag),
    [NL, "<", atom_to_list(Tag), ehtml_attrs(Attrs), "></",
     atom_to_list(Tag), ">"];
ehtml_expand({Tag, Attrs, Body}) when is_atom(Tag) ->
    Ts = atom_to_list(Tag),
    NL = ehtml_nl(Tag),
    [NL, "<", Ts, ehtml_attrs(Attrs), ">", ehtml_expand(Body), "</", Ts, ">"];
ehtml_expand([H|T]) -> [ehtml_expand(H)|ehtml_expand(T)];
ehtml_expand([]) -> [];
ehtml_expand(Fun) when is_function(Fun) ->
    ehtml_expand(Fun()).


ehtml_attrs([]) -> [];
ehtml_attrs([Attribute|Tail]) when is_atom(Attribute) ->
    [[$ |atom_to_list(Attribute)]|ehtml_attrs(Tail)];
ehtml_attrs([Attribute|Tail]) when is_list(Attribute) ->
    [" ", Attribute|ehtml_attrs(Tail)];
ehtml_attrs([{Name, {Mod, Fun, Args}} | Tail])
  when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ehtml_attrs([{Name,  Mod:Fun(Args)} | Tail]);
ehtml_attrs([{Name, Value} | Tail]) when is_function(Value) ->
    ehtml_attrs([{Name, Value()} | Tail]);
ehtml_attrs([{Name, Value} | Tail]) ->
    ValueString = [$", if
                           is_atom(Value) -> atom_to_list(Value);
                           is_list(Value) -> Value;
                           is_integer(Value) -> integer_to_list(Value);
                           is_float(Value) -> float_to_list(Value)
                       end, $"],
    [[$ |atom_to_list(Name)], [$=|ValueString]|ehtml_attrs(Tail)];
ehtml_attrs([{check, Name, {Mod, Fun, Args}} | Tail])
  when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ehtml_attrs([{check, Name,  Mod:Fun(Args)} | Tail]);
ehtml_attrs([{check, Name, Value} | Tail]) when is_function(Value) ->
    ehtml_attrs([{check, Name, Value()} | Tail]);
ehtml_attrs([{check, Name, Value} | Tail]) ->
    Val = if
              is_atom(Value) -> atom_to_list(Value);
              is_list(Value) -> Value;
              is_integer(Value) -> integer_to_list(Value);
              is_float(Value) -> float_to_list(Value)
          end,
    Q = case deepmember($", Val) of
            true -> $';
            false -> $"
        end,
    ValueString = [Q,Value,Q],
    [[$ |atom_to_list(Name)], [$=|ValueString]|ehtml_attrs(Tail)].



%% Tags for which we must not add extra white space.
%% FIXME: should there be anything more in this list?

ehtml_nl(a) -> [];
ehtml_nl(br) -> [];
ehtml_nl(span) -> [];
ehtml_nl(em) -> [];
ehtml_nl(strong) -> [];
ehtml_nl(dfn) -> [];
ehtml_nl(code) -> [];
ehtml_nl(samp) -> [];
ehtml_nl(kbd) -> [];
ehtml_nl(var) -> [];
ehtml_nl(cite) -> [];
ehtml_nl(abbr) -> [];
ehtml_nl(acronym) -> [];
ehtml_nl(q) -> [];
ehtml_nl(sub) -> [];
ehtml_nl(sup) -> [];
ehtml_nl(ins) -> [];
ehtml_nl(del) -> [];
ehtml_nl(img) -> [];
ehtml_nl(tt) -> [];
ehtml_nl(i) -> [];
ehtml_nl(b) -> [];
ehtml_nl(big) -> [];
ehtml_nl(small) -> [];
ehtml_nl(strike) -> [];
ehtml_nl(s) -> [];
ehtml_nl(u) -> [];
ehtml_nl(font) -> [];
ehtml_nl(basefont) -> [];
ehtml_nl(input) -> [];
ehtml_nl(button) -> [];
ehtml_nl(object) -> [];
ehtml_nl(_) -> "\n".




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
ehtml_expander(Bin, Before, After) when is_binary(Bin) ->
    ehtml_expander_done(yaws_api:htmlize(Bin), Before, After);

ehtml_expander({ssi,File, Del, Bs}, Before, After) ->
    Str = case yaws_server:ssi(File, Del, Bs) of
              {error, Rsn} ->
                  io_lib:format("ERROR: ~p~n",[Rsn]);
              X ->
                  X
          end,
    ehtml_expander_done(Str, Before, After);

ehtml_expander({pre_html, X}, Before, After) ->
    ehtml_expander_done(X, Before, After);
%% Tags
ehtml_expander({Tag}, Before, After) ->
    ehtml_expander_done(["<", atom_to_list(Tag), " />"], Before, After);
ehtml_expander({Tag, Attrs}, Before, After) ->
    NL = ehtml_nl(Tag),
    ehtml_expander_done([NL, "<", atom_to_list(Tag), ehtml_attrs(Attrs), "></",
                         atom_to_list(Tag), ">"],
                        Before,
                        After);
ehtml_expander({Tag, Attrs, Body}, Before, After) ->
    ehtml_expander(Body,
                   [["\n<", atom_to_list(Tag),
                     ehtml_attrs_expander(Attrs), ">"]|
                    Before],
                   ["</", atom_to_list(Tag), ">"|After]);
%% Variable references
ehtml_expander(Var, Before, After) when is_atom(Var) ->
    [reverse(Before), {ehtml, ehtml_var_name(Var)}, After];
%% Lists
ehtml_expander([H|T], Before, After) ->
    ehtml_expander(T, [ehtml_expander(H, [], [])|Before], After);
ehtml_expander([], Before, After) ->
    ehtml_expander_done("", Before, After).

%% Expander for attributes. The attribute name and value can each be a
%% variable reference.
ehtml_attrs_expander([]) -> "";
ehtml_attrs_expander([{Var,Val}|T]) ->
    [[" ",
      ehtml_attr_part_expander(Var),
      "=",
      "\"", ehtml_attr_part_expander(Val), "\""]|
     ehtml_attrs_expander(T)];
ehtml_attrs_expander([Var|T]) ->
    [[" ",
      ehtml_attr_part_expander(Var)]|
     ehtml_attrs_expander(T)];
ehtml_attrs_expander(Var) when is_atom(Var) ->
    %% Var in the cdr of an attribute list
    [{ehtml_attrs, ehtml_var_name(Var)}].

ehtml_attr_part_expander(A) when is_atom(A) ->
    case atom_to_list(A) of
        [$$|_Rest] -> {preformatted, ehtml_var_name(A)};
        Other -> Other
    end;
ehtml_attr_part_expander(I) when is_integer(I) -> integer_to_list(I);
ehtml_attr_part_expander(S) when is_list(S) -> S.

ehtml_expander_done(X, Before, After) -> [reverse([X|Before]), After].

%% Compress an EHTML expander, converting all adjacent bits of text into
%% binaries.
%% Returns: [binary() | {ehtml, Var} | {preformatted, Var}, {ehtml_attrs, Var}]
%% Var = atom()
ehtml_expander_compress([Tag|T], Acc) when is_tuple(Tag) ->
    [list_to_binary(reverse(Acc)), Tag | ehtml_expander_compress(T, [])];
ehtml_expander_compress([], Acc) -> [list_to_binary(reverse(Acc))];
ehtml_expander_compress([H|T], Acc) when is_integer(H) ->
    ehtml_expander_compress(T, [H|Acc]).

%% Apply an expander with the variable bindings in Env.  Env is a list of
%% {VarName, Value} tuples, where VarName is an atom and Value is an ehtml
%% term.
ehtml_apply(Expander, Env) -> [ehtml_eval(X, Env) || X <- Expander].

ehtml_eval(Bin, _Env) when is_binary(Bin) -> Bin;
ehtml_eval({Type, Var}, Env) ->
    case lists:keysearch(Var, 1, Env) of
        false -> erlang:error({ehtml_unbound, Var});
        {value, {Var, Val}} ->
            case Type of
                ehtml -> ehtml_expand(Val);
                preformatted -> Val;
                ehtml_attrs -> ehtml_attrs(Val)
            end
    end.

%% Get the name part of a variable reference.
%% e.g. ehtml_var_name('$foo') -> foo.
ehtml_var_name(A) when is_atom(A) ->
    case atom_to_list(A) of
        [$$|Rest] -> list_to_atom(Rest);
        _Other -> erlang:error({bad_ehtml_var_name, A})
    end.

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


%% call_cgi calls the script `Scriptfilename' (full path).  If
%% `Exefilename' is given, it is the executable to handle this,
%% otherwise `Scriptfilame' is assumed to be executable itself.
%%
%% Note however, that these functions usually generate stream content.
%% (If you have good use for a version generating {content, _, _}
%% instead, contact carsten@codimi.de)
%%
%% Also note, that they may return `get_more' and expect to be called
%% again.

call_cgi(Arg, Scriptfilename) ->
    yaws_cgi:call_cgi(Arg, Scriptfilename).

call_cgi(Arg, Exefilename, Scriptfilename) ->
    yaws_cgi:call_cgi(Arg, Exefilename, Scriptfilename).

%% call_fci_responder issues a responder role call to the FastCGI
%% application server. It returns the same return value as out/1.
%%
%% call_fci_authorizer issues a authorizer role call to the FastCGI
%% application server. It returns:
%%
%% {denied, Out} : Access is denied. Out is the same return value as
%% out/1.
%%
%% {allowed, Variables} : Access is allowed. Variables is a list of
%% environment variables returned by the authorization server using
%% Variable-XXX: YYY headers.
%%
%% Note: the FastCGI filter role is not yet supported.
%%
%% The following information is taken from the server configuration:
%% - The hostname (or address) and port number of the application server.
%% - Extra CGI variables.
%% - Trace FastCGI protocol messages?
%% - Log application server error messages?
%%
%% The caller can optionally provide an Options argument which supports
%% the following options. These override the defaults taken from the
%% server config.
%%
%% {app_server_host, string() | ip_address()} : The hostname or IP address
%% of the application server.
%%
%% {app_server_port, int()} : The TCP port number of the application server.
%%
%% {path_info, string()} : Override the patinfo string from Arg.
%%
%% {extra_env, [{string(), string()}]} : Extra environment variables to be
%% passed to the application server, as a list of name-value pairs.
%%
%% trace_protocol : Trace FastCGI protocol messages.
%%
%% log_app_error : Log application errors (output to stderr and non-zero
%% exit value).
%%
call_fcgi_responder(Arg) ->
    yaws_cgi:call_fcgi_responder(Arg).

call_fcgi_responder(Arg, Options) ->
    yaws_cgi:call_fcgi_responder(Arg, Options).

call_fcgi_authorizer(Arg) ->
    yaws_cgi:call_fcgi_authorizer(Arg).

call_fcgi_authorizer(Arg, Options) ->
    yaws_cgi:call_fcgi_authorizer(Arg, Options).

%%

deepmember(_C,[]) ->
    false;
deepmember(C,[C|_Cs]) ->
    true;
deepmember(C,[L|Cs]) when is_list(L) ->
    case deepmember(C,L) of
        true  -> true;
        false -> deepmember(C,Cs)
    end;
deepmember(C,[N|Cs]) when C /= N ->
    deepmember(C, Cs).


%%  Parse a Set-Cookie header.
%%
%%  RFC (2109) ports are from RFC 2965
%%
%%  "Cookie:" cookie-version 1*((";" | ",") cookie-value)
%%  "Set-Cookie:"  cookies
%%  "Set-Cookie2:" cookies
%%  cookie-value    =       NAME "=" VALUE [";" path] [";" domain] [";" port]
%%  cookie          =       NAME "=" VALUE *( ";" cookie-av )
%%  cookie-version  =       "$Version" "=" value
%%  NAME            =       attr
%%  VALUE           =       value
%%  path            =       "$Path" "=" value
%%  domain          =       "$Domain" "=" value
%%  port            =       "$Port" "=" <"> value <">
%%
%%  cookie-av       = "Comment" "=" value
%%                  | "CommentURL" "=" <"> http_URL <">
%%                  | "Discard"
%%                  | "Domain" "=" value
%%                  | "Max-Age" "=" value
%%                  | "Path" "=" value
%%                  | "Port" [ "=" <"> portlist <"> ]
%%                  | "Secure"
%%                  | "Version" "=" 1*DIGIT
%%

parse_set_cookie(Str) ->
    parse_set_cookie(Str, #setcookie{}).

parse_set_cookie([], Cookie) ->
    Cookie;
parse_set_cookie(Str, Cookie) ->
    Rest00 = skip_space(Str),
    {Key,Rest0} = parse_set_cookie_key(Rest00, []),
    Rest1 = skip_space(Rest0),
    case Rest1 of
        [$=|Rest2] ->
            {Value,Quoted,Rest3} = parse_set_cookie_value(Rest2),
            NewC=add_set_cookie(Cookie,yaws:to_lower(Key),Value,Quoted),
            parse_set_cookie(Rest3,NewC);
        [$;|Rest2] ->
            NewC =add_set_cookie(Cookie,yaws:to_lower(Key),undefined,false),
            parse_set_cookie(Rest2,NewC);
        _ ->
            Cookie
    end.

%%

parse_set_cookie_key([], Acc) ->
    {lists:reverse(Acc), []};
parse_set_cookie_key(T=[$=|_], Acc) ->
    {lists:reverse(Acc), T};
parse_set_cookie_key(T=[$;|_], Acc) ->
    {lists:reverse(Acc), T};
parse_set_cookie_key([C|T], Acc) ->
    parse_set_cookie_key(T, [C|Acc]).

%%

parse_set_cookie_value([$"|T]) ->
    parse_quoted(T,[]);
parse_set_cookie_value(T) ->
    parse_set_cookie_value(T,[]).

parse_set_cookie_value([],Acc) ->
    {lists:reverse(Acc), false, []};
parse_set_cookie_value(T=[$;|_], Acc) ->
    {lists:reverse(Acc), false, T};
parse_set_cookie_value([C|T], Acc) ->
    parse_set_cookie_value(T, [C|Acc]).

parse_quoted([], Acc) ->
    {lists:reverse(Acc), true, []};
parse_quoted([$"|T], Acc) ->
    {lists:reverse(Acc), true, T};
parse_quoted([$\\,C|T], Acc) ->
    parse_quoted(T,[C,$\\|Acc]);
parse_quoted([C|T], Acc) ->
    parse_quoted(T,[C|Acc]).
%%

add_set_cookie(C, Key, Value, Quoted) when C#setcookie.key==undefined ->
    C#setcookie{key=Key,value=Value,quoted=Quoted};
add_set_cookie(C, "comment", Value, _Quoted) ->
    C#setcookie{comment=Value};
add_set_cookie(C, "commenturl", Value, _Quoted) ->
    C#setcookie{comment_url=Value};
add_set_cookie(C, "discard", Value, _Quoted) ->
    C#setcookie{discard=Value};
add_set_cookie(C, "domain", Value, _Quoted) ->
    C#setcookie{domain=Value};
add_set_cookie(C, "max-age", Value, _Quoted) ->
    C#setcookie{max_age=Value};
add_set_cookie(C, "path", Value, _Quoted) ->
    C#setcookie{path=Value};
add_set_cookie(C, "port", Value, _Quoted) ->
    C#setcookie{port=Value};
add_set_cookie(C, "secure", Value, _Quoted) ->
    C#setcookie{secure=Value};
add_set_cookie(C, "version", Value, _Quoted) ->
    C#setcookie{version=Value};
add_set_cookie(C, _Key, _Value, _Quoted) ->
    C.

%%

format_set_cookie(C) when C#setcookie.value == undefined ->
    [C#setcookie.key|format_set_cookie_opts(C)];
format_set_cookie(C) when C#setcookie.quoted ->
    [C#setcookie.key,$=,$",C#setcookie.value,$"|
     format_set_cookie_opts(C)];
format_set_cookie(C) ->
    [C#setcookie.key,$=,C#setcookie.value|
     format_set_cookie_opts(C)].

add_opt(_Key,undefined) -> [];
add_opt(Key,Opt) -> [$;,Key,$=,Opt].

format_set_cookie_opts(C) ->
    [add_opt("Path",C#setcookie.path),
     add_opt("Port",C#setcookie.port),
     add_opt("Domain",C#setcookie.domain),
     add_opt("Secure",C#setcookie.secure),
     add_opt("Expires",C#setcookie.expires),
     add_opt("Max-Age",C#setcookie.max_age),
     add_opt("Discard",C#setcookie.discard),
     add_opt("Comment",C#setcookie.comment),
     add_opt("CommentURL",C#setcookie.comment_url),
     add_opt("version",C#setcookie.version)].

%%

skip_space([]) -> [];
skip_space([$ |T]) -> skip_space(T);
skip_space([$\t|T]) -> skip_space(T);
skip_space(T) -> T.

%%


getvar(ARG,Key) when is_atom(Key) ->
    getvar(ARG, atom_to_list(Key));
getvar(ARG,Key) ->
    case (ARG#arg.req)#http_request.method of
        'POST' -> postvar(ARG, Key);
        'GET' -> queryvar(ARG, Key);
        _ -> undefined
    end.


queryvar(ARG,Key) when is_atom(Key) ->
    queryvar(ARG, atom_to_list(Key));
queryvar(ARG, Key) ->
    Parse = case get(query_parse) of
                undefined ->
                    Pval = yaws_api:parse_query(ARG),
                    put(query_parse, Pval),
                    Pval;
                Val0 ->
                    Val0
            end,
    filter_parse(Key, Parse).

postvar(ARG, Key) when is_atom(Key) ->
    postvar(ARG, atom_to_list(Key));
postvar(ARG, Key) ->
    Parse = case get(post_parse) of
                undefined ->
                    Pval = yaws_api:parse_post(ARG),
                    put(post_parse, Pval),
                    Pval;
                Val0 ->
                    Val0
            end,
    filter_parse(Key, Parse).

filter_parse(Key, Parse) ->
    case lists:filter(fun(KV) ->
                              (Key == element(1, KV))
                                  andalso
                                    (element(2, KV) /= undefined)
                      end,
                      Parse) of
        [] -> undefined;
        [{_, V}] -> {ok,V};
        %% Multivalued case - return list of values
        Vs -> list_to_tuple(lists:map(fun(KV) ->
                                              element(2, KV)
                                      end,
                                      Vs))
    end.


binding(Key) ->
    case get({binding, Key}) of
        undefined -> erlang:error({unknown_binding, Key});
        Value -> Value
    end.

binding_exists(Key) ->
    case get({binding, Key}) of
        undefined -> false;
        _ -> true
    end.



%% Return the parsed url that the client requested.
request_url(ARG) ->
    SC = get(sc),
    Headers = ARG#arg.headers,
    {abs_path, Path} = (ARG#arg.req)#http_request.path,
    DecPath = url_decode(Path),
    {P,Q} = yaws:split_at(DecPath, $?),
    #url{scheme = case SC#sconf.ssl of
                      undefined ->
                          "http";
                      _ ->
                          "https"
                  end,
         host = case Headers#headers.host of
                    undefined ->
                        yaws:upto_char($:, SC#sconf.servername);
                    HostHdr ->
                        yaws:upto_char($:, HostHdr)
                end,
         port = case {SC#sconf.ssl, SC#sconf.port} of
                    {_, 80} ->
                        undefined;
                    {_, 443} ->
                        undefined;
                    {_, Port} ->
                        Port
                end,
         path = P,
         querypart = Q}.



%% remove sick characters

sanitize_file_name(".." ++ T) ->
    sanitize_file_name([$.|T]);
sanitize_file_name([H|T]) ->
    case lists:member(H,  " &;'`{}!\\?<>\"()$") of
        true ->
            sanitize_file_name(T);
        false ->
            [H|sanitize_file_name(T)]
    end;
sanitize_file_name([]) ->
    [].



%% to be used in embedded mode, make it possible
%% to pass a config to yaws from another data source
%% than /etc/yaws/yaws.conf, for example from a database
%% this code is also called by the server -h hup code
setconf(GC0, Groups0) ->
    setconf(GC0, Groups0, true).
setconf(GC0, Groups0, CheckCertsChanged) ->
    CertsChanged = if CheckCertsChanged == true ->
                           lists:member(yes,gen_server:call(
                                              yaws_server,
                                              check_certs, infinity));
                      true ->
                           false
                   end,
    if
        CertsChanged ->
            application:stop(ssl),
            application:start(ssl);
        true ->
            ok
    end,

    {GC, Groups1} = yaws_config:verify_upgrade_args(GC0, Groups0),
    Groups2 = lists:map(fun(X) -> yaws_config:add_yaws_auth(X) end, Groups1),
    {ok, OLDGC, OldGroups} = yaws_api:getconf(),
    case {yaws_config:can_hard_gc(GC, OLDGC),
          yaws_config:can_soft_setconf(GC, Groups2, OLDGC, OldGroups)} of
        {true, true} ->
            yaws_config:soft_setconf(GC, Groups2, OLDGC, OldGroups);
        {true, false} ->
            yaws_config:hard_setconf(GC, Groups2);
        _ ->
            {error, need_restart}
    end.




%% return {ok, GC, Groups}.
getconf() ->
    gen_server:call(yaws_server, getconf, infinity).


embedded_start_conf(DocRoot) when is_list(DocRoot) ->
    embedded_start_conf(DocRoot, []).
embedded_start_conf(DocRoot, SL) when is_list(DocRoot), is_list(SL) ->
    embedded_start_conf(DocRoot, SL, []).
embedded_start_conf(DocRoot, SL, GL)
  when is_list(DocRoot), is_list(SL), is_list(GL) ->
    embedded_start_conf(DocRoot, SL, GL, "default").
embedded_start_conf(DocRoot, SL, GL, Id)
  when is_list(DocRoot), is_list(SL), is_list(GL) ->
    case application:load(yaws) of
        ok -> ok;
        {error, {already_loaded,yaws}} -> ok;
        _ -> exit("cannot load yaws")
    end,
    ok = application:set_env(yaws, embedded, true),
    ok = application:set_env(yaws, id, Id),
    ChildSpecs = yaws_sup:child_specs(),
    GC = yaws:create_gconf(GL, Id),
    SCList  = case SL of
                  [] ->
                      [[]];
                  [Cnf|_] when is_tuple(Cnf) ->
                      [[yaws:create_sconf(DocRoot, SL)]];
                  [Cnf|_] when is_list(Cnf) ->
                      [[yaws:create_sconf(DocRoot, SLItem)] || SLItem <- SL]
              end,
    SoapChild = yaws_config:add_yaws_soap_srv(GC, false),

    %% In case a server is started before any configuration has been set,
    %% this makes it possible to get hold of the 'pending' configuration.
    %% (see for example the start of the yaws_session_server)
    ok = application:set_env(yaws, embedded_conf, [{sclist,SCList},{gc,GC}]),

    yaws:mkdir(yaws:id_dir(Id)),
    {ok, SCList, GC, ChildSpecs ++ SoapChild}.


%% Function which is invoked typically from an index.yaws file
dir_listing(Arg) ->
    dir_listing(Arg, ".").
dir_listing(Arg, RelDir) ->
    %% .yaws.auth
    Dir0 = filename:dirname(Arg#arg.fullpath),
    Dir = case RelDir of
              "." -> Dir0;
              _ -> filename:join([Dir0, RelDir])
          end,
    Req = Arg#arg.req,
    case file:list_dir(Dir) of
        {ok, Data0} ->
            Data = Data0 -- [".yaws.auth", "index.yaws"],
            yaws_ls:list_directory(Arg, Arg#arg.clisock, Data,
                                   Dir,
                                   Req,  false),
            ok;
        _Err ->
            %% Just ignore errors ??, the programmer has to
            %% make sure it's a valid path here
            ok
    end.

%% Returns #redir_self{} record
redirect_self(A) ->
    SC = get(sc),
    {Port, PortStr} =
        case {SC#sconf.rmethod, SC#sconf.ssl, SC#sconf.port} of
            {"https", _, 443} -> {443, ""};
            {"http", _, 80} -> {80, ""};
            {_, undefined, 80} -> {80, ""};
            {_, undefined, Port2} ->
                {port, [$:|integer_to_list(Port2)]};
            {_, _SSL, 443} ->
                {443, ""};
            {_, _SSL, Port2} ->
                {Port2, [$:|integer_to_list(Port2)]}
        end,
    H = A#arg.headers,
    Host0 = yaws:redirect_host(get(sc), H#headers.host),
    %% redirect host contains the port number - for mysterious reasons
    Host = case string:tokens(Host0, ":") of
               [H0, _] -> H0;
               [H1] -> H1
           end,
    {Scheme, SchemeStr} =
        case {SC#sconf.ssl,SC#sconf.rmethod} of
            {_, Method} when is_list(Method) ->
                {list_to_atom(Method), Method++"://"};
            {undefined,_} ->
                {http, "http://"};
            {_SSl,_} ->
                {https, "https://"}
        end,
    #redir_self{host = Host,
                scheme = Scheme,
                scheme_str = SchemeStr,
                port = Port,
                port_str = PortStr}.

%% Boyer-Moore searching, used for parsing multipart/form-data
bm_start(Str) ->
    Len = length(Str),
    Tbl = bm_set_shifts(Str, Len),
    {Tbl, list_to_binary(Str), lists:reverse(Str), Len}.

bm_find(Bin, SearchCtx) ->
    bm_find(Bin, SearchCtx, 0).
bm_find(Bin, {_, _, _, Len}, Pos) when size(Bin) < (Pos + Len) ->
    nomatch;
bm_find(Bin, {Tbl, BStr, RevStr, Len}=SearchCtx, Pos) ->
    case Bin of
        <<_:Pos/binary, BStr:Len/binary, _/binary>> ->
            {Pos, Len};
        <<_:Pos/binary, NoMatch:Len/binary, _/binary>> ->
            RevNoMatch = lists:reverse(binary_to_list(NoMatch)),
            Shift = bm_next_shift(RevNoMatch, RevStr, 0, Tbl),
            bm_find(Bin, SearchCtx, Pos+Shift)
    end.

bm_set_shifts(Str, Len) ->
    erlang:make_tuple(256, Len, bm_set_shifts(Str, 0, Len, [])).
bm_set_shifts(_Str, Count, Len, Acc) when Count =:= Len-1 ->
    lists:reverse(Acc);
bm_set_shifts([H|T], Count, Len, Acc) ->
    Shift = Len - Count - 1,
    bm_set_shifts(T, Count+1, Len, [{H+1,Shift}|Acc]).

bm_next_shift([H|T1], [H|T2], Comparisons, Tbl) ->
    bm_next_shift(T1, T2, Comparisons+1, Tbl);
bm_next_shift([H|_], _, Comparisons, Tbl) ->
    erlang:max(element(H+1, Tbl) - Comparisons, 1).
