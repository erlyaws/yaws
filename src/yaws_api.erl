%%----------------------------------------------------------------------
%%% File    : yaws_api.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose :
%%% Created : 24 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_api).
-author('klacke@hyber.org').

-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").



-export([parse_query/1, parse_post/1,
         parse_multipart_post/1, parse_multipart_post/2,
         parse_multipart/2, parse_multipart/3]).
-export([code_to_phrase/1, ssi/2, redirect/1]).
-export([setcookie/2, setcookie/3, setcookie/4, setcookie/5, setcookie/6]).
-deprecated([{setcookie, 2, eventually},
             {setcookie, 3, eventually},
             {setcookie, 4, eventually},
             {setcookie, 5, eventually},
             {setcookie, 6, eventually}]).
-export([set_cookie/3]).
-export([pre_ssi_files/2,  pre_ssi_string/1, pre_ssi_string/2,
         set_content_type/1,
         htmlize/1, htmlize_char/1, f/2, fl/1]).
-export([find_cookie_val/2, secs/0,
         url_decode/1, url_decode_q_split/1, url_decode_with_encoding/2,
         url_encode/1, parse_url/1, parse_url/2, format_url/1,
         format_partial_url/2]).
-export([is_absolute_URI/1]).
-export([path_norm/1, path_norm_reverse/1,
         sanitize_file_name/1]).
-export([get_line/1, mime_type/1, mime_type/2]).
-export([stream_chunk_deliver/2, stream_chunk_deliver_blocking/2,
         stream_chunk_end/1]).
-export([stream_process_deliver/2, stream_process_deliver_chunk/2,
         stream_process_deliver_final_chunk/2, stream_process_end/2]).
-export([websocket_send/2, websocket_close/1, websocket_close/2]).
-export([get_sslsocket/1]).
-export([new_cookie_session/1, new_cookie_session/2, new_cookie_session/3,
         cookieval_to_opaque/1, request_url/1,
         print_cookie_sessions/0,
         replace_cookie_session/2, replace_cookie_session/3,
         delete_cookie_session/1]).

-export([getconf/0,
         setconf/2,
         get_listen_port/1,
         embedded_start_conf/1, embedded_start_conf/2,
         embedded_start_conf/3, embedded_start_conf/4]).

-export([set_status_code/1, reformat_header/1, reformat_header/2,
         reformat_request/1, reformat_response/1, reformat_url/1]).

-export([set_trace/1,
         set_tty_trace/1,
         set_access_log/1]).

-export([call_cgi/2, call_cgi/3]).

-export([call_fcgi_responder/1, call_fcgi_responder/2,
         call_fcgi_authorizer/1, call_fcgi_authorizer/2]).

-export([ehtml_expand/1, ehtml_expander/1, ehtml_apply/2,
         ehtml_expander_test/0]).

-export([parse_set_cookie/1, parse_cookie/1, format_set_cookie/1,
         format_cookie/1, postvar/2, queryvar/2, getvar/2]).

-export([binding/1,binding_exists/1,
         dir_listing/1, dir_listing/2, redirect_self/1]).

-export([arg_clisock/1, arg_client_ip_port/1, arg_headers/1, arg_req/1,
         arg_orig_req/1, arg_clidata/1, arg_server_path/1, arg_querydata/1,
         arg_appmoddata/1, arg_docroot/1, arg_docroot_mount/1, arg_fullpath/1,
         arg_cont/1, arg_state/1, arg_pid/1, arg_opaque/1, arg_appmod_prepath/1,
         arg_prepath/1,
         arg_pathinfo/1]).
-export([http_request_method/1, http_request_path/1, http_request_version/1,
         http_response_version/1, http_response_status/1,
         http_response_phrase/1,
         headers_connection/1, headers_accept/1, headers_host/1,
         headers_if_modified_since/1, headers_if_match/1,
         headers_if_none_match/1,
         headers_if_range/1, headers_if_unmodified_since/1, headers_range/1,
         headers_referer/1, headers_user_agent/1, headers_accept_ranges/1,
         headers_cookie/1, headers_keep_alive/1, headers_location/1,
         headers_content_length/1, headers_content_type/1,
         headers_content_encoding/1, headers_authorization/1,
         headers_transfer_encoding/1, headers_x_forwarded_for/1,
         headers_other/1]).

-export([set_header/2, set_header/3, merge_header/2, merge_header/3,
         get_header/2, get_header/3, delete_header/2]).

-import(lists, [flatten/1, reverse/1]).

%% These are a bunch of accessor functions that are useful inside
%% yaws scripts.

arg_clisock(#arg{clisock = X}) -> X.
arg_client_ip_port(#arg{client_ip_port = X}) -> X.
arg_headers(#arg{headers = X}) -> X.
arg_req(#arg{req = X}) -> X.
arg_orig_req(#arg{orig_req = X}) -> X.
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
    case get(query_parse) of
        undefined ->
            Res = case Arg#arg.querydata of
                      [] -> [];
                      D  -> parse_post_data_urlencoded(D)
                  end,
            put(query_parse, Res),
            Res;
        Res ->
            Res
    end.

%% parse url encoded POST data
parse_post(Arg) ->
    case get(post_parse) of
        undefined ->
            H = Arg#arg.headers,
            Res = case H#headers.content_type of
                      "application/x-www-form-urlencoded"++_ ->
                          case Arg#arg.clidata of
                              [] -> [];
                              D  -> parse_post_data_urlencoded(D)
                          end;
                      _ ->
                          []
                  end,
            put(post_parse, Res),
            Res;
        Res ->
            Res
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
%% {result, Res} if this is the last (or only) segment.
%%
%% or {error, Reason} if an error occurred during the parsing.
%%
%% Res is a list of {head, {Name, Hdrs}} | {part_body, Binary} | {body, Binary}
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
%%                    {html, f("<pre>Done </pre>",[])};
%%             {error, Reason} ->
%%                    {html, f("An error occured: ~p", [Reason])}
%%        end.
%%
%% handle_res(A, [{head, {Name, Hdrs}}|T]) ->
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
    case H#headers.content_type of
        undefined ->
            {error, no_content_type};
        "multipart/form-data"++Line ->
            case Arg#arg.cont of
                {cont, Cont} ->
                    parse_multipart(un_partial(Arg#arg.clidata), {cont, Cont});
                undefined ->
                    LineArgs = parse_arg_line(Line),
                    {value, {_, Boundary}} = lists:keysearch("boundary", 1,
                                                             LineArgs),
                    parse_multipart(un_partial(Arg#arg.clidata),
                                    Boundary, Options)
            end;
        _Other ->
            {error, no_multipart_form_data}
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
parse_arg_value([$\\,$"], Key, Value, _, _) ->
    make_parse_line_reply(Key, [$\\|Value], []);
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
    {{yaws:funreverse(Key, fun yaws:to_lowerchar/1),
      lists:reverse(Value)}, Rest}.


-record(mp_parse_state, {
          state,
          boundary_ctx,
          boundary_len,
          hdr_end_ctx,
          old_data,
          data_type
         }).

%% Stateful parser of multipart data - allows easy re-entry
parse_multipart(Data, St) ->
    parse_multipart(Data, St, [list]).
parse_multipart(Data, St, Options) ->
    case parse_multi(Data, St, Options) of
        {cont, St2, Res} -> {cont, {cont, St2}, lists:reverse(Res)};
        {result, Res}    -> {result, lists:reverse(Res)};
        {error, Reason}  -> {error, Reason}
    end.

parse_multi(Data, #mp_parse_state{state=boundary}=ParseState, Acc) ->
    %% Find the beginning of the next part or the last boundary
    case binary:match(Data, ParseState#mp_parse_state.boundary_ctx) of
        {Pos, Len} ->
            %% If Pos != 0, ignore data preceding the boundary
            case Data of
                <<_:Pos/binary, Rest/binary>> when size(Rest) < Len+2 ->
                    %% Not enough data to tell if it is the last boundary or not
                    {cont, ParseState#mp_parse_state{old_data=Rest}, Acc};
                <<_:Pos/binary, _:Len/binary, "\r\n", Rest/binary>> ->
                    %% It is not the last boundary, so parse the next part
                    NPState = ParseState#mp_parse_state{state=start_headers},
                    parse_multi(Rest, NPState, Acc);
                <<_:Pos/binary, _:Len/binary, "--\r\n", _/binary>> ->
                    %% Match on the last boundary and ignore remaining data
                    {result, Acc};
                <<_:Pos/binary, Boundary:Len/binary, "--", Rest/binary>> when size(Rest) < 2 ->
                    %% Partial match on the last boundary; need more data
		    {cont, ParseState#mp_parse_state{old_data = <<Boundary/binary, "--", Rest/binary>>}, Acc};
                _ ->
                    {error, malformed_multipart_post}
            end;
        nomatch ->
            %% No boundary found, request more data. Here we keep just enough
            %% data to match on the boundary the next time
            DLen = size(Data),
            BLen = ParseState#mp_parse_state.boundary_len,
            SkipLen = erlang:max(DLen - BLen, 0),
            KeepLen = erlang:min(BLen, DLen),
            <<_:SkipLen/binary, OldData:KeepLen/binary>> = Data,
            {cont, ParseState#mp_parse_state{old_data=OldData}, Acc}
    end;

parse_multi(Data, #mp_parse_state{state=start_headers}=ParseState, Acc) ->
    parse_multi(Data, ParseState, Acc, [], []);

parse_multi(Data, #mp_parse_state{state=body}=ParseState, Acc) ->
    %% Find the end of this part (i.e the next boundary)
    case binary:match(Data, ParseState#mp_parse_state.boundary_ctx) of
        {Pos, _Len} ->
            %% Extract the body and keep the boundary
            <<Body:Pos/binary, Rest/binary>> = Data,
            BodyData = case ParseState#mp_parse_state.data_type of
                           list   -> binary_to_list(Body);
                           binary -> Body
                       end,
            NAcc = [{body, BodyData}|Acc],
            NParseState = ParseState#mp_parse_state{state=boundary},
            parse_multi(Rest, NParseState, NAcc);
        nomatch ->
            %% No boundary found, request more data.
            DLen = size(Data),
            BLen = ParseState#mp_parse_state.boundary_len,
            SkipLen = erlang:max(DLen - BLen, 0),
            KeepLen = erlang:min(BLen, DLen),
            <<PartData:SkipLen/binary, OldData:KeepLen/binary>> = Data,
            NParseState = ParseState#mp_parse_state{state=body,
                                                    old_data=OldData},
            BodyData = case ParseState#mp_parse_state.data_type of
                           list   -> binary_to_list(PartData);
                           binary -> PartData
                       end,
            {cont, NParseState, [{part_body, BodyData}|Acc]}
    end;

parse_multi(Data, {cont, #mp_parse_state{old_data=OldData}=ParseState}, _) ->
    %% Reentry point
    NData = <<OldData/binary, Data/binary>>,
    parse_multi(NData, ParseState, []);

parse_multi(Data, Boundary, Options) ->
    %% Initial entry point
    FullBoundary = list_to_binary(["\r\n--", Boundary]),
    BoundaryCtx  = binary:compile_pattern(FullBoundary),
    HdrEndCtx    = binary:compile_pattern(<<"\r\n\r\n">>),
    DataType     = lists:foldl(fun(_,      list)      -> list;
				  (list,   _)         -> list;
				  (binary, undefined) -> binary;
				  (_,      Acc)       -> Acc
			       end, undefined, Options),
    ParseState = #mp_parse_state{state        = boundary,
                                 boundary_ctx = BoundaryCtx,
                                 boundary_len = size(FullBoundary),
                                 hdr_end_ctx  = HdrEndCtx,
                                 data_type    = DataType},
    parse_multi(<<"\r\n", Data/binary>>, ParseState, []).


parse_multi(Data, #mp_parse_state{state=start_headers}=ParseState,
            Acc, [], []) ->
    %% Find the end of headers for this part
    case binary:match(Data, ParseState#mp_parse_state.hdr_end_ctx) of
        {_Pos, _Len} ->
            %% We have all headers, we can parse it
            NParseState = ParseState#mp_parse_state{state=headers},
            parse_multi(Data, NParseState, Acc, [], []);
        nomatch ->
            {cont, ParseState#mp_parse_state{old_data=Data}, Acc}
    end;
parse_multi(Data, #mp_parse_state{state=headers}=ParseState, Acc, Name, Hdrs) ->
    case erlang:decode_packet(httph_bin, Data, [{packet_size, 16#4000}]) of
        {ok, http_eoh, Rest} ->
            %% All headers are parsed, get the body now
            Head = case Name of
                       [] -> lists:reverse(Hdrs);
                       _  -> {Name, lists:reverse(Hdrs)}
                   end,
            NParseState = ParseState#mp_parse_state{state=body},
            parse_multi(Rest, NParseState, [{head, Head}|Acc]);
        {ok, {http_header, _, Hdr, _, HdrVal}, Rest} when is_atom(Hdr) ->
            Header = {case Hdr of
                          'Content-Type' -> content_type;
                          Else           -> Else
                      end,
                      binary_to_list(HdrVal)},
            parse_multi(Rest, ParseState, Acc, Name, [Header|Hdrs]);
        {ok, {http_header, _, Hdr, _, HdrVal}, Rest} ->
            HdrValStr = binary_to_list(HdrVal),
            case yaws:to_lower(binary_to_list(Hdr)) of
                "content-disposition" ->
                    "form-data"++Params = HdrValStr,
                    Parameters = parse_arg_line(Params),
                    {_, NewName} = lists:keyfind("name", 1, Parameters),
                    parse_multi(Rest, ParseState, Acc,
                                NewName, Parameters++Hdrs);
                LowerHdr ->
                    parse_multi(Rest, ParseState, Acc,
                                Name, [{LowerHdr, HdrValStr}|Hdrs])
            end;
        _ ->
            {error, malformed_multipart_post}
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
code_to_phrase(102) -> "Processing";
code_to_phrase(200) -> "OK";
code_to_phrase(201) -> "Created";
code_to_phrase(202) -> "Accepted";
code_to_phrase(203) -> "Non-Authoritative Information";
code_to_phrase(204) -> "No Content";
code_to_phrase(205) -> "Reset Content";
code_to_phrase(206) -> "Partial Content";
code_to_phrase(207) -> "Multi-Status";
code_to_phrase(208) -> "Already Reported";
code_to_phrase(226) -> "IM Used";
code_to_phrase(300) -> "Multiple Choices";
code_to_phrase(301) -> "Moved Permanently";
code_to_phrase(302) -> "Found";
code_to_phrase(303) -> "See Other";
code_to_phrase(304) -> "Not Modified";
code_to_phrase(305) -> "Use Proxy";
code_to_phrase(306) -> "(Unused)";
code_to_phrase(307) -> "Temporary Redirect";
code_to_phrase(308) -> "Permanent Redirect";
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
code_to_phrase(418) -> "I'm a teapot";
code_to_phrase(420) -> "Enhance Your Calm";
code_to_phrase(422) -> "Unprocessable Entity";
code_to_phrase(423) -> "Locked";
code_to_phrase(424) -> "Failed Dependency";
code_to_phrase(425) -> "Unordered Collection";
code_to_phrase(426) -> "Upgrade Required";
code_to_phrase(428) -> "Precondition Required";
code_to_phrase(429) -> "Too Many Requests";
code_to_phrase(431) -> "Request Header Fields Too Large";
code_to_phrase(451) -> "Unavailable For Legal Reasons";
code_to_phrase(500) -> "Internal Server Error";
code_to_phrase(501) -> "Not Implemented";
code_to_phrase(502) -> "Bad Gateway";
code_to_phrase(503) -> "Service Unavailable";
code_to_phrase(504) -> "Gateway Timeout";
code_to_phrase(505) -> "HTTP Version Not Supported";
code_to_phrase(506) -> "Variant Also Negotiates";
code_to_phrase(507) -> "Insufficient Storage";
code_to_phrase(508) -> "Loop Detected";
code_to_phrase(510) -> "Not Extended";
code_to_phrase(511) -> "Network Authentication Required";

%% Below are some non-HTTP status codes from other protocol standards that
%% we've seen used with HTTP in the wild, so we include them here. HTTP 1.1
%% section 6.1.1 allows for this sort of extensibility, but we recommend
%% sticking with the HTTP status codes above for maximal portability and
%% interoperability.
%%
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
    {MS, S, _} = yaws:get_time_tuple(),
    (MS * 1000000) + S.

cookie_option(secure) ->
    "; Secure";
cookie_option(http_only) ->
    "; HttpOnly";
cookie_option(I) ->
    throw({badarg, I}).
cookie_option(expires, UTC) when is_tuple(UTC) ->
    ["; Expires=" | yaws:universal_time_as_string(UTC)];
cookie_option(max_age, Age) when is_integer(Age) ->
    V = if Age < 0 -> "0"; true -> integer_to_list(Age) end,
    ["; Max-Age=" | V];
cookie_option(path, Path) when is_list(Path), Path =/= [] ->
    ["; Path=" | Path];
cookie_option(domain, Domain) when is_list(Domain), Domain =/= [] ->
    ["; Domain=" | Domain];
cookie_option(comment, Comment) when is_list(Comment), Comment=/= [] ->
    ["; Comment=" | Comment];
cookie_option(I, _) ->
    throw({badarg, I}).

%% @doc Generate a set_cookie header field tuple.
%%      This function is more RFC6265 compliant than setcookie/6 and
%%      therefore it deprecates setcookie/6 completely.
set_cookie(Key, Value, Options)
        when is_list(Key), is_list(Value), is_list(Options) ->
    %% RFC6265 (4.1.1): Name=Value options must come first.
    {NV,SV} = lists:foldl(fun
        ({N,V}, {L1, L2}) -> {[cookie_option(N,V) | L1], L2};
        (N,     {L1, L2}) -> {L1, [cookie_option(N) | L2]}
    end, {[], []}, Options),
    {header, {set_cookie, [Key, $=, Value, "; Version=1", NV | SV]}}.

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
%% if several cookies with the same name are passed fron the browser,
%% only the first match is returned
find_cookie_val(Name, #arg{}=A) ->
    find_cookie_val(Name, (A#arg.headers)#headers.cookie);
find_cookie_val(Name, Cookies) ->
    find_cookie_val2(yaws:to_lower(Name), Cookies).

find_cookie_val2(_, []) ->
    [];
find_cookie_val2(Name, [Cookie|Rest]) ->
    L = parse_cookie(Cookie),
    case lists:keyfind(Name, #cookie.key, L) of
        #cookie{value=undefined} -> [];
        #cookie{value=Value}     -> Value;
        false                    -> find_cookie_val2(Name, Rest)
    end.


%%
url_decode(Path) ->
    url_decode_with_encoding(Path, file:native_name_encoding()).

url_decode_with_encoding(Path, Encoding) ->
    {DecPath, QS} = url_decode(Path, []),
    DecPath1 = case Encoding of
                   latin1 ->
                       DecPath;
                   utf8 ->
                       case unicode:characters_to_list(list_to_binary(DecPath)) of
                           UTF8DecPath when is_list(UTF8DecPath) -> UTF8DecPath;
                           _ -> DecPath
                       end
               end,
    case QS of
        [] -> lists:flatten(DecPath1);
        _  -> lists:flatten([DecPath1, $?, QS])
    end.

url_decode([], Acc) ->
    {lists:reverse(Acc), []};
url_decode([$?|Tail], Acc) ->
    %% Don't decode the query string here, that is parsed separately.
    {lists:reverse(Acc), Tail};
url_decode([$%, Hi, Lo | Tail], Acc) ->
    Hex = yaws:hex_to_integer([Hi, Lo]),
    url_decode(Tail, [Hex|Acc]);
url_decode([H|T], Acc) when is_integer(H) ->
    url_decode(T, [H|Acc]);
%% deep lists
url_decode([H|T], Acc) when is_list(H) ->
    case url_decode(H, Acc) of
        {P1, []} ->
            {P2, QS} = url_decode(T, []),
            {[P1,P2], QS};
        {P1, QS} ->
            {P1, QS++T}
    end.


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
    {DecPath, QS} = url_decode_q_split(Path, []),
    case file:native_name_encoding() of
        latin1 ->
            {DecPath, QS};
        utf8 ->
            case unicode:characters_to_list(list_to_binary(DecPath)) of
                UTF8DecPath when is_list(UTF8DecPath) -> {UTF8DecPath, QS};
                _ -> {DecPath, QS}
            end
    end.

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


url_encode(URL) when is_list(URL) ->
    Bin = case file:native_name_encoding() of
              latin1 -> list_to_binary(URL);
              utf8   -> unicode:characters_to_binary(URL)
          end,
    %% ReservedChars = "!*'();:@&=+$,/?%#[]",
    UnreservedChars = sets:from_list("ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                     "abcdefghijklmnopqrstuvwxyz"
                                     "0123456789-_.~"),
    flatten([url_encode_byte(Byte, UnreservedChars) || <<Byte>> <= Bin]).

url_encode_byte($:, _) -> $:;  % FIXME: both : and / should be encoded, but
url_encode_byte($/, _) -> $/;  % too much code currently assumes they're not
url_encode_byte(Byte, UnreservedChars) ->
    case sets:is_element(Byte, UnreservedChars) of
        true -> Byte;
        false ->
            case yaws:integer_to_hex(Byte) of
                [X, Y] -> [$%, X, Y];
                [X]    -> [$%, $0, X]
            end
    end.

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
    mime_type(get(sc), FileName).

mime_type(S, FileName) ->
    case filename:extension(FileName) of
        [_|T] -> element(2, mime_types:t(S, T));
        []    -> element(2, mime_types:t(S, []))
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

stream_process_deliver({ssl, SslSock}, IoList) ->
    ssl:send(SslSock, IoList);
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
stream_process_end({ssl, SslSock}, YawsPid) ->
    ssl:controlling_process(SslSock, YawsPid),
    YawsPid ! endofstreamcontent;
stream_process_end(Sock, YawsPid) ->
    gen_tcp:controlling_process(Sock, YawsPid),
    YawsPid ! endofstreamcontent.


websocket_send(#ws_state{}=WSState, {Type, Data}) ->
    yaws_websockets:send(WSState, {Type, Data});
websocket_send(#ws_state{}=WSState, #ws_frame{}=Frame) ->
    yaws_websockets:send(WSState, Frame);
%% Pid must be the process in control of the websocket connection.
websocket_send(Pid, {Type, Data}) when is_pid(Pid) ->
    yaws_websockets:send(Pid, {Type, Data});
websocket_send(Pid, #ws_frame{}=Frame) when is_pid(Pid) ->
    yaws_websockets:send(Pid, Frame).

websocket_close(#ws_state{}=WSState) ->
    yaws_websockets:close(WSState, normal);
websocket_close(Pid) when is_pid(Pid) ->
    yaws_websockets:close(Pid, normal).
websocket_close(#ws_state{}=WSState, Reason) ->
    yaws_websockets:close(WSState, Reason);
websocket_close(Pid, Reason) when is_pid(Pid) ->
    yaws_websockets:close(Pid, Reason).


%% returns {ok, SSL socket} if an SSL socket, undefined otherwise
get_sslsocket({ssl, SslSocket}) ->
    {ok, SslSocket};
get_sslsocket(_Socket) ->
    undefined.

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
replace_cookie_session(Cookie, NewOpaque, Cleanup) ->
    yaws_session_server:replace_session(Cookie, NewOpaque, Cleanup).

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
    yaws_trace:set_tty_trace(Bool).



set_status_code(Code) ->
    {status, Code}.




%% returns [ Header1, Header2 .....]
reformat_header(H) ->
    FormatFun = fun(Hname, {multi, Values}) ->
                        [lists:flatten(io_lib:format("~s: ~s", [Hname, Val])) ||
                            Val <- Values];
                   (Hname, Str) ->
                        lists:flatten(io_lib:format("~s: ~s", [Hname, Str]))
                end,
    reformat_header(H, FormatFun).
reformat_header(H, FormatFun) ->
    lists:zf(fun({Hname, Str}) ->
                     I =  FormatFun(Hname, Str),
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
              if H#headers.content_encoding == undefined ->
                      undefined;
                 true ->
                      {"Content-Encoding", H#headers.content_encoding}
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
              end,
              if H#headers.x_forwarded_for == undefined ->
                      undefined;
                 true ->
                      {"X-Forwarded-For", H#headers.x_forwarded_for}
              end

             ]
            ) ++
        lists:map(
          fun({http_header,_,K,_,V}) ->
                  FormatFun(K,V)
          end, H#headers.other).


set_header(#headers{}=Hdrs, {Header, Value}) ->
    set_header(Hdrs, Header, Value).

set_header(#headers{}=Hdrs, connection, Value) ->
    Hdrs#headers{connection = Value};
set_header(#headers{}=Hdrs, {lower, "connection"}, Value) ->
    Hdrs#headers{connection = Value};
set_header(#headers{}=Hdrs, accept, Value) ->
    Hdrs#headers{accept = Value};
set_header(#headers{}=Hdrs, {lower, "accept"}, Value) ->
    Hdrs#headers{accept = Value};
set_header(#headers{}=Hdrs, host, Value) ->
    Hdrs#headers{host = Value};
set_header(#headers{}=Hdrs, {lower, "host"}, Value) ->
    Hdrs#headers{host = Value};
set_header(#headers{}=Hdrs, if_modified_since, Value) ->
    Hdrs#headers{if_modified_since = Value};
set_header(#headers{}=Hdrs, {lower, "if-modified-since"}, Value) ->
    Hdrs#headers{if_modified_since = Value};
set_header(#headers{}=Hdrs, if_match, Value) ->
    Hdrs#headers{if_match = Value};
set_header(#headers{}=Hdrs, {lower, "if-match"}, Value) ->
    Hdrs#headers{if_match = Value};
set_header(#headers{}=Hdrs, if_none_match, Value) ->
    Hdrs#headers{if_none_match = Value};
set_header(#headers{}=Hdrs, {lower, "if-none-match"}, Value) ->
    Hdrs#headers{if_none_match = Value};
set_header(#headers{}=Hdrs, if_range, Value) ->
    Hdrs#headers{if_range = Value};
set_header(#headers{}=Hdrs, {lower, "if-range"}, Value) ->
    Hdrs#headers{if_range = Value};
set_header(#headers{}=Hdrs, if_unmodified_since, Value) ->
    Hdrs#headers{if_unmodified_since = Value};
set_header(#headers{}=Hdrs, {lower, "if-unmodified-since"}, Value) ->
    Hdrs#headers{if_unmodified_since = Value};
set_header(#headers{}=Hdrs, range, Value) ->
    Hdrs#headers{range = Value};
set_header(#headers{}=Hdrs, {lower, "range"}, Value) ->
    Hdrs#headers{range = Value};
set_header(#headers{}=Hdrs, referer, Value) ->
    Hdrs#headers{referer = Value};
set_header(#headers{}=Hdrs, {lower, "referer"}, Value) ->
    Hdrs#headers{referer = Value};
set_header(#headers{}=Hdrs, user_agent, Value) ->
    Hdrs#headers{user_agent = Value};
set_header(#headers{}=Hdrs, {lower, "user-agent"}, Value) ->
    Hdrs#headers{user_agent = Value};
set_header(#headers{}=Hdrs, accept_ranges, Value) ->
    Hdrs#headers{accept_ranges = Value};
set_header(#headers{}=Hdrs, {lower, "accept-ranges"}, Value) ->
    Hdrs#headers{accept_ranges = Value};
set_header(#headers{}=Hdrs, cookie, Value) ->
    Hdrs#headers{cookie = Value};
set_header(#headers{}=Hdrs, {lower, "cookie"}, Value) ->
    Hdrs#headers{cookie = Value};
set_header(#headers{}=Hdrs, keep_alive, Value) ->
    Hdrs#headers{keep_alive = Value};
set_header(#headers{}=Hdrs, {lower, "keep-alive"}, Value) ->
    Hdrs#headers{keep_alive = Value};
set_header(#headers{}=Hdrs, location, Value) ->
    Hdrs#headers{location = Value};
set_header(#headers{}=Hdrs, {lower, "location"}, Value) ->
    Hdrs#headers{location = Value};
set_header(#headers{}=Hdrs, content_length, Value) ->
    Hdrs#headers{content_length = Value};
set_header(#headers{}=Hdrs, {lower, "content-length"}, Value) ->
    Hdrs#headers{content_length = Value};
set_header(#headers{}=Hdrs, content_type, Value) ->
    Hdrs#headers{content_type = Value};
set_header(#headers{}=Hdrs, {lower, "content-type"}, Value) ->
    Hdrs#headers{content_type = Value};
set_header(#headers{}=Hdrs, content_encoding, Value) ->
    Hdrs#headers{content_encoding = Value};
set_header(#headers{}=Hdrs, {lower, "content-encoding"}, Value) ->
    Hdrs#headers{content_encoding = Value};
set_header(#headers{}=Hdrs, authorization, Value) ->
    Hdrs#headers{authorization = Value};
set_header(#headers{}=Hdrs, {lower, "authorization"}, Value) ->
    Hdrs#headers{authorization = Value};
set_header(#headers{}=Hdrs, transfer_encoding, Value) ->
    Hdrs#headers{transfer_encoding = Value};
set_header(#headers{}=Hdrs, {lower, "transfer-encoding"}, Value) ->
    Hdrs#headers{transfer_encoding = Value};
set_header(#headers{}=Hdrs, x_forwarded_for, Value) ->
    Hdrs#headers{x_forwarded_for = Value};
set_header(#headers{}=Hdrs, {lower, "x-forwarded-for"}, Value) ->
    Hdrs#headers{x_forwarded_for = Value};
set_header(#headers{}=Hdrs, Header, Value) when is_atom(Header) ->
    set_header(Hdrs, atom_to_list(Header), Value);
set_header(#headers{}=Hdrs, Header, Value) when is_binary(Header) ->
    set_header(Hdrs, binary_to_list(Header), Value);
set_header(#headers{}=Hdrs, Header, Val) when is_binary(Val) ->
    set_header(Hdrs, {lower, string:to_lower(Header)}, binary_to_list(Val));
set_header(#headers{other=Other}=Hdrs, {lower, Header}, undefined) ->
    Handler = fun(_, true, Acc) ->
                      Acc;
                 (HdrVal, false, Acc) ->
                      [HdrVal|Acc]
              end,
    NewOther = fold_others(Header, Handler, Other, []),
    Hdrs#headers{other = lists:reverse(NewOther)};
set_header(#headers{other=Other}=Hdrs, {lower, Header}, Val) ->
    HdrName = erlang_header_name(Header),
    Handler = fun({http_header, Int, _, Rsv, _}, true, {Acc, _}) ->
                      {[{http_header, Int, HdrName, Rsv, Val}|Acc],true};
                 (HdrVal, false, {Acc, Found}) ->
                      {[HdrVal|Acc], Found}
              end,
    {NewOther0, Found} = fold_others(Header, Handler, Other, {[], false}),
    NewOther = case Found of
                   true ->
                       NewOther0;
                   false ->
                       [{http_header, 0, HdrName, undefined, Val}|NewOther0]
               end,
    Hdrs#headers{other = lists:reverse(NewOther)};
set_header(#headers{}=Hdrs, Header, undefined) ->
    set_header(Hdrs, {lower, string:to_lower(Header)}, undefined);
set_header(#headers{}=Hdrs, Header, Value) ->
    set_header(Hdrs, {lower, string:to_lower(Header)}, Value).

merge_header(#headers{}=Hdrs, {Header, Value}) ->
    merge_header(Hdrs, Header, Value).

merge_header(#headers{}=Hdrs, _Header, undefined) ->
    Hdrs;
merge_header(#headers{}=Hdrs, Header, Value) when is_atom(Header) ->
    merge_header(Hdrs, atom_to_list(Header), Value);
merge_header(#headers{}=Hdrs, Header, Value) when is_binary(Header) ->
    merge_header(Hdrs, binary_to_list(Header), Value);
merge_header(#headers{}=Hdrs, Header, Value) when is_binary(Value) ->
    merge_header(Hdrs, Header, binary_to_list(Value));
merge_header(Hdrs, {lower, "set-cookie"}=LHdr, Value) ->
    NewValue = case get_header(Hdrs, LHdr) of
                   undefined ->
                       {multi, [Value]};
                   {multi, MultiVal} ->
                       {multi, MultiVal ++ [Value]};
                   ExistingValue ->
                       {multi, [ExistingValue, Value]}
               end,
    set_header(Hdrs, LHdr, NewValue);
merge_header(Hdrs, {lower, _Header}=LHdr, Value) ->
    NewValue = case get_header(Hdrs, LHdr) of
                   undefined ->
                       Value;
                   ExistingValue ->
                       ExistingValue ++ ", " ++ Value
               end,
    set_header(Hdrs, LHdr, NewValue);
merge_header(#headers{}=Hdrs, Header, Value) ->
    merge_header(Hdrs, {lower, string:to_lower(Header)}, Value).

get_header(#headers{}=Hdrs, connection) ->
    Hdrs#headers.connection;
get_header(#headers{}=Hdrs, {lower, "connection"}) ->
    Hdrs#headers.connection;
get_header(#headers{}=Hdrs, accept) ->
    Hdrs#headers.accept;
get_header(#headers{}=Hdrs, {lower, "accept"}) ->
    Hdrs#headers.accept;
get_header(#headers{}=Hdrs, host) ->
    Hdrs#headers.host;
get_header(#headers{}=Hdrs, {lower, "host"}) ->
    Hdrs#headers.host;
get_header(#headers{}=Hdrs, if_modified_since) ->
    Hdrs#headers.if_modified_since;
get_header(#headers{}=Hdrs, {lower, "if-modified-since"}) ->
    Hdrs#headers.if_modified_since;
get_header(#headers{}=Hdrs, if_match) ->
    Hdrs#headers.if_match;
get_header(#headers{}=Hdrs, {lower, "if-match"}) ->
    Hdrs#headers.if_match;
get_header(#headers{}=Hdrs, if_none_match) ->
    Hdrs#headers.if_none_match;
get_header(#headers{}=Hdrs, {lower, "if-none-match"}) ->
    Hdrs#headers.if_none_match;
get_header(#headers{}=Hdrs, if_range) ->
    Hdrs#headers.if_range;
get_header(#headers{}=Hdrs, {lower, "if-range"}) ->
    Hdrs#headers.if_range;
get_header(#headers{}=Hdrs, if_unmodified_since) ->
    Hdrs#headers.if_unmodified_since;
get_header(#headers{}=Hdrs, {lower, "if-unmodified-since"}) ->
    Hdrs#headers.if_unmodified_since;
get_header(#headers{}=Hdrs, range) ->
    Hdrs#headers.range;
get_header(#headers{}=Hdrs, {lower, "range"}) ->
    Hdrs#headers.range;
get_header(#headers{}=Hdrs, referer) ->
    Hdrs#headers.referer;
get_header(#headers{}=Hdrs, {lower, "referer"}) ->
    Hdrs#headers.referer;
get_header(#headers{}=Hdrs, user_agent) ->
    Hdrs#headers.user_agent;
get_header(#headers{}=Hdrs, {lower, "user-agent"}) ->
    Hdrs#headers.user_agent;
get_header(#headers{}=Hdrs, accept_ranges) ->
    Hdrs#headers.accept_ranges;
get_header(#headers{}=Hdrs, {lower, "accept-ranges"}) ->
    Hdrs#headers.accept_ranges;
get_header(#headers{}=Hdrs, cookie) ->
    Hdrs#headers.cookie;
get_header(#headers{}=Hdrs, {lower, "cookie"}) ->
    Hdrs#headers.cookie;
get_header(#headers{}=Hdrs, keep_alive) ->
    Hdrs#headers.keep_alive;
get_header(#headers{}=Hdrs, {lower, "keep-alive"}) ->
    Hdrs#headers.keep_alive;
get_header(#headers{}=Hdrs, location) ->
    Hdrs#headers.location;
get_header(#headers{}=Hdrs, {lower, "location"}) ->
    Hdrs#headers.location;
get_header(#headers{}=Hdrs, content_length) ->
    Hdrs#headers.content_length;
get_header(#headers{}=Hdrs, {lower, "content-length"}) ->
    Hdrs#headers.content_length;
get_header(#headers{}=Hdrs, content_type) ->
    Hdrs#headers.content_type;
get_header(#headers{}=Hdrs, {lower, "content-type"}) ->
    Hdrs#headers.content_type;
get_header(#headers{}=Hdrs, content_encoding) ->
    Hdrs#headers.content_encoding;
get_header(#headers{}=Hdrs, {lower, "content-encoding"}) ->
    Hdrs#headers.content_encoding;
get_header(#headers{}=Hdrs, authorization) ->
    Hdrs#headers.authorization;
get_header(#headers{}=Hdrs, {lower, "authorization"}) ->
    Hdrs#headers.authorization;
get_header(#headers{}=Hdrs, transfer_encoding) ->
    Hdrs#headers.transfer_encoding;
get_header(#headers{}=Hdrs, {lower, "transfer-encoding"}) ->
    Hdrs#headers.transfer_encoding;
get_header(#headers{}=Hdrs, x_forwarded_for) ->
    Hdrs#headers.x_forwarded_for;
get_header(#headers{}=Hdrs, {lower, "x-forwarded-for"}) ->
    Hdrs#headers.x_forwarded_for;
get_header(#headers{}=Hdrs, Header) when is_atom(Header) ->
    get_header(Hdrs, atom_to_list(Header));
get_header(#headers{}=Hdrs, Header) when is_binary(Header) ->
    get_header(Hdrs, binary_to_list(Header));
get_header(#headers{other = Other}, {lower, Header}) ->
    Handler = fun({http_header, _, _, _, Value}, true, _Acc) ->
                      throw(Value);
                 (_, false, Acc) ->
                      Acc
              end,
    catch fold_others(Header, Handler, Other, undefined);
get_header(#headers{}=Hdrs, Header) ->
    get_header(Hdrs, {lower, string:to_lower(Header)}).

get_header(#headers{}=Hdrs, Header, Default) ->
    case get_header(Hdrs, Header) of
        undefined ->
            Default;
        Value ->
            Value
    end.

delete_header(#headers{}=Hdrs, Header) ->
    set_header(Hdrs, Header, undefined).

%% assumes that LowerHdr is already downcased
fold_others(LowerHdr, Handler, Other, StartAcc) ->
    lists:foldl(fun({http_header, _, Hdr, _, _}=HdrVal, Acc) ->
                        HdrNm = string:to_lower(
                                  if
                                      is_atom(Hdr) -> atom_to_list(Hdr);
                                      is_binary(Hdr) -> binary_to_list(Hdr);
                                      true -> Hdr
                                  end),
                        Handler(HdrVal, HdrNm == LowerHdr, Acc)
                end, StartAcc, Other).

erlang_header_name("cache-control")       -> 'Cache-Control';
erlang_header_name("date")                -> 'Date';
erlang_header_name("pragma")              -> 'Pragma';
erlang_header_name("upgrade")             -> 'Upgrade';
erlang_header_name("via")                 -> 'Via';
erlang_header_name("accept-charset")      -> 'Accept-Charset';
erlang_header_name("accept-encoding")     -> 'Accept-Encoding';
erlang_header_name("accept-language")     -> 'Accept-Language';
erlang_header_name("from")                -> 'From';
erlang_header_name("max-forwards")        -> 'Max-Forwards';
erlang_header_name("proxy-authorization") -> 'Proxy-Authorization';
erlang_header_name("age")                 -> 'Age';
erlang_header_name("proxy-authenticate")  -> 'Proxy-Authenticate';
erlang_header_name("public")              -> 'Public';
erlang_header_name("retry-after")         -> 'Retry-After';
erlang_header_name("server")              -> 'Server';
erlang_header_name("vary")                -> 'Vary';
erlang_header_name("warning")             -> 'Warning';
erlang_header_name("www-authenticate")    -> 'Www-Authenticate';
erlang_header_name("allow")               -> 'Allow';
erlang_header_name("content-base")        -> 'Content-Base';
erlang_header_name("content-encoding")    -> 'Content-Encoding';
erlang_header_name("content-language")    -> 'Content-Language';
erlang_header_name("content-location")    -> 'Content-Location';
erlang_header_name("content-md5")         -> 'Content-Md5';
erlang_header_name("content-range")       -> 'Content-Range';
erlang_header_name("etag")                -> 'Etag';
erlang_header_name("expires")             -> 'Expires';
erlang_header_name("last-modified")       -> 'Last-Modified';
erlang_header_name("set-cookie")          -> 'Set-Cookie';
erlang_header_name("set-cookie2")         -> 'Set-Cookie2';
erlang_header_name("proxy-connection")    -> 'Proxy-Connection';
erlang_header_name(Name)                  -> capitalize_header(Name).

capitalize_header(Name) ->
    %% Before R16B Erlang capitalized words inside header names only for
    %% headers less than 20 characters long. In R16B that length was raised
    %% to 50. Using decode_packet lets us be portable.
    {ok, {http_header, _, Result, _, _}, _} =
        erlang:decode_packet(httph, list_to_binary([Name, <<": x\r\n\r\n">>]),
                             []),
    Result.

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
         Url#url.host == undefined orelse Url#url.host == [] ->
             yaws:redirect_host(SC, undefined);
         true ->
             Url#url.host
     end,
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
%%         {Module, Fun, [Args]} | fun/0 |
%%         binary() | character()
%% Tag   = atom()
%% Attrs = [{Key, Value}]
%% Key   = atom()
%% Value = string() | binary() | atom() | integer() | float() |
%%         {Module, Fun, [Args]} | fun/0
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
    ["<", atom_to_list(Tag), ehtml_end_tag(Tag)];
ehtml_expand({pre_html, X}) -> X;
ehtml_expand({Mod, Fun, Args})
  when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ehtml_expand(Mod:Fun(Args));
ehtml_expand({Tag, Attrs}) ->
    NL = ehtml_nl(Tag),
    [NL, "<", atom_to_list(Tag), ehtml_attrs(Attrs), ehtml_end_tag(Tag)];
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
    ValueString = [$", value2string(Value), $"],
    [[$ |atom_to_list(Name)], [$=|ValueString]|ehtml_attrs(Tail)];
ehtml_attrs([{check, Name, {Mod, Fun, Args}} | Tail])
  when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ehtml_attrs([{check, Name,  Mod:Fun(Args)} | Tail]);
ehtml_attrs([{check, Name, Value} | Tail]) when is_function(Value) ->
    ehtml_attrs([{check, Name, Value()} | Tail]);
ehtml_attrs([{check, Name, Value} | Tail]) ->
    Val = value2string(Value),
    Q = case deepmember($", Val) of
            true -> $';
            false -> $"
        end,
    ValueString = [Q,Val,Q],
    [[$ |atom_to_list(Name)], [$=|ValueString]|ehtml_attrs(Tail)].

value2string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
value2string(String) when is_list(String) -> String;
value2string(Binary) when is_binary(Binary) -> Binary;
value2string(Integer) when is_integer(Integer) -> integer_to_list(Integer);
value2string(Float) when is_float(Float) -> float_to_list(Float).



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


%% Void elements must not have an end tag (</tag>) in HTML5, while for most
%% elements a proper end tag (<tag></tag>, not <tag />) is mandatory.
%%
%% http://www.w3.org/TR/html5/syntax.html#void-elements
%% http://www.w3.org/TR/html5/syntax.html#syntax-tag-omission

-define(self_closing, " />"). % slash ignored in HTML5

ehtml_end_tag(area) -> ?self_closing;
ehtml_end_tag(base) -> ?self_closing;
ehtml_end_tag(br) -> ?self_closing;
ehtml_end_tag(col) -> ?self_closing;
ehtml_end_tag(embed) -> ?self_closing;
ehtml_end_tag(hr) -> ?self_closing;
ehtml_end_tag(img) -> ?self_closing;
ehtml_end_tag(input) -> ?self_closing;
ehtml_end_tag(keygen) -> ?self_closing;
ehtml_end_tag(link) -> ?self_closing;
ehtml_end_tag(meta) -> ?self_closing;
ehtml_end_tag(param) -> ?self_closing;
ehtml_end_tag(source) -> ?self_closing;
ehtml_end_tag(track) -> ?self_closing;
ehtml_end_tag(wbr) -> ?self_closing;
ehtml_end_tag(Tag) -> ["></", atom_to_list(Tag), ">"].


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
    ehtml_expander_done(["<", atom_to_list(Tag), ehtml_end_tag(Tag)],
                        Before, After);
ehtml_expander({Tag, Attrs}, Before, After) ->
    NL = ehtml_nl(Tag),
    ehtml_expander_done([NL, "<", atom_to_list(Tag), ehtml_attrs(Attrs),
                         ehtml_end_tag(Tag)],
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
%% {path_info, string()} : Override the pathinfo string from Arg.
%%
%% {extra_env, [{string()|binary(), string()|binary()}]} : Extra
%% environment variables to be passed to the application server, as a list
%% of name-value pairs.
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
    deepmember(C, Cs);
deepmember(_C,<<>>) ->
    false;
deepmember(C, <<C,_Cs/binary>>) ->
    true;
deepmember(C, <<_,Cs/binary>>) ->
    deepmember(C, Cs).


%%  . Parse a Set-Cookie header, following the RFC6265:
%%
%% "Set-Cookie: " set-cookie-string
%%    set-cookie-string = cookie-pair *( ";" SP cookie-av )
%%    cookie-pair       = cookie-name "=" cookie-value
%%    cookie-name       = token
%%    cookie-value      = *cookie-octet / ( DQUOTE *cookie-octet DQUOTE )
%%    cookie-octet      = %x21 / %x23-2B / %x2D-3A / %x3C-5B / %x5D-7E
%%    token             = <token, defined in [RFC2616], Section 2.2>
%%
%%    cookie-av         = expires-av / max-age-av / domain-av / path-av /
%%                        secure-av / httponly-av / extension-av
%%    expires-av        = "Expires=" <rfc1123-date, defined in [RFC2616]>
%%    max-age-av        = "Max-Age=" [1-9] *DIGIT
%%    domain-av         = "Domain=" <subdomain> ; defined in [RFC1034]
%%    path-av           = "Path=" <any CHAR except CTLs or ";">
%%    secure-av         = "Secure"
%%    httponly-av       = "HttpOnly"
%%    extension-av      = <any CHAR except CTLs or ";">
%%
%% NOTE: in RFC2109 and RFC2965, multiple cookies, separated by comma, can be
%% defined in a single header. So, To be backward compatible with these RFCs,
%% comma is forbidden in 'path-av' and 'extension-av' except for double-quoted
%% value.
%%
%%
%%  . Parse a Cookie header, following the RFC6265:
%%
%% "Cookie: " cookie-string
%%    cookie-string = cookie-pair *( ";" SP cookie-pair )
%%
%% NOTE: To be backward compatible with RFCs, comma is considered as a cookie
%% separator, like semicolon.
%%
parse_set_cookie(Str) ->
    parse_set_cookie(Str, []).

parse_set_cookie([], [SetCookie]) ->
    SetCookie;
parse_set_cookie([], SetCookies) ->
    lists:reverse(SetCookies);
parse_set_cookie(Str, SetCookies) ->
    case do_parse_set_cookie(Str) of
        {#setcookie{extensions=Exts}=C0, Rest} ->
            C1 = C0#setcookie{extensions=lists:reverse(Exts)},
            parse_set_cookie(Rest, [C1|SetCookies]);
        error ->
            []
    end.


do_parse_set_cookie(Str) ->
    {Key, Rest0} = parse_cookie_key(skip_space(Str), []),
    case yaws:to_lower(Key) of
        [] ->
            error;
        K ->
            Cookie0 = #setcookie{key=K, quoted=false},
            case skip_space(Rest0) of
                [$=|Rest1] ->
                    {V, Q, Rest2} = parse_cookie_value(skip_space(Rest1)),
                    Cookie1 = Cookie0#setcookie{value=V, quoted=Q},
                    parse_set_cookie_result(Cookie1, skip_space(Rest2));
                [$;|Rest1] -> parse_set_cookie_options(Rest1, Cookie0);
                [$,|Rest1] -> {Cookie0, Rest1};
                []         -> {Cookie0, []};
                _          -> error
            end
    end.

parse_set_cookie_options(Str, Cookie0) ->
    {Key, Rest0} = parse_cookie_key(skip_space(Str), []),
    case yaws:to_lower(Key) of
        [] ->
            {Cookie0, Rest0};
        "domain" ->
            case skip_space(Rest0) of
                [$=|Rest1] ->
                    {V,_,Rest2} = parse_set_cookie_domain(skip_space(Rest1),[]),
                    Cookie1 = Cookie0#setcookie{domain=V},
                    parse_set_cookie_result(Cookie1, skip_space(Rest2));
                [$;|Rest1] -> parse_set_cookie_options(Rest1, Cookie0);
                [$,|Rest1] -> {Cookie0, Rest1};
                []         -> {Cookie0, []};
                _          -> error
            end;
        "max-age" ->
            case skip_space(Rest0) of
                [$=|Rest1] ->
                    {V,_,Rest2} = parse_set_cookie_maxage(skip_space(Rest1),[]),
                    Cookie1 = Cookie0#setcookie{max_age=V},
                    parse_set_cookie_result(Cookie1, skip_space(Rest2));
                [$;|Rest1] -> parse_set_cookie_options(Rest1, Cookie0);
                [$,|Rest1] -> {Cookie0, Rest1};
                []         -> {Cookie0, []};
                _          -> error
            end;
        "expires" ->
            case skip_space(Rest0) of
                [$=|Rest1] ->
                    {V, _, Rest2} = parse_set_cookie_expires(skip_space(Rest1)),
                    Cookie1 = Cookie0#setcookie{expires=V},
                    parse_set_cookie_result(Cookie1, skip_space(Rest2));
                [$;|Rest1] -> parse_set_cookie_options(Rest1, Cookie0);
                [$,|Rest1] -> {Cookie0, Rest1};
                []         -> {Cookie0, []};
                _          -> error
            end;
        "path" ->
            case skip_space(Rest0) of
                [$=|Rest1] ->
                    {V, _, Rest2} = parse_cookie_value(skip_space(Rest1)),
                    Cookie1 = Cookie0#setcookie{path=V},
                    parse_set_cookie_result(Cookie1, skip_space(Rest2));
                [$;|Rest1] -> parse_set_cookie_options(Rest1, Cookie0);
                [$,|Rest1] -> {Cookie0, Rest1};
                []         -> {Cookie0, []};
                _          -> error
            end;
        "secure" ->
            Cookie1 = Cookie0#setcookie{secure=true},
            parse_set_cookie_result(Cookie1, skip_space(Rest0));
        "httponly" ->
            Cookie1 = Cookie0#setcookie{http_only=true},
            parse_set_cookie_result(Cookie1, skip_space(Rest0));
        K ->
            Exts = Cookie0#setcookie.extensions,
            case skip_space(Rest0) of
                [$=|Rest1] ->
                    {V, Q, Rest2} = parse_cookie_value(skip_space(Rest1)),
                    Cookie1 = Cookie0#setcookie{extensions=[{K,V,Q}|Exts]},
                    parse_set_cookie_result(Cookie1, skip_space(Rest2));
                [$;|Rest1] ->
                    Cookie1 = Cookie0#setcookie{
                                extensions=[{K,undefined,false}|Exts]
                               },
                    parse_set_cookie_options(Rest1, Cookie1);
                [$,|Rest1] ->
                    Cookie1 = Cookie0#setcookie{
                                extensions=[{K,undefined,false}|Exts]
                               },
                    {Cookie1, Rest1};
                [] ->
                    Cookie1 = Cookie0#setcookie{
                                extensions=[{K,undefined,false}|Exts]
                               },
                    {Cookie1, []};
                _ ->
                    error
            end
    end.


parse_set_cookie_domain([C|_]=Rest, []) when C < $A orelse C > $Z orelse
                                             C < $a orelse C > $z orelse
                                             C /= $. ->
    parse_cookie_value(Rest);
parse_set_cookie_domain([C|_]=Rest, [_|_]=Acc) when C < $0 orelse C > $9 orelse
                                                    C < $A orelse C > $Z orelse
                                                    C < $a orelse C > $z orelse
                                                    C /= $. orelse C /= $- ->
    {lists:reverse(Acc), false, Rest};
parse_set_cookie_domain([], Acc) ->
    {lists:reverse(Acc), false, []};
parse_set_cookie_domain([C|T], Acc) ->
    parse_set_cookie_domain(T, [C|Acc]).


parse_set_cookie_maxage([C|_]=Rest, []) when C < $1 orelse C > $9 ->
    parse_cookie_value(Rest);
parse_set_cookie_maxage([C|_]=Rest, [_|_]=Acc) when C < $0 orelse C > $9 ->
    {lists:reverse(Acc), false, Rest};
parse_set_cookie_maxage([], Acc) ->
    {lists:reverse(Acc), false, []};
parse_set_cookie_maxage([C|T], Acc) ->
    parse_set_cookie_maxage(T, [C|Acc]).


%% First of all, try to parse valid rfc1123 date (faster), then use a regex
%% (more permissive)
parse_set_cookie_expires([D,A,Y,$,,$\s,D1,D2,SEP,M,O,N,SEP,Y1,Y2,Y3,Y4,$\s,
                          H1,H2,$:,M1,M2,$:,S1,S2,$\s,Z1,Z2,Z3|Rest])
  when SEP =:= $- orelse SEP =:= $\s ->
    {[D,A,Y,$,,$\s,D1,D2,SEP,M,O,N,SEP,Y1,Y2,Y3,Y4,$\s,
      H1,H2,$:,M1,M2,$:,S1,S2,$\s,Z1,Z2,Z3], false, Rest};
parse_set_cookie_expires(Str) ->
    RE = "^("
        "(?:[a-zA-Z]+,\s+)?"                    %% Week day
        "[0-9]+(?:\s|-)[a-zA-Z]+(?:\s|-)[0-9]+" %% DD Month YYYY
        "\s+[0-9]+:[0-9]+:[0-9]+"               %% hh:mm:ss
        "(?:\s+[a-zA-Z]+)?"                     %% timezone
        ")"
        "(.*)$",
    case re:run(Str, RE, [{capture, all_but_first, list}, caseless]) of
        {match, [Date, Rest]} -> {Date, false, Rest};
        nomatch               -> parse_cookie_value(Str)
    end.


parse_set_cookie_result(Cookie, [$;|Rest]) ->
    parse_set_cookie_options(Rest, Cookie);
parse_set_cookie_result(Cookie, [$,|Rest]) ->
    {Cookie, Rest};
parse_set_cookie_result(Cookie, []) ->
    {Cookie, []};
parse_set_cookie_result(_, _) ->
    error.


%%
parse_cookie(Str) ->
    parse_cookie(Str, []).

parse_cookie([], Cookies) ->
    lists:reverse(Cookies);
parse_cookie(Str, Cookies) ->
    {Key, Rest0} = parse_cookie_key(skip_space(Str), []),
    case yaws:to_lower(Key) of
        [] ->
            [];
        K ->
            case skip_space(Rest0) of
                [$=|Rest1] ->
                    {V, Q, Rest2} = parse_cookie_value(skip_space(Rest1)),
                    C = #cookie{key=K, value=V, quoted=Q},
                    case skip_space(Rest2) of
                        [$;|Rest3] -> parse_cookie(Rest3, [C|Cookies]);
                        [$,|Rest3] -> parse_cookie(Rest3, [C|Cookies]);
                        []         -> parse_cookie([], [C|Cookies]);
                        _          -> []
                    end;
                [$;|Rest1] -> parse_cookie(Rest1, [#cookie{key=K}|Cookies]);
                [$,|Rest1] -> parse_cookie(Rest1, [#cookie{key=K}|Cookies]);
                []         -> parse_cookie([], [#cookie{key=K}|Cookies]);
                _          -> []
            end
    end.


%%
%% All CHAR except ('=' | ';' | ',' | SP | HT | CRLF | LF)
parse_cookie_key([], Acc) ->
    {lists:reverse(Acc), []};
parse_cookie_key(T=[$=|_], Acc) ->
    {lists:reverse(Acc), T};
parse_cookie_key(T=[$;|_], Acc) ->
    {lists:reverse(Acc), T};
parse_cookie_key(T=[$,|_], Acc) ->
    {lists:reverse(Acc), T};
parse_cookie_key(T=[$\s|_], Acc) ->
    {lists:reverse(Acc), T};
parse_cookie_key(T=[$\t|_], Acc) ->
    {lists:reverse(Acc), T};
parse_cookie_key(T=[$\r,$\n|_], Acc) ->
    {lists:reverse(Acc), T};
parse_cookie_key(T=[$\n|_], Acc) ->
    {lists:reverse(Acc), T};
parse_cookie_key([C|T], Acc) ->
    parse_cookie_key(T, [C|Acc]).


%%
parse_cookie_value([$"|T]) ->
    parse_cookie_quoted(T,[]);
parse_cookie_value(T) ->
    parse_cookie_value(T,[]).

%% All CHAR except (';' | ',' | SP | HT | CRLF | LF)
parse_cookie_value([],Acc) ->
    {lists:reverse(Acc), false, []};
parse_cookie_value(T=[$;|_], Acc) ->
    {lists:reverse(Acc), false, T};
parse_cookie_value(T=[$,|_], Acc) ->
    {lists:reverse(Acc), false, T};
parse_cookie_value(T=[$\s|_], Acc) ->
    {lists:reverse(Acc), false, T};
parse_cookie_value(T=[$\t|_], Acc) ->
    {lists:reverse(Acc), false, T};
parse_cookie_value(T=[$\r,$\n|_], Acc) ->
    {lists:reverse(Acc), false, T};
parse_cookie_value(T=[$\n|_], Acc) ->
    {lists:reverse(Acc), false, T};
parse_cookie_value([C|T], Acc) ->
    parse_cookie_value(T, [C|Acc]).


%% All CHAR except ('"' | CTLs) but including LWS and escape DQUOTEs
%%   CTL = any US-ASCII control character (octets 0 - 31) and DEL (127)
%%   LWS = [CRLF] 1*( SP | HT )
parse_cookie_quoted([], Acc) ->
    {lists:reverse(Acc), true, []};
parse_cookie_quoted([$"|T], Acc) ->
    {lists:reverse(Acc), true, T};
parse_cookie_quoted([$\\,C|T], Acc) ->
    parse_cookie_quoted(T,[C,$\\|Acc]);
parse_cookie_quoted([$\t|T], Acc) ->
    parse_cookie_quoted(T,[$\t|Acc]);
parse_cookie_quoted([$\r,$\n,$\s|T], Acc) ->
    parse_cookie_quoted(T,[$\s,$\n,$\r|Acc]);
parse_cookie_quoted([$\r,$\n,$\t|T], Acc) ->
    parse_cookie_quoted(T,[$\t,$\n,$\r|Acc]);
parse_cookie_quoted([C|T], Acc) when C > 31 andalso C < 127 ->
    parse_cookie_quoted(T,[C|Acc]);
parse_cookie_quoted(T, Acc) ->
    {lists:reverse(Acc), true, T}.


%%
format_set_cookie(C) when C#setcookie.value == undefined ->
    [C#setcookie.key|format_cookie_opts(C)];
format_set_cookie(C) when C#setcookie.quoted ->
    [C#setcookie.key,$=,$",C#setcookie.value,$"|format_cookie_opts(C)];
format_set_cookie(C) ->
    [C#setcookie.key,$=,C#setcookie.value|format_cookie_opts(C)].

%%
format_cookie([Cookie]) ->
    format_cookie(Cookie);
format_cookie([Cookie|Rest]) ->
    [format_cookie(Cookie),$;,$\s|format_cookie(Rest)];
format_cookie(#cookie{key=Key, value=undefined}) ->
    Key;
format_cookie(#cookie{key=Key, value=Value, quoted=true}) ->
    [Key,$=,$",Value,$"];
format_cookie(#cookie{key=Key, value=Value, quoted=false}) ->
    [Key,$=,Value].

%%
format_cookie_opts(C=#setcookie{}) ->
    [
     add_opt("Domain",   C#setcookie.domain,    false),
     add_opt("Max-Age",  C#setcookie.max_age,   false),
     add_opt("Expires",  C#setcookie.expires,   false),
     add_opt("Path",     C#setcookie.path,      false),
     add_opt("Secure",   C#setcookie.secure,    false),
     add_opt("HttpOnly", C#setcookie.http_only, false)
    ] ++ [add_opt(K,V,Q) || {K,V,Q} <- C#setcookie.extensions].


add_opt(_, undefined, _) -> [];
add_opt(_, false, _)     -> [];
add_opt(Key, true, _)    -> [$;,$\s,Key];
add_opt(Key, Opt, true)  -> [$;,$\s,Key,$=,$",Opt,$"];
add_opt(Key, Opt, false)  -> [$;,$\s,Key,$=,Opt].


%%
skip_space([])          -> [];
skip_space([$\s|T])     -> skip_space(T);
skip_space([$\t|T])     -> skip_space(T);
skip_space([$\r,$\n|T]) -> skip_space(T);
skip_space([$\n|T])     -> skip_space(T);
skip_space(T)           -> T.


%%
getvar(ARG,Key) when is_atom(Key) ->
    getvar(ARG, atom_to_list(Key));
getvar(ARG,Key) ->
    filter_parse(Key, yaws_api:parse_query(ARG), yaws_api:parse_post(ARG)).


queryvar(ARG,Key) when is_atom(Key) ->
    queryvar(ARG, atom_to_list(Key));
queryvar(ARG, Key) ->
    filter_parse(Key, yaws_api:parse_query(ARG), []).

postvar(ARG, Key) when is_atom(Key) ->
    postvar(ARG, atom_to_list(Key));
postvar(ARG, Key) ->
    filter_parse(Key, [], yaws_api:parse_post(ARG)).

filter_parse(Key, QueryParse, PostParse) ->
    Fun = fun({K,V}) -> (Key == K andalso V /= undefined) end,
    Values = lists:filter(Fun, QueryParse) ++ lists:filter(Fun, PostParse),
    case Values of
        [] -> undefined;
        [{_, V}] -> {ok,V};
        %% Multivalued case - return list of values
        _  -> list_to_tuple(lists:map(fun({_,V}) -> V end, Values))
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
    case CheckCertsChanged of
        true ->
            CertCheck = gen_server:call(yaws_server, check_certs, infinity),
            case lists:member(yes, CertCheck) of
                true ->
                    application:stop(ssl),
                    application:start(ssl);
                false ->
                    ok
            end;
        false ->
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
            ok = yaws_config:hard_setconf(GC, Groups2);
        _ ->
            {error, need_restart}
    end.

%% return {ok, GC, Groups}.
getconf() ->
    gen_server:call(yaws_server, getconf, infinity).

%% return listen port number for the given sconf, useful if yaws is used in
%% a test scenario where the configured port number is 0 (for requesting an
%% ephemeral port)
get_listen_port(SC) ->
    yaws_server:listen_port(SC).

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
