-module(yaws_cgi).
-author('carsten@codimi.de').

-include_lib("yaws/include/yaws_api.hrl").
-include("yaws_debug.hrl").

-export([call_cgi/5, call_cgi/4, call_cgi/3, call_cgi/2]).

-export([cgi_worker/6]).


% TO DO:  Handle failure and timeouts.

% call_cgi calls the script `Scriptfilename' (full path).
% If `Exefilename' is given, it is the executable to handle this, 
% otherwise `Scriptfilame' is assumed to be executable itself.
% 
% Corresponding to a URI of
%    `http://somehost/some/dir/script.cgi/path/info', 
% `Pathinfo' should be set to `/path/info'.

% These functions can be used from a `.yaws' file.
% Note however, that they usually generate stream content.

call_cgi(Arg, Scriptfilename) ->				     
    call_cgi(Arg, undefined, Scriptfilename, undefined, []).

call_cgi(Arg, Exefilename, Scriptfilename) ->
    call_cgi(Arg, Exefilename, Scriptfilename, undefined, []).

call_cgi(Arg, Exefilename, Scriptfilename, Pathinfo) ->
    call_cgi(Arg, Exefilename, Scriptfilename, Pathinfo, []).

call_cgi(Arg, Exefilename, Scriptfilename, Pathinfo, ExtraEnv) ->
    case Arg#arg.state of
	{cgistate, Worker} ->
	    case Arg#arg.cont of
		cgicont -> 
		    handle_clidata(Arg, Worker);
		undefined ->
		    ?Debug("Error while reading clidata: ~p~n", 
			[Arg#arg.clidata]),
						% Error, what to do?
		    exit(normal)
	    end;
	_ ->
	    Worker = start_worker(Arg, Exefilename, Scriptfilename, 
				  Pathinfo, ExtraEnv),
	    handle_clidata(Arg, Worker)
    end.

handle_clidata(Arg, Worker) ->
    case Arg#arg.clidata of
	undefined ->
	    end_of_clidata(Arg, Worker);
	{partial, Data} ->
	    send_clidata(Worker, Data),
	    {get_more, cgicont, {cgistate, Worker}};
	Data when binary(Data) ->
	    send_clidata(Worker, Data),
	    end_of_clidata(Arg, Worker)
    end.

end_of_clidata(Arg, Worker) ->
    Worker ! {self(), end_of_clidata},
    get_from_worker(Arg, Worker).

send_clidata(Worker, Data) ->
    Worker ! {self(), clidata, Data},
    receive
	{Worker, clidata_receipt} -> ok
    end.


start_worker(Arg, Exefilename, Scriptfilename, Pathinfo, ExtraEnv) ->
    ExeFN = case Exefilename of 
		undefined -> Scriptfilename;
		"" -> Scriptfilename;
		FN -> FN
	    end,
    PI = case Pathinfo of
	     undefined -> Arg#arg.pathinfo;
	     OK -> OK
	 end,
    Worker = spawn(?MODULE, cgi_worker, 
		   [self(), Arg, ExeFN, Scriptfilename, PI, ExtraEnv]),
    Worker.


get_from_worker(Arg, Worker) ->
    {Headers, Data} = get_resp(Worker),
    AllResps = lists:map(fun(X)->do_header(Arg, X, Data) end, Headers),
    {ContentResps, Others} = filter2(fun iscontent/1, AllResps),
    {RedirResps, OtherResps} = filter2(fun isredirect/1, Others), 
    case RedirResps of
	[R|_] ->
	    Worker ! {self(), no_data},
	    OtherResps ++ [R];
	[] ->
	    case ContentResps of
		[C={streamcontent, _, _}|_] ->
		    Worker ! {self(), stream_data},
		    OtherResps++[C];
		[C={content, _, _}|_] ->
		    Worker ! {self(), no_data},
		    OtherResps++[C];
		[] ->
		    Worker ! {self(), no_data},
		    OtherResps
	    end
    end.


filter2(Pred, Xs) ->
    filter2(Pred, Xs, [], []).

filter2(_Pred, [], Ts, Fs) ->
    {lists:reverse(Ts), lists:reverse(Fs)};
filter2(Pred, [X|Xs], Ts, Fs) ->
    case Pred(X) of
	true ->
	    filter2(Pred, Xs, [X|Ts], Fs);
	false ->
	    filter2(Pred, Xs, Ts, [X|Fs])
    end.


iscontent({content, _, _}) ->
    true;
iscontent({streamcontent, _, _}) ->
    true;
iscontent(_) ->
    false.

isredirect({redirect_local, _}) ->
    true;
isredirect({redirect, _}) ->
    true;
isredirect(_) ->
    false.

checkdef(undefined) ->
    "";
checkdef(L) ->
    L.



deep_drop_prefix([], L) ->
    L;
deep_drop_prefix([X|Xs], [X|Ys]) when integer(X) ->
    deep_drop_prefix(Xs, Ys);
deep_drop_prefix([X|Xs], Ys) when list(X) ->
    deep_drop_prefix(X++Xs, Ys);
deep_drop_prefix(Xs, [Y|Ys]) when list(Y) ->
    deep_drop_prefix(Xs, Y++Ys);
deep_drop_prefix(_, _) ->
    false.


get_socket_peername(Socket={sslsocket,_,_}) ->
    {ok, {IP, _Port}}=ssl:peername(Socket),
    yaws:fmt_ip(IP);
get_socket_peername(Socket) ->
    {ok, {IP, _Port}}=inet:peername(Socket),
    yaws:fmt_ip(IP).


cgi_env(Arg, Scriptfilename, Pathinfo, ExtraEnv) ->
    H = Arg#arg.headers,
    R = Arg#arg.req,
    case R#http_request.path of
	{abs_path, RequestURI} -> ok;
	_ -> RequestURI = undefined
    end,
    {Maj,Min} = R#http_request.version,
    {Hostname, Hosttail}=lists:splitwith(fun(X)->X /= $: end, 
					 checkdef(H#headers.host)),
    Hostport = case Hosttail of

		   [$: | P] -> P;
		   [] -> "80"
	       end,
    PeerAddr = get_socket_peername(Arg#arg.clisock),
    Scriptname = deep_drop_prefix(Arg#arg.docroot, Arg#arg.fullpath),
    ExtraEnv ++
	lists:filter(
	  fun(X)->case X of {_,L} when list(L)->true;_->false end 
	  end,
	  ([
	    {"SERVER_SOFTWARE", "Yaws/"++yaws_vsn:version()},
	    {"SERVER_NAME", Hostname},
	    {"GATEWAY_INTERFACE", "CGI/1.1"},
	    {"SERVER_PROTOCOL", 
	     lists:flatten(
	       ["HTTP/",integer_to_list(Maj),".",integer_to_list(Min)])},
	    {"SERVER_PORT", Hostport},
	    {"REQUEST_METHOD", yaws:to_list(R#http_request.method)},
	    {"REQUEST_URI", RequestURI},
	    {"PATH_INFO", checkdef(Pathinfo)},
						% {"SCRIPT_FILENAME", ??? }
	    {"PATH_TRANSLATED", Scriptfilename},    % This seems not to
						% correspond to the
						% documentation I have
						% read, but it works
						% with PHP.
	    {"SCRIPT_NAME", Scriptname},
						%{"REMOTE_HOST", ""},  We SHOULD send this
	    {"REMOTE_ADDR", PeerAddr},
						%{"AUTH_TYPE", ""},    Fixme:  We MUST send this, but I don't
						% understand it.
						%{"REMOTE_USER", ""},
	    {"QUERY_STRING", checkdef(Arg#arg.querydata)},
	    {"CONTENT_TYPE", H#headers.content_type},
	    {"CONTENT_LENGTH", H#headers.content_length},
	    {"HTTP_ACCEPT", H#headers.accept},
	    {"HTTP_USER_AGENT", H#headers.user_agent},
	    {"HTTP_COOKIE", flatten_val(make_cookie_val(H#headers.cookie))}
	   ]++lists:map(fun({http_header,_,Var,_,Val})->{tohttp(Var),Val} end,
			H#headers.other)
	  )).

tohttp(X) ->
    "HTTP_"++lists:map(fun tohttp_c/1, yaws:to_list(X)).

tohttp_c($-) ->
    $_;
tohttp_c(C) when C >= $a , C =< $z ->
    C - $a + $A;
tohttp_c(C) ->
    C.


make_cookie_val([]) ->
    undefined;
make_cookie_val([C]) ->
    C;
make_cookie_val([C|CS]) ->
    [make_cookie_val(CS), $; | C].


%% Seems not to be necessary, but open_port documentation says that
%% value has to be a string.

flatten_val(L) when list(L) ->
    lists:flatten(L);
flatten_val(X) ->
    X.


notslash($/) ->
    false;
notslash(_) ->
    true.

pathof(F) ->
    case lists:dropwhile(fun notslash/1, lists:reverse(F)) of
	"/" ->
	    "/";
	[$/ | Tail] -> lists:reverse(Tail)
    end.



% We almost always generate stream content.
% Actually, we could do away with `content' altogether.

do_header(_Arg, "Content-type: "++CT, {partial_data, Data}) ->
    {streamcontent, CT, Data};
do_header(_Arg, "Content-type: "++CT, {all_data, Data}) ->
    {content, CT, Data};
do_header(_Arg, "Content-Type: "++CT, {partial_data, Data}) ->
    {streamcontent, CT, Data};
do_header(_Arg, "Content-Type: "++CT, {all_data, Data}) ->
    {content, CT, Data};
do_header(_Arg, "Location: "++Loc, _) ->
    {redirect_local, {any_path, Loc}};
do_header(_Arg, "Status: "++[N1,N2,N3|_], _) ->
    {status, list_to_integer([N1,N2,N3])};
do_header(_Arg, Line, _) ->
    {header, Line}.    % Is this correct?



get_resp(Worker) ->
    get_resp([], Worker).

get_resp(Hs, Worker) ->
    receive
	{Worker, header, H} ->
 	    ?Debug("~p~n", [{Worker, header, H}]),
	    get_resp([H|Hs], Worker);
	{Worker, all_data, Data} ->
 	    ?Debug("~p~n", [{Worker, all_data, Data}]),
	    {Hs, {all_data, Data}};
	{Worker, partial_data, Data} ->
 	    ?Debug("~p~n", [{Worker, partial_data, binary_to_list(Data)}]),
	    {Hs, {partial_data, Data}};
	{Worker, failure, _} ->
	    {[], undef};
        _Other ->
 	    ?Debug("~p~n", [_Other]),
	    get_resp(Hs, Worker)
    end.


cgi_worker(Parent, Arg, Exefilename, Scriptfilename, Pathinfo, ExtraEnv) ->
    Env = cgi_env(Arg, Scriptfilename, Pathinfo, ExtraEnv),
    ?Debug("~p~n", [Env]),
    CGIPort = open_port({spawn, Exefilename},
			[{env, Env}, 
			 {cd, pathof(Scriptfilename)},
			 exit_status,
			 binary]),
    pass_through_clidata(Parent, CGIPort),
    do_work(Parent, Arg, CGIPort).



pass_through_clidata(Parent, CGIPort) ->
    receive
	{Parent, clidata, Clidata} ->
	    ?Debug("Got clidata ~p~n", [binary_to_list(Clidata)]),
	    Parent ! {self(), clidata_receipt},
	    CGIPort ! {self(), {command, Clidata}},
	    pass_through_clidata(Parent, CGIPort);
	{Parent, end_of_clidata} ->
	    ?Debug("End of clidata~n", []),
	    ok
    end.

    
do_work(Parent, Arg, Port) ->
    header_loop(Parent, Arg, {start, Port}).

header_loop(Parent, Arg, S) ->
    case get_line(S) of
	{failure, F} ->
	    Parent ! {self(), failure, F};
	{[], T} ->
	    case T of
		{middle, Data, Port} ->
		    Parent ! {self(), partial_data, Data},
		    receive
			{Parent, stream_data} ->
			    data_loop(Arg#arg.pid, Port);
			{Parent, no_data} ->
			    ok
		    end;
		{ending, Data, _} ->
		    Parent ! {self(), all_data, Data},
		    receive
			{Parent, stream_data} ->
			    yaws_api:stream_chunk_end(Arg#arg.pid);
			{Parent, no_data} ->
			    ok
		    end
	    end;
	{H, T} ->
	    Parent ! {self(), header, H},
	    header_loop(Parent, Arg, T)
    end.


data_loop(Pid, Port) ->
    receive
	{Port, {data,Data}} ->
 	    ?Debug("~p~n", [{data, binary_to_list(Data)}]),
	    yaws_api:stream_chunk_deliver_blocking(Pid, Data),
	    data_loop(Pid, Port);
	{Port, {exit_status, _Status}} ->
 	    ?Debug("~p~n", [{exit_status, _Status}]),	    
	    yaws_api:stream_chunk_end(Pid);
        _Other ->
 	    ?Debug("~p~n", [_Other]),
	    data_loop(Pid, Port)
    end.
    


get_line({start, Port}) ->
    receive
	{Port, {data,Data}} ->
	    get_line([], {middle, Data, Port});
	{Port, {exit_status, 0}} ->
 	    ?Debug("~p~n", [{exit_status, 0}]),
	    get_line([], {ending, <<>>, Port});
	{Port, {exit_status, Status}} when Status /=0 ->
 	    ?Debug("~p~n", [{exit_status, Status}]),
	    {failure, {exit_status, Status}};
        _Other ->
 	    ?Debug("~p~n", [_Other]),	    
	    get_line({start, Port})
    end;
get_line(State) ->
    get_line([], State).


get_line(Acc, {S, <<10, Tail/binary>>, Port}) ->
    {lists:reverse(Acc), {S, Tail, Port}};
get_line(Acc, {S, <<13, 10, Tail/binary>>, Port}) ->
    {lists:reverse(Acc), {S, Tail, Port}};
get_line(Acc, {middle, <<>>, Port}) ->
    get_line(Acc, add_cgi_resp(<<>>, Port));
get_line(Acc, {middle, <<13>>, Port}) ->          % We SHOULD test for CRLF.
    get_line(Acc, add_cgi_resp(<<13>>, Port));    % Would be easier without.
get_line(Acc, {ending, <<>>, Port}) ->
    {lists:reverse(Acc), {ending, <<>>, Port}};
get_line(Acc, {S, <<C, Tail/binary>>, Port}) ->
    get_line([C|Acc], {S, Tail, Port}).


add_cgi_resp(Bin, Port) ->
    receive
	{Port, {data,Data}} ->
	    {middle, <<Bin/binary, Data/binary>>, Port};
	{Port, {exit_status, _Status}} ->
 	    ?Debug("~p~n", [{exit_status, _Status}]),	    
	    {ending, Bin, Port};
        _Other ->
 	    ?Debug("~p~n", [_Other]),
	    add_cgi_resp(Bin, Port)
    end.



