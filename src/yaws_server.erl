%%%----------------------------------------------------------------------
%%% File    : yaws_server.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 16 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_server).
-author('klacke@hyber.org').

-compile(export_all).
%%-export([Function/Arity, ...]).

-behaviour(gen_server).
-include("yaws.hrl").
-include_lib("kernel/include/file.hrl").

%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-import(filename, [join/1]).
-import(lists, [foreach/2, map/2, flatten/1, flatmap/2]).


-record(gs, {gconf,
	     group,         %% list of #sconf{} s
	     l,             %% listen socket
	     mnum = 0,      %% dyn compiled erl module  number
	     sessions = 0,  %% number of HTTP sessions
	     reqs = 0}).    %% number of HTTP requests


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, yaws_server}, yaws_server, [], []).

status() ->
    gen_server:call(?MODULE, status).

stats() -> 
    {S, Time} = status(),
    {GC, Srvs, _} = S,
    Diff = calendar:time_difference(Time, calendar:local_time()),
    G = fun(L) -> lists:reverse(lists:keysort(2, L)) end,
    
    R= flatmap(
      fun({Pid, SCS}) ->
	      map(
		fun(SC) ->
			
			E = SC#sconf.ets,
			L = ets:match(E, {{urlc_total, '$1'}, '$2'}),
			{SC#sconf.servername,  
			 flatten(yaws:fmt_ip(SC#sconf.listen)),
			 G(map(fun(P) -> list_to_tuple(P) end, L))}
		end, SCS)
      end, Srvs),
    {Diff, R}.

			      
get_app_args() ->
    AS=init:get_arguments(),
    Debug = case application:get_env(yaws, debug) of
		undefined ->
		    lists:member({yaws, ["debug"]}, AS);
		_ ->
		    true
	    end,
    Trace = case application:get_env(yaws, trace) of
		undefined ->
		    case {lists:member({yaws, ["trace", "http"]}, AS),
			  lists:member({yaws, ["trace", "traffic"]}, AS)} of
			{true, _} ->
			    {true, http};
			{_, true} ->
			    {true, traffic};
			_ ->
			    false
		    end;
		{ok, http} ->
		    {true, http};
		{ok, traffic} ->
		    {true, traffic};
		_ ->
		    false
	    end,
    Conf = case application:get_env(yaws, conf) of
		undefined ->
		   find_c(AS);
		{ok, File} ->
		    {file, File}
	    end,
    {Debug, Trace, Conf}.

find_c([{conf, [File]} |_]) ->
    {file, File};
find_c([_|T]) ->
    find_c(T);
find_c([]) ->
    false.




%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    put(start_time, calendar:local_time()),  %% for uptime
    {Debug, Trace, Conf} = get_app_args(),

    case yaws_config:load(Conf, Trace, Debug) of
	{ok, Gconf, Sconfs} ->
	    erase(logdir),
	    ?Debug("Conf = ~p~n", [?format_record(Gconf, gconf)]),
	    yaws_log:setdir(Gconf#gconf.logdir, Sconfs),
	    case Gconf#gconf.trace of
		{true, What} ->
		    yaws_log:open_trace(What);
		_ ->
		    ok
	    end,

	    init2(Gconf, Sconfs);
	{error, E} ->
	    case erase(logdir) of
		undefined ->
		    error_logger:format("Bad conf: ~p", [E]),
		    {stop, E};
		Dir ->
		    yaws_log:setdir(Dir),
		    yaws_log:sync_errlog("bad conf: ~s",[E]),
		    {stop, E}
	    end
    end.


init2(Gconf, Sconfs) ->
    lists:foreach(
      fun(D) ->
	      code:add_pathz(D)
      end, Gconf#gconf.ebin_dir),

    process_flag(trap_exit, true),

    %% start the individual server processes
    L = lists:map(
	  fun(Group) ->
		  proc_lib:start_link(?MODULE, gserv, [Gconf, Group])
	  end, Sconfs),
    L2 = lists:zf(fun({error, F, A}) ->
			  yaws_log:sync_errlog(F, A),
			  false;
		     ({Pid, SCs}) ->
			  true
		  end, L),
    if
	length(L) == length(L2) ->
	    {ok, {Gconf, L2, 0}};
	true ->
	    {stop, "failed to start server "}
    end.

		     

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(status, From, State) ->
    Reply = {State, get(start_time)},
    {reply, Reply, State};
handle_call(mnum, From, {GC, Group, Mnum}) ->
    {reply, Mnum+1,   {GC, Group, Mnum+1}}.




%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_info(Msg, State) ->
    ?Debug("GOT ~p~n", [Msg]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.


%% One server per IP we listen to
gserv(GC, Group0) ->
    ?TC([{record, GC, gconf}]),
    Group = map(fun(SC) -> 
			E = ets:new(yaws_code, [public, set]),
			ets:insert(E, {num_files, 0}),
			ets:insert(E, {num_bytes, 0}),
			SC#sconf{ets = E}
		end, Group0),
    SC = hd(Group),
    case gen_tcp:listen(SC#sconf.port, opts(SC)) of
	{ok, Listen} ->
	    error_logger:info_msg("Listening to ~s:~w for servers ~p~n",
			      [yaws:fmt_ip(SC#sconf.listen),
			       SC#sconf.port,
			       catch map(fun(S) ->  S#sconf.servername end, 
					 Group)]),
	    file:make_dir("/tmp/yaws"),
	    {ok, Files} = file:list_dir("/tmp/yaws"),
	    lists:foreach(
	      fun(F) -> file:delete("/tmp/yaws/" ++ F) end, Files),
	    proc_lib:init_ack({self(), Group}),
	    GS = #gs{gconf = GC,
		     group = Group,
		     l = Listen},
	    acceptor(GS),
	    gserv(GS, [], 0);
	Err ->
	    error_logger:format("Failed to listen ~s:~w  : ~p~n",
				[yaws:fmt_ip(SC#sconf.listen),
				 SC#sconf.port, Err]),
	    proc_lib:init_ack({error, "Can't listen to socket: ~p ",[Err]}),
	    exit(normal)
    end.
			

gserv(GS, Ready, Rnum) ->
    receive
	{From , status} ->
	    From ! {self(), GS},
	    gserv(GS, Ready, Rnum);
	{From, next} when Ready == [] ->
	    acceptor(GS),
	    gserv(GS, Ready, Rnum);
	{From, next} ->
	    [R|RS] = Ready,
	    R ! {self(), accept},
	    gserv(GS, RS, Rnum-1);
	{From, done_client, Int} ->
	    GS2 = GS#gs{sessions = GS#gs.sessions + 1,
			reqs = GS#gs.reqs + Int},
	    if
		Rnum == 8 ->
		    From ! {self(), stop},
		    gserv(GS2, Ready, Rnum);
		Rnum < 8 ->
		    gserv(GS2, [From | Ready], Rnum+1)
	    end
    end.
	    


opts(SC) ->
    [binary, 
     {ip, SC#sconf.listen},
     {packet, http},
     {reuseaddr, true},
     {active, false}
    ].

acceptor(GS) ->
    proc_lib:spawn_link(yaws_server, acceptor0, [GS, self()]).
acceptor0(GS, Top) ->
    ?TC([{record, GS, gs}]),
    L = GS#gs.l,
    X = gen_tcp:accept(L),
    ?Debug("Accept ret:~p L=~p~n", [X,L]),
	    
    Top ! {self(), next},
    case X of
	{ok, Client} ->
	    case (GS#gs.gconf)#gconf.trace of  %% traffic trace
		{true, _} ->
		    {ok, {IP, Port}} = inet:peername(Client),
		    Str = ?F("New connection from ~s:~w~n", [yaws:fmt_ip(IP),Port]),
		    yaws_log:trace_traffic(from_client, Str);
		_ ->
		    ok
	    end,

	    Res = (catch aloop(Client, GS,  0)),
	    gen_tcp:close(Client),
	    ?Debug("RES = ~p~n", [Res]),
	    case  (GS#gs.gconf)#gconf.debug of
		true  ->
		    io:format("DIE: ~p~n", [Res]);
		_ ->
		    ok
	    end,
	    case Res of
		{ok, Int} when integer(Int) ->
		    Top ! {self(), done_client, Int};
		{'EXIT', normal} ->
		    Top ! {self(), done_client, 0};
		{'EXIT', Reason} ->
		    error_logger:format("~p~n", [Reason]),
		    Top ! {self(), done_client, 0}
	    end,
	    %% we cache processes
	    receive
		{Top, stop} ->
		    exit(normal);
		{Top, accept} ->
		    acceptor0(GS, Top)
	    end;
	_ ->
	    exit(normal)
    end.
     


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

aloop(CliSock, GS, Num) ->
    ?TC([{record, GS, gs}]),
    case get_headers(CliSock, GS#gs.gconf) of
	done ->
	    {ok, Num};
	{Req, H} ->
	    Sconf = pick_sconf(H, GS#gs.group),
	    ?Debug("Sconf: ~p", [?format_record(Sconf, sconf)]),
	    ?TC([{record, Sconf, sconf}]),
	    ?Debug("Headers = ~p~n", [?format_record(H, headers)]),
	    Res = apply(yaws_server, Req#http_request.method, 
			[CliSock, GS#gs.gconf, Sconf, Req, H]),
	    maybe_access_log(CliSock, Sconf, Req),
	    case Res of
		continue ->
		    aloop(CliSock, GS, Num+1);
		done ->
		    {ok, Num+1}
	    end
    end.


pick_sconf(H, Group) ->
    case H#headers.host of
	undefined ->
	    pick_default(Group);
	Host ->
	    pick_host(Host, Group, Group)
    end.

pick_default([]) ->
    yaws_log:errlog("No default host found in server group ",[]),
    exit(normal);
pick_default([H|T]) when H#sconf.default_server_on_this_ip == true ->
    H;
pick_default([_|T]) ->
    pick_default(T).


pick_host(Host, [H|T], Group) when H#sconf.servername == Host ->
    H;
pick_host(Host, [_|T], Group) ->
    pick_host(Host, T, Group);
pick_host(Host, [], Group) ->
    pick_default(Group).


maybe_access_log(CliSock, SC, Req) ->
    ?TC([{record, SC, sconf}]),
    case SC#sconf.access_log of
	true ->
	    {ok, {Ip, Port}} = inet:peername(CliSock),
	    Status = case erase(status_code) of
			 undefined -> "-";
			 I -> integer_to_list(I)
		     end,
	    Len = case erase(content_length) of
		      undefined ->
			  "-";
		      I2 -> integer_to_list(I2)
		  end,
	    Path = get_path(Req#http_request.path),
	    Meth = atom_to_list(Req#http_request.method),
	    yaws_log:accesslog(SC#sconf.servername, Ip, 
			       [Meth, $\s, Path] , Status, Len);
	false ->
	    ignore
    end.


get_path({abs_path, Path}) ->
    Path.

cli_recv(S, Num, GC) when GC#gconf.trace == false ->
    gen_tcp:recv(S, Num, GC#gconf.timeout);
cli_recv(S, Num, GC) ->
    Res = gen_tcp:recv(S, Num, GC#gconf.timeout),
    case Res of
	{ok, Val} when tuple(Val) ->
	    yaws_log:trace_traffic(from_client, ?F("~p~n", [Val]));
	{error, What} ->
	    yaws_log:trace_traffic(from_client, ?F("~n~p~n", [What]));
	{ok, http_eoh} ->
	    ok;
	{ok, Val} when GC#gconf.trace == {true, traffic} ->
	    yaws_log:trace_traffic(from_client, Val)
    end,
    Res.



cli_write(S, Data, GC) when GC#gconf.trace /= {trace, traffic} ->
    gen_tcp:send(S, Data);
cli_write(S, Data, GC) ->
    yaws_log:trace_traffic(from_server, Data),
    gen_tcp:send(S, Data).


cli_write_headers(S, Data, GC) when GC#gconf.trace == false ->
    gen_tcp:send(S, Data);
cli_write_headers(S, Data, GC) ->
    yaws_log:trace_traffic(from_server, Data),
    gen_tcp:send(S, Data).



get_headers(CliSock, GC) ->
    ?TC([{record, GC, gconf}]),
    inet:setopts(CliSock, [{packet, http}]),
    case cli_recv(CliSock, 0, GC) of
	{ok, R} when element(1, R) == http_request ->
	    H = get_headers(CliSock, R, GC, #headers{}),
	    {R, H};
	{error, timeout} ->
	    done;
	{error, closed} ->
	    done;
	Err ->
	    ?Debug("Got ~p~n", [Err]),
	    exit(normal)
    end.

get_headers(CliSock, Req, GC, H) ->
    ?TC([{record, GC, gconf}]),
    case cli_recv(CliSock, 0, GC) of
	{ok, {http_header,  Num, 'Host', _, Host}} ->
	    get_headers(CliSock, Req, GC, H#headers{host = Host});
	{ok, {http_header, Num, 'Connection', _, Conn}} ->
	    get_headers(CliSock, Req, GC, H#headers{connection = Conn});
	{ok, {http_header, Num, 'Accept', _, Accept}} ->
	    get_headers(CliSock, Req, GC, H#headers{accept = Accept});
	{ok, {http_header, Num, 'If-Modified-Since', _, X}} ->
	    get_headers(CliSock, Req, GC, H#headers{if_modified_since = X});
	{ok, {http_header, Num, 'If-Match', _, X}} ->
	    get_headers(CliSock, Req, GC, H#headers{if_match = X});
	{ok, {http_header, Num, 'If-None-Match', _, X}} ->
	    get_headers(CliSock, Req, GC, H#headers{if_none_match = X});
	{ok, {http_header, Num, 'If-Range', _, X}} ->
	    get_headers(CliSock, Req, GC, H#headers{if_range = X});
	{ok, {http_header, Num, 'If-Unmodified-Since', _, X}} ->
	    get_headers(CliSock, Req, GC, 
			H#headers{if_unmodified_since = X});
	{ok, {http_header, Num, 'Range', _, X}} ->
	    get_headers(CliSock, Req, GC, H#headers{range = X});
	{ok, {http_header, Num, 'Referer',_, X}} ->
	    get_headers(CliSock, Req, GC, H#headers{referer = X});
	{ok, {http_header, Num, 'User-Agent', _, X}} ->
	    get_headers(CliSock, Req, GC, H#headers{user_agent = X});
	{ok, {http_header, Num, 'Accept-Ranges', _, X}} ->
	    get_headers(CliSock, Req, GC, H#headers{accept_ranges = X});
	{ok, {http_header, Num, 'Cookie', _, X}} ->
	    get_headers(CliSock, Req, GC, 
			H#headers{cookie = [X|H#headers.cookie]});
	{ok, {http_header, Num, 'Keep-Alive', _, X}} ->
	    get_headers(CliSock, Req, GC, H#headers{keep_alive = X});
	{ok, {http_header, Num, 'Content-Length', _, X}} ->
	    get_headers(CliSock, Req, GC, H#headers{content_length = X});
	{ok, http_eoh} ->
	    H;
	{ok, Other} ->
	    ?Debug("OTHER header ~p~n", [Other]),
	    get_headers(CliSock, Req, GC, H);
	Err ->
	    exit(normal)
    
    end.


%% ret:  continue | done
'GET'(CliSock, GC, SC, Req, Head) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    ?Debug("GET ~p", [Req#http_request.path]),
    ok = inet:setopts(CliSock, [{packet, raw}, binary]),
    flush(CliSock, Head#headers.content_length),
    ARG = make_arg(CliSock, Head, Req, GC, SC),
    handle_request(CliSock, GC, SC, Req, Head, ARG).


'POST'(CliSock, GC, SC, Req, Head) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    ?Debug("POST Req=~p H=~p~n", [?format_record(Req, http_request),
				  ?format_record(Head, headers)]),
    ok = inet:setopts(CliSock, [{packet, raw}, binary]),
    Bin = case Head#headers.content_length of
	undefined ->
	    case Head#headers.connection of
		"close" ->
		    get_client_data(CliSock, all, GC);
		_ ->
		    exit(normal)
	    end;
	Len ->
	    get_client_data(CliSock, list_to_integer(Len), GC)
    end,
    ?Debug("POST data = ~s~n", [binary_to_list(Bin)]),
    ARG = make_arg(CliSock, Head, Req, GC, SC),
    ARG2 = ARG#arg{clidata = Bin},
    handle_request(CliSock, GC, SC, Req, Head, ARG2).
    

make_arg(CliSock, Head, Req, GC, SC) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    #arg{clisock = CliSock,
	 headers = Head,
	 req = Req,
	 docroot = SC#sconf.docroot}.



-record(urltype, {type,   %% error | yaws | regular | directory | dotdot
		  finfo,
		  path,
		  fullpath,
		  data,    %% Binary | FileDescriptor | DirListing | undefined
		  mime,    %% MIME type
		  q,       %% query for GET requests
		  wwwauth = false  %% or #auth{}
		 }).


handle_request(CliSock, GC, SC, Req, H, ARG) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    UT =  url_type(GC, SC,P=get_path(Req#http_request.path)),
    case UT#urltype.type of
	error ->
	    deliver_404(CliSock, GC, SC, Req);
	directory ->
	    yaws_ls:list_directory(CliSock, UT#urltype.data, P, GC, SC);
	regular -> 
	    deliver_file(CliSock, GC, SC, Req, H, UT);
	yaws ->
	    do_yaws(CliSock, GC, SC, Req, H, 
		    ARG#arg{querydata = UT#urltype.q}, UT);
	dotdot ->
	    deliver_403(CliSock, Req, GC)
    end.

	


deliver_403(CliSock, Req, GC) ->
    H = make_date_and_server_headers(),
    B = list_to_binary("<html> <h1> 403 Forbidden, no .. paths "
		       "allowed  </h1></html>"),
    D = [make_400(403), H, make_connection_close(true),
	 make_content_length(size(B)), crnl()],
    send_headers_and_data(true, CliSock, D, B, GC),
    done.



deliver_404(CliSock, GC, SC,  Req) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    H = make_date_and_server_headers(),
    B = not_found_body(get_path(Req#http_request.path), GC, SC),
    D = [make_400(404), H, make_connection_close(true),
	 make_content_length(size(B)), crnl()],
    send_headers_and_data(true, CliSock, D, B, GC),
    done.


do_yaws(CliSock, GC, SC, Req, H, ARG, UT) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    FileAtom = list_to_atom(UT#urltype.fullpath),
    Mtime = mtime(UT#urltype.finfo),
    case ets:lookup(SC#sconf.ets, FileAtom) of
	[{FileAtom, Mtime1, Spec}] when Mtime1 == Mtime ->
	    deliver_dyn_file(CliSock, GC, SC, Req, H, Spec, ARG,UT);
	Other  ->
	    del_old_files(Other),
	    {ok, Spec} = yaws_compile:compile_file(UT#urltype.fullpath, GC, SC),
	    ?Debug("Spec for file ~s is:~n~p~n",[UT#urltype.fullpath, Spec]),
	    ets:insert(SC#sconf.ets, {FileAtom, Mtime, Spec}),
	    deliver_dyn_file(CliSock, GC, SC, Req, H, Spec, ARG, UT)
    end.


del_old_files([]) ->
    ok;
del_old_files([{FileAtom, Mtime1, Spec}]) ->
    lists:foreach(
      fun({mod, _, _, _,  Mod, Func}) ->
	      F="/tmp/yaws/" ++ yaws:to_list(Mod) ++ ".erl",
	      file:delete(F);
	 (_) ->
	      ok
      end, Spec).
		     

get_client_data(CliSock, all, GC) ->
    get_client_data(CliSock, all, cli_recv(CliSock, 4000, GC), GC);
get_client_data(CliSock, Len, GC) ->
    case cli_recv(CliSock, Len, GC) of
	{ok, B} when size(B) == Len ->
	    B;
	_ ->
	    exit(normal)
    end.

get_client_data(CliSock, all, {ok, B}, GC) ->
    B2 = get_client_data(CliSock, all, cli_recv(CliSock, 4000, GC)),
    <<B/binary, B2/binary>>;
get_client_data(CliSock, all, eof, GC) ->
    <<>>.


do_dyn_headers(CliSock, GC, SC, Bin, Fd, Req, Head, [H|T], ARG) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    {DoClose, Chunked} = case Req#http_request.version of
			     {1, 0} -> {true, []};
			     {1, 1} -> {false, make_chunked()}
			 end,
    case H of
	{mod, LineNo, YawsFile, SkipChars, Mod, some_headers} ->
	    MimeType = "text/html",
	    OutH = make_dyn_headers(DoClose, MimeType),
	    {_, Bin2} = skip_data(Bin, Fd, SkipChars),
	    case (catch apply(Mod, some_headers, [ARG])) of
		{ok, Out} ->
		    {[make_200(),OutH, Chunked, Out, 
		      crnl_if_not_chunked(DoClose)], 
		     [],T, DoClose, Bin2};
		ok ->
		    {[make_200(),OutH, Chunked, 
		      crnl_if_not_chunked(DoClose)], [], T, DoClose, Bin2};
		close ->
		    exit(normal);
		Err ->
		    {[make_200(), OutH, crnl()],
		     ?F("<p><br> <pre>yaws code ~w:~w(~p) crashed or ret bad val:"
			" ~n~p~n</pre><br>~n", 
			[Mod, some_headers, [Head], Err]),
		     T, true, Bin2}
	    end;
	{mod, LineNo, YawsFile, SkipChars, Mod, all_headers} ->
	    {_, Bin2} = skip_data(Bin, Fd, SkipChars),
	    case (catch apply(Mod, all_headers, [ARG])) of
		{ok, StatusCode, Out} when integer(StatusCode) ->
		    put(status_code, StatusCode),
		    {[Out, crnl_if_not_chunked(DoClose)], 
		     [], T, DoClose, Bin2};
		close ->
		    exit(normal);
		Err ->
		    MimeType = "text/html",
		    OutH = make_dyn_headers(DoClose, MimeType),
		    {[make_200(), OutH, crnl()],
		      ?F("<p> yaws code ~w:~w(~p) crashed or returned bad value:"
			 " ~n~p~n", 
			 [Mod, all_headers, [Head], Err]), T,  true, Bin2}
	    end;
	_ ->
	    MimeType = "text/html",
	    OutH = make_dyn_headers(DoClose, MimeType),
	    {[make_200(),OutH, Chunked, crnl_if_not_chunked(DoClose)], 
	     [], [H|T], DoClose, Bin}
    end.



crnl_if_not_chunked(false) ->
    [];
crnl_if_not_chunked(true) ->
    crnl().


	   

%% do the header and continue
deliver_dyn_file(CliSock, GC, SC, Req, Head, Specs, ARG, UT) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    Fd = ut_open(UT),
    Bin = ut_read(Fd),
    {S2, Content, Tail, DoClose, Bin2} = 
	do_dyn_headers(CliSock,GC, SC,Bin,Fd,  Req,Head,Specs,ARG),
    ?Debug("TAIL =~p~n", [Tail]),
    send_headers(CliSock, S2, GC),
    if
	Content /= [] ->
	    safe_send(true, CliSock, Content, GC);
	true ->
	    ok
    end,
    deliver_dyn_file(CliSock, GC, SC, Req, Head, UT, DoClose, Bin2, Fd, Tail, ARG).



deliver_dyn_file(CliSock, GC, SC, Req, Head, UT, DC, Bin, Fd, [H|T],ARG) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}, {record, UT,urltype}]),
    ?Debug("deliver_dyn_file: ~p~n", [H]),
    case H of
	{mod, LineNo, YawsFile, NumChars, Mod, out} ->
	    {_, Bin2} = skip_data(Bin, Fd, NumChars),
	    safe_call(DC, LineNo, YawsFile, CliSock, Mod, out, [ARG], GC),
	    deliver_dyn_file(CliSock, GC, SC, Req, Head, UT, DC,
			     Bin2,Fd,T,ARG);
	{data, 0} ->
	    deliver_dyn_file(CliSock, GC, SC, Req, Head, UT,DC, Bin, Fd,T,ARG);
	{data, NumChars} ->
	    {Send, Bin2} = skip_data(Bin, Fd, NumChars),
	    safe_send(DC, CliSock, Send, GC),
	    deliver_dyn_file(CliSock, GC, SC, Req, Bin2,
			     UT, DC, Bin2, Fd, T,ARG);
	{error, NumChars, Str} ->
	    {_, Bin2} = skip_data(Bin, Fd, NumChars),
	    safe_send(DC, CliSock, Str, GC),
	    deliver_dyn_file(CliSock, GC, SC, Req, Bin2,
			     UT, DC, Bin2, Fd, T,ARG)
    end;
deliver_dyn_file(CliSock, GC, SC, Req, Head, UT, DC, Bin, Fd, [],ARG) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    ?Debug("deliver_dyn: done~n", []),
    case DC of
	true ->
	    done;
	false ->
	    %gen_tcp:send(CliSock, crnl()),
	    tcp_send(CliSock, [crnl(), "0", crnl()], GC),
	    continue
    end.


%% what about trailers ??

skip_data(List, Fd, Sz) when list(List) ->
    skip_data(list_to_binary(List), Fd, Sz);
skip_data(Bin, Fd, Sz) when binary(Bin) ->
    ?Debug("Skip data ~p bytes ", [Sz]),
     case  Bin of
	 <<Head:Sz/binary ,Tail/binary>> ->
	     {Head, Tail};
	 _ ->
	     case file:read(Fd, 4000) of
		 {ok, Bin2} -> 
		     Bin3 = <<Bin/binary, Bin2/binary>>,
		     skip_data(Bin3, Fd, Sz);
		 Err ->
		     ?Debug("EXIT in skip_data: ~p  ~p~n", [Bin, Sz]),
		     exit(normal)
	     end
     end;
skip_data({bin, Bin}, _, Sz) ->
    <<Head:Sz/binary ,Tail/binary>> = Bin,
    {Head, {bin, Tail}}.



to_binary(B) when binary(B) ->
    B;
to_binary(L) when list(L) ->
    list_to_binary(L).


send_headers_and_data(DC, Sock, Headers, Data, GC) ->
    send_headers(Sock, Headers, GC),
    safe_send(DC, Sock, Data, GC).


send_headers(Sock, Data, GC) ->
    case cli_write_headers(Sock, Data, GC) of
	ok ->
	    ok;
	Err ->
	    yaws_debug:format(GC, 
			      "Failed to send ~p on socket: ~p~n",
			      [Data, Err]),
	    exit(normal)
    end.


safe_send(DoClose, Sock, Data, GC) ->
    if
	DoClose == true  ->
	    case tcp_send(Sock, Data, GC) of
		ok ->
		    ok;
		Err ->
		    yaws_debug:format(GC, 
				      "Failed to send ~p on socket: ~p~n",
				      [Data, Err]),
		    exit(normal)
	    end;
	DoClose == false ->
	    B = to_binary(Data),
	    Len = size(B),
	    Data2 = [crnl(), yaws:integer_to_hex(Len) , crnl(), B],
	    case tcp_send(Sock, Data2, GC) of
		ok ->
		    ok;

		Err ->
		    yaws_debug:format(GC, 
				      "Failed to send ~p on socket: ~p~n",
				      [Data, Err]),

		    exit(normal)
	    end
    end.

		    
safe_call(DoClose, LineNo, YawsFile, CliSock, M, F, A, GC) ->
    ?Debug("safe_call ~w:~w(~p)~n", 
	   [M, F, ?format_record(hd(A), arg)]),
    case (catch apply(M,F,A)) of
	{'EXIT', Err} ->
	    safe_send(DoClose, CliSock, 
		      ?F("~n<pre> ~nERROR erl ~w/1 code  crashed:~n "
			 "File: ~s:~w~n"
			 "Reason: ~p~n</pre>~n",
			 [F, YawsFile, LineNo, Err]), GC),
	    yaws:elog("erl code  ~w/1 crashed: ~n"
		      "File: ~s:~w~n"
		      "Reason: ~p", [F, YawsFile, LineNo, Err]);
	
	{ok, Out} ->
	    safe_send(DoClose, CliSock, Out, GC);
	Other ->
	    safe_send(DoClose, CliSock, 
		      ?F("~n<pre>~nERROR erl code ~w/1 returned bad value: ~n "
			 "File: ~s:~w~n"
			 "Value: ~p~n</pre>~n",
			 [F, YawsFile, LineNo, Other]), GC),
	    yaws:elog("erl code  ~w/1 returned bad value: ~n"
		      "File: ~s:~w~n"
		      "Value: ~p", [F, YawsFile, LineNo, Other]);

	close ->
	    gen_tcp:close(CliSock),
	    exit(normal)
    end.



ut_open(UT) ->
    case UT#urltype.data of
	undefined ->
	    {ok, Fd} = file:open(UT#urltype.fullpath, [read, raw]),
	    Fd;
	B when binary(B) ->
	    {bin, B}
    end.


ut_read(Bin = {bin, B}) ->
    Bin;
ut_read(Fd) ->
    file:read(Fd, 4000).


ut_close({bin, _}) ->
    ok;
ut_close(Fd) ->
    file:close(Fd).

	


deliver_file(CliSock, GC, SC, Req, InH, UT) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}, {record, UT, urltype}]),
    DoClose = do_close(Req, InH),
    OutH = make_static_headers(Req, UT, DoClose),
    Fd = ut_open(UT),
    Bin = ut_read(Fd),
    send_headers(CliSock, [make_200(), OutH, crnl()], GC),
    case Bin of
	{bin, Binary} ->
	    tcp_send(CliSock, Binary, GC);
	{ok, Binary} ->
	    send_loop(CliSock, Binary, Fd, GC)
    end,
    ut_close(Fd),
    if
	DoClose == true ->
	    done;
	DoClose == false ->
	    continue
    end.


do_close(Req, H) ->
    case Req#http_request.version of
	{1, 0} ->
	     case H#headers.keep_alive of
		 undefined ->
		     true;
		 _ ->
		     false
	     end;
	_ ->
	    false
    end.


send_loop(CliSock, Data, Fd, GC) ->
    case tcp_send(CliSock, Data, GC) of
	ok ->
	    case ut_read(Fd) of
		{ok, Data2} ->
		    send_loop(CliSock, Data2, Fd, GC);
		eof ->
		    ok
	    end;
	Err ->
	    Err
    end.
	     


make_dyn_headers(DoClose, MimeType) ->
    [make_date_header(),
     make_server_header(),
     make_content_type(MimeType),
     make_connection_close(DoClose),
     make_non_cache_able()
    ].


make_x_pad() ->
    "X-Pad: avoid browser bug\r\n".


make_non_cache_able() ->
    []. %% FIXME

make_date_and_server_headers() ->
    [make_date_header(),
     make_server_header()
    ].

make_static_headers(Req, UT, DoClose) ->    
    [make_date_and_server_headers(),
     make_last_modified(UT#urltype.finfo),
     make_etag(UT#urltype.finfo),
     make_accept_ranges(),
     make_content_length(UT#urltype.finfo),
     make_content_type(UT#urltype.mime),
     make_connection_close(DoClose)
    ].



make_200() ->
    put(status_code, 200),
    "HTTP/1.1 200 OK\r\n".

make_400(Code) ->
    put(status_code, Code),
    ["HTTP/1.1 ", integer_to_list(Code), " ",
     yaws_api:code_to_phrase(Code), crnl()].



crnl() ->
    "\r\n".



%% FIXME optimize the goddamn date generations
make_date_header() ->
    N = element(2, now()),
    case get(date_header) of
	{Str, Secs} when (Secs+10) > N ->
	    H = ["Date: ", yaws:universal_time_as_string(), crnl()],
	    put(date_header, {H, N}),
	    H;
	{Str, Secs} ->
	    Str;
	undefined ->
	    H = ["Date: ", yaws:universal_time_as_string(), crnl()],
	    put(date_header, {H, N}),
	    H
    end.
%% FIXME read vsn from conf at compile time
make_server_header() ->
    ["Server: Yaws/0.32 Yet Another Web Server", crnl()].
make_last_modified(_) ->
    [];
make_last_modified(FI) ->
    N = element(2, now()),
    Inode = FI#file_info.inode,  %% unix only
    case get({last_modified, Inode}) of
	{Str, Secs} when N < (Secs+10) ->
	    Str;
	_ ->
	    S = do_make_last_modified(FI),
	    put({last_modified, Inode}, {S, N}),
	    S
    end.

do_make_last_modified(FI) ->
    Then = FI#file_info.mtime,
    UTC = calendar:now_to_universal_time(Then),
    Local = calendar:universal_time_to_local_time(UTC),
    Str = yaws:date_and_time(Local, UTC),
    ["Last-Modified: ", yaws:date_and_time_to_string(Str), crnl()].
make_etag(File) ->
    [].
make_accept_ranges() ->
    "Accept-Ranges: bytes\r\n".
make_content_length(Size) when integer(Size) ->
    put(content_length, Size),
    ["Content-Length: ", integer_to_list(Size), crnl()];
make_content_length(FI) ->
    Size = FI#file_info.size,
    put(content_length, Size),
    ["Content-Length: ", integer_to_list(Size), crnl()].
make_content_type(MimeType) ->
    ["Content-Type: ", MimeType, crnl()].

make_connection_close(true) ->
    "Connection: close\r\n";
make_connection_close(false) ->
    [].
make_chunked() ->
    ["Transfer-Encoding: chunked",  crnl()].

now_secs() ->
    {M,S,_}=now(),
    (M*1000000)+S.


%% a file cache,
url_type(GC, SC, Path) ->
    ?TC([{record, GC, gconf}, {record, SC, sconf}]),
    E = SC#sconf.ets,
    update_total(E, Path),
    case ets:lookup(E, {url, Path}) of
	[] ->
	    UT = do_url_type(SC#sconf.docroot, Path),
	    ?TC([{record, UT, urltype}]),
	    cache_file(GC, SC, Path, UT);
	[{_, When, UT}] ->
	    N = now_secs(),
	    FI = UT#urltype.finfo,
	    if
		When + 30 > N ->
		    %% more than 30 secs old entry
		    ets:delete(E, {url, Path}),
		    ets:delete(E, {urlc, Path}),
		    ets:update_counter(E, num_files, -1),
		    ets:update_counter(E, num_bytes, -FI#file_info.size),
		    url_type(GC, SC, Path);
		true ->
		    ets:update_counter(E, {urlc, Path}, 1),
		    UT
	    end
    end.

update_total(E, Path) ->
    case (catch ets:update_counter(E, {urlc_total, Path}, 1)) of
	{'EXIT', _} ->
	    ets:insert(E, {{urlc_total, Path}, 1});
	_ ->
	    ok
    end.


cache_file(GC, SC, Path, UT) when UT#urltype.type == regular ;
				  UT#urltype.type == yaws ->
    E = SC#sconf.ets,
    [{num_files, N}] = ets:lookup(E, num_files),
    [{num_bytes, B}] = ets:lookup(E, num_bytes),
    FI = UT#urltype.finfo,
    if
	N + 1 > GC#gconf.max_num_cached_files ->
	    error_logger:info("Max NUM cached files reached for server "
			      "~p", [SC#sconf.servername]),
	    cleanup_cache(E, num),
	    cache_file(GC, SC, Path, UT);
	B + FI#file_info.size > GC#gconf.max_num_cached_bytes ->
	    error_logger:info("Max size cached bytes reached for server "
			      "~p", [SC#sconf.servername]),
	    cleanup_cache(E, size),
	    cache_file(GC, SC, Path, UT);
	true ->
	    if
		FI#file_info.size > GC#gconf.max_size_cached_file ->
		    UT;
		true ->
		    {ok, Bin} = file:read_file(UT#urltype.fullpath),
		    UT2 = UT#urltype{data = Bin},
		    ets:insert(E, {{url, Path}, now_secs(), UT2}),
		    ets:insert(E, {{urlc, Path}, 1}),
		    ets:update_counter(E, num_files, 1),
		    ets:update_counter(E, num_bytes, FI#file_info.size),
		    UT2
	    end
    end;
cache_file(GC, SC, Path, UT) ->
    UT.


cleanup_cache(E, size) ->
    %% remove the largest files with the least hit count  (urlc)
    uhhh;
cleanup_cache(E, num) ->
    %% remove all files with a low hit count
    uhhh.


%% return #urltype{}
do_url_type(Droot, Path) ->
    case split_path(Droot, Path, [], []) of
	slash ->
	    case file:read_file_info([Droot, Path, "/index.yaws"]) of
		{ok, FI} ->
		    #urltype{type = yaws,
			     finfo = FI,
			     mime = "text/html",
			     fullpath = ?f([Droot, Path,"/index.yaws"])};
		_ ->
		    case file:read_file_info([Droot, Path, "/index.html"]) of
			{ok, FI} ->
			    #urltype{type = regular,
				     finfo = FI,
				     mime = "text/html",
				     fullpath =?f([Droot,Path,"/index.html"])};
			_ ->
			    case file:list_dir([Droot, Path]) of
				{ok, List} ->
				    #urltype{type = directory, data=List};
				Err ->
				    #urltype{type=error}
			    end
		    end
	    end;
	dotdot ->  %% generate 403 forbidden
	    #urltype{type=dotdot};
	Other ->
	    Other
    end.


split_path(DR, [$/], Comps, []) ->
    %% its a URL that ends with /
    slash;
split_path(DR, [$/, $/ |Tail], Comps, Part) ->  %% security clause
    split_path(DR, [$/|Tail], Comps, Part);
split_path(DR, [$/, $., $., $/ |Tail], _, [H|T]) ->  %% security clause
    split_path(DR, Tail, [], T);
split_path(DR, [$/, $., $., $/ |Tail], _, []) -> %% security clause
    dotdot;
split_path(DR, [], Comps, Part) ->
    ret_reg_split(DR, Comps, Part, []);
split_path(DR, [$?|Tail], Comps, Part) ->
    ret_reg_split(DR, Comps, Part, Tail);
split_path(DR, [$/|Tail], Comps, Part)  when Part /= [] ->
    ?Debug("Tail=~s Part=~s", [Tail,Part]),
    split_path(DR, [$/|Tail], [lists:reverse(Part) | Comps], []);
split_path(DR, [$#|Tail], Comps, Part) ->  %% fragment
    ret_reg_split(DR, Comps, Part, []);
split_path(DR, [$~|Tail], Comps, Part) ->  %% fragment
    ret_user_dir(DR, Comps, Part, Tail);
split_path(DR, [H|T], Comps, Part) ->
    split_path(DR, T, Comps, [H|Part]).

 %% http:/a.b.c/~user URLs
ret_user_dir(DR, [], "/", User) ->
    ?Debug("User = ~p~n", [User]),
    %% FIXME doesn't work if passwd contains ::
    Home = lists:nth(6, string:tokens(os:cmd(["grep ", User, " /etc/passwd "]),[$:])),
    L = [Home, "/public_html/index.html"],
    case file:read_file_info(L) of
	{ok, FI} -> 
	    #urltype{mime = "text/html",
		     finfo = FI,
		     type = regular,
		     fullpath = lists:flatten(L)};
	Err ->
	    #urltype{type=error, data=Err}
    end.


ret_reg_split(DR, Comms, File, Query) ->
    ?Debug("ret_reg_split(~p)", [[DR, Comms, File]]),
    L = [DR, lists:reverse(Comms), lists:reverse(File)],
    case file:read_file_info(L) of
	{ok, FI} when FI#file_info.type == regular ->
	    {X, Mime} = suffix_type(File),
	    #urltype{type=X, finfo=FI,
		     fullpath = lists:flatten(L),
		     mime=Mime, q=Query};
	{ok, FI} when FI#file_info.type == directory ->
	    case file:list_dir(L) of
		{ok, List} ->
		    #urltype{type=directory, data=List};
		Err ->
		    #urltype{type=error, data=Err}
	    end;
	Err ->
	    #urltype{type=error, data=Err}
    end.

%% FIXME add all mime types here
suffix_type("sway." ++ _) ->
    {yaws, "text/html"};
suffix_type("lmth." ++ _) ->
    {regular, "text/html"};
suffix_type("gpj." ++ _) ->
    {regular, "image/jpeg"};
suffix_type("fig." ++ _) ->
    {regular, "image/gif"};
suffix_type("zg." ++ _) ->
    {regular, "application/x-gzip"};
suffix_type(_) ->
    {regular, "application/octet-stream"}.


flush(Sock, undefined) ->
    ok;
flush(Sock, 0) ->
    ok;
flush(Sock, Sz) ->
    gen_tcp:recv(Sock, Sz, 1000).

			

tcp_send(S, Data, GC) ->
    ok = cli_write(S, Data, GC).



mtime(F) ->
    F#file_info.mtime.



not_found_body(Url, GC, SC) ->
    ?Debug("Sconf: ~p", [?format_record(SC, sconf)]),
    L = ["<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">"
	 "<HTML><HEAD>"
	 "<TITLE>404 Not Found</TITLE>"
	 "</HEAD><BODY>"
	 "<H1>Not Found</H1>"
	 "The requested URL ", 
	 Url, 
	 " was not found on this server.<P>"
	 "<HR>",
	 yaws:address(GC, SC),
	 "  </BODY></HTML>"
	],
    list_to_binary(L).


