%%%----------------------------------------------------------------------
%%% File    : webmail.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 24 Jun 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(webmail).
-author('klacke@hyber.org').
-include("webmail.hrl").
-include("../../include/yaws_api.hrl").

-behaviour(gen_server).

-compile(export_all).
%%-export([Function/Arity, ...]).

%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {pophost = {127,0,0,1},
		popport = 110,
		smpthost = {127,0,0,1},
		smtpport = 25,
		num = 0,
		sessions = []}).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    gen_server:start({local, webmail}, webmail, [], []).
start_link() ->
    gen_server:start_link({local, webmail}, webmail, [], []).
restart() ->  %% debug func
    stop(),
    c:l(?MODULE),
    start().
stop() ->
    gen_server:call(?MODULE, stop).

state() ->
    gen_server:call(?MODULE, state).

is_connected(User) ->
    gen_server:call(?MODULE, {is_connected, User}).

newsession(User, Password) ->
    S = state(),
    case is_connected(User) of
	{true, Cookie} ->
	    {ok, Cookie};
	false ->
	    case pop3lib_cli:connect([{user, User},
				      {passwd, Password},
				      {addr, S#state.pophost},
				      {port, S#state.popport}]) of
		{ok, Session} ->
		    gen_server:call(?MODULE, {newsession, User, Session});
		Err ->
		    Err
	    end
    end.
			 
login_status(A) -> 
    H=A#arg.headers,
    C = H#headers.cookie,
    case yaws_api:find_cookie_val("ymail", C) of
	[] ->
	    {"<p> No user or not logged in", nouser, nouser};
	C2 ->
	    User = cookie_to_user(C2),
	    case is_connected(User) of
		{true, _} ->
		    {io_lib:format("<p> User ~s is logged in", [User]), ok, User};
		false ->
		    {io_lib:format("<p> User ~s is not logged in", [User]),
		     nok, User}
	    end
    end.

cookie_to_user(C) ->
    hd(string:tokens(C, "--")).

get_all_mails(User) ->
    gen_server:call(?MODULE, {get_all_mails, User}).
get_mail(User, Num) ->
    gen_server:call(?MODULE, {get_mail, User, Num}).
logout(User) ->
    gen_server:call(?MODULE, {logout, User}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    file:make_dir("/tmp/webmail"),
    os:cmd("rm -f /tmp/webmail/*"),
    {X, Y, Z} = now(),
    random:seed(X, Y, Z),
    {ok, #state{}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call(state, From, State) ->
    {reply, State, State};

handle_call({is_connected, User}, From, State) ->
    R = case lists:keysearch(User, 2, State#state.sessions) of
	{value, S} ->
		tick(S),
		{true, S#s.cookie};
	    false ->
		false
	end,
    {reply, R, State};


handle_call({newsession, User, PopSession}, From, State) ->
    N = random:uniform(16#ffffffff),
    Cookie = User ++ [$-, $- | integer_to_list(N)],
    S = #s{user = User,
	   cookie = Cookie,
	   pops = PopSession,
	   logintime = calendar:local_time(),
	   idletimer = spawn_link(?MODULE, idle_timer, [self()])},
    S2 = State#state{sessions = [S | State#state.sessions]},
    {reply, {ok, Cookie, User}, S2};

handle_call({get_all_mails, User}, From, State) ->
    R = case lists:keysearch(User, 2, State#state.sessions) of
	    {value, S} ->
		tick(S),
		case (catch get_mails(S#s.pops)) of
		    {'EXIT', Reason} ->
			{error, Reason};
		    List ->
			{ok, List}
		end;
	    false ->
		{error, "Not logged in"}
	end,
    {reply, R, State};



handle_call({get_mail, User, Num}, From, State) ->
    R = case lists:keysearch(User, 2, State#state.sessions) of
	    {value, S} ->
		tick(S),
		case (catch do_get_mail(S#s.pops, Num)) of
		    {'EXIT', Reason} ->
			{error, Reason};
		    List ->
			{ok, List}
		end;
	    false ->
		{error, "Not logged in"}
	end,
    {reply, R, State};

handle_call({logout, User}, From, State) ->
    R = case lists:keysearch(User, 2, State#state.sessions) of
	    {value, S} ->
		tick(S),
		(catch pop3lib_cli:quit(S#s.pops));
	    _ ->
		ok
	end,
    {reply, ok, State#state{sessions = 
			    lists:keydelete(User, 2, State#state.sessions)}};

handle_call(num, From, State) ->
    N = State#state.num,
    {reply, N, State#state{num = 1+N}};

handle_call(stop, _,State) ->
    {stop, ok, ok, State}.

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
handle_info({From, idle_timeout}, State) ->
    case lists:keysearch(From, #s.idletimer, State#state.sessions) of
	{value, S} ->
	    pop3lib_cli:quit(S#s.pops),
	    {noreply, State#state{sessions = State#state.sessions -- [S]}};
	_ ->
	    {noreply, State}
    end.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------



tick(S) ->
    S#s.idletimer ! tick.


idle_timer(Top) ->
    receive
	stop ->
	    unlink(Top),
	    exit(normal);
	tick ->
	    idle_timer(Top)
    after (600 * 1000) ->  %% 10 minutes idle time
	    unlink(Top),
	    Top ! {self(), idle_timeout},
	    exit(normal)
    end.

 %% return a list of #mail records
get_mails(S) ->
    {ok, L} = pop3lib_cli:scan(S),
    lists:map(fun({Num, Sz}) ->
		      {ok, Str} = pop3lib_cli:retrieve(S, Num),
		      Mail = parse(#mail{popnum = Num,
					 size = Sz},
				   Str),
		      %% drop content
		      Mail#mail{content = undefined}
	      end, L).


do_get_mail(S, Num) ->
    {ok, Str} = pop3lib_cli:retrieve(S, Num),
    parse(#mail{popnum=Num}, Str).


parse(M, L) ->
    case yaws_api:get_line(L) of
	{line, Line, Tail} ->
	    io:format("Line: ~p~n", [Line]),
	    parse(parse_line(M,  Line), Tail);
	{lastline, Line, Tail} ->
	    M2 = parse_line(M, Line),
	    M2#mail{content = Tail}
    end.

split_colon([$:|Tail], A) ->
    {lists:reverse(A), string:strip(Tail)};
split_colon([H|T], A) ->
    split_colon(T, [H|A]).

parse_line(M, L) ->
    {Head, Data} = split_colon(L, []),
    io:format("Head = ~p  Data = ~p~n", [Head, Data]),
    case Head of
	"Return-Path" ->
	    M#mail{return_path = Data};
	"Received" ->
	    M#mail{received = Data};
	"Date" ->
	    M#mail{date = Data};
	"From" ->
	    M#mail{from = Data};
	"To" ->
	    M#mail{to = Data};
	"Cc" ->
	    M#mail{cc = Data};
	"Subject" ->
	    M#mail{subject = Data};
	"Message-ID" ->
	    M#mail{message_id = Data};
	"Mime-Version" ->
	    M#mail{mime_version = Data};
	"Content-Type" ->
	    M#mail{content_type = Data};
	"Content-Disposition" ->
	    M#mail{content_disposition = Data};
	"User-Agent" ->
	    M#mail{user_agent = Data};
	"Content-Transfer-Encoding" ->
	    M#mail{content_transfer_encoding = Data};
	Other ->
	    M#mail{other_h = [Data | M#mail.other_h]}
    end.

iso2html("=?iso-8859-1?Q?" ++ Tail) ->					 
    iso2html2(Tail);
iso2html(X) ->
    yaws_api:htmlize_l(X).


iso2html2([$=, Hi, Lo |Tail]) ->
    I = yaws:hex_to_integer([Hi,Lo]),
    S = io_lib:format("&#~w;",[I]),
    [S | iso2html2(Tail)];
iso2html2(_) ->
    [].
				

toptab(Mail) ->
        TopTab = 
	io_lib:format("
<table bgcolor=\"#ffffaa00\" border=\"2\" 
       cellpadding=\"5\" cellspacing=\"2\">
<tr border=\"2\">
  <td> <a href=\"reply.yaws?num=~w\">Reply</a> </td>
  <td> <a href=\"del_mail.yaws?num=~w\">Delete </a> </td>
  <td> <a href=\"save_mail.yaws?num=~w\">Save </a> </td>
</table>", 
	  [Mail#mail.popnum,Mail#mail.popnum,Mail#mail.popnum]),
    {html, TopTab}.


render_mail(Mail) ->
    Headers = yaws_api:pre_ssi_string(someh(Mail), "man"),
    Su = ["<h3> Subject: ", webmail:iso2html(Mail#mail.subject), "<h3>"],
    Bodies = bodies(Mail), % yaws_api:pre_ssi_string(Mail#mail.content),
    [toptab(Mail), Headers, {html, Su}, Bodies].

bodies(M) ->
    case M#mail.content_type of
	"multipart/mixed;" ++ Boundary ->
	    multi_bodies(M,string:strip(Boundary, both));
	_ ->
	    part_body(M#mail.content_disposition,
		      M#mail.content_transfer_encoding,
		      M#mail.content_type,
		      M#mail.content)
    end.

multi_bodies(M, Boundary) ->
    case string:tokens(Boundary, "=") of
	["boundary", B] ->
	    multi_bodies2(M, rmall($",B));
	_ ->
	    {html, "<p> unrendereable multipart mail ... no boundary"}
    end.
multi_bodies2(M, B) ->
    Parts = get_parts(M#mail.content, B, scan, [], []),
    Ms = lists:map(fun(P) ->
			   io:format("P = ~p~n", [P]),
			   parse(#mail{}, dropcrnl(P)) end, Parts),
    lists:map(fun(M2) ->
		      part_body(M2#mail.content_disposition,
				M2#mail.content_transfer_encoding,
				M2#mail.content_type,
				M2#mail.content)
	      end, Ms).


get_parts([], B, scan, Ack, Part) ->
    exit({"weird boundary", B});

get_parts(C, B, scan, Ack, Part) ->
    case is_prefix(B, C) of
	{true, Tail} ->
	    get_parts(Tail, B, in, Ack, []);
	false ->
	    get_parts(tl(C), B, scan, Ack, Part)
    end;

get_parts([], _B, in, Ack, Part) ->
    lists:reverse(Ack);

get_parts(C, B, in, Ack, Part) ->
    case is_prefix(B, C) of
	{true, T} ->
	    get_parts(T, B, in, [lists:reverse(Part)|Ack], []);
	false ->
	    get_parts(tl(C), B, in, Ack, [hd(C)|Part])
    end.


is_prefix([], T) ->
    {true, T};
is_prefix([H|T1], [H|T2]) ->
    is_prefix(T1, T2);
is_prefix(_,_) ->
    false.

	    
rmall(X, [X|T]) ->
    rmall(X,T);
rmall(X, [H|T]) ->
    [H|rmall(X,T)];
rmall(_,[]) ->
    [].


dropcrnl([$\n|T]) ->
    dropcrnl(T);
dropcrnl([$\r|T]) ->
    dropcrnl(T);
dropcrnl(T) ->
    T.



semi_split(undefined) ->
    [];
semi_split(L) ->
    string:tokens(L, "; ").

url_to_att(Te, Ct, FileEq, C) ->
    ["filename", FileName] = string:tokens(FileEq, "="),
    C2 = transfer_decode(C, Ct),
    Num = gen_server:call(?MODULE, num),
    File = integer_to_list(Num) ++ [$_|FileName],
    FN = "/tmp/webmail/" ++ File,
    file:write_file(FN, C2),
    {html, io_lib:format("<p> <a href=\"att.yaws?file=~s\">~s</a> ",
			 [File, FileName])}.


part_body(Cd, Te, Ct, C) ->
    case semi_split(Cd) of
	["attachment", FileEq] ->
	    url_to_att(Te, Ct, FileEq, C);
	["attachment"] ->
	    url_to_att(Te, Ct, "filename=unknown", C);
	_ ->
	    inline(Te, Ct, C)
    end.

inline(Te, Ct, C) ->    
    C2 = transfer_decode(C, Te),
    case can_render(Ct) of
	{true, text} ->
	    yaws_api:pre_ssi_string(C2);
	{true, html} ->
	    {html, C2};
	false ->
	    ok
    end.


can_render("text/plain" ++_) ->
    {true, text};
can_render("text/html"++_) ->
    {true, html};
can_render(_) ->
    false.
	    

transfer_decode(C, "base64") ->
    yaws:uue_to_list(C);
transfer_decode(C, _) ->
    C.


someh(M) ->
    lists:flatten(["From: ", M#mail.from, "\n",
		   "To: ", M#mail.to, "\n",
		   case M#mail.cc of
		       undefined -> [];
		       _ -> "Cc: ", M#mail.cc, "\n"
		   end,
		   "Date: ", M#mail.date, "\n"]).

