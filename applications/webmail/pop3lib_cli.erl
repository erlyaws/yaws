-module(pop3lib_cli).
-author('tnt@home.se').
%%% --------------------------------------------------------------------
%%% Created : 3 Mar 1998 by tnt@home.se
%%% Function: A POP3 client library
%%%
%%% Copyright (C) 1998,1999,2000,2001  Torbjörn Törnkvist, tnt@home.se
%%%
%%% This program is free software; you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation; either version 2 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%%%
%%% --------------------------------------------------------------------
-vc('$Id$ ').
-export([connect/1,stat/1,scan/1,scan/2,retrieve/2,delete/2,
	 reset/1,quit/1,uidl/1,uidl/2,top/3]).

%% The session key

-record(sk, {user,            % User name
	     addr,            % Host address
	     passwd,          % Password
	     sockfd,          % Socket filedesc.
	     port=110,        % The POP3 server port number
	     apop=false,      % Use APOP authentication if true.
	     log_mod,         % Log module
	     snoop=false}).   % Trace on/off

-define(CR, 13).
-define(LF, 10).

%% --------------------
%% Init the session key

init_session(Options) ->
    set_options(Options,#sk{}).

set_options([{snoop,Flag}|T],S) ->
    set_options(T,S#sk{snoop=Flag});
set_options([{user,User}|T],S) ->
    set_options(T,S#sk{user=User});
set_options([{passwd,Pw}|T],S) ->
    set_options(T,S#sk{passwd=Pw});
set_options([{addr,Addr}|T],S) ->
    set_options(T,S#sk{addr=Addr});
set_options([{port,Port}|T],S) ->
    set_options(T,S#sk{port=Port});
set_options([apop|T],S) ->
    set_options(T,S#sk{apop=true});
set_options([upass|T],S) ->
    set_options(T,S#sk{apop=false});
set_options([X|_],_) ->
    throw({error,{unknown_option,X}});
set_options([],S) ->
    S.

%% ------------------------------------------
%% Set up a connection to a POP3 server using
%% the specified UserId and Passwd
%% ------------------------------------------

connect(Options) when list(Options) ->
    catch do_connect(Options).

do_connect(Options) when list(Options) ->
    S = init_session(Options),
    Opts = [{packet,raw},{reuseaddr,true},{active,false}],
    case gen_tcp:connect(S#sk.addr,S#sk.port,Opts) of
	{ok,Sock} -> get_greeting(S#sk{sockfd=Sock});
	_         -> {error,connect_failed}
    end.

%% -----------------------------------------------
%% Get the initial greeting from the server and
%% perform the specified authentication procedure.

get_greeting(S) ->
    case recv_sl(S#sk.sockfd) of
	{[$+,$O,$K|T],_} ->
	    answer_greeting(S,T);
	{[$-,$E,$R,$R|T],_} ->
	    if_snoop(S,sender,"-ERR" ++ T),
	    {error,T}
    end.

answer_greeting(S,T) when S#sk.apop==false ->
    if_snoop(S,sender,"+OK" ++ T),
    Msg = "USER " ++ S#sk.user,
    deliver(S,Msg),
    if_snoop(S,client,Msg),
    send_passwd(S);
answer_greeting(S,T) when S#sk.apop==true ->
    if_snoop(S,sender,"+OK" ++ T),
    TS = parse_banner_timestamp(T),
    Digest = epop_md5:string(TS ++ S#sk.passwd),
    Msg = "APOP " ++ S#sk.user ++ " " ++ Digest,
    deliver(S,Msg),
    if_snoop(S,client,Msg),
    get_ok(S).

parse_banner_timestamp(Banner) ->
    case regexp:match(Banner,"<.*>") of
	{match,Start,Length} ->
	    string:substr(Banner,Start,Length);
	_ ->
	    throw({error,apop_banner_timestamp})
    end.

%% -------------------------------------------
%% Wait for an expected ok from the server and
%% reply with the password.

send_passwd(S) ->
    case recv_sl(S#sk.sockfd) of
	{[$+,$O,$K|T],_} ->
	    if_snoop(S,sender,"+OK" ++ T),
	    Msg = "PASS " ++ S#sk.passwd,
	    deliver(S,Msg),
	    if_snoop(S,client,Msg),
	    get_ok(S);
	{[$-,$E,$R,$R|T],_} ->
	    if_snoop(S,sender,"-ERR" ++ T),
	    {error,T}
    end.

%% -------------------
%% Send a STAT request
%% -------------------

stat(S) ->
    Msg = "STAT",
    deliver(S,Msg),
    if_snoop(S,client,Msg),
    get_stat(S).

get_stat(S) ->
    case recv_sl(S#sk.sockfd) of
	{[$+,$O,$K|T],_} ->
	    if_snoop(S,sender,"+OK" ++ T),
	    [NumMsg,TotSize] = string:tokens(T," \r\n"),
	    {ok,{s2i(NumMsg),s2i(TotSize)}};
	{[$-,$E,$R,$R|T],_} ->
	    if_snoop(S,sender,"-ERR" ++ T),
	    {error,T}
    end.

%% ------------------------
%% Send a scan list request
%% ------------------------

scan(S) -> do_scan(S,"LIST",true).

scan(S,Num) when integer(Num) ->
    do_scan(S,"LIST " ++ integer_to_list(Num),false).

do_scan(S,Msg,MultiLine) ->
    deliver(S,Msg),
    if_snoop(S,client,Msg),
    get_scanlist(S,MultiLine).

get_scanlist(S,MultiLine) ->
    case scan_recv(S#sk.sockfd,MultiLine) of
	{[$+,$O,$K|T],_} when MultiLine==true ->
	    [Line1|Ls] = tokenize("+OK" ++ T),
	    if_snoop(S,sender,Line1),
	    F = fun(L) -> if_snoop(S,sender,L) end,
	    lists:foreach(F,Ls),
	    F2 = fun(Str) -> [Num,Sz] = string:tokens(Str," "),
			     {l2i(Num),l2i(Sz)}
		 end,
	    {ok,lists:map(F2,Ls)};
	{[$+,$O,$K|T],_} when MultiLine==false ->
	    if_snoop(S,sender,"+OK" ++ T),
	    [MsgNum,MsgSize] = string:tokens(T," "),
	    {ok,{l2i(MsgNum),l2i(MsgSize)}};
	{[$-,$E,$R,$R|T],_} ->
	    %% According to RFC-1939 page 6. We can only
	    %% receive an error when a specific argument
	    %% was specified in LIST, i.e MultiLine==false.
	    if_snoop(S,sender,"-ERR" ++ T),
	    {error,T}
    end.

%% Do we expect a multi line response or not ?

scan_recv(SockFd,true)  -> recv_ml(SockFd);
scan_recv(SockFd,false) -> recv_sl(SockFd).

%% ------------------
%% Get specified mail
%% ------------------

retrieve(S,MsgNum) when integer(MsgNum) -> 
    Msg = "RETR " ++ integer_to_list(MsgNum),
    deliver(S,Msg),
    if_snoop(S,client,Msg),
    get_retrieve(S).

top(S,MsgNum,Lines) when integer(MsgNum), integer(Lines) -> 
    Msg = "TOP " ++ integer_to_list(MsgNum) ++ " " ++ integer_to_list(Lines),
    deliver(S,Msg),
    if_snoop(S,client,Msg),
    get_retrieve(S).

get_retrieve(S) ->
    case recv_ml_on_ok(S#sk.sockfd) of
	{[$+,$O,$K|T],_} ->
	    {Line,Ls} = get_line("+OK" ++ T),
	    if (S#sk.snoop==true) ->
		    if_snoop(S,sender,Line),
		    io:fwrite("~s~n",[Ls]);
	       true -> true
	    end,
	    {ok,Ls};
	{[$-,$E,$R,$R|T],_} ->
	    if_snoop(S,sender,"-ERR" ++ T),
	    {error,T};
	Else ->
	    Else
    end.

get_line(Str) -> 
    F = fun($\n) -> false;
	   (C)   -> true
	end,
    {Line,[Nl|Rest]} = lists:splitwith(F,Str),
    {Line,Rest}.

%% -------------------
%% Send a uidl request
%% -------------------

uidl(S) -> do_uidl(S,"UIDL",true).

uidl(S,Num) when integer(Num) ->
    do_uidl(S,"UIDL " ++ integer_to_list(Num),false).

do_uidl(S,Msg,MultiLine) ->
    deliver(S,Msg),
    if_snoop(S,client,Msg),
    get_uidllist(S,MultiLine).

get_uidllist(S,MultiLine) ->
    case uidl_recv(S#sk.sockfd,MultiLine) of
	{[$+,$O,$K|T],_} when MultiLine==true ->
	    [Line1|Ls] = tokenize("+OK" ++ T),
	    if_snoop(S,sender,Line1),
	    F = fun(L) -> if_snoop(S,sender,L) end,
	    lists:foreach(F,Ls),
	    F2 = fun(Str) -> [Num,Id] = string:tokens(Str," "),
			     {l2i(Num),Id}
		 end,
	    {ok,lists:map(F2,Ls)};
	{[$+,$O,$K|T],_} when MultiLine==false ->
	    if_snoop(S,sender,"+OK" ++ T),
	    [MsgNum,MsgId] = string:tokens(T," "),
	    {ok,{l2i(MsgNum),MsgId}};
	{[$-,$E,$R,$R|T],_} ->
	    %% We assume that the behaviour here is similar
	    %% to the LIST request, see comment above.
	    if_snoop(S,sender,"-ERR" ++ T),
	    {error,T}
    end.

%% Do we expect a multi line response or not ?

uidl_recv(SockFd,true)  -> recv_ml(SockFd);
uidl_recv(SockFd,false) -> recv_sl(SockFd).


%% ----------------------
%% Mark mail for deletion
%% ----------------------

delete(S,MsgNum) when integer(MsgNum) ->
    Msg = "DELE " ++ integer_to_list(MsgNum),
    deliver(S,Msg),
    if_snoop(S,client,Msg),
    case get_ok(S) of
	{ok,_} -> ok;
	Else   -> Else
    end.


%% --------------------------------------------
%% Remove all delete marks made in this session
%% --------------------------------------------
    
reset(S) -> 
    Msg = "RSET",
    deliver(S,Msg),
    if_snoop(S,client,Msg),
    case get_ok(S) of
	{ok,_} -> ok;
	Else   -> Else
    end.

%% ----------------
%% Quit the session
%% ----------------

quit(S) -> 
    Msg = "QUIT",
    deliver(S,Msg),
    if_snoop(S,client,Msg),
    Res = case get_ok(S) of
	      {ok,_} -> ok;
	      Else   -> Else
	  end,
    gen_tcp:close(S#sk.sockfd),
    Res.

%% ---------------------------------------------
%% Wait for an expected ok from the server which
%% ends this transaction.

get_ok(S) ->
    case recv_sl(S#sk.sockfd) of
	{[$+,$O,$K|T],_} ->
	    if_snoop(S,sender,"+OK" ++ T),
	    {ok,S};
	{[$-,$E,$R,$R|T],_} ->
	    if_snoop(S,sender,"+ERR" ++ T),
	    {error,T}
    end.
    

%% -----------------------------
%% Send a CRLF terminated string

deliver(S,Msg) -> gen_tcp:send(S#sk.sockfd,Msg ++ "\r\n").


%% ---------------------------------------
%% Print trace info if snoop option is set

if_snoop(S,Who,Msg) when S#sk.snoop==true ->
    io:fwrite("~s: ~s~n",[who(Who),Msg]);
if_snoop(_,_,_) ->
    true.

who(sender) -> "S";
who(client) -> "C".



s2i(String) when list(String) ->
    l2i(strip(String)).

%% Remove any trailing stuff from the (ascii) integer value
strip([H|T]) when H>-48,H=<57 -> [H|strip(T)];
strip(_)                      -> [].

l2i(List) when list(List)  -> list_to_integer(List);
l2i(Int) when integer(Int) -> Int.

%% ---------------------------------------------------------
%% If we are receiving a positive response, then receive
%% it as a multi-line response. Otherwise as a single-line.
%% ---------------------------------------------------------

recv_ml_on_ok(S) ->
    case recv_3_chars(S) of
	[$+,$O,$K|T] ->
	    recv_ml(S,[$+,$O,$K|T]);
	Else ->
	    recv_sl(S,Else)
    end.

recv_3_chars(S) -> recv_3_chars(S,recv(S)).

recv_3_chars(S,Cs) when length(Cs)>=3 -> Cs;
recv_3_chars(S,Cs) -> recv_3_chars(S,Cs ++ recv(S)).
		    
%% ------------------------------------------
%% Receive a CRLF.CRLF terminated multi-line.
%% ------------------------------------------

recv_ml(S) ->
    recv_ml(S,[]).

recv_ml(S,Cc) ->
    rml(1,S,Cc,[]).

%% A simple state-event machine to handle the byte stuffing
%% of the termination octet. See also page.2 in the RFC-1939.
%% Since we are using a raw socket we are using this
%% continuation based style of programming.

rml(1,S,[?CR|T],Mline)        -> rml(2,S,T,[?CR|Mline]);     % goto next state
rml(1,S,[?LF|T],Mline)        -> rml(3,S,T,[?LF|Mline]);     % goto next state
rml(1,S,[H|T],Mline)          -> rml(1,S,T,[H|Mline]);       % stay

rml(2,S,[?LF|T],Mline)        -> rml(3,S,T,[?LF|Mline]);     % goto next state
rml(2,S,[H|T],Mline)          -> rml(1,S,[H|T],Mline);       % continue

rml(3,S,[$.|T],Mline)         -> rml(4,S,T,[$.|Mline]);      % goto next state
rml(3,S,[H|T],Mline)          -> rml(1,S,[H|T],Mline);       % continue

rml(4,S,[?CR|T],Mline)        -> rml(5,S,T,[?CR|Mline]);     % goto next state
rml(4,S,[?LF|T],Mline)        -> rml(6,S,T,[?LF|Mline]);     % goto next state
rml(4,S,[H|T],[$.|Mline])     -> rml(1,S,[H|T],Mline);       % continue

rml(5,S,[?LF|T],Mline)        -> rml(6,S,T,[?LF|Mline]);     % goto next state
rml(5,S,[H|T],[$.|Mline])     -> rml(1,S,[H|T],Mline);       % (de-)byte stuff

rml(6,S,T,[?LF,?CR,$.|Mline]) -> {lists:reverse(Mline),T};   % accept
rml(6,S,T,[?LF,$.|Mline])     -> {lists:reverse(Mline),T};   % accept

rml(State,S,[],Mline)         -> rml(State,S,recv(S),Mline). % get more


%% -----------------------------------------------------
%% Receive a complete single-line (ended by a CRLF pair.
%% Returns: {Single-Line, Continuation-Characters (Cc) }
%% Where Cc is the characters next to be processed.
%% -----------------------------------------------------

recv_sl(S) ->
    recv_sl(S,[]).

recv_sl(S,Cc) ->
    complete_sl(S,Cc,[]).

complete_sl(S,[?CR|T],Line) ->
    complete_sl_lf(S,T,[?CR|Line]);
complete_sl(S,[H|T],Line) ->
    complete_sl(S,T,[H|Line]);
complete_sl(S,[],Line) ->
    complete_sl(S,recv(S),Line).

complete_sl_lf(S,[?LF|T],Line) ->
    {lists:reverse([?LF|Line]),T};
complete_sl_lf(S,[H|T],Line) ->
    complete_sl(S,T,[?LF|Line]);
complete_sl_lf(S,[],Line) ->
    complete_sl_lf(S,recv(S),Line).

recv(S) ->
    case gen_tcp:recv(S,0) of
	{ok,Packet} -> Packet;
	Else        -> exit(Else)
    end.

%% -----------------------------------------------
%% Tokenize using \r\n as the two separators.
%% -----------------------------------------------

tokenize(L) ->
    tokenize(skip(L),[]).

tokenize([],Acc) ->
    lists:reverse(Acc);
tokenize(L,Acc) ->
    {Token,Rest} = get_token(L),
    tokenize(skip(Rest),[Token|Acc]).

skip([$\r,$\n|T]) -> skip(T);
skip(L)       -> L.

get_token(L) -> get_token(L,[]).

get_token([H|T],Acc) when H=/=$\r,H=/=$\n ->
    get_token(T,[H|Acc]);
get_token(L,Acc) -> {lists:reverse(Acc),L}.

