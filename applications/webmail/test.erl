%%%----------------------------------------------------------------------
%%% File    : test.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created :  2 Jul 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(test).
-author('klacke@hyber.org').

-compile(export_all).

pop() ->
    pop(1).

pop(N) ->
    {ok, S} = pop3lib_cli:connect([{user, "klacke"},
			 {addr, {127,0,0,1}},
			 {passwd, "ulMer9"}]),
    
    Stat = pop3lib_cli:stat(S),
    Scan = pop3lib_cli:scan(S),
    {ok, Re} = pop3lib_cli:retrieve(S, N),
    io:format("Mail ~p: ~n~s", [N, Re]),
    {Stat, Scan}.


s() ->
   {ok, S} = pop3lib_cli:connect([{user, "klacke"},
			 {addr, {127,0,0,1}},
			 {passwd, "ulMer9"}]),
    S.

s2() ->
   {ok, S} = pop3lib_cli:connect([{user, "klacke"},
			 {addr, {213,67,177,217}},
			 {passwd, "ulMer9"}]),
    S.
    

pop2() ->
    webmail:get_mails(s()).


