%%%----------------------------------------------------------------------
%%% File    : smtp_client.erl
%%% Author  : Joe <joe@bluetail.com>
%%% Purpose : Send mail to an SMTP host
%%% Created : 29 Oct 1999 by Joe <joe@bluetail.com>
%%%----------------------------------------------------------------------

-module(smtp_client).

-compile(export_all).

-export([test/1, send_mail/6, send_mail1/6]).

test(1) ->
    send_mail("mail.bluetail.com", 25, 
	 "licence_server@bluetail.com", 
	 "joe@bluetail.com",
	 "BMR 1.1 licence",
	 "Hello joe\n").

send_mail(Host, Port, From, To, Subject, Str) ->
    send_mail1(Host, Port, From, To, Subject, "\n\n" ++ Str).

send_mail1(Host, Port, From, To, Subject, Str) ->
    S = e_open(Host, Port),
    e_expect_eXXX(S, "2XX"),
    e_send(S, "Mail from:<" ++ From ++ ">\n"),
    e_expect_eXXX(S,  "2XX"),
    e_send(S,  "RCPT TO:<" ++ To ++ ">\n"),
    e_expect_eXXX(S,  "250"),
    e_send(S,  "DATA\n"),
    e_expect_eXXX(S,  "354"),
    Str1 = "From:" ++ From ++ "\nTo:" ++ To ++ "\nSubject:" ++ 
	Subject ++ Str,
    e_send(S,  Str1),
    e_send(S,  "\n.\n"),
    e_expect_eXXX(S,  "250"),
    e_send(S, "QUIT\n"),
    e_expect_eXXX(S,  "2XX"), 
    e_expect_portClosed(S).

%%----------------------------------------------------------------------
%% Primitives

e_open(Host, Port) ->
    case gen_tcp:connect(Host, Port, 
			 [binary, {packet, 0},{active,true}]) of
	{ok, Socket} ->
	    %% io:format("Opening ~p ~p~n", [Host, Port]),
	    Socket;
	Other ->
	    io:format("cannot open a port to:~p~n", [Host]),
	    exit(error)
    end.

e_send(Port, Str) ->
    %% io:format(">>> ~p~n", [Str]),
    gen_tcp:send(Port, Str),
    ok.

%% expect a three digit SMTP return code

e_expect_eXXX(Port, Reply) ->
    case receive_reply(Port) of
	port_closed ->
	    io:format("Expecting ~p but port_closed~n", [Reply]),
	    e_close(Port),
	    exit(expect);
	Str ->
	    case match(Reply, Str) of
		true -> 
		    ok;
		false -> 
		    io:format("Expecting ~p received ~p ~n", [Reply, Str]),
		    e_close(Port),
		    exit(expect)
	    end
    end.

e_expect_portClosed(Port) ->
    %% io:format("e_expect_portClosed~n"),
    case receive_reply(Port) of
	port_closed ->
	    true;
	Str ->
	    io:format("Expecting port closed got:~p~n",[Str]),
	    e_close(Port),
	    exit(expect)
    end.

e_close(Port) ->
    gen_tcp:close(Port).

receive_reply(Port) ->
    receive_reply(Port, []).

receive_reply(Port, SoFar0) ->
    receive
	{tcp,Port,B} ->
	    SoFar1 = SoFar0 ++ binary_to_list(B),
	    %% io:format("<<< ~s~n", [SoFar1]),
	    case regexp:match(SoFar1, "[0-9][0-9][0-9] .*\r\n$") of
		{match,Start,Len} -> string:substr(SoFar1, Start, 3);
		nomatch           -> receive_reply(Port, SoFar1)
	    end;
	{tcp_closed,Port} -> 
            port_closed;
        Other ->
            io:format("other stuff:~p~n", [Other]),
            receive_reply(Port, SoFar0)
    end.

match([H|T], [H|T1]) ->
    match(T, T1);
match([$X|T], [H|T1]) ->
    match(T, T1);
match([], []) ->
    true;
match(_, _) ->
    false.




