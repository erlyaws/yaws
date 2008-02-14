%%%-------------------------------------------------------------------
%%% File    : attachment.erl
%%% Author  :  <klacke@hyber.org>
%%% Description : 
%%%
%%% Created :  4 Feb 2004 by  <klacke@hyber.org>
%%%-------------------------------------------------------------------
-module(attachment).
-compile(export_all).

out(A) ->
    case mail:check_session(A) of
        {ok, Session} ->
            case {yaws_api:queryvar(A, "nr"),yaws_api:queryvar(A,"form")} of
                {{ok, Nr},{ok,"text"}} ->
                    mail:send_attachment_plain(Session, yaws:to_integer(Nr));
                {{ok, Nr},_} ->
                    mail:send_attachment(Session, yaws:to_integer(Nr));
                _ ->
                    err()
            end;
        Error ->
            err()
    end.


err() ->
    [{status, 404},
     {header, {connection, "close"}}].

     
     
     


