%    -*- Erlang -*- 
%    File:        smtp.erl  (~jb/work/yaws/applications/mail/src/smtp.erl)
%    Author:        Johan Bevemyr
%    Created:        Tue Feb 24 23:15:59 2004
%    Purpose:   

-module('smtp').
-author('jb@bevemyr.com').

-export([send/6]).

%% Example

%
% smtp:send("mail.bevemyr.com", "jb@bevemyr.com", 
%            ["katrin@bevemyr.com","jb@bevemyr.com"],
%            "Test Subject",
%            "My Message", [{"file1.txt","text/plain","hej hopp igen"}]).
%


send(Server, From, To, Subject, Message, Attached) ->
    {ok, Port} = smtp_init(Server, From, To),
    Boundary="--Next_Part("++boundary_date()++")--",
    CommonHeaders = 
        [mail_header("To: ", To),
         mail_header("From: ", From),
         mail_header("Subject: ", Subject)],
    Headers = case Attached of
                  [] ->
                      [mail_header("Content-Type: ", "text/plain"),
                       mail_header("Content-Transfer-Encoding: ", "8bit")];
                  _ ->
                      [mail_header("Mime-Version: ", "1.0"),
                       mail_header("Content-Type: ",
                                   "Multipart/Mixed;\r\n boundary=\""++
                                   Boundary++"\""),
                       mail_header("Content-Transfer-Encoding: ", "8bit")]
              end,
    smtp_send_part(Port, [CommonHeaders, Headers, "\r\n"]),
    case Attached of
        [] ->
            ok;
        _ ->
            smtp_send_part(Port, ["--",Boundary,"\r\n",
                                  mail_header("Content-Type: ",
                                              "Text/Plain; charset=us-ascii"),
                                  mail_header("Content-Transfer-Encoding: ",
                                              "8bit"),
                                  "\r\n"])
    end,
    smtp_send_message(Port, Message),
    case Attached of
        [] ->
            smtp_send_part(Port, ["\r\n.\r\n"]),
            smtp_close(Port);
        Files ->
            smtp_send_attachments(Port, Boundary, Files),
            smtp_send_part(Port, ["\r\n.\r\n"]),
            smtp_close(Port)
    end.


smtp_send_attachments(Port, Boundary, []) ->
    smtp_send_part(Port, ["\r\n--",Boundary,"--\r\n"]);
smtp_send_attachments(Port, Boundary, [{FileName,ContentType,Data}|Rest]) ->
    smtp_send_part(Port, ["\r\n--",Boundary,"\r\n",
                          mail_header("Content-Type: ", ContentType),
                          mail_header("Content-Transfer-Encoding: ",
                                      "base64"),
                          mail_header("Content-Disposition: ",
                                      "attachment; filename=\""++
                                      FileName++"\""),
                          "\r\n"
                         ]),
    smtp_send_b64(Port, Data),
    smtp_send_attachments(Port, Boundary, Rest).


smtp_send_b64(Port, Data) ->
    B64 = str2b64_final(Data),
    gen_tcp:send(Port, B64).

boundary_date() ->
    dat2str_boundary(calendar:local_time()).

dat2str_boundary({{Y, Mo, D}, {H, M, S}}) ->
    lists:flatten(
      io_lib:format("~s_~2.2.0w_~s_~w_~2.2.0w:~2.2.0w:~2.2.0w_~w",
                    [weekday(Y,Mo,D), D, int_to_mt(Mo),
                     Y,H,M,S,random:uniform(5000)])).


smtp_init(Server, From, Recipients) ->
    {ok, Port} = gen_tcp:connect(Server, 25, [{active, false},
                                              {reuseaddr,true},
                                              binary]),
    smtp_expect(220, Port, "SMTP server does not respond"),
    smtp_put( smtp_from(From), Port ),
    smtp_expect(250, Port, "Sender not accepted by mail server"),
    send_recipients(Recipients, Port),
    smtp_put("DATA", Port),
    smtp_expect(354, Port, "Message not accepted by mail server."),
    {ok, Port}.


smtp_close(Port) ->
    smtp_put(".", Port),
    smtp_expect(250, Port, "Message not accepted by mail server."),
    gen_tcp:close(Port),
    ok.

smtp_send_part(Port, Data) ->
    gen_tcp:send(Port, Data).

smtp_send_message(Port, Data) ->
    {_LastNL, Escaped} = dot_escape(Data, true),
    gen_tcp:send(Port, Escaped).

send_recipients( Recipients, Port ) ->
        Fun = fun (R) ->
                        smtp_put( smtp_recipient(R), Port),
                        smtp_expect(250, Port, io_lib:format("Recipient ~s not accepted.",[R]))
                end,
        lists:foreach( Fun, Recipients ).

smtp_put(Message, Port) ->
    gen_tcp:send(Port, [Message,"\r\n"]).

smtp_expect(Code, Port, ErrorMsg) ->
    smtp_expect(Code, Port, [], ErrorMsg).

smtp_expect(Code, Port, Acc, ErrorMsg) ->
    Res = gen_tcp:recv(Port, 0, 15000),
    case Res of
        {ok, Bin} ->
            NAcc = Acc++binary_to_list(Bin),
            case string:chr(NAcc, $\n) of
                0 ->
                    smtp_expect(Code, Port, NAcc, ErrorMsg);
                _N ->
                    ResponseCode = to_int(NAcc),
                    if 
                        ResponseCode == Code -> ok;
                        true -> throw({error, ErrorMsg})
                    end
            end;
        Err ->
            throw({error, Err})
    end.

%% add smtp from prelude. add <> around address (if needed)
smtp_from( Address ) ->
        lists:append( "MAIL FROM: ", add_angle_brackets( Address ) ).

%% add smtp recipients prelude. add <> around address (if needed)
smtp_recipient( Address ) ->
        lists:append( "RCPT TO: ", add_angle_brackets( Address ) ).

%% make sure the address has <> around itself
add_angle_brackets( Address ) ->
        add_angle_bracket_start( add_angle_bracket_close(Address) ).

add_angle_bracket_start( [$<|T] ) -> [$<|T];
add_angle_bracket_start( Address ) -> [$<|Address].

%% add > at the end of address, if it is not present
add_angle_bracket_close( Address ) ->
        case lists:reverse( Address ) of
        [$>|_T] -> Address;
        Reversed -> lists:reverse( [$>|Reversed] )
        end.

%% Add an . at all lines starting with a dot.

dot_escape(Data, NL) ->
    dot_escape(Data, NL, []).

dot_escape([], NL, Acc) ->
    {NL, lists:reverse(Acc)};
dot_escape([$.|Rest], true, Acc) ->
    dot_escape(Rest, false, [$.,$.|Acc]);
dot_escape([$\n|Rest], _, Acc) ->
    dot_escape(Rest, true, [$\n|Acc]);
dot_escape([C|Rest], _, Acc) ->    
    dot_escape(Rest, false, [C|Acc]).

%%

%dot_unescape(Data) ->
%    {_,Dt} = dot_unescape(Data, true, []),
%    Dt.
%
%dot_unescape([], NL, Acc) ->
%    {NL, lists:reverse(Acc)};
%dot_unescape([$.|Rest], true, Acc) ->
%    dot_unescape(Rest, false, Acc);
%dot_unescape([$\n|Rest], _, Acc) ->
%    dot_unescape(Rest, true, [$\n|Acc]);
%dot_unescape([L|Rest], NL, Acc) when list(L) ->
%    {NL2, L2} = dot_unescape(L, NL, []),
%    dot_unescape(Rest, NL2, [L2|Acc]);
%dot_unescape([C|Rest], _, Acc) ->
%    dot_unescape(Rest, false, [C|Acc]).


%%

str2b64_final(String) ->
    str2b64_final(String, []).


str2b64_final([], Acc) ->
    lists:reverse(Acc);
str2b64_final(String, Acc) ->
    case str2b64_line(String, []) of
        {ok, Line, Rest} ->
            str2b64_final(Rest, ["\n",Line|Acc]);
        {more, Cont} ->
            lists:reverse(["\n",str2b64_end(Cont)|Acc])
    end.

%%

str2b64_line(S, []) -> str2b64_line(S, [], 0);
str2b64_line(S, {Rest,Acc,N}) -> str2b64_line(Rest ++ S, Acc, N).

str2b64_line(S, Out, 76) -> {ok,lists:reverse(Out),S};
str2b64_line([C1,C2,C3|S], Out, N) ->
    O1 = e(C1 bsr 2),
    O2 = e(((C1 band 16#03) bsl 4) bor (C2 bsr 4)),
    O3 = e(((C2 band 16#0f) bsl 2) bor (C3 bsr 6)),
    O4 = e(C3 band 16#3f),
    str2b64_line(S, [O4,O3,O2,O1|Out], N+4);
str2b64_line(S, Out, N) ->
    {more,{S,Out,N}}.

%%

str2b64_end({[C1,C2],Out,_N}) ->
    O1 = e(C1 bsr 2),
    O2 = e(((C1 band 16#03) bsl 4) bor (C2 bsr 4)),
    O3 = e((C2 band 16#0f) bsl 2),
    lists:reverse(Out, [O1,O2,O3,$=]);
str2b64_end({[C1],Out,_N}) ->
    O1 = e(C1 bsr 2),
    O2 = e((C1 band 16#03) bsl 4),
    lists:reverse(Out, [O1,O2,$=,$=]);
str2b64_end({[],Out,_N}) -> lists:reverse(Out);
str2b64_end([]) -> [].

%%

e(X) when X >= 0, X < 26 -> X + $A;
e(X) when X >= 26, X < 52 -> X + $a - 26;
e(X) when X >= 52, X < 62 -> X + $0 - 52;
e(62) -> $+;
e(63) -> $/;
e(X) -> erlang:fault({badchar,X}).


%%


weekday(Y,Mo,D) ->
    int_to_wd(calendar:day_of_the_week(Y,Mo,D)).

int_to_wd(1) -> "Mon";
int_to_wd(2) -> "Tue";
int_to_wd(3) -> "Wed";
int_to_wd(4) -> "Thu";
int_to_wd(5) -> "Fri";
int_to_wd(6) -> "Sat";
int_to_wd(7) -> "Sun".

int_to_mt(1)  -> "Jan";
int_to_mt(2)  -> "Feb";
int_to_mt(3)  -> "Mar";
int_to_mt(4)  -> "Apr";
int_to_mt(5)  -> "May";
int_to_mt(6)  -> "Jun";
int_to_mt(7)  -> "Jul";
int_to_mt(8)  -> "Aug";
int_to_mt(9)  -> "Sep";
int_to_mt(10) -> "Oct";
int_to_mt(11) -> "Nov";
int_to_mt(12) -> "Dec".

%%

mail_header(_Key, []) -> [];
mail_header(Key, Val) -> Key++Val++"\r\n".

%%

to_int(Str) ->
    to_int(Str, 0).

to_int([D|Ds], Acc) when D >= $0, D =< $9->
    to_int(Ds, Acc*10+D-$0);
to_int(_, Acc) -> Acc.

%%

