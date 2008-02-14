%    -*- Erlang -*- 
%    File:        chat.erl  (chat.erl)
%    Author:        Johan Bevemyr
%    Created:        Thu Nov 18 21:27:41 2004
%    Purpose:   

-module('chat').
-author('jb@son.bevemyr.com').

-define(COLOR1,  "#ffc197").
-define(COLOR2,  "#ff6600").
-define(COLOR3,  "#887da7").
-define(COLOR4,  "#afa2d3").
-define(LOCATION, "test").

%% There is a bug in the erlang inet driver which causes yaws
%% to ignore requests after a POST. This bug is present in the
%% now current release R10B-3. We have submitted a bugfix to
%% erlbugs so it may be fixed in some future release. Until then...

-define(ERL_BUG, true).

-export([check_session/1, get_user/1, login/2, chat_server_init/0,
        session_server/0,dynamic_headers/0, display_login/2]).

-export([chat_read/1, chat_write/1]).

-include("../../../include/yaws_api.hrl").

-record(user, {last_read,
               buffer=[],
               user,
               pid,
               color,
               cookie}).

 
login(User, Password) ->
    session_server(),
    erlang:send(chat_server, {new_session, User, self()}),
    receive
        {session_manager, Cookie, Session} ->
            chat_server ! {join_message, User},
            {ok, Cookie};
        _ ->
            error
    end.

%% FIXME: way to simple session handling. The system will behave
%% very badly if two users log in with the same user name!!!

check_session(A) ->
    H = A#arg.headers,
    case yaws_api:find_cookie_val("sessionid", H#headers.cookie) of
        [] ->
            display_login(A, "not logged in");
        CVal ->
            case check_cookie(CVal) of
                error ->
                    display_login(A, "not logged in");
                Session ->
                    {ok, Session}
            end
    end.

check_cookie(Cookie) ->
    session_server(),
    chat_server ! {get_session, Cookie, self()},
    receive
        {session_manager, {ok, Session}} ->
            Session;
        {session_manager, error} ->
            error
    end.

get_user(Session) ->
    Session#user.user.

display_login(A, Status) ->
    (dynamic_headers() ++
     [{ehtml,
       [{body, [{onload,"document.f.user.focus();"},{bgcolor,?COLOR3}],
         [{table, [{border,0},{bgcolor,?COLOR2},{cellspacing,1},
                   {width,"100%"}],
           {tr,[{bgcolor,?COLOR1},{height,30}],
            {td,[{nowrap,true},{align,left},{valign,middle}],
                   {b,[],
                    {font, [{size,4},{color,black}],
                     ["Chat at ", ?LOCATION]}}}}},
          {pre_html, io_lib:format("<p>Your login status is: ~s</p>",
                                   [Status])},
          {form,
           [{method,post},
            {name,f},
            {action, "login.yaws"},
            {autocomplete,"off"}],
           {table,[{cellspacing, "5"}],
            [{tr, [],
              [{td, [], {p, [], "Username:"}},
               {td, [], {input, [{name, user},
                                 {type, text},
                                 {size, "20"}]}}
              ]},
             {tr, [],
              [{td, [], {p, [], "Password:"}},
               {td, [], {input, [{name, password},
                                 {type, password},
                                 {size, "20"}]}}]},
             {tr, [],
              {td, [{align, "right"}, {colspan, "2"}],
               {input, [{type, submit},
                        {value, "Login"}]}}}
            ]}}]
        }]
      }]).


session_server() ->
    case whereis(chat_server) of
        undefined ->
            Pid = proc_lib:spawn(?MODULE, chat_server_init, []),
            register(chat_server, Pid);
        _ ->
            done
    end.

%%

chat_server_init() ->
    process_flag(trap_exit, true),
    io:format("Starting chat server\n"),
    put(color_idx, 0),
    chat_server([]).

%%

chat_server(Users0) ->
    Users = gc_users(Users0),
    %% io:format("Users = ~p\n", [Users]),
    receive
        {get_session, Cookie, From} ->
            %% io:format("get_session ~p\n", [Cookie]),
            case lists:keysearch(Cookie, #user.cookie, Users) of
                {value, Session} ->
                    From ! {session_manager, {ok, Session}};
                false ->
                    From ! {session_manager, error}
            end,
            chat_server(Users);
        {new_session, User, From} ->
            Cookie = integer_to_list(random:uniform(1 bsl 50)),
            Session = #user{cookie=Cookie, user=User, color=pick_color()},
            From ! {session_manager, Cookie, Session},
            chat_server([Session|Users]);
        {write, Session, Msg} ->
            NewUsers = send_to_all(msg,
                                   fmt_msg(Session#user.user, Msg,
                                           Session#user.color),
                                   Users),
            chat_server(NewUsers);
        {send_to, User, Msg} ->
            NewUsers = send_to_one(msg, Msg, Users, User),
            chat_server(NewUsers);
        {join_message, User} ->
            NewUsers0 = send_to_all(msg,fmt_join(User), Users),
            NewUsers1 = send_to_all(members,
                                    fmt_members(NewUsers0), NewUsers0),
            chat_server(NewUsers1);
        {members, User} ->
            NewUsers1 = send_to_one(members,
                                    fmt_members(Users),
                                    Users, User),
            chat_server(NewUsers1);
        {left_message, User} ->
            NewUsers0 = send_to_all(msg,fmt_left(User), Users),
            NewUsers1 = send_to_all(members,
                                    fmt_members(NewUsers0), NewUsers0),
            chat_server(NewUsers1);
        {read, Session, Pid} ->
            %% io:format("~p want read ~p\n", [Session#user.user, Pid]),
            NewUsers = user_read(Users, Session, Pid),
            chat_server(NewUsers);
        {cancel_read, Pid} ->
            NewUsers = cancel_read(Users, Pid),
            chat_server(NewUsers)
    after
        5000 ->
            chat_server(Users)
    end.
            

%%

cancel_read([], _Pid) ->
    [];
cancel_read([U|Us], Pid) when U#user.pid == Pid ->
    Now = inow(now()),
    [U#user{pid=undefined,last_read=Now}|Us];
cancel_read([U|Us], Pid) ->
    [U|cancel_read(Us, Pid)].

%%

user_read(Users, User, Pid) ->
    user_read(Users, User, Pid, Users).

user_read([], User, Pid, All) ->
    All;

user_read([U|Users], User, Pid, All) when U#user.cookie == User#user.cookie ->
    if U#user.buffer /= [] ->
            Pid ! {msgs,lists:reverse(U#user.buffer)},
            [U#user{buffer=[]}|Users];
       true ->
            [U#user{pid=Pid}|Users]
    end;

user_read([U|Users], User, Pid, All) ->
    [U|user_read(Users, User, Pid, All)].

%%

send_to_all(Type, Msg, Users) ->
    Now = inow(now()),
    F = fun(U) ->
                if U#user.pid /= undefined ->
                        %% io:format("Sending ~p to ~p\n", [Msg, U#user.user]),
                        U#user.pid ! {msgs, [{Type, Msg}]},
                        U#user{pid=undefined, last_read = Now}; 
                   true ->
                        U#user{buffer=[{Type,Msg}|U#user.buffer]}
                end
        end,
    lists:map(F, Users).

%%

send_to_one(Type, Msg, Users, User) ->
    Now = inow(now()),
    F = fun(U) when U#user.cookie == User#user.cookie  ->
                if U#user.pid /= undefined ->
                        %% io:format("Sending ~p to ~p\n", [Msg, U#user.user]),
                        U#user.pid ! {msgs, [{Type, Msg}]},
                        U#user{pid=undefined, last_read = Now}; 
                   true ->
                        U#user{buffer=[{Type,Msg}|U#user.buffer]}
                end;
           (U) ->
                U
        end,
    lists:map(F, Users).

%%


gc_users(Users) ->
    Now = inow(now()),
    gc_users(Users, Now).

gc_users([], _Now) ->
    [];
gc_users([U|Us], Now) ->
    if U#user.pid == undefined, (Now-U#user.last_read > 10) ->
            self() ! {left_message, U#user.user},
            gc_users(Us, Now);
       true ->
            [U|gc_users(Us, Now)]
    end.

%

inow(Now) ->
    {MSec, Sec, _} = Now,
    MSec*1000000 + Sec.

%

dynamic_headers() ->
    [yaws_api:set_content_type("text/html"),
     {header, {cache_control, "no-cache"}},
     {header, "Expires: -1"}].

%

chat_read(A) ->
    session_server(),
    case check_session(A) of
        {ok, Session} ->
            chat_server ! {read, Session, self()},
            if length(A#arg.querydata) > 0 ->
                    chat_server ! {members, Session};
               true ->
                    ok
            end,
            receive
                {msgs, Messages} ->
                    M = [fmt_type(Type,L) || {Type, L} <- Messages],
                    dynamic_headers()++[{html, ["ok",M]}, break]
            after
                20000 ->
                    catch erlang:send(chat_server, {cancel_read, self()}),
                    dynamic_headers()++[{html, "timeout"}, break]
            end;
        Error ->
            dynamic_headers()++[{html, "error"}, break]
    end.

type2tag(msg) -> $m;
type2tag(members) -> $e.

%

fmt_type(Type, L) ->
    Data = list_to_binary(L),
    [type2tag(Type), integer_to_list(size(Data)),":", Data].

%

-ifdef(ERL_BUG).
chat_write(A) ->
    session_server(),
    case check_session(A) of
        {ok, Session} ->
            chat_server ! {write, Session, A#arg.clidata},
            [{html, "ok"},
              break];
        Error ->
            Error
    end.
-else.
chat_write(A) ->
    session_server(),
    case check_session(A) of
        {ok, Session} ->
            chat_server ! {write, Session,A#arg.clidata},
            [{header, {connection,"close"}},
             {html, "ok"},
             break];
        Error ->
            Error
    end.
-endif.

%%

fmt_join(User) ->
    ["<strong>",date_str()," ",User, " joined</strong>"].

%%

fmt_left(User) ->
    ["<strong>",date_str()," ",User," left</strong>"].

%%

fmt_msg(User, Msg, Color) ->
    ["<font color=",Color,">",date_str()," <strong>",User,":</strong></font> ",
     Msg].

%%

fmt_members(Users) -> 
    [[U#user.user,"<br>"] || U <- Users].

%%

date_str() ->
    {_,{H,M,S}} = calendar:local_time(),
    io_lib:format("<small>(~2.2.0w:~2.2.0w:~2.2.0w)</small>", [H,M,S]).

%%

pick_color() ->
    Nr = get(color_idx),
    put(color_idx, (Nr+1) rem 4),
    colors(Nr).

%%

colors(0) -> "blue";
colors(1) -> "orange";
colors(2) -> "red";
colors(3) -> "green".

