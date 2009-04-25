
%% a small shoppingcart example which tries to show
%% a variety of tricks and tacticts to display a 
%% shoppingcart style page with server side state.


-module(shopcart).
-author('klacke@hyber.org').

-compile(export_all).
-include("../../include/yaws_api.hrl").
-include_lib("kernel/include/inet.hrl").


%% this is the opaque structure we pass to the
%% yaws cookie session server

-record(sess, {
          user,
          passwd,
          addr,
          items = []}).
          

%% this function extracts the session from the cookie
check_cookie(A) ->
    H = A#arg.headers,
    case yaws_api:find_cookie_val("ssid", H#headers.cookie) of
        Val when Val /= [] ->
            case yaws_api:cookieval_to_opaque(Val) of
                {ok, Sess} ->
                    {ok, Sess, Val};
                {error, {has_session, Sess}} ->
                    {ok, Sess};
                Else ->
                    Else
            end;
        [] ->
            {error, nocookie}
    end.


%% this function is calle first in all out yaws files,
%% it will autologin users that are not logged in
top(A) ->
    case check_cookie(A) of
        {ok, _Session, _Cookie} ->
            ok;
        {error, _Reason} ->
            login(A)
    end.



%% generate a css head  the title of the page set dynamically
css_head(PageTitle) ->
    Z = 
    [<<"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>

<head>
 <meta name=\"keywords\" content=\"Nortel Extranet VPN\">
 <title>">>,
     PageTitle,
     <<"</title>
 <meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">
 <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\">
</head>

<body bgcolor=\"linen\">

">>
    ],
    {html, Z}.



%% the little status field in the upper left corner
head_status(User) ->
    {ehtml, 
     {table, [],
      {tr, [],
       [{td, [{width, "30%"}],
         {table, [ {border, "1"}, {bgcolor, beige},{bordercolor, black}],
          [{tr, [], {td, [], pb("User: ~s", [User])}}
          ]}
        },
        {td, [{align, right}], {img, [{src, "junk.jpg"}  
                                     ]}}
       ]
      }
     }
    }.


%% bold paragraph according to style.css
pb(Fmt, Args) ->
    {p, [{class, pb}], io_lib:format(Fmt, Args)}.


%% toprow of buttons to push
toprow() ->
    {ehtml,
     {table, [{cellspacing, "4"}, 
              {bgcolor, tan},
              {width, "100%"}
              ],
      [
       {tr, [], 
        [{td, [], {a, [{href, "buy.yaws"}] , {p, [{class, toprow}], "Buy"}}},
         {td, [], {a, [{href, "logout.yaws"}], {p, [{class, toprow}], "Logout"}}},
         {td, [], {a, [{href, "source.html"}], {p, [{class, toprow}], "The Source"}}},
         {td, [{width, "70%"}], ""} 
        ]}
      ]
     }
    }.



%% kinda hackish since we us ehtml
bot() ->
    {html, "</body> \n </html> \n"}.



%% This function displays the login page
login(A) ->
    CSS = css_head("Shopcart"),
    Head = head_status("Not lgged in"),
    Top = toprow(),
    Login =
        {ehtml,
         [{h2, [], "Shopcart login"},
          {form, [{method, get},
                  {action, "loginpost.yaws"}],
           [
            {p, [], "Username"},
            {input, [{name, user},
                     {type, text},
                     {value, "Joe Junk shopper"},
                     {size, "48"}]},
            
            
            {p, [], "Password"},
            {input, [{name, password},
                     {type, text},
                     {value, "xyz123"},
                     {size, "48"}]},
            
            {input, [{type, submit},
                     {value, "Login"}]},

            {input, [{name, url},
                     {type, hidden},
                     {value, xpath((A#arg.req)#http_request.path, A)}]}
           ]
           }
         ]},
    [CSS, Head, Top, Login, bot(), break].




logout(A) ->
    {ok, _Sess, Cookie} = check_cookie(A),
    yaws_api:delete_cookie_session(Cookie),
    {ehtml, {h3, [], "Yo, "}}.




%% This is the function that gets invoked when the
%% user has attempted to login
%% The trick used here is to pass the original URL in a hidden
%% field into this function, if the login is successful, we redirect
%% to the original URL.

loginpost(A) ->
    case {yaws_api:queryvar(A, "user"),
          yaws_api:queryvar(A, "url"),
          yaws_api:queryvar(A, "password")} of

        {{ok, User},
         {ok, Url},
         {ok, Pwd}} ->
            
            %% here's the place to validate the user
            %% we allow all users,
            io:format("User ~p logged in ~n", [User]),
            Sess = #sess{user = User,
                         passwd = Pwd},
            Cookie = yaws_api:new_cookie_session(Sess),
            [yaws_api:redirect(Url),
             yaws_api:setcookie("ssid",Cookie)];
        _ ->
            login(A)
    end.

xpath({abs_path, P}, _A) ->    
    P.

%% this is the function that gets the form when the user
%% hits "update Cart"

formupdate(A) ->
    {ok, Sess, Cookie} = check_cookie(A),
    _J = junk(),
    Items = Sess#sess.items,
    L = yaws_api:parse_post(A),
    I2 = handle_l(L, Items),
    Sess2 = Sess#sess{items = I2},
    yaws_api:replace_cookie_session(Cookie, Sess2),
    {redirect, "index.yaws"}.  %% force browser to reload
    
handle_l([], Items) ->
    Items;
handle_l([{Str, Num} | Tail], Items) ->
    case catch list_to_integer(Num) of
        Int when is_integer(Int) ->
            handle_l(Tail, [{Str, Int} | lists:keydelete(Str,1, Items)]);
        _ ->
            handle_l(Tail, Items)
    end.


ip(A) ->
    S = A#arg.clisock,
    case inet:peername(S) of
        {ok, {Ip, _Port}} ->
            case inet:gethostbyaddr(Ip) of
                {ok, HE} ->
                    io_lib:format("~s/~s", [fmtip(Ip), HE#hostent.h_name]);
                _Err ->
                    io_lib:format("~s", [fmtip(Ip)])
            end;
        _ ->
            []
    end.

fmtip({A,B,C,D}) ->
    io_lib:format("~w.~w.~w.~w", [A,B,C,D]).


%% generate the final "you have bought page ... "
buy(A) ->
    {ok, Sess, _Cookie} = check_cookie(A),
    Css = css_head("Shopcart"),
    Head = head_status(Sess#sess.user),
    Top = toprow(),
    BROWS = b_rows(Sess#sess.items),
    Res = 
        if
            length (BROWS) > 0 ->
                {ehtml,
                 [{h4, [], "Congratulations, you have bought"},
                  {table, [],BROWS},
                  {hr},
                  {p , [{class, toprow}],
                   io_lib:format(
                     "Items are at this very moment being shipped to the"
                     " residens of the computer with IP: ~s~n", [ip(A)])}
                 ]
                };
            true ->
                {ehtml,
                 [{h4, [], "Congratulations, you have bought nothing"}]}
        end,

                 
    [Css, Head, Top, Res, bot()].


b_rows(Items) ->
    J = junk(),
    Desc = {tr,[],
            [
             {th, [], pb("Description",[])},
             {th, [], pb("Quantity",[])},
             {th, [], pb("Sum ",[])}]},

    [Desc | b_rows(Items, J, 0, [])].

b_rows([{Desc, Num}|Tail], Junk, Ack, TRS) when Num >  0 ->
    {value, {_, Price}} = lists:keysearch(Desc, 1, Junk),
    A = Num * Price,
    TR = {tr, [], 
          [{td, [], Desc},
           {td, [], io_lib:format("~w", [Num])},
           {td, [], io_lib:format("~w", [A])}
          ]},
    b_rows(Tail, Junk, A+Ack, [TR|TRS]);

b_rows([{_Desc, Num}|Tail], Junk, Ack, TRS) when Num ==  0 ->
     b_rows(Tail, Junk, Ack, TRS);

b_rows([], _, Ack, TRS) when Ack > 0 ->
    Tax = round(0.27 * Ack),
    Empty = {td, [], []},
    TaxRow = {tr, [],
              [
               {td, [],  pb("Swedish VAT tax 27% ",[])},
               Empty,
               {td, [], pb("~w", [Tax])}
              ]},
    Total = {tr, [],
              [
               {td, [],  pb("Total ",[])},
               Empty,
               {td, [], pb("~w", [Ack + Tax])}
              ]},
    
    lists:reverse([Total, TaxRow | TRS]);
b_rows(_, _,_,_) ->
    [].




%% this is the main function which displays 
%% the shopcart .....
%% the entire shopcart is one big form which gets posted
%% when the user updates the shopcart
index(A) ->
    {ok, Sess, _Cookie} = check_cookie(A),
    io:format("Inside index: ~p~n", [Sess#sess.items]),
    Css = css_head("Shopcart"),
    Head = head_status(Sess#sess.user),
    Top = toprow(),
    Cart =
        {ehtml,
         {form, 
          [{name, form},
           {method,post},
           {action, "shopcart_form.yaws"}],
          [
           {table, [{bgcolor, linen}, {border, "2"}],
            rows(Sess#sess.items)},

           {input, [{type, submit}, {value, "Update Cart"}]}
          ]
         }
        },
    
    [Css, Head, Top, Cart, bot()].


%% this function gets a list of
%% {JunkString, Num} and displays the current shopcart

rows(Items) ->
    Junk = junk(),
    First = {tr, [],
             [{th, [], pb("Num Items", [])},
              {th, [], pb("Item description", [])},
              {th, [], pb("Price SEK ",[])} 
             ]},
    
    L = lists:map(
          fun({Desc, Price}) ->
                  {tr, [],
                    [{td, [],
                      {input, [{type, text},
                               {width, "4"},
                               {value, jval(Desc, Items)},
                               {name, Desc}]}},
                     {td, [], {p, [], Desc}},
                     {td, [], pb("~w ", [Price])}
                    ]}
          end, Junk),
    
    Total = total(Items, Junk, 0),
    Tax = round(0.27 * Total),
    T = [{tr, [],
          [{td, [], pb("Sum accumulated",[])},
           {td, [{colspan, "2"}], pb("~w SEK", [Total])}
          ]
         },
         {tr, [],
          [
           {td, [], pb("Swedish VAT tax 27 % :-)",[])},
           {td, [{colspan, "2"}], pb("~w SEK", [Tax])}
          ]
         },

         {tr, [],
          [
           {td, [], pb("Total",[])},
           {td, [{colspan, "2"}], pb("~w SEK", [Total  + Tax])}
          ]
         }
        ],
    
    _Rows = [First | L] ++ T.





%% The Items are picked up by the
%% formupdate function and set accordingly in the opaque state
%% this function recalculates the sum total

total([{Str, Num} | Tail], Junk, Ack) ->
    {value, {Str, Price}} = lists:keysearch(Str, 1, Junk),
    total(Tail, Junk, (Num * Price) + Ack);
total([], _,Ack) ->
    Ack.


%% We need to set the value in each input field                      
jval(Str, Items) ->
    case lists:keysearch(Str, 1, Items) of
        {value, {_, Num}} when is_integer(Num) ->
            integer_to_list(Num);
        false ->
            "0"
    end.


%% the store database :-)
%% {Description, Price} tuples
junk() ->
    [{"Toothbrush in rainbow colours", 18},
     {"Twinset of extra soft towels", 66},
     {"Hangover pill - guaranteed to work", 88},
     {"Worlk-out kit that fits under your bed", 1900},
     {"100 pack of headache pills", 7},
     {"Free subscription to MS update packs", 999},
     {"Toilet cleaner", 1111},
     {"Body lotion 4 litres", 888},
     {"Yello, a lifetime supply", 99}].

