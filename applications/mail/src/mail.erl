%    -*- Erlang -*- 
%    File:	mail.erl  (~jb/mail.erl)
%    Author:	Johan Bevemyr
%    Created:	Sat Oct 25 10:59:24 2003
%    Purpose:   

% RFC 822
% RFC 1939
% RFC 2048 

-module('mail').
-author('jb@trut.bluetail.com').

-export([parse_headers/1, list/1, list/3, ploop/5,pop_request/4, diff/2,
	 session_manager_init/0, check_cookie/1, check_session/1, 
	 login/2, display_login/2, stat/3, showmail/2, compose/1, compose/7,
	 send/6, get_val/3, logout/1, base64_2_str/1, retr/4, 
	 delete/2, send_attachment/2]).

-include("../../../include/yaws_api.hrl").
-include("defs.hrl").


-record(info,
	{
	  nr,
	  size,
	  headers
	 }).

-record(mail,
	{
	  from="",
	  to="",
	  cc="",
	  bcc="",
	  subject="",
	  date="",
	  content_type,
	  transfer_encoding,
	  content_disposition,
	  other = []
	 }).

-record(pstate,
	{
	  port,
	  user,
	  pass,
	  cmd,
	  acc = [],
	  from,
	  lines,
	  reply=[],
	  more=true,
	  remain
	 }).

-record(satt, {
	  num,
	  filename,
	  ctype,
	  data}).

-record(session,
	{
	  user,
	  passwd,
	  cookie,
	  attachments = []   %% list of #satt{} records
	 }).

-define(RETRYTIMEOUT, 300).
-define(RETRYCOUNT, 5).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
						%
build_toolbar(Entries) ->
    {table, [{bgcolor,"c0c0c0"},{cellpadding,0},{cellspacing,0},{border,0}],
     [{tr,[],{td, [{colspan,20},{height,1},{bgcolor,white}],
	      {img, [{src,"spacer.gif"}, {width,1},{height,1},
		     {alt,""}, {border,0}],[]}}},
      {tr,[], build_toolbar(Entries, -1)},
      {tr,[],{td, [{colspan,20},{height,1},{bgcolor,gray}],
	      {img, [{src,"spacer.gif"}, {width,1},{height,1},
		     {alt,""}, {border,0}],[]}}},
      {tr,[],{td, [{colspan,20},{height,1}],
	      {img, [{src,"spacer.gif"}, {width,1},{height,1},
		     {alt,""}, {border,0}],[]}}}]}.

build_toolbar([], Used) ->
    Percent = integer_to_list(100-Used)++"%",
    [{td, [nowrap,{width,Percent},{valign,middle},{align,left}],[]}];
build_toolbar([{[],Url,Cmd}|Rest], Used) ->
    if Used == -1 ->
	    [];
       true ->
	    [{td, [nowrap,{width,"1%"},{valign,middle},{align,left}],
	      {img, [{src,"tool-div.gif"},{width,2},{height,16},
		     {alt,""},{border,0},{hspace,2}]}}]
    end ++
	[{td, [nowrap,{width,"2%"},{valign,middle},{align,left}],
	  [{a, [{class,nolink}, {href,Url}],
	    {font, [{size,2},{color,"#000000"},{title,Cmd}],
	     {pre_html,Cmd}}}]} |
	 build_toolbar(Rest, Used+3)];    
build_toolbar([{Gif,Url,Cmd}|Rest], Used) ->
    (if Used == -1 ->
	     [];
	true ->
	     [{td, [nowrap,{width,"1%"},{valign,middle},{align,left}],
	       {img, [{src,"tool-div.gif"},{width,2},{height,16},
		      {alt,""},{border,0},{hspace,2}]}}]
     end ++
     [{td, [nowrap,{width,"2%"},{valign,middle},{align,left}],
       {a, [{class,nolink},
	    {href,Url}],
	[{img, [{src,Gif},{vspace,2},{width,20},
		{height,20},{alt,Cmd},{border,0}],[]}]}

      },
      {td, [nowrap,{width,"2%"},{valign,middle},{align,left}],
       [{a, [{class,nolink},
	     {href,Url}],
	 {font, [{size,2},{color,"#000000"},{title,Cmd}], Cmd}}]} |
      build_toolbar(Rest, Used+4)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
						%
						%

delete(Session, ToDelete) ->
    tick_session(Session#session.cookie),
    Req = [del(M) || M <- ToDelete],
    pop_request(Req++[{"QUIT",sl}], popserver(),
		Session#session.user, Session#session.passwd),
    {redirect_local, {rel_path, "mail.yaws"}}.

send(Session, To, Cc, Bcc, Subject, Msg) ->
    tick_session(Session#session.cookie),
    RTo = parse_addr(To),
    RCc = parse_addr(Cc),
    RBcc = parse_addr(Bcc),
    Recipients = RTo ++ RCc ++ RBcc,
    Date = date_and_time_to_string(yaws:date_and_time()),
    MailDomain = maildomain(),
    Headers =
	[mail_header("To: ", To),
	 mail_header("From: ", Session#session.user++"@"++MailDomain),
	 mail_header("Cc: ", Cc),
	 mail_header("Bcc: ", Bcc),
	 mail_header("Subject: ", Subject),
	 mail_header("Content-Type: ", "text/plain"),
	 mail_header("Content-Transfer-Encoding: ", "8bit")],
    Message = io_lib:format("~sDate: ~s\r\n\r\n~s\r\n.\r\n",
			    [Headers, Date, Msg]),
    case smtp_send(smtpserver(), Session, Recipients, Message) of
	ok ->
	    {redirect_local, {rel_path,"mail.yaws"}};
	{error, Reason} ->
	    (dynamic_headers() ++
	     compose(Session, Reason, To, Cc, Bcc, Subject, Msg))
    end.

mail_header(_Key, []) -> [];
mail_header(Key, Val) -> Key++Val++"\r\n".

compose(Session) ->
    compose(Session, "","","","","","").

compose(Session, Reason, To, Cc, Bcc, Subject, Msg) ->
    tick_session(Session#session.cookie),
    (dynamic_headers()++
     [{ehtml,
       [{script,[{src,"mail.js"}],[]},
	{style, [{type,"text/css"}],
	 "A:link    { color: 0;text-decoration: none}\n"
	 "A:visited { color: 0;text-decoration: none}\n"
	 "A:active  { color: 0;text-decoration: none}\n"
	 "DIV.tag-body { background: white; }\n"},
	{body,[{bgcolor,silver},{marginheight,0},{link,"#000000"},
	       {topmargin,0},{leftmargin,0},{rightmargin,0},
	       {marginwidth,0}, {onload, "document.compose.to.focus();"}],
	 [{form, [{name,compose},{action,"send.yaws"},{method,post}
%		 ,{enctype,"multipart/form-data"}
		 ],
	   [{table, [{border,0},{bgcolor,"c0c0c0"},{cellspacing,0},
		     {width,"100%"}],
	     {tr,[],{td,[{nowrap,true},{align,left},{valign,middle}],
		     {font, [{size,6},{color,black}],
		      "Yaws WebMail at "++maildomain()}}}},
	    build_toolbar([{"tool-send.gif",
			    "javascript:setComposeCmd('send');","Send"},
			   {"", "mail.yaws", "Close"}]),
	    {table, [{width,645},{border,0},{bgcolor,silver},{cellspacing,0},
		     {cellpadding,0}],
	     if
		 Reason == [] -> [];
		 true ->
		     [
		      {tr,[],[{td,[{colspan,2},{height,35},{align,left},
				   {valign,top}],
			       {font,[{color,red},{size,2},nowrap],
				["Error: ",Reason]}}]}
		     ]
	     end ++
	     [{tr,[],[{td,[{height,0},{align,left},{valign,top}],[]},
		      {td,[{height,0},{align,left},{valign,top}],[]}]},
	      {tr,[],[{td,[{height,35},{align,left},{valign,top}],
		       {font,[{color,"#000000"},{size,2},nowrap],
			{pre_html,"&nbsp;To:&nbsp;"}}},
		      {td,[{height,35},{align,left},{valign,top}],
		       {input,[{name,to},{type,text},{size,66},
			       {check,value,quote(To)}]}}]},
	      {tr,[],[{td,[{height,0},{align,left},{valign,top}],[]},
		      {td,[{height,0},{align,left},{valign,top}],[]}]},
	      {tr,[],[{td,[{height,35},{align,left},{valign,top}],
		       {font,[{color,"#000000"},{size,2},nowrap],
			{pre_html,"&nbsp;Cc:&nbsp;"}}},
		      {td,[{height,35},{align,left},{valign,top}],
		       {input,[{name,cc},{type,text},{size,66},
			       {check,value,quote(Cc)}]}}]},
	      {tr,[],[{td,[{height,0},{align,left},{valign,top}],[]},
		      {td,[{height,0},{align,left},{valign,top}],[]}]},
	      {tr,[],[{td,[{height,35},{align,left},{valign,top}],
		       {font,[{color,"#000000"},{size,2},nowrap],
			{pre_html,"&nbsp;Bcc:&nbsp;"}}},
		      {td,[{height,35},{align,left},{valign,top}],
		       {input,[{name,bcc},{type,text},{size,66},
			       {check,value,quote(Bcc)}]}}
		     ]},
	      {tr,[],[{td,[{height,35},{align,left},{valign,top},nowrap],
		       {font,[{color,"#000000"},{size,2}],
			{pre_html,"&nbsp;Subject:&nbsp;"}}},
		      {td,[{colspan,3},{align,left},{valign,top}],
		       {input,[{name,subject},{type,text},{size,66},
			       {check,value,quote(Subject)}]}}]}
	     ]
	    },
	    {table,[{width,645},{border,0},{cellspacing,0},{cellpadding,0}],
	     {tr,[],
	      [
	       build_tabs(["Message","Attachments"]),
	       {'div', [{id, "tab-body:0"},{style,"display: block;"}],
		{table, [{bgcolor,silver},{border,0},{cellspacing,0},
			 {cellpadding,0}],
		 {tr,[],
		  {td,[{align,left},{valign,top}],
		   {textarea, [{wrap,virtual},{name,text},{cols,78},{rows,21}],
		    Msg}}
		 }
		}
	       },
	       {'div', [{id, "tab-body:1"},{style,"display: none;"}],
		["Attached files:",
		 {table,[],
		  file_attachements(5)
		 }
		]
	       }
	      ]
	      }
	     },
	    {input,[{type,hidden},{name,cmd},{value,""}],[]}
	   ]
	  }
	 ]
	}
       ]
      }]).


file_attachements(0) -> [];
file_attachements(N) ->
    [file_attachement(N)|file_attachements(N-1)].

file_attachement(N) ->
    I = integer_to_list(N),
    {tr,[],
     [{td,[],"File: "},
      {td,[],
       {input, [{type,"file"},{name,"file"++I},{size,"30"}],[]}},
      {td,[],
       {select,[{name,"type"++I}],
	[{option,[{value,"binary"}],"Binary"},
	 {option,[{value,"text"}],"Text"}
	]}
      }
     ]
    }.
    

build_tabs(Tabs) ->
    [{'div',
      [{align,"left"}],
      {table,[{border,"0"},
	      {cellspacing,"0"},
	      {cellpadding,"0"}],
       {tr,[],
	build_tab(Tabs,0)}}},
     {'div',[{align,"left"}],
      {table,[{width,645},{border,0},{cellspacing,0},{cellpadding,0}],
       {tr,[],{td,[{height,8},{background,"tab-hr.gif"}],[]}}}}
     ].

build_tab([],_) -> [];
build_tab([T|Ts], N=0) ->
    I = integer_to_list(N),
    [{td,[{width,6}],
      {img,[{src,"tab-left_active.gif"}, {border,0}, {id,"tab-left:"++I}],[]}},
     {td,[{align,"center"},
	  {style,"cursor: pointer; background: url(tab-bg_active.gif)"},
	  {onClick,"changeActiveTab("++I++")"},
	  {id,"tab-bg:"++I}], T},
     {td, [{width,6}],
      {img,[{src,"tab-right_active.gif"}, {border,0}, {id,"tab-right:"++I}],[]}}|
     build_tab(Ts,N+1)];
build_tab([T|Ts], N) ->
    I = integer_to_list(N),
    [{td,[{width,6}],
      {img,[{src,"tab-left_inactive.gif"}, {border,0}, {id,"tab-left:"++I}],[]}},
     {td,[{align,"center"},
	  {style,"cursor: pointer; background: url(tab-bg_inactive.gif)"},
	  {onClick,"changeActiveTab("++I++")"},
	  {id,"tab-bg:"++I}], T},
     {td, [{width,6}],
      {img,[{src,"tab-right_inactive.gif"}, {border,0}, {id,"tab-right:"++I}],[]}}|
     build_tab(Ts,N+1)].

showmail(Session, MailNr) ->
    showmail(Session, MailNr, ?RETRYCOUNT).

showmail(Session, MailNr, 0) ->
    format_error("Mailbox locked by other mail session.");
showmail(Session, MailNr, Count) ->
    MailStr = integer_to_list(MailNr),
    tick_session(Session#session.cookie),

    Formated = 
	case retr(popserver(), Session#session.user,
		  Session#session.passwd, MailNr) of
	    {error, Reason} ->
		case string:str(lowercase(Reason), "lock") of
		    0 ->
			format_error(Reason);
		    N ->
			sleep(?RETRYTIMEOUT),
			showmail(Session, MailNr, Count-1)
		end;
	    Message ->
		format_message(Session, Message, MailNr, "1")
	end,

    (dynamic_headers() ++
     [{ehtml,
       [{script,[{src,"mail.js"}], []},
	{style, [{type,"text/css"}],
	 ".conts    { visibility:hidden }\n"
	 "A:link    { color: 0;text-decoration: none}\n"
	 "A:visited { color: 0;text-decoration: none}\n"
	 "A:active  { color: 0;text-decoration: none}\n"
	 "DIV.msg-body { background: white; }\n"
	},
	{body,[{bgcolor,silver},{marginheight,0},{topmargin,0},{leftmargin,0},
	       {rightmargin,0},{marginwidth,0}],
	 [{table, [{border,0},{bgcolor,"c0c0c0"},{cellspacing,0},
		  {width,"100%"}],
	  {tr,[],{td,[{nowrap,true},{align,left},{valign,middle}],
		  {font, [{size,6},{color,black}],
		   "WebMail at "++maildomain()}}}}] ++
      	 Formated
	}
       ]}]).

list(Session) ->
    list(Session, ?RETRYCOUNT).

list(Session, 0) ->
    format_error("Mailbox locked by other mail process.");
list(Session, Count) ->
    tick_session(Session#session.cookie),
    case list(popserver(), Session#session.user, Session#session.passwd) of
	{error, Reason} ->
	    case string:str(lowercase(Reason), "lock") of
		0 ->
		    format_error(Reason);
		N ->
		    sleep(?RETRYTIMEOUT),
		    list(Session, Count-1)
	    end;
	H ->
	    (dynamic_headers()++
	     [{ehtml,
	       [{script,[],
		 "function setCmd(val) { \n"
		 "   if (val == 'delete') {\n"
		 "      var res = confirm('Are you sure you want"
		 " to delete the selected emails?');\n" 
		 "      if (res) { \n"
		 "           document.list.cmd.value=val;\n"
		 "           document.list.submit();\n"
		 "      } else { \n"
		 "           return;\n"
		 "      }\n"
		 "   }\n"
		 "   document.list.cmd.value=val;\n"
		 "   document.list.submit();\n"
		 "}"
		},
		{style,[{type,"text/css"}],
		 "A:link    { color: black; text-decoration: none}\n"
		 "A:visited { color: black; text-decoration: none}\n"
		 "A:active  { color: black; text-decoration: none}\n"
		 ".AList    { color: black; text-decoration: none}\n"
		 ".Head     { border-right:1px solid white}"},
		{form, [{name,list},{action,"listop.yaws"},{method,post}],
		 [{table, [{border,0},{bgcolor,"c0c0c0"},
			   {cellspacing,0},{width,"100%"}],
		   {tr,[],{td,[{nowrap,true},{align,left},{valign,middle}],
			   {font, [{size,6},{color,black}],
			    "WebMail at "++maildomain()}}}},
		  build_toolbar([{"tool-newmail.gif","compose.yaws",
				  "New Message"},
				 {"tool-delete.gif",
				  "javascript:setCmd('delete')",
				  "Delete"},
				 {"","logout.yaws","Logout"}]),
		  {table, [{border,0},{bgcolor,"666666"},{cellspacing,0},
			   {width,"100%"}],
		   {tr,[],{td,[{nowrap,true},{align,left},{valign,middle}],
			   {font, [{size,2},{color,"#ffffff"}],
			    "Inbox for "++Session#session.user}}}},
		  {table, [{border,0},{cellspacing,0},{cellpadding,1},
			   {width,"100%"}],
		   [{tr, [{bgcolor,"c0c0c0"},{valign,middle}],
		     [{th,[{align,left},{valign,middle},{class,head}],
		       {font,[{size,2},{color,black}],"Nr"}},
		      {th,[{class,head}],
		       {img,[{src,"view-mark.gif"},{width,13},
			     {height,13}],[]}},
		      {th,[{align,left},{valign,middle},{class,head}],
		       {font,[{size,2},{color,black}],"From"}},
		      {th,[{align,left},{valign,middle},{class,head}],
		       {font,[{size,2},{color,black}],"Subject"}},
		      {th,[{align,left},{valign,middle},{class,head}],
		       {font,[{size,2},{color,black}],"Date"}},
		      {th,[{align,left},{valign,middle},{class,head}],
		       {font,[{size,2},{color,black}],"Size"}}]}] ++
		   format_summary(H)},
		  {input,[{type,hidden},{name,cmd},{value,""}],[]}
		 ]}]}])
    end.

format_summary(Hs) ->
    [format_summary_line(H) || H <- Hs].

format_summary_line(I) ->
    H = I#info.headers,
    {tr, [{align,center},{valign,top}],
     [{td, [{nowrap,true},{align,left},{valign,top},{class,"List"}],
       {a, [{href,"showmail.yaws?nr="++integer_to_list(I#info.nr)}],
	{font,[{size,2},{color,black}],{b,[],integer_to_list(I#info.nr)}}}},
      {td, [{nowrap,true},{align,center},{valign,top},{class,"List"}],
       {input, [{type,checkbox},{name,I#info.nr},{value,yes}],[]}},
      {td, [{nowrap,true},{align,left},{valign,top},{class,"List"}],
       {a, [{href,"showmail.yaws?nr="++integer_to_list(I#info.nr)}],
	{font,[{size,2},{color,black}],{b,[],format_from(H#mail.from)}}}},
      {td, [{nowrap,true},{align,left},{valign,top},{class,"List"}],
       {a, [{href,"showmail.yaws?nr="++integer_to_list(I#info.nr)}],
	{font,[{size,2},{color,black}],{b,[],decode(H#mail.subject)}}}},
      {td, [{nowrap,true},{align,left},{valign,top},{class,"List"}],
       {a, [{href,"showmail.yaws?nr="++integer_to_list(I#info.nr)}],
	{font,[{size,2},{color,black}],
	 {b,[],format_date(parse_date(H#mail.date))}}}},
      {td, [{nowrap,true},{align,left},{valign,top},{class,"List"}],
       {a, [{href,"showmail.yaws?nr="++integer_to_list(I#info.nr)}],
	{font,[{size,2},{color,black}],{b,[],integer_to_list(I#info.size)}}}}
     ]}.

format_from(From) ->
    case string:chr(From,$<) of
	0 ->
	    From;
	N ->
	    NewF=string:strip(unquote(decode(string:substr(From,1,N-1)))),
	    if 
		NewF == [] -> From;
		true -> NewF
	    end
    end.

parse_addr(AddrStr) ->
    Addrs = token_addrs(AddrStr, [], false),
    Op =
	fun(From) ->
		case {string:chr(From,$<),string:chr(From,$>)} of
		    {S,E} when S>0, E>0 ->
			string:substr(From,S,(E-S)+1);
		    _ ->
			string:strip(From)
		end
	end,
    Fs = [Op(F) || F <- Addrs].

format_addr(Addr) ->
    format_list(Addr).

token_addrs([], [], _) ->
    [];
token_addrs([], Acc, _) ->
    [lists:reverse(Acc)];
token_addrs([C=$"|R], Acc, true) ->
    token_addrs(R, [C|Acc], false);
token_addrs([C=$"|R], Acc, false) ->
    token_addrs(R, [C|Acc], true);
token_addrs([C=$,|R], Acc, false) ->
    [lists:reverse(Acc)|token_addrs(R, [], false)];
token_addrs([C|R], Acc, InQuote) ->
    token_addrs(R, [C|Acc], InQuote).

format_list([]) -> [];
format_list([E]) -> E;
format_list([E|Es]) ->
    E++","++format_list(Es).

decode([]) -> [];
decode([$=,$?|Rest]) ->
    decode_scan(Rest);
decode([C|Cs]) ->
    [C | decode(Cs)].

decode_scan([]) -> [];
decode_scan([$?,$b,$?|Rest]) ->
    decode_b64(Rest,[]);
decode_scan([$?,$B,$?|Rest]) ->
    decode_b64(Rest,[]);
decode_scan([$?,$q,$?|Rest]) ->
    decode_q(Rest,[]);
decode_scan([$?,$Q,$?|Rest]) ->
    decode_q(Rest,[]);
decode_scan([$?,_,$?|Rest]) ->
    decode(Rest);
decode_scan([_|Rest]) ->
    decode_scan(Rest).

decode_q([], Acc) ->
    lists:revers(Acc);
decode_q([$?,$=|Rest], Acc) ->
    [lists:reverse(Acc)|decode(Rest)];
decode_q([$=,H1,H2|Rest], Acc) ->
    decode_q(Rest, [yaws:hex_to_integer([H1,H2])|Acc]);
decode_q([C|Cs], Acc) ->
    decode_q(Cs, [C|Acc]).

decode_b64([],Acc) ->
    Str = lists:reverse(Acc),
    case catch base64_2_str(Str) of
	{'EXIT',_} -> Str;
	Dec -> Dec
    end;
decode_b64([$?,$=|Rest],Acc) ->
    Str = lists:reverse(Acc),
    case catch base64_2_str(Str) of
	{'EXIT',_} -> [Str|decode(Rest)];
	Dec -> [Dec|decode(Rest)]
    end;
decode_b64([C|Rest], Acc) ->
    decode_b64(Rest,[C|Acc]).

unquote([]) -> [];
unquote([$"|R]) -> unquote(R);
unquote([C|R]) -> [C|unquote(R)].

quote([]) ->
    [];
quote([$"|Cs]) ->
    ["&quot;"|quote(Cs)];
quote([C|Cs]) ->
    [C|quote(Cs)].

display_login(A, Status) ->
    (dynamic_headers() ++
     [{ehtml,
       [{body, [{onload,"document.f.user.focus();"}],
	 [{table, [{border,0},{bgcolor,"c0c0c0"},{cellspacing,0},
		   {width,"100%"}],
	   {tr,[],{td,[{nowrap,true},{align,left},{valign,middle}],
		   {font, [{size,6},{color,black}],
		    "WebMail at "++maildomain()}}}},
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

logout(Session) ->
    logout_cookie(Session#session.cookie),
    (dynamic_headers() ++
     [{redirect_local, {rel_path,"mail.yaws"}}]).

login(User, Password) ->
    case stat(popserver(), strip(User), strip(Password)) of
	{ok, _} ->
	    {ok, new_session(User, Password)};
	{error, Reason} ->
	    {error, Reason}
    end.

check_session(A) ->
    H = A#arg.headers,
    case yaws_api:find_cookie_val("mailsession", H#headers.cookie) of
	[] ->
	    display_login(A, "not logged in");
	CVal ->
	    case mail:check_cookie(CVal) of
		error ->
		    display_login(A, "not logged in");
		Session ->
		    {ok, Session}
	    end
    end.

strip(Str) ->
    lists:filter(fun(C)->not(lists:member(C,"\r\n"))end,Str).

dynamic_headers() ->
    [yaws_api:set_content_type("text/html"),
     {header, {cache_control, "no-store"}},
     {header, "Expires: -1"}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% session server
%%

tick_session(Cookie) ->
    session_server(),
    mail_session_manager ! {tick_session, Cookie}.

new_session(User, Password) ->
    session_server(),
    mail_session_manager !
	{new_session, #session{user=User,passwd=Password}, self()},
    receive
	{session_manager, Cookie} ->
	    Cookie
    end.

check_cookie(Cookie) ->
    session_server(),
    mail_session_manager ! {get_session, Cookie, self()},
    receive
	{session_manager, {ok, Session}} ->
	    Session;
	{session_manager, error} ->
	    error
    end.

logout_cookie(Cookie) ->
    session_server(),
    mail_session_manager ! {del_session, Cookie}.

session_server() ->
    case whereis(mail_session_manager) of
	undefined ->
	    Pid = proc_lib:spawn(?MODULE, session_manager_init, []),
	    register(mail_session_manager, Pid);
	_ ->
	    done
    end.

session_manager_init() ->
    {X,Y,Z} = seed(),
    random:seed(X, Y, Z),
    session_manager([], now(), read_config()).

session_manager(C0, LastGC0, Cfg) ->
    %% Check GC first to avoid GC starvation.
    GCDiff = diff(LastGC0,now()),
    {LastGC, C} =
	if GCDiff > 5000 ->
		C2 = session_manager_gc(C0, Cfg),
		{now(), C2};
	   true ->
		{LastGC0, C0}
	end,

    receive
	{get_session, Cookie, From} ->
	    case lists:keysearch(Cookie, 1, C) of
		{value, {_,Session,_}} ->
		    From ! {session_manager, {ok, Session}};
		false ->
		    From ! {session_manager, error}
	    end,
	    session_manager(C, LastGC, Cfg);
	{new_session, Session, From} ->
	    Cookie = integer_to_list(random:uniform(1 bsl 50)),
	    From ! {session_manager, Cookie},
	    session_manager([{Cookie, Session#session{cookie=Cookie},
			      now()}|C], LastGC, Cfg);
	{tick_session, Cookie} ->
	    case lists:keysearch(Cookie, 1, C) of
		{value, {Cookie,Session,_}} ->
		    session_manager(
		      lists:keyreplace(Cookie,1,C,
				       {Cookie,Session,now()}), LastGC, Cfg);
		false ->
		    session_manager(C, LastGC, Cfg)
	    end;
	{del_session, Cookie} ->
	    C3 = lists:keydelete(Cookie, 1, C),
	    session_manager(C3, LastGC, Cfg);
	{From, cfg , Req} ->
	    sm_reply(Req, From, Cfg),
	    session_manager(C, LastGC, Cfg);
	{session_set_attach_data, From, Cookie, Fname, Ctype, Data} ->
	    case lists:keysearch(Cookie, 1, C) of
		{value, {_,Session,_}} ->
		    Atts = Session#session.attachments,
		    [A|As] = add_att(Fname, Ctype, Data, Atts),
		    From ! {session_manager, A#satt.num},
		    S2 = Session#session{attachments = [A|As]},
		    session_manager(lists:keyreplace(
				      Cookie,1,C,
				      {Cookie,S2,now()}), LastGC, Cfg);
		false ->
		    session_manager(C, LastGC, Cfg)
	    end;
	{session_get_attach_data, From, Cookie, Num} ->
	    case lists:keysearch(Cookie, 1, C) of
		{value, {_,Session,_}} ->
		    Atts = Session#session.attachments,
		    case lists:keysearch(Num, #satt.num, Atts) of
			false ->
			    From ! {session_manager, error};
			{value, A} ->
			    From ! {session_manager, A}
		    end;
		false ->
		    ignore
	    end,
	    session_manager(C, LastGC, Cfg)
    after
	5000 ->
	    %% garbage collect sessions
	    C3 = session_manager_gc(C, Cfg),
	    session_manager(C3, now(), Cfg)
    end.


add_att(Fname, Ctype, Data, Atts) ->
    case lists:keysearch(Fname, #satt.filename, Atts) of
	false ->
	    [#satt{num = length(Atts) + 1,
		   filename = Fname,
		   ctype = Ctype,
		   data = Data} | Atts];

	{value, A} when A#satt.data == Data ->
	    [A | lists:keydelete(A#satt.num, #satt.num, Atts)];
	{value, A} ->
	    [#satt{num = length(Atts) + 1,
		   filename = Fname,
		   ctype = Ctype,
		   data = Data} | Atts]
    end.
		



session_manager_gc(C, Cfg) ->
    lists:zf(fun(Entry={Cookie,Session,Time}) ->
		     Diff = diff(Time,now()),
		     TTL = Cfg#cfg.ttl,
		     if Diff > TTL ->
			     false;
			true ->
			     {true, Entry}
		     end
	     end, C).    

sm_reply(ttl, From, Cfg) -> 
    From ! {session_manager, Cfg#cfg.ttl};
sm_reply(popserver, From, Cfg) ->
    From ! {session_manager, Cfg#cfg.popserver};
sm_reply(smtpserver, From, Cfg) ->
    From ! {session_manager, Cfg#cfg.smtpserver};
sm_reply(maildomain, From, Cfg) ->
    From ! {session_manager, Cfg#cfg.maildomain};
sm_reply(sendtimeout, From, Cfg) ->
    From ! {session_manager, Cfg#cfg.sendtimeout}.


req(Req) ->
    session_server(),
    mail_session_manager ! {self(), cfg, Req},
    receive {session_manager, Reply} ->
	    Reply
    after 10000 ->
	    exit("No reply from session manager")
    end.

ttl() ->         req(ttl).
popserver() ->   req(popserver).
smtpserver() ->  req(smtpserver).
maildomain() ->  req(maildomain).
sendtimeout() -> req(sendtimeout).


    
    
diff({M1,S1,_}, {M2,S2,_}) ->
    (M2-M1)*1000000+(S2-S1).

seed() ->
    case (catch list_to_binary(
		  os:cmd("dd if=/dev/urandom ibs=12 count=1 2>/dev/null"))) of
	<<X:32, Y:32, Z:32>> ->
	    {X, Y, Z};
	_ ->
	    now()
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

retr(Server, User, Password, Nr) ->
    Req = [ret(Nr)],
    case pop_request(Req, Server, User, Password) of
	[{ok,Msg}] ->
	    Msg;
	[{error, Reason}] ->
	    {error, Reason}
    end.

parse_message(Msg) ->
    split_head_body(Msg, []).

split_head_body(Msg, Acc) ->
    case get_next_line(Msg) of
	{error, Reason} ->
	    {error, Reason};
	{[], Rest} ->
	    {lists:reverse(Acc), Rest};
	{Line, Rest} ->
	    split_head_body(Rest, [Line|Acc])
    end.

get_next_line(Data) ->
    %% io:format("Data = ~p\n", [Data]),
    get_next_line(Data,[]).

get_next_line([D|Ds], Acc) ->
    case split_reply(D,[]) of
	more ->
	    get_next_line(Ds, [D|Acc]);
	{Pre, Rest} when Acc==[] ->
	    {Pre, [Rest|Ds]};
	{Pre, Rest} ->
	    {lists:flatten(lists:reverse([Pre|Acc])), [Rest|Ds]}
    end.

stat(Server, User, Password) ->
    case pop_request([{"STAT",sl}], Server, User, Password) of
	[{ok, Stat}] ->
	    {ok, Stat};
	{error, Reason} ->
	    {error, Reason}
    end.


rtop(Server, User, Password, Nr) ->
    Req = [top(Nr)],
    [{ok,Msg}] = pop_request(Req, Server, User, Password),
    {parse_headers(Msg), [M++"\n" || M <- Msg]}.

list(Server, User, Password) ->
    case pop_request([{"LIST",ml}], Server, User, Password) of
	[{ok, Stats}] ->
	    Info = lists:reverse([info(S) || S <- Stats]),
	    Req = [top(I#info.nr) || I <- Info],
	    Res = pop_request(Req, Server, User, Password),
	    Hdrs = lists:map(fun({ok,Ls}) -> parse_headers(Ls) end, Res),
	    add_hdrs(Info,Hdrs);
	{error, Reason} ->
	    {error, Reason}
    end.

list(Server, User, Password, From, To) ->
    case pop_request([{"LIST",ml}], Server, User, Password) of
	[{ok, Stats}] ->
	    SortStats = lists:sublist(lists:reverse(Stats), From, To),
	    Info = [info(S) || S <- SortStats],
	    Req = [top(I#info.nr) || I <- Info],
	    Res = pop_request(Req, Server, User, Password),
	    Hdrs = lists:map(fun({ok,Ls}) -> parse_headers(Ls) end, Res),
	    add_hdrs(Info,Hdrs);
	{error, Reason} ->
	    {error, Reason}
    end.

add_hdrs([], []) -> [];
add_hdrs([I|Is], [H|Hs]) ->
    [I#info{headers=H}|add_hdrs(Is,Hs)].

info(Str) ->
    [NrStr,SizeStr|_] = string:tokens(Str, " \t"),
    #info{nr=to_int(NrStr),size=to_int(SizeStr)}.

top(I) -> {"TOP "++integer_to_list(I)++" 0", ml}.
ret(I) -> {"RETR "++integer_to_list(I), sized}.

del(I) -> {"DELE "++atom_to_list(I), sl}.


to_int(Str) ->
    to_int(Str, 0).

to_int([D|Ds], Acc) when D >= $0, D =< $9->
    to_int(Ds, Acc*10+D-$0);
to_int(_, Acc) -> Acc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_headers(Lines) ->
    parse_headers(Lines, #mail{}).

parse_headers([], Headers) ->
    Headers;
parse_headers([L1,[$\t|L2]|Lines], Headers) ->
    parse_headers([L1++" "++L2|Lines], Headers);
parse_headers([L1,[$ |L2]|Lines], Headers) ->
    parse_headers([L1++" "++L2|Lines], Headers);
parse_headers([Line|Lines], Headers) ->
    case string:chr(Line, $:) of
	0 ->
	    Headers;
	N ->
	    Key = lowercase(string:strip(string:sub_string(Line, 1, N-1))),
	    Value = string:sub_string(Line, N+2),
	    NewH = add_header(Key, Value, Headers),
	    parse_headers(Lines, NewH)
    end.

parse_header_value(Header) ->
    [Key|Options] = string:tokens(Header, ";"),
    Opts = [parse_key_value(O) || O <- Options],
    {Key,Opts}.


parse_key_value(O) ->
    parse_key_value(O, []).

parse_key_value([], Acc) ->
    {lists:reverse(Acc), []};
parse_key_value([$=|Rest], Acc) ->
    Value = unquote(string:strip(Rest)),
    Key = string:strip(lists:reverse(Acc)),
    {Key, Value};
parse_key_value([C|Cs], Acc) ->
    parse_key_value(Cs, [C|Acc]).


lowercase(Str) ->
    [lowercase_ch(S) || S <- Str].


lowercase_ch(C) when C>=$A, C=<$Z -> C + 32;
lowercase_ch(C) -> C.

add_header("content-transfer-encoding", Value, H) ->
    H#mail{transfer_encoding = lowercase(Value)};
add_header("content-type", Value, H) ->
    H#mail{content_type = parse_header_value(Value)};
add_header("content-disposition", Value, H) ->
    H#mail{content_disposition = parse_header_value(Value)};
add_header("from", Value, H) ->    H#mail{from = Value};
add_header("to", Value, H) ->      H#mail{to = Value};
add_header("cc", Value, H) ->      H#mail{cc = Value};
add_header("bcc", Value, H) ->     H#mail{bcc = Value};
add_header("subject", Value, H) -> H#mail{subject = Value};
add_header("date", Value, H) ->    H#mail{date = Value};
add_header(Other, Value, H) ->     H#mail{other = [{Other,Value}|
						   H#mail.other]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pop_request(Command, Server, User, Password) ->
    proc_lib:spawn_link(?MODULE, ploop,
			[Command, Server, User, Password, self()]),
    receive
	{pop_response, Response} ->
	    Response
    end.

%%
%% first authenticate then run a bunch of commands
%% 

ploop(Command, Server, User, Password, From) ->
    case gen_tcp:connect(Server, 110, [{active, false},
				       {reuseaddr,true},
				       binary]) of
	{ok, Port} ->
	    State = #pstate{port=Port,
			    user=User,
			    pass=Password,
			    cmd=Command,
			    from=From},
	    ploop(init, State);
	_ ->
	    {error, "Failed to contact mail server."}
    end.

						%



ploop(init, State) ->
    case receive_reply(State) of
	{ok, Reply, State2} ->
	    psend("USER " ++ State#pstate.user, State#pstate.port),
	    ploop(user, State2);
	{error, Reason, State2} ->
	    State#pstate.from ! {pop_response, {error, Reason}},
	    gen_tcp:close(State#pstate.port);
	{more, State2} ->
	    ploop(init, State2)
    end;

ploop(user, State) ->
    case receive_reply(State) of
	{ok, Reply, State2} ->
	    psend("PASS " ++ State#pstate.pass, State#pstate.port),
	    ploop(pass, State2);
	{error, Reason, State2} ->
	    State#pstate.from ! {pop_response, {error, Reason}},
	    gen_tcp:close(State#pstate.port);
	{more, State2} ->
	    ploop(user, State2)
    end;
ploop(pass, State) ->
    case receive_reply(State) of
	{ok, Reply, State2} ->
	    next_cmd(State);
	{error, Reason, State2} ->
	    State#pstate.from ! {pop_response, {error, Reason}},
	    gen_tcp:close(State#pstate.port);
	{more, State2} ->
	    ploop(pass, State2)
    end;
ploop(sl, State) ->
    case receive_reply(State) of
	{ok, Reply, State2} ->
	    next_cmd(State2#pstate{reply=[{ok,Reply}|State2#pstate.reply]});
	{error, Reason, State2} ->
	    next_cmd(State2#pstate{reply=[{error,Reason}|
					  State2#pstate.reply]});
	{more, State2} ->
	    ploop(sl, State2)
    end;
ploop(sized, State) ->
    case receive_reply(State) of
	{ok, Reply, State2} ->
	    Size = to_int(Reply),
	    ploop(sized_cont, State2#pstate{remain=Size,lines=[]});
	{error, Reason, State2} ->
	    next_cmd(State2#pstate{reply=[{error,Reason}|
					  State2#pstate.reply]});
	{more, State2} ->
	    ploop(ml, State2)
    end;
ploop(sized_cont, State) ->
    case receive_data(State) of
	{error, Reason, State2} ->
	    next_cmd(State2#pstate{reply=[{error,Reason}|
					  State2#pstate.reply]});
	{more, State2} ->
	    ploop(sized_cont, State2);
	{done, State2} ->
	    Data = lists:reverse(State2#pstate.lines),
	    next_cmd(State2#pstate{reply=[{ok, Data}|State2#pstate.reply]})
    end;
ploop(ml, State) ->
    case receive_reply(State) of
	{ok, Reply, State2} ->
	    ploop(ml_cont, State2#pstate{lines=[]});
	{error, Reason, State2} ->
	    next_cmd(State2#pstate{reply=[{error,Reason}|
					  State2#pstate.reply]});
	{more, State2} ->
	    ploop(ml, State2)
    end;
ploop(ml_cont, State) ->
    case receive_reply(State) of
	{line, Line, State2} ->
	    Lines = State2#pstate.lines,
	    ploop(ml_cont, State2#pstate{lines=[Line|Lines]});
	{error, Reason, State2} ->
	    next_cmd(State2#pstate{reply=[{error,Reason}|
					  State2#pstate.reply]});
	{more, State2} ->
	    ploop(ml_cont, State2);
	{done, State2} ->
	    Lines = lists:reverse(State2#pstate.lines),
	    next_cmd(State2#pstate{reply=[{ok, Lines}|State2#pstate.reply]})
    end.

%%

next_cmd(State=#pstate{cmd=Cmd,reply=Reply}) when Cmd==[]->
    State#pstate.from ! {pop_response, lists:reverse(Reply)},
    gen_tcp:close(State#pstate.port);
next_cmd(State=#pstate{cmd=[Cmd|Cmds]}) ->
    {C,S} = Cmd,
    psend(C, State#pstate.port),
    ploop(S, State#pstate{cmd=Cmds}).

%%

psend(Str, Port) ->
    gen_tcp:send(Port, Str++"\r\n").

%%

receive_reply(State=#pstate{port=Port,acc=Acc,more=false}) ->
    check_reply(State#pstate.acc, State);
receive_reply(State=#pstate{port=Port,acc=Acc,more=true}) ->
    Res = gen_tcp:recv(Port, 0),
    case Res of
	{ok, Bin} ->
	    NAcc = Acc++binary_to_list(Bin),
	    check_reply(NAcc, State);
	Err ->
	    {error, Err, State}
    end.


%%

receive_data(State=#pstate{port=Port,acc=Acc,more=false,remain=Remain}) ->
    if
	Remain =< length(Acc) ->
	    {Lines, NAcc} = split_at(Acc, Remain),
	    State2 = State#pstate{acc=NAcc,lines=[Lines|State#pstate.lines],
				  remain=0,more=false},
	    {done, State2};
	true ->
	    Rem = Remain - length(Acc),
	    State2 = State#pstate{acc=[],lines=[Acc|State#pstate.lines],
				  remain=Rem, more=true},
	    {more, State2}
    end;
receive_data(State=#pstate{port=Port,acc=Acc,more=true}) when length(Acc)>0 ->
    receive_data(State#pstate{more=false});
receive_data(State=#pstate{port=Port,acc=[],more=true,remain=Remain}) ->
    Res = gen_tcp:recv(Port, 0),
    case Res of
	{ok, Bin} ->
	    Acc = binary_to_list(Bin),
	    if
		Remain =< length(Acc) ->
		    {Lines, NAcc} = split_at(Acc, Remain),
		    State2 = State#pstate{acc=NAcc,
					  lines=[Lines|State#pstate.lines],
					  remain=0,more=false},
		    {done, State2};
		true ->
		    Rem = Remain - length(Acc),
		    State2 = State#pstate{acc=[],
					  lines=[Acc|State#pstate.lines],
					  remain=Rem, more=true},
		    {more, State2}
	    end;
	Err ->
	    {error, Err, State}
    end.

						%

check_reply(Str, State) ->
    case split_reply(Str, []) of
	{"+OK " ++ Res, Rest} ->
	    NewS = State#pstate{acc=Rest,more=false},
	    {ok, Res, NewS};
	{"-ERR " ++ Res, Rest} ->
	    NewS = State#pstate{acc=Rest,more=false},
	    {error, Res, NewS};
	{".", Rest} ->
	    NewS = State#pstate{acc=Rest,more=false},
	    {done, NewS};
	{"."++Line, Rest} ->
	    NewS = State#pstate{acc=Rest,more=false},
	    {line, Line, NewS};
	{Line, Rest} ->
	    NewS = State#pstate{acc=Rest,more=false},
	    {line, Line, NewS};
	more ->
	    {more, State#pstate{acc=Str, more=true}}
    end.

						%

split_reply("\r\n"++Rest, Pre) ->
    {lists:reverse(Pre), Rest};
split_reply([H|T], Pre) ->
    split_reply(T, [H|Pre]);
split_reply("", Pre) ->
    more.

						%

split_at(L,N) ->
    split_at(L,N,[]).

split_at(L,0,Acc) ->
    {lists:reverse(Acc),L};
split_at([C|Cs], N, Acc) ->    
    split_at(Cs, N-1, [C|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_val(Key, L, Default) ->
    case lists:keysearch(Key, 1, L) of
	{value, {_, undefined}} -> Default;
	{value, {_, Val}} -> Val;
	_ -> Default
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

smtp_send(Server, Session, Recipients, Message) ->
    case catch smtp_send2(Server, Session, Recipients, Message) of
	ok ->
	    ok;
	{error, Reason} ->
	    {error, Reason};
	_ ->
	    {error, "Failed to send message."}
    end.

smtp_send2(Server, Session, Recipients, Message) ->
    {ok, Port} = gen_tcp:connect(Server, 25, [{active, false},
					      {reuseaddr,true},
					      binary]),
    smtp_expect(220, Port, "SMTP server does not respond"),
    smtp_put("MAIL FROM: " ++ Session#session.user++"@"++maildomain(), Port),
    smtp_expect(250, Port, "Sender not accepted by mail server"),
    send_recipients(Recipients,Port),
    smtp_put("DATA", Port),
    smtp_expect(354, Port, "Message not accepted by mail server."),
    smtp_put(Message, Port),
    smtp_put(".", Port),
    smtp_expect(250, Port, "Message not accepted by mail server."),
    smtp_put("QUIT", Port),
    ok.

send_recipients([], Port) ->
    ok;
send_recipients([R|Rs], Port) ->
    smtp_put("RCPT TO: " ++ R, Port),
    smtp_expect(250, Port, io_lib:format("Recipient ~s not accepted.",[R])),
    send_recipients(Rs, Port).

smtp_put(Message, Port) ->
    gen_tcp:send(Port, [Message,"\r\n"]).

smtp_expect(Code, Port, ErrorMsg) ->
    smtp_expect(Code, Port, [], ErrorMsg).

smtp_expect(Code, Port, Acc, ErrorMsg) ->
    Res = gen_tcp:recv(Port, 0, sendtimeout()),
    case Res of
	{ok, Bin} ->
	    NAcc = Acc++binary_to_list(Bin),
	    case string:chr(NAcc, $\n) of
		0 ->
		    smtp_expect(Code, Port, NAcc, ErrorMsg);
		N ->
		    ResponseCode = to_int(NAcc),
		    if 
			ResponseCode == Code -> ok;
			true -> throw({error, ErrorMsg})
		    end
	    end;
	Err ->
	    throw({error, Err})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

base64_2_str(Str) ->
    b642str(Str, 0, 0, []).

b642str([$=|_], Acc, N, Out) ->
    case N of
	2 ->
	    %% If I have seen two characters before the =
	    %% Them I'm encoding one byte
	    lists:reverse([(Acc bsr 4)|Out]);
	3 ->
	    %% If I have seen three characters before the =
	    %% Them I'm encoding two bytes
	    B1 = Acc bsr 10,
	    B2 = (Acc bsr 2) band 16#ff,
	    lists:reverse([B2,B1|Out]);
	_ ->
	    exit({bad,b64,N})
    end;
b642str([H|T], Acc, N, Out) ->
    case d(H) of
	no ->
	    b642str(T, Acc, N, Out);
	I  -> 
	    Acc1 = (Acc bsl 6) bor I,
	    case N of 
		3 ->
		    B1 = Acc1 bsr 16,
		    B2 = (Acc1 band 16#ffff) bsr 8,
		    B3 = (Acc1 band 16#ff),
		    b642str(T, 0, 0, [B3,B2,B1|Out]);
		_ ->
		    b642str(T, Acc1, N+1, Out)
	    end
    end;
b642str([], 0, 0, Out) ->
    lists:reverse(Out).

d(X) when X >= $A, X =<$Z ->  X - $A;
d(X) when X >= $a, X =<$z ->  X - $a + 26;
d(X) when X >= $0, X =<$9 ->  X - $0 + 52;
d($+)                     -> 62;
d($/)                     -> 63;
d(_)                      -> no.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

date_and_time_to_string(DAT) ->
    case validate_date_and_time(DAT) of
	true ->
	    dat2str(DAT);
	false ->
	    exit({badarg, {?MODULE, date_and_time_to_string, [DAT]}})
    end.

dat2str([Y1,Y2, Mo, D, H, M, S | Diff]) ->
    lists:flatten(
      io_lib:format("~s, ~2.2.0w ~s ~w ~2.2.0w:~2.2.0w:~2.2.0w",
		    [weekday(Y1,Y2,Mo,D), D, int_to_mt(Mo),
		     y(Y1,Y2),H,M,S]) ++
      case Diff of
	  [Sign,Hd,Md] ->
	      io_lib:format("~c~2.2.0w~2.2.0w",
			    [Sign,Hd,Md]);
	  _ -> []
      end).

y(Y1, Y2) -> 256 * Y1 + Y2.

weekday(Y1,Y2,Mo,D) ->
    int_to_wd(calendar:day_of_the_week(Y1*256+Y2,Mo,D)).

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

validate_date_and_time([Y1,Y2, Mo, D, H, M, S | Diff]) 
  when 0 =< Y1, 0 =< Y2, 0 < Mo, Mo < 13, 0 < D, D < 32, 0 =< H,
       H < 24, 0 =< M, M < 60, 0 =< S, S < 61  ->
    case check_diff(Diff) of
	true ->
	    calendar:valid_date(y(Y1,Y2), Mo, D);
	false ->
	    false
    end;
validate_date_and_time(_) -> false.

check_diff([]) -> true;
check_diff([$+, H, M]) when 0 =< H, H < 12, 0 =< M, M < 60 -> true;
check_diff([$-, H, M]) when 0 =< H, H < 12, 0 =< M, M < 60 -> true;
check_diff(_) -> false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_error(Reason) ->
    [build_toolbar([{"","mail.yaws","Close"}]),
     {p, [], {font, [{size,4},{color,red}],["Error: ", Reason]}}].

format_message(Session, Message, MailNr, Depth) ->
    {HeadersList,Msg} = parse_message(Message),
    H = parse_headers(HeadersList),
    Headers = [[Head,$\n] || Head <- HeadersList],
    Formated = format_body(Session, H, Msg, Depth),
    MailStr = integer_to_list(MailNr),
    To = lists:flatten(decode(H#mail.to)),
    From = lists:flatten(decode(H#mail.from)),
    Subject = lists:flatten(decode(H#mail.subject)),
    CC = lists:flatten(decode(H#mail.cc)),
    ToolBar =
	if 
	    MailNr == -1 ->
		[{"tool-newmail.gif", "javascript:setCmd('reply');", "Reply"}];
	    true ->
		[{"tool-newmail.gif","compose.yaws","New"},
		 {"tool-newmail.gif", "javascript:setCmd('reply');", "Reply"},
		 {"","javascript:changeActive("++Depth++");",
		  "<div id='msg-button:"++Depth++"' style='display: block;'>Headers</div>"
		  "<div id='hdr-button:"++Depth++"' style='display: none;' >Message</div>"
		 },
		 {"tool-delete.gif","javascript:setCmd('delete');", "Delete"},
		 {"","mail.yaws","Close"}]
	end,
    [{form, [{name,compose},{action,"reply.yaws"},{method,post}],
     [build_toolbar(ToolBar),
      {table,[{width,645},{height,"100%"},{border,0},{bgcolor,silver},
	      {cellspacing,0},{callpadding,0}],
       {tr,[],{td,[{valign,top},{height,"1%"}],
	       [{table,
		 [{border,0},{cellspacing,0},{cellpadding,0},{width,"100%"},
		  {bgcolor,silver}],
		 [{tr,[],
		   [{td,[{valign,middle},{align,left},{width,"15%"},
			 {height,25}],
		     {font, [{color,"#000000"},{size,2}],
		      {nobr,[],{pre_html,"&nbsp;From:&nbsp;"}}}},
		    {td, [{valign,middle},{align,left}],
		     {font, [{color,"#000000"},{size,2}],
		      [{pre_html,"&nbsp;"},
		       unquote(From)]}},
		    {td,[{valign,middle},{align,right},{height,"25"}],
		     {font, [{color,"#000000"},{size,2}],
		      {nobr,[],{pre_html,"&nbsp;Sent:&nbsp;"}}}},
		    {td, [nowrap,{valign,middle},{align,right},
			  {width,"30%"}],
		     {font, [{color,"#000000"},{size,2}],
		      {pre_html,"&nbsp;"++H#mail.date}}}]},
		  {tr,[],
		   [{td,[{valign,top},{align,left},{width,"15%"},
			 {height,25}],
		     {font, [{color,"#000000"},{size,2}],
		      {nobr,[],{pre_html,"&nbsp;To:&nbsp;"}}}},
		    {td, [{valign,top},{align,left},{width,"100%"}],
		     {font, [{color,"#000000"},{size,2}],
		      [{pre_html,"&nbsp;"},
		       unquote(To)]}}]},
		  {tr,[],
		   [{td,[{valign,middle},{align,left},{width,"15%"},
			 {height,25}],
		     {font, [{color,"#000000"},{size,2}],
		      {nobr,[],{pre_html,"&nbsp;Cc:&nbsp;"}}}},
		    {td, [{valign,middle},{align,left},{width,"100%"}],
		     {font, [{color,"#000000"},{size,2}],
		      [{pre_html,"&nbsp;"},CC]}}]},
		  {tr,[],
		   [{td,[{valign,middle},{align,left},{width,"15%"},
			 {height,25}],
		     {font, [{color,"#000000"},{size,2}],
		      {nobr,[],{pre_html,"&nbsp;Subject:&nbsp;"}}}},
		    {td, [{valign,middle},{align,left},{width,"100%"}],
		     {font, [{color,"#000000"},{size,2}],
		      [{pre_html,"&nbsp;"},Subject]}}]}
		 ]},
		{table, [{width,"100%"},{border,1},{cellpadding,6},
			 {class,msgbody}],
		 [{tr,[],
		   {td,[{width,"100%"},{height,300},{valign,top},
			{bgcolor,white}],
		    {p,[],{font,[{size,3},{id, contents}],
			   [
			    {'div', [{id,"msg-body:msg"++Depth},
				   {class,"msg-body"},
				   {style,"display: block;"}],
			     Formated
			    },
			    {'div', [{id,"msg-body:hdr"++Depth},
				   {class,"msg-body"},
				   {style, "display: none;"}],
			     {pre, [], Headers}
			    }
			   ]
			  }
		    }
		   }
		  }
		 ]
		}
	       ]
	      }
       }
      }] ++
     if
	 MailNr == -1 -> [];
	 true ->
	     [{input,[{type,hidden},{name,nr}, {value,MailNr}],[]}]
     end++
     [{input,[{type,hidden},{name,from},
	      {check,value,yaws_api:url_encode(From)}],[]},
      {input,[{type,hidden},{name,to},
	      {check,value,yaws_api:url_encode(To)}],[]},
      {input,[{type,hidden},{name,cc},
	      {check,value,yaws_api:url_encode(CC)}],[]},
      {input,[{type,hidden},{name,bcc},
	      {check,value,yaws_api:url_encode(decode(H#mail.bcc))}],[]},
      {input,[{type,hidden},{name,subject},
	      {check,value,yaws_api:url_encode(Subject)}],[]},
      {input,[{type,hidden},{name,cmd},{value,""}],[]}
     ]
    }].

select_alt_body([], [First|_]) -> First;
select_alt_body([Prefered|Rest], Bodies) ->
    case [Body || Body <- Bodies, has_body_type(Prefered,Body)] of
	[] ->
	    select_alt_body(Rest, Bodies);
	[First|_] ->
	    First
    end.
		  
has_body_type(Type, {H,B}) ->
    case H#mail.content_type of
	{CT, _Ops} ->
	    CTL = lowercase(CT),
	    CTL == Type;
	_ -> false
    end.

format_body(Session, H,Msg,Depth) ->
    ContentType =
	case H#mail.content_type of
	    {CT,Ops} -> {lowercase(CT), Ops};
	    Other -> Other
	end,
    case {ContentType,H#mail.transfer_encoding} of
	{{"text/html",_}, Encoding} ->
	    Decoded = decode_message(Encoding, Msg),
	    {pre_html, Decoded};
	{{"text/plain",_}, Encoding} ->
	    Decoded = decode_message(Encoding, Msg),
	    {pre, [], wrap_text(Decoded, 80)};
	{{"multipart/mixed",Opts}, Encoding} ->
	    {value, {_,Boundary}} = lists:keysearch("boundary",1,Opts),
	    [{Headers,Body}|Parts] = parse_multipart(Msg, Boundary),
	    PartHeaders =
		lists:foldl(fun({K,V},MH) ->
				    add_header(K,V,MH)
			    end, #mail{}, Headers),
	    [format_body(Session, PartHeaders, Body, Depth++".1"),
	     format_attachements(Session, Parts, Depth)];
	{{"multipart/alternative",Opts}, Encoding} ->
	    {value, {_,Boundary}} = lists:keysearch("boundary",1,Opts),
	    Parts = parse_multipart(Msg, Boundary),
	    HParts =
		lists:map(
		  fun({Head,Body}) ->
			  NewHead =
			      lists:foldl(fun({K,V},MH) ->
						  add_header(K,V,MH)
					  end, #mail{}, Head),
			  {NewHead, Body}
		  end, Parts),
	    {H1,B1} = select_alt_body(["text/html","text/plain"],HParts),
	    format_body(Session, H1,B1,Depth++".1");
	{{"multipart/signed",Opts}, Encoding} ->
	    {value, {_,Boundary}} = lists:keysearch("boundary",1,Opts),
	    [{Headers,Body}|Parts] = parse_multipart(Msg, Boundary),
	    PartHeaders =
		lists:foldl(fun({K,V},MH) ->
				    add_header(K,V,MH)
			    end, #mail{}, Headers),
	    format_body(Session, PartHeaders, Body, Depth++".1");
	{{"message/rfc822",Opts}, Encoding} ->
	    Decoded = decode_message(Encoding, Msg),
	    format_message(Session, Decoded, -1, Depth);
	{_,_} ->
	    {pre, [], wrap_text(Msg,80)}
    end.

format_attachements(S, [], _Depth) -> [];
format_attachements(S, Bs, Depth) ->
    [{table,[{bgcolor, "lightgrey"}],
      [
       {tr,[], {td, [], {h5,[], "Attachments:"}}},
       {tr, [], {td, [], {table, [], format_attach(S, Bs, Depth)}}}]}].

format_attach(_S, [], Depth) ->
    [];
format_attach(S, [{Headers,B0}|Bs], Depth) ->
    B = 
	if list(B0) -> list_to_binary(B0);
	   true -> B0
	end,
    H = lists:foldl(fun({K,V},MH) -> add_header(K,V,MH) end, #mail{}, Headers),
    Cookie = S#session.cookie,
    FileName = extraxt_h_info(H),
    HttpCtype = yaws_api:mime_type(FileName),
    mail_session_manager ! {session_set_attach_data, self(), Cookie,
			    FileName,HttpCtype,B},
    receive
	{session_manager, Num} ->
	    [{tr,[],{td,[],
		     {a, [{href,io_lib:format("attachment/~s?nr=~w",
					      [FileName,Num])}],
		      FileName}}} |
	     format_attach(S, Bs, Depth)]
    after 10000 ->
	    format_attach(S, Bs, Depth)
    end.


extraxt_h_info(H) ->
    L = case {H#mail.content_type, H#mail.content_disposition} of
	    {undefined, undefined} ->
		[];
	    {undefined, {_, LL}} ->
		LL;
	    {{_,LL}, undefined} ->
		LL;
	    {{_,L1}, {_,L2}} ->
		L1 ++ L2
	end,
    case lists:keysearch("filename", 1, L) of
	false ->
	    "attachment.txt";
	{value, {_, FN}} ->
	    FN
    end.


decode_message("7bit"++_, Msg) -> Msg;
decode_message("8bit"++_, Msg) -> Msg;
decode_message("base64"++_, Msg) ->
    case catch base64_2_str(lists:flatten(Msg)) of
	{'EXIT', _} -> Msg;
	Decoded -> Decoded
    end;
decode_message("quoted-printable"++_, Msg) ->
    case catch quoted_2_str(lists:flatten(Msg)) of
	{'EXIT', Reason} -> 
	    io:format("failed to decode quoted-printable ~p\n", [Reason]),
	    Msg;
	Decoded -> Decoded
    end;
decode_message(_, Msg) -> Msg.
    
	    
quoted_2_str(Msg) ->
    quoted_2_str(Msg, []).

quoted_2_str([], Acc) ->
    lists:reverse(Acc);
quoted_2_str([$=,$\r,$\n|Rest], Acc) ->
    quoted_2_str_scan(Rest,Acc);
quoted_2_str([$=,H1,H2|Rest], Acc) ->
    quoted_2_str(Rest, [yaws:hex_to_integer([H1,H2])|Acc]);
quoted_2_str([$\r,$\n|Rest], Acc) ->
    quoted_2_str_scan(Rest, [$\n|Acc]);
quoted_2_str([C|Cs], Acc) ->
    quoted_2_str(Cs, [C|Acc]).

quoted_2_str_scan([$ |Rest], Acc) ->
    quoted_2_str_scan(Rest, Acc);
quoted_2_str_scan([$\t|Rest], Acc) ->
    quoted_2_str_scan(Rest, Acc);
quoted_2_str_scan([$\v|Rest], Acc) ->
    quoted_2_str_scan(Rest, Acc);
quoted_2_str_scan(Rest, Acc) ->
    quoted_2_str(Rest, Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%


parse_multipart(Data, Boundary) ->
    Res = parse_multipart(Data, Boundary, []),
    process_parts(Res, [], [], []).

parse_multipart([], _State, Res) ->
    Res;
parse_multipart([D|Ds], State, Res) ->
    case yaws_api:parse_multipart(D, State) of
	{cont, Cont, NewRes} ->
	    parse_multipart(Ds, Cont, Res++NewRes);
	{result, NewRes} ->
	    Res++NewRes
    end.

process_parts([], [], [], Res) ->
    lists:reverse(Res);
process_parts([{head,{Headers}}|Ps], [], [], Res) ->
    process_parts(Ps, Headers, [], Res);
process_parts([{body,B}|Ps], [], Body, Res) ->  % ignore headless body
    process_parts(Ps, [], [], Res);
process_parts([{body,B}|Ps], Head, Body, Res) ->
    process_parts(Ps, [], [], [{Head, lists:reverse([B|Body])}|Res]);
process_parts([{part_body,B}|Ps], Head, Body, Res) ->
    process_parts(Ps, Head, [B|Body], Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The text to wrap may be arbitrarily nested. We deal with this
% without flattening the whole thing.
%

wrap_text(Text, Max) ->
    wrap_text(Text, [], [], 0, Max).

wrap_text([], [], Unwrapped, Col, Max) ->
    if
	Col < Max ->
	    lists:reverse(Unwrapped);
	true ->
	    [$\n|lists:reverse(Unwrapped)]
    end;

wrap_text([], Cont, Unwrapped, Col, Max) ->
    wrap_text(Cont, [], Unwrapped, Col, Max);

wrap_text([L|Rest], [], Unwrapped, Col, Max) when list(L) ->
    wrap_text(L, Rest, Unwrapped, Col, Max);

wrap_text([L|Rest], Cont, Unwrapped, Col, Max) when list(L) ->
    wrap_text(L, [Rest|Cont], Unwrapped, Col, Max);

wrap_text([C|Rest], Cont, Unwrapped, Col, Max) when Col < Max ->
    case char_class(C) of
	space ->
	    [lists:reverse(Unwrapped),
	     [C|wrap_text(Rest, Cont, [], Col+1, Max)]];
	tab ->
	    [lists:reverse(Unwrapped),
	     [C|wrap_text(Rest, Cont, [], Col+8, Max)]];
	nl ->
	    [lists:reverse(Unwrapped),
	     [C|wrap_text(Rest, Cont, [], 0, Max)]];
	text ->
	    wrap_text(Rest, Cont, [C|Unwrapped], Col+1, Max)
    end;
wrap_text([C|Rest], Cont, Unwrapped, Col, Max) when Col >= Max ->
    case char_class(C) of
	space ->
	    [[$\n|lists:reverse(Unwrapped)], [C],
	     wrap_text(Rest, Cont, [], length(Unwrapped), Max)];
	tab ->
	    [[$\n|lists:reverse(Unwrapped)], [C],
	     wrap_text(Rest, Cont, [], length(Unwrapped), Max)];
	nl ->
	    [[$\n|lists:reverse(Unwrapped)], [C],
	     wrap_text(Rest, Cont, [], length(Unwrapped), Max)];
	text ->
	    wrap_text(Rest, Cont, [C|Unwrapped], Col+1, Max)
    end.

char_class($\n) -> nl;
char_class($\r) -> nl;
char_class($ )  -> space;
char_class($\t) -> tab;
char_class(O)   -> text.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sleep(X) ->
    receive
	xxxxxxx ->  ok
    after
	X -> ok
    end.

	
%%%%%%%%%%%%%%%%%%%%%% read cfg file %%%%%%%%%%%%%%%%%%
%% def for root is: /etc/mail/yaws-webmail.conf

read_config() ->
    Paths = case yaws:getuid() of
		{ok, "0"} ->
		    ["/etc/mail/yaws-webmail.conf"];
		_ ->
		    [filename:join([os:getenv("HOME"),"yaws-webmail.conf"]),
		     "./yaws-webmail.conf",
		     "/etc/mail/yaws-webmail.conf"]
	    end,
    case yaws:first(fun(F) -> yaws:exists(F) end, Paths) of
	false ->
	    error_logger:info_msg("yaws webmail: Can't find no config file .. "
				  "using defaults",[]),
	    #cfg{};
	{ok, _, File} ->
	    read_config(File)
    end.

read_config(File) ->
    error_logger:info_msg("Yaws webmail: Using config file ~s~n", [File]),
    case file:open(File, [read]) of
	{ok, FD} ->
	    read_config(FD, #cfg{}, 1, io:get_line(FD, ''));
	Err ->
	    error_logger:info_msg("Yaws webmail: Can't open config file ... "
				  "using defaults",[]),
	    #cfg{}
    end.

read_config(FD, Cfg, Lno, eof) ->
    file:close(FD),
    Cfg;
read_config(FD, Cfg, Lno, Chars) ->
    Next = io:get_line(FD, ''),
    case yaws_config:toks(Chars) of
	[] ->
	    read_config(FD, Cfg, Lno+1, Next);
	["ttl", '=', IntList] ->
	    case (catch list_to_integer(IntList)) of
		{'EXIT', _} ->
		    error_logger:info_msg("Yaws webmail:  expect integer at "
					  "line ~p", [Lno]),
		    read_config(FD, Cfg, Lno+1, Next);
		Int ->
		    read_config(FD, Cfg#cfg{ttl = Int}, Lno+1, Next)
	    end;
	["popserver", '=', Server] ->
	    read_config(FD, Cfg#cfg{popserver = Server}, Lno+1, Next);
	
	["smtpserver", '=', Domain] ->
	    read_config(FD, Cfg#cfg{smtpserver = Domain}, Lno+1, Next);
	["maildomain", '=', Domain] ->
	    read_config(FD, Cfg#cfg{maildomain = Domain}, Lno+1, Next);
	["sendtimeout", '=', IntList] ->
	    case (catch list_to_integer(IntList)) of
		{'EXIT', _} ->
		    error_logger:info_msg("Yaws webmail:  expect integer at "
					  "line ~p", [Lno]),
		    read_config(FD, Cfg, Lno+1, Next);
		Int ->
		    read_config(FD, Cfg#cfg{sendtimeout = Int}, Lno+1, Next)
	    end;
	[H|_] ->
	    error_logger:info_msg("Yaws webmail: Unexpected tokens ~p at "
				  "line ~w", [H, Lno]),
	    read_config(FD, Cfg, Lno+1, Next)
    end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(date, {year, month, day, hours, minutes, seconds}).

parse_date([]) -> [];
parse_date(Date) ->
    D = parse_date(Date, #date{}),
    if 
	integer(D#date.year),integer(D#date.month),
	integer(D#date.day),integer(D#date.hours),
	integer(D#date.minutes),integer(D#date.seconds) ->
	    {{D#date.year, D#date.month, D#date.day},
	     {D#date.hours, D#date.minutes, D#date.seconds}};
	true -> error
    end.

parse_date([], D) -> D;
parse_date([D|Ds], Date) ->
    case char_type(D) of
	space -> parse_date(Ds, Date);
	alpha when Date#date.month == undefined ->
	    case is_month(lowercase([D|Ds])) of
		false ->
		    parse_date(Ds, Date);
		{true, M, Rest} ->
		    parse_date(Rest, Date#date{month=M})
	    end;
	alpha ->
	    parse_date(Ds, Date);
	digit ->
	    case parse_time([D|Ds]) of
		error ->
		    {Number,Rest} = get_number([D|Ds], 0),
		    if
			Number < 32, Date#date.day == undefined ->
			    parse_date(Rest, Date#date{day=Number});
			Number < 50, Date#date.year == undefined ->
			    parse_date(Rest, Date#date{year=Number+2000});
			Number < 100, Date#date.year == undefined ->
			    parse_date(Rest, Date#date{year=Number+1900});
			Number > 1900, Date#date.year == undefined ->
			    parse_date(Rest, Date#date{year=Number});
			true ->
			    parse_date(Rest, Date)
		    end;
		{Hours, Minutes, Seconds, Rest} ->
		    parse_date(Rest, Date#date{hours=Hours,
					       minutes=Minutes,
					       seconds=Seconds})
	    end;
	_ ->
	    parse_date(Ds, Date)
    end.
		      
is_month("jan"++Rest) -> {true, 1, Rest};
is_month("feb"++Rest) -> {true, 2, Rest};
is_month("mar"++Rest) -> {true, 3, Rest};
is_month("apr"++Rest) -> {true, 4, Rest};
is_month("may"++Rest) -> {true, 5, Rest};
is_month("jun"++Rest) -> {true, 6, Rest};
is_month("jul"++Rest) -> {true, 7, Rest};
is_month("aug"++Rest) -> {true, 8, Rest};
is_month("sep"++Rest) -> {true, 9, Rest};
is_month("oct"++Rest) -> {true, 10, Rest};
is_month("nov"++Rest) -> {true, 11, Rest};
is_month("dec"++Rest) -> {true, 12, Rest};
is_month(_) -> false.

enc_month(1) -> "Jan";
enc_month(2) -> "Feb";
enc_month(3) -> "Mar";
enc_month(4) -> "Apr";
enc_month(5) -> "May";
enc_month(6) -> "Jun";
enc_month(7) -> "Jul";
enc_month(8) -> "Aug";
enc_month(9) -> "Sep";
enc_month(10) -> "Oct";
enc_month(11) -> "Nov";
enc_month(12) -> "Dec".

char_type(D) when D>=$a, D=<$z -> alpha;
char_type(D) when D>=$A, D=<$Z -> alpha;
char_type(D) when D>=$0, D=<$9 -> digit;
char_type($\ ) -> space;
char_type($\n) -> space;
char_type($\t) -> space;
char_type($\v) -> space;
char_type(_) -> unknown.

get_number([D|Ds], N) when D>=$0, D=<$9 ->
    get_number(Ds, N*10+(D-$0));
get_number(Rest, N) -> {N, Rest}.

parse_time(Time) ->
    F = fun() ->
		{Hour,[$:|R1]}    = get_number(Time, 0),
		{Minutes,[$:|R2]} = get_number(R1, 0),
		{Seconds,R3}      = get_number(R2, 0),
		{Hour, Minutes, Seconds, R3}
	end,
    case catch F() of
	{Hour, Minutes, Seconds, Rest} when integer(Hour),
				      integer(Minutes),
				      integer(Seconds) ->
	    {Hour, Minutes, Seconds, Rest};
	_ -> error
    end.
		
format_date({{Year,Month,Day},{Hour,Minutes,Seconds}}) ->
    M = enc_month(Month),
    io_lib:format("~2..0w ~s ~4..0w ~2..0w:~2..0w:~2..0w",
		  [Day, M, Year, Hour, Minutes, Seconds]);
format_date(Seconds) when integer(Seconds) ->
    Zero = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Time = Zero + Seconds,
    Date = calendar:gregorian_seconds_to_datetime(Time),
    format_date(Date);
format_date([]) -> [];
format_date(error) -> [].





send_attachment(Session, Number) ->
    mail_session_manager ! {session_get_attach_data, self(), 
			    Session#session.cookie, Number},
    receive
	{session_manager, error} ->
	    none;
	{session_manager, A} ->
	    {content, A#satt.ctype, A#satt.data}
    after 15000 ->
	    exit(normal)
    end.


