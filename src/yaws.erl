%%%----------------------------------------------------------------------
%%% File    : yaws.erl
%%% Author  : Claes Wikstrom <klacke@bluetail.com>
%%% Purpose : 
%%% Created : 16 Jan 2002 by Claes Wikstrom <klacke@bluetail.com>
%%%----------------------------------------------------------------------

-module(yaws).
-author('klacke@bluetail.com').

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include("yaws_debug.hrl").



-include_lib("kernel/include/file.hrl").

-compile(export_all).
-import(lists, [reverse/1, reverse/2]).

%%-export([Function/Arity, ...]).


start() ->
    application:start(yaws, permanent).
stop() ->
    application:stop(yaws).

hup(Sock) ->
    spawn(fun() ->
		  group_leader(whereis(user), self()),
		  dohup(Sock)
	  end).

dohup(Sock) ->
    {Debug, Trace, TraceOut, Conf, _RunMod, _Embed} = 
	yaws_sup:get_app_args(),
    Res = (catch case yaws_config:load(Conf, Trace, TraceOut, Debug) of
		     {ok, Gconf, Sconfs} ->
			 yaws_api:setconf(Gconf, Sconfs);
		     Err ->
			 Err
		 end),
    gen_tcp:send(Sock, io_lib:format("hupped: ~p~n", [Res])),
    gen_tcp:close(Sock).
    


%%% misc funcs
first(_F, []) ->
    false;
first(F, [H|T]) ->
    case F(H) of
	{ok, Val} ->
	    {ok, Val, H};
	ok ->
	    {ok, ok, H};
	_ ->
	    first(F, T)
    end.

	    
elog(F, As) ->
    error_logger:format(F, As).



filesize(Fname) ->
    case file:read_file_info(Fname) of
	{ok, FI} when FI#file_info.type == regular ->
	    {ok, FI#file_info.size};
	{ok, FI} ->
	    {error,  FI#file_info.type};
	Err ->
	    Err
    end.
%% 
upto(0, []) ->
    [];
upto(_I, []) ->
    [];
upto(0, _) ->
    " ....";
upto(_I, [0|_]) ->
    " ....";
upto(I,[H|T]) ->
    [H|upto(I-1, T)].
zip([H1|T1], [H2|T2]) ->
    [{H1, H2} |zip(T1, T2)];
zip([], []) ->
    [].





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Date and Time functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

date_and_time() ->
    case catch erlang:now() of
	{'EXIT', _} -> % We don't have info about UTC
	    short_time(calendar:local_time());
	Now ->
	    UTC = calendar:now_to_universal_time(Now),
	    Local = calendar:universal_time_to_local_time(UTC),
	    date_and_time(Local, UTC)
    end.
 
date_and_time(Local, UTC) ->
    DiffSecs = calendar:datetime_to_gregorian_seconds(Local) -
	       calendar:datetime_to_gregorian_seconds(UTC),
    short_time(Local) ++ diff(DiffSecs).

%% short_time

short_time({{Y,M,D},{H,Mi,S}}) ->
    [y1(Y), y2(Y), M, D, H, Mi, S].

%% Format date according to ISO 8601
date_and_time_to_string(DAT) ->
    case validate_date_and_time(DAT) of
	true ->
	    dat2str(DAT);
	false ->
	    erlang:fault({badarg, {?MODULE, date_and_time_to_string, [DAT]}})
    end.

universal_time_to_string(UTC) ->
    Local = calendar:universal_time_to_local_time(UTC),
    DT = local_time_to_date_and_time(Local),
    date_and_time_to_string(DT).


dat2str([Y1,Y2, Mo, D, H, M, S | Diff]) ->
    lists:flatten(
      io_lib:format("~s ~w-~2.2.0w-~2.2.0w,~2.2.0w:~2.2.0w:~2.2.0w",
		    [weekday(Y1,Y2,Mo,D), y(Y1,Y2),Mo,D,H,M,S]) ++
      case Diff of
	  [Sign,Hd,Md] ->
	      io_lib:format("~c~2.2.0w~2.2.0w",
			    [Sign,Hd,Md]);
	  _ -> []
      end).

weekday(Y1,Y2,Mo,D) ->
    int_to_wd(calendar:day_of_the_week(Y1*256+Y2,Mo,D)).

int_to_wd(1) ->
    "Mon";
int_to_wd(2) ->
    "Tue";
int_to_wd(3) ->
    "Wed";
int_to_wd(4) ->
    "Thu";
int_to_wd(5) ->
    "Fri";
int_to_wd(6) ->
    "Sat";
int_to_wd(7) ->
    "Sun".

y1(Y) -> (Y bsr 8) band 255.
y2(Y) -> Y band 255.

y(Y1, Y2) -> 256 * Y1 + Y2.
    
diff(Secs) ->
    case calendar:seconds_to_daystime(Secs) of
	{0, {H, M,_}} ->
	    [$+, H, M];
	{-1, _} ->
	    {0, {H, M, _}} = calendar:seconds_to_daystime(-Secs),
	    [$-, H, M]
    end.

universal_time_to_date_and_time(UTC) ->
    short_time(UTC) ++ [$+, 0, 0].

local_time_to_date_and_time(Local) ->
    UTC = calendar:local_time_to_universal_time(Local),
    date_and_time(Local, UTC).

date_and_time_to_universal_time([Y1,Y2, Mo, D, H, M, S]) ->
    %% Local time specified, convert to UTC
    Local = {{y(Y1,Y2), Mo, D}, {H, M, S}},
    calendar:local_time_to_universal_time(Local);
date_and_time_to_universal_time([Y1,Y2, Mo, D, H, M, S, Sign, Hd, Md]) ->
    %% Time specified as local time + diff from UTC. Conv to UTC.
    Local = {{y(Y1,Y2), Mo, D}, {H, M, S}},
    LocalSecs = calendar:datetime_to_gregorian_seconds(Local),
    Diff = (Hd*60 + Md)*60,
    UTCSecs = if Sign == $+ -> LocalSecs - Diff;
		 Sign == $- -> LocalSecs + Diff
	      end,
    calendar:gregorian_seconds_to_datetime(UTCSecs).

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
check_diff([$+, H, M]) when 0 =< H, H < 24, 0 =< M, M < 60 -> true;
check_diff([$-, H, M]) when 0 =< H, H < 24, 0 =< M, M < 60 -> true;
check_diff(_) -> false.
  

%% to_string

to_string(X) when float(X) ->
    io_lib:format("~.2.0f",[X]);
to_string(X) when integer(X) ->
    integer_to_list(X);
to_string(X) when atom(X) ->
    atom_to_list(X);
to_string(X) ->
    lists:concat([X]).

%%


to_list(L) when list(L) ->
    L;
to_list(A) when atom(A) ->
    atom_to_list(A).


lowercase([C|S]) -> [lowercase(C)|lowercase(S)];
lowercase(C) when C>=$A, C=<$Z -> C+32;
lowercase(C) -> C.

%%

uppercase([C|S]) -> [uppercase(C)|uppercase(S)];
uppercase(C) when C>=$a, C=<$z -> C-32;
uppercase(C) -> C.

%%

lowercase_string(String) ->
    lists:map(fun(X) -> lowercase(X) end, String).

%% integer_to_hex


integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
	{'EXIT', _} ->
	    old_integer_to_hex(I);
	Int ->
	    Int
    end.


old_integer_to_hex(I) when I<10 ->
    integer_to_list(I);
old_integer_to_hex(I) when I<16 ->
    [I-10+$A];
old_integer_to_hex(I) when I>=16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).




%% hex_to_integer


hex_to_integer(Hex) ->
    case catch erlang:list_to_integer(Hex, 16) of
	{'EXIT', _} ->
	    old_hex_to_integer(Hex);
	X ->
	    X
    end.


old_hex_to_integer(Hex) ->
    DEHEX = fun (H) when H >= $a, H =< $f -> H - $a + 10;
		(H) when H >= $A, H =< $F -> H - $A + 10;
		(H) when H >= $0, H =< $9 -> H - $0
	    end,
    lists:foldl(fun(E, Acc) -> Acc*16+DEHEX(E) end, 0, Hex).



%% string_to_hex

string_to_hex(String) ->
    HEXC = fun (D) when D > 9 -> $a + D - 10;
	       (D) ->            $0 + D
	   end,
    lists:foldr(fun (E, Acc) ->
			[HEXC(E div 16),HEXC(E rem 16)|Acc]
		end, [],
		String).


%% hex_to_string

hex_to_string(Hex) ->
    DEHEX = fun (H) when H >= $a -> H - $a + 10;
		(H) when H >= $A -> H - $A + 10;
		(H) ->              H - $0
	    end,
    {String, _} =
	lists:foldr(fun (E, {Acc, nolow}) ->
			    {Acc, DEHEX(E)};
			(E, {Acc, LO})  ->
			    {[DEHEX(E)*16+LO|Acc], nolow}
		    end, {[], nolow},
		    Hex),
    String.


%% mk_list

mk_list([]) ->
    [];
mk_list([X]) ->
    to_string(X);
mk_list([X|Rest]) ->
    [to_string(X)," ",mk_list(Rest)].



universal_time_as_string() ->
    time_to_string(calendar:universal_time(), "GMT").
local_time_as_gmt_string(LocalTime) ->
    time_to_string(calendar:local_time_to_universal_time(LocalTime),"GMT").


time_to_string( {{Year, Month, Day}, {Hour, Min, Sec}}, Zone) ->
    io_lib:format("~s, ~s ~s ~w ~s:~s:~s ~s",
		  [day(Year, Month, Day),
		   mk2(Day), month(Month), Year,
		   mk2(Hour), mk2(Min), mk2(Sec), Zone]).



mk2(I) when I < 10 ->
    [$0 | integer_to_list(I)];
mk2(I) ->
    integer_to_list(I).

day(Year, Month, Day) ->
    int_to_wd(calendar:day_of_the_week(Year, Month, Day)).

month(1) ->
    "Jan";
month(2) ->
    "Feb";
month(3) ->
    "Mar";
month(4) ->
    "Apr";
month(5) ->
    "May";
month(6) ->
    "Jun";
month(7) ->
    "Jul";
month(8) ->
    "Aug";
month(9) ->
    "Sep";
month(10) ->
    "Oct";
month(11) ->
    "Nov";
month(12) ->
    "Dec".



month_str_to_int("Jan") ->
    1;
month_str_to_int("Jun") ->
    6;
month_str_to_int("Jul") ->
    7;
month_str_to_int("Feb") ->
    2;
month_str_to_int("Mar") ->
    3;
month_str_to_int("Apr") ->
    4;
month_str_to_int("May") ->
    5;
month_str_to_int("Aug") ->
    8;
month_str_to_int("Sep") ->
    9;
month_str_to_int("Oct") ->
    10;
month_str_to_int("Nov") ->
    11;
month_str_to_int("Dec") ->
    12.



day_str_to_int("Mon") ->
    1;
day_str_to_int("Tue") ->
    2;
day_str_to_int("Wed") ->
    3;
day_str_to_int("Thu") ->
    4;
day_str_to_int("Fri") ->
    5;
day_str_to_int("Sat") ->
    6;
day_str_to_int("Sun") ->
    7.


%% Wed, 23 Jan 2002 19:07:44 GMT

stringdate_to_datetime([$ |T]) ->
    stringdate_to_datetime(T);
stringdate_to_datetime([_D1, _D2, _D3, $\,, $ |Tail]) ->
    stringdate_to_datetime1(Tail).

stringdate_to_datetime1([A, B, $\s |T]) ->
    stringdate_to_datetime2(T, list_to_integer([A,B]));

stringdate_to_datetime1([A, $\s |T]) ->
    stringdate_to_datetime2(T, list_to_integer([A])).



stringdate_to_datetime2([M1, M2, M3, $\s , Y1, Y2, Y3, Y4, $\s ,
			 H1, H2, $:, Min1, Min2,$:, 
			 S1, S2,$\s ,$G, $M, $T|_], Day) ->
    {{list_to_integer([Y1,Y2,Y3,Y4]), 
      month_str_to_int([M1, M2, M3]), Day},
     {list_to_integer([H1, H2]),
      list_to_integer([Min1, Min2]),
      list_to_integer([S1, S2])}}.




%% used by If-Modified-Since header code
is_modified_p(FI, UTC_string) ->
    case catch stringdate_to_datetime(UTC_string) of
	{'EXIT', _ } ->
	    true;
	UTC ->
	    Mtime = FI#file_info.mtime,
	    MtimeUTC = calendar:local_time_to_universal_time(Mtime),
	    MtimeUTC > UTC
    end.




ticker(Time, Msg) ->
    S = self(),
    spawn_link(yaws, ticker, [Time, S, Msg]).
ticker(Time, To, Msg) ->
    process_flag(trap_exit, true),
    yaws_ticker:ticker(Time, To, Msg).


fmt_ip({A,B,C,D}) ->
    [integer_to_list(A), $.,
     integer_to_list(B), $.,
     integer_to_list(C), $.,
     integer_to_list(D)].



parse_ip(Val) ->
    case string:tokens(Val, [$.]) of
	Nums = [_X1, _X2, _X3,_X4] ->
	    L = lists:map(
		  fun(S) -> (catch list_to_integer(S)) end, 
		  Nums),
	    case lists:zf(fun(I) when integer(I),
				      0 =< I,
				      I =< 255 ->
				  true;
			     (_) ->
				  false
			  end, L) of
		L2 when length(L2) == 4 ->
		    list_to_tuple(L2);
		_ ->
		    error
	    end;
	_ ->
	    error
    end.



address() ->
    ?F("<address> ~s Server at ~s </address>",
	[
	(get(gc))#gconf.yaws,
	(get(sc))#sconf.servername]).




is_space($\s) ->
    true;
is_space($\r) ->
    true;
is_space($\n) ->
    true;
is_space($\t) ->
    true;
is_space(_) ->
    false.


strip_spaces(String) ->
    strip_spaces(String, both).

strip_spaces(String, left) ->
    drop_spaces(String);
strip_spaces(String, right) ->
    lists:reverse(drop_spaces(lists:reverse(String)));
strip_spaces(String, both) ->
    strip_spaces(drop_spaces(String), right).

drop_spaces([]) ->
    [];
drop_spaces(YS=[X|XS]) ->
    case is_space(X) of
	true ->
	    drop_spaces(XS);
	false ->
	    YS
    end.


%%% basic uuencode and decode functionality    

list_to_uue(L) -> list_to_uue(L, []).

list_to_uue([], Out) ->
    reverse([$\n,enc(0)|Out]);
list_to_uue(L, Out) ->
    {L45, L1} = get_45(L),
    Encoded = encode_line(L45),
    list_to_uue(L1, reverse(Encoded, Out)).

uue_to_list(L) -> 
    uue_to_list(L, []).

uue_to_list([], Out) ->
    reverse(Out);
uue_to_list(L, Out) ->
    {Decoded, L1} = decode_line(L),
    uue_to_list(L1, reverse(Decoded, Out)).

encode_line(L) ->
    [enc(length(L))|encode_line1(L)].

encode_line1([C0, C1, C2|T]) ->
    Char1 = enc(C0 bsr 2),
    Char2 = enc((C0 bsl 4) band 8#60 bor (C1 bsr 4) band 8#17),
    Char3 = enc((C1 bsl 2) band 8#74 bor (C2 bsr 6) band 8#3),
    Char4 = enc(C2 band 8#77),
    [Char1,Char2,Char3,Char4|encode_line1(T)];
encode_line1([C1, C2]) -> encode_line1([C1, C2, 0]);
encode_line1([C])      -> encode_line1([C,0,0]);
encode_line1([])       -> [$\n].

decode_line([H|T]) ->
    case dec(H) of 
        0   -> {[], []};
        Len -> decode_line(T, Len, [])
    end.

decode_line([P0,P1,P2,P3|T], N, Out) when N >= 3->
    Char1 = 16#FF band ((dec(P0) bsl 2) bor (dec(P1) bsr 4)),
    Char2 = 16#FF band ((dec(P1) bsl 4) bor (dec(P2) bsr 2)),
    Char3 = 16#FF band ((dec(P2) bsl 6) bor dec(P3)),
    decode_line(T, N-3, [Char3,Char2,Char1|Out]);
decode_line([P0,P1,P2,_|T], 2, Out) ->
    Char1  = 16#FF band ((dec(P0) bsl 2) bor (dec(P1) bsr 4)),
    Char2  = 16#FF band ((dec(P1) bsl 4) bor (dec(P2) bsr 2)),
    {reverse([Char2,Char1|Out]), tl(T)};
decode_line([P0,P1,_,_|T], 1, Out) ->
    Char1  = 16#FF band ((dec(P0) bsl 2) bor (dec(P1) bsr 4)),
    {reverse([Char1|Out]), tl(T)};
decode_line(T, 0, Out) ->
    {reverse(Out), tl(T)}.

get_45(L) -> get_45(L, 45, []).

get_45(L, 0, F)     -> {reverse(F), L};
get_45([H|T], N, L) -> get_45(T, N-1, [H|L]);
get_45([], _N, L)    -> {reverse(L), []}.

%% enc/1 is the basic 1 character encoding function to make a char printing
%% dec/1 is the inverse

enc(0) -> $`;
enc(C) -> (C band 8#77) + $ .

dec(Char) -> (Char - $ ) band 8#77.

to_octal(I) -> reverse(to_octal1(I)).

to_octal1(0) ->  [];
to_octal1(I) ->  [$0 + I band 7|to_octal1(I bsr 3)].

oct_to_dig(O) -> oct_to_dig(O, 0).

oct_to_dig([], D)    -> D;
oct_to_dig([H|T], D) -> oct_to_dig(T, D*8 + H - $0).



printversion() ->
    io:format("Yaws ~s~n", [yaws_vsn:version()]),
    init:stop().

%% our default arg rewriteer does's of cource nothing
arg_rewrite(A) ->
    A.



delall(H, [H|T]) ->
    delall(H,T);
delall(H,[H1|T]) ->
    [H1 | delall(H, T)];
delall(_,[]) ->
    [].



to_lowerchar(C) when C >= $A, C =< $Z ->
    C+($a-$A);
to_lowerchar(C) ->
    C.

to_lower([C|Cs]) when C >= $A, C =< $Z ->
    [C+($a-$A)|to_lower(Cs)];
to_lower([C|Cs]) ->
    [C|to_lower(Cs)];
to_lower([]) ->
    [].


to_integer(I) when list(I) ->
    list_to_integer(I);
to_integer(I) when integer(I) ->
    I.


funreverse(List, Fun) ->
    funreverse(List, Fun, []).

funreverse([H|T], Fun, Ack) ->
    funreverse(T, Fun, [Fun(H)|Ack]);
funreverse([], _Fun, Ack) ->
    Ack.


%% splits Str in two parts
%% First part leading Upto SubStr and remaining part after
split_at(Str, Substr) ->
    split_at(Str, Substr,[]).

split_at(Str, Substr, Ack) ->
    case is_prefix(Substr, Str) of
	{true, Tail} ->
	    {lists:reverse(Ack), Tail};
	false ->
	    case Str of
		[] ->
		    {lists:reverse(Ack), []};
		[H|T] ->
		    split_at(T, Substr, [H|Ack])
	    end
    end.


%% is arg1 a prefix of arg2
is_prefix([H|T1], [H|T2]) ->
    is_prefix(T1, T2);
is_prefix([], T) ->
    {true, T};
is_prefix(_,_) ->
    false.

    
%% Split a string of words seperated by Sep into a list of words and
%% strip off white space.
%%
%% HTML semantics are used, such that empty words are omitted.


split_sep(undefined, _Sep) ->
    [];
split_sep(L, Sep) ->
    case drop_spaces(L) of
	[] -> [];
	[Sep|T] -> split_sep(T, Sep);
	[C|T]   -> split_sep(T, Sep, [C], [])
    end.

split_sep([], _Sep, AccL) ->
    lists:reverse(AccL);
split_sep([Sep|T], Sep, AccL) ->
    split_sep(T, Sep, AccL);
split_sep([C|T], Sep, AccL) ->
    split_sep(T, Sep, [C], AccL).

split_sep([], _Sep, AccW, AccL) ->
    lists:reverse([lists:reverse(drop_spaces(AccW))|AccL]);
split_sep([Sep|Tail], Sep, AccW, AccL) ->
    split_sep(drop_spaces(Tail), 
	      Sep, 
	      [lists:reverse(drop_spaces(AccW))|AccL]);
split_sep([C|Tail], Sep, AccW, AccL) ->
    split_sep(Tail, Sep, [C|AccW], AccL).
		      
		    
%% header parsing

parse_qval(S) ->
    parse_qval([], S).

parse_qval(A, ";q="++Q) ->   
    {lists:reverse(A), parse_qvalue(Q)};
parse_qval(A, "") ->
    {lists:reverse(A), 1000};
parse_qval(A, [C|T]) ->
    parse_qval([C|A], T).


parse_qvalue("0") ->
    0;
parse_qvalue("0.") ->
    0;
parse_qvalue("1") ->
    1000;
parse_qvalue("1.") ->
    1000;
parse_qvalue("1.0") ->
    1000;
parse_qvalue("1.00") ->
    1000;
parse_qvalue("1.000") ->
    1000;
parse_qvalue("0."++[D1]) ->
    three_digits_to_integer(D1,$0,$0);
parse_qvalue("0."++[D1,D2]) ->
    three_digits_to_integer(D1,D2,$0);
parse_qvalue("0."++[D1,D2,D3]) ->
    three_digits_to_integer(D1,D2,D3);
parse_qvalue(_) ->
						% error
    0.

three_digits_to_integer(D1, D2, D3) ->
    100*(D1-$0)+10*(D2-$0)+D3-$0.

has_buggy_deflate(UserAgent, Mime) ->
    UA = parse_ua(UserAgent),
    in_ua(
      fun("Mozilla/5.0") ->
	      in_comment(
		fun("rv:0."++_) ->
			true;
		   ("Konqueror"++_) ->
			true;
		   (_) ->
			false
		end,
		UA);
	 ("Mozilla/4.0") ->
	      in_comment(
		fun("MSIE"++_) ->
						% The fact that IE is
						% broken does of
						% course mean that we
						% will have to abondon
						% `deflate' (in favor
						% of `gzip')
						% altogether.  To
						% do...
			true;
		   (_) ->
			false
		end,
		UA);
	 ("AppleWebKit"++_) ->
	      true;
	 ("Galeon"++_) ->
	      true;
	 ("w3m"++_) ->
						% Problems when saving.
	      Mime /= "text/html"; 
	 (_)  ->
	      false
      end,
      UA).

%% Gzip encoding

accepts_gzip(H, Mime) ->
    case [Val || {_,_,'Accept-Encoding',_,Val}<- H#headers.other] of
	[] ->
	    false;
	[AcceptEncoding] ->
	    EncodingList = [parse_qval(X) || 
			       X <- split_sep(AcceptEncoding, $,)],
	    case [Q || {"gzip", Q} <- EncodingList] 
		++ [Q || {"*", Q} <- EncodingList] of
		[] ->
		    false;
		[Q|_] -> (Q > 100)  % just for fun
			     and not has_buggy_gzip(
				       H#headers.user_agent, Mime)
	    end;
	_ ->
	    false
    end.

%%% Advice partly taken from Apache's documentation of `mod_deflate'.

						% Only Netscape
						% 4.06-4.08 is really
						% broken.
has_buggy_gzip("Mozilla/4.06"++_, _) ->
    true;
has_buggy_gzip("Mozilla/4.07"++_, _) ->
    true;
has_buggy_gzip("Mozilla/4.08"++_, _) ->
    true;
						% Everything else
						% handles at least
						% HTML.
has_buggy_gzip(_, "text/html") ->
    false;
has_buggy_gzip(UserAgent, Mime) ->
    UA = parse_ua(UserAgent),
    in_ua(fun("Mozilla/4"++_) ->
						% Netscape 4.x may
						% choke on anything
						% not HTML.
		  case Mime of
						% IE doesn't, but some
						% versions are said to
						% have issues with
						% plugins.
		      "application/pdf" ->
			  true;
		      _ -> not in_comment(
				 fun("MSIE"++_) ->
					 true;
				    (_) -> false
				 end,
				 UA)
		  end;
	     ("w3m"++_) ->
						% W3m does not
						% decompress when
						% saving.
		  true;
	     ("Opera") ->
						% Opera 6 does not
						% uncompress downloads.
		  in_ua(fun("6."++_) -> true;
			   (_) -> false
			end, UA);
	     ("Opera/6."++_) -> 
		  true;
	     (_) ->
		  false
	  end,
	  UA).
		  

%%% Parsing of User-Agent header.
%%% Yes, this looks a bit like overkill.

tokenize_ua([], Acc) ->
    lists:reverse(Acc);
tokenize_ua([$\\ , C|T], Acc) ->
    tokenize_ua(T, [C|Acc]);
tokenize_ua([$(|T], Acc) ->
    tokenize_ua(T, [popen | Acc]);
tokenize_ua([$)|T], Acc) ->
    tokenize_ua(T, [pclose | Acc]);
tokenize_ua([C|T], Acc) ->
    tokenize_ua(T, [C|Acc]).

parse_ua(Line) ->
    case catch parse_ua_l(tokenize_ua(Line, [])) of
	{'EXIT', _} -> [];
	Res -> Res
    end.

parse_ua_l(Line) ->
    case drop_spaces(Line) of
	[] ->
	    [];
	[popen|T] ->
	    {Comment, Tail} = parse_comment(T),
	    [Comment | parse_ua_l(Tail)];
	[pclose|T] ->
						% Error, ignore
	    parse_ua_l(T);
	L ->
	    {UA, Tail} = parse_ua1(L),
	    [UA | parse_ua_l(Tail)]
    end.

parse_comment(L) ->	    
    parse_comment(L, [], []).

parse_comment([], _, _) ->
						% Error
    {error, []};
parse_comment([pclose|T], CAcc, CsAcc) ->
    {{comment, lists:reverse([lists:reverse(CAcc)|CsAcc])}, T};
parse_comment([popen|T], CAcc, CsAcc) ->
    {Comment, Tail} = parse_comment(T),
    parse_comment(drop_spaces(Tail), 
		  [], [Comment, lists:reverse(CAcc) | CsAcc]);
parse_comment([$;|T], CAcc, CsAcc) ->
    parse_comment(drop_spaces(T), [], [lists:reverse(CAcc)|CsAcc]);
parse_comment([C|T], CAcc, CsAcc) ->
    parse_comment(T, [C|CAcc], CsAcc).


parse_ua1(L) ->
    parse_ua1(L, []).

parse_ua1([], Acc) ->
    {{ua,lists:reverse(Acc)}, []};
parse_ua1([popen|T], Acc) ->
    {{ua, lists:reverse(Acc)}, [popen|T]};
parse_ua1([pclose|T], _Acc) ->
    {error, T};
parse_ua1([$ |T], Acc) ->
    {{ua, lists:reverse(Acc)}, T};
parse_ua1([C|T], Acc) ->
    parse_ua1(T, [C|Acc]).


in_ua(Pred, L) -> 
    lists:any(
      fun(X) -> 
	      case X of
		  {ua, UA} -> Pred(UA);
		  _ -> false
	      end
      end, L).

in_comment(_Pred, []) ->
    false;
in_comment(Pred, [{comment, Cs}|T]) ->
    case in_comment_l(Pred, Cs) of
	true ->
	    true;
	false ->
	    in_comment(Pred, T)
    end;
in_comment(Pred, [_|T]) ->
    in_comment(Pred, T).


in_comment_l(Pred, Cs) ->
    lists:any(fun(X) ->
		      case X of
			  {comment, Cs1} ->
			      in_comment_l(Pred, Cs1);
			  error -> false;
			  L -> Pred(L)
		      end
	      end, Cs).

%% imperative out header management

outh_set_status_code(Code) ->
    put(outh, (get(outh))#outh{status = Code}),
    ok.

outh_set_non_cacheable(_Version) ->
    put(outh, (get(outh))#outh{cache_control = "Cache-Control: no-cache\r\n"}),
    ok.

outh_set_content_type(Mime) ->
    put(outh, (get(outh))#outh{content_type = 
			       make_content_type_header(Mime)}),
    ok.

outh_set_content_encoding(Encoding) ->
    put(outh, (get(outh))#outh{content_encoding = 
			       make_content_encoding_header(Encoding)}),
    ok.

outh_set_cookie(C) ->
    put(outh, (get(outh))#outh{set_cookie = ["Set-Cookie: " , C, "\r\n"]}),
    ok.


outh_clear_headers() ->
    H = get(outh),
    put(outh, #outh{status = H#outh.status,
		    doclose = true,
		    chunked = false,
		    connection  = make_connection_close_header(true)}),
    ok.


outh_set_static_headers(Req, UT, Headers) ->
    outh_set_static_headers(Req, UT, Headers, all).

outh_set_static_headers(Req, UT, Headers, Range) ->
    H = get(outh),
    FIL = (UT#urltype.finfo)#file_info.size,
    {DoClose0, Chunked0} = dcc(Req, Headers),
    {DoDeflate, Length} 
	= case Range of 
	      all ->
		  case UT#urltype.deflate of
		      DB when binary(DB) -> % cached
			  case accepts_gzip(Headers, UT#urltype.mime) of
			      true -> {true, size(DB)};
			      false -> {false, FIL}
			  end;
		      undefined -> {false, FIL};
		      dynamic ->
			  case accepts_gzip(Headers, UT#urltype.mime) of
			      true ->
				  {true, undefined};
			      false ->
				  {false, FIL}
			  end
		  end;
	      {fromto, From, To, _} ->
		  {false, To - From + 1}
	  end,
    ContentEncoding = case DoDeflate of
			  true -> deflate;
			  false -> identity
		      end,
    Chunked = Chunked0 and (Length == undefined),
    DoClose =
	if
	    DoClose0 == true ->
		true;
	    ((Length == undefined) and not Chunked) ->
						% We cannot keep the
						% connection alive,
						% because the client
						% has no way of
						% knowing the end of
						% the content data.
		true;
	    DoClose0 == keep_alive ->
		keep_alive;
	    true ->
		DoClose0
	end,

    H2 = H#outh{
	   status = case Range of
			all -> 200;
			{fromto, _, _, _} -> 206
		    end,
	   chunked = Chunked,
	   encoding = ContentEncoding,
	   date = make_date_header(),
	   server = make_server_header(),
	   last_modified = make_last_modified_header(UT#urltype.finfo),
	   etag = make_etag_header(UT#urltype.finfo),
	   content_range = make_content_range_header(Range),
	   content_length = make_content_length_header(Length),
	   content_type = make_content_type_header(UT#urltype.mime),
	   content_encoding = make_content_encoding_header(ContentEncoding),
	   transfer_encoding = make_transfer_encoding_chunked_header(Chunked),
	   connection  = make_connection_close_header(DoClose),
	   doclose = DoClose,
	   contlen = Length
	  },
    put(outh, H2).

outh_set_304_headers(Req, UT, Headers) ->
     H = get(outh),
     {DoClose, _Chunked} = dcc(Req, Headers),
     H2 = H#outh{
          status = 304,
          chunked = false,
          date = make_date_header(),
          server = make_server_header(),
          last_modified = make_last_modified_header(UT#urltype.finfo),
          etag = make_etag_header(UT#urltype.finfo),
          content_length = make_content_length_header(0),
          connection  = make_connection_close_header(DoClose),
          doclose = DoClose,
          contlen = 0
              },
     put(outh, H2).

outh_set_dyn_headers(Req, Headers, UT) ->
    H = get(outh),
    {DoClose, Chunked} = dcc(Req, Headers),
    H2 = H#outh{
	   status = 200,
	   date = make_date_header(),
	   server = make_server_header(),
	   connection = make_connection_close_header(DoClose),
	   content_type = make_content_type_header(UT#urltype.mime),
	   doclose = DoClose,
	   chunked = Chunked,
	   transfer_encoding = 
	       make_transfer_encoding_chunked_header(Chunked)},
    
    put(outh, H2).


outh_set_connection(What) ->
    H = get(outh),
    H2 = H#outh{connection = make_connection_close_header(What),
		doclose = What},
    put(outh, H2),
    ok.


outh_set_content_length(Int) ->
    H = get(outh),
    H2 = H#outh{content_length = make_content_length_header(Int),
		contlen=Int},
    put(outh, H2).



outh_set_dcc(Req, Headers) ->
    H = get(outh),
    {DoClose, Chunked} = dcc(Req, Headers),
    H2 = H#outh{connection = make_connection_close_header(DoClose),
		doclose = DoClose,
		chunked = Chunked,
		transfer_encoding = 
		make_transfer_encoding_chunked_header(Chunked)},
    put(outh, H2).


%% can only turn if off, not on.
%% if it allready is off, it's off because the cli headers forced us.

outh_set_transfer_encoding_off() ->
    H = get(outh),
    H2 = H#outh{chunked = false,
		transfer_encoding = 
		make_transfer_encoding_chunked_header(false)},
    put(outh, H2).


outh_fix_doclose() ->
    H = get(outh),
    if
	(H#outh.doclose /= true) and (H#outh.contlen==undefined) and
	(H#outh.chunked == false) ->
	    put(outh, H#outh{doclose=true,
			     connection=make_connection_close_header(true)});
	true -> ok
    end.
	    

dcc(Req, Headers) ->
    DoClose = case Req#http_request.version of
		  {1, 0} -> 
		      case Headers#headers.connection of
			  "close" -> true;
			  "Keep-Alive" -> keep_alive;
			  _ -> true                                          
		      end;              
		  {1, 1} ->
		      Headers#headers.connection == "close";
		  {0,9} ->
		      true
	      end,
    Chunked = case Req#http_request.version of
		  {1, 0} ->
		      false;
		  {1,1} ->
		      true;
		  {0,9} ->
		      false
	      end,
    {DoClose, Chunked}.


		  


%%
%% The following all make_ function return an actual header string
%%

make_allow_header() ->
    "Allow: GET, POST, PUT, OPTIONS, HEAD\r\n".
make_server_header() ->
    ["Server: Yaws/", yaws_vsn:version(), " Yet Another Web Server\r\n"].



make_last_modified_header(FI) ->
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
    ["Last-Modified: ", yaws:local_time_as_gmt_string(Then), "\r\n"].



make_location_header(Where) ->
    ["Location: ", Where, "\r\n"].


make_etag_header(FI) ->
    ETag = make_etag(FI),
    ["Etag: ", ETag, "\r\n"].

make_etag(FI) ->
    {{Y,M,D}, {H,Min, S}}  = FI#file_info.mtime,
    Inode = FI#file_info.inode,
    pack_bin( <<0:6,(Y band 2#11111111):8,M:4,D:5,H:5,Min:6,S:6,Inode:32>> ).

pack_bin(<<_:6,A:6,B:6,C:6,D:6,E:6,F:6,G:6,H:6,I:6,J:6,K:6>>) ->
    [$",
     pc(A),pc(B),pc(C),pc(D),pc(E),pc(F),pc(G),pc(H),pc(I),pc(J),pc(K),
     $"].

%% Like Base64 for no particular reason.
pc(X) when X >= 0, X < 26 -> 
    X + $A;
pc(X) when X >= 26, X < 52 -> 
    X - 26 + $a;
pc(X) when X >= 52, X < 62 ->
    X - 52 + $0;
pc(62) ->
    $+;
pc(63) ->
    $/.



make_content_type_header(no_content_type) ->
    undefined;
make_content_type_header(MimeType) ->
    ["Content-Type: ", MimeType, "\r\n"].


make_content_range_header(all) ->
    undefined;
make_content_range_header({fromto, From, To, Tot}) ->
    ["Content-Range: bytes ", 
     integer_to_list(From), $-, integer_to_list(To),
     $/, integer_to_list(Tot), $\r, $\n].

make_content_length_header(Size) when integer(Size) ->
    ["Content-Length: ", integer_to_list(Size), "\r\n"];
make_content_length_header(FI) when record(FI, file_info) ->
    Size = FI#file_info.size,
    ["Content-Length: ", integer_to_list(Size), "\r\n"];
make_content_length_header(_) ->
    undefined.

make_content_encoding_header(identity) ->
    undefined;
make_content_encoding_header(deflate) ->
    "Content-Encoding: gzip\r\n".

make_connection_close_header(true) ->
    "Connection: close\r\n";
make_connection_close_header(false) ->
    undefined;
make_connection_close_header(keep_alive) ->
    "Connection: Keep-Alive\r\n".

make_transfer_encoding_chunked_header(true) ->
    "Transfer-Encoding: chunked\r\n";
make_transfer_encoding_chunked_header(false) ->
    undefined.

make_www_authenticate_header(Realm) ->
    ["WWW-Authenticate: Basic realm=\"", Realm, ["\"\r\n"]].


make_date_header() ->
    N = element(2, now()),
    case get(date_header) of
	{_Str, Secs} when (Secs+10) < N ->
	    H = ["Date: ", yaws:universal_time_as_string(), "\r\n"],
	    put(date_header, {H, N}),
	    H;
	{Str, _Secs} ->
	    Str;
	undefined ->
	    H = ["Date: ", yaws:universal_time_as_string(), "\r\n"],
	    put(date_header, {H, N}),
	    H
    end.



%% access functions into the outh record

outh_get_status_code() ->
    (get(outh))#outh.status.

outh_get_contlen() ->
    (get(outh))#outh.contlen.

outh_get_act_contlen() ->
    (get(outh))#outh.act_contlen.

outh_inc_act_contlen(Int) ->
    O = get(outh),
    L = case O#outh.act_contlen of
	    undefined ->
		Int;
	    Len ->    
		Len+Int
	end,
    put(outh, O#outh{act_contlen = L}),
    L.

outh_get_doclose() ->
    (get(outh))#outh.doclose.

outh_get_chunked() ->
    (get(outh))#outh.chunked.

outh_get_content_encoding() ->
    (get(outh))#outh.encoding.
    
outh_get_content_encoding_header() ->
    (get(outh))#outh.content_encoding.

outh_get_content_type() ->    
    case (get(outh))#outh.content_type of
	[_, Mime, _] ->
	    Mime
    end.

outh_serialize() ->
    H = get(outh),
    Code = case H#outh.status of
	       undefined -> 200;
	       Int -> Int
	   end,
    StatusLine = ["HTTP/1.1 ", integer_to_list(Code), " ",
		  yaws_api:code_to_phrase(Code), "\r\n"],
    GC=get(gc),
    if ?gc_has_debug(GC) ->
	    yaws_debug:check_headers(H);
       true ->
	    ok
    end,
    case H#outh.content_encoding of
	undefined ->
	    ContentEncoding = make_content_encoding_header(H#outh.encoding);
	ContentEncoding ->
	    ok
    end,	
    Headers = [noundef(H#outh.connection),
	       noundef(H#outh.server),
	       noundef(H#outh.location),
	       noundef(H#outh.cache_control),
	       noundef(H#outh.date),
	       noundef(H#outh.allow),
	       noundef(H#outh.last_modified),
	       noundef(H#outh.etag),
	       noundef(H#outh.content_range),
	       noundef(H#outh.content_length),
	       noundef(H#outh.content_type),
	       noundef(ContentEncoding),
	       noundef(H#outh.set_cookie),
	       noundef(H#outh.transfer_encoding),
	       noundef(H#outh.www_authenticate),
	       noundef(H#outh.other)
	      ],
    {StatusLine, Headers}.
    
	       
noundef(undefined) ->
    [];
noundef(Str) ->
    Str.

	

accumulate_header({X, erase}) when atom(X) ->
    erase_header(X);

%% special headers

accumulate_header({connection, What}) ->
    DC = case What of
	     "close" ->
		 true;
	     _ ->
		 false
	 end,
    H = get(outh),
    put(outh, (H#outh{connection = ["Connection: ", What, "\r\n"],
		      doclose = DC}));
accumulate_header({"Connection", What}) ->
    accumulate_header({connection, What});

accumulate_header({location, What}) ->
    put(outh, (get(outh))#outh{location = ["Location: " , What, "\r\n"]});
accumulate_header({"Location", What}) ->
    accumulate_header({location, What});

accumulate_header({cache_control, What}) ->
    put(outh, (get(outh))#outh{cache_control = ["Cache-Control: " , What, "\r\n"]});
accumulate_header({"Cache-Control", What}) ->
    accumulate_header({cache_control, What});

accumulate_header({set_cookie, What}) ->
    O = get(outh),
    Old = case O#outh.set_cookie of
	      undefined -> "";
	      X -> X
	  end,
    put(outh, O#outh{set_cookie = ["Set-Cookie: " , What, "\r\n"|Old]});
accumulate_header({"Set-Cookie", What}) ->
    accumulate_header({set_cookie, What});

accumulate_header({content_type, What}) ->
    put(outh, (get(outh))#outh{content_type = ["Content-Type: " , What, "\r\n"]});
accumulate_header({"Content-Type", What}) ->
    accumulate_header({content_type, What});

accumulate_header({content_encoding, What}) ->
    put(outh, (get(outh))#outh{content_encoding = 
			       ["Content-Encoding: " , What, "\r\n"]});
accumulate_header({"Content-Encoding", What}) ->
    accumulate_header({content_encoding, What});

accumulate_header({content_length, Len}) when integer(Len) ->
    H = get(outh),
    put(outh, H#outh{
		chunked = false,
		transfer_encoding = undefined,
		contlen = Len,
		content_length = make_content_length_header(Len)});
accumulate_header({"Content-Length", Len}) ->
    case Len of
	I when integer(I) ->
	    accumulate_header({content_length, I});
	L when list(L) ->
	    accumulate_header({content_length, list_to_integer(L)})
    end;

%% non-special headers (which may be special in a future Yaws version)

accumulate_header({Name, What}) when list(Name) ->
    H = get(outh),
    Old = case H#outh.other of
	      undefined ->
		  [];
	      V ->
		  V
	  end,
    H2 = H#outh{other = [Name, ": ", What, "\r\n", Old]},
    put(outh, H2);



%% backwards compatible clause
accumulate_header(Data) when list(Data) ->
    Str = lists:flatten(Data),
    accumulate_header(split_header(Str)).

split_header(Str) ->
    split_header(Str, []).
split_header([], A) ->
    {lists:reverse(A), ""};
split_header([$:, $ |W], A) ->
    {lists:reverse(A), W};
split_header([$:|W], A) ->
    {lists:reverse(A), W};
split_header([C|S], A) ->
    split_header(S, [C|A]).


erase_header(connection) ->
    put(outh, (get(outh))#outh{connection = undefined});
erase_header(cache_control) ->
    put(outh, (get(outh))#outh{cache_control = undefined});
erase_header(set_cookie) ->
    put(outh, (get(outh))#outh{set_cookie = undefined});
erase_header(content_type) ->
    put(outh, (get(outh))#outh{content_type = undefined});
erase_header(content_encoding) ->
    put(outh, (get(outh))#outh{content_encoding = undefined});
erase_header(location) ->
    put(outh, (get(outh))#outh{location = undefined}).






	
setuser(undefined) ->	     
    ignore;
setuser(User) ->	     
    erl_ddll:load_driver(filename:dirname(code:which(?MODULE)) ++ 
			 "/../priv/", "setuid_drv"),
    P = open_port({spawn, "setuid_drv " ++ [$s|User]}, []),
    receive
	{P, {data, "ok " ++ IntList}} ->
	    {ok, IntList}
    end.

getuid() ->
    case os:type() of
	{win32, _} ->
	    {ok, "XXX"};
	_ ->
	    erl_ddll:load_driver(filename:dirname(code:which(?MODULE)) ++ 
				 "/../priv/", "setuid_drv"),
	    P = open_port({spawn, "setuid_drv g"},[]),
	    receive
		{P, {data, "ok " ++ IntList}} ->
		    {ok, IntList}
	    end
    end.


idu(User) ->
    erl_ddll:load_driver(filename:dirname(code:which(?MODULE)) ++ 
			 "/../priv/", "setuid_drv"),
    P = open_port({spawn, "setuid_drv " ++ [$u|User]}, []),
    receive
	{P, {data, "ok " ++ IntList}} ->
	    {ok, IntList}
    end.

user_to_home(User) ->
    erl_ddll:load_driver(filename:dirname(code:which(?MODULE)) ++ 
			 "/../priv/", "setuid_drv"),
    P = open_port({spawn, "setuid_drv " ++ [$h|User]}, []),
    receive
	{P, {data, "ok " ++ Home}} ->
	    Home
    end.

uid_to_name(Uid) ->
    erl_ddll:load_driver(filename:dirname(code:which(?MODULE)) ++ 
			 "/../priv/", "setuid_drv"),
    P = open_port({spawn, "setuid_drv " ++ [$n|integer_to_list(Uid)]}, []),
    receive
	{P, {data, "ok " ++ Name}} ->
	    Name
    end.

tmp_dir() ->
    case os:type() of
	{win32,_} ->
	    "c:/winnt/temp";
	_ -> 
	    "/tmp"
    end.


% Try to create a tmp directory in the current directory.
% In case of any problem terminate yaws.
%
create_tmp_dir() ->
    ErrorMessage = "TEMP, and TMP variables undefined. ",
    Dir = case file:get_cwd() of
	      {ok, CurDir} ->
		  CurDir ++ "/tmp";
	      Err ->
		  io:format(
		    ErrorMessage++"Canont access current directory; error: ~p~n",
		    [Err]),
		  init:stop()
	  end,
    case file:make_dir(Dir) of
	ok -> Dir;
	{error, eexist} ->
	    case file:read_file_info("tmp") of
		{ok, FI} when FI#file_info.type==directory ->
		    Dir;
		_ ->
		    io:format(
		      ErrorMessage++"Canont access directory ~p~n",
		      [Dir]),
		    init:stop()
	    end;
	Error ->
	    io:format(
	      ErrorMessage++"Canont create directory ~p, Error: ~p~n",
	      [Dir,Error]),
	    init:stop()
    end.

%
% String represeting temporary directory that
% can be used for format functions (io,io_lib).
% WIN32 path can contain $~, so they are doubled.
%
tmp_dir_fstr() ->
    lists:foldr(fun($~, Str) -> [$~, $~ | Str];
		   (C , Str) -> [C | Str]
		end,
		[],
		tmp_dir()).

exists(F) ->
    case file:open(F, [read, raw]) of
	{ok, Fd} ->
	    file:close(Fd),
	    ok;
	_ ->
	    false
    end.


%%
%%
%% http/tcp send receive functions
%%
%%


do_recv(Sock, Num, nossl) ->
    gen_tcp:recv(Sock, Num, ?READ_TIMEOUT);
do_recv(Sock, Num, ssl) ->
    case erase(ssltrail) of %% hack from above ...
	undefined ->
	    split_recv(ssl:recv(Sock, 0), Num);   %% ignore Num val ??? TO ??
	Bin ->
	    split_recv({ok, Bin}, Num)
    end.

%% weird ... ssl module doesn't respect Num arg for binary socks
split_recv({ok, B}, Num) when integer(Num) ->
    case B of
	<<Bin:Num/binary , Tail/binary>> ->
	    put(ssltrail, Tail),
	    {ok, Bin};
	_ ->
	    {ok, B}
    end;
split_recv(E, _) ->
    E.


	
cli_recv(S, Num, SslBool) ->
    Res = do_recv(S, Num, SslBool),
    cli_recv_trace((get(gc))#gconf.trace, Res),
    Res.

cli_recv_trace(false, _) -> ok;
cli_recv_trace(Trace, Res) ->
    case Res of
	{ok, Val} when tuple(Val) ->
	    yaws_log:trace_traffic(from_client, ?F("~p~n", [Val]));
	{error, What} ->
	    yaws_log:trace_traffic(from_client, ?F("~n~p~n", [What]));
	{ok, http_eoh} ->
	    ok;
	{ok, Val} when Trace == {true, traffic} ->
	    yaws_log:trace_traffic(from_client, Val);
	_ ->
	    ok
    end.



gen_tcp_send(S, Data) ->
    Res = case (get(sc))#sconf.ssl of
	      undefined ->
		  gen_tcp:send(S, Data);
	      _SSL ->
		  ssl:send(S, Data)
	  end,
    case ?gc_has_debug((get(gc))) of
	false ->
	    case Res of
		ok ->
		    ok;
		Err ->
		    erlang:fault(Err)
	    end;
	true ->
	    case Res of
		ok ->
		    ?Debug("Sent ~p~n", [yaws_debug:nobin(Data)]),
		    ok;
		Err ->
		    {B2, Size} = strip(Data),
		    yaws_debug:derror("Failed to send ~w bytes:~n~p "
				      "on socket ~p: ~p~n~p~n",
				      [Size, B2, S, Err,
				       yaws_debug:nobin(Data)]),
		    erlang:fault(Err)
	    end
    end.


strip(Data) ->
    L = list_to_binary([Data]),
    case L of
	<<Head:50/binary, _/binary>> ->
	    {binary_to_list(<<Head/binary, ".....">>), size(L)};
	_ ->
	    {binary_to_list(L), size(L)}
    end.
	 


%% This is the api function
%% return {Req, Headers}
%%     or closed

http_get_headers(CliSock, SSL) ->
    Res = 
	case SSL of
	    ssl ->
		case yaws_ssl:ssl_get_headers(CliSock) of
		    {Req, H, Trail} ->
			case get(ssltrail) of   %% hack hack hack
			    undefined ->
				put(ssltrail, Trail);
			    B ->
				put(ssltrail, <<Trail/binary,B/binary>>)
			end,
			{Req, H};
		    R -> 
			R
		end;
	    nossl ->
		nossl_http_get_headers(CliSock)
	end,
    GC = get(gc),
    if
	GC#gconf.trace == false ->
	    Res;
	tuple(Res) ->
	    {Request, Headers} = Res,
	    ReqStr = yaws_api:reformat_request(Request),
	    HStr = lists:map(
		     fun(H) -> [H, "\r\n"] end,
		     yaws_api:reformat_header(Headers)),
	    yaws_log:trace_traffic(from_client, 
				   ?F("~n~s~n~s~n",[ReqStr, HStr])),
	    Res;
	Res == closed ->
	    yaws_log:trace_traffic(from_client, "closed\n"),
	    closed
    end.


headers_to_str(Headers) ->
    lists:map(
      fun(H) -> [H, "\r\n"] end,
      yaws_api:reformat_header(Headers)).
	


nossl_http_get_headers(CliSock) ->
    inet:setopts(CliSock, [{packet, http}]),
    case http_recv_request(CliSock) of
	bad_request ->
	    {#http_request{method=bad_request, version={0,9}},
	     #headers{}};
	closed -> 
	    closed;
	R -> 
	    H = http_get_headers(CliSock, R,  #headers{}),
	    {R, H}
    end.


http_recv_request(CliSock) ->
    case do_recv(CliSock, 0,  nossl) of
	{ok, R} when record(R, http_request) ->
	    R;
	{ok, R} when record(R, http_response) ->
	    R;
	{error, {http_error, "\r\n"}} ->
	    http_recv_request(CliSock);
	{error, {http_error, "\n"}} ->
	    http_recv_request(CliSock);
	{error, {http_error, _}} ->
	    bad_request;
	{error, closed} -> closed;
	{error, timeout} -> closed;
	_Other ->
	    ?Debug("Got ~p~n", [_Other]),
	    exit(normal)
    end.


		  
http_get_headers(CliSock, Req, H) ->
    case do_recv(CliSock, 0, nossl) of
	{ok, {http_header,  _Num, 'Host', _, Host}} ->
	    http_get_headers(CliSock, Req, H#headers{host = Host});
	{ok, {http_header, _Num, 'Connection', _, Conn}} ->
	    http_get_headers(CliSock, Req, H#headers{connection = Conn});
	{ok, {http_header, _Num, 'Accept', _, Accept}} ->
	    http_get_headers(CliSock, Req, H#headers{accept = Accept});
	{ok, {http_header, _Num, 'If-Modified-Since', _, X}} ->
	    http_get_headers(CliSock, Req,  
			     H#headers{if_modified_since = X});
	{ok, {http_header, _Num, 'If-Match', _, X}} ->
	    http_get_headers(CliSock, Req, H#headers{if_match = X});
	{ok, {http_header, _Num, 'If-None-Match', _, X}} ->
	    http_get_headers(CliSock, Req, H#headers{if_none_match = X});
	{ok, {http_header, _Num, 'If-Range', _, X}} ->
	    http_get_headers(CliSock, Req, H#headers{if_range = X});
	{ok, {http_header, _Num, 'If-Unmodified-Since', _, X}} ->
	    http_get_headers(CliSock, Req,  
			H#headers{if_unmodified_since = X});
	{ok, {http_header, _Num, 'Range', _, X}} ->
	    http_get_headers(CliSock, Req, H#headers{range = X});
	{ok, {http_header, _Num, 'Referer',_, X}} ->
	    http_get_headers(CliSock, Req, H#headers{referer = X});
	{ok, {http_header, _Num, 'User-Agent', _, X}} ->
	    http_get_headers(CliSock, Req, H#headers{user_agent = X});
	{ok, {http_header, _Num, 'Accept-Ranges', _, X}} ->
	    http_get_headers(CliSock, Req, H#headers{accept_ranges = X});
	{ok, {http_header, _Num, 'Cookie', _, X}} ->
	    http_get_headers(CliSock, Req,  
			H#headers{cookie = [X|H#headers.cookie]});
	{ok, {http_header, _Num, 'Keep-Alive', _, X}} ->
	    http_get_headers(CliSock, Req, H#headers{keep_alive = X});
	{ok, {http_header, _Num, 'Content-Length', _, X}} ->
	    http_get_headers(CliSock, Req, H#headers{content_length = X});
	{ok, {http_header, _Num, 'Content-Type', _, X}} ->
	    http_get_headers(CliSock, Req, H#headers{content_type = X});
	{ok, {http_header, _Num, 'Transfer-Encoding', _, X}} ->
	    http_get_headers(CliSock, Req, H#headers{transfer_encoding=X});
	{ok, {http_header, _Num, 'Location', _, X}} ->
	    http_get_headers(CliSock, Req, H#headers{location=X});
	{ok, {http_header, _Num, 'Authorization', _, X}} ->
	    http_get_headers(CliSock, Req,  
			H#headers{authorization = parse_auth(X)});

	{ok, http_eoh} ->
	    H;

	%% these are here to be a little forgiving to
	%% bad (typically test script) clients

	{error, {http_error, "\r\n"}} ->
	     http_get_headers(CliSock, Req, H);
	{error, {http_error, "\n"}} ->
	     http_get_headers(CliSock, Req, H);

	%% auxilliary headers we don't have builtin support for
	{ok, X} ->
	    ?Debug("OTHER header ~p~n", [X]),
	    http_get_headers(CliSock, Req,  
			     H#headers{other=[X|H#headers.other]});
	_Err ->
	    exit(normal)
    
    end.



	
parse_auth(Orig = "Basic " ++ Auth64) ->
    case decode_base64(Auth64) of
	{error, _Err} ->
	    undefined;
	Auth ->
	    case string:tokens(Auth, ":") of
		[User, Pass] ->
		    {User, Pass, Orig};
		_ ->
		    undefined
	    end
    end;
parse_auth(_) ->
    undefined.



decode_base64([]) ->
  [];
decode_base64([Sextet1,Sextet2,$=,$=|Rest]) ->
  Bits2x6=
    (d(Sextet1) bsl 18) bor
    (d(Sextet2) bsl 12),
  Octet1=Bits2x6 bsr 16,
  [Octet1|decode_base64(Rest)];
decode_base64([Sextet1,Sextet2,Sextet3,$=|Rest]) ->
  Bits3x6=
    (d(Sextet1) bsl 18) bor
    (d(Sextet2) bsl 12) bor
    (d(Sextet3) bsl 6),
  Octet1=Bits3x6 bsr 16,
  Octet2=(Bits3x6 bsr 8) band 16#ff,
  [Octet1,Octet2|decode_base64(Rest)];
decode_base64([Sextet1,Sextet2,Sextet3,Sextet4|Rest]) ->
  Bits4x6=
    (d(Sextet1) bsl 18) bor
    (d(Sextet2) bsl 12) bor
    (d(Sextet3) bsl 6) bor
    d(Sextet4),
  Octet1=Bits4x6 bsr 16,
  Octet2=(Bits4x6 bsr 8) band 16#ff,
  Octet3=Bits4x6 band 16#ff,
  [Octet1,Octet2,Octet3|decode_base64(Rest)];
decode_base64(_CatchAll) ->
  {error, bad_base64}.

d(X) when X >= $A, X =<$Z ->
    X-65;
d(X) when X >= $a, X =<$z ->
    X-71;
d(X) when X >= $0, X =<$9 ->
    X+4;
d($+) -> 62;
d($/) -> 63;
d(_) -> 63.




slash_append("/", [$/|T]) ->
    [$/|T];
slash_append("/", T) ->
    [$/|T];
slash_append([], [$/|T]) ->
    [$/|T];
slash_append([], T) ->
    [$/|T];
slash_append([H|T], X) ->
    [H | slash_append(T,X)].




%% aux help function
uid_change_files(GC, Dir, Files) ->
    case GC#gconf.username of
	undefined ->  
	    %% uid change feature not used
	    ok;
	_Uname when GC#gconf.uid /= "0" ->
	    %% we're not root and can't do anything about the sitiation
	    ok;
	Uname ->
	    case (catch list_to_integer(element(2,yaws:idu(Uname)))) of
		Int when integer(Int) ->
		    file:change_owner(Dir, Int),
		    lists:foreach(
		      fun(FN) ->
			      F=filename:join([Dir,FN]),
			      case file:change_owner(F, Int) of
				  ok -> ok;
				  {error, Rsn} ->
				      error_logger:format("Failed to chown "
							  "~p: ~p",
							  [F, Rsn]),
				      erlang:fault(Rsn)
			      end
		      end, Files);
		Err ->
		    error_logger:format("Bad username ~p cannot get "
					"numeric uid~n", [Uname]),
		    erlang:fault(Err)
	    end
    end.



flag(CurFlag, Bit, true) ->
    CurFlag bor Bit;
flag(CurFlag, Bit, false) ->
    CurFlag band (bnot Bit).



%% misc debug funcs .... use from cli only
restart() ->
    stop(),
    load(),
    start().


modules() ->
    application:load(yaws),
    M = case application:get_all_key(yaws) of
	    {ok, L} ->
		case lists:keysearch(modules, 1, L) of
		    {value, {modules, Mods}} ->
			Mods;
		    _ ->
			[]
		end;
	    _ ->
		[]
	end,
    M.


load() ->
    load(modules()).
load(M) ->
    lists:foreach(fun(Mod) ->
			  ?Debug("Load ~p~n", [Mod]),
			  c:l(Mod)
		  end, M).



upto_char(Char, [Char|_]) ->
    [];
upto_char(Char, [H|T]) when integer(H) ->
    [H|upto_char(Char, T)];
upto_char(_, []) ->
    [];
%% deep lists
upto_char(Char, [H|T]) when list(H) ->
    case lists:member(Char ,H) of
	true ->
	    upto_char(Char, H);
	false ->
	    [H, upto_char(Char, T)]
    end.



%% map over deep list and maintain
%% list structure as is

deepmap(Fun, [H|T]) when list(H) ->
    [deepmap(Fun, H) | deepmap(Fun, T)];
deepmap(Fun, [H|T]) ->
    [Fun(H) | deepmap(Fun,T)];
deepmap(_Fun, []) ->
    [].

    
sconf_to_srvstr(SC) ->
    redirect_scheme(SC) ++
	redirect_host(SC,undefined) ++
	redirect_port(SC).

redirect_scheme(SC) ->
    case {SC#sconf.ssl,SC#sconf.rmethod} of
	{_, Method} when list(Method) ->
	    Method++"://";
	{undefined,_} ->
	    "http://";
	{_SSl,_} ->
	    "https://"
    end.    

redirect_host(SC, HostHdr) ->
    case SC#sconf.rhost of
	undefined ->
	    if HostHdr == undefined ->
		    ServerName = SC#sconf.servername,
		    SnameNoPort = 
			case string:chr(ServerName, $:) of
			    0 ->
				ServerName;
			    N ->
				lists:sublist(ServerName, N-1)
			end,
		    SnameNoPort ++ redirect_port(SC);
	       true ->
		    HostHdr
	    end;
	_ ->
	    SC#sconf.rhost
    end.

redirect_port(SC) ->
    case {SC#sconf.rmethod, SC#sconf.ssl, SC#sconf.port} of
	{"https", _, 443} -> "";
	{"http", _, 80} -> "";
	{_, undefined, 80} -> "";
	{_, undefined, Port} -> 
               [$:|integer_to_list(Port)];
	{_, _SSL, 443} ->
               "";
	{_, _SSL, Port} -> 
               [$:|integer_to_list(Port)]
    end.    

redirect_scheme_port(SC) ->
    Scheme = redirect_scheme(SC),
    PortPart = redirect_port(SC),
    {Scheme, PortPart}.

