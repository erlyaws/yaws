%%%----------------------------------------------------------------------
%%% File    : yaws.erl
%%% Author  : Claes Wikstrom <klacke@bluetail.com>
%%% Purpose : 
%%% Created : 16 Jan 2002 by Claes Wikstrom <klacke@bluetail.com>
%%%----------------------------------------------------------------------

-module(yaws).
-author('klacke@bluetail.com').
-include ("yaws.hrl").
-include_lib("kernel/include/file.hrl").

-compile(export_all).
-import(lists, [reverse/1, reverse/2]).

%%-export([Function/Arity, ...]).


start() ->
    application:start(yaws).
stop() ->
    application:stop(yaws).

hup(Sock) ->
    spawn(fun() ->
		  group_leader(whereis(user), self()),
		  dohup(Sock)
	  end).

dohup(Sock) ->
    io:format("in dohup~n", []),
    {Debug, Trace, Conf, RunMod, Embed} = yaws_server:get_app_args(),
    Res = (catch case yaws_config:load(Conf, Trace, Debug) of
	{ok, Gconf, Sconfs} ->
	    io:format("Call setconf ~n", []),	  
	    gen_server:call(yaws_server, {setconf, Gconf, Sconfs});
	Err ->
	    Err
    end),
    gen_tcp:send(Sock, io_lib:format("hupped: ~p~n", [Res])),
    gen_tcp:close(Sock).
    

%% use from cli only
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
	    exit({badarg, {?MODULE, date_and_time_to_string, [DAT]}})
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
check_diff([$+, H, M]) when 0 =< H, H < 12, 0 =< M, M < 60 -> true;
check_diff([$-, H, M]) when 0 =< H, H < 12, 0 =< M, M < 60 -> true;
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


lowercase([C|S]) -> [lowercase(C)|S];
lowercase(C) when C>=$A, C=<$Z -> C+32;
lowercase(C) -> C.

%%

uppercase([C|S]) -> [uppercase(C)|S];
uppercase(C) when C>=$a, C=<$z -> C-32;
uppercase(C) -> C.

%%

lowercase_string(String) ->
    lists:map(fun(X) -> lowercase(X) end, String).

%% integer_to_hex

integer_to_hex(I) when I<10 ->
    integer_to_list(I);
integer_to_hex(I) when I<16 ->
    [I-10+$A];
integer_to_hex(I) when I>=16 ->
    N = trunc(I/16),
    integer_to_hex(N) ++ integer_to_hex(I rem 16).

%% hex_to_integer

hex_to_integer(Hex) ->
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
    7;
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
    ticker2(Time, To, Msg).

ticker2(Time, To, Msg) ->
    receive
	{'EXIT', _} ->
	    exit(normal)
    after Time ->
	    To ! Msg
    end,
    ticker2(Time, To, Msg).

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






address(GConf, Sconf) ->
     ?F("<address> ~s Server at ~s:~w </address>",
       [
	GConf#gconf.yaws,
	yaws:fmt_ip(Sconf#sconf.listen),
	Sconf#sconf.port]).




is_space($\s) ->
    true;
is_space($\r) ->
    true;
is_space($\n) ->
    true;
is_space($\r) ->
    true;
is_space(_) ->
    false.



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

