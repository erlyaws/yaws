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
%%-export([Function/Arity, ...]).


start() ->
    application:start(yaws).
stop() ->
    application:stop(yaws).

hup() ->
    spawn(fun() ->
		  group_leader(whereis(user), self()),
		  stop(),
		  start()
	  end).


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
first(F, []) ->
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
upto(I, []) ->
    [];
upto(0, _) ->
    " ....";
upto(I, [0|_]) ->
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
stringdate_to_datetime([D1, D2, D3, $\,, $ |Tail]) ->
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
	Nums = [X1, X2, X3,X4] ->
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




mktags() ->
    tags:dirs(["."]),
    init:stop().


