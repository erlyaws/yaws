%%%----------------------------------------------------------------------
%%% File    : yaws_debug.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created :  7 Feb 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_debug).
-author('klacke@hyber.org').
-compile(export_all).
-include("yaws.hrl").


typecheck([{record, Rec, X} | Tail], File, Line) when atom(X),
						      element(1, Rec) == X ->
    typecheck(Tail, File, Line);
typecheck([{int, Int} |Tail], File, Line) when integer(Int) ->
    typecheck(Tail, File, Line);
typecheck([Err|_], File, Line) ->
    io:format(user, "TC ERROR ~s:~w:~n~p",
	      [File, Line, Err]),
    exit(tcerr);
typecheck([], _,_) ->
    ok.






%% returns {record, RecName, [Field1, Val1} .....]
format_record(Record, Name, Fields) ->
    case tuple_to_list(Record) of
	[Name | Rest] ->
	    {record, Name, format_record(Rest, Fields)};
	_X ->
	    ?Debug("Bad record ~p is not ~p~n", [_X, Name]),
	    "badrecord"
    end.

format_record([], []) ->
    [];
format_record([Val|Vals], [F|Fs]) ->
    [{F, ?F("~p", [Val])} |
     format_record(Vals, Fs)].






assert(equal,X,Y,_) when X==Y ->
    ok;
assert(neq,X,Y,_) when X/=Y ->
    ok;
assert(integer,X,_,_) when integer(X) ->
    ok;
assert(list,X,_,_) when list(X) ->
    ok;
assert({list,length,equal},X,Y,_) when list(X), length(X)==Y ->
    ok;
assert(greater,X,Y,_) when integer(X), integer(Y), X>Y ->
    ok;
assert(min,X,Y,_) when integer(X), integer(Y), X>=Y ->
    ok;
assert(lesser,X,Y,_) when integer(X), integer(Y), X<Y ->
    ok;
assert(max,X,Y,_) when integer(X), integer(Y), X=<Y ->
    ok;
assert(interval,X,{Min,Max},_) when integer(X), integer(Min), 
				    integer(Max), 
				    X>=Min, Max>=X ->
    ok;
assert('fun', Fun, _, Failure) ->
    case catch Fun() of
	true -> ok;
	_Other -> fail(Failure)
    end;

assert(in,X,L,Failure) when list(L) ->
    case lists:member(X,L) of
	true -> ok;
	_ -> fail(Failure)
    end;

assert(_,_,_,Failure) ->
    fail(Failure).

fail({assert,File,Line,Message}) ->
    io:format(user, "Assertion FAILED ~p:~p, pid ~w exiting: ~p~n",
	      [File, Line, self(), Message]),
    exit(assertion_failed);
fail({alert,File,Line,Message}) ->
    io:format(user, "Assert WARNING ~p:~p, pid ~w: ~p~n",
	      [File, Line, self(), Message]),
    ok;
fail({{debug,Fstr}, File,Line,Fmt, Args}) ->
    Str = lists:flatten(
	    io_lib:format("~s <~p> ~s:~p, pid ~w: ~n",
			  [Fstr, node(), filename:basename(File), 
			   Line, self()])),
    
    case io:format(user, Str ++ Fmt ++ "~n", Args) of
	ok -> ok;
	_ -> io:format(user, "ERROR ~p:~p: Pid ~w: (bad format)~n~p,~p~n",
		      [File, Line, self(), Fmt, Args]),
	    
	    ok
    end;

fail({format, File,Line,Fmt,Args}) ->
    case io:format(user, Fmt,Args) of
	ok -> ok;
	_ ->
	    io:format(user, "ERROR ~p:~p: Pid ~w: (bad format)~n~p,~p~n",
		      [File, Line, self(), Fmt, Args]),
	    
	    ok
    end.




format(GC, F, A) ->
    if
	GC#gconf.debug == true ->
	    io:format(F, A);
	true ->
	    ok
    end.
