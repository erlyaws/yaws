%%%----------------------------------------------------------------------
%%% File    : yaws_debug.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created :  7 Feb 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_debug).
-author('klacke@hyber.org').
-compile(export_all).


-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include("yaws_debug.hrl").


typecheck([{record, Rec, X} | Tail], File, Line) when atom(X),
						      element(1, Rec) == X ->
    typecheck(Tail, File, Line);
typecheck([{int, Int} |Tail], File, Line) when integer(Int) ->
    typecheck(Tail, File, Line);
typecheck([Err|_], File, Line) ->
    io:format(user, "TC ERROR ~s:~w:~n~p",
	      [File, Line, Err]),
    erlang:fault(tcerr);
typecheck([], _,_) ->
    ok.


%% returns {record, RecName, [Field1, Val1} .....]
format_record(Record, Name, Fields) ->
    case tuple_to_list(Record) of
	[Name | Rest] ->
	    io_lib:format("record ~w\n~s", [Name,
					    format_record(Rest, Fields)]);
	_X ->
	    ?Debug("Bad record ~p is not ~p~n", [_X, Name]),
	    "badrecord"
    end.

format_record([], []) ->
    [];
format_record([Val|Vals], [F|Fs]) when is_integer(Val); 
				       Val == []; 
				       atom(Val);
				       is_float(Val)->
    [io_lib:format("     ~w = ~w\n", [F,Val]),
     format_record(Vals, Fs)];
format_record([Val|Vals], [F|Fs]) ->
    case is_string(Val) of
	true ->
	    [io_lib:format("     ~w = ~s\n", [F,Val]),
	     format_record(Vals, Fs)];
	false ->
	    [io_lib:format("     ~w = ~p~n", [F, nobin(Val)]),
	     format_record(Vals, Fs)]
    end.

is_string(L) when list(L) ->
    lists:filter(fun(X) when integer(X),
			     $A < X, X < $z ->
			 false;
		    (_) ->
			 true
		 end,L) == [];
is_string(_) ->
    false.
						    


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
    erlang:fault(assertion_failed);
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


format(F, A)  ->
    case ?gc_has_debug((get(gc))) of
	true ->
	    io:format("yaws:" ++ F, A);
	false ->
	    ok
    end.

derror(F, A) ->
    case ?gc_has_debug((get(gc))) of
	true ->
	    error_logger:error_msg("yaws:" ++ F, A);
	false ->
	    ok
    end.

dinfo(F, A) ->
    case ?gc_has_debug((get(gc))) of
	true ->
	    error_logger:info_msg("yaws:" ++ F, A);
	false ->
	    ok
    end.


mktags() ->
    tags:dirs(["."]),
    init:stop().



xref([Dir]) ->
    io:format("~p~n", [xref:d(Dir)]),
    init:stop().


pids() ->
    lists:zf(
      fun(P) ->
	      case process_info(P) of
		  L when list(L) ->
		      {value, {_, {M1, _,_}}} = 
			  lists:keysearch(current_function, 1, L),
		      {value, {_, {M2, _,_}}} = 
			  lists:keysearch(initial_call, 1, L),
		      S1= atom_to_list(M1),
		      S2 = atom_to_list(M2),
		      case {S1, S2} of
			  {"yaws" ++ _, _} ->
			      {true, P};
			  {_, "yaws"++_} ->
			      {true, P};
			  _ ->
			      false
		      end;
		  _ ->
		      false
	      end
      end,
      processes()).

	      

eprof() ->
    eprof:start(),
    eprof:profile(pids()),
    io:format("Ok run some traffic \n", []).
			  


-define(h_check(H, Field),
	f_check(H#outh.Field, Field)).

f_check(undefined, _Field) ->
    ok;
f_check(Str, Field) ->
    case lists:reverse(lists:flatten(Str)) of
	[$\n, $\r , H | _Tail] ->
	    case lists:member(H, [$\n, $\r]) of
		true ->
		    error_logger:format("Bad <~p> header:~n"
					"  ~p~nToo many newlines",
					[Field, Str]),
		    exit(normal);
		false ->
		    ok
	    end;
	_Other ->
	    error_logger:format("Bad <~p> header:~n"
				"~p~nNot ending with CRNL~n",
				[Field, Str]),
	    exit(normal)
    end.

check_headers(H) ->
    ?h_check(H, connection),
    ?h_check(H, server),
    ?h_check(H, location),
    ?h_check(H, cache_control),
    ?h_check(H, date),
    ?h_check(H, allow),
    ?h_check(H, last_modified),
    ?h_check(H, etag),
    ?h_check(H, content_range),
    ?h_check(H, content_length),
    ?h_check(H, content_encoding),
    ?h_check(H, set_cookie),
    ?h_check(H, transfer_encoding),
    ?h_check(H, www_authenticate),
    check_other(H#outh.other).


check_other(undefined) ->
    ok;
check_other(L0) ->
    L = lists:flatten(L0),
    case lists:dropwhile(fun(X) -> not lists:member(X, ["\r\n"]) end, L) of
	[] ->
	    ok;
	[$\r, $\n, H | _Tail] ->
	     case lists:member(H, [$\n, $\r]) of
		true ->
		     bad_other(L);
		 false ->
		     ok
	     end;
	_Other ->
	    bad_other(L)
    end.


bad_other(L) ->
     Bad = lists:takewhile(
	     fun(X) -> not lists:member(X, ["\r\n"]) end, L),
    error_logger:format("Bad header:~p~n"
			"Too many newlines",
			[Bad]),
    exit(normal).




nobin(X) ->
    case catch xnobin(X) of
	{'EXIT', Reason} ->
	    error_logger:format("~p~n~p~n", [X, Reason]),
	    erlang:fault(Reason);
	Res ->
	    Res
    end.


xnobin(B) when binary(B) ->
    lists:flatten(io_lib:format("#Bin(~w)", [size(B)]));
xnobin(L) when list(L) ->
    lists:map(fun(X) -> xnobin(X) end, L);
xnobin(T) when tuple(T) ->
    list_to_tuple(xnobin(tuple_to_list(T)));
xnobin(X) ->
    X.
