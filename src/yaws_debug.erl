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
    [{F, ?F("~p", [nobin(Val)])} |
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


format(F, A)  ->
    case ?gc_has_debug((get(gc))) of
	true ->
	    io:format(F, A);
	false ->
	    ok
    end.

derror(F, A) ->
    case ?gc_has_debug((get(gc))) of
	true ->
	    error_logger:error_msg(F, A);
	false ->
	    ok
    end.

dinfo(F, A) ->
    case ?gc_has_debug((get(gc))) of
	true ->
	    error_logger:info_msg(F, A);
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
			  


%% not used
check_headers(L) ->
    Hs = string:tokens(lists:flatten(L), "\r\n"),
    io:format("XX ~p~n", [Hs]),
    lists:foreach(
      fun(H) ->
	  case lists:reverse(H) of
	      [_,_,$\r|_] ->
		  error_logger:error_msg("Bad header ~p, it contains"
					      " '\\r' or '\\n' at end ", 
					      [lists:flatten(H)]),
		  exit(normal);
	      [_,_,$\n|_] ->
		  error_logger:error_msg("Bad header ~p, it contains"
					      " '\\r' or '\\n' at end ", 
					      [lists:flatten(H)]),
		  exit(normal);
	      _ ->
		  ok
	  end
      end, Hs).


check_headers([$\r, $\n |Tail], Last) when Tail /= [] ->
    case lists:member(hd(Tail), [$\r, $\n]) of
	true ->
	     error_logger:error_msg("Bad header ~p, it contains"
			     " '\\r' or '\\n' at end ", [lists:reverse(Last)]),
	    exit(normal);
	_ ->
	    check_headers(Tail, [])
    end;

check_headers([H|T], Last) ->
    check_headers(T, [H|Last]);
check_headers([], _) ->
    ok.



nobin(X) ->
    case catch xnobin(X) of
	{'EXIT', Reason} ->
	    io:format("~p~n~p~n", [X, Reason]),
	    exit(Reason);
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
