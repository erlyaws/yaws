%%%----------------------------------------------------------------------
%%% File    : mime_type_c.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 10 Jul 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(mime_type_c).
-author('klacke@hyber.org').
-compile(export_all).

%% this module reads the mime.types file and creates
%% the mod mime_types.erl
%% default type is text/plain

compile() ->
    R = (catch c()),
    io:format("~p~n ",[R]),
    erlang:halt().


c() ->
    {ok, F} = file:open("mime.types", [read]),
    io:format("Compiling mime.types ... ~n", []),
    T = ets:new(aa, [set, public]),
    c(F, T, io:get_line(F, '')).

c(F, T, eof) ->
    file:close(F),
    gen(T);
c(F, T, {error, terminated}) ->
    file:close(F),
    gen(T);
c(F, T, Line) ->
    case Line of
	[$#|_] ->
	    c(F, T,  io:get_line(F, ''));
	[$\s|_ ] ->
	    c(F, T,  io:get_line(F, ''));
	L ->
	    case string:tokens(L, [$\s, $\t]) of
		[_] ->  %% uhh single entry ..
		    c(F, T,  io:get_line(F, ''));
		[MimeType | Exts] ->
		    lists:foreach(
		      fun(E) ->
			      ets:insert(T, {E, MimeType}),
			      ets:insert(T, {up(E), MimeType})
		      end, Exts)
	    end
    end,
    c(F, T, io:get_line(F, '')).

up(L) ->
    lists:map(fun(C) -> upper(C) end, L).
upper(C) ->
    if
	C >= $a, C =< $z ->
	    C - ($a - $A);
	true ->
	    C
    end.

nonl([]) ->
    [];
nonl([$\n|T]) ->
    nonl(T);
nonl([H|T]) ->
    [H|nonl(T)].


gen(T) ->
    {ok, Fd} = file:open("mime_types.erl", [write]),
    io:format(Fd, 
	      "-module(mime_types). ~n"
	      "-compile(export_all). ~n", []),
   
    L = lists:sort(ets:tab2list(T)),
    io:format(Fd, "t(\"yaws\") -> {yaws, ~p};~n", ["text/html"]),
    lists:foreach(
      fun({Ext, MT}) ->
	      io:format(Fd, "t(~p) -> {regular, ~p};~n", [nonl(Ext), MT])
      end, L),
    io:format(Fd, "t(_) -> {regular, \"text/plain\"}.~n~n~n", []),



    io:format(Fd, "revt(\"sway\") -> {yaws, ~p};~n", ["text/html"]),
    lists:foreach(
      fun({Ext, MT}) ->
	      io:format(Fd, "revt(~p) -> {regular, ~p};~n", 
			[nonl(lists:reverse(Ext)), MT])
      end, L),
    io:format(Fd, "revt(_) -> {regular, 
                               \"text/plain\"}.~n~n~n", []).

    
		  
		 
			  


		      
