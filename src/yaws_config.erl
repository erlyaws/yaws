%%%----------------------------------------------------------------------
%%% File    : yaws_config.erl
%%% Author  : Claes Wikstrom <klacke@bluetail.com>
%%% Purpose : 
%%% Created : 16 Jan 2002 by Claes Wikstrom <klacke@bluetail.com>
%%%----------------------------------------------------------------------

-module(yaws_config).
-author('klacke@bluetail.com').
-include("yaws.hrl").
-include_lib("kernel/include/file.hrl").

-compile(export_all).
%%-export([Function/Arity, ...]).


%% where to look for yaws.conf 
paths() ->
    case os:cmd("id -u") of
	[$0 |_] -> %% root 
	    ["./yaws.conf", 
	     "/etc/yaws.conf"];
	_ -> %% developer
	    [filename:join([os:getenv("HOME"), "yaws.conf"]),
	     "./yaws.conf", 
	     "/etc/yaws.conf"]
    end.



%% load the config

load(false, Trace, Debug) ->
    case yaws:first(fun(F) -> exists(F) end, paths()) of
	false ->
	    {error, "Can't find no config file "};
	{ok, _, File} ->
	    load({file, File}, Trace, Debug)
    end;
load({file, File}, Trace, Debug) ->
    yaws_log:infolog("Using config file ~s", [File]),
    case file:open(File, [read]) of
	{ok, FD} ->
	    GC = make_default_gconf(),
	    R = (catch fload(FD, globals, GC#gconf{file = File,
						   trace = Trace,
						   debug = Debug
						  }, 
			     undefined, 
			     [], 1, io:get_line(FD, ''))),
	    ?Debug("FLOAD: ~p", [R]),
	    case R of
		{ok, GC2, Cs} ->
		    validate_cs(GC2, Cs);
		Err ->
		    ?Debug("Load err: ~p", [Err]),
		    Err
	    end;
	_ ->
	    {error, "Can't open config file" ++ File}
    end.


exists(F) ->
    case file:open(F, [read, raw]) of
	{ok, Fd} ->
	    file:close(Fd),
	    ok;
	_ ->
	    false
    end.



validate_cs(GC, Cs) ->
    L = lists:map(fun(SC) -> {{SC#sconf.listen, SC#sconf.port}, SC} end,Cs),
    L2 = lists:map(fun(X) -> element(2, X) end, lists:sort(L)),
    L3 = arrange(L2, start, [], []),
    ?Debug("Arrange: ~p", [L3]),
    case validate_groups(L3) of
	ok ->
	    {ok, GC, L3};
	Err ->
	    Err
    end.

validate_groups([]) ->
    ok;
validate_groups([H|T]) ->
    case (catch validate_group(H)) of
	ok ->
	    validate_groups(T);
	Err ->
	    Err
    end.

validate_group(List) ->
    
    %% first, max one ssl per group
    io:format("List = ~p~n", [List]),

    case lists:filter(fun(C) ->
			      C#sconf.ssl == on
		      end, List) of
	L when length(L) > 1 ->
	    throw({error, ?F("Max one ssl server per IP: ~p", 
			     [(hd(L))#sconf.servername])});
	_ ->
	    ok
    end,

    %% if we have multiple servers on the same IP, one must be default
    case lists:filter(fun(C) ->
			      C#sconf.default_server_on_this_ip
		      end, List) of  
	L2 when length(List) /= 1, length(L2) > 1 ->
	    throw({error, ?F("Need exactly ONE server which is "
			     "default_server_on_this_ip in server group "
			     "containing ~p", [(hd(List))#sconf.servername])});
	_ ->
	    ok
    end,
    ok.
    

arrange([C|Tail], start, [], B) ->
    arrange(Tail, {in, C}, [C], B);
arrange([], _, [], B) ->
    B;
arrange([], _, A, B) ->
    [A|B];
arrange([C1|Tail], {in, C0}, A, B) ->
    if
	C1#sconf.listen == C0#sconf.listen,
	C1#sconf.port == C0#sconf.port ->
	    arrange(Tail, {in, C0}, [C1|A], B);
	true ->
	    arrange(Tail, {in, C1}, [], [A|B])
    end.



make_default_gconf() ->
    Y = yaws_dir(),
    #gconf{yaws_dir = Y,
	   ebin_dir = [filename:join([Y, "examples/ebin"])],
	   include_dir = [filename:join([Y, "examples/include"])],
	   logdir = ".",
	   yaws = "Yaws 0.2"}.


make_default_sconf() ->
    Y = yaws_dir(),
    #sconf{docroot = filename:join([Y, "www"])}.


yaws_dir() ->
    P = filename:split(code:which(?MODULE)),
    P1 = del_tail(P),
    filename:join(P1).


del_tail([H, ".." |Tail]) ->
    del_tail(Tail);
del_tail([X, Y]) ->
    [];
del_tail([H|T]) ->
    [H|del_tail(T)].



%% two states, global, server
fload(FD, globals, GC, C, Cs, Lno, eof) ->
    file:close(FD),
    {ok, GC, Cs};
fload(FD, _,  GC, C, Cs, Lno, eof) ->
    {error, ?F("Unexpected end of file at line ~w", [Lno])};
 
fload(FD, globals, GC, C, Cs, Lno, Chars) -> 
    %?Debug("Chars: ~s", [Chars]),
    Next = io:get_line(FD, ''),
    case toks(Chars) of
	[] ->
	    fload(FD, globals, GC, C, Cs, Lno+1, Next);

	["trace", '=', Bstr] ->
	    case Bstr of
		"true" ->
		    fload(FD, globals, GC#gconf{trace = true},
			  C, Cs, Lno+1, Next);
		"false" ->
		    fload(FD, globals, GC#gconf{trace = undefined},
			  C, Cs, Lno+1, Next);
		_ ->
		    {error, ?F("Expect bool at line ~w",[Lno])}
	    end;
	
	["logdir", '=', Dir] ->
	    case is_dir(Dir) of
		true ->
		    put(logdir, Dir),
		    fload(FD, globals, GC#gconf{logdir = Dir},
			  C, Cs, Lno+1, Next);
		false ->
		    {error, ?F("Expect directory at line ~w", [Lno])}
	    end;

	["ebin_dir", '=', Dir] ->
	    case is_dir(Dir) of
		true ->
		    fload(FD, globals, GC#gconf{ebin_dir = [Dir|GC#gconf.ebin_dir]},
			  C, Cs, Lno+1, Next);
		false ->
		    {error, ?F("Expect directory at line ~w", [Lno])}
	    end;

	["include_dir", '=', Dir] ->
	    case is_dir(Dir) of
		true ->
		    fload(FD, globals, GC#gconf{include_dir=[Dir|GC#gconf.include_dir]},
			  C, Cs, Lno+1, Next);
		false ->
		    {error, ?F("Expect directory at line ~w", [Lno])}
	    end;
	["keepalive_timeout", '=', Val] ->
	    case (catch list_to_integer(Val)) of
		 I when integer(I) ->
		    fload(FD, globals, GC#gconf{keepalive_timeout = I},
			  C, Cs, Lno+1, Next);
		_ ->
		     {error, ?F("Expect integer at line ~w", [Lno])}
	     end;

	["max_num_cached_files", '=', Val] ->
	    case (catch list_to_integer(Val)) of
		 I when integer(I) ->
		    fload(FD, globals, GC#gconf{max_num_cached_files = I},
			  C, Cs, Lno+1, Next);
		_ ->
		     {error, ?F("Expect integer at line ~w", [Lno])}
	     end;


	["max_num_cached_bytes", '=', Val] ->
	    case (catch list_to_integer(Val)) of
		 I when integer(I) ->
		    fload(FD, globals, GC#gconf{max_num_cached_bytes = I},
			  C, Cs, Lno+1, Next);
		_ ->
		     {error, ?F("Expect integer at line ~w", [Lno])}
	     end;


	["max_size_cached_file", '=', Val] ->
	    case (catch list_to_integer(Val)) of
		 I when integer(I) ->
		    fload(FD, globals, GC#gconf{max_size_cached_file = I},
			  C, Cs, Lno+1, Next);
		_ ->
		     {error, ?F("Expect integer at line ~w", [Lno])}
	     end;



	['<', "server", Server, '>'] ->  %% first server 
	    fload(FD, server, GC, #sconf{servername = Server},
		  Cs, Lno+1, Next);
	[H|_] ->
	    {error, ?F("Unexpected tokens ~p at line ~w", [H, Lno])}
    end;

	
fload(FD, server, GC, C, Cs, Lno, Chars) ->
    %?Debug("Chars: ~s", [Chars]),
    Next = io:get_line(FD, ''),
     case toks(Chars) of
	 [] ->
	     fload(FD, globals, GC, C, Cs, Lno+1, Next);
	 ["port", '=', Val] ->
	     case (catch list_to_integer(Val)) of
		 I when integer(I) ->
		     C2 = C#sconf{port = I},
		     fload(FD, server, GC, C2, Cs, Lno, Next);
		 _ ->
		     {error, ?F("Expect integer at line ~w", [Lno])}
	     end;
	 ["listen", '=', IP] ->
	     case yaws:parse_ip(IP) of
		 error ->
		     {error, ?F("Expect IP address at line ~w:", [Lno])};
		 Addr ->
		     C2 = C#sconf{listen = Addr},
		     fload(FD, server, GC, C2, Cs, Lno, Next)
	     end;
	 ["docroot", '=', Root] ->
	     case is_dir(Root) of
		 true ->
		     C2 = C#sconf{docroot = Root},
		     fload(FD, server, GC, C2, Cs, Lno, Next);
		 false ->
		     {error, ?F("Expect directory at line ~w", [Lno])}
	     end;
	 ["default_server_on_this_ip", '=', Bool] ->
	     case is_bool(Bool) of
		 {true, Val} ->
		     C2 = C#sconf{default_server_on_this_ip = Val},
		     fload(FD, server, GC, C2, Cs, Lno, Next);
		 false ->
		     {error, ?F("Expect true|false at line ~w", [Lno])}
	     end;
	 ['<', "/server", '>'] ->
	      fload(FD, globals, GC, undefined, [C|Cs], Lno+1, Next);
	 [H|_] ->
	     {error, ?F("Unexpected input ~p at line ~w", [H, Lno])}
     end.

	    

is_bool("true") ->
    {true, true};
is_bool("false") ->
    {true, false};
is_bool(_) ->
    false.


is_dir(Val) ->
    case file:read_file_info(Val) of
	{ok, FI} when FI#file_info.type == directory ->
	    true;
	_ ->
	    false
    end.


%% tokenizer
toks(Chars) ->
    toks(Chars, free, [], []). % two accumulators

toks([$#|T], Mode, Ack, Tack) ->
    toks([], Mode, Ack, Tack);

toks([H|T], free, Ack, Tack) -> 
    %?Debug("Char=~p", [H]),
    case {is_string_char(H), is_special(H), is_space(H)} of
	{_, _, true} ->
	    toks(T, free, Ack, Tack);
	{_, true, _} ->
	    toks(T, free, [], [list_to_atom([H]) | Tack]);
	{true, _,_} ->
	    toks(T, string, [H], Tack);
	{false, false, false} ->
	    %% weird char, let's ignore it
	    toks(T, free, Ack, Tack)
    end;
toks([C|T], string, Ack, Tack) -> 
    %?Debug("Char=~p", [C]),
    case {is_string_char(C), is_special(C), is_space(C)} of
	{true, _,_} ->
	    toks(T, string, [C|Ack], Tack);
	{_, true, _} ->
	    toks(T, free, [], [list_to_atom([C]), lists:reverse(Ack)|Tack]);
	{_, _, true} ->
	    toks(T, free, [], [lists:reverse(Ack)|Tack]);
	{false, false, false} ->
	    %% weird char, let's ignore it
	    toks(T, free, Ack, Tack)

    end;
toks([], string, Ack, Tack) ->
    lists:reverse([lists:reverse(Ack) | Tack]);
toks([], free, _,Tack) ->
    lists:reverse(Tack).

is_string_char(C) ->
    if
	$a =< C, C =< $z ->
	     true;
	$A =< C, C =< $Z ->
	    true;
	$0 =< C, C =< $9 ->
	    true;
	true ->
	    lists:member(C, [$., $/, $:, $_, $-])
    end.

is_space(C) ->
    lists:member(C, [$\s, $\n, $\t, $\r]).

is_special(C) ->
    lists:member(C, [$=, $<, $>]).

    

