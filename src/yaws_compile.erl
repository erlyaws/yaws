%%%----------------------------------------------------------------------
%%% File    : yaws_compile.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 20 Feb 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_compile).
-author('klacke@hyber.org').

-compile(export_all).
-include("yaws.hrl").



%%  tada !!
%% returns a CodeSpec which is:
%% a list  {data, NumChars} | 
%%         {mod, LineNo, YawsFile, NumSkipChars,  Mod, Func} | 
%%         {error, NumSkipChars, E}}

% each erlang fragment inside <erl> .... </erl> is compiled into
% its own module


-record(comp, {
	  gc,     %% global conf
	  sc,     %% server conf
	  startline = 0,
	  modnum = 1,
	  infile,
	  infd,
	  outfile,
	  outfd}).



comp_opts(GC) ->
    ?Debug("I=~p~n", [GC#gconf.include_dir]),
    I = lists:map(fun(Dir) -> {i, Dir} end, GC#gconf.include_dir),
    YawsDir = {i, "/home/klacke/yaws/include"},
    I2 = [YawsDir | I],
    Opts = [binary, report_errors | I2],
    ?Debug("Compile opts = ~p~n", [Opts]),
    Opts.


compile_file(File, GC, SC) ->
    case file:open(File, [read]) of
	{ok, Fd} ->
	    Spec = compile_file(#comp{infile = File, 
				      infd = Fd, gc = GC, sc = SC}, 
				1,  
				io:get_line(Fd, ''), init, 0, []),
	    Spec;
	_Err ->
	    yaws:elog("can't open ~s~n", [File]),
	    exit(normal)
    end.

compile_file(C, _LineNo, eof, _Mode, NumChars, Ack) ->
    file:close(C#comp.infd),
    {ok, lists:reverse([{data, NumChars} |Ack])};


%% skip initial space if first thing is <erl> otherwise not
compile_file(C, LineNo,  Chars, init, NumChars, Ack) ->
    case Chars -- [$\s, $\t, $\n, $\r] of
	[] ->
	    ?Debug("SKIP ~p~n", [Chars]),
	    L=length(Chars),
	    compile_file(C, LineNo+1, line(C), init, NumChars-L, Ack);
	"<erl>" ++ _ ->  %% first chunk is erl, skip whistespace
	    compile_file(C, LineNo,  Chars, html, NumChars, Ack);
	_ ->
	    %% first chunk is html, keep whitespace
	    Fd=C#comp.infd,
	    file:position(Fd, bof),
	    compile_file(C,1,io:get_line(Fd,''),html,0,[])
    end;

compile_file(C, LineNo,  Chars = "<erl>" ++ _Tail, html,  NumChars, Ack) ->
    ?Debug("start erl:~p",[LineNo]),
    C2 = new_out_file(LineNo, C, C#comp.gc),
    C3 = C2#comp{startline = LineNo},
    L = length(Chars),
    if
	NumChars > 0 ->
	    compile_file(C3, LineNo+1, line(C) , erl,L, 
			 [{data, NumChars} | Ack]);
	true -> %% just ignore zero byte data segments
	    compile_file(C3, LineNo+1, line(C) , erl, L + (-NumChars), Ack) %hack
    end;

compile_file(C, LineNo,  Chars = "</erl>" ++ _Tail, erl, NumChars, Ack) ->
    ?Debug("stop erl:~p",[LineNo]),
    file:close(C#comp.outfd),
    NumChars2 = NumChars + length(Chars),
    case proc_compile_file(C#comp.outfile, comp_opts(C#comp.gc)) of
	{ok, ModuleName, Binary} ->
	    case code:load_binary(ModuleName, C#comp.outfile, Binary) of
		{module, ModuleName} ->
		    C2 = C#comp{modnum = C#comp.modnum+1},
		    L2 = check_exported(C, LineNo,NumChars2, ModuleName),
		    compile_file(C2, LineNo+1, line(C),html,0,L2++Ack);
		Err ->
		    A2 = gen_err(C, LineNo, NumChars2,
				 ?F("Cannot load module ~p: ~p", 
				    [ModuleName, Err])),
		    compile_file(C, LineNo+1, line(C),
				 html, 0, [A2|Ack])
	    end;
	{error, Errors, _Warnings} ->
	    %% FIXME remove outfile here ... keep while debuging
	    A2 = comp_err(C, LineNo, NumChars2, Errors),
	    compile_file(C, LineNo+1, line(C), html, 0, [A2|Ack]);
	{error, Str} ->  
	    %% this is boring but does actually happen
	    %% in order to get proper user errors here we need to catch i/o
	    %% or hack compiler/parser
	    yaws:elog("Dynamic compile error in file ~s, line~w",
		      [C#comp.infile, LineNo]),
	    A2 = {error, NumChars2, ?F("<pre> Dynamic compile error in file "
				       " ~s line ~w~n~s </pre>", 
				       [C#comp.infile, LineNo, Str])},
	    compile_file(C, LineNo+1, line(C), html, 0, [A2|Ack])
    end;

compile_file(C, LineNo,  Chars = "<pre>" ++ _Tail, html,  NumChars, Ack) ->
    ?Debug("start pre:~p",[LineNo]),
    compile_file(C, LineNo+1, line(C) , pre, NumChars + length(Chars), Ack);

compile_file(C, LineNo,  Chars = "</pre>" ++ _Tail, pre,  NumChars, Ack) ->
    ?Debug("stop pre:~p",[LineNo]),
    compile_file(C, LineNo+1, line(C) , html, NumChars + length(Chars), Ack);

compile_file(C, LineNo,  Chars, erl, NumChars, Ack) ->
    case has_tag(Chars, "</erl>") of
	{ok, Skipped, Chars2} ->
	    compile_file(C, LineNo,  Chars2, erl, NumChars + Skipped, Ack);
	false ->
	    io:format(C#comp.outfd, "~s", [Chars]),
	    compile_file(C, LineNo+1, line(C), erl, NumChars + 
			 length(Chars), Ack)
    end;


compile_file(C, LineNo,  Chars, html, NumChars, Ack) ->
    case has_tag(Chars, "<erl>") of
	{ok, Skipped, Chars2} ->
	    compile_file(C, LineNo,  Chars2, html, NumChars+Skipped, Ack);
	false ->
	    compile_file(C, LineNo+1, line(C), html, NumChars + 
			 length(Chars), Ack)
    end;

compile_file(C, LineNo,  Chars, pre, NumChars, Ack) ->
    compile_file(C, LineNo+1, line(C), pre, NumChars + length(Chars), Ack).



has_tag(L, Str) ->
    has_tag(L, Str, 0).
has_tag([H|T], Tag, Num) ->
    case yaws:is_space(H) of
	true ->
	    has_tag(T, Tag, Num+1);
	false ->
	    case lists:prefix(Tag, [H|T]) of
		true ->
		    {ok, Num, [H|T]};
		false ->
		    false
	    end
    end;
has_tag(_,_,_) ->
    false.

check_exported(C, LineNo, NumChars, Mod) ->
    case is_exported(out, 1, Mod) of
	true ->
	    [{mod, C#comp.startline, C#comp.infile, 
	      NumChars,Mod,out}];
	false ->
	    ?Debug("XX ~p~n", [C]), 
	    [gen_err(C, LineNo, NumChars,
		     "out/1 is not defined ")]
    end.

line(C) ->
    io:get_line(C#comp.infd, '').

is_exported(Fun, A, Mod) ->
    case (catch Mod:module_info()) of
	List when list(List) ->
	    case lists:keysearch(exports, 1, List) of
		{value, {exports, Exp}} ->
		    lists:member({Fun, A}, Exp);
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

	     
%% this will generate 9 lines
new_out_file(Line, C, GC) ->
    Mnum = gen_server:call(yaws_server, mnum),
    Module = [$m | integer_to_list(Mnum)],
    OutFile = lists:flatten(
		io_lib:format(
		  "/tmp/yaws/~s/~s.erl",[GC#gconf.uid, Module])),

    %% "/tmp/yaws/" ++ Module ++ ".erl",

    ?Debug("Writing outout file~s~n", [OutFile]),
    {ok, Out} = file:open(OutFile, [write]),
    ok = io:format(Out, "-module(~s).~n-compile(export_all).~n~n", [Module]),
    io:format(Out, "%%~n%% code at line ~w from file ~s~n%%~n",
	      [Line, C#comp.infile]),

    io:format(Out, "-import(yaws_api, [f/2, fl/1, parse_post_data/2]). ~n~n", []),
    io:format(Out, '-include("~s/include/yaws_api.hrl").~n', 
	      [GC#gconf.yaws_dir]),
    C#comp{outfd = Out,
	   outfile = OutFile}.


gen_err(C, _LineNo, NumChars, Err) ->
    S = io_lib:format("<p> Error in File ~s Erlang code beginning "
		      "at line ~w~n"
		      "Error is: ~p~n", [C#comp.infile, C#comp.startline, 
					 Err]),
    yaws:elog("~s~n", [S]),
    {error, NumChars, S}.


comp_err(C, _LineNo, NumChars, Err) ->
    case Err of
	[{_FileName, [ErrInfo|_]} |_] ->
	    {Line0, Mod, E}=ErrInfo,
	    Line = Line0 + C#comp.startline - 9,
	    ?Debug("XX ~p~n", [{_LineNo, Line0}]),
	    Str = io_lib:format("~s:~w: ~s\n", 
				[C#comp.infile, Line,
				 apply(Mod, format_error, [E])]),
	    HtmlStr = ?F("~n<pre>~nDynamic compile error: ~s~n</pre>~n", 
			[Str]),
	    yaws:elog("Dynamic compiler err ~s", [Str]),
	    {error, NumChars,  HtmlStr};
	_Other ->
	    yaws:elog("Dynamic compile error", []),
	    {error, NumChars, ?F("<pre> Compile error - "
				 "Other err ~p</pre>~n", [Err])}
    end.


%% due to compiler not producing proper error
%% we NEED to catch all io produced by the compiler

proc_compile_file(F, Opts) ->
    G = group_leader(),
    group_leader(self(), self()),
    P = proc_lib:spawn(?MODULE, compiler_proc, [self(), F, Opts]),
    Res = get_compiler_data(P, []),
    group_leader(G, self()),
    Res.

compiler_proc(Top, F, Opts) ->
    R = (catch compile:file(F, Opts)),
    Top ! {self(), result, R}.


get_compiler_data(P, Ack) ->
    receive
	{P, result, {ok, Mod, Bin}} ->
	    {ok, Mod, Bin};
	{io_request, P1, P2, {put_chars, M, F, A}} ->
	    P1 ! {io_reply, P2, ok},
	    Str = apply(M, F, A),
	    get_compiler_data(P, [Str|Ack]);
	{P, result, {error, Errors, Warnings}} ->
	    {error, Errors, Warnings};
	{P, result, error} ->
	    S = lists:map(
		  fun(S) -> S ++ "\n" end, lists:reverse(Ack)),
	    {error, S}
    end.

					    


	    




