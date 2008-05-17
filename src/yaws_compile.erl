%%%----------------------------------------------------------------------
%%% File    : yaws_compile.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 20 Feb 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_compile).
-author('klacke@hyber.org').

-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").



%%  tada !!
%% returns a CodeSpec which is:
%% a list  {data, NumChars} | 
%%         {mod, LineNo, YawsFile, NumSkipChars,  Mod, Func} | 
%%         {error, NumSkipChars, E}}

%% each erlang fragment inside <erl> .... </erl> is compiled into
%% its own module


-record(comp, {
          gc,     %% global conf
          sc,     %% server conf
          startline = 0,
          modnum = 1,
          infile,
          infd,
          outfile,
          outfd}).

-export([compile_file/1]).
%% internal exports
-export([compiler_proc/3]).

comp_opts(GC) ->
    ?Debug("I=~p~n", [GC#gconf.include_dir]),
    I = lists:map(fun(Dir) -> {i, Dir} end, GC#gconf.include_dir),
    Warnings = case get(use_yfile_name) of
                   true  -> [return_warnings, debug_info];
                   _     -> []
               end,
    Opts = [binary, return_errors] ++ Warnings ++ I,
    ?Debug("Compile opts = ~p~n", [Opts]),
    Opts.


compile_file(File) ->
    GC=get(gc), SC=get(sc),
    case get(use_yfile_name) of
        true ->
            %% Run by 'yaws -check'
            put(yfile,filename:rootname(yaws:to_list(File)));
        _ ->
            put(yfile,yaws:to_list(File))
    end,
    %% broken erlang compiler isn't
    %% reentrant, can only have one erlang compiler at a time running 
    global:trans({yaws, self()},
                 fun() ->
                         ?Debug("Compile ~s~n", [File]),
                         case file_open(File) of
                             {ok, Fd} ->
                                 Spec = compile_file(
                                          #comp{infile = File, 
                                                infd = Fd, gc = GC, sc = SC}, 
                                          1,   
                                          get_line(), init, 0, [], 0),
                                 erase(yfile),
                                 erase(yfile_data),
                                 erase(yfile_data_orig),
                                 ?Debug("Spec: ~p~n", [Spec]),
                                 Spec;
                             _Err ->
                                 yaws:elog("can't open ~s~n", [File]),
                                 exit(normal)
                         end
                 end,
                 [node()], infinity).


clump_data([{data, I}, {data, J} | Tail]) ->
    clump_data([{data, I+J}|Tail]);
clump_data([H|T]) ->
    [H|clump_data(T)];
clump_data([]) ->
    [].


compile_file(C,  _LineNo, eof, _Mode, NumChars, Ack, Errors) ->
    file_close(C#comp.infd),
    {ok, [{errors, Errors} | 
          clump_data(lists:reverse([{data, NumChars} |Ack]))]};


%% skip initial space if first thing is <erl> otherwise not
compile_file(C, LineNo,  Chars, init, NumChars, Ack, Errs) ->
    case Chars -- [$\s, $\t, $\n, $\r] of
        [] ->
            ?Debug("SKIP ~p~n", [Chars]),
            L=length(Chars),
            compile_file(C, LineNo+1, line(), init, NumChars-L, Ack, Errs);
        "<erl" ++ _ ->  %% first chunk is erl, skip whistespace
            compile_file(C, LineNo,  Chars, html, NumChars, Ack, Errs);
        _ ->
            %% first chunk is html, keep whitespace
            file_position_bof(),
            compile_file(C,1,line(),html,0,[], Errs)
    end;

compile_file(C, LineNo,  Chars = "<erl" ++ Tail, html,  NumChars, Ack,Es) ->
    ?Debug("start erl:~p",[LineNo]),
    C2 = new_out_file(LineNo, C, Tail, C#comp.gc),
    C3 = C2#comp{startline = LineNo},
    L = length(Chars),
    if
        NumChars > 0 ->
            compile_file(C3, LineNo+1, line() , erl,L, 
                         [{data, NumChars} | Ack], Es);
        true -> %% just ignore zero byte data segments
            compile_file(C3, LineNo+1, line() , erl, L + (-NumChars), 
                         Ack, Es) %hack
    end;

compile_file(C, LineNo,  Chars = "<verbatim>" ++ _Tail, html,  
             NumChars, Ack,Es) ->
    ?Debug("start verbatim:~p",[LineNo]),
    Len = length(Chars),
    C2 = C#comp{outfile = ["<pre>\n"]},  %% use as accumulator
    compile_file(C2,  LineNo+1, line() , verbatim , Len, [{data, NumChars} | Ack], Es);

compile_file(C, LineNo,  Chars = "</verbatim>" ++ _Tail, verbatim, 
             NumChars, Ack, Es) ->
    Data = list_to_binary(lists:reverse(["</pre>\n" | C#comp.outfile])),
    Len = length(Chars),
    compile_file(C#comp{outfile = undefined}, LineNo, line(), html, 0, 
                 [{verbatim, NumChars+Len, Data} |Ack], Es);

compile_file(C, LineNo,  Chars, verbatim, NumChars, Ack,Es) ->
    case has_str(Chars, ["</verbatim>"]) of
        {ok, Skipped, Chars2} ->
            compile_file(C, LineNo,  Chars2, verbatim, 
                         NumChars + Skipped, Ack,Es);
        false ->
            C2 = C#comp{outfile = [yaws_api:htmlize(Chars) | C#comp.outfile]},
            compile_file(C2, LineNo+1, line(), verbatim, NumChars + 
                         length(Chars), Ack,Es)
    end;

compile_file(C, LineNo,  _Chars = "</erl>" ++ Tail, erl, NumChars, Ack, Es) ->
    ?Debug("stop erl:~p",[LineNo]),
    file:close(C#comp.outfd),
    case proc_compile_file(C#comp.outfile, comp_opts(C#comp.gc)) of
        {ok, ModuleName, Binary, Warnings} ->
            case get(use_yfile_name) of
                true ->
                    file:write_file("../../ebin/" ++ 
                                    filename:rootname(C#comp.outfile)++".beam",
                                    Binary);
                _ ->
                    ok
            end,
            comp_warn(C, Warnings),
            case code:load_binary(ModuleName, C#comp.outfile, Binary) of
                {module, ModuleName} ->
                    C2 = C#comp{modnum = C#comp.modnum+1},
                    L2 = check_exported(C, LineNo, NumChars, ModuleName),
                    compile_file(C2, LineNo, Tail, html, 0,
                                 L2++[{skip, 6}|Ack], Es);
                Err ->
                    A2 = gen_err(C, LineNo, NumChars,
                                 ?F("Cannot load module ~p: ~p", 
                                    [ModuleName, Err])),
                    compile_file(C, LineNo, Tail, html, 0,
                                 [A2, {skip, 6}|Ack], Es+1)
            end;
        {error, Errors, Warnings} ->
            %% FIXME remove outfile here ... keep while debuging
            A2 = comp_err(C, LineNo, NumChars, Errors, Warnings),
            compile_file(C, LineNo, Tail, html, 0, [A2, {skip, 6}|Ack], Es+1);
        {error, Str} ->  
            %% this is boring but does actually happen
            %% in order to get proper user errors here we need to catch i/o
            %% or hack compiler/parser
            yaws:elog("Dynamic compile error in file ~s (~s), line ~w~n~s",
                      [C#comp.infile, C#comp.outfile,LineNo, Str]),
            A2 = {error, NumChars, ?F("<pre> Dynamic compile error in file "
                                      " ~s line ~w~n~s </pre>", 
                                      [C#comp.infile, LineNo, Str])},
            compile_file(C, LineNo, Tail, html, 0, [A2, {skip, 6}|Ack], Es+1)
    end;

compile_file(C, LineNo,  Chars, erl, NumChars, Ack,Es) ->
    case has_str(Chars, ["</erl>"]) of
        {ok, Skipped, Chars2} ->
            compile_file(C, LineNo,  Chars2, erl, NumChars + Skipped, Ack,Es);
        false ->
            ?Debug("Gen: ~s", [Chars]),
            io:format(C#comp.outfd, "~s", [Chars]),
            compile_file(C, LineNo+1, line(), erl, NumChars + 
                         length(Chars), Ack,Es)
    end;

compile_file(C, LineNo, [], html, NumChars, Ack, Es) ->
    compile_file(C, LineNo+1, line(), html, NumChars, Ack, Es);

compile_file(C, LineNo,  Chars, html, NumChars, Ack,Es) ->
    case has_str(Chars, ["<erl", "%%", "<verbatim>"]) of
        {ok, Skipped, "<erl"++_ = Chars2} ->
            compile_file(C, LineNo, Chars2, html, NumChars+Skipped, Ack, Es);
        {ok, Skipped, "<verbatim>"++_ = Chars2} ->
            compile_file(C, LineNo, Chars2, html, NumChars+Skipped, Ack, Es);
        {ok, Skipped, "%%"++Chars2} ->
            compile_file(C, LineNo, Chars2, binding, 2,
                         [{data, NumChars+Skipped}|Ack], Es);
        false ->
            compile_file(C, LineNo, tl(Chars), html, NumChars+1, Ack, Es)
    end;

compile_file(C, LineNo, [], binding, NumChars, Ack, Es) ->
    compile_file(C, LineNo+1, line(), html, NumChars, Ack, Es);

compile_file(C, LineNo, "%%"++Chars, binding, NumChars, Ack, Es) ->
    compile_file(C, LineNo, Chars, html, 0, [{binding, NumChars+2}|Ack], Es);

compile_file(C, LineNo, [_H|T], binding, NumChars, Ack, Es) ->
    compile_file(C, LineNo, T, binding, NumChars+1, Ack, Es).


has_str(L, Strs) -> has_str(L, Strs, 0).
has_str([H|T], Strs, Num) ->
    case yaws:is_space(H) of
        true -> has_str(T, Strs, Num+1);
        false ->
            case lists:any(fun(Str) -> lists:prefix(Str, [H|T]) end, Strs) of
                true -> {ok, Num, [H|T]};
                false -> false
            end
    end;
has_str(_,_,_) -> false.


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

line() ->
    get_line().

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


new_out_file_module(Tail) ->
    case Tail of
        ">" ++ _ ->
            Mnum = case catch gen_server:call(yaws_server, mnum, infinity) of
                       {'EXIT', _} ->
                           1;
                       Other ->
                           Other
                   end,
            Prefix = case get(use_yfile_name) of
                         true -> filename:rootname(get(yfile))++"_yaws";
                         _    -> "m"
                     end,
            Prefix ++ integer_to_list(Mnum);
        _ ->
            case string:tokens(Tail, " =>\r\n\"") of
                ["module", Module] ->
                    Module
            end
    end.

new_out_file_name(Module, GC) ->
    case get(use_yfile_name) of
        true ->
            Module ++ ".erl";
        _ ->
            filename:join([yaws:id_dir(GC#gconf.id), Module ++ ".erl"])
    end.

%% this will generate 10 lines
new_out_file(Line, C, Tail, GC) ->
    Module = new_out_file_module(Tail),
    OutFile = new_out_file_name(Module, GC),
    ?Debug("Writing outout file~s~n", [OutFile]),
    {ok, Out} = file:open(OutFile, [write]),
    ok = io:format(Out, "-module(\'~s\').~n-export([out/1]).~n~n", [Module]),
    ok = io:format(Out, "-yawsfile('" ++ get(yfile) ++ "').~n",[]),
    io:format(Out, "%%~n%% code at line ~w from file ~s~n%%~n",
              [Line, C#comp.infile]),
    io:format(Out, "-import(yaws_api, [f/2, fl/1, postvar/2, queryvar/2])."
              " ~n~n", []),
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


comp_err(C, LineNo, NumChars, Err, Warns) ->
    case get(use_yfile_name) of
        true ->
            report_errors(C, Err),
            report_warnings(C, Warns),
            {error, NumChars,  ""};
        _ ->
            comp_err(C, LineNo, NumChars, Err)
    end.

comp_err(C, _LineNo, NumChars, Err) ->
    case Err of
        [{_FileName, [ {Line0, Mod, E} |_]} |_] when integer(Line0) ->
            Line = Line0 + C#comp.startline - 10,
            ?Debug("XX ~p~n", [{_LineNo, Line0}]),
            Str = io_lib:format("~s:~w:~n ~s\ngenerated file at: ~s~n", 
                                [C#comp.infile, Line,
                                 apply(Mod, format_error, [E]),
                                 C#comp.outfile
                                ]),
            HtmlStr = ?F("~n<pre>~nDynamic compile error: ~s~n</pre>~n", 
                         [Str]),
            yaws:elog("Dynamic compiler err ~s", [Str]),
            {error, NumChars,  HtmlStr};
        _Other ->
            yaws:elog("Dynamic compile error ~p", [Err]),
            {error, NumChars, ?F("<pre> Compile error - "
                                 "Other err ~p</pre>~n", [Err])}
    end.

comp_warn(C, Warnings) ->
    case get(use_yfile_name) of
        true ->
            report_warnings(C, Warnings);
        _ ->
            ok
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
            {ok, Mod, Bin, []};
        {P, result, {ok, Mod, Bin, Warnings}} ->
            {ok, Mod, Bin, Warnings};
        {io_request, P1, P2, {put_chars, M, F, A}} ->
            P1 ! {io_reply, P2, ok},
            Str = apply(M, F, A),
            get_compiler_data(P, [Str|Ack]);
        {P, result, {error, Errors, Warnings}} ->
            {error, Errors, Warnings};
        {P, result, error} ->
            S = lists:map(
                  fun(S) -> S ++ "\n" end, lists:reverse(Ack)),
            {error, S};
        {P, result, {'EXIT', Reason}} ->
            S = lists:flatten(io_lib:format("~p", [Reason])),
            {error, S} 
    end.



%% This code is so that we get the \r in the line
%% when we're parsing msdos files.

file_open(Fname) ->
    case file:read_file(Fname) of
        {ok, Bin} ->
            put(yfile_data, binary_to_list(Bin)),
            put(yfile_data_orig, Bin),
            {ok, yfile_data};
        Err ->
            Err
    end.

file_close(Key) ->
    erase(Key).

file_position_bof() ->
    put(yfile_data, binary_to_list(get(yfile_data_orig))).

get_line() ->
    case get (yfile_data) of
        [] ->
            eof;
        Chars ->
            case get_line_from_chars(Chars, []) of
                {ok, Line, Tail} ->
                    put (yfile_data, Tail),
                    Line;
                need_more ->
                    put(yfile_data, []),
                    Chars
            end
    end.

get_line_from_chars([$\r, $\n | Tail], Line) ->
    {ok, lists:reverse([$\n, $\r|Line]), Tail};

get_line_from_chars([$\n | Tail], Line) ->
    {ok, lists:reverse([$\n|Line]), Tail};

get_line_from_chars([], _Line) ->
    need_more;
get_line_from_chars([H|T], Line) ->
    get_line_from_chars(T, [H|Line]).



%% -----------------------------------------------------------------
%% From compile.erl in order to print proper error/warning messages
%% if compiled with check option.
report_errors(C, Errors) ->
    Check = true,
    case Check of
        true ->
            File = "./" ++ filename:basename(C#comp.infile),
            SLine = C#comp.startline - 10,
            lists:foreach(fun ({{_F,_L},Eds}) -> list_errors(File, SLine, Eds);
                              ({_F,Eds})      -> list_errors(File, SLine, Eds)
                          end, Errors);
        false ->
            ok
    end.

report_warnings(C, Ws0) ->
    Check = true,
    case Check of
        true ->
            File = "./" ++ filename:basename(C#comp.infile),
            SLine = C#comp.startline - 10,
            Ws1 = lists:flatmap(fun({{_F,_L},Eds}) ->
                                        format_message(File, SLine, Eds);
                                   ({_F,Eds}) ->
                                        format_message(File, SLine, Eds)
                                end,
                                Ws0),
            Ws = ordsets:from_list(Ws1),
            lists:foreach(fun({_,Str}) -> io:put_chars(Str) end, Ws);
        false -> ok
    end.

format_message(F, SLine, [{Line0,Mod,E}|Es]) ->
    Line = Line0 + SLine,
    M = {{F,Line},io_lib:format("~s:~w: Warning: ~s\n", [F,Line,Mod:format_error(E)])},
    [M|format_message(F, SLine, Es)];
format_message(F, SLine, [{Mod,E}|Es]) ->
    M = {none,io_lib:format("~s: Warning: ~s\n", [F,Mod:format_error(E)])},
    [M|format_message(F, SLine, Es)];
format_message(_, _, []) -> [].

%% list_errors(File, StartLine, ErrorDescriptors) -> ok

list_errors(F, SLine, [{Line0,Mod,E}|Es]) ->
    Line = Line0 + SLine,
    io:fwrite("~s:~w: ~s\n", [F,Line,Mod:format_error(E)]),
    list_errors(F, SLine, Es);
list_errors(F, SLine, [{Mod,E}|Es]) ->
    io:fwrite("~s: ~s\n", [F,Mod:format_error(E)]),
    list_errors(F, SLine, Es);
list_errors(_F, _SLine, []) ->
    ok.

