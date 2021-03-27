-module(yaws_compile).
-author('klacke@hyber.org').

-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").

-export([compile_file/1, compile_file/2]).

-record(comp, {gc, %% global conf
               script,
               hash,
               line = 1,
               nummod = 1,
               check_script = false,
               nberrors = 0,
               numchars = 0,
               verbatim,
               erlcode,
               comp_opts = []}).

-spec compile_file(File) -> {ok, NbErrors, CodeSpec} when
      File         :: file:filename(),
      NbErrors     :: non_neg_integer(),
      CodeSpec     :: [Data | Binding | YawsMod | Verbatim | Skip | Error],
      Data         :: {data, NumChars},
      Binding      :: {binding, NumSkipChars},
      YawsMod      :: {mod, LineNo, YawsFile, NumSkipChars,  Mod, Func},
      Verbatim     :: {verbatim, NumSkipChars, Html},
      Skip         :: {skip, NumSkipChars},
      Error        :: {error, NumSkipChars, Reason},
      NumChars     :: non_neg_integer(),
      NumSkipChars :: non_neg_integer(),
      LineNo       :: non_neg_integer(),
      YawsFile     :: file:filename(),
      Mod          :: atom(),
      Func         :: atom(),
      Html         :: iolist(),
      Reason       :: iolist().

-define(IS_SPACE(C), (C =:= $\s orelse C =:= $\t orelse
                      C =:= $\r orelse C =:= $\n)).

-define(IS_BINDING_CHAR(C), ((C >= $a andalso C =< $z) orelse
                             (C >= $A andalso C =< $Z) orelse
                             (C >= $0 andalso C =< $9) orelse
                             C =:= $_ orelse C =:= $- orelse C =:= $.)).

compile_file(File) ->
    compile_file(File, []).

compile_file(File, Opts) ->
    case file:read_file(File) of
        {ok, Bin} ->
            GC = get(gc),
            CheckFlag = (get(check_yaws_script) =:= true),
            Comp = #comp{gc             = GC,
                         script         = File,
                         hash           = integer_to_list(erlang:phash2(File)),
                         check_script   = CheckFlag,
                         comp_opts      = compile_opts(GC, Opts, CheckFlag)},
            global:trans({{yaws, Comp#comp.hash}, self()},
                         fun() -> do_compile_file(Bin, Comp) end,
                         [node()], infinity);
        {error, Reason} ->
            S = ?F("failed to open '~s': ~p~n",
                   [File, file:format_error(Reason)]),
            case get(check_yaws_script) of
                true  -> io:put_chars(S);
                _     -> yaws:elog("~s", [S])
            end,
            {ok, 1, [{error, 0, S}]}
    end.

do_compile_file(Bin, Comp) ->
    ?Debug("Compile ~s~n", [Comp#comp.script]),
    {ok, Spec0, NewComp} = compile_file(Bin, Comp, init, []),
    Spec1 = join_data(Spec0),
    ?Debug("NbErrors: ~p - Spec: ~p~n", [NewComp#comp.nberrors, Spec1]),
    {ok, NewComp#comp.nberrors, Spec1}.


%% Possible states:
%%   * init,
%%   * html_tag, html, html_close_tag,
%%   * verbatim_tag, verbatim, verbatim_close_tag
%%   * erl_tag, erl, erl_string, erl_atom, erl_close_tag
compile_file(<<>>, Comp, erl, Spec) ->
    compile_erl(0, <<>>, Comp, Spec);
compile_file(<<>>, Comp, _State, Spec) ->
    NewComp = Comp#comp{numchars = 0,
                        verbatim = undefined,
                        erlcode  = undefined},
    NewSpec = if
                  Comp#comp.numchars == 0 -> lists:reverse(Spec);
                  true -> lists:reverse([{data, Comp#comp.numchars}|Spec])
              end,
    {ok, NewSpec, NewComp};

%% init state
compile_file(<<$\n,Bin/binary>>, Comp, init, Spec) ->
    NewComp = Comp#comp{line     = Comp#comp.line+1,
                        numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, init, Spec);
compile_file(<<C,Bin/binary>>, Comp, init, Spec) when ?IS_SPACE(C) ->
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, init, Spec);
compile_file(<<"<erl>",Bin/binary>>, Comp, init, Spec) ->
    %% accumulate all spaces and skip it iff erl tag if found.
    ?Debug("Start parsing erlang block at line ~p", [Comp#comp.line]),
    ErlCode = {Comp#comp.line, Comp#comp.line, undefined, []},
    NewComp = Comp#comp{numchars = Comp#comp.numchars+5,
                        erlcode  = ErlCode},
    compile_file(Bin, NewComp, erl, Spec);
compile_file(<<"<erl",C,Bin/binary>>, Comp, init, Spec) when ?IS_SPACE(C) ->
    %% accumulate all spaces and skip it iff erl tag if found.
    ?Debug("Start parsing erlang block at line ~p", [Comp#comp.line]),
    ErlCode = {Comp#comp.line, Comp#comp.line, undefined, []},
    NewComp = Comp#comp{numchars = Comp#comp.numchars+4,
                        erlcode  = ErlCode},
    compile_file(<<C,Bin/binary>>, NewComp, erl_tag, Spec);
compile_file(Bin, Comp, init, Spec) ->
    compile_file(Bin, Comp, html, Spec);

%% html_tag state
compile_file(<<"%%",Bin/binary>>, Comp, html_tag, Spec) ->
    case parse_binding(Bin) of
        {ok, Skipped, Rest} ->
            NewComp = Comp#comp{numchars = 0},
            compile_file(Rest, NewComp, html_tag,
                         [{binding, Skipped+2}, {data, Comp#comp.numchars}|Spec]);
        not_found ->
            NewComp = Comp#comp{numchars = Comp#comp.numchars+2},
            compile_file(Bin, NewComp, html_tag, Spec)
    end;
compile_file(<<$>,Bin/binary>>, Comp, html_tag, Spec) ->
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, html, Spec);
compile_file(<<$\n,Bin/binary>>, Comp, html_tag, Spec) ->
    NewComp = Comp#comp{line     = Comp#comp.line+1,
                        numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, html_tag, Spec);
compile_file(<<_,Bin/binary>>, Comp, html_tag, Spec) ->
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, html_tag, Spec);

%% html state
compile_file(<<"<erl>",Bin/binary>>, Comp, html, Spec) ->
    ?Debug("Start parsing erlang block at line ~p", [Comp#comp.line]),
    ErlCode = {Comp#comp.line, Comp#comp.line, undefined, []},
    NewComp = Comp#comp{numchars = 5,
                        erlcode  = ErlCode},
    compile_file(Bin, NewComp, erl, [{data, Comp#comp.numchars}|Spec]);
compile_file(<<"<erl",C,Bin/binary>>, Comp, html, Spec) when ?IS_SPACE(C) ->
    ?Debug("Start parsing erlang block at line ~p", [Comp#comp.line]),
    ErlCode = {Comp#comp.line, Comp#comp.line, undefined, []},
    NewComp = Comp#comp{numchars = 4,
                        erlcode  = ErlCode},
    compile_file(<<C, Bin/binary>>, NewComp, erl_tag,
                 [{data, Comp#comp.numchars}|Spec]);
compile_file(<<"<verbatim>",Bin/binary>>, Comp, html, Spec) ->
    ?Debug("Start parsing verbatim block at line ~p", [Comp#comp.line]),
    NewComp = Comp#comp{numchars = 10,
                        verbatim = {[], []}},
    compile_file(Bin, NewComp, verbatim, [{data, Comp#comp.numchars}|Spec]);
compile_file(<<"<verbatim",C,Bin/binary>>, Comp, html, Spec) when ?IS_SPACE(C) ->
    ?Debug("Start parsing verbatim block at line ~p", [Comp#comp.line]),
    NewComp = Comp#comp{numchars = 9,
                        verbatim = {[], []}},
    compile_file(<<C, Bin/binary>>, NewComp, verbatim_tag,
                 [{data, Comp#comp.numchars}|Spec]);
compile_file(<<"</",Bin/binary>>, Comp, html, Spec) ->
    NewComp = Comp#comp{numchars = Comp#comp.numchars+2},
    compile_file(Bin, NewComp, html_close_tag, Spec);
compile_file(<<$<,Bin/binary>>, Comp, html, Spec) ->
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, html_tag, Spec);
compile_file(<<"%%",Bin/binary>>, Comp, html, Spec) ->
    case parse_binding(Bin) of
        {ok, Skipped, Rest} ->
            ?Debug("Binding key '~s' parsed at line ~p",
                   [binary:part(Bin, 0, Skipped), Comp#comp.line]),
            NewComp = Comp#comp{numchars = 0},
            compile_file(Rest, NewComp, html,
                         [{binding, Skipped+2}, {data, Comp#comp.numchars}|Spec]);
        not_found ->
            NewComp = Comp#comp{numchars = Comp#comp.numchars+2},
            compile_file(Bin, NewComp, html, Spec)
    end;
compile_file(<<$\n,Bin/binary>>, Comp, html, Spec) ->
    NewComp = Comp#comp{line     = Comp#comp.line+1,
                        numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, html, Spec);
compile_file(<<_,Bin/binary>>, Comp, html, Spec) ->
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, html, Spec);

%% html_close_tag state
compile_file(<<"%%",Bin/binary>>, Comp, html_close_tag, Spec) ->
    case parse_binding(Bin) of
        {ok, Skipped, Rest} ->
            NewComp = Comp#comp{numchars = 0},
            compile_file(Rest, NewComp, html_close_tag,
                         [{binding, Skipped+2}, {data, Comp#comp.numchars}|Spec]);
        not_found ->
            NewComp = Comp#comp{numchars = Comp#comp.numchars+2},
            compile_file(Bin, NewComp, html_close_tag, Spec)
    end;
compile_file(<<$\n,Bin/binary>>, Comp, html_close_tag, Spec) ->
    NewComp = Comp#comp{line     = Comp#comp.line+1,
                        numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, html_close_tag, Spec);
compile_file(<<$>,Bin/binary>>, Comp, html_close_tag, Spec) ->
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, html, Spec);
compile_file(<<_,Bin/binary>>, Comp, html_close_tag, Spec) ->
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, html_close_tag, Spec);

%% verbatim_tag state
compile_file(<<"/>",Bin/binary>>, Comp, verbatim_tag, Spec) ->
    NewComp = Comp#comp{numchars = Comp#comp.numchars+2,
                        verbatim = undefined},
    %% TODO: empty verbatim ?
    compile_file(Bin, NewComp, html, Spec);
compile_file(<<$>,Bin/binary>>, Comp, verbatim_tag, Spec) ->
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, verbatim, Spec);
compile_file(<<$\n,Bin/binary>>, Comp, verbatim_tag, Spec) ->
    {Attrs, Data} = Comp#comp.verbatim,
    NewComp = Comp#comp{line     = Comp#comp.line+1,
                        numchars = Comp#comp.numchars+1,
                        verbatim = {[$\n|Attrs], Data}},
    compile_file(Bin, NewComp, verbatim_tag, Spec);
compile_file(<<C,Bin/binary>>, Comp, verbatim_tag, Spec) ->
    {Attrs, Data} = Comp#comp.verbatim,
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1,
                        verbatim = {[C|Attrs], Data}},
    compile_file(Bin, NewComp, verbatim_tag, Spec);

%% verbatim state
compile_file(<<"</verbatim>",Bin/binary>>, Comp, verbatim, Spec) ->
    compile_verbatim(11, Bin, Comp, Spec);
compile_file(<<"</verbatim",C,Bin/binary>>, Comp, verbatim, Spec) when ?IS_SPACE(C) ->
    NewComp = Comp#comp{numchars = Comp#comp.numchars+10},
    compile_file(<<C,Bin/binary>>, NewComp, verbatim_close_tag, Spec);
compile_file(<<$\n,Bin/binary>>, Comp, verbatim, Spec) ->
    {Attrs, Data} = Comp#comp.verbatim,
    NewComp = Comp#comp{line     = Comp#comp.line+1,
                        numchars = Comp#comp.numchars+1,
                        verbatim = {Attrs, [$\n|Data]}},
    compile_file(Bin, NewComp, verbatim, Spec);
compile_file(<<C,Bin/binary>>, Comp, verbatim, Spec) ->
    {Attrs, Data} = Comp#comp.verbatim,
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1,
                        verbatim = {Attrs, [yaws_api:htmlize_char(C)|Data]}},
    compile_file(Bin, NewComp, verbatim, Spec);

%% verbatim_close_tag state
compile_file(<<$\n,Bin/binary>>, Comp, verbatim_close_tag, Spec) ->
    NewComp = Comp#comp{line     = Comp#comp.line+1,
                        numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, verbatim_close_tag, Spec);
compile_file(<<$>,Bin/binary>>, Comp, verbatim_close_tag, Spec) ->
    compile_verbatim(1, Bin, Comp, Spec);
compile_file(<<_,Bin/binary>>, Comp, verbatim_close_tag, Spec) ->
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, verbatim_close_tag, Spec);

%% erl_tag state
compile_file(<<"/>",Bin/binary>>, Comp, erl_tag, Spec) ->
    NewComp = Comp#comp{numchars = Comp#comp.numchars+2,
                        erlcode  = undefined},
    %% TODO: empty erl ?
    compile_file(Bin, NewComp, html, Spec);
compile_file(<<$>,Bin/binary>>, Comp, erl_tag, Spec) ->
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, erl, Spec);
compile_file(<<C,"module",Bin/binary>>, Comp, erl_tag, Spec) when ?IS_SPACE(C) ->
    case parse_tag_attribute(Bin) of
        {ok, Lines, Skipped, ModName, Data} ->
            {SLine, ELine, _, _} = Comp#comp.erlcode,
            NewComp = Comp#comp{line     = Comp#comp.line+Lines,
                                numchars = Comp#comp.numchars+Skipped+7,
                                erlcode  = {SLine, ELine+Lines, ModName, []}},
            compile_file(Data, NewComp, erl_tag, Spec);
        not_found ->
            %% ignore malformed module attribute
            NewComp = Comp#comp{numchars = Comp#comp.numchars+7},
            compile_file(Bin, NewComp, erl_tag, Spec)
    end;
compile_file(<<$\n,Bin/binary>>, Comp, erl_tag, Spec) ->
    NewComp = Comp#comp{line     = Comp#comp.line+1,
                        numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, erl_tag, Spec);
compile_file(<<_,Bin/binary>>, Comp, erl_tag, Spec) ->
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, erl_tag, Spec);

%% erl state
compile_file(<<"</erl>",Bin/binary>>, Comp, erl, Spec) ->
    compile_erl(6, Bin, Comp, Spec);
compile_file(<<"</erl",C,Bin/binary>>, Comp, erl, Spec) when ?IS_SPACE(C) ->
    NewComp = Comp#comp{numchars = Comp#comp.numchars+5},
    compile_file(<<C,Bin/binary>>, NewComp, erl_close_tag, Spec);
compile_file(<<$",Bin/binary>>, Comp, erl, Spec) ->
    {SLine, ELine, ModName, Code} = Comp#comp.erlcode,
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1,
                        erlcode  = {SLine, ELine, ModName, [$"|Code]}},
    compile_file(Bin, NewComp, erl_string, Spec);
compile_file(<<$',Bin/binary>>, Comp, erl, Spec) ->
    {SLine, ELine, ModName, Code} = Comp#comp.erlcode,
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1,
                        erlcode  = {SLine, ELine, ModName, [$'|Code]}},
    compile_file(Bin, NewComp, erl_atom, Spec);
compile_file(<<$\n,Bin/binary>>, Comp, erl, Spec) ->
    {SLine, ELine, ModName, Code} = Comp#comp.erlcode,
    NewComp = Comp#comp{line     = Comp#comp.line+1,
                        numchars = Comp#comp.numchars+1,
                        erlcode  = {SLine, ELine+1, ModName, [$\n|Code]}},
    compile_file(Bin, NewComp, erl, Spec);
compile_file(<<$%, $%, Bin/binary>>, Comp, erl, Spec) ->
    {SLine, ELine, ModName, Code} = Comp#comp.erlcode,
    NewComp = Comp#comp{numchars = Comp#comp.numchars+2,
                        erlcode  = {SLine, ELine, ModName, [$%,$%|Code]}},
    compile_file(Bin, NewComp, erl_comment, Spec);
compile_file(<<$%, Bin/binary>>, Comp, erl, Spec) ->
    {SLine, ELine, ModName, Code} = Comp#comp.erlcode,
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1,
                        erlcode  = {SLine, ELine, ModName, [$%|Code]}},
    compile_file(Bin, NewComp, erl_comment, Spec);
compile_file(<<C,Bin/binary>>, Comp, erl, Spec) ->
    {SLine, ELine, ModName, Code} = Comp#comp.erlcode,
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1,
                        erlcode  = {SLine, ELine, ModName, [C|Code]}},
    compile_file(Bin, NewComp, erl, Spec);

%% erl_string state
compile_file(<<$\n,Bin/binary>>, Comp, erl_string, Spec) ->
    {SLine, ELine, ModName, Code} = Comp#comp.erlcode,
    NewComp = Comp#comp{line     = Comp#comp.line+1,
                        numchars = Comp#comp.numchars+1,
                        erlcode  = {SLine, ELine+1, ModName, [$\n|Code]}},
    compile_file(Bin, NewComp, erl_string, Spec);
compile_file(<<$\\,$",Bin/binary>>, Comp, erl_string, Spec) ->
    {SLine, ELine, ModName, Code} = Comp#comp.erlcode,
    NewComp = Comp#comp{numchars = Comp#comp.numchars+2,
                        erlcode  = {SLine, ELine, ModName, [$",$\\|Code]}},
    compile_file(Bin, NewComp, erl_string, Spec);
compile_file(<<$",Bin/binary>>, Comp, erl_string, Spec) ->
    {SLine, ELine, ModName, Code} = Comp#comp.erlcode,
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1,
                        erlcode  = {SLine, ELine, ModName, [$"|Code]}},
    compile_file(Bin, NewComp, erl, Spec);
compile_file(<<C,Bin/binary>>, Comp, erl_string, Spec) ->
    {SLine, ELine, ModName, Code} = Comp#comp.erlcode,
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1,
                        erlcode  = {SLine, ELine, ModName, [C|Code]}},
    compile_file(Bin, NewComp, erl_string, Spec);

%% erl_atom state
compile_file(<<$\n,Bin/binary>>, Comp, erl_atom, Spec) ->
    {SLine, ELine, ModName, Code} = Comp#comp.erlcode,
    NewComp = Comp#comp{line     = Comp#comp.line+1,
                        numchars = Comp#comp.numchars+1,
                        erlcode  = {SLine, ELine+1, ModName, [$\n|Code]}},
    compile_file(Bin, NewComp, erl_atom, Spec);
compile_file(<<$\\,$',Bin/binary>>, Comp, erl_atom, Spec) ->
    {SLine, ELine, ModName, Code} = Comp#comp.erlcode,
    NewComp = Comp#comp{numchars = Comp#comp.numchars+2,
                        erlcode  = {SLine, ELine, ModName, [$',$\\|Code]}},
    compile_file(Bin, NewComp, erl_atom, Spec);
compile_file(<<$',Bin/binary>>, Comp, erl_atom, Spec) ->
    {SLine, ELine, ModName, Code} = Comp#comp.erlcode,
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1,
                        erlcode  = {SLine, ELine, ModName, [$'|Code]}},
    compile_file(Bin, NewComp, erl, Spec);
compile_file(<<C,Bin/binary>>, Comp, erl_atom, Spec) ->
    {SLine, ELine, ModName, Code} = Comp#comp.erlcode,
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1,
                        erlcode  = {SLine, ELine, ModName, [C|Code]}},
    compile_file(Bin, NewComp, erl_atom, Spec);

%% erl_comment state
compile_file(<<$\n,Bin/binary>>, Comp, erl_comment, Spec) ->
    {SLine, ELine, ModName, Code} = Comp#comp.erlcode,
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1,
                        erlcode  = {SLine, ELine, ModName, [$\n|Code]}},
    compile_file(Bin, NewComp, erl, Spec);
compile_file(<<C,Bin/binary>>, Comp, erl_comment, Spec) ->
    {SLine, ELine, ModName, Code} = Comp#comp.erlcode,
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1,
                        erlcode  = {SLine, ELine, ModName, [C|Code]}},
    compile_file(Bin, NewComp, erl_comment, Spec);

%% erl_close_tag state
compile_file(<<$\n,Bin/binary>>, Comp, erl_close_tag, Spec) ->
    NewComp = Comp#comp{line     = Comp#comp.line+1,
                        numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, erl_close_tag, Spec);
compile_file(<<$>,Bin/binary>>, Comp, erl_close_tag, Spec) ->
    compile_erl(1, Bin, Comp, Spec);
compile_file(<<_,Bin/binary>>, Comp, erl_close_tag, Spec) ->
    NewComp = Comp#comp{numchars = Comp#comp.numchars+1},
    compile_file(Bin, NewComp, erl_close_tag, Spec).

compile_verbatim(N, Bin, Comp, Spec) ->
    ?Debug("Stop parsing verbatim block at line ~p", [Comp#comp.line]),
    {Attrs, Data} = Comp#comp.verbatim,
    NewComp = Comp#comp{numchars=0, verbatim=undefined},
    Html = case Attrs of
               [] ->
                   ["<pre>", lists:reverse(Data), "</pre>"];
               _ ->
                   ["<pre ",lists:reverse(Attrs),$>,lists:reverse(Data),"</pre>"]
           end,
    NewSpec = [{verbatim,Comp#comp.numchars+N,Html}|Spec],
    compile_file(Bin, NewComp, html, NewSpec).

compile_erl(N, Bin, Comp, Spec) ->
    ?Debug("Stop parsing erlang block at line ~p", [Comp#comp.line]),
    NewComp = Comp#comp{nummod   = Comp#comp.nummod+1,
                        numchars = 0,
                        erlcode  = undefined},
    case handle_erlang_block(Comp) of
        skip ->
            S = {skip, Comp#comp.numchars+N},
            compile_file(Bin, NewComp, html, [S|Spec]);
        {ok, Mod, Fun} ->
            {Line, _, _, _} = Comp#comp.erlcode,
            S = {mod, Line, Comp#comp.script, Comp#comp.numchars+N,  Mod, Fun},
            compile_file(Bin, NewComp, html, [S|Spec]);
        {error, Reason} ->
            S = case Comp#comp.check_script of
                    true  ->
                        io:put_chars(Reason),
                        Reason;
                    false ->
                        yaws:elog("~s", [Reason]),
                        ?F("~n<pre>~n~s~n</pre>~n", [Reason])
                end,
            compile_file(Bin, NewComp#comp{nberrors=NewComp#comp.nberrors+1},
                         html, [{error, Comp#comp.numchars+N, S}|Spec])
    end.

join_data([])                          -> [];
join_data([{data, I}, {data, J}|Tail]) -> join_data([{data, I+J}|Tail]);
join_data([H|T])                       -> [H|join_data(T)].

parse_binding(<<>>) ->
    not_found;
parse_binding(<<"%%",_/binary>>) ->
    not_found;
parse_binding(Bin) ->
    parse_binding(Bin, 0).

parse_binding(<<"%%",Bin/binary>>, NumChars) ->
    {ok, NumChars+2, Bin};
parse_binding(<<C,Bin/binary>>, NumChars) when ?IS_BINDING_CHAR(C) ->
    parse_binding(Bin, NumChars+1);
parse_binding(_, _) ->
    not_found.

parse_tag_attribute(<<>>) ->
    not_found;
parse_tag_attribute(Bin) ->
    case skip_space(Bin) of
        {ok, Lines, Skipped, <<$=,Data/binary>>} ->
            parse_tag_attribute(Data, Lines, Skipped+1);
        _ ->
            not_found
    end.

parse_tag_attribute(Bin, Lines, NumChars) ->
    case skip_space(Bin) of
        {ok, N, Skipped, <<$",Data/binary>>} ->
            parse_tag_attribute(Data, $", [], Lines+N, NumChars+Skipped+1);
        {ok, N, Skipped, <<$',Data/binary>>} ->
            parse_tag_attribute(Data, $', [], Lines+N, NumChars+Skipped+1);
         _ ->
            not_found
    end.

parse_tag_attribute(<<>>, _Sep, _Value, _Lines, _NumChars) ->
    not_found;
parse_tag_attribute(<<Sep,Bin/binary>>, Sep, Value, Lines, NumChars) ->
    {ok, Lines, NumChars+1, lists:reverse(Value), Bin};
parse_tag_attribute(<<$\\,Sep,Bin/binary>>, Sep, Value, Lines, NumChars) ->
    parse_tag_attribute(Bin, Sep, [Sep,$\\|Value], Lines, NumChars+2);
parse_tag_attribute(<<C,Bin/binary>>, Sep, Value, Lines, NumChars) ->
    parse_tag_attribute(Bin, Sep, [C|Value], Lines, NumChars+1).

skip_space(Bin) ->
    skip_space(Bin, 0, 0).

skip_space(<<$\n,Bin/binary>>, Lines, NumChars) ->
    skip_space(Bin, Lines+1, NumChars);
skip_space(<<C,Bin/binary>>, Lines, NumChars) when ?IS_SPACE(C) ->
    skip_space(Bin, Lines, NumChars+1);
skip_space(Bin, Lines, NumChars) ->
    {ok, Lines, NumChars, Bin}.

handle_erlang_block(Comp) ->
    {Line, _, ModName, _} = Comp#comp.erlcode,
    case check_module_name(ModName, Comp) of
        ok ->
            {Module, File} = get_module_info(ModName, Comp),
            ?Debug("Writting generated module ~s in file ~s~n", [Module, File]),
            case dump_erlang_block(File, Module, Comp) of
                ok ->
                    Res = compile_and_load_erlang_block(File, Comp),
                    file:delete(File),
                    Res;
                {error, Reason} ->
                    S = io_lib:format("Error in file ~s at line ~p~n"
                                      "    Failed to create temp file '~s': ~s~n",
                                      [Comp#comp.script, Line,
                                       File, file:format_error(Reason)]),
                    {error, lists:flatten(S)}
            end;
        {error, Reason} ->
            S = io_lib:format("Error in file ~s at line ~p~n"
                              "    Cannot create generated module '~s': ~s~n",
                              [Comp#comp.script, Line,
                               ModName, Reason]),
            {error, lists:flatten(S)}
    end.

get_module_info(undefined, Comp) ->
    N = integer_to_list(Comp#comp.nummod),
    case Comp#comp.check_script of
        true  ->
            YFile   = filename:rootname(Comp#comp.script),
            ModName = lists:flatten([YFile, "_yaws_", N]),
            {filename:basename(ModName), ModName++".erl"};
        false ->
            Dir     = yaws:id_dir((Comp#comp.gc)#gconf.id),
            ModName = lists:flatten(["m_", Comp#comp.hash, $_, N]),
            {ModName, filename:join([Dir, ModName++".erl"])}
    end;
get_module_info(ModName, Comp) ->
    case Comp#comp.check_script of
        true  ->
            Dir = filename:dirname(Comp#comp.script),
            {ModName, filename:join([Dir, ModName++".erl"])};
        false ->
            Dir = yaws:id_dir((Comp#comp.gc)#gconf.id),
            {ModName, filename:join([Dir, ModName++".erl"])}
    end.

check_module_name(undefined, _) ->
    ok;
check_module_name("", _) ->
    {error, "empty module name"};
check_module_name(ModName, Comp) ->
    Mod = list_to_atom(ModName),
    case code:is_loaded(Mod) of
        {file, _} ->
            case lists:keyfind(yawsfile, 1, Mod:module_info(attributes)) of
                {yawsfile, Script} when Script =:= Comp#comp.script ->
                    ok;
                {yawsfile, OtherSript} ->
                    S = io_lib:format("try to override generated module "
                                      "owned by script ~s", [OtherSript]),
                    {error, S};
                false ->
                    S = io_lib:format("try to override existing module", []),
                    {error, S}
            end;
        false ->
            ok
    end.

dump_erlang_block(File, Module, Comp) ->
    case file:open(File, [write]) of
        {ok, Fd} ->
            GC = Comp#comp.gc,
            {SLine, ELine, _, Code} = Comp#comp.erlcode,
            Data = [
                    "-module('",Module,"').\n",
                    "\n",
                    "-export([out/1]).\n",
                    "\n",
                    "-yawsfile(\"",Comp#comp.script,"\").\n",
                    "\n",
                    "%%\n",
                    "%% code between lines ",integer_to_list(SLine)," and ",
                    integer_to_list(ELine),"\n",
                    "%%\n",
                    "\n",
                    "-import(yaws_api, [f/2, fl/1, postvar/2, queryvar/2]).\n",
                    "\n",
                    "-include(\"",GC#gconf.yaws_dir,"/include/yaws_api.hrl\").\n",
                    "\n",
                    lists:reverse(Code)
                   ],
            file:write(Fd, Data),
            file:close(Fd),
            ok;
        Else ->
            Else
    end.

compile_and_load_erlang_block(File, Comp) ->
    case compile_erlang_block(File, Comp) of
        {ok, Mod, Bin, Ws} ->
            report_compile_warnings(Comp, Ws),
            load_erlang_block(Mod, Bin, Comp);
        {error, Es, Ws} ->
            {error, format_compile_error(Comp, Es, Ws)}
    end.

compile_erlang_block(File, Comp) ->
    case compile:file(File, Comp#comp.comp_opts) of
        {ok, Module} ->
            {ok, Module, <<>>, []};
        {ok, Module, Binary} when is_binary(Binary) ->
            {ok, Module, Binary, []};
        {ok, Module, Warnings} ->
            {ok, Module, <<>>, Warnings};
        {ok, Module, Binary, Warnings} ->
            {ok, Module, Binary, Warnings};
        {error, Errors, Warnings} ->
            {error, Errors, Warnings}
    end.

load_erlang_block(_Mod, <<>>, _Comp) ->
    skip;
load_erlang_block(Mod, Bin, Comp) ->
    {Line, _, _, _} = Comp#comp.erlcode,
    case code:load_binary(Mod, "", Bin) of
        {module, Mod} ->
            case lists:member({out,1}, Mod:module_info(exports)) of
                true ->
                    {ok, Mod, out};
                false ->
                    S = io_lib:format("Error in file ~s at line ~p~n"
                                      "    out/1 is not defined~n",
                                      [Comp#comp.script, Line]),
                    {error, lists:flatten(S)}
            end;
        Err ->
            S = io_lib:format("Error in file ~s at line ~p~n"
                              "    Cannot load generated module ~p: ~p~n",
                              [Comp#comp.script, Line, Mod, Err]),
            {error, lists:flatten(S)}
    end.

compile_opts(GC, Opts0, CheckFlag) ->
    ?Debug("Includes = ~p~n", [GC#gconf.include_dir]),
    I = lists:map(fun(Dir) -> {i, Dir} end, GC#gconf.include_dir),
    Warnings = if
                   CheckFlag == true -> [return_warnings];
                   true              -> []
               end,
    Opts = [binary, return_errors] ++ Warnings ++ I ++ Opts0,
    ?Debug("Compile options = ~p~n", [Opts]),
    Opts.

format_compile_error(Comp, Errors, Warnings) ->
    S1 = report_errors(Comp,   Errors),
    S2 = report_warnings(Comp, Warnings),
    S = io_lib:format("Dynamic compile error:~n~s~s", [S1, S2]),
    lists:flatten(S).

report_compile_warnings(#comp{check_script=true}=Comp, Warnings) ->
    io:put_chars(report_warnings(Comp, Warnings));
report_compile_warnings(_, _) ->
    ok.

line(N) -> N - 15.

%% -----------------------------------------------------------------
%% Adapted from compile.erl.
report_errors(C, Es0) ->
    File = C#comp.script,
    {StartLine, _, _, _} = C#comp.erlcode,
    SLine = line(StartLine),
    lists:flatmap(fun ({{_F,_L},Eds}) -> format_errors(File, SLine, Eds);
                      ({_F,Eds})      -> format_errors(File, SLine, Eds)
                  end, Es0).

report_warnings(C, Ws0) ->
    File = C#comp.script,
    {StartLine, _, _, _} = C#comp.erlcode,
    SLine = line(StartLine),
    lists:flatmap(fun({{_F,_L},Eds}) -> format_warnings(File, SLine, Eds);
                     ({_F,Eds})      -> format_warnings(File, SLine, Eds)
                  end, Ws0).

format_warnings(F, SLine, [{none,Mod,E}|Es]) ->
    M = io_lib:format("~s: Warning: ~s~n", [F,Mod:format_error(E)]),
    [M|format_warnings(F, SLine, Es)];
format_warnings(F, SLine, [{{Line0,_},Mod,E}|Es]) ->
    format_warnings(F, SLine, [{Line0,Mod,E}|Es]);
format_warnings(F, SLine, [{Line0,Mod,E}|Es]) ->
    Line = erlang:max(0, Line0 + SLine),
    M = io_lib:format("~s:~w: Warning: ~s~n", [F,Line,Mod:format_error(E)]),
    [M|format_warnings(F, SLine, Es)];
format_warnings(F, SLine, [{Mod,E}|Es]) ->
    M = io_lib:format("~s: Warning: ~s~n", [F,Mod:format_error(E)]),
    [M|format_warnings(F, SLine, Es)];
format_warnings(_, _, []) ->
    [].

format_errors(F, SLine, [{none,Mod,E}|Es]) ->
    M = io_lib:format("~s: ~s~n", [F,Mod:format_error(E)]),
    [M|format_errors(F, SLine, Es)];
format_errors(F, SLine, [{{Line0,_},Mod,E}|Es]) ->
    format_errors(F, SLine, [{Line0,Mod,E}|Es]);
format_errors(F, SLine, [{Line0,Mod,E}|Es]) ->
    Line = erlang:max(0, Line0 + SLine),
    M = io_lib:format("~s:~w: ~s~n", [F,Line,Mod:format_error(E)]),
    [M|format_errors(F, SLine, Es)];
format_errors(F, SLine, [{Mod,E}|Es]) ->
    M = io_lib:format("~s: ~s~n", [F,Mod:format_error(E)]),
    [M|format_errors(F, SLine, Es)];
format_errors(_F, _SLine, []) ->
    [].
