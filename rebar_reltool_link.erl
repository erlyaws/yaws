%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2011 Dmitry Demeshchuk
%% Copyright (c) 2011 Tuncer Ayaz
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(rebar_reltool_link).

-export([pre_generate/2, post_generate/2]).

%% ===================================================================
%% Public API
%% ===================================================================

pre_generate(_, _) ->
    maybe_symlink_root_dir().

post_generate(_, _) ->
    case has_symlink_dir() of
        {true, SymlinkDir} ->
            ok = rebar_file_utils:rm_rf(SymlinkDir),
            ok = file:del_dir(fake_lib_dir());
        false ->
            ok
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

maybe_symlink_root_dir() ->
    case rebar_app_utils:is_app_dir("..") of
        {true, RootAppFile} ->
            RootApp = rebar_app_utils:app_name(RootAppFile),
            LibDir = fake_lib_dir(),
            SymlinkDir = filename:join([LibDir, RootApp]),
            case filelib:is_dir(SymlinkDir) of
                false ->
                    ok = filelib:ensure_dir(filename:join(SymlinkDir, "dummy"));
                true ->
                    ok
            end,
            Files = [F || F <- filelib:wildcard("../*"), F =/= LibDir],
            make_links(SymlinkDir, Files),
            ok;
        false ->
            ok
    end.

fake_lib_dir() ->
    filename:join("..", "fake_lib_dir").

make_links(SymlinkDir, Files) ->
    lists:foreach(
      fun(File) ->
              Filename = filename:basename(File),
              Existing = filename:absname(filename:join([File])),
              New = filename:join([SymlinkDir, Filename]),
              ok = make_symlink(Existing, New)
      end, Files).

make_symlink(Existing, New) ->
    case filelib:is_dir(New) of
        true ->
            ok = file:delete(New);
        false ->
            ok
    end,
    file:make_symlink(Existing, New).

has_symlink_dir() ->
    case rebar_app_utils:is_app_dir("..") of
        {true, RootAppFile} ->
            RootApp = rebar_app_utils:app_name(RootAppFile),
            LibDir = fake_lib_dir(),
            SymlinkDir = filename:join([LibDir, RootApp]),
            case filelib:is_dir(SymlinkDir) of
                true -> {true, SymlinkDir};
                false -> false
            end;
        false ->
            false
    end.
