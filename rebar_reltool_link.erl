%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2011 Dmitry Demeschuk
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
    %% Create symlinks to the current directory in the ?FAKE_LIB_DIR
    %% directory to include the root application to the release. Works
    %% for UNIX systems only.
    case os:type() of
        {unix, _} -> maybe_symlink_root_dir();
        _ -> ok
    end.

post_generate(_, _) ->
    case has_symlink_dir() of
        {true, SymlinkDir} -> rebar_file_utils:rm_rf(SymlinkDir);
        false -> ok
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).
-define(FAKE_LIB_DIR, "fake_lib_dir").
-define(FAKE_LIB_DIR2, "../fake_lib_dir").

maybe_symlink_root_dir() ->
    case rebar_app_utils:is_app_dir("..") of
        {true, RootAppSrc} ->
            RootApp = rebar_app_utils:app_name(RootAppSrc),
            SymlinkDir = filename:join(["..", ?FAKE_LIB_DIR, RootApp]),
            case filelib:is_dir(SymlinkDir) of
                false ->
                    rebar_utils:sh(?FMT("mkdir -p ~s", [SymlinkDir]), []);
                true ->
                    ok
            end,
            Files = [F || F <- filelib:wildcard("../*"), F =/= ?FAKE_LIB_DIR2],
            [begin
                 Filename = filename:basename(File),
                 Source = filename:absname(filename:join([File])),
                 Target = filename:join([SymlinkDir, Filename]),
                 rebar_utils:sh(?FMT("ln -Fs ~s ~s", [Source, Target]), [])
             end
             || File <- Files],
            ok;
        false ->
            ok
    end.

has_symlink_dir() ->
    case rebar_app_utils:is_app_dir("..") of
        {true, RootAppSrc} ->
            RootApp = rebar_app_utils:app_name(RootAppSrc),
            SymlinkDir = filename:join(["..", ?FAKE_LIB_DIR, RootApp]),
            case filelib:is_dir(SymlinkDir) of
                true -> {true, SymlinkDir};
                false -> false
            end;
        false ->
            false
    end.
