%%%-------------------------------------------------------------------
%%% File    : yaws_log_file_h.erl
%%% Author  :  <klacke@hyber.org>
%%% Description :
%%%
%%% Created : 11 Mar 2004 by  <klacke@hyber.org>
%%%-------------------------------------------------------------------


%% Just extending the error_logger_file_h abit,
%% If they change the internals of that module, this module
%% breaks, but then again, the otp crew doesn't ever appear to
%% change anything that might break anything .....

-module(yaws_log_file_h).
-behaviour(gen_event).
-include_lib("kernel/include/file.hrl").

-export([init/1,
         handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).



%% This one is used when we are started directly.
init(File) ->
    process_flag(trap_exit, true),
    {ok, [Major,Minor], _} = io_lib:fread("~d.~d", erlang:system_info(version)),
    case file:open(File, [append]) of
        {ok, Fd} when {Major,Minor} < {7,1} ->  %% Pre 18.1
            {ok, {Fd, File, []}};
        {ok, Fd} ->                             %% Post 18.1
            {ok, {st, Fd, File, [], unlimited}};
        Error ->
            error_logger:error_msg(
              "Failed to set Yaws error report handler: ~p~n", [Error]
             ),
            Error
    end.

%% Pre 18.1
handle_call(reopen, {Fd, File, Prev}) ->
    {ok, ok, {reopen(Fd,File), File, Prev}};
handle_call(wrap, {Fd, File, Prev}) ->
    {ok, ok, {wrap(Fd,File), File, Prev}};
handle_call(size, {Fd, File, Prev}) ->
    {ok, size(Fd,File), {Fd, File, Prev}};

%% Post 18.1
handle_call(reopen, {st, Fd, File, Prev, Depth}) ->
    {ok, ok, {st, reopen(Fd,File), File, Prev, Depth}};
handle_call(wrap, {st, Fd, File, Prev, Depth}) ->
    {ok, ok, {st, wrap(Fd,File), File, Prev, Depth}};
handle_call(size, {st, Fd, File, Prev, Depth}) ->
    {ok, size(Fd,File), {st, Fd, File, Prev, Depth}};

handle_call(X, S) ->
    error_logger_file_h:handle_call(X,S).


handle_event(X, S) ->
    error_logger_file_h:handle_event(X, S).
handle_info(X, S) ->
    error_logger_file_h:handle_info(X, S).


terminate(Reason, State) ->
    error_logger_file_h:terminate(Reason, State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


reopen(Fd, File) ->
    file:close(Fd),
    {ok, Fd2} = file:open(File, [write,append]),
    Fd2.

wrap(Fd, File) ->
    Old = File ++ ".old",
    file:delete(Old),
    file:close(Fd),
    file:rename(File, Old),
    {ok, Fd2} = file:open(File, [write,append]),
    Fd2.

size(Fd, File) ->
    file:sync(Fd),
    case file:read_file_info(File) of
        {ok, FI} -> {ok, FI#file_info.size};
        Error    -> Error
    end.
