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

-export([init/1,
        handle_event/2, handle_call/2, handle_info/2,
        terminate/2, code_change/3]).



%% This one is used when we are started directly.
init(File) ->
    init(File, []).

init(File, PrevHandler) ->
    process_flag(trap_exit, true),
    case file:open(File, [write, append]) of
        {ok,Fd} ->
            {ok, {Fd, File, PrevHandler}};
        Error ->
            Error
    end.

handle_call(reopen, {Fd, File, Prev}) ->
    file:close(Fd),
    {ok, Fd2} = file:open(File, [write,append]),
    {ok, ok, {Fd2, File, Prev}};

handle_call(wrap, {Fd, File, Prev}) ->
    Old = File ++ ".old",
    file:delete(Old),
    file:close(Fd),
    file:rename(File, Old),
    {ok, Fd2} = file:open(File, [write,append]),
    {ok, ok, {Fd2, File, Prev}};

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
