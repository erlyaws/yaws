%%%----------------------------------------------------------------------
%%% File    : server_sent_events.erl
%%% Author  : Steve Vinoski <vinoski@ieee.org>
%%% Purpose : Server-Sent Events example
%%% Created : 1 June 2012 by Steve Vinoski <vinoski@ieee.org>
%%%----------------------------------------------------------------------
-module(server_sent_events).
-behaviour(gen_server).

-include("yaws_api.hrl").

%% API
-export([out/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          sock,
          yaws_pid,
          timer
         }).

out(A) ->
    case (A#arg.req)#http_request.method of
        'GET' ->
            case yaws_api:get_header(A#arg.headers, accept) of
                undefined ->
                    {status, 406};
                Accept ->
                    case string:str(Accept, "text/event-stream") of
                        0 ->
                            {status, 406};
                        _ ->
                            {ok, Pid} = gen_server:start(?MODULE, [A], []),
                            yaws_sse:headers(Pid)
                    end
            end;
        _ ->
            {status, 405}
    end.

init([Arg]) ->
    process_flag(trap_exit, true),
    {ok, #state{sock=Arg#arg.clisock}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ok, YawsPid}, State) ->
    {ok, Timer} = timer:send_interval(1000, self(), tick),
    {noreply, State#state{yaws_pid=YawsPid, timer=Timer}};
handle_info({discard, _YawsPid}, State) ->
    %% nothing to do
    {stop, normal, State};
handle_info(tick, #state{sock=Socket}=State) ->
    Time = erlang:localtime(),
    Data = yaws_sse:data(httpd_util:rfc1123_date(Time)),
    yaws_sse:send_events(Socket, Data),
    {noreply, State};
handle_info({tcp_closed, _}, State) ->
    {stop, normal, State#state{sock=closed}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{sock=Socket, yaws_pid=YawsPid, timer=Timer}) ->
    case Timer of
        undefined ->
            ok;
        _ ->
            timer:cancel(Timer)
    end,
    yaws_api:stream_process_end(Socket, YawsPid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
