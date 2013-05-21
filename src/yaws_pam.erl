%%%-------------------------------------------------------------------
%%% File    : yaws_pam.erl
%%% Author  :  <klacke@hyber.org>
%%% Description :
%%%
%%% Created : 20 Dec 2005 by  <klacke@hyber.org>
%%%-------------------------------------------------------------------
-module(yaws_pam).
-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0,
         start_link/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([auth/2,
         close/1]).


-define(TO, (1000 * 60 * 3)).


-record(user, {i,    %% sid
               from, %% pid
               ref}).%% monitorref

-record(state, {i,
                port,
                mode,
                sids = [], % active sessions [#user{}]
                srv,
                reqs = []  % outstandig requests [#user{}]
               }).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
start_link(Service, UseAccounting, UseSess) ->
    Args = [Service, UseAccounting, UseSess],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

auth(User, Password) ->
    case has_nul(User) or has_nul(Password) of
        false ->
            try  gen_server:call(?MODULE, {auth, User, Password}, 15000) of
                 Ret ->
                    Ret
            catch _:_ ->
                    {no, {"auth", "timeout"}}
            end;
        true ->
            %% PAM can't handle embedded NUL (nor can the "port protocol")
            %% - and it's probably a DOS attempt anyway, do a delay
            timer:sleep(1000),
            {no, {"auth", "Authentication failure"}}
    end.

%% yaws never use close, ... no session mgmt in yaws
close(Handle) ->
    gen_server:call(?MODULE, {close, Handle}, infinity).

has_nul(<<B/binary>>) ->
    lists:member(0, binary_to_list(B));
has_nul(L) ->
    lists:member(0, L).


%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------

init([]) ->
    {Srv, Act,Sess} =
        {okundef(application:get_env(pam_service)),
         okundef(application:get_env(pam_use_acct)),
         okundef(application:get_env(pam_use_sess))},
    init([Srv, Act, Sess]);

init([undefined, _Act, _Sess]) ->
    error_logger:format("pam: need service in pam environment\n", []),
    {stop, noservice};
init([SRV, Act, Sess]) ->

    %% we never want to use the accounting
    %% in yaws

    M1 = case Act of
             undefined ->
                 "";
             true ->
                 "A";
             false ->
                 []
         end,
    %% and we definitely never want to use the
    %% the session capability in yaws since noone is never
    %% ever going to close the session
    M2 = case Sess of
             undefined ->
                 "";
             true ->
                 "S";
             false ->
                 []
         end,
    Mode = M1 ++ M2,

    %% we're not starting the portprogram now, it's done
    %% on demand.
    {ok, #state{i = 0,
                mode = Mode,
                srv = SRV,
                port = undefined,
                sids = [],
                reqs = []}}.

okundef({ok,Val}) ->
    Val;
okundef(undefined) ->
    undefined.


%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({auth, User, Password}, From, State0) ->
    State = ensure_port(State0),
    I = integer_to_list(State#state.i),
    port_command(State#state.port, [$a, User, 0, Password, 0,
                                    State#state.mode,0, I, 0]),
    Ref = erlang:monitor(process, element(1, From)),
    U = #user{i = State#state.i,
              ref = Ref,
              from = From},
    R = [U| State#state.reqs],

    {noreply, State#state{i = State#state.i + 1,
                          reqs = R}, ?TO};

handle_call({close, _Sid}, _From, State = #state{port=undefined}) ->
    {reply, ok, State};
handle_call({close, Sid}, _From, State = #state{port = Port}) ->
    case lists:keysearch(Sid, #user.i, State#state.sids) of
        {value, U} ->
            erlang:demonitor(U#user.ref);
        false ->
            ok
    end,
    port_command(Port, [$c, integer_to_list(Sid), 0]),

    {reply, ok, State#state{
                  sids = lists:keydelete(Sid, #user.i, State#state.sids)},?TO}.


%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State, ?TO}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(timeout, State) when
      State#state.port /= undefined,
      State#state.sids == [],
      State#state.reqs == [] ->
    unlink(State#state.port),
    port_close(State#state.port),
    {noreply, State#state{port = undefined}};

handle_info(timeout, State) ->
    {noreply, State, ?TO};

handle_info({'EXIT', Port, _}, State = #state{port = Port}) ->
    error_logger:format("epam port program died \n",[]),
    lists:foreach(
      fun(U) -> gen_server:reply(U#user.from, {no, "epam died"}) end,
      State#state.reqs),
    {noreply, State#state{sids = [],
                          reqs = [],
                          port = undefined
                         }};
handle_info({'DOWN', MonitorRef, _Type, _Object, _Info}, State)
  when State#state.port /= undefined ->
    case lists:keysearch(MonitorRef, #user.ref, State#state.sids) of
        {value, U} ->
            port_command(State#state.port,
                         [$c, integer_to_list(U#user.i), 0]),
            S2 = lists:keydelete(MonitorRef, #user.ref, State#state.sids),
            {noreply, State#state{sids = S2}, ?TO};
        false ->
            {noreply, State, ?TO}
    end;

handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info}, State) ->
    {noreply, State};

handle_info({_Port, {data, Str}}, State) ->
    case string:tokens(Str, " \n") of
        ["pam", IntStr | Reply] ->
            I = list_to_integer(IntStr),
            {value, U} = lists:keysearch(I, #user.i, State#state.reqs),
            R = case reply(U#user.from, I, Reply) of
                    yes ->
                        [U |State#state.sids];
                    no ->
                        State#state.sids
                end,
            {noreply,
             State#state{reqs = lists:keydelete(I,#user.i,State#state.reqs),
                         sids = R}, ?TO};
        _Other ->
            error_logger:format("epam: ~s", [Str]),
            {noreply, State, ?TO}
    end.



%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


reply(From, Sid, ["yes"]) ->
    gen_server:reply(From, {yes, Sid}),
    yes;
reply(From, _Sid, ["no", What |Reason ]) ->
    gen_server:reply(From, {no, {What, fsp(Reason)}}),
    no.

fsp([]) -> [];
fsp([X]) -> X;
fsp([H|T]) -> H ++ " " ++ fsp(T).



ensure_port(S = #state{port = undefined, srv = Srv}) ->
    Prg0 = filename:dirname(code:which(?MODULE)) ++
        "/../priv/epam ",

    Prg = Prg0 ++ Srv,
    P = open_port({spawn, Prg}, [{packet, 2}]),
    receive
        {P, {data, "ok"}} ->
            S#state{port = P};
        {P, {data, ErrStr}} ->
            error_logger:format("epam: ~s~n", [ErrStr]),
            exit(noepam);
        {'EXIT', P, _} ->
            error_logger:format("yaws_pam: Cannot start epam",[]),
            exit(noepam)
    end;
ensure_port(S)->
    S.


