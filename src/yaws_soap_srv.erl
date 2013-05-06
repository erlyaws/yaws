%%%-------------------------------------------------------------------
%%% Created : 29 Nov 2006 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Author  : Willem de Jong (w.a.de.jong@gmail.com).
%%% Desc    : A SOAP server.
%%%-------------------------------------------------------------------
-module(yaws_soap_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, start_link/2,
         setup/1, setup/2, setup/3,
         worker/1,
         handler/4
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("../include/yaws_api.hrl").
-include("../include/yaws.hrl").
-include("../include/soap.hrl").

-define(SERVER, ?MODULE).

%% State
-record(s, {
          num_of_workers = 0,
          workers = [],      % list of Pids
          busy_workers = [], % list of {Pids, From} pairs.
          queue = [],        % list of waiting jobs
          wsdl_list = []     % list of {Id, WsdlModel} tuples, where Id == {M,F}
         }).

-define(OK_CODE, 200).
-define(BAD_MESSAGE_CODE, 400).
%% -define(METHOD_NOT_ALLOWED_CODE, 405).
-define(SERVER_ERROR_CODE, 500).

-define(DEFAULT_NUM_OF_WORKERS, 3).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    start_link([]).
%%
start_link(N) when is_integer(N) ->
    start_link([], N);
start_link(L) ->
    start_link(L, ?DEFAULT_NUM_OF_WORKERS).
%%
start_link(L, N) ->
    %% We are dependent on erlsom
    case code:ensure_loaded(erlsom) of
        {error, _} ->
            Emsg = "could not load erlsom",
            error_logger:error_msg("~p: exiting, reason: ~s~n",
                                   [?MODULE, Emsg]),
            {error, Emsg};
        {module, erlsom} ->
            gen_server:start_link({local, ?SERVER}, ?MODULE, {L, N}, [])
    end.

%%% To be called from yaws_rpc.erl
%%% Return according to yaws_rpc:eval_payload/6
handler(Args, Id, Payload, SessionValue) ->
    Headers = Args#arg.headers,
    SoapAction = yaws_soap_lib:findHeader("SOAPAction", Headers#headers.other),
    case gen_server:call(?SERVER, {request, Id, Payload,
                                   SessionValue, SoapAction}, infinity) of
        {ok, XmlDoc, ResCode, undefined} ->
            {false, XmlDoc, ResCode};
        {ok, XmlDoc, ResCode, SessVal} ->
            {true, 0, SessVal, XmlDoc, ResCode};
        {error, _, _} = Error ->
            Error;
        false ->
            false    % soap notify
    end.

%% Setup a SOAP interface according to the config file.
setup(_ConfigFile) ->
    tbd.

setup(Id, WsdlFile) when is_tuple(Id),size(Id)==2 ->
    Wsdl = yaws_soap_lib:initModel(WsdlFile),
    gen_server:call(?SERVER, {add_wsdl, Id, Wsdl}, infinity).

%% PrefixOrOptions can be either a prefix (a String) or a property
%% list. It is used to construct the options that are passed to Erlsom
%% to compile the WSDL file. Passing a string ("Prefix") is equivalent
%% to [{prefix, "Prefix"}].
%% If a list of erlsom options is passed, and this does not contain
%% the {prefix, ...} option, the yaws_soap default ("p") will be used.
setup(Id, WsdlFile, PrefixOrOptions) when is_tuple(Id),size(Id)==2 ->
    Wsdl = yaws_soap_lib:initModel(WsdlFile, PrefixOrOptions),
    gen_server:call(?SERVER, {add_wsdl, Id, Wsdl}, infinity).

%% Send message to worker
worker(X) ->
    gen_server:cast(?MODULE, {worker, X, self()}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init({L, N}) -> % { [ {{Mod,Handler}, WsdlFile} ] , NumOfWorkers }
    WsdlList = lists:foldl(fun(SoapSrvMod, OldList) ->
                                   setup_on_init( SoapSrvMod, OldList )
                           end,[],L),
    gen_server:cast(?MODULE, complete_init),
    {ok, #s{wsdl_list = WsdlList, num_of_workers = N}}.

setup_on_init( {Id, WsdlFile}, OldList ) when is_tuple(Id),size(Id) == 2 ->
    Wsdl = yaws_soap_lib:initModel(WsdlFile),
    uinsert({Id, Wsdl}, OldList);
setup_on_init( {Id, WsdlFile, Prefix}, OldList ) when is_tuple(Id),
                                                      size(Id) == 2 ->
    Wsdl = yaws_soap_lib:initModel(WsdlFile, Prefix),
    uinsert({Id, Wsdl}, OldList).

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({add_wsdl, Id, WsdlModel}, _From, State) ->
    yaws_soap_sup:setup({Id, WsdlModel}),
    NewWsdlList = uinsert({Id, WsdlModel}, State#s.wsdl_list),
    {reply, ok, State#s{wsdl_list = NewWsdlList}};
%%
handle_call(Req = {request, _Id, _Payload, _SessionValue, _SoapAction}, From, State) ->
    {noreply, call_worker({int_request, Req, From}, State)};
handle_call(_, _, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(complete_init, State) ->
    {ok, _} = supervisor:start_child(yaws_sup, yaws_soap_sup:child_spec()),
    yaws_soap_sup:start_children(State#s.num_of_workers),
    {noreply, State};
%%
handle_cast({worker, started, Pid}, State = #s{busy_workers = Busy}) ->
    erlang:monitor(process, Pid),
    case State#s.wsdl_list of
        [] -> ok;
        Wsdls -> yaws_soap_srv_worker:setup(Pid, init, Wsdls)
    end,
    {noreply, State#s{busy_workers = [{Pid, dummy} | Busy]}};
%%
handle_cast({worker, done, Pid}, State) ->
    #s{workers = Workers,
       busy_workers = Busy,
       queue = Queue} = State,
    State1 = State#s{workers = [Pid | Workers],
                     busy_workers = lists:keydelete(Pid, 1, Busy)},
    State2 = case Queue of
                 [] -> State1;
                 [Q | Qs] -> call_worker(Q, State1#s{queue = Qs})
             end,
    {noreply, State2};
%%
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', _, process, Pid, Info}, State) ->
    #s{workers = Workers,
       busy_workers = Busy} = State,
    case lists:keysearch(Pid, 1, Busy) of
        {value, {Pid, From}} ->
            gen_server:reply(
              From, srv_error(f("Process termination: ~p", [Info])));
        _ -> ok
    end,
    {noreply, State#s{workers = lists:delete(Pid, Workers),
                      busy_workers = lists:keydelete(Pid, 1, Busy)}};
%%
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

f(S,A) -> lists:flatten(io_lib:format(S,A)).

srv_error(Error) ->
    error_logger:error_msg("~p(~p): Srv Error: ~p~n",
                           [?MODULE, ?LINE, Error]),
    Fault = yaws_soap_lib:makeFault("Server", "Server error"),
    {error, Fault, ?SERVER_ERROR_CODE}.

uinsert({K,_} = E, [{K,_}|T]) -> [E|T];
uinsert(E, [H|T])             -> [H|uinsert(E,T)];
uinsert(E, [])                -> [E].

call_worker(Req = {int_request, _, From},
            State = #s{workers = [Worker | Workers],
                       busy_workers = Busy})->
    case catch yaws_soap_srv_worker:call(Worker, Req) of
        ok ->
            State#s{workers = Workers,
                    busy_workers = [{Worker, From} | Busy]};
        {'EXIT',{noproc,_}} ->
            call_worker(Req, State#s{workers = Workers})
    end;
%%
call_worker(Req, State = #s{queue = Queue}) ->
    State#s{queue = [Req | Queue]}.
