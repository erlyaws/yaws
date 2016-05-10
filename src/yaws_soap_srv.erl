%%%-------------------------------------------------------------------
%%% Created : 29 Nov 2006 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Author  : Willem de Jong (w.a.de.jong@gmail.com).
%%% Desc    : A SOAP server.
%%%-------------------------------------------------------------------
-module(yaws_soap_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         setup/1, setup/2, setup/3,
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
          wsdl_list = []  % list of {Id, WsdlModel} tuples, where Id == {M,F}
         }).

-define(OK_CODE, 200).
-define(BAD_MESSAGE_CODE, 400).
%% -define(METHOD_NOT_ALLOWED_CODE, 405).
-define(SERVER_ERROR_CODE, 500).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    start_link([]).
start_link(L) ->
    %% We are dependent on erlsom
    case code:ensure_loaded(erlsom) of
        {error, _} ->
            Emsg = "could not load erlsom",
            error_logger:error_msg("~p: exiting, reason: ~s~n",
                                   [?MODULE, Emsg]),
            {error, Emsg};
        {module, erlsom} ->
            gen_server:start_link({local, ?SERVER}, ?MODULE, L, [])
    end.

%%% To be called from yaws_rpc.erl
%%% Return according to yaws_rpc:eval_payload/6
handler(Args, Id, Payload, SessionValue) ->
    {ok, WsdlModel} = gen_server:call(?SERVER, {get_wsdl, Id}, infinity),
    Headers = Args#arg.headers,
    SoapAction = yaws_soap_lib:findHeader("SOAPAction", Headers#headers.other),
    Res = request(WsdlModel, Id, Payload, SessionValue, SoapAction),
    case Res of
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
init(L) -> %% [ {{Mod,Handler}, WsdlFile} ]
    WsdlList = lists:foldl( fun( SoapSrvMod, OldList) ->
                                    setup_on_init( SoapSrvMod, OldList )
                            end,[],L),
    {ok, #s{wsdl_list = WsdlList}}.

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
    NewWsdlList = uinsert({Id, WsdlModel}, State#s.wsdl_list),
    {reply, ok, State#s{wsdl_list = NewWsdlList}};
handle_call({get_wsdl, Id}, _From, State) ->
    {reply, get_model(State, Id), State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
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

request(Model, {M,F}, {Req, Attachments}, SessionValue, Action) ->
    %%error_logger:info_report([?MODULE, {payload, Req}]),
    case catch yaws_soap_lib:parseMessage(Req, Model) of
        {ok, Header, Body} ->
            %% call function
            result(Model, catch apply(M, F, [Header, Body,
                                             Action, SessionValue,
					     Attachments]));
        {error, Error} ->
            cli_error(Error);
        OtherError ->
            srv_error(io_lib:format("Error parsing message: ~p", [OtherError]))
    end;
request(Model, {M,F}, Req, SessionValue, Action) ->
    %%error_logger:info_report([?MODULE, {payload, Req}]),
    Umsg = (catch erlsom_lib:toUnicode(Req)),
    case catch yaws_soap_lib:parseMessage(Umsg, Model) of
        {ok, Header, Body} ->
            %% call function
            result(Model, catch apply(M, F, [Header, Body,
                                             Action, SessionValue]));
        {error, Error} ->
            cli_error(Error);
        OtherError ->
            srv_error(io_lib:format("Error parsing message: ~p", [OtherError]))
    end.

%%% Analyse the result and produce some output
result(Model, {ok, ResHeader, ResBody, ResCode, SessVal}) ->
    return(Model, ResHeader, ResBody, ResCode, SessVal, undefined);
result(Model, {ok, ResHeader, ResBody}) ->
    return(Model, ResHeader, ResBody, ?OK_CODE, undefined, undefined);
result(Model, {ok, ResHeader, ResBody, Files}) ->
    return(Model, ResHeader, ResBody, ?OK_CODE, undefined, Files);
result(_Model, {error, client, ClientMssg}) ->
    cli_error(ClientMssg);
result(_Model, false) ->   % soap notify !
    false;
result(_Model, Error) ->
    srv_error(io_lib:format("Error processing message: ~p", [Error])).

return(#wsdl{model = Model}, ResHeader, ResBody, ResCode, SessVal, Files) ->
    return(Model, ResHeader, ResBody, ResCode, SessVal, Files);
return(Model, ResHeader, ResBody, ResCode, SessVal, Files)
  when not is_list(ResBody) ->
    return(Model, ResHeader, [ResBody], ResCode, SessVal, Files);
return(Model, ResHeader, ResBody, ResCode, SessVal, Files) ->
    %% add envelope
    Header2 = case ResHeader of
                  undefined -> undefined;
                  _         -> #'soap:Header'{choice = ResHeader}
              end,
    Envelope = #'soap:Envelope'{'Body' =  #'soap:Body'{choice = ResBody},
                                'Header' = Header2},
    case catch erlsom:write(Envelope, Model) of
        {ok, XmlDoc} ->
	    case Files of
		undefined ->
		    {ok, XmlDoc, ResCode, SessVal};
		_ ->
		    DIME = yaws_dime:encode(XmlDoc, Files),
		    {ok, DIME, ResCode, SessVal}
	    end;
        {error, WriteError} ->
            srv_error(f("Error writing XML: ~p", [WriteError]));
        OtherWriteError ->
            error_logger:error_msg("~p(~p): OtherWriteError=~p~n",
                                   [?MODULE, ?LINE, OtherWriteError]),
            srv_error(f("Error writing XML: ~p", [OtherWriteError]))
    end.

f(S,A) -> lists:flatten(io_lib:format(S,A)).

cli_error(Error) ->
    error_logger:error_msg("~p(~p): Cli Error: ~p~n",
                           [?MODULE, ?LINE, Error]),
    Fault = yaws_soap_lib:makeFault("Client", "Client error"),
    {error, Fault, ?BAD_MESSAGE_CODE}.

srv_error(Error) ->
    error_logger:error_msg("~p(~p): Srv Error: ~p~n",
                           [?MODULE, ?LINE, Error]),
    Fault = yaws_soap_lib:makeFault("Server", "Server error"),
    {error, Fault, ?SERVER_ERROR_CODE}.




get_model(State, Id) ->
    case lists:keysearch(Id, 1, State#s.wsdl_list) of
        {value, {_, Model}} -> {ok, Model};
        _                   -> {error, "model not found"}
    end.

uinsert({K,_} = E, [{K,_}|T]) -> [E|T];
uinsert(E, [H|T])             -> [H|uinsert(E,T)];
uinsert(E, [])                -> [E].
