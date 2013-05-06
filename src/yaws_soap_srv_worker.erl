%%%-------------------------------------------------------------------
%%% File    : yaws_soap_srv_worker.erl
%%% Author  : Jan Nyström <JanHenryNystrom@gmail.com>
%%%           Andreas Hellström <andreas.hellstrom@teliasonera.com>
%%% Desc    : Handling of soap workers.
%%% Created : 4 Mar 2008 by Jan Nyström
%%%-------------------------------------------------------------------
-module(yaws_soap_srv_worker).

-behaviour(gen_server).

%% API
-export([start_link/0, call/2, setup/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("../include/yaws_api.hrl").
-include("../include/yaws.hrl").
-include("../include/soap.hrl").

-define(SERVER, ?MODULE).

-record(state, {wsdl_list = []}).

-define(OK_CODE, 200).
-define(BAD_MESSAGE_CODE, 400).
-define(SERVER_ERROR_CODE, 500).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() -> gen_server:start_link(?MODULE, no_args, []).

setup(Server, Type, Wsdls) -> gen_server:cast(Server, {wsdl, Type, Wsdls}).

call(Server, Request) -> gen_server:call(Server, Request).

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
init(no_args) ->
    yaws_soap_srv:worker(started),
    yaws_soap_srv:worker(done),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({int_request,
             {request, Id, Payload, SessionValue, SoapAction}, OrigFrom}, From, State) ->
    gen_server:reply(From, ok),
    Reply = request(Id, Payload, SessionValue, SoapAction, State),
    gen_server:reply(OrigFrom, Reply),
    yaws_soap_srv:worker(done),
    {noreply, State};
handle_call(_Request, _, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({wsdl, init, Wsdls}, State) ->
    {noreply, State#state{wsdl_list = Wsdls}};
handle_cast({wsdl, add, W = {Id, _}}, State = #state{wsdl_list = List}) ->
    {noreply, State#state{wsdl_list = [W | lists:keydelete(Id, 1, List)]}};
handle_cast(_, State) ->
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

request({M,F} = Id, {Req, Attachments}, SessionValue, Action, State) ->
    Model = (catch get_model(State, Id)),
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
request({M,F} = Id, Req, SessionValue, Action, State) ->
    %%error_logger:info_report([?MODULE, {payload, Req}]),
    Model = (catch get_model(State, Id)),
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
    {value, {_, Model}} = lists:keysearch(Id, 1, State#state.wsdl_list),
    Model.

%% utf8_encode(UnicodeString) ->
%%    binary_to_list(unicode:characters_to_binary(UnicodeString)).
