%%%-------------------------------------------------------------------
%%% Created : 29 Nov 2006 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Author  : Willem de Jong (w.a.de.jong@gmail.com).
%%% Desc.   : Common SOAP code.
%%%-------------------------------------------------------------------

%%% modified (WdJ, May 2007): deal with imports in the WSDL.
%%% modified (WdJ, August 2007): the WSDL can contain more than 1 schema

-module(yaws_soap_lib).

-export([initModel/1, initModel/2,
         initModelFile/1,
         config_file_xsd/0,
         call/3, call/4, call/5, call/6,
         call_attach/4, call_attach/5, call_attach/7,
         write_hrl/2, write_hrl/3,
         findHeader/2,
         parseMessage/2,
         makeFault/2,
         is_wsdl/1, wsdl_model/1, wsdl_op_service/1,
         wsdl_op_port/1, wsdl_op_operation/1,
         wsdl_op_binding/1, wsdl_op_address/1,
         wsdl_op_action/1, wsdl_operations/1,
         get_operation/2
        ]).


%%% For testing...
-export([qtest/0]).


-include("../include/soap.hrl").

-define(HTTP_REQ_TIMEOUT, 20000).

%%-define(dbg(X,Y),
%%        error_logger:info_msg("*dbg ~p(~p): " X,
%%                              [?MODULE, ?LINE | Y])).
-define(dbg(X,Y), true).


-record(yaws_soap_config, {atts, xsd_path,  user_module, wsdl_file, add_files}).
-record(xsd_file, {atts, name, prefix, import_specs}).
-record(import_specs, {atts, namespace, prefix, location}).

-define(DefaultPrefix, "p").


%%%
%%% Writes the header file (record definitions) for a WSDL file
%%%
write_hrl(WsdlURL, Output) when is_list(WsdlURL) ->
    write_hrl(initModel(WsdlURL), Output);
write_hrl(#wsdl{model = Model}, Output) when is_list(Output) ->
    erlsom:write_hrl(Model, Output).

write_hrl(WsdlURL, Output, PrefixOrOptions)
  when is_list(WsdlURL),is_list(PrefixOrOptions) ->
    write_hrl(initModel(WsdlURL, PrefixOrOptions), Output).



%%% For testing only...
qtest() ->
    call("http://www.webservicex.net/WeatherForecast.asmx?WSDL",
         "GetWeatherByPlaceName",
         ["Boston"]).

%%% --------------------------------------------------------------------
%%% Access functions
%%% --------------------------------------------------------------------
is_wsdl(Wsdl) when is_record(Wsdl,wsdl) -> true;
is_wsdl(_)                           -> false.

wsdl_operations(#wsdl{operations = Ops}) -> Ops.

wsdl_model(#wsdl{model = Model}) -> Model.

wsdl_op_service(#operation{service = Service}) -> Service.

wsdl_op_port(#operation{port = Port}) -> Port.

wsdl_op_operation(#operation{operation = Op}) -> Op.

wsdl_op_binding(#operation{binding = Binding}) -> Binding.

wsdl_op_address(#operation{address = Address}) -> Address.

wsdl_op_action(#operation{action = Action}) -> Action.


%%% --------------------------------------------------------------------
%%% For Quick deployment
%%% --------------------------------------------------------------------
call(WsdlURL, Operation, ListOfData) when is_list(WsdlURL) ->
    Wsdl = initModel(WsdlURL, ?DefaultPrefix),
    call(Wsdl, Operation, ListOfData);
call(Wsdl, Operation, ListOfData) when is_record(Wsdl, wsdl) ->
    case get_operation(Wsdl#wsdl.operations, Operation) of
        {ok, Op} ->
            Msg = mk_msg(?DefaultPrefix, Operation, ListOfData),
            call(Wsdl, Operation, Op#operation.port,
                 Op#operation.service, [], Msg);
        Else ->
            Else
    end.

%%% --------------------------------------------------------------------
%%% With additional specified prefix
%%% --------------------------------------------------------------------
call(WsdlURL, Operation, ListOfData, prefix, Prefix) when is_list(WsdlURL) ->
    Wsdl = initModel(WsdlURL, Prefix),
    call(Wsdl, Operation, ListOfData, prefix, Prefix );
call(Wsdl, Operation, ListOfData, prefix, Prefix) when is_record(Wsdl, wsdl) ->
    case get_operation(Wsdl#wsdl.operations, Operation) of
        {ok, Op} ->
            Msg = mk_msg(Prefix, Operation, ListOfData),
            call(Wsdl, Operation, Op#operation.port,
                 Op#operation.service, [], Msg);
        Else ->
            Else
    end.


%%% --------------------------------------------------------------------
%%% Takes the actual records for the Header and Body message.
%%% --------------------------------------------------------------------
call(WsdlURL, Operation, Header, Msg) when is_list(WsdlURL) ->
    Wsdl = initModel(WsdlURL, ?DefaultPrefix),
    call(Wsdl, Operation, Header, Msg);
call(Wsdl, Operation, Header, Msg) when is_record(Wsdl, wsdl) ->
    case get_operation(Wsdl#wsdl.operations, Operation) of
        {ok, Op} ->
            call(Wsdl, Operation, Op#operation.port, Op#operation.service,
                 Header, Msg);
        Else ->
            Else
    end.


mk_msg(Prefix, Operation, ListOfData) ->
    list_to_tuple([list_to_atom(Prefix++":"++Operation), % record name
                   []                                    % anyAttribs
                   | ListOfData]).                       % rest of record data

get_operation([#operation{operation = X} = Op|_], X) ->
    {ok, Op};
get_operation([_|T], Op)                             ->
    get_operation(T, Op);
get_operation([], _Op)                               ->
    {error, "operation not found"}.


%%% --------------------------------------------------------------------
%%% Make a SOAP request (no attachments)
%%% --------------------------------------------------------------------
call(Wsdl, Operation, Port, Service, Headers, Message) ->
    call_attach(Wsdl, Operation, Port, Service, Headers, Message, []).


%%% --------------------------------------------------------------------
%%% For Quick deployment (with attachments)
%%% --------------------------------------------------------------------
call_attach(WsdlURL, Operation, ListOfData, Attachments)
  when is_list(WsdlURL) ->
    Wsdl = initModel(WsdlURL, ?DefaultPrefix),
    call_attach(Wsdl, Operation, ListOfData, Attachments);
call_attach(Wsdl, Operation, ListOfData, Attachments)
  when is_record(Wsdl, wsdl) ->
    case get_operation(Wsdl#wsdl.operations, Operation) of
        {ok, Op} ->
            Msg = mk_msg(?DefaultPrefix, Operation, ListOfData),
            call_attach(Wsdl, Operation, Op#operation.port,
                        Op#operation.service, [], Msg, Attachments);
        Else ->
            Else
    end.

%%% --------------------------------------------------------------------
%%% Takes the actual records for the Header and Body message
%%% (with attachments)
%%% --------------------------------------------------------------------
call_attach(WsdlURL, Operation, Header, Msg, Attachments)
  when is_list(WsdlURL) ->
    Wsdl = initModel(WsdlURL, ?DefaultPrefix),
    call_attach(Wsdl, Operation, Header, Msg, Attachments);
call_attach(Wsdl, Operation, Header, Msg, Attachments)
  when is_record(Wsdl, wsdl) ->
    case get_operation(Wsdl#wsdl.operations, Operation) of
        {ok, Op} ->
            call_attach(Wsdl, Operation, Op#operation.port,
                        Op#operation.service,
                        Header, Msg, Attachments);
        Else ->
            Else
    end.


%%% --------------------------------------------------------------------
%%% Make a SOAP request (with attachments)
%%% --------------------------------------------------------------------
call_attach(#wsdl{operations = Operations, model = Model},
            Operation, Port, Service, Headers, Message, Attachments) ->
    %% find the operation
    case findOperation(Operation, Port, Service, Operations) of
        #operation{address = URL, action = SoapAction} ->
            %% Add the Soap envelope
            Envelope = mk_envelope(Message, Headers),
            %% Encode the message
            case erlsom:write(Envelope, Model) of
                {ok, XmlMessage} ->

                    {ContentType, Request} =
                        make_request_body(XmlMessage, Attachments),
                    HttpHeaders = [],
                    HttpClientOptions = [],
                    ?dbg("+++ Request = ~p~n", [Request]),
                    HttpRes = http_request(URL, SoapAction, Request,
                                           HttpClientOptions, HttpHeaders,
                                           ContentType),
                    ?dbg("+++ HttpRes = ~p~n", [HttpRes]),
                    case HttpRes of
                        {ok, _Code, _ReturnHeaders, Body} ->
                            parseMessage(Body, Model);
                        Error ->
                            %% in case of HTTP error: return
                            %% {error, description}
                            Error
                    end;
                {error, EncodingError} ->
                    {error, {encoding_error, EncodingError}}
            end;
        false ->
            {error, {unknown_operation, Operation}}
    end.

%%%
%%% returns {ok, Header, Body} | {error, Error}
%%%
parseMessage(Message, #wsdl{model = Model}) ->
    parseMessage(Message, Model);
%%
parseMessage(Message, Model) ->
    case erlsom:scan(Message, Model) of
        {ok, #'soap:Envelope'{'Body' = #'soap:Body'{choice = Body},
                              'Header' = undefined}, _} ->
            {ok, undefined, Body};
        {ok, #'soap:Envelope'{'Body' = #'soap:Body'{choice = Body},
                              'Header' = #'soap:Header'{choice = Header}}, _} ->
            {ok, Header, Body};
        {error, ErrorMessage} ->
            {error, {decoding, Message, ErrorMessage}}
    end.


findOperation(_Operation, _Port, _Service, []) ->
    false;
findOperation(Operation, Port, Service,
              [Op = #operation{operation = Operation,
                               port = Port, service = Service} | _]) ->
    Op;
findOperation(Operation, Port, Service, [#operation{} | Tail]) ->
    findOperation(Operation, Port, Service, Tail).


mk_envelope(M, H) when is_tuple(M) -> mk_envelope([M], H);
mk_envelope(M, H) when is_tuple(H) -> mk_envelope(M, [H]);
%%
mk_envelope(Messages, []) when is_list(Messages) ->
    #'soap:Envelope'{'Body' =  #'soap:Body'{choice = Messages}};
mk_envelope(Messages, Headers) when is_list(Messages),is_list(Headers) ->
    #'soap:Envelope'{'Body'   =  #'soap:Body'{choice   = Messages},
                     'Header' =  #'soap:Header'{choice = Headers}}.

%%% --------------------------------------------------------------------
%%% Parse a WSDL file and return a 'Model'
%%% --------------------------------------------------------------------
initModel(WsdlFile) ->
    initModel(WsdlFile, ?DefaultPrefix).

%% PrefixOrOptions can be a property list that contains the options
%% for Erlsom, or a String. If it is a string, this is used as the
%% Erlsom 'prefix' option (and the other options are left unspecified).
initModel(WsdlFile, PrefixOrOptions) ->
    Options = case is_string(PrefixOrOptions) of
        no ->
          %% It is an option list
          %% Add the default prefix at the end - it will only be used
          %% if no other prefix is specified
          PrefixOrOptions ++ [{prefix, ?DefaultPrefix}];
        _ ->
          %% just the prefix
          [{prefix, PrefixOrOptions}]
    end,
    PrivDir = priv_dir(),
    initModel2(WsdlFile, Options, PrivDir, undefined, undefined).

initModelFile(ConfigFile) ->
    {ok, ConfigSchema} = erlsom:compile_xsd(config_file_xsd()),
    %% read (parse) the config file
    {ok, Config, _} = erlsom:scan_file(ConfigFile, ConfigSchema),
    #yaws_soap_config{xsd_path = XsdPath,
                      wsdl_file = Wsdl,
                      add_files = AddFiles} = Config,
    #xsd_file{name = WsdlFile, prefix = Prefix, import_specs = Import} = Wsdl,
    initModel2(WsdlFile, [{prefix, Prefix}], XsdPath, Import, AddFiles).

priv_dir() ->
    yaws:get_priv_dir().

initModel2(WsdlFile, ErlsomOptions, Path, Import, AddFiles) ->
    WsdlName = filename:join([Path, "wsdl.xsd"]),
    IncludeWsdl = {"http://schemas.xmlsoap.org/wsdl/", "wsdl", WsdlName},
    {ok, WsdlModel} = erlsom:compile_xsd_file(
                        filename:join([Path, "soap.xsd"]),
                        [{prefix, "soap"},
                         {include_files, [IncludeWsdl]}]),
    %% add the xsd model (since xsd is also used in the wsdl)
    WsdlModel2 = erlsom:add_xsd_model(WsdlModel),
    Options = ErlsomOptions ++ makeOptions(Import),
    %% parse Wsdl
    {Model, Operations} = parseWsdls([WsdlFile], WsdlModel2,
                                     Options, {undefined, []}),
    %% TODO: add files as required
    %% now compile envelope.xsd, and add Model
    {ok, EnvelopeModel} = erlsom:compile_xsd_file(
                            filename:join([Path, "envelope.xsd"]),
                            [{prefix, "soap"}]),
    SoapModel = erlsom:add_model(EnvelopeModel, Model),
    SoapModel2 = addModels(AddFiles, SoapModel),
    #wsdl{operations = Operations, model = SoapModel2}.


%%% --------------------------------------------------------------------
%%% Parse a list of WSDLs and import (recursively)
%%% Returns {Model, Operations}
%%% --------------------------------------------------------------------
parseWsdls([], _WsdlModel, _Options, Acc) ->
    Acc;
parseWsdls([WsdlFile | Tail], WsdlModel, Options, {AccModel, AccOperations}) ->
    {ok, WsdlFileContent} = get_url_file(rmsp(WsdlFile)),
    {ok, ParsedWsdl, _} = erlsom:scan(WsdlFileContent, WsdlModel),
    %% get the xsd elements from this model, and hand it over to erlsom_compile.
    Xsds = getXsdsFromWsdl(ParsedWsdl),
    %% Now we need to build a list: [{Namespace, Xsd, Prefix}, ...] for
    %% all the Xsds in the WSDL.
    %% This list is used when a schema includes one of the other schemas.
    %% The AXIS java2wsdl tool generates wsdls that depend on this feature.
    ImportList = makeImportList(Xsds, []),
    Model2 = addSchemas(Xsds, AccModel, Options, ImportList),
    Ports = getPorts(ParsedWsdl),
    Operations = getOperations(ParsedWsdl, Ports),
    Imports = getImports(ParsedWsdl),
    Acc2 = {Model2, Operations ++ AccOperations},
    %% process imports (recursively, so that imports in the imported files are
    %% processed as well).
    %% For the moment, the namespace is ignored on operations etc.
    %% this makes it a bit easier to deal with imported wsdl's.
    Acc3 = parseWsdls(Imports, WsdlModel, Options, Acc2),
    parseWsdls(Tail, WsdlModel, Options, Acc3).

%%% --------------------------------------------------------------------
%%% build a list: [{Namespace, Xsd}, ...] for all the Xsds in the WSDL.
%%% This list is used when a schema inlcudes one of the other schemas.
%%% The AXIS java2wsdl tool generates wsdls that depend on this feature.
makeImportList([], Acc) ->
    Acc;
makeImportList([ Xsd | Tail], Acc) ->
    makeImportList(Tail, [{erlsom_lib:getTargetNamespaceFromXsd(Xsd),
                           undefined, Xsd} | Acc]).


%%% --------------------------------------------------------------------
%%% compile each of the schemas, and add it to the model.
%%% Returns Model
%%% (TODO: using the same prefix for all XSDS makes no sense)
%%% --------------------------------------------------------------------
addSchemas([], AccModel, _Options, _ImportList) ->
    AccModel;
addSchemas([Xsd| Tail], AccModel, Options, ImportList) ->
    Model2 = case Xsd of
                 undefined ->
                     AccModel;
                 _ ->
                     {ok, Model} =
                         erlsom_compile:compile_parsed_xsd(
                           Xsd,
                           [{include_files, ImportList} |Options]),
                     case AccModel of
                         undefined -> Model;
                         _ -> erlsom:add_model(AccModel, Model)
                     end
             end,
    addSchemas(Tail, Model2, Options, ImportList).

%%% --------------------------------------------------------------------
%%% Get a file from an URL spec.
%%% --------------------------------------------------------------------
get_url_file("http://"++_ = URL) ->
    case httpc:request(URL) of
        {ok,{{_HTTP,200,_OK}, _Headers, Body}} ->
            {ok, Body};
        {ok,{{_HTTP,RC,Emsg}, _Headers, _Body}} ->
            error_logger:error_msg("~p: http-request got: ~p~n",
                                   [?MODULE, {RC, Emsg}]),
            {error, "failed to retrieve: "++URL};
        {error, Reason} ->
            error_logger:error_msg("~p: http-request failed: ~p~n",
                                   [?MODULE, Reason]),
            {error, "failed to retrieve: "++URL}
    end;
get_url_file("file://"++Fname) ->
    {ok, Bin} = file:read_file(Fname),
    {ok, binary_to_list(Bin)};
%% added this, since this is what is used in many WSDLs (i.e.: just a filename).
get_url_file(Fname) ->
    {ok, Bin} = file:read_file(Fname),
    {ok, binary_to_list(Bin)}.


%%% --------------------------------------------------------------------
%%% Make a HTTP Request
%%% --------------------------------------------------------------------
http_request(URL, SoapAction, Request, Options, Headers, ContentType) ->
    case code:ensure_loaded(ibrowse) of
        {module, ibrowse} ->
            %% If ibrowse exist in the path then let's use it...
            ibrowse_request(URL, SoapAction, Request, Options,
                            Headers, ContentType);
        _ ->
            %% ...otherwise, let's use the OTP http client.
            inets_request(URL, SoapAction, Request, Options,
                          Headers, ContentType)
    end.

inets_request(URL, SoapAction, Request, Options, Headers, ContentType) ->
    NHeaders = [{"SOAPAction", SoapAction}|Headers],
    NewHeaders = case proplists:get_value("Host", NHeaders) of
                     undefined ->
                         [{"Host", "localhost:8800"}|NHeaders];
                     _ ->
                         NHeaders
                 end,
    NewOptions = [{cookies, enabled}|Options],
    httpc:set_options(NewOptions),
    case httpc:request(post,
                       {URL,NewHeaders,
                        ContentType,
                        Request},
                       [{timeout,?HTTP_REQ_TIMEOUT}],
                       [{sync, true}, {full_result, true},
                        {body_format, string}]) of
        {ok,{{_HTTP,200,_OK},ResponseHeaders,ResponseBody}} ->
            {ok, 200, ResponseHeaders, ResponseBody};
        {ok,{{_HTTP,500,_Descr},ResponseHeaders,ResponseBody}} ->
            {ok, 500, ResponseHeaders, ResponseBody};
        {ok,{{_HTTP,ErrorCode,_Descr},ResponseHeaders,ResponseBody}} ->
            {ok, ErrorCode, ResponseHeaders, ResponseBody};
        Other ->
            Other
    end.

ibrowse_request(URL, SoapAction, Request, Options, Headers, ContentType) ->
    case start_ibrowse() of
        ok ->
            NewHeaders = [{"Content-Type", ContentType},
                          {"SOAPAction", SoapAction} | Headers],
            case ibrowse:send_req(URL, NewHeaders, post, Request, Options) of
                {ok, Status, ResponseHeaders, ResponseBody} ->
                    {ok, list_to_integer(Status), ResponseHeaders,
                     ResponseBody};
                {error, Reason} ->
                    {error, Reason}
            end;
        error ->
            {error, "could not start ibrowse"}
    end.

start_ibrowse() ->
    case ibrowse:start() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        _ -> error
    end.


rmsp(Str) -> string:strip(Str, left).


make_request_body(Content, []) ->
    {"text/xml; charset=utf-8",
     "<?xml version=\"1.0\" encoding=\"utf-8\"?>"++ Content};
make_request_body(Content, AttachedFiles) ->
    {"application/dime",
     yaws_dime:encode("<?xml version=\"1.0\" encoding=\"utf-8\"?>" ++ Content,
                      AttachedFiles)}.

makeFault(FaultCode, FaultString) ->
    try
        "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">"
            "<SOAP-ENV:Body>"
            "<SOAP-ENV:Fault>"
            "<faultcode>SOAP-ENV:" ++ FaultCode ++ "</faultcode>" ++
            "<faultstring>" ++ FaultString ++ "</faultstring>" ++
            "</SOAP-ENV:Fault>"
            "</SOAP-ENV:Body>"
            "</SOAP-ENV:Envelope>"
    catch
        _:_ ->
            "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">"
                "<SOAP-ENV:Body>"
                "<SOAP-ENV:Fault>"
                "<faultcode>SOAP-ENV:Server</faultcode>"
                "<faultstring>Server error</faultstring>"
                "</SOAP-ENV:Fault>"
                "</SOAP-ENV:Body>"
                "</SOAP-ENV:Envelope>"
    end.

%% record http_header is not defined??
findHeader(Label, Headers) ->
    findHeader0(yaws:to_lower(Label), Headers).

findHeader0(_Label, []) ->
    undefined;
findHeader0(Label, [{_,_,Hdr,_,Val}|T]) ->
    case {Label, yaws:to_lower(Hdr)} of
        {X,X} -> Val;
        _     -> findHeader0(Label, T)
    end;
findHeader0(_Label, undefined) ->
    undefined.


makeOptions(undefined) ->
    [];
makeOptions(Import) ->
    lists:map(fun makeOption/1, Import).

%% -record(import_specs, {atts, namespace, prefix, location}).
makeOption(#import_specs{namespace = Ns, prefix = Pf, location = Lc}) ->
    {Ns, Pf, Lc}.


addModels(undefined, Model) ->
    Model;
addModels(Import, Model) ->
    lists:foldl(fun addModel/2, Model, Import).

%% -record(xsd_file, {atts, name, prefix, import_specs}).
addModel(undefined, Acc) ->
    Acc;
addModel(#xsd_file{name = XsdFile, prefix = Prefix, import_specs = Import},
         Acc) ->
    Options = makeOptions(Import),
    {ok, Model2} = erlsom:add_xsd_file(XsdFile, [{prefix, Prefix}|Options],Acc),
    Model2.

%% returns [#port{}]
%% -record(port, {service, port, binding, address}).
getPorts(ParsedWsdl) ->
    Services = getTopLevelElements(ParsedWsdl, 'wsdl:tService'),
    getPortsFromServices(Services, []).

getPortsFromServices([], Acc) ->
    Acc;
getPortsFromServices([Service|Tail], Acc) ->
    getPortsFromServices(Tail, getPortsFromService(Service) ++ Acc).

getPortsFromService(#'wsdl:tService'{name = Name, port = Ports}) ->
    getPortsInfo(Ports, Name, []).

getPortsInfo([], _Name, Acc) ->
    Acc;

getPortsInfo([#'wsdl:tPort'{name = Name,
                            binding = Binding,
                            choice =
                                [#'soap:tAddress'{location = URL}]} | Tail],
             ServiceName, Acc) ->
    getPortsInfo(Tail, ServiceName, [#port{service = ServiceName,
                                           port = Name,
                                           binding = Binding,
                                           address = URL}|Acc]);
%% non-soap bindings are ignored.
getPortsInfo([#'wsdl:tPort'{} | Tail], ServiceName, Acc) ->
    getPortsInfo(Tail, ServiceName, Acc).


getTopLevelElements(#'wsdl:tDefinitions'{choice = TLElements}, Type) ->
    getTopLevelElements(TLElements, Type, []).

getTopLevelElements([], _Type, Acc) ->
    Acc;
getTopLevelElements([#'wsdl:anyTopLevelOptionalElement'{choice = Tuple}| Tail],
                    Type, Acc) ->
    case element(1, Tuple) of
        Type -> getTopLevelElements(Tail, Type, [Tuple|Acc]);
        _ -> getTopLevelElements(Tail, Type, Acc)
    end.

getImports(Definitions) ->
    Imports = getTopLevelElements(Definitions, 'wsdl:tImport'),
    lists:map(fun(Import) -> Import#'wsdl:tImport'.location end, Imports).

%% returns [#operation{}]
getOperations(ParsedWsdl, Ports) ->
    Bindings = getTopLevelElements(ParsedWsdl, 'wsdl:tBinding'),
    getOperationsFromBindings(Bindings, Ports, []).

getOperationsFromBindings([], _Ports, Acc) ->
    Acc;
getOperationsFromBindings([Binding|Tail], Ports, Acc) ->
    getOperationsFromBindings(Tail, Ports,
                              getOperationsFromBinding(Binding, Ports) ++ Acc).

getOperationsFromBinding(#'wsdl:tBinding'{name = BindingName,
                                          type = BindingType,
                                          choice = _Choice,
                                          operation = Operations}, Ports) ->
    %% TODO: get soap info from Choice
    getOperationsFromOperations(Operations, BindingName, BindingType,
                                Operations, Ports, []).

getOperationsFromOperations([], _BindingName, _BindingType, _Operations,
                            _Ports, Acc) ->
    Acc;

getOperationsFromOperations([#'wsdl:tBindingOperation'{name = Name,
                                                       choice = Choice} | Tail],
                            BindingName, BindingType, Operations, Ports, Acc) ->
    %% get SOAP action from Choice,
    case Choice of
        [#'soap:tOperation'{soapAction = Action}] ->
            %% lookup Binding in Ports, and create a combined result
            Ports2 = searchPorts(BindingName, Ports),
            %% for each port, make an operation record
            CombinedPorts = combinePorts(Ports2, Name, BindingName, Action),
            getOperationsFromOperations(
              Tail, BindingName, BindingType,
              Operations, Ports, CombinedPorts ++ Acc);
        _ ->
            getOperationsFromOperations(Tail, BindingName, BindingType,
                                        Operations, Ports, Acc)
    end.

combinePorts(Ports, Name, BindingName, Action) ->
    combinePorts(Ports, Name, BindingName, Action, []).

combinePorts([], _Name, _BindingName, _Action, Acc) ->
    Acc;
combinePorts([#port{service = Service,
                    port = PortName,
                    address = Address} | Tail],
             Name, BindingName, Action, Acc) ->
    combinePorts(Tail, Name, BindingName, Action,
                 [#operation{service = Service,
                             port = PortName, operation = Name,
                             binding = BindingName,
                             address = Address, action = Action} | Acc]).

searchPorts(BindingName, Ports) ->
    searchPorts(BindingName, Ports, []).

searchPorts(_BindingName, [], Acc) ->
    Acc;
searchPorts(BindingName, [Port | Tail], Acc) ->
    PortBinding = erlsom_lib:localName(Port#port.binding),
    case PortBinding of
        BindingName ->
            searchPorts(BindingName, Tail, [Port | Acc]);
        _ ->
            searchPorts(BindingName, Tail, Acc)
    end.

%% copied from yaws/json.erl
is_string([]) -> yes;
is_string(List) -> is_string(List, non_unicode).

is_string([C|Rest], non_unicode)
  when C >= 0, C =< 255 -> is_string(Rest, non_unicode);
is_string([C|Rest], _) when C =< 65000 -> is_string(Rest, unicode);
is_string([], non_unicode) -> yes;
is_string([], unicode) -> unicode;
is_string(_, _) -> no.

getXsdsFromWsdl(Definitions) ->
    case getTopLevelElements(Definitions, 'wsdl:tTypes') of
        [#'wsdl:tTypes'{choice = Xsds}] -> Xsds;
        [] -> []
    end.

config_file_xsd() ->
    "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">"
        "  <xs:element name=\"yaws_soap_config\">"
        "     <xs:complexType>"
        "       <xs:sequence>"
        "         <xs:element name=\"xsd_path\" type=\"xs:string\" minOccurs=\"0\"/>"
        "         <xs:element name=\"user_module\" type=\"xs:string\"/>"
        "         <xs:element name=\"wsdl_file\" type=\"xsd_file\"/>"
        "         <xs:element name=\"add_file\" type=\"xsd_file\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>"
        "       </xs:sequence>"
        "     </xs:complexType>"
        "  </xs:element>"
        "  <xs:complexType name=\"xsd_file\">"
        "    <xs:sequence>"
        "      <xs:element name=\"import_specs\" type=\"import_specs\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>"
        "    </xs:sequence>"
        "    <xs:attribute name=\"name\" type=\"string\" use=\"required\"/>"
        "    <xs:attribute name=\"prefix\" type=\"string\"/>"
        "  </xs:complexType>"
        "  <xs:complexType name=\"import_specs\">"
        "    <xs:attribute name=\"namespace\" type=\"string\" use=\"required\"/>"
        "    <xs:attribute name=\"prefix\" type=\"string\"/>"
        "    <xs:attribute name=\"location\" type=\"string\"/>"
        "  </xs:complexType>"
        "</xs:schema>".


