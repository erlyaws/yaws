%% Copyright (C) 2003 Joakim Grebenö <jocke@gleipnir.com>.
%% All rights reserved.
%%
%% Copyright (C) 2006 Gaspar Chilingarov <nm@web.am>
%%                      Gurgen Tumanyan <barbarian@armkb.com>
%% All rights reserved.
%%
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met: 
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer. 
%% 2. Redistributions in binary form must reproduce the above
%%    copyright notice, this list of conditions and the following
%%    disclaimer in the documentation and/or other materials provided
%%    with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
%% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% NOTE: This module was originally called yaws_jsonrpc.
%% It was hacked to transparently supports haXe remoting as well,
%% hence its name was changed to the more generic 'yaws_rpc'.
%% 
%% modified by Yariv Sadan (yarivvv@gmail.com)

-module(yaws_rpc).
-author("Gaspar Chilingarov <nm@web.am>, Gurgen Tumanyan <barbarian@armkb.com>").                    
-modified_by("Yariv Sadan <yarivvv@gmail.com>").

-export([handler/2]).
-export([handler_session/2, handler_session/3]).

%-define(debug, 1).
%-include("../../yaws/src/yaws_debug.hrl").

-include("../include/yaws_api.hrl").

%%% ###################################################################### 
%%% public interface
%%%

%%%
%%% use rpc handler which can automagically start sessions if we need
%%%
handler_session(Args, Handler) ->
    handler_session(Args, Handler, 'SID').

%%%
%%% allow overriding session Cookie name
%%%
handler_session(Args, Handler, SID_NAME) when is_atom(SID_NAME) ->
    handler_session(Args, Handler, atom_to_list(SID_NAME));

handler_session(Args, Handler, SID_NAME) ->
    handler(Args, Handler, {session, SID_NAME}).    % go to generic handler

%%%
%%% xmlrpc:handler compatible call
%%% no session support will be available
handler(Args, Handler) ->
    handler(Args, Handler, simple).


%%% ###################################################################### 
%%% private functions
%%%

%%% we should be called from yaws page or module
handler(Args, Handler, Type) when is_record(Args, arg) -> % {{{
    case parse_request(Args) of
    ok ->
        handle_payload(Args, Handler, Type);
    {status, StatusCode} ->        % cannot parse request
        send(Args, StatusCode)
    end. % }}}

-define(ERROR_LOG(Reason),
    error_logger:error_report({?MODULE, ?LINE, Reason})).

-define(LOG(Reason), ?ERROR_LOG(Reason)).

%%%
%%% check that request come in reasonable protocol version and reasonable method
%%%
parse_request(Args) -> % {{{
    Req = Args#arg.req,
    case {Req#http_request.method, Req#http_request.version} of
    {'POST', {1,0}} ->
%        ?Debug("HTTP Version 1.0~n", []),
        ok;
    {'POST', {1,1}} ->
%        ?Debug("HTTP Version 1.1~n", []),
        ok;
    {'POST', _HTTPVersion} -> {status, 505};
    {_Method, {1,1}} -> {status, 501};
    _ -> {status, 400}
     end.  % }}}

handle_payload(Args, Handler, Type) -> % {{{
    RpcType = recognize_rpc_type(Args),
    % haXe parameters are URL encoded
    {Payload,DecodedStr} = 
	case RpcType of
	    T when T==haxe; T==json ->
		PL = binary_to_list(Args#arg.clidata),
		%%    ?Debug("rpc plaintext call ~p ~n", [PL]),
		{PL, yaws_api:url_decode(PL)};
	    soap_dime ->
		[{_,_,_,Req}|As]=yaws_dime:decode(Args#arg.clidata),
		{Args#arg.clidata,
		 {binary_to_list(Req),As}};
	    _ ->
		PL = binary_to_list(Args#arg.clidata),
		{PL, PL}
		%%    ?Debug("rpc plaintext call ~p ~n", [PL])
	end,
    case decode_handler_payload(RpcType, DecodedStr) of
        {ok, DecodedPayload, ID} ->
            % ?Debug("client2erl decoded call ~p ~n", [DecodedPayload]),
            eval_payload(Args, Handler, DecodedPayload, Type, ID, RpcType);
        {error, Reason} ->
            ?ERROR_LOG({html, client2erl, Payload, Reason}),
            send(Args, 400, RpcType)
    end. % }}}

%%% Identify the RPC type. We first try recognize haXe by the
%%% "X-Haxe-Remoting" HTTP header, then the "SOAPAction" header,
%%% and if those are absent we assume the request is JSON.
recognize_rpc_type(Args) ->
    case (Args#arg.headers)#headers.content_type of
	"application/dime" -> soap_dime;
	_ ->
	    OtherHeaders = ((Args#arg.headers)#headers.other),
	    recognize_rpc_hdr([{X,Y,yaws:to_lower(Z),Q,W} || {X,Y,Z,Q,W} <- OtherHeaders])
    end.

recognize_rpc_hdr([{_,_,"x-haxe-remoting",_,_}|_]) -> haxe;
recognize_rpc_hdr([{_,_,"soapaction",_,_}|_])      -> soap;
recognize_rpc_hdr([_|T])                           -> recognize_rpc_hdr(T);
recognize_rpc_hdr([])                              -> json.

    
%%%
%%% call handler/3 and provide session support
eval_payload(Args, {M, F}, Payload, {session, CookieName}, ID, RpcType) -> % {{{
    {SessionValue, Cookie} =
        case yaws_api:find_cookie_val(CookieName, (Args#arg.headers)#headers.cookie) of
            [] ->      % have no session started, just call handler
                {undefined, undefined};
            Cookie2 -> % get old session data
                case yaws_api:cookieval_to_opaque(Cookie2) of
                    {ok, OP} ->
                        {OP, Cookie2};
                    {error, _ErrMsg} -> % cannot get corresponding session
                        {undefined, undefined}
                end
        end,
    CbackFun = callback_fun(M, F, Args, Payload, SessionValue, RpcType),
    case catch CbackFun() of
        {'EXIT', Reason} ->
            ?ERROR_LOG({M, F, {'EXIT', Reason}}),
            send(Args, 500, RpcType);
        {error, Reason} ->
            ?ERROR_LOG({M, F, Reason}),
            send(Args, 500, RpcType);
        {error, Reason, Rc} ->
            ?ERROR_LOG({M, F, Reason}),
            send(Args, Rc, Reason, [], RpcType);
        {false, ResponsePayload} ->
            % do not have updates in session data
            encode_send(Args, 200, ResponsePayload, [], ID, RpcType);
        {false, ResponsePayload, RespCode} ->
            % do not have updates in session data
            encode_send(Args, RespCode, ResponsePayload, [], ID, RpcType);
        false ->   % soap notify
            false; 
        {true, _NewTimeout, NewSessionValue, ResponsePayload} -> % be compatible with xmlrpc module
            CO = handle_cookie(Cookie, CookieName, SessionValue, NewSessionValue, M, F),
            encode_send(Args, 200, ResponsePayload, CO, ID, RpcType);
        {true, _NewTimeout, NewSessionValue, ResponsePayload, RespCode} -> % be compatible with xmlrpc module
            CO = handle_cookie(Cookie, CookieName, SessionValue, NewSessionValue, M, F),
            encode_send(Args, RespCode, ResponsePayload, CO, ID, RpcType)
    end; % }}}

%%%
%%% call handler/2 without session support
%%%
eval_payload(Args, {M, F}, Payload, simple, ID, RpcType) -> % {{{
    case catch M:F(Args#arg.state, Payload) of
        {'EXIT', Reason} ->
            ?ERROR_LOG({M, F, {'EXIT', Reason}}),
            send(Args, 500);
        {error, Reason} ->
            ?ERROR_LOG({M, F, Reason}),
            send(Args, 500);
        {false, ResponsePayload} ->
            encode_send(Args, 200, ResponsePayload, [], ID, RpcType);
        false -> % Soap notify !?
            false;
        {true, _NewTimeout, _NewState, ResponsePayload} ->
            encode_send(Args, 200, ResponsePayload, [], ID, RpcType)
    end. % }}}  

handle_cookie(Cookie, CookieName, SessionValue, NewSessionValue, M, F) ->
    case NewSessionValue of
        undefined when Cookie == undefined -> []; % nothing to do
        undefined -> % rpc handler requested session delete
            yaws_api:delete_cookie_session(Cookie), []; % XXX: may be return set-cookie with empty val?
        _ ->  % any other value will stored in session
            case SessionValue of
                undefined -> % got session data and should start new session now
                    Cookie1 = yaws_api:new_cookie_session(NewSessionValue),
                    case get_expire(M, F) of
                        false ->
                            yaws_api:setcookie(CookieName, Cookie1, "/"); % return set_cookie header
                        Expire ->
                            yaws_api:setcookie(CookieName, Cookie1, "/", Expire) % return set_cookie header
                    end;
                _ -> 
                    yaws_api:replace_cookie_session(Cookie, NewSessionValue),
                    [] % nothing to add to yaws data
            end
    end.

%%% Make it possible for callback module to set Cookie Expire string!
get_expire(M, F) -> 
    case catch M:F(cookie_expire) of
        Expire when list(Expire) -> Expire;
        _                        -> false
    end.

callback_fun(M, F, Args, Payload, SessionValue, RpcType) when RpcType==soap; RpcType==soap_dime ->
    fun() -> yaws_soap_srv:handler(Args, {M,F}, Payload, SessionValue) end;
callback_fun(M, F, Args, Payload, SessionValue, _RpcType) ->
    fun() -> M:F(Args#arg.state, Payload, SessionValue) end.


%%% XXX compatibility with XMLRPC handlers
%%% XXX - potential bug here?
encode_send(Args, StatusCode, [Payload], AddOn, ID, RpcType) ->  % {{{
    encode_send(Args, StatusCode, Payload, AddOn, ID, RpcType); % }}}

encode_send(Args, StatusCode, Payload, AddOn, ID, RpcType) -> % {{{
%    ?Debug("rpc response ~p ~n", [Payload]),
    case encode_handler_payload(Payload, ID, RpcType) of
        {ok, EncodedPayload, NewRpcType} ->
%        ?Debug("rpc encoded response ~p ~n", [EncodedPayload]),
            send(Args, StatusCode, EncodedPayload, AddOn, NewRpcType);
        {ok, EncodedPayload} ->
%        ?Debug("rpc encoded response ~p ~n", [EncodedPayload]),
            send(Args, StatusCode, EncodedPayload, AddOn, RpcType);
        {error, Reason} ->
            ?ERROR_LOG({rpc_encode, payload, Payload, Reason}),
            send(Args, 500, RpcType)
    end. % }}}

send(Args, StatusCode) -> send(Args, StatusCode, json).

send(Args, StatusCode, RpcType) -> send(Args, StatusCode, "", [], RpcType). %  {{{

send(Args, StatusCode, Payload, AddOnData, RpcType) when not is_list(AddOnData) ->
    send(Args, StatusCode, Payload, [AddOnData], RpcType);

send(_Args, StatusCode, Payload, AddOnData, RpcType) ->
    A = [
    {status, StatusCode}, 
    content_hdr(RpcType, Payload),
    {header, {content_length, lists:flatlength(Payload) }}
    ] ++ AddOnData,
    A
     . % }}}

content_hdr(json, Payload) -> {content, "application/json", Payload};
content_hdr(_, Payload)    -> {content, "text/xml", Payload}.  % FIXME  would like to add charset info here !!

encode_handler_payload({Xml,[]}, _ID, soap_dime) ->   % {{{
    {ok, Xml, soap};

encode_handler_payload({Xml,As}, _ID, soap_dime) ->   % {{{
	EncodedPayload = yaws_dime:encode(Xml, As),
    {ok, EncodedPayload};

encode_handler_payload(Xml, _ID, soap_dime) ->   % {{{
    {ok, Xml, soap};

encode_handler_payload({Xml,[]}, _ID, soap) ->   % {{{
    {ok, Xml};

encode_handler_payload({Xml,As}, _ID, soap) ->   % {{{
	EncodedPayload = yaws_dime:encode(Xml, As),
    {ok, EncodedPayload, soap_dime};

encode_handler_payload(Xml, _ID, soap) ->   % {{{
    {ok, Xml};

encode_handler_payload({response, [ErlStruct]}, ID, RpcType) ->   % {{{
    encode_handler_payload({response, ErlStruct}, ID, RpcType);
    
encode_handler_payload({response, ErlStruct}, ID, RpcType) ->
    StructStr =
        case RpcType of
            json -> json:encode({struct, [ {result, ErlStruct}, {id, ID}]});
            haxe -> [$h, $x, $r | haxe:encode(ErlStruct)]
        end,
    {ok, StructStr}.  % }}}

decode_handler_payload(json, JSonStr) -> %{{{
    try 
        {ok, Obj} = json:decode_string(JSonStr),
        Method = list_to_atom(jsonrpc:s(Obj, method)),
        {array, Args} = jsonrpc:s(Obj, params),
        ID = jsonrpc:s(Obj, id),
        {ok, {call, Method, Args}, ID}
    catch 
        error:Err ->
                ?ERROR_LOG({ json_decode , JSonStr , Err }),
           {error, Err}
    end; %}}}

decode_handler_payload(haxe, [$_, $_, $x, $= | HaxeStr]) ->
    try
        {done, {ok, {array, [MethodName | _]}}, Cont} = haxe:decode(HaxeStr),
        {done, {ok, Args}, _Cont2} = haxe:decode_next(Cont),
        
        %% ID is undefined because haXe remoting doesn't automagically handle
        %% sessions.
        {ok, {call, list_to_atom(MethodName), Args}, undefined}
    catch
        error:Err -> {error, Err}
    end;
decode_handler_payload(haxe, _HaxeStr) ->
    {error, missing_haxe_prefix};

decode_handler_payload(soap_dime, Payload) ->
    {ok, Payload, undefined};
decode_handler_payload(soap, Payload) ->
    {ok, Payload, undefined}.


% vim: tabstop=4 ft=erlang
