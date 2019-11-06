%% -*- coding: latin-1 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WARNING DEPRECATED WARNING DEPRECATED WARNING DEPRECATED WARNING DEPRECATED
%%% WARNING DEPRECATED WARNING DEPRECATED WARNING DEPRECATED WARNING DEPRECATED
%%%
%%% Use module yaws_rpc.erl instead
%%%
%%% This module is deprecated.
%%%
%%% Do not report problems with this module, as they will not be fixed. You
%%% should instead convert your code to use the yaws_rpc module.
%%%
%%% WARNING DEPRECATED WARNING DEPRECATED WARNING DEPRECATED WARNING DEPRECATED
%%% WARNING DEPRECATED WARNING DEPRECATED WARNING DEPRECATED WARNING DEPRECATED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

-module(yaws_jsonrpc).
-author("Gaspar Chilingarov <nm@web.am>, Gurgen Tumanyan <barbarian@armkb.com>").
-export([handler/2]).
-export([handler_session/2, handler_session/3]).

%%-define(debug, 1).
-include("yaws_debug.hrl").

-include("../include/yaws_api.hrl").

%%% ######################################################################
%%% public interface
%%%

%%%
%%% use jsonrpc handler which can automagically start sessions if we need
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
handler(Args, Handler, Type) when is_record(Args, arg) ->
    case parse_request(Args) of
        ok ->
            handle_payload(Args, Handler, Type);
        {status, StatusCode} ->        % cannot parse request
            send(Args, StatusCode)
    end.

-define(ERROR_LOG(Reason),
        error_logger:error_report({?MODULE, ?LINE, Reason})).

%%%
%%% check that request come in reasonable protocol version and reasonable method
%%%
parse_request(Args) ->
    case {(Args#arg.req)#http_request.method,
          (Args#arg.req)#http_request.version} of
        {'POST', {1,0}} ->
            ?Debug("HTTP Version 1.0~n", []),
            ok;
        {'POST', {1,1}} ->
            ?Debug("HTTP Version 1.1~n", []),
            ok;
        {'POST', _HTTPVersion} -> {status, 505};
        {_Method, {1,1}} -> {status, 501};
        _ -> {status, 400}
    end.

handle_payload(Args, Handler, Type) ->
    Payload = binary_to_list(Args#arg.clidata),
    ?Debug("jsonrpc plaintext call ~p ~n", [Payload]),
    case decode_handler_payload(Payload) of
        {ok, DecodedPayload, ID} ->
            ?Debug("json2erl decoded call ~p ~n", [DecodedPayload]),
            eval_payload(Args, Handler, DecodedPayload, Type, ID);
        {error, Reason} ->
            ?ERROR_LOG({html, json2erl, Payload, Reason}),
            send(Args, 400)
    end.

%%%
%%% call handler/3 and provide session support
eval_payload(Args, {M, F}, Payload, {session, CookieName},ID) ->
    {SessionValue, Cookie} =
        case yaws_api:find_cookie_val(CookieName,
                                      (Args#arg.headers)#headers.cookie) of
            [] ->      % have no session started, just call handler
                {undefined, undefined};
            Cookie2 -> % get old session data
                case yaws_api:cookieval_to_opaque(Cookie2) of
                    {ok, OP} ->
                        yaws_api:cookieval_to_opaque(Cookie2),
                        {OP, Cookie2};
                    {error, _ErrMsg} -> % cannot get corresponding session
                        {undefined, undefined}
                end
        end,

    case catch M:F(Args#arg.state, Payload, SessionValue) of
        {'EXIT', Reason} ->
            ?ERROR_LOG({M, F, {'EXIT', Reason}}),
            send(Args, 500);
        {error, Reason} ->
            ?ERROR_LOG({M, F, Reason}),
            send(Args, 500);
        {false, ResponsePayload} ->
            %% do not have updates in session data
            encode_send(Args, 200, ResponsePayload, [], ID);
        {true, _NewTimeout, NewSessionValue, ResponsePayload} ->
            %% be compatible with xmlrpc module
            CO = case NewSessionValue of
                     undefined when Cookie == undefined -> []; % nothing to do
                     undefined -> % rpc handler requested session delete
                         yaws_api:delete_cookie_session(Cookie), [];
                     %% XXX: may be return set-cookie with empty val?
                     _ ->
                         %% any other value will stored in session
                         case SessionValue of
                             undefined ->
                                 %% got session data and should start
                                 %% new session now
                                 Cookie1 = yaws_api:new_cookie_session(
                                             NewSessionValue),
                                 yaws_api:setcookie(CookieName, Cookie1, "/");
                             _ ->
                                 yaws_api:replace_cookie_session(
                                   Cookie, NewSessionValue),
                                 [] % nothing to add to yaws data
                         end
                 end,
            encode_send(Args, 200, ResponsePayload, CO, ID)
    end;

%%%
%%% call handler/2 without session support
%%%
eval_payload(Args, {M, F}, Payload, simple, ID) ->
    case catch M:F(Args#arg.state, Payload) of
        {'EXIT', Reason} ->
            ?ERROR_LOG({M, F, {'EXIT', Reason}}),
            send(Args, 500);
        {error, Reason} ->
            ?ERROR_LOG({M, F, Reason}),
            send(Args, 500);
        {false, ResponsePayload} ->
            encode_send(Args, 200, ResponsePayload, [],ID);
        {true, _NewTimeout, _NewState, ResponsePayload} ->
            encode_send(Args, 200, ResponsePayload, [],ID)
    end.


%%% XXX compatibility with XMLRPC handlers
%%% XXX - potential bug here?
encode_send(Args, StatusCode, [Payload], AddOn, ID) ->
    encode_send(Args, StatusCode, Payload, AddOn, ID);

encode_send(Args, StatusCode, Payload, AddOn, ID) ->
    {ok, EncodedPayload} = encode_handler_payload(Payload, ID),
    send(Args, StatusCode, EncodedPayload, AddOn).

send(Args, StatusCode) -> send(Args, StatusCode, "", []).

send(Args, StatusCode, Payload, AddOnData) when not is_list(AddOnData) ->
    send(Args, StatusCode, Payload, [AddOnData]);

send(_Args, StatusCode, Payload, AddOnData) ->
    A = [
    {status, StatusCode},
    {content, "text/xml", Payload},
    {header, {content_length, lists:flatlength(Payload) }}
    ] ++ AddOnData,
    A.


encode_handler_payload({response, [ErlStruct]},ID) ->
    encode_handler_payload({response, ErlStruct}, ID);

encode_handler_payload({response, ErlStruct},ID) ->
    StructStr = json2:encode({struct, [{result, ErlStruct}, {id, ID}]}),
    {ok, StructStr}.

decode_handler_payload(JSonStr) ->
    try
        {ok, JSON} = json2:decode_string(JSonStr),
        Method = list_to_atom(jsonrpc:s(JSON, method)),
        {array, Args} = jsonrpc:s(JSON, params),
        ID = jsonrpc:s(JSON, id),

        {ok, {call, Method, Args}, ID}
    catch
        error:Err -> {error, Err}
    end.


