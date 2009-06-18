%%%-------------------------------------------------------------------
%%% File    : authmod_gssapi.erl
%%% Author  : Mikael Magnusson <mikael@skinner.hem.za.org>
%%% Description : Negotiate authentication module supporting GSSAPI
%%%           and SPNEGO
%%%
%%% Created : 17 May 2007 by Mikael Magnusson <mikael@skinner.hem.za.org>
%%%-------------------------------------------------------------------
%%%
%%% Copyright (c) 2007 Mikael Magnusson
%%% All rights reserved. 
%%%
%%% Redistribution and use in source and binary forms, with or without 
%%% modification, are permitted provided that the following conditions 
%%% are met: 
%%%
%%% 1. Redistributions of source code must retain the above copyright 
%%%    notice, this list of conditions and the following disclaimer. 
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright 
%%%    notice, this list of conditions and the following disclaimer in the 
%%%    documentation and/or other materials provided with the distribution. 
%%%
%%% 3. Neither the name of the copyright owner nor the names of its
%%%    contributors may be used to endorse or promote products derived from
%%%    this software without specific prior written permission. 
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE INSTITUTE AND CONTRIBUTORS ``AS IS'' AND 
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%%% ARE DISCLAIMED.  IN NO EVENT SHALL THE INSTITUTE OR CONTRIBUTORS BE LIABLE 
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS 
%%% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY 
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
%%% SUCH DAMAGE. 




%%  this code adds support for SPNEGO and GSSAPI negotiation to yaws. 
%%  It's compatible with both Linux/Unix and Windows. 
%%  Supporting both Kerberos for windows (kfw) and SSPI on Windows.

%%  It's implemented as an authmod called authmod_gssapi. 
%%  Adding it to start_mod in <server> and authmod in an <auth> tag 
%%  activates the module. It expects a Kerberos keytab in <opaque>. 
%%  The keytab should contain key(s) for "HTTP/<fqdn>@<REALM>",
%%   where <fqdn> is the fully qualified domain name of the host and <REALM> 
%%  the kerberos realm.

%%  For example:

%%  <server fqdn>    
%%     port = 80
%%     listen = 0.0.0.0
%%     docroot = /usr/share/yaws
%%     start_mod = authmod_gssapi
%%     <auth>        authmod = authmod_gssapi
%%         dir = /
%%     </auth>    
%%     <opaque>        
%%         keytab = /etc/yaws/http.keytab
%%     </opaque>
%%  </server>

%%  The authmod_gssapi module depends on egssapi 
%%  from: http://www.hem.za.org/egssapi/






-module(authmod_gssapi).

-export([
         start/1,
         stop/0,
         auth/2,
	 get_header/0,
	 out/1
        ]).

-include("yaws.hrl").
-include("yaws_api.hrl").

-define(SERVER, ?MODULE).
-define(SUPERVISOR, yaws_sup).

%%-define(ENABLE_DEBUG, yes).

-ifdef(ENABLE_DEBUG).
-define(INFO, io:format).
-define(DEBUG, io:format).
-else.
-define(INFO, ignore).
-define(DEBUG, ignore).
-endif.

-define(WARNING, io:format).
-define(ERROR, io:format).


start(Sconf) when is_record(Sconf, sconf) ->
    Opaque = Sconf#sconf.opaque,
    start_opaque(Opaque);
    

start(Keytab) when is_list(Keytab) ->
    ChildSpec =
        {?SERVER,
         {egssapi, start_link, [{local, ?SERVER}, Keytab]},
         permanent,
         1000,
         worker,
         [egssapi, spnego]},

    supervisor:start_child(?SUPERVISOR, ChildSpec).

stop() ->
    egssapi:stop(?SERVER),
    supervisor:terminate_child(?SUPERVISOR, ?SERVER),
    supervisor:delete_child(?SUPERVISOR, ?SERVER).

out(Arg) ->
    yaws_outmod:out(Arg).

auth(Arg, Auth) when is_record(Arg, arg),
                      is_record(Auth, auth) ->

    H = Arg#arg.headers,

    ?INFO("~p~n", [?MODULE]),

    case H#headers.authorization of
        {_, _, "Negotiate " ++ Data} ->
            ?INFO("Negotiate~n", []),
            Bin = base64:decode(Data),

             case catch spnego:accept_sec_context(?SERVER, Bin) of
                {'EXIT', Reason} ->
                    ?ERROR("spnego failed EXIT:~p~n", [Reason]),
                     throw(Reason);
                {error, Reason} ->
                    ?ERROR("spnego failed error:~p~n", [Reason]),
                    throw(Reason);
                {ok, {Context, User, Ccname, Resp}} ->
                    ?DEBUG("spnego user ok ~p~n", [User]),
                    spnego:delete_sec_context(Context),
                    {true, {User, Ccname, base64:encode(Resp)}};
                E ->
                    ?ERROR("spnego error ~p~n", [E]),
                    throw(error)
            end;
        _ ->
            ?INFO("Request auth~n"),
            {appmod, ?MODULE}
    end.

%% The header that is set when authentication fails
get_header() -> 
    yaws:make_www_authenticate_header("Negotiate").


start_opaque(Opaque) when is_list(Opaque) ->
    if
        is_list(Opaque) ->
            Keytab = get_option("keytab", Opaque),
            start(Keytab);
        true ->
            throw(keytab_not_found)
    end.

get_option(Name, Options) when is_list(Options) ->
    case lists:keysearch(Name, 1, Options) of
        {value, {Name, Value}} ->
            Value;
        false ->
            throw(not_found)
    end.

-ifndef(ENABLE_DEBUG).
ignore(_) -> ok.
ignore(_,_) -> ok.
-endif.

