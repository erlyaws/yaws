%% Copyright (C) 2006 Gaspar Chilingarov <nm@web.am>
%%                    Gurgen Tumanyan <barbarian@armkb.com>
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

-module(jsonrpc).
-author("Gaspar Chilingarov <nm@web.am>, Gurgen Tumanyan <barbarian@armkb.com>").
-vsn("3").
-export([s/2]).        % extract element from proplist

%%% lookup element in proplist
s({struct, List}, ElemName) ->
    s(List, ElemName);
s(List, ElemName) when is_list(List) ->
    case lists:keysearch(ElemName,1,List) of
        {value,{ElemName,Val}} ->
            Val;
        false when is_atom(ElemName) ->
            ElemList = atom_to_list(ElemName),
            case lists:keysearch(ElemList,1,List) of
                {value,{ElemList,Val}} ->
                    Val;
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.


% vim: tabstop=4 ft=erlang
