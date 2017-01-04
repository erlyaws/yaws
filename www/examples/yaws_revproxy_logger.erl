-module(yaws_revproxy_logger).

-behaviour(yaws_logger).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-export([open_log/3, close_log/3, wrap_log/4, write_log/4]).

-define(REVPROXY_WHITELIST, [{192,168,0,1}, {192,168,0,2}]).

%% ===================================================================
open_log(ServerName, Type, Dir) ->
    yaws_log:open_log(ServerName, Type, Dir).


close_log(ServerName, Type, State) ->
    yaws_log:close_log(ServerName, Type, State).


wrap_log(ServerName, Type, Data, LogWrapSize) ->
    yaws_log:wrap_log(ServerName, Type, Data, LogWrapSize).


write_log(ServerName, auth, State, {Ip, Path, Item}) ->
    yaws_log:write_log(ServerName, auth, State, {Ip, Path, Item});
write_log(ServerName, access, State, {Ip, Req, InH, OutH, Time}) ->
    RealIp = real_client_ip(Ip, ?REVPROXY_WHITELIST, InH),
    yaws_log:write_log(ServerName, access, State, {RealIp, Req, InH, OutH, Time}).


real_client_ip(Ip, ProxyWhitelist, Hdrs) ->
    case lists:member(Ip, ProxyWhitelist) of
        true ->
            FwdFor = Hdrs#headers.x_forwarded_for,
            case yaws:split_sep(FwdFor, $,) of
                [FirstIp|_Proxies] ->
                    %% We might check if the last proxy is the remote
                    %% address of the request, i.e hd(_Proxies) =:= Ip.
                    case inet_parse:address(FirstIp) of
                        {error, _}     -> unknown;
                        {ok, ClientIp} -> ClientIp
                    end;
                [] ->
                    Ip
            end;
        false ->
            Ip
    end.
