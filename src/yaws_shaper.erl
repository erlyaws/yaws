%%%----------------------------------------------------------------------
%%% File    : yaws_shaper.erl
%%% Author  : Christopher Faulet <christopher@yakaz.com>
%%% Purpose :
%%% Created : 14 Dec 2010 by Christopher Faulet <christopher@yakaz.com>
%%%----------------------------------------------------------------------

-module(yaws_shaper).
-author('christopher@yakaz.com').


-export([behaviour_info/1]).

%% API
-export([
         check/2,
         update/3
        ]).


-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{check,1}, {update,3}];
behaviour_info(_Other) ->
    undefined.


check(#sconf{shaper=undefined}, _) ->
    allow;
check(#sconf{shaper=Mod}, IP) ->
    case catch Mod:check(IP) of
        allow ->
            allow;
        {deny, Status, Msg} ->
            {deny, Status, Msg};
        _ ->
            allow
    end.

update(#sconf{shaper=undefined}, _, _) ->
    ok;
update(#sconf{shaper=Mod}, IP, Req) ->
    Bytes = case Req#http_request.method of
                'HEAD' -> 0;
                _  ->
                    case yaws:outh_get_contlen() of
                        undefined ->
                            case yaws:outh_get_act_contlen() of
                                undefined -> 0;
                                Actlen    -> Actlen
                            end;
                        I2 -> I2
                    end
            end,
    catch Mod:update(IP, 1, Bytes).
