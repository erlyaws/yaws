-module(utils).

%%% File    : utils.erl
%%% Author  : Mickael Remond <mickael.remond@erlang-fr.org>
%%% Description : Various general purpose helper functions
%%% Created : 22 Oct 2003 by Mickael Remond
%%%                          <mickael.remond@erlang-fr.org>

-export([time_to_string/1]).

%% Time (as return by calendar:local_time() to string conversion.
time_to_string( {{Y,Mo,D},{H,Mi,S}} ) ->
    String = io_lib:format( "~4.4w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
			    [Y,Mo,D,H,Mi,S] ),
    lists:flatten(String).


