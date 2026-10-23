%%%----------------------------------------------------------------------
%%% File    : yaws_acceptor_sup.erl
%%% Author  : Per Andersson <per@ionio.se>
%%% Purpose : Supervise acceptors
%%% Created : 12 Oct 2026 by Per Andersson <per@ionio.se>
%%%----------------------------------------------------------------------

-module(yaws_acceptor_sup).
-author('per@ionio.se').
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([new_acceptors/3, acceptor/4]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok,
     {#{strategy => simple_one_for_one,
        intensity => 0,
        period => 1},
      [#{id => undefined,
         start => {?MODULE, acceptor, []},
         restart => transient,
         shutdown => 120000,
         type => worker,
         modules => [yaws_server]}]}}.


new_acceptors(Top, GC, Group) ->
    ListenDefs = yaws_server:listen_defs(GC, Group),
    new_acceptors(ListenDefs, Top, GC, Group).


new_acceptors([], _, _, _) ->
    [];
new_acceptors([ListenDef | ListenDefs], Top, GC, Group) ->
    {ok, {Pid, SCs}} =
        supervisor:start_child(?MODULE, [ListenDef, Top, GC, Group]),
    [{Pid, SCs} | new_acceptors(ListenDefs, Top, GC, Group)].


acceptor(ListenDef, Top, GC, Group) ->
    proc_lib:start_link(yaws_server, gserver, [ListenDef, Top, GC, Group]).
