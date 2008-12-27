-module(test).
-compile(export_all).


%% used by Makefile to connect to running yaws node
connect([Host]) ->
    Node = list_to_atom("test@" ++ atom_to_list(Host)),
    io:format("Connectimng to ~p~n", [Node]),
    X = (catch rpc:call(Node, shell, start, [])),
    timer:sleep(infinity).


                   
