-module(test).
-compile(export_all).


%% used by Makefile to connect to running yaws node
connect([Host]) ->
    Node = list_to_atom("test@" ++ atom_to_list(Host)),
    io:format("Connectimng to ~p~n", [Node]),
    X = (catch rpc:call(Node, shell, start, [])),
    timer:sleep(infinity).


run(Eunits) ->
    try 
        lists:foreach(fun(E) ->
                              io:format("Running ~p~n", [E]),
                              E:test()
                      end, Eunits),
        erlang:halt(0)
    catch
        _:_ ->
            io:format("Test failure ~p~n", [erlang:get_stacktrace()]),
            receive nono -> ok
            after 1000 -> ok
            end,
            erlang:halt(1)
    end.

        

                   
