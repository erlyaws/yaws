-module(test).
-compile(export_all).


run([]) ->
    erlang:halt(0);
run([E|Eunits]) ->
    try
        io:format("Running ~p~n", [E]),
        ok = E:test(),
        run(Eunits)
    catch
        _:_ ->
            erlang:halt(1)
    end.
