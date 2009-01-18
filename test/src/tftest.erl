-module(tftest).

%%% external exports
-export([start/0, start/1]).

%% Use these functions if you want to start/stop coverage yourself
-export([cover_start/1, cover_stop/1]).
-export([get_headers/1]).
-include("../include/tftest.hrl").

start() ->
    start(true).
start([_]) ->
    start(false);

start(DoHalt) ->
    spawn(fun() -> start1(DoHalt) end).

start1(DoHalt) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> start2() end),
    receive
	{'EXIT', Pid, tests_ok} ->
	    io:format("\n** All tests completed successfully.\n"),
	    dohalt(DoHalt, 0);
	{'EXIT', Pid, {tests_failed, M, Line, Error}} ->
	    {ok, Wd} = file:get_cwd(),
	    io:format("**************\n"
		      "** test failed at ~p.erl:~p\n"
		      "   in ~p\n"
		      "** reason: \n"
		      "   ~p\n"
		      "**************\n",
		      [M, Line, Wd, Error]),
	    %% allow time to flush output
	    timer:sleep(1000),
	    dohalt(DoHalt, 1);
	{'EXIT', Pid, R} ->
	    io:format("**************\n"
		      "** test failed ~p\n",
		      [R]),
	    %% allow time to flush output
	    timer:sleep(1000),
	    dohalt(DoHalt, 1)
    end.

start2() ->
    CM =
	case catch app_test:cover_modules() of
	    {'EXIT', _} ->
		[];
	    Mods ->
		cover_start(Mods)
	end,
    application:load(sasl),
    application:set_env(sasl, errlog_type, error),
    application:start(sasl),
    TEST = os:getenv("TEST"),
    case catch app_test_start(TEST) of
	ok ->
	    cover_stop(CM),
	    exit(tests_ok);
	Error ->
	    {M,Line} = case get('$line') of
			   undefined -> {undefined, undefined};
			   ML -> ML
		       end,
	    exit({tests_failed, M, Line, Error})
    end.


app_test_start(false) ->
    app_test:start();
app_test_start(Mod) ->
    app_test:start([list_to_atom(Mod)]).

%% @spec (Mods::[atom()]) -> CoveredModules::[atom()]
cover_start(Mods) ->
    cover:start(),
    %% First cover-compile listed modules
    CM = lists:foldl(
	   fun (M, Ms) ->
		   case module_source_or_beam(M) of
		       {source, File} ->
			   {ok, _} = cover:compile(File),
			   [M|Ms];
		       {beam, File} ->
			   {ok, _} = cover:compile_beam(File),
			   [M|Ms];
		       _ ->
			   Ms
		   end
	   end, [], Mods),
    %% Now merge any previous state
    case file:open(cover_state_file(), [read]) of
	{ok, F} ->
	    file:close(F),
	    cover:import(cover_state_file());
	_ ->
	    ok
    end,
    CM.

module_source_or_beam(M) ->
    CInfo = M:module_info(compile),
    {value, {options, COptions}} = lists:keysearch(options, 1, CInfo),
    Source = case lists:keysearch(source, 1, CInfo) of
		 {value, {source, SFile}} ->
		     case file:open(SFile, [read]) of
			 {ok, SFd} ->
			     file:close(SFd),
			     SFile;
			 _ ->
			     unknown
		     end;
		 _ ->
		     unknown
	    end,
    Beam = case lists:keysearch(outdir, 1, COptions) of
	       {value, {outdir, Dir = "/" ++ _}} ->
		   BFile = filename:join(Dir, atom_to_list(M) ++ ".beam"),
		   case file:open(BFile, [read]) of
		       {ok, BFd} ->
			   file:close(BFd),
			   BFile;
		       _ ->
			   unknown
		   end;
	       _ ->
		   unknown
	   end,
    case lists:member(debug_info, COptions) of
	true when Beam =/= unknown ->
	    {beam, Beam};
	_ when Source =/= unknown ->
	    {source, Source};
	_ ->
	    false
    end.

cover_state_file() -> "/tmp/COVER.coverdata".

%% @spec (CoveredModules::[atom()]) -> void()
cover_stop([]) ->
    ok;
cover_stop(Mods) ->
    lists:foreach(fun (M) -> cover:analyse_to_file(M) end, Mods),
    cover:export(cover_state_file()),		% Keep state
    cover:stop(),
    ok.

dohalt(true, Int) ->
    erlang:halt(Int);
dohalt(false, _) ->
    ok.


%% read out the http headers from a socket
get_headers(C) ->
    get_headers(C, gen_tcp:recv(C, 0), []).
get_headers(_C, {ok, http_eoh}, Ack) ->
    Ack;
get_headers(_C, {error, R}, _) ->
    {error, R};
get_headers(C, {ok, H}, Ack) ->
    get_headers(C, gen_tcp:recv(C, 0), [H|Ack]).

