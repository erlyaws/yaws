%%%----------------------------------------------------------------------
%%% File    : yaws_ctl.erl
%%% Author  : Claes Wikstrom <klacke@bluetail.com>
%%% Purpose : 
%%% Created : 29 Apr 2002 by Claes Wikstrom <klacke@bluetail.com>
%%%----------------------------------------------------------------------


%% some code to remoteley control a running yaws server

-module(yaws_ctl).
-author('klacke@bluetail.com').

-compile(export_all).
-include_lib("kernel/include/file.hrl").
-define(F, "/tmp/yaws.ctl").


start(_Top) ->
    case gen_tcp:listen(0, [{packet, 2},
			    {active, false},
			    binary,
			    {ip, {127,0,0,1}},
			    {reuseaddr, true}]) of
	{ok,  L} ->
	    case inet:sockname(L) of
		{ok, {_, Port}} ->
		    F = ?F,
		    file:write_file(F, io_lib:format("~w", [Port])),
		    {ok, FI} = file:read_file_info(F),
		    M = FI#file_info.mode,
		    M2 = M bor (8#00222),
		    file:write_file_info(F, FI#file_info{mode = M2}), %% ign ret
		    aloop(L);
		_Err ->
		    error_logger:format("Cannot get sockname for ctlsock",[])
	    end;
	_Err ->
	    error_logger:format("Cannot listen on ctl socket ",[])
    end.


aloop(L) ->
    case gen_tcp:accept(L) of
	{ok, A} ->
	    handle_a(A);
	_Err ->
	    ignore
    end,
    aloop(L).

handle_a(A) ->
    case gen_tcp:recv(A, 0) of
	{ok, Data} ->
	    case binary_to_term(Data) of
		hup ->
		    yaws:hup();
		stop ->
		    init:stop();
		_Other ->
		    ignore
	    end,
	    gen_tcp:close(A);
	_Err ->
	    ignore
    end.

actl(Term) ->
    case file:read_file(?F) of
	{ok, B} ->
	    L = binary_to_list(B),
	    I = list_to_integer(L),
	    case gen_tcp:connect({127,0,0,1}, I,
				 [{active, false},
				  {reuseaddr, true},
				  binary,
				  {packet, 2}]) of
		{ok, Fd} ->
		    gen_tcp:send(Fd, term_to_binary(Term)),
		    gen_tcp:close(Fd);
		Err ->
		    Err
	    end;
	Err ->
	    io:format("yaws: Cannot open runfile ~s ... server not running ?? ~n",
		      [?F])
    end,
    init:stop().


%% send a hup (kindof) to the yaws server to make it
%% reload its configuration and clear its caches

hup() ->
    actl(hup).

stop() ->	
    actl(stop).




		    

		    


		

