%%% File    : yaws_sendfile.erl
%%% Author  : Steve Vinoski <vinoski@ieee.org>
%%% Description : sendfile interface
%%% Created :  9 Nov 2008 by Steve Vinoski <vinoski@ieee.org>

-module(yaws_sendfile).
-author('vinoski@ieee.org').

-include("../include/yaws.hrl").
-include_lib("kernel/include/file.hrl").

-export([send/2, send/3, send/4]).

send(Out, Filename) ->
    send(Out, Filename, 0, all).
send(Out, Filename, Offset) ->
    send(Out, Filename, Offset, all).

send(Out, Filename, Offset, Count) ->
    GC             = get(gc),
    ChunkSize      = GC#gconf.large_file_chunk_size,
    case ?gc_use_erlang_sendfile(GC) of
        true ->
            erlang_sendfile(Out, Filename, Offset, Count, ChunkSize);
        false ->
            compat_send(Out, Filename, Offset, Count, ChunkSize)
    end.

bytes_to_transfer(Filename, Offset, Count) ->
    case Count of
        all ->
            case file:read_file_info(Filename) of
                {ok, #file_info{size = Size}} -> Size - Offset;
                Error -> Error
            end;
        Count when is_integer(Count) ->
            Count;
        _ ->
            {error, badarg}
    end.

erlang_sendfile(Out, Filename, Offset, Count, ChunkSize) ->
    Count = bytes_to_transfer(Filename, Offset, Count),
    case Count of
	{error, _}=Error1 ->
	    Error1;
	_ ->
	    case file:open(Filename, [raw, read, binary]) of
		{ok, RawFile} ->
		    Res = file:sendfile(RawFile, Out, Offset, Count,
					[{chunk_size, ChunkSize}]),
		    ok = file:close(RawFile),
		    Res;
		Error2 ->
		    Error2
	    end
    end.

compat_send(Out, Filename, Offset, Count0, ChunkSize) ->
    Count = case Count0 of
                0 -> all;
                _ -> Count0
            end,
    case file:open(Filename, [read, binary, raw]) of
        {ok, Fd} ->
            file:position(Fd, {bof, Offset}),
            Ret = loop_send(Fd, ChunkSize, file:read(Fd, ChunkSize), Out,
                            Count, 0),
            file:close(Fd),
            Ret;
        Err ->
            Err
    end.

loop_send(Fd, ChunkSize, {ok, Bin}, Out, all, BytesSent) ->
    case gen_tcp:send(Out, Bin) of
        ok ->
            loop_send(Fd, ChunkSize, file:read(Fd, ChunkSize), Out, all,
                      BytesSent+size(Bin));
        Err ->
            Err
    end;
loop_send(_Fd, _ChunkSize, eof, _Out, _, BytesSent) ->
    {ok, BytesSent};
loop_send(Fd, ChunkSize, {ok, Bin}, Out, Count, BytesSent) ->
    Sz = size(Bin),
    if Sz < Count ->
            case gen_tcp:send(Out, Bin) of
                ok ->
                    loop_send(Fd, ChunkSize, file:read(Fd, ChunkSize),
                              Out, Count-Sz, BytesSent+Sz);
                Err ->
                    Err
            end;
       Sz == Count ->
            case gen_tcp:send(Out, Bin) of
                ok  -> {ok, BytesSent+Sz};
                Err -> Err
            end;
       Sz > Count ->
            <<Deliver:Count/binary , _/binary>> = Bin,
            case gen_tcp:send(Out, Deliver) of
                ok  -> {ok, BytesSent+Count};
                Err -> Err
            end
    end;
loop_send(_Fd, _, Err, _, _, _) ->
    Err.
