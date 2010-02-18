%%% File    : yaws_sendfile_compat.erl
%%% Author  : Claes Wikstrom, klacke@hyber.org
%%% Description : Conditional OS dependent call to sendfile
%%% Created :  Sat Dec 20 20:00:11 CET 2008

-module(yaws_sendfile_compat).
-export([start_link/0, init/1, stop/0, send/2, send/3, send/4, enabled/0]).

-include_lib("kernel/include/file.hrl").
-include("yaws_configure.hrl").

-include("../include/yaws.hrl").

%% will be true for MacOsX, FreeBSD, Linux
-ifdef(HAVE_SENDFILE).

enabled() ->
    true.
start_link() ->
    yaws_sendfile:start_link().
stop() ->
    yaws_sendfile:stop().
init(ShLib) ->
    yaws_sendfile:init(ShLib).
send(Out, FileName) ->
    case yaws_sendfile:send(Out, FileName) of
        {error, eoverflow} ->
            compat_send(Out, FileName, 0, all);
        Other ->
            Other
    end.
send(Out, FileName, Offset) ->
    case yaws_sendfile:send(Out, FileName, Offset) of
        {error, eoverflow} ->
            compat_send(Out, FileName, Offset, all);
        Other ->
            Other
    end.
send(Out, FileName, Offset, Count) ->
    case yaws_sendfile:send(Out, FileName, Offset, Count) of
        {error, eoverflow} ->
            compat_send(Out, FileName, Offset, Count);
        Other ->
            Other
    end.

-else.

%% Emulate sendfile, this is true for win32, qnx, solaris. OpenBSD,NetBSD I
%% still don't know

enabled() ->
    false.
start_link() ->
    ignore.
stop() ->
    ok.
init(_) ->
    ok.
send(Out, Filename) ->
    send(Out, Filename, 0, all).
send(Out, Filename, Offset) ->
    send(Out, Filename, Offset, all).
send(Out, Filename, Offset, Count) ->
    compat_send(Out, Filename, Offset, Count).

-endif.

compat_send(Out, Filename, Offset, Count) ->
    case file:open(Filename, [read, binary, raw]) of
        {ok, Fd} ->
            file:position(Fd, {bof, Offset}),
            ChunkSize = (get(gc))#gconf.large_file_chunk_size,
            Ret = loop_send(Fd, ChunkSize, file:read(Fd, ChunkSize), Out, Count),
            file:close(Fd),
            Ret;
        Err ->
            Err
    end.

loop_send(Fd, ChunkSize, {ok, Bin}, Out, all) ->
    case gen_tcp:send(Out, Bin) of
        ok ->
            loop_send(Fd, ChunkSize, file:read(Fd, ChunkSize), Out, all);
        Err ->
            Err
    end;
loop_send(_Fd, _ChunkSize, eof, _Out, _) ->
    ok;
loop_send(Fd, ChunkSize, {ok, Bin}, Out, Count) ->
    Sz = size(Bin),
    if Sz < Count ->
            case gen_tcp:send(Out, Bin) of
                ok ->
                    loop_send(Fd, ChunkSize, file:read(Fd, ChunkSize),
                              Out, Count-Sz);
                Err ->
                    Err
            end;
       Sz == Count ->
            gen_tcp:send(Out, Bin);
       Sz > Count ->
            <<Deliver:Count/binary , _/binary>> = Bin,
            gen_tcp:send(Out, Deliver)
    end;
loop_send(_Fd, _, Err, _,_) ->
    Err.


