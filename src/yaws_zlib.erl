%%% Utility functions for zlib.
-module(yaws_zlib).
-author('carsten@codimi.de').


-include("../include/yaws.hrl").


-export([gzipInit/1, gzipInit/2, gzipEnd/1, gzipDeflate/4, gzip/1, gzip/2]).


gzipInit(Z) ->
    gzipInit(Z, #deflate{}).

gzipInit(Z, DOpts) ->
    ok = zlib:deflateInit(Z, DOpts#deflate.compression_level, deflated,
                          DOpts#deflate.window_size, DOpts#deflate.mem_level,
                          DOpts#deflate.strategy),
    undefined.


gzipEnd(Z) ->
    zlib:deflateEnd(Z).


gzipDeflate(Z, undefined, Bin, Flush) ->
    Crc32 = zlib:crc32(Z),
    Head = <<
                                                % ID
             16#1f, 16#8b,
                                                % deflate
             8:8,
                                                % flags
             0:8,
                                                % mtime
             0:32,
                                                % xflags
             0:8,
                                                % OS_UNKNOWN
                                                % Set to Unix instead?
             255:8>>,
    {ok, Priv, Bs} = gzipDeflate(Z, {Crc32,0}, Bin, Flush),
    {ok, Priv, [Head | Bs]};

gzipDeflate(Z, {Crc32,Size}, Bin, Flush) ->
    Bs = zlib:deflate(Z, Bin, Flush),
    {ok, Crc1} = crc32(Z, Crc32, Bin),
    Size1 = Size+size(Bin),
    Data =
        if
            Flush == finish ->
                                                % Appending should not
                                                % hurt, so let's be a
                                                % bit more consistent
                                                % here.
                Bs ++ [<<Crc1:32/little, Size1:32/little>>];
            true ->
                Bs
        end,
    {ok, {Crc1, Size1}, Data}.


%% like zlib:gzip/1, but returns an io list
gzip(Data) ->
    gzip(Data, #deflate{}).

gzip(Data, DOpts) when is_binary(Data) ->
    Z = zlib:open(),
    {ok, _, D} = gzipDeflate(Z, gzipInit(Z, DOpts), Data, finish),
    gzipEnd(Z),
    zlib:close(Z),
    {ok, D};

gzip(Data, DOpts) ->
    Z = zlib:open(),
    gzip_loop(Z, gzipInit(Z, DOpts), Data, [], []).

gzip_loop(Z, P, [], [], A) ->
    {ok, _, D} = gzipDeflate(Z, P, <<>>, finish),
    gzipEnd(Z),
    zlib:close(Z),
    {ok, [A|D]};
gzip_loop(Z, P, B, C, A) when is_binary(B) ->
    {ok, P1, D} = gzipDeflate(Z, P, B, none),
    gzip_loop(Z, P1, C, [],
              case D of
                  [] -> A;
                  _ ->
                      case A of
                          [] -> D;
                          _ -> [A|D]
                      end
              end);
gzip_loop(Z, P, [I|T], C, A) when is_integer(I) ->
    gzip_loop(Z, P, list_to_binary([I|T]), C, A);
gzip_loop(Z, P, [H], C, A) ->
    gzip_loop(Z, P, H, C, A);
gzip_loop(Z, P, [H|T], C, A) ->
    gzip_loop(Z, P, H, [T|C], A);
gzip_loop(Z, P, [], C, A) ->
    gzip_loop(Z, P, C, [], A);
gzip_loop(Z, P, I, C, A) when is_integer(I) ->
    gzip_loop(Z, P, <<I>>, C, A).


%% To work around a bug in zlib.

crc32(Z, CRC, Binary) ->
    case port_control(Z, 17, <<CRC:32, Binary/binary>>) of
        [2,A,B,C,D] -> {ok, (A bsl 24)+(B bsl 16)+(C bsl 8)+D}
    end.

