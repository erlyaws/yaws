%%%-------------------------------------------------------------------
%%% File    : dime.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Encoding and decoding of DIME messages.
%%% The Direct Internet Message Encapsulation (DIME) specification
%%% defines a mechanism for packaging binary data with SOAP messages.
%%% http://bgp.potaroo.net/ietf/all-ids/draft-nielsen-dime-02.txt
%%% Layout of a DIME encoded message is like this
%%%<pre>
%%%  0                   1                   2                   3
%%%  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%% |         |M|M|C|       |       |                               |
%%% | VERSION |B|E|F| TYPE_T| RESRVD|         OPTIONS_LENGTH        |
%%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%% |            ID_LENGTH          |           TYPE_LENGTH         |
%%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%% |                          DATA_LENGTH                          |
%%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%% |                                                               /
%%% /                     OPTIONS + PADDING                         /
%%% /                                                               |
%%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%% |                                                               /
%%% /                          ID + PADDING                         /
%%% /                                                               |
%%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%% |                                                               /
%%% /                        TYPE + PADDING                         /
%%% /                                                               |
%%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%% |                                                               /
%%% /                        DATA + PADDING                         /
%%% /                                                               |
%%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%%<pre>
%%% @end
%%%
%%% Created :  7 Apr 2008 by Anders Nygren <>
%%%-------------------------------------------------------------------
-module(yaws_dime).

%% API
-export([encode/2,
         decode/1,
         pad_len/1]).


-include_lib("kernel/include/file.hrl").

-define(VERSION1, 1).
-define(T_UNCHANGED, 0).
-define(T_MEDIA_TYPE, 1).
-define(T_ABS_URI, 2).
-define(T_UNKNOWN, 3).
-define(T_NONE, 4).
-define(SOAP_URI, "http://schemas.xmlsoap.org/soap/envelope").
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec (Req, Attachments) ->binary()
%% Description:
%%--------------------------------------------------------------------
encode(Req, []) ->
    encode_part(1, 1, 0, ?T_ABS_URI, <<"">>, <<"">>, <<?SOAP_URI>>, Req);
encode(Req, As) ->
        list_to_binary([encode_part(1, 0, 0, ?T_ABS_URI,
                                    <<"">>, <<"">>, <<?SOAP_URI>>, Req)|
        encode_attachments(As)]).

encode_attachments([{attachment, Id, Type, File}]) ->
    [encode_part(0, 1, 0, ?T_ABS_URI, <<"">>, list_to_binary(Id),
                 list_to_binary(Type), File)];
encode_attachments([{attachment, Id, Type, File} | As]) ->
    [encode_part(0, 0, 0, ?T_ABS_URI, <<"">>, list_to_binary(Id),
                 list_to_binary(Type), File)|
     encode_attachments(As)].

encode_part(MB, ME, CF, TypeT, Opts, ID, Type, Data) ->
    Opts_len = size_of(Opts),
    Opts_pad = pad_len(Opts_len),
    Id_len = size_of(ID),
    Id_pad = pad_len(Id_len),
    Type_len = size_of(Type),
    Type_pad = pad_len(Type_len),
    Data_len = size_of(Data),
    Data_pad = pad_len(Data_len),
    Data1 = get_data(Data),
    <<?VERSION1:5, MB:1, ME:1, CF:1, TypeT:4, 0:4,
     Opts_len:1/big-integer-unit:16,
     Id_len:1/big-integer-unit:16,
     Type_len:1/big-integer-unit:16,
     Data_len:1/big-integer-unit:32,
     Opts:Opts_len/binary-unit:8, 0:Opts_pad/integer-unit:8,
     ID:Id_len/binary-unit:8,     0:Id_pad/integer-unit:8,
     Type:Type_len/binary-unit:8, 0:Type_pad/integer-unit:8,
     Data1:Data_len/binary-unit:8, 0:Data_pad/integer-unit:8>>.

pad_len(Len) ->
    case Len rem 4 of
        0 ->
            0;
        N ->
            4-N
    end.

size_of(X) when is_list(X) ->
    length(X);
size_of(X) when is_binary(X)->
    size(X);
size_of({file, File}) ->
    {ok,R} = file:read_file_info(File),
    R#file_info.size.

get_data({file, File}) ->
    {ok, Data} = file:read_file(File),
    Data;
get_data(Data) when is_list(Data) ->
    list_to_binary(Data);

get_data(Data) ->
    Data.
%%--------------------------------------------------------------------
%% @spec (Msg::binary()) ->
%% @doc Decode a DIME encoded message.
%%--------------------------------------------------------------------
decode(Msg) ->
    decode_recs(Msg, [], []).

decode_recs(<<>>, Acc, _Chunks) ->
    Acc;
decode_recs(Msg, Acc, Chunks) ->
    case {decode_rec(Msg), Chunks} of
        %% A normal record
        {{_ME=0, _CF=0, Opts, ID, Type, Chunk, More}, []} ->
            decode_recs(More, [{Opts, ID, Type, Chunk}|Acc], []);

        %% The last chunk of a block, but not the last record
        {{_ME=0, _CF=0, Opts, ID, Type, Chunk, More}, Chunks} ->
            Cs = lists:reverse([{Opts, ID, Type, Chunk}|Chunks]),
            %% Only the first chunk has values for Opts, Id and Type
            {ROpts, RID, RType, _Chunk} = hd(Cs),
            Block = list_to_binary([Ch || {_Opts, _ID, _Type, Ch} <- Cs]),
            error_logger:info_report([?MODULE, decode_rec, merging,
                                      {id, ID},{type, Type},
                                      {chunks, length(Cs)}]),
            decode_recs(More, [{ROpts, RID, RType, Block}|Acc], []);

        %% Last record, but not chunked
        {{_ME=1, _CF=0, Opts, ID, Type, Data, _More}, []} ->
            lists:reverse([{Opts, ID, Type, Data}|Acc]);

        %% Last record, and last chunk of block
        {{_ME=1, _CF=0, Opts, ID, Type, Chunk, _More}, Chunks} ->
            Cs = lists:reverse([{Opts, ID, Type, Chunk}|Chunks]),
            {ROpts, RID, RType, _Chunk} = hd(Cs),
            Block = list_to_binary([Ch || {_Opts, _ID, _Type, Ch} <- Cs]),
            error_logger:info_report([?MODULE, decode_rec, merging,
                                      {id, ID},{type, Type},
                                      {chunks, length(Cs)}]),
            lists:reverse([{ROpts, RID, RType, Block}|Acc]);

        %% First or intermediate chunk, but not the last
        {{_ME=0, _CF=1, Opts, ID, Type, Data, More}, Chunks} ->
            decode_recs(More, Acc, [{Opts, ID, Type, Data}|Chunks]);

        %% Something wrong, ME=1 and CF=1
        ErrorResult ->
                error_logger:error_report([?MODULE, decode_recs,
                                           {result, ErrorResult}])
    end.

decode_rec(<<?VERSION1:5, _MB:1, ME:1, CF:1, _Type_T:4, _Res:4,
            Opt_Len:1/big-integer-unit:16,
            ID_Len:1/big-integer-unit:16,
            Type_Len:1/big-integer-unit:16,
            Data_Len:1/big-integer-unit:32,
            Rest/binary>>=Block) ->
    error_logger:info_report([?MODULE, decode_rec,
                              {version,1},
                              {mb,_MB},{me,ME},{cf,CF},{type_t,_Type_T},
                              {res,_Res},{opt_len,Opt_Len},{id_len,ID_Len},
                              {type_len,Type_Len},{date_len,Data_Len},
                              {total_rec_size,byte_size(Block)}]),
    Opt_pad = pad_len(Opt_Len),
    Id_pad = pad_len(ID_Len),
    Type_pad = pad_len(Type_Len),
    Data_pad = pad_len(Data_Len),
    Header = Opt_Len+Opt_pad+ID_Len+Id_pad+Type_Len+Type_pad,
    DataTot = Data_Len+Data_pad,
    case byte_size(Rest) of
        N when N >= Header+DataTot ->
            <<Opts:Opt_Len/binary-unit:8, _:Opt_pad/binary-unit:8,
             ID:ID_Len/binary-unit:8,     _:Id_pad/binary-unit:8,
             Type:Type_Len/binary-unit:8, _:Type_pad/binary-unit:8,
             Data:Data_Len/binary-unit:8, _:Data_pad/binary-unit:8,
             More/binary >> = Rest,
            {ME, CF, Opts, ID, Type, Data, More};
        N when N > Header->
            <<Opts:Opt_Len/binary-unit:8, _:Opt_pad/binary-unit:8,
             ID:ID_Len/binary-unit:8,     _:Id_pad/binary-unit:8,
             Type:Type_Len/binary-unit:8, _:Type_pad/binary-unit:8,
             Data/binary>> = Rest,
            error_logger:error_report([?MODULE, decode_rec, short_record,
                                       {need, Header+DataTot},
                                       {has, N}]),
            {ME, CF, Opts, ID, Type, Data, <<>>};
        N ->
            error_logger:error_report([?MODULE, decode_rec, short_record,
                                       {need, Header+DataTot},
                                       {has, N}]),
            {error, not_enough_data}
    end;

decode_rec(Bin) ->
    error_logger:error_report([?MODULE, decode_rec, short_record,
                               no_header,
                               {has, byte_size(Bin)}]),
    {error, no_header}.


%%====================================================================
%% Internal functions
%%====================================================================
