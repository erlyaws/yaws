-module(yaws_content_negotiation).
-export([compute_rsva/2, parse_accept_field/1, match_media_type/2]).

-include("../include/yaws_api.hrl").

%% TODO:  analyze impact of spaces in headers; when/where to strip?

%% RFC 2296
compute_rsva(Arg, VariantList) ->
    Headers = Arg#arg.headers,
    Accept = parse_accept_field(Headers#headers.accept),
                                                % move these into #headers{} !
    AcceptCharset = parse_accept_field(
                      get_other_header('Accept-Charset', Headers)),
    AcceptLanguage = parse_accept_field(
                       get_other_header('Accept-Language', Headers)),
    AcceptFeatures = [], % yet to be implemented
    RankedVariants = overall_quality(VariantList, Accept, 
                                     AcceptCharset, AcceptLanguage, 
                                     AcceptFeatures),
    case determine_result(RankedVariants) of
        none ->
            %% TODO:  build Alternates: header field
            VariantList;
        URI ->
            %% TODO:  must be neighbour variant (same root as request)
            URI
    end.

get_other_header(Name, Headers) ->
    case lists:keysearch(Name, 3, Headers) of
        {value, {_,_,Name,_,Field}} -> Field;
        false -> undefined
    end.

determine_result(V) ->
    determine_result(V, {[], {0,1}}).

determine_result([{URI,{Q,X}}|T], {_,{AccQ,_}}) when Q > AccQ ->
    determine_result(T, {URI,{Q,X}});
determine_result([_|T], Acc) ->
    determine_result(T, Acc);
determine_result([], {URI,{_,0}}) -> URI;
determine_result([], _) -> none.

overall_quality(VariantList, Accept, AcceptCharset, AcceptLanguage, 
                AcceptFeatures) ->
    overall_quality(VariantList, Accept, AcceptCharset, 
                    AcceptLanguage, AcceptFeatures, []).

overall_quality([{URI,Qs,Attributes}|T], Accept, AcceptCharset, 
                AcceptLanguage, AcceptFeatures, Acc) ->
    %% QX = {Qvalue, [0=definitive, >0=speculative]}
    Qt = case {lists:keysearch(type, 1, Attributes), Accept} of
             {false, _} -> {1000, 0};
             {_, []} -> {1000, 1};
             {{value, {type, MediaType}}, Accept} ->
                 match_media_type(MediaType, Accept)
         end,
    Qc = case {lists:keysearch(charset, 1, Attributes), AcceptCharset} of
             {false, _} -> {1000, 0};
             {_, []} -> {1000, 1};
             {{value, {charset, Charset}}, AcceptCharset} ->
                 match_charset(Charset, AcceptCharset)
         end,
    Ql = case {lists:keysearch(language, 1, Attributes), AcceptLanguage} of
             {false, _} -> {1000, 0};
             {_, []} -> {1000, 1};
             {{value, {language, Languages}}, AcceptLanguage} ->
                 match_language(Languages, AcceptLanguage)
         end,
    Qf = {1000, 0},  % Accept-Features:  not yet implemented
    Q = round5(Qs, Qt, Qc, Ql, Qf),
    overall_quality(T, Accept, AcceptCharset, AcceptLanguage, 
                    AcceptFeatures, [{URI, Q}|Acc]);
%% fallback-variant
overall_quality([URI|T], Accept, AcceptCharset, AcceptLanguage, 
                AcceptFeatures, Acc) 
  when is_list(URI) ->
    Q = {1, 0},  % {0.000001, definitive}
    overall_quality(T, Accept, AcceptCharset, AcceptLanguage, 
                    AcceptFeatures, [{URI, Q}|Acc]);
overall_quality([], _Accept, _AcceptCharset, _AcceptLanguage, 
                _AcceptFeatures, Acc) -> Acc.

%% this functions rounds to five decimal places but to avoid
%% using floats we have 1 represent 0.000001 and 1000000 = 1.0
round5(Qs, {Qt, A}, {Qc, B}, {Ql, C}, {Qf, D}) ->
    Q = ((Qs * 1000) * (Qt * 1000) * (Qc * 1000) * (Ql * 1000) * (Qf * 1000)),
    {((Q div 10000000000000000000000000) * 10), (A+B+C+D)}.

match_media_type(MediaType, AcceptRanges) ->
    case lists:keysearch(MediaType, 1, AcceptRanges) of
        {ok, {_, Q}} -> {Q, 0};
        false ->
            [SimpleMediaType|_] = yaws:split_sep(MediaType, $;),
            match_media_type_simple(SimpleMediaType, AcceptRanges)
    end.

match_media_type_simple(SimpleMediaType, AcceptRanges) ->
    case lists:keysearch(SimpleMediaType, 1, AcceptRanges) of
        {ok, {_, Q}} -> {Q, 0};
        false ->
            [Type|_] = yaws:split_sep(SimpleMediaType, $/),
            match_media_type_only(Type, AcceptRanges)
    end.

match_media_type_only(Type, AcceptRanges) ->
    case lists:keysearch(Type++"/*", 1, AcceptRanges) of
        {ok, {_, Q}} -> {Q, 1};
        false ->
            case lists:keysearch("*/*", 1, AcceptRanges) of
                {ok, {_, Q}} -> {Q, 1};
                false -> {0, 0}
            end
    end.

match_charset(Charset, AcceptCharset) ->
    case lists:keysearch(Charset, 1, AcceptCharset) of
        {ok, {_, Q}} -> {Q, 0};
        false -> {0, 0}
    end.

match_language(Languages, AcceptLanguage) ->
    match_language(Languages, AcceptLanguage, 0).

match_language([Language|T], AcceptLanguage, Qacc) ->
    Qacc1 = case lists:keysearch(Language, 1, AcceptLanguage) of
                {ok, {_, Q1}} when Q1 > Qacc -> Q1;
                _ -> Qacc
            end,
    Qacc2 = case yaws:split_sep(Language, $-) of
                [H|T] when length(T) > 0 ->
                    case lists:keysearch(H, 1, AcceptLanguage) of
                        {ok, {_, Q2}} when Q2 > Qacc1 -> Q2;
                        _ -> Qacc1
                    end;
                [Language] -> Qacc1
            end,
    match_language(T, AcceptLanguage, Qacc2);
match_language([], AcceptLanguage, Qacc) ->
    case lists:keysearch("*", 1, AcceptLanguage) of
        {ok, {_, Q}} when Q > Qacc -> {Q, 1};
        _ -> {Qacc, 0}
    end.

parse_accept_field(undefined) -> [];
parse_accept_field(Field) ->
    parse_accept_field(yaws:split_sep(Field, $,), []).

parse_accept_field([H|T], Acc) ->
    R = parse_accept_element(yaws:split_sep(H, $;)),
    parse_accept_field(T, Acc ++ [R]);
parse_accept_field([], Acc) -> Acc.

parse_accept_element(L) ->
    parse_accept_element(L, []).

parse_accept_element(["q="++Q|_], Acc) ->
    {Acc, yaws:parse_qvalue(Q)};
parse_accept_element([H|T], []) ->
    parse_accept_element(T, H);
parse_accept_element([H|T], Acc) ->
    parse_accept_element(T, Acc++";"++H);
parse_accept_element([], Acc) ->
    {Acc, 1000}.


