-module(yaws_dynopts).

-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").

-export([
         have_ssl_honor_cipher_order/0,
         have_ssl_client_renegotiation/0,
         have_ssl_sni/0,
         have_ssl_log_alert/0,
         have_ssl_handshake/0,
         have_erlang_now/0,
         have_rand/0,
         have_start_error_logger/0,
         have_http_uri_parse/0,
         have_safe_relative_path/0,

         unique_triple/0,
         get_time_tuple/0,
         now_secs/0,
         random_seed/3,
         random_uniform/1,
         connection_information/2,
         ssl_handshake/2,
         start_error_logger/0,
         http_uri_parse/1,
         safe_relative_path/2,

         generate/1,
         purge_old_code/0,
         is_generated/0
        ]).

-export([is_greater/2, is_less/2,
         is_greater_or_equal/2, is_less_or_equal/2]).

%% SSL option honor_cipher_order was added in release 17 (ERTS >= 6.0)
have_ssl_honor_cipher_order() ->
    is_greater_or_equal(erlang:system_info(version), "6.0").

%% SSL option client_renegotiation was added in release 18 (ERTS >= 7.0)
have_ssl_client_renegotiation() ->
    is_greater_or_equal(erlang:system_info(version), "7.0").

%% SSL sni support was added in release 18  (ERTS >= 7.0)
have_ssl_sni() ->
    is_greater_or_equal(erlang:system_info(version), "7.0").

%% SSL option log_alert SSL was added in R16B02 (ERTS >= 5.10.3)
have_ssl_log_alert() ->
    is_greater_or_equal(erlang:system_info(version), "5.10.3").

%% ssl:ssl_accept/2 is deprecated since release 21 (ERTS >= 10.0)
have_ssl_handshake() ->
    is_greater_or_equal(erlang:system_info(version), "10.0").

%% erlang:now/0 is deprecated since releases 18 (ERTS >= 7.0)
have_erlang_now() ->
    is_less(erlang:system_info(version), "7.0").

%% random module is deprecated since releases 19 (ERTS >= 8.0)
have_rand() ->
    (code:which(rand) /= non_existing).

%% error_logger is legacy as of release 21 (ERTS >= 10.0)
have_start_error_logger() ->
    is_greater_or_equal(erlang:system_info(version), "10.0").

%% http_uri is legacy as of release 23 (ERTS >= 11.0)
have_http_uri_parse() ->
    is_less(erlang:system_info(version), "11.0").

%% safe_relative_path was added in release 23 (ERTS >= 11.0)
have_safe_relative_path() ->
    is_greater_or_equal(erlang:system_info(version), "11.0").

unique_triple() ->
    case have_erlang_now() of
        true ->
            (fun erlang:now/0)();
        false ->
            F = fun erlang:unique_integer/1,
            {F([positive]), F([positive]), F([positive])}
    end.

get_time_tuple() ->
    case have_erlang_now() of
        true  -> (fun erlang:now/0)();
        false -> (fun erlang:timestamp/0)()
    end.

now_secs() ->
    {M,S,_} = case have_erlang_now() of
                  true  -> (fun erlang:now/0)();
                  false -> (fun erlang:timestamp/0)()
              end,
    (M*1000000)+S.

random_seed(A,B,C) ->
    case have_rand() of
        true  -> rand:seed(exsplus, {A,B,C});
        false -> (fun random:seed/3)(A,B,C)
    end.

random_uniform(N) ->
    case have_rand() of
        true  -> rand:uniform(N);
        false -> (fun random:uniform/1)(N)
    end.

connection_information(Sock, Items) ->
    case have_ssl_sni() of
        true  -> (fun ssl:connection_information/2)(Sock, Items);
        false -> undefined
    end.

ssl_handshake(Sock, Timeout) ->
    case have_ssl_handshake() of
        true -> (fun ssl:handshake/2)(Sock, Timeout);
        false ->
            case (fun ssl:ssl_accept/2)(Sock, Timeout) of
                ok -> {ok, Sock};
                Error -> Error
            end
    end.

start_error_logger() ->
    case have_start_error_logger() of
        true ->
            case (fun logger:get_handler_config/1)(error_logger) of
                {ok, _} -> ok;
                {error, _} ->
                    LoggerArgs = (fun maps:from_list/1)([{level, info},
                                                         {filter_default, log},
                                                         {filters, []}]),
                    (fun logger:add_handler/3)(error_logger, error_logger,
                                               LoggerArgs)
            end;
        false ->
            ok
    end.

http_uri_parse(Uri) ->
    case have_http_uri_parse() of
        true -> (fun http_uri:parse/1)(Uri);
        false ->
            case (fun uri_string:parse/1)(Uri) of
                {error,_,_}=Error -> Error;
                UriMap ->
                    Scheme = case maps:find(scheme, UriMap) of
                                 {ok, S} -> list_to_existing_atom(S);
                                 error -> ''
                             end,
                    UserInfo = case maps:find(userinfo, UriMap) of
                                   {ok, U} -> U;
                                   error -> ""
                               end,
                    Host = case maps:find(host, UriMap) of
                               {ok, H} -> H;
                               error -> ""
                           end,
                    Port = case maps:find(port, UriMap) of
                               {ok, Po} -> Po;
                               error ->
                                   case Scheme of
                                       http -> 80;
                                       https -> 443;
                                       _ -> undefined
                                   end
                           end,
                    Path = case maps:find(path, UriMap) of
                               {ok, Pa} -> Pa;
                               error -> ""
                           end,
                    Query = case maps:find(query, UriMap) of
                                {ok, Q} -> [$?|Q];
                                error -> ""
                            end,
                    {ok, {Scheme, UserInfo, Host, Port, Path, Query}}
            end
    end.

safe_relative_path(File, Cwd) ->
    case have_safe_relative_path() of
        true -> filelib_safe_relative_path_2(File, Cwd);
        false -> yaws:safe_rel_path(File, Cwd)
    end.

%% Make dialyzer happy, don't call filelib:safe_relative_path/2 for OTP < 23.
-ifdef(OTP_RELEASE).
  -if(?OTP_RELEASE >= 23).
filelib_safe_relative_path_2(File, Cwd) ->
    (fun filelib:safe_relative_path/2)(File, Cwd).
  -else.
%% This will never happen, but fallback if it happens anyway.
filelib_safe_relative_path_2(File, Cwd) ->
    yaws:safe_rel_path(File, Cwd).
  -endif.
-else.
%% This will never happen, but fallback if it happens anyway.
filelib_safe_relative_path_2(File, Cwd) ->
    yaws:safe_rel_path(File, Cwd).
-endif.

is_greater         (Vsn1, Vsn2) -> compare_version(Vsn1, Vsn2) == greater.
is_less            (Vsn1, Vsn2) -> compare_version(Vsn1, Vsn2) == less.
is_greater_or_equal(Vsn1, Vsn2) -> not is_less(Vsn1, Vsn2).
is_less_or_equal   (Vsn1, Vsn2) -> not is_greater(Vsn1, Vsn2).

compare_version(Vsn, Vsn) ->
    equal;
compare_version(Vsn1, Vsn2) ->
    compare_version1(string:tokens(Vsn1, "."), string:tokens(Vsn2, ".")).

compare_version1([], []) ->
    equal;
compare_version1(_X, []) ->
    greater;
compare_version1([], _X) ->
    less;
compare_version1([X1], [X2]) ->
    %% For last digit ignore everything after the "-", if any
    Y1 = lists:takewhile(fun(X) -> X /= $- end, X1),
    Y2 = lists:takewhile(fun(X) -> X /= $- end, X2),
    compare_digit(Y1, Y2);
compare_version1([X1], [X2|_]) ->
    %% For last digit ignore everything after the "-", if any
    Y1 = lists:takewhile(fun(X) -> X /= $- end, X1),
    case compare_digit(Y1, X2) of
        equal -> less;
        Else  -> Else
    end;
compare_version1([X1|_], [X2]) ->
    %% For last digit ignore everything after the "-", if any
    Y2 = lists:takewhile(fun(X) -> X /= $- end, X2),
    case compare_digit(X1, Y2) of
        equal -> greater;
        Else  -> Else
    end;
compare_version1([X|Rest1], [X|Rest2]) ->
    compare_version1(Rest1, Rest2);
compare_version1([X1|Rest1], [X2|Rest2]) ->
    case compare_digit(X1, X2) of
        equal -> compare_version1(Rest1, Rest2);
        Else  -> Else
    end.

compare_digit(X, X) ->
    equal;
compare_digit(X1, X2) when length(X1) > length(X2) ->
    greater;
compare_digit(X1, X2) when length(X1) < length(X2) ->
    less;
compare_digit(X1, X2) ->
    case X1 > X2 of
        true  -> greater;
        false -> less
    end.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_generated() -> false.

purge_old_code() -> not_necessary.

generate(GC) ->
    code:ensure_loaded(crypto),
    code:ensure_loaded(inet),
    TmpDir = case os:getenv("TMPDIR") of
                 false ->
                     case os:getenv("TEMPDIR") of
                         false -> "/tmp";
                         Temp -> Temp
                     end;
                 Tmp -> Tmp
             end,
    case {filelib:is_dir(yaws:id_dir(GC#gconf.id)),
          filelib:is_dir(yaws:tmpdir(TmpDir))} of
        {true, _} ->
            File = filename:join(yaws:id_dir(GC#gconf.id), "yaws_dynopts.erl"),
            generate1(File);
        {_, true} ->
            File = filename:join(yaws:tmpdir("/tmp"), "yaws_dynopts.erl"),
            generate1(File);
        _ ->
            start_error_logger(),
            error_logger:format("Cannot write yaws_dynopts.erl~n"
                                "Use the default version~n", [])
    end.

generate1(ModFile) ->
    case write_module(ModFile) of
        ok ->
            Opts = compile_options(),
            case compile:file(ModFile, Opts) of
                {ok, ModName, Binary} ->
                    case code:load_binary(ModName, [], Binary) of
                        {module, ModName} ->
                            ok;
                        {error, What} ->
                            start_error_logger(),
                            error_logger:format(
                              "Cannot load module '~p': ~p~n"
                              "Use the default version~n",
                              [ModName, What]
                             )
                    end;
                {error, Errors, _} ->
                    start_error_logger(),
                    lists:foreach(
                      fun({File,FileErrors}) ->
                              lists:foreach(
                                fun({Line,ErrorMod,ErrorDesc}) ->
                                        error_logger:format(
                                          "~s:~w: ~s~n",
                                          [File, Line,
                                           ErrorMod:format_error(ErrorDesc)])
                                end, FileErrors)
                      end, Errors),
                    error_logger:format("Use the default version of ~s~n",
                                        [ModFile])
            end;
        {error, Reason} ->
            start_error_logger(),
            error_logger:format("Cannot write ~p: ~p~n"
                                "Use the default version~n", [ModFile, Reason])
    end.

write_module(ModFile) ->
    file:write_file(ModFile, source()).

compile_options() ->
    [binary, report, return_errors,
     {d, 'HAVE_SSL_HONOR_CIPHER_ORDER',    have_ssl_honor_cipher_order()},
     {d, 'HAVE_SSL_CLIENT_RENEGOTIATION',  have_ssl_client_renegotiation()},
     {d, 'HAVE_SSL_LOG_ALERT',             have_ssl_log_alert()}
    ]
        ++
        case have_erlang_now() of
            true  -> [{d, 'HAVE_ERLANG_NOW'}];
            false -> []
        end
        ++
        case have_rand() of
            true  -> [{d, 'HAVE_RAND'}];
            false -> []
        end
        ++
        case have_ssl_sni() of
            true  -> [{d, 'HAVE_SSL_SNI'}];
            false -> []
        end
        ++
        case have_ssl_handshake() of
            true  -> [{d, 'HAVE_SSL_HANDSHAKE'}];
            false -> []
        end
        ++
        case have_start_error_logger() of
            true  -> [{d, 'HAVE_START_ERROR_LOGGER'}];
            false -> []
        end
        ++
        case have_http_uri_parse() of
            true -> [{d, 'HAVE_HTTP_URI_PARSE'}];
            false -> []
        end
        ++
        case have_safe_relative_path() of
            true -> [{d, 'HAVE_SAFE_RELATIVE_PATH'}];
            false -> []
        end.

source() ->
    IncDir  = yaws:get_inc_dir(),
    Src = [
           "-module(yaws_dynopts).",
           "",
           "-include(\"" ++ filename:join(IncDir, "yaws.hrl") ++ "\").",
           "-include(\"" ++ filename:join(IncDir, "yaws_api.hrl") ++ "\").",
           "",
           "-export([",
           "    have_ssl_honor_cipher_order/0,",
           "    have_ssl_client_renegotiation/0,",
           "    have_ssl_sni/0,",
           "    have_ssl_log_alert/0,",
           "    have_ssl_handshake/0,",
           "    have_erlang_now/0,",
           "    have_rand/0,"
           "    have_start_error_logger/0,"
           "    have_http_uri_parse/0,",
           "    have_safe_relative_path/0,",
           "",
           "    unique_triple/0,",
           "    get_time_tuple/0,",
           "    now_secs/0,",
           "    random_seed/3,",
           "    random_uniform/1,",
           "    connection_information/2,",
           "    ssl_handshake/2,"
           "    start_error_logger/0,",
           "    http_uri_parse/1,",
           "    safe_relative_path/2,",
           "",
           "    generate/1,",
           "    purge_old_code/0,",
           "    is_generated/0",
           "   ]).",
           "",
           "",
           "generate(_) -> ok.",
           "purge_old_code() -> code:soft_purge(?MODULE).",
           "is_generated() -> true.",
           "",
           "have_ssl_honor_cipher_order()   -> ?HAVE_SSL_HONOR_CIPHER_ORDER.",
           "have_ssl_client_renegotiation() -> ?HAVE_SSL_CLIENT_RENEGOTIATION.",
           "have_ssl_log_alert()            -> ?HAVE_SSL_LOG_ALERT.",
           "",
           "-ifdef(HAVE_ERLANG_NOW).",
           "have_erlang_now() -> true.",
           "unique_triple() ->",
           "    now().",
           "get_time_tuple() ->",
           "    now().",
           "now_secs() ->",
           "    {M,S,_} = now(),",
           "    (M*1000000)+S.",
           "-else.",
           "have_erlang_now() -> false.",
           "unique_triple() ->",
           "    {erlang:unique_integer([positive]),",
           "     erlang:unique_integer([positive]),",
           "     erlang:unique_integer([positive])}.",
           "get_time_tuple() ->",
           "    erlang:timestamp().",
           "now_secs() ->",
           "    {M,S,_} = erlang:timestamp(),",
           "    (M*1000000)+S.",
           "-endif.",
           "",
           "-ifdef(HAVE_RAND).",
           "have_rand() -> true.",
           "random_seed(A,B,C) -> rand:seed(exsplus, {A,B,C}).",
           "random_uniform(N)  -> rand:uniform(N).",
           "-else.",
           "have_rand() -> false.",
           "random_seed(A,B,C) -> random:seed(A,B,C).",
           "random_uniform(N)  -> random:uniform(N).",
           "-endif.",
           "",
           "-ifdef(HAVE_SSL_SNI).",
           "have_ssl_sni() -> true.",
           "connection_information(Sock, Items) -> ",
           "    ssl:connection_information(Sock, Items).",
           "-else.",
           "have_ssl_sni() -> false.",
           "connection_information(_, _) -> undefined.",
           "-endif.",
           "",
           "-ifdef(HAVE_SSL_HANDSHAKE).",
           "have_ssl_handshake() -> true.",
           "ssl_handshake(Sock, Timeout) ->",
           "    ssl:handshake(Sock, Timeout).",
           "-else.",
           "have_ssl_handshake() -> false.",
           "ssl_handshake(Sock, Timeout) ->",
           "    case ssl:ssl_accept(Sock, Timeout) of"
           "        ok -> {ok, Sock};",
           "        Error -> Error",
           "    end.",
           "-endif.",
           "-ifdef(HAVE_START_ERROR_LOGGER).",
           "have_start_error_logger() -> true.",
           "start_error_logger() ->",
           "    case logger:get_handler_config(error_logger) of",
           "        {ok, _} -> ok;",
           "        {error, _} ->",
           "            logger:add_handler(error_logger, error_logger,",
           "                               #{level => info, filter_default => log,"
           "                                 filters => []})",
           "    end.",
           "-else.",
           "have_start_error_logger() -> false.",
           "start_error_logger() -> ok.",
           "-endif.",
           "-ifdef(HAVE_HTTP_URI_PARSE).",
           "have_http_uri_parse() -> true.",
           "http_uri_parse(Uri) -> http_uri:parse(Uri).",
           "-else.",
           "have_http_uri_parse() -> false.",
           "http_uri_parse(Uri) ->",
           "    case uri_string:parse(Uri) of",
           "        {error,_,_}=Error -> Error;",
           "        UriMap ->",
           "            Scheme = case maps:find(scheme, UriMap) of",
           "                         {ok, S} -> list_to_existing_atom(S);",
           "                         error -> ''",
           "                     end,",
           "            UserInfo = case maps:find(userinfo, UriMap) of",
           "                           {ok, U} -> U;",
           "                           error -> []",
           "                       end,",
           "            Host = case maps:find(host, UriMap) of",
           "                       {ok, H} -> H;",
           "                       error -> []",
           "                   end,",
           "            Port = case maps:find(port, UriMap) of",
           "                       {ok, Po} -> Po;",
           "                       error ->",
           "                           case Scheme of",
           "                               http -> 80;",
           "                               https -> 443;",
           "                               _ -> undefined",
           "                           end",
           "                   end,",
           "            Path = case maps:find(path, UriMap) of",
           "                       {ok, Pa} -> Pa;",
           "                       error -> []",
           "                   end,",
           "            Query = case maps:find(query, UriMap) of",
           "                        {ok, Q} -> [$?|Q];",
           "                        error -> []",
           "                    end,",
           "            {ok, {Scheme, UserInfo, Host, Port, Path, Query}}",
           "    end.",
           "-endif.",
           "-ifdef(HAVE_SAFE_RELATIVE_PATH).",
           "have_safe_relative_path() -> true.",
           "safe_relative_path(Filename, Cwd) ->",
           "    filelib:safe_relative_path(Filename, Cwd).",
           "-else.",
           "have_safe_relative_path() -> false.",
           "safe_relative_path(File, Cwd) ->",
           "    yaws:safe_rel_path(File, Cwd).",
           "-endif."
          ],
    string:join(Src, "\n").
