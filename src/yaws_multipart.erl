-module(yaws_multipart).
-export([read_multipart_form/2]).

-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").

-record(upload, {
          fd,
          filename,
          fixed_filename,
          last = false,
          param_name,
          param_running_value,
          params,
          running_file_size = 0,
          max_file_size,
          no_temp_file,
          temp_dir = yaws:tmpdir("/tmp"),
          temp_file
         }).

read_multipart_form(A, Options) when A#arg.state == undefined ->
    State     = #upload{params = dict:new()},
    NewState = read_options(Options,State),
    multipart(A, NewState);
read_multipart_form(A, _Options) ->
    multipart(A, A#arg.state).

read_options([], State) -> State;
read_options([Option|Rest], State) ->
    NewState = case Option of
                   {max_file_size, SizeInBytes} ->
                       State#upload{max_file_size = SizeInBytes};
                   no_temp_file ->
                       State#upload{no_temp_file = true};
                   {temp_file, FullPath} ->
                       State#upload{fixed_filename = FullPath};
                   {temp_dir, Dir} ->
                       true = filelib:is_dir(Dir),
                       State#upload{temp_dir = Dir}
               end,
    read_options(Rest, NewState).

multipart(A, State) ->
    Parse = yaws_api:parse_multipart_post(A),
    case Parse of
        {cont, Cont, Res} ->
            case addFileChunk(A, Res, State) of
                {done, NewState} ->
                    {done, NewState#upload.params};
                {cont, NewState} ->
                    {get_more, Cont, NewState};
                Error={error, _Reason} ->
                    Error
            end;
        {result, Res} ->
            case addFileChunk(A, Res, State#upload{last=true}) of
                {done, S2} ->
                    {done,S2#upload.params};
                Error={error, _Reason} ->
                    Error
            end
    end.


addFileChunk(A, [{part_body, Data}|Res], State) ->
    addFileChunk(A, [{body, Data}|Res], State);

addFileChunk(_A, [], State) when State#upload.last == true ->
    {done, close_previous_param(State)};

addFileChunk(_A, [], State) ->
    {cont, State};

addFileChunk(A, [{head, {_Name, Opts}}|Res], State ) ->
    S1 = close_previous_param(State),
    S2 = lists:foldl(
           fun({filename, Fname0}, RunningState) ->
                   case create_temp_file(State) of
                       [undefined, undefined] ->
                           %% values will be stored in memory as
                           %% dictated by state#upload.no_temp_file
                           RunningState#upload{
                             filename            = Fname0,
                             param_running_value = undefined,
                             running_file_size   = 0};
                       [Fname, Fd] ->
                           RunningState#upload{
                             fd                  = Fd,
                             filename            = Fname0,
                             temp_file           = Fname,
                             param_running_value = undefined,
                             running_file_size   = 0}
                   end;
              ({name, ParamName}, RunningState) ->
                   RunningState#upload{
                     param_name          = ParamName,
                     param_running_value = undefined}
           end,
           S1,
           Opts),
    addFileChunk(A,Res,S2);

addFileChunk(A, [{body, Data}|Res], State) when State#upload.fd /= undefined ->
    NewSize = compute_new_size(State,Data),
    Check    = check_param_size(State, NewSize),
    case Check of
        ok ->
            ok = file:write(State#upload.fd, Data),
            addFileChunk(A, Res, State#upload{running_file_size = NewSize});
        Error={error, _Reason} ->
            Error
    end;

addFileChunk(A, [{body, Data}|Res], State) ->
    NewSize = compute_new_size(State,Data),
    Check    = check_param_size(State, NewSize),
    case Check of
        ok ->
            NewState =
                case State#upload.param_running_value of
                    undefined ->
                        State#upload{param_running_value = Data};
                    PrevValue ->
                        NewData = compute_new_value(PrevValue, Data),
                        State#upload{param_running_value = NewData}
                end,
            addFileChunk(A, Res, NewState#upload{running_file_size = NewSize});
        Error={error, _Reason} ->
            Error
    end.

create_temp_file(State) ->
    case State#upload.no_temp_file of
        undefined ->
            FilePath =
                case State#upload.fixed_filename of
                    undefined ->
                        {A, B, C} = now(),
                        FileName = "yaws_"++ integer_to_list(A) ++
                            "_" ++ integer_to_list(B) ++
                            "_" ++ integer_to_list(C),
                        filename:join([State#upload.temp_dir, FileName]);
                    Filename ->
                        Filename
                end,
            {ok,Fd} = file:open(FilePath, [write,binary]),
            [FilePath, Fd];
        true ->
            [undefined, undefined]
    end
        .

close_previous_param(State = #upload{param_name = undefined}) ->
    State;
close_previous_param(State = #upload{param_name = ParamName}) ->
    S2 = case State#upload.filename of
             undefined ->
                 ParamValue = State#upload.param_running_value,
                 State#upload{
                   params = dict:store(ParamName, ParamValue,
                                       State#upload.params)};
             _ ->
                 ParamInfo = [{filename, State#upload.filename}],
                 ParamInfo2 = case State#upload.fd of
                                  undefined ->
                                      lists:append(
                                        ParamInfo,
                                        [{value,
                                          State#upload.param_running_value}]);
                                  Fd ->
                                      file:close(Fd),
                                      lists:append(ParamInfo,
                                                   [{temp_file,
                                                     State#upload.temp_file}])
                              end,
                 State#upload{
                   filename = undefined,
                   fd       = undefined,
                   temp_file= undefined,
                   running_file_size = 0,
                   params   = dict:store(ParamName, ParamInfo2,
                                         State#upload.params)
                  }
         end,
    S2#upload{param_name          = undefined,
              param_running_value = undefined}.

compute_new_size(State, Data) ->
    case Data of
        undefined ->
            State#upload.running_file_size;
        _ ->
            State#upload.running_file_size + iolist_size(Data)
    end.


check_param_size(State, NewSize) ->
    case State#upload.max_file_size of
        undefined -> ok;
        MaxSizeInBytes ->
            case NewSize > MaxSizeInBytes of
                true ->
                    {error, io_lib:format("~p is too large",
                                          [State#upload.param_name])};
                false ->
                    ok
            end
    end.

compute_new_value(PrevValue, NewData) ->
    case NewData of
        Data when is_binary(NewData) ->
            <<PrevValue/binary, Data/binary>>;
        Data when is_list(NewData) ->
            PrevValue ++ Data
    end.
