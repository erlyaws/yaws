-module(posttest).
-export([out/1]).

-include("../../include/yaws.hrl").
-include("../../include/yaws_api.hrl").

out(Arg) ->
    Url = yaws_api:request_url(Arg),
    case Url#url.path of
        "/posttest/chunked/" ++ ExpectedSize ->
            TE = yaws:to_lower((Arg#arg.headers)#headers.transfer_encoding),
            if
                TE =:= "chunked" ->
                    handle_post(list_to_integer(ExpectedSize), Arg);
                true ->
                    Reason = io_lib:format("Expected a chunked transfer-encoding request\n~p",
                                           [Arg#arg.headers]),
                    handle_post(0, Arg#arg{state={flush,500,Reason}})
            end;
        "/posttest/" ++ ExpectedSize ->
            handle_post(list_to_integer(ExpectedSize), Arg);
        _ ->
            Reason = "unknown path: " ++ Url#url.path,
            handle_post(0, Arg#arg{state={flush,500,Reason}})
    end.



handle_post(_, #arg{clidata=Data, state={flush, HttpCode, Reason}}) ->
    %% Catch an error here but flush all remaining data.
    case Data of
        {partial, _} -> {get_more, undefined, {flush, HttpCode, Reason}};
        _            -> [{status, HttpCode}, {html, Reason}]
    end;

handle_post(ExpectedSize, #arg{clidata=Data, cont=undefined}=Arg)
  when is_binary(Data) ->
    %% This is not a partial request
    %% Expected: content_length =:= ExpectedSize AND
    %%           content_length < partial_post_size.

    SC = get(sc),
    if
        size(Data) =:= ExpectedSize andalso
        size(Data) < SC#sconf.partial_post_size ->
            {status, 200};
        true ->
            Reason = io_lib:format("Post data too big. "
                                   "Received: ~p bytes - Max: ~p bytes",
                                   [size(Data), SC#sconf.partial_post_size]),
            handle_post(ExpectedSize, Arg#arg{state={flush,500,Reason}})
    end;
handle_post(ExpectedSize, #arg{clidata={partial, Data}, cont=Cont}=Arg)
  when is_binary(Data) ->
    %% next Chunk of a partial request.
    if
        Cont =:= undefined ->
            %% First chunk
            {get_more, {cont, size(Data)}, undefined};
        true ->
            {cont, Sz0} = Cont,
            Sz1 = Sz0 + size(Data),
            if
                Sz1 =< ExpectedSize ->
                    {get_more, {cont, Sz1}, undefined};
                true ->
                    SC = get(sc),
                    Reason = io_lib:format("Chunk too big. "
                                           "Received: ~p bytes - Max: ~p bytes",
                                           [size(Data),
                                            SC#sconf.partial_post_size]),
                    handle_post(ExpectedSize, Arg#arg{state={flush,500,Reason}})
            end
    end;
handle_post(ExpectedSize, #arg{clidata=Data, cont=Cont}=Arg)
  when is_binary(Data) ->
    %% Last chunk of a partial request.
    {cont, Sz0} = Cont,
    Sz1 = Sz0 + size(Data),
    if
        Sz1 =:= ExpectedSize ->
            {status, 200};
        true ->
            Reason = io_lib:format("Received data does not match "
                                   "the expected size. "
                                   "Received: ~p bytes - Expected: ~p bytes",
                                   [Sz1, ExpectedSize]),
            handle_post(ExpectedSize, Arg#arg{state={flush,500,Reason}})
    end.
