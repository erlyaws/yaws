-module(phptest).
-export([out/1]).

-include("yaws.hrl").
-include("yaws_api.hrl").

out(Arg) ->
    File = Arg#arg.fullpath,
    case filelib:is_file(File) of
        true ->
            case file:read_file(File) of
                {ok, Content} ->
                    [{status, 200}, {content, "text/plain", Content}];
                {error, Reason} ->
                    [{status, 500}, {html, file:format_error(Reason)}]
            end;
        false ->
            Reason = "file not found: " ++ File,
            [{status, 500}, {html, Reason}]
    end.
