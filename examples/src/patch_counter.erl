-module(patch_counter).
-behaviour(gen_server).

-export([out/1, init/1, handle_call/3, handle_cast/2]).

-include("yaws_api.hrl").

-define(PATCH_TYPE, "application/vnd.example.counter-patch").
-define(MAX_BODY, 4096).
-define(MAX_COUNT, 1000000).

out(#arg{req = #http_request{method = 'OPTIONS'}}) ->
    [{status, 200}, allow(), accept_patch()];
out(#arg{req = #http_request{method = Method}})
  when Method =:= 'GET'; Method =:= 'HEAD' ->
    Count = call(read),
    [{header, {etag, etag(Count)}},
     {content, "text/plain", [integer_to_list(Count), "\n"]}];
out(#arg{req = #http_request{method = 'PATCH'}, headers = Headers} = Arg) ->
    case supported_type(yaws_api:headers_content_type(Headers)) of
        true -> patch_body(Arg);
        false -> reject(Arg, [{status, 415}, accept_patch()])
    end;
out(Arg) ->
    reject(Arg, [{status, 405}, allow()]).

supported_type(undefined) -> false;
supported_type(Type) -> yaws:to_lower(string:trim(Type, both, " \t")) =:= ?PATCH_TYPE.

reject(#arg{clidata = {partial, _}}, Reply) ->
    [{header, {connection, "close"}} | Reply];
reject(_, Reply) -> Reply.

patch_body(#arg{clidata = {partial, Bytes}, cont = Cont} = Arg) ->
    case collect(Bytes, Cont) of
        too_large -> reject(Arg, [{status, 413}]);
        More -> {get_more, More, undefined}
    end;
patch_body(#arg{clidata = Bytes, cont = Cont, headers = Headers}) ->
    case collect(Bytes, Cont) of
        too_large -> {status, 413};
        {_, Chunks} ->
            case if_match(yaws_api:headers_if_match(Headers)) of
                missing -> [{status, 428}, {header, {cache_control, "no-store"}}];
                invalid -> {status, 400};
                Condition ->
                    Body = iolist_to_binary(lists:reverse(Chunks)),
                    case binary:split(Body, <<"\n">>, [global, trim_all]) of
                        [] -> {status, 400};
                        Lines -> call({patch, Condition, Lines})
                    end
            end
    end.

collect(Bytes, undefined) -> collect(Bytes, {0, []});
collect(Bytes, {Size, Chunks}) when Size + byte_size(Bytes) =< ?MAX_BODY ->
    {Size + byte_size(Bytes), [Bytes | Chunks]};
collect(_, _) -> too_large.

if_match(undefined) -> missing;
if_match(Header) ->
    case string:trim(Header, both, " \t") of
        "*" -> any;
        Value ->
            Tag = "(?:W/)?\"[!#-~\\x80-\\xFF]*\"",
            Field = ["[ \t]*(?:", Tag, ")?[ \t]*"],
            case re:run(Value, ["\\A", Field, "(?:,", Field, ")*\\z"], [{capture, none}]) of
                nomatch -> invalid;
                match ->
                    case re:run(Value, Tag, [global, {capture, first, list}]) of
                        {match, Tags} -> {tags, lists:append(Tags)};
                        nomatch -> {tags, []}
                    end
            end
    end.

matches(any, _) -> true;
matches({tags, Tags}, ETag) -> lists:member(ETag, Tags).

apply_patch([], Count) -> {ok, Count};
apply_patch([<<"set ", Value/binary>> | Rest], _) ->
    apply_value(Value, 0, Rest);
apply_patch([<<"add ", Value/binary>> | Rest], Count) ->
    apply_value(Value, Count, Rest);
apply_patch(_, _) -> {error, 400}.

apply_value(Value, Base, Rest) ->
    try binary_to_integer(Value) + Base of
        Count when Count >= 0, Count =< ?MAX_COUNT -> apply_patch(Rest, Count);
        _ -> {error, 422}
    catch
        error:badarg -> {error, 400}
    end.

allow() -> {header, {allow, "GET, HEAD, OPTIONS, PATCH"}}.

accept_patch() -> {header, {"Accept-Patch", ?PATCH_TYPE}}.

etag(Count) -> "\"" ++ integer_to_list(Count) ++ "\"".

call(Request) ->
    Pid = case whereis(?MODULE) of
              undefined ->
                  case gen_server:start({local, ?MODULE}, ?MODULE, [], []) of
                      {ok, Started} -> Started;
                      {error, {already_started, Started}} -> Started
                  end;
              Running -> Running
          end,
    gen_server:call(Pid, Request).

init([]) -> {ok, 0}.

handle_call(read, _From, Count) ->
    {reply, Count, Count};
handle_call({patch, Condition, Lines}, _From, Count) ->
    case matches(Condition, etag(Count)) of
        false -> {reply, {status, 412}, Count};
        true ->
            case apply_patch(Lines, Count) of
                {ok, NewCount} ->
                    {reply, [{status, 204}, {header, {etag, etag(NewCount)}}], NewCount};
                {error, Status} -> {reply, {status, Status}, Count}
            end
    end.

handle_cast(_, Count) -> {noreply, Count}.
