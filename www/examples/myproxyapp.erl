-module(myproxyapp).

-include_lib("yaws/include/yaws_api.hrl").

-export([out/1, get_counters/0]).

-define(TABLE, file_stats).

%% Return all counters sorted. the total always be the first one.
get_counters() ->
    create_table(),
    Counters = ets:tab2list(?TABLE),
    lists:sort(fun({_,N1}, {_,N2}) -> (N1 > N2) end, Counters).


%% Increment counter for the specified extension (or set it to 1 of
%% update_counter failed)
incr_counter(Ext) ->
    try
        ets:update_counter(?TABLE, Ext, 1)
    catch
        _:_ ->
            ets:insert(?TABLE, {Ext, 1}),
            1
    end.


%% Create the ETS table using a lock to be handle concurrency. It will be
%% attached to 'yaws_server' process to prevent its removal when the request
%% process will die.
do_create_table() ->
    global:trans({?TABLE, self()},
                 fun() ->
                         case ets:info(?TABLE) of
                             undefined ->
                                 Opts = [set, public, named_table,
                                         {heir, whereis(yaws_server), []}],
                                 ets:new(?TABLE, Opts);
                             _ ->
                                 ok
                         end
                 end, [node()], infinity).


%% Check if the table exists and create it if necessary
create_table() ->
    case ets:info(?TABLE, size) of
        undefined -> do_create_table();
        _         -> ok
    end.

out(A) ->
    create_table(),

    %% Retrieve the real path, using everything after "/myproxyapp" part
    Path = case A#arg.pathinfo of
               undefined -> "/";
               PI        -> PI
           end,

    %% Build the URL adding the query-string, if any
    Url = case A#arg.querydata of
              undefined -> Path;
              QD        -> Path ++ "?" ++ QD
          end,

    %% Get the file type corresponding to the Path by checking its file
    %% extension
    Ext = case filename:extension(Path) of
              ".yaws" -> yaws_script;
              ".html" -> html;
              ".png"  -> image;
              ".gif"  -> image;
              ".jpg"  -> image;
              ".css"  -> stylesheet;
              ".js"   -> javascript;
              _       -> other
          end,

    %% Update the counter corresponding to the extension and the total counter
    incr_counter(Ext),
    incr_counter(all),

    %% Override the default WEBROOT to be sure all link will be rewritten with
    %% the right prefix
    put("WEBROOT", "/myproxyapp"),

    %% Now, we can load the requested ressource
    {page, Url}.
