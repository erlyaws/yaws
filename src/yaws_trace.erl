-module(yaws_trace).
-author('christopher@yakaz.com').

-behaviour(gen_server).

-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").

%% API
-export([
         start_link/0,
         setup/1,
         set_tty_trace/1,
         get_type/1,
         open/0, open/1,
         write/2, write/3,
         disable_trace/1, enable_trace/2,
         get_tracedir/0,
         set_filter/1, unset_filter/0, get_filter/0,

         %% Internal functions exported to be used in filters
         get_header_value/2,
         get_request_value/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {tracedir,
                type      = false,
                traces    = dict:new(),
                tty_trace = false,
                filter}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


setup(GC) ->
    gen_server:cast(?MODULE, {setup, GC}).


set_tty_trace(Bool) ->
    gen_server:cast(?MODULE, {tty_trace, Bool}).


get_type(#gconf{trace={true, Type}}) ->
    Type;
get_type(_) ->
    undefined.



open() ->
    open(self()).
open(Owner) ->
    gen_server:cast(?MODULE, {open, Owner}).


write(Tag, Data) ->
    write(self(), Tag, Data).
write(Pid, Tag, Data) ->
    gen_server:cast(?MODULE, {write, Pid, Tag, Data}).


disable_trace(infinity) ->
    {ok, GC, Groups} = yaws_server:getconf(),
    case GC#gconf.trace of
        false ->
            error_logger:info_msg("Trace already disabled~n", []);
        {true, _} ->
            yaws_api:setconf(GC#gconf{trace=false}, Groups),
            error_logger:info_msg("Trace disabled~n", [])
    end;
disable_trace(Time) ->
    {ok, GC, Groups} = yaws_server:getconf(),
    case GC#gconf.trace of
        false ->
            error_logger:info_msg("Trace already disabled~n", []);
        {true, CurType} ->
            yaws_api:setconf(GC#gconf{trace=false}, Groups),
            timer:apply_after(Time, ?MODULE, enable_trace, [CurType, infinity]),
            error_logger:info_msg("Trace disabled for ~w milliseconds~n",[Time])
    end.


enable_trace(Type, infinity) when Type =:= http; Type =:= traffic ->
    {ok, GC, Groups} = yaws_server:getconf(),
    case GC#gconf.trace of
        {true, Type} ->
            error_logger:info_msg("~p trace already enabled~n", [Type]);
        {true, _} ->
            yaws_api:setconf(GC#gconf{trace={true, Type}}, Groups),
            error_logger:info_msg("~p trace enabled~n", [Type]);
        false ->
            yaws_api:setconf(GC#gconf{trace={true, Type}}, Groups),
            error_logger:info_msg("~p trace enabled~n", [Type])
    end;
enable_trace(Type, Time) when Type =:= http; Type =:= traffic ->
    {ok, GC, Groups} = yaws_server:getconf(),
    case GC#gconf.trace of
        {true, Type} ->
            error_logger:info_msg("~p trace already enabled~n", [Type]);
        {true, CurType} ->
            yaws_api:setconf(GC#gconf{trace={true, Type}}, Groups),
            timer:apply_after(Time, ?MODULE, enable_trace, [CurType, infinity]),
            error_logger:info_msg("~p trace enabled for ~w milliseconds~n",
                                  [Type, Time]);
        false ->
            yaws_api:setconf(GC#gconf{trace={true, Type}}, Groups),
            timer:apply_after(Time, ?MODULE, disable_trace, [infinity]),
            error_logger:info_msg("~p trace enabled for ~w milliseconds~n",
                                  [Type, Time])
    end.

get_tracedir() ->
    gen_server:call(?MODULE, get_tracedir, infinity).

set_filter(Filter) ->
    Expr = filter2expr(Filter),
    Fun  = lists:flatten([
                          "fun(Ip, Req, Headers) ->\n",
                          "  try\n"
                          "    (", Expr, ")\n",
                          "  catch\n",
                          "    _:_ -> false\n"
                          "  end\n"
                          " end."
                         ]),
    Tokens = case erl_scan:string(Fun) of
                 {ok, Toks, _} -> Toks;
                 _             -> throw({error, {invalid_filter, scan_failed}})
             end,
    ExprList = case erl_parse:parse_exprs(Tokens) of
                   {ok, E} -> E;
                   _       -> throw({error, {invalid_filter, parse_failed}})
               end,
    try erl_eval:exprs(ExprList, []) of
        {value, Result, _} ->
            gen_server:call(?MODULE, {set_filter, Result}, infinity)
    catch
        _:_ ->
            throw({error, {invalid_filter, eval_failed}})
    end.

unset_filter() ->
    gen_server:call(?MODULE, unset_filter, infinity).

get_filter() ->
    gen_server:call(?MODULE, get_filter, infinity).


%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.


%% ----
handle_call({set_filter, Fun}, _From, State) ->
    {reply, ok, State#state{filter=Fun}};
handle_call(unset_filter, _From, State) ->
    {reply, ok, State#state{filter=undefined}};
handle_call(get_filter, _From, State) ->
    {reply, State#state.filter, State};
handle_call(get_tracedir, _From, State) ->
    {reply, State#state.tracedir, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% ----
handle_cast({setup, #gconf{trace=false}=GC}, State) ->
    Bool = ?gc_has_tty_trace(GC),
    {noreply, State#state{type=false, tty_trace=Bool, tracedir=undefined}};
handle_cast({setup, GC}, State) ->
    {true, Type} = GC#gconf.trace,
    Bool = ?gc_has_tty_trace(GC),
    Dir  = filename:join(GC#gconf.logdir, subdir()),
    case file:make_dir(Dir) of
        ok ->
            error_logger:info_msg("Trace directory ~p created~n", [Dir]),
            {noreply, State#state{type=Type, tty_trace=Bool, tracedir=Dir}};
        {error, Reason} ->
            error_logger:error_msg("Failed to create trace directory ~p: ~p~n",
                                   [Dir, file:format_error(Reason)]),
            {noreply, State#state{type=false, tty_trace=Bool}}
    end;

handle_cast({tty_trace, Bool}, State) ->
    {noreply, State#state{tty_trace=Bool}};

handle_cast({open, _}, #state{type=false}=State) ->
    {noreply, State};
handle_cast({open, Owner}, State) ->
    Type = State#state.type,
    case dict:find(Owner, State#state.traces) of
        {ok, {Type, _Fd}} ->
            {noreply, State};
        {ok, {_OtherType, OldFd}} ->
            file:close(OldFd),
            case open_trace(State#state.tracedir, filename(Type, Owner)) of
                {ok, NewFd} ->
                    erlang:monitor(process, Owner),
                    D = dict:store(Owner, {Type,NewFd}, State#state.traces),
                    {noreply, State#state{traces=D}};
                error ->
                    D = dict:erase(Owner, State#state.traces),
                    {noreply, State#state{traces=D}}
            end;
        error ->
            case open_trace(State#state.tracedir, filename(Type, Owner)) of
                {ok, NewFd} ->
                    erlang:monitor(process, Owner),
                    D = dict:store(Owner, {Type,NewFd}, State#state.traces),
                    {noreply, State#state{traces=D}};
                error ->
                    {noreply, State}
            end
    end;

handle_cast({write, _, _, _}, #state{type=false}=State) ->
    {noreply, State};
handle_cast({write, Pid, Tag, Data}, State) ->
    TTYTrace = State#state.tty_trace,
    case dict:find(Pid, State#state.traces) of
        {ok, {_Type, Fd}} -> write_trace(Pid, Fd, Tag, Data, TTYTrace);
        error             -> ok
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


%% ----
handle_info({'DOWN', _MRef, _Type, Pid, _Info}, State) ->
    case dict:find(Pid, State#state.traces) of
        {ok, {_, Fd}} ->
            file:close(Fd),
            D = dict:erase(Pid, State#state.traces),
            {noreply, State#state{traces=D}};
        error ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.


%% ----
terminate(_Reason, State) ->
    [file:close(Fd) || {_Owner, Fd} <- dict:to_list(State#state.traces)],
    ok.


%% ----
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================
subdir() ->
    {{Y,M,D}, {HH,MM,SS}} = erlang:localtime(),
    lists:flatten(
      io_lib:format("trace_~4.10.0B~2.10.0B~2.10.0B_~2.10.0B~2.10.0B~2.10.0B",
                    [Y,M,D,HH,MM,SS])
     ).

filename(Type, Owner) ->
    lists:concat(["trace.", pid_to_list(Owner), ".", Type]).


open_trace(Dir, File) ->
    case file:open(filename:join([Dir, File]), [write, raw]) of
        {ok, Fd} ->
            Date = iso_8601_fmt(),
            Str  = ["=====\n===== TRACE STARTED ", Date, "\n=====\n\n"],
            file:write(Fd, Str),
            {ok, Fd};
        {error, Reason} ->
            error_logger:error_msg("Failed to open trace ~p: ~p~n",
                                   [File, file:format_error(Reason)]),
            error
    end.


write_trace(Pid, Fd, from_server, Data, TTYTrace) ->
    Date = iso_8601_fmt(),
    Head = io_lib:format("~n[~s] ===== SRV -> CLI =====~n", [Date]),
    write_trace(Pid, Fd, [Head, Data], TTYTrace);
write_trace(Pid, Fd, from_client, Data, TTYTrace) ->
    Date = iso_8601_fmt(),
    Head = io_lib:format("~n[~s] ===== CLI -> SRV =====~n", [Date]),
    write_trace(Pid, Fd, [Head, Data], TTYTrace);
write_trace(Pid, Fd, _, Data, TTYTrace) ->
    Date = iso_8601_fmt(),
    Head = io_lib:format("~n[~s]~n", [Date]),
    write_trace(Pid, Fd, [Head, Data], TTYTrace).


write_trace(Pid, Fd, Str, true) ->
    file:write(Fd, Str),
    io:format("Worker: ~p ~s~n", [Pid, flatten([Str])]);
write_trace(_Pid, Fd, Str, false) ->
    file:write(Fd, Str).


iso_8601_fmt() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
    {_,_,MicroSec} = yaws:get_time_tuple(),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B.~w",
                  [Year, Month, Day, Hour, Min, Sec, (MicroSec div 1000)]).


flatten(L) ->
    binary_to_list(iolist_to_binary(L)).



%%====================================================================
filter2expr({'and', Tests}) ->
    L = [lists:flatten(["(", filter2expr(T), ")"]) || T <- Tests],
    string:join(L, " andalso ");
filter2expr({'or', Tests}) ->
    L = [lists:flatten(["(", filter2expr(T), ")"]) || T <- Tests],
    string:join(L, " orelse ");
filter2expr({'and', T1, T2}) ->
    Expr1 = lists:flatten(["(", filter2expr(T1), ")"]),
    Expr2 = lists:flatten(["(", filter2expr(T2), ")"]),
    lists:flatten([Expr1, " andalso ", Expr2]);
filter2expr({'or', T1, T2}) ->
    Expr1 = lists:flatten(["(", filter2expr(T1), ")"]),
    Expr2 = lists:flatten(["(", filter2expr(T2), ")"]),
    lists:flatten([Expr1, " orelse ", Expr2]);
filter2expr({'not', T}) ->
    Expr = lists:flatten(["(", filter2expr(T), ")"]),
    lists:flatten(["not ", Expr]);


filter2expr({'equal', Elmt, Value}) ->
    Expr = [element_to_string(Elmt), " =:= ", value_to_string(Value)],
    lists:flatten(Expr);
filter2expr({'startby', Elmt, Prefix}) ->
    Expr = ["lists:prefix(", value_to_string(Prefix), ",",
            element_to_string(Elmt), ")"],
    lists:flatten(Expr);
filter2expr({'endwith', Elmt, Suffix}) ->
    Expr = ["lists:suffix(", value_to_string(Suffix), ",",
            element_to_string(Elmt), ")"],
    lists:flatten(Expr);
filter2expr({'contain', Elmt, Part}) ->
    Expr = ["string:str(", element_to_string(Elmt), ",",
            value_to_string(Part), ") =/= 0"],
    lists:flatten(Expr);
filter2expr({'match', Elmt, Re}) ->
    Expr = ["re:run(", element_to_string(Elmt), ",", value_to_string(Re),
            ", [{capture,none}]) =:= match"],
    lists:flatten(Expr);

filter2expr(F) ->
    throw({error, {unknown_filter, F}}).



value_to_string(Value) ->
    io_lib:format("~p", [Value]).

element_to_string(ip) ->
    "Ip";
element_to_string({header, H}) ->
    lists:concat(["yaws_trace:get_header_value(", H, ", Headers)"]);
element_to_string({request, R}) when R =:= method;  R =:= path; R =:= version ->
    lists:concat(["yaws_trace:get_request_value(", R, ", Req)"]);
element_to_string(E) ->
    throw({error, {unknown_element, E}}).


get_header_value(connection, Headers) ->
    case yaws_api:headers_connection(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(accept, Headers) ->
    case yaws_api:headers_accept(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(host, Headers) ->
    case yaws_api:headers_host(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(if_modified_since, Headers) ->
    case yaws_api:headers_if_modified_since(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(if_match, Headers) ->
    case yaws_api:headers_if_match(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(if_none_match, Headers) ->
    case yaws_api:headers_if_none_match(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(if_range, Headers) ->
    case yaws_api:headers_if_range(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(if_unmodified_since, Headers) ->
    case yaws_api:headers_if_unmodified_since(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(range, Headers) ->
    case yaws_api:headers_range(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(referer, Headers) ->
    case yaws_api:headers_referer(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(user_agent, Headers) ->
    case yaws_api:headers_user_agent(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(accept_ranges, Headers) ->
    case yaws_api:headers_accept_ranges(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(cookie, Headers) ->
    lists:flatten(yaws_api:headers_cookie(Headers));
get_header_value(keep_alive, Headers) ->
    case yaws_api:headers_keep_alive(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(location, Headers) ->
    case yaws_api:headers_location(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(content_length, Headers) ->
    case yaws_api:headers_content_length(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(content_type, Headers) ->
    case yaws_api:headers_content_length(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(content_encoding, Headers) ->
    case yaws_api:headers_content_encoding(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(authorization, Headers) ->
    case yaws_api:headers_authorization(Headers) of
        undefined                    -> [];
        {undefined, undefined, Orig} -> lists:flatten(["::",Orig]);
        {User, Pass, Orig}           -> lists:flatten([User,":",Pass,":",Orig])
    end;
get_header_value(transfer_encoding, Headers) ->
    case yaws_api:headers_transfer_encoding(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(x_forwarded_for, Headers) ->
    case yaws_api:headers_x_forwarded_for(Headers) of
        undefined -> [];
        Value     -> Value
    end;
get_header_value(Name, Headers) when is_atom(Name) ->
    Others = yaws_api:headers_other(Headers),
    case lists:keyfind(Name, 3, Others) of
        {http_header,_,_,_,Value} ->
            Value;
        false ->
            get_header_value(atom_to_list(Name), Headers)
    end;
get_header_value(Name, Headers) ->
    Others = yaws_api:headers_other(Headers),
    case lists:keyfind(Name, 3, Others) of
        {http_header,_,_,_,Value} ->
            Value;
        false ->
            []
    end.


get_request_value(method, Req) ->
    yaws_api:http_request_method(Req);
get_request_value(path, Req) ->
    case yaws_api:http_request_path(Req) of
        {abs_path, Path} -> Path;
        _                -> []
    end;
get_request_value(version, Req) ->
    case yaws_api:http_request_version(Req) of
        {Maj, Min} -> lists:concat(["HTTP/", Maj, ".", Min]);
        _          -> "HTTP/0.9"
    end.
