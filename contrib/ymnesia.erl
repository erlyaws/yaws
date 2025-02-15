%%%-------------------------------------------------------------------
%%% Created : 14 Oct 2005 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.   : A naive Mnesia table viewer Yaws-app interface.
%%%
%%% @author Torbjorn Tornkvist <tobbe@tornkvist.org>
%%%
%%% @doc Ymnesia is a Yaws appmod to view Mnesia tables.
%%%      Add <b>ymnesia</b> as an appmod to your Yaws configuration.
%%%      Point your browser to: &lt;url&gt;/ymnesia
%%%
%%%      <ul>
%%%      <li> You can search on arbitrary Erlang terms.
%%%           <br/>(<i>atoms need to be single-quoted</i>)</li>
%%%      <li> The checkbox control if the attribute should be shown in
%%%           the result. <br/>No checkbox means: <i>show all attributes</i>.</li>
%%%      </ul>
%%%
%%%      <p>
%%%      To test it, add it as an appmod to you Yaws configuration, e.g:
%%%
%%%          appmods = [{"showdb", ymnesia}}
%%%
%%%      then point your browser to:  http://&lt;host>/showdb/
%%%      </p>
%%% @end
%%%
%%%-------------------------------------------------------------------
-module(ymnesia).

-export([out/1]).

-include("../include/yaws_api.hrl").


-define(elog(X,Y), error_logger:info_msg("*elog ~p:~p: " X,
                                         [?MODULE, ?LINE | Y])).

%%% Possible to call mnesia in another way
-define(MNESIA(Mod, Args), apply(mnesia, Mod, Args)).
%%-define(MNESIA(Mod, Args), fdapi:call_in_master(mnesia, Mod, Args)).


%%% @private
out(A) ->
    %%?elog("Inside Ymnesia~n", []),
    case string:tokens(A#arg.appmoddata, "/.") of
        ["table" | _] ->
            case is_post(A) of
                true ->
                    L = yaws_api:parse_post(A),
                    {Cbox, Rest} = extract_cbox(L),
                    Name = lk("tablename", Rest),
                    Sp = ?MNESIA(table_info, [l2a(Name), wild_pattern]),
                    case catch table(Cbox, Sp, l2a(Name)) of
                        {'EXIT', Reason} ->
                            ?elog("Error , reason: ~p~n", [Reason]),
                            error_page("table not found: "++Name);
                        Else ->
                            Else
                    end;
                false ->
                    return_top_page()
            end;
        _ ->
            return_top_page()
    end.

return_top_page() ->
    {ehtml,
     [{head, [],
       [meta() ++
            style()]},
      {body, [],
       mk_table_tab()}]}.

is_post(A) ->
    case (A#arg.req)#http_request.method of
        'GET'  -> false;
        'POST' -> true
    end.

meta() ->
    [{pre_html,
      "<META HTTP-EQUIV=\"EXPIRES\" CONTENT=\""
      "Sun, 16 Oct 2004 11:12:01 GMT\">"}].

style() ->
    [{style, [{type, "text/css"}],
      [{pre_html,
        ["table {border-collapse: collapse; border: solid black 1px;}\n"
         "p {padding: 5px; font-weight: bold;}\n"
         "input[type=text] {vertical-align: bottom; width: 100%; font-size: 80%;}\n"
         "input[type=checkbox] {vertical-align: top; font-size: 80%;}\n"
         "span.attribute {vertical-align: top; font-size: 80%;}\n"
         "th {padding: 5px; border: solid black 1px;}\n"
         "td {padding: 5px; border: solid black 1px;}\n"
        ]}]}].


%%% Build the result page.
table(Cbox, Sp, Table) when is_atom(Table) ->
    case catch ?MNESIA(table_info, [Table, attributes]) of
        Headers when is_list(Headers) ->
            Vp = view_pattern(Cbox, lists:map(fun(X) -> a2l(X) end, Headers)),
            {Q, Result} = do_query(Sp),
            {ehtml,
             [{head, [],
               [meta() ++
                    style()]},
              {body, [],
               [{'div', [],
                 {p, [], "Query: "++Q}},
                {'div', [],
                 {p, [], "Table: "++a2l(Table)}} |
                mk_tab(Vp, Headers, t2l(Result))]}]};
        Else ->
            Else
    end.

%%% Create a pattern denoting which fields to show in the result.
view_pattern(Cs, L) -> view_pattern(Cs, L, 1).

view_pattern([Cbox|Cs], [Cbox|T], N) -> [N | view_pattern(Cs, T, N+1)];
view_pattern(Cs, [_|T], N)           -> view_pattern(Cs, T, N+1);
view_pattern([], [], _)              -> [].



%%% Create a table of: Table | Table-attribute-1 | ... | Table-attribute-N
%%% where each table is a Form
mk_table_tab() ->
    Rows = get_tables(),
    [{'div', [],
      [{table, [],
        lists:map(fun(Row) ->
                          {tr, [],
                           {form, [{action, "table.yaws"},
                                   {method, "post"},
                                   {name, Row}],
                            [{td, [], sublnk(a2l(Row))} |
                             mk_input_fields(Row)]}}
                  end, Rows)}]}].

%%% Create each table cell; consisting of the attribute name and an input field.
mk_input_fields(Table) ->
    As = get_attributes(Table),
    Max = max_noof_attrs(),
    lists:map(fun(0) ->
                      {td, [], []};
                 (Attribute) ->
                      A = a2l(Attribute),
                      {td, [],
                       [{input, [{type, "checkbox"}, {name, "cbox_"++A}]},
                        {span, [{class, "attribute"}], A},
                        {input, [{type, "text"}, {name, A}]}]}
              end, As ++ lists:duplicate(Max-length(As), 0)).

extract_cbox(L) ->
    extract_cbox(L, [], []).

extract_cbox([{"cbox_"++Cbox,_}|T], Cs, Rs) ->
    extract_cbox(T, [Cbox|Cs], Rs);
extract_cbox([H|T], Cs, Rs) ->
    extract_cbox(T, Cs, [H|Rs]);
extract_cbox([], Cs, Rs) ->
    {lists:reverse(Cs), lists:reverse(Rs)}.


%%% Build the result table.
mk_tab(Vp, Headers, Rows) ->
    [{'div', [],
      [{table, [],
        [{tr, [],
          [{th, [], a2l(X)} || X <- vp(Vp,Headers)]} |
         lists:map(fun(Row) ->
                           {tr, [],
                            [{td, [], massage(W)} || W <- vp(Vp,Row)]}
                   end, Rows)]}]}].

%%% Match the view pattern to select which entries to let through.
vp([], L) -> L;
vp(Vp, L) -> vp(Vp, L, 1).

vp([N|Vp], [H|T], N) -> [H|vp(Vp, T, N+1)];
vp(Vp, [_|T], N)     -> vp(Vp, T, N+1);
vp([], [], _)        -> [].

%%% Create a link that submit the form: onclick
sublnk(E) ->
    {a, [{href, "#"},
         {onclick, E++".submit();"}],
     [E,
      {input, [{type, "hidden"},
               {name, "tablename"},
               {value, E}]}]}.

massage(W) ->
    lists:flatten(io_lib:format("~p",[W])).

do_query(Sp) ->
    {lists:flatten(io_lib:format("mnesia:match_object(~p)", [Sp])),
     lists:keysort(2, ?MNESIA(dirty_match_object, [Sp]))}.

error_page(Msg) ->
    {html,
     [Msg]}.


get_tables() ->
    ?MNESIA(system_info, [tables]) -- [schema].

get_attributes(Table) ->
    ?MNESIA(table_info, [Table, attributes]).

max_noof_attrs() ->
    lists:foldl(fun(Table, Max) ->
                        erlang:max(length(get_attributes(Table)), Max)
                end, 0, get_tables()).


a2l(A) when is_atom(A) -> atom_to_list(A);
a2l(L) when is_list(L) -> L.

l2a(L) when is_list(L) -> list_to_atom(L);
l2a(A) when is_atom(A) -> A.

lk(Key, L) ->
    {value, {_,Val}} = lists:keysearch(Key, 1, L),
    Val.

t2l(L) ->
    lists:map(fun(T) -> tail(tuple_to_list(T)) end, L).

tail([]) -> [];
tail(L)  -> tl(L).

