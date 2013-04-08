%%%----------------------------------------------------------------------
%%% File    : yaws_rss.erl
%%% Created : 15 Dec 2004 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%%
%%% @doc A Yaws RSS feed interface.
%%%
%%% @author  Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% @end
%%%
%%% $Id$
%%%----------------------------------------------------------------------
-module(yaws_rss).

-behaviour(gen_server).

%% External exports
-export([start/0, start_link/0, open/1, open/2, close/0, close/2,
         insert/5, insert/6, insert/7, retrieve/2]).

-export([t_setup/0, t_exp/0, t_xopen/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(s, {
          open_apps = [],    % activated applications
          expire = false,    % false | days
          rm_exp = false,    % remove expired items
          max=infinite,      % maximum number of elements in DB
          days=7,            % maximum number of days in DB
          counter}).         % item counter

-define(SERVER, ?MODULE).
-define(DB, ?MODULE).
-define(DB_FNAME, "yaws_rss.dets").
-define(ITEM(App, Tag, Counter, Item), {{App, Tag, Counter}, Item}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%%
%%% @spec open(App::atom()) ->
%%%         {ok, DB::db()} | {error, string()}
%%%
%%% @type db(). An opaque handle leading to an RSS database.
%%%
%%% @doc See {@link open/2}
%%% @end
open(App) ->
    open(App, []).

%%%
%%% @spec open(App::atom(), Opts::list()) ->
%%%         {ok, DB::db()} | {error, string()}
%%%
%%% @doc Open a RSS database.
%%%      Per default <em>dets</em> is used as database,
%%%      but by using the <em>db_mod</em> option it is
%%%      possible to use your own database.<br/>
%%%      These are the options:
%%%      <p><dl>
%%%
%%%      <dt>{db_mod, Module}</dt>
%%%      <dd>If specified, the following functions will be
%%%      called:<ul>
%%%      <li>Module:open(Opts)</li>
%%%      <li>Module:insert(App,Tag,Title,Link,Desc,Creator,GregSec)</li>
%%%      <li>Module:retrieve(App,Tag) -&gt; {Title, Link, Desc, Creator, GregSecs}</li>
%%%      <li>Module:close(DbName)</li></ul>
%%%      This means that the default DB won't be used, and
%%%      no expiration handling will be done. Only the producing of
%%%      XML will thus be done. Also, the whole <em>Opts</em> will be
%%%      passed un-interpreted to the other DB module.</dd>
%%%
%%%      <dt>{db_dir, Dir}</dt>
%%%      <dd>Specifies the directory where the database will be created.
%%%      Default is: /tmp</dd>
%%%
%%%      <dt>{expire, Expire}</dt>
%%%      <dd>Specifies what method to use to expire items. Possible values
%%%      are: <em>false</em>, <em>days</em>, meaning
%%%      never expire, expire after a number of days.
%%%      Default is to never expire items.</dd>
%%%
%%%      <dt>{days, Number}</dt>
%%%      <dd>Specifies the number of days befor an item is expired.
%%%      Default is 7 days.</dd>
%%%
%%%      <dt>{rm_exp, Bool}</dt>
%%%      <dd>Specifies if expired items should be removed from
%%%      the database. Default is to not remove any items.</dd>
%%%
%%%      <dt>{max, Number}</dt>
%%%      <dd>Specifies the maximum number of items that should
%%%      be stored in the database. The default in <em>infinite</em></dd>
%%%      </dl></p>
%%%      <p>If no database exist, a new one will be created.
%%%      The returned database handle is to be used with {@link close/1}.</p>
%%% @end
%%%
open(App, Opts) ->
    %% This is called during read of yaws.conf during startup, so make sure this
    %% server is up and running before invoking it
    ok = wait_for_server(?SERVER),
    gen_server:call(?SERVER, {open, App, Opts}, infinity).

%%%
%%% @spec close() -> ok | {error, string()}
%%%
%%% @doc Close the RSS database.
%%% @end
close() ->
    gen_server:call(?SERVER, {close, ?DB}, infinity).

%%%
%%% @spec close(DbMod::atom(), DbName::atom()) ->
%%%          ok | {error, string()}
%%%
%%% @doc Close the user provided RSS database.
%%%      A call to; <em>DbMod:close(DbName)</em> will be made.
%%% @end
close(DBmod, DBname) ->
    gen_server:call(?SERVER, {close, DBmod, DBname}, infinity).

%%%
%%% @spec insert(App::atom(), Tag::atom(), Title::string(),
%%%              Link::string(), Desc::string()) ->
%%%          ok | {error, string()}
%%%
%%% @doc Insert an RSS item into the <em>{App,Tag}</em> RSS feed.
%%%      An application (App) can maintain several feeds each
%%%      one refered to with a symbolic name (Tag).
%%%      <em>Link</em> should be a URL pointing to the item.
%%%      <p>In case another database backend is used, the
%%%      <em>Tag</em> has the format: <em>{DbModule, OpaqueTag}</em>
%%%      where <em>DbModule</em> is the database backend module
%%%      to be called, and <em>OpaqueTag</em> the Tag that is
%%%      used in <em>DbModule:insert(Tag, ...)</em></p>
%%% @end
%%%
insert(App, Tag, Title, Link, Desc) ->
    insert(App, Tag, Title, Link, Desc, "").

%%%
%%% @spec insert(App::atom(), Tag::atom(), Title::string(),
%%%              Link::string(), Desc::string(),
%%%              Creator::string()) ->
%%%          ok | {error, string()}
%%%
%%% @doc Works as {@link insert/5} but takes an extra argument
%%%      <em>Creator</em> which may contains an identification
%%%      of who created the item.
%%% @end
insert(App, Tag, Title, Link, Desc, Creator) ->
    GregSecs = calendar:datetime_to_gregorian_seconds({date(),time()}),
    insert(App, Tag, Title, Link, Desc, Creator, GregSecs).

%%%
%%% @spec insert(App::atom(), Tag::atom(), Title::string(),
%%%              Link::string(), Desc::string(),
%%%              Creator::string(), GregSecs::integer()) ->
%%%          ok | {error, string()}
%%%
%%% @doc Works as {@link insert/6} but takes an extra argument
%%%      <em>GregSecs</em> which is the creation time of the item
%%%      in Gregorian Seconds.
%%% @end
insert(App, Tag, Title, Link, Desc, Creator, GregSecs) ->
    Args = {App, Tag, Title, Link, Desc, Creator, GregSecs},
    gen_server:call(?SERVER, {insert, Args}, infinity).


%%%
%%% @spec retrieve(App::atom(), Tag::atom()) ->
%%%           {ok, RSSContent::iolist()} |{error, string()}
%%%
%%% @type ioList().  A deep list of strings and/or binaries.
%%%
%%% @doc Retrieve the <em>RSScontent</em> (in XML and all...)
%%%      to be delivered to a RSS client.
%%%      <p>In case another database backend is used, the
%%%      <em>Tag</em> has the format: <em>{DbModule, OpaqueTag}</em>
%%%      where <em>DbModule</em> is the database backend module
%%%      to be called, and <em>OpaqueTag</em> the Tag that is
%%%      used in <em>DbModule:retrieve(Tag)</em> which must return
%%%      a list of tuples: <em>{Title, Link, Desc, Creator, GregSecs}</em></p>
%%% @end
retrieve(App, Tag) ->
    gen_server:call(?SERVER, {retrieve, App, Tag}, infinity).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    {ok, #s{}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({open, App, Opts}, _From, State) ->
    {NewState, Res} = do_open_dir(State, App, Opts),
    {reply, Res, NewState};
%%
handle_call({close, DB}, _From, State) ->
    dets:close(DB),
    {reply, ok, State};
%%
handle_call({close, DBMod, DBname}, _From, State) ->
    catch apply(DBMod, close, [DBname]),
    {reply, ok, State};
%%
handle_call({insert, Args}, _From, State) ->
    {NewState, Res} = do_insert(State, Args),
    {reply, Res, NewState};
%%
handle_call({retrieve, App, Tag}, _From, State) ->
    {NewState, Res} = do_retrieve(State, App, Tag),
    {reply, Res, NewState}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Handle upgrade
%% Returns: new State data
%%----------------------------------------------------------------------
code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%%
%%% Check what database store that should be used.
%%% Per default 'dets' is used.
%%%
do_open_dir(State, App, Opts) ->
    case get_db_mod(Opts, dets) of
        dets ->
            File = get_db_file(Opts),
            Expire = get_expire(Opts, #s.expire),
            Max = get_max(Opts, #s.max),
            Days = get_days(Opts, #s.days),
            RmExp = get_rm_exp(Opts, #s.rm_exp),
            case dets:is_dets_file(File) of
                false ->
                    {State, {error, "not a proper dets file"}};
                _     ->
                    case catch dets:open_file(?DB, [{file, File}]) of
                        {ok,DB} = Res   ->
                            {State#s{
                               open_apps = u_insert(App, State#s.open_apps),
                               expire = Expire,
                               days = Days,
                               rm_exp = RmExp,
                               max = Max,
                               counter = init_counter(DB)},
                             Res};
                        {error, _Reason} ->
                            {State, {error, "open dets file"}}
                    end
            end;
        DBmod ->
            {State, catch apply(DBmod, open, Opts)}
    end.

get_db_file(Opts) ->
    Dir = get_db_dir(Opts, "/tmp"),
    Dir ++ "/" ++ a2l(?DB) ++ ".dets".

init_counter(DB) ->
    case dets:lookup(DB, counter) of
        []            -> dets:insert(DB, {counter, 0}), 0;
        [{counter,N}] -> N
    end.

set_counter(DB, N) ->
    dets:insert(DB, {counter, N}).

do_insert(State, {App, {DbMod,Tag}, Title, Link, Desc, Creator, GregSecs}) ->
    {State, catch apply(DbMod, insert, [App, Tag,Title,Link,
                                        Desc,Creator,GregSecs])};
do_insert(State, {App, Tag, Title, Link, Desc, Creator, GregSecs}) ->
    case lists:member(App, State#s.open_apps) of
        true ->
            Counter = if (State#s.max > 0) ->
                              (State#s.counter + 1) rem State#s.max;
                         true ->
                              State#s.counter + 1
                      end,
            Item = {Title, Link, Desc, Creator, GregSecs},
            Res = dets:insert(?DB, ?ITEM(App, Tag, Counter, Item)),
            set_counter(?DB, Counter),
            {State#s{counter = Counter}, Res};
        false ->
            {State, {error, "no open DB"}}
    end.


do_retrieve(State, App, {DbMod,Tag}) ->
    {State, catch apply(DbMod, retrieve, [App, Tag])};
do_retrieve(State, App, Tag) ->
    case lists:member(App, State#s.open_apps) of
        true ->
            F = fun(?ITEM(Xa, Xt, _Counter, Item), Acc)
                      when Xa == App, Xt == Tag ->
                        [Item|Acc];
                   (_, Acc) ->
                        Acc
                end,
            Items = sort_items(expired(State, dets:foldl(F, [], ?DB))),
            Xml = to_xml(Items),
            {State, {ok, Xml}};
        false ->
            {State, {error, "no open DB"}}
    end.



-define(ONE_DAY, 86400).  % 24*60*60 seconds
-define(X(GregSecs), {Title, Link, Desc, Creator, GregSecs}).

%%% Filter away expired items !!
expired(State, List) when State#s.expire == days ->
    Gs = calendar:datetime_to_gregorian_seconds({date(),time()}),
    Old = Gs - (?ONE_DAY * State#s.days),
    F = fun(?X(GregSecs), Acc) when GregSecs > Old ->
                [?X(GregSecs) | Acc];
           (_, Acc) ->
                Acc
        end,
    lists:foldl(F, [], List);
expired(_State, List) ->
    List.

-undef(X).



%%%
%%% Sort on creation date !!
%%% Item = {Title, Link, Desc, Creator, GregSecs},
%%%
sort_items(Is) ->
    lists:keysort(5,Is).


to_xml([{Title, Link, Desc, Creator, GregSecs}|Tail]) ->
    Date = w3cdtf(GregSecs),
    [["<item>\n",
      "<title>", yaws_api:htmlize(Title), "</title>\n",
      "<link>", Link, "</link>\n",
      "<guid>", Link, "</guid>\n",
      "<description>", yaws_api:htmlize(Desc), "</description>\n",
      "<dc:creator>", Creator, "</dc:creator>\n",
      "<dc:date>", Date, "</dc:date>\n",
      "</item>\n"] |
     to_xml(Tail)];
to_xml([]) ->
    [].

%%%
%%% Create W3CDTF (http://www.w3.org/TR/NOTE-datetime) formatted date
%%% w3cdtf(GregSecs) -> "YYYY-MM-DDThh:mm:ssTZD"
%%%
w3cdtf(GregSecs) ->    Date = calendar:gregorian_seconds_to_datetime(GregSecs),
                       {{Y, Mo, D},{H, Mi, S}} = Date,
                       [UDate|_] = calendar:local_time_to_universal_time_dst(
                                     Date),
                       {DiffD,{DiffH,DiffMi,_}}=calendar:time_difference(
                                                  UDate,Date),
                       w3cdtf_diff(Y, Mo, D, H, Mi, S, DiffD, DiffH, DiffMi).

%%%  w3cdtf's helper function
w3cdtf_diff(Y, Mo, D, H, Mi, S, _DiffD, DiffH, DiffMi)
  when DiffH < 12,  DiffH /= 0 ->
    i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "+" ++ add_zero(DiffH) ++ ":"  ++ add_zero(DiffMi);

w3cdtf_diff(Y, Mo, D, H, Mi, S, DiffD, DiffH, DiffMi)
  when DiffH > 12,  DiffD == 0 ->
    i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "+" ++ add_zero(DiffH) ++ ":"  ++
        add_zero(DiffMi);

w3cdtf_diff(Y, Mo, D, H, Mi, S, DiffD, DiffH, DiffMi)
  when DiffH > 12,  DiffD /= 0, DiffMi /= 0 ->
    i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "-" ++ add_zero(23-DiffH) ++
        ":" ++ add_zero(60-DiffMi);

w3cdtf_diff(Y, Mo, D, H, Mi, S, DiffD, DiffH, DiffMi)
  when DiffH > 12,  DiffD /= 0, DiffMi == 0 ->
    i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "-" ++ add_zero(24-DiffH) ++
        ":" ++ add_zero(DiffMi);

w3cdtf_diff(Y, Mo, D, H, Mi, S, _DiffD, DiffH, _DiffMi) when DiffH == 0 ->
    i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "Z".

add_zero(I) when is_integer(I) -> add_zero(i2l(I));
add_zero([A])               -> [$0,A];
add_zero(L) when is_list(L)    -> L.



get_db_mod(Opts, Def)  -> lkup(db_mod, Opts, Def).
get_db_dir(Opts, Def)  -> lkup(db_dir, Opts, Def).
get_expire(Opts, Def)  -> lkup(expire, Opts, Def).
get_max(Opts, Def)     -> lkup(max, Opts, Def).
get_days(Opts, Def)    -> lkup(days, Opts, Def).
get_rm_exp(Opts, Def ) -> lkup(rm_exp, Opts, Def).

lkup(Key, List, Def) ->
    case lists:keysearch(Key, 1, List) of
        {value,{_,Value}} -> Value;
        _                 -> Def
    end.


u_insert(H, [H|T]) -> T;
u_insert(E, [H|T]) -> [H|u_insert(E,T)];
u_insert(E, [])    -> [E].


i2l(I) when is_integer(I) -> integer_to_list(I).

a2l(A) when is_atom(A) -> atom_to_list(A).




t_setup() ->
    %%open([{db_file, "yaws_rss.dets"}, {max,7}]),
    insert(test,xml,"Normalizing XML, Part 2",
           "http://www.xml.com/pub/a/2002/12/04/normalizing.html",
           "In this second and final look at applying relational "
           "normalization techniques to W3C XML Schema data modeling, "
           "Will Provost discusses when not to normalize, the scope "
           "of uniqueness and the fourth and fifth normal forms."),
    insert(test,xml,"The .NET Schema Object Model",
           "http://www.xml.com/pub/a/2002/12/04/som.html",
           "Priya Lakshminarayanan describes in detail the use of "
           "the .NET Schema Object Model for programmatic manipulation "
           "of W3C XML Schemas."),
    insert(test,xml,"SVG's Past and Promising Future",
           "http://www.xml.com/pub/a/2002/12/04/svg.html",
           "In this month's SVG column, Antoine Quint looks back at "
           "SVG's journey through 2002 and looks forward to 2003.").


t_exp() ->
    %%open([{db_file, "yaws_rss.dets"}, {expire,days}]),
    insert(test,xml,"Expired article",
           "http://www.xml.com/pub/a/2002/12/04/normalizing.html",
           "In this second and final look at applying relational "
           "normalization techniques to W3C XML Schema data modeling, "
           "Will Provost discusses when not to normalize, the scope "
           "of uniqueness and the fourth and fifth normal forms.",
           "tobbe",
           63269561882).  % 6/12-2004

t_xopen() ->
    open([{db_file, "yaws_rss.dets"},
          {expire,days},
          {days, 20}]).

wait_for_server(Server) ->
    wait_for_server(Server, 20).

wait_for_server(_Server, 0) ->
    {error, timeout};
wait_for_server(Server, N) ->
    case erlang:whereis(Server) of
        undefined ->
            receive after 500 -> ok end,
            wait_for_server(Server, N-1);
        _ ->
            ok
    end.
