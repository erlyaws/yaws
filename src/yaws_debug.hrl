%%%----------------------------------------------------------------------
%%% File    : yaws_debug.hrl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose :
%%% Created :  7 Feb 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-author('klacke@hyber.org').



-define(F(Format,Args),
         lists:flatten(io_lib:format(Format,Args))).

-define(f(L), lists:flatten(L)).

-define(W(A), ?F("~w", [A])).

-define(format_record(Rec, Name),
        yaws_debug:format_record(Rec, Name, record_info(fields, Name))).



-define(Trace(What, Fmt, Args), if Trace == false ->
                                        ok;
                                   _ ->
                                        yaws_debug:dtrace(What,Fmt,Args)
                                end).



-ifdef(debug).


%% Possible Ops are, equal | neq | integer | list | {list, length, equal}
%% | greater | min | max | interval | {in, X , List}




-define(Dassert(X,Op,Y,Msg),
        yaws_debug:assert(Op,X,Y,{assert,?FILE,?LINE,Msg})).


-define(Dalert(X,Op,Y,Msg),
        yaws_debug:assert(Op,X,Y,{alert,?FILE,?LINE,Msg})).

-define(Deval(Expr),Expr).

-define(Debug(F, A),
        yaws_debug:assert([],0,0,{{debug,"DEBUG"}, ?FILE,?LINE,F, A})).


%% ease of use, just do ?Dvar(Variable)
-define(Dvar(Var), ?Debug("Var = ~p~n", [Var])).

-define(TC(L), yaws_debug:typecheck(L, ?FILE, ?LINE)).


-define(Derror(Fmt,Args),
        yaws_debug:assert([],0,0,{{debug,"ERROR"}, ?FILE,?LINE,Fmt,Args})).


-define(Dformat(Fmt,Args),
        yaws_debug:assert([],0,0,{format, ?FILE,?LINE,Fmt,Args})).


-define(Dfunassert(Fun, Msg),
        yaws_debug:assert('fun', Fun, 0, {assert,?FILE,?LINE,Msg})).

-else. %% not debug_mode

-define(DLOG(F, A), ?LOG(F, A)).
-define(Dassert(X,Op,Y,Msg),debug_disabled).
-define(Dalert(X,Op,Y,Msg),debug_disabled).
-define(Deval(Expr),debug_disabled).
-define(Debug(F, A),debug_disabled).
-define(Dvar(Var), debug_disabled).
-define(Dformat(Fmt,Args),debug_disabled).
-define(Dfunassert(Fun, Msg), debug_disabled).
-define(Derror(Fmt,Args),debug_disabled).
-define(TC(L), debug_disabled).

-endif. %% debug defined

