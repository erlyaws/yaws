%%
%% See '8.5 Benchmarking' under 'Efficiency Guide' in the OTP documentation.
%%
%
% This particular benchmark was assembled to learn about Erlang benchmarking on a simple case and to 
% satisfy my curiosity as to why the yaws_server:conc_path/1 function was used instead of one of the 
% 'lists' functions such as lists:append, and why conc_path isn't tail-recursive.
%
% Note that I'm not suggesting that conc_path is necessarily worth of particular attention regarding performance!
% (though it  may be.. )
% Applying this sort of benchmark to such small parts of Yaws at this stage (v 1.68) could quite reasonably be classed
% as 'premature optimization' I think ;)
% Proper profiling would need to be done to determine what code paths are actually performance sensitive and worthy
% of attention.
%


-module(conc_path_bm).
-author('julian@precisium.com.au').

-include("bench.hrl").

-include("../../include/yaws_api.hrl").
-include("../yaws_debug.hrl").
-include("../../include/yaws.hrl").


-export([benchmarks/0]).
-export([conc_path_orig/1,conc_path_tweaked/1, conc_path_tail/1, conc_path_lappend/1, conc_path_lflatten/1, conc_path_lconcat/1]).

benchmarks() ->
 {20000, [conc_path_orig, conc_path_tweaked, conc_path_tail, conc_path_lappend, conc_path_lflatten, conc_path_lconcat]}.



%--------------------------------------
% original conc_path from yaws_server.erl
%--------------------------------------
conc_path_orig(Iter) ->
        do_conc_path_orig(Iter, getData()).

do_conc_path_orig(0, _Path) -> ok;
do_conc_path_orig(Iter, Path) ->

        ?rep20(conc_path(Path)),

        do_conc_path_orig(Iter-1, Path).


%--------------------------------------
% slightly tweaked conc_path from yaws_server.erl
%--------------------------------------
conc_path_tweaked(Iter) ->
        do_conc_path_tweaked(Iter, getData()).

do_conc_path_tweaked(0, _Path) -> ok;
do_conc_path_tweaked(Iter, Path) ->

        ?rep20(conc_path2(Path)),

        do_conc_path_tweaked(Iter-1, Path).

%--------------------------------------
% tail recursive version of conc_path
%--------------------------------------
conc_path_tail(Iter) ->
        do_conc_path_tail(Iter, getData()).

do_conc_path_tail(0, _Path) -> ok;
do_conc_path_tail(Iter, Path) ->

        ?rep20(conc_path_tailrecursive(Path)),

        do_conc_path_tail(Iter-1, Path).


%--------------------------------------
% lists:flatten 
%--------------------------------------
conc_path_lflatten(Iter) ->
        do_conc_path_lflatten(Iter, getData()).

do_conc_path_lflatten(0, _Path) -> ok;
do_conc_path_lflatten(Iter, Path) ->

        ?rep20(lists:flatten(Path)),

        do_conc_path_lflatten(Iter-1, Path).


%--------------------------------------
% lists:concat
%--------------------------------------
conc_path_lconcat(Iter) ->
        do_conc_path_lconcat(Iter, getData()).

do_conc_path_lconcat(0, _Path) -> ok;
do_conc_path_lconcat(Iter, Path) ->

        ?rep20(lists:concat(Path)),

        do_conc_path_lconcat(Iter-1, Path).

%--------------------------------------
% lists:append
%--------------------------------------
conc_path_lappend(Iter) ->
        do_conc_path_lappend(Iter, getData()).

do_conc_path_lappend(0, _Path) -> ok;
do_conc_path_lappend(Iter, Path) ->

        ?rep20(lists:append(Path)),

        do_conc_path_lappend(Iter-1, Path).




%--------------------------------------
% Which function performs better, unsuprisingly, varies a bit depending on the data.
%--------------------------------------


getData() ->
 ["usr/", "local/", "www/", "some.doc"].

getData2() ->
["/usr/", "local/", "etc"].


getData3() ->
 ["usr/", "local", "/", "etc" , "/", "www/somewhere strange", "/", "SomeUserOrSomething/blah/", "blah/"].


getData4() ->
["/usr/", "local/"].

getData5() ->
["/usr/"].


%--------------------------------------


conc_path([]) ->
    [];
conc_path([H|T]) ->
    H ++ conc_path(T).


conc_path2([]) ->
    [];
conc_path2([H|[]]) ->
        H;
conc_path2([H|T]) ->
    H ++ conc_path2(T).



conc_path_tailrecursive([]) ->
        [];
conc_path_tailrecursive([H|T]) ->
        c2(T,H).

c2([],Acc) ->
        Acc;
c2([H|[]],Acc) ->
        H ++ Acc;
c2([H|T],Acc) ->
        c2(T,Acc ++ H).
