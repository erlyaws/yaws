-module(yaws_vdir).

-export([arg_rewrite/1]).

-include("../include/yaws_api.hrl").

-export([join/2]).

join(List, Sep) ->
    lists:foldl(fun(A, "") -> A; (A, Acc) -> Acc ++ Sep ++ A
                end, "", List).


arg_rewrite(ARG) ->
    Req = ARG#arg.req,
    %%io:fwrite("----->rewrite_mod for request: ~p\n",[ARG#arg.req]),

    ARG2 = case Req#http_request.path of
               {abs_path, RawPath} ->
                   try yaws_api:url_decode_q_split(RawPath) of
                       {"", _QueryPart} ->
                           ARG;
                       {"/", _QueryPart} ->
                           %%don't allow vdir to be specified for root -
                           %% it doesn't make sense
                           ARG;
                       {DecPath, _QueryPart} ->
                           SC = get(sc),

                           %%vdirpath/3 will return the longest(ie most specific)
                           %% 'virtual directory' match for our request
                           %%It retrieves the vdir definitions from #arg.opaque
                           case yaws_server:vdirpath(SC, ARG, DecPath) of
                               {"",_MainDocRoot} ->
                                   %%no virtual dir corresponding to this
                                   %% http_request.path

                                   ARG;
                               {Virt,DocRoot} ->

                                   %%the virtual-path of our request matches a
                                   %% vdir specification
                                   %% - rewrite ARG accordingly.

                                   ARG#arg{docroot = DocRoot,
                                           docroot_mount = Virt}
                           end
                   catch
                       _:_ ->
                           %%broken request - ignore let yaws_server handle it.
                           ARG
                   end;
               _Else ->
                   ARG
           end,

    ARG2.
