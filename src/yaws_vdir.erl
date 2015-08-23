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

    case Req#http_request.path of
        {abs_path, RawPath} ->
            case (catch yaws_api:url_decode_q_split(RawPath)) of
                {'EXIT', _} ->
                    %%broken request - ignore let yaws_server handle it.
                    ARG2 = ARG;
                {"", _QueryPart} ->
                    ARG2 = ARG;
                {"/", _QueryPart} ->
                    %%don't allow vdir to be specified for root -
                    %% it doesn't make sense
                    ARG2 = ARG;
                {DecPath, _QueryPart} ->
                    SC = get(sc),

                    %%vdirpath/3 will return the longest(ie most specific)
                    %% 'virtual directory' match for our request
                    %%It retrieves the vdir definitions from #arg.opaque
                    case yaws_server:vdirpath(SC, ARG, DecPath) of
                        {"",_MainDocRoot} ->
                            %%no virtual dir corresponding to this
                            %% http_request.path

                            ARG2 = ARG;
                        {Virt,DocRoot} ->

                            %%the virtual-path of our request matches a
                            %% vdir specification
                            %% - rewrite ARG accordingly.

                            ARG2 = ARG#arg{docroot = DocRoot,
                                           docroot_mount = Virt}
                    end
            end;
        _Else ->
            ARG2 = ARG
    end,

    ARG2.

