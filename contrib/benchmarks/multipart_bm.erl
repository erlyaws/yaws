-module(multipart_bm).
-include("../../include/yaws_api.hrl").

-export([benchmarks/0]).

-export([long_msg/1]).

benchmarks() ->
    {100, [long_msg]}.

long_msg(Iter) ->
    Sep = make_rand_bin(32),
    Body = binary:copy(make_rand_bin(10240), 100),
    Msg = list_to_binary(["--", Sep, "\r\n",
                          "Content-Disposition: form-data; name=\"abc123\"; ",
                          "filename=\"abc123\"\r\n",
                          "Content-Type: text/plain\r\n",
                          "Test-Header: sampledata\r\n\r\n",
                          Body,
                          "\r\n--",Sep,"--\r\n"
                         ]),
    long_msg(Iter, mk_arg(Msg, Sep), Body).

long_msg(0, _Msg, _Body) -> ok;
long_msg(Iter, Msg, Body) ->
    {result, Res} = yaws_api:parse_multipart_post(Msg, [binary]),
    Body = proplists:get_value(body, Res),
    %%io:format("~p~n", [Res]),
    long_msg(Iter - 1, Msg, Body).

make_rand_bin(Length) ->
    State = rand:seed_s(exs64),
    make_rand_bin(Length, State, <<>>).
make_rand_bin(0, _State, Acc) -> Acc;
make_rand_bin(Length, State, Acc) ->
    {Rand, State1} = rand:uniform_s($z - $a, State),
    make_rand_bin(Length - 1, State1, <<Acc/binary, (Rand + $a - 1)>>).

mk_arg(Data, Sep) ->
    ContentType = binary_to_list(<<"multipart/form-data; boundary=", Sep/binary>>),
    Req = #http_request{method = 'POST'},
    Headers = #headers{content_type = ContentType},
    #arg{headers = Headers, req = Req, clidata = Data}.
