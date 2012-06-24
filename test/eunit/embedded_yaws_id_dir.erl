-module(embedded_yaws_id_dir).

-include_lib("eunit/include/eunit.hrl").

id_dir_test() ->
    Id = "id_dir_test",
    GconfList = [{id, Id},
                 {logdir, "./logs"},
                 {ebin_dir, ["./ebin"]}],
    Docroot = yaws:tmpdir(),
    SconfList = [{port, 9999},
                 {servername, Id},
                 {listen, {127,0,0,1}},
                 {docroot, Docroot}],
    {ok, _SCList, _GC, _ChildSpecs} = yaws_api:embedded_start_conf(
                                        Docroot, SconfList, GconfList, Id),
    try
        {ok,
         {file_info, _, directory, read_write, _, _, _, _, _, _, _, _, _, _}} =
            file:read_file_info(yaws:id_dir(Id)),
        ok
    after
        ok = file:del_dir(yaws:id_dir(Id))
    end.
