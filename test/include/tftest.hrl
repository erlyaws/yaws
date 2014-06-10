-define(line, put('$line',{?MODULE,?LINE}),).

-define(srcdir, filename:dirname(
                  proplists:get_value(source, ?MODULE:module_info(compile))
                 )).

-define(builddir, filename:dirname(code:which(?MODULE))).
