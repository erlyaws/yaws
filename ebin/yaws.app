{application,yaws,
 [{description,"yaws WWW server"},
  {vsn,"0.22"},
  {modules,[yaws, yaws_app, yaws_config, yaws_server, yaws_sup, yaws_api, yaws_log, yaws_ls, yaws_debug, yaws_compile]},
  {registered, []},
  {mod,{yaws_app,[]}},
  {env, []},
  {applications,[kernel,stdlib]}]}.
