{application,yaws,
 [{description,"yaws WWW server"},
  {vsn,"0.21"},
  {modules,[yaws, yaws_app, yaws_config, yaws_server, yaws_sup, yaws_api, yaws_log, yaws_ls, yaws_debug]},
  {registered, []},
  {mod,{yaws_app,[]}},
  {env, []},
  {applications,[kernel,stdlib]}]}.
