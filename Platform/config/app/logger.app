{application, logger,
 [{description, "error logger application!"},
  {id, "gateway"},
  {vsn, "0.1"},
  {modules, []},
  {registered, []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {logger_app, []}},
  {env, [{log_level,5},
  			{logger_file_dir, "./log"}]}
  ]}.
