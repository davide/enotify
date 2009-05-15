{application, enotify_app,
 [{description, "eNotify Server"},
  {vsn, "1.0"},
  {modules, [enotify_app, enotify_sup, enotify]},
  {registered, [enotify]},
  {applications, [kernel, stdlib]},
  {mod, {enotify_app, [mingw]}},
  {env, [{timeout, 3000}]}
 ]}.
