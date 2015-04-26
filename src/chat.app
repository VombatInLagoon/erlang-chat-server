{application, chat, 
  [{description, "Robust Chat Server"},
   {vsn, "0.1"},
   {modules, [chat, server, server_sup]},
   {registered, [chat]},
   {applications, [kernel, stdlib]},
   {mod, {chat, []}},
   {env, [{name, "Robust Chat Server"}, 
          {port, 6667}]}
 ]
}.