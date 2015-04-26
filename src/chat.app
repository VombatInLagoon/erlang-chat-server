{application, chat, 
  [{description, "Robust Chat Server"},
   {vsn, "0.1"},
   {modules, [chat, server, server_sup, 
              controller, controller_supervisor]},
   {registered, [chat]},
   {applications, [kernel, stdlib]},
   {mod, {chat, []}},
   {env, [{name, "Robust Chat Server"}, 
          {port, 6667}]}
 ]
}.