{application, chat, 
  [{description, "Robust Chat Server"},
   {vsn, "0.1"},
   {modules, [chat, chat_server, chat_server_sup, 
              chat_controller, chat_controller_supervisor]},
   {registered, [chat]},
   {applications, [kernel, stdlib]},
   {mod, {chat, []}},
   {env, [{name, "Robust Chat Server"}, 
          {port, 6667}]}
 ]
}.