-module(chat).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    {ok, _ControllerPid} = controller_sup:start_link(), 
    {ok, ServerPid}      = server_sup:start_link(), 
    io:fwrite("Chat server started successfully!~n"),
    {ok, ServerPid}.

stop(_State) ->
    ok.
 
