-module(chat).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    io:fwrite("Chat Server~n"),
    controller_sup:start_link(),
    server_sup:start_link().

stop(_State) ->
    ok.
