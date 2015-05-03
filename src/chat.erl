%%%-----------------------------------------------------------------------------
%%% @author Amin
%%% @copyright 2015 Free Software
%%% @doc Chat server application behaviour. Shall be used to start 
%%%      the caht application.
%%% @end
%%%-----------------------------------------------------------------------------
-module(chat).
-behaviour(application).

%% application callbacks
-export([start/2]).
-export([stop/1]).

%%%=============================================================================
%%% application callbacks
%%%=============================================================================

start(_Type, _Args) ->
    {ok, _ControllerPid} = chat_controller_sup:start_link(), 
    {ok, ServerPid}      = chat_server_sup:start_link(), 
    io:fwrite("Chat server started successfully!~n"),
    {ok, ServerPid}.

stop(_State) ->
    ok.
 
