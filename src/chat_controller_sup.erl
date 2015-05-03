%%%-----------------------------------------------------------------------------
%%% @author Amin 
%%% @copyright 2015 Free Software
%%% @doc Supervisor for the chat controller. 
%%% @end
%%%-----------------------------------------------------------------------------
-module(chat_controller_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callback
-export([init/1]).

-include("chat.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the controller process and supervise it.
%% @spec start_link() -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%------------------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%=============================================================================
%%% supervisor callbacks
%%%=============================================================================

init([]) ->

    ControllerSpec =  {?CONTROLLER,
                       {?CONTROLLER, start, []},
                       permanent, 1000, worker, [?CONTROLLER]},

    ChildSpecs = [ControllerSpec],

    ok = supervisor:check_childspecs(ChildSpecs),
    StartSpecs = {{one_for_one, 60, 3600}, ChildSpecs},
    {ok, StartSpecs}.
