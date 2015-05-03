-module(chat_controller_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("chat.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    ControllerSpec =  {?CONTROLLER,
                       {?CONTROLLER, start, []},
                       permanent, 1000, worker, [?CONTROLLER]},

    ChildSpecs = [ControllerSpec],

    ok = supervisor:check_childspecs(ChildSpecs),
    StartSpecs = {{one_for_one, 60, 3600}, ChildSpecs},
    {ok, StartSpecs}.
