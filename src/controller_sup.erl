-module(controller_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    ControllerSpec =  {controller,
                       {controller, start, []},
                       permanent, 1000, worker, [controller]},

    ChildSpecs = [ControllerSpec],

    ok = supervisor:check_childspecs(ChildSpecs),
    StartSpecs = {{one_for_one, 60, 3600}, ChildSpecs},
    {ok, StartSpecs}.
