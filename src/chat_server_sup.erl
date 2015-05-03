%%%-----------------------------------------------------------------------------
%%% @author Amin 
%%% @copyright 2015 Free Software
%%% @doc Supervisor for the chat server. 
%%% @end
%%%-----------------------------------------------------------------------------
-module(chat_server_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_socket/0]).

%% supervisor callback
-export([init/1]).

-include("chat.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts 20 Chat servers and supervise them.
%% @spec start_link() -> {ok, Pid}
%% where 
%%  Pid = pid()
%% @end
%%------------------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% @doc Assigns a server process to every new client.
%% @spec start_socket() -> {ok, Pid}
%% where
%%  Pid = pid() 
%% @end
%%------------------------------------------------------------------------------
start_socket() ->
    supervisor:start_child(?MODULE, []).

%%%=============================================================================
%%% supervisor callbacks
%%%=============================================================================

init([]) ->
    {ok, Port} = application:get_env(port),
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, {packet,line}]),
    spawn_link(fun empty_listeners/0),
    {ok, {{simple_one_for_one, 60, 3600},
          [{?SERVER,
            {?SERVER, start_link, [ListenSocket]}, % pass the socket!
            temporary, 1000, worker, [?SERVER]}
          ]}}.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Start with 20 listeners so that many multiple connections can
%%      be started at once, without serialization. In best circumstances,
%%      a process would keep the count active at all times to insure nothing
%%      bad happens over time when processes get killed too much.
%% @spec empty_listeners() -> ok
%% @end
%%------------------------------------------------------------------------------
empty_listeners() ->
    [start_socket() || _ <- lists:seq(1,20)],
    ok.
