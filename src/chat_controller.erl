%%%-----------------------------------------------------------------------------
%%% @author Amin 
%%% @copyright 2015 Free software
%%% @doc Controller is responsible mainly for adding users to the data center 
%%%      and removing their name when they disconnect from the server.                
%%%      It does its job via getting requests from chat server.                   
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(chat_controller).
-behaviour(gen_server).

%% API
-export([start/0]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2,
         code_change/3]).

-include("chat.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts chat server controller.
%%
%% @spec start() -> {ok, Pid}
%% where 
%%  Pid = pid()
%% @end
%%------------------------------------------------------------------------------
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

init([]) ->
    % ets table is used to keep track of online users (nicks)
    % and their associated sockets.
    Users = ets:new(users,[set]),
    {ok, Users}.

handle_call({check_nick, Nick, Socket}, _From, Users) ->  
    Response = check_nick(Nick, Users, Socket),
    {reply, Response, Users};

handle_call({disconnect, Nick}, _From, Users) ->
    Response = disconnect_nick(Nick, Users),
    {reply, Response, Users};

handle_call(_Message, _From, State) ->
    {reply, error, State}.

handle_cast({say, Nick, Msg}, Users) ->
    broadcast(Nick, Msg, Users),
    {noreply, Users}; 

handle_cast({nick_list, Socket}, Users) ->
    nick_list(Socket, Users),
    {noreply, Users};

handle_cast({private_message, Nick, Recv, Msg}, Users) ->
    private_message(Recv, Nick, Msg, Users),
    {noreply, Users};

handle_cast({join, Nick}, Users) ->
    broadcast(Nick, "joined the chat! \n", Users),
    {noreply, Users};

handle_cast({left, Nick}, Users) ->
    broadcast(Nick, "left the chat! \n", Users),
    {noreply, Users};

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Message, State) -> 
    {noreply, State}.

terminate(_Reason, _State) -> 
    ok.

code_change(_OldVersion, State, _Extra) -> 
    {ok, State}.
 
%%%=============================================================================
%%% Internal functions
%%%=============================================================================

broadcast(Nick, Msg, Users) ->
    FormatMsg = format_message(Nick, Msg),
    Sockets = [Sock || {N, Sock} <- ets:tab2list(Users), N =/= Nick],
    lists:foreach(fun(Sock) -> gen_tcp:send(Sock, FormatMsg) end, Sockets).

nick_list(Socket, Users) ->
    Nicks = [N ++ " " || {N, S} <- ets:tab2list(Users), S =/= Socket],
    gen_tcp:send(Socket, "Online people: " ++ Nicks ++ "\n").

private_message(Reciver, Nick, Msg, Users) ->
    FormatMsg = format_message(Nick, Msg),
    case ets:lookup(Users, Reciver) of
        [] -> ok;
        [{_,Sock}] ->
            gen_tcp:send(Sock, ?PRIVMARK ++ FormatMsg)
    end.

check_nick(Nick, Users, Socket) ->
    case ets:insert_new(Users, {Nick, Socket}) of
        true ->
            ok;
        false ->
            nick_in_use
    end.

disconnect_nick(Nick, Users) ->
    ets:delete(Users, Nick),
    {ok, Users}.

%%%=============================================================================
%%% Helper functions
%%%=============================================================================

format_message(Nick, Msg) ->
    FormattedMsg = format_time() ++ " " ++ Nick ++ ":" ++ Msg ++ "\n",
    FormattedMsg.

format_time() ->
    {H, M, S} = time(),
    io_lib:format('(~2..0b:~2..0b:~2..0b)', [H, M, S]).

