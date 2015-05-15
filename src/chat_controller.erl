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
    % dictionary is used to keep track of online users
    % and their associated sockets.
    Users = dict:new(),
    {ok, Users}.

handle_call({check_nick, Nick, Socket}, _From, Users) ->
    Response = case dict:is_key(Nick, Users) of
                   true ->
                       NewUsers = Users,
                       nick_in_use;
                   false ->
                       NewUsers = dict:append(Nick, Socket, Users),
                       {ok, user_list(NewUsers)}
               end,
    {reply, Response, NewUsers};

handle_call({disconnect, Nick}, _From, Users) ->
    Response = case dict:is_key(Nick, Users) of
                   true ->
                       NewUsers = dict:erase(Nick, Users),
                       ok;
                   false ->
                       NewUsers = Users,
                       user_not_found
               end,
    {reply, Response, NewUsers};

handle_call(_Message, _From, State) ->
    {reply, error, State}.

handle_cast({say, Nick, Msg}, Users) ->
    broadcast(Nick, Msg, Users),
    {noreply, Users}; 

handle_cast({nick_list, Nick}, Users) ->
    nick_list(Nick, Users),
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
    UpdatedDict = dict:erase(Nick, Users),
    Sockets = [hd(SockAsList) || {_, SockAsList} <- dict:to_list(UpdatedDict)],
    lists:map(fun(Sock) -> gen_tcp:send(Sock, FormatMsg) end, Sockets).

user_list(Users) ->
    UserList = dict:fetch_keys(Users),
    string:join(UserList, " ").

nick_list(Nick, Users) ->
    case dict:find(Nick, Users) of 
        {ok, SockAsList} ->
            Nicks = user_list(dict:erase(Nick, Users)),
            gen_tcp:send(hd(SockAsList), "Online people: " ++ Nicks ++ "\n");
        _ -> ok    
    end.

private_message(Recv, Nick, Msg, Users) ->
    FormatMsg = format_message(Nick, Msg),
    Temp = dict:find(Recv, Users),
    case Temp of
        {ok, SockAsList} ->
            gen_tcp:send(hd(SockAsList), "**" ++ FormatMsg);
        _ ->
            ok
    end.

%%%=============================================================================
%%% Helper functions
%%%=============================================================================

format_message(Nick, Msg) ->
    FormattedMsg = format_time() ++ " " ++ Nick ++ ":" ++ Msg ++ "\n",
    FormattedMsg.

format_time() ->
    {H, M, S} = time(),
    io_lib:format('(~2..0b:~2..0b:~2..0b)', [H, M, S]).

