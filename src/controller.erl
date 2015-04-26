-module(controller).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% This is called when a connection is made to the server
init([]) ->
    Users = dict:new(), % It maps nick to socket
    {ok, Users}.

% handle_call is invoked in response to gen_server:call
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

% handle_cast is invoked in response to gen_server:cast
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


% auxiliary functions
broadcast(Nick, Msg, Users) ->
    FormatMsg = format_message(Nick, Msg),
    Sockets = [Value || {_, [Value|_]} <- dict:to_list(dict:erase(Nick, Users))],
    lists:map(fun(Sock) -> gen_tcp:send(Sock, FormatMsg) end, Sockets).

user_list(Users) ->
    UserList = dict:fetch_keys(Users),
    string:join(UserList, " ").

nick_list(Nick, Users) ->
    case dict:find(Nick, Users) of 
        {ok, [Socket|_]} ->
            Nicks = user_list(dict:erase(Nick, Users)),
            gen_tcp:send(Socket, "Online people: " ++ Nicks ++ "\n");
          _ -> ok    
     end.

private_message(Recv, Nick, Msg, Users) ->
    FormatMsg = format_message(Nick, Msg),
    Temp = dict:find(Recv, Users),
    case Temp of
        {ok, [Socket|_]} ->
            gen_tcp:send(Socket, "**" ++ FormatMsg);
        _ ->
            ok
    end.

format_message(Nick, Msg) ->
    FormattedMsg = format_time() ++ " " ++ Nick ++ ":" ++ Msg ++ "\n",
    FormattedMsg.

format_time() ->
    {H, M, S} = time(),
    io_lib:format('(~2..0b:~2..0b:~2..0b)', [H, M, S]).

% Definitions to avoid gen_server compile warnings
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
