%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server is responsible to accept connections from clients and manage the    %%
%% chat sessions.                                                             %%
%% It works with controller process to keep track of users who join and leave %%
%% chat server.                                                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(server).
-behaviour(gen_server).

-include("chat.hrl").

-record(state, {name, % client's name
                next,
                socket}). % the current socket

-export([start_link/1]).
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         code_change/3, 
         terminate/2]).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket}}.

handle_call(_E, _From, State) ->
    {noreply, State}.

handle_cast(accept, S = #state{socket=ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    server_sup:start_socket(),
    send(AcceptSocket, 
         "Welcome to the chat server. ~n"
         "Enter your nick name! ~n", []),
    {noreply, S#state{socket=AcceptSocket, next=nick}};

handle_cast({chat, Nick, Str}, S) ->
    gen_server:cast(controller, {say, Nick, Str}),
    {noreply, S#state{name=Nick, next=chat}}.

%% clients input comming from socket
handle_info({tcp, Socket, ?QUIT++_}, S = #state{name=Nick, next=chat}) ->
    quit(Socket, Nick, S),
    {stop, normal, S};

handle_info({tcp, Socket, ?NICKS++_}, S = #state{name=Nick, next=chat}) ->
    gen_server:cast(controller, {nick_list, Nick}),
    refresh_socket(Socket),
    {noreply, S#state{name=Nick, next=chat}};

handle_info({tcp, Socket, ?ME++_}, S = #state{name=Nick, next=chat}) ->
    who_am_i(Nick, Socket),
    refresh_socket(Socket),
    {noreply, S#state{name=Nick, next=chat}};

handle_info({tcp, Socket, ?PRIVATE++Rest}, S = #state{name=Nick, next=chat}) ->
    {Recv, [_|Msg]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Rest),
    gen_server:cast(controller, {private_message, Nick, Recv, clean(Msg)}),
    refresh_socket(Socket),
    {noreply, S#state{name=Nick, next=chat}};

handle_info({tcp, Socket, ?HELP++_}, S = #state{name=Nick, next=chat}) ->
    help(Socket, Nick),
    refresh_socket(Socket),
    {noreply, S#state{name=Nick, next=chat}};

handle_info({tcp, _Socket, Str}, S = #state{socket=Socket, next=nick}) 
  when Str =:= ?CRLF ; Str =:= ?CR ; Str =:= ?LF ->
    refresh_socket(Socket),
    {noreply, S#state{socket=Socket, next=nick}};

handle_info({tcp, _Socket, Str}, S = #state{socket=Socket, next=nick}) ->
    Reply = set_nick(Socket, clean(Str), S),
    refresh_socket(Socket),
    Reply;

handle_info({tcp, _Socket, Str}, S = #state{socket=Socket, name=Nick, next=chat})
  when Str =:= ?CRLF ; Str =:= ?CR ; Str =:= ?LF ->
    refresh_socket(Socket),
    {noreply, S#state{name=Nick, next=chat}};

handle_info({tcp, _Socket, ?IGNORE++_Str}, S = #state{socket=Socket, name=Nick, next=chat}) ->
    io:format("We decided to keep this secret! ;-)~n", []),
    refresh_socket(Socket),
    {noreply, S#state{name=Nick, next=chat}};

handle_info({tcp, _Socket, Str}, S = #state{socket=Socket, name=Nick, next=chat}) ->
    gen_server:cast(self(), {chat, Nick, clean(Str)}), 
    refresh_socket(Socket),
    {noreply, S};

handle_info({tcp_closed, _Socket}, S) ->
    {stop, normal, S};

handle_info({tcp_error, _Socket, _}, S) ->
    {stop, normal, S};

handle_info(E, S) ->
    io:format("unexpected: ~p~n", [E]),
    {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
    ok;

terminate(_Reason, _State) ->
    io:format("terminate reason: ~p~n", [_Reason]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
quit(Socket, Nick, S) ->
    send(Socket, "Goodbye ~p", [Nick]),
    io:format("~p Left the chat server! ~n", [Nick]),
    gen_server:call(controller, {disconnect, Nick}),
    gen_tcp:close(S#state.socket).

set_nick(Socket, Nick, S) ->
    Response = gen_server:call(controller, {check_nick, Nick, Socket}),
    case Response of 
        nick_in_use -> 
            send(Socket, "Nick in use! Pick something else.", []),
            {noreply, S#state{socket=Socket, next=nick}};
        {ok, _} ->
            io:format("~p Joined chat! ~n", [Nick]),
            send(Socket, "Your nick name is ~p. ~n"
                         "You can always get some help using !h ~n"
                       , [Nick]),
            gen_server:cast(controller, {join, Nick}),
            {noreply, S#state{socket=Socket, name=Nick, next=chat}}
    end.

who_am_i(Nick, Socket) ->
    send(Socket, Nick, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clean(Str) ->   
    hd(string:tokens(Str, "\r\n")).

help(Socket, _Nick) ->  
    Menu = 
        "Your massages are broadcasted by default.~n" ++
        "In order to quit the chat enter !q.~n" ++
        "To send private message to a user start the message with !p:Nick ~n" ++
        "To see list of all active nicks you can enter !n ~n" ++
        "To see your nick name enter !m ~n",
    send(Socket, Menu, []).    

send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
    refresh_socket(Socket),
    ok.

refresh_socket(Socket) ->
    ok = inet:setopts(Socket, [{active, once}]).    
