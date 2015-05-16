%%%-----------------------------------------------------------------------------
%%% @author Amin
%%% @copyright 2015 Free Software
%%% @doc Server is responsible to accept connections from clients and manage the    
%%%      chat sessions.                                                             
%%%      It works with controller process to keep track of users who join and 
%%%      leave chat server.                                                               
%%% @end
%%%-----------------------------------------------------------------------------
-module(chat_server).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         code_change/3, 
         terminate/2]).

-include("chat.hrl").

-record(state, {name, % client's name
                next,
                socket}). % the current socket

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts chat server.
%% 
%% @spec start_link(Socket::socket()) -> {ok, Pid}
%% where 
%%  Pid = pid()   
%% @end
%%------------------------------------------------------------------------------
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

init(Socket) ->
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket}}.

handle_call(_E, _From, State) ->
    {noreply, State}.

handle_cast(accept, S = #state{socket=ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    chat_server_sup:start_socket(),
    send(AcceptSocket, 
         "Welcome to the chat server. ~n"
         "Please choose a nick name.  ~n"
         "Note that nick names are case sensitive! ~n", []),
    {noreply, S#state{socket=AcceptSocket, next=nick}};

handle_cast({chat, Nick, Str}, S) ->
    gen_server:cast(?CONTROLLER, {say, Nick, Str}),
    {noreply, S#state{name=Nick, next=chat}}.

%% clients input comming from socket
handle_info({tcp, Socket, ?QUIT++_}, S = #state{name=Nick, next=chat}) ->
    quit(Socket, Nick, S),
    {stop, normal, S};

handle_info({tcp, Socket, ?NICKS++_}, S = #state{name=Nick, next=chat}) ->
    gen_server:cast(?CONTROLLER, {nick_list, Nick}),
    refresh_socket(Socket),
    {noreply, S#state{name=Nick, next=chat}};

handle_info({tcp, Socket, ?ME++_}, S = #state{name=Nick, next=chat}) ->
    who_am_i(Nick, Socket),
    refresh_socket(Socket),
    {noreply, S#state{name=Nick, next=chat}};

handle_info({tcp, Socket, ?PRIV++Rest}, S = #state{name=Nick, next=chat}) ->
    send_private_msg(Socket, Nick, Rest),
    refresh_socket(Socket),
    {noreply, S#state{name=Nick, next=chat}};

handle_info({tcp, Socket, ?HELP++_}, S = #state{name=Nick, next=chat}) ->
    help(Socket, Nick),
    refresh_socket(Socket),
    {noreply, S#state{name=Nick, next=chat}};

handle_info({tcp, Socket, Str}, S = #state{next=nick}) 
  when Str =:= ?CRLF ; Str =:= ?CR ; Str =:= ?LF ->
    refresh_socket(Socket),
    {noreply, S#state{socket=Socket, next=nick}};

handle_info({tcp, Socket, Str}, S = #state{next=nick}) ->
    Reply = set_nick(Socket, clean(Str), S),
    refresh_socket(Socket),
    Reply;

handle_info({tcp, Socket, Str}, S = #state{name=Nick, next=chat})
  when Str =:= ?CRLF ; Str =:= ?CR ; Str =:= ?LF ->
    refresh_socket(Socket),
    {noreply, S#state{name=Nick, next=chat}};

handle_info({tcp, Socket, ?IGNORE++_Str}, S = #state{name=Nick, next=chat}) ->
    io:format("We decided to keep this secret! ;-)~n", []),
    refresh_socket(Socket),
    {noreply, S#state{name=Nick, next=chat}};

handle_info({tcp, Socket, Str}, S = #state{name=Nick, next=chat}) ->
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

terminate(normal, S = #state{name=Nick}) ->
    gen_server:call(?CONTROLLER, {disconnect, Nick}),
    gen_tcp:close(S#state.socket),
    ok;

terminate(_Reason, S = #state{name=Nick}) ->
    gen_server:call(?CONTROLLER, {disconnect, Nick}),
    gen_tcp:close(S#state.socket),
    io:format("terminate reason: ~p~n", [_Reason]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

quit(Socket, Nick, S) ->
    send(Socket, "Goodbye ~p", [Nick]),
    io:format("~p Left the chat server! ~n", [Nick]),
    gen_server:call(?CONTROLLER, {disconnect, Nick}),
    gen_tcp:close(S#state.socket).

send_private_msg(Socket, Nick, RawMsg) ->
    case string:tokens(RawMsg, ":") of
        [Reciver, Msg] -> 
            gen_server:cast(?CONTROLLER, {private_message, Nick, Reciver, 
                                          clean(Msg)});
        _ -> send(Socket, "Malformed private message. ~n"
                          "Use format !p:Nick:Message ~n", [])
    end. 

set_nick(Socket, Nick, S) ->
    Response = gen_server:call(?CONTROLLER, {check_nick, Nick, Socket}),
    case Response of 
        nick_in_use -> 
            send(Socket, "Nick in use! Pick something else.", []),
            {noreply, S#state{socket=Socket, next=nick}};
        {ok, _} ->
            io:format("~p Joined chat! ~n", [Nick]),
            send(Socket, "Your nick name is ~p. ~n"
                         "You can always get some help using !h ~n"
                       , [Nick]),
            gen_server:cast(?CONTROLLER, {join, Nick}),
            {noreply, S#state{socket=Socket, name=Nick, next=chat}}
    end.

who_am_i(Nick, Socket) ->
    send(Socket, Nick, []).

%%%=============================================================================
%%% Helper functions
%%%=============================================================================

clean(Str) ->   
    hd(string:tokens(Str, "\r\n")).

help(Socket, _Nick) ->  
    HelpMenu = 
        "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n" ++
        "%%% Your massages are broadcasted by default!            %%%~n" ++
        "%%% In order to quit the chat enter !q                   %%%~n" ++
        "%%% To send private message to users use !p:Nick:Message %%%~n" ++
        "%%% To see list of all active nicks you can enter !n     %%%~n" ++
        "%%% To see your nick name enter !m                       %%%~n" ++
        "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n",
    send(Socket, HelpMenu, []).    

send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
    refresh_socket(Socket),
    ok.

refresh_socket(Socket) ->
    ok = inet:setopts(Socket, [{active, once}]).    
