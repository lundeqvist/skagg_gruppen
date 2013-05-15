-module(imup_listener).
-author('grupp4').


-export([listen/3]).

-define(TCP_OPTIONS, [binary, {packet, 2}, {active, false}, {reuseaddr, true}]).

%%-------------------------------------------------------------------------
%% @spec (ReceiverPID, Port, HandlerPID) -> Pid | {error,Error}
%% @doc To be called by the main server process to be able to listen to port Port and HandlerPID will receive the messages.
%%      If listen/2 fails with Reason, the function returns {error,Reason}.
%%      If successful it will return the Pid of the socket.
%%
%% === Example ===
%% <div class="example">```
%% HandlerPID = Process which can handle messages
%% 1> ServerSocket = imup_listen(5555, HandlerPID)
%% #Port<0.669>'''
%% </div>      
%% @end
%%-------------------------------------------------------------------------
%% -spec listen(Port, HandlerPID) -> port() | tuple() when
%%       Port::integer(),
%%       HandlerPID::pid().
%% listen(Port, HandlerPID) ->
%%     process_flag(trap_exit, true),
%%     {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
%%     spawn_link(fun() -> accept(LSocket, HandlerPID) end),
%%     LSocket.



-spec listen(ReceiverPID, Port, HandlerPID) -> port() | tuple() when
      ReceiverPID::pid(),
      Port::integer(),
      HandlerPID::pid().
listen(ReceiverPID, Port, HandlerPID) ->  
    {Ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    CommunicationPID = spawn_link(fun() -> communication(HandlerPID, running) end),
    AcceptorPID = spawn_link(fun() -> accept(LSocket, CommunicationPID) end),
    ReceiverPID ! {Ok, LSocket},
    restarter(AcceptorPID, CommunicationPID, HandlerPID, LSocket).


%%------------------------------
% Internal Functions
%%------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Communication function %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
communication(HandlerPID, State) ->
    case State of
	running ->
	    receive
		{update_handler, NewHandlerPID} ->
		    communication(NewHandlerPID, State);
		{get_handler, ReceiverPID} ->
		    ReceiverPID ! {handler, HandlerPID},
		    communication(HandlerPID, State);
		pause ->
		    communication(HandlerPID, paused);
		resume ->
		    communication(HandlerPID, running);
		_ ->
		    exit(unexpected_question)
	    end;
	paused ->
	    receive
		{update_handler, NewHandlerPID} ->
		    communication(NewHandlerPID, State);
		{get_handler, ReceiverPID} ->
		    ReceiverPID ! paused,
		    communication(HandlerPID, State);
		pause ->
		    communication(HandlerPID, paused);
		resume ->
		    communication(HandlerPID, running);
		_ ->
		    exit(unexpected_question)
	    end;	    
	_ ->
	    exit(unknown_state)
    end.


%%%%%%%%%%%%%%%%%%%%%%
%% Restart function %%
%%%%%%%%%%%%%%%%%%%%%%
restarter(AcceptorPID, CommunicationPID, HandlerPID, LSocket) ->
    receive
	{'EXIT', _PID, normal} ->
	    exit(normal);
	{'EXIT', PID, _Reason} ->
	    case PID of
		AcceptorPID ->
		    NewAcceptorPID = spawn_link(fun() -> accept(LSocket, CommunicationPID) end),
		    restarter(NewAcceptorPID, CommunicationPID, HandlerPID, LSocket);    
		CommunicationPID ->
		    NewCommunicationPID = spawn_link(fun() -> communication(HandlerPID, running) end),
		    AcceptorPID ! {update_communication, NewCommunicationPID},
		    restarter(AcceptorPID, NewCommunicationPID, HandlerPID, LSocket);
		_ ->
		    exit(unknown_pid)
	    end;
	pause ->
	    CommunicationPID ! pause,
	    restarter(AcceptorPID, CommunicationPID, HandlerPID, LSocket);
	resume ->
	    CommunicationPID ! resume,
	    restarter(AcceptorPID, CommunicationPID, HandlerPID, LSocket);
	{update_handler, NewHandlerPID} ->
	    CommunicationPID ! {update_handler, NewHandlerPID},
	    AcceptorPID ! {update_handler, NewHandlerPID},
	    restarter(AcceptorPID, CommunicationPID, NewHandlerPID, LSocket);
	{communicate, Message} ->
	    CommunicationPID ! Message,
	    restarter(AcceptorPID, CommunicationPID, HandlerPID, LSocket);
	Reason -> 
	    exit(Reason)
    end.


handlerReceiver(ParentPID) ->
    ParentPID ! {get_handler, self()},
    receive
	{handler, Handler} ->
	    Handler;
	paused ->
	    timer:sleep(100),
	    handlerReceiver(ParentPID);
	_ ->
	    exit(no_handler)
    end.



% Wait for incoming connections and spawn a process that will process incoming packets.
%% accept(LSocket, HandlerPID) ->
%%     {ok, Socket} = gen_tcp:accept(LSocket),
%%     Pid = spawn(fun() ->
%% 			io:format("Connection accepted ~n", []),
%% 			loop(Socket, HandlerPID)
%% 		end),
%%     gen_tcp:controlling_process(Socket, Pid),
%%     accept(LSocket, HandlerPID).



% Wait for incoming connections and spawn a process that will process incoming packets.
accept(LSocket, CommunicationPID) ->
    AcceptorParentPID = self(),
    AcceptorPID = spawn_link(fun() -> acceptor(LSocket, AcceptorParentPID) end),
    acceptRestarter(CommunicationPID, AcceptorPID, []).


acceptRestarter(CommunicationPID, AcceptorPID, PidList) ->   
    receive
	{'EXIT', _PID, normal} ->
	    exit(normal);
	{'EXIT', PID, _Reason} ->
	    case PID of
		AcceptorPID ->
		    ok;
		_ ->
		    exit(unknown_pid)
	    end;
	{insert_pid, Pid} ->
	    acceptRestarter(CommunicationPID, AcceptorPID, [Pid|PidList]);
	{remove_pid, Pid} ->
	    TmpList = lists:delete(Pid, PidList),
	    acceptRestarter(CommunicationPID, AcceptorPID, TmpList);
	{get_handler, ReceiverPID} ->
	    HandlerPID = handlerReceiver(CommunicationPID),
	    ReceiverPID ! {handler, HandlerPID},
	    acceptRestarter(CommunicationPID, AcceptorPID, PidList);
	{update_handler, NewHandler} ->
	    [P ! {update_handler, NewHandler} || P <- PidList],
	    acceptRestarter(CommunicationPID, AcceptorPID, PidList);
	{update_communication, NewCommunicationPID} ->
	    acceptRestarter(NewCommunicationPID, AcceptorPID, PidList);
	_ ->
	    exit(unknown_message)
    end.
    


% Wait for incoming connections and spawn a process that will process incoming packets.
acceptor(LSocket, ParentPID) ->
    HandlerPID = handlerReceiver(ParentPID), 
	
    {ok, Socket} = gen_tcp:accept(LSocket),
    Pid = spawn(fun() ->
			io:format("Connection accepted ~n", []),
			loop(Socket, HandlerPID)
		end),
    gen_tcp:controlling_process(Socket, Pid),
    ParentPID ! {insert_pid, Pid},
    acceptor(LSocket, ParentPID).



%% Loop receiving data from client %%
loop(Sock, HandlerPID) ->
    inet:setopts(Sock, [{active, once}]),
    receive
	{tcp, Socket, Data} ->
	    io:format("Got packet: ~p == ", [Data]),

	    FormatedData = process_data(Socket, Data, HandlerPID),
	    io:format("~p~n", [FormatedData]),
	    convey_message(Socket, FormatedData),

	    loop(Socket, HandlerPID);
	{tcp_closed, Socket} ->
	    io:format("Socket ~p closed~n", [Socket]);
	{tcp_error, Socket, Reason} ->
	    io:format("Error on socket ~p reason: ~p~n", [Socket, Reason]);
	{update_handler, NewHandlerPID} ->
	    loop(Sock, NewHandlerPID);
	_ ->
	    loop(Sock, HandlerPID)
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%
%% Processing Function %%
%%%%%%%%%%%%%%%%%%%%%%%%%
-spec process_data(S, D, P) -> term() when
      S::port(),
      D::binary(),
      P::pid().
process_data(Socket, Data, HandlerPID) when is_binary(Data) ->
    case binary_to_term(Data) of
	{send_host, GameID, Msg} ->
	    HandlerPID ! {get_host, self(), GameID},
	    receive
		{host_is, _HID, HSOCK} ->
		    convey_message(HSOCK, {message, Msg}),
		    {ok, delivered};
		_ ->
		    {error, nohost}
	    end;
	{send_all, GameID, Msg} ->
	    HandlerPID ! {get_players_pids, self(), GameID},
	    receive
		{players, SList} ->
		    TmpFun = fun(S) ->
		    		     convey_message(S, {message, Msg})
		    	     end,
		    lists:foreach(TmpFun, SList),
		    %%[convey_message(Sock, {message, Msg}) || Sock <- SList],
		    {ok, delivered};
		_ ->
		    {error, noplayers}
	    end;
	{send_to_id, ReceiverID, Msg} ->
	    HandlerPID ! {get_client_pid, self(), ReceiverID},
	    receive
	       {pid, SockPID} ->
		    convey_message(SockPID, {message, Msg}),
		    {ok, delivered};
		_ ->
		    {error, noid}
	    end;
	{host_game, GameID} ->
	    HandlerPID ! {host_game, self(), Socket, GameID},
	    receive
		{ok, hosting_game} ->
		    {ok, hosting_game};
		_ ->
		    {error, not_hosting}
	    end;	    
	{join_game, GameID} ->
	    HandlerPID ! {join_game, self(), Socket, GameID},
	    receive
		{ok, joined_game} ->
		    {ok, joined_game};
		_ ->
		    {error, not_joined}
	    end;
	{get_users, GameID} ->
	    HandlerPID ! {get_players, self(), GameID},
	    receive
		{players, SList} ->
		    HandlerPID ! {get_client_id, self(), Socket},
		    receive
			{id, CID} ->
			    {message, {GameID, CID, SList}};
			_ ->
			    {error, noid}
		    end;
		_ ->
		    {error, nousers}
	    end;	    
	{start_game, _GameID} ->
	    tbi;
	{end_game, GameID} ->
	    HandlerPID ! {remove_game, self(), GameID},
	    receive
		{ok, game_removed} ->
		    {ok, game_ended};
		_ ->
		    {error, game_not_ended}
	    end;
	{enter, SenderID} ->
	    HandlerPID ! {insert_client, self(), Socket, SenderID},
	    receive
		{ok, inserted} ->
		    {ok, entered};
		_ ->
		    {error, not_entered}
	    end;
	disconnect ->
	    HandlerPID ! {remove_client, self(), Socket},
	    receive
		{ok, removed} ->
		    disconnect;
		_ ->
		    {error, not_disconnected}
	    end;
	Dat ->
	    Dat
    end;
process_data(_Socket, Data, _DictPID) ->
    Data.

convey_message(Socket, Data) when is_binary(Data) ->
    gen_tcp:send(Socket, Data);
convey_message(Socket, Data) ->
    BinData = term_to_binary(Data),
    gen_tcp:send(Socket, BinData).



