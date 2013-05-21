-module(imup_client).
-author('grupp4').

-export([connect/1, connect/2, connect/3, disconnect/1, send/2]).


-define(TCP_OPTIONS, [binary, {packet, 2}, {active, false}, {reuseaddr, true}]).

-record(server_info, {ip="localhost",port=5555, recvpid}).

%%-------------------------------------------------------------------------
%% @spec (Port) -> Port | {error,Error}
%% @doc To be called by the clientcode to connect to the server.
%%      Port is the port of which to connect on.
%%      If connect/1 fails with Reason, the function returns {error,Reason}.
%%      If successful it will return the Pid of the socket.
%%
%% === Example ===
%% <div class="example">```
%% Connecting to localhost on port 5555.
%% 1> {Socket, PID} = imup_client:connect(5555).
%% #Port<0.669>'''
%% </div>      
%% @end
%%-------------------------------------------------------------------------
-spec connect(Port) -> port() | tuple() when
      Port::integer().
connect(PortNo) ->
    %%{ok, Socket} = gen_tcp:connect("localhost", PortNo, ?TCP_OPTIONS),
    case gen_tcp:connect("localhost", PortNo, ?TCP_OPTIONS) of
	{ok, Socket} ->
	    process_flag(trap_exit, true),
	    _RPID = spawn_link(fun() -> recv(Socket) end),
	    Socket;
	{error, Reason} ->
	    io:format("Error connecting to localhost on port ~p, with reason: ~p ~n", [PortNo, Reason])
    end.

%%-------------------------------------------------------------------------
%% @spec (IP, Port) -> {Port, Pid} | {error,Error}
%% @doc To be called by the clientcode to connect to the server
%%      IP is the ip-address of server, Port is which port to connect on.
%%      If connect/1 fails with Reason, the function returns {error,Reason}.
%%      If successful it will return the Pid of the socket.
%%
%% === Example ===
%% <div class="example">```
%% Connecting to localhost on port 5555.
%% 1> Socket = imup_client:connect("localhost", 5555).
%% #Port<0.669>
%% 2> Socket2 = imup_client:connect({127,0,0,1}, 5555).
%% #Port<0.670>'''
%% </div>      
%% @end
%%-------------------------------------------------------------------------
-spec connect(IP, Port) -> port() | tuple() when
      IP::string() | tuple(),
      Port::integer().
connect(IP, PortNo) ->
    %%{ok, Socket} = gen_tcp:connect(IP, PortNo, [binary, {active, false}, {packet, 2}]),
    case gen_tcp:connect(IP, PortNo, [binary, {active, false}, {packet, 2}]) of
	{ok, Socket} ->
	    process_flag(trap_exit, true),
	    _RPID = spawn_link(fun() -> recv(Socket) end),
	    Socket;
	{error, Reason} ->
	    io:format("Error connecting to ~p on port ~p, with reason: ~p ~n", [IP, PortNo, Reason])
    end.  


%%-------------------------------------------------------------------------
%% @spec (IP, Port, ReceiverPID) -> {Port, Pid} | {error,Error}
%% @doc To be called by the clientcode to connect to the server
%%      IP is the ip-address of server,
%%      Port is which port to connect on,
%%      ReceiverPID is a process id of which to forward messages to.
%%      If connect/1 fails with Reason, the function returns {error,Reason}.
%%      If successful it will return the Pid of the socket.
%%
%% === Example ===
%% <div class="example">```
%% Connecting to localhost on port 5555 and forwarding messages to pid HPID.
%% 1> Socket = imup_client:connect("localhost", 5555, HPID).
%% #Port<0.669>
%% 2> Socket2 = imup_client:connect({127,0,0,1}, 5555, HPID).
%% #Port<0.670>'''
%% </div>      
%% @end
%%-------------------------------------------------------------------------
-spec connect(IP, Port, ReceiverPID) -> pid() | tuple() when
      IP::string() | tuple(),
      Port::integer(),
      ReceiverPID::pid().
connect(IP, PortNo, ReceiverPID) ->
    RPID = self(),
    spawn(fun() ->
		  connectAux(RPID, IP, PortNo, ReceiverPID)
	  end),
    receive
	{ok, HandlerPID} ->
	    HandlerPID;
	{error, Reason} ->
	    {error, Reason}
    end.

connectAux(ParPID, IP, PortNo, ReceiverPID) ->
    case gen_tcp:connect(IP, PortNo, ?TCP_OPTIONS) of
	{ok, Socket} ->
	    process_flag(trap_exit, true),
	    ServInfo = #server_info{ip=IP, port=PortNo, recvpid=ReceiverPID},
	    HandlerPID = spawn_link(fun() -> handler(Socket, ServInfo, 0) end),
	    RPID = spawn_link(fun() -> recv(Socket, HandlerPID, ReceiverPID) end),
	    ParPID ! {ok, HandlerPID},
	    restarter(HandlerPID, RPID);
	{error, Reason} ->
	    io:format("Error connecting to ~p on port ~p, with reason: ~p ~n", [IP, PortNo, Reason]),
	    ParPID ! {error, Reason}
    end.    



sendS(Socket, Message) ->
    BinMsg = term_to_binary(Message),
    gen_tcp:send(Socket, BinMsg).
%%    {ok, A} = gen_tcp:recv(Socket, 0),
    %%A.


%%-------------------------------------------------------------------------
%% @spec (Socket, Message) -> ok | {error, Error}
%% @doc To be called by the clientcode to send a message to the server.
%%      Socket is a port returned by connect/1/2/3,
%%      Message is anything.
%%      If send/2 fails with Reason, the function returns {error,Reason}.
%%      If successful it will return ok.
%%
%% === Example ===
%% <div class="example">```
%% Connecting to localhost on port 5555.
%% 1> Socket = imup_client:connect(5555).
%% #Port<0.669>
%% 2> imup_client:sendS(Socket, {this,can,be,anything}).
%% ok'''
%% </div>      
%% @end
%%-------------------------------------------------------------------------
-spec send(Socket, Message) -> ok | tuple() when
      Socket::pid(),
      Message::any().
send(HandlerPID, Message) ->
    HandlerPID ! {get_socket, self()},
    receive
	{socket, Socket} ->
	    sendS(Socket, Message);
	_ ->
	    {error, nosocket}
    end.




%%-------------------------------------------------------------------------
%% @spec (Socket) -> ok
%% @doc To be called by the clientcode to disconnect from server.
%%      Socket is a port of which connect/1/2/3 has returned,
%%      If successful it will return ok.
%%
%% === Example ===
%% <div class="example">```
%% Connecting to localhost on port 5555 and then disconnecting.
%% 1> Socket = imup_client:connect(5555).
%% #Port<0.669>
%% 2> imup_client:disconnect(Socket)
%% ok'''
%% </div>      
%% @end
%%-------------------------------------------------------------------------
-spec disconnect(Socket) -> ok when
      Socket::port().
disconnect(Socket) ->
    send(Socket, disconnect),
    io:format("Disconnected from server. ~n", []).
    %%gen_tcp:close(Socket).



    
%%------------------------------
% Internal Functions
%%------------------------------

recv(Socket) ->
    {ok, BinData} = gen_tcp:recv(Socket, 0),
    Data = binary_to_term(BinData),
    %%io:format("Received: ~p~n", [Data]),

    case Data of
	{message, Msg} ->
	    io:format("Message received: ~p ~n", [Msg]),
	    recv(Socket);
	%% send to specific client code
	{ok, Message} ->
	    io:format("ok, ~p. ~n", [Message]),
	    recv(Socket);
	{error, Reason} ->
	    io:format("Error with reason: ~p ~n", [Reason]);
	disconnect ->
	    %%io:format("Disconnected from server.", []),
	    gen_tcp:close(Socket);
    	_ ->
    	    io:format("Unexpected data received. ~n", []),
	    recv(Socket)
    	    %%exit(unexpected_data_received)
    end.




recv(Socket, HandlerPID, ReceiverPID) ->
    %%{ok, BinData} = gen_tcp:recv(Socket, 0),
    case gen_tcp:recv(Socket, 0) of
	{ok, BinData} ->
	    Data = binary_to_term(BinData),
	    
	    case Data of
		{message, Msg} ->
		    io:format("Message received: ~p ~n", [Msg]),
		    ReceiverPID ! Msg,
		    recv(Socket, HandlerPID, ReceiverPID);
		%% send to specific client code
		{ok, Message} ->
		    io:format("ok, ~p. ~n", [Message]),
		    recv(Socket, HandlerPID, ReceiverPID);
		{uniqueid, UniqueID} ->
		    io:format("Set uniqueid to ~p ~n", [UniqueID]),
		    HandlerPID ! {set_uniqueid, UniqueID},
		    recv(Socket, HandlerPID, ReceiverPID);
		reconnected ->
		    io:format("Reconnected! ~n", []),
		    recv(Socket, HandlerPID, ReceiverPID);
		{error, Reason} ->
		    io:format("Error with reason: ~p ~n", [Reason]);
		disconnect ->
		    %%io:format("Disconnected from server.", []),
		    gen_tcp:close(Socket);
		_Dat ->
		    %%ReceiverPID ! {unexpected_data, Dat},
		    recv(Socket, HandlerPID, ReceiverPID)
	    end;
	{error, closed} ->
	    %% reconnect
	    HandlerPID ! {get_serv_info_uid, self()},
	    receive
		{serv_info_uid, ServInfo, UniqueID} ->
		    NewSocket = simpleConnect(ServInfo, 10),
		    
		    sendS(NewSocket, {reconnect, Socket, UniqueID}),

		    HandlerPID ! {update_socket, NewSocket},
		    recv(NewSocket, HandlerPID, ReceiverPID);
		_ ->
		    exit(noservinfo)
	    end;
	{error, Reason} ->
	    exit(Reason)
    end.


restarter(HandlerPID, RecvPID) ->
    receive
	{'EXIT', _PID, normal} ->
	    exit(normal);
	{'EXIT', PID, _Reason} ->
	    case PID of
		HandlerPID ->
		    exit(RecvPID, normal),
		    exit(handler_died);
		RecvPID ->
		    HandlerPID ! {get_all, self()},
		    receive
			{all, Socket, ServInfo} ->
			    NewReceiverPID = spawn_link(fun() -> recv(Socket, HandlerPID, ServInfo#server_info.recvpid) end),
			    restarter(HandlerPID, NewReceiverPID);
			_ ->
			    exit(noinfo)
		    end;
		_ ->
		    exit(unknown_pid)
	    end;
	_ ->
	    exit(unknown_message)
    end.



handler(Socket, ServInfo, UniqueID) -> 
    receive
	{get_socket, ReceiverPID} ->
	    ReceiverPID ! {socket, Socket},
	    handler(Socket, ServInfo, UniqueID);
	{update_socket, NewSocket} ->
	    handler(NewSocket, ServInfo, UniqueID);
	{get_serv_info_uid, ReceiverPID} ->
	    ReceiverPID ! {serv_info_uid, ServInfo, UniqueID},
	    handler(Socket, ServInfo, UniqueID);
	{get_all, ReceiverPID} ->
	    ReceiverPID ! {all, Socket, ServInfo},
	    handler(Socket, ServInfo, UniqueID);
	{set_uniqueid, NewUniqueID} ->
	    handler(Socket, ServInfo, NewUniqueID);
	{get_uniqueid, ReceiverPID} ->
	    ReceiverPID ! {uniqueid, UniqueID},
	    handler(Socket, ServInfo, UniqueID);
	_ ->
	    exit(unknown_question)
    end.


simpleConnect(ServInfo, N) when N > 0 ->
    case gen_tcp:connect(ServInfo#server_info.ip, ServInfo#server_info.port, ?TCP_OPTIONS) of
	{ok, Socket} ->
	    Socket;
	{error, Reason} ->
	    io:format("Error connecting to ~p on port ~p, with reason: ~p ~n", [ServInfo#server_info.ip, ServInfo#server_info.port, Reason]),
	    simpleConnect(ServInfo, N-1)
    end;
simpleConnect(_ServInfo, 0) ->
    exit(timed_out_connection).


    
