-module(imup_client).
-author('grupp4').

-export([connect/1, connect/2, connect/3, disconnect/1, send/2]).


-define(TCP_OPTIONS, [binary, {packet, 2}, {active, false}, {reuseaddr, true}]).

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
    {ok, Socket} = gen_tcp:connect("localhost", PortNo, ?TCP_OPTIONS),
    RPID = spawn_link(fun() -> recv(Socket) end),
    Socket.


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
    {ok, Socket} = gen_tcp:connect(IP, PortNo, [binary, {active, false}, {packet, 2}]),
    RPID = spawn_link(fun() -> recv(Socket) end),
    Socket.



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
-spec connect(IP, Port, ReceiverPID) -> port() | tuple() when
      IP::string() | tuple(),
      Port::integer(),
      ReceiverPID::pid().
connect(IP, PortNo, ReceiverPID) ->
    {ok, Socket} = gen_tcp:connect(IP, PortNo, [binary, {active, false}, {packet, 2}]),
    RPID = spawn_link(fun() -> recv(Socket, ReceiverPID) end),
    Socket.


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
%% 2> imup_client:send(Socket, {this,can,be,anything}).
%% ok'''
%% </div>      
%% @end
%%-------------------------------------------------------------------------
-spec send(Socket, Message) -> ok | tuple() when
      Socket::port(),
      Message::any().
send(Socket, Message) ->
    BinMsg = term_to_binary(Message),
    gen_tcp:send(Socket, BinMsg).
%%    {ok, A} = gen_tcp:recv(Socket, 0),
    %%A.




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




recv(Socket, ReceiverPID) ->
    {ok, BinData} = gen_tcp:recv(Socket, 0),
    Data = binary_to_term(BinData),
    %%io:format("Received: ~p~n", [Data]),

    case Data of
	{message, Msg} ->
	    %%io:format("Message received: ~p ~n", [Msg]),
	    ReceiverPID ! Msg,
	    recv(Socket, ReceiverPID);
	%% send to specific client code
	{ok, Message} ->
	    %%io:format("ok, ~p. ~n", [Message]),
	    ReceiverPID ! {ok, Message},
	    recv(Socket, ReceiverPID);
	{error, Reason} ->
	    io:format("Error with reason: ~p ~n", [Reason]);
	disconnect ->
	    %%io:format("Disconnected from server.", []),
	    gen_tcp:close(Socket);
    	Dat ->
	    ReceiverPID ! {unexpected_data, Dat},
	    recv(Socket, ReceiverPID)
    end.

    
