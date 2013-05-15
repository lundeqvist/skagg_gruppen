-module(converter).
-export([start/0,pong/2]).
     
pong(Lists, Socket) ->    
    receive
        stop ->
            io:format("Pong finished...~n",[]);         
        
	%%This is the comunication mailbox to java
 	{ComMailbox, GameID, PlayerID, ping} ->           
	    NewList = lists:append(Lists, [{{GameID, PlayerID},ComMailbox}]),   
	    io:format("Ping~n",[]),
            ComMailbox ! {self(),pong},	   	   
	    pong(NewList, Socket);
	
	%%When communication with the server can start
	{MailBox, GameID, PlayerID, {ServerIP, PortNumber, ClientIP}} ->
	    io:format("Client-login på server~n", []),
	    io:format(GameID, []),
	    io:format("~n",[]),
	    io:format(PlayerID, []),
	    io:format("~n",[]),
	    io:format(ServerIP, []),
	    io:format("~n",[]),
	    io:format(PortNumber, []),
	    io:format("~n",[]),
	    io:format(ClientIP, []),
	    io:format("~n",[]),
	    Bool = is_atom(ServerIP),
	    io:format(Bool, []),
	    SIP = atom_to_list(ServerIP),
	    %%erlang:string_atom(SIP, ServerIP),
	    %%atom_to_list(ServerIP),
	    Bool2 = is_list(SIP),
	    
	    io:format(Bool2,[]),

	    Port = list_to_integer(atom_to_list(PortNumber)),
	    %%Testar med localhost istället för serverip
	    %%imup_client:connect("127.0.0.1", 5555, self()),
	    
	    TmpSocket = imup_client:connect(SIP, 5555, self()),

	    %%io:format("After connect ~n", []),
	    imup_client:send(TmpSocket, {enter, PlayerID}),
	    imup_client:send(TmpSocket, {join_game, GameID}),
	    imup_client:send(TmpSocket, {get_users, GameID}),	        
	    pong(Lists, TmpSocket);
	%% VIKTORS KOD, vad som nu behövs skickas till servern för att logga in
	
	%%{MailBox, GameID, host, {ServerIP, PortNumber, HostIP}} ->
	  %%  io:format("Host-login på server~n", []),
	    
	%%loggin för host på server
	%%Socket = imup_client:connect(ServerIP, PortNumber, self()),
	%%imup_client:send(Socket, {enter, PlayerID}),
	%%imup_client:send(Socket, {host_game, GameID}),
	%%imup_client:send(Socket, {get_users, GameID}),
	%%pong(Lists);
	
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%Detta kommer från "servern"
	{GameID, PlayerID, Message} ->	   
	    List = [MailBox || {X, MailBox} <- Lists, X =:= {GameID, PlayerID}],
	    io:format("From server", []),
	    io:format("~n", []),
	    io:format(GameID, []),
	    io:format("~n", []),
	    io:format(PlayerID, []),
	    io:format("~n~n", []),
	    [A] = List,
	    A ! {self(), GameID, PlayerID, Message},
	    pong(Lists, Socket);
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	

	


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%från host
	%%när man inte får flytta ska man få ett meddelande till chattrutan i det fönstret



	%%Detta ska skickas vidare till servern, från klient java
	{MailBox, GameID, PlayerID, NewPosition} ->
	    io:format(GameID,[]),
	    %%self() ! {send_host, SpelId,{a,ID, SpelId, NewPosition}},
	    imup_client:send(Socket,{GameID, PlayerID, NewPosition}),
	    pong(Lists, Socket);
	
	%%{MailBox, chatt, {ID, SpelId},Message} ->
	  %%  self() ! {send_all, SpelId{ID, SpelId, Message}},
	    %%pong(Lists);
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		
	%%{1, {ID, gameID}, move/newPosition}
	%%{2, {ID, gameID}, chatMessage}
	%%{3, ID, startGame, co-player} 
	%%{4, ID, sendInvite, invited}
	%%{5, ID, inviteID, accept/dont accept}	
	%%{6, ID, enter} -> när man loggar in.
	





	%%
	_ ->
	    io:format("error~n",[])
    end.
 
start() ->
    register(pong,spawn(converter,pong,[[], null])).

