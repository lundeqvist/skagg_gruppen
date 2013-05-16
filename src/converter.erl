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
	    SIP = atom_to_list(ServerIP),
	    Port = list_to_integer(atom_to_list(PortNumber)),	    
	    TmpSocket = imup_client:connect(SIP,Port, self()),
	    imup_client:send(TmpSocket, {enter, PlayerID}),  
	    imup_client:send(TmpSocket, {join_game, GameID}),	    
	    imup_client:send(TmpSocket, {send_all, GameID, {get_data, users}}),	               
	    pong(Lists, TmpSocket);	
	
	
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

	{MailBox, GameID, PlayerID, {join_game}} ->
	    io:format("join game ~n", []),
	    imup_client:send(Socket, {join_game, GameID}),
	    pong(Lists, Socket);

	%%Detta ska skickas vidare till servern, från klient java
	{MailBox, GameID, PlayerID, Message} ->
	    io:format("Från client med gameID ",[]),
	    io:format(GameID,[]),
	    io:format("~n",[]),	    
	    imup_client:send(Socket,{send_all, GameID, Message}),
	    io:format("skickat vidare till servern ~n", []),
	    pong(Lists, Socket);	
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

