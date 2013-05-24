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
	    io:format("GameID: ",[]),
	    io:format(GameID,[]),
	    io:format("~n",[]),
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
	
	%%Detta kommer från "servern"
	{GameID, PlayerID, Message} ->	  
	    io:format("Message received from server from: ",[]),
	    io:format(GameID,[]),
	    io:format("~n",[]),
	    List = [MailBox || {X, MailBox} <- Lists, X =:= {GameID, PlayerID}],
	    if List =:= [] ->
		    List1 = [MailBox || {{X,Y}, MailBox} <- Lists, X =:= GameID],
		    if List1 =:= [] ->
			    io:format("Mailbox dont exists with playerID and gameID~n",[]),
			    io:format(PlayerID,[]),
			    io:format("~n",[]),
			    io:format(GameID,[]),
			    io:format("~n",[]),
			    List2 = [MailBox || {{X,Y}, MailBox} <- Lists, X =:= gameMenu],
			    [A] = List2,
			    A ! {self(), GameID, PlayerID, Message},
			    pong(Lists, Socket);
		       true ->
			    io:format("Mailbox exists with gameID~n",[]),
			    io:format(GameID,[]),
			    io:format("~n",[]),
			    [A|_] = List1,
			    A ! {self(), GameID, PlayerID, Message},
			    pong(Lists, Socket)		
		    end;
	       true ->
		    io:format("Mailbox exists with playerID and gameID~n",[]),
		    io:format(PlayerID,[]),
		    io:format("~n",[]),
		    io:format(GameID,[]),
		    io:format("~n",[]),
		    [A] = List,
		    A ! {self(), GameID, PlayerID, Message},
		    pong(Lists, Socket)	    
	    end;			        

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%Detta ska skickas vidare till servern, från klient java
	{MailBox, GameID, PlayerID, Message, OtherPlayers} ->
	    io:format("Invite sent~n", []),
	    imup_client:send(Socket, {send_to_users, OtherPlayers, {GameID, PlayerID, Message}}),
	    pong(Lists, Socket);
	
	{MailBox, GameID, PlayerID, {join_game}} ->
	    io:format("join game ~n", []),
	    io:format(PlayerID, []), 
	    io:format("~n",[]),
	    imup_client:send(Socket, {join_game, GameID}),
	    pong(Lists, Socket);

	{MailBox, GameID, PlayerID, {remove_player}} ->
	    io:format("Remove player~n",[]),
	    imup_client:send(Socket, {remove_player, GameID, PlayerID}),
	    io:format("Disconnect~n",[]),
	    imup_client:send(Socket, {send_all, GameID, {get_data, users}}),
	    imup_client:send(Socket, disconnect),
	    exit(normal);
	    %%pong(Lists, Socket);	

	%%{MailBox, GameID, PlayerID, {disconnect}} ->
	    %%io:format("Disconnect~n",[]),
	    %%imup_client:send(Socket, {send_all, GameID, {get_data, users}}),
	    %%imup_client:send(Socket, disconnect),
	   %% exit(normal);
	%%pong(Lists, Socket);


	{MailBox, GameID, PlayerID, Message} ->
	    io:format("Från client med gameID ",[]),
	    io:format(GameID,[]),
	    io:format("~n",[]),	    
	    imup_client:send(Socket,{send_all, GameID, {GameID, PlayerID, Message}}),
	    pong(Lists, Socket);	
	
	_ ->
	    io:format("error~n",[])
    end.
 
start() ->
    register(pong,spawn(converter,pong,[[], null])).

