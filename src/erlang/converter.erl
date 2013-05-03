-module(converter).
-export([start/0,pong/1]).
     
pong(Lists) ->    
    receive
        stop ->
            io:format("Pong finished...~n",[]);         
        
 	{StartCommunicationWithJavaId,ID,GameID,ping} ->           
	    NewList = append({PlayerID, GameID}, StartCommunicationWithJavaId, Lists),
	    io:format("Ping~n",[]),
            StartCommunicationWithJavaId ! {self(),pong},    %% 
	    pong(NewList);

	%%Detta kommer från "servern"
	{a,Skruffs,NewPosition} ->
	    [MailBox ! {self(), NewPosition}|| {Tupeln, MailBox} <- Lists, Tupeln =:= Skruffs],
	    pong(Lists);

	{b, PlayerID, GameID, Message} ->	   
	    Recipient = find({PlayerID, GameID}, Lists),
	    Recipient ! {self(), Message},
	    pong(Lists);

	{MailBox, "send_moveRequest",{PlayerID, GameID}, newPosition} ->
		self() ! {send_host, GameID, {a, PlayerID, GameID, newPosition}};
		
	{Mailbox, "send_chatLine",{PlayerID, GameID}, chatLine} ->
		self() ! {send_all, GameID, {b, PlayerID, GameID}, chatLine};
		
	
		
		
	
%%	{MailBox,1,Skruffs,NewPosition} ->
%%	    self() ! {};  

	{MailBox, 2, {PlayerID, GameId},Message} ->
	    self() ! {b, PlayerID, GameId, Message},
	    pong(Lists);
	
	
	
	%%{1, {PlayerID, gameID}, move/newPosition}
	%%{2, {PlayerID, gameID}, chatMessage}
	%%{3, PlayerID, startGame, co-player} 
	%%{4, PlayerID, sendInvite, invited}
	%%{5, PlayerID, inviteID, accept/dont accept}
	
	%%{6, ID, enter} -> när man loggar in.
	_ ->
	    io:format("error~n",[])
    end.
 
start() ->
    register(pong,spawn(converter,pong,[[]])).

