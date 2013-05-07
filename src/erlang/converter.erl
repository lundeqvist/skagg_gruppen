-module(converter).
-export([start/0,pong/1]).
     
pong(Lists) ->    
    receive
        stop ->
            io:format("Pong finished...~n",[]);         
        
 	{StartCommunicationWithJavaId,ID,GameID,ping} ->           
	    NewList = dict:append({ID, GameID}, StartCommunicationWithJavaId, Lists),
	    io:format("Ping~n",[]),
            StartCommunicationWithJavaId ! {self(),pong},    %% 
	    pong(NewList);

	%% Detta kommer från "servern"
	{a,Skruffs,NewPosition} ->
	    [MailBox ! {self(), NewPosition}|| {Tupeln, MailBox} <- Lists, Tupeln =:= Skruffs],
	    pong(Lists);

	{b, PlayerID, GameID, Message} ->	   
	    RecipientID = dict:find({PlayerID, GameID}, Lists),
	    RecipientID ! {self(), Message},
	    pong(Lists);
		
	{c, RecipientID, InviterID, Message} ->
		RecipientID ! {self(), InviterID, Message},
		pong(Lists);
	
	{d, InviterID, Message} ->
		InviterID ! {self(), Message},
		pong(Lists);
	
	
	%% Detta kommer ifrån klienter/hosts
	{MailBox, "send_moveRequest",{PlayerID, GameID}, newPosition} ->
		self() ! {send_host, GameID, {a, PlayerID, GameID, newPosition}};
		
	{Mailbox, "send_chatLine",{PlayerID, GameID}, chatLine} ->
		self() ! {send_all, GameID, {b, PlayerID, GameID, chatLine}};
		
	{Mailbox, "send_invite", {PlayerID, Game}, InvitedPlayerID} ->
		self() ! {send_to_id, InvitedPlayerID, {c, PlayerID, Game, "send_invite"}};
		
	{Mailbox, "send_replyToInvite", {PlayerID, InviterID}, answer} ->
		self() ! {send_to_id, InviterID, {d, PlayerID, answer}};
		
		
	
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

