-module(converter).
-export([start/0,pong/1]).
	 
pong(Lists) ->    
	receive
    	stop ->
        	io:format("Pong finished...~n",[]);    	 
   	 
	 {StartCommunicationWithJavaId,GameID, PlayerID, ping} ->      	 
    	NewList = lists:append(Lists, [{{GameID, PlayerID},StartCommunicationWithJavaId}]),
    	io:format("Ping~n",[]),
        	StartCommunicationWithJavaId ! {self(),pong},
    	pong(NewList);

    %%Detta kommer fr�n "servern"
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    {GameID, PlayerID, Message} ->       
    	List = [MailBox || {Tupeln, MailBox} <- Lists, Tupeln =:= {GameID, PlayerID}],
    	io:format(Message,[]),
    	%%[{Tuple, MailBox}|_] = Lists,   	 
    	[A] = List,
    	A ! {self(), GameID, PlayerID, Message},
    	pong(Lists);

   	 
    %% FR�N JAVAKLIENTER && JAVAHOSTS
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    {MailBox,GameID, PlayerID, NewPosition} ->
    	self() ! {GameID, PlayerID, NewPosition}, 
    	pong(Lists);
   	 
    %% {MailBox, GameID, PlayerID, {PlayerID, ServerIP, PortNumber, ClientIP}} ->
   	 %% VIKTORS KOD, vad som nu beh�vs skickas till servern f�r att logga in
   	 
%%    {MailBox, GameID, host, {ServerIP, PortNumber, HostIP}} ->
   	 %%loggin f�r host p� server
   	 
    
    

    _ ->
    	io:format("error~n",[])
	end.
 
start() ->
	register(pong,spawn(converter,pong,[[]])).


