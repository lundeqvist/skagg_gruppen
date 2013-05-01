-module(converter).
-export([start/0,pong/1]).
     
pong(Lists) ->    
    receive
        stop ->
            io:format("Pong finished...~n",[]);         
        
 	{StartCommunicationWithJavaId,ID,Spel,ping} ->           
	    NewList = lists:append(Lists, [{{ID,Spel},StartCommunicationWithJavaId}]),
	    %%NewList = lists:append(Lists, [StartCommunicationWithJavaId]),
	    io:format("Ping~n",[]),
            StartCommunicationWithJavaId ! {self(),pong},
	    pong(NewList);

	%%Detta kommer från "servern"
	{a,Skruffs,NewPosition} ->
	    [MailBox ! {self(), NewPosition}|| {Tupeln, MailBox} <- Lists, Tupeln =:= Skruffs],
	    pong(Lists);

	{b, ID, Spel, Message} ->	   
	    List = [MailBox || {Tupeln, MailBox} <- Lists, Tupeln =:= {ID, Spel}],
	    io:format(Message,[]),
	    %%[{Tuple, MailBox}|_] = Lists,	    
	    [A] = List,
	    A ! {self(), Message},
	    pong(Lists);

	 
	{MailBox,1,Skruffs,NewPosition} ->
	    self() ! {a,Skruffs,NewPosition};  

	{MailBox, 2, {ID, SpelId},Message} ->
	    self() ! {b, ID, SpelId, Message},
	    pong(Lists);
	
	
	
	%%{1, {ID, gameID}, move/newPosition}
	%%{2, {ID, gameID}, chatMessage}
	%%{3, ID, startGame, co-player} 
	%%{4, ID, sendInvite, invited}
	%%{5, ID, inviteID, accept/dont accept}
	
	%%{6, ID, enter} -> när man loggar in.
	_ ->
	    io:format("error~n",[])
    end.
 
start() ->
    register(pong,spawn(converter,pong,[[]])).

