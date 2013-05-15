-module(imup_tests).
-author('grupp4').

%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                          %%
%%			   EUnit Test Cases                                 %%
%%                                                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% general_test() ->
%%     RF = fun(Expected) ->
%% 			 (receive 
%% 			      Expected ->
%% 				  Expected;
%% 			      Any ->
%% 				  {error, Any}
%% 			  end)
%% 		 end,
%%     ReceiverFun = fun(Pid) ->
%% 			  F = (fun(F, Pid) ->
%% 				       (receive
%% 					    {ok, Message} ->
%% 						Pid ! {ok, Message},
%% 						F();
%% 					    done ->
%% 						done;
%% 					    _ ->
%% 						Pid ! {error, unexpected_data},
%% 						F()
%% 					end)
%% 			       end),
%% 			  F(F, Pid)
%% 		  end,

%%     RPid = spawn(fun() -> ReceiverFun(self()) end),
%%     imup_server:start(5555),
%%     Socket1 = imup_client:connect("localhost", 5555, RPid),
%%     Socket2 = imup_client:connect("localhost", 5555, RPid),

%%     imup_client:send(Socket1, {enter, pOne}),
%%     {ok, Ent1} = RF({ok, entered}),
%%     ?assertEqual(Ent1, entered).
 
    %% ?assertEqual(RF({ok, entered}), {ok, entered}),
    %% imup_client:send(Socket2, {enter, pTwo}),
    %% ?assertEqual(RF({ok, entered}), {ok, entered}).

    %% imup_client:send(Socket1, {host_game, theGAME}),
    %% ?assertEqual(RF({ok, hosting_game}), {ok, hosting_game}),

    %% imup_client:send(Socket2, {join_game, theGAME}),
    %% ?assertEqual(RF({ok, joined_game}), {ok, joined_game}),

    %% imup_client:send(Socket1, {send_to_id, pTwo, hello}),
    %% Next = receive
    %% 	hello ->
    %% 	    {ok, delivered};
    %% 	{ok, delivered} ->
    %% 	    hello;
    %% 	_ ->
    %% 	    {error, unexpected_data}
    %% end,
    %% ?assertEqual(RF(Next), Next).
    

%%?assertEqual(matrix, matrix).

%%    [?_assertEqual(N+1, length(seqs(N))) || N <- lists:seq(1, 55)].

