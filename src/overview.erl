%% @doc  I.M.U.P.
%% - Interactive multi-user program
%%
%%
%%
%%      If start/1 fails with Reason, the function returns {error,Reason}.
%%      If successful it will return the Pid of the socket.
%%
%% === Example ===
%% <div class="example">```
%% An example of a server with two clients,
%% connecting, entering, hosting a game,
%% joining a game, and sending various messages.
%%
%% 1> ServerSocket = imup_server:start(5555).
%% #Port<0.669>
%% 2> Socket1 = imup_client:connect(5555).
%% Connection accepted 
%% #Port<0.681>
%% 3> Socket2 = imup_client:connect(5555).
%% Connection accepted 
%% #Port<0.683>
%% 4> imup_client:send(Socket1, {enter, pOne}).
%% ok
%% ok, entered. 
%% 5> imup_client:send(Socket2, {enter, pTwo}).
%% ok
%% ok, entered. 
%% 6> imup_client:send(Socket1, {host_game, theGAME}).
%% ok
%% ok, hosting_game. 
%% 7> imup_client:send(Socket2, {join_game, theGAME}).
%% ok
%% ok, joined_game. 
%% 8> imup_client:send(Socket1, {send_all, theGAME, "hey everyone in theGAME!"}).
%% ok
%% Message received: "hey everyone in theGAME!" 
%% Message received: "hey everyone in theGAME!" 
%% ok, delivered. 
%% 9> imup_client:send(Socket1, {send_to_id, pTwo, "hello pTwo, how are you?"}).
%% ok
%% ok, delivered. 
%% Message received: "hello pTwo, how are you?" 
%% 10> imup_client:send(Socket2, {send_to_id, pOne, "hi pOne, I'm fine thanks."}).
%% ok
%% Message received: "hi pOne, I'm fine thanks." 
%% ok, delivered. 
%% 11> imup_client:send(Socket2, {send_host, theGAME, "so, what game is this guys?"}).
%% ok
%% Message received: "so, what game is this guys?" 
%% ok, delivered. 
%% 12> imup_client:send(Socket2, {send_host, theGAME, "so, what game should we play mr host?"}).
%% ok
%% Message received: "so, what game should we play mr host?" 
%% ok, delivered.'''
%% </div>      
%% @end
%%
%%
%%
%%
-module(overview).




