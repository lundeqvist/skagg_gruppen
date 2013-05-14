-module(imup_server).
-author('grupp4').

%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").

-compile([handler_test/0, start_test/0]). 

%%-behaviour(gen_server).
%% $ cd ../ebin
%%  $ erl -boot start_sasl
%%  ...
%%  1> appmon:start().
%%  {ok,<0.44.0>}
%%  2> application:start(tcp_server).
%%  ok



-export([start/1]).

-define(TCP_OPTIONS, [binary, {packet, 2}, {active, false}, {reuseaddr, true}]).
-define(PORT, 8080).



%%-------------------------------------------------------------------------
%% @spec (Socket) -> Pid | {error,Error}
%% @doc To be called by the user in order to start the server.
%%      If start/1 fails with Reason, the function returns {error,Reason}.
%%      If successful it will return the Pid of the socket.
%%
%% === Example ===
%% <div class="example">```
%% Starting a server which listens on port 5555.
%% 1> ServerSocket = imup_server:start(5555).
%% #Port<0.669>'''
%% </div>      
%% @end
%%-------------------------------------------------------------------------
-spec start(Port) -> pid() | tuple() when
      Port::integer().
start(Port) ->
    spawn(fun() ->
		  init(self(), Port)
	  end),
    receive
	{ok, ServerPort} ->
	    ServerPort;
	_ ->
	    {error, unable_to_start_server}
    end.

init(RPid, Port) ->
    process_flag(trap_exit, true),
    ClientDict = dict:new(),
    GamesDict = dict:new(),
    HandlerPID = spawn_link(fun() -> handler(ClientDict, GamesDict) end),
    SSocket = imup_listener:listen(Port, HandlerPID),
    RPid ! SSocket.
    
			    
    
		  


%%------------------------------
% Internal Functions
%%------------------------------

handler(Clients, Games) ->
    receive
	{insert_client, ReceiverPID, Socket, Alias} ->
	    TmpClients = dict:store(Socket, Alias, Clients),
	    TmpClients2 = dict:store(Alias, Socket, TmpClients),
	    ReceiverPID ! {ok, inserted},
	    handler(TmpClients2, Games);
	{remove_client, ReceiverPID, ClientPID} ->
	    {ok, CID} = dict:find(ClientPID, Clients),
	    TmpClients = dict:erase(ClientPID, Clients),
	    TmpClients2 = dict:erase(CID, TmpClients),
	    ReceiverPID ! {ok, removed},
	    handler(TmpClients2, Games);	    
	{get_client_id, ReceiverPID, ClientPID} ->
	    {ok , CID} = dict:find(ClientPID, Clients),
	    ReceiverPID ! {id, CID},
	    handler(Clients, Games);
	{get_client_pid, ReceiverPID, ClientID} ->
	    {ok, CPID} = dict:find(ClientID, Clients),
	    ReceiverPID ! {pid, CPID},
	    handler(Clients, Games);
	{host_game, ReceiverPID, HostPID, GameID} ->
	    {ok, HID} = dict:find(HostPID, Clients),
	    TmpGames = dict:append(GameID, HID, Games),
	    ReceiverPID ! {ok, hosting_game},
	    handler(Clients, TmpGames);
	{join_game, ReceiverPID, PlayerPID, GameID} ->
	    {ok, PID} = dict:find(PlayerPID, Clients),
	    TmpGames = dict:append(GameID, PID, Games),
	    ReceiverPID ! {ok, joined_game},
	    handler(Clients, TmpGames);
	{remove_game, ReceiverPID, GameID} ->
	    TmpGames = dict:erase(GameID, Games),
	    ReceiverPID ! {ok, game_removed},
	    handler(Clients, TmpGames);
	{get_players, ReceiverPID, GameID} ->
	    {ok, PList} = dict:find(GameID, Games),
	    SockList = [Cli || {ok, Cli} <- [dict:find(CID, Clients) || CID <- PList] ],
	    ReceiverPID ! {players, SockList},
	    handler(Clients, Games);
	{get_host, ReceiverPID, GameID} ->
	    {ok, [HID|_T]} = dict:find(GameID, Games),
	    {ok, HPID} = dict:find(HID, Clients),
	    ReceiverPID ! {host_is, HID, HPID},
	    handler(Clients, Games);
	{exit, Reason} ->
	    exit(Reason);
	done ->
	    {ok, done};
	_ ->
	    %%exit("Unexpected question")
	    {error, unexpected_question}
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                          %%
%%			   EUnit Test Cases                                 %%
%%                                                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_test() ->
    ?assertEqual(matrix, matrix).

%%    [?_assertEqual(N+1, length(seqs(N))) || N <- lists:seq(1, 55)].


handler_test() ->
    ClientDict = dict:new(),
    GamesDict = dict:new(),
    HandlerPID = spawn(fun() -> handler(ClientDict, GamesDict) end),
    {Socket1, ID1} = {sock1, id1},
    {Socket2, ID2} = {sock2, id2},
    GameID = theGAME,

    ReceiveFun = fun(Expected) ->
			 (receive 
			      Expected ->
				  Expected;
			      Any ->
				  {error, Any}
			  end)
		 end,
    


    HandlerPID ! {insert_client, self(), Socket1, ID1},
    {ok, _} = ReceiveFun({ok, inserted}),
    
    HandlerPID ! {insert_client, self(), Socket2, ID2},
    {ok, _} = ReceiveFun({ok, inserted}),

    HandlerPID ! {host_game, self(), Socket1, GameID},
    {ok, _} = ReceiveFun({ok, hosting_game}),

    HandlerPID ! {join_game, self(), Socket2, GameID},
    {ok, _} = ReceiveFun({ok, joined_game}),

    %% Test send_host
    HandlerPID ! {get_host, self(), GameID},
    receive
	{host_is, HID, HSOCK} ->
	    ?assertEqual(HID, ID1),
	    ?assertEqual(HSOCK, Socket1);
	_ ->
	    exit(send_host_failed)
    end,
    %%
    %% Test get_client_pid / get_client_id
    HandlerPID ! {get_client_id, self(), Socket1},
    receive
	{id, ID} ->
	    ?assertEqual(ID1, ID);
	_ ->
	    exit(get_client_id_failed)
    end,
    HandlerPID ! {get_client_pid, self(), ID1},
    receive
	{pid, PID} ->
	    ?assertEqual(Socket1, PID);
	_ ->
	    exit(get_client_pid_failed)
    end,
    %%
    %% Test get_players
    HandlerPID ! {get_players, self(), GameID},
    receive
	{players, SList} ->
	    SockList = [Socket1, Socket2],
	    [?_assertEqual(S1, S2) || S1 <- SList,
				     S2 <- SockList];
	_ ->
	    exit(get_players_failed)
    end.
    
	    
    
    
    
    
