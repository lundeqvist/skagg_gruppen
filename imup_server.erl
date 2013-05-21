-module(imup_server).
-author('grupp4').

%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").

-compile([handler_test/0, start_test/0]). 



-export([start/1, stop/1]).

-define(TCP_OPTIONS, [binary, {packet, 2}, {active, false}, {reuseaddr, true}]).
-define(PORT, 5555).



%%-------------------------------------------------------------------------
%% @spec (Socket) -> Pid | {error,Error}
%% @doc To be called by the user in order to start the server.
%%      If start/1 fails with Reason, the function returns {error,Reason}.
%%      If init/1 returns {stop,Reason}, the process is
%%      terminated and the function returns {error,Reason}.
%% @end
%%-------------------------------------------------------------------------
-spec start(Port) -> tuple() when
      Port::integer().
start(Port) ->
    RPID = self(),
    spawn(fun() -> init(RPID, Port) end),
    receive
	{ok, LSocket} ->
	    {server_started, LSocket};
	{error, Reason} ->
	    exit(Reason);
	_ ->
	    exit(unable_to_start_server)
    end.

    
init(RPID, Port) ->		  
    process_flag(trap_exit, true),
    ClientDict = dict:new(),
    GamesDict = dict:new(),
    BackupPID = spawn_link(fun() -> backup(ClientDict, GamesDict) end),
    HandlerPID = spawn_link(fun() -> handler(ClientDict, GamesDict, BackupPID) end),
    ListenerPID = spawn_link(fun() -> imup_listener:listen(RPID, Port, HandlerPID) end),
    restarter(BackupPID, HandlerPID, ListenerPID).



%%-------------------------------------------------------------------------
%% @spec (Socket) -> ok
%% @doc To be called by the user in order to stop the server.
%% @end
%%-------------------------------------------------------------------------
-spec stop(SSock) -> ok when
      SSock::port().
stop(ServerSocket) ->
    gen_tcp:close(ServerSocket).


%%------------------------------
% Internal Functions
%%------------------------------
%%%%%%%%%%%%%%%%%%%%%%
%% Restart function %%
%%%%%%%%%%%%%%%%%%%%%%
restarter(BackupPID, HandlerPID, ListenerPID) ->
    receive
	{'EXIT', _PID, normal} ->
	    exit(normal);
	{'EXIT', PID, _Reason} ->
	    case PID of
		BackupPID ->
		    TmpDict = dict:new(),
		    NewBackupPID = spawn_link(fun() -> backup(TmpDict, TmpDict) end),
		    HandlerPID ! {backup, NewBackupPID},
		    restarter(NewBackupPID, HandlerPID, ListenerPID);
		HandlerPID ->
		    %%                   %%
		    % Pause server actions %
		    %%                   %%
		    ListenerPID ! pause,

		    TmpDict = dict:new(),
		    NewHandlerPID = spawn_link(fun() -> handler(TmpDict, TmpDict, BackupPID) end),
		    BackupPID ! {get_backups, HandlerPID},
		    %%%                               %%%
		    %% Communicate to update HandlerPID %
		    %%%                               %%%
		    ListenerPID ! {update_handler, NewHandlerPID},
		    
		    
		    %%                     %%
		    % Resume server actions %
		    %%                     %%
		    ListenerPID ! resume,

		    restarter(BackupPID, NewHandlerPID, ListenerPID);
		ListenerPID ->
		    exit(BackupPID, normal),
		    exit(HandlerPID, normal),
		    io:format("Server has failed. ~n", []),
		    exit(normal);
		_ ->
		    exit(unknown_pid)
	    end;
	Reason -> 
	    exit(Reason)
    end.



%%%%%%%%%%%%%%%%%%%%%%
%% Handler Function %%
%%%%%%%%%%%%%%%%%%%%%%
handler(Clients, Games, BackupPID) ->
    receive
	{insert_client, ReceiverPID, Socket, Alias} ->
	    TmpClients = dict:store(Socket, Alias, Clients),
	    TmpClients2 = dict:store(Alias, Socket, TmpClients),
	    ReceiverPID ! {ok, inserted},

	    BackupPID ! {update_clients, TmpClients2},
	    handler(TmpClients2, Games, BackupPID);
	{remove_client, ReceiverPID, ClientPID} ->
	    {ok, CID} = dict:find(ClientPID, Clients),
	    TmpClients = dict:erase(ClientPID, Clients),
	    TmpClients2 = dict:erase(CID, TmpClients),
	    ReceiverPID ! {ok, removed},

	    BackupPID ! {update_clients, TmpClients2},
	    handler(TmpClients2, Games, BackupPID);	    
	{get_client_id, ReceiverPID, ClientPID} ->
	    {ok , CID} = dict:find(ClientPID, Clients),
	    ReceiverPID ! {id, CID},
	    handler(Clients, Games, BackupPID);
	{get_client_pid, ReceiverPID, ClientID} ->
	    {ok, CPID} = dict:find(ClientID, Clients),
	    ReceiverPID ! {pid, CPID},
	    handler(Clients, Games, BackupPID);
	{host_game, ReceiverPID, HostPID, GameID} ->
	    {ok, HID} = dict:find(HostPID, Clients),
	    TmpGames = dict:append(GameID, HID, Games),
	    ReceiverPID ! {ok, hosting_game},

	    BackupPID ! {update_games, TmpGames},
	    handler(Clients, TmpGames, BackupPID);
	{join_game, ReceiverPID, PlayerPID, GameID} ->
	    {ok, PID} = dict:find(PlayerPID, Clients),
	    TmpGames = dict:append(GameID, PID, Games),
	    ReceiverPID ! {ok, joined_game},

	    BackupPID ! {update_games, TmpGames},
	    handler(Clients, TmpGames, BackupPID);
	{remove_game, ReceiverPID, GameID} ->
	    TmpGames = dict:erase(GameID, Games),
	    ReceiverPID ! {ok, game_removed},

	    BackupPID ! {update_games, TmpGames},
	    handler(Clients, TmpGames, BackupPID);
	{get_players_pids, ReceiverPID, GameID} ->
	    {ok, PList} = dict:find(GameID, Games),
	    SockList = [Cli || {ok, Cli} <- [dict:find(CID, Clients) || CID <- PList] ],
	    ReceiverPID ! {players, SockList},
	    handler(Clients, Games, BackupPID);
	{get_players_ids, ReceiverPID, GameID} ->
	    {ok, PList} = dict:find(GameID, Games),

	    ReceiverPID ! {players, PList},
	    handler(Clients, Games, BackupPID);
	{get_players, ReceiverPID, GameID} ->
	    {ok, PList} = dict:find(GameID, Games),
	    SockList = [{Cli,Cid} || {{ok, Cli}, Cid} <- [{dict:find(CID, Clients),CID} || CID <- PList] ],
	    %% {Socket, ClientID} %%
	    ReceiverPID ! {players, SockList},
	    handler(Clients, Games, BackupPID);	    
	{get_host, ReceiverPID, GameID} ->
	    {ok, [HID|_T]} = dict:find(GameID, Games),
	    {ok, HPID} = dict:find(HID, Clients),
	    ReceiverPID ! {host_is, HID, HPID},
	    handler(Clients, Games, BackupPID);
	{backup, NewBackupPID} ->
	    NewBackupPID ! {update_all, Clients, Games},
	    handler(Clients, Games, NewBackupPID);
	{update_backup_pid, NewBackupPID} ->
	    handler(Clients, Games, NewBackupPID);
	{receive_backup, NewClients, NewGames} ->
	    handler(NewClients, NewGames, BackupPID);
	{exit, Reason} ->
	    exit(Reason);
	done ->
	    exit(normal);
	_ ->
	    %%exit("Unexpected question")
	    exit(unexpected_question) %%{error, unexpected_question}
    end.


%%%%%%%%%%%%%%%%%%%%%
%% Backup Function %%
%%%%%%%%%%%%%%%%%%%%%
backup(Clients, Games) ->
    receive
	{update_clients, NewClients} ->
	    backup(NewClients, Games);
	{update_games, NewGames} ->
	    backup(Clients, NewGames);
	{get_backups, ReceiverPID} ->
	    ReceiverPID ! {receive_backup, Clients, Games},
	    backup(Clients, Games);
	{update_all, NewClients, NewGames} ->
	    backup(NewClients, NewGames);
	_ ->
	    exit(unexpected_question)
    end.

	  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                          %%
%%			   EUnit Test Cases                                 %%
%%                                                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_test() ->
    ?assertEqual(matrix, matrix).

%%    [?_assertEqual(N+1, length(seqs(N))) || N <- lists:seq(1, 55)].


handler_backup_test() ->
    ClientDict = dict:new(),
    GamesDict = dict:new(),
    BackupPID = spawn_link(fun() -> backup(ClientDict, GamesDict) end),
    HandlerPID = spawn(fun() -> handler(ClientDict, GamesDict, BackupPID) end),
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
