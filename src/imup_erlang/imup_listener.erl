-module(imup_listener).
-author('grupp4').


-export([listen/2]).

-define(TCP_OPTIONS, [binary, {packet, 2}, {active, false}, {reuseaddr, true}]).



%%-------------------------------------------------------------------------
%% @spec (Port, HandlerPID) -> Pid | {error,Error}
%% @doc To be called by the main server process to be able to listen to port Port and HandlerPID will receive the messages.
%%      If listen/2 fails with Reason, the function returns {error,Reason}.
%%      If successful it will return the Pid of the socket.
%%
%% === Example ===
%% <div class="example">```
%% HandlerPID = Process which can handle messages
%% 1> ServerSocket = imup_listen(5555, HandlerPID)
%% #Port<0.669>'''
%% </div>      
%% @end
%%-------------------------------------------------------------------------
-spec listen(Port, HandlerPID) -> pid() | tuple() when
      Port::integer(),
      HandlerPID::pid().
listen(Port, HandlerPID) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    spawn_link(fun() -> accept(LSocket, HandlerPID) end),
    LSocket.
		   



% Wait for incoming connections and spawn a process that will process incoming packets.
accept(LSocket, HandlerPID) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    Pid = spawn(fun() ->
			io:format("Connection accepted ~n", []),
			loop(Socket, HandlerPID)
		end),
    gen_tcp:controlling_process(Socket, Pid),
    accept(LSocket, HandlerPID).



% Echo back whatever data we receive on Socket
loop(Sock, HandlerPID) ->
    inet:setopts(Sock, [{active, once}]),
    receive
	{tcp, Socket, Data} ->
	    %%io:format("Got packet: ~p == ", [Data]),
	    FormatedData = process_data(Socket, Data, HandlerPID),
	    %%io:format("~p~n", [FormatedData]),

	    convey_message(Socket, FormatedData),

	    loop(Socket, HandlerPID);
	{tcp_closed, Socket} ->
	    io:format("Socket ~p closed~n", [Socket]);
	{tcp_error, Socket, Reason} ->
	    io:format("Error on socket ~p reason: ~p~n", [Socket, Reason])
    end.




%%------------------------------
% Internal Functions
%%------------------------------


-spec process_data(S, D, P) -> term() when
      S::port(),
      D::binary(),
      P::pid().
process_data(Socket, Data, HandlerPID) when is_binary(Data) ->
    case binary_to_term(Data) of
	{send_host, GameID, Msg} ->
	    HandlerPID ! {get_host, self(), GameID},
	    receive
		{host_is, _HID, HSOCK} ->
		    convey_message(HSOCK, {message, Msg}),
		    {ok, delivered};
		_ ->
		    {error, nohost}
	    end;
	{send_all, GameID, Msg} ->
	    HandlerPID ! {get_players, self(), GameID},
	    receive
		{players, SList} ->
		    TmpFun = fun(S) ->
		    		     convey_message(S, {message, Msg})
		    	     end,
		    lists:foreach(TmpFun, SList),
		    %%[convey_message(Sock, {message, Msg}) || Sock <- SList],
		    {ok, delivered};
		_ ->
		    {error, noplayers}
	    end;
	{send_to_id, ReceiverID, Msg} ->
	    HandlerPID ! {get_client_pid, self(), ReceiverID},
	    receive
	       {pid, SockPID} ->
		    convey_message(SockPID, {message, Msg}),
		    {ok, delivered};
		_ ->
		    {error, noid}
	    end;
	{host_game, GameID} ->
	    HandlerPID ! {host_game, self(), Socket, GameID},
	    receive
		{ok, hosting_game} ->
		    {ok, hosting_game};
		_ ->
		    {error, not_hosting}
	    end;	    
	{join_game, GameID} ->
	    HandlerPID ! {join_game, self(), Socket, GameID},
	    receive
		{ok, joined_game} ->
		    {ok, joined_game};
		_ ->
		    {error, not_joined}
	    end;
	{start_game, _GameID} ->
	    tbi;
	{end_game, GameID} ->
	    HandlerPID ! {remove_game, self(), GameID},
	    receive
		{ok, game_removed} ->
		    {ok, game_ended};
		_ ->
		    {error, game_not_ended}
	    end;
	{enter, SenderID} ->
	    HandlerPID ! {insert_client, self(), Socket, SenderID},
	    receive
		{ok, inserted} ->
		    {ok, entered};
		_ ->
		    {error, not_entered}
	    end;
	disconnect ->
	    HandlerPID ! {remove_client, self(), Socket},
	    receive
		{ok, removed} ->
		    disconnect;
		_ ->
		    {error, not_disconnected}
	    end;
	Dat ->
	    Dat
    end;
process_data(_Socket, Data, _DictPID) ->
    Data.



convey_message(Socket, Data) when is_binary(Data) ->
    gen_tcp:send(Socket, Data);
convey_message(Socket, Data) ->
    BinData = term_to_binary(Data),
    gen_tcp:send(Socket, BinData).



