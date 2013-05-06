-module(imup_listener).
-author('grupp4').


-export([listen/2]).

-define(TCP_OPTIONS, [binary, {packet, 2}, {active, false}, {reuseaddr, true}]).

listen(Port, DictPID) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    spawn_link(fun() -> accept(LSocket, DictPID) end).
		   



% Wait for incoming connections and spawn a process that will process incoming packets.
accept(LSocket, DictPID) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    Pid = spawn(fun() ->
			io:format("Connection accepted ~n", []),
			DictPID ! {insert, Socket, Socket},
			loop(Socket)
		end),
    gen_tcp:controlling_process(Socket, Pid),
    accept(LSocket, DictPID).

% Echo back whatever data we receive on Socket
loop(Sock) ->
    inet:setopts(Sock, [{active, once}]),
    receive
	{tcp, Socket, Data} ->
	    io:format("Got packet: ~p~n", [Data]),
	    %%FormatedData = processData(Data),
	    gen_tcp:send(Socket, Data),
	    loop(Socket);
	{tcp_closed, Socket} ->
	    io:format("Socket ~p closed~n", [Socket]);
	{tcp_error, Socket, Reason} ->
	    io:format("Error on socket ~p reason: ~p~n", [Socket, Reason])
    end.
