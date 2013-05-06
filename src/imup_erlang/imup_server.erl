-module(imup_server).
-author('grupp4').

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


start(Port) ->
    process_flag(trap_exit, true),
    Dict = dict:new(),
    DictPID = spawn_link(fun() -> clients(Dict) end),
    imup_listener:listen(Port, DictPID).
    
    %%{ok, LSocket} = gen_tcp:
				 


%% % Call imup_server:listen() to start the server.
%% start2(Port) ->
%%     Dict = dict:new(), % Client dictionary
%%     DictPID = spawn_link(fun() -> clients(Dict) end),
%%     {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
%%     spawn_link(fun() -> accept(LSocket, DictPID) end),
%%     receive
%% 	{'EXIT', PID, Reason} ->
%% 	    io:format("~p Process ~w terminated with reason ~w!~n", [self(), PID, Reason])
%%     end.



%%------------------------------
% Internal Functions
%%------------------------------
processData(Data) ->
    ok.

clients(Dict) ->
    receive
	{insert, Socket, Alias} ->
	    clients(dict:append(Socket, Alias, Dict));
	clients ->
	    dict:to_list(Dict),
	    clients(Dict)
    end.


errorHandling() ->
    receive
 	{'EXIT', PID, Reason} ->
 	    io:format("~p Process ~w terminated with reason ~w!~n", [self(), PID, Reason])	
    end.

