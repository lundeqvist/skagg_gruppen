-module(test).

-export([testing/0]).


testing() ->
	HerpaPID = spawn(fun() -> ghey("Herpa") end),
	DerpaPID = spawn(fun() -> ghey("Derpa") end),
	Pid = spawn(fun() -> testR(HerpaPID, DerpaPID) end),
	Pid.
	
testR(HerpaPID, DerpaPID) ->
	receive
		sleepyherpa ->
			erlang:suspend_process(HerpaPID),
			testR(HerpaPID, DerpaPID);
		sleepyderpa ->
			erlang:suspend_process(DerpaPID),
			testR(HerpaPID, DerpaPID);
		wakeyherpa ->
			erlang:resume_process(HerpaPID),
			testR(HerpaPID, DerpaPID);
		wakeyderpa ->
			erlang:resume_process(DerpaPID),
			testR(HerpaPID, DerpaPID);
		_ ->
			exit(fack)			
	end.
	
ghey(Message) ->
	io:format("~p ~n", [Message]),
	timer:sleep(1000),
	ghey(Message).
