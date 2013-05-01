-module(imup_client).
-author('grupp4').

-export([send/2, send/3]).

send(PortNo, Message) ->
    {ok, Socket} = gen_tcp:connect("localhost", PortNo, [{active, false}, {packet, 2}]),
    gen_tcp:send(Socket, Message),
    A = gen_tcp:recv(Socket, 0),
    gen_tcp:close(Socket),
    A.

send(IP, PortNo, Message) ->
    {ok, Socket} = gen_tcp:connect(IP, PortNo, [{active, false}, {packet, 2}]),
    gen_tcp:send(Socket, Message),
    A = gen_tcp:recv(Socket, 0),
    gen_tcp:close(Socket),
    A.
    
