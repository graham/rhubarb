-module(server).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

listen(Port) ->
    {ok, Socket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(Socket).

accept(Socket) ->
    {ok, ClientSocket} = gen_tcp:accept(Socket),
    io:format("Connection Opened ~p.~n", [ClientSocket]),
    spawn( fun() -> handle_client_socket(ClientSocket) end ),
    accept(Socket).

handle_client_socket(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            gen_tcp:send(Socket, Data),
            handle_client_socket(Socket);
        {error, closed} ->
            io:format("Connection Closed ~p.~n", [Socket])
    end.

main(_) ->
    Server = listen(8000).
