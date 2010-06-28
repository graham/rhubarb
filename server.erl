-module(server).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

listen(Port) ->
    {ok, Socket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(Socket).

accept(Socket) ->
    {ok, ClientSocket} = gen_tcp:accept(Socket),
    spawn( fun() -> handle_client_socket(ClientSocket) end ),
    accept(Socket).

handle_client_socket(ClientSocket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            gen_tcp:send(Socket, Data),
            handle_client_socket(ClientSocket);
        {error, closed} ->
            ok
    end.

