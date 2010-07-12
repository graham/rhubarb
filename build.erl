#! /usr/bin/env escript
% Whatever!

-import(make).

-include_lib("eunit/include/eunit.hrl").

main(Args) ->
    io:format("Args: ~p~n", [Args]),

    case lists:member("clean", Args) of
        true ->
            run("rm *.beam");
        false ->
            ok
    end,
    
    case lists:member("build", Args) of
        true ->
            make:all();
        false ->
            ok
    end,

    case lists:member("add", Args) of
        true ->
            add_files_to_emake(lists:split(1, Args));
        false ->
            ok
    end,
    
    io:format("Erlang Builder 0.2~n").


add_files_to_emake(_Filelist) ->
    ok.

% Found these nice run shell script functions here:
% http://piotrga.wordpress.com/2010/04/02/how-to-run-a-system-command-in-erlang/
% Thanks!

run(Cmd) ->
    run(Cmd, 60000).

run(Cmd, Timeout) ->
    
    Port = erlang:open_port({spawn, Cmd},[exit_status]),
    loop(Port,[], Timeout).

loop(Port, Data, Timeout) ->
    receive
        {Port, {data, NewData}} ->
            loop(Port, Data++NewData, Timeout);
        {Port, {exit_status, 0}} -> Data;
        {Port, {exit_status, S}} -> throw({commandfailed, S})
    after Timeout ->
            throw(timeout)
    end.
