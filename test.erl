#! /usr/bin/env escript
% Whatever! 10.0

-import(data_bool).
-import(data_binary).
-import(data_list).
-import(data_number).
-import(make).

-include_lib("eunit/include/eunit.hrl").

main(_) ->
    make:all(),
    io:format("  Bool:   "),
    data_bool:test(),
    io:format("  List:   "),
    data_list:test(),
    io:format("  Binary: "),
    data_binary:test(),
    io:format("  Number: "),
    data_number:test(),
    
    io:format("~n"),
    io:format("  Key:    "),
    key:test().

       
