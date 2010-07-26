#! /usr/bin/env escript
% Whatever! 10.0

-import(data_bool).
-import(data_binary).
-import(data_list).
-import(data_number).
-import(bucket).

-import(parser_plain).

-import(make).

-include_lib("eunit/include/eunit.hrl").

main(_) ->
    make:all(),
    
    diskio:start(),
    
    io:format("Data Types:~n"),
    io:format("  Bool:   "),
    data_bool:test(),
    io:format("  List:   "),
    data_list:test(),
    io:format("  Binary: "),
    data_binary:test(),
    io:format("  Number: "),
    data_number:test(),

    io:format("~n"),
    io:format("Command Parsers:~n"),
    io:format("  Parser Plain: "),
    
    io:format("~n"),
    io:format("  Bucket: "),
    bucket:test(),

    io:format("~n"),
    io:format("  Key:    "),
    key:test().

       
