#! /usr/bin/env escript
%%! -smp enable

% Whatever!

-import(make).
-import(parser_plain).

-include_lib("eunit/include/eunit.hrl").

main(_) ->
    make:all(),

    Bucket = bucket:start_bucket("Testing"),
    Command = parser_plain:parse_text(<<"mykey\nrpush\n100">>),

    ok.
