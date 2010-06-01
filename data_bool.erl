-module(data_bool).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

default() ->
    false.

do_action(Action, nil, Data) ->
    do_action(Action, default(), Data);

% Returns a {Response, NewValue}.
do_action(Action, OldValue, Data) ->
    case Action of 
        get ->
            {OldValue, OldValue};
        set ->
            case Data of 
                0 ->
                    {1, false};
                false ->
                    {1, false};
                nil ->
                    {1, false};
                _ ->
                    {1, true}
            end
    end.

get_set_test() ->
    ?assertEqual( do_action( get, nil, nil ), {false, false} ),
    ?assertEqual( do_action( set, false, true ), {1, true} ),
    ?assertEqual( do_action( set, false, 123 ), {1, true} ).
