-module(data_list).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

default() ->
    [].

do_action(Action, nil, Data) ->
    do_action(Action, default(), Data);

% Returns a {Response, NewValue}.
do_action(Action, OldValue, Data) ->
    case Action of 
        rpush ->
            NewData = lists:append(OldValue, lists:flatten([Data])),
            {length(NewData), NewData};
        rpop ->
            Length = length(OldValue),
            case Length of
                _ when Length > 0 ->
                    {NewData, [Split]} = lists:split(Length - 1, OldValue);
                0 ->
                    {NewData, Split} = {[], nil}
            end,
            {Split, NewData};
        brpop ->
            Length = length(OldValue),
            case Length of
                _ when Length > 0 ->
                    {NewData, [Split]} = lists:split(Length - 1, OldValue),
                    {Split, NewData};
                0 ->
                    {NewData, Split} = {[], nil},
                    {Split, NewData, command_blocked}
            end;
        lpush ->
            NewData = lists:append(lists:flatten([Data]), OldValue),
            {length(NewData), NewData};
        lpop ->
            Length = length(OldValue),
            case Length of
                _ when Length > 0 ->
                    {[Split], NewData} = lists:split(1, OldValue);
                0 ->
                    {NewData, Split} = {[], nil}
            end,
            {Split, NewData};
        blpop ->
            Length = length(OldValue),
            case Length of
                _ when Length > 0 ->
                    {[Split], NewData} = lists:split(1, OldValue),
                    {Split, NewData};
                0 ->
                    {NewData, Split} = {[], nil},
                    {Split, NewData, command_blocked}
            end;
        length ->
            {length(OldValue), OldValue};
        range ->
            {Start, End} = Data,
            Range = lists:sublist(OldValue, Start+1, End-Start),
            {Range, OldValue};
        get ->
            {OldValue, OldValue};
        set ->
            {OldValue, Data}
    end.

do_query(Action, OldValue, Data) ->
    case Action of 
        max ->
            lists:max(OldValue);
        min ->
            lists:min(OldValue);
        sum ->
            lists:sum(OldValue)
    end.

% Tests to prove that it works.
list_get_set_test() ->
    ?assertEqual( do_action(get, [1], nil), {[1], [1]} ),
    ?assertEqual( do_action(set, [], [1,2,3]), {[], [1,2,3]} ).

list_rpush_test() ->
    ?assertEqual( do_action(rpush, [], 1), {1, [1]} ),
    ?assertEqual( do_action(rpush, [1], 2), {2, [1,2]} ),
    ?assertEqual( do_action(rpush, [1,2], 3), {3, [1,2,3]} ),
    ?assertEqual( do_action(rpush, [1,2], [3,4]), {4, [1,2,3,4]} ).

list_lpush_test() ->
    ?assertEqual( do_action(lpush, [], 1), {1, [1]} ),
    ?assertEqual( do_action(lpush, [2], 1), {2, [1,2]} ),
    ?assertEqual( do_action(lpush, [1,2], [3,4]), {4, [3,4,1,2]} ).

list_rpop_test() ->
    ?assertEqual( do_action(rpop, [1,2,3], nil), {3, [1,2]} ),
    ?assertEqual( do_action(rpop, [], nil), {nil, []} ).

list_lpop_test() ->
    ?assertEqual( do_action(lpop, [1,2,3], nil), {1, [2,3]} ),
    ?assertEqual( do_action(lpop, [], nil), {nil, []} ).
    
list_length_test() ->
    ?assertEqual( do_action(length, [1,2,3], nil), {3, [1,2,3]} ).

list_range_test() ->
    ?assertEqual( do_action(range, [1,2,3,4,5,6], { 2,4 }), {[3,4], [1,2,3,4,5,6]} ),
    ?assertEqual( do_action(range, [1,2,3,4,5,6], { 1,5 }), {[2,3,4,5], [1,2,3,4,5,6]} ).

list_blocking_test() ->
    ?assertEqual( do_action(blpop, [], nil), {nil, [], command_blocked} ),
    ?assertEqual( do_action(brpop, [], nil), {nil, [], command_blocked} ).
    
list_query_test() ->
    ?assertEqual( do_query(max, [1,2,3], nil), 3 ),
    ?assertEqual( do_query(min, [1,2,3], nil), 1 ),
    ?assertEqual( do_query(sum, [1,2,3], nil), 6 ).

    
