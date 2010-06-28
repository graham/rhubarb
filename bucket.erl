-module(bucket).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-import(key).

start_bucket(Name) ->
    key:start(data_dict, Name).


