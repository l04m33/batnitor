-module(data_ai).
-include("common.hrl").

-export([get/1]).


get(0) ->
    [];

get(1) ->
    [{default, {action, p1, p2}, {target, rival, front}}].

