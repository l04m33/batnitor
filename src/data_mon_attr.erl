-module(data_mon_attr).

-compile([export_all]).

-include("common.hrl").

init_ets() ->
    ets:new(ets_monster_attr, [named_table, public, {keypos, #mon_attr.id}, set]).

set(MonGroupList) ->
    ets:delete_all_objects(ets_monster_attr),
    ets:insert(ets_monster_attr, MonGroupList).

get(ID) ->
    case ets:lookup(ets_monster_attr, ID) of
        [{_, MonAttr}] ->
            MonAttr;
        [] ->
            undefined
    end.

