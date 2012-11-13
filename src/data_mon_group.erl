-module(data_mon_group).

-compile([export_all]).

-include("common.hrl").

init_ets() ->
    ets:new(ets_monster_group, [named_table, public, {keypos, #mon_group.id}, set]).

set(MonGroupList) ->
    ets:delete_all_objects(ets_monster_group),
    ets:insert(ets_monster_group, MonGroupList).

get(ID) ->
    case ets:lookup(ets_monster_group, ID) of
        [{_, MonGroup}] ->
            MonGroup;
        [] ->
            undefined
    end.

