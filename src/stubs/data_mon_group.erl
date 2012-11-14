-module(data_mon_group).

-compile([export_all]).

-include("common.hrl").

init_ets() ->
    ets:new(ets_monster_group, [named_table, public, {keypos, #mon_group.id}, set]).

is_empty() ->
    ets:info(ets_monster_group, size) == 0.

set(MonGroup) when is_record(MonGroup, mon_group) ->
    ets:insert(ets_monster_group, MonGroup);

set(MonGroupList) when is_list(MonGroupList) ->
    ets:delete_all_objects(ets_monster_group),
    ets:insert(ets_monster_group, MonGroupList).

get(ID) ->
    case ets:lookup(ets_monster_group, ID) of
        [MonGroup] ->
            MonGroup;
        [] ->
            undefined
    end.

