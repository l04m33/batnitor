-module(data_mon_group).

-compile([export_all]).

init_ets() ->
    ets:new(ets_monster_group, [named_table, public, {keypos, 1}, set]).

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

