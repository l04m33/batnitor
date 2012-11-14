-module(data_mon_attr).

-compile([export_all]).

-include("common.hrl").

init_ets() ->
    ets:new(ets_monster_attr, [named_table, public, {keypos, #mon_attr.id}, set]).

is_empty() ->
    ets:info(ets_monster_attr, size) == 0.

set(MonAttr) when is_record(MonAttr, mon_attr) ->
    ets:insert(ets_monster_attr, MonAttr);

set(MonAttrList) when is_list(MonAttrList) ->
    ets:delete_all_objects(ets_monster_attr),
    ets:insert(ets_monster_attr, MonAttrList).

get(ID) ->
    case ets:lookup(ets_monster_attr, ID) of
        [MonAttr] ->
            MonAttr;
        [] ->
            undefined
    end.

