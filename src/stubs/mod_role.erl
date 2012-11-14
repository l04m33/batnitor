-module(mod_role).

-compile([export_all]).

-include("common.hrl").

get_on_battle_list(ID) ->
    [RoleInfo] = ets:lookup(ets_role_rec, {0, ID}),
    RoleF = fun(Pos) ->
        RoleInfo#role {
            gd_isBattle = Pos,
            gd_roleRank = case Pos of
                              1 -> 1;
                              _ -> 0
                          end
        }
    end,
    lists:map(RoleF, lists:seq(1, 3)).

