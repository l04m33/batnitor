-module(mod_role).

-compile([export_all]).

-include("common.hrl").

get_on_battle_list(ID) ->
    [RoleInfo] = ets:lookup(ets_role_rec, {0, ID}),
    [MiscInfo] = ets:lookup(ets_role_misc_rec, {0, ID}),
    {_, _, _, _, _, Skills} = MiscInfo,
    [{FirstPos, _} | _] = Skills,
    RoleF = fun({Pos, Skill}) ->
        RoleInfo#role {
            gd_isBattle = Pos,
            gd_roleRank = case Pos of
                              FirstPos -> 1;
                              _ -> 0
                          end,
            gd_skill    = Skill
        }
    end,
    lists:map(RoleF, Skills).

