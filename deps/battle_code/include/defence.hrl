-ifndef(__DEFENCE_HRL__).
-define(__DEFENCE_HRL__, 1).

-record(defence_points, {
        gd_accountID,
        gd_points}).

-record(defence_points_types, {
        gd_accountID = {integer},
        gd_points    = {integer}}).

-record(history_king_info, {
    type,
    id,
    role_id,
    name,
    level}).

-record(defence_public_state, {
    cur_wave,
    next_wave_time,
    cur_hp,
    rank_info,
    mon_info,
    cell_group_idx,
    hard_deadline = 0,
    player_attack_enhance_percentage}).

-endif.

