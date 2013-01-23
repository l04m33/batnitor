-ifndef(__GUILD_HUNTING_HRL__).
-define(__GUILD_HUNTING_HRL__, 1).

-define(GUILD_HUNTING_SCENE_ID, data_scene:get_guild_hunting_scene()).

-define(GUILD_HUNTING_INIT_MON_NUM, 15).

-define(GUILD_HUNTING_NORMAL_DAMAGE, data_guild_hunting:get_misc_setting(8)).
-define(GUILD_HUNTING_BOOSTED_DAMAGE, data_guild_hunting:get_misc_setting(9) * data_guild_hunting:get_misc_setting(8)).

-define(GUILD_HUNTING_BASE_MON_ID, 1000).

-define(GUILD_HUNTING_TIME,      (10*60*1000)). % 从开始打怪到结束的总时间
-define(GUILD_HUNTING_KICK_TIME, 15000).        % 结束后等待踢人的时间，单位是1/1000秒
-define(GUILD_HUNTING_BOSS_TIME, 30000).        % 打boss的时间

-define(GUILD_HUNTING_BONUS_TIME_1, (3*60*1000)).
-define(GUILD_HUNTING_BONUS_TIME_2, (2*60*1000)).
-define(GUILD_HUNTING_BONUS_TIME_3, (1*60*1000)).

-define(GUILD_HUNTING_DAILY_START_TIME, {20, 40, 0}).

-define(GUILD_HUNTING_MON_STATE_NORMAL,  1).
-define(GUILD_HUNTING_MON_STATE_BOOSTED, 2).

-define(GUILD_HUNTING_GOLD_ARROW_COST, data_guild_hunting:get_misc_setting(7)).

-define(GUILD_HUNTING_CD_INTERVAL, 2).      % in seconds

-define(GUILD_HUNTING_NORMAL_REWARD(Lv),  data_guild_hunting:get_reward_by_level(Lv)).

-define(GUILD_HUNTING_CRATE_REWARD,         data_guild_hunting:get_misc_setting(13)).
-define(GUILD_HUNTING_GREATER_CRATE_REWARD, data_guild_hunting:get_misc_setting(14)).
-define(GUILD_HUNTING_TOP_CRATE_REWARD,     data_guild_hunting:get_misc_setting(20)).

-define(GUILD_HUNTING_CRATE_DROP_RATE, data_guild_hunting:get_misc_setting(12)).
-define(GUILD_HUNTING_BOSS_CRATE_DROP_RATE, data_guild_hunting:get_misc_setting(18)).

-record(gh_round_info, {
    round,
    mon_proto_id,
    mon_num,
    norm_color,
    purple_rate,
    orange_rate,
    mon_hp,
    boss_hp}).

-record(gh_mon_info, {
    id,
    proto_id,
    type,           % normal | boss
    color,          % white | green | blue | purple | orange
    location,       % {X, Y}
    hp,
    max_hp,
    state}).

-record(gh_player_info, {
    id,
    last_attack_time}).

-record(guild_ranking_state, {
    keys,
    name,
    top_players}).

-record(player_ranking_state, {
    keys,
    name,
    attack_times,
    total_coins,
    total_merit}).

-endif.
