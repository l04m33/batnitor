
-define(CROSS_TEAM_SUCCESS, 2).
-define(CROSS_MATE_SUCCESS, 1).
-define(CROSS_FAILED, 0).
%% 请求邀请种类 1是全服 2是好友 3是本帮
-define(CROSS_INVITE_TYPE_ALL, 1).
-define(CROSS_INVITE_TYPE_FRIEND, 2).
-define(CROSS_INVITE_TYPE_GUILD, 3).

-define(CROSS_RANK_LIST, [today_sort, tianti_sort, combat_sort, win_rate, win_streak]).

-define(CROSS_MAP_ID,                5700).  %% 藏宝洞地图ID

-define(CROSS_RANK_TYPE_TODAY, today_sort).
-define(CROSS_RANK_TYPE_TIANTI, tianti_sort).
-define(CROSS_RANK_TYPE_COMBAT, combat_sort).
-define(CROSS_RANK_TYPE_WIN_RATE, win_rate).
-define(CROSS_RANK_TYPE_WIN_STREAK, win_streak).


-define(CROSS_EXIT_TEAM_TYPE_MASTER_LEAVE, 1). %% 队长离开队伍
-define(CROSS_EXIT_TEAM_TYPE_MATE_LEAVE, 2). %% 队员离开队伍
-define(CROSS_EXIT_TEAM_TYPE_FIRE, 3). %% 队员被开除

-define(CROSS_MATCH_MATE, 1).
-define(CROSS_MATCH_TEAM, 2).

%% 快速加入队伍时匹配用到的常量
-define(QUICKLY_MATCH_BEGIN_STAGE, 1).

%% 每页展示玩家数
-define(CROSS_ENTRIES_PER_PAGE, 10).

%% 匹配成功后展示玩家资料的时间。。。
-define(CROSS_SHOW_PLAYER_TIME, 10).

%% 每页显示的队伍数
-define(CROSS_TEAMS_PER_PAGE, 6).

-define(CROSS_NPC_ID, 111).

-define(CROSS_INDEX_BASE, 10000000).

-define(CROSS_TIANTI_BASE_LEVEL, 1000).

-define(CROSS_CACHEREF_LIST, [cache_account,
                              cache_role,
                              cache_title,
                              cache_soul_ball2,
                              cache_dressing,
                              cache_item,
                              cache_qi_hun,
                              cache_guild_info,
                              cache_guild_member,
                              cache_economy,
                              cache_hero_soul,
                              cache_zhen_fa,
                              cache_vip,
                              cache_qihun_pinjie]).

-define(ETS_CROSS_PLAY, record_cross_play).%% 跨服进来玩家列表
-define(ETS_CROSS_TEAM, record_cross_team).%% 跨服创建队伍列表
-define(ETS_CROSS_PLAY_OUT, record_cross_out_play). %% 跨服出去玩家列表 
-define(ETA_TEAM_MATCH, team_match).

-define(ETS_QUICKLY_MATCH, record_quickly_match). %% 快速匹配的ets

-define(CACHE_CROSS_PLAY_INFO, cache_util:get_register_name(record_cross_play_info)).
-define(CACHE_CROSS_RANK_STATE, cache_util:get_register_name(cross_rank_state)).
-define(CACHE_CROSS_RANK_INFO, cache_util:get_register_name(cross_rank_info)).
-define(CACHE_RECORD_CROSS_NODE, cache_util:get_register_name(record_cross_node)).


%% 快速匹配跨服队友的ets record
-record(record_quickly_match,
       {
        id = 0,            %% 申请匹配人id
        combat = 0,        %% 申请匹配人的战斗力
        stage = 0,         %% 匹配状态共 1,2,3三个状态
        max_combat = 0,    %% 可以匹配的最大战斗力
        min_combat = 0        %% 可以匹配的最小战斗力
        }).

-record(cross_rank_info,
        {player_id,
         level,
         combat,
         name}
        ).

-record(cross_rank_info_types,
        {
         player_id = {integer},
         level = {integer},
         combat = {integer},
         name = {term}
        }
        ).

-record(cross_rank_state,
   {
    rank_name,
    rank_data
%%     today_sort,
%%     tianti_sort,
%%     combat_sort,
%%     win_rate_sort,
%%     win_continuation_sort
    }).

-record(cross_rank_state_types,
   {
    rank_name = {term},
    rank_data = {term}
    }).

%% 玩家数据，参加过跨服活动的玩家都会在master节点上生产一条这个数据
-record(record_cross_play_info,
        {
         id = 0,               %% 玩家id
         name = "",
         tianti_level = 1000,  %% 天梯等级
         combat = 0,           %% 战斗力
         level = 0,            %% 等级
         win_total = 0,         %% 胜场
         total_battle = 0,    %% 总场数
         win_continuation = 0, %% 连胜
         highest_tianti_level = 0,
         wansheng_count = 0,
         highest_win_continuation = 0,
         highest_combat = 0,
         highest_tianti_rank = 101,
         highest_combat_rank = 101,
         highest_win_rate_rank = 101,
         highest_win_continuation_rank = 101,
         lilian_total = 0
        }
       ).

-record(record_cross_play_info_types,
        {
         id = {integer},               %% 玩家id
         name = {term},
         tianti_level = {integer},   %% 天梯等级
         combat = {integer},           %% 战斗力
         level = {integer},          %% 等级
         win_total = {integer},         %% 胜场
         total_battle = {integer},    %% 总场数
         win_continuation = {integer},  %% 连胜
         highest_tianti_level = {integer},
         wansheng_count = {integer},
         highest_win_continuation = {integer},
         highest_combat = {integer},
         highest_tianti_rank = {integer},
         highest_combat_rank = {integer},
         highest_win_rate_rank = {integer},
         highest_win_continuation_rank = {integer},
         lilian_total = {integer}
        }
       ).


-record(record_cross_play,
        {
         id,
         tianti_level,
         name,
         combat,
         level,
         battle_pid,
         pvp_total,
         pvp_today,
         win_total,
         win_streak,
         win_today,
         success_bin,
         state = normal  %% 状态  pvp_leave 战斗时掉线，
        }
       ).

-record(record_cross_team,
        {
         id = 0,
         master_name = "",
         create_time = 0,
         combat = 0,
         level = 0,
         careerID = 1,
         tianti_level = ?CROSS_TIANTI_BASE_LEVEL,
         mate_id = 0
         }
       ).

%% slave节点的玩家状态
-record(record_cross_out_play,
        {
         id,
         status = null, %% null match 
		 battle_node = null
        }
       ).

-record(record_cross_rank_info,
        {
         id,
         level,
         name,
         combat,
         tianti_level
         }).

-record(team_match,
        {
         team_master_id,
         team_mate_id,
         team_score,
         team_level = 0,
         match_stage = 1
         }).

-record(record_cross_node,
        {
         node_index,
         node
         }).

-record(record_cross_node_types,
        {
         node_index = {integer},
         node = {term}
         }).

%% 机器人
-record(robot, {
				gd_AccountID = 0,
				gd_SceneID   = 0,
				gd_PosX      = 0,
				gd_PosY      = 0,
				gd_RoomNum   = 0,
				gd_State     = idle,
				gd_TeamID    = 0,
				gd_IsBall    = 0,
				gd_IsDie     = 0,
				gd_OtherList = []
				}).