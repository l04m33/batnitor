-define(STAGE_FLOOR_CACHE_REF, cache_util:get_register_name(stage_floor_info)).
-define(STAGE_TIME_CACHE_REF, cache_util:get_register_name(stage_time_info)).
-define(STAGE_HISTORY_CACHE_REF, cache_util:get_register_name(stage_history)).
-define(STAGE_MAP_ID,	3400).		%% 挑战魂将地图id
-define(STAGE_MAX_STAR,	10).		%% 定义挑战魂将最大星级
-define(STAGE_CAN_RESET_COUNT,	1).	%% 定义可重置次数

%% 每层挑战魂将进度
-define(STAGE_MONSTER_NOT_KILL,	0).	%% 没有击杀小怪
-define(STAGE_MONSTER_KILL,		1).	%% 只杀死了小怪，BOSS没被击杀
-define(STAGE_BOSS_KILL,		2).	%% BOSS被击杀

%% 定义挑战魂将层数范围
-define(STAGE_FLOOR_RANGE, {1, 6}).

%% 定义挑战魂将开启等级
-define(STAGE_OPEN_LEVEL,	40).

%% 定义玩家每层挑战信息
-record(stage_floor_info, 
	{
		stage_floor_key = {0, 0},			%% 索引值 {gd_AccountID, floor} ({玩家id, 层数})
		star			= 1,				%% 星级
		challenge_info	= [],				%% 挑战/杀怪信息(被杀怪物id列表)
		integral		= 0,				%% 评分
		monster_kill_flag	= 0				%% 杀怪标志 0 没杀过怪 1 击杀小怪 2 击杀BOSS
	}
).

-record(stage_floor_info_types,
	{
		stage_floor_key	= {{integer}, {integer}},
		star			= {integer},
		challenge_info	= {term},
		integral		= {integer},
		monster_kill_flag	= {integer}
	}
).

%% 定义玩家最后挑战时间、最后重置时间信息
-record(stage_time_info,
	{
		gd_AccountID		= 0,			%% 玩家账号id
		last_battle_time	= 0,			%% 最后挑战时间
		last_reset_time		= 0,			%% 最后重置时间
		last_battle_floor	= 1,			%% 最后挑战层数
		reset_count			= 0				%% 在该天已重置次数
	}
).

-record(stage_time_info_types,
	{
		gd_AccountID		= {integer},
		last_battle_time	= {integer},
		last_reset_time		= {integer},
		last_battle_floor	= {integer},
		reset_count			= {integer}
	}
).

%% 定义每层每星级历史记录
-record(stage_history,
	{
		stage_history_key	= {0, 0},		%% 索引值 {floor, star} ({层数, 星级})
		integral			= 0,			%% 评分
		gd_AccountID		= 0,			%% 最佳积分玩家id
		gd_name				= ""			%% 最佳积分玩家昵称
	}
).

-record(stage_history_types,
	{
		stage_history_key	= {{integer}, {integer}},
		integral			= {integer},
		gd_AccountID		= {integer},
		gd_name				= {string}
	}
).

%% 定义挑战魂将关卡配置
-record(stage_monster_cfg,
	{
		floor				= 0,			%% 关卡数
		star				= 0,			%% 星级数
		monster				= []			%% 怪物配置
	}
).