%% 玩家在比武场中的信息
-record(comp_info, {
					  gd_AccountID   = 0,				%% 账号ID
					  gd_RoleLevel   = 0,				%% 角色等级
					  gd_Score       = 0,				%% 荣誉积分
					  gd_KillNum     = 0,				%% 杀人数
					  gd_DieNum      = 0,				%% 被杀数
					  gd_TeamID      = 0,				%% 队伍  1-红 2-蓝
					  gd_IsBall      = 0,				%% 是否绣球 0-否  1-是
					  gd_Ability     = 0,				%% 战斗力
					  gd_Consecutive = 0,				%% 连胜场次
					  gd_MaxCon      = 0,				%% 最高连胜场次
					  gd_MerList     = 0,				%% 上阵佣兵信息
					  gd_IsDie       = false,			%% 当前是否阵亡
					  gd_IsBattle    = false,			%% 当前是否在战斗
					  gd_IsLeave     = false,			%% 是否已经离开比武场
					  gd_IsCloaking  = false,			%% 是否隐身
					  gd_IsDouble    = false,			%% 是否使用双倍积分丹
					  gd_DoubleTimes = 0,				%% 双倍丹使用次数
					  gd_IsFreeze    = false			%% 是否使用定身丹
					 }).

%% 比武场全局状态
-record(g_comp_status, {
						key = key,
						status = none,
						first_bloods = [],
						banners = [],
						apply_time = 0,
						begin_time = 0,
						end_time = 0,
						apply_forbits = []
					   }).

%% 比武场个人状态
-record(s_comp_status, {
						id = 0,					%% 玩家ID
						apply_level = 0,		%% 报名等级
						apply_time = 0,			%% 报名时间
						
						revive_times = 0,		%% 使用复活丹次数
						speed_time = 0,			%% 上次使用加速丹时间
						cloaking_time = 0,		%% 上次使用隐身丹时间
						double_time = 0,		%% 上次使用双倍荣誉丹时间
						restore_time=0,			%% 上次使用恢复丹时间
						fly_time=0				%% 上次使用传送丹时间
					   }).

%% 比武场战斗信息
-record(comp_battle_info, {
							comp_level     = 0,
							att_id         = 0,
							att_win_score  = 0,
							att_loss_score = 0,
							att_is_ball    = 0,
							att_con_win    = 0,
							def_id         = 0,
							def_win_score  = 0,
							def_loss_score = 0,
							def_is_ball    = 0,
							def_con_win    = 0
						   }).

%% 比武场前三名信息
-record(comp_top_info, {
						gd_CompLevel = 0,							%% 比武场等级
						gd_AccountID = 0,							%% 账号ID
						gd_RoleID    = 0,							%% 角色ID
						gd_Slogan    = "我就是比武冠军，来膜拜吧。",	%% 冠军口号
						gd_DressList = []							%% 其他装扮信息（坐骑、翅膀、装备等）
					   }).

-record(comp_top_info_types, {
							  gd_CompLevel = {integer},		%% 比武等级
							  gd_CompRank  = {integer},		%% 排名
							  gd_RoleID    = {integer},		%% 角色ID
							  gd_Slogan    = {string},		%% 冠军口号
							  gd_DressList = {term}		%% 其他装扮信息（坐骑、翅膀、装备等）
							 }).

%% 比武场奖励信息
-record(comp_award_info, {
						  gd_IsWin        = 0,		%% 是否胜利方 1-是 2-否
						  gd_Rank         = 0,		%% 排名
						  gd_KillNum      = 0,		%% 杀人数
						  gd_Consecutive  = 0,		%% 最高连杀数
						  gd_DieNum       = 0,		%% 被杀数
						  gd_Score        = 0		%% 荣誉积分
						 }).

-define(CACHE_COMP_TOP_REF, cache_util:get_register_name(comp_top_info)).

-define(TIME_CD_TEAM_BANNER, 360).

-define(TIME_CD_SPEED_UP, 60).
-define(TIME_CD_CLOAKING, 60).
-define(TIME_CD_DOUBLE, 90).
-define(TIME_CD_RESTORE, 120).
-define(TIME_CD_FLY, 120).

-define(TIME_LAST_SPEED_UP, 10).
-define(TIME_LAST_CLOAKING, 10).

-define(DRUG_TYPE_SPEED,		1).
-define(DRUG_TYPE_CLOAKING,		2).
-define(DRUG_TYPE_FREEZE,		3).
-define(DRUG_TYPE_DOUBLE,		4).
-define(DRUG_TYPE_RESTORE,		5).
-define(DRUG_TYPE_FLY,			6).
-define(DRUG_TYPE_REVIVE,		7).

-define(BANNER_IS_RAISE_N,		1).  %% 队旗未升起
-define(BANNER_IS_RAISE_Y,		2).  %% 队旗已升起

-define(COMP_CLEAR_PROTECT_COST,	10).		%% 清除战斗保护状态的金币
-define(COMP_REVIVE_COST,			8).			%% 复活需要的金币

-define(COMP_LEVEL_LIST, 		[1,2,3]).

-define(COMP_BALL_Y,			1). %% 绣球
-define(COMP_BALL_N,			0).	%% 非绣球
-define(COMP_TEAM_RED,			1).	%% 红队
-define(COMP_TEAM_BLUE,			2).	%% 蓝队