
-record (marstower, {
	gd_AccountID     = 0,		%% 主键：玩家id
	gd_CurrentFloor  = 0,       %% 当前层数（1~10）
	gd_CurrentLevel  = 0,		%% 当前关数（1~10）
	gd_LastTime 	 = 0,		%% 上次更新时间
	gd_ResetTimes    = 0,		%% 重置次数
	gd_Point 		 = 0,		%% 积分
	gd_AchieveLevel  = 0,       %% 通关层数
	gd_AchieveTime   = 0,		%% 通关时间
	gd_MonsterList   = [],		%% 怪物列表
	gd_AutoCallengeTimes  = 0,	%% 自动挑战的次数
	gd_FinishTime	 = 0		%% 完成时间
	}).

-record (marstower_types, {
	gd_AccountID     = {integer},
	gd_CurrentFloor  = {integer},
	gd_CurrentLevel  = {integer},
	gd_LastTime 	 = {integer},
	gd_ResetTimes    = {integer},
	gd_Point 		 = {integer},
	gd_AchieveLevel  = {integer},
	gd_AchieveTime	 = {integer},
	gd_MonsterList   = {term},
	gd_AutoCallengeTimes  = {integer},
	gd_FinishTime	 = {integer}
	}).

-record (marstower_king, {
	gd_Floor = 0,
	gd_IsAccount = 0,
	gd_ID = 0,
	gd_Lock = 0
	}).

-record (marstower_king_types, {
	gd_Floor = {integer},
	gd_IsAccount = {integer},
	gd_ID = {integer},
	gd_Lock = {integer}
	}).

-define(MAX_FREE_RESET_TIMES,1).

