
-record (marstower, {
	gd_AccountID     = 0,		%% 主键：玩家id
	gd_CurrentFloor  = 0,       %% 当前层数（1~10）
	gd_CurrentLevel  = 0,		%% 当前关数（1~10）
	gd_LastTime 	 = 0,		%% 上次更新时间
	gd_ResetTimes    = 0,		%% 重置次数
	gd_AchieveLevel  = 0,       %% 通关层数
	gd_AchieveTime   = 0,		%% 通关时间
	gd_MonsterList   = [],		%% 怪物列表
	gd_FinishTime	 = 0,		%% 完成时间
	gd_RewardInfo	 = [] 		%% 奖励列表
	}).

-record (marstower_types, {
	gd_AccountID     = {integer},
	gd_CurrentFloor  = {integer},
	gd_CurrentLevel  = {integer},
	gd_LastTime 	 = {integer},
	gd_ResetTimes    = {integer},
	gd_AchieveLevel  = {integer},
	gd_AchieveTime	 = {integer},
	gd_MonsterList   = {term},
	gd_FinishTime	 = {integer},
	gd_RewardInfo    = {term}
	}).

-record (marstower_king, { 	%% 霸主
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

-record(marstower_guaji,{
	account_id = 0,       %% 账户id
	current_level = 0,	  %% 当前关卡
	max_level = 0,		  %% 最高关卡
	timer_ref = none,	  %% 计时器
	end_time = 0,		  %% 挂机结束时间
	reward_list = [] 	  %% 奖励列表[#marstower_guaji_award]
	}).

-record(marstower_guaji_award,{
	level = 0,
	silver = 0,
	exp = 0,
	items = []
	}).

-define(MAX_FREE_RESET_TIMES,1).

