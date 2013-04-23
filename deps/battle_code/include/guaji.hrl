-include("common.hrl").

-record(guaji,{
	gd_AccountID = 0,
	gd_Times = 0,
	gd_LastTime = 0,
	gd_Percent = 0,         %% 血量指标，低于某百分比自动加血
	gd_IsUseDrug = 0,	    %% 低于血量指标，使用药品
	gd_IsUseBlood = 0,		%% 气血用尽，使用气血包
	gd_IsAutoStop = 0,		%% 药品不足，自动停止打怪
	gd_IsAutoBuy = 0		%% 是否自动购买挂机次数
	}).
	
-record(guaji_types,{
	gd_AccountID = {integer},
	gd_Times = {integer},
	gd_LastTime = {integer},
	gd_Percent = {integer},
	gd_IsUseDrug = {integer},
	gd_IsUseBlood = {integer},
	gd_IsAutoStop = {integer},
	gd_IsAutoBuy = {integer}
	}).

-define(DailyGuajiTimes,200).  %% 日常挂机免费次数区别于VIP模块中的免费次数,这字段除了每天重置还作记录购买次数用
-define(AutoBuyTimes, 20).

%% 这些地图中不扣除挂机次数
-define(GUAJI_EXCEPTION_SCENE_TYPES, 
        [?SCENE_DUNGEON, 
         ?SCENE_BOSS, 
         ?SCENE_MARSTOWER, 
         ?SCENE_STAGE, 
         ?SCENE_CAVE]).

