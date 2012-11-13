	
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

-define(DailyGuajiTimes,200).
-define(AutoBuyTimes, 20).