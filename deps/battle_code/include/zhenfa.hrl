-define(ZHENFA_KAN,			1).		%% 坎                               
-define(ZHENFA_GEN,	 		2).		%% 艮
-define(ZHENFA_ZHEN,		3).		%% 震
-define(ZHENFA_XUN,			4).		%% 巽
-define(ZHENFA_LI,			5).		%% 离
-define(ZHENFA_KUN,			6).		%% 坤
-define(ZHENFA_DUI,			7).		%% 兑
-define(ZHENFA_QIAN,		8).		%% 乾


-define(MIN_ZHENFA_ID,	?ZHENFA_KAN).	%% 最小的卦象id
-define(MAX_ZHENFA_ID,	?ZHENFA_QIAN).	%% 最大的卦象id

-record(zhen_fa, {
	gd_accountId    = 0,			%% 玩家id
	gd_levelingId   = 0,			%% 当前已修炼的卦象
	gd_level        = 0,			%% 上次修炼的卦象的等级
	gd_stage        = 0             %% 境界阶段
	}).

-record(zhen_fa_types, {
	gd_accountId    = {integer},
	gd_levelingId   = {integer},
	gd_level        = {integer},
	gd_stage    	= {integer}	
	}).