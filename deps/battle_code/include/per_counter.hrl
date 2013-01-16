%% 永久性的类型
-define(PER_COUNTER_FIRST_RECHARGE,	1). %% 首次充值
-define(PER_COUNTER_FIRST_SAVE_WEB, 2).	%% 首次收藏网页 
-define(PER_COUNTER_FIRST_JOIN_GUILD,3).%% 首次加入帮派
-define(PER_COUNTER_SPECIAL_DUNGEON,4).	%% 特殊副本
-define(PER_COUNTER_SPECIAL_DUNGEON_1200,5). %% 1200特殊新手副本
-define(PER_COUNTER_PHONE_BINDING_GIFT,6). %% 绑定手机礼包
-define(PER_COUNTER_NEW_GIFT,		7).	%% 新手礼包
-define(PER_COUNTER_NOTIC,			8).	%% 公告
-define(PER_COUNTER_XUNXIAN,		9).	%% 第一次寻仙给个三级气血石

-record(per_counter,{
	key           = {0, 0},
	gd_updateTime = 0,				%% 更新时间
	gd_counter    = 0 				%% 计数器
	}).
