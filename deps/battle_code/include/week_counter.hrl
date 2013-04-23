%% 每周清零,由排行榜对时间进行判断重置计数
-define(WEEK_COUNTER_FLOWER_SEND,	1). %% 每周送花
-define(WEEK_COUNTER_FLOWER_RECV,	2). %% 每周收花

-define(WEEK_COUNTER_GET_CROSS_PVP, 3). %% 获取全服奖励次数

-record(week_counter,{
	key           = {0, 0},
	gd_updateTime = 0,				%% 更新时间
	gd_counter    = 0 				%% 计数器
	}).
