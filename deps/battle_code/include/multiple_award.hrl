-record(gen_award, {
	economy = #economy{},			%% 经济类奖励(类型为 economy record 类型)
	items = [],						%% 道具类奖励(类型为[{道具ID, 数量, 是否绑定}...])
	log_type = 0					%% 日志类型
	}).
	
-record(multiple_award, {
	system_id = 0,					%% 系统id
	multiple = 1,					%% 奖励加成倍数
	begin_time = 0,					%% 生效时间
	end_time = 0					%% 失效时间
	}).

-define(SYSTEM_AWARD_CARD_CHANGE,	1).			%% 新手卡兑换
-define(SYSTEM_AWARD_BOSS,			2).			%% 世界boss
-define(SYSTEM_AWARD_YUNBIAO,		3).			%% 运镖
-define(SYSTEM_AWARD_BIND_PHONE,    4).			%% 绑定手机
-define(SYSTEM_AWARD_STAGE,			5).			%% 挑战魂将
-define(SYSTEM_AWARD_TERRITORY_WAR,	6).			%% 领地战
