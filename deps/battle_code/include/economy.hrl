-record(economy,{
	gd_accountId       = 0,
	gd_silver          = 0,
	gd_gold            = 0,		%% 非绑定元宝
	gd_bind_gold       = 0,		%% 绑定元宝
	gd_practice        = 0,		%% 阅历
	gd_popularity      = 0, 	%% 声望或叫官职贡献
	gd_totalPopularity = 0,		%% 累计总声望
	gd_HonourScore     = 0,		%% 当前荣誉积分
	gd_TotHonourScore  = 0, 	%% 累计荣誉积分
	gd_lingli          = 0		%% 灵力（求求暂时不要用啊，用了后果自负）
	}).

-record(economy_types,{
	gd_accountId       = {integer},
	gd_silver          = {integer},
	gd_gold            = {integer},
	gd_bind_gold       = {integer},
	gd_practice        = {integer},
	gd_popularity      = {integer},
	gd_totalPopularity = {integer},
	gd_HonourScore     = {integer},
	gd_TotHonourScore  = {integer},
	gd_lingli          = {integer}
	}).


-define(ETS_ECONOMY,ets_economy).

-define(DISPLAY,1).
-define(NOT_DISPLAY,0).


