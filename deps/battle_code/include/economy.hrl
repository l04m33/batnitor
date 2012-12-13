-record(economy,{
	gd_accountId        = 0,
	gd_silver           = 0,
	gd_gold             = 0,		%% 非绑定元宝
	gd_bind_gold        = 0,		%% 绑定元宝
	gd_practice         = 0,		%% 阅历
	gd_popularity       = 0, 	%% 声望或叫官职贡献
	gd_tot_popularity   = 0,		%% 累计总声望
	gd_junwei           = 0,		%% 军威
	gd_honour_score     = 0,		%% 当前荣誉积分
	gd_tot_honour_score = 0, 	%% 累计荣誉积分
	gd_school_point     = 0,		%% 当前师门积分
	gd_king_point       = 0,		%% 守卫国王积分
	gd_tower_point		= 0,		%% 爬塔积分
	gd_lingli           = 0		%% 灵力（求求暂时不要用啊，用了后果自负）
	}).

-record(economy_types,{
	gd_accountId        = {integer},
	gd_silver           = {integer},
	gd_gold             = {integer},
	gd_bind_gold        = {integer},
	gd_practice         = {integer},
	gd_popularity       = {integer},
	gd_tot_popularity   = {integer},
	gd_junwei           = {integer},
	gd_honour_score     = {integer},
	gd_tot_honour_score = {integer},
	gd_school_point     = {integer},
	gd_king_point       = {integer},
	gd_tower_point		= {integer},
	gd_lingli           = {integer}
	}).


-define(ETS_ECONOMY,ets_economy).

-define(DISPLAY,1).
-define(NOT_DISPLAY,0).


