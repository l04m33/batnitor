-define(LOTTERY_BAG_SIZE, 180).			%% 投壶背包大小
-define(ADVANCE_ITEM_SIZE, 10).			%% 投壶高级物品获得历史记录条数
-define(LOTTERY_HISTORY_SIZE, 20).		%% 投壶历史记录显示条数
-define(LOTTERY_RAND_MAX, 10000).		%% 定义投壶几率范围(1~10000)

-define(LOTTERY_CACHE_REF, cache_util:get_register_name(lottery)).

-define(FAIL,		0). %% 失败
-define(SUCCESS,	1).	%% 成功

%% 定义投壶玩家数据表结构
-record(lottery, 
	{
		gd_AccountID		= 0,		%% 账号id
		gd_lottery_bag		= []		%% 投壶背包信息
	}
).

-record(lottery_types, 
	{
		gd_AccountID		= {integer},
		gd_lottery_bag		= {term}
	}
).

%% 定义投壶价格数据结构
-record(lottery_price,
	{
		type		= 0,		%% 投壶类型 1 普通投壶 2 高级投壶
		count		= 0,		%% 投壶次数 目前有 1 10 50 三种
		price		= 0			%% 投壶价格
	}
).

%% 定义投壶物品产出概率数据结构
-record(lottery_rate,
	{
		type		= 0,		%% 投壶类型 1 普通投壶 2 高级投壶
		item_id		= 0,		%% 道具原型id
		rate		= 0,		%% 几率
		is_show		= 0			%% 是否展示 0 否 1 是
	}
).