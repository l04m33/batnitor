-define(CACHE_ONLINE_EFFECT, cache_util:get_register_name(online_effect)).

%% 类型说明
%% 类型值小于10000，说明效果时间只算在线时间（指定一段明确的在线时间）
%% 类型值大于10000，说明效果时间不管在不在线都算（指定一个明确的结束时间）
-define(ONLINE_EFFECT_TYPE_CHANGE,			1). %% 变身卡类型
-define(ONLINE_EFFECT_TYPE_ONLINE_AWARD,	2). %% 在线奖励
-define(ONLINE_EFFECT_TYPE_FLY,				3). %% 免费飞行
-define(ONLINE_EFFECT_TYPE_MAY_TITLE,		4). %% 五一称号
-define(ONLINE_EFFECT_TYPE_PRAY_RAIN,		10001). %% 清明祈雨
-define(ONLINE_EFFECT_TYPE_KAIFU_PLAN,		10002). %% 10002 - 10007 已被元宝计划占用
-define(ONLINE_EFFECT_TYPE_LEVEL_AWARD,		10008). %% 新手等级奖励
-define(ONLINE_EFFECT_TYPE_DEL_ITEM,		10009). %% 物品到期删除类型
-define(ONLINE_EFFECT_TYPE_DEL_HORSE,		10010). %% 临时坐骑到期删除类型




-define(IS_CANCEL_N, 						0).	%% 剩余时间等于不删除记录
-define(IS_CANCEL_Y, 						1).	%% 剩余时间等于删除记录

%% 在线BUFF效果记录
-record(online_effect, {
						 key           = {0, 0},	%% {AccountID, Type}
						 gd_RemainTime = 0,			%% 类型<10000剩余时间 | 类型>10000结束时间
						 gd_OtherInfo  = 0,			%% 其他信息（随意，根据实际需求填写该值）
						 gd_CBModule   = [],		%% 剩余时间等于0回调的模块列表，与方法和参数一一对应
						 gd_CBFunction = [],		%% 剩余时间等于0回调的方法列表
						 gd_CBArgs     = [],		%% 剩余时间等于0回调的参数列表
						 gd_CBIsCancel = 0			%% 剩余时间等于0是否删除记录
						}
		).

-record(online_effect_types, {
							   key           = {{integer}, {integer}},		%% {AccountID, Type}
							   gd_RemainTime = {integer},					%% 类型<10000剩余时间 | 类型>10000结束时间
							   gd_OtherInfo  = {integer},					%% 其他信息（随意，根据实际需求填写该值）
							   gd_CBModule   = {term},						%% 剩余时间等于0回调的模块列表，与方法和参数一一对应
							   gd_CBFunction = {term},						%% 剩余时间等于0回调的方法列表
						 	   gd_CBArgs     = {term},						%% 剩余时间等于0回调的参数列表
						 	   gd_CBIsCanceL = {integer}					%% 剩余时间等于0是否删除记录
						}
		).

%% 下面是个坑爹的记录，后面把他拿掉 TODO，完全可用上面的记录替代
-record(online_effect_args, {
							 account_id  = 0,
							 type        = 0,
							 remain_time = 0,
							 other_info  = 0,
							 module      = [],
							 function    = [],
							 args        = [],
							 is_cancel   = 0
							 }
		).