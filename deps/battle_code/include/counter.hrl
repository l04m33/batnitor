%% 计数器模块，每晚的凌晨12:00会进行重置
%% 要增加一个计数器的话，要完成如下步骤：
%% 		1.按顺序在下面的counter记录增加相应的字段

-define(COUNTER_CACHE_REF, cache_util:get_register_name(counter)).

-define(COUNTER_HORN,'CounterHorn').
% -define(COUNTER_DUNGEON,		1).	%% 购买副本的次数（不需要了）
-define(COUNTER_ZHAOSHU,		2).	%% 诏书每天只领取一次
-define(COUNTER_PRAY,			3). %% 祝福次数
-define(COUNTER_PRAYED,			4). %% 被祝福次数
-define(COUNTER_HORSE_FEED,		5). %% 坐骑喂养次数
-define(COUNTER_FENGLU,			6). %% 俸禄领取次数
-define(COUNTER_CYCLIC_TASK_MIN,    11). %% 循环任务已接次数
%% NOTE: 中间的保留，用作不同类型循环任务的已接次数记录
-define(COUNTER_CYCLIC_TASK_MAX,    19). %% 循环任务已接次数

-define(COUNTER_ARENA_WARD, 20).   %%竞技场每天奖励计数
-define(COUNTER_ARENA_CHALLENGE_TIMES, 15). %%竞技场每天挑战次数
-define(COUNTER_ARENA_BUY_CHALLENGE_TIMES, 31).
-define(COUNTER_YUNBIAO_ZHUANYUN_TIMES, 26).%%运镖转运次数记录
-define(COUNTER_YUNBIAO_TIMES, 27).%%运镖次数记录
-define(COUNTER_YUNBIAO_ROB_TIMES,28).%%劫镖次数记录
-define(COUNTER_YUNBIAO_ROBED_TIMES, 29).%%被劫次数记录
-define(COUNTER_YUNBIAO_NUM, 30).   %%每天运镖总人数

%%boss
-define(INSPIRE_SUCCES, 31).  %%鼓舞
%% -define(GOLD_INSPIRE_SUCCES,  32).  %%金币鼓舞
-define(CLEAN_BATTLE_CD, 33).   %%boss cd

%%招财进宝
-define(COUNTER_WEALTH_LUCK,34). %%招财进宝运势
-define(COUNTER_WEALTH_GAMBLED,35). %%招财进宝次数
-define(COUNTER_WEALTH_DISPLAY_LUCK,36). %%招财进宝显示给用户的运势

-define(COUNTER_LOGIN_TIMES,37).    %%记录玩家每天登陆次数，用来发放每天登陆奖励

-define(COUNTER_ANTI_TOO_MANY_BATTLE,38). %%防止每天刷怪太多次

-record (counter, {
	key           = {0, 0},
	gd_updateTime = 0,				%% 更新时间
	gd_counter    = 0 				%% 计数器
	}).


-record (counter_types, {
	key           = {{integer}, {integer}},
	gd_updateTime = {integer},
	gd_counter    = {integer}
	}).
