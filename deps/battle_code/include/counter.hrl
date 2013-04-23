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
-define(COUNTER_ARENA_CHALLENGE_TIMES, 21). %%竞技场每天挑战次数
-define(COUNTER_ARENA_BUY_CHALLENGE_TIMES, 22).
-define(COUNTER_YUNBIAO_ZHUANYUN_TIMES, 26).%%运镖转运次数记录
-define(COUNTER_YUNBIAO_TIMES, 27).%%运镖次数记录
-define(COUNTER_YUNBIAO_ROB_TIMES,28).%%劫镖次数记录
-define(COUNTER_YUNBIAO_ROBED_TIMES, 29).%%被劫次数记录
-define(COUNTER_YUNBIAO_NUM, 30).   %%每天运镖总人数
-define(COUNTER_GUILD_JOIN_MAX, 31).   %% 公会每天最多加入的人数
-define(COUNTER_GUILD_GOLD_MAX, 32).   %% 公会每天最多捐献金元宝数
%% -define(GOLD_INSPIRE_SUCCES,  32).  %%金币鼓舞
-define(CLEAN_BATTLE_CD, 33).   %%boss cd

%%招财进宝
-define(COUNTER_WEALTH_LUCK,34). %%招财进宝运势
-define(COUNTER_WEALTH_GAMBLED,35). %%招财进宝次数
-define(COUNTER_WEALTH_DISPLAY_LUCK,36). %%招财进宝显示给用户的运势

-define(COUNTER_LOGIN_TIMES,37).    %%记录玩家每天登陆次数，用来发放每天登陆奖励

-define(COUNTER_ANTI_TOO_MANY_BATTLE,38). %%防止每天刷怪太多次
-define(COUNTER_ONLINE_AWARD_INDEX,40). %%在线奖励次数

-define(COUNTER_DAZUO_TIME,	39).	%% 打坐时间每天不超过30分钟

-define(COUNTER_ANSWER_5,	41).	%% 答题道具：天机显现
-define(COUNTER_ANSWER_6,	42).	%% 答题道具：去伪存真
-define(COUNTER_ANSWER_7,	43).	%% 答题道具：双倍积分

-define(COUNTER_FLOWER_POPULARITY_RECV, 44).  %% 收花每天获得军功
-define(COUNTER_FLOWER_POPULARITY_SEND, 45).  %% 送花每天获得军功
-define(COUNTER_CROSS_PVP_TOTAL, 46).  %% 今天参加跨服的次数
-define(COUNTER_CROSS_PVP_WIN, 47).  %% 今天参加跨服胜利的次数
-define(COUNTER_CROSS_PVP_LILIAN, 48).  %% 今天参加跨服获得历练
-define(COUNTER_WATER_OTHER, 49).  %% 今天给别人浇水的次数
-define(COUNTER_CROSS_PVP, 50).  %% 跨服pvp次数
-define(COUNTER_GUILD_DONATE, 51).  %% 公会当天捐献数
-define(COUNTER_WORLD_BOSS_AWARD_JIANGHU, 52).   %% 世界boss达到江湖义士挑战目标的奖励
-define(COUNTER_WORLD_BOSS_AWARD_CHUMO, 53).   %% 世界boss达到除魔卫道挑战目标的奖励
-define(COUNTER_WORLD_BOSS_AWARD_EMO, 54).   %% 世界boss达到恶魔猎手挑战目标的奖励
-define(COUNTER_WORLD_BOSS_AWARD_HERO, 55).   %% 世界boss达到英雄之魂挑战目标的奖励
-define(COUNTER_WORLD_BOSS_AWARD_ZHENGDAO, 56).   %% 世界boss达到正道领袖挑战目标的奖励
-define(COUNTER_WORLD_BOSS_ATTEND_TIME, 57).   %% 世界boss达到江湖义士挑战目标的奖励
%%清明节活动
-define(COUNTER_PRAY_RAIN, 58).   %% 是否已经祈雨
-define(COUNTER_HAVE_PRAYED, 59).   %% 今天是否领过祈雨礼包
-define(COUNTER_JUNLIANG, 60).   %% 每天征收军粮次数

%%boss
-define(INSPIRE_SUCCES, 61).  %%鼓舞

-define(COUNTER_SWORD,	62).  %%神剑每天挖掘次数

-define(COUNTER_KAIFU_PLAN, 63).   %% 开服计划

-define(COUNTER_SLAVE_ROBED, 64).   %% 被抢劫次数

-define(COUNTER_SLAVE_ROB, 65).   %% 抢劫次数
%%五一活动
-define(COUNTER_MAY_ENTER_MARSTOWER, 66).   %% 进入塔次数

-define(COUNTER_MAY_KILL_MONSTER, 67).   %% 野外怪击杀次数
-define(COUNTER_MAY_DUNGEON_PASS, 68).   %% 副本通关次数
-define(COUNTER_MAY_ZHIQIN, 	  69).   %% 执勤礼包领取


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
