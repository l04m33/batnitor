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
-define(PER_COUNTER_TASK_JUNGONG,	10). %% 前3次军工任务给紫色橙色
-define(PER_COUNTER_MEDIA_GIFT,		11).%% 媒体礼包
-define(PER_COUNTER_LOTTERY,		12).%% 投壶（开服活动）
-define(PER_COUNTER_WEALTH,		    13).%% 招财（开服活动）
-define(PER_COUNTER_TOTAL_RECHARGE,	14).%% 累计充值
-define(PER_COUNTER_GOT_FIRST_RECHARGE_GIFT,	15). %% 领取首充礼包
-define(PER_COUNTER_OFFLINE_TOTAL_RECHARGE,	16).%% 离线累积充值
-define(PER_COUNTER_COIN_DUNGEON,	17).%% 铜币副本第一次不出随机怪
-define(PER_COUNTER_SWORD,			18).%% 是否第一次挖宝
-define(PER_COUNTER_FIRST_VIP,		19).%% 是否第一次vip
-define(PER_COUNTER_ORDER_GIFT,		20).%% 预约礼包
-define(PER_COUNTER_TASK_JUNGONG_2, 21).%% 军工任务完成后不能给橙色
-define(PER_COUNTER_TASK_TRACE,		22).%% 悬赏任务第一次刷新给两个橙的
-define(PER_COUNTER_SPECIAL_DUNGEON_1200_ITEM,	23).	%% 副本特殊物品
-define(PER_COUNTER_SPECIAL_DUNGEON_1202, 24). %% 1202特殊新手副本
-define(PER_COUNTER_SPECIAL_DUNGEON_1203, 25). %% 1203.。。。。。
-define(PER_COUNTER_LIANHUN,	26).%% 累计炼魂

%% TODO: 新节日开始的时候下面两个counter要清空……
-define(PER_COUNTER_FEST_CHARGE,    27).        %% 节日累计充值
-define(PER_COUNTER_USED_FEST_POINT,28).        %% 节日金元宝消耗积分

-define(PER_COUNTER_MAY_USE_GOLD, 	  29).   %% 五一活动使用元宝数量
-define(PER_COUNTER_MAY_USE_GOLD_GIFT_BASE, 30). 
%%***************************注意:31-39已经被用于五一 消费积分******
-define(PER_COUNTER_FRESH_TIME, 40). 

-record(per_counter,{
	key           = {0, 0},
	gd_updateTime = 0,				%% 更新时间
	gd_counter    = 0 				%% 计数器
	}).
