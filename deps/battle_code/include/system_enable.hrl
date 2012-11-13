-define(SE_ENABLE_SYSTEM,1).
-define(SE_DISABLE_SYSTEM,0).

-define(SE_FULL_SYSTEM_LIST, data_enable_system:get_all_system()).
%% [
%% 		1,		%%宝石系统
%% 		2,		%%锻造系统
%% 		3,		%%帮会系统
%% 		4,		%%阵法
%% 		5,		%%武将
%% 		6,		%%坐骑
%% 		7,		%%任务
%% 		8,		%%好友
%% 		9,		%%背包
%% 		10,		%%角色
%% 		11,		%%投壶系统
%% 		12,		%%寻仙系统
%% 		13,		%%竞技场
%% 		14,		%%英雄塔
%% 		15,		%%封地
%% 		16,		%%每日指引
%% 		17,		%%三国目标
%% 		18,		%%公会打坐
%% 		19,		%%双倍运镖
%% 		20,		%%世界boss
%%		21,		%%双倍运镖
%% 	]).

-define(ARENA_SYSTEM, 13).
-define(ARENA_SYSTEM_ENABLE_TYPE, 3).

-define(FENGDI_SYSTEM, 15).
-define(FENGDI_SYSTEM_ENABLE_TYPE, 2).

-define(DOUBLE_YUN_BIAO,18).
-define(DOUBLE_YUN_BIAO_TYPE,5).

-define(BOSS_SYSTEM,20).
-define(BOSS_SYSTEM_ENABLE_TYPE,6).

-define(DOUBLE_DAZUO,18).
-define(DOUBLE_DAZUO_TYPE,4).

-define(DEFAULT_ENABLE,1).
-define(TASK_ENABLE,2).
-define(LEVEL_ENABLE,3).