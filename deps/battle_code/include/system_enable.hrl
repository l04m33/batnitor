-define(SE_ENABLE_SYSTEM,1).
-define(SE_DISABLE_SYSTEM,0).

%% 协议10015 控制图标是否闪
-define(SE_NEED_FLIGHT, 1).
-define(SE_NOT_NEED_FLIGHT, 0).
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
%%		24,		%%比武
%%		53，	%%领地战
%% 	]).
-define(DEFAULT_ENABLE,0).	  %% 默认开启
-define(DEFAULT_DISABLE,1).   %% 默认不开启

-define(GUILD_SYSTEM, 3).
-define(ARENA_SYSTEM, 13).
-define(FENGDI_SYSTEM, 15).
-define(BOSS_SYSTEM,20).
-define(DOUBLE_DAZUO,18).
-define(COMP_SYSTEM, 22).
-define(DOUBLE_YUNBIAO_AWARD, 19).
-define(DEFEND_KING, 23).
-define(SYSTEM_COMP, 35).
-define(SWORD,  32).
-define(BATTLE_EXTRA_MP_END,    33).
-define(BATTLE_EXTRA_MP,        34).
-define(SYSTEM_ONLINE_AWARD, 45).
-define(SYSTEM_GUILD_HUNTING, 42).
-define(SYSTEM_TASK_TRACE,	 44).  %% 悬赏任务
-define(SYSTEM_TASK_JUNGONG,	 26).  %% 悬赏任务
-define(ANSWER,		49).  %% 问答系统
-define(SYSTEM_RUSH_RANK,     50).
-define(SYSTEM_TERRITORY_WAR,	53).
-define(KAIFU,		54).
-define(CROSS_SYSTEM, 55).
-define(FESTIVAL_QINGMING_SYSTEM, 57).
-define(JUNLIANG, 58).


-define(DOUBLE_DAZUO_TYPE,4).
-define(DOUBLE_YUNBIAO_TYPE, 5).
-define(BOSS_SYSTEM_ENABLE_TYPE,6).
-define(DEFEND_KING_TYPE,7).
-define(SWORD_TYPE, 8).
-define(SYSTEM_COMP_TYPE, 9).
-define(SYSTEM_GUILD_HUNTING_TYPE, 10).
-define(SYSTEM_ONLINE_AWARD_TYPE, 11).
-define(SYSTEM_TASK_TRACE_TYPE,	 12).  %% 悬赏任务
-define(ANSWER_TYPE,	13).
-define(SYSTEM_RUSH_RANK_TYPE, 14).
-define(SYSTEM_TERRITORY_WAR_TYPE,	15).
-define(KAIFU_TYPE,		16).
-define(CROSS_TYPE,     17).
-define(FESTIVAL_TYPE,  18).
-define(JUNLIANG_TYPE,  19).

