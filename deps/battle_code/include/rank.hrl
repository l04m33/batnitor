-define(RANKINGS_ENTRIES_PER_PAGE,10).

-define(RANK_CALC_TIME,1000*10*60).

-define(CACHE_RANK,cache_util:get_register_name(rank_status)).
-define(CACHE_WEAPON_RANK,cache_util:get_register_name(rank_weapon)).

-record
(
   rank_status,
   {
		playerId = 0, 			%% 玩家id
		achievePoint = 0, 		%% 玩家成就点
		mainRoleCombat = 0 ,	%% 玩家主角战斗力
		allRolesCombat = 0, %% 玩家所有佣兵战斗力
		yuanbiaoSilver = 0, %% 前一日运镖所得
		zuoqi = 0, %% 坐骑的战斗力
		touhu = 0,	 %% 通过投壶得到的紫、橙装备数
		worldBoss = 0, %% 上次活动中对世界boss的伤害
		kingGuard = 0, %% 上次守卫国王击杀的怪物
		yuanbiaoSilverUpdateTime = 0, %% 寻仙上次更新时间
   		xunxian = 0, %% 召唤到南山老妖的总次数，
        xunxianTotal = 0, %% 寻仙总次数
		worldBossUpdateTime = 0 %%  世界boss上次更新时间
	}
).


-record
(
	rank_status_types,
	{
    	playerId = {integer},
		achievePoint = {integer},
		mainRoleCombat = {integer},
		allRoleCombat = {integer},
		yuanbiaoSilver = {integer},
		zuoqi = {integer},
		touhu = {integer},
		yuanbiaoSilverUpdateTime ={integer},
		worldBossUpdateTime ={integer},
		worldBoss = {integer},
		kingGuard = {integer},
		xunxian = {integer},
        xunxianTotal = {integer}
    }
).

%%  装备排行榜
-record
(
    rank_weapon,
    {
        weaponWorldId = 0,%% 装备世界id
        combat = 0, %% 装备战斗力
        level = 0, %% 装备等级
        ownerPlayerId = 0, %% 所有者id
        weaponOrId = 0 %% 装备原型id
     }
).

-record
(
    rank_weapon_types,
    {
        weaponWorldId = {integer},
        combat = {integer},
        level = {integer},
        ownerPlayerId = {integer},
        weaponOrId = {integer}
     }
).