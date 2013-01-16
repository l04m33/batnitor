-define(RANKINGS_ENTRIES_PER_PAGE,10).

-define(RANK_CALC_TIME,1000*10*60).

-define(CACHE_RANK,cache_util:get_register_name(rank_status)).
-define(CACHE_WEAPON_RANK,cache_util:get_register_name(rank_weapon)).
-define(CACHE_RANK_COMBAT, cache_util:get_register_name(rank_combat)).

%% Range 值 1总战斗力；2主角战斗力；3财富；4军功；5荣誉；6成就；7在线时间；8捕鱼；9竞技场；10装备；11投壶；12寻仙，13坐骑,14藏宝洞,15世界BOSS,16答题,17押镖,18守卫国主,19等级，20，传奇
-define(RANK_TOTOL_COMBAT,1).
-define(RANK_MAN_COMBAT, 2).
-define(RANK_SILVER, 3).
-define(RANK_JUNGONG, 4).
-define(RANK_HONOUR, 5).
-define(RANK_ACHIEVE, 6).
-define(RANK_ONLINE, 7).
-define(RANK_FISH, 8).
-define(RANK_ARENA, 9).
-define(RANK_EQUIMENT, 10).
-define(RANK_TOUHU, 11).
-define(RANK_XUNXIAN, 12).
-define(RANK_HORSE, 13).
-define(RANK_BAODONG, 14).
-define(RANK_WORLD_BOSS, 15).
-define(RANK_QUEASION, 16).
-define(RANK_YABIAO, 17).
-define(RANK_KING_GUILDE, 18).
-define(RANK_LEVEL, 19).
-define(RANK_CHUANQI, 20).

-record
(
   rank_combat,
   {
        playerId = 0,
        gd_CombatPoint = 0
   }
).

-record
(
   rank_combat_types,
   {
        playerId = {integer},
        gd_CombatPoint = {integer}
   }
).

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