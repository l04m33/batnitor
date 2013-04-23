
-define(CACHE_CHUANQI, cache_util:get_register_name(chuanqi)).

-define(CHUANQI_LEVEL_50, 10001).
-define(CHUANQI_LEVEL_60, 10002).
-define(CHUANQI_LEVEL_70, 10003).
-define(CHUANQI_LEVEL_80, 10004).
-define(CHUANQI_LEVEL_90, 10005).
-define(CHUANQI_LEVEL_100, 10006).
-define(CHUANQI_TOWER_King_3, 10007).
-define(CHUANQI_TOWER_King_6, 10008).
-define(CHUANQI_TOWER_King_8, 10009).
-define(CHUANQI_TOWER_King_10, 10010).
-define(CHUANQI_CHALLENGE_KING, 10011).
-define(CHUANQI_EQUIMEMT_SRONG_5, 10012). %%全身装备强化到5
-define(CHUANQI_EQUIMEMT_SRONG_7, 10013).
-define(CHUANQI_EQUIMEMT_SRONG_10, 10014).
-define(CHUANQI_EQUIMEMT_SRONG_13, 10015).
-define(CHUANQI_EQUIMEMT_SRONG_15, 10016).
-define(CHUANQI_FLY_5, 10017).
-define(CHUANQI_FLY_10, 10018).
-define(CHUANQI_COMBAT_1W, 10019).
-define(CHUANQI_COMBAT_3W, 10020).
-define(CHUANQI_COMBAT_8W, 10021).
-define(CHUANQI_GUILD_10, 10022).
-define(CHUANQI_ACHIEVE_500, 10023).
-define(CHUANQI_EQUIMENT_WANMEI, 10024). %% 第一个全身装备完美
-define(CHUANQI_EQUEMENT_CHUANSHUO, 10025). %% 第一个全身装备传说
-define(CHUANQI_STONE_9, 10026). %% 第一个合成9级宝石
-define(CHUANQI_XIAOYAO, 10027). %% 第一个军功达到逍遥王
-define(CHUANQI_SHENQI_FULL, 10028). %% 第一个官邸神器升到满级
-define(CHUANQI_SKLL_FULL, 10029). %% 第一个所有技能升满
-record
(
   chuanqi,
    {
     chuanqiId = 0,
     playerId = 0,
     is_get = 0 %% 是否领取 0为领取，1已领取
     }
).
-record
(
   chuanqi_types,
   {
        chuanqiId = {integer},
        playerId = {integer},
        is_get = {integer}
    }
).
