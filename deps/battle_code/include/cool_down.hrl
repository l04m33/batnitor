

%% ets cool_down ets表索引
-define(ETS_COOL_DOWN, cache_util:get_register_name(ets_cool_down)).

%% cd类型，所有使用的cd类型必须定义在个地方：
-define(COOl_DOWN_TYPE_TEST, 1).
-define(ARENA_CD, 2).%%竞技场cd类型
-define(BOSS_BATTLE_CD, 3).%%世界boss战斗cd
-define(Qihun_CD, 4).%%棋魂cd
-define(CD_QUILT_GUILD, 5).     %% 帮派退出cd;
-define(CD_TYPE_COMP, 6).		%% 比武保护CD
-define(CD_TYPE_SWORD, 7).  %% 神剑CD

%%　cd原型定义
-record
(
   ets_cool_down,
   {
        key = {0,0},
        cdEndTime = 0,
  		updateTime = 0
   }
).

%%　cd原型字段类型定义
-record
(
   ets_cool_down_types,
   {
        key = {{integer},{integer}},
        cdEndTime = {integer},
		updateTime = {integer}
   }
).
