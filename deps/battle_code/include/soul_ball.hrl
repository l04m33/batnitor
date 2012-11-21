%% 魂珠最高等级
-define(SOUL_BALL_MAX_LEVEL, 50). 

-define(MINGHUN, 1).
-define(YUANHUN, 2).
-define(LIHUN, 3).
-define(MINHUN, 4).
-define(ETS_SOUL_BALL, cache_util:get_register_name(soul_ball)).
-define(HUNSHI, 293).

-define(HUNSHI_GOLD, data_soul_ball:get_soul_cost()).
%% 魂珠数据结构
-record(
    soul_ball,
    {
	    key = {0,0}, %% {PlayerId,SoulType}
		soulExp = 0
	}
   ).

-record(
    soul_ball_types,
    {
	    key = {{integer},{integer}},
		soulExp = {integer}
	}
   ).