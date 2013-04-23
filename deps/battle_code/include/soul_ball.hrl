
-define(ETS_SOUL_BALL, cache_util:get_register_name(soul_ball)).
-define(CACHE_SOUL_BALL, cache_util:get_register_name(soul_ball2)).
-define(HUNSHI, 293).

-define(HUNSHI_GOLD, data_soul_ball:get_soul_cost()).

-record(
    soul_ball2,
    {
        playerId = 0,
        soulExp = 0
    }
   ).

-record(
    soul_ball2_types,
    {
        playerId = {integer},
        soulExp = {integer}
    }
   ).

%% 魂珠数据结构
-record(
    soul_ball,
    {
	    key = {0,0},
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

