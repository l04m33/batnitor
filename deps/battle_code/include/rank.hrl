-define(RANKINGS_ENTRIES_PER_PAGE,10).

-define(RANK_CALC_TIME,1000*10*60).

-define(CACHE_RANK,cache_util:get_register_name(rank_status)).

-record
(
   rank_status,
   {
		playerId = 0, %% 玩家id
		achievePoint = 0, %% 玩家成就点
		mainRoleCombat = 0 , %% 玩家主角战斗力
		allRolesCombat = 0 %% 玩家所有佣兵战斗力
	
	}
).

-record
(
	rank_status_types,
	{
    	playerId = {integer},
		achievePoint = {integer},
		mainRoleCombat = {integer},
		allRoleCombat = {integer}
    }
).