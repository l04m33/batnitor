-define(WEALTH_BAG_COMMON,1).
-define(WEALTH_BAG_MINI,2).
-define(WEALTH_BAG_AMAZE,3).
-define(WEALTH_BAG_DULUXE,4).
-define(WEALTH_BAG_SUPREME,5).


-define(WEALTH_GAMBLE_1,0).
-define(WEALTH_GAMBLE_10,1).

-define(WEALTH_TREE_STAT1,1).
-define(WEALTH_TREE_STAT2,2).
-define(WEALTH_TREE_STAT3,3).


-record(wealth_rank,
	{
		seq = 0,
		id = 0,
		bag_type = ?WEALTH_BAG_COMMON,
		silver = 0
	}).

-record(wealth_rank_types,
	{
		seq = {integer},
		id = {integer},
		bag_type = {integer},
		silver = {integer}
	}).

-define(WEALTH_RANK_REF, cache_util:get_register_name(wealth_rank)).