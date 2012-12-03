
-define(CACHE_CHIEF_DISCIPLE, cache_util:get_register_name(chief_disciple)).
-define(TYPE_HUWEI, 1).
-define(TYPE_MENGJIANG, 2).
-define(TYPE_JUNSHI, 3).

-record (chief_disciple, {
	type                   = 0,
	playerId               = 0
	}).



-record (chief_disciple_types, {
	type           	= {integer},		%% 是否完成
	playerId 	 	= {integer}		%% 是否有奖励
	}).

