-define(INIT_KING_MONSTER_ID, 1).		%% 定义第一个国王的怪物ID
-define(INIT_KING_MONSTER_NAME, 1).		%% 定义第一个国王的怪物名字
-define(CHALLENGE_KING_CACHE_REF, cache_util:get_register_name(challenge_king)).
-define(FIRST_CHALLENGE_KING_ASK_COMBAT, 10000).		%% 定义第一次挑战国王所需战斗力

-record(challenge_king,
	{
		fkey = king,		%% key值目前写死为king
		king_id = 0,		%% 当前国王对应玩家id
		challenger_id = 0	%% 有挑战资格的玩家id
	}
).

-record(challenge_king_types,
	{
		fkey = {term},
		king_id = {integer},
		challenger_id = {integer}
	}
).

-record(challenge_king_state,
	{
		id = 0				%% 玩家id
	}
).

-record(g_challenge_king_state,
	{
		who_challenging = 0		%% 正在挑战国王的玩家id
	}
).