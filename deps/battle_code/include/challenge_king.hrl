-define(INIT_KING_MONSTER_ID, 400).		%% 定义第一个国王的怪物ID
-define(INIT_KING_MONSTER_NAME, "国王").		%% 定义第一个国王的怪物名字
-define(CHALLENGE_KING_CACHE_REF, cache_util:get_register_name(challenge_king)).
-define(FIRST_CHALLENGE_KING_ASK_LEVEL, 40).		%% 定义第一次挑战国王所需等级

-define(NOTICE_BY_COMBAT,	0).		%% 战斗力超过1w时通知
-define(NOTICE_BY_ARENA,	1).		%% 竞技场第一名时通知

-define(KING_TYPE_MONSTER,	0).		%% 国王类型为怪物
-define(KING_TYPE_PLAYER,	1).		%% 国王类型为玩家

-define(SEND_KING_AWAWRD,	{20, 0, 0}).	%% 发放挑战国王奖励时间

-define(HAVE_NOT_NOTICE,	0).		%% 未通知获得挑战国王资格
-define(HAVE_NOTICE,		1).		%% 通知过获得挑战国王资格

-define(NOTICE_CHALLENGE_KING_TEXT, "乱世谁称王！三国乱世等级达40级即可获得挑战国王的资格！谁将成为神勇无比的新国王！").

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
		challenger_id = {term}
	}
).

-record(challenge_king_state,
	{
		id = 0,				%% 玩家id
		is_noticed = 0		%% 是否已经通知过获得挑战国王资格了 HAVE_NOT_NOTICE 否 HAVE_NOTICE 是
	}
).

-record(g_challenge_king_state,
	{
		who_challenging = 0,		%% 正在挑战国王的玩家id
		monitor_ref = undefined		%% 对正在挑战国王的战斗进程的monitor
	}
).

%% 定义国王奖励数据结构
-record(king_award,
	{
		level = 0,				%% 国王等级
		silver = 0,				%% 银币
		jungong = 0,			%% 军功
		exp = 0					%% 经验
	}
).