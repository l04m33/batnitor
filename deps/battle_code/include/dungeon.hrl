-define(TIMES_TO_BUG, 			5).	%% 副本可以购买的进入次数
-define(MAX_AWARD_ID, 			8).	%% 转盘上最大的物品个数

-define(DUNGEON_NORMAL,			1). %% 普通副本
-define(DUNGEON_DIFFICULT,		2). %% 精英副本

-record (dungeon, {
	key             = {0, 0},		%% {gd_accountId, dungeonId}
	enterTimes      = 0,			%% 进入次数
	buyTimes      	= 0,			%% 购买次数
	updateTime      = 0,			%% 次数的更新时间
	maxAttDamage    = 0,			%% 历史最大的一次伤害
	totalRound      = 0,			%% 总的战斗回合数
	totalDamageRecv = 0,			%% 总的承受的伤害值
	bestRank		= 0,            %% 最佳的名次
	score	      	= 0,
	firstFlag		= 0,			%% 第一次进入要控制剧情播放
	isPass			= 0				%% 是否通关
	}).

-record (dungeon_types, {
	key             = {{integer}, {integer}},
	enterTimes    	= {integer},
	buyTimes    	= {integer},
	updateTime    	= {integer},
	maxAttDamage    = {integer},
	totalRound      = {integer},
	totalDamageRecv = {integer},
	bestRank		= {integer},
	score			= {integer},
	firstFlag		= {integer},
	isPass			= {integer}	
	}).

-record(dungeon_status,{
	gd_accountId    = 0,
	gd_state		= []
	}).

-record(dungeon_status_types,{
	gd_accountId    = {integer},
	gd_state		= {term}
	}).

-record (dungeon_process, {
	id               = 0,
	x                = 0,
	y                = 0,
	scope            = 0,
	monster_group_id = 0
	}).