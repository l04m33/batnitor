-define(TIMES_TO_BUG, 			5).	%% 副本可以购买的进入次数
-define(MAX_AWARD_ID, 			8).	%% 转盘上最大的物品个数

-define(DUNGEON_NORMAL,			1). %% 普通副本
-define(DUNGEON_DIFFICULT,		2). %% 精英副本

-record (dungeon, {
	key             = {0, 0},		%% {gd_accountId, dungeonId}
	enterTimes      = 0,			%% 进入次数
	buyTimes      	= 0,			%% 购买次数
	vipBuyTimes		= 0,			%% VIP购买次数
	updateTime      = 0,			%% 次数的更新时间
	maxAttDamage    = 0,			%% 历史最大的一次伤害
	totalRound      = 0,			%% 总的战斗回合数
	totalDamageRecv = 0,			%% 总的承受的伤害值
	bestRank		= 0,            %% 最佳的名次
	score	      	= 0,
	firstFlag		= 0,			%% 第一次进入要控制剧情播放
	isPass			= 0,			%% 是否通关
	offlineTimes	= 0
	}).

-record (dungeon_types, {
	key             = {{integer}, {integer}},
	enterTimes    	= {integer},
	buyTimes    	= {integer},
	vipBuyTimes		= {integer},
	updateTime    	= {integer},
	maxAttDamage    = {integer},
	totalRound      = {integer},
	totalDamageRecv = {integer},
	bestRank		= {integer},
	score			= {integer},
	firstFlag		= {integer},
	isPass			= {integer},
	offlineTimes	= {integer}
	}).

-record(dungeon_guaji,{
	key  = {0,0},   %% {playerid,dungeonid}
	times      = 0,
	end_time   = 0,
	timer_ref  = none
	}).

-record (dungeon_state, {
	player_id         = 0,
	scene_id		  = 0,		%% 当前副本的地图id
	npc_id			  = 0,      %% 进入时候的NPCID
	left_process  	  = [],		%% 当前副本的剩余进度
	process_level	  = 1,		%% 当前副本的难度
	enter_level		  = 0,		%% 进入时的等级
	max_att_damage    = 0,		%% 攻击最高伤害
	total_round       = 0,		%% 副本打完时的战斗回合数（每次累积）
	total_damage_recv = 0,		%% 副本打完时的个人承受的总伤害(每次累积)
	seckill			  = 0,		%% 秒杀数据
	extra_process	  = [],		%% 特殊怪
	award_id          = -1
	%% 转盘奖励id，-1：副本没完成，0：副本完成但没产生奖励，> 0：奖励产生了
	}).	

-record(dungeon_state_types, {
	player_id         = {integer},
	scene_id		  = {integer},		
	npc_id			  = {integer},      
	left_process  	  = {term},			
	process_level	  = {integer},
	enter_level		  = {integer},		
	max_att_damage    = {integer},		
	total_round       = {integer},		
	total_damage_recv = {integer},
	seckill			  = {integer},	
	extra_process	  = {term},	
	award_id          = {integer}
	}).

-record (dungeon_process, {
	id               = 0,
	x                = 0,
	y                = 0,
	scope            = 0,
	monster_group_id = 0
	}).