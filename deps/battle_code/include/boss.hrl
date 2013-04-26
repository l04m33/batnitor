
-define(ETS_BOSS, ets_boss).
-define(ETS_BOSS_HP, ets_boss_hp).
-define(BOSS_CACHE_REF, cache_util:get_register_name(boss_rec)).
-define(WORLD_BOSS_REF, cache_util:get_register_name(world_boss_rec)).
-define(BOSS_CARD_REF, cache_util:get_register_name(boss_card)).
-define(ETS_BOSS_DAMAGE, ets_boss_damage).
-define(SILVER_INSPIRE, 2).%%银币鼓舞
-define(GOLD_INSPIRE, 1).%%金币鼓舞
-define(RANKING_FIRST_THREE, 3).%%击杀boss排名前三
-define(BOSS_CD_TIME, 30).   %%世界boss战斗cd
-define(UPDAT_BOSS_RABKING_TIME, 3000).%%定时刷新伤害排行榜

-define(BOSS_RANK_LENGTH, 20). %%boss 排行榜长度
-define(BROADCAST_TIME, [{{15,0,0},{16,0,0},"下午4点"}, {{10,0,0},{11,0,0},"上午11点"}]).%%15点开始广播

-define(BOSS_TIME_LIST, [{39300, 39600, 40800}, {57300, 57600, 58800}]). %% 早上BOSS开启时间
-define(BOSS_CARD_LOW, 0).	%%世界boss低级离线卡
-define(BOSS_CARD_ADVANCE, 1).	%%世界boss高级离线卡

%% used by data_boss.erl to set time config 
-record(boss_time,
	{
		register_time,
		begin_time,
		end_time
	}
).

-record(boss_rec,
	{
		fkey               = boss,
	 	boss_id            = 0,    		 %% integer() 上次排名第一玩家的id
		boss_nickname      = "",          %% 上次排名第一玩家的昵称
		boss_level         = 40,         %% 初始设为20，如果玩家能在活动时间内击杀BOSS，则下一次世界BOSS等级提升一级，如果没能在规定时间内击杀，则等级不变。
		boss_hp			   = 0,  		 %% integer(),
	 	is_battle 		   = false,  	 %% true | false    is available for fighting
		is_survival  	   = false,   	 %% true | false
		is_open			   = false            %% true | false    is available for registering
%% 		scene_id		   = 0,
%% 		level			   = {},
%% 		queue			   = {[],[]},       		 %% queue(), waiting queue
%% 		queue_count		   = 0,         %% items in the queue
%% 		rooms			   = 0,               %% [table()]              
%% 		damage             = {}  %% table()
	}			
).

-record(boss_types,
	{
		fkey               = {term},
	 	boss_id            = {integer},    		 %% integer() 上次排名第一玩家的id  资源Id
		boss_nickname      = {string},          %% 上次排名第一玩家的昵称
		boss_level         = {integer},         %% 初始设为20，如果玩家能在活动时间内击杀BOSS，则下一次世界BOSS等级提升一级，如果没能在规定时间内击杀，则等级不变。
		boss_hp			   = {integer},  		 %% integer(),
	 	is_battle 		   = {term},  	 %% true | false    is available for fighting
		is_survival  	   = {term}, 	 %% true | false
		is_open			   = {term}            %% true | false    is available for registering
	}			
).

-record(boss_card,
	{
	 	key = none, 	%% {card_type, player_id} card_type 0表示低级世界boss离线卡, 1表示高级世界boss卡
		number = 0		%% 使用卡的数量
	}			
).

-record(boss_card_types,
	{
		key = {{integer}, {integer}},
		number = {integer}
	}			
).


-record(world_boss_rec,
	{
		player_id,
		attend_times,
		injure,
		kill_times
	}
).

-record(world_boss_types,
	{
		player_id		= {integer},	%%玩家id
		attend_times	= {integer},	%%玩家参加世界boss次数
		injure			= {integer},	%%玩家对世界boss产生的伤害总数
		kill_times		= {integer}		%%玩家杀死世界boss的总次数
	}
).

%% boss_damage is an ets whose name is stored 
%% in the damage field of record boss
-record(boss_damage,
	{
		id   = 0,                   %% player's ID
		damage_value = 0 ,          %% how many damages this player deal to the boss
        damage_rep_award = 0 ,   	%% 单次伤害换算的声望奖励累加值
		state  = 0  ,     			%% 0 在boss场景外，1在boss场景里面
		last_damage_value = 0,		%% 上单场打boss伤害
		last_battle_time = 0,		%% 玩家上次发起世界boss战斗时间
		cd_time = 0					%% 玩家cd时间
	}
).

-record(ets_boss_damage_ranking,
	{
		key				= {0, 0},	%% 排行key值{伤害总值, 玩家id}
		damage_rec		= #boss_damage{}	%% 玩家伤害record
	}
).

-record(boss_hp,{
				fkey = boss_hp,
				hp_value = 0}).

-define(BOSS_SCENE_STATE_LIVE,	0).		%% 可打boss状态
-define(BOSS_SCENE_STATE_DIE,	1).		%% 处于死亡cd状态
-define(WORLD_BOSS_JIANGHU,	1).		%% 江湖义士
-define(WORLD_BOSS_CHUMO,	2).		%% 除魔卫道
-define(WORLD_BOSS_EMO,	3).		%% 恶魔猎手
-define(WORLD_BOSS_HERO,	4).		%% 英雄之魂
-define(WORLD_BOSS_ZHENGDAO,	5).		%% 正道领袖
-define(LIMIT_TYPE_ATTEND_TIMES, 1). %%目标达成条件,进入次数
-define(LIMIT_TYPE_INJURE, 2). %%目标达成条件,伤害总量
-define(LIMIT_TYPE_KILL_TIMES, 3). %%目标达成条件,杀死世界boss次数
-define(RESTART_G_BOSS, 0). %%标识g_boss是否是重启了
