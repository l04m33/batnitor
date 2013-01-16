
-define(ETS_BOSS, ets_boss).
-define(ETS_BOSS_HP, ets_boss_hp).
-define(BOSS_CACHE_REF, cache_util:get_register_name(boss_rec)).
-define(ETS_BOSS_DAMAGE, ets_boss_damage).
-define(SILVER_INSPIRE, 2).%%银币鼓舞
-define(GOLD_INSPIRE, 1).%%金币鼓舞
-define(RANKING_FIRST_THREE, 3).%%击杀boss排名前三
-define(BOSS_CD_TIME, 30).   %%世界boss战斗cd
-define(UPDAT_BOSS_RABKING_TIME, 3000).%%定时刷新伤害排行榜

-define(BOSS_RANK_LENGTH, 10). %%boss 排行榜长度
-define(BROADCAST_TIME, [{{15,0,0},{16,0,0},"下午4点"}, {{9,30,0},{10,30,0},"上午10点30分"}]).%%15点开始广播

-define(BOSS_TIME_LIST, [{37500, 37800, 39000}, {57300, 57600, 58800}]). %% 早上BOSS开启时间


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
		boss_level         = 30,         %% 初始设为20，如果玩家能在活动时间内击杀BOSS，则下一次世界BOSS等级提升一级，如果没能在规定时间内击杀，则等级不变。
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
%% 		scene_id		   = {integer},
%% 		level			   = {term},
%% 		queue			   = {term},       		 %% queue(), waiting queue
%% 		queue_count		   = {integer},         %% items in the queue
%% 		rooms			   = {integer},               %% [table()]              
%% 		damage             = {term} %% table()
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
		last_battle_time = 0		%% 玩家上次发起世界boss战斗时间
	}
).

-record(boss_hp,{
				fkey = boss_hp,
				hp_value = 0}).

-define(BOSS_SCENE_STATE_LIVE,	0).		%% 可打boss状态
-define(BOSS_SCENE_STATE_DIE,	1).		%% 处于死亡cd状态