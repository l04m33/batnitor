
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
-define(BEGIN_BROADCAST_TIME, {5,0,0}).%%早上5点开始广播
-define(OVER_BROADCAST_TIME,{16,0,0}). %%16点boss战斗开始，停止播放

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
		last_damage_value = 0		%% 上单场打boss伤害
	}
).

-record(boss_hp,{
				fkey = boss_hp,
				hp_value = 0}).

