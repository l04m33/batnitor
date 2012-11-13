%% outdoor monster header file

-define(MON_IDLE,      0).
-define(MON_IN_BATTLE, 1).
-define(MON_DEAD,      2).

-define(ETS_MONSTER, ets_monster).

-define(MON_MOVE_STILL,  1).
-define(MON_MOVE_RADIUS, 2).
-define(MON_MOVE_FIX,    3).

-record(monster,
	{
		coord_x, 	                    %% current x pos
	 	coord_y, 	                    %% current y pos
		id,                             %% 
		group_id, 
		state       = ?MON_IDLE,        %% ?MON_IDLE | ?MON_IN_BATTLE | ?MON_DEAD
		type,                           %% revive | refresh | undead
		category    = ?MON_MOVE_RADIUS, %% still | move
		dead_time   = 0,
		path_index  = 2,
		path        = [],
		full_path   = [],               %% patroling path
		level_limit = 0,			    %% min level to fight with this monster
		radius      = 2
	}
).

-record(monster_scene,
	{
	 	scene_id,
		alive  = [],
	 	dead   = []
	}			
).

