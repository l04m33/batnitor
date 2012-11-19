%% 每天可以进入副本的次数
-define(DUNGEON_TICKET, 	5).

-define(SCENE_CITY, 		1).		%% 城镇地图
-define(SCENE_OUTDOOR, 		2).		%% 野外地图
-define(SCENE_DUNGEON, 		3).		%% 副本地图
-define(SCENE_ARENA, 		5).		%% 竞技场地图
-define(SCENE_BOSS, 		6).		%% 世界BOSS竞技场地图
-define(SCENE_MARSTOWER,    7).	    %% 英雄塔地图

-define(INIT_MAP,           1100).  %% 起始地图
-define(SPECIAL_MAP, 		1900). 	%% 原野村要做特殊的处理
-define(SPECIAL_NUM, 		20). 	%% 原野村虚拟房间的个数
-define(MAX_PLAYER_IN_ROOM, 70). 	%% 在原野村虚拟房间中的最大人数
-define(MOVE_CHECK_SCOPE,   8).

-define(ETS_SCENE_ROOM,     ets_scene_room).
-define(ETS_SCENE_INFO,     ets_scene_info).
-define(ETS_MASK_INFO,      ets_mask_info).

-define(CELL_HEIGHT, 12).
-define(CELL_WIDTH,  12).
-define(SCENE_CACHE_REF, cache_util:get_register_name(position)).

-define(SCENE_STATE_RB,			1).
-define(SCENE_STATE_BATTLE,		2).
-define(SCENE_STATE_DAZUO,		4).
-define(SCENE_STATE_COMP_TEAM,	16).
-define(SCENE_STATE_COMP_BALL,	32).
-define(SCENE_STATE_COMP_SPEED,	64).
-define(SCENE_STATE_FLY,    8).

-define(ROOM_NUMBERS, 100).
-define(MAX_PLAYER,   30).

-type xpos() :: integer().
-type ypos() :: integer().

-record(mask_key, {scene, x, y}).

-record(scene_mask, {key, can_move}).



-record(position,
	{
		gd_accountID = 0,
		scene        = 0,
		x            = 0,
		y            = 0,
		access_map	 = []
	}
).

-record(position_types, 
	{
		gd_accountID = {integer},
		scene        = {integer},
		x            = {integer},
		y            = {integer},
		access_map	 = {term}
	}
).


%%=================== 场景记录  =====================================
-record(scene,
	{
		id = 0,					%% 场景id
		type = 0,				%% 场景类型，1：城镇地图， 2：剧情地图， 3：副本地图
		row = 0,				%% 客户端将地图划分了多少行
		column = 0,				%% 客户端将地图划分了多少列				
		level = 0,				%% 主城等级
		pop_restrict = 0,		%% 人口限制，0表示无限制
		max_level_restrict = 0,	%% 进入的最高等级限制，0表示无限制
		min_level_restrict = 0,	%% 进入的最低等级限制，0表示无限制
		times_restrict = 0		%%  进入次数限制，0表示无限制
    }
).	

-record(equip_info,
	{
		weapon = 0,
		kaijia = 0,
		pifeng = 0,
		shoes  = 0,
		ring   = 0
	}
).

-record(grid, 
	{
		grid_id,
		player_id
	}
).

-record(scene_info, 
	{
		scene_id,
		cols,
		rows,
		type,        %% scene type: city? dungeon? arena? ... etc
		cellcols,
	 	cellrows
	}		
).

-record(scene_room, 
	{
		id,          %% player_id
	 	scene_id,    %% scene_id
	 	room_id      %% room index
	}		
).

%%=================== 玩家位于地图cell中记录  =====================================   
-record(player_cell, 
	{
		player_id      = 0,		%% 玩家id
		scene_id 	   = 0,		%% 玩家所在场景
		role_id        = 0,		%% int16:当前主角佣兵原型Id
		
		cell           = 0,		%% 九宫格格子的id，从1开始计数的
		nickname       = "",	%% 玩家角色昵称
		x              = 0,		%% 玩家所在x坐标
		y              = 0,		%% 玩家所在y坐标
		
		title          = 0,		%% int8:成就称号id
		level          = 0,		%% int8:当前玩家主角等级
		guild_lv	   = 0,		%% 帮派等级
		guild_name	   = "",	%% 帮派名称
		role_rank	   = 2,		%% 1:游客, 2:普通玩家, 3:GM, 6:指导员

		pet_data	   = undefined, %% #pet{},

		state          = 0,		%% 玩家在场景中的状态，如战斗、打坐、跑商等
		rb_data        = [],
		dazuo_data     = [],

		comp_team_data = [],	%% 比武场队伍状态
		comp_ball_data = [],	%% 比武场绣球状态
		comp_speed_data= [],	%% 比武场加速状态

		fly_data       = [],
		
		wing_data	   = 0,		%% 翅膀数据，目前为是否有翅膀这个状态
		horse_data 	   = 0,		%% 坐骑数据
		
		
		equip_data	   = #equip_info{},
		
		stealth_data   = false,

		path           = [],
		send_pid       = 0,		%% 玩家的广播进程ID列表
		move_queue_pid = none 	%% 移动广播进程
  }
).


