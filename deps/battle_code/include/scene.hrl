%% 每天可以进入副本的次数
-define(DUNGEON_TICKET, 	5).

-define(SCENE_CITY, 		1).		%% 城镇地图
-define(SCENE_OUTDOOR, 		2).		%% 野外地图
-define(SCENE_DUNGEON, 		3).		%% 副本地图
-define(SCENE_ARENA, 		5).		%% 竞技场地图
-define(SCENE_BOSS, 		6).		%% 世界BOSS竞技场地图
-define(SCENE_MARSTOWER,    7).	    %% 英雄塔地图
-define(SCENE_COMP,			8).		%% 比武地图
-define(SCENE_TERRITORY_WAR,9).		%% 领地战地图
-define(SCENE_STAGE,		10).	%% 挑战魂将
-define(SCENE_DEFENCE,		11).	%% 群魔乱舞
-define(SCENE_GUILD_HUNTING,12).	%% 帮会活动
-define(SCENE_CAVE,			13).	%% 藏宝洞地图

-define(INIT_MAP,           1100).  %% 起始地图
-define(SPECIAL_MAP, 		1900). 	%% 原野村要做特殊的处理
-define(MAX_PLAYER_IN_ROOM, data_system:get(33)). 	%% 在原野村虚拟房间中的最大人数
-define(SPECIAL_NUM, 		data_system:get(34)). 	%% 原野村虚拟房间的个数
-define(MOVE_CHECK_SCOPE,   15).

-define(ETS_SCENE_ROOM,     ets_scene_room).
-define(ETS_SCENE_INFO,     ets_scene_info).
-define(ETS_MASK_INFO,      ets_mask_info).

-define(CELL_HEIGHT, 15).
-define(CELL_WIDTH,  30).
-define(SCENE_CACHE_REF, cache_util:get_register_name(position)).

-define(CLIENT_BLOCK_SIZE, 30).
-define(HALF_CLIENT_BLOCK_SIZE, 15).
-define(TO_CLIENT_COORD(N), (N * ?CLIENT_BLOCK_SIZE + ?HALF_CLIENT_BLOCK_SIZE)).

-define(SCENE_STATE_RB,			1).
-define(SCENE_STATE_BATTLE,		2).
-define(SCENE_STATE_DAZUO,		4).
-define(SCENE_STATE_FLY,		8).
-define(SCENE_STATE_COMP_TEAM,	16).
-define(SCENE_STATE_COMP_BALL,	32).
-define(SCENE_STATE_COMP_SPEED,	64).
-define(SCENE_STATE_TERRITORY_WAR,	128).
-define(SCENE_STATE_WING,		256).
-define(SCENE_STATE_FASHION,	512).

-define(SCENE_SPEED_NORMAL,		1).
-define(SCENE_SPEED_RB,			2).
-define(SCENE_SPEED_COMP,		5).
-define(SCENE_SPEED_COMP_DRUG,	4).
-define(SCENE_SPEED_FLY,		3).
-define(SCENE_SPEED_FIRST_RB,	6).

-define(SCENE_MAX_PLAYER_PER_PACKAGE, 20). %% 11001协议每包最多人数，超过此人数分包
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
        guild_id       = 0,     %% 帮派ID
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
        comp_room_id   = 0,     %% 比武场房间ID
		
		ability_data   = 0,

		fly_data       = [],
		
		fashion_data   = 0,		%% 时装数据
		wing_data	   = 0,		%% 翅膀数据，目前为是否有翅膀这个状态
		horse_data 	   = 0,		%% 坐骑数据
		equip_data	   = #equip_info{},
		
		follow_role_data = 0,	%% 跟随武将ID 0表示没跟随武将

		king_data      = 0,		%% 是否国王 0-是 1-否
		chief_disciple_data = 0,%% 首席弟子 0-否 1-虎卫 2-猛将  3-军师
		
		speed_data     = 0,		%% 移动速度
		
		show_data      = 0,     %% 变身ID（怪物的ID）
		
		supremacy_title_data   = 0, %% 至尊称号
		legend_title_data      = 0, %% 传奇称号
		achievement_title_data = 0, %% 成就称号
		honour_title_data      = 0, %% 荣誉称号
		special_title_data     = 0, %% 特殊称号
		
		stealth_data   = false,

		die_data	   = 0,

		war_title      = 0, %% 领地战称号
		war_crown      = 0, %% 领地战头上的皇冠
		wudi_data      = 0,
		dingshen_data  = 0,
		speedup_data   = 0,
		speedcut_data  = 0,

		path           = [],
		territory_war_camp = [],
		send_pid       = 0,		%% 玩家的广播进程ID列表
		move_queue_pid = none 	%% 移动广播进程
  }
).
