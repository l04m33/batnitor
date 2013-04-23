%% 定义双方势力
-define(TERRITORY_WAR_ATT, 1).				%% 攻击方
-define(TERRITORY_WAR_DEF, 2).				%% 防守方

-define(TERRITORY_WAR_WEEK_TIME, 		data_system:get(77)).		%% 定义领地战时间 [星期几]
-define(TERRITORY_WAR_FIRST_DAY_NOTICE_TIME,    {19, 30, 0}).	%% 开服第一天领地战通知时间
-define(TERRITORY_WAR_NOTICE_TIME, 		{12, 0, 0}).		%% 定义领地战通知时间
-define(TERRITORY_WAR_REGISTER_TIME, 	data_system:get(114)).		%% 定义领地战报名时间
-define(TERRITORY_WAR_BEGIN_TIME, 		data_system:get(115)).		%% 定义领地战开始时间
-define(TERRITORY_WAR_END_TIME, 		data_system:get(116)).		%% 定义领地战结束时间
-define(TERRITORY_WAR_DELAYED_REGISTER_TIME,    (10*60)).   %% 领地战开始后允许报名的时间，单位是秒

%% 定义领地战状态
-define(TERRITORY_WAR_STATE_NOT_OPEN,	0).				%% 领地战未开启
-define(TERRITORY_WAR_STATE_PREPARE,	1).				%% 领地战准备
-define(TERRITORY_WAR_STATE_OPEN,		2).				%% 领地战开启

%% 定义领地战总塔数
-define(TERRITORY_WAR_TOWER_NUM,		3).
%% 定义领地战参加公会数
-define(TERRITORY_WAR_GUILD_NUM,		20).
%% 定义玩家初始生命数
-define(TERRITORY_WAR_RIVIVE_CNT,		6).
%% 定义塔初始HP值
-define(TERRITORY_WAR_TOWER_HP,			1000).
%% 定义每次扣除塔HP值
-define(TERRITORY_WAR_TOWER_DAMAGE,		1).

%% 定义塔状态
-define(TOWER_STATE_CAN_ATT,			0).			%% 可攻击
-define(TOWER_STATE_CAN_NOT_ATT,		1).			%% 无敌(不可攻击)

-define(TERRITORY_WAR_MAP_ID,			3300).		%% 定义领地战地图id
-define(TERRITORY_WAR_TEAM_TYPE_ATT,	3301).		%% 定义领地战队伍类型(进攻方)
-define(TERRITORY_WAR_TEAM_TYPE_DEF,	3302).		%% 定义领地战队伍类型(防守方)
-define(TERRITORY_WAR_TEAM_NPC_ATT,		3301).		%% 定义领地战队伍NCP类型(进攻方)
-define(TERRITORY_WAR_TEAM_NPC_DEF,		3302).		%% 定义领地战队伍NPC类型(防守方)

-define(OFFSET_PER_PLAYER,				3).			%% 定义队伍中每个玩家增加出战佣兵角色偏移量

%% 定义领地战公会信息cache
-define(CACHE_TERRITORY_GUILD, cache_util:get_register_name(territory_war_guild_info)).

%% 定义领地战玩家排名ets表名
-define(TERRITORY_WAR_PLAYER_RANK,		ets_territory_war_player_rank).
%% 定义领地战公会排名ets表名
-define(TERRITORY_WAR_GUILD_RANK,		ets_territory_war_guild_rank).

%% 定义领地战玩家排名(玩家id为索引)ets表名
-define(TERRITORY_WAR_PLAYER_ID_RANK,	ets_territory_war_rank_res_player_id).
%% 定义领地战公会排名(公会id为索引)ets表名
-define(TERRITORY_WAR_GUILD_ID_RANK,	ets_territory_war_rank_res_guild_id).

-define(TERRITORY_WAR_PROP_GROUP,		5).		%% 定义道具批次
-define(TERRITORY_WAR_PROP_REFRESH_INTERVAL,	60).		%% 道具刷新间隔(秒)
-define(TERRITORY_WAR_PROP_BAG_SIZE,	3).		%% 领地战道具背包空间

-define(TERRITORY_WAR_ATT_TOWER_CD,		30).	%% 攻击塔cd时间
-define(TERRITORY_WAR_BATTLE_CD,		10000).	%% in 1/1000 secs

-record(territory_camp_info, {
    camp_id  = 0,
    guild_id = 0,
    sum_revive = 0,
    sum_player = 0}).

%% 定义参加领地战玩家信息ets表结构
-record(ets_territory_war_player_info,
	{
		id					= 0,			%% 玩家id
		nick_name			= "",			%% 玩家昵称
		combat_power		= 0,			%% 玩家战斗力
		guild_camp			= 0,			%% 玩家公会阵营(攻/防)
		guild_id			= 0,			%% 玩家公会id
		integral			= 0,			%% 玩家积分
		rest_rivive_cnt		= 0,			%% 剩余生命数
		battle_role_info	= [],			%% 所有出战佣兵信息
		team_id				= 0,			%% 玩家队伍id
		continue_att_tower	= 0,			%% 连续拆塔次数
		continue_win		= 0,			%% 连胜数
		prop_list			= [],			%% 玩家携带道具列表 [{道具唯一ID, 道具原型ID}]
		last_att_tower_time	= 0,			%% 玩家上次拆塔时间
		is_in_scene			= 0,			%% 玩家是否在领地战场景中(0 否 1 是)
        cur_att_tower_time  = data_system:get(97),      %% 拆塔读条时间
        cur_att_tower_cd_time  = data_system:get(99),   %% 拆塔CD时间
        is_dead             = 0
	}
).

%% 定义参加领地战公会信息表结构
-record(territory_war_guild_info, 
	{
		guild_id		= 0,			%% 公会id
		guild_camp		= 0,			%% 公会阵营(攻/防)
		guild_integral	= 0,			%% 公会积分
		guild_exp		= 0				%% 公会功勋
	}
).

-record(territory_war_guild_info_types, 
	{
		guild_id		= {integer},	%% 公会id
		guild_camp		= {integer},	%% 公会阵营(攻/防)
		guild_integral	= {integer},	%% 公会积分
		guild_exp		= {integer}		%% 公会功勋
	}
).

%% 定义领地战状态ets表
-record(ets_territory_war_state,
	{
		fkey			= territory_war,	%% key值
		state			= 0,				%% 领地战状态(未开启、报名、开启)
		att_sum_revive	= 0,				%% 进攻方剩余总生命数
		def_sum_revive	= 0,				%% 防守方剩余总生命数
		att_num			= 0,				%% 进攻剩余人数
		def_num			= 0,				%% 防守剩余人数
		begin_time		= 0,				%% 领地战开始时间
		end_time		= 0					%% 结束时间
	}
).

%% 定义用于保存塔剩余HP的ets表
-record(ets_tower_hp,
	{
		tower_id		= 0,			%% 塔id
		tower_hp		= 0,			%% 塔hp
		tower_state		= 0				%% 塔状态(0 普通 1 无敌)
	}
).

%% 定义领地战玩家进程进程状态数据结构
-record(player_territory_war_state,
	{
		id				= 0,				%% 玩家id
		battle_pid		= none,				%% 玩家正在进行的战斗的pid
		is_att_tower	= 0,				%% 是否正在攻击塔 0 否 1 是
		att_tower_timer	= none,				%% 攻击塔定时器消息
		buff_prop		= 0					%% 身上道具状态
	}
).

%% 定义领地战全局进程进程状态
-record(g_territory_war_state, 
	{
		auto_increase_index		= 1			%% 领地战队列自增索引
	}
).

%% 定义攻击塔队列ets数据结构
-record(ets_att_tower_queue, 
	{
		index			= 0,				%% 玩家队列顺序索引
		id				= 0,				%% 玩家id
		pid				= undefined			%% 玩家领地战pid
	}
).

%% 定义攻击塔玩家ets表
-record(ets_att_tower_player,
	{
		id				= 0,				%% 玩家id
		tower_id		= 0,				%% 玩家攻击塔id
		index			= 0					%% 玩家在攻击塔队列中的索引(若玩家为防守方，index为0)
	}
).

%% 定义防守塔玩家列表
-record(ets_def_tower_queue,
	{
		id				= 0					%% 玩家id
	}
).

%% 定义排行榜ets表
-record(ets_territory_war_rank,
	{
		key				= {0, 0},			%% 索引，{分数, 账号id}
		name			= "",				%% 名字
        other_info      = 0                 %% 保存其他信息用
	}
).

%% 定义保存玩家领地战排名最终结果ets表
-record(ets_territory_war_rank_res,
	{
		id				= 0,				%% 玩家/公会id
		rank			= 0					%% 玩家排名
	}
).

%% 定义领地战道具原型数据结构
-record(territory_war_prop_proto, 
	{
		proto_id		= 0,				%% 道具原型id
		last_time		= 0,				%% 持续时间
		src_id			= 0,				%% 资源id
		effect			= []				%% 效果参数
	}
).

%% 定义领地战道具批次数据结构
-record(territory_war_prop_group,
	{
		group_id		= 0,				%% 道具批次
		prop_list		= []				%% 道具列表
	}
).

%% 定义领地战道具刷新数据结构
-record(territory_war_prop_refresh,
	{
		unique_id		= 0,				%% 道具唯一ID
		group_id		= 0,				%% 道具批次ID
		pos_x			= 0,				%% x坐标
		pos_y			= 0,				%% y坐标
		proto_id		= 0					%% 道具类型id
	}
).
