%% =======================================================================
%% ========================== 有关种植系统的 =============================
%% 种植状态
-define(PLANTING_NO,		1).		%% 未种植
-define(PLANTING_GROWN,     2).     %% 生长期
-define(PLANTING_MATURE,	4).		%% 种子已成熟
-define(PLANTING_GANHE,     3).     %% 干涸期

%% 种子类型
-define(SEED_EXP,			1).		%% 经验种子
-define(SEED_SILVER,		2).		%% 银币种子
-define(SEED_JUNGONG,       3).     %% 军功种子
-define(SEED_DAOJU,         4).     %% 道具种植
%% 种子品质
-define(SEED_QUALITY_1,		1).		%% 普通种子
-define(SEED_QUALITY_2,		2).		%% 优秀种子
-define(SEED_QUALITY_3,		3).		%% 杰出种子
-define(SEED_QUALITY_4,		4).		%% 卓越种子
-define(SEED_QUALITY_5,		5).		%% 完美种子
-define(GEN_CACHE_ETS_WATER, cache_util:get_register_name(water)).


-define(FENGDI_CACHE_REF, 		cache_util:get_register_name(fengdi_data)).
-define(SLAVE_CACHE_REF, 		cache_util:get_register_name(slave)).
-define(SLAVE_OWNER_CACHE_REF, 	cache_util:get_register_name(slave_owner)).


%% 日志，花园操作类型
-define(LOG_GARDEN_OP_WATERING, 1).	%% 浇水
-define(LOG_GARDEN_OP_PLANTING, 2).	%% 种植
-define(LOG_GARDEN_OP_HARVEST,	3).	%% 收获
-define(LOG_OP_SUCCESS,			1).	%% 操作成功
-define(LOG_OP_FAIL,			0).	%% 操作失败

-define(MATURE_SEED_GANHE_TIME, 60*60). %% 成熟植物枯萎周期

%% 浇水多少次土地可以变成肥沃
-define(FERTILITY_TIMES,	5).

%% 土地每次种植的cd
-define(LAND_CD_TIME, 4*60*60).

%% 好友封底信息列表每页显示页数
-define(FRIEDN_WATER_INFO_ENTRIES_PER_PAGE, 10).
%% 每天最多浇水次数
-define(MAX_WATER_PERDAY,   20).

%% 每天给一个玩家最多浇水次数
-define(MAX_WATER_PLAYER_TIMES,   2).

%% 每次对封地操作获得的经验
-define(FENGDI_ADD_EXP_PER_OPRETA,   1).

-define(ITME_SEED_GOLD,   617).

%% 土地的最大数量
-define(MAX_LAND,			6).

-define(LAND_CACHE_REF,         cache_util:get_register_name(land)).

-define(WATER_COUNTER_REF,         cache_util:get_register_name(waterCounter)).

%% 这个record包含了封地所有系统中的一些细碎的数据
-record(fengdi_data, {
	gd_accountId     = 0,
	exp_seed_quality = ?SEED_QUALITY_1,			%% 当前刷新出来的经验种子品质
	sil_seed_quality = ?SEED_QUALITY_1,			%% 当前刷新出来的银币种子品质
    jun_seed_quality = ?SEED_QUALITY_1,         %% 当前刷新出来的军功种子品质
    dao_seed_quality = ?SEED_QUALITY_1,         %% 当前刷新出来的道具种子品质
    fengdi_exp = 0
	}).

-record(fengdi_data_types, {
	gd_accountId     = {integer},
	exp_seed_quality = {integer},
	sil_seed_quality = {integer},
    dao_seed_quality = {integer},
    jun_seed_quality = {integer},
    fengdi_exp = {integer}
	}).

%% 为了约束玩家一天最大浇水次数（目前是50次）
-record
(
    waterCounter,
    {
        playeId = 0,
        waterCount = 0,
        lastWaterTime = 0
    }
).
-record
(
    waterCounterTypes,  
    {
        playeId = {integer},
        waterCount = {integer},
        lastWaterTime = {integer}
    }
).

%% 为了约束每天最多只能给其他一个玩家浇水2次
-record
(
    water,
    {
        key = {0,0},
        count = 0,
        data = 0   
    }
).

-record
(
    water_types,
    {
        key = {{integer},{integer}},
        count = {integer},
        data = {integer}  
    }
).

-record(land, {
		key            = {0, 0},			%% {玩家id, 土地id}
		state          = ?PLANTING_NO,		%% 种植状态
		cd_time        = 0,					%% 下次开始种植的时间
		seed_type      = 0,					%% 种子类型
		seed_quality   = 0,					%% 种子品质
		seed_data      = 0,					%% 种子相关的数据（目前只有种子为经验种子时，这个数据为武将id）
		update_time    = 0					%% 浇水更新时间
	}).

-record(land_types, {
		key            = {{integer}, {integer}},
		state          = {integer},
		cd_time        = {integer},
		seed_type      = {integer},
		seed_quality   = {integer},
		seed_data      = {integer},
		update_time    = {integer}
	}).
%% =======================================================================
%% =============================== end ===================================

%% =======================================================================
%% ============================= 奴隶系统 ================================
-define(CAGE_NOT_OPEN,			-1).	%% 笼子未开启
-define(CAGE_OPENED,			 0).	%% 笼子已开启

-define(SLAVE_EXPIRE_TIME,24*60*60).	%% 奴隶的过期时间

-define(MAX_WORK_TIMES,			 6).	%% 最大的劳作次数
-define(WORK_CD,			 15*60).	%% 每次劳作产生的cd

-define(WORK_HUNT,				 1).	%% 劳作：打猎
-define(WORK_FARM,				 2).	%% 劳作：翻地
-define(WORK_FEED_HORSE,		 3).	%% 劳作：喂马

%% 奴隶的属性
-record(slave, {
		gd_accountId = 0,				%% 奴隶自己的id
		slave_owner  = 0,				%% 奴隶主id
		end_time     = 0,				%% 成为奴隶后的奴役结束时间
		taxes        = 0				%% 成为某个奴隶主的奴隶期间押镖所得的总银币收益产生的税金
	}).

-record(slave_types, {
		gd_accountId = {integer},
		slave_owner  = {integer},
		end_time     = {integer},
		taxes        = {integer}
	}).

-record(slave_detail, {
		pos         = 0,				%% 奴隶所在笼子的位置
		slave_id    = 0,				%% 奴隶id
		slave_level = 0,				%% 奴隶等级
		slave_name  = 0,				%% 奴隶名称
		end_time    = 0,				%% the same as #slave.end_time
		taxes       = 0					%% the same as #slave.taxes
	}).

-record(slave_owner_detail, {
		id    = 0,						%% 奴隶主id
		name  = "",						%% 奴隶主名称
		level = 0						%% 奴隶主等级
	}).

-record(non_slave_detail, {
		id     = 0,						%% 非奴隶id
		name   = "",					%% 非奴隶名称
		level  = 0,						%% 非奴隶等级
		career = 0						%% 该玩家的主角色的职业id
	}).

%% 奴隶主属性
-record(slave_owner, {
		gd_accountId = 0,				%% 奴隶主自己的id
		slave1       = ?CAGE_OPENED,	%% 奴隶1的id
		slave2       = ?CAGE_OPENED,	%% 奴隶2的id
		slave3       = ?CAGE_NOT_OPEN,	%% 奴隶3的id
		slave4       = ?CAGE_NOT_OPEN,	%% 奴隶4的id
		slave5       = ?CAGE_NOT_OPEN,	%% 奴隶5的id
		slave6       = ?CAGE_NOT_OPEN,	%% 奴隶6的id
		work_times   = 0,				%% 劳作次数
		update_time  = 0,				%% 劳作更新时间
		next_time    = 0				%% 下次劳作的开始时间

	}).

-record(slave_owner_types, {
		gd_accountId = {integer},
		slave1       = {integer},
		slave2       = {integer},
		slave3       = {integer},
		slave4       = {integer},
		slave5       = {integer},
		slave6       = {integer},
		work_times   = {integer},
		update_time  = {integer},
		next_time    = {integer}

	}).


%% =======================================================================
%% =============================== end ===================================