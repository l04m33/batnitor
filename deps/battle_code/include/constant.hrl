-define(ARENA_NUM,500). %%竞技场最大人数

-define(TIME_OFFSET,3). %%午夜时间偏移数，三点重置每日事件

-define(MAX_FCM_ONLINE_TIME, 3*60*60).			%% 防沉迷最大在线时长
-define(FCM_OFFLIEN_RESET_TIME, 5*60*60).		%% 防沉迷离线重置时长

-define(MAX_MOVE_LEN, 10).
-define(CAN_JUMP_SCOPE, 4).

-define(S_IDLE, 0).							%% 玩家状态清除		 0x0000 0000 = 0
-define(S_RUN_BUSINESS, 1).					%% 玩家处于跑商行走状态  0x0000 0001 = 1(第7位0表示背包是黄色)
-define(S_RUN_BUSINESS2, 65).				%% 玩家处于跑商行走状态  0x0100 0001 = 65(第7位1表示背包是紫色)
-define(S_RUN_BUSINESS_BATTLE, 3).			%% 玩家处于跑商战斗状态 0x0000 0011 = 3
-define(S_PVP_BATTLE, 2).					%% 处于在线竞技战斗状态 0x0000 0010 = 2
-define(S_PVE_BATTLE, 4).                   %% 玩家在和野外怪战斗
-define(S_NORMAL_RUNNING, 5).				%% 普通跑步模式
-define(S_2GOLD_RUNNING, 6).				%% 2金跑步模式
-define(S_10GOLD_RUNNING, 7).				%% 10金跑步模式
-define(S_20GOLD_RUNNING, 8).				%% 20金跑步模式

-define(MONSTER_BATTLE_IDLE, 0).			%% 在线PVP中怪物的空闲状态
-define(MONSTER_IN_BATTLE, 1).				%% 在线PVP中怪物的战斗中状态
-define(MONSTER_BATTLE_WIN, 2).				%% 在线PVP中怪物的战斗胜利状态
-define(MONSTER_BATTLE_LOSE, 3).			%% 在线PVP中怪物的战斗失败状态


-define(COLOR_WHITE, 1).		%% 白色
-define(COLOR_GREEN, 2).		%% 绿色
-define(COLOR_BLUE, 3).			%% 蓝色
-define(COLOR_PURPLE, 4).		%% 紫色
-define(COLOR_ORANGE, 5).		%% 橙色
-define(COLOR_RED, 6).			%% 红色

-define(ACCOUNT_RANK_VISITOR, 1).	%% 游客
-define(ACCOUNT_RANK_PLAYER, 2).	%% 普通玩家
-define(ACCOUNT_RANK_GM, 3).		%% gm
-define(ACCOUNT_RANK_SENIOR, 6).	%% 资深玩家

-define(HIS_RECORD_ARENA,his_record_arena).

-define(BATTLE_PROGRESS_NOT_IMPLEMENT,0).


-define(SECONDS_PER_MINUTE, 60).
-define(SECONDS_PER_HOUR, 3600).
-define(SECONDS_PER_DAY, 86400).
-define(DAYS_PER_YEAR, 365).
-define(DAYS_PER_LEAP_YEAR, 366).
-define(DAYS_PER_4YEARS, 1461).
-define(DAYS_PER_100YEARS, 36524).
-define(DAYS_PER_400YEARS, 146097).
-define(DAYS_FROM_0_TO_1970, 719528).

%%数据库连接
%% -define(DB, mod_configure:read(db,"server.config")).
%% -define(DB_HOST, mod_configure:read(db_host,"server.config")).
%% -define(DB_PORT, mod_configure:read(db_port,"server.config")).
%% -define(DB_USER, mod_configure:read(db_user,"server.config")).
%% -define(DB_PASS, mod_configure:read(db_pass,"server.config")).
%% -define(DB_NAME, mod_configure:read(db_name,"server.config")).
%% -define(DB_ENCODE, mod_configure:read(db_encode,"server.config")).
-define(DB, 		util:get_app_env(db)).
-define(DB_HOST, 	util:get_app_env(db_host)).
-define(DB_PORT, 	util:get_app_env(db_port)).
-define(DB_USER, 	util:get_app_env(db_user)).
-define(DB_PASS, 	util:get_app_env(db_pass)).
-define(DB_NAME, 	util:get_app_env(db_name)).
-define(DB_ENCODE, 	util:get_app_env(db_encode)).


-define(USER_LOG_DB, 	sd_mysql_conn_user_log).
-define(USER_LOG_DB_NAME, 	util:get_app_env(user_log_db_name)).

-define(SERVER_OFF_SET,util:get_app_env(server_offset)).
-define(RPC_TIMEOUT,5).

-define(MAX_ACCOUNT_PER_SERVER, 1000000).