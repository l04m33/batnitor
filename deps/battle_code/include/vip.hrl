
-record(vip,{
	gd_AccountID = 0,
	gd_Level = 0,
	gd_EndTime = 0,
	gd_LastTime = 0,
	gd_InfoList = []
	}).

-record(vip_types,{
	gd_AccountID = {integer},
	gd_Level = {integer},
	gd_EndTime = {integer},
	gd_LastTime = {integer},
	gd_InfoList = {term}
	}).

-define(VIP_TYPE_XUNXIAN,9).    %% 寻仙
-define(VIP_TYPE_ZHAOCAI,7).	%% 招财进宝
-define(VIP_TYPE_FLY,10).		%% 飞行
-define(VIP_TYPE_GUAJI,13).		%% 挂机
-define(VIP_TYPE_DUNGEON,14).	%% 副本
-define(VIP_TYPE_FLYSHOE,18).	%% 小飞鞋
-define(VIP_TYPE_HORN,20).		%% 小喇叭