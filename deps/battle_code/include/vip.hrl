
-record(vip,{
	gd_AccountID    = 0,
	gd_Level        = 0,
	gd_EndTime      = 0,
	gd_LastTime     = 0,
	gd_InfoList     = [],
	gd_FeedbackGold = 0,
	gd_BufferTime   = 0,
	gd_First        = 0
	}).

-record(vip_types,{
	gd_AccountID    = {integer},
	gd_Level        = {integer},
	gd_EndTime      = {integer},
	gd_LastTime     = {integer},
	gd_InfoList     = {term},
	gd_FeedbackGold = {integer},
	gd_BufferTime   = {integer},
	gd_First        = {integer}
	}).

-define(VIP_TYPE_FEEDBACK,1).	%% 绑定元宝回馈
-define(VIP_TYPE_BUFF,2).		%% VIP BUFF加成
-define(VIP_TYPE_HUNSHI,3).		%% 魂石
-define(VIP_TYPE_SILVER,4).    %% 银币
-define(VIP_TYPE_FEED,5).		%% 饲料
-define(VIP_TYPE_FLYSHOE,6).	%% 小飞鞋
-define(VIP_TYPE_ZHAOCAI,7).	%% 招财进宝
-define(VIP_TYPE_XUNXIAN,8).    %% 寻仙
-define(VIP_TYPE_FLY,999).		%% 飞行
-define(VIP_TYPE_GUAJI,9).		%% 挂机
-define(VIP_TYPE_DUNGEON,10).	%% 副本
-define(VIP_TYPE_ARENA,12).		%% 竞技场
-define(VIP_TYPE_HORN,13).		%% 小喇叭
-define(VIP_TYPE_YUNBIAO,14).	%% 运镖
-define(VIP_TYPE_JUNGONG_TASK,15).%% 军工任务
-define(VIP_TYPE_DAZUO,21).		%% 打坐




