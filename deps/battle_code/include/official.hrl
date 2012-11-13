%% 官职、官邸、器魂和神器的头文件

-define(QIHUN_JING,			1).		%% 精
-define(QIHUN_LI,			2).		%% 力
-define(QIHUN_YUAN,			3).		%% 元
-define(QIHUN_DUN,			4).		%% 盾
-define(QIHUN_YU,			5).		%% 御
-define(QIHUN_ZHUN,			6).		%% 准
-define(QIHUN_SHAN,			7).		%% 闪
-define(QIHUN_YUN,			8).		%% 运
-define(QIHUN_SU,			9).		%% 速
-define(QIHUN_BAO,			10).	%% 暴

-define(MIN_QIHUN_ID,	?QIHUN_JING).	%% 最小的器魂id
-define(MAX_QIHUN_ID,	 ?QIHUN_BAO).	%% 最大的器魂id

%%俸禄状态
-define(YILING_FENGLU,  1).
-define(MEILING_FENGLU, 0).

-record(qi_hun, {
	gd_accountId    = 0,			%% 玩家id
	gd_levelingId   = ?MIN_QIHUN_ID,%% 当前正在修炼的或下一个要修炼的器魂
	gd_level        = 0,			%% 上次修炼的器魂的等级
	gd_isInLeveling = 0,			%% 是否处于修炼状态(0：没有，1：处于)
	gd_beginTime    = 0,			%% 上次修炼开始时间
	gd_stage        = 0 			%% 神器阶段id
	}).

-record(qi_hun_types, {
	gd_accountId    = {integer},
	gd_levelingId   = {integer},
	gd_level        = {integer},
	gd_isInLeveling = {integer},
	gd_beginTime    = {integer},	
	gd_stage    	= {integer}	
	}).

-record(qihun_pinjie, {
	gd_accountId = 0,			%% 玩家id
	gd_jing      = {0, 0},		%% 以下的字段都是{品阶等级, 完美度}
	gd_li        = {0, 0},
	gd_yuan      = {0, 0},
	gd_dun       = {0, 0},
	gd_yu        = {0, 0},
	gd_zhun      = {0, 0},
	gd_shan      = {0, 0},
	gd_yun       = {0, 0},
	gd_su        = {0, 0},
	gd_bao       = {0, 0}
	}).

-record(qihun_pinjie_types, {
	gd_accountId = {integer},		
	gd_jing      = {term},	
	gd_li        = {term},
	gd_yuan      = {term},
	gd_dun       = {term},
	gd_yu        = {term},
	gd_zhun      = {term},
	gd_shan      = {term},
	gd_yun       = {term},
	gd_su        = {term},
	gd_bao       = {term}
	}).