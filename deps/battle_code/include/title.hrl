%% Author: Administrator
%% Created: 2012-6-11
%% Description: TODO: Add description to pet_extend

-define(CACHE_GLOBAL_TITLE_REF, cache_util:get_register_name(title)).
-define(CACHE_GLOBAL_MAY_TITLE_REF, cache_util:get_register_name(may_title)).

%% 是否展示翅膀
-define(IS_SHOW_TITLE_N, 0).			%% 隐藏翅膀
-define(IS_SHOW_TITLE_Y, 1).			%% 展示翅膀

-define(TITLE_TYPE_SUPREMACY,			1).		%% 至尊称号
-define(TITLE_TYPE_LEGEND,				2).		%% 传奇称号
-define(TITLE_TYPE_ACHIEVEMENT,			3).		%% 成就称号
-define(TITLE_TYPE_HONOUR,				4).		%% 荣誉称号
-define(TITLE_TYPE_SPECIAL,				5).		%% 特殊称号

%% ========================== 传奇称号分类 ============================= %%
-define(LEGEND_TYPE_ROLE_LEVEL,			1).
-define(LEGEND_TYPE_TOWER_NUM,			2).
-define(LEGEND_TYPE_FIRST_KING,			3).
-define(LEGEND_TYPE_EQUIP_INTEN,		4).
-define(LEGEND_TYPE_WING_LEVEL,			5).
-define(LEGEND_TYPE_ABILITY,			6).
-define(LEGEND_TYPE_GUILD,				7).
-define(LEGEND_TYPE_ACHIEVEMENT,		8).
-define(LEGEND_TYPE_EQUIP_QUALITY,		9).
-define(LEGEND_TYPE_STONE_LEVEL,		10).
-define(LEGEND_TYPE_OFFICE_LEISURE,		11).
-define(LEGEND_TYPE_CANDYSTAR_STAGE,	12).
-define(LEGEND_TYPE_MAX_SKILL_LEVEL,	13).
-define(LEGEND_TYPE_MAIN_ABILITY,		14).

%% ========================= 荣誉称号分类 ============================= %%
-define(HONOUR_TYPE_MANOR,			1).		%% 领地战个人第一
-define(HONOUR_TYPE_GUILD,			2).		%% 公会排名第一
-define(HONOUR_TYPE_DEFENCE,		3).		%% 群魔乱舞
-define(HONOUR_TYPE_COMP,			4).		%% 比武冠军
-define(HONOUR_TYPE_HONOUR_SCORE,	5).		%% 荣誉排行版第一
-define(HONOUR_TYPE_KILL_BOSS,		6).		%% 击杀世界BOSS
-define(HONOUR_TYPE_MANOR_GUILD,	7).		%% 领地战公会第一

%% ========================== 至尊称号 ============================= %%
-define(SUPREMACY_INDEX_ABILITY,			1).		%% 战斗力至尊称号
-define(SUPREMACY_INDEX_ABILITY_LEN,		10).	%% 战斗力至尊称号数量
-define(SUPREMACY_INDEX_LEVEL,			11).	%% 等级至尊称号
-define(SUPREMACY_INDEX_LEVEL_LEN,		10).	%% 等级至尊称号数量
-define(SUPREMACY_INDEX_WEALTH,			21).	%% 财富至尊称号
-define(SUPREMACY_INDEX_WEALTH_LEN,		10).	%% 财富至尊称号数量
-define(SUPREMACY_INDEX_RECV_FLOWER,		31).	%% 收花至尊称号
-define(SUPREMACY_INDEX_RECV_FLOWER_LEN,	20).	%% 收花至尊称号数量
-define(SUPREMACY_INDEX_SEND_FLOWER,		51).	%% 送花至尊称号
-define(SUPREMACY_INDEX_SEND_FLOWER_LEN,	10).	%% 送花至尊称号数量
-define(SUPREMACY_INDEX_ARENA,			61).	%% 竞技场至尊称号
-define(SUPREMACY_INDEX_ARENA_LEN,		10).	%% 竞技场至尊称号数量
-define(SUPREMACY_INDEX_CROSS_TIANTI,          71).    %% 跨服天梯
-define(SUPREMACY_INDEX_CROSS_TIANTI_LEN,      10).    %% 竞技场至尊称号数量
-define(SUPREMACY_INDEX_CROSS_COMBAT,          81).    %% 跨服战斗力
-define(SUPREMACY_INDEX_CROSS_COMBAT_LEN,      10).    %% 竞技场至尊称号数量
-define(SUPREMACY_INDEX_CROSS_RATE,          91).    %% 跨服胜率
-define(SUPREMACY_INDEX_CROSS_RATE_LEN,      10).    %% 竞技场至尊称号数量
-define(SUPREMACY_INDEX_CROSS_LIANSHENG,          101).    %% 跨服连胜
-define(SUPREMACY_INDEX_CROSS_LIANSHENG_LEN,      10).    %% 竞技场至尊称号数量

%% ========================== 传奇称号 ============================= %%
-define(LEGEND_INDEX_50_LEVEL,			1).		%% 一骑绝尘
-define(LEGEND_INDEX_60_LEVEL,			2).		%% 拔山盖世
-define(LEGEND_INDEX_70_LEVEL,			3).		%% 独步天下
-define(LEGEND_INDEX_80_LEVEL,			4).		%% 冠绝一时
-define(LEGEND_INDEX_90_LEVEL,			5).		%% 天下无双
-define(LEGEND_INDEX_100_LEVEL,			6).		%% 千古传奇
-define(LEGEND_INDEX_3_TOWER,			7).		%% 霸气外露
-define(LEGEND_INDEX_6_TOWER,			8).		%% 雄霸一方
-define(LEGEND_INDEX_8_TOWER,			9).		%% 称雄四海
-define(LEGEND_INDEX_10_TOWER,			10).	%% 雄霸天下
-define(LEGEND_INDEX_FIRST_KING,		11).	%% 首任国主
-define(LEGEND_INDEX_7_EQUIP_LEVEL,		12).	%% 勇冠三军
-define(LEGEND_INDEX_10_EQUIP_LEVEL,	13).	%% 功力盖世
-define(LEGEND_INDEX_13_EQUIP_LEVEL,	14).	%% 呼风唤雨
-define(LEGEND_INDEX_15_EQUIP_LEVEL,	15).	%% 唯我独尊
-define(LEGEND_INDEX_10_WING_LEVEL,		16).	%% 羽化飞仙
-define(LEGEND_INDEX_ABILITY,			17).	%% 霸绝天下
-define(LEGEND_INDEX_GUILD_CHAIRMAN,	18).	%% 号令群雄
-define(LEGEND_INDEX_ACHIEVEMENT,		19).	%% 功成名就
-define(LEGEND_INDEX_PERFECT_EQUIP,		20).	%% 完美无缺
-define(LEGEND_INDEX_LEGEND_EQUIP,		21).	%% 我是传说
-define(LEGEND_INDEX_9_STONE_LEVEL,		22).	%% 通灵圣石
-define(LEGEND_INDEX_LEISURE,			23).	%% 唯我逍遥
-define(LEGEND_INDEX_ALCHEMY,			24).	%% 神器铸造师
-define(LEGEND_INDEX_MAX_SKILL_LEVEL,	25).	%% 踏破苍穹

%% ========================= 成就称号 ============================= %%
-define(ACHIEVEMENT_INDEX_ADD_GUILD,			1).		%% 一骑绝尘
-define(ACHIEVEMENT_INDEX_60_LEVEL,			2).		%% 拔山盖世
-define(ACHIEVEMENT_INDEX_70_LEVEL,			3).		%% 独步天下
-define(ACHIEVEMENT_INDEX_80_LEVEL,			4).		%% 冠绝一时
-define(ACHIEVEMENT_INDEX_90_LEVEL,			5).		%% 天下无双
-define(ACHIEVEMENT_INDEX_100_LEVEL,			6).		%% 千古传奇
-define(ACHIEVEMENT_INDEX_3_TOWER,			7).		%% 霸气外露
-define(ACHIEVEMENT_INDEX_6_TOWER,			8).		%% 雄霸一方
-define(ACHIEVEMENT_INDEX_8_TOWER,			9).		%% 称雄四海
-define(ACHIEVEMENT_INDEX_10_TOWER,			10).	%% 雄霸天下
-define(ACHIEVEMENT_INDEX_KILL_BOSS,			11).	%% 狩猎之王
-define(ACHIEVEMENT_INDEX_7_EQUIP_LEVEL,		12).	%% 勇冠三军
-define(ACHIEVEMENT_INDEX_10_EQUIP_LEVEL,	13).	%% 功力盖世
-define(ACHIEVEMENT_INDEX_13_EQUIP_LEVEL,	14).	%% 呼风唤雨
-define(ACHIEVEMENT_INDEX_15_EQUIP_LEVEL,	15).	%% 唯我独尊
-define(ACHIEVEMENT_INDEX_10_WING_LEVEL,		16).	%% 羽化飞仙
-define(ACHIEVEMENT_INDEX_ABILITY,			17).	%% 霸绝天下
-define(ACHIEVEMENT_INDEX_GUILD_CHAIRMAN,	18).	%% 号令群雄
-define(ACHIEVEMENT_INDEX_ACHIEVEMENT,		19).	%% 功成名就
-define(ACHIEVEMENT_INDEX_PERFECT_EQUIP,		20).	%% 完美无缺
-define(ACHIEVEMENT_INDEX_LEGEND_EQUIP,		21).	%% 我是传说
-define(ACHIEVEMENT_INDEX_9_STONE_LEVEL,		22).	%% 通灵圣石
-define(ACHIEVEMENT_INDEX_LEISURE,			23).	%% 唯我逍遥
-define(ACHIEVEMENT_INDEX_ALCHEMY,			24).	%% 神器铸造师
-define(ACHIEVEMENT_INDEX_MAX_SKILL_LEVEL,	25).	%% 踏破苍穹

%% ========================= 荣誉称号 ============================= %%
-define(HONOUR_INDEX_MANOR_NO1,			1).		%% 匹马纵横
-define(HONOUR_INDEX_GUILD_NO1,			2).		%% 巍巍之帮
-define(HONOUR_INDEX_DEFENCE_NO1,		3).		%% 狂乱的贵公子
-define(HONOUR_INDEX_DEFENCE_NO2,		4).		%% 攘夷志士
-define(HONOUR_INDEX_DEFENCE_NO3,		5).		%% 攘夷志士
-define(HONOUR_INDEX_DEFENCE_NO4,		6).		%% 攘夷志士
-define(HONOUR_INDEX_LOW_COMP_NO1,		7).		%% 初级比武冠军
-define(HONOUR_INDEX_MID_COMP_NO1,		8).		%% 中级比武冠军
-define(HONOUR_INDEX_HIGH_COMP_NO1,		9).		%% 高级比武冠军
-define(HONOUR_INDEX_HONOUR_SCORE_NO1,	10).	%% 勇冠天下
-define(HONOUR_INDEX_KILL_BOSS,			11).	%% 狩猎之王
-define(HONOUR_INDEX_MANOR_GUILD_NO1,	12).	%% 领地战公会第一

%% ========================= 特殊称号 ============================= %%
-define(SPECIAL_INDEX_OLD_PLAYER,	1).		%% 我是老玩家
-define(SPECIAL_INDEX_INSTRUCTOR,	2).		%% 新手指导员
-define(SPECIAL_INDEX_BLUE_VIP,		3).		%% 蓝砖VIP
-define(SPECIAL_INDEX_PURPLE_VIP,	4).		%% 紫钻VIP
-define(SPECIAL_INDEX_KING_VIP,		5).		%% 至尊VIP



-record(title, {
				 key           = {0, 0},
				 gd_TitleList  = [],
				 gd_UseTitleID = 0
				}).

-record(title_types, {
					   key           = {{integer}, {integer}},		%% 称号类型
					   gd_TitleList  = {term},						%% 称号列表
					   gd_UseTitleID = {integer}
					  }).

-record(may_title,{
				   player_id = 0,
				   titles = []}).

-record(may_title_types,{
					player_id = {integer},
					titles = {term}}).

-record(fest_title,{
					index,type,money_type,amount,rate
					}).