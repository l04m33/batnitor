%% Author: Administrator
%% Created: 2012-6-11
%% Description: TODO: Add description to pet_extend

-define(CACHE_DRESS_REF, cache_util:get_register_name(dressing)).

%% 是否展示翅膀
-define(IS_SHOW_DRESS_N, 0).			%% 隐藏翅膀
-define(IS_SHOW_DRESS_Y, 1).			%% 展示翅膀

-define(FRESH_SKILL_MODE_ONCE,  1).			%% 刷新一次翅膀技能
-define(FRESH_SKILL_MODE_BATCH, 2).			%% 批量刷新翅膀技能

-define(FRESH_SKILL_TYPE_PRACTICE,	1).			%% 历练刷新技能
-define(FRESH_SKILL_TYPE_GOLD,		2).			%% 金币刷新技能

-record(dressing, {
				   gd_DressKey			= {0, none},	%% 类型 {AccountID, horse|fashione|wing}
				   gd_DressID			= 0,			%% 装备ID
				   gd_IsShow			= 0,			%% 是否展示
				   gd_TotExp			= 0,			%% 总经验
				   gd_LastTime			= 0,			%% 上次升级经验时间
				   gd_DressList			= [1],			%% 可用装扮
				   gd_IntLevel			= 0,			%% 强化等级
				   gd_AdvanceTotExp     = 0,			%% 进阶总经验
				   gd_PendantInfo       = [],			%% 坐骑挂饰信息
				   gd_PendantList		= []			%% 可用挂饰
			  }).

-record(dressing_types, {
				   gd_DressKey			= {{integer}, {term}},	%% 类型 {AccountID, horse|fashione|wing}
				   gd_DressID			= {integer},			%% 装备ID
				   gd_IsShow			= {integer},			%% 是否展示
				   gd_TotExp			= {integer},			%% 总经验
				   gd_LastTime			= {integer},			%% 上次升级经验时间
				   gd_DressList			= {term},				%% 可用装扮
				   gd_IntLevel			= {integer},			%% 强化等级
				   gd_AdvanceTotExp     = {integer},			%% 进阶总经验
				   gd_PendantInfo       = {term},				%% 坐骑挂饰信息
				   gd_PendantList		= {term}				%% 可用挂饰
			  }).

%% -record(cfg_skill,{
%% 				   cfg_SkillID        = 0,
%% 				   cfg_SkillType      = 0,
%% 				   cfg_SkillLevel     = 0,
%% 				   cfg_FrontSkill     = 0,
%% 				   cfg_SkillAttrRate  = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
%% 				   cfg_Speed          = 0
%% 				  }).