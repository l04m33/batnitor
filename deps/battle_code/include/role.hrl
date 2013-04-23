-define(MAX_EMPLOYABLE,  5).		%% 初始可以招募的佣兵数


%% 4个可提升体天赋的属性的index值
-define (TALENT_LILIANG, 		1).
-define (TALENT_YUANSHENG, 		2).
-define (TALENT_TIPO, 			3).
-define (TALENT_MINJIE,			4).

-define(NORMAL_ZHAOSHU,			1).	%% 普通诏书
-define(BETTER_ZHAOSHU,			2).	%% 明智诏书
-define(BEST_ZHAOSHU,			3).	%% 顶级诏书

-define(FREE_NORMAL_ZHAOSHU,	1).	%% 普通诏书的免费次数

-define(CAREER_HUWEI, 			1). %% 虎卫
-define(CAREER_MENGJIANG,		2). %% 猛将
-define(CAREER_JUNSHI_ATT, 		3). %% 军师(攻击型)
-define(CAREER_JUNSHI_HELP, 	4). %% 军师(辅助型)

%% 技能类型
-define(SKILL_SPECIAL, 	1).		%% 无双技能
-define(SKILL_GIFT, 	2).		%% 天赋技能
-define(SKILL_NORMAL, 	3).		%% 普通被动技能（可以被刷新的）
-define(SKILL_NORMAL2, 	4).		%% 普通主动技能（不可以被刷新的）
-define(SKILL_GUILD, 	5).		%% 公会普通被动技能（只有主角有）

%% 技能效果类型
-define(SKILL_EFFECT_BATTLE,	1). %% 战斗中有效
-define(SKILL_EFFECT_FIXED,		2). %% 固定加成



%% =========================== 角色进程的state ======================
-record (role_state, {
			player_id = 0,
			player_pid = none
	}).

%%=================== 佣兵记录  =====================================
-record(role, {
			key                = none,		%% 佣兵记录的key为一个记录:{player_id, mer_id}
			gd_roleRank        = 0,			%% 角色类别，1：为领主佣兵，0：其他佣兵
			gd_isFired         = 0,			%% 是否为玩家已解雇的佣兵，0：没解雇， 1：已解雇
			gd_roleLevel       = 1,			%% 等级
			gd_isBattle        = 0,			%% 是否出战，若不出战，则为0，否则为站位的位子（1到6）
			gd_exp             = 0,			%% 当前经验
			gd_skill           = [],			%% 技能列表
			
			%% 4 foster attribute
			gd_fliliang        = 0,			%% 培养所获得腕力
			gd_fyuansheng      = 0,			%% 培养所获得元神
			gd_ftipo           = 0,			%% 培养所获得体魄
			gd_fminjie         = 0,			%% 培养所获得敏捷
			
			gd_tliliang        = 0,			%% 腕力天赋的提升值
			gd_tyuansheng      = 0,			%% 元神天赋的提升值
			gd_ttipo           = 0,			%% 体魄天赋的提升值
			gd_tminjie         = 0,			%% 敏捷天赋的提升值
			
			gd_careerID        = 0,			%% 职业类型编号
			gd_roleSex         = 0,			%% 性别，0：男，1：女
			%% 4个基础属性
			gd_liliang         = 0,			%% 腕力
			gd_yuansheng       = 0,			%% 元神
			gd_tipo            = 0,			%% 体魄
			gd_minjie          = 0,			%% 敏捷	
			
			gd_liliangTalent   = 0,			%% 腕力的天赋
			gd_yuanshengTalent = 0,			%% 元神的天赋
			gd_tipoTalent      = 0,			%% 体魄的天赋
			gd_minjieTalent    = 0,			%% 敏捷的天赋
			
			gd_speed           = 0,			%% 攻击速度
			gd_baoji           = 0,			%% 暴击
			gd_shanbi          = 0,			%% 闪避
			gd_gedang          = 0,			%% 格挡
			gd_mingzhong       = 0,			%% 命中率
			gd_zhiming         = 0,			%% 致命
			gd_xingyun         = 0,			%% 幸运
			gd_fanji           = 0,			%% 反击
			gd_pojia           = 0,			%% 破甲
			
			gd_angry 		   = 0,			%% 怒气下限
			gd_max_angry	   = 100,		%% 怒气上限
			gd_up_angry		   = 0,			%% 怒气提升

			gd_currentHp       = 0,			%% 当前血量
			gd_maxHp           = 0,			%% 最大血量
			p_def              = 0,			%% 物理防御
			m_def              = 0,			%% 魔法防御
			p_att              = 0,			%% 攻击力
			m_att              = 0,			%% 魔攻
			star_lv            = 0,			%% 武将星级
			star 			   = 0,
			gd_name            = ""			%% 佣兵名称
	}).

-record(role_types, {
			key                = {{integer}, {integer}},		
			gd_roleRank        = {integer},		
			gd_isFired         = {integer},		
			gd_roleLevel       = {integer},		
			gd_isBattle        = {integer},	
			gd_exp             = {integer},		
			gd_skill           = {term},	
			
			%% 4 foster attr,
			gd_fliliang        = {integer},		
			gd_fyuansheng      = {integer},		
			gd_ftipo           = {integer},		
			gd_fminjie         = {integer},	
			
			gd_tliliang        = {integer},	
			gd_tyuansheng      = {integer},	
			gd_ttipo           = {integer},	
			gd_tminjie         = {integer},	
			
			gd_careerID        = {integer},
			gd_roleSex         = {integer},
			%% 4个基础属性
			gd_liliang         = {integer},		
			gd_yuansheng       = {integer},	
			gd_tipo            = {integer},		
			gd_minjie          = {integer},		
			
			gd_liliangTalent   = {integer},	
			gd_yuanshengTalent = {integer},	
			gd_tipoTalent      = {integer},	
			gd_minjieTalent    = {integer},	

			gd_speed           = {integer},		
			gd_baoji           = {integer},		
			gd_shanbi          = {integer},		
			gd_gedang          = {integer},		
			gd_mingzhong       = {integer},		
			gd_zhiming         = {integer},		
			gd_xingyun         = {integer},		
			gd_fanji           = {integer},		
			gd_pojia           = {integer},		
			
			gd_angry		   = {integer},
			gd_max_angry	   = {integer},
			gd_up_angry        = {integer},

			gd_currentHp       = {integer},
			gd_maxHp           = {integer},
			p_def              = {integer},
			m_def              = {integer},
			p_att              = {integer},
			m_att              = {integer},
			star_lv            = {integer},
			star 			   = {integer},
			gd_name            = {string}
	}).

-record(role_update_attri, {
			gd_liliang    = 0,			%% 腕力
			gd_yuansheng  = 0,			%% 元神
			gd_tipo       = 0,			%% 体魄
			gd_minjie     = 0,			%% 敏捷	
			
			gd_speed      = 0,			%% 攻击速度
			gd_baoji      = 0,			%% 暴击
			gd_shanbi     = 0,			%% 闪避
			gd_gedang     = 0,			%% 格挡
			gd_mingzhong  = 0,			%% 命中率
			gd_zhiming    = 0,			%% 致命
			gd_xingyun    = 0,			%% 幸运
			gd_fanji      = 0,			%% 反击
			gd_pojia      = 0,			%% 破甲
			
			gd_angry 	  = 0,
			gd_max_angry  = 0,
			gd_up_angry   = 0,

			gd_currentHp  = 0,			%% 当前血量
			gd_maxHp      = 0,			%% 最大血量
			p_def         = 0,			%% 物理防御
			m_def         = 0,			%% 魔法防御
			p_att         = 0,			%% 攻击力
			m_att         = 0			%% 魔攻
	}).

-record (role_data, {
			gd_accountId      = 0,
			gd_EmployableList = [],		%% 玩家可招募的佣兵列表
			% gd_fosterFlag     = 1,		%% 记录培养的阶段
			% gd_junwei		  = 0,		%% 用于招募神将的君威
			gd_FollowRole	  = 0		%% 跟随将领
	}).
-record (role_data_types, {
			gd_accountId      = {integer},
			gd_EmployableList = {term},
			% gd_fosterFlag     = {integer},
			% gd_junwei     	  = {integer},
			gd_FollowRole	  = {integer}
	}).

-record(skill_info, {
	mode_id       = 0,
	class_id      = 0,	%% 技能属性分类（如都是暴击类或是连击类技能）
	type          = 0,	%% 技能类型
	effect        = 0,	%% 技能效果（1：战斗中有效，2：固定加成）
	level_up_exp  = 0,	%% 升级所需经验
	next_skill_id = 0,	%% 升级后的技能原型id
	point		  = 0 	%% 增加的战斗力
	}).


-record (foster_rate, {
	liliang   = 0,			%% 力量的培养概率
	yuansheng = 0,			%% 元神的培养概率
	tipo      = 0,			%% 体魄的培养概率
	minjie    = 0			%% 敏捷的培养概率
	}).

-record(talent_rate, {
	liliang   = 0,			%% 力量天赋的提升概率
	yuansheng = 0,			%% 元神天赋的提升概率
	tipo      = 0,			%% 体魄天赋的提升概率
	minjie    = 0			%% 敏捷天赋的提升概率
	}).

-record (junwei_role, {
	gd_accountId = 0,
	gd_junling = 0,
	gd_luck = 0,
	gd_angry = 0,
	gd_lastTime = 0
	}).

-record (junwei_role_types, {
	gd_accountId = {integer},
	gd_junling = {integer},
	gd_luck = {integer},
	gd_angry = {integer},
	gd_lastTime = {integer}
	}).

