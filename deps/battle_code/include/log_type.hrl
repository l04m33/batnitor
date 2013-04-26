%% 这个头文件用于定义用户日志数据的宏定义，如金币或银币消耗的类别


% %% ======================== 金币改变的操作类型 ==========================
% -define(GOLD_FROM_GM,1999).					%%GM获得
% -define(GOLD_CHARGE_MONEY, 1001). 			%% 1;		// 充钱
% -define(GOLD_FROM_MAIL, 1002).				%% ;		2056// 邮件寄送
% -define(GOLD_FROM_USE_ITEM, 1003).			%% 16;      2061// 使用物品获得
% -define(GOLD_FROM_WLSD,             1004).	%%         2078// 藏宝图获得
% -define(GOLD_ACHIEVE, 1008). 				%% 8;		// 成就
% -define(GOLD_TARGET, 1021).					%% 21;		// 目标系统获得
% -define(GOLD_FROM_CONSUME,1022).			%% 22;		// 回馈系统获得
% -define(GOLD_FROM_GUIDE,1023).				%% 23;		//	每日指引获得
% -define(GOLD_FROM_VIP,1024).				%% 24;		// VIP充值回馈获得
% -define(GOLD_WEALTH_GAMBLE,1025).				%%招财进宝
% -define(GOLD_FROM_GIFT,	1026).				%% 礼包获得



% -define(GOLD_COMPLETE_DAILY_TASK, 2010).	 	%% ;		// 直接完成日常任务
% -define(GOLD_GUILD_CREATE, 2020). 			%% ;		// 公会创建
% -define(GOLD_BUY_ITEM_COST, 2028).			%% ;		// 用于商城购买物品
% -define(GOLD_INTENSIFY_EQUIP_COST, 2029).		%% ;		// 用于装备强化保护
% -define(GOLD_EXTEND_BAG_COST, 2030).			%% ;		// 用于扩充背包格子
% -define(GOLD_EXTEND_BANK_COST, 2031).			%% ;		// 用于扩充仓库格子
% -define(GOLD_XUNXIAN_COST, 2034).				%% ;		// 用于寻仙
% -define(GOLD_FOSTER_CARD_COST, 			2060).%%	// 用于购买培养卡
% -define(GOLD_FIX_SKILL_COST, 2062).			%%      	// 刷新技能时用于固定技能
% -define(GOLD_BUY_ZHAOSHU, 2063).				%%      	// 用于购买诏书 
% -define(GOLD_FEED_HORSE, 2064).        		%% 			// 喂养坐骑
% -define(GOLD_TALENT_CARD_COST, 2065).     	%% 			// 用于购买天赋卡
% -define(GOLD_HORSE_EQUIP_COST, 2066).     	%% 			// 用于购买坐骑时装
% -define(GOLD_FINISH_QIHUN_LEVELING, 2067).	%% 			// 用于立刻完成器魂修炼
% -define(GOLD_FINISH_QIHUN_CD, 		2068).	%% 			// 用于清除器魂修炼的cd
% -define(GOLD_QILING_COST, 2070).				%%			// 用于启灵装备
% -define(GOLD_XILIAN_ITEM_COST, 2071).			%%			// 用于洗练装备
% -define(GOLD_UP_PINJIE, 	2072).			%%			// 用于提升器魂品阶
% -define(GOLD_BUG_DUNGEON_TIMES, 	2073).	%%			// 用于购买进入副本的次数
% -define(GOLD_UPGRATE_ITEM_COST,	2074).		%%			// 用于升级装备
% -define(GOLD_OPEN_LAND, 	2075).			%%			// 用于开垦土地
% -define(GOLD_REFRESH_SEED, 	2076).			%%			// 用于刷新种子的品质
% -define(GOLD_CLEAN_PLANTING_CD, 	2077).	%%			// 用于清除种植等待cd
% -define(GOLD_CLEAN_WORK_CD,         2079).	%%         // 用于清除奴隶劳作cd
% -define(GOlD_BUY_GUAJI_COST,		2080).	%%			// 用于购买挂机
% -define(GOlD_REFRESH_BIAOCHE_COST,2081).		%%		// 用于刷新镖车
% -define(GOlD_ZHUANYUN_COST,		2082).		%%		// 用于转运
% -define(GOLD_RESET_TOWER,		2083).		%%	// 用于重置英雄塔
% -define(GOLD_USE_TO_INSPIRE,  2084).			%%      用于鼓舞
% -define(GOLD_USE_TO_CLEAN_CD,  2085).		%%用于清除boss战斗cd
% % -define(GOLD_CLEAR_ENTER_GUILD, 2087).		%% 退出后加入公会要24小时的cd，清除cd要扣费

% -define(GOLD_FRESH_MYSTICAL_SHOP,2089).		%% 用于刷新神秘商店物品
% -define(GOLD_BUY_MYSTICAL_SHOP_ITEM, 2090).	%% 用于购买神秘商店物品
% -define(GOlD_REFRESH_JUNGONGTASK, 2091).			%% 用于刷新军工任务
% -define(GOlD_AUTO_COMPLETE_JUNGONGTASK, 2092).	%% 用于自动完成军工任务
% -define(GOLD_CLEAR_COMP_CD_COST,2093).			%% 清除比武场CD
% -define(GOLD_USE_COMP_REVIE_DRUG,2094).			%% 比武场使用复活丹
% -define(GOLD_TOWER_FINISH_CHALLENGE,2095). 	%% 马上完成爬塔自动挑战
% -define(GOLD_COST_JUNLING,2096).					%% 购买军令
% -define(GOLD_LOTTERY,				2097).	%% 投壶花费
% % -define(GOLD_ONE_KEY_COMPLETE_QIHUN,2098). %% 一键完成器魂升级
% -define(GOLD_USE_EVOKE_DRESS,		2099).	%% 幻化外形装扮消耗（包括坐骑、翅膀等）
% -define(GOLD_USE_ADD_DRESS_EXP,		2100).	%% 增加外形装扮消耗（包括坐骑、翅膀等）
% -define(GOLD_UP_VIP,				2101).	%% 用于升级VIP
% -define(GOLD_CHUANGONG,				2102).	%% 用于传功
% -define(GOLD_SPEED_UP_DUNGEON_GUAJI,2103).	%% 用于副本挂机加速


% %% ======================== 银币改变的操作类型 ==========================
% -define(SILVER_FROM_GM,	1999).	%%GM获得
% -define(SILVER_TASK, 1003). 					%% 3    任务所得
% -define(SILVER_FROM_MONSTER, 1005).			%% 5    怪物掉落
% -define(SILVER_FROM_ACHIEVE, 1009). 			%% 9    成就所得
% -define(SILVER_FROM_PRAY, 1015). 				%% 15 祝福获得
% -define(SILVER_FROM_ITEM_SELL, 1021).			%% 21 出售物品获得
% -define(SILVER_FROM_XUNXIAN_SELL_ITEM, 1023).	%% 23 寻仙物品出售获得
% -define(SILVER_FROM_GUILD_COMP, 1028).		%% 28 公会战奖励
% -define(SILVER_FROM_MAIL, 1031).				%% 31   邮件寄送
% -define(SILVER_FROM_USE_ITEM, 1034).			%% 34   使用物品获得
% -define(SILVER_FROM_PLANTING, 1035).			%% 35   种植银币种子所得
% -define(SILVER_FROM_FENGLU,					1036).	%% 领取俸禄获得
% -define(SILVER_FROM_SLAVE_WORK,				1037).	%% 奴隶劳作所得
% -define(SILVER_FROM_SLAVE_TAX,				1038).	%% 从奴隶那得到的税金
% -define(SILVER_FROM_ROB_YUNBIAO,            1039).    %%从劫镖得到
% -define(SILVER_FROM_WATERING, 1040).                  %% 帮助好友浇水获得
% -define(SILVER_FROM_ARENA_DAILY_AWARD,      1041).    %%竞技场每天奖励
% -define(SILVER_FROM_WLSD,					1042). 	%%藏宝图所得
% -define(SILVER_FROM_YUNBIAO,				1043).    %%运镖获得
% -define(SILVER_FROM_BOSS_BATTLE,			1044).	%%boss战斗获得
% -define(SILVER_FROM_GUILD_SKILL,            1045).    %%工会技能升级失败退回钱;
% -define(SILVER_FROM_TARGET,					1046).    %%目标系统获得
% -define(SILVER_FROM_JUNGONGTASK,			1047).	%%军功任务中获得
% -define(SILVER_FROM_CONSUME,				1048).	%%回馈活动获得
% -define(SILVER_FROM_GUIDE,					1049).	%%每日指引获得
% -define(SILVER_FROM_ARENA_CHALLENGE_AWARD,	1050).	%%竞技场挑战获得
% -define(SILVER_FROM_WEALTH,					1051).	%%招财进宝获得
% -define(SILVER_FROM_COMP,					1052).	%%比武奖励获得
% -define(SILVER_FROM_MARSTOWER,				1053).	%%爬塔自动挑战获得
% -define(SILVER_FROM_DEFENCE_MON,			1054).	%%群魔乱舞打怪掉落
% -define(SILVER_FROM_VIP,					1055).	%%VIP获得
% -define(SILVER_FROM_GIFT,					1056).	%%礼包获得

% -define(SILVER_GUILD_CREATE,2005).			%% 1004   公会创建消耗		wyx20111128 add
% -define(SILVER_BUY_ITEM_COST, 2009).        %% 1008   用于商店购买物品
% -define(SILVER_INTENSIFY_EQUIP_COST, 2013).	%% 1012   用于装备强化
% -define(SILVER_XUNXIAN_COST, 2015).			%% 1014   用于寻仙
% -define(SILVER_EMPLOY_ROLE, 2019).          %% 1018   用于招募佣兵
% -define(SILVER_REFRESH_SKILL, 2020).        %% 1019   用于刷新技能
% -define(SILVER_FEED_HORSE, 2021).        	%% 1020   喂养坐骑
% -define(SILVER_LEVELING_QIHUN, 2022).       %% 1021   用于修炼器魂
% -define(SILVER_XILIAN_ITEM_COST,2023).		%% 1022	  用于洗练装备
% -define(SILVER_UP_TALENT,2024).				%% 1023	  用于提升天赋
% -define(SILVER_FOSTER,2025).				%% 1024	  用于武将培养
% -define(SILVER_UPQUALITY_ITEM_COST,2026).	%% 1025   用于提升装备品质
% -define(SILVER_UPGRATE_ITEM_COST,2027).		%% 1026	  用于升级装备
% -define(SILVER_INLAY_ITEM_COST,2028).		%% 1027	  用于镶嵌宝石
% -define(SILVER_BACKOUT_ITEM_COST,2029).		%% 1028   用于拆卸宝石
% -define(SILVER_COMPOSE_ITEM_COST,2030).		%% 1029   用于合成宝石
% -define(SILVER_CONVERT_ITEM_COST,2031).		%% 1030   用于转化宝石
% -define(SILVER_CARVE_ITEM_COST,2032).		%% 1031   用于雕刻宝石
% -define(SILVER_USE_TO_INSPIRE,2033).		%% 27000     用于鼓舞
% -define(SILVER_UPGRATE_SKILL,2034).			%% 1041   用于升级技能
% -define(SILVER_LEARN_GUILD_SKILL, 2035).     %%        用于升级工会技能
% -define(SILVER_BUY_MYSTICAL_SHOP_ITEM, 2036).		%% 用于购买神秘商店物品
% -define(SILVER_MARSTOWER_AUTO_CHALLENGE,2037). %% 用于爬塔自动挑战
% -define(SILVER_ONE_KEY_COMPLETE_QIHUN,2038). %% 一键完成器魂升级
% -define(SILVER_USE_ADD_DRESS_EXP,     2039).	%% 增加外形装扮消耗（包括坐骑、翅膀等）
% -define(SILVER_CHUANGONG,			  2040).	%% 用于传功
% -define(SILVER_COST_DUNGEON_GUAJI,	  2041).	%% 用于副本挂机

% %% 积分
% -define(POINT_FROM_TASK,			  1001).	%% 任务获得
% -define(POINT_FROM_ACTIVITY,		  1002).	%% 活动获得
% -define(POINT_FROM_JUNWEI_ROLE,		  1003).	%% 军威将领获得
% -define(POINT_FROM_GM,				  1004).	%% gm获得
% -define(POINT_CHANGE,				  1005).	%% 兑换获得
% -define(POINT_FROM_DONATE,			  1006).	%% 捐献获得

% -define(POINT_USE_CHANGE,			  2001).	%% 用于兑换物品
% -define(POINT_USE_EMPLOY,			  2002).	%% 用于招募将领
% -define(POINT_USE_SKILL,			  2003).	%% 用于学习提升技能


% %%有一些操作，同时使用多种东西的
% -define(ONE_KEY_COMPLETE_QIHUN,?SILVER_ONE_KEY_COMPLETE_QIHUN).

% %% ========================= 物品来源去向 ==========================
% -define(ITEM_FROM_GM,				1999).		%% GM获得
% -define(ITEM_FROM_BAG_GIFT,			1001).		%% 使用礼包获得
% -define(ITEM_FROM_STONE_BACKOUT,	1003).		%% 宝石拆卸获得
% -define(ITEM_FROM_FRAGMANT_COMPOS,	1004).		%% 碎片合成获得
% -define(ITEM_FROM_XUNXIAN,			1007).		%% 炼金获得
% -define(ITEM_FROM_ACHIEVE,			1012).		%% 成就获得
% -define(ITEM_FROM_BATTLE,			1013).		%% 战斗获得
% -define(ITEM_FROM_TASK,				1015).		%% 任务获得
% -define(ITEM_FROM_MAIL,				1016).		%% 邮件获得
% -define(ITEM_FROM_SPLIT,			1021).		%% 拆分后获得
% -define(ITEM_FROM_GUILD_COMP,		1023).		%% 公会竞赛奖励
% -define(ITEM_FROM_FIRST_CHARGE,		1024).		%% 首次充值奖励礼包
% -define(ITEM_FROM_USE_ITEM,			1026).		%% 使用物品获得
% -define(ITEM_FROM_QILING,			1027).		%% 启灵获得
% -define(ITEM_FROM_XILIAN,			1028).		%% 洗练获得
% -define(ITEM_FROM_UPQUALITY,		1029).		%% 提升品质获得
% -define(ITEM_FROM_UPGRATE,			1030).		%% 升级获得
% -define(ITEM_FROM_INLAY,			1031).		%% 镶嵌宝石获得
% -define(ITEM_FROM_WLSD,				1032).       %% 从藏宝图获得
% -define(ITEM_FROM_COMPOSE,			1033).		%% 合成宝石获得
% -define(ITEM_FROM_CONVERT,			1034).		%% 转化宝石获得
% -define(ITEM_FROM_CARVE,			1035).		%% 雕刻宝石获得
% -define(ITEM_FROM_DUNGEON_AWARD,	1036).		%% 打完副本的奖励
% -define(ITEM_FROM_MARSTOWER,		1037).		%% 英雄塔获得
% -define(ITEM_FROM_BUY_HORN,         1038).       %% 用银币买小喇叭获得
% -define(ITEM_FROM_EMPLOY,			1039).		%% 招募时获得
% -define(ITEM_FROM_TARGET,			1040).  		%% 目标系统获得
% -define(ITEM_FROM_CONSUME,			1041).		%% 回馈活动获得
% -define(ITEM_FROM_GUIDE,			1042).		%% 每日指引获得
% -define(ITEM_FROM_HONOUR_EXCHANGE,	1043).		%% 荣誉点兑换获得
% -define(ITEM_FROM_LOTTERY,			1044).		%% 投壶获得
% -define(ITEM_FROM_VIP,				1045).		%% VIP礼包
% -define(ITEM_FROM_SHOP,				1046).		%% 商店购买
% -define(HU_LAO_GUAN,				1047).      %% 虎牢关掉落
% -define(ITEM_FROM_SWORD,			1048).		%% 神剑活动掉落
% -define(ITEM_FROM_DEFENCE_MON,		1049).		%% 群魔乱舞打怪掉落
% -define(ITEM_FROM_CHANGE,			1050).		%% 积分兑换获得
% -define(ITEM_FROM_SWORD_2,			1051).		%% 神剑活动中勇猛掉落
% -define(ITEM_FROM_MYSTICAL_SHOP,	1052).		%% 神秘商店购买
% -define(ITEM_FROM_BUY_BACK,			1053).		%% 回购获得

% -define(ITEM_ADD_STACK,				2001).		%% 堆叠后增加（包括物品移动，物品生成，宝石拆卸、交易获得等情况）

% -define(ITEM_DEC_STACK,				3001).		%% 堆叠后减少
% -define(ITEM_DEC_USE,				3002).		%% 使用后减少
% -define(ITEM_DEC_FRAGMANT_COMPOS,	3005).		%% 碎片合成后减少
% -define(ITEM_DEC_SPLIT,				3007).		%% 拆分后减少
% -define(ITEM_DEC_STUFF_USE,			3008).		%% 扣除材料


% -define(ITEM_DEL_USE,				4002).		%% 使用删除
% -define(ITEM_DEL_FRAGMANT_COMPOS,	4005).		%% 碎片合成删除
% -define(ITEM_DEL_STACK,				4008).		%% 堆叠删除
% -define(ITEM_DEL_FROM_MOVE,			4012).		%% 物品移动删除
% -define(ITEM_DEL_STUFF_USE,			4013).		%% 使用材料删除
% -define(ITEM_DEL_THROW,				4014).		%% 丢弃删除
% -define(ITEM_DEL_SELL,				4015).		%% 出售删除

% -define(HP_ADD_FROM_GM,				400).		%% GM加血

% %% ========================= 物品世界唯一ID转变原因 ==========================
% -define(ITEM_CHANGE_BY_TRADE,		1).			%% 交易后转变

% %% ========================= 佣兵经验来源 ==========================
% -define(EXP_FROM_TASK,				1001).		%% 任务获得
% -define(EXP_FROM_FRIENDS,			1002).		%% 好友祝福获得
% %% -define(EXP_FROM_TRAIN,				1003).		%% 训练所得
% %% -define(EXP_FROM_SPEED_UP,			1004).		%% 突飞所得
% %% -define(EXP_FROM_CARD,				1005).		%% 经验卡
% -define(EXP_FROM_BATTLE,			1006).		%% 战斗所得
% -define(EXP_FROM_USE_ITEM,			1007).		%% 使用物品所得
% -define(EXP_FROM_PLANTING,			1008).		%% 种植经验种子所得
% -define(EXP_FROM_SLAVE_WORK,		1009).		%% 奴隶劳作所得
% -define(EXP_FROM_DAZUO,				1010).	%% 打坐所得
% -define(EXP_DROM_GUIDE,				1011).	%% 每日指引获得
% -define(EXP_FROM_MARSTOWER,			1012).	%% 爬塔获得
% -define(EXP_FROM_MAIL,				1013).	%% 邮件获得
% -define(EXP_FROM_JUNGONG_TASK,		1013).	%% 军工任务获得


% %% ========================= 历练改变 ==========================
% -define(PRACTICE_FROM_GM,				1999).	%gm获取
% -define(PRACTICE_FROM_USE_ITEM, 		1019).		%%使用物品获得

% -define(PRACTICE_USE_ADD_DRESS_EXP,     2001).	%% 增加外形装扮消耗（包括坐骑、翅膀等）


% -define(POPULARITY_FROM_GM,						1999).	%gm获取
% -define(POPULARITY_FROM_ARENA,					1003).	%% 竞技场获取声望
% -define(POPULARITY_FROM_ARENA_DAILY_AWARD,		1004).	%% 竞技场每日奖励
% -define(POPULARITY_FROM_USE_ITEM,				1011).	%% 使用物品获得
% -define(POPULARITY_FROM_SLAVE_WORK,				1012).	%% 奴隶劳作所得
% -define(POPULARITY_FROM_TASK,					1013).	%% 任务获得
% -define(POPULARITY_FROM_ROB_YUNBIAO,            1014).  %% 劫镖获得
% -define(POPULARITY_FROM_ACHIEVE,				1015).	%% 成就获得
% -define(POPULARITY_FROM_YUNBIAO,  				1016).  %% 运镖获得				
% -define(POPULARITY_FROM_MAIL,					1017).  %% 邮件获得
% -define(POPULARITY_FROM_BOSS_BATTLE,        	1018).	%% boss战斗获得
% -define(POPULARITY_FROM_TARGET,					1019).	%% 目标系统获得
% -define(POPULARITY_FROM_JUNGONGTASK,			1020).	%% 军功任务中获得
% -define(POPULARITY_FROM_CONSUME,				1021).	%% 回馈活动中获得
% -define(POPULARITY_FROM_GUIDE,					1022).	%% 每日指引获得
% -define(POPULARITY_FROM_ARENA_CHALLENGE,		1023).	%% 竞技场挑战获得

% -define(POPULARITY_UP_PINJIE,					2001).	%% 用于提升器魂品阶
% -define(POPULARITY_UPGRATE_SKILL,				2002).	%% 用于升级技能

% %% ======================== 灵力改变的操作类型 ==========================
% -define(WAKAN_FORM_USE_ITEM,1001).		%% 使用物品获得

% %% ======================== 荣誉积分改变的操作类型 ==========================
% -define(HONOUR_SCORE_FROM_GM,			1999).		%% GM获得
% -define(HONOUR_SCORE_FROM_COMP,			1001).		%% 比武获得
% -define(HONOUR_SCORE_USE_EXCHANGE_ITEM,	2001).		%% 积分兑换物品

%% 道具变更类型
-define(LOG_ITEM_CREATE, 1).				%% 新增道具
-define(LOG_ITEM_DELETE, 2).				%% 删除道具
-define(LOG_ITEM_UPDATE, 3).				%% 道具变更


%% 雇佣动作
-define(LOG_EMPLOY_ACTION_TYPE_FIRE,	0).		%% 解雇
-define(LOG_EMPLOY_ACTION_TYPE_EMPLOY,	1).		%% 雇佣

%% 角色是否第一次雇佣
-define(lOG_ROLE_NOT_FIRST_EMPLOY,	0).			%% 角色不是第一次雇佣
-define(LOG_ROLE_FIRST_EMPLOY,		1).			%% 角色是第一次雇佣

%% 任务完成状态
-define(TASK_STATE_ACCEPT,			1).			%% 接受任务
-define(TASK_STATE_COMPLETE,		2).			%% 完成任务
-define(TASK_STATE_SUBMIT,			3).			%% 提交任务
-define(TASK_STATE_GIVE_UP,			4).			%% 放弃任务


%% 通用添加类型(不区分添加物品/经济类型，只区分来源)
-define(GENERAL_ADD_CARD_CHANGE,		10000).		%% 新手卡兑换
-define(GENERAL_ADD_BOSS_BATTLE,		10001).		%% 世界boss奖励
-define(GENERAL_ADD_YUNBIAO,			10002).		%% 运镖奖励
-define(GENERAL_ADD_ROB_YUNBIAO,		10003).		%% 劫镖奖励
-define(GENERAL_ADD_BIND_PHONE,		    10004).		%% 绑定手机奖励

%% 装备日志类型
-define(EQUIP_LOG_TYPE_ADD,				1).			%% 装备生成
-define(EQUIP_LOG_TYPE_INTEN,			2).			%% 强化
-define(EQUIP_LOG_TYPE_WASH,			3).			%% 洗练
-define(EQUIP_LOG_TYPE_QILING,			4).			%% 启灵
-define(EQUIP_LOG_TYPE_QUALITY,			5).			%% 提升品质
-define(EQUIP_LOG_TYPE_UPGRADE,			6).			%% 升级（变成另一个装备）
-define(EQUIP_LOG_TYPE_INLAY,			7).			%% 镶嵌宝石
-define(EQUIP_LOG_TYPE_BACKOUT,			8).			%% 拆卸宝石
-define(EQUIP_LOG_TYPE_DEL,				9).			%% 装备消失
-define(EQUIP_LOG_TYPE_FASHION,			10).		%% 更换时装（变成另一个装备）
-define(EQUIP_LOG_TYPE_BEINHERIT,		11).		%% 被传承（传承中的低级装备）
-define(EQUIP_LOG_TYPE_INHERIT,			12).		%% 传承（传承中的高级装备）

%%=================================来源=========================================
-define(FROM_MAIL,	0).

-define(FROM_GM,				19999).		%% Gm获得
%% 系统
-define(FROM_BATTLE,			10001).		%% 战斗中获得
-define(FROM_TASK,				10002).		%% 任务获得
-define(FROM_DEFENCE,			10003).		%% 守卫国王获得
-define(FROM_ACHIEVE,			10004).		%% 成就系统中获得
-define(FROM_TARGET,			10005).		%% 目标系统中获得
-define(FROM_CONSUME,			10006).		%% 回馈系统中获得
-define(FROM_GUIDE,				10007).		%% 每日指引获得
-define(FROM_VIP,				10008).		%% VIP系统中获得
-define(FROM_WEALTH,			10009).		%% 招财进宝中获得
-define(FROM_GIFT,				10010).		%% 礼包系统中获得
-define(FROM_XUNXIAN,			10011).		%% 寻仙中获得
-define(FROM_GUILD_COMP,		10012).		%% 公会战获得
-define(FROM_YUNBIAO,			10013).		%% 运镖获得
-define(FROM_ROB_YUNBIAO,		10014).		%% 劫镖获得
-define(FROM_ARENA_DAILY,		10015).		%% 竞技场每日奖励
-define(FROM_ARENA_CHALLENGE,	10016).		%% 竞技场挑战获得
-define(FROM_BOSS,				10017).		%% 世界boss
-define(FROM_JUNGONGTASK,		10018).		%% 军工任务获得
-define(FROM_COMP,				10019).		%% 比武获得
-define(FROM_MARSTOWER,			10020).		%% 爬塔获得
-define(FROM_SWORD,				10021).		%% 天灵神剑获得
-define(FROM_SWORD_RAN,			10022).		%% 天灵神剑活动概率获得
-define(FROM_JUNWEI_ROLE,		10023).		%% 军威将领获得
-define(FROM_DUNGEON,			10024).		%% 副本获得
-define(FROM_LOTTERY,			10025).		%% 投壶获得
-define(FROM_DAZUO,				10026).		%% 打坐获得
-define(FROM_CAHLLENGE_KING,	10027).		%% 挑战国王获得
-define(FROM_CHIEF,				10028).		%% 首席弟子获得
-define(FROM_FENGDI,			10029).		%% 封地系统获得
-define(FROM_SLAVE,				10030).		%% 奴隶系统获得
-define(FROM_MARKET,			10031).		%% 商城获得
-define(FROM_MARKET_2,			10032).		%% 商城抢购获得
-define(FROM_TRACETASK,			10033).		%% 悬赏任务获得
-define(FROM_ONLINE_AWARD,		10034).		%% 在线奖励获得
-define(FROM_ACTIVITY,			10035).		%% 系统活动获得
-define(FROM_GUILD_HUNTING,     10036).		%% 帮派活动获得
-define(FROM_STAGE,				10037).		%% 在挑战魂将中获得
-define(FROM_RUSH_RANK,         10038).     %% 在冲榜活动获得
-define(FROM_ANSWER,			10039).		%% 答题活动获得
-define(FROM_EXP_RETRIEVE,      10040).		%% 经验找回
-define(FROM_CAVE,				10041).		%% 藏宝洞获得
-define(FROM_CROSS,             10042).     %% 跨服
-define(FROM_GUILD_HUNTING_TARGET, 10043).  %% 公会打猎目标获得 
-define(FROM_RESOLVE_PENDANT,	10044).		%% 分解坐骑挂饰
-define(FROM_LUCKY,				10045).		%% 抽奖获得
-define(FROM_FEST,              10046).     %% 节日活动获得
-define(FROM_FEST_EXCHANGE,     10047).     %% 节日活动兑换
-define(FROM_TRADE,				10048).		%% 交易行获得
-define(FROM_DUNGEON_CHANGE,	10049).		%% 副本兑换获得
-define(FROM_FEST_BUY,			10050).		%% 活动购买获得
-define(FROM_TRADE_2,			20010).		%% 交易行取回获得

%% 常用操作
-define(FROM_CHARGE,			11001).		%% 充值获得
-define(FROM_USE_ITEM,			11002).		%% 使用物品获得
-define(FROM_USE_WISD,			11003).		%% 使用藏宝图获得
-define(FROM_SHOP,				11004).		%% 商店获得
-define(FROM_MYS_SHOP,			11005).		%% 神秘商店获得
-define(FROM_CHANGE,			11006).		%% 兑换获得
-define(FROM_PRAY,				11007).		%% 好友祝福获得
-define(FROM_SELL,				11008).		%% 出售物品获得
-define(FROM_PLANTING,			11009).		%% 封地种植获得
-define(FROM_FENGLU,			11010).		%% 领取俸禄获得
-define(FROM_SLAVE_WORK,		11011).		%% 奴隶劳作获得
-define(FROM_SLAVE_TAX,			11012).		%% 奴隶税收获得
-define(FROM_WATERING,			11013).		%% 帮好友浇水获得
-define(FROM_GUILD_SKILL,		11014).		%% 公会技能退费获得
-define(FROM_DONATE,			11015).		%% 捐献获得
-define(FROM_BAG_GIFT,			11016).		%% 礼包中获得
-define(FROM_EMPLOY,			11017).		%% 招募获得
-define(FROM_BUY_BACK,			11018).		%% 回购获得
-define(FROM_ZHUAN_YUN,			11019).		%% 转运获得
-define(FROM_FLOWER,			11020).		%% 好友送花获得
-define(FROM_CHANGE_SCHOOL,		11021).		%% 师门积分兑换获得
-define(FROM_CHANGE_HONOUR,		11022).		%% 比武积分兑换获得
-define(FROM_CHANGE_KING_POINT,	11023).		%% 群魔乱舞积分兑换获得
-define(FROM_CHANGE_SWORD,		11024).		%% 神剑积分兑换获得
-define(FROM_CHANGE_MANOR,		11025).		%% 领地战积分兑换获得
-define(FROM_CHANGE_HUNT,		11027).		%% 狩猎积分兑换获得
-define(FROM_CHANGE_CROSS,		11028).		%% 天梯跨服积分兑换获得
-define(FROM_CHANGE_CAVE,		11029).		%% 藏宝洞积分兑换获得
-define(FROM_WEAPON,            11030).     %% 装备卷轴兑换获得
-define(FROM_CHANGE_TOWER,		11031).		%% 爬塔积分兑换获得
-define(FROM_CHANGE_ARENA,		11032).		%% 竞技场积分兑换获得
-define(FROM_PRAY_RAIN,			11033).		%% 祈雨活动奖励礼包 
-define(FROM_QINGMING_SEED,		11034).		%% 清明种子兑换
-define(FROM_LEVY_JUNLIANG,		11035).		%% 征收军粮
-define(FROM_JUNLIANG,			11036).		%% 军粮升级中获得
-define(FROM_MAY,				11037).		%% 五一活动获得



%%物品类操作
-define(FROM_STONE_BACKOUT,		12001).		%% 宝石摘除获得
-define(FROM_FRAGMANT_COMPOS,	12002).		%% 碎片合成获得
-define(FROM_SPLIT,				12003).		%% 拆分获得
-define(FROM_QILING,			12004).		%% 启灵获得
-define(FROM_XILIAN,			12005).		%% 洗练获得
-define(FROM_UPQUALITY,			12006).		%% 提升品质获得
-define(FROM_UPGRATE,			12007).		%% 装备升级获得
-define(FROM_INLAY,				12008).		%% 镶嵌宝石获得
-define(FROM_COMPOSE,			12009).		%% 宝石合成获得
-define(FROM_CONVERT,			12010).		%% 宝石转化获得
-define(FROM_CARVE,				12011).		%% 宝石雕刻获得
-define(FROM_CHANGE_WEAPON,		12012).		%% 武器转化获得
-define(FROM_INHERIT,			12013).		%% 装备传承获得
-define(FROM_DECOMPOSE,			12014).		%% 装备分解获得
-define(FROM_KAIFU,				12015).		%% 元宝计划获得

-define(ITEM_ADD_STACK,			13012).		%% 堆叠获得

%% 邮件类
-define(FROM_FRIEND,			14001).		%% 好友获得
-define(FROM_GUILD,				14002).		%% 公会获得
-define(FROM_SYSTEM,			14003).		%% 系统获得
-define(FROM_TERRITORY_WAR,		14004).		%% 领地战获得
-define(FROM_WORLD_BOSS,		14005).		%% 世界boss获得

%%=================================去向=========================================
%% 系统
-define(USE_XUNXIAN,			20001).		%% 用于寻仙
-define(USE_WEALTH,				20002).		%% 用于招财
-define(USE_LOTTERY,			20003).		%% 用于投壶
-define(USE_VIP,				20004).		%% 用于VIP
-define(USE_CAVE,				20005).		%% 用于藏宝洞
-define(USE_JIANGHUN,			20006).		%% 用于将魂
-define(USE_TRADE,				20007).		%% 用于交易行
-define(USE_UP_LEVEL,			20008).		%% 用于升级佣兵
-define(USE_BUY_FEST_ITEM,		20009).		%% 用于购买活动物品


%% 日常
-define(USE_FINISH_TASK,		21001).		%% 用于自动完成任务
-define(USE_GUILD_CREATE,		21002).		%% 用于创建公会
-define(USE_SHOP_BUY,			21003).		%% 用于商店购买
-define(USE_MYS_SHOP_BUY,		21004).		%% 用于神秘商店购买
-define(USE_EXTEND_BAG,			21005).		%% 用于开启背包格子
-define(USE_EXTEND_BANK,		21006).		%% 用于开启仓库
-define(USE_REFRESH_SKILL,		21007).		%% 用于刷新技能
-define(USE_FEED_HORSE,			21008).		%% 用于喂马
-define(USE_BUY_HORSE_EQUIP,	21009).		%% 用于购买时装
-define(USE_QIHUN_CD,			21010).		%% 用于器魂的清cd
-define(USE_UP_PINJIE,			21011).		%% 用于提升器魂品阶
-define(USE_REFRESH_BIAOCHE,	21012).		%% 用于刷新镖车
-define(USE_ZHUANYUN,			21013).		%% 用于押镖转运
-define(USE_RESET_TOWER,		21014).		%% 用于重置爬塔
-define(USE_INSPIRE,			21015).		%% 用于boss鼓舞
-define(USE_BOSS_CD,			21016).		%% 用于清除bossCD
-define(USE_FRESH_MYS_SHOP,		21017).		%% 用于刷新神秘商店
-define(USE_REF_JUNGONGTASK,	21018).		%% 用于刷新军工任务
-define(USE_JUNGONGTASK,		21019).		%% 用于自动完成军工任务
-define(USE_COMP_CD,			21020).		%% 用于清除比武cd
-define(USE_COMP_REVIE,			21021).		%% 用于使用比武复活丹
-define(USE_TOWER_AUTO_FINISH,	21022).		%% 用于爬塔挂机的加速
-define(USE_EVOKE_DRESS,		21023).		%% 用于幻化坐骑
-define(USE_ADD_DRESS_EXP,		21024).		%% 用于坐骑升级
-define(USE_UP_VIP,				21025).		%% 用于升级VIP
-define(USE_CHUANGONG,			21026).		%% 用于传功
-define(USE_SPEED_DUNGEON,		21027).		%% 用于加速副本挂机
-define(USE_UP_TALENT,			21028).		%% 用于提升天赋
-define(USE_FOSTER,				21029).		%% 用于培养
-define(USE_UPGRATE_SKILL,		21030).		%% 用于升级技能
-define(USE_GUILD_SKILL,		21031).		%% 用于升级公会技能
-define(USE_DUNGEON_GUAJI,		21032).		%% 用于副本挂机
-define(USE_ONEKEY_QIHUN,		21033).		%% 一键完成器魂修炼
-define(USE_QIHUN,				21034).		%% 用于修炼器魂
-define(USE_CHANGE,				21035).		%% 用于兑换
-define(USE_JUNLING,			21036).		%% 用于购买军令
-define(USE_EMPLOY,				21037).		%% 用于招募
-define(USE_MARKET,				21038).		%% 用于商城购买
-define(USE_TRACETASK,			21039).		%% 用于悬赏任务
-define(USE_RESET_STAGE,		21040).		%% 用于重置挑战魂将
-define(USE_GUILD_RECRUIT,      21041).     %% 用于帮派招募
-define(USE_SOUL_BALL,          21042).     %% 用于猎魂
-define(USE_GUILD_DONATE,       21043).     %% 用于工会捐款
-define(USE_GUILD_HUNTING,      21044).     %% 用于帮派活动（买箭、用箭）
-define(USE_BUY_FLOWER,         21045).     %% 用于购买鲜花
-define(USE_RETRIEVE_EXP,       21046).     %% 用于找回经验
-define(USE_ROLE_FOSTER,        21047).     %% 用于培养属性
-define(USE_ROLE_TALENT,        21048).     %% 用于培养天赋
-define(USE_ZHENFA,             21049).     %% 用于研习阵法
-define(USE_DUNGEON_TIMES,		21050).		%% 用于购买副本次数
-define(USE_OPEN_LAND,			21051).		%% 用于开垦土地
-define(USE_REFRESH_SEED,		21052).		%% 用于刷新种子
-define(USE_PLANTING_CD,		21053).		%% 用于清除种植cd
-define(USE_WORK_CD,			21054).		%% 用于清除劳作cd
-define(USE_BUY_GUAJI,			21055).		%% 用于挂机
-define(USE_ADD_DRESS_ADVANCE_EXP,21056).	%% 用于坐骑升阶
-define(USE_IDENTIFY_PEADANT,	21057).		%% 用于鉴定坐骑挂饰
-define(USE_JUNLIANG,			21058).		%% 用于征收军粮
-define(USE_ADVANCE_FASHION,	21059).		%% 用于时装进阶
%% -define(CLEAN_ARENA_BATTLE_CD,	21059).		%% 用于清除竞技场CD（原31006）
%% -define(BUY_ARENA_CHALLENGE_TIMES, 21060).	%% 用于购买竞技场次数(原31007)

%% 装备
-define(USE_INTEN,				22001).		%% 用于装备强化
-define(USE_INTEN_RATE,			22002).		%% 用于提升强化成功率
-define(USE_QILING,				22003).		%% 用于启灵装备
-define(USE_XILIAN,				22004).		%% 用于洗练装备
-define(USE_XILIAN_STAR,		22005).		%% 用于提升洗练星星
-define(USE_UPGRATE,			22006).		%% 用于升级
-define(USE_UPQUALITY,			22007).		%% 用于提升品质
-define(USE_BACKOUT,			22008).		%% 用于拆除宝石
-define(USE_INLAY,				22009).		%% 用于镶嵌宝石
-define(USE_COMPOSE,			22010).		%% 用于宝石合成
-define(USE_CONVERT,			22011).		%% 用于宝石转化
-define(USE_CARVE,				22012).		%% 用于宝石雕刻
-define(USE_CHANGE_WEAPON,		22013).		%% 用于武器转换
-define(USE_INHERIT,			22014).		%% 用于装备传承
-define(USE_DECOMPOSE,			22015).		%% 用于装备分解


-define(ITEM_DEC_STACK,				23001).		%% 堆叠后减少
-define(ITEM_DEC_USE,				23002).		%% 使用后减少
-define(ITEM_DEC_FRAGMANT_COMPOS,	23005).		%% 碎片合成后减少
-define(ITEM_DEC_SPLIT,				23007).		%% 拆分后减少
-define(ITEM_DEC_STUFF_USE,			23008).		%% 扣除材料

-define(ITEM_DEL_TIME_OUT,			24001).		%% 限时物品到时间删除
-define(ITEM_DEL_USE,				24002).		%% 使用删除
-define(ITEM_DEL_FRAGMANT_COMPOS,	24005).		%% 碎片合成删除
-define(ITEM_DEL_STACK,				24008).		%% 堆叠删除
-define(ITEM_DEL_FROM_MOVE,			24012).		%% 物品移动删除
-define(ITEM_DEL_STUFF_USE,			24013).		%% 使用材料删除
-define(ITEM_DEL_THROW,				24014).		%% 丢弃删除
-define(ITEM_DEL_SELL,				24015).		%% 出售删除
-define(ITEM_DEL_U_EQUIP,			24016).		%% 礼包装备合并删除
