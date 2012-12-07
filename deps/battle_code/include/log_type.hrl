%% 这个头文件用于定义用户日志数据的宏定义，如金币或银币消耗的类别


%% ======================== 金币改变的操作类型 ==========================
-define(GOLD_FROM_GM,1999).					%%GM获得
-define(GOLD_CHARGE_MONEY, 1001). 			%% 1;		// 充钱
-define(GOLD_CHARGE_CARD, 1002). 			%% 2;		// 充卡
-define(GOLD_MAIL, 1003). 					%% 3;		// 邮件
-define(GOLD_SYSTEM, 1004). 				%% 4;		// 后台系统
-define(GOLD_GIFT, 1005). 					%% 5;		// 礼包
-define(GOLD_PRAY, 1006). 					%% 6;		// 好友祝福
-define(GOLD_TAX, 1007). 					%% 7;		// 征收
-define(GOLD_ACHIEVE, 1008). 				%% 8;		// 成就
-define(GOLD_TARGET_CARD, 1009). 			%% 9;		// 目标达成翻牌
-define(GOLD_ARENA_CARD, 1010). 			%% 10;		// 竞技场翻牌
-define(GOLD_GAIN_FROM_TRADE, 1011).		%% 11;		// 交易所得
-define(GOLD_GUILD_SALARY, 1012).			%% 12;		// 公会福利
-define(GOLD_FROM_TASK, 1013).				%% 13;		// 任务所得
-define(GOLD_FROM_BARRIER_AWARD, 1014).		%% 14;		// 通关奖励所得
-define(GOLD_FROM_ONLINE_AWARD, 1015).		%% 15;      // 在线奖励所得
-define(GOLD_FROM_USE_BILL, 1016).			%% 16;      // 使用金票获得
-define(GOLD_FROM_GARDEN_HARVEST, 1017).	%% 17;      // 家园收获所得
-define(GOLD_FROM_TRADE_CANCEL, 1018).		%% 18;		// 取消交易获得
-define(GOLD_FROM_CONSUME_BONUS, 1019).		%% 19;		// 消耗活动奖励
-define(GOLD_FROM_GUILD_COMP, 1020).		%% 20;		// 公会竞赛奖励
-define(GOLD_TARGET, 1021).					%% 21;		// 目标系统获得
-define(GOLD_FROM_CONSUME,1022).			%% 22;		// 回馈系统获得
-define(GOLD_FROM_GUIDE,1023).				%% 23;		//	每日指引获得
-define(GOLD_FROM_VIP,1024).				%% 24;		// VIP充值回馈获得
-define(GOLD_WEALTH_GAMBLE,1025).				%%招财进宝


-define(GOLD_FORCE_TAX, 2001). 				%% = 21;	// 强征花销
-define(GOLD_TRAIN_TIME, 2002). 				%% ;		// 训练时间
-define(GOLD_TRAIN_MODE, 2003). 				%% ;		// 训练类型
-define(GOLD_TRAIN_QUEUE, 2004). 				%% ;		// 训练队列
-define(GOLD_HOLY_QUEUE, 2005). 				%% ;		// 圣痕队列
-define(GOLD_HOLY_CD, 2006). 					%% ;		// 圣痕CD
-define(GOLD_INTENSIFY_RATE, 2007). 			%% ;		// 装备强化概率
-define(GOLD_INTENSIFY_CD, 2008). 			%% ;		// 装备强化CD
-define(GOLD_REFRESH_DAILY_TASK, 2009). 		%% ;		// 刷新日常任务
-define(GOLD_COMPLETE_DAILY_TASK, 2010).	 	%% ;		// 直接完成日常任务
-define(GOLD_SHOPPING, 2011). 				%% ;		// 商城购买
-define(GOLD_COMMISSION, 2012). 				%% ;		// 委派CD
-define(GOLD_FORMATION_UP_CD, 2013). 			%% ;		// 阵型升级CD
-define(GOLD_BUG_DUNGEON_TICKET, 2014). 		%% ;		// 购买进入副本次数
-define(GOLD_BUG_ENERGY, 2015). 				%% ;		// 购买精力值
-define(GOLD_FOSTER_TYPE, 2016). 				%% ;		// 培养类型
-define(GOLD_CONTRIBUTE_CD, 2017). 			%% ;		// 地区捐献CD
-define(GOLD_GUILD_UP, 2018). 				%% ;		// 公会升级
-define(GOLD_TRAIN_SPEED_UP, 2019). 			%% ;		// 金币突飞
-define(GOLD_GUILD_CREATE, 2020). 			%% ;		// 公会创建 wyx 20111128add
-define(GOLD_GUILD_DONATE, 2021). 			%% ;		// 公会捐献 wyx 20111128add
-define(GOLD_BOXING, 2022). 					%% ;		// 挑战擂台赛
-define(GOLD_GEM_REFRESH, 2023).
-define(GOLD_BUG_HORN, 2024).					%% ;		// 用于购买小喇叭
-define(GOLD_LEVEL_UP_PET, 2025).				%% ;		// 用于升级宠物
-define(GOLD_UP_TALENT_PROTECT, 2026).		%% ;		// 用于佣兵提升天赋的保护消耗
-define(GOLD_CHANGE_PET_NAME, 2027).			%% ;		// 用于宠物更名
-define(GOLD_BUY_ITEM_COST, 2028).			%% ;		// 用于商城购买物品
-define(GOLD_INTENSIFY_EQUIP_COST, 2029).		%% ;		// 用于装备强化保护
-define(GOLD_EXTEND_BAG_COST, 2030).			%% ;		// 用于扩充背包格子
-define(GOLD_EXTEND_BANK_COST, 2031).			%% ;		// 用于扩充仓库格子
-define(GOLD_EXTEND_CD_COST, 2032).			%% ;		// 用于扩充圣痕CD队列
-define(GOLD_CLEAR_CD_COST, 2033).			%% ;		// 用于清除CD状态
-define(GOLD_XUNXIAN_COST, 2034).				%% ;		// 用于炼金
-define(GOLD_REVIVE_COST, 2035).				%% ;		// 战斗失败后复活消耗
-define(GOLD_TRADE_COST, 2036).				%% ;		// 用于交易
-define(GOLD_TOWER_COST, 2037).				%% ;		// 下水道金币必过一层
-define(GOLD_REFRESH_SEED_COST, 2038).		%% ;		// 家园刷新种子消耗
-define(GOLD_ARENA_ENEGRY, 2039).				%% ;		// 购买竞技场次数消耗
-define(GOLD_CYCLIC_TASK_COST, 2040).			%% ;		// 购买竞技场次数消耗
-define(GOLD_RUNNING_COST, 2041).				%% ;		// 用于跑步
-define(GOLD_BUY_VIP_CARD, 2042).				%% ;		// 用于购买VIP卡
-define(GOLD_COMPLETE_FAST_BATTLE, 2043).		%% ;		// 用于立刻完成快速挑战
-define(GOLD_DRAGON_HUNT_COST, 2044).			%% ;		// 抽奖系统消耗
-define(GOLD_CLEAR_BATTLE_CD_COST, 2045).		%% ;		// 用于清除普通战斗CD
-define(GOLD_CLEAR_INTEN_CD_COST, 2046).		%% ;		// 用于清除强化CD
-define(GOLD_CLEAR_TRAIN_CD_COST, 2047).  	%% ;		// 用于清除培养CD
-define(GOLD_CLEAR_HOLY_CD_COST, 2048).		%% ;		// 用于清除圣痕CD
-define(GOLD_CLEAR_TAX_CD_COST, 2049).		%% ;		// 用于清除征收CD
-define(GOLD_CLEAR_ARENA_CD_COST, 2050).		%% ;		// 用于清除角斗场
-define(GOLD_GET_HELPER_MER, 2051).			%% ;		// 摘星星时召唤帮助佣兵
-define(GOLD_BUY_FLOWER, 2052).				%% ;		// 用于送花 
-define(GOLD_TATTOO_GAMBLE, 2053).			%% ;		// 刻纹猜拳
-define(GOLD_IDENTIFY_COST, 2054).			%% ;		// 用于鉴定武器
-define(GOLD_LOCK_TAVERN_CAREER_COST, 2055).	%% ;	// 用于锁定酒馆佣兵职业 
-define(GOLD_FROM_MAIL, 2056).				%% ;		// 邮件寄送
-define(GOLD_FROM_GIFT_BAG, 2057).			%% 16;      // 使用礼包获得
-define(GOLD_RESET_TOWER_COST, 2058).			%%       	// 用于下水道重置
-define(GOLD_ARENA_GOLD_CHALLANGE_COST, 2059).%%	// 用于竞技场的金币越级挑战
-define(GOLD_FOSTER_CARD_COST, 			2060).%%	// 用于购买培养卡
-define(GOLD_FROM_USE_ITEM, 2061).			%% 16;      // 使用物品获得
-define(GOLD_FIX_SKILL_COST, 2062).			%%      	// 刷新技能时用于固定技能
-define(GOLD_BUY_ZHAOSHU, 2063).				%%      	// 用于购买诏书 
-define(GOLD_FEED_HORSE, 2064).        		%% 			// 喂养坐骑
-define(GOLD_TALENT_CARD_COST, 2065).     	%% 			// 用于购买天赋卡
-define(GOLD_HORSE_EQUIP_COST, 2066).     	%% 			// 用于购买坐骑时装
-define(GOLD_FINISH_QIHUN_LEVELING, 2067).	%% 			// 用于立刻完成器魂修炼
-define(GOLD_FINISH_QIHUN_CD, 		2068).	%% 			// 用于清除器魂修炼的cd
-define(GOLD_QILING_COST, 2070).				%%			// 用于启灵装备
-define(GOLD_XILIAN_ITEM_COST, 2071).			%%			// 用于洗练装备
-define(GOLD_UP_PINJIE, 	2072).			%%			// 用于提升器魂品阶
-define(GOLD_BUG_DUNGEON_TIMES, 	2073).	%%			// 用于购买进入副本的次数
-define(GOLD_UPGRATE_ITEM_COST,	2074).		%%			// 用于升级装备
-define(GOLD_OPEN_LAND, 	2075).			%%			// 用于开垦土地
-define(GOLD_REFRESH_SEED, 	2076).			%%			// 用于刷新种子的品质
-define(GOLD_CLEAN_PLANTING_CD, 	2077).	%%			// 用于清除种植等待cd
-define(GOLD_FROM_WLSD,             2078).	%%         // 藏宝图获得
-define(GOLD_CLEAN_WORK_CD,         2079).	%%         // 用于清除奴隶劳作cd
-define(GOlD_BUY_GUAJI_COST,		2080).	%%			// 用于购买挂机
-define(GOlD_REFRESH_BIAOCHE_COST,2081).		%%		// 用于刷新镖车
-define(GOlD_ZHUANYUN_COST,		2082).		%%		// 用于转运
-define(GOLD_RESET_TOWER,		2083).		%%	// 用于重置英雄塔
-define(GOLD_USE_TO_INSPIRE,  2084).			%%      用于鼓舞
-define(GOLD_USE_TO_CLEAN_CD,  2085).		%%用于清除boss战斗cd
-define(GOLD_BUY_HORN,			2086).		%%	// 用于买小喇叭
-define(GOLD_CLEAR_ENTER_GUILD, 2087).		%% 退出后加入公会要24小时的cd，清除cd要扣费
-define(GOLD_BUY_HUNSHI, 2088).				%% 买魂石
-define(GOLD_FRESH_MYSTICAL_SHOP,2089).		%% 用于刷新神秘商店物品
-define(GOLD_BUY_MYSTICAL_SHOP_ITEM, 2090).	%% 用于购买神秘商店物品
-define(GOlD_REFRESH_JUNGONGTASK, 2091).			%% 用于刷新军工任务
-define(GOlD_AUTO_COMPLETE_JUNGONGTASK, 2092).	%% 用于自动完成军工任务
-define(GOLD_CLEAR_COMP_CD_COST,2093).			%% 清除比武场CD
-define(GOLD_USE_COMP_REVIE_DRUG,2094).			%% 比武场使用复活丹
-define(GOLD_TOWER_FINISH_CHALLENGE,2095). 	%% 马上完成爬塔自动挑战
-define(GOLD_COST_JUNLING,2096).					%% 购买军令
-define(GOLD_LOTTERY,				2097).	%% 投壶花费
-define(GOLD_ONE_KEY_COMPLETE_QIHUN,2098). %% 一键完成器魂升级
-define(GOLD_USE_EVOKE_DRESS,		2099).	%% 幻化外形装扮消耗（包括坐骑、翅膀等）
-define(GOLD_USE_ADD_DRESS_EXP,		2100).	%% 增加外形装扮消耗（包括坐骑、翅膀等）
-define(GOLD_UP_VIP,				2101).	%% 用于升级VIP
-define(GOLD_CHUANGONG,				2102).	%% 用于传功


%% ======================== 银币改变的操作类型 ==========================
-define(SILVER_FROM_GM,	1999).	%%GM获得
-define(SILVER_TAX, 1001). 					%% 1    征收所得
-define(SILVER_FORCE_TAX, 1002).				%% 2    强征所得
-define(SILVER_TASK, 1003). 					%% 3    任务所得
-define(SILVER_FROM_GOLD, 1004).				%% 4    金币兑换
-define(SILVER_FROM_MONSTER, 1005).			%% 5    怪物掉落
-define(SILVER_SYSTEM, 1006). 					%% 6    系统原有
-define(SILVER_GIFT_BAG, 1007).				%% 7    礼包奖励
-define(SILVER_GIFT_LOTTERY, 1008). 			%% 8    礼品券
-define(SILVER_FROM_ACHIEVE, 1009). 			%% 9    成就所得
-define(SILVER_GUILD_SALARY, 1010). 			%% 10    公会福利
%% -define(SILVER_FROM_TASK, 11). 				%% 11    任务所得
-define(SILVER_GAIN_FROM_TRADE, 1012).		%% 12  交易所得
-define(SILVER_ARENA_CARD, 1013). 			%% 13  竞技场翻牌
-define(SILVER_FROM_RUN_BUSINESS, 1014). 		%% 14 跑商获得
-define(SILVER_FROM_PRAY, 1015). 				%% 15 祝福获得
-define(SILVER_FROM_ONLINE_ARENA, 1016). 		%% 16 在线竞技获得
-define(SILVER_FROM_BARRIER_AWARD, 1017).		%% 17 通关奖励所得
-define(SILVER_FROM_ONLINE_AWARD,	1018).	%% 18 在线奖励所得
-define(SILVER_FROM_ARENA_AWARD,	1019).	%% 19 离线竞技场排名奖励
-define(SILVER_FROM_USE_BILL, 1020).			%% 20 使用银票获得
-define(SILVER_FROM_ITEM_SELL, 1021).			%% 21 出售物品获得
-define(SILVER_FROM_EQUIP_DEMOTE, 1022).		%% 22 武器降级获得
-define(SILVER_FROM_XUNXIAN_SELL_ITEM, 1023).	%% 23 寻仙物品出售获得
-define(SILVER_FROM_GARDEN_HARVEST, 1024).	%% 24 家园收获所得
-define(SILVER_FROM_DRAGON_HUNT, 1025).		%% 25 抽奖所得
-define(SILVER_FROM_CYCLIC_TASK, 1026).		%% 26 循环任务奖励所得
-define(SILVER_FROM_CONSUME_BONUS, 1027).		%% 27 消耗活动奖励
-define(SILVER_FROM_GUILD_COMP, 1028).		%% 28 公会战奖励
-define(SILVER_FROM_LIVENESS, 1029).			%% 29 活跃度奖励
-define(SILVER_TATTOO_GAMBLE_REWARD, 1030).	%% 30 刻纹猜拳奖励
-define(SILVER_FROM_MAIL, 1031).				%% 31   邮件寄送
-define(SILVER_FROM_GIFT_BAG, 1032).			%% 32   使用礼包获得
-define(SILVER_FROM_MEDITATION, 1033).		%% 33 冥想获得
-define(SILVER_FROM_USE_ITEM, 1034).			%% 34   使用物品获得
-define(SILVER_FROM_PLANTING, 1035).			%% 35   种植银币种子所得
-define(SILVER_FROM_FENGLU,					1036).	%% 领取俸禄获得
-define(SILVER_FROM_SLAVE_WORK,				1037).	%% 奴隶劳作所得
-define(SILVER_FROM_SLAVE_TAX,				1038).	%% 从奴隶那得到的税金
-define(SILVER_FROM_ROB_YUNBIAO,            1039).    %%从劫镖得到
-define(SILVER_FROM_WATERING, 1040).                  %% 帮助好友浇水获得
-define(SILVER_FROM_ARENA_DAILY_AWARD,      1041).    %%竞技场每天奖励
-define(SILVER_FROM_WLSD,					1042). 	%%藏宝图所得
-define(SILVER_FROM_YUNBIAO,				1043).    %%运镖获得
-define(SILVER_FROM_BOSS_BATTLE,			1044).	%%boss战斗获得
-define(SILVER_FROM_GUILD_SKILL,            1045).    %%工会技能升级失败退回钱;
-define(SILVER_FROM_TARGET,					1046).    %%目标系统获得
-define(SILVER_FROM_JUNGONGTASK,			1047).	%%军功任务中获得
-define(SILVER_FROM_CONSUME,				1048).	%%回馈活动获得
-define(SILVER_FROM_GUIDE,					1049).	%%每日指引获得
-define(SILVER_FROM_ARENA_CHALLENGE_AWARD,	1050).	%%竞技场挑战获得
-define(SILVER_FROM_WEALTH,					1051).	%%招财进宝获得
-define(SILVER_FROM_COMP,					1052).	%%比武奖励获得
-define(SILVER_FROM_MARSTOWER,				1053).	%%爬塔自动挑战获得

-define(SILVER_BUY_TIME, 2001). 			%% 1000    买道具消耗
-define(SILVER_INTENSIFY_COST, 2002). 		%% 1001    强化消耗
-define(SILVER_EMPLOY_COST, 2003). 			%% 1002   招募消耗
-define(SILVER_TRAIN_COST, 2004). 			%% 1003   训练消耗
-define(SILVER_GUILD_CREATE,2005).			%% 1004   公会创建消耗		wyx20111128 add
-define(SILVER_GUILD_DONATE,2006).			%% 1005        公会捐献消耗	wyx20111128 add
-define(SILVER_BOXING, 2007). 				%% 1004        挑战擂台赛
-define(SILVER_RUN_BUSINESS, 2008).         %% 1007   跑商银币
-define(SILVER_BUY_ITEM_COST, 2009).        %% 1008   用于商店购买物品
-define(SILVER_COMPOS_STONE_COST, 2010).	%% 1009   用于宝石合成
-define(SILVER_INLAY_STONE_COST, 2011).		%% 1010   用于宝石镶嵌
-define(SILVER_BACKOUT_STONE_COST, 2012).	%% 1011   用于宝石拆除
-define(SILVER_INTENSIFY_EQUIP_COST, 2013).	%% 1012   用于装备强化
-define(SILVER_UPGRADE_HOLY_COST, 2014).	%% 1013   用于圣痕升级
-define(SILVER_XUNXIAN_COST, 2015).			%% 1014   用于寻仙
-define(SILVER_RUNNING_COST, 2016).			%% 1015   用于跑步
-define(SILVER_COMPOS_RING_COST, 2017).		%% 1016        用于戒指合成
-define(SILVER_BOSS, 2018).                 %% 1017   用于世界BOSS
-define(SILVER_EMPLOY_ROLE, 2019).          %% 1018   用于招募佣兵
-define(SILVER_REFRESH_SKILL, 2020).        %% 1019   用于刷新技能
-define(SILVER_FEED_HORSE, 2021).        	%% 1020   喂养坐骑
-define(SILVER_LEVELING_QIHUN, 2022).       %% 1021   用于修炼器魂
-define(SILVER_XILIAN_ITEM_COST,2023).		%% 1022	  用于洗练装备
-define(SILVER_UP_TALENT,2024).				%% 1023	  用于提升天赋
-define(SILVER_FOSTER,2025).				%% 1024	  用于武将培养
-define(SILVER_UPQUALITY_ITEM_COST,2026).	%% 1025   用于提升装备品质
-define(SILVER_UPGRATE_ITEM_COST,2027).		%% 1026	  用于升级装备
-define(SILVER_INLAY_ITEM_COST,2028).		%% 1027	  用于镶嵌宝石
-define(SILVER_BACKOUT_ITEM_COST,2029).		%% 1028   用于拆卸宝石
-define(SILVER_COMPOSE_ITEM_COST,2030).		%% 1029   用于合成宝石
-define(SILVER_CONVERT_ITEM_COST,2031).		%% 1030   用于转化宝石
-define(SILVER_CARVE_ITEM_COST,2032).		%% 1031   用于雕刻宝石
-define(SILVER_USE_TO_INSPIRE,2033).		%% 27000     用于鼓舞
-define(SILVER_UPGRATE_SKILL,2034).			%% 1041   用于升级技能
-define(SILVER_LEARN_GUILD_SKILL, 2035).     %%        用于升级工会技能
-define(SILVER_BUY_MYSTICAL_SHOP_ITEM, 2036).		%% 用于购买神秘商店物品
-define(SILVER_MARSTOWER_AUTO_CHALLENGE,2037). %% 用于爬塔自动挑战
-define(SILVER_ONE_KEY_COMPLETE_QIHUN,2038). %% 一键完成器魂升级
-define(SILVER_USE_ADD_DRESS_EXP,     2039).	%% 增加外形装扮消耗（包括坐骑、翅膀等）
-define(SILVER_CHUANGONG,			  2040).	%% 用于传功


%%有一些操作，同时使用多种东西的
-define(ONE_KEY_COMPLETE_QIHUN,?SILVER_ONE_KEY_COMPLETE_QIHUN).

%% ========================= 物品来源去向 ==========================
-define(ITEM_FROM_GM,				1999).		%% GM获得
-define(ITEM_FROM_BAG_GIFT,			1001).		%% 使用礼包获得
-define(ITEM_FROM_STONE_COMPOS,		1002).		%% 宝石合成获得
-define(ITEM_FROM_STONE_BACKOUT,	1003).		%% 宝石拆卸获得
-define(ITEM_FROM_FRAGMANT_COMPOS,	1004).		%% 碎片合成获得
-define(ITEM_FROM_CREATE_ROLE,		1005).		%% 创建角色获得
-define(ITEM_FROM_EMPLOY_MER,		1006).		%% 招募佣兵获得
-define(ITEM_FROM_XUNXIAN,			1007).		%% 炼金获得
-define(ITEM_FROM_ONLINE_ARENA,		1008).		%% 在线竞技获得
-define(ITEM_FROM_ONLINE_AWARD,		1009).		%% 在线奖励（登录奖励）获得
-define(ITEM_FROM_TRADE,			1010).		%% 交易获得
-define(ITEM_FROM_ACHIEVE,			1012).		%% 成就获得
-define(ITEM_FROM_BATTLE,			1013).		%% 战斗获得
-define(ITEM_FROM_PERFECT_FINISH,	1014).		%% 完美通过获得
-define(ITEM_FROM_TASK,				1015).		%% 任务获得
-define(ITEM_FROM_MAIL,				1016).		%% 邮件获得
-define(ITEM_FROM_GARDEN_HARVEST,	1017).		%% 家园收获所得
-define(ITEM_FROM_GARDEN_REF_SEED,  1018).		%% 从家园里刷出来的种子
-define(ITEM_FROM_DRAGON_HUNT,      1019).		%% 抽奖得到的物品
-define(ITEM_FROM_RING_UPGRADE,		1020).		%% 戒指提升品质获得
-define(ITEM_FROM_SPLIT,			1021).		%% 拆分后获得
-define(ITEM_CONSUME_BONUS,			1022).		%% 消耗活动奖励
-define(ITEM_FROM_GUILD_COMP,		1023).		%% 公会竞赛奖励
-define(ITEM_FROM_FIRST_CHARGE,		1024).		%% 首次充值奖励礼包
-define(ITEM_FROM_SCORE_EXCHANGE,	1025).		%% 角斗场积分兑换
-define(ITEM_FROM_USE_ITEM,			1026).		%% 使用物品获得
-define(ITEM_FROM_QILING,			1027).		%% 启灵获得
-define(ITEM_FROM_XILIAN,			1028).		%% 洗练获得
-define(ITEM_FROM_UPQUALITY,		1029).		%% 提升品质获得
-define(ITEM_FROM_UPGRATE,			1030).		%% 升级获得
-define(ITEM_FROM_INLAY,			1031).		%% 镶嵌宝石获得
-define(ITEM_FROM_WLSD,				1032).       %% 从藏宝图获得
-define(ITEM_FROM_COMPOSE,			1033).		%% 合成宝石获得
-define(ITEM_FROM_CONVERT,			1034).		%% 转化宝石获得
-define(ITEM_FROM_CARVE,			1035).		%% 雕刻宝石获得
-define(ITEM_FROM_DUNGEON_AWARD,	1036).		%% 打完副本的奖励
-define(ITEM_FROM_MARSTOWER,		1037).		%% 英雄塔获得
-define(ITEM_FROM_BUY_HORN,         1038).       %% 用银币买小喇叭获得
-define(ITEM_FROM_EMPLOY,			1039).		%% 招募时获得
-define(ITEM_FROM_TARGET,			1040).  		%% 目标系统获得
-define(ITEM_FROM_CONSUME,			1041).		%% 回馈活动获得
-define(ITEM_FROM_GUIDE,			1042).		%% 每日指引获得
-define(ITEM_FROM_HONOUR_EXCHANGE,	1043).		%% 荣誉点兑换获得
-define(ITEM_FROM_LOTTERY,			1044).		%% 投壶获得
-define(ITEM_FROM_VIP,				1045).		%% VIP礼包
-define(ITEM_FROM_SHOP,				1046).		%% 商店购买
-define(HU_LAO_GUAN,				1047).      %% 虎牢关掉落
-define(ITEM_FROM_SWORD,			1048).		%% 神剑活动掉落

-define(ITEM_ADD_STACK,				2001).		%% 堆叠后增加（包括物品移动，物品生成，宝石拆卸、交易获得等情况）

-define(ITEM_DEC_STACK,				3001).		%% 堆叠后减少
-define(ITEM_DEC_USE,				3002).		%% 使用后减少
-define(ITEM_DEC_STONE_COMPOS,		3003).		%% 宝石合成后减少
-define(ITEM_DEC_STONE_INLAY,		3004).		%% 宝石镶嵌后减少
-define(ITEM_DEC_FRAGMANT_COMPOS,	3005).		%% 碎片合成后减少
-define(ITEM_DEC_RING_UPGRADE,		3006).		%% 戒指提升品质减少
-define(ITEM_DEC_SPLIT,				3007).		%% 拆分后减少
-define(ITEM_DEC_STUFF_USE,			3008).		%% 扣除材料


-define(ITEM_DEL_SHOP,				4001).		%% 卖商店后删除
-define(ITEM_DEL_USE,				4002).		%% 使用删除
-define(ITEM_DEL_STONE_COMPOS,		4003).		%% 宝石合成后删除
-define(ITEM_DEL_STONE_INLAY,		4004).		%% 宝石镶嵌后删除
-define(ITEM_DEL_FRAGMANT_COMPOS,	4005).		%% 碎片合成删除
-define(ITEM_DEL_TRADE,				4006).		%% 交易后删除
-define(ITEM_DEL_CONVERT,			4007).		%% 票据兑换删除
-define(ITEM_DEL_STACK,				4008).		%% 堆叠删除
-define(ITEM_DEL_RING_COMPOS,		4009).		%% 合成戒指删除
-define(ITEM_DEL_RING_UPGRADE,		4010).		%% 戒指提升品质删除
-define(ITEM_DEL_RESOLVE_EQUIP,		4011).		%% 装备分解后删除
-define(ITEM_DEL_FROM_MOVE,			4012).		%% 物品移动删除
-define(ITEM_DEL_STUFF_USE,			4013).		%% 使用材料删除
-define(ITEM_DEL_THROW,				4014).		%% 丢弃删除

-define(HP_ADD_FROM_GM,				400).		%% GM加血

%% ========================= 物品世界唯一ID转变原因 ==========================
-define(ITEM_CHANGE_BY_TRADE,		1).			%% 交易后转变

%% ========================= 佣兵经验来源 ==========================
-define(EXP_FROM_TASK,				1001).		%% 任务获得
-define(EXP_FROM_FRIENDS,			1002).		%% 好友祝福获得
-define(EXP_FROM_TRAIN,				1003).		%% 训练所得
-define(EXP_FROM_SPEED_UP,			1004).		%% 突飞所得
-define(EXP_FROM_CARD,				1005).		%% 经验卡
-define(EXP_FROM_BATTLE,			1006).		%% 战斗所得
-define(EXP_FROM_USE_ITEM,			1007).		%% 使用物品所得
-define(EXP_FROM_PLANTING,			1008).		%% 种植经验种子所得
-define(EXP_FROM_SLAVE_WORK,		1009).		%% 奴隶劳作所得
-define(EXP_FROM_DAZUO,				1010).	%% 打坐所得
-define(EXP_DROM_GUIDE,				1011).	%% 每日指引获得
-define(EXP_FROM_MARSTOWER,			1012).	%% 爬塔获得

%% ========================= 精力改变 ==========================
-define(ENERGY_FROM_GM,				1999).		%% gm
-define(ENERGY_RECOVERY,			1001).		%% 精力回复
-define(ENERGY_BUY,					1002).		%% 精力购买
-define(ENERGY_FROM_DRAGON_HUNT,	1003).		%% 抽奖抽到的精力
-define(ENERGY_FROM_CONSUME_BONUS,	1004).		%% 消耗活动奖励
-define(ENERGY_FROM_LIVENESS,		1005).		%% 活跃度奖励
-define(ENERGY_FROM_ONLINE_AWARD,	1006).		%% 在线奖励
-define(ENERGY_FROM_RUNNING,		1007).		%% 跑步
-define(ENERGY_FROM_TASK,			1008).		%% 任务
-define(ENERGY_FROM_TAX,			1009).		%% 征收


-define(ENERGY_ENTER_DUNGEON,		2001).		%% 进入挑战副本消耗
-define(ENERGY_ONLINE_ARENA_PVE,	2002).		%% 在线竞技打怪
-define(ENERGY_TOWER,				2003).		%% 下水道打怪
-define(ENERGY_SCENE_MONSTER,		2004).		%% 打野外怪
-define(ENERGY_STORY_PROCESS,		2005).		%% 打进度副本
-define(ENERGY_DUNGEON_PROCESS,		2006).		%% 打挑战副本中的怪
-define(ENERGY_ONLINE_ARENA_PVP,	2007).		%% 在线竞技PVP
-define(ENERGY_RUN_BUSINESS,		2008).		%% 跑商战斗
-define(ENERGY_GRAB_RB,				2009).		%% 劫商战斗
-define(ENERGY_BOXING_PVE,			2010).		%% 擂台赛PVE
-define(ENERGY_BOXING_PVP,			2011).		%% 擂台赛PVP
-define(ENERGY_ARENA_PVE,			2012).		%% 离线竞技PVE
-define(ENERGY_ARENA_PVP,			2013).		%% 离线竞技PVP
-define(ENERGY_GARDEN,				2014).		%% 家园
-define(ENERGY_CYCLIC_TASK_COST,	2015).		%% 自动完成循环任务消耗
-define(ENERGY_FROM_MELEE,  		2016).      %% 团战

-define(PRACTICE_FROM_GM,				1999).	%gm获取
-define(PRACTICE_FROM_USE_BILL,			1001).		%%历练券
-define(PRACTICE_FROM_BATTLE,			1002).		%%战斗获取
-define(PRACTICE_FROM_FAST_BATTLE,		1003).		%%扫荡获取
-define(PRACTICE_FROM_ARENA,			1004).		%%竞技场获取
-define(PRACTICE_FROM_CONSUME_BONUS,	1005).		%%每日消耗活动获取
-define(PRACTICE_FROM_DRAGON_HUNT,		1006).		%%猎龙获取
-define(PRACTICE_FROM_GARDEN,			1007).		%%花园获取
-define(PRACTICE_FROM_GUILD_SALARY,		1009).		%%帮派福利
-define(PRACTICE_FROM_ONLINE_AWARD,		1010).		%%每日抽奖
-define(PRACTICE_FROM_RELATION,			1011).		%%好友
-define(PRACTICE_FROM_RUNNING,			1012).		%%跑步
-define(PRACTICE_FROM_TASK,				1013).		%%任务
-define(PRACTICE_FROM_GOLD_FAST_BATTLE,	1014).		%%金币扫荡
-define(PRACTICE_FROM_PICK_STAR,		1015).		%%  摘星
-define(PRACTICE_FROM_MELEE, 			1016).           %% 团战 
-define(PRACTICE_FROM_GIFT_BAG, 		1017).		%%使用礼包获得       
-define(PRACTICE_FROM_MEDITATION, 		1018).		%%冥想获得
-define(PRACTICE_FROM_USE_ITEM, 		1019).		%%使用物品获得

-define(PRACTICE_USE_ADD_DRESS_EXP,     2001).	%% 增加外形装扮消耗（包括坐骑、翅膀等）


-define(POPULARITY_FROM_GM,						1999).	%gm获取
-define(POPULARITY_FROM_BATTLE,					1001).	%% 世界boss
-define(POPULARITY_FROM_HORN,					1002).	%% 小喇叭声望
-define(POPULARITY_FROM_ARENA,					1003).	%% 竞技场获取声望
-define(POPULARITY_FROM_ARENA_DAILY_AWARD,		1004).	%% 竞技场每日奖励
-define(POPULARITY_FROM_CONSUME_BONUS,			1005).	%% 每日消耗活动获取
-define(POPULARITY_FROM_DRAGON_HUNT,			1006).	%% 猎龙获取
-define(POPULARITY_FROM_FLOWER,					1007).	%% 猎龙获取
-define(POPULARITY_FROM_GUILD_COMP,				1008).	%% 帮战获取
-define(POPULARITY_FROM_ONLINE_ARENA,			1009).	%% 在线竞技获取
-define(POPULARITY_FROM_GIFT_BAG,				1010).	%% 使用礼包获得
-define(POPULARITY_FROM_USE_ITEM,				1011).	%% 使用物品获得
-define(POPULARITY_FROM_SLAVE_WORK,				1012).	%% 奴隶劳作所得
-define(POPULARITY_FROM_TASK,					1013).	%% 任务获得
-define(POPULARITY_FROM_ROB_YUNBIAO,            1014).  %% 劫镖获得
-define(POPULARITY_FROM_ACHIEVE,				1015).	%% 成就获得
-define(POPULARITY_FROM_YUNBIAO,  				1016).  %% 运镖获得				
-define(POPULARITY_FROM_MAIL,					1017).  %% 邮件获得
-define(POPULARITY_FROM_BOSS_BATTLE,        	1018).	%% boss战斗获得
-define(POPULARITY_FROM_TARGET,					1019).	%% 目标系统获得
-define(POPULARITY_FROM_JUNGONGTASK,			1020).	%% 军功任务中获得
-define(POPULARITY_FROM_CONSUME,				1021).	%% 回馈活动中获得
-define(POPULARITY_FROM_GUIDE,					1022).	%% 每日指引获得
-define(POPULARITY_FROM_ARENA_CHALLENGE,		1023).	%% 竞技场挑战获得
-define(POPULARITY_UP_PINJIE,					1024).	%% 用于提升器魂品阶
-define(POPULARITY_UPGRATE_SKILL,				1025).	%% 用于升级技能

%% ======================== 灵力改变的操作类型 ==========================
-define(WAKAN_FORM_USE_ITEM,1001).		%% 使用物品获得

%% ======================== 荣誉积分改变的操作类型 ==========================
-define(HONOUR_SCORE_FROM_GM,			1999).
-define(HONOUR_SCORE_FROM_COMP,			1001).
-define(HONOUR_SCORE_USE_EXCHANGE_ITEM,	2001).

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