%% 这个头文件用于定义用户日志数据的宏定义，如金币或银币消耗的类别


%% ======================== 金币改变的操作类型 ==========================
-define(GOLD_CHARGE_MONEY, 1). 			%% 1;		// 充钱
-define(GOLD_CHARGE_CARD, 2). 			%% 2;		// 充卡
-define(GOLD_MAIL, 3). 					%% 3;		// 邮件
-define(GOLD_SYSTEM, 4). 				%% 4;		// 后台系统
-define(GOLD_GIFT, 5). 					%% 5;		// 礼包
-define(GOLD_PRAY, 6). 					%% 6;		// 好友祝福
-define(GOLD_TAX, 7). 					%% 7;		// 征收
-define(GOLD_ACHIEVE, 8). 				%% 8;		// 成就
-define(GOLD_TARGET_CARD, 9). 			%% 9;		// 目标达成翻牌
-define(GOLD_ARENA_CARD, 10). 			%% 10;		// 竞技场翻牌
-define(GOLD_GAIN_FROM_TRADE, 11).		%% 11;		// 交易所得
-define(GOLD_GUILD_SALARY, 12).			%% 12;		// 公会福利
-define(GOLD_FROM_TASK, 13).			%% 13;		// 任务所得
-define(GOLD_FROM_BARRIER_AWARD, 14).	%% 14;		// 通关奖励所得
-define(GOLD_FROM_ONLINE_AWARD, 15).	%% 15;      // 在线奖励所得
-define(GOLD_FROM_USE_BILL, 16).		%% 16;      // 使用金票获得
-define(GOLD_FROM_GARDEN_HARVEST, 17).	%% 17;      // 家园收获所得
-define(GOLD_FROM_TRADE_CANCEL, 18).	%% 18;		// 取消交易获得
-define(GOLD_FROM_CONSUME_BONUS, 19).	%% 19;		// 消耗活动奖励
-define(GOLD_FROM_GUILD_COMP, 20).		%% 20;		// 公会竞赛奖励
-define(GOLD_TARGET, 21).			%% 21;		// 目标系统获得

-define(GOLD_FORCE_TAX, 21). 			%% = 21;	// 强征花销
-define(GOLD_TRAIN_TIME, 22). 			%% ;		// 训练时间
-define(GOLD_TRAIN_MODE, 23). 			%% ;		// 训练类型
-define(GOLD_TRAIN_QUEUE, 24). 			%% ;		// 训练队列
-define(GOLD_HOLY_QUEUE, 25). 			%% ;		// 圣痕队列
-define(GOLD_HOLY_CD, 26). 				%% ;		// 圣痕CD
-define(GOLD_INTENSIFY_RATE, 27). 		%% ;		// 装备强化概率
-define(GOLD_INTENSIFY_CD, 28). 		%% ;		// 装备强化CD
-define(GOLD_REFRESH_DAILY_TASK, 29). 	%% ;		// 刷新日常任务
-define(GOLD_COMPLETE_DAILY_TASK, 30). 	%% ;		// 直接完成日常任务
-define(GOLD_SHOPPING, 31). 			%% ;		// 商城购买
-define(GOLD_COMMISSION, 32). 			%% ;		// 委派CD
-define(GOLD_FORMATION_UP_CD, 33). 		%% ;		// 阵型升级CD
-define(GOLD_BUG_DUNGEON_TICKET, 34). 	%% ;		// 购买进入副本次数
-define(GOLD_BUG_ENERGY, 35). 			%% ;		// 购买精力值
-define(GOLD_FOSTER_TYPE, 36). 			%% ;		// 培养类型
-define(GOLD_CONTRIBUTE_CD, 37). 		%% ;		// 地区捐献CD
-define(GOLD_GUILD_UP, 38). 			%% ;		// 公会升级
-define(GOLD_TRAIN_SPEED_UP, 39). 		%% ;		// 金币突飞
-define(GOLD_GUILD_CREATE, 40). 		%% ;		// 公会创建 wyx 20111128add
-define(GOLD_GUILD_DONATE, 41). 		%% ;		// 公会捐献 wyx 20111128add
-define(GOLD_BOXING, 42). 				%% ;		// 挑战擂台赛
-define(GOLD_GEM_REFRESH, 50).
-define(GOLD_BUG_HORN, 51).				%% ;		// 用于购买小喇叭
-define(GOLD_LEVEL_UP_PET, 52).			%% ;		// 用于升级宠物
-define(GOLD_UP_TALENT_PROTECT, 53).	%% ;		// 用于佣兵提升天赋的保护消耗
-define(GOLD_CHANGE_PET_NAME, 54).		%% ;		// 用于宠物更名
-define(GOLD_BUY_ITEM_COST, 55).		%% ;		// 用于商城购买物品
-define(GOLD_INTENSIFY_EQUIP_COST, 56).	%% ;		// 用于装备强化保护
-define(GOLD_EXTEND_BAG_COST, 57).		%% ;		// 用于扩充背包格子
-define(GOLD_EXTEND_BANK_COST, 58).		%% ;		// 用于扩充仓库格子
-define(GOLD_EXTEND_CD_COST, 59).		%% ;		// 用于扩充圣痕CD队列
-define(GOLD_CLEAR_CD_COST, 60).		%% ;		// 用于清除CD状态
-define(GOLD_XUNXIAN_COST, 61).			%% ;		// 用于炼金
-define(GOLD_REVIVE_COST, 62).			%% ;		// 战斗失败后复活消耗
-define(GOLD_TRADE_COST, 63).			%% ;		// 用于交易
-define(GOLD_TOWER_COST, 64).			%% ;		// 下水道金币必过一层
-define(GOLD_REFRESH_SEED_COST, 65).	%% ;		// 家园刷新种子消耗
-define(GOLD_ARENA_ENEGRY, 66).			%% ;		// 购买竞技场次数消耗
-define(GOLD_CYCLIC_TASK_COST, 67).		%% ;		// 购买竞技场次数消耗
-define(GOLD_RUNNING_COST, 68).			%% ;		// 用于跑步
-define(GOLD_BUY_VIP_CARD, 69).			%% ;		// 用于购买VIP卡
-define(GOLD_COMPLETE_FAST_BATTLE, 70).	%% ;		// 用于立刻完成快速挑战
-define(GOLD_DRAGON_HUNT_COST, 71).		%% ;		// 抽奖系统消耗
-define(GOLD_CLEAR_BATTLE_CD_COST, 72).	%% ;		// 用于清除普通战斗CD
-define(GOLD_CLEAR_INTEN_CD_COST, 73).	%% ;		// 用于清除强化CD
-define(GOLD_CLEAR_TRAIN_CD_COST, 74).  %% ;		// 用于清除培养CD
-define(GOLD_CLEAR_HOLY_CD_COST, 75).	%% ;		// 用于清除圣痕CD
-define(GOLD_CLEAR_TAX_CD_COST, 76).	%% ;		// 用于清除征收CD
-define(GOLD_CLEAR_ARENA_CD_COST, 77).	%% ;		// 用于清除角斗场
-define(GOLD_GET_HELPER_MER, 78).		%% ;		// 摘星星时召唤帮助佣兵
-define(GOLD_BUY_FLOWER, 79).			%% ;		// 用于送花 
-define(GOLD_TATTOO_GAMBLE, 80).		%% ;		// 刻纹猜拳
-define(GOLD_IDENTIFY_COST, 81).		%% ;		// 用于鉴定武器
-define(GOLD_LOCK_TAVERN_CAREER_COST, 82).	%% ;	// 用于锁定酒馆佣兵职业 
-define(GOLD_FROM_MAIL, 83).			%% ;		// 邮件寄送
-define(GOLD_FROM_GIFT_BAG, 84).		%% 16;      // 使用礼包获得
-define(GOLD_RESET_TOWER_COST, 85).		%%       	// 用于下水道重置
-define(GOLD_ARENA_GOLD_CHALLANGE_COST, 86).	%%	// 用于竞技场的金币越级挑战
-define(GOLD_FOSTER_CARD_COST, 			87).	%%	// 用于购买培养卡
-define(GOLD_FROM_USE_ITEM, 88).		%% 16;      // 使用物品获得
-define(GOLD_FIX_SKILL_COST, 89).		%%      	// 刷新技能时用于固定技能
-define(GOLD_BUY_ZHAOSHU, 90).			%%      	// 用于购买诏书 
-define(GOLD_FEED_HORSE, 91).        	%% 			// 喂养坐骑
-define(GOLD_TALENT_CARD_COST, 92).     %% 			// 用于购买天赋卡
-define(GOLD_HORSE_EQUIP_COST, 93).     %% 			// 用于购买坐骑时装
-define(GOLD_FINISH_QIHUN_LEVELING, 94).%% 			// 用于立刻完成器魂修炼
-define(GOLD_FINISH_QIHUN_CD, 		95).%% 			// 用于清除器魂修炼的cd
-define(GOLD_QILING_COST, 96).			%%			// 用于启灵装备
-define(GOLD_XILIAN_ITEM_COST, 97).		%%			// 用于洗练装备
-define(GOLD_UP_PINJIE, 	98).		%%			// 用于提升器魂品阶
-define(GOLD_BUG_DUNGEON_TIMES, 	99).%%			// 用于购买进入副本的次数
-define(GOLD_UPGRATE_ITEM_COST,	100).	%%			// 用于升级装备
-define(GOLD_OPEN_LAND, 	101).		%%			// 用于开垦土地
-define(GOLD_REFRESH_SEED, 	102).		%%			// 用于刷新种子的品质
-define(GOLD_CLEAN_PLANTING_CD, 	103).%%			// 用于清除种植等待cd
-define(GOLD_FROM_WLSD,             104).%%         // 藏宝图获得
-define(GOLD_CLEAN_WORK_CD,         105).%%         // 用于清除奴隶劳作cd
-define(GOlD_BUY_GUAJI_COST,		106).	%%			// 用于购买挂机
-define(GOlD_REFRESH_BIAOCHE_COST,		107).	%%		// 用于刷新镖车
-define(GOlD_ZHUANYUN_COST,		108).	         %%		// 用于转运
-define(GOLD_RESET_TOWER,		109).			%%	// 用于重置英雄塔
-define(GOLD_USE_TO_INSPIRE,  110).		%%      用于鼓舞
-define(GOLD_USE_TO_CLEAN_CD,  111).           %%用于清除boss战斗cd
-define(GOLD_BUY_HORN,			112).			%%	// 用于买小喇叭
-define(GOLD_CLEAR_ENTER_GUILD, 113).			%% 退出后加入公会要24小时的cd，清除cd要扣费
-define(GOLD_BUY_HUNSHI, 114).			%% 买魂石
-define(GOLD_FRESH_MYSTICAL_SHOP,115).			%% 用于刷新神秘商店物品
-define(GOLD_BUY_MYSTICAL_SHOP_ITEM, 116).		%% 用于购买神秘商店物品
-define(GOlD_REFRESH_JUNGONGTASK, 117).			%% 用于刷新军工任务
-define(GOlD_AUTO_COMPLETE_JUNGONGTASK, 118).	%% 用于自动完成军工任务


%% ======================== 银币改变的操作类型 ==========================
-define(SILVER_TAX, 1). 					%% 1    征收所得
-define(SILVER_FORCE_TAX, 2).				%% 2    强征所得
-define(SILVER_TASK, 3). 					%% 3    任务所得
-define(SILVER_FROM_GOLD, 4).				%% 4    金币兑换
-define(SILVER_FROM_MONSTER, 5).			%% 5    怪物掉落
-define(SILVER_SYSTEM, 6). 					%% 6    系统原有
-define(SILVER_GIFT_BAG, 7).				%% 7    礼包奖励
-define(SILVER_GIFT_LOTTERY, 8). 			%% 8    礼品券
-define(SILVER_FROM_ACHIEVE, 9). 			%% 9    成就所得
-define(SILVER_GUILD_SALARY, 10). 			%% 10    公会福利
%% -define(SILVER_FROM_TASK, 11). 				%% 11    任务所得
-define(SILVER_GAIN_FROM_TRADE, 12).		%% 12  交易所得
-define(SILVER_ARENA_CARD, 13). 			%% 13  竞技场翻牌
-define(SILVER_FROM_RUN_BUSINESS, 14). 		%% 14 跑商获得
-define(SILVER_FROM_PRAY, 15). 				%% 15 祝福获得
-define(SILVER_FROM_ONLINE_ARENA, 16). 		%% 16 在线竞技获得
-define(SILVER_FROM_BARRIER_AWARD, 17).		%% 17 通关奖励所得
-define(SILVER_FROM_ONLINE_AWARD,	18).	%% 18 在线奖励所得
-define(SILVER_FROM_ARENA_AWARD,	19).	%% 19 离线竞技场排名奖励
-define(SILVER_FROM_USE_BILL, 20).			%% 20 使用银票获得
-define(SILVER_FROM_ITEM_SELL, 21).			%% 21 出售物品获得
-define(SILVER_FROM_EQUIP_DEMOTE, 22).		%% 22 武器降级获得
-define(SILVER_FROM_XUNXIAN_SELL_ITEM, 23).	%% 23 寻仙物品出售获得
-define(SILVER_FROM_GARDEN_HARVEST, 24).	%% 24 家园收获所得
-define(SILVER_FROM_DRAGON_HUNT, 25).		%% 25 抽奖所得
-define(SILVER_FROM_CYCLIC_TASK, 26).		%% 26 循环任务奖励所得
-define(SILVER_FROM_CONSUME_BONUS, 27).		%% 27 消耗活动奖励
-define(SILVER_FROM_GUILD_COMP, 28).		%% 28 公会战奖励
-define(SILVER_FROM_LIVENESS, 29).			%% 29 活跃度奖励
-define(SILVER_TATTOO_GAMBLE_REWARD, 30).	%% 30 刻纹猜拳奖励
-define(SILVER_FROM_MAIL, 31).				%% 31   邮件寄送
-define(SILVER_FROM_GIFT_BAG, 32).			%% 32   使用礼包获得
-define(SILVER_FROM_MEDITATION, 33).		%% 33 冥想获得
-define(SILVER_FROM_USE_ITEM, 34).			%% 34   使用物品获得
-define(SILVER_FROM_PLANTING, 35).			%% 35   种植银币种子所得
-define(SILVER_FROM_FENGLU,					36).	%% 领取俸禄获得
-define(SILVER_FROM_SLAVE_WORK,				37).	%% 奴隶劳作所得
-define(SILVER_FROM_SLAVE_TAX,				38).	%% 从奴隶那得到的税金
-define(SILVER_FROM_ROB_YUNBIAO,            39).    %%从劫镖得到
-define(SILVER_FROM_WATERING, 40).                  %% 帮助好友浇水获得
-define(SILVER_FROM_ARENA_DAILY_AWARD,      41).    %%竞技场每天奖励
-define(SILVER_FROM_WLSD,					42). 	%%藏宝图所得
-define(SILVER_FROM_YUNBIAO,				43).    %%运镖获得
-define(SILVER_FROM_BOSS_BATTLE,			44).	%%boss战斗获得
-define(SILVER_FROM_GUILD_SKILL,            45).    %%工会技能升级失败退回钱;
-define(SILVER_FROM_TARGET,					46).    %%目标系统获得
-define(SILVER_FROM_JUNGONGTASK,			47).	%%军功任务中获得

-define(SILVER_BUY_TIME, 1000). 			%% 1000    买道具消耗
-define(SILVER_INTENSIFY_COST, 1001). 		%% 1001    强化消耗
-define(SILVER_EMPLOY_COST, 1002). 			%% 1002   招募消耗
-define(SILVER_TRAIN_COST, 1003). 			%% 1003   训练消耗
-define(SILVER_GUILD_CREATE,1004).			%% 1004   公会创建消耗		wyx20111128 add
-define(SILVER_GUILD_DONATE,1005).			%% 1005        公会捐献消耗	wyx20111128 add
-define(SILVER_BOXING, 1006). 				%% 1004        挑战擂台赛
-define(SILVER_RUN_BUSINESS, 1007).         %% 1007   跑商银币
-define(SILVER_BUY_ITEM_COST, 1008).        %% 1008   用于商店购买物品
-define(SILVER_COMPOS_STONE_COST, 1009).	%% 1009   用于宝石合成
-define(SILVER_INLAY_STONE_COST, 1010).		%% 1010   用于宝石镶嵌
-define(SILVER_BACKOUT_STONE_COST, 1011).	%% 1011   用于宝石拆除
-define(SILVER_INTENSIFY_EQUIP_COST, 1012).	%% 1012   用于装备强化
-define(SILVER_UPGRADE_HOLY_COST, 1013).	%% 1013   用于圣痕升级
-define(SILVER_XUNXIAN_COST, 1014).			%% 1014   用于寻仙
-define(SILVER_RUNNING_COST, 1015).			%% 1015   用于跑步
-define(SILVER_COMPOS_RING_COST, 1016).		%% 1016        用于戒指合成
-define(SILVER_BOSS, 1017).                 %% 1017   用于世界BOSS
-define(SILVER_EMPLOY_ROLE, 1018).          %% 1018   用于招募佣兵
-define(SILVER_REFRESH_SKILL, 1019).        %% 1019   用于刷新技能
-define(SILVER_FEED_HORSE, 1020).        	%% 1020   喂养坐骑
-define(SILVER_LEVELING_QIHUN, 1021).       %% 1021   用于修炼器魂
-define(SILVER_XILIAN_ITEM_COST,1022).		%% 1022	  用于洗练装备
-define(SILVER_UP_TALENT,1023).				%% 1023	  用于提升天赋
-define(SILVER_FOSTER,1024).				%% 1024	  用于武将培养
-define(SILVER_UPQUALITY_ITEM_COST,1025).	%% 1025   用于提升装备品质
-define(SILVER_UPGRATE_ITEM_COST,1026).		%% 1026	  用于升级装备
-define(SILVER_INLAY_ITEM_COST,1027).		%% 1027	  用于镶嵌宝石
-define(SILVER_BACKOUT_ITEM_COST,1028).		%% 1028   用于拆卸宝石
-define(SILVER_COMPOSE_ITEM_COST,1029).		%% 1029   用于合成宝石
-define(SILVER_CONVERT_ITEM_COST,1030).		%% 1030   用于转化宝石
-define(SILVER_CARVE_ITEM_COST,1031).		%% 1031   用于雕刻宝石
-define(SILVER_USE_TO_INSPIRE,27000).		%% 27000     用于鼓舞
-define(SILVER_UPGRATE_SKILL,1041).			%% 1041   用于升级技能
-define(SILVER_LEARN_GUILD_SKILL, 1042).     %%        用于升级工会技能
-define(SILVER_BUY_MYSTICAL_SHOP_ITEM, 1043).		%% 用于购买神秘商店物品


%% ========================= 物品来源去向 ==========================
-define(ITEM_FROM_SHOP,				100).		%% 商店购买
-define(ITEM_FROM_BAG_GIFT,			101).		%% 使用礼包获得
-define(ITEM_FROM_STONE_COMPOS,		102).		%% 宝石合成获得
-define(ITEM_FROM_STONE_BACKOUT,	103).		%% 宝石拆卸获得
-define(ITEM_FROM_FRAGMANT_COMPOS,	104).		%% 碎片合成获得
-define(ITEM_FROM_CREATE_ROLE,		105).		%% 创建角色获得
-define(ITEM_FROM_EMPLOY_MER,		106).		%% 招募佣兵获得
-define(ITEM_FROM_XUNXIAN,			107).		%% 炼金获得
-define(ITEM_FROM_ONLINE_ARENA,		108).		%% 在线竞技获得
-define(ITEM_FROM_ONLINE_AWARD,		109).		%% 在线奖励（登录奖励）获得
-define(ITEM_FROM_TRADE,			110).		%% 交易获得
-define(ITEM_FROM_ACHIEVE,			112).		%% 成就获得
-define(ITEM_FROM_BATTLE,			113).		%% 战斗获得
-define(ITEM_FROM_PERFECT_FINISH,	114).		%% 完美通过获得
-define(ITEM_FROM_TASK,				115).		%% 任务获得
-define(ITEM_FROM_MAIL,				116).		%% 邮件获得
-define(ITEM_FROM_GARDEN_HARVEST,	117).		%% 家园收获所得
-define(ITEM_FROM_GARDEN_REF_SEED,  118).		%% 从家园里刷出来的种子
-define(ITEM_FROM_DRAGON_HUNT,      119).		%% 抽奖得到的物品
-define(ITEM_FROM_RING_UPGRADE,		120).		%% 戒指提升品质获得
-define(ITEM_FROM_SPLIT,			121).		%% 拆分后获得
-define(ITEM_CONSUME_BONUS,			122).		%% 消耗活动奖励
-define(ITEM_FROM_GUILD_COMP,		123).		%% 公会竞赛奖励
-define(ITEM_FROM_FIRST_CHARGE,		124).		%% 首次充值奖励礼包
-define(ITEM_FROM_SCORE_EXCHANGE,	125).		%% 角斗场积分兑换
-define(ITEM_FROM_USE_ITEM,			126).		%% 使用物品获得
-define(ITEM_FROM_QILING,			127).		%% 启灵获得
-define(ITEM_FROM_XILIAN,			128).		%% 洗练获得
-define(ITEM_FROM_UPQUALITY,		129).		%% 提升品质获得
-define(ITEM_FROM_UPGRATE,			130).		%% 升级获得
-define(ITEM_FROM_INLAY,			131).		%% 镶嵌宝石获得
-define(ITEM_FROM_WLSD,				132).       %% 从藏宝图获得
-define(ITEM_FROM_COMPOSE,			133).		%% 合成宝石获得
-define(ITEM_FROM_CONVERT,			134).		%% 转化宝石获得
-define(ITEM_FROM_CARVE,			135).		%% 雕刻宝石获得
-define(ITEM_FROM_DUNGEON_AWARD,	136).		%% 打完副本的奖励
-define(ITEM_FROM_MARSTOWER,		137).		%% 英雄塔获得
-define(ITEM_FROM_BUY_HORN,         138).       %% 用银币买小喇叭获得
-define(ITEM_FROM_GM,				199).		%% GM获得
-define(ITEM_FROM_EMPLOY,			200).		%% 招募时获得
-define(ITEM_FROM_TARGET,			201).  		%% 目标系统获得

-define(ITEM_ADD_STACK,				200).		%% 堆叠后增加（包括物品移动，物品生成，宝石拆卸、交易获得等情况）
-define(ITEM_DEC_STACK,				201).		%% 堆叠后减少
-define(ITEM_DEC_USE,				202).		%% 使用后减少
-define(ITEM_DEC_STONE_COMPOS,		203).		%% 宝石合成后减少
-define(ITEM_DEC_STONE_INLAY,		204).		%% 宝石镶嵌后减少
-define(ITEM_DEC_FRAGMANT_COMPOS,	205).		%% 碎片合成后减少
-define(ITEM_DEC_RING_UPGRADE,		206).		%% 戒指提升品质减少
-define(ITEM_DEC_SPLIT,				207).		%% 拆分后减少
-define(ITEM_DEC_STUFF_USE,			208).		%% 扣除材料


-define(ITEM_DEL_THROW,				300).		%% 丢弃删除
-define(ITEM_DEL_SHOP,				301).		%% 卖商店后删除
-define(ITEM_DEL_USE,				302).		%% 使用删除
-define(ITEM_DEL_STONE_COMPOS,		303).		%% 宝石合成后删除
-define(ITEM_DEL_STONE_INLAY,		304).		%% 宝石镶嵌后删除
-define(ITEM_DEL_FRAGMANT_COMPOS,	305).		%% 碎片合成删除
-define(ITEM_DEL_TRADE,				306).		%% 交易后删除
-define(ITEM_DEL_CONVERT,			307).		%% 票据兑换删除
-define(ITEM_DEL_STACK,				308).		%% 堆叠删除
-define(ITEM_DEL_RING_COMPOS,		309).		%% 合成戒指删除
-define(ITEM_DEL_RING_UPGRADE,		310).		%% 戒指提升品质删除
-define(ITEM_DEL_RESOLVE_EQUIP,		311).		%% 装备分解后删除
-define(ITEM_DEL_FROM_MOVE,			312).		%% 物品移动删除
-define(ITEM_DEL_STUFF_USE,			313).		%% 使用材料删除


-define(HU_LAO_GUAN,				1200).      %% 虎牢关掉落

-define(HP_ADD_FROM_GM,				400).		%% GM加血

%% ========================= 物品世界唯一ID转变原因 ==========================
-define(ITEM_CHANGE_BY_TRADE,		1).			%% 交易后转变

%% ========================= 佣兵经验来源 ==========================
-define(EXP_FROM_TASK,				1).		%% 任务获得
-define(EXP_FROM_FRIENDS,			2).		%% 好友祝福获得
-define(EXP_FROM_TRAIN,				3).		%% 训练所得
-define(EXP_FROM_SPEED_UP,			4).		%% 突飞所得
-define(EXP_FROM_CARD,				5).		%% 经验卡
-define(EXP_FROM_BATTLE,			6).		%% 战斗所得
-define(EXP_FROM_USE_ITEM,			7).		%% 使用物品所得
-define(EXP_FROM_PLANTING,			8).		%% 种植经验种子所得
-define(EXP_FROM_SLAVE_WORK,		9).		%% 奴隶劳作所得
-define(EXP_FROM_DAZUO,				10).	%% 打坐所得

%% ========================= 精力改变 ==========================
-define(ENERGY_RECOVERY,			1).		%% 精力回复
-define(ENERGY_BUY,					2).		%% 精力购买
-define(ENERGY_ENTER_DUNGEON,		3).		%% 进入挑战副本消耗
-define(ENERGY_ONLINE_ARENA_PVE,	4).		%% 在线竞技打怪
-define(ENERGY_TOWER,				5).		%% 下水道打怪
-define(ENERGY_SCENE_MONSTER,		6).		%% 打野外怪
-define(ENERGY_STORY_PROCESS,		7).		%% 打进度副本
-define(ENERGY_DUNGEON_PROCESS,		8).		%% 打挑战副本中的怪
-define(ENERGY_ONLINE_ARENA_PVP,	9).		%% 在线竞技PVP
-define(ENERGY_RUN_BUSINESS,		10).	%% 跑商战斗
-define(ENERGY_GRAB_RB,				11).	%% 劫商战斗
-define(ENERGY_BOXING_PVE,			12).	%% 擂台赛PVE
-define(ENERGY_BOXING_PVP,			13).	%% 擂台赛PVP
-define(ENERGY_ARENA_PVE,			14).	%% 离线竞技PVE
-define(ENERGY_ARENA_PVP,			15).	%% 离线竞技PVP
-define(ENERGY_GARDEN,				16).	%% 家园
-define(ENERGY_CYCLIC_TASK_COST,	17).	%% 自动完成循环任务消耗
-define(ENERGY_FROM_DRAGON_HUNT,	18).	%% 抽奖抽到的精力
-define(ENERGY_FROM_CONSUME_BONUS,	19).	%% 消耗活动奖励
-define(ENERGY_FROM_GM,	20).				%% gm
-define(ENERGY_FROM_LIVENESS,	21).		%% 活跃度奖励
-define(ENERGY_FROM_ONLINE_AWARD,	22).		%% 在线奖励
-define(ENERGY_FROM_RUNNING,	23).		%% 跑步
-define(ENERGY_FROM_TASK,	24).		%% 任务
-define(ENERGY_FROM_TAX,	25).		%% 征收
-define(ENERGY_FROM_MELEE,  26).             %% 团战

-define(PRACTICE_FROM_USE_BILL,		1).		%%历练券
-define(PRACTICE_FROM_BATTLE,		2).		%%战斗获取
-define(PRACTICE_FROM_FAST_BATTLE,	3).		%%扫荡获取
-define(PRACTICE_FROM_ARENA,	4).		%%竞技场获取
-define(PRACTICE_FROM_CONSUME_BONUS,5).		%%每日消耗活动获取
-define(PRACTICE_FROM_DRAGON_HUNT,6).		%%猎龙获取
-define(PRACTICE_FROM_GARDEN,7).		%%花园获取
-define(PRACTICE_FROM_GM,8).		%%gm获取
-define(PRACTICE_FROM_GUILD_SALARY,9).		%%帮派福利
-define(PRACTICE_FROM_ONLINE_AWARD,10).		%%每日抽奖
-define(PRACTICE_FROM_RELATION,11).		%%好友
-define(PRACTICE_FROM_RUNNING,12).		%%跑步
-define(PRACTICE_FROM_TASK,13).		%%任务
-define(PRACTICE_FROM_GOLD_FAST_BATTLE,14).		%%金币扫荡
-define(PRACTICE_FROM_PICK_STAR,15).		%%  摘星
-define(PRACTICE_FROM_MELEE, 16).           %% 团战 
-define(PRACTICE_FROM_GIFT_BAG, 17).		%%使用礼包获得       
-define(PRACTICE_FROM_MEDITATION, 18).		%%冥想获得
-define(PRACTICE_FROM_USE_ITEM, 19).		%%使用物品获得



-define(POPULARITY_FROM_BATTLE,					1).		%% 世界boss
-define(POPULARITY_FROM_HORN,					2).		%% 小喇叭声望
-define(POPULARITY_FROM_ARENA,					3).		%% 竞技场获取声望
-define(POPULARITY_FROM_ARENA_DAILY_AWARD,		4).		%% 竞技场每日奖励
-define(POPULARITY_FROM_CONSUME_BONUS,			5).		%% 每日消耗活动获取
-define(POPULARITY_FROM_DRAGON_HUNT,			6).		%% 猎龙获取
-define(POPULARITY_FROM_FLOWER,					7).		%% 猎龙获取
-define(POPULARITY_FROM_GUILD_COMP,				8).		%% 帮战获取
-define(POPULARITY_FROM_ONLINE_ARENA,			9).		%% 在线竞技获取
-define(POPULARITY_FROM_GIFT_BAG,				10).	%% 使用礼包获得
-define(POPULARITY_FROM_USE_ITEM,				11).	%% 使用物品获得
-define(POPULARITY_FROM_SLAVE_WORK,				12).	%% 奴隶劳作所得
-define(POPULARITY_FROM_TASK,					13).	%% 任务获得
-define(POPULARITY_FROM_ROB_YUNBIAO,            14).    %% 劫镖获得
-define(POPULARITY_FROM_ACHIEVE,				15).	%% 成就获得
-define(POPULARITY_FROM_YUNBIAO,  				16).    %% 运镖获得				
-define(POPULARITY_FROM_MAIL,					17).  	%% 邮件获得
-define(POPULARITY_FROM_BOSS_BATTLE,        	18).	%% boss战斗获得
-define(POPULARITY_FROM_TARGET,					19).	%% 目标系统获得
-define(POPULARITY_FROM_JUNGONGTASK,			20).    %% 军功任务中获得

-define(POPULARITY_FROM_GM,						99).	%%gm获取

-define(POPULARITY_UP_PINJIE,					100).	%% 用于提升器魂品阶

-define(POPULARITY_UPGRATE_SKILL,				101).	%% 用于升级技能


-define(WAKAN_FORM_USE_ITEM,1002).		%% 使用物品获得



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