

-define(ERR_UNKNOWN, 0).					%% 未知错误
-define(ERR_NO_USER, 1).					%% 用户不存在
-define(ERR_OTHER_LINE, 2).					%% 用户不在本线
-define(ERR_OFFLINE, 3).					%% 用户不在线（包括本线和其它线）
-define(ERR_NOT_ENOUGH_GOLD, 4).			%% 金币不足
-define(ERR_NOT_ENOUGH_SILVER, 5).			%% 银币不足
-define(ERR_NOT_ENOUGH_PRACTICE, 6).		%% 历练不足
-define(ERR_IN_CD, 7).						%% CD未清完
-define(ERR_SCENE_RESTRICT, 8).				%% 场景不允许
%% -define(ERR_NOT_ENOUGH_HOLY_LEVEL, 9).		%% 主圣痕等级不足
-define(ERR_NOT_ENOUGH_MER_LEVEL, 10).		%% 佣兵等级不足
-define(ERR_NOT_ENOUGH_POPULARITY, 11).		%% 威望不足
-define(ERR_CARRER_NOT_ALLOWED, 12).		%% 职业不允许
-define(ERR_TOO_MANY_MER, 13).				%% 佣兵数已满（不能提升了）
%% -define(ERR_TOO_MANY_MER2, 14).				%% 佣兵数已满（可升级为VIP，增加可招募佣兵数2）
%% -define(ERR_STORY_NOT_COMPLETED, 15).		%% 剧情未完成
%% -define(ERR_MER_NOT_ENOUGH, 16).			%% 佣兵不足
-define(ERR_SILVER_EXCEED_ENOUGH, 17).		%% 银币超过上限
%% -define(ERR_TAX_COUNT_USE_UP_ENOUGH, 18).	%% 本天征收次数用完
-define(ERR_NOT_ENOUGH_VIP_LEVEL, 19).		%% VIP等级不足
-define(ERR_NOT_ENOUGH_PER_MER_LEVEL, 20).	%% 主佣兵等级不足
%% -define(ERR_NOT_NEARBY_NPC, 21).			%% 玩家不在指定NPC附近
%% -define(ERR_NOT_NEARBY_SHOP, 22).			%% 玩家不在商店附近
%% -define(ERR_PLAYER_CHEAT, 23).				%% 玩家开挂
%% -define(ERR_ENERGY_NOT_ENOUGH, 24).			%% 玩家精力不足
%% -define(ERR_ENERGY_NOT_ENOUGH2, 25).		%% 组队中的另一个成员精力不足
%% -define(ERR_NOT_ENOUGH_PER_MER_LEVEL1, 26).	%% 对方主佣兵等级不足
-define(ERR_NOT_ENOUGH_JUN_WEI, 27).		%% 君威不足
-define(ERR_NOT_ENOUGH_GUAN_ZHI, 28).		%% 官职不够
-define(ERR_ROLE_HAS_ITEMS,		29).		%% 请先脱下佣兵装备
-define(ERR_SKILL_LEVEL_MAX,    30). 		%% 技能已达到最高级
-define(ERR_SKILL_NUM_NOT_MATCH, 31).		%% 保留的技能个数不正确
-define(ERR_MAP_CANNOT_GUAJI,    32).       %% 该地图不允许挂机
-define(ERR_NOT_GUILD_SKILL,     33).       %% 技能不是公会技能
-define(ERR_MUST_LEARN_FROM_ONE, 34).       %% 公会技能必须从一级学起
-define(ERR_NOT_ONE_BY_ONE,		 35).	    %% 公会技能每次只能升一级
-define(ERR_GUIDE_BOX_ERR,		 36).		%% 没有可领取奖励
-define(ERR_NOT_ENOUGH_HONOUR,   37).		%% 荣誉点不足
-define(ERR_BAG_NO_SHOE,		 38).		%% 背包里没有小飞鞋
-define(ERR_NO_FEEDBACK_GOLD,	 39).		%% 没有可领取的元宝

%% 场景
%% -define(ERR_SCENE_NOT_COMPLETE,			11001).			%% 场景进度未完成
%% -define(ERR_MOVE_CHECK_FAILED,			11002).			%% 场景 move check 失败
%% -define(ERR_TEAM_SCENE_NOT_PERMITED,	11003).			%% 未达到进入组队场景要求

%% 副本
-define(ERR_DUNGEON_NO_BUY_TIMES,		11201).			%% 副本可购买次数已用完
-define(ERR_PROCESS_NO_FINISH,			11202).			%% 前一区域怪物未打完
-define(ERR_DUNGEON_NO_IN_TIMES,		11203).			%% 副本进入次数已用完
-define(ERR_AWARD_HAVE_TAKE,			11204).			%% 副本奖励已经领取
-define(ERR_DUNGEON_NOT_CLEAR,			11205).			%% 副本未完成
-define(ERR_AWARD_HAVE_CREAT,			11206).			%% 请不要再转转盘
-define(ERR_MATE_NOT_ENOUGH_LEVEL,		11207).			%% 队友等级不够
-define(ERR_MATE_NO_TIMES,				11208).			%% 队友副本进入次数已用完

%% 物品系统 add by wangyl 2011-10-21
-define(ERR_ITEM_NOT_EXIST,				12001).			%% 物品不存在
%% -define(ERR_ITEM_MOD_NOT_EXIST,			12002).			%% 物品原型不存在
%% -define(ERR_ITEM_OVER_MAX,				12003).			%% 物品数量超限
-define(ERR_ITEM_BAG_NOT_ENOUGH,		12004).			%% 背包容量不足
%% -define(ERR_ITEM_BANK_NOT_ENOUGH,		12005).			%% 仓库容量不足
%% -define(ERR_ITEM_POS_NOT_MATCH,			12006).			%% 物品位置不匹配
%% -define(ERR_ITEM_TYPE_NOT_MATCH,			12007).			%% 物品类型不匹配
%% -define(ERR_ITEM_BIND_NOT_MATCH,			12008).			%% 物品绑定信息不匹配
%% -define(ERR_ITEM_ALREADY_BIND,			12009).			%% 物品已绑定
-define(ERR_ITEM_NOT_IN_BAG,			12010).			%% 物品不在背包中
%% -define(ERR_ITEM_NOT_PROP,				12011).			%% 物品非道具
-define(ERR_ITEM_NOT_EQUIPMENT,			12012).			%% 物品非装备
%% -define(ERR_ITEM_NOT_STONE,				12013).			%% 物品非宝石
%% -define(ERR_ITEM_NOT_RUNE,				12014).			%% 物品非保护符
%% -define(ERR_ITEM_GIFT_BAG,				12015).			%% 物品不能为礼包
%% -define(ERR_ITEM_NOT_GIFT_BAG,			12016).			%% 物品不是礼包
-define(ERR_ITEM_NOT_ENOUGH,			12017).			%% 物品数量不足
%% -define(ERR_ITEM_RANK_ERR,				12018).			%% 物品所在位置不允许操作
-define(ERR_ITEM_NOT_THROW,				12019).			%% 物品不能被丢弃
%% -define(ERR_ITEM_SELL_OVER_MAX,			12020).			%% 出售物品个数超限
-define(ERR_ITEM_BAG_OVER_MAX,			12021).			%% 当前背包格子数已达最大值
%% -define(ERR_ITEM_BANK_OVER_MAX,			12022).			%% 当前仓库格子数已达最大值
-define(ERR_ITEM_ILLEGAL_BAG_POS,		12023).			%% 物品所在背包格子非法
%% -define(ERR_ITEM_ILLEGAL_BANK_POS,		12024).			%% 物品所在仓库格子非法
%% -define(ERR_ITEM_ILLEGAL_EQUIP_POS,		12025).			%% 物品所在装备格子非法
%% -define(ERR_ITEM_ILLEGAL_INLAY_POS,		12026).			%% 物品所在镶嵌位置非法
%% -define(ERR_ITEM_BAG_POS_NOT_NULL,		12027).			%% 背包格子已有物品
%% -define(ERR_ITEM_BANK_POS_NOT_NULL,		12028).			%% 仓库格子已有物品
%% -define(ERR_ITEM_TYPE_NOT_STONE,			12029).			%% 非宝石类型
%% -define(ERR_ITEM_STONE_NOT_COMPOS,		12030).			%% 非可合成宝石
%% -define(ERR_ITEM_STONE_COMPOS_NUM,		12031).			%% 合成宝石数量错
%% -define(ERR_ITEM_STONE_DIFF_TYPE,		12032).			%% 合成宝石类型不一致
-define(ERR_ITEM_HOLE_NOT_INLAY,		12033).			%% 拆卸装备孔位未镶嵌宝石
-define(ERR_ITEM_ERR_EQUIP_POS,			12034).			%% 装备位置错
-define(ERR_ITEM_NOT_EQUIP,				12035).			%% 装备位置未装备物品
%% -define(ERR_ITEM_NOT_DEMOTE,			 	12036).			%% 装备不能被降级
%% -define(ERR_ITEM_EQUIPMENT_NOT_EXIST,	12037).			%% 装备不存在
-define(ERR_ITEM_STONE_NOT_EXIST,		12038).			%% 宝石不存在
-define(ERR_ITEM_HOLE_NOT_EXIST,		12039).			%% 装备镶嵌孔位不存在
-define(ERR_ITEM_HOLE_NOT_NULL,			12040).			%% 装备孔位已镶嵌了宝石
-define(ERR_ITEM_INLAY_SAME_TYPE,		12041).			%% 装备已镶嵌了同类型宝石
%% -define(ERR_ITEM_UNBELONG_PLAYER,		12042).			%% 物品不属于玩家
%% -define(ERR_ITEM_NOT_SILVER_BILL,		12043).			%% 物品非银票票据
%% -define(ERR_ITEM_NOT_GOLD_BILL,			12044).			%% 物品非金票票据
%% -define(ERR_ITEM_RUNE_LEVEL,				12045).			%% 保护符与合成宝石所需保护符不一致
-define(ERR_ITEM_EQUIP_INLAY_STONE,		12046).			%% 装备有镶嵌宝石
-define(ERR_ITEM_BAG_EXTEND,			12047).			%% 所选背包格子已开启过
%% -define(ERR_ITEM_BANK_EXTEND,			12048).			%% 所选仓库格子已开启过
%% -define(ERR_ITEM_RUNE_NUM,				12049).			%% 保护符数量不足
-define(ERR_ITEM_INTENSIFY_LEVEL_MAX,	12050).			%% 装备已达到最高强化级别
%% -define(ERR_ITEM_SHOP_NOT_SELL,			12051).			%% 本商店无此物品出售
-define(ERR_ITEM_NOT_SELL,				12052).			%% 物品不能出售
%% -define(ERR_ITEM_NOT_BUY,				12053).			%% 商城无此物品出售
%% -define(ERR_ITEM_PRIM_RING_NOT_EXIST,	12054).			%% 主戒指不存在
%% -define(ERR_ITEM_PRIM_NOT_RING,			12055).			%% 主戒指位置非戒指
%% -define(ERR_ITEM_SUB_RING_NOT_EXIST,		12056).			%% 副戒指不存在
%% -define(ERR_ITEM_SUB_NOT_RING,			12057).			%% 副戒指位置非戒指
%% -define(ERR_ITEM_SUB_RING_LEVEL,			12058).			%% 副戒指与主戒指等级不一致
%% -define(ERR_ITEM_SUB_INLAY_STONE,		12059).			%% 副戒指已镶嵌宝石
%% -define(ERR_STAR_STONE_NUM,				12060).			%% 星石数量不足
%% -define(ERR_ITEM_ON_SELL,				12061).			%% 物品出售锁定
%% -define(ERR_ITEM_TYPE_NOT_RING,			12062).			%% 非戒指类型
%% -define(ERR_ITEM_RING_NOT_COMPOS,		12063).			%% 非可合成戒指
%% -define(ERR_ITEM_RING_COMPOS_NUM,		12064).			%% 合成戒指数量错
%% -define(ERR_ITEM_RING_DIFF_TYPE,			12065).			%% 合成戒指类型不一致
%% -define(ERR_ITEM_RING_DIFF_LEVEL,		12066).			%% 合成戒指星级不一致
%% -define(ERR_ITEM_NOT_RESOLVE,			12067).			%% 物品不能被分解
%% -define(ERR_ITEM_NOT_SPLIT,				12068).			%% 物品不能被拆分
-define(ERR_ITEM_SPLIT_NUM,				12069).			%% 物品拆分数量错
%% -define(ERR_ITEM_STAR_LEVEL_MAX,			12070).			%% 星级已达到最高等级
%% -define(ERR_ITEM_NO_IDENTIFY,			12071).			%% 物品未被鉴定过
%% -define(ERR_ITEM_INTSIFY_LEVEL_NOT_ZERO,12072).			%% 被分解物品强化等级必须为0级
-define(ERR_ITEM_NOT_USE,				12073).			%% 物品不能被使用
-define(ERR_ITEM_NOT_QILING,			12074).			%% 物品不能被启灵
-define(ERR_ITEM_NOT_UPGRATE,			12075).			%% 物品不能被升级
-define(ERR_ITEM_UPGRATE_LEVEL_MAX,		12076).			%% 装备已达到最高强化级别升级级别
-define(ERR_ITEM_UPQUALITY_LEVEL_MAX,	12077).			%% 装备已达到最高品质
-define(ERR_ITEM_NOT_UPQUALITY,			12078).			%% 物品不能被提升品质
-define(ERR_ITEM_HAVE_QILING,			12079).			%% 物品已经被启灵过
-define(ERR_ITEM_INTENSIFY_LEVEL_ZERO,	12080).			%% 装备强化等级为0
-define(ERR_ITEM_NOT_XILIAN,			12081).			%% 物品不能被洗练
-define(ERR_XILIAN_LOCKSTAR_ERR,		12082).			%% 洗练锁定星星过低
-define(ERR_ITEM_INTENSIFY_HOLERATE_MAX,	12083).		%% 强化概率超过100
-define(ERR_ITEM_INTENSIFY_BASERATE_MAX,	12084).		%% 强化基础概率超过上限
-define(ERR_ITEM_MESSAGE_ERR,				12085).     %% 信息反馈有误
-define(COLLECT_NOT_EXIT,					12086).     %% 采集信息不存在
-define(ERR_ITEM_INLAY_TYPE_ERROR,			12087).     %% 装备不能镶嵌该种宝石
-define(ERR_MARSTOWER_BUY_CLOSE,			12088).		%% 爬塔买回层数尚未开启
-define(ERR_SKILL_NOT_EXIST,				12089).		%% 找不到技能书中的指定技能
-define(ERR_ITEM_LEVEL_BIGGER,				12090).		%% 装备不能超过佣兵等级
-define(ITEM_CREATE_POS_NOTNULL,			12091).     %% 佣兵带有同类物品，不能创建
-define(ITEM_CREATE_POS_ERR,				12092).     %% 创建佣兵物品位置非法
-define(ERR_INPUT_CARV_NUM, 				12093).     %% 请先输入雕刻宝石数目
-define(ERR_ITEM_XILIAN_LOCK_ALL,			12094).		%% 最多锁定5个属性
-define(ERR_ITEM_MYSTICAL_SHOP_ITEM_NUM,	12095).		%% 限购次数已满
-define(ERR_ITEM_MYSTICAL_SHOP_REFRESH,		12096).		%% 神秘商店已刷新，请重新打开神秘商店
-define(ERR_SKILL_HAVE_LEARNED,				12097).		%% 已经拥有该技能,不需学习

%% 寻仙系统
-define(ERR_XUNXIAN_LEVEL_UNUSE,		12101).			%% 寻仙需要30级方能开启
-define(ERR_XUNXIAN_NOT_EXIST,			12102).			%% 寻仙配置信息不存在
-define(ERR_XUNXIAN_ILLEGAL_INFO,		12103).			%% 信息非法
-define(ERR_XUNXIAN_ILLEGAL_ITEM_INDEX,	12104).			%% 寻仙物品索引非法
-define(ERR_XUNXIAN_ITEM_UNPICK,		12105).			%% 寻仙物品不能拾起
-define(ERR_XUNXIAN_NO_ITEMS_PICK,		12106).			%% 无可一键拾起寻仙物品
-define(ERR_XUNXIAN_NO_ITEMS_SELL,		12107).			%% 无可一键卖出寻仙物品
-define(ERR_XUNXIAN_ILLEGAL_LEVEL,  	12108).			%% 寻仙级别非法
-define(ERR_XUNXIAN_ITEMS_FULL,			12109).			%% 寻仙物品已满
-define(ERR_XUNXIAN_DEPRECATED,			12110).			%% 银币低于5万，不推荐银币寻仙
-define(ERR_XUNXIAN_POS_MAX,			12111).			%% 仙人等级已经最高

%% 官职、器魂、品阶、封地
-define(ERR_OFFICIAL_REACH_MAX,			13001).			%% 器魂等级已到达最大等级了
-define(ERR_LOCK_FAILED,				13002).			%% 锁定非奴隶玩家失败
-define(ERR_NO_FREE_CAGE,				13003).			%% 已没有空余的笼子了
-define(ERR_ALREADY_FREEDOM,			13004).			%% 玩家已经自由了
-define(ERR_NO_SLAVES,					13005).			%% 玩家已经没有奴隶了

%%邮件系统
-define(ERR_MAIL_WRONG_TITLE,            14001).        %%邮件标题错误
-define(ERR_MAIL_WRONG_NAME,             14002).		%%邮件用户名字错误
-define(ERR_MAIL_WRONG_CONTENT,          14003).		%%邮件内容错误
-define(ERR_MAIL_SEARCH_SQL_ERROR,       14004).        %%查询数据库出错
-define(ERR_MAIL_ITEM_HAVE_TAKE,		 14005).		%%附件已经领取过
-define(ERR_MAIL_ITEM_NOT_EXIT,			 14006).        %%附件不存在
-define(ERR_NO_MAIL_CAN_TAKE,			 14007).		%%没有可提取的附件


%% 佣兵系统
-define(ERR_MER_NOT_EXIST,				15001).			    %% 佣兵不存在
%% -define(ERR_MER_LEVEL_MUST_L_HOLY, 		15002).			%% 佣兵等级不能超过主圣痕等级
%% -define(ERR_CANCLE_TRAIN, 				15003).			%% 佣兵的训练取消了
%% -define(ERR_NOT_IN_TRAIN, 				15004).			%% 佣兵 IS not in training
-define(ERR_TALENT_FULL, 				15005).			    %% 天赋已达当前等级的培养上限
-define(ERR_NO_TALENT_CARD,				15006).			    %%  没有天赋卡
%% -define(ERR_NO_FOSTER_CARD,				15007).			%% 没有培养卡
-define(ERR_ON_BATTLE_MAX,				15008).				%% 出战人数已满
-define(ERR_SKILL_LOCK_ALL,				15009).				%% 请不要锁定所有技能
-define(ERR_FOSTER_MAX,					15010).				%% 属性已达当前等级的培养上限
-define(ERR_ROLE_HAS_ONBATTLE,			15011).				%% 请先取消出战状态

%%guild error info										    %% 公会错误代码
 -define(GUILD_ERROR_NAME_EXIST,			19001).			%% 公会名已存在
 -define(GUILD_ERROR_SILVER_NOTENOUGH,	    19002).			%% 银币不足
-define(GUILD_ERROR_MEMBER_OVERFLOW,		19003).			%% 公会成员数量已满
%% -define(GUILD_ERROR_SUCCESS_QUIT,		19004).			%% 成功的退出公会
-define(GUILD_ERROR_NOT_DONATE,			    19005).			%% 请先为帮派贡献一次
%% -define(GUILD_ERROR_DISMISS_CANCEL,		19006).			%% 取消公会解散成功
%% -define(GUILD_ERROR_POSITION_OVERFLOW,	19007).			%% 该职位人员已满
%% -define(GUILD_ERROR_ASSIGN_NOPOWER,		19008).			%% 无权进行分派
%% -define(GUILD_ERROR_LEVEL_NOTENOUGH,		19009).			%% 用户等级不足
%% -define(GUILD_ERROR_DISMISS_NOPOWER,		19010).			%% 无权解散公会
%% -define(GUILD_ERROR_DISMISS_NOCONDITION,19011).			%% 解散条件不足
-define(GUILD_ERROR_ALREADY_GET_WELFARE,	19012).			%% 您今天已经领取过福利了
-define(GUILD_ERROR_HISMERIT_NOTENOUGH,	    19013).			%% 历史功勋不足

%% -define(GUILD_ERROR_TECH_SUCCESS,		19014).			%% 公会科技升级成功
%% -define(GUILD_ERROR_LEVEL_OVERFLOW,		19015).			%% 所需等级超出
%% -define(GUILD_ERROR_MISSIONAWARD_GET,	19016).			%% 已经获取公会奖励
-define(GUILD_ERROR_DONATE_SILVER_OVERFLOW,19017).		    %% 已经达到当天捐献银币的上限
%% -define(GUILD_ERROR_MISSION_NUM_OVERFLOW,19018).		%% 当天领取的公会任务已超出
%% -define(GUILD_ERROR_MISSION_APPLY,		19019).			%% 申请公会任务
%% -define(GUILD_ERROR_BEDISMISSED,			19020).			%% 已被踢出公会
%% -define(GUILD_ERROR_APPLY_CANCEL,		19021).			%% 取消公会申请
%% -define(GUILD_ERROR_BEENINGUILD,			19022).			%% 用户已经在公会中
  -define(GUILD_ERROR_GOLD_NOTENOUGH,		19023).			%% 金币不足
%% -define(GUILD_ERROR_NOTINGUILD,			19024).			%% 用户当前不在公会中
%% -define(GUILD_ERROR_NOEXIST,				19025).			%% 所查找的公会不存在或用户不存在
%% -define(GUILD_ERROR_PROHIBITED_WORD,		19026).			%% 公会名称或者公会宣言中含有屏蔽词
%% -define(GUILD_ERROR_NOT_IN_APPLY_LIST,	19027).			%% 批准的成员不在申请列表中
 -define(GUILD_ERROR_FIRST_DAY_SALARY,	19028).			%% 第一天不能领福利
%% -define(GUILD_ERROR_GUILD_QUIT_CD,		19029).			%% 退出公会后一天内不能再次加入
%% -define(GUILD_ERROR_COMPETITION_CANT_APPLY, 19030).		%% 没到公会战报名时间
%% -define(GUILD_ERROR_NORMAL,				19031).			%% 其他错误
-define(GUILD_ERROR_PROHIBITED_WORDS, 19032).                %% 非法语言
-define(GUILD_ERROR_FIRE_TOP_FIVE, 19033).                  %%不能开除历史功勋前5的成员
-define(GUILD_ERROR_TRANSFER_SUCCESS, 19034).               %% 转让帮主成功
-define(GUILD_ERROR_TRANSFER_FAIL, 19035).                  %% 转让的成员要精英及以上职位，历史功勋前10
-define(GUILD_ERROR_APPLY_REACH_LIMIT,	19036).			%% 申请的工会的数目已经达到上限

%% 战斗系统
-define(ERR_BATTLE_ATTACKER_IS_ON_BATTLE,    20001).   %% 玩家正在战斗
-define(ERR_BATTLE_DEFENDER_IS_ON_BATTLE,    20002).   %% 被挑战的玩家正在战斗中
-define(ERR_BATTLE_DEFENDER_IS_NOT_ON_LINE,  20003).   %% 被挑战的玩家不在线
-define(ERR_BATTLE_MONSTER_IS_ON_BATTLE,	 20004).    %%当前怪物正在战斗中，请稍后再来


%% CD系统
%% -define(ERR_CD_TYPE_NOT_EXIST,			23001).			%% CD类型不存在
%% -define(ERR_CD_ILLEGAL_CONFIG,			23002).			%% CD配置信息不存在
-define(ERR_CD_CDING,					23003).			%% CD状态中
%% -define(ERR_CD_OVER_MAX_NUM,				23004).			%% CD队列超过上限
%% -define(ERR_CD_NUM_NOT_EXIST,			23005).			%% CD队列号不存在
%% -define(ERR_CD_NOT_NEED_CLEAR,			23006).			%% CD无需清除

%% VIP系统
-define(ERR_HIGHT_LEVEL_NOT_OVER,			28001).			%% 高级VIP未失效
-define(ERR_VIP_YEAR_NOT_OVER,				28002).			%% 半年费VIP未失效 
-define(ERR_VIP_YEAR_HAVE_BUY,				28003).			%% 已经购买了半年费VIP

%% 组队
%% -define(ERR_TEAM_IN_BATTLE,				30001).			%% 战斗中，不能组队
 -define(ERR_TEAM_IN_ANOTHER_TEAM,		30002).			%% 在另一个队伍中，不能组队
 -define(ERR_TEAM_ALREADY_IN_TEAM_SCENE,	30003).			%% 在组队场景中，不能组队
 -define(ERR_TEAM_ALREADY_IN_YUN_BIAO,	30009).			%% 被邀请玩家运镖中，不能组队
 -define(ERR_TEAM_ALREADY_IN_BATTLE,	30010).			%% 被邀请玩家战斗中，不能组队
 -define(ERR_TEAM_FULL_SELF,	30011).			%% 你的队伍人数已满，
 -define(ERR_TEAM_INVITE_SUCCESS,	30012).			%% 组队邀请已发出，请耐心等待，
 -define(ERR_TEAM_NOT_LEAD,	30013).			%% 您不是队长，不能邀请，
 -define(ERR_TEAM_IN_COMP, 30014).         %% 比武中，不能邀请

 -define(ERR_TEAM_INVITE_SELF,			30004).			%% 抱歉，不能邀请自己
-define(ERR_TEAM_NOT_TEAM_SCENE,			30005).			%% 被邀请者等级不足，不能进入此副本
-define(ERR_TEAM_OFFLINE,			30006).			%% 被邀请者不在线
-define(ERR_TEAM_FULL,			30007).			%% 队伍已满。不能加入
-define(ERR_TEAM_NOT_EXIST,			30008).			%% 队伍已经解散
%% 魂石系统
-define(ERR_HUNSHI_NOT_ENOUGH,			35001).			%% 魂石不足
-define(ERR_hunshi_NOT_HAVE_TO_UP,			35002).			%% 魂珠已经升级到可达到的最大等级

%% 打坐
-define(ERR_NOT_IN_DAZUO,				49001).			%% 用户不在打坐状态
%% 挂机
-define(ERR_GUAJI_TIMES_ZERO,			49002).			%% 挂机次数为0
%% 英雄塔
-define(ERR_RESET_TIMES_ZERO,			49003).			%% 重置次数为0
-define(ERR_ACHIEVE_FLOOR_NOTENOUGHT,	49004).			%% 通关层数不够
-define(ERR_NOTENOUGHT_POINT,			49005).			%% 积分不够
-define(ERR_KING_BUSY,					49006).			%% 霸主被人挑战中
-define(ERR_HAVE_BEEN_KING,				49007).			%% 已经是霸主身份
-define(ERR_CHALLENGE_TIMEOUT,			49008).			%% 挑战超时
-define(ERR_IS_THE_REST_LEVEL,			49009).			%% 已重置到最底层
-define(ERR_CANT_CHANGLE_SELF,			49010).			%% 不能挑战自己
-define(ERR_OUTO_CHALLENGING,			49011).			%% 请先取消自动挑战
-define(ERR_ACHIEVE_FLOOR_HIGHTER,		49012). 		%% 未通关的关卡不能挂机
-define(ERR_NO_AUTO_CHALLENGE,			49013).			%% 没有自动挑战任务
%% 成就
-define(ERR_ACHIEVE_AWARD_ERR,			29001).			%% 成就奖励不可领取
%%
-define(ERR_TARGET_AWARD_ERR,			34001).			%% 目标奖励不可领取

%% 好友
-define(ERR_WRONG_USER_NAME,18000).    %%错误的用户名
-define(ERR_ALREADY_FRIEND,18001).		%%已经是好友
-define(ERR_ALREADY_IN_BLACK_LIST,18002). %%已经在黑名单
-define(ERR_FRIEND_OFFLINE,18003).  		%%好友不在线
-define(ERR_ADD_FRIEND_TO_SELF,18004).  		%%加自己为好友
-define(ERR_PAY_TIMES_IS_FULL,18005).  		%%今日祝福次数已满
-define(ERR_PAYED_TIMES_IS_FULL,18006).  		%%今日好友被祝福次数已满

%%运镖
-define(ERR_ROB_TIMES_ZERO, 26000).     		%%您今天的打劫次数已满
-define(ERR_ROBED_TIMES_ZERO, 26001).  			%%镖主很可怜了，手下留情吧
-define(ERR_NOT_YUNBIAO_STATE, 26002).  		%%非运镖状态
-define(ERR_YUNBIAO_TIMES_ZERO, 26003). 		%%您今天的运镖次数已满
-define(ERR_ROBER_IS_YUNBIAOING, 26004).     	%%您自己也在运镖，不能劫镖哦
-define(ERR_BEING_ROBED, 26005).         		%%镖主正在和别人混战，还是坐山观虎斗好了
-define(ERR_ROBER_IS_SO_SMALL, 26006).   		%%您不能以强示弱，欺压小辈哦
-define(ERR_YUNBIAO_FIRST_FINISH_NOW,	26007). %%请您先把手上的镖车送到目的地！
-define(ERR_AREADY_HIGH_TYPE,			26008). %%您当前的镖车已是最高品质
-define(ERR_JIXING_HIGHEST,				26009). %%当前吉星等级已经达到上限！
-define(ERR_ROBER_LEVEL_TOO_LOWER,		26010). %%您还未到30级，不要做劫镖这么危险的事情啊

%% 种植
-define(ERR_FENGDI_WATER,                16001).        %% 浇水次数超过2次     
%% 聊天
-define(ERR_CAN_NOT_CHAT,         16003). %% 您当前处于禁言状态
-define(ERR_PROHIBIT_WORD,16004).			%%含有敏感词
%%竞技系统
-define(ERR_RANK_BEHIND, 31000).     %%您不能挑战后面哦
-define(ERR_RANK_YOURSELF, 31001).     %%您不能挑战自己哦
-define(ERR_NO_FIVE_CHALLENGE_TIMES, 31002). %%挑战次数没达到5次哦
-define(ERR_REACH_THE_MAX_CHALLENGE_TIME, 31003). %%已达到最大挑战次数
-define(ERR_ARENA_SYSTEM_NOT_OPEN, 31004).  %%竞技场30级开启

%%boss
-define(ERR_SILVER_INSPIRE_OVER, 62001). %%银币鼓舞达到最大
-define(ERR_GOLD_INSPIRE_OVER, 62002). %%金币鼓舞达到最大
-define(ERR_SILVER_INSPIRE_NO_SUCCES, 62003).%%银币鼓舞没有成功=======
-define(ERR_BOSS_BATTLE_IS_NOT_OPEN, 62004).  %%世界BOSS战斗还未开启，请耐心等待

-define(ERR_FENGDI_WATER2,                16001).        %% 浇水次数超过2次    
-define(ERR_FENGDI_WATER50,                16002).        %% 浇水次数超过50次  

-define(ERR_CAN_NOT_CHAT_USER_NOT_ONLINE,  16005). 		%% 用户不在线
-define(ERR_CAN_NOT_CHAT_ERROR,         16006). 		%% 异常聊天
-define(ERR_CAN_NOT_CHAT_HORN_NOT,         16007). 		%% 小喇叭不足
-define(ERR_CAN_NOT_IN_TEAM,  16008). 					%% 您不在组队中，无法发送聊天内容

-define(ERR_JUNGONG_TASK_HAVE_RECEIVE, 36000).			%% 请先完成当前军功任务
-define(ERR_JUNGONG_TASK_ALL_DONE,	36001).				%% 今日军功任务已做完
-define(ERR_JUNGONG_TASK_NOT_FINISH, 36002).			%% 军功任务未完成

-define(ERR_COMP_ALREADY_APPLY,			37000).					%% 退出比武后不能重新进入
-define(ERR_COMP_IS_DIE,				37001).					%% 您已经阵亡，请使用还魂丹复活
-define(ERR_COMP_OTHER_IS_DIE,			37002).					%% 对方已阵亡，请等待TA复活
-define(ERR_COMP_NOT_DIE,				37003).					%% 您还未阵亡，无需使用还魂丹
-define(ERR_COMP_ALREADY_LEAVE,			37004).					%% 您已经离开比武场
-define(ERR_COMP_OTHER_ALREADY_LEAVE,	37005).					%% 对方已经离开比武场
-define(ERR_COMP_OTHER_IS_CLOAKING,		37006).					%% 对方已经隐形
-define(ERR_COMP_END_IN_ADVANCE,		37007).					%% 比武已经提前结束
-define(ERR_COMP_ALREADY_RAISE_BANNER,	37008).					%% 已经升旗了
-define(ERR_COMP_NOT_COMP_TIME,			37009).					%% 现在不是比武活动时间，不能报名

-define(ERR_HAVE_CHALLENGER, 63001).					%% 通知客户端国王正在被其他人挑战
-define(COMBAT_POWER_TOO_LOW, 63002).					%% 出战角色战斗力总和小于1000
-define(CANNOT_CHALLENGE_SELF, 63003).					%% 不能挑战自己
-define(NOT_FIRST_IN_ARENA, 63004).						%% 24点结算时不是竞技场第一名
-define(KING_CHANGE_TODAY, 63005).						%% 今天国王已经易主，明天才能挑战
