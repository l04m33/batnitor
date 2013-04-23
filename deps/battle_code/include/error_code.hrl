

-define(ERR_UNKNOWN, 0).					%% 未知错误
-define(ERR_NO_USER, 1).					%% 用户不存在
-define(ERR_OTHER_LINE, 2).					%% 用户不在本线
-define(ERR_OFFLINE, 3).					%% 用户不在线（包括本线和其它线）
-define(ERR_NOT_ENOUGH_GOLD, 4).			%% 金币不足
-define(ERR_NOT_ENOUGH_SILVER, 5).			%% 银币不足
-define(ERR_NOT_ENOUGH_PRACTICE, 6).		%% 历练不足
-define(ERR_IN_CD, 7).						%% CD未清完
-define(ERR_SCENE_RESTRICT, 8).				%% 场景不允许
-define(ERR_NOT_ENOUGH_POINT, 9).			%% 积分不足
-define(ERR_NOT_ENOUGH_MER_LEVEL, 10).		%% 佣兵等级不足
-define(ERR_NOT_ENOUGH_POPULARITY, 11).		%% 威望不足
-define(ERR_CARRER_NOT_ALLOWED, 12).		%% 职业不允许
-define(ERR_TOO_MANY_MER, 13).				%% 佣兵数已满（不能提升了）
%% -define(ERR_TOO_MANY_MER2, 14).				%% 佣兵数已满（可升级为VIP，增加可招募佣兵数2）
-define(ERR_NOT_ENOUGH_BIND_GOLD, 14).		%% 银元宝不足
-define(ERR_NOT_ENOUGH_JUNLIANG, 16).			%% 军粮不足
-define(ERR_SILVER_EXCEED_ENOUGH, 17).		%% 银币超过上限
%% -define(ERR_TAX_COUNT_USE_UP_ENOUGH, 18).	%% 本天征收次数用完
-define(ERR_NOT_ENOUGH_VIP_LEVEL, 19).		%% VIP等级不足
-define(ERR_NOT_ENOUGH_PER_MER_LEVEL, 20).	%% 主佣兵等级不足
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
-define(ERR_NOT_ENOUHT_JUNLING,	 40).		%% 军令不足
-define(ERR_CANT_USE_TOGETHER,	 41).		%% 该物品不能批量使用
-define(ERR_NO_ONBATTLE_ROLES,	 42).		%% 没有出战佣兵
-define(ERR_NOT_CHARGE_YET,	     43).		%% 还没有充值过


%% 场景
-define(ERR_SCENE_FORBIT_FLY,			11001).			%% 飞行前请骑上6级坐骑/穿上强化+10翅膀
-define(ERR_SCENE_FLY_HIDE_HORSE,		11002).			%% 飞行中不能下骑乘
-define(ERR_SCENE_FLY_UNEQUIP_WING,		11003).			%% 飞行中不能脱下翅膀
-define(ERR_SCENE_FLY_CARD_TIME_OUT,	11004).			%% 飞行体验卡时间到，飞行能力消失
-define(ERR_SCENE_FLY_CARD_BEGIN,		11005).			%% 你获得了飞行能力，请注意左上角剩余时间

%% 副本
-define(ERR_DUNGEON_NO_BUY_TIMES,		11201).			%% 副本可购买次数已用完
-define(ERR_PROCESS_NO_FINISH,			11202).			%% 前一区域怪物未打完
-define(ERR_DUNGEON_NO_IN_TIMES,		11203).			%% 副本进入次数已用完
-define(ERR_AWARD_HAVE_TAKE,			11204).			%% 副本奖励已经领取
-define(ERR_DUNGEON_NOT_CLEAR,			11205).			%% 副本未完成
-define(ERR_AWARD_HAVE_CREAT,			11206).			%% 请不要再转转盘
-define(ERR_MATE_NOT_ENOUGH_LEVEL,		11207).			%% 队友等级不够
-define(ERR_MATE_NO_TIMES,				11208).			%% 队友副本进入次数已用完
-define(ERR_DUNGEON_NOT_ENOUGH_TIMES,	11209).			%% 副本进入次数不足
-define(ERR_DUNGEON_IS_IN_GUAJI,		11210).			%% 已经有该副本的挂机任务
-define(ERR_DUNGEON_IS_NOT_IN_GUAJI,	11211).			%% 没有该副本的挂机任务
-define(ERR_DUNGEON_TEAM_IS_IN_GUAJI,	11212).			%% 队友挂机中,不能进入
-define(ERR_DUNGEON_NOT_PASS,			11213).			%% 自动挂机功能在通关副本后开启
-define(ERR_DUNGEON_GUAJI_START,		11214).			%% 副本自动挂机开始！
-define(ERR_DUNGEON_TEAM_LEAVE,			11215).			%% 队员已离队，无法重新进入
-define(ERR_DUNGEON_NOT_PASS_2,			11216).			%% 副本未通关

%% 物品系统 add by wangyl 2011-10-21
-define(ERR_ITEM_NOT_EXIST,				12001).			%% 物品不存在
-define(ERR_ITEM_CHANGE_CARD,			12002).			%% 已使用的变身卡效果还没消失
-define(ERR_ITEM_CHANGE_FLY,			12003).			%% 已使用的飞行体验卡效果还没消失
-define(ERR_ITEM_BAG_NOT_ENOUGH,		12004).			%% 背包容量不足
-define(ERR_ITEM_WING_NOT_UNEQUIP,		12005).			%% 翅膀不能卸下
-define(ERR_ITEM_LEVEL_TOO_LOW,			12006).			%% 完美合成时等级不够
-define(ERR_ITEM_FASHION_NOT_UNEQUIP,	12007).			%% 时装不能卸下
%% -define(ERR_ITEM_POS_NOT_MATCH,			12006).			%% 物品位置不匹配
%% -define(ERR_ITEM_TYPE_NOT_MATCH,			12007).			%% 物品类型不匹配
%% -define(ERR_ITEM_BIND_NOT_MATCH,			12008).			%% 物品绑定信息不匹配
%% -define(ERR_ITEM_ALREADY_BIND,			12009).			%% 物品已绑定
-define(ERR_ITEM_NOT_IN_BAG,			12010).			%% 物品不在背包中
%% -define(ERR_ITEM_NOT_PROP,				12011).			%% 物品非道具
-define(ERR_ITEM_NOT_EQUIPMENT,			12012).			%% 物品非装备
-define(ERR_ITEM_NOT_SAME_TYPE,			12013).			%% 只能在同种装备间传承
-define(ERR_ITEM_INHERIT_LOW,			12014).			%% 高级装备不能传承给低级装备
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
-define(ERR_MARKET_ITEM_NOT_EXIT,			12053).		%% 抢购物品已刷新，请重新打开商城
-define(ERR_MARKET_BUY_MAX,					12054).		%% 限购次数已满
-define(ERR_MARKET_ITEM_MAX,				12055).		%% 该商品已被抢购一空
-define(ERR_XILIAN_CANT_TAKE,				12056).		%% 每次只能选取一个洗练结果
-define(ERR_ON_BATTLE_ROLE_EXP_MAX,			12057).		%% 出战佣兵的经验已满
-define(ERR_ITEM_NOT_QILING,			12074).			%% 物品不能被启灵
-define(ERR_ITEM_NOT_MAIN_ROLE,				12058).		%% 佣兵不能装备该物品
-define(ERR_ITEM_NOT_WEAPON,				12060).		%% 物品非武器
-define(ERR_ITEM_ONLY_ONCE,					12061).		%% 开服计划道具只能买一次
-define(ERR_ITEM_ONLY_ONE,					12062).		%% 开服计划道具只能买一个
-define(ERR_ITEM_KAIFU_PLAN,				12063).		%% 元宝计划道具提示

-define(ERR_ITEM_SPLIT_NUM,				12069).			%% 物品拆分数量错

-define(ERR_ITEM_UPQUALITY_SAVE,		12070).			%% 保护成功，失败不降级！
-define(ERR_ITEM_NOT_USE,				12073).			%% 物品不能被使用
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
-define(ERR_STONE_LEVEL_MAX,				12098).		%% 宝石已经是最高级
-define(ERR_ITRM_HAS_JEWEL,					12099).		%% 请先拆下装备的宝石


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
-define(ERR_ROBED_TIME_FULL,				13003).			%% 被抢劫次数已满
-define(ERR_ALREADY_FREEDOM,			13004).			%% 玩家已经自由了
-define(ERR_NO_SLAVES,					13005).			%% 玩家已经没有奴隶了
-define(ERR_OFFICIAL_ONE_KEY_NOT_ENOUGH,13006).			%% 一键升级器魂金钱不足
-define(ERR_OFFICIAL_LEVEL_FULL,        13007).         %% 器魂等级已到达最高级
-define(ERR_BATTLE_FREE,                13008).         %% 你已经通过战斗获得自用
-define(ERR_SLAVE_BATTLE_FOR_SELF,      13017).         %% 不能抓自己为奴隶
-define(ERR_SLAVE_ALREADY_SLAVED,      13015).         %% 已经是奴隶了
-define(ERR_SLAVE_NOT_OPEN,             13016).         %% 该玩家还未开启奴隶系统
-define(ERR_SLAVE_NOT_PLAYER,             13020).         %% 该玩家不存在
-define(ERR_FRESH_FAILED,             13021).         %% 该玩家不存在
-define(ERR_FENGDI_ROB_FULL,             13022).         %% 抢劫次数已满
-define(ERR_FENGDI_ROB_SELF,             13023).         %% 抢劫自己

%% 答题活动
-define(ERR_ANSWER_TIME_NOREACH,		13009).			%% 还没到答题时间
-define(ERR_ANSWER_TIME_OVER,			13010).			%% 答题时间已结束
-define(ERR_ANSWER_HAVE_TAKE,			13011).			%% 答案选定后不能更改
-define(ERR_ANSWER_EFFECT_HAVE_USE,		13012).			%% 一次只能使用一种锦囊
-define(ERR_ANSWER_PROP_OVER,			13013).			%% 该种道具已用光
-define(ERR_ANSWER_PROP_TIME_ERR,		13014).			%% 只能在阅题时间使用锦囊

%% --------------------------------交易行------------------------------------
-define(ERR_TRADE_NONE,					13200).			%% 该商品已下架
-define(ERR_TRADE_BUY_SUCCESS,			13201).			%% 购买成功！请点击邮件领取
-define(ERR_TRADE_SELL_SUCCESS,			13202).			%% 托卖成功
-define(ERR_ITEM_NOT_TRADE,				13203).			%% 物品不能被交易
-define(ERR_TRADE_PRICE_LOW,			13204).			%% 售价不能低于物品的最低交易价格

%%邮件系统
-define(ERR_MAIL_WRONG_TITLE,            14001).        %%邮件标题错误
-define(ERR_MAIL_WRONG_NAME,             14002).		%%邮件用户名字错误
-define(ERR_MAIL_WRONG_CONTENT,          14003).		%%邮件内容错误
-define(ERR_MAIL_SEARCH_SQL_ERROR,       14004).        %%查询数据库出错
-define(ERR_MAIL_ITEM_HAVE_TAKE,		 14005).		%%附件已经领取过
-define(ERR_MAIL_ITEM_NOT_EXIT,			 14006).        %%附件不存在
-define(ERR_NO_MAIL_CAN_TAKE,			 14007).		%%没有可提取的附件
-define(ERR_NOT_INGUILD,				 14008).		%%你还没有加入任何公会
-define(ERR_MAIL_TITLE_PROHIBIT_WORD,	 14009).		%%邮件标题含有敏感词，请修改
-define(ERR_MAIL_CONTENT_PROHIBIT_WORD,	 14010).		%%邮件内容含有敏感词，请修改
-define(ERR_SEND_SUCCESS,				 14011).		%%发送成功


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
-define(ERR_ROLE_HAS_OWN,				15012).				%% 已拥有该武将
-define(ERR_SPECIAL_ROLE,				15013).				%% 请先获取该武将的诏令
-define(ERR_NOT_ON_BATTLE,				15014).				%% 武将还没出战 TODO
-define(ERR_ROLE_LEVEL_MAX,				15015).				%% 佣兵已达最大等级



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
-define(GUILD_ERROR_SYSTEM_NOT_OPEN,        19019).         %% 帮派系统还未开启

-define(GUILD_ERROR_ADD_OTHER_GUILD,		19014).			%% 已经加入其它帮派
-define(GUILD_ERROR_RECRUIT_MEMBER_MAX,		19015).			%% 工会成员数量已经到达上线
-define(GUILD_RECRUIT_APP_OK,	19016).			%% 招募申请通过
-define(GUILD_ERROR_DONATE_SILVER_OVERFLOW,19017).		    %% 已经达到当天捐献银币的上限
-define(GUILD_RECRUIT_APP_NOT_OK,19018).		%%  招募申请没有通过
%% -define(GUILD_ERROR_MISSION_APPLY,		19019).			%% 申请公会任务
-define(GUILD_ERROR_NOT_EXP_TOP_TEM,			19020).			%% 帮主只能转让给历史功勋前十名的玩家
%% -define(GUILD_ERROR_APPLY_CANCEL,		19021).			%% 取消公会申请
 -define(GUILD_ERROR_BEENINGUILD,			19022).			%% 用户已经在公会中
  -define(GUILD_ERROR_GOLD_NOTENOUGH,		19023).			%% 金币不足
  % -define(GUILD_ERROR_NOTINGUILD,			19024).			%% 用户当前不在公会中
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
-define(GUILD_ERROR_APPLY_SUCCESS, 19037). %% 帮会申请通过
-define(GUILD_ERROR_SEARCH_NULL, 19038). %% 搜索结果为空
-define(GUILD_ERROR_RANK_FULL, 19039). %% 该职位已满

-define(GUILD_HUNTING_ERROR_GOLD_ARROW_NOT_ENOUGH, 19040).  %% 帮派活动里的黄金箭数量不足
-define(GUILD_HUNTING_ERROR_PENDING,               19041).  %% 还没到活动时间
-define(GUILD_HUNTING_ERROR_NOT_IN_GUILD,          19042).  %% 还没加入帮派哦～
-define(GUILD_APPLY_MAX_TODAY,          19043).  %% 还没加入帮派哦～
-define(GUILD_GOLD_MAX_TODAY,          19044).  %% 今日金元宝捐献到达上限

%% 战斗系统
-define(ERR_BATTLE_ATTACKER_IS_ON_BATTLE,    20001).   %% 玩家正在战斗
-define(ERR_BATTLE_DEFENDER_IS_ON_BATTLE,    20002).   %% 被挑战的玩家正在战斗中
-define(ERR_BATTLE_DEFENDER_IS_NOT_ON_LINE,  20003).   %% 被挑战的玩家不在线
-define(ERR_BATTLE_MONSTER_IS_ON_BATTLE,	 20004).    %%当前怪物正在战斗中，请稍后再来
-define(ERR_BATTLE_TOO_MUCH_BATTLE_SINGLE_DAY, 20005).  %%当天刷怪超过800次，没有经验

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
-define(ERR_BUFFER_HAVE_USED,				28004).			%% VIP祝福已使用过
-define(ERR_HAVE_BEEN_VIP,					28005).			%% 已经是VIP,不能使用体验卡
-define(VIP_1,								28006).			%% 恭喜你升级成为蓝钻会员！
-define(VIP_2,								28007).			%% 恭喜你升级成为紫金会员！
-define(VIP_3,								28008).			%% 恭喜你升级成为至尊会员！
-define(ERR_NOT_VIP,						28009).			%% 当前还不是VIP！
-define(ERR_FRIEND_HAVE_BEEN_VIP,			28010).			%% 好友已经是VIP用户

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
 -define(ERR_TEAM_CAN_NOT_APPLY_LEVEL_NOT_OK, 30015).         %% 等级不够，不能申请此副本组队
 -define(ERR_TEAM_NOT_APPLY_ALREADY_IN_YUN_BIAO,  30016).         %% 运镖中，不能申请组队
 -define(ERR_TEAM_NOT_APPLY_IN_BATTLE,    30017).         %% 战斗中不能申请组队
 -define(ERR_TEAM_APPLY_IN_COMP, 30018).         %% 比武中，不能申请组队
 -define(ERR_TEAM_APPLY_IN_TEAM_SCENE,    30019).         %% 已经在副本中，不能申请组队
 -define(ERR_TEAM_APPLY_SUCCESS,    30020).         %% 组队申请已经发出
 -define(ERR_TEAM_APPROVE_SUCCESS,    30021).         %% 组队审核通过通知已经发出，请等候
 -define(ERR_TEAM_ADD_SUCCESS,    30022).         %% 加入队伍成功
 -define(ERR_TEAM_APPLY_FULL,    30023).         %% 您申请的队伍人数已满
 -define(ERR_TEAM_IN_CAN_TEAM_SCENE,    30024).         %% 您申请的队伍人数已满
 -define(ERR_TEAM_NOT_IN_ANY_TEAM,    30025).         %% 您不在队伍中
 -define(ERR_TEAM_NOT_ACCEPT_IN_NOW,    30026).         %% 当前场景汇总，您不能接受组队
 -define(ERR_TEAM_MATE_CANNOT_SET_FORMATION,    30027). %% 队员无法设置阵型

-define(ERR_ZHENTU_PASS_WD_ERR, 58001).
-define(ERR_TEAM_MATE_NOT_READY, 58002).
%% 神剑
-define(ERR_SWORD_NOT_START,32001). 		%% 天灵神剑活动未开始
-define(ERR_SWORD_LEVEL_UNMATCH, 32002).	%% 天灵神剑活动30级开启
-define(ERR_SWORD_LEVEL_UNMATCH_1,	32003). %% 需要等级30-49级
-define(ERR_SWORD_LEVEL_UNMATCH_2,	32004). %% 需要等级50-69级
-define(ERR_SWORD_LEVEL_UNMATCH_3,	32005). %% 需要达到70级以上
-define(ERR_SWORD_DIG_TIMES_OVER,	32006). %% 今天共参加神剑抢夺30次，已达上限！


-define(ERR_TEAM_INVITE_SELF,			30004).			%% 抱歉，不能邀请自己
-define(ERR_TEAM_NOT_TEAM_SCENE,			30005).			%% 被邀请者等级不足，不能进入此副本
-define(ERR_TEAM_OFFLINE,			30006).			%% 被邀请者不在线
-define(ERR_TEAM_FULL,			30007).			%% 队伍已满。不能加入
-define(ERR_TEAM_NOT_EXIST,			30008).			%% 队伍已经解散
%% 魂石系统
-define(ERR_HUNSHI_NOT_ENOUGH,			35001).			%% 魂石不足
-define(ERR_hunshi_NOT_HAVE_TO_UP,			35002).			%% 魂珠已经升级到可达到的最大等级
-define(ERR_HUNZHU_NOT_LEVELING,			35003).			%% 魂珠升级已经结束，请重新打开


%% 打坐
-define(ERR_NOT_IN_DAZUO,				49001).			%% 用户不在打坐状态
%% 挂机
-define(ERR_GUAJI_TIMES_ZERO,			49002).			%% 挂机次数为0
%% 英雄塔
-define(ERR_RESET_TIMES_ZERO,			49003).			%% 重置次数为0
-define(ERR_ACHIEVE_FLOOR_NOTENOUGHT,	49004).			%% 通关层数不够
-define(ERR_NOTENOUGHT_POINT,			49005).			%% 积分不够
-define(ERR_KING_BUSY,					49006).			%% 霸主被人挑战中
-define(ERR_HAVE_BEEN_KING,				49007).			%% 已经是霸主身份,只能挑战更高层霸主
-define(ERR_CHALLENGE_TIMEOUT,			49008).			%% 挑战超时
-define(ERR_IS_THE_REST_LEVEL,			49009).			%% 已重置到最底层
-define(ERR_CANT_CHANGLE_SELF,			49010).			%% 不能挑战自己
-define(ERR_OUTO_CHALLENGING,			49011).			%% 您正处于战神塔自动挂机中
-define(ERR_ACHIEVE_FLOOR_HIGHTER,		49012). 		%% 未通关的关卡不能挂机
-define(ERR_NO_AUTO_CHALLENGE,			49013).			%% 没有自动挑战任务
-define(ERR_AUTO_GUAJI_FINISH,			40014).			%% 战神塔自动挂机已经完成
-define(ERR_UN_REACH,					40015).			%% 到达本层第十关方可挑战本层霸主
-define(ERR_UN_MATCH,					40016).			%% 不能挑战低于最大通关层数的霸主


%% 成就
-define(ERR_ACHIEVE_AWARD_ERR,			29001).			%% 成就奖励不可领取
%%
-define(ERR_TARGET_AWARD_ERR,			34001).			%% 目标奖励不可领取

%% 好友
-define(ERR_WRONG_USER_NAME,18000).    %%错误的用户名
-define(ERR_ALREADY_FRIEND,18001).		%%已经是好友
-define(ERR_ALREADY_IN_BLACK_LIST,18002). %%已经在黑名单
-define(ERR_FRIEND_OFFLINE,18003).  		%%好友不在线
-define(ERR_PAY_TIMES_IS_FULL,18005).  		%%今日祝福次数已满
-define(ERR_ADD_FRIEND_TO_SELF,18004).  		%%加自己为好友
-define(ERR_PAYED_TIMES_IS_FULL,18006).  		%%今日好友被祝福次数已满
-define(ERR_REQUEST_ALREADY_SEND,18007).         %%今日好友被祝福次数已满
-define(ERR_INVITE_IN_BLACK,18008).         %%今日好友被祝福次数已满
-define(ERR_SELF_REACH_MAX_FRIEND_COUNT,18009).         %%您的好友数量以到达上线
-define(ERR_OTHER_REACH_MAX_FRIEND_COUNT,18010).         %%对方好友数量到达上线
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
-define(ERR_JIXING_HIGHEST,				26009). %%当前桃花运势已经是最高等级了！
-define(ERR_ROBER_LEVEL_TOO_LOWER,		26010). %%您还未到30级，不要做劫镖这么危险的事情啊
-define(ERR_ROB_TASK_ACCEPT, 26011).			%%对方是菜鸟,请高抬贵手
-define(ERR_NO_YUNBIAO_TASK_COMMIT,26012).		%%不在运镖,不能提交任务
-define(ERR_ZHUANYUN_MAX_TIMES,			26013). %%今天转运次数已经用完！

%% 种植
-define(ERR_FENGDI_WATER,                16001).        %% 浇水次数超过2次     
-define(ERR_FENGDI_REFRESH_HIGHEST,                16022).        %% 已经刷新到了最高品质
-define(ERR_FENGDI_NOT_NULL,                16023).        %% 土地不为空
-define(ERR_FENGDI_WATER_COUNT_BIG_THEN_20,                16024).        %% 超过20次
-define(ERR_FENGDI_LEVEL_NOT_ENOUGH,                16025).        %% 超过20次
-define(ERR_FENGDI_NOT_NEED_WATER,                16026).        %% 超过20次

%% 聊天
-define(ERR_CAN_NOT_CHAT,         16003). %% 您当前处于禁言状态
-define(ERR_PROHIBIT_WORD,16004).			%%含有敏感词
-define(ERR_PROHIBIT_WORD_IN_TITLE,16015).           %%含有敏感词
-define(ERR_LEVEL_NOT_ENOUGH,16016).           %%未到15级不能发言
%%竞技系统
-define(ERR_RANK_BEHIND, 31000).     %%您不能挑战后面哦
-define(ERR_RANK_YOURSELF, 31001).     %%您不能挑战自己哦
-define(ERR_NO_FIVE_CHALLENGE_TIMES, 31002). %%挑战次数没达到5次哦
-define(ERR_REACH_THE_MAX_CHALLENGE_TIME, 31003). %%已达到最大挑战次数
-define(ERR_ARENA_SYSTEM_NOT_OPEN, 31004).  %%竞技场30级开启
-define(ERR_ARENA_CHALLENGE_LIST_CHANGE, 31005).  %%可挑战列表已发生变化
-define(ERR_ARENA_RANK_NOT_ENOUGH, 31006).  %% 竞技场排名不足

%%boss
-define(ERR_SILVER_INSPIRE_OVER, 62001). %%银币鼓舞达到最大
-define(ERR_GOLD_INSPIRE_OVER, 62002). %%金币鼓舞达到最大
-define(ERR_SILVER_INSPIRE_NO_SUCCES, 62003).%%银币鼓舞没有成功=======
-define(ERR_BOSS_BATTLE_IS_NOT_OPEN, 62004).  %%世界BOSS战斗还未开启，请耐心等待
-define(ERR_BOSS_BATTLE_TOO_OFTEN, 62005).  %%战斗太过频繁，请稍候
-define(ERR_BOSS_BATTLE_ROBOT_IN, 62006).  %%使用了世界boss离线卡,无法进入

-define(ERR_FENGDI_WATER2,                16001).        %% 浇水次数超过2次    
-define(ERR_FENGDI_WATER50,                16002).        %% 浇水次数超过50次  

-define(ERR_CAN_NOT_CHAT_USER_NOT_ONLINE,  16005). 		%% 用户不在线
-define(ERR_CAN_NOT_CHAT_ERROR,         16006). 		%% 异常聊天
-define(ERR_CAN_NOT_CHAT_HORN_NOT,         16007). 		%% 小喇叭不足
-define(ERR_CAN_NOT_IN_TEAM,  16008). 					%% 您不在组队中，无法发送聊天内容


%% 军工，悬赏任务
-define(ERR_JUNGONG_TASK_HAVE_RECEIVE, 36000).			%% 请先完成当前军功任务
-define(ERR_JUNGONG_TASK_ALL_DONE,	36001).				%% 今日军功任务已做完
-define(ERR_JUNGONG_TASK_NOT_FINISH, 36002).			%% 军功任务未完成
-define(ERR_TASK_TRACE_HAVE_RECEIVE,  36003).			%% 一次只可接受一个任务
-define(ERR_TASK_TRACE_HAVE_FINIFSH,  36004).			%% 请先提交完成了的任务
-define(ERR_TASK_TRACE_NO_TASK,		 36005).			%% 今天的悬赏任务已全部完成
-define(ERR_TASK_TRACE_NO_TASK_RECEIVE,	 36006).		%% 今天的悬赏任务已经接完
-define(ERR_LEVY_JUNLIANG_MAX,	 	36007).		%% 今天军粮的征收次数已经用完


-define(ERR_COMP_ALREADY_APPLY,			37000).					%% 退出比武后不能重新进入
-define(ERR_COMP_IS_DIE,				37001).					%% 您已经阵亡，请使用还魂丹复活
-define(ERR_COMP_OTHER_IS_DIE,			37002).					%% 对方已阵亡，请等待TA复活
-define(ERR_COMP_NOT_DIE,				37003).					%% 您还未阵亡，无需使用还魂丹
-define(ERR_COMP_ALREADY_LEAVE,			37004).					%% 您已经离开比武场
-define(ERR_COMP_OTHER_ALREADY_LEAVE,	37005).					%% 对方已经离开比武场
-define(ERR_COMP_OTHER_IS_CLOAKING,		37006).					%% 对方已经隐形
-define(ERR_COMP_END_IN_ADVANCE,		37007).					%% 您所在等级段的比武已经结束
-define(ERR_COMP_ALREADY_RAISE_BANNER,	37008).					%% 已经升旗了
-define(ERR_COMP_NOT_COMP_TIME,			37009).					%% 现在不是比武活动时间，不能报名
-define(ERR_COMP_UNKNOWN,				37010).					%% 您的比武状态异常，请退出比武场重新报名
-define(ERR_COMP_MAX_DOUBLE_TIMES,		37011).					%% 双倍丹已使用完
-define(ERR_COMP_HIGH_TO_MID,			37012).					%% 由于70级以上玩家数量不足开启高级比武场，你被安排在中级比武场比赛
-define(ERR_COMP_HIGH_TO_LOW,			37013).					%% 由于70级以上玩家数量不足开启高级比武场，你被安排在初级比武场比赛
-define(ERR_COMP_MID_TO_LOW,			37014).					%% 由于50级以上玩家数量不足开启中级比武场，你被安排在初级比武场比赛

-define(ERR_DEFENCE_MONSTER_OUT_OF_RANGE, 40001).       %% 群魔乱舞怪物不在攻击距离之内
-define(ERR_DEFENCE_BATTLE_START_UP,      40002).       %% 群魔乱舞无法发起战斗
-define(ERR_DEFENCE_PENDING,              40003).       %% 群魔乱舞活动还没开始哦
-define(ERR_DEFENCE_BATTLE_FLY_MON,       40004).       %% 请先起飞再攻击飞行怪
-define(ERR_DEFENCE_BATTLE_LAND_MON,      40005).       %% 请先降落再攻击地面怪

-define(ERR_HAVE_CHALLENGER, 63001).					%% 通知客户端国王正在被其他人挑战
-define(COMBAT_POWER_TOO_LOW, 63002).					%% 玩家等级不到40级，无法挑战国王
-define(CANNOT_CHALLENGE_SELF, 63003).					%% 不能挑战自己
-define(NOT_FIRST_IN_ARENA, 63004).						%% 24点结算时不是竞技场第一名
-define(KING_CHANGE_TODAY, 63005).						%% 今天国王已经易主，明天才能挑战

%% =========================== 投壶相关 =================================
-define(ERR_GOLD_NOT_ENOUGH, 65000).					%% 投壶所需元宝不足
-define(ERR_BAG_SIZE_NOT_ENOUGH, 65001).				%% 投壶背包所需空间不足
-define(ERR_BAG_SLOT_NOT_ENOUGH, 65002).				%% 正式背包空间不足
-define(ERR_BAG_SLOT_INDEX_NOT_EXIST, 65003).			%% 所请求的位置没有物品

%% =========================== 外形装扮相关 ======================================
-define(ERR_DRESS_ALREADY_EVOKE, 			51000).		%% 坐骑已经幻化过
-define(ERR_DRESS_NOT_EVOKE,				51001).		%% 坐骑尚未幻化过
-define(ERR_DRESS_LEVEL_NOT_ENOUGH, 		51002).		%% 坐骑等级不足
-define(ERR_DRESS_ALREADY_MAX_LEVEL, 		51003).		%% 坐骑已达最高等级
-define(ERR_DRESS_OVER_BUY_NUM,				51004).		%% 一种坐骑卡只能购买一个
-define(ERR_DRESS_PENDANT_ALREADY_EQUIP,	51005).		%% 挂饰已经装备
-define(ERR_DRESS_NOT_PENDANT_HOLE,			51006).		%% 没有挂饰孔
-define(ERR_DRESS_RESOLVE_EQUIP_PENDANT,	51007).		%% 挂饰已装备，请卸下后分解
-define(ERR_DRESS_MAX_ADVANCE_LEVEL, 		51008).		%% 坐骑已达最高阶数
-define(ERR_FASHION_NOT_EQUIP,				51100).		%% 时装未装备
-define(ERR_FASHION_NOT_EVOKE,				51101).		%% 时装未获得
-define(ERR_FASHION_ALREADY_EVOKE,			51102).		%% 您已经有该时装了
-define(ERR_FASHION_OVER_BUY_NUM,			51103).		%% 一种时装只能购买一个
-define(ERR_FASHION_ROLE_NOT_MATCH,			51104).		%% 欲购买的时装不能装备
-define(ERR_FASHION_MAX_ADVANCE_LEVEL,		51105).		%% 时装已达最高阶数
-define(ERR_WING_NOT_EQUIP,					51200).		%% 翅膀未装备
-define(ERR_WING_NOT_EVOKE,					51201).		%% 翅膀未获得
-define(ERR_WING_OVER_BUY_NUM,				51202).		%% 一种翅膀只能购买一个
-define(ERR_WING_ALREADY_EVOKE,				51203).		%% 您已经有该翅膀了




%%  ===========跨服系统============
-define(ERR_CROSS_QUICK_JOIN_TIMEOUT, 57001).
-define(ERR_CROSS_MATCH_TEAM_FAILED, 57002).
-define(ERR_CROSS_FIRE, 57003).
-define(ERR_CROSS_TEAM_MATE_LEAVE, 57004).
-define(ERR_CROSS_NOT_HAVE_FIT_TEAM, 57005).
-define(ERR_CROSS_LEVEL_NOT_ENOUGH, 57006).
-define(ERR_CROSS_COMBAT_NOT_ENOUGH, 57007).
-define(ERR_CROSS_COUNT_FULL, 57008).
-define(ERR_CROSS_STATE_YUNBIAO, 57009).
-define(ERR_CROSS_STATE_BATTLE, 57010).
-define(ERR_CROSS_STATE_NOT_OK_SCENE, 57011).
-define(ERR_CROSS_STATE_IN_TEAM, 57012).
-define(ERR_CROSS_STATE_IN_COMP, 57013).
-define(ERR_CROSS_MATE_CAN_NOT_START, 57014).
-define(ERR_CROSS_IN_TEAM, 57015).
-define(ERR_CROSS_MATCH_TEAM_TIME_OUT, 57016).
-define(ERR_CROSS_MATE_GIVE_UP, 57017).
-define(ERR_CROSS_MATE_TIME_NOT_OK, 57018).
-define(ERR_CROSS_MATE_CROSS, 57019).
-define(ERR_CROSS_NO_AWARD, 57020).


-define(ERROR_INTERNAL_NOT_REG_SYSTEM_ID, 65500).   %% 内部错误，有未被识别的系统id

%% ============================ 礼包相关错误码 ===============================
-define(ERR_GIFT_HAVE_RECEIVE_NEW_CARD,	41000).		%% 该玩家已经领取过新手卡
-define(ERR_GIFT_NEW_CARD_DATA_ERR,		41001).		%% 新手卡数据错误
-define(ERR_GIFT_NETWORD_ERR,			41002).		%% 网络通讯错误
-define(ERR_GIFT_NOT_NULL_BAG_SLOT,		41003).		%% 背包没有多余空间
-define(ERR_GIFT_PHONE_BINDING_RECEIVED,41004).		%% 已经领了手机绑定礼包
-define(ERR_GIFT_INVALID_KEY,           41005).		%% 无效Key

%% ========================== 挑战魂将相关错误码 ==============================
-define(ERR_STAGE_IN_SCENE,				43000).		%% 正在挑战魂将，无法重置
-define(ERR_STAGE_TODAY_HAVE_RESET,		43001).		%% 今天重置次数超出限制
-define(ERR_STAGE_GOLD_NOT_ENOUGH,		43002).		%% 元宝不足
-define(ERR_STAGE_DATA_TIMEOUT,			43003).		%% 数据超时
-define(ERR_STAGE_LEVEL_NOT_REACH,		43004).		%% 未到达开启等级
-define(ERR_STAGE_NOT_IN_SCENE,			43005).		%% 不在指定场景中，无法跳转
-define(ERR_STAGE_FLOOR_NOT_OPEN,		43006).		%% 该层尚未开启
-define(ERR_STAGE_FLOOR_OUT_OF_RANGE,	43007).		%% 超出可跳转范围
-define(ERR_STAGE_NOT_EXISTS_RESET_FLOOR,	43008).	%% 没有需要重置的关卡
-define(ERR_STAGE_HAS_OTHER_MONSTER,	43009).		%% 请先清完小怪再攻击boss吧！

%% ========================== 切磋相关错误码 ==============================
-define(ERR_LEVEL_TOO_HIGH,				43100).		%% 对方等级过低，不可以欺凌弱小哦！
-define(ERR_LEVEL_TOO_LOW,				43101).		%% 对方还是新手，请手下留情！
-define(ERR_NOT_IN_SAME_SCENE,			43102).		%% 对方已经离开了！
-define(ERR_REJECT_YOUR_REQ,			43103).		%% 对方拒绝了你的切磋请求
-define(ERR_NO_REPLY,					43104).		%% 对方无回复
-define(ERR_OFF_LINE,					43105).		%% 玩家不在线
-define(ERR_IS_IN_BATTLE,				43106).		%% 玩家正在和其他人切磋
-define(ERR_SOMEONE_IN_TEAM, 			43107).		%% 您或对方正处于组队状态，不能进行切磋
-define(ERR_HAS_REQUEST, 			    43108).		%% 您的请求已发出，请耐心等待对方回复
-define(ERR_HAS_TIMEOUT, 			    43109).		%% 您的操作已超时
-define(ERR_CANNOT_PK_SCENE, 			43110).		%% 当前场景不能进行切磋

%% =========================== 在线奖励相关错误码 ===============================
-define(ERR_ONLINE_AWARD_TIME,			44000).		%% 在线奖励领取还没到时间领取
-define(ERR_ONLINE_AWARD_MAX,			44001).		%% 今天在线奖励已经全部领完

%% =========================== 称号系统相关错误码 ===============================
-define(ERR_TITLE_NOT_ACHIEVE,			52000).		%% 称号未达成


%% =========================== 冲榜活动 ====================================
-define(ERR_RUSH_GET_AWARD_ALREADY,		36201).    %%已经领取
-define(ERR_RUSH_GET_NOT_OK,			36202).    %% 未达成 
-define(ERR_RUSH_IS_OVER,				36203).    %% 开服活动已经结束

%% ======================== 领地战相关错误码 ===============================
-define(ERR_TERRITORY_WAR_NOT_OPEN,				45000).		%% 领地战未开启
-define(ERR_TERRITORY_WAR_GUILD_CAN_NOT_JOIN,	45001).		%% 玩家所在公会无法参加领地战
-define(ERR_TERRITORY_WAR_ALREADY_IN_SCENE,		45002).		%% 玩家已经在场景中
-define(ERR_TERRITORY_WAR_NOT_IN_SCENE,			45003).		%% 玩家不在场景中
-define(ERR_TERRITORY_WAR_HAVE_START,			45004).		%% 领地战已经开始，无法进入
-define(ERR_TERRITORY_WAR_NO_BATTLE_ROLE,		45005).		%% 玩家没有可出战佣兵
-define(ERR_TERRITORY_WAR_PLAYER_DATA_NOT_EXIST,45006).		%% 邀请者数据不存在
-define(ERR_TERRITORY_WAR_HAVE_A_TEAM,			45007).		%% 邀请者已经有队伍
-define(ERR_TERRITORY_WAR_INVITED_DATA_NOT_EXIST,45008).	%% 被邀请者数据不存在
-define(ERR_TERRITORY_WAR_INVITED_HAVE_A_TEAM,	45009).		%% 被邀请者已经有队伍
-define(ERR_TERRITORY_WAR_NOT_IN_ONE_GUILD,		45010).		%% 双方不在同一个公会
-define(ERR_TERRITORY_WAR_DATA_NOT_EXIST,		45011).		%% 玩家数据不存在
-define(ERR_TERRITORY_WAR_SOMEONE_NOT_IN_SCENE,	45012).		%% 你或你的队友不在场景中
-define(ERR_TERRITORY_WAR_HAVE_NO_TEAM,			45013).		%% 队伍不存在
-define(ERR_TERRITORY_WAR_TOWER_NOT_EXIST,		45014).		%% 塔不存在
-define(ERR_TERRITORY_WAR_TOWER_HAVE_DAMAGE,	45015).		%% 塔已经被破坏
-define(ERR_TERRITORY_WAR_TOWER_CAN_NOT_ATT,	45016).		%% 塔是无敌状态，无法被攻击
-define(ERR_TERRITORY_WAR_IN_ATT_TOWER,			45017).		%% 已经在攻击塔
-define(ERR_TERRITORY_WAR_CAMP_DENY,			45018).		%% 所在阵营无法进行该操作
-define(ERR_TERRITORY_WAR_NOT_LEADER,			45019).		%% 不是队长，无法进行该操作
-define(ERR_TERRITORY_WAR_SAME_CAMP,			45020).		%% 相同阵营，无法攻击
-define(ERR_TERRITORY_WAR_NOT_ATT_TOWER,		45021).		%% 不处在攻击塔状态
-define(ERR_TERRITORY_WAR_BAG_NO_NULL_SLOT,		45022).		%% 道具背包已满
-define(ERR_TERRITORY_WAR_PROP_NOT_EXIST,		45023).		%% 道具已被其它人拾取
-define(ERR_TERRITORY_WAR_USE_PROP_NOT_EXIST,	45024).		%% 使用道具不存在
-define(ERR_TERRITORY_WAR_CAN_NOT_CREATE_TEAM_IN_BATTLE,	45025).		%% 战斗中不能组队
-define(ERR_TERRITORY_WAR_IN_YUNBIAO,			45026).		%% 玩家在运镖中无法进入领地战
-define(ERR_TERRITORY_WAR_PLAYER_NO_GUILD,		45027).		%% 请先加入公会
-define(ERR_TERRITORY_WAR_CD_TIME,				45028).		%% CD时间中
-define(ERR_TERRITORY_WAR_BATTLE_CD,            45029).		%% 

-define(ERR_ZHENFA_LEVEL_FULL,        48000).         %% 阵法等级已到达最高级

%% --------------------------------将魂----------------------------------
-define(ERR_HERO_SOUL_BAG_FULL,		      50000).		%% 将魂包裹已满，无法进行拾取或卸除操作
-define(ERR_HERO_SOUL_ITEMS_FULL,		  50001).		%% 位置已满，无法炼魂，请拾取后继续
-define(ERR_HERO_SOUL_NOT_EXIST,		  50002).		%% 炼魂配置信息不存在
-define(ERR_HERO_SOUL_GARBAGE,		      50003).		%% 没有废品可以出售
-define(ERR_HERO_SOUL_NO_CELL,		      50004).		%% 无空位，无法装备
-define(ERR_HERO_SOUL_CANT_PICK,		  50005).		%% 无可拾取的将魂
-define(ERR_HERO_SOUL_HAS_PUT_ON,		  50006).		%% 已装备同类型的将魂，请先卸除再进行装备
-define(ERR_HERO_SOUL_CANT_MERGE,		  50007).		%% 没有可融合的将魂
-define(ERR_HERO_SOUL_CANT_UPDATE,		  50008).		%% 已经升级到最高级，无法继续融合
-define(ERR_HERO_SOUL_CANT_LOCK_MERGE,	  50009).		%% 锁定的将魂不允许参与融合
-define(ERR_HERO_SOUL_NOT_ENOUGH_SCORE,	  50010).		%% 炼魂积分不足

%% --------------------------------藏宝洞----------------------------------
-define(ERR_CAVE_TIMES_OVER,		  38201).		%% 你今日藏宝洞次数已用完
-define(ERR_CAVE_LIVES_OVER,		  38202).		%% 复活次数已用完
-define(ERR_CAVE_INSPIRE_MAX,		  38203).		%% 鼓舞已达到最大值
-define(ERR_CAVE_MATE_TIMES_OVER,	  38204).		%% 队友今日藏宝洞次数已用完
-define(ERR_CAVE_IN_GUAJI,			  38205).		%% 藏宝洞自动挂机中
-define(ERR_CAVE_GUAJI_CLOSE,		  38206).		%% 挂机功能于通关15层之后开启
-define(ERR_CAVE_GUAJI_FINISH,		  38207).		%% 藏宝洞自动挂机完成！
-define(ERR_CAVE_NOT_IN_GUAJI,		  38208).		%% 没有藏宝洞的挂机任务
-define(ERR_CAVE_GUAJI_LEVEL_WRONG,	  38209).		%% 挂机必须从第一层开始
-define(ERR_CAVE_MATE_IN_GUAJI,		  38210).		%% 队友藏宝洞自动挂机中
-define(ERR_CAVE_NOT_FIRST,			  38211).		%% 不在第一层，无法组队挑战
-define(ERR_CAVE_MATE_NOT_FIRST,	  38212).		%% 队友不在第一层，无法组队挑战
-define(ERR_CAVE_BUY_TIMES_OVER,	  38213).		%% 藏宝洞购买次数已用完
-define(ERR_DUNGEON_IS_IN_GUAJI_2,	  38214).		%% 正在进行副本挂机，无法同时开启多个挂机功能
-define(ERR_MARSTOWER_IS_IN_GUAJI_2,  38215).		%% 正在进行战神塔挂机，无法同时开启多个挂机功能
-define(ERR_CAVE_IS_IN_GUAJI_2,	  	  38216).		%% 正在进行藏宝洞挂机，无法同时开启多个挂机功能

%% --------------------------------节日----------------------------------
-define(ERR_QINGMING_ADD_BUFF,		  54100).		%% 祈雨成功提示
-define(ERR_FEST_POINTS_NOT_ENOUGH,   54101).       %% 节日积分不足
-define(ERR_NOT_USE_IN_THIS_SCENE,	   54102).		%% 此物品只可在精英虎牢关中使用
-define(ERR_HAS_BEEN_IN_BATTLE,	   		54103).		%% 您正在参与战斗，不能继续使用此物品
-define(ERR_CAN_ONLY_USE_BY_LEAD,	   	54104).		%% 只有队长方可使用

%% --------------------------------送花----------------------------------
-define(ERR_CANT_SEND_SELF,		  53000).		%% 您不可送花给自己

%% --------------------------------活动抽奖----------------------------------
-define(ERR_NOT_ENOUGH_ITEMS,		  54200).		%% 你没有美酒可用来祭奠英雄

