%% Author: Administrator
%% Created: 2012-6-11
%% Description: TODO: Add description to pet_extend

-define(CACHE_BAG_REF, cache_util:get_register_name(bag)).
-define(CACHE_ITEM_REF, cache_util:get_register_name(item)).
-define(CACHE_MYSTICAL_SHOP_REF, cache_util:get_register_name(mystical_shop)).

-define(BAG_TYPE_BAG,				1).
-define(BAG_TYPE_BANK,				2).
-define(BAG_TYPE_ROLE,				3).
-define(BAG_TYPE_SHOP,				4).				   %% 物品回购的临时背包

-define(SHOP_SAVE,					8).				   %% 商店保留的回购物品数量

-define(ITEM_BIND_N,				0).
-define(ITEM_BIND_Y,				1).

-define(ITEM_THROW_N,				0).
-define(ITEM_THROW_Y,				1).

-define(ITEM_USE_N,					0).
-define(ITEM_USE_Y,					1).

%% 物品大类型
-define(ITEM_TYPE_EQUIP,			1).					%% 装备
-define(ITEM_TYPE_STONE,			2).					%% 宝石
-define(ITEM_TYPE_PROP,				3).					%% 道具

%% 物品第二类型
-define(EQUIP_TYPE_WEAPON,			1).					%% 武器
-define(EQUIP_TYPE_ARMORS,			2).					%% 防具
-define(EQUIP_TYPE_CLOAK,			3).					%% 披风
-define(EQUIP_TYPE_SHOES,			4).					%% 鞋子
-define(EQUIP_TYPE_RING,			5).					%% 戒指
-define(EQUIP_TYPE_WING,			7).					%% 翅膀
-define(EQUIP_TYPE_FASHION,			8).					%% 时装
-define(EQUIP_TYPE_FRAGMENT,		10).				%% 装备碎片

%% 材料小类型
-define(STONE_TYPE_INT_STONE,		1).					%% 智力宝石
-define(STONE_TYPE_BRAWN_STONE,		2).					%% 腕力宝石
-define(STONE_TYPE_PHYSIQUE_STONE,	3).					%% 体格宝石
-define(STONE_TYPE_GODHEAD_STONE,	4).					%% 神格宝石
-define(STONE_TYPE_FRAGMENT,		10).				%% 宝石碎片

%% 道具小类型
-define(PROP_TYPE_RUNE,			1).						%% 保护符
-define(PROP_TYPE_GIFTBAG,		2).						%% 礼包
-define(PROP_TYPE_SILVER_BILL,	3).						%% 银票票据
-define(PROP_TYPE_GOLD_BILL,	4).						%% 金票票据
-define(PROP_TYPE_HORN,			5).						%% 小喇叭
-define(PROP_TYPE_FLY_SHOES,	6).						%% 小飞鞋
-define(PROP_TYPE_TASK,			10).					%% 任务道具
-define(PROP_TYPE_CHANGE_CARD,	26).					%% 变身卡

%%宝石操作协议
-define(ONE_STONE_ONCE, 0).
-define(ALL_STONE_ONCE, 1).

-record(item, {
			   key = {0, 0},
			   cfg_ItemID = 0,
			   gd_RoleID = 0,
			   gd_BagType = 0,
			   gd_BagPos = 0,
			   gd_StackNum = 0,
			   gd_IntensifyLevel = 0,
			   gd_IntenRate = 0,
			   gd_RemouldLevel = 0,
			   gd_IsBind = 0,
			   gd_InlayInfo = [],
			   gd_IdentifyInfo = [],
			   gd_CreateTime = 0,
			   gd_EndTime = 0,
			   gd_LuckyStar = 0,
			   gd_XilianInfo = [],
			   gd_Quality = 1,
			   gd_IsQiling = 0,
			   gd_IntenFailRate = 0
			  }).

-record(item_types, {
					 key = {{integer}, {integer}},
					 cfg_ItemID = {integer},
					 gd_RoleID = {integer},
					 gd_BagType = {integer},
					 gd_BagPos = {integer},
					 gd_StackNum = {integer},
					 gd_IntensifyLevel = {integer},
					 gd_IntenRate = {integer},
					 gd_RemouldLevel = {integer},
					 gd_IsBind = {integer},
					 gd_InlayInfo = {term},
					 gd_IdentifyInfo = {term},
					 gd_CreateTime = {integer},
					 gd_EndTime = {integer},
					 gd_LuckyStar = {integer},
			   		 gd_XilianInfo = {term},
			   		 gd_Quality = {integer},
			         gd_IsQiling = {integer},
			         gd_IntenFailRate = {integer}
					}).

-record(bag, {
				  key = {0, 0},
				  gd_CurrNum = 0,
				  gd_NullNumList = []
				  }).

-record(bag_types, {
						key = {{integer}, {integer}},
						gd_CurrNum = {integer},
						gd_NullNumList = {term}
					   }).

-record(attr_info, {
					}).

-record(cfg_item, {
				   cfg_ItemID = 0,
				   cfg_Name = "",
				   cfg_FirstType = 0,
				   cfg_SecondType = 0,  %%装备位置
				   cfg_GradeLevel = 0,      
				   cfg_Career = 0,          
				   cfg_RoleLevel = 0,
				   cfg_Wakan,
				   cfg_BuyGold = 0,
				   cfg_BuySilver = 0,
				   cfg_SellSilver = 0,
				   cfg_IsThrow = 0, 
				   cfg_IsUse = 0,
				   cfg_IsInten = 0,
				   cfg_IsAnnouncement = 0,
				   cfg_IntenRate = 0,
				   cfg_StackMax = 0,
				   cfg_ComposeNum = 0,
				   cfg_ComposeID = 0,
				   cfg_PiecesNum = 0,
				   cfg_FullID = 0,
				   cfg_SuitID = 0,
				   cfg_AttrInfo = #attr_info{},
				   cfg_UseEffect = [],
				   cfg_IsXilian = 1,
				   cfg_IsQiling = 1,
				   cfg_IsUpgrate = 1,
				   cfg_IsUpquality = 1,
				   cfg_Sort = 1
			  }).

%% 神秘商店
-record(mystical_shop, {
						gd_AccountID     = 0,  		    %% 账号ID
						gd_LastFreshTime = 0,      		%% 上次刷新时间
						gd_FreeTimes     = 2,			%% 免费次数
						gd_ItemList      = []      		%% 物品销售列表
					   }).

-record(mystical_shop_types, {
							  gd_AccountID     = {integer},
							  gd_LastFreshTime = {integer},
							  gd_FreeTimes     = {integer},
							  gd_ItemList      = {term}
							 }).

-record(market,{
				gd_id = 0,		%% 抢购的物品
				gd_num = 0		%% 剩余数量
				}).

-record(market_types,{
					gd_id = {integer},		%% 抢购的物品
					gd_num = {integer}		%% 剩余数量
					}).

-record(market_account,{
						key = {0,0},
						gd_num = 0
						}).

-record(market_account_types,{
							key = {{integer},{integer}},
							gd_num = {integer}
							}).

-define(MYSTICAL_SHOP_OPEN_LEVEL, 30).
-define(MYSTICAL_SHOP_FRESH_TIME, 21600).