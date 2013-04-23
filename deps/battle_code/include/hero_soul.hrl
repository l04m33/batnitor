-define(PAGE_ZERO_MAX_ITEM_POS,8).
-define(PAGE_ONE_MAX_ITEM_POS,16).
-define(PAGE_TWO_MAX_ITEM_POS,16).
-define(NUM_OF_BAG_OPEN_AT_FIRST,data_hero_soul:get_open_bag_at_first()).

-record (hero_soul, {
	gd_AccountID     = 0,		%% 主键：玩家id
	gd_BagList       = [{0,?NUM_OF_BAG_OPEN_AT_FIRST}], %% {merid（0-为背包，其他为武将ID,BagNum（开启的格子数）}
	gd_LastTime 	 = 0,		%% 上次炼魂时间
	gd_FreeTimes 	 = 0,		%% 剩余免费次数
	gd_FlamePos      = 1,		%% 火焰位置
	gd_ItemList 	 = []		%% 将魂列表
	}).

-record (hero_soul_types, {
	gd_AccountID     = {integer},
	gd_BagList       = {term},
	gd_LastTime 	 = {integer},
	gd_FreeTimes 	 = {integer},
	gd_FlamePos      = {integer},
	gd_ItemList 	 = {term}
	}).

-record (soul, {
	mer_id       = 0,
	add_attri    = 0,
	is_locked    = 0,
	soul_id      = 0,
	soul_level   = 1,
	soul_quality = 0,
	soul_exp     = 0,
	soul_page    = 2,%(炼魂页面为第二页，默认)
	soul_pos     = 0
	}).
