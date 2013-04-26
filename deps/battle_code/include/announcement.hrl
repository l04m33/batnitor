-ifndef(__ANNOUNCEMENT_HRL__).
-define(__ANNOUNCEMENT_HRL__, 1).

-include("types.hrl").

-define(ANNOUNCE_ACHIEVEMENT, 	1).
-define(ANNOUNCE_RUN_BUSINESS,	2).
-define(ANNOUNCE_ROB_SUCCESS,	3).
-define(ANNOUNCE_ROB_FAIL,		4).
-define(ANNOUNCE_PURPLE_ITEM,	5).
-define(ANNOUNCE_ONLINE_ARENA,	6).
-define(ANNOUNCE_ONLINE_AWARD,	7).
-define(ANNOUNCE_GUILD,			8).
-define(ANNOUNCE_OFFLINE_ARENA,	10).
-define(ANNOUNCE_OFFLINE_ARENA_FIRST_RANKS,12).
-define(ANNOUNCE_FLOWER,		14).
-define(ANNOUNCE_WORLD_BOSS_KILLER,		20).
-define(SEND_ORANGE_CAR, 23).
-define(SEND_LUCKY_STRAR, 24).
-define(SEND_TOWER_KING, 25).
-define(SEND_XUNXIAN, 26).
-define(SEND_ACHIEVEMENT, 27).
-define(Fight_NO1_LOGIN, 28).
-define(SEND_QINMI_99, 29).
-define(SEND_SHENQI_UP,30).
-define(SEND_GUANZHI_UP, 31).
-define(SEND_WIN_FLOOR, 32).
-define(SEND_WORLD_BOSS_INSPIRE, 34).
-define(SEND_ITEM_STRONG, 35).
-define(SEND_QUALITY_UP, 36).
-define(SEND_LEVEL_UP, 37).
-define(SEND_ITEM_CREATE_TOUHU, 41).
-define(SEND_ITEM_CREATE_TOWER, 42).
-define(SEND_ITEM_CREATE_SOUL, 43).
-define(SEND_ITEM_CREATE_FUBEN, 44).
-define(SEND_ITEM_CREATE_MONSTER, 45).
-define(SEND_ITEM_CREATE_LIBAO, 46).
-define(SEND_ITEM_CREATE_SHOUCHONG, 47).
-define(SEND_ITEM_CREATE_WORLD_BOSS, 48).
-define(SEND_ITEM_CREATE_KING, 49).
-define(SEND_ITEM_CREATE, 50).
-define(SEND_ITEM_CREATE_WISD, 53).
-define(SEND_ITEM_CREATE_BOSS, 54).
-define(SEND_ITEM_CREATE_DEFENCE, 40).
-define(SEND_VIP, 55).
-define(SEND_COMP_FIRST_BLOOD, 56).
-define(SEND_COMP_CON_WIN, 57).
-define(SEND_COMP_STOP_CON_WIN, 58).
-define(SEND_COMP_RAISE_BANNER, 59).
-define(SEND_COMP_TOP_RANK, 60).
-define(SEND_TEAM_RECRUIT, 61).
-define(SEND_SOUL_BALL, 62).
-define(SEND_WEALTH, 63).
-define(SEND_GUARD_FOR_KING_RANK, 64).
-define(SEND_COMP_REMOVE_BANNER, 65).
-define(SEND_TOUHU, 66).
-define(SEND_SWORD, 67).
-define(SEND_HORSE_UP_LEVEL, 68).
-define(SEND_SHENJIAN_WIN, 69).
-define(SEND_SHENJIAN_LOSE, 70).
-define(SEND_ITEM_CREATE_SWORD, 71).
-define(SEND_ITEM_CREATE_MYSTICA, 72).
-define(SEND_HORSE_CHANGE, 73).%%坐骑幻化
-define(SEND_VIP_UP, 74).
-define(SEND_KING_LOGIN, 75).
-define(SEND_GUILD_RECRUIT, 76).
-define(SEND_TAOHUAYUN, 77).
-define(SEND_STAGE_BEST_KILL, 78).
-define(SEND_JINGJIE, 79).
-define(SEND_CROSS_WIN_HIGH_RANK, 80).
-define(SEND_CROSS_WIN_ALL, 81).
-define(SEND_HUNT_SOUL, 82).
-define(SEND_ITEM_CREATE_CHANGE, 83).
-define(SEND_ITEM_CREATE_FIRST_VIP, 84).
-define(SEND_ITEM_CREATE_FIRST_LUCKY, 85).
-define(SEND_ITEM_CREATE_MARKET_2, 86).
-define(SEND_SHOP_FRESH, 87).

-define(CARD_FROM_RESURRECTION, 1).
-define(CARD_FROM_HUNTING,      2).

-define(ANNOUNCE_TYPE_CHAT_WINDOW,      1).
-define(ANNOUNCE_TYPE_TOP_BANNER,       2).
-define(ANNOUNCE_TYPE_POP_UP_WINDOW,    3).

-record(player_event, 
    {
        type       = none :: none | arena | garden | boxing,
        content    = {}   :: any()
    }).

-endif.

