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
-define(ANNOUNCE_PET,			9).
-define(ANNOUNCE_OFFLINE_ARENA,	10).
-define(ANNOUNCE_HORN,			11).
-define(ANNOUNCE_OFFLINE_ARENA_FIRST_RANKS,12).
-define(ANNOUNCE_BOXING_HOST_CHANGES,	13).
-define(ANNOUNCE_FLOWER,		14).
-define(ANNOUNCE_GUILD_DONATE,	15).
-define(ANNOUNCE_FIRST_CHARGE_REWARD,	16).
-define(ANNOUNCE_WORLD_BOSS_RANK,		17).
-define(ANNOUNCE_NEW_CARD,		18).
-define(ANNOUNCE_EXCHANGE_CARD,		    19).
-define(ANNOUNCE_WORLD_BOSS_KILLER,		20).
-define(SEND_ARENA_NO1_CHANGE,23).
-define(ADD_DELETE_FRIEND, 21).
-define(SEND_ITEM_QUALIY, 22).
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
-define(SEND_TIANFU, 33).
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
-define(SEND_VIP, 55).
-define(SEND_COMP_FIRST_BLOOD, 56).
-define(SEND_COMP_CON_WIN, 57).
-define(SEND_COMP_STOP_CON_WIN, 58).
-define(SEND_COMP_RAISE_BANNER, 59).
-define(SEND_COMP_TOP_RANK, 60).
-define(SEND_TEAM_RECRUIT, 61).
-define(SEND_SOUL_BALL, 62).
-define(SEND_WEALTH, 63).


-define(CARD_FROM_RESURRECTION, 1).
-define(CARD_FROM_HUNTING,      2).

-record(player_event, 
    {
        type       = none :: none | arena | garden | boxing,
        content    = {}   :: any()
    }).

-endif.

