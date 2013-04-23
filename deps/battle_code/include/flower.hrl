-ifndef(__FLOWER_HRL__).
-define(__FLOWER_HRL__, 1).

%int8:    0-赠送成功  1-非好友   2-不在线或不存在    3-黑名单   4-回吻
-define(SEND_FLOWER_SUCCESS,0).
-define(NOT_A_FRIEND,1).
-define(USER_OFFLINE_OR_NOT_EXIST,2).
-define(IN_BLACK_LIST,3).
-define(RETURN_KISS,4).

-define(NO,0).
-define(YES,1).

-define(SEND,1).
-define(RECV,2).

-define(NINETY_NINE_EFFECT,0).
-define(NINE_HUNDRED_AND_NINETY_NINE_EFFECT, 1).

-define(MAX_POPULARITY_FROM_FLOWER, 5200).

-define(IS_ANNOUNCE_FLOWER,	data_system:get(75)).

-record(flower_info,
    {
        num,
        item_id,
        popularity,
        familiar
    }).

-endif.
