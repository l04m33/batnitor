-ifndef(__FEST_HRL__).
-define(__FEST_HRL__, true).

-define(FEST_QINGMING,          1).
-define(FEST_QINGMING_CHARGE,   2).
-define(FEST_MAY,   3).

-define(FEST_TAQING_SEED_LIMIT, 10).
-define(FEST_TAQING_SEED_GIFT_ITEMID, 707).
-define(FEST_TAQING_SEED_ITEMID, 708).

-define(FEST_REWARD_CACHE_REF, cache_util:get_register_name(fest_reward_stat)).

-define(FEST_REWARD_TYPE_CHARGE, 1).

-define(FEST_REWARD_SEIZED, 2).
-define(FEST_REWARD_CAN_SEIZE, 1).
-define(FEST_REWARD_CANNOT_SEIZE, 0).

-record(fest_reward_stat, {
    key = {0, 0, 0},
    gd_stat = [],
    gd_time = 0}).

-record(fest_reward_stat_types, {
    key = {{integer}, {integer}, {integer}},
    gd_stat = {term},
    gd_time = {integer}}).

-endif.

