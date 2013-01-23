

-define(CACHE_RUSH_RANK, cache_util:get_register_name(record_rush_rank)).
-define(RUSH_RANK_NOT_OK, 0). %%未达成
-define(RUSH_RANK_OK, 1). %% 已达成未领取
-define(RUSH_RANK_GETED, 2). %% 已领取
-define(RUSH_RANK_CAN_NOT_OK, -1). %%不可能完成

-record(record_rush_rank,
        {
         key = {0,0},
         state = 0, %% 0未完成，1以完成待领取。 2已领取 -1没完成，以后也不可能完成
         rate = 0  %% 完成率，比如收集5颗6级宝石达成任务，这个值可能是1,2,3,4,5
        }).

-record(record_rush_rank_types,
        {
         key = {{integer},{integer}},
         state = {integer}, %% 0未完成，1以完成待领取。 2已领取  , -1没完成，以后也不可能完成
         rate = {integer}
        }).