
-define(CACHE_CHUANQI, cache_util:get_register_name(chuanqi)).
-record
(
   chuanqi,
    {
     chuanqiId = 0,
     playerId = 0
     }
).
-record
(
   chuanqi_types,
   {
        chuanqiId = {integer},
        playerId = {integer}
    }
).
