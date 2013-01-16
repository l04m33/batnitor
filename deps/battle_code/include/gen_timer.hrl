-define(CACHE_GEN_TIMER, cache_util:get_register_name(cache_gen_timer)).

%% add_timer(GenTimerRef, Timeout, UserKey, UserData) ->
-record
(
   cache_gen_timer,
   {
        key = {undefined, undefined}, %% genTimerRef,userKey
		timeout_time = 0, 		%% 超时 时间点
		userData = undefined %% timer的数据
	}
).
-record
(
   cache_gen_timer_types,
   {
        key = {{term}, {term}},
        timeout_time = {integer},       %% 超时 时间点
        userData = {term} %% timer的数据
    }
).