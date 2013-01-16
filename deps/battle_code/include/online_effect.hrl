-define(CACHE_ONLINE_EFFECT, cache_util:get_register_name(online_effect)).
-define(ONLINE_EFFECT_TYPE_CHANGE,			1). %% 变身卡类型
-define(ONLINE_EFFECT_TYPE_ONLINE_AWARD,	2). %% 在线奖励

-define(IS_CANCEL_N, 						0).
-define(IS_CANCEL_Y, 						1).

-record(online_effect, {
						 key           = {0, 0},			%% {AccountID, Type}
						 gd_RemainTime = 0,
						 gd_OtherInfo  = 0,
						 gd_CBModule   = none,
						 gd_CBFunction = none,	
						 gd_CBArgs     = [],
						 gd_CBIsCancel = 0
						}
		).

-record(online_effect_types, {
							   key           = {{integer}, {integer}},
							   gd_RemainTime = {integer},
							   gd_OtherInfo  = {integer},
							   gd_CBModule   = {term},
							   gd_CBFunction = {term},	
						 	   gd_CBArgs     = {term},
						 	   gd_CBIsCanceL = {integer}
						}
		).

-record(online_effect_args, {
							 account_id  = 0,
							 type        = 0,
							 remain_time = 0,
							 other_info  = 0,
							 module      = none,
							 function    = none,
							 args        = [],
							 is_cancel   = 0
							 }
		).