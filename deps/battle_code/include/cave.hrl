

-define(CAVE_MAP_ID,				3900).	%% 藏宝洞地图ID
-define(CAVE_MAX_BUY_TIMES,			1).		%% 每天可购买次数

-record(cave,{
			gd_account_id       = 0,
			gd_achieve          = 0,
			gd_current          = 0,
			gd_last_time        = 0,
			gd_res_times        = 0,
			gd_buy_times		= 0,
			gd_inspire          = 0,
			gd_lives            = 0,
			gd_achieve_time     = 0,
			gd_mon_list         = [],
			gd_guaji_times      = 0,
			gd_finish_time      = 0,
			gd_guaji_start_time = 0,
			gd_guaji_award_info = [],
			gd_award_info       = [],
			gd_diff				= 1
	}).

-record(cave_types,{
			gd_account_id       = {integer},
			gd_achieve          = {integer},
			gd_current          = {integer},
			gd_last_time        = {integer},
			gd_res_times        = {integer},
			gd_buy_times		= {integer},
			gd_inspire          = {integer},
			gd_lives            = {integer},
			gd_achieve_time     = {integer},
			gd_mon_list         = {term},
			gd_guaji_times      = {integer},
			gd_finish_time      = {integer},
			gd_guaji_start_time = {integer},
			gd_guaji_award_info = {term},
			gd_award_info       = {term},
			gd_diff				= {integer}
			}).