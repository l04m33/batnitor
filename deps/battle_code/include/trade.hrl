
-record(trade,{
			id        = 0,
			accountid = 0,
			item_name = "",
			account_name = "",
			level	  = 0,
			quality	  = 0,
			cfg       = 0,
			num       = 0,
			price     = 0,
			end_time  = 0
	}).

-record(trade_types,{
			id        = {integer},
			accountid = {integer},
			item_name = {term},
			account_name = {term},
			level	  = {integer},
			quality	  = {integer},
			cfg       = {integer},
			num       = {integer},
			price     = {integer},
			end_time  = {integer}
	}).

-record(trade_filter,{
			type1 = 0,
			type2 = 0,
			level = 0,
			quality = 0,
			price_top = 0,
			price_down = 0,
			item_name = "",
			account_name = "",
			accountid = 0
	}).
