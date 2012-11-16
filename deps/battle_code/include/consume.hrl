
-record (consume, {
	gd_accountID = 0,
	gd_progress = 0,
	gd_level = 1,
	gd_box = 0,
	gd_lastTime = 0
	}).

-record (consume_types, {
	gd_accountID = {integer},
	gd_progress = {integer},
	gd_level = {integer},
	gd_box = {integer},
	gd_lastTime = {integer}
	}).