
-record(vip,{
	gd_AccountID = 0,
	gd_Level = 0,
	gd_EndTime = 0,
	gd_InfoList = []
	}).

-record(vip_types,{
	gd_AccountID = {integer},
	gd_Level = {integer},
	gd_EndTime = {integer},
	gd_InfoList = {term}
	}).