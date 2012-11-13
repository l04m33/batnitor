%% 玩家首次充值记录
-record (first_charge, {
	gd_accountId  = 0,
	gd_awardState = 1		%% 奖励状态，1：未领取 2：已领取
	}).

-record (first_charge_types, {
	gd_accountId  = {integer},
	gd_awardState = {integer}
	}).