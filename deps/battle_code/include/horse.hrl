-record (horse, {
	gd_accountId     = 0,		%% 主键：玩家id
	gd_horse 		 = 0,		%% 坐骑原型id（目前只有有没有马这个区分）
	gd_isShow 		 = 0,		%% 是否显示（0：不显示，1：显示）
	gd_exp           = 0,		%% 坐骑的总经验
	gd_curHorseEquip = 0,		%% 坐骑的当前时装
	gd_equipList     = []		%% 购买过的坐骑时装列表
	}).

-record (horse_types, {
	gd_accountId     = {integer},
	gd_horse         = {integer},
	gd_isShow        = {integer},
	gd_exp           = {integer},
	gd_curHorseEquip = {integer},
	gd_equipList     = {term}
	}).