-module(data_skill).

-compile(export_all).

-include("common.hrl").

%% 获取所有同一类技能属性id的普通技能
all_normal_skill(201) ->
	[201001, 201010, 201009, 201008, 201007, 201006, 201005, 201004, 201003, 201002];

all_normal_skill(202) ->
	[202010, 202009, 202008, 202007, 202006, 202005, 202004, 202003, 202002, 202001];

all_normal_skill(203) ->
	[203007, 203008, 203009, 203010, 203006, 203005, 203004, 203003, 203002, 203001];

all_normal_skill(204) ->
	[204007, 204008, 204009, 204010, 204006, 204005, 204004, 204003, 204002, 204001];

all_normal_skill(205) ->
	[205007, 205008, 205009, 205010, 205006, 205005, 205004, 205003, 205002, 205001];

all_normal_skill(206) ->
	[206007, 206008, 206009, 206010, 206006, 206005, 206004, 206003, 206002, 206001];

all_normal_skill(207) ->
	[207007, 207008, 207009, 207010, 207006, 207005, 207004, 207003, 207002, 207001];

all_normal_skill(208) ->
	[208007, 208008, 208009, 208010, 208006, 208005, 208004, 208003, 208002, 208001];

all_normal_skill(209) ->
	[209007, 209008, 209009, 209010, 209006, 209005, 209004, 209003, 209002, 209001];

all_normal_skill(210) ->
	[210007, 210008, 210009, 210010, 210006, 210005, 210004, 210003, 210002, 210001];

all_normal_skill(211) ->
	[211007, 211008, 211009, 211010, 211006, 211005, 211004, 211003, 211002, 211001];

all_normal_skill(212) ->
	[212007, 212008, 212009, 212010, 212006, 212005, 212004, 212003, 212002, 212001];

all_normal_skill(213) ->
	[213007, 213008, 213009, 213010, 213006, 213005, 213004, 213003, 213002, 213001];

all_normal_skill(214) ->
	[214007, 214008, 214009, 214010, 214006, 214005, 214004, 214003, 214002, 214001];

all_normal_skill(215) ->
	[215007, 215008, 215009, 215010, 215006, 215005, 215004, 215003, 215002, 215001];

all_normal_skill(216) ->
	[216007, 216008, 216009, 216010, 216006, 216005, 216004, 216003, 216002, 216001];

all_normal_skill(217) ->
	[217007, 217008, 217009, 217010, 217006, 217005, 217004, 217003, 217002, 217001];

all_normal_skill(218) ->
	[218007, 218008, 218009, 218010, 218006, 218005, 218004, 218003, 218002, 218001];

all_normal_skill(219) ->
	[219007, 219008, 219009, 219010, 219006, 219005, 219004, 219003, 219002, 219001];

all_normal_skill(220) ->
	[220007, 220008, 220009, 220010, 220006, 220005, 220004, 220003, 220002, 220001];

all_normal_skill(221) ->
	[221007, 221008, 221009, 221010, 221006, 221005, 221004, 221003, 221002, 221001];

all_normal_skill(222) ->
	[222009, 222008, 222007, 222006, 222005, 222004, 222003, 222002, 222001, 222010].


%%================================================
%% 获取技能书列表
get_skill_book_list() ->
	[262, 263, 264, 265, 266, 267, 268, 269, 270, 271, 413, 286, 287, 288].


%%================================================
%% 获取技能书对应的属性
get_skill_book_exp1(286) ->
	20;

get_skill_book_exp1(287) ->
	40;

get_skill_book_exp1(288) ->
	60.


%%================================================
%% 获取技能书对应的属性
get_skill_book_exp2(262) ->
	60;

get_skill_book_exp2(263) ->
	180;

get_skill_book_exp2(264) ->
	300;

get_skill_book_exp2(265) ->
	480;

get_skill_book_exp2(266) ->
	630;

get_skill_book_exp2(267) ->
	900;

get_skill_book_exp2(268) ->
	1200;

get_skill_book_exp2(269) ->
	1600;

get_skill_book_exp2(270) ->
	2300;

get_skill_book_exp2(271) ->
	3000.


%%================================================
%% 主角无双技能书列表
get_special_skill_book_list() ->
	[413].


%%================================================
%% 主角无双技能书经验
get_special_skill_book_exp(413) ->
	800.


%%================================================
%% 获取技能书对应的属性
get_use_skillbook_cost(262) ->
	6000;

get_use_skillbook_cost(263) ->
	10000;

get_use_skillbook_cost(264) ->
	15000;

get_use_skillbook_cost(265) ->
	20000;

get_use_skillbook_cost(266) ->
	30000;

get_use_skillbook_cost(267) ->
	40000;

get_use_skillbook_cost(268) ->
	50000;

get_use_skillbook_cost(269) ->
	70000;

get_use_skillbook_cost(270) ->
	80000;

get_use_skillbook_cost(271) ->
	100000;

get_use_skillbook_cost(413) ->
	50000;

get_use_skillbook_cost(286) ->
	8000;

get_use_skillbook_cost(287) ->
	16000;

get_use_skillbook_cost(288) ->
	24000.


%%================================================
%% 获取所有的技能属性分类id
get_all_skill_class() ->
	[201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222].


%%================================================
%% 根据技能属性id获取其概率
get_skill_class_rate(201) -> 10;

get_skill_class_rate(202) -> 10;

get_skill_class_rate(203) -> 10;

get_skill_class_rate(204) -> 10;

get_skill_class_rate(205) -> 10;

get_skill_class_rate(206) -> 10;

get_skill_class_rate(207) -> 10;

get_skill_class_rate(208) -> 10;

get_skill_class_rate(209) -> 10;

get_skill_class_rate(210) -> 10;

get_skill_class_rate(211) -> 10;

get_skill_class_rate(212) -> 20;

get_skill_class_rate(213) -> 10;

get_skill_class_rate(214) -> 10;

get_skill_class_rate(215) -> 10;

get_skill_class_rate(216) -> 10;

get_skill_class_rate(217) -> 10;

get_skill_class_rate(218) -> 30;

get_skill_class_rate(219) -> 10;

get_skill_class_rate(220) -> 10;

get_skill_class_rate(221) -> 10;

get_skill_class_rate(222) -> 50.


%%================================================
%% 根据技能id获取器刷出的概率
get_skill_rate(100001) -> 0;

get_skill_rate(101001) -> 0;

get_skill_rate(102001) -> 0;

get_skill_rate(103001) -> 0;

get_skill_rate(104001) -> 0;

get_skill_rate(104002) -> 0;

get_skill_rate(104003) -> 0;

get_skill_rate(104004) -> 0;

get_skill_rate(104005) -> 0;

get_skill_rate(104006) -> 0;

get_skill_rate(104007) -> 0;

get_skill_rate(104008) -> 0;

get_skill_rate(104009) -> 0;

get_skill_rate(104010) -> 0;

get_skill_rate(105001) -> 0;

get_skill_rate(105002) -> 0;

get_skill_rate(105003) -> 0;

get_skill_rate(105004) -> 0;

get_skill_rate(105005) -> 0;

get_skill_rate(105006) -> 0;

get_skill_rate(105007) -> 0;

get_skill_rate(105008) -> 0;

get_skill_rate(105009) -> 0;

get_skill_rate(105010) -> 0;

get_skill_rate(106001) -> 0;

get_skill_rate(106002) -> 0;

get_skill_rate(106003) -> 0;

get_skill_rate(106004) -> 0;

get_skill_rate(106005) -> 0;

get_skill_rate(106006) -> 0;

get_skill_rate(106007) -> 0;

get_skill_rate(106008) -> 0;

get_skill_rate(106009) -> 0;

get_skill_rate(106010) -> 0;

get_skill_rate(107001) -> 0;

get_skill_rate(107002) -> 0;

get_skill_rate(107003) -> 0;

get_skill_rate(107004) -> 0;

get_skill_rate(107005) -> 0;

get_skill_rate(107006) -> 0;

get_skill_rate(107007) -> 0;

get_skill_rate(107008) -> 0;

get_skill_rate(107009) -> 0;

get_skill_rate(107010) -> 0;

get_skill_rate(108001) -> 0;

get_skill_rate(108002) -> 0;

get_skill_rate(108003) -> 0;

get_skill_rate(108004) -> 0;

get_skill_rate(108005) -> 0;

get_skill_rate(108006) -> 0;

get_skill_rate(108007) -> 0;

get_skill_rate(108008) -> 0;

get_skill_rate(108009) -> 0;

get_skill_rate(108010) -> 0;

get_skill_rate(109001) -> 0;

get_skill_rate(109002) -> 0;

get_skill_rate(109003) -> 0;

get_skill_rate(109004) -> 0;

get_skill_rate(109005) -> 0;

get_skill_rate(109006) -> 0;

get_skill_rate(109007) -> 0;

get_skill_rate(109008) -> 0;

get_skill_rate(109009) -> 0;

get_skill_rate(109010) -> 0;

get_skill_rate(110001) -> 0;

get_skill_rate(110002) -> 0;

get_skill_rate(110003) -> 0;

get_skill_rate(110004) -> 0;

get_skill_rate(110005) -> 0;

get_skill_rate(110006) -> 0;

get_skill_rate(110007) -> 0;

get_skill_rate(110008) -> 0;

get_skill_rate(110009) -> 0;

get_skill_rate(110010) -> 0;

get_skill_rate(111001) -> 0;

get_skill_rate(111002) -> 0;

get_skill_rate(111003) -> 0;

get_skill_rate(111004) -> 0;

get_skill_rate(111005) -> 0;

get_skill_rate(111006) -> 0;

get_skill_rate(111007) -> 0;

get_skill_rate(111008) -> 0;

get_skill_rate(111009) -> 0;

get_skill_rate(111010) -> 0;

get_skill_rate(112001) -> 0;

get_skill_rate(112002) -> 0;

get_skill_rate(112003) -> 0;

get_skill_rate(112004) -> 0;

get_skill_rate(112005) -> 0;

get_skill_rate(112006) -> 0;

get_skill_rate(112007) -> 0;

get_skill_rate(112008) -> 0;

get_skill_rate(112009) -> 0;

get_skill_rate(112010) -> 0;

get_skill_rate(113001) -> 0;

get_skill_rate(113002) -> 0;

get_skill_rate(113003) -> 0;

get_skill_rate(113004) -> 0;

get_skill_rate(113005) -> 0;

get_skill_rate(113006) -> 0;

get_skill_rate(113007) -> 0;

get_skill_rate(113008) -> 0;

get_skill_rate(113009) -> 0;

get_skill_rate(113010) -> 0;

get_skill_rate(114001) -> 0;

get_skill_rate(114002) -> 0;

get_skill_rate(114003) -> 0;

get_skill_rate(114004) -> 0;

get_skill_rate(114005) -> 0;

get_skill_rate(114006) -> 0;

get_skill_rate(114007) -> 0;

get_skill_rate(114008) -> 0;

get_skill_rate(114009) -> 0;

get_skill_rate(114010) -> 0;

get_skill_rate(115001) -> 0;

get_skill_rate(115002) -> 0;

get_skill_rate(115003) -> 0;

get_skill_rate(115004) -> 0;

get_skill_rate(115005) -> 0;

get_skill_rate(115006) -> 0;

get_skill_rate(115007) -> 0;

get_skill_rate(115008) -> 0;

get_skill_rate(115009) -> 0;

get_skill_rate(115010) -> 0;

get_skill_rate(116001) -> 0;

get_skill_rate(116002) -> 0;

get_skill_rate(116003) -> 0;

get_skill_rate(116004) -> 0;

get_skill_rate(116005) -> 0;

get_skill_rate(116006) -> 0;

get_skill_rate(116007) -> 0;

get_skill_rate(116008) -> 0;

get_skill_rate(116009) -> 0;

get_skill_rate(116010) -> 0;

get_skill_rate(117001) -> 0;

get_skill_rate(117002) -> 0;

get_skill_rate(117003) -> 0;

get_skill_rate(117004) -> 0;

get_skill_rate(117005) -> 0;

get_skill_rate(117006) -> 0;

get_skill_rate(117007) -> 0;

get_skill_rate(117008) -> 0;

get_skill_rate(117009) -> 0;

get_skill_rate(117010) -> 0;

get_skill_rate(118001) -> 0;

get_skill_rate(118002) -> 0;

get_skill_rate(118003) -> 0;

get_skill_rate(118004) -> 0;

get_skill_rate(118005) -> 0;

get_skill_rate(118006) -> 0;

get_skill_rate(118007) -> 0;

get_skill_rate(118008) -> 0;

get_skill_rate(118009) -> 0;

get_skill_rate(118010) -> 0;

get_skill_rate(119001) -> 0;

get_skill_rate(119002) -> 0;

get_skill_rate(119003) -> 0;

get_skill_rate(119004) -> 0;

get_skill_rate(119005) -> 0;

get_skill_rate(119006) -> 0;

get_skill_rate(119007) -> 0;

get_skill_rate(119008) -> 0;

get_skill_rate(119009) -> 0;

get_skill_rate(119010) -> 0;

get_skill_rate(201001) -> 3;

get_skill_rate(201002) -> 2;

get_skill_rate(201003) -> 1;

get_skill_rate(201004) -> 0;

get_skill_rate(201005) -> 0;

get_skill_rate(201006) -> 0;

get_skill_rate(201007) -> 0;

get_skill_rate(201008) -> 0;

get_skill_rate(201009) -> 0;

get_skill_rate(201010) -> 0;

get_skill_rate(202001) -> 3;

get_skill_rate(202002) -> 2;

get_skill_rate(202003) -> 1;

get_skill_rate(202004) -> 0;

get_skill_rate(202005) -> 0;

get_skill_rate(202006) -> 0;

get_skill_rate(202007) -> 0;

get_skill_rate(202008) -> 0;

get_skill_rate(202009) -> 0;

get_skill_rate(202010) -> 0;

get_skill_rate(203001) -> 3;

get_skill_rate(203002) -> 2;

get_skill_rate(203003) -> 1;

get_skill_rate(203004) -> 0;

get_skill_rate(203005) -> 0;

get_skill_rate(203006) -> 0;

get_skill_rate(203007) -> 0;

get_skill_rate(203008) -> 0;

get_skill_rate(203009) -> 0;

get_skill_rate(203010) -> 0;

get_skill_rate(204001) -> 3;

get_skill_rate(204002) -> 2;

get_skill_rate(204003) -> 1;

get_skill_rate(204004) -> 0;

get_skill_rate(204005) -> 0;

get_skill_rate(204006) -> 0;

get_skill_rate(204007) -> 0;

get_skill_rate(204008) -> 0;

get_skill_rate(204009) -> 0;

get_skill_rate(204010) -> 0;

get_skill_rate(205001) -> 3;

get_skill_rate(205002) -> 2;

get_skill_rate(205003) -> 1;

get_skill_rate(205004) -> 0;

get_skill_rate(205005) -> 0;

get_skill_rate(205006) -> 0;

get_skill_rate(205007) -> 0;

get_skill_rate(205008) -> 0;

get_skill_rate(205009) -> 0;

get_skill_rate(205010) -> 0;

get_skill_rate(206001) -> 3;

get_skill_rate(206002) -> 2;

get_skill_rate(206003) -> 1;

get_skill_rate(206004) -> 0;

get_skill_rate(206005) -> 0;

get_skill_rate(206006) -> 0;

get_skill_rate(206007) -> 0;

get_skill_rate(206008) -> 0;

get_skill_rate(206009) -> 0;

get_skill_rate(206010) -> 0;

get_skill_rate(207001) -> 3;

get_skill_rate(207002) -> 2;

get_skill_rate(207003) -> 1;

get_skill_rate(207004) -> 0;

get_skill_rate(207005) -> 0;

get_skill_rate(207006) -> 0;

get_skill_rate(207007) -> 0;

get_skill_rate(207008) -> 0;

get_skill_rate(207009) -> 0;

get_skill_rate(207010) -> 0;

get_skill_rate(208001) -> 3;

get_skill_rate(208002) -> 2;

get_skill_rate(208003) -> 1;

get_skill_rate(208004) -> 0;

get_skill_rate(208005) -> 0;

get_skill_rate(208006) -> 0;

get_skill_rate(208007) -> 0;

get_skill_rate(208008) -> 0;

get_skill_rate(208009) -> 0;

get_skill_rate(208010) -> 0;

get_skill_rate(209001) -> 3;

get_skill_rate(209002) -> 2;

get_skill_rate(209003) -> 1;

get_skill_rate(209004) -> 0;

get_skill_rate(209005) -> 0;

get_skill_rate(209006) -> 0;

get_skill_rate(209007) -> 0;

get_skill_rate(209008) -> 0;

get_skill_rate(209009) -> 0;

get_skill_rate(209010) -> 0;

get_skill_rate(210001) -> 3;

get_skill_rate(210002) -> 2;

get_skill_rate(210003) -> 1;

get_skill_rate(210004) -> 0;

get_skill_rate(210005) -> 0;

get_skill_rate(210006) -> 0;

get_skill_rate(210007) -> 0;

get_skill_rate(210008) -> 0;

get_skill_rate(210009) -> 0;

get_skill_rate(210010) -> 0;

get_skill_rate(211001) -> 3;

get_skill_rate(211002) -> 2;

get_skill_rate(211003) -> 1;

get_skill_rate(211004) -> 0;

get_skill_rate(211005) -> 0;

get_skill_rate(211006) -> 0;

get_skill_rate(211007) -> 0;

get_skill_rate(211008) -> 0;

get_skill_rate(211009) -> 0;

get_skill_rate(211010) -> 0;

get_skill_rate(212001) -> 3;

get_skill_rate(212002) -> 2;

get_skill_rate(212003) -> 1;

get_skill_rate(212004) -> 0;

get_skill_rate(212005) -> 0;

get_skill_rate(212006) -> 0;

get_skill_rate(212007) -> 0;

get_skill_rate(212008) -> 0;

get_skill_rate(212009) -> 0;

get_skill_rate(212010) -> 0;

get_skill_rate(213001) -> 3;

get_skill_rate(213002) -> 2;

get_skill_rate(213003) -> 1;

get_skill_rate(213004) -> 0;

get_skill_rate(213005) -> 0;

get_skill_rate(213006) -> 0;

get_skill_rate(213007) -> 0;

get_skill_rate(213008) -> 0;

get_skill_rate(213009) -> 0;

get_skill_rate(213010) -> 0;

get_skill_rate(214001) -> 3;

get_skill_rate(214002) -> 2;

get_skill_rate(214003) -> 1;

get_skill_rate(214004) -> 0;

get_skill_rate(214005) -> 0;

get_skill_rate(214006) -> 0;

get_skill_rate(214007) -> 0;

get_skill_rate(214008) -> 0;

get_skill_rate(214009) -> 0;

get_skill_rate(214010) -> 0;

get_skill_rate(215001) -> 3;

get_skill_rate(215002) -> 2;

get_skill_rate(215003) -> 1;

get_skill_rate(215004) -> 0;

get_skill_rate(215005) -> 0;

get_skill_rate(215006) -> 0;

get_skill_rate(215007) -> 0;

get_skill_rate(215008) -> 0;

get_skill_rate(215009) -> 0;

get_skill_rate(215010) -> 0;

get_skill_rate(216001) -> 3;

get_skill_rate(216002) -> 2;

get_skill_rate(216003) -> 1;

get_skill_rate(216004) -> 0;

get_skill_rate(216005) -> 0;

get_skill_rate(216006) -> 0;

get_skill_rate(216007) -> 0;

get_skill_rate(216008) -> 0;

get_skill_rate(216009) -> 0;

get_skill_rate(216010) -> 0;

get_skill_rate(217001) -> 3;

get_skill_rate(217002) -> 2;

get_skill_rate(217003) -> 1;

get_skill_rate(217004) -> 0;

get_skill_rate(217005) -> 0;

get_skill_rate(217006) -> 0;

get_skill_rate(217007) -> 0;

get_skill_rate(217008) -> 0;

get_skill_rate(217009) -> 0;

get_skill_rate(217010) -> 0;

get_skill_rate(218001) -> 3;

get_skill_rate(218002) -> 2;

get_skill_rate(218003) -> 1;

get_skill_rate(218004) -> 0;

get_skill_rate(218005) -> 0;

get_skill_rate(218006) -> 0;

get_skill_rate(218007) -> 0;

get_skill_rate(218008) -> 0;

get_skill_rate(218009) -> 0;

get_skill_rate(218010) -> 0;

get_skill_rate(219001) -> 3;

get_skill_rate(219002) -> 2;

get_skill_rate(219003) -> 1;

get_skill_rate(219004) -> 0;

get_skill_rate(219005) -> 0;

get_skill_rate(219006) -> 0;

get_skill_rate(219007) -> 0;

get_skill_rate(219008) -> 0;

get_skill_rate(219009) -> 0;

get_skill_rate(219010) -> 0;

get_skill_rate(220001) -> 3;

get_skill_rate(220002) -> 2;

get_skill_rate(220003) -> 1;

get_skill_rate(220004) -> 0;

get_skill_rate(220005) -> 0;

get_skill_rate(220006) -> 0;

get_skill_rate(220007) -> 0;

get_skill_rate(220008) -> 0;

get_skill_rate(220009) -> 0;

get_skill_rate(220010) -> 0;

get_skill_rate(221001) -> 3;

get_skill_rate(221002) -> 2;

get_skill_rate(221003) -> 1;

get_skill_rate(221004) -> 0;

get_skill_rate(221005) -> 0;

get_skill_rate(221006) -> 0;

get_skill_rate(221007) -> 0;

get_skill_rate(221008) -> 0;

get_skill_rate(221009) -> 0;

get_skill_rate(221010) -> 0;

get_skill_rate(222001) -> 3;

get_skill_rate(222002) -> 2;

get_skill_rate(222003) -> 1;

get_skill_rate(222004) -> 0;

get_skill_rate(222005) -> 0;

get_skill_rate(222006) -> 0;

get_skill_rate(222007) -> 0;

get_skill_rate(222008) -> 0;

get_skill_rate(222009) -> 0;

get_skill_rate(222010) -> 0;

get_skill_rate(223001) -> 0;

get_skill_rate(224001) -> 0;

get_skill_rate(225001) -> 0;

get_skill_rate(226001) -> 0;

get_skill_rate(227001) -> 0;

get_skill_rate(228001) -> 0;

get_skill_rate(229001) -> 0;

get_skill_rate(230001) -> 0;

get_skill_rate(231001) -> 0;

get_skill_rate(232001) -> 0;

get_skill_rate(233001) -> 0;

get_skill_rate(234001) -> 0;

get_skill_rate(235001) -> 0;

get_skill_rate(236001) -> 0;

get_skill_rate(237001) -> 0;

get_skill_rate(238001) -> 0;

get_skill_rate(239001) -> 0;

get_skill_rate(240001) -> 0;

get_skill_rate(241001) -> 0;

get_skill_rate(242001) -> 0;

get_skill_rate(243001) -> 0;

get_skill_rate(244001) -> 0;

get_skill_rate(245001) -> 0;

get_skill_rate(246001) -> 0;

get_skill_rate(247001) -> 0;

get_skill_rate(248001) -> 0;

get_skill_rate(249001) -> 0;

get_skill_rate(250001) -> 0;

get_skill_rate(251001) -> 0;

get_skill_rate(252001) -> 0;

get_skill_rate(253001) -> 0;

get_skill_rate(254001) -> 0;

get_skill_rate(255001) -> 0;

get_skill_rate(256001) -> 0;

get_skill_rate(257001) -> 0;

get_skill_rate(258001) -> 0;

get_skill_rate(259001) -> 0;

get_skill_rate(260001) -> 0;

get_skill_rate(261001) -> 0;

get_skill_rate(262001) -> 0;

get_skill_rate(263001) -> 0;

get_skill_rate(264001) -> 0;

get_skill_rate(265001) -> 0;

get_skill_rate(266001) -> 0;

get_skill_rate(267001) -> 0;

get_skill_rate(268001) -> 0;

get_skill_rate(269001) -> 0;

get_skill_rate(270001) -> 0;

get_skill_rate(271001) -> 0;

get_skill_rate(272001) -> 0;

get_skill_rate(273001) -> 0;

get_skill_rate(274001) -> 0;

get_skill_rate(275001) -> 0;

get_skill_rate(276001) -> 0;

get_skill_rate(277001) -> 0;

get_skill_rate(278001) -> 0;

get_skill_rate(279001) -> 0;

get_skill_rate(280001) -> 0;

get_skill_rate(281001) -> 0;

get_skill_rate(282001) -> 0;

get_skill_rate(283001) -> 0;

get_skill_rate(284001) -> 0;

get_skill_rate(285001) -> 0;

get_skill_rate(286001) -> 0;

get_skill_rate(287001) -> 0;

get_skill_rate(288001) -> 0;

get_skill_rate(289001) -> 0;

get_skill_rate(301001) -> 0;

get_skill_rate(301002) -> 0;

get_skill_rate(301003) -> 0;

get_skill_rate(301004) -> 0;

get_skill_rate(301005) -> 0;

get_skill_rate(301006) -> 0;

get_skill_rate(301007) -> 0;

get_skill_rate(301008) -> 0;

get_skill_rate(301009) -> 0;

get_skill_rate(301010) -> 0;

get_skill_rate(301011) -> 0;

get_skill_rate(301012) -> 0;

get_skill_rate(301013) -> 0;

get_skill_rate(301014) -> 0;

get_skill_rate(301015) -> 0;

get_skill_rate(301016) -> 0;

get_skill_rate(301017) -> 0;

get_skill_rate(301018) -> 0;

get_skill_rate(301019) -> 0;

get_skill_rate(301020) -> 0;

get_skill_rate(301021) -> 0;

get_skill_rate(301022) -> 0;

get_skill_rate(301023) -> 0;

get_skill_rate(301024) -> 0;

get_skill_rate(301025) -> 0;

get_skill_rate(301026) -> 0;

get_skill_rate(301027) -> 0;

get_skill_rate(301028) -> 0;

get_skill_rate(301029) -> 0;

get_skill_rate(301030) -> 0;

get_skill_rate(302001) -> 0;

get_skill_rate(302002) -> 0;

get_skill_rate(302003) -> 0;

get_skill_rate(302004) -> 0;

get_skill_rate(302005) -> 0;

get_skill_rate(302006) -> 0;

get_skill_rate(302007) -> 0;

get_skill_rate(302008) -> 0;

get_skill_rate(302009) -> 0;

get_skill_rate(302010) -> 0;

get_skill_rate(302011) -> 0;

get_skill_rate(302012) -> 0;

get_skill_rate(302013) -> 0;

get_skill_rate(302014) -> 0;

get_skill_rate(302015) -> 0;

get_skill_rate(302016) -> 0;

get_skill_rate(302017) -> 0;

get_skill_rate(302018) -> 0;

get_skill_rate(302019) -> 0;

get_skill_rate(302020) -> 0;

get_skill_rate(302021) -> 0;

get_skill_rate(302022) -> 0;

get_skill_rate(302023) -> 0;

get_skill_rate(302024) -> 0;

get_skill_rate(302025) -> 0;

get_skill_rate(302026) -> 0;

get_skill_rate(302027) -> 0;

get_skill_rate(302028) -> 0;

get_skill_rate(302029) -> 0;

get_skill_rate(302030) -> 0;

get_skill_rate(303001) -> 0;

get_skill_rate(303002) -> 0;

get_skill_rate(303003) -> 0;

get_skill_rate(303004) -> 0;

get_skill_rate(303005) -> 0;

get_skill_rate(303006) -> 0;

get_skill_rate(303007) -> 0;

get_skill_rate(303008) -> 0;

get_skill_rate(303009) -> 0;

get_skill_rate(303010) -> 0;

get_skill_rate(303011) -> 0;

get_skill_rate(303012) -> 0;

get_skill_rate(303013) -> 0;

get_skill_rate(303014) -> 0;

get_skill_rate(303015) -> 0;

get_skill_rate(303016) -> 0;

get_skill_rate(303017) -> 0;

get_skill_rate(303018) -> 0;

get_skill_rate(303019) -> 0;

get_skill_rate(303020) -> 0;

get_skill_rate(303021) -> 0;

get_skill_rate(303022) -> 0;

get_skill_rate(303023) -> 0;

get_skill_rate(303024) -> 0;

get_skill_rate(303025) -> 0;

get_skill_rate(303026) -> 0;

get_skill_rate(303027) -> 0;

get_skill_rate(303028) -> 0;

get_skill_rate(303029) -> 0;

get_skill_rate(303030) -> 0;

get_skill_rate(304001) -> 0;

get_skill_rate(304002) -> 0;

get_skill_rate(304003) -> 0;

get_skill_rate(304004) -> 0;

get_skill_rate(304005) -> 0;

get_skill_rate(304006) -> 0;

get_skill_rate(304007) -> 0;

get_skill_rate(304008) -> 0;

get_skill_rate(304009) -> 0;

get_skill_rate(304010) -> 0;

get_skill_rate(304011) -> 0;

get_skill_rate(304012) -> 0;

get_skill_rate(304013) -> 0;

get_skill_rate(304014) -> 0;

get_skill_rate(304015) -> 0;

get_skill_rate(304016) -> 0;

get_skill_rate(304017) -> 0;

get_skill_rate(304018) -> 0;

get_skill_rate(304019) -> 0;

get_skill_rate(304020) -> 0;

get_skill_rate(304021) -> 0;

get_skill_rate(304022) -> 0;

get_skill_rate(304023) -> 0;

get_skill_rate(304024) -> 0;

get_skill_rate(304025) -> 0;

get_skill_rate(304026) -> 0;

get_skill_rate(304027) -> 0;

get_skill_rate(304028) -> 0;

get_skill_rate(304029) -> 0;

get_skill_rate(304030) -> 0;

get_skill_rate(305001) -> 0;

get_skill_rate(305002) -> 0;

get_skill_rate(305003) -> 0;

get_skill_rate(305004) -> 0;

get_skill_rate(305005) -> 0;

get_skill_rate(305006) -> 0;

get_skill_rate(305007) -> 0;

get_skill_rate(305008) -> 0;

get_skill_rate(305009) -> 0;

get_skill_rate(305010) -> 0;

get_skill_rate(305011) -> 0;

get_skill_rate(305012) -> 0;

get_skill_rate(305013) -> 0;

get_skill_rate(305014) -> 0;

get_skill_rate(305015) -> 0;

get_skill_rate(305016) -> 0;

get_skill_rate(305017) -> 0;

get_skill_rate(305018) -> 0;

get_skill_rate(305019) -> 0;

get_skill_rate(305020) -> 0;

get_skill_rate(305021) -> 0;

get_skill_rate(305022) -> 0;

get_skill_rate(305023) -> 0;

get_skill_rate(305024) -> 0;

get_skill_rate(305025) -> 0;

get_skill_rate(305026) -> 0;

get_skill_rate(305027) -> 0;

get_skill_rate(305028) -> 0;

get_skill_rate(305029) -> 0;

get_skill_rate(305030) -> 0;

get_skill_rate(306001) -> 0;

get_skill_rate(306002) -> 0;

get_skill_rate(306003) -> 0;

get_skill_rate(306004) -> 0;

get_skill_rate(306005) -> 0;

get_skill_rate(306006) -> 0;

get_skill_rate(306007) -> 0;

get_skill_rate(306008) -> 0;

get_skill_rate(306009) -> 0;

get_skill_rate(306010) -> 0;

get_skill_rate(306011) -> 0;

get_skill_rate(306012) -> 0;

get_skill_rate(306013) -> 0;

get_skill_rate(306014) -> 0;

get_skill_rate(306015) -> 0;

get_skill_rate(306016) -> 0;

get_skill_rate(306017) -> 0;

get_skill_rate(306018) -> 0;

get_skill_rate(306019) -> 0;

get_skill_rate(306020) -> 0;

get_skill_rate(306021) -> 0;

get_skill_rate(306022) -> 0;

get_skill_rate(306023) -> 0;

get_skill_rate(306024) -> 0;

get_skill_rate(306025) -> 0;

get_skill_rate(306026) -> 0;

get_skill_rate(306027) -> 0;

get_skill_rate(306028) -> 0;

get_skill_rate(306029) -> 0;

get_skill_rate(306030) -> 0;

get_skill_rate(307001) -> 0;

get_skill_rate(307002) -> 0;

get_skill_rate(307003) -> 0;

get_skill_rate(307004) -> 0;

get_skill_rate(307005) -> 0;

get_skill_rate(307006) -> 0;

get_skill_rate(307007) -> 0;

get_skill_rate(307008) -> 0;

get_skill_rate(307009) -> 0;

get_skill_rate(307010) -> 0;

get_skill_rate(307011) -> 0;

get_skill_rate(307012) -> 0;

get_skill_rate(307013) -> 0;

get_skill_rate(307014) -> 0;

get_skill_rate(307015) -> 0;

get_skill_rate(307016) -> 0;

get_skill_rate(307017) -> 0;

get_skill_rate(307018) -> 0;

get_skill_rate(307019) -> 0;

get_skill_rate(307020) -> 0;

get_skill_rate(307021) -> 0;

get_skill_rate(307022) -> 0;

get_skill_rate(307023) -> 0;

get_skill_rate(307024) -> 0;

get_skill_rate(307025) -> 0;

get_skill_rate(307026) -> 0;

get_skill_rate(307027) -> 0;

get_skill_rate(307028) -> 0;

get_skill_rate(307029) -> 0;

get_skill_rate(307030) -> 0;

get_skill_rate(308001) -> 0;

get_skill_rate(308002) -> 0;

get_skill_rate(308003) -> 0;

get_skill_rate(308004) -> 0;

get_skill_rate(308005) -> 0;

get_skill_rate(308006) -> 0;

get_skill_rate(308007) -> 0;

get_skill_rate(308008) -> 0;

get_skill_rate(308009) -> 0;

get_skill_rate(308010) -> 0;

get_skill_rate(308011) -> 0;

get_skill_rate(308012) -> 0;

get_skill_rate(308013) -> 0;

get_skill_rate(308014) -> 0;

get_skill_rate(308015) -> 0;

get_skill_rate(308016) -> 0;

get_skill_rate(308017) -> 0;

get_skill_rate(308018) -> 0;

get_skill_rate(308019) -> 0;

get_skill_rate(308020) -> 0;

get_skill_rate(308021) -> 0;

get_skill_rate(308022) -> 0;

get_skill_rate(308023) -> 0;

get_skill_rate(308024) -> 0;

get_skill_rate(308025) -> 0;

get_skill_rate(308026) -> 0;

get_skill_rate(308027) -> 0;

get_skill_rate(308028) -> 0;

get_skill_rate(308029) -> 0;

get_skill_rate(308030) -> 0;

get_skill_rate(309001) -> 0;

get_skill_rate(309002) -> 0;

get_skill_rate(309003) -> 0;

get_skill_rate(309004) -> 0;

get_skill_rate(309005) -> 0;

get_skill_rate(309006) -> 0;

get_skill_rate(309007) -> 0;

get_skill_rate(309008) -> 0;

get_skill_rate(309009) -> 0;

get_skill_rate(309010) -> 0;

get_skill_rate(309011) -> 0;

get_skill_rate(309012) -> 0;

get_skill_rate(309013) -> 0;

get_skill_rate(309014) -> 0;

get_skill_rate(309015) -> 0;

get_skill_rate(309016) -> 0;

get_skill_rate(309017) -> 0;

get_skill_rate(309018) -> 0;

get_skill_rate(309019) -> 0;

get_skill_rate(309020) -> 0;

get_skill_rate(309021) -> 0;

get_skill_rate(309022) -> 0;

get_skill_rate(309023) -> 0;

get_skill_rate(309024) -> 0;

get_skill_rate(309025) -> 0;

get_skill_rate(309026) -> 0;

get_skill_rate(309027) -> 0;

get_skill_rate(309028) -> 0;

get_skill_rate(309029) -> 0;

get_skill_rate(309030) -> 0;

get_skill_rate(310001) -> 0;

get_skill_rate(310002) -> 0;

get_skill_rate(310003) -> 0;

get_skill_rate(310004) -> 0;

get_skill_rate(310005) -> 0;

get_skill_rate(310006) -> 0;

get_skill_rate(310007) -> 0;

get_skill_rate(310008) -> 0;

get_skill_rate(310009) -> 0;

get_skill_rate(310010) -> 0;

get_skill_rate(310011) -> 0;

get_skill_rate(310012) -> 0;

get_skill_rate(310013) -> 0;

get_skill_rate(310014) -> 0;

get_skill_rate(310015) -> 0;

get_skill_rate(310016) -> 0;

get_skill_rate(310017) -> 0;

get_skill_rate(310018) -> 0;

get_skill_rate(310019) -> 0;

get_skill_rate(310020) -> 0;

get_skill_rate(310021) -> 0;

get_skill_rate(310022) -> 0;

get_skill_rate(310023) -> 0;

get_skill_rate(310024) -> 0;

get_skill_rate(310025) -> 0;

get_skill_rate(310026) -> 0;

get_skill_rate(310027) -> 0;

get_skill_rate(310028) -> 0;

get_skill_rate(310029) -> 0;

get_skill_rate(310030) -> 0;

get_skill_rate(311001) -> 0;

get_skill_rate(311002) -> 0;

get_skill_rate(311003) -> 0;

get_skill_rate(311004) -> 0;

get_skill_rate(311005) -> 0;

get_skill_rate(311006) -> 0;

get_skill_rate(311007) -> 0;

get_skill_rate(311008) -> 0;

get_skill_rate(311009) -> 0;

get_skill_rate(311010) -> 0;

get_skill_rate(311011) -> 0;

get_skill_rate(311012) -> 0;

get_skill_rate(311013) -> 0;

get_skill_rate(311014) -> 0;

get_skill_rate(311015) -> 0;

get_skill_rate(311016) -> 0;

get_skill_rate(311017) -> 0;

get_skill_rate(311018) -> 0;

get_skill_rate(311019) -> 0;

get_skill_rate(311020) -> 0;

get_skill_rate(311021) -> 0;

get_skill_rate(311022) -> 0;

get_skill_rate(311023) -> 0;

get_skill_rate(311024) -> 0;

get_skill_rate(311025) -> 0;

get_skill_rate(311026) -> 0;

get_skill_rate(311027) -> 0;

get_skill_rate(311028) -> 0;

get_skill_rate(311029) -> 0;

get_skill_rate(311030) -> 0;

get_skill_rate(312001) -> 0;

get_skill_rate(312002) -> 0;

get_skill_rate(312003) -> 0;

get_skill_rate(312004) -> 0;

get_skill_rate(312005) -> 0;

get_skill_rate(312006) -> 0;

get_skill_rate(312007) -> 0;

get_skill_rate(312008) -> 0;

get_skill_rate(312009) -> 0;

get_skill_rate(312010) -> 0;

get_skill_rate(312011) -> 0;

get_skill_rate(312012) -> 0;

get_skill_rate(312013) -> 0;

get_skill_rate(312014) -> 0;

get_skill_rate(312015) -> 0;

get_skill_rate(312016) -> 0;

get_skill_rate(312017) -> 0;

get_skill_rate(312018) -> 0;

get_skill_rate(312019) -> 0;

get_skill_rate(312020) -> 0;

get_skill_rate(312021) -> 0;

get_skill_rate(312022) -> 0;

get_skill_rate(312023) -> 0;

get_skill_rate(312024) -> 0;

get_skill_rate(312025) -> 0;

get_skill_rate(312026) -> 0;

get_skill_rate(312027) -> 0;

get_skill_rate(312028) -> 0;

get_skill_rate(312029) -> 0;

get_skill_rate(312030) -> 0;

get_skill_rate(313001) -> 0;

get_skill_rate(313002) -> 0;

get_skill_rate(313003) -> 0;

get_skill_rate(313004) -> 0;

get_skill_rate(313005) -> 0;

get_skill_rate(313006) -> 0;

get_skill_rate(313007) -> 0;

get_skill_rate(313008) -> 0;

get_skill_rate(313009) -> 0;

get_skill_rate(313010) -> 0;

get_skill_rate(313011) -> 0;

get_skill_rate(313012) -> 0;

get_skill_rate(313013) -> 0;

get_skill_rate(313014) -> 0;

get_skill_rate(313015) -> 0;

get_skill_rate(313016) -> 0;

get_skill_rate(313017) -> 0;

get_skill_rate(313018) -> 0;

get_skill_rate(313019) -> 0;

get_skill_rate(313020) -> 0;

get_skill_rate(313021) -> 0;

get_skill_rate(313022) -> 0;

get_skill_rate(313023) -> 0;

get_skill_rate(313024) -> 0;

get_skill_rate(313025) -> 0;

get_skill_rate(313026) -> 0;

get_skill_rate(313027) -> 0;

get_skill_rate(313028) -> 0;

get_skill_rate(313029) -> 0;

get_skill_rate(313030) -> 0;

get_skill_rate(314001) -> 0;

get_skill_rate(314002) -> 0;

get_skill_rate(314003) -> 0;

get_skill_rate(314004) -> 0;

get_skill_rate(314005) -> 0;

get_skill_rate(314006) -> 0;

get_skill_rate(314007) -> 0;

get_skill_rate(314008) -> 0;

get_skill_rate(314009) -> 0;

get_skill_rate(314010) -> 0;

get_skill_rate(314011) -> 0;

get_skill_rate(314012) -> 0;

get_skill_rate(314013) -> 0;

get_skill_rate(314014) -> 0;

get_skill_rate(314015) -> 0;

get_skill_rate(314016) -> 0;

get_skill_rate(314017) -> 0;

get_skill_rate(314018) -> 0;

get_skill_rate(314019) -> 0;

get_skill_rate(314020) -> 0;

get_skill_rate(314021) -> 0;

get_skill_rate(314022) -> 0;

get_skill_rate(314023) -> 0;

get_skill_rate(314024) -> 0;

get_skill_rate(314025) -> 0;

get_skill_rate(314026) -> 0;

get_skill_rate(314027) -> 0;

get_skill_rate(314028) -> 0;

get_skill_rate(314029) -> 0;

get_skill_rate(314030) -> 0;

get_skill_rate(401001) -> 0;

get_skill_rate(401002) -> 0;

get_skill_rate(401003) -> 0;

get_skill_rate(401004) -> 0;

get_skill_rate(401005) -> 0;

get_skill_rate(401006) -> 0;

get_skill_rate(401007) -> 0;

get_skill_rate(401008) -> 0;

get_skill_rate(401009) -> 0;

get_skill_rate(401010) -> 0;

get_skill_rate(402001) -> 0;

get_skill_rate(402002) -> 0;

get_skill_rate(402003) -> 0;

get_skill_rate(402004) -> 0;

get_skill_rate(402005) -> 0;

get_skill_rate(402006) -> 0;

get_skill_rate(402007) -> 0;

get_skill_rate(402008) -> 0;

get_skill_rate(402009) -> 0;

get_skill_rate(402010) -> 0;

get_skill_rate(403001) -> 0;

get_skill_rate(403002) -> 0;

get_skill_rate(403003) -> 0;

get_skill_rate(403004) -> 0;

get_skill_rate(403005) -> 0;

get_skill_rate(403006) -> 0;

get_skill_rate(403007) -> 0;

get_skill_rate(403008) -> 0;

get_skill_rate(403009) -> 0;

get_skill_rate(403010) -> 0;

get_skill_rate(404001) -> 0;

get_skill_rate(404002) -> 0;

get_skill_rate(404003) -> 0;

get_skill_rate(404004) -> 0;

get_skill_rate(404005) -> 0;

get_skill_rate(404006) -> 0;

get_skill_rate(404007) -> 0;

get_skill_rate(404008) -> 0;

get_skill_rate(404009) -> 0;

get_skill_rate(404010) -> 0;

get_skill_rate(405001) -> 0;

get_skill_rate(406001) -> 0.


%%================================================
%% 根据技能id获取其详细信息
%% 普通攻击
skill_info(100001) ->
	#skill_info{
		mode_id       = 100001,
		class_id      = 100,
		type          = 0,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 正气诀
skill_info(101001) ->
	#skill_info{
		mode_id       = 101001,
		class_id      = 101,
		type          = 2,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 突刺诀
skill_info(102001) ->
	#skill_info{
		mode_id       = 102001,
		class_id      = 102,
		type          = 2,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 凝神决
skill_info(103001) ->
	#skill_info{
		mode_id       = 103001,
		class_id      = 103,
		type          = 2,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 威震四方
skill_info(104001) ->
	#skill_info{
		mode_id       = 104001,
		class_id      = 104,
		type          = 1,
		effect        = 1,
		level_up_exp  = 4000,
		next_skill_id = 104002
	};

%% 威震四方
skill_info(104002) ->
	#skill_info{
		mode_id       = 104002,
		class_id      = 104,
		type          = 1,
		effect        = 1,
		level_up_exp  = 11500,
		next_skill_id = 104003
	};

%% 威震四方
skill_info(104003) ->
	#skill_info{
		mode_id       = 104003,
		class_id      = 104,
		type          = 1,
		effect        = 1,
		level_up_exp  = 20000,
		next_skill_id = 104004
	};

%% 威震四方
skill_info(104004) ->
	#skill_info{
		mode_id       = 104004,
		class_id      = 104,
		type          = 1,
		effect        = 1,
		level_up_exp  = 36000,
		next_skill_id = 104005
	};

%% 威震四方
skill_info(104005) ->
	#skill_info{
		mode_id       = 104005,
		class_id      = 104,
		type          = 1,
		effect        = 1,
		level_up_exp  = 80000,
		next_skill_id = 104006
	};

%% 威震四方
skill_info(104006) ->
	#skill_info{
		mode_id       = 104006,
		class_id      = 104,
		type          = 1,
		effect        = 1,
		level_up_exp  = 120000,
		next_skill_id = 104007
	};

%% 威震四方
skill_info(104007) ->
	#skill_info{
		mode_id       = 104007,
		class_id      = 104,
		type          = 1,
		effect        = 1,
		level_up_exp  = 180000,
		next_skill_id = 104008
	};

%% 威震四方
skill_info(104008) ->
	#skill_info{
		mode_id       = 104008,
		class_id      = 104,
		type          = 1,
		effect        = 1,
		level_up_exp  = 240000,
		next_skill_id = 104009
	};

%% 威震四方
skill_info(104009) ->
	#skill_info{
		mode_id       = 104009,
		class_id      = 104,
		type          = 1,
		effect        = 1,
		level_up_exp  = 320000,
		next_skill_id = 104010
	};

%% 威震四方
skill_info(104010) ->
	#skill_info{
		mode_id       = 104010,
		class_id      = 104,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 坚若磐石
skill_info(105001) ->
	#skill_info{
		mode_id       = 105001,
		class_id      = 105,
		type          = 4,
		effect        = 1,
		level_up_exp  = 980,
		next_skill_id = 105002
	};

%% 坚若磐石
skill_info(105002) ->
	#skill_info{
		mode_id       = 105002,
		class_id      = 105,
		type          = 4,
		effect        = 1,
		level_up_exp  = 2300,
		next_skill_id = 105003
	};

%% 坚若磐石
skill_info(105003) ->
	#skill_info{
		mode_id       = 105003,
		class_id      = 105,
		type          = 4,
		effect        = 1,
		level_up_exp  = 7340,
		next_skill_id = 105004
	};

%% 坚若磐石
skill_info(105004) ->
	#skill_info{
		mode_id       = 105004,
		class_id      = 105,
		type          = 4,
		effect        = 1,
		level_up_exp  = 16150,
		next_skill_id = 105005
	};

%% 坚若磐石
skill_info(105005) ->
	#skill_info{
		mode_id       = 105005,
		class_id      = 105,
		type          = 4,
		effect        = 1,
		level_up_exp  = 39100,
		next_skill_id = 105006
	};

%% 坚若磐石
skill_info(105006) ->
	#skill_info{
		mode_id       = 105006,
		class_id      = 105,
		type          = 4,
		effect        = 1,
		level_up_exp  = 76500,
		next_skill_id = 105007
	};

%% 坚若磐石
skill_info(105007) ->
	#skill_info{
		mode_id       = 105007,
		class_id      = 105,
		type          = 4,
		effect        = 1,
		level_up_exp  = 154700,
		next_skill_id = 105008
	};

%% 坚若磐石
skill_info(105008) ->
	#skill_info{
		mode_id       = 105008,
		class_id      = 105,
		type          = 4,
		effect        = 1,
		level_up_exp  = 289000,
		next_skill_id = 105009
	};

%% 坚若磐石
skill_info(105009) ->
	#skill_info{
		mode_id       = 105009,
		class_id      = 105,
		type          = 4,
		effect        = 1,
		level_up_exp  = 527000,
		next_skill_id = 105010
	};

%% 坚若磐石
skill_info(105010) ->
	#skill_info{
		mode_id       = 105010,
		class_id      = 105,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 背水一战
skill_info(106001) ->
	#skill_info{
		mode_id       = 106001,
		class_id      = 106,
		type          = 4,
		effect        = 1,
		level_up_exp  = 490,
		next_skill_id = 106002
	};

%% 背水一战
skill_info(106002) ->
	#skill_info{
		mode_id       = 106002,
		class_id      = 106,
		type          = 4,
		effect        = 1,
		level_up_exp  = 2300,
		next_skill_id = 106003
	};

%% 背水一战
skill_info(106003) ->
	#skill_info{
		mode_id       = 106003,
		class_id      = 106,
		type          = 4,
		effect        = 1,
		level_up_exp  = 7340,
		next_skill_id = 106004
	};

%% 背水一战
skill_info(106004) ->
	#skill_info{
		mode_id       = 106004,
		class_id      = 106,
		type          = 4,
		effect        = 1,
		level_up_exp  = 16150,
		next_skill_id = 106005
	};

%% 背水一战
skill_info(106005) ->
	#skill_info{
		mode_id       = 106005,
		class_id      = 106,
		type          = 4,
		effect        = 1,
		level_up_exp  = 39100,
		next_skill_id = 106006
	};

%% 背水一战
skill_info(106006) ->
	#skill_info{
		mode_id       = 106006,
		class_id      = 106,
		type          = 4,
		effect        = 1,
		level_up_exp  = 76500,
		next_skill_id = 106007
	};

%% 背水一战
skill_info(106007) ->
	#skill_info{
		mode_id       = 106007,
		class_id      = 106,
		type          = 4,
		effect        = 1,
		level_up_exp  = 154700,
		next_skill_id = 106008
	};

%% 背水一战
skill_info(106008) ->
	#skill_info{
		mode_id       = 106008,
		class_id      = 106,
		type          = 4,
		effect        = 1,
		level_up_exp  = 289000,
		next_skill_id = 106009
	};

%% 背水一战
skill_info(106009) ->
	#skill_info{
		mode_id       = 106009,
		class_id      = 106,
		type          = 4,
		effect        = 1,
		level_up_exp  = 527000,
		next_skill_id = 106010
	};

%% 背水一战
skill_info(106010) ->
	#skill_info{
		mode_id       = 106010,
		class_id      = 106,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 浴血狂击
skill_info(107001) ->
	#skill_info{
		mode_id       = 107001,
		class_id      = 107,
		type          = 4,
		effect        = 1,
		level_up_exp  = 570,
		next_skill_id = 107002
	};

%% 浴血狂击
skill_info(107002) ->
	#skill_info{
		mode_id       = 107002,
		class_id      = 107,
		type          = 4,
		effect        = 1,
		level_up_exp  = 2700,
		next_skill_id = 107003
	};

%% 浴血狂击
skill_info(107003) ->
	#skill_info{
		mode_id       = 107003,
		class_id      = 107,
		type          = 4,
		effect        = 1,
		level_up_exp  = 8640,
		next_skill_id = 107004
	};

%% 浴血狂击
skill_info(107004) ->
	#skill_info{
		mode_id       = 107004,
		class_id      = 107,
		type          = 4,
		effect        = 1,
		level_up_exp  = 19000,
		next_skill_id = 107005
	};

%% 浴血狂击
skill_info(107005) ->
	#skill_info{
		mode_id       = 107005,
		class_id      = 107,
		type          = 4,
		effect        = 1,
		level_up_exp  = 46000,
		next_skill_id = 107006
	};

%% 浴血狂击
skill_info(107006) ->
	#skill_info{
		mode_id       = 107006,
		class_id      = 107,
		type          = 4,
		effect        = 1,
		level_up_exp  = 90000,
		next_skill_id = 107007
	};

%% 浴血狂击
skill_info(107007) ->
	#skill_info{
		mode_id       = 107007,
		class_id      = 107,
		type          = 4,
		effect        = 1,
		level_up_exp  = 182000,
		next_skill_id = 107008
	};

%% 浴血狂击
skill_info(107008) ->
	#skill_info{
		mode_id       = 107008,
		class_id      = 107,
		type          = 4,
		effect        = 1,
		level_up_exp  = 340000,
		next_skill_id = 107009
	};

%% 浴血狂击
skill_info(107009) ->
	#skill_info{
		mode_id       = 107009,
		class_id      = 107,
		type          = 4,
		effect        = 1,
		level_up_exp  = 620000,
		next_skill_id = 107010
	};

%% 浴血狂击
skill_info(107010) ->
	#skill_info{
		mode_id       = 107010,
		class_id      = 107,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 战意激荡
skill_info(108001) ->
	#skill_info{
		mode_id       = 108001,
		class_id      = 108,
		type          = 4,
		effect        = 1,
		level_up_exp  = 550,
		next_skill_id = 108002
	};

%% 战意激荡
skill_info(108002) ->
	#skill_info{
		mode_id       = 108002,
		class_id      = 108,
		type          = 4,
		effect        = 1,
		level_up_exp  = 2570,
		next_skill_id = 108003
	};

%% 战意激荡
skill_info(108003) ->
	#skill_info{
		mode_id       = 108003,
		class_id      = 108,
		type          = 4,
		effect        = 1,
		level_up_exp  = 8210,
		next_skill_id = 108004
	};

%% 战意激荡
skill_info(108004) ->
	#skill_info{
		mode_id       = 108004,
		class_id      = 108,
		type          = 4,
		effect        = 1,
		level_up_exp  = 18050,
		next_skill_id = 108005
	};

%% 战意激荡
skill_info(108005) ->
	#skill_info{
		mode_id       = 108005,
		class_id      = 108,
		type          = 4,
		effect        = 1,
		level_up_exp  = 43700,
		next_skill_id = 108006
	};

%% 战意激荡
skill_info(108006) ->
	#skill_info{
		mode_id       = 108006,
		class_id      = 108,
		type          = 4,
		effect        = 1,
		level_up_exp  = 85500,
		next_skill_id = 108007
	};

%% 战意激荡
skill_info(108007) ->
	#skill_info{
		mode_id       = 108007,
		class_id      = 108,
		type          = 4,
		effect        = 1,
		level_up_exp  = 172900,
		next_skill_id = 108008
	};

%% 战意激荡
skill_info(108008) ->
	#skill_info{
		mode_id       = 108008,
		class_id      = 108,
		type          = 4,
		effect        = 1,
		level_up_exp  = 323000,
		next_skill_id = 108009
	};

%% 战意激荡
skill_info(108009) ->
	#skill_info{
		mode_id       = 108009,
		class_id      = 108,
		type          = 4,
		effect        = 1,
		level_up_exp  = 589000,
		next_skill_id = 108010
	};

%% 战意激荡
skill_info(108010) ->
	#skill_info{
		mode_id       = 108010,
		class_id      = 108,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 霸刃连斩
skill_info(109001) ->
	#skill_info{
		mode_id       = 109001,
		class_id      = 109,
		type          = 1,
		effect        = 1,
		level_up_exp  = 4000,
		next_skill_id = 109002
	};

%% 霸刃连斩
skill_info(109002) ->
	#skill_info{
		mode_id       = 109002,
		class_id      = 109,
		type          = 1,
		effect        = 1,
		level_up_exp  = 11500,
		next_skill_id = 109003
	};

%% 霸刃连斩
skill_info(109003) ->
	#skill_info{
		mode_id       = 109003,
		class_id      = 109,
		type          = 1,
		effect        = 1,
		level_up_exp  = 20000,
		next_skill_id = 109004
	};

%% 霸刃连斩
skill_info(109004) ->
	#skill_info{
		mode_id       = 109004,
		class_id      = 109,
		type          = 1,
		effect        = 1,
		level_up_exp  = 36000,
		next_skill_id = 109005
	};

%% 霸刃连斩
skill_info(109005) ->
	#skill_info{
		mode_id       = 109005,
		class_id      = 109,
		type          = 1,
		effect        = 1,
		level_up_exp  = 80000,
		next_skill_id = 109006
	};

%% 霸刃连斩
skill_info(109006) ->
	#skill_info{
		mode_id       = 109006,
		class_id      = 109,
		type          = 1,
		effect        = 1,
		level_up_exp  = 120000,
		next_skill_id = 109007
	};

%% 霸刃连斩
skill_info(109007) ->
	#skill_info{
		mode_id       = 109007,
		class_id      = 109,
		type          = 1,
		effect        = 1,
		level_up_exp  = 180000,
		next_skill_id = 109008
	};

%% 霸刃连斩
skill_info(109008) ->
	#skill_info{
		mode_id       = 109008,
		class_id      = 109,
		type          = 1,
		effect        = 1,
		level_up_exp  = 240000,
		next_skill_id = 109009
	};

%% 霸刃连斩
skill_info(109009) ->
	#skill_info{
		mode_id       = 109009,
		class_id      = 109,
		type          = 1,
		effect        = 1,
		level_up_exp  = 320000,
		next_skill_id = 109010
	};

%% 霸刃连斩
skill_info(109010) ->
	#skill_info{
		mode_id       = 109010,
		class_id      = 109,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 横扫千军
skill_info(110001) ->
	#skill_info{
		mode_id       = 110001,
		class_id      = 110,
		type          = 4,
		effect        = 1,
		level_up_exp  = 980,
		next_skill_id = 110002
	};

%% 横扫千军
skill_info(110002) ->
	#skill_info{
		mode_id       = 110002,
		class_id      = 110,
		type          = 4,
		effect        = 1,
		level_up_exp  = 2300,
		next_skill_id = 110003
	};

%% 横扫千军
skill_info(110003) ->
	#skill_info{
		mode_id       = 110003,
		class_id      = 110,
		type          = 4,
		effect        = 1,
		level_up_exp  = 7340,
		next_skill_id = 110004
	};

%% 横扫千军
skill_info(110004) ->
	#skill_info{
		mode_id       = 110004,
		class_id      = 110,
		type          = 4,
		effect        = 1,
		level_up_exp  = 16150,
		next_skill_id = 110005
	};

%% 横扫千军
skill_info(110005) ->
	#skill_info{
		mode_id       = 110005,
		class_id      = 110,
		type          = 4,
		effect        = 1,
		level_up_exp  = 39100,
		next_skill_id = 110006
	};

%% 横扫千军
skill_info(110006) ->
	#skill_info{
		mode_id       = 110006,
		class_id      = 110,
		type          = 4,
		effect        = 1,
		level_up_exp  = 76500,
		next_skill_id = 110007
	};

%% 横扫千军
skill_info(110007) ->
	#skill_info{
		mode_id       = 110007,
		class_id      = 110,
		type          = 4,
		effect        = 1,
		level_up_exp  = 154700,
		next_skill_id = 110008
	};

%% 横扫千军
skill_info(110008) ->
	#skill_info{
		mode_id       = 110008,
		class_id      = 110,
		type          = 4,
		effect        = 1,
		level_up_exp  = 289000,
		next_skill_id = 110009
	};

%% 横扫千军
skill_info(110009) ->
	#skill_info{
		mode_id       = 110009,
		class_id      = 110,
		type          = 4,
		effect        = 1,
		level_up_exp  = 527000,
		next_skill_id = 110010
	};

%% 横扫千军
skill_info(110010) ->
	#skill_info{
		mode_id       = 110010,
		class_id      = 110,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 龙牙突刺
skill_info(111001) ->
	#skill_info{
		mode_id       = 111001,
		class_id      = 111,
		type          = 4,
		effect        = 1,
		level_up_exp  = 490,
		next_skill_id = 111002
	};

%% 龙牙突刺
skill_info(111002) ->
	#skill_info{
		mode_id       = 111002,
		class_id      = 111,
		type          = 4,
		effect        = 1,
		level_up_exp  = 2300,
		next_skill_id = 111003
	};

%% 龙牙突刺
skill_info(111003) ->
	#skill_info{
		mode_id       = 111003,
		class_id      = 111,
		type          = 4,
		effect        = 1,
		level_up_exp  = 7340,
		next_skill_id = 111004
	};

%% 龙牙突刺
skill_info(111004) ->
	#skill_info{
		mode_id       = 111004,
		class_id      = 111,
		type          = 4,
		effect        = 1,
		level_up_exp  = 16150,
		next_skill_id = 111005
	};

%% 龙牙突刺
skill_info(111005) ->
	#skill_info{
		mode_id       = 111005,
		class_id      = 111,
		type          = 4,
		effect        = 1,
		level_up_exp  = 39100,
		next_skill_id = 111006
	};

%% 龙牙突刺
skill_info(111006) ->
	#skill_info{
		mode_id       = 111006,
		class_id      = 111,
		type          = 4,
		effect        = 1,
		level_up_exp  = 76500,
		next_skill_id = 111007
	};

%% 龙牙突刺
skill_info(111007) ->
	#skill_info{
		mode_id       = 111007,
		class_id      = 111,
		type          = 4,
		effect        = 1,
		level_up_exp  = 154700,
		next_skill_id = 111008
	};

%% 龙牙突刺
skill_info(111008) ->
	#skill_info{
		mode_id       = 111008,
		class_id      = 111,
		type          = 4,
		effect        = 1,
		level_up_exp  = 289000,
		next_skill_id = 111009
	};

%% 龙牙突刺
skill_info(111009) ->
	#skill_info{
		mode_id       = 111009,
		class_id      = 111,
		type          = 4,
		effect        = 1,
		level_up_exp  = 527000,
		next_skill_id = 111010
	};

%% 龙牙突刺
skill_info(111010) ->
	#skill_info{
		mode_id       = 111010,
		class_id      = 111,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 乘胜追击
skill_info(112001) ->
	#skill_info{
		mode_id       = 112001,
		class_id      = 112,
		type          = 4,
		effect        = 1,
		level_up_exp  = 550,
		next_skill_id = 112002
	};

%% 乘胜追击
skill_info(112002) ->
	#skill_info{
		mode_id       = 112002,
		class_id      = 112,
		type          = 4,
		effect        = 1,
		level_up_exp  = 2570,
		next_skill_id = 112003
	};

%% 乘胜追击
skill_info(112003) ->
	#skill_info{
		mode_id       = 112003,
		class_id      = 112,
		type          = 4,
		effect        = 1,
		level_up_exp  = 8210,
		next_skill_id = 112004
	};

%% 乘胜追击
skill_info(112004) ->
	#skill_info{
		mode_id       = 112004,
		class_id      = 112,
		type          = 4,
		effect        = 1,
		level_up_exp  = 18050,
		next_skill_id = 112005
	};

%% 乘胜追击
skill_info(112005) ->
	#skill_info{
		mode_id       = 112005,
		class_id      = 112,
		type          = 4,
		effect        = 1,
		level_up_exp  = 43700,
		next_skill_id = 112006
	};

%% 乘胜追击
skill_info(112006) ->
	#skill_info{
		mode_id       = 112006,
		class_id      = 112,
		type          = 4,
		effect        = 1,
		level_up_exp  = 85500,
		next_skill_id = 112007
	};

%% 乘胜追击
skill_info(112007) ->
	#skill_info{
		mode_id       = 112007,
		class_id      = 112,
		type          = 4,
		effect        = 1,
		level_up_exp  = 172900,
		next_skill_id = 112008
	};

%% 乘胜追击
skill_info(112008) ->
	#skill_info{
		mode_id       = 112008,
		class_id      = 112,
		type          = 4,
		effect        = 1,
		level_up_exp  = 323000,
		next_skill_id = 112009
	};

%% 乘胜追击
skill_info(112009) ->
	#skill_info{
		mode_id       = 112009,
		class_id      = 112,
		type          = 4,
		effect        = 1,
		level_up_exp  = 589000,
		next_skill_id = 112010
	};

%% 乘胜追击
skill_info(112010) ->
	#skill_info{
		mode_id       = 112010,
		class_id      = 112,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 破军之势
skill_info(113001) ->
	#skill_info{
		mode_id       = 113001,
		class_id      = 113,
		type          = 4,
		effect        = 1,
		level_up_exp  = 570,
		next_skill_id = 113002
	};

%% 破军之势
skill_info(113002) ->
	#skill_info{
		mode_id       = 113002,
		class_id      = 113,
		type          = 4,
		effect        = 1,
		level_up_exp  = 2700,
		next_skill_id = 113003
	};

%% 破军之势
skill_info(113003) ->
	#skill_info{
		mode_id       = 113003,
		class_id      = 113,
		type          = 4,
		effect        = 1,
		level_up_exp  = 8640,
		next_skill_id = 113004
	};

%% 破军之势
skill_info(113004) ->
	#skill_info{
		mode_id       = 113004,
		class_id      = 113,
		type          = 4,
		effect        = 1,
		level_up_exp  = 19000,
		next_skill_id = 113005
	};

%% 破军之势
skill_info(113005) ->
	#skill_info{
		mode_id       = 113005,
		class_id      = 113,
		type          = 4,
		effect        = 1,
		level_up_exp  = 46000,
		next_skill_id = 113006
	};

%% 破军之势
skill_info(113006) ->
	#skill_info{
		mode_id       = 113006,
		class_id      = 113,
		type          = 4,
		effect        = 1,
		level_up_exp  = 90000,
		next_skill_id = 113007
	};

%% 破军之势
skill_info(113007) ->
	#skill_info{
		mode_id       = 113007,
		class_id      = 113,
		type          = 4,
		effect        = 1,
		level_up_exp  = 182000,
		next_skill_id = 113008
	};

%% 破军之势
skill_info(113008) ->
	#skill_info{
		mode_id       = 113008,
		class_id      = 113,
		type          = 4,
		effect        = 1,
		level_up_exp  = 340000,
		next_skill_id = 113009
	};

%% 破军之势
skill_info(113009) ->
	#skill_info{
		mode_id       = 113009,
		class_id      = 113,
		type          = 4,
		effect        = 1,
		level_up_exp  = 620000,
		next_skill_id = 113010
	};

%% 破军之势
skill_info(113010) ->
	#skill_info{
		mode_id       = 113010,
		class_id      = 113,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 龙战八方
skill_info(114001) ->
	#skill_info{
		mode_id       = 114001,
		class_id      = 114,
		type          = 1,
		effect        = 1,
		level_up_exp  = 4000,
		next_skill_id = 114002
	};

%% 龙战八方
skill_info(114002) ->
	#skill_info{
		mode_id       = 114002,
		class_id      = 114,
		type          = 1,
		effect        = 1,
		level_up_exp  = 11500,
		next_skill_id = 114003
	};

%% 龙战八方
skill_info(114003) ->
	#skill_info{
		mode_id       = 114003,
		class_id      = 114,
		type          = 1,
		effect        = 1,
		level_up_exp  = 20000,
		next_skill_id = 114004
	};

%% 龙战八方
skill_info(114004) ->
	#skill_info{
		mode_id       = 114004,
		class_id      = 114,
		type          = 1,
		effect        = 1,
		level_up_exp  = 36000,
		next_skill_id = 114005
	};

%% 龙战八方
skill_info(114005) ->
	#skill_info{
		mode_id       = 114005,
		class_id      = 114,
		type          = 1,
		effect        = 1,
		level_up_exp  = 80000,
		next_skill_id = 114006
	};

%% 龙战八方
skill_info(114006) ->
	#skill_info{
		mode_id       = 114006,
		class_id      = 114,
		type          = 1,
		effect        = 1,
		level_up_exp  = 120000,
		next_skill_id = 114007
	};

%% 龙战八方
skill_info(114007) ->
	#skill_info{
		mode_id       = 114007,
		class_id      = 114,
		type          = 1,
		effect        = 1,
		level_up_exp  = 180000,
		next_skill_id = 114008
	};

%% 龙战八方
skill_info(114008) ->
	#skill_info{
		mode_id       = 114008,
		class_id      = 114,
		type          = 1,
		effect        = 1,
		level_up_exp  = 240000,
		next_skill_id = 114009
	};

%% 龙战八方
skill_info(114009) ->
	#skill_info{
		mode_id       = 114009,
		class_id      = 114,
		type          = 1,
		effect        = 1,
		level_up_exp  = 320000,
		next_skill_id = 114010
	};

%% 龙战八方
skill_info(114010) ->
	#skill_info{
		mode_id       = 114010,
		class_id      = 114,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 奇门遁甲
skill_info(115001) ->
	#skill_info{
		mode_id       = 115001,
		class_id      = 115,
		type          = 4,
		effect        = 1,
		level_up_exp  = 980,
		next_skill_id = 115002
	};

%% 奇门遁甲
skill_info(115002) ->
	#skill_info{
		mode_id       = 115002,
		class_id      = 115,
		type          = 4,
		effect        = 1,
		level_up_exp  = 2300,
		next_skill_id = 115003
	};

%% 奇门遁甲
skill_info(115003) ->
	#skill_info{
		mode_id       = 115003,
		class_id      = 115,
		type          = 4,
		effect        = 1,
		level_up_exp  = 7340,
		next_skill_id = 115004
	};

%% 奇门遁甲
skill_info(115004) ->
	#skill_info{
		mode_id       = 115004,
		class_id      = 115,
		type          = 4,
		effect        = 1,
		level_up_exp  = 16150,
		next_skill_id = 115005
	};

%% 奇门遁甲
skill_info(115005) ->
	#skill_info{
		mode_id       = 115005,
		class_id      = 115,
		type          = 4,
		effect        = 1,
		level_up_exp  = 39100,
		next_skill_id = 115006
	};

%% 奇门遁甲
skill_info(115006) ->
	#skill_info{
		mode_id       = 115006,
		class_id      = 115,
		type          = 4,
		effect        = 1,
		level_up_exp  = 76500,
		next_skill_id = 115007
	};

%% 奇门遁甲
skill_info(115007) ->
	#skill_info{
		mode_id       = 115007,
		class_id      = 115,
		type          = 4,
		effect        = 1,
		level_up_exp  = 154700,
		next_skill_id = 115008
	};

%% 奇门遁甲
skill_info(115008) ->
	#skill_info{
		mode_id       = 115008,
		class_id      = 115,
		type          = 4,
		effect        = 1,
		level_up_exp  = 289000,
		next_skill_id = 115009
	};

%% 奇门遁甲
skill_info(115009) ->
	#skill_info{
		mode_id       = 115009,
		class_id      = 115,
		type          = 4,
		effect        = 1,
		level_up_exp  = 527000,
		next_skill_id = 115010
	};

%% 奇门遁甲
skill_info(115010) ->
	#skill_info{
		mode_id       = 115010,
		class_id      = 115,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 龙落雷
skill_info(116001) ->
	#skill_info{
		mode_id       = 116001,
		class_id      = 116,
		type          = 4,
		effect        = 1,
		level_up_exp  = 490,
		next_skill_id = 116002
	};

%% 龙落雷
skill_info(116002) ->
	#skill_info{
		mode_id       = 116002,
		class_id      = 116,
		type          = 4,
		effect        = 1,
		level_up_exp  = 2300,
		next_skill_id = 116003
	};

%% 龙落雷
skill_info(116003) ->
	#skill_info{
		mode_id       = 116003,
		class_id      = 116,
		type          = 4,
		effect        = 1,
		level_up_exp  = 7340,
		next_skill_id = 116004
	};

%% 龙落雷
skill_info(116004) ->
	#skill_info{
		mode_id       = 116004,
		class_id      = 116,
		type          = 4,
		effect        = 1,
		level_up_exp  = 16150,
		next_skill_id = 116005
	};

%% 龙落雷
skill_info(116005) ->
	#skill_info{
		mode_id       = 116005,
		class_id      = 116,
		type          = 4,
		effect        = 1,
		level_up_exp  = 39100,
		next_skill_id = 116006
	};

%% 龙落雷
skill_info(116006) ->
	#skill_info{
		mode_id       = 116006,
		class_id      = 116,
		type          = 4,
		effect        = 1,
		level_up_exp  = 76500,
		next_skill_id = 116007
	};

%% 龙落雷
skill_info(116007) ->
	#skill_info{
		mode_id       = 116007,
		class_id      = 116,
		type          = 4,
		effect        = 1,
		level_up_exp  = 154700,
		next_skill_id = 116008
	};

%% 龙落雷
skill_info(116008) ->
	#skill_info{
		mode_id       = 116008,
		class_id      = 116,
		type          = 4,
		effect        = 1,
		level_up_exp  = 289000,
		next_skill_id = 116009
	};

%% 龙落雷
skill_info(116009) ->
	#skill_info{
		mode_id       = 116009,
		class_id      = 116,
		type          = 4,
		effect        = 1,
		level_up_exp  = 527000,
		next_skill_id = 116010
	};

%% 龙落雷
skill_info(116010) ->
	#skill_info{
		mode_id       = 116010,
		class_id      = 116,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 虚空炽炎
skill_info(117001) ->
	#skill_info{
		mode_id       = 117001,
		class_id      = 117,
		type          = 4,
		effect        = 1,
		level_up_exp  = 550,
		next_skill_id = 117002
	};

%% 虚空炽炎
skill_info(117002) ->
	#skill_info{
		mode_id       = 117002,
		class_id      = 117,
		type          = 4,
		effect        = 1,
		level_up_exp  = 2570,
		next_skill_id = 117003
	};

%% 虚空炽炎
skill_info(117003) ->
	#skill_info{
		mode_id       = 117003,
		class_id      = 117,
		type          = 4,
		effect        = 1,
		level_up_exp  = 8210,
		next_skill_id = 117004
	};

%% 虚空炽炎
skill_info(117004) ->
	#skill_info{
		mode_id       = 117004,
		class_id      = 117,
		type          = 4,
		effect        = 1,
		level_up_exp  = 18050,
		next_skill_id = 117005
	};

%% 虚空炽炎
skill_info(117005) ->
	#skill_info{
		mode_id       = 117005,
		class_id      = 117,
		type          = 4,
		effect        = 1,
		level_up_exp  = 43700,
		next_skill_id = 117006
	};

%% 虚空炽炎
skill_info(117006) ->
	#skill_info{
		mode_id       = 117006,
		class_id      = 117,
		type          = 4,
		effect        = 1,
		level_up_exp  = 85500,
		next_skill_id = 117007
	};

%% 虚空炽炎
skill_info(117007) ->
	#skill_info{
		mode_id       = 117007,
		class_id      = 117,
		type          = 4,
		effect        = 1,
		level_up_exp  = 172900,
		next_skill_id = 117008
	};

%% 虚空炽炎
skill_info(117008) ->
	#skill_info{
		mode_id       = 117008,
		class_id      = 117,
		type          = 4,
		effect        = 1,
		level_up_exp  = 323000,
		next_skill_id = 117009
	};

%% 虚空炽炎
skill_info(117009) ->
	#skill_info{
		mode_id       = 117009,
		class_id      = 117,
		type          = 4,
		effect        = 1,
		level_up_exp  = 589000,
		next_skill_id = 117010
	};

%% 虚空炽炎
skill_info(117010) ->
	#skill_info{
		mode_id       = 117010,
		class_id      = 117,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 繁星流火
skill_info(118001) ->
	#skill_info{
		mode_id       = 118001,
		class_id      = 118,
		type          = 4,
		effect        = 1,
		level_up_exp  = 570,
		next_skill_id = 118002
	};

%% 繁星流火
skill_info(118002) ->
	#skill_info{
		mode_id       = 118002,
		class_id      = 118,
		type          = 4,
		effect        = 1,
		level_up_exp  = 2700,
		next_skill_id = 118003
	};

%% 繁星流火
skill_info(118003) ->
	#skill_info{
		mode_id       = 118003,
		class_id      = 118,
		type          = 4,
		effect        = 1,
		level_up_exp  = 8640,
		next_skill_id = 118004
	};

%% 繁星流火
skill_info(118004) ->
	#skill_info{
		mode_id       = 118004,
		class_id      = 118,
		type          = 4,
		effect        = 1,
		level_up_exp  = 19000,
		next_skill_id = 118005
	};

%% 繁星流火
skill_info(118005) ->
	#skill_info{
		mode_id       = 118005,
		class_id      = 118,
		type          = 4,
		effect        = 1,
		level_up_exp  = 46000,
		next_skill_id = 118006
	};

%% 繁星流火
skill_info(118006) ->
	#skill_info{
		mode_id       = 118006,
		class_id      = 118,
		type          = 4,
		effect        = 1,
		level_up_exp  = 90000,
		next_skill_id = 118007
	};

%% 繁星流火
skill_info(118007) ->
	#skill_info{
		mode_id       = 118007,
		class_id      = 118,
		type          = 4,
		effect        = 1,
		level_up_exp  = 182000,
		next_skill_id = 118008
	};

%% 繁星流火
skill_info(118008) ->
	#skill_info{
		mode_id       = 118008,
		class_id      = 118,
		type          = 4,
		effect        = 1,
		level_up_exp  = 340000,
		next_skill_id = 118009
	};

%% 繁星流火
skill_info(118009) ->
	#skill_info{
		mode_id       = 118009,
		class_id      = 118,
		type          = 4,
		effect        = 1,
		level_up_exp  = 620000,
		next_skill_id = 118010
	};

%% 繁星流火
skill_info(118010) ->
	#skill_info{
		mode_id       = 118010,
		class_id      = 118,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 战意激荡
skill_info(119001) ->
	#skill_info{
		mode_id       = 119001,
		class_id      = 119,
		type          = 4,
		effect        = 1,
		level_up_exp  = 550,
		next_skill_id = 119002
	};

%% 战意激荡
skill_info(119002) ->
	#skill_info{
		mode_id       = 119002,
		class_id      = 119,
		type          = 4,
		effect        = 1,
		level_up_exp  = 2570,
		next_skill_id = 119003
	};

%% 战意激荡
skill_info(119003) ->
	#skill_info{
		mode_id       = 119003,
		class_id      = 119,
		type          = 4,
		effect        = 1,
		level_up_exp  = 8210,
		next_skill_id = 119004
	};

%% 战意激荡
skill_info(119004) ->
	#skill_info{
		mode_id       = 119004,
		class_id      = 119,
		type          = 4,
		effect        = 1,
		level_up_exp  = 18050,
		next_skill_id = 119005
	};

%% 战意激荡
skill_info(119005) ->
	#skill_info{
		mode_id       = 119005,
		class_id      = 119,
		type          = 4,
		effect        = 1,
		level_up_exp  = 43700,
		next_skill_id = 119006
	};

%% 战意激荡
skill_info(119006) ->
	#skill_info{
		mode_id       = 119006,
		class_id      = 119,
		type          = 4,
		effect        = 1,
		level_up_exp  = 85500,
		next_skill_id = 119007
	};

%% 战意激荡
skill_info(119007) ->
	#skill_info{
		mode_id       = 119007,
		class_id      = 119,
		type          = 4,
		effect        = 1,
		level_up_exp  = 172900,
		next_skill_id = 119008
	};

%% 战意激荡
skill_info(119008) ->
	#skill_info{
		mode_id       = 119008,
		class_id      = 119,
		type          = 4,
		effect        = 1,
		level_up_exp  = 323000,
		next_skill_id = 119009
	};

%% 战意激荡
skill_info(119009) ->
	#skill_info{
		mode_id       = 119009,
		class_id      = 119,
		type          = 4,
		effect        = 1,
		level_up_exp  = 589000,
		next_skill_id = 119010
	};

%% 战意激荡
skill_info(119010) ->
	#skill_info{
		mode_id       = 119010,
		class_id      = 119,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 守护
skill_info(201001) ->
	#skill_info{
		mode_id       = 201001,
		class_id      = 201,
		type          = 3,
		effect        = 1,
		level_up_exp  = 600,
		next_skill_id = 201002
	};

%% 守护
skill_info(201002) ->
	#skill_info{
		mode_id       = 201002,
		class_id      = 201,
		type          = 3,
		effect        = 1,
		level_up_exp  = 900,
		next_skill_id = 201003
	};

%% 守护
skill_info(201003) ->
	#skill_info{
		mode_id       = 201003,
		class_id      = 201,
		type          = 3,
		effect        = 1,
		level_up_exp  = 2000,
		next_skill_id = 201004
	};

%% 守护
skill_info(201004) ->
	#skill_info{
		mode_id       = 201004,
		class_id      = 201,
		type          = 3,
		effect        = 1,
		level_up_exp  = 6000,
		next_skill_id = 201005
	};

%% 守护
skill_info(201005) ->
	#skill_info{
		mode_id       = 201005,
		class_id      = 201,
		type          = 3,
		effect        = 1,
		level_up_exp  = 13000,
		next_skill_id = 201006
	};

%% 守护
skill_info(201006) ->
	#skill_info{
		mode_id       = 201006,
		class_id      = 201,
		type          = 3,
		effect        = 1,
		level_up_exp  = 16000,
		next_skill_id = 201007
	};

%% 守护
skill_info(201007) ->
	#skill_info{
		mode_id       = 201007,
		class_id      = 201,
		type          = 3,
		effect        = 1,
		level_up_exp  = 20000,
		next_skill_id = 201008
	};

%% 守护
skill_info(201008) ->
	#skill_info{
		mode_id       = 201008,
		class_id      = 201,
		type          = 3,
		effect        = 1,
		level_up_exp  = 24000,
		next_skill_id = 201009
	};

%% 守护
skill_info(201009) ->
	#skill_info{
		mode_id       = 201009,
		class_id      = 201,
		type          = 3,
		effect        = 1,
		level_up_exp  = 30000,
		next_skill_id = 201010
	};

%% 守护
skill_info(201010) ->
	#skill_info{
		mode_id       = 201010,
		class_id      = 201,
		type          = 3,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 毒
skill_info(202001) ->
	#skill_info{
		mode_id       = 202001,
		class_id      = 202,
		type          = 3,
		effect        = 1,
		level_up_exp  = 600,
		next_skill_id = 202002
	};

%% 毒
skill_info(202002) ->
	#skill_info{
		mode_id       = 202002,
		class_id      = 202,
		type          = 3,
		effect        = 1,
		level_up_exp  = 900,
		next_skill_id = 202003
	};

%% 毒
skill_info(202003) ->
	#skill_info{
		mode_id       = 202003,
		class_id      = 202,
		type          = 3,
		effect        = 1,
		level_up_exp  = 2000,
		next_skill_id = 202004
	};

%% 毒
skill_info(202004) ->
	#skill_info{
		mode_id       = 202004,
		class_id      = 202,
		type          = 3,
		effect        = 1,
		level_up_exp  = 6000,
		next_skill_id = 202005
	};

%% 毒
skill_info(202005) ->
	#skill_info{
		mode_id       = 202005,
		class_id      = 202,
		type          = 3,
		effect        = 1,
		level_up_exp  = 13000,
		next_skill_id = 202006
	};

%% 毒
skill_info(202006) ->
	#skill_info{
		mode_id       = 202006,
		class_id      = 202,
		type          = 3,
		effect        = 1,
		level_up_exp  = 16000,
		next_skill_id = 202007
	};

%% 毒
skill_info(202007) ->
	#skill_info{
		mode_id       = 202007,
		class_id      = 202,
		type          = 3,
		effect        = 1,
		level_up_exp  = 20000,
		next_skill_id = 202008
	};

%% 毒
skill_info(202008) ->
	#skill_info{
		mode_id       = 202008,
		class_id      = 202,
		type          = 3,
		effect        = 1,
		level_up_exp  = 24000,
		next_skill_id = 202009
	};

%% 毒
skill_info(202009) ->
	#skill_info{
		mode_id       = 202009,
		class_id      = 202,
		type          = 3,
		effect        = 1,
		level_up_exp  = 30000,
		next_skill_id = 202010
	};

%% 毒
skill_info(202010) ->
	#skill_info{
		mode_id       = 202010,
		class_id      = 202,
		type          = 3,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 连击
skill_info(203001) ->
	#skill_info{
		mode_id       = 203001,
		class_id      = 203,
		type          = 3,
		effect        = 1,
		level_up_exp  = 600,
		next_skill_id = 203002
	};

%% 连击
skill_info(203002) ->
	#skill_info{
		mode_id       = 203002,
		class_id      = 203,
		type          = 3,
		effect        = 1,
		level_up_exp  = 900,
		next_skill_id = 203003
	};

%% 连击
skill_info(203003) ->
	#skill_info{
		mode_id       = 203003,
		class_id      = 203,
		type          = 3,
		effect        = 1,
		level_up_exp  = 2000,
		next_skill_id = 203004
	};

%% 连击
skill_info(203004) ->
	#skill_info{
		mode_id       = 203004,
		class_id      = 203,
		type          = 3,
		effect        = 1,
		level_up_exp  = 6000,
		next_skill_id = 203005
	};

%% 连击
skill_info(203005) ->
	#skill_info{
		mode_id       = 203005,
		class_id      = 203,
		type          = 3,
		effect        = 1,
		level_up_exp  = 13000,
		next_skill_id = 203006
	};

%% 连击
skill_info(203006) ->
	#skill_info{
		mode_id       = 203006,
		class_id      = 203,
		type          = 3,
		effect        = 1,
		level_up_exp  = 16000,
		next_skill_id = 203007
	};

%% 连击
skill_info(203007) ->
	#skill_info{
		mode_id       = 203007,
		class_id      = 203,
		type          = 3,
		effect        = 1,
		level_up_exp  = 20000,
		next_skill_id = 203008
	};

%% 连击
skill_info(203008) ->
	#skill_info{
		mode_id       = 203008,
		class_id      = 203,
		type          = 3,
		effect        = 1,
		level_up_exp  = 24000,
		next_skill_id = 203009
	};

%% 连击
skill_info(203009) ->
	#skill_info{
		mode_id       = 203009,
		class_id      = 203,
		type          = 3,
		effect        = 1,
		level_up_exp  = 30000,
		next_skill_id = 203010
	};

%% 连击
skill_info(203010) ->
	#skill_info{
		mode_id       = 203010,
		class_id      = 203,
		type          = 3,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 暴击
skill_info(204001) ->
	#skill_info{
		mode_id       = 204001,
		class_id      = 204,
		type          = 3,
		effect        = 2,
		level_up_exp  = 600,
		next_skill_id = 204002
	};

%% 暴击
skill_info(204002) ->
	#skill_info{
		mode_id       = 204002,
		class_id      = 204,
		type          = 3,
		effect        = 2,
		level_up_exp  = 900,
		next_skill_id = 204003
	};

%% 暴击
skill_info(204003) ->
	#skill_info{
		mode_id       = 204003,
		class_id      = 204,
		type          = 3,
		effect        = 2,
		level_up_exp  = 2000,
		next_skill_id = 204004
	};

%% 暴击
skill_info(204004) ->
	#skill_info{
		mode_id       = 204004,
		class_id      = 204,
		type          = 3,
		effect        = 2,
		level_up_exp  = 6000,
		next_skill_id = 204005
	};

%% 暴击
skill_info(204005) ->
	#skill_info{
		mode_id       = 204005,
		class_id      = 204,
		type          = 3,
		effect        = 2,
		level_up_exp  = 13000,
		next_skill_id = 204006
	};

%% 暴击
skill_info(204006) ->
	#skill_info{
		mode_id       = 204006,
		class_id      = 204,
		type          = 3,
		effect        = 2,
		level_up_exp  = 16000,
		next_skill_id = 204007
	};

%% 暴击
skill_info(204007) ->
	#skill_info{
		mode_id       = 204007,
		class_id      = 204,
		type          = 3,
		effect        = 2,
		level_up_exp  = 20000,
		next_skill_id = 204008
	};

%% 暴击
skill_info(204008) ->
	#skill_info{
		mode_id       = 204008,
		class_id      = 204,
		type          = 3,
		effect        = 2,
		level_up_exp  = 24000,
		next_skill_id = 204009
	};

%% 暴击
skill_info(204009) ->
	#skill_info{
		mode_id       = 204009,
		class_id      = 204,
		type          = 3,
		effect        = 2,
		level_up_exp  = 30000,
		next_skill_id = 204010
	};

%% 暴击
skill_info(204010) ->
	#skill_info{
		mode_id       = 204010,
		class_id      = 204,
		type          = 3,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 神佑复生
skill_info(205001) ->
	#skill_info{
		mode_id       = 205001,
		class_id      = 205,
		type          = 3,
		effect        = 1,
		level_up_exp  = 600,
		next_skill_id = 205002
	};

%% 神佑复生
skill_info(205002) ->
	#skill_info{
		mode_id       = 205002,
		class_id      = 205,
		type          = 3,
		effect        = 1,
		level_up_exp  = 900,
		next_skill_id = 205003
	};

%% 神佑复生
skill_info(205003) ->
	#skill_info{
		mode_id       = 205003,
		class_id      = 205,
		type          = 3,
		effect        = 1,
		level_up_exp  = 2000,
		next_skill_id = 205004
	};

%% 神佑复生
skill_info(205004) ->
	#skill_info{
		mode_id       = 205004,
		class_id      = 205,
		type          = 3,
		effect        = 1,
		level_up_exp  = 6000,
		next_skill_id = 205005
	};

%% 神佑复生
skill_info(205005) ->
	#skill_info{
		mode_id       = 205005,
		class_id      = 205,
		type          = 3,
		effect        = 1,
		level_up_exp  = 13000,
		next_skill_id = 205006
	};

%% 神佑复生
skill_info(205006) ->
	#skill_info{
		mode_id       = 205006,
		class_id      = 205,
		type          = 3,
		effect        = 1,
		level_up_exp  = 16000,
		next_skill_id = 205007
	};

%% 神佑复生
skill_info(205007) ->
	#skill_info{
		mode_id       = 205007,
		class_id      = 205,
		type          = 3,
		effect        = 1,
		level_up_exp  = 20000,
		next_skill_id = 205008
	};

%% 神佑复生
skill_info(205008) ->
	#skill_info{
		mode_id       = 205008,
		class_id      = 205,
		type          = 3,
		effect        = 1,
		level_up_exp  = 24000,
		next_skill_id = 205009
	};

%% 神佑复生
skill_info(205009) ->
	#skill_info{
		mode_id       = 205009,
		class_id      = 205,
		type          = 3,
		effect        = 1,
		level_up_exp  = 30000,
		next_skill_id = 205010
	};

%% 神佑复生
skill_info(205010) ->
	#skill_info{
		mode_id       = 205010,
		class_id      = 205,
		type          = 3,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 吸血
skill_info(206001) ->
	#skill_info{
		mode_id       = 206001,
		class_id      = 206,
		type          = 3,
		effect        = 1,
		level_up_exp  = 600,
		next_skill_id = 206002
	};

%% 吸血
skill_info(206002) ->
	#skill_info{
		mode_id       = 206002,
		class_id      = 206,
		type          = 3,
		effect        = 1,
		level_up_exp  = 900,
		next_skill_id = 206003
	};

%% 吸血
skill_info(206003) ->
	#skill_info{
		mode_id       = 206003,
		class_id      = 206,
		type          = 3,
		effect        = 1,
		level_up_exp  = 2000,
		next_skill_id = 206004
	};

%% 吸血
skill_info(206004) ->
	#skill_info{
		mode_id       = 206004,
		class_id      = 206,
		type          = 3,
		effect        = 1,
		level_up_exp  = 6000,
		next_skill_id = 206005
	};

%% 吸血
skill_info(206005) ->
	#skill_info{
		mode_id       = 206005,
		class_id      = 206,
		type          = 3,
		effect        = 1,
		level_up_exp  = 13000,
		next_skill_id = 206006
	};

%% 吸血
skill_info(206006) ->
	#skill_info{
		mode_id       = 206006,
		class_id      = 206,
		type          = 3,
		effect        = 1,
		level_up_exp  = 16000,
		next_skill_id = 206007
	};

%% 吸血
skill_info(206007) ->
	#skill_info{
		mode_id       = 206007,
		class_id      = 206,
		type          = 3,
		effect        = 1,
		level_up_exp  = 20000,
		next_skill_id = 206008
	};

%% 吸血
skill_info(206008) ->
	#skill_info{
		mode_id       = 206008,
		class_id      = 206,
		type          = 3,
		effect        = 1,
		level_up_exp  = 24000,
		next_skill_id = 206009
	};

%% 吸血
skill_info(206009) ->
	#skill_info{
		mode_id       = 206009,
		class_id      = 206,
		type          = 3,
		effect        = 1,
		level_up_exp  = 30000,
		next_skill_id = 206010
	};

%% 吸血
skill_info(206010) ->
	#skill_info{
		mode_id       = 206010,
		class_id      = 206,
		type          = 3,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 破怒
skill_info(207001) ->
	#skill_info{
		mode_id       = 207001,
		class_id      = 207,
		type          = 3,
		effect        = 1,
		level_up_exp  = 600,
		next_skill_id = 207002
	};

%% 破怒
skill_info(207002) ->
	#skill_info{
		mode_id       = 207002,
		class_id      = 207,
		type          = 3,
		effect        = 1,
		level_up_exp  = 900,
		next_skill_id = 207003
	};

%% 破怒
skill_info(207003) ->
	#skill_info{
		mode_id       = 207003,
		class_id      = 207,
		type          = 3,
		effect        = 1,
		level_up_exp  = 2000,
		next_skill_id = 207004
	};

%% 破怒
skill_info(207004) ->
	#skill_info{
		mode_id       = 207004,
		class_id      = 207,
		type          = 3,
		effect        = 1,
		level_up_exp  = 6000,
		next_skill_id = 207005
	};

%% 破怒
skill_info(207005) ->
	#skill_info{
		mode_id       = 207005,
		class_id      = 207,
		type          = 3,
		effect        = 1,
		level_up_exp  = 13000,
		next_skill_id = 207006
	};

%% 破怒
skill_info(207006) ->
	#skill_info{
		mode_id       = 207006,
		class_id      = 207,
		type          = 3,
		effect        = 1,
		level_up_exp  = 16000,
		next_skill_id = 207007
	};

%% 破怒
skill_info(207007) ->
	#skill_info{
		mode_id       = 207007,
		class_id      = 207,
		type          = 3,
		effect        = 1,
		level_up_exp  = 20000,
		next_skill_id = 207008
	};

%% 破怒
skill_info(207008) ->
	#skill_info{
		mode_id       = 207008,
		class_id      = 207,
		type          = 3,
		effect        = 1,
		level_up_exp  = 24000,
		next_skill_id = 207009
	};

%% 破怒
skill_info(207009) ->
	#skill_info{
		mode_id       = 207009,
		class_id      = 207,
		type          = 3,
		effect        = 1,
		level_up_exp  = 30000,
		next_skill_id = 207010
	};

%% 破怒
skill_info(207010) ->
	#skill_info{
		mode_id       = 207010,
		class_id      = 207,
		type          = 3,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 坚盾
skill_info(208001) ->
	#skill_info{
		mode_id       = 208001,
		class_id      = 208,
		type          = 3,
		effect        = 2,
		level_up_exp  = 600,
		next_skill_id = 208002
	};

%% 坚盾
skill_info(208002) ->
	#skill_info{
		mode_id       = 208002,
		class_id      = 208,
		type          = 3,
		effect        = 2,
		level_up_exp  = 900,
		next_skill_id = 208003
	};

%% 坚盾
skill_info(208003) ->
	#skill_info{
		mode_id       = 208003,
		class_id      = 208,
		type          = 3,
		effect        = 2,
		level_up_exp  = 2000,
		next_skill_id = 208004
	};

%% 坚盾
skill_info(208004) ->
	#skill_info{
		mode_id       = 208004,
		class_id      = 208,
		type          = 3,
		effect        = 2,
		level_up_exp  = 6000,
		next_skill_id = 208005
	};

%% 坚盾
skill_info(208005) ->
	#skill_info{
		mode_id       = 208005,
		class_id      = 208,
		type          = 3,
		effect        = 2,
		level_up_exp  = 13000,
		next_skill_id = 208006
	};

%% 坚盾
skill_info(208006) ->
	#skill_info{
		mode_id       = 208006,
		class_id      = 208,
		type          = 3,
		effect        = 2,
		level_up_exp  = 16000,
		next_skill_id = 208007
	};

%% 坚盾
skill_info(208007) ->
	#skill_info{
		mode_id       = 208007,
		class_id      = 208,
		type          = 3,
		effect        = 2,
		level_up_exp  = 20000,
		next_skill_id = 208008
	};

%% 坚盾
skill_info(208008) ->
	#skill_info{
		mode_id       = 208008,
		class_id      = 208,
		type          = 3,
		effect        = 2,
		level_up_exp  = 24000,
		next_skill_id = 208009
	};

%% 坚盾
skill_info(208009) ->
	#skill_info{
		mode_id       = 208009,
		class_id      = 208,
		type          = 3,
		effect        = 2,
		level_up_exp  = 30000,
		next_skill_id = 208010
	};

%% 坚盾
skill_info(208010) ->
	#skill_info{
		mode_id       = 208010,
		class_id      = 208,
		type          = 3,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 法盾
skill_info(209001) ->
	#skill_info{
		mode_id       = 209001,
		class_id      = 209,
		type          = 3,
		effect        = 2,
		level_up_exp  = 600,
		next_skill_id = 209002
	};

%% 法盾
skill_info(209002) ->
	#skill_info{
		mode_id       = 209002,
		class_id      = 209,
		type          = 3,
		effect        = 2,
		level_up_exp  = 900,
		next_skill_id = 209003
	};

%% 法盾
skill_info(209003) ->
	#skill_info{
		mode_id       = 209003,
		class_id      = 209,
		type          = 3,
		effect        = 2,
		level_up_exp  = 2000,
		next_skill_id = 209004
	};

%% 法盾
skill_info(209004) ->
	#skill_info{
		mode_id       = 209004,
		class_id      = 209,
		type          = 3,
		effect        = 2,
		level_up_exp  = 6000,
		next_skill_id = 209005
	};

%% 法盾
skill_info(209005) ->
	#skill_info{
		mode_id       = 209005,
		class_id      = 209,
		type          = 3,
		effect        = 2,
		level_up_exp  = 13000,
		next_skill_id = 209006
	};

%% 法盾
skill_info(209006) ->
	#skill_info{
		mode_id       = 209006,
		class_id      = 209,
		type          = 3,
		effect        = 2,
		level_up_exp  = 16000,
		next_skill_id = 209007
	};

%% 法盾
skill_info(209007) ->
	#skill_info{
		mode_id       = 209007,
		class_id      = 209,
		type          = 3,
		effect        = 2,
		level_up_exp  = 20000,
		next_skill_id = 209008
	};

%% 法盾
skill_info(209008) ->
	#skill_info{
		mode_id       = 209008,
		class_id      = 209,
		type          = 3,
		effect        = 2,
		level_up_exp  = 24000,
		next_skill_id = 209009
	};

%% 法盾
skill_info(209009) ->
	#skill_info{
		mode_id       = 209009,
		class_id      = 209,
		type          = 3,
		effect        = 2,
		level_up_exp  = 30000,
		next_skill_id = 209010
	};

%% 法盾
skill_info(209010) ->
	#skill_info{
		mode_id       = 209010,
		class_id      = 209,
		type          = 3,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 敏捷
skill_info(210001) ->
	#skill_info{
		mode_id       = 210001,
		class_id      = 210,
		type          = 3,
		effect        = 2,
		level_up_exp  = 600,
		next_skill_id = 210002
	};

%% 敏捷
skill_info(210002) ->
	#skill_info{
		mode_id       = 210002,
		class_id      = 210,
		type          = 3,
		effect        = 2,
		level_up_exp  = 900,
		next_skill_id = 210003
	};

%% 敏捷
skill_info(210003) ->
	#skill_info{
		mode_id       = 210003,
		class_id      = 210,
		type          = 3,
		effect        = 2,
		level_up_exp  = 2000,
		next_skill_id = 210004
	};

%% 敏捷
skill_info(210004) ->
	#skill_info{
		mode_id       = 210004,
		class_id      = 210,
		type          = 3,
		effect        = 2,
		level_up_exp  = 6000,
		next_skill_id = 210005
	};

%% 敏捷
skill_info(210005) ->
	#skill_info{
		mode_id       = 210005,
		class_id      = 210,
		type          = 3,
		effect        = 2,
		level_up_exp  = 13000,
		next_skill_id = 210006
	};

%% 敏捷
skill_info(210006) ->
	#skill_info{
		mode_id       = 210006,
		class_id      = 210,
		type          = 3,
		effect        = 2,
		level_up_exp  = 16000,
		next_skill_id = 210007
	};

%% 敏捷
skill_info(210007) ->
	#skill_info{
		mode_id       = 210007,
		class_id      = 210,
		type          = 3,
		effect        = 2,
		level_up_exp  = 20000,
		next_skill_id = 210008
	};

%% 敏捷
skill_info(210008) ->
	#skill_info{
		mode_id       = 210008,
		class_id      = 210,
		type          = 3,
		effect        = 2,
		level_up_exp  = 24000,
		next_skill_id = 210009
	};

%% 敏捷
skill_info(210009) ->
	#skill_info{
		mode_id       = 210009,
		class_id      = 210,
		type          = 3,
		effect        = 2,
		level_up_exp  = 30000,
		next_skill_id = 210010
	};

%% 敏捷
skill_info(210010) ->
	#skill_info{
		mode_id       = 210010,
		class_id      = 210,
		type          = 3,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 强力
skill_info(211001) ->
	#skill_info{
		mode_id       = 211001,
		class_id      = 211,
		type          = 3,
		effect        = 2,
		level_up_exp  = 600,
		next_skill_id = 211002
	};

%% 强力
skill_info(211002) ->
	#skill_info{
		mode_id       = 211002,
		class_id      = 211,
		type          = 3,
		effect        = 2,
		level_up_exp  = 900,
		next_skill_id = 211003
	};

%% 强力
skill_info(211003) ->
	#skill_info{
		mode_id       = 211003,
		class_id      = 211,
		type          = 3,
		effect        = 2,
		level_up_exp  = 2000,
		next_skill_id = 211004
	};

%% 强力
skill_info(211004) ->
	#skill_info{
		mode_id       = 211004,
		class_id      = 211,
		type          = 3,
		effect        = 2,
		level_up_exp  = 6000,
		next_skill_id = 211005
	};

%% 强力
skill_info(211005) ->
	#skill_info{
		mode_id       = 211005,
		class_id      = 211,
		type          = 3,
		effect        = 2,
		level_up_exp  = 13000,
		next_skill_id = 211006
	};

%% 强力
skill_info(211006) ->
	#skill_info{
		mode_id       = 211006,
		class_id      = 211,
		type          = 3,
		effect        = 2,
		level_up_exp  = 16000,
		next_skill_id = 211007
	};

%% 强力
skill_info(211007) ->
	#skill_info{
		mode_id       = 211007,
		class_id      = 211,
		type          = 3,
		effect        = 2,
		level_up_exp  = 20000,
		next_skill_id = 211008
	};

%% 强力
skill_info(211008) ->
	#skill_info{
		mode_id       = 211008,
		class_id      = 211,
		type          = 3,
		effect        = 2,
		level_up_exp  = 24000,
		next_skill_id = 211009
	};

%% 强力
skill_info(211009) ->
	#skill_info{
		mode_id       = 211009,
		class_id      = 211,
		type          = 3,
		effect        = 2,
		level_up_exp  = 30000,
		next_skill_id = 211010
	};

%% 强力
skill_info(211010) ->
	#skill_info{
		mode_id       = 211010,
		class_id      = 211,
		type          = 3,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 强法
skill_info(212001) ->
	#skill_info{
		mode_id       = 212001,
		class_id      = 212,
		type          = 3,
		effect        = 2,
		level_up_exp  = 600,
		next_skill_id = 212002
	};

%% 强法
skill_info(212002) ->
	#skill_info{
		mode_id       = 212002,
		class_id      = 212,
		type          = 3,
		effect        = 2,
		level_up_exp  = 900,
		next_skill_id = 212003
	};

%% 强法
skill_info(212003) ->
	#skill_info{
		mode_id       = 212003,
		class_id      = 212,
		type          = 3,
		effect        = 2,
		level_up_exp  = 2000,
		next_skill_id = 212004
	};

%% 强法
skill_info(212004) ->
	#skill_info{
		mode_id       = 212004,
		class_id      = 212,
		type          = 3,
		effect        = 2,
		level_up_exp  = 6000,
		next_skill_id = 212005
	};

%% 强法
skill_info(212005) ->
	#skill_info{
		mode_id       = 212005,
		class_id      = 212,
		type          = 3,
		effect        = 2,
		level_up_exp  = 13000,
		next_skill_id = 212006
	};

%% 强法
skill_info(212006) ->
	#skill_info{
		mode_id       = 212006,
		class_id      = 212,
		type          = 3,
		effect        = 2,
		level_up_exp  = 16000,
		next_skill_id = 212007
	};

%% 强法
skill_info(212007) ->
	#skill_info{
		mode_id       = 212007,
		class_id      = 212,
		type          = 3,
		effect        = 2,
		level_up_exp  = 20000,
		next_skill_id = 212008
	};

%% 强法
skill_info(212008) ->
	#skill_info{
		mode_id       = 212008,
		class_id      = 212,
		type          = 3,
		effect        = 2,
		level_up_exp  = 24000,
		next_skill_id = 212009
	};

%% 强法
skill_info(212009) ->
	#skill_info{
		mode_id       = 212009,
		class_id      = 212,
		type          = 3,
		effect        = 2,
		level_up_exp  = 30000,
		next_skill_id = 212010
	};

%% 强法
skill_info(212010) ->
	#skill_info{
		mode_id       = 212010,
		class_id      = 212,
		type          = 3,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 强身
skill_info(213001) ->
	#skill_info{
		mode_id       = 213001,
		class_id      = 213,
		type          = 3,
		effect        = 2,
		level_up_exp  = 600,
		next_skill_id = 213002
	};

%% 强身
skill_info(213002) ->
	#skill_info{
		mode_id       = 213002,
		class_id      = 213,
		type          = 3,
		effect        = 2,
		level_up_exp  = 900,
		next_skill_id = 213003
	};

%% 强身
skill_info(213003) ->
	#skill_info{
		mode_id       = 213003,
		class_id      = 213,
		type          = 3,
		effect        = 2,
		level_up_exp  = 2000,
		next_skill_id = 213004
	};

%% 强身
skill_info(213004) ->
	#skill_info{
		mode_id       = 213004,
		class_id      = 213,
		type          = 3,
		effect        = 2,
		level_up_exp  = 6000,
		next_skill_id = 213005
	};

%% 强身
skill_info(213005) ->
	#skill_info{
		mode_id       = 213005,
		class_id      = 213,
		type          = 3,
		effect        = 2,
		level_up_exp  = 13000,
		next_skill_id = 213006
	};

%% 强身
skill_info(213006) ->
	#skill_info{
		mode_id       = 213006,
		class_id      = 213,
		type          = 3,
		effect        = 2,
		level_up_exp  = 16000,
		next_skill_id = 213007
	};

%% 强身
skill_info(213007) ->
	#skill_info{
		mode_id       = 213007,
		class_id      = 213,
		type          = 3,
		effect        = 2,
		level_up_exp  = 20000,
		next_skill_id = 213008
	};

%% 强身
skill_info(213008) ->
	#skill_info{
		mode_id       = 213008,
		class_id      = 213,
		type          = 3,
		effect        = 2,
		level_up_exp  = 24000,
		next_skill_id = 213009
	};

%% 强身
skill_info(213009) ->
	#skill_info{
		mode_id       = 213009,
		class_id      = 213,
		type          = 3,
		effect        = 2,
		level_up_exp  = 30000,
		next_skill_id = 213010
	};

%% 强身
skill_info(213010) ->
	#skill_info{
		mode_id       = 213010,
		class_id      = 213,
		type          = 3,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 愤怒
skill_info(214001) ->
	#skill_info{
		mode_id       = 214001,
		class_id      = 214,
		type          = 3,
		effect        = 1,
		level_up_exp  = 600,
		next_skill_id = 214002
	};

%% 愤怒
skill_info(214002) ->
	#skill_info{
		mode_id       = 214002,
		class_id      = 214,
		type          = 3,
		effect        = 1,
		level_up_exp  = 900,
		next_skill_id = 214003
	};

%% 愤怒
skill_info(214003) ->
	#skill_info{
		mode_id       = 214003,
		class_id      = 214,
		type          = 3,
		effect        = 1,
		level_up_exp  = 2000,
		next_skill_id = 214004
	};

%% 愤怒
skill_info(214004) ->
	#skill_info{
		mode_id       = 214004,
		class_id      = 214,
		type          = 3,
		effect        = 1,
		level_up_exp  = 6000,
		next_skill_id = 214005
	};

%% 愤怒
skill_info(214005) ->
	#skill_info{
		mode_id       = 214005,
		class_id      = 214,
		type          = 3,
		effect        = 1,
		level_up_exp  = 13000,
		next_skill_id = 214006
	};

%% 愤怒
skill_info(214006) ->
	#skill_info{
		mode_id       = 214006,
		class_id      = 214,
		type          = 3,
		effect        = 1,
		level_up_exp  = 16000,
		next_skill_id = 214007
	};

%% 愤怒
skill_info(214007) ->
	#skill_info{
		mode_id       = 214007,
		class_id      = 214,
		type          = 3,
		effect        = 1,
		level_up_exp  = 20000,
		next_skill_id = 214008
	};

%% 愤怒
skill_info(214008) ->
	#skill_info{
		mode_id       = 214008,
		class_id      = 214,
		type          = 3,
		effect        = 1,
		level_up_exp  = 24000,
		next_skill_id = 214009
	};

%% 愤怒
skill_info(214009) ->
	#skill_info{
		mode_id       = 214009,
		class_id      = 214,
		type          = 3,
		effect        = 1,
		level_up_exp  = 30000,
		next_skill_id = 214010
	};

%% 愤怒
skill_info(214010) ->
	#skill_info{
		mode_id       = 214010,
		class_id      = 214,
		type          = 3,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 格挡
skill_info(215001) ->
	#skill_info{
		mode_id       = 215001,
		class_id      = 215,
		type          = 3,
		effect        = 2,
		level_up_exp  = 600,
		next_skill_id = 215002
	};

%% 格挡
skill_info(215002) ->
	#skill_info{
		mode_id       = 215002,
		class_id      = 215,
		type          = 3,
		effect        = 2,
		level_up_exp  = 900,
		next_skill_id = 215003
	};

%% 格挡
skill_info(215003) ->
	#skill_info{
		mode_id       = 215003,
		class_id      = 215,
		type          = 3,
		effect        = 2,
		level_up_exp  = 2000,
		next_skill_id = 215004
	};

%% 格挡
skill_info(215004) ->
	#skill_info{
		mode_id       = 215004,
		class_id      = 215,
		type          = 3,
		effect        = 2,
		level_up_exp  = 6000,
		next_skill_id = 215005
	};

%% 格挡
skill_info(215005) ->
	#skill_info{
		mode_id       = 215005,
		class_id      = 215,
		type          = 3,
		effect        = 2,
		level_up_exp  = 13000,
		next_skill_id = 215006
	};

%% 格挡
skill_info(215006) ->
	#skill_info{
		mode_id       = 215006,
		class_id      = 215,
		type          = 3,
		effect        = 2,
		level_up_exp  = 16000,
		next_skill_id = 215007
	};

%% 格挡
skill_info(215007) ->
	#skill_info{
		mode_id       = 215007,
		class_id      = 215,
		type          = 3,
		effect        = 2,
		level_up_exp  = 20000,
		next_skill_id = 215008
	};

%% 格挡
skill_info(215008) ->
	#skill_info{
		mode_id       = 215008,
		class_id      = 215,
		type          = 3,
		effect        = 2,
		level_up_exp  = 24000,
		next_skill_id = 215009
	};

%% 格挡
skill_info(215009) ->
	#skill_info{
		mode_id       = 215009,
		class_id      = 215,
		type          = 3,
		effect        = 2,
		level_up_exp  = 30000,
		next_skill_id = 215010
	};

%% 格挡
skill_info(215010) ->
	#skill_info{
		mode_id       = 215010,
		class_id      = 215,
		type          = 3,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 闪避
skill_info(216001) ->
	#skill_info{
		mode_id       = 216001,
		class_id      = 216,
		type          = 3,
		effect        = 2,
		level_up_exp  = 600,
		next_skill_id = 216002
	};

%% 闪避
skill_info(216002) ->
	#skill_info{
		mode_id       = 216002,
		class_id      = 216,
		type          = 3,
		effect        = 2,
		level_up_exp  = 900,
		next_skill_id = 216003
	};

%% 闪避
skill_info(216003) ->
	#skill_info{
		mode_id       = 216003,
		class_id      = 216,
		type          = 3,
		effect        = 2,
		level_up_exp  = 2000,
		next_skill_id = 216004
	};

%% 闪避
skill_info(216004) ->
	#skill_info{
		mode_id       = 216004,
		class_id      = 216,
		type          = 3,
		effect        = 2,
		level_up_exp  = 6000,
		next_skill_id = 216005
	};

%% 闪避
skill_info(216005) ->
	#skill_info{
		mode_id       = 216005,
		class_id      = 216,
		type          = 3,
		effect        = 2,
		level_up_exp  = 13000,
		next_skill_id = 216006
	};

%% 闪避
skill_info(216006) ->
	#skill_info{
		mode_id       = 216006,
		class_id      = 216,
		type          = 3,
		effect        = 2,
		level_up_exp  = 16000,
		next_skill_id = 216007
	};

%% 闪避
skill_info(216007) ->
	#skill_info{
		mode_id       = 216007,
		class_id      = 216,
		type          = 3,
		effect        = 2,
		level_up_exp  = 20000,
		next_skill_id = 216008
	};

%% 闪避
skill_info(216008) ->
	#skill_info{
		mode_id       = 216008,
		class_id      = 216,
		type          = 3,
		effect        = 2,
		level_up_exp  = 24000,
		next_skill_id = 216009
	};

%% 闪避
skill_info(216009) ->
	#skill_info{
		mode_id       = 216009,
		class_id      = 216,
		type          = 3,
		effect        = 2,
		level_up_exp  = 30000,
		next_skill_id = 216010
	};

%% 闪避
skill_info(216010) ->
	#skill_info{
		mode_id       = 216010,
		class_id      = 216,
		type          = 3,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 命中
skill_info(217001) ->
	#skill_info{
		mode_id       = 217001,
		class_id      = 217,
		type          = 3,
		effect        = 2,
		level_up_exp  = 600,
		next_skill_id = 217002
	};

%% 命中
skill_info(217002) ->
	#skill_info{
		mode_id       = 217002,
		class_id      = 217,
		type          = 3,
		effect        = 2,
		level_up_exp  = 900,
		next_skill_id = 217003
	};

%% 命中
skill_info(217003) ->
	#skill_info{
		mode_id       = 217003,
		class_id      = 217,
		type          = 3,
		effect        = 2,
		level_up_exp  = 2000,
		next_skill_id = 217004
	};

%% 命中
skill_info(217004) ->
	#skill_info{
		mode_id       = 217004,
		class_id      = 217,
		type          = 3,
		effect        = 2,
		level_up_exp  = 6000,
		next_skill_id = 217005
	};

%% 命中
skill_info(217005) ->
	#skill_info{
		mode_id       = 217005,
		class_id      = 217,
		type          = 3,
		effect        = 2,
		level_up_exp  = 13000,
		next_skill_id = 217006
	};

%% 命中
skill_info(217006) ->
	#skill_info{
		mode_id       = 217006,
		class_id      = 217,
		type          = 3,
		effect        = 2,
		level_up_exp  = 16000,
		next_skill_id = 217007
	};

%% 命中
skill_info(217007) ->
	#skill_info{
		mode_id       = 217007,
		class_id      = 217,
		type          = 3,
		effect        = 2,
		level_up_exp  = 20000,
		next_skill_id = 217008
	};

%% 命中
skill_info(217008) ->
	#skill_info{
		mode_id       = 217008,
		class_id      = 217,
		type          = 3,
		effect        = 2,
		level_up_exp  = 24000,
		next_skill_id = 217009
	};

%% 命中
skill_info(217009) ->
	#skill_info{
		mode_id       = 217009,
		class_id      = 217,
		type          = 3,
		effect        = 2,
		level_up_exp  = 30000,
		next_skill_id = 217010
	};

%% 命中
skill_info(217010) ->
	#skill_info{
		mode_id       = 217010,
		class_id      = 217,
		type          = 3,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 幸运
skill_info(218001) ->
	#skill_info{
		mode_id       = 218001,
		class_id      = 218,
		type          = 3,
		effect        = 2,
		level_up_exp  = 600,
		next_skill_id = 218002
	};

%% 幸运
skill_info(218002) ->
	#skill_info{
		mode_id       = 218002,
		class_id      = 218,
		type          = 3,
		effect        = 2,
		level_up_exp  = 900,
		next_skill_id = 218003
	};

%% 幸运
skill_info(218003) ->
	#skill_info{
		mode_id       = 218003,
		class_id      = 218,
		type          = 3,
		effect        = 2,
		level_up_exp  = 2000,
		next_skill_id = 218004
	};

%% 幸运
skill_info(218004) ->
	#skill_info{
		mode_id       = 218004,
		class_id      = 218,
		type          = 3,
		effect        = 2,
		level_up_exp  = 6000,
		next_skill_id = 218005
	};

%% 幸运
skill_info(218005) ->
	#skill_info{
		mode_id       = 218005,
		class_id      = 218,
		type          = 3,
		effect        = 2,
		level_up_exp  = 13000,
		next_skill_id = 218006
	};

%% 幸运
skill_info(218006) ->
	#skill_info{
		mode_id       = 218006,
		class_id      = 218,
		type          = 3,
		effect        = 2,
		level_up_exp  = 16000,
		next_skill_id = 218007
	};

%% 幸运
skill_info(218007) ->
	#skill_info{
		mode_id       = 218007,
		class_id      = 218,
		type          = 3,
		effect        = 2,
		level_up_exp  = 20000,
		next_skill_id = 218008
	};

%% 幸运
skill_info(218008) ->
	#skill_info{
		mode_id       = 218008,
		class_id      = 218,
		type          = 3,
		effect        = 2,
		level_up_exp  = 24000,
		next_skill_id = 218009
	};

%% 幸运
skill_info(218009) ->
	#skill_info{
		mode_id       = 218009,
		class_id      = 218,
		type          = 3,
		effect        = 2,
		level_up_exp  = 30000,
		next_skill_id = 218010
	};

%% 幸运
skill_info(218010) ->
	#skill_info{
		mode_id       = 218010,
		class_id      = 218,
		type          = 3,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 反震
skill_info(219001) ->
	#skill_info{
		mode_id       = 219001,
		class_id      = 219,
		type          = 3,
		effect        = 1,
		level_up_exp  = 600,
		next_skill_id = 219002
	};

%% 反震
skill_info(219002) ->
	#skill_info{
		mode_id       = 219002,
		class_id      = 219,
		type          = 3,
		effect        = 1,
		level_up_exp  = 900,
		next_skill_id = 219003
	};

%% 反震
skill_info(219003) ->
	#skill_info{
		mode_id       = 219003,
		class_id      = 219,
		type          = 3,
		effect        = 1,
		level_up_exp  = 2000,
		next_skill_id = 219004
	};

%% 反震
skill_info(219004) ->
	#skill_info{
		mode_id       = 219004,
		class_id      = 219,
		type          = 3,
		effect        = 1,
		level_up_exp  = 6000,
		next_skill_id = 219005
	};

%% 反震
skill_info(219005) ->
	#skill_info{
		mode_id       = 219005,
		class_id      = 219,
		type          = 3,
		effect        = 1,
		level_up_exp  = 13000,
		next_skill_id = 219006
	};

%% 反震
skill_info(219006) ->
	#skill_info{
		mode_id       = 219006,
		class_id      = 219,
		type          = 3,
		effect        = 1,
		level_up_exp  = 16000,
		next_skill_id = 219007
	};

%% 反震
skill_info(219007) ->
	#skill_info{
		mode_id       = 219007,
		class_id      = 219,
		type          = 3,
		effect        = 1,
		level_up_exp  = 20000,
		next_skill_id = 219008
	};

%% 反震
skill_info(219008) ->
	#skill_info{
		mode_id       = 219008,
		class_id      = 219,
		type          = 3,
		effect        = 1,
		level_up_exp  = 24000,
		next_skill_id = 219009
	};

%% 反震
skill_info(219009) ->
	#skill_info{
		mode_id       = 219009,
		class_id      = 219,
		type          = 3,
		effect        = 1,
		level_up_exp  = 30000,
		next_skill_id = 219010
	};

%% 反震
skill_info(219010) ->
	#skill_info{
		mode_id       = 219010,
		class_id      = 219,
		type          = 3,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 破甲
skill_info(220001) ->
	#skill_info{
		mode_id       = 220001,
		class_id      = 220,
		type          = 3,
		effect        = 2,
		level_up_exp  = 600,
		next_skill_id = 220002
	};

%% 破甲
skill_info(220002) ->
	#skill_info{
		mode_id       = 220002,
		class_id      = 220,
		type          = 3,
		effect        = 2,
		level_up_exp  = 900,
		next_skill_id = 220003
	};

%% 破甲
skill_info(220003) ->
	#skill_info{
		mode_id       = 220003,
		class_id      = 220,
		type          = 3,
		effect        = 2,
		level_up_exp  = 2000,
		next_skill_id = 220004
	};

%% 破甲
skill_info(220004) ->
	#skill_info{
		mode_id       = 220004,
		class_id      = 220,
		type          = 3,
		effect        = 2,
		level_up_exp  = 6000,
		next_skill_id = 220005
	};

%% 破甲
skill_info(220005) ->
	#skill_info{
		mode_id       = 220005,
		class_id      = 220,
		type          = 3,
		effect        = 2,
		level_up_exp  = 13000,
		next_skill_id = 220006
	};

%% 破甲
skill_info(220006) ->
	#skill_info{
		mode_id       = 220006,
		class_id      = 220,
		type          = 3,
		effect        = 2,
		level_up_exp  = 16000,
		next_skill_id = 220007
	};

%% 破甲
skill_info(220007) ->
	#skill_info{
		mode_id       = 220007,
		class_id      = 220,
		type          = 3,
		effect        = 2,
		level_up_exp  = 20000,
		next_skill_id = 220008
	};

%% 破甲
skill_info(220008) ->
	#skill_info{
		mode_id       = 220008,
		class_id      = 220,
		type          = 3,
		effect        = 2,
		level_up_exp  = 24000,
		next_skill_id = 220009
	};

%% 破甲
skill_info(220009) ->
	#skill_info{
		mode_id       = 220009,
		class_id      = 220,
		type          = 3,
		effect        = 2,
		level_up_exp  = 30000,
		next_skill_id = 220010
	};

%% 破甲
skill_info(220010) ->
	#skill_info{
		mode_id       = 220010,
		class_id      = 220,
		type          = 3,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 反击
skill_info(221001) ->
	#skill_info{
		mode_id       = 221001,
		class_id      = 221,
		type          = 3,
		effect        = 2,
		level_up_exp  = 600,
		next_skill_id = 221002
	};

%% 反击
skill_info(221002) ->
	#skill_info{
		mode_id       = 221002,
		class_id      = 221,
		type          = 3,
		effect        = 2,
		level_up_exp  = 900,
		next_skill_id = 221003
	};

%% 反击
skill_info(221003) ->
	#skill_info{
		mode_id       = 221003,
		class_id      = 221,
		type          = 3,
		effect        = 2,
		level_up_exp  = 2000,
		next_skill_id = 221004
	};

%% 反击
skill_info(221004) ->
	#skill_info{
		mode_id       = 221004,
		class_id      = 221,
		type          = 3,
		effect        = 2,
		level_up_exp  = 6000,
		next_skill_id = 221005
	};

%% 反击
skill_info(221005) ->
	#skill_info{
		mode_id       = 221005,
		class_id      = 221,
		type          = 3,
		effect        = 2,
		level_up_exp  = 13000,
		next_skill_id = 221006
	};

%% 反击
skill_info(221006) ->
	#skill_info{
		mode_id       = 221006,
		class_id      = 221,
		type          = 3,
		effect        = 2,
		level_up_exp  = 16000,
		next_skill_id = 221007
	};

%% 反击
skill_info(221007) ->
	#skill_info{
		mode_id       = 221007,
		class_id      = 221,
		type          = 3,
		effect        = 2,
		level_up_exp  = 20000,
		next_skill_id = 221008
	};

%% 反击
skill_info(221008) ->
	#skill_info{
		mode_id       = 221008,
		class_id      = 221,
		type          = 3,
		effect        = 2,
		level_up_exp  = 24000,
		next_skill_id = 221009
	};

%% 反击
skill_info(221009) ->
	#skill_info{
		mode_id       = 221009,
		class_id      = 221,
		type          = 3,
		effect        = 2,
		level_up_exp  = 30000,
		next_skill_id = 221010
	};

%% 反击
skill_info(221010) ->
	#skill_info{
		mode_id       = 221010,
		class_id      = 221,
		type          = 3,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 致命
skill_info(222001) ->
	#skill_info{
		mode_id       = 222001,
		class_id      = 222,
		type          = 3,
		effect        = 2,
		level_up_exp  = 600,
		next_skill_id = 222002
	};

%% 致命
skill_info(222002) ->
	#skill_info{
		mode_id       = 222002,
		class_id      = 222,
		type          = 3,
		effect        = 2,
		level_up_exp  = 900,
		next_skill_id = 222003
	};

%% 致命
skill_info(222003) ->
	#skill_info{
		mode_id       = 222003,
		class_id      = 222,
		type          = 3,
		effect        = 2,
		level_up_exp  = 2000,
		next_skill_id = 222004
	};

%% 致命
skill_info(222004) ->
	#skill_info{
		mode_id       = 222004,
		class_id      = 222,
		type          = 3,
		effect        = 2,
		level_up_exp  = 6000,
		next_skill_id = 222005
	};

%% 致命
skill_info(222005) ->
	#skill_info{
		mode_id       = 222005,
		class_id      = 222,
		type          = 3,
		effect        = 2,
		level_up_exp  = 13000,
		next_skill_id = 222006
	};

%% 致命
skill_info(222006) ->
	#skill_info{
		mode_id       = 222006,
		class_id      = 222,
		type          = 3,
		effect        = 2,
		level_up_exp  = 16000,
		next_skill_id = 222007
	};

%% 致命
skill_info(222007) ->
	#skill_info{
		mode_id       = 222007,
		class_id      = 222,
		type          = 3,
		effect        = 2,
		level_up_exp  = 20000,
		next_skill_id = 222008
	};

%% 致命
skill_info(222008) ->
	#skill_info{
		mode_id       = 222008,
		class_id      = 222,
		type          = 3,
		effect        = 2,
		level_up_exp  = 24000,
		next_skill_id = 222009
	};

%% 致命
skill_info(222009) ->
	#skill_info{
		mode_id       = 222009,
		class_id      = 222,
		type          = 3,
		effect        = 2,
		level_up_exp  = 30000,
		next_skill_id = 222010
	};

%% 致命
skill_info(222010) ->
	#skill_info{
		mode_id       = 222010,
		class_id      = 222,
		type          = 3,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 扰乱军心
skill_info(223001) ->
	#skill_info{
		mode_id       = 223001,
		class_id      = 223,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 破阵攻心
skill_info(224001) ->
	#skill_info{
		mode_id       = 224001,
		class_id      = 224,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 天护之阵
skill_info(225001) ->
	#skill_info{
		mode_id       = 225001,
		class_id      = 225,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 镇守
skill_info(226001) ->
	#skill_info{
		mode_id       = 226001,
		class_id      = 226,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 怒袭
skill_info(227001) ->
	#skill_info{
		mode_id       = 227001,
		class_id      = 227,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 复仇
skill_info(228001) ->
	#skill_info{
		mode_id       = 228001,
		class_id      = 228,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 吸血
skill_info(229001) ->
	#skill_info{
		mode_id       = 229001,
		class_id      = 229,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 绝杀
skill_info(230001) ->
	#skill_info{
		mode_id       = 230001,
		class_id      = 230,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 追魂之刃
skill_info(231001) ->
	#skill_info{
		mode_id       = 231001,
		class_id      = 231,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 虚空一击
skill_info(232001) ->
	#skill_info{
		mode_id       = 232001,
		class_id      = 232,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 流云刺
skill_info(233001) ->
	#skill_info{
		mode_id       = 233001,
		class_id      = 233,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 虎啸破
skill_info(234001) ->
	#skill_info{
		mode_id       = 234001,
		class_id      = 234,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 连环杀阵
skill_info(235001) ->
	#skill_info{
		mode_id       = 235001,
		class_id      = 235,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 冰凌笺
skill_info(236001) ->
	#skill_info{
		mode_id       = 236001,
		class_id      = 236,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 祭风术
skill_info(237001) ->
	#skill_info{
		mode_id       = 237001,
		class_id      = 237,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 分光诀
skill_info(238001) ->
	#skill_info{
		mode_id       = 238001,
		class_id      = 238,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 凝劲术
skill_info(239001) ->
	#skill_info{
		mode_id       = 239001,
		class_id      = 239,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 激狂诀
skill_info(240001) ->
	#skill_info{
		mode_id       = 240001,
		class_id      = 240,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 狂风划影
skill_info(241001) ->
	#skill_info{
		mode_id       = 241001,
		class_id      = 241,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 三魂回春
skill_info(242001) ->
	#skill_info{
		mode_id       = 242001,
		class_id      = 242,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 元灵之光
skill_info(243001) ->
	#skill_info{
		mode_id       = 243001,
		class_id      = 243,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 净衣术
skill_info(244001) ->
	#skill_info{
		mode_id       = 244001,
		class_id      = 244,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 药王经
skill_info(245001) ->
	#skill_info{
		mode_id       = 245001,
		class_id      = 245,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 仙风万里
skill_info(246001) ->
	#skill_info{
		mode_id       = 246001,
		class_id      = 246,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 清心咒
skill_info(247001) ->
	#skill_info{
		mode_id       = 247001,
		class_id      = 247,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 坚若磐石
skill_info(248001) ->
	#skill_info{
		mode_id       = 248001,
		class_id      = 248,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 105006
	};

%% 背水一战
skill_info(249001) ->
	#skill_info{
		mode_id       = 249001,
		class_id      = 249,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 106006
	};

%% 战意激荡
skill_info(250001) ->
	#skill_info{
		mode_id       = 250001,
		class_id      = 250,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 108006
	};

%% 霸刃连斩
skill_info(251001) ->
	#skill_info{
		mode_id       = 251001,
		class_id      = 251,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 109006
	};

%% 横扫千军
skill_info(252001) ->
	#skill_info{
		mode_id       = 252001,
		class_id      = 252,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 110006
	};

%% 暴怒冲锋
skill_info(253001) ->
	#skill_info{
		mode_id       = 253001,
		class_id      = 253,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 乘胜追击
skill_info(254001) ->
	#skill_info{
		mode_id       = 254001,
		class_id      = 254,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 破军之势
skill_info(255001) ->
	#skill_info{
		mode_id       = 255001,
		class_id      = 255,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 113006
	};

%% 龙战八方
skill_info(256001) ->
	#skill_info{
		mode_id       = 256001,
		class_id      = 256,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 雷光咒
skill_info(257001) ->
	#skill_info{
		mode_id       = 257001,
		class_id      = 257,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 116006
	};

%% 强兵咒
skill_info(258001) ->
	#skill_info{
		mode_id       = 258001,
		class_id      = 258,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 117006
	};

%% 破军咒
skill_info(259001) ->
	#skill_info{
		mode_id       = 259001,
		class_id      = 259,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 118006
	};

%% 削弱
skill_info(260001) ->
	#skill_info{
		mode_id       = 260001,
		class_id      = 260,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 睡眠
skill_info(261001) ->
	#skill_info{
		mode_id       = 261001,
		class_id      = 261,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 摧枯拉朽
skill_info(262001) ->
	#skill_info{
		mode_id       = 262001,
		class_id      = 262,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 大地震击
skill_info(263001) ->
	#skill_info{
		mode_id       = 263001,
		class_id      = 263,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 狂风骤雨
skill_info(264001) ->
	#skill_info{
		mode_id       = 264001,
		class_id      = 264,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 撕裂怒吼
skill_info(265001) ->
	#skill_info{
		mode_id       = 265001,
		class_id      = 265,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 剑刃乱舞
skill_info(266001) ->
	#skill_info{
		mode_id       = 266001,
		class_id      = 266,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 破胆怒吼
skill_info(267001) ->
	#skill_info{
		mode_id       = 267001,
		class_id      = 267,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 春日花语
skill_info(268001) ->
	#skill_info{
		mode_id       = 268001,
		class_id      = 268,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 压制打击
skill_info(269001) ->
	#skill_info{
		mode_id       = 269001,
		class_id      = 269,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 血性饥渴
skill_info(270001) ->
	#skill_info{
		mode_id       = 270001,
		class_id      = 270,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 华光普照
skill_info(271001) ->
	#skill_info{
		mode_id       = 271001,
		class_id      = 271,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 疯狂追击
skill_info(272001) ->
	#skill_info{
		mode_id       = 272001,
		class_id      = 272,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 诅咒之印
skill_info(273001) ->
	#skill_info{
		mode_id       = 273001,
		class_id      = 273,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 伤害吸收
skill_info(274001) ->
	#skill_info{
		mode_id       = 274001,
		class_id      = 274,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 钉刺护盾
skill_info(275001) ->
	#skill_info{
		mode_id       = 275001,
		class_id      = 275,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 背水一击
skill_info(276001) ->
	#skill_info{
		mode_id       = 276001,
		class_id      = 276,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 汲取
skill_info(277001) ->
	#skill_info{
		mode_id       = 277001,
		class_id      = 277,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 削弱
skill_info(278001) ->
	#skill_info{
		mode_id       = 278001,
		class_id      = 278,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 睡眠
skill_info(279001) ->
	#skill_info{
		mode_id       = 279001,
		class_id      = 279,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 重击
skill_info(280001) ->
	#skill_info{
		mode_id       = 280001,
		class_id      = 280,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 强攻
skill_info(281001) ->
	#skill_info{
		mode_id       = 281001,
		class_id      = 281,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 梦魇阴影
skill_info(282001) ->
	#skill_info{
		mode_id       = 282001,
		class_id      = 282,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 毒雾
skill_info(283001) ->
	#skill_info{
		mode_id       = 283001,
		class_id      = 283,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 幽冥剧毒
skill_info(284001) ->
	#skill_info{
		mode_id       = 284001,
		class_id      = 284,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 伤害反弹
skill_info(285001) ->
	#skill_info{
		mode_id       = 285001,
		class_id      = 285,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 削弱
skill_info(286001) ->
	#skill_info{
		mode_id       = 286001,
		class_id      = 286,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 睡眠
skill_info(287001) ->
	#skill_info{
		mode_id       = 287001,
		class_id      = 287,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 重击
skill_info(288001) ->
	#skill_info{
		mode_id       = 288001,
		class_id      = 288,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 强攻
skill_info(289001) ->
	#skill_info{
		mode_id       = 289001,
		class_id      = 289,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 物攻强化
skill_info(301001) ->
	#skill_info{
		mode_id       = 301001,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301002
	};

%% 物攻强化
skill_info(301002) ->
	#skill_info{
		mode_id       = 301002,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301003
	};

%% 物攻强化
skill_info(301003) ->
	#skill_info{
		mode_id       = 301003,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301004
	};

%% 物攻强化
skill_info(301004) ->
	#skill_info{
		mode_id       = 301004,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301005
	};

%% 物攻强化
skill_info(301005) ->
	#skill_info{
		mode_id       = 301005,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301006
	};

%% 物攻强化
skill_info(301006) ->
	#skill_info{
		mode_id       = 301006,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301007
	};

%% 物攻强化
skill_info(301007) ->
	#skill_info{
		mode_id       = 301007,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301008
	};

%% 物攻强化
skill_info(301008) ->
	#skill_info{
		mode_id       = 301008,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301009
	};

%% 物攻强化
skill_info(301009) ->
	#skill_info{
		mode_id       = 301009,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301010
	};

%% 物攻强化
skill_info(301010) ->
	#skill_info{
		mode_id       = 301010,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301011
	};

%% 物攻强化
skill_info(301011) ->
	#skill_info{
		mode_id       = 301011,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301012
	};

%% 物攻强化
skill_info(301012) ->
	#skill_info{
		mode_id       = 301012,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301013
	};

%% 物攻强化
skill_info(301013) ->
	#skill_info{
		mode_id       = 301013,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301014
	};

%% 物攻强化
skill_info(301014) ->
	#skill_info{
		mode_id       = 301014,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301015
	};

%% 物攻强化
skill_info(301015) ->
	#skill_info{
		mode_id       = 301015,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301016
	};

%% 物攻强化
skill_info(301016) ->
	#skill_info{
		mode_id       = 301016,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301017
	};

%% 物攻强化
skill_info(301017) ->
	#skill_info{
		mode_id       = 301017,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301018
	};

%% 物攻强化
skill_info(301018) ->
	#skill_info{
		mode_id       = 301018,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301019
	};

%% 物攻强化
skill_info(301019) ->
	#skill_info{
		mode_id       = 301019,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301020
	};

%% 物攻强化
skill_info(301020) ->
	#skill_info{
		mode_id       = 301020,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301021
	};

%% 物攻强化
skill_info(301021) ->
	#skill_info{
		mode_id       = 301021,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301022
	};

%% 物攻强化
skill_info(301022) ->
	#skill_info{
		mode_id       = 301022,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301023
	};

%% 物攻强化
skill_info(301023) ->
	#skill_info{
		mode_id       = 301023,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301024
	};

%% 物攻强化
skill_info(301024) ->
	#skill_info{
		mode_id       = 301024,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301025
	};

%% 物攻强化
skill_info(301025) ->
	#skill_info{
		mode_id       = 301025,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301026
	};

%% 物攻强化
skill_info(301026) ->
	#skill_info{
		mode_id       = 301026,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301027
	};

%% 物攻强化
skill_info(301027) ->
	#skill_info{
		mode_id       = 301027,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301028
	};

%% 物攻强化
skill_info(301028) ->
	#skill_info{
		mode_id       = 301028,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301029
	};

%% 物攻强化
skill_info(301029) ->
	#skill_info{
		mode_id       = 301029,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 301030
	};

%% 物攻强化
skill_info(301030) ->
	#skill_info{
		mode_id       = 301030,
		class_id      = 301,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 法攻强化
skill_info(302001) ->
	#skill_info{
		mode_id       = 302001,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302002
	};

%% 法攻强化
skill_info(302002) ->
	#skill_info{
		mode_id       = 302002,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302003
	};

%% 法攻强化
skill_info(302003) ->
	#skill_info{
		mode_id       = 302003,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302004
	};

%% 法攻强化
skill_info(302004) ->
	#skill_info{
		mode_id       = 302004,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302005
	};

%% 法攻强化
skill_info(302005) ->
	#skill_info{
		mode_id       = 302005,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302006
	};

%% 法攻强化
skill_info(302006) ->
	#skill_info{
		mode_id       = 302006,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302007
	};

%% 法攻强化
skill_info(302007) ->
	#skill_info{
		mode_id       = 302007,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302008
	};

%% 法攻强化
skill_info(302008) ->
	#skill_info{
		mode_id       = 302008,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302009
	};

%% 法攻强化
skill_info(302009) ->
	#skill_info{
		mode_id       = 302009,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302010
	};

%% 法攻强化
skill_info(302010) ->
	#skill_info{
		mode_id       = 302010,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302011
	};

%% 法攻强化
skill_info(302011) ->
	#skill_info{
		mode_id       = 302011,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302012
	};

%% 法攻强化
skill_info(302012) ->
	#skill_info{
		mode_id       = 302012,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302013
	};

%% 法攻强化
skill_info(302013) ->
	#skill_info{
		mode_id       = 302013,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302014
	};

%% 法攻强化
skill_info(302014) ->
	#skill_info{
		mode_id       = 302014,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302015
	};

%% 法攻强化
skill_info(302015) ->
	#skill_info{
		mode_id       = 302015,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302016
	};

%% 法攻强化
skill_info(302016) ->
	#skill_info{
		mode_id       = 302016,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302017
	};

%% 法攻强化
skill_info(302017) ->
	#skill_info{
		mode_id       = 302017,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302018
	};

%% 法攻强化
skill_info(302018) ->
	#skill_info{
		mode_id       = 302018,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302019
	};

%% 法攻强化
skill_info(302019) ->
	#skill_info{
		mode_id       = 302019,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302020
	};

%% 法攻强化
skill_info(302020) ->
	#skill_info{
		mode_id       = 302020,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302021
	};

%% 法攻强化
skill_info(302021) ->
	#skill_info{
		mode_id       = 302021,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302022
	};

%% 法攻强化
skill_info(302022) ->
	#skill_info{
		mode_id       = 302022,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302023
	};

%% 法攻强化
skill_info(302023) ->
	#skill_info{
		mode_id       = 302023,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302024
	};

%% 法攻强化
skill_info(302024) ->
	#skill_info{
		mode_id       = 302024,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302025
	};

%% 法攻强化
skill_info(302025) ->
	#skill_info{
		mode_id       = 302025,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302026
	};

%% 法攻强化
skill_info(302026) ->
	#skill_info{
		mode_id       = 302026,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302027
	};

%% 法攻强化
skill_info(302027) ->
	#skill_info{
		mode_id       = 302027,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302028
	};

%% 法攻强化
skill_info(302028) ->
	#skill_info{
		mode_id       = 302028,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302029
	};

%% 法攻强化
skill_info(302029) ->
	#skill_info{
		mode_id       = 302029,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 302030
	};

%% 法攻强化
skill_info(302030) ->
	#skill_info{
		mode_id       = 302030,
		class_id      = 302,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 物防强化
skill_info(303001) ->
	#skill_info{
		mode_id       = 303001,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303002
	};

%% 物防强化
skill_info(303002) ->
	#skill_info{
		mode_id       = 303002,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303003
	};

%% 物防强化
skill_info(303003) ->
	#skill_info{
		mode_id       = 303003,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303004
	};

%% 物防强化
skill_info(303004) ->
	#skill_info{
		mode_id       = 303004,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303005
	};

%% 物防强化
skill_info(303005) ->
	#skill_info{
		mode_id       = 303005,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303006
	};

%% 物防强化
skill_info(303006) ->
	#skill_info{
		mode_id       = 303006,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303007
	};

%% 物防强化
skill_info(303007) ->
	#skill_info{
		mode_id       = 303007,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303008
	};

%% 物防强化
skill_info(303008) ->
	#skill_info{
		mode_id       = 303008,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303009
	};

%% 物防强化
skill_info(303009) ->
	#skill_info{
		mode_id       = 303009,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303010
	};

%% 物防强化
skill_info(303010) ->
	#skill_info{
		mode_id       = 303010,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303011
	};

%% 物防强化
skill_info(303011) ->
	#skill_info{
		mode_id       = 303011,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303012
	};

%% 物防强化
skill_info(303012) ->
	#skill_info{
		mode_id       = 303012,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303013
	};

%% 物防强化
skill_info(303013) ->
	#skill_info{
		mode_id       = 303013,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303014
	};

%% 物防强化
skill_info(303014) ->
	#skill_info{
		mode_id       = 303014,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303015
	};

%% 物防强化
skill_info(303015) ->
	#skill_info{
		mode_id       = 303015,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303016
	};

%% 物防强化
skill_info(303016) ->
	#skill_info{
		mode_id       = 303016,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303017
	};

%% 物防强化
skill_info(303017) ->
	#skill_info{
		mode_id       = 303017,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303018
	};

%% 物防强化
skill_info(303018) ->
	#skill_info{
		mode_id       = 303018,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303019
	};

%% 物防强化
skill_info(303019) ->
	#skill_info{
		mode_id       = 303019,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303020
	};

%% 物防强化
skill_info(303020) ->
	#skill_info{
		mode_id       = 303020,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303021
	};

%% 物防强化
skill_info(303021) ->
	#skill_info{
		mode_id       = 303021,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303022
	};

%% 物防强化
skill_info(303022) ->
	#skill_info{
		mode_id       = 303022,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303023
	};

%% 物防强化
skill_info(303023) ->
	#skill_info{
		mode_id       = 303023,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303024
	};

%% 物防强化
skill_info(303024) ->
	#skill_info{
		mode_id       = 303024,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303025
	};

%% 物防强化
skill_info(303025) ->
	#skill_info{
		mode_id       = 303025,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303026
	};

%% 物防强化
skill_info(303026) ->
	#skill_info{
		mode_id       = 303026,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303027
	};

%% 物防强化
skill_info(303027) ->
	#skill_info{
		mode_id       = 303027,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303028
	};

%% 物防强化
skill_info(303028) ->
	#skill_info{
		mode_id       = 303028,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303029
	};

%% 物防强化
skill_info(303029) ->
	#skill_info{
		mode_id       = 303029,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 303030
	};

%% 物防强化
skill_info(303030) ->
	#skill_info{
		mode_id       = 303030,
		class_id      = 303,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 法防强化
skill_info(304001) ->
	#skill_info{
		mode_id       = 304001,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304002
	};

%% 法防强化
skill_info(304002) ->
	#skill_info{
		mode_id       = 304002,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304003
	};

%% 法防强化
skill_info(304003) ->
	#skill_info{
		mode_id       = 304003,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304004
	};

%% 法防强化
skill_info(304004) ->
	#skill_info{
		mode_id       = 304004,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304005
	};

%% 法防强化
skill_info(304005) ->
	#skill_info{
		mode_id       = 304005,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304006
	};

%% 法防强化
skill_info(304006) ->
	#skill_info{
		mode_id       = 304006,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304007
	};

%% 法防强化
skill_info(304007) ->
	#skill_info{
		mode_id       = 304007,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304008
	};

%% 法防强化
skill_info(304008) ->
	#skill_info{
		mode_id       = 304008,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304009
	};

%% 法防强化
skill_info(304009) ->
	#skill_info{
		mode_id       = 304009,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304010
	};

%% 法防强化
skill_info(304010) ->
	#skill_info{
		mode_id       = 304010,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304011
	};

%% 法防强化
skill_info(304011) ->
	#skill_info{
		mode_id       = 304011,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304012
	};

%% 法防强化
skill_info(304012) ->
	#skill_info{
		mode_id       = 304012,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304013
	};

%% 法防强化
skill_info(304013) ->
	#skill_info{
		mode_id       = 304013,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304014
	};

%% 法防强化
skill_info(304014) ->
	#skill_info{
		mode_id       = 304014,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304015
	};

%% 法防强化
skill_info(304015) ->
	#skill_info{
		mode_id       = 304015,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304016
	};

%% 法防强化
skill_info(304016) ->
	#skill_info{
		mode_id       = 304016,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304017
	};

%% 法防强化
skill_info(304017) ->
	#skill_info{
		mode_id       = 304017,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304018
	};

%% 法防强化
skill_info(304018) ->
	#skill_info{
		mode_id       = 304018,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304019
	};

%% 法防强化
skill_info(304019) ->
	#skill_info{
		mode_id       = 304019,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304020
	};

%% 法防强化
skill_info(304020) ->
	#skill_info{
		mode_id       = 304020,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304021
	};

%% 法防强化
skill_info(304021) ->
	#skill_info{
		mode_id       = 304021,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304022
	};

%% 法防强化
skill_info(304022) ->
	#skill_info{
		mode_id       = 304022,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304023
	};

%% 法防强化
skill_info(304023) ->
	#skill_info{
		mode_id       = 304023,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304024
	};

%% 法防强化
skill_info(304024) ->
	#skill_info{
		mode_id       = 304024,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304025
	};

%% 法防强化
skill_info(304025) ->
	#skill_info{
		mode_id       = 304025,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304026
	};

%% 法防强化
skill_info(304026) ->
	#skill_info{
		mode_id       = 304026,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304027
	};

%% 法防强化
skill_info(304027) ->
	#skill_info{
		mode_id       = 304027,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304028
	};

%% 法防强化
skill_info(304028) ->
	#skill_info{
		mode_id       = 304028,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304029
	};

%% 法防强化
skill_info(304029) ->
	#skill_info{
		mode_id       = 304029,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 304030
	};

%% 法防强化
skill_info(304030) ->
	#skill_info{
		mode_id       = 304030,
		class_id      = 304,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 速度强化
skill_info(305001) ->
	#skill_info{
		mode_id       = 305001,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305002
	};

%% 速度强化
skill_info(305002) ->
	#skill_info{
		mode_id       = 305002,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305003
	};

%% 速度强化
skill_info(305003) ->
	#skill_info{
		mode_id       = 305003,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305004
	};

%% 速度强化
skill_info(305004) ->
	#skill_info{
		mode_id       = 305004,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305005
	};

%% 速度强化
skill_info(305005) ->
	#skill_info{
		mode_id       = 305005,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305006
	};

%% 速度强化
skill_info(305006) ->
	#skill_info{
		mode_id       = 305006,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305007
	};

%% 速度强化
skill_info(305007) ->
	#skill_info{
		mode_id       = 305007,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305008
	};

%% 速度强化
skill_info(305008) ->
	#skill_info{
		mode_id       = 305008,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305009
	};

%% 速度强化
skill_info(305009) ->
	#skill_info{
		mode_id       = 305009,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305010
	};

%% 速度强化
skill_info(305010) ->
	#skill_info{
		mode_id       = 305010,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305011
	};

%% 速度强化
skill_info(305011) ->
	#skill_info{
		mode_id       = 305011,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305012
	};

%% 速度强化
skill_info(305012) ->
	#skill_info{
		mode_id       = 305012,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305013
	};

%% 速度强化
skill_info(305013) ->
	#skill_info{
		mode_id       = 305013,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305014
	};

%% 速度强化
skill_info(305014) ->
	#skill_info{
		mode_id       = 305014,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305015
	};

%% 速度强化
skill_info(305015) ->
	#skill_info{
		mode_id       = 305015,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305016
	};

%% 速度强化
skill_info(305016) ->
	#skill_info{
		mode_id       = 305016,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305017
	};

%% 速度强化
skill_info(305017) ->
	#skill_info{
		mode_id       = 305017,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305018
	};

%% 速度强化
skill_info(305018) ->
	#skill_info{
		mode_id       = 305018,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305019
	};

%% 速度强化
skill_info(305019) ->
	#skill_info{
		mode_id       = 305019,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305020
	};

%% 速度强化
skill_info(305020) ->
	#skill_info{
		mode_id       = 305020,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305021
	};

%% 速度强化
skill_info(305021) ->
	#skill_info{
		mode_id       = 305021,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305022
	};

%% 速度强化
skill_info(305022) ->
	#skill_info{
		mode_id       = 305022,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305023
	};

%% 速度强化
skill_info(305023) ->
	#skill_info{
		mode_id       = 305023,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305024
	};

%% 速度强化
skill_info(305024) ->
	#skill_info{
		mode_id       = 305024,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305025
	};

%% 速度强化
skill_info(305025) ->
	#skill_info{
		mode_id       = 305025,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305026
	};

%% 速度强化
skill_info(305026) ->
	#skill_info{
		mode_id       = 305026,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305027
	};

%% 速度强化
skill_info(305027) ->
	#skill_info{
		mode_id       = 305027,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305028
	};

%% 速度强化
skill_info(305028) ->
	#skill_info{
		mode_id       = 305028,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305029
	};

%% 速度强化
skill_info(305029) ->
	#skill_info{
		mode_id       = 305029,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 305030
	};

%% 速度强化
skill_info(305030) ->
	#skill_info{
		mode_id       = 305030,
		class_id      = 305,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 气血强化
skill_info(306001) ->
	#skill_info{
		mode_id       = 306001,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306002
	};

%% 气血强化
skill_info(306002) ->
	#skill_info{
		mode_id       = 306002,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306003
	};

%% 气血强化
skill_info(306003) ->
	#skill_info{
		mode_id       = 306003,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306004
	};

%% 气血强化
skill_info(306004) ->
	#skill_info{
		mode_id       = 306004,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306005
	};

%% 气血强化
skill_info(306005) ->
	#skill_info{
		mode_id       = 306005,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306006
	};

%% 气血强化
skill_info(306006) ->
	#skill_info{
		mode_id       = 306006,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306007
	};

%% 气血强化
skill_info(306007) ->
	#skill_info{
		mode_id       = 306007,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306008
	};

%% 气血强化
skill_info(306008) ->
	#skill_info{
		mode_id       = 306008,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306009
	};

%% 气血强化
skill_info(306009) ->
	#skill_info{
		mode_id       = 306009,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306010
	};

%% 气血强化
skill_info(306010) ->
	#skill_info{
		mode_id       = 306010,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306011
	};

%% 气血强化
skill_info(306011) ->
	#skill_info{
		mode_id       = 306011,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306012
	};

%% 气血强化
skill_info(306012) ->
	#skill_info{
		mode_id       = 306012,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306013
	};

%% 气血强化
skill_info(306013) ->
	#skill_info{
		mode_id       = 306013,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306014
	};

%% 气血强化
skill_info(306014) ->
	#skill_info{
		mode_id       = 306014,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306015
	};

%% 气血强化
skill_info(306015) ->
	#skill_info{
		mode_id       = 306015,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306016
	};

%% 气血强化
skill_info(306016) ->
	#skill_info{
		mode_id       = 306016,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306017
	};

%% 气血强化
skill_info(306017) ->
	#skill_info{
		mode_id       = 306017,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306018
	};

%% 气血强化
skill_info(306018) ->
	#skill_info{
		mode_id       = 306018,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306019
	};

%% 气血强化
skill_info(306019) ->
	#skill_info{
		mode_id       = 306019,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306020
	};

%% 气血强化
skill_info(306020) ->
	#skill_info{
		mode_id       = 306020,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306021
	};

%% 气血强化
skill_info(306021) ->
	#skill_info{
		mode_id       = 306021,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306022
	};

%% 气血强化
skill_info(306022) ->
	#skill_info{
		mode_id       = 306022,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306023
	};

%% 气血强化
skill_info(306023) ->
	#skill_info{
		mode_id       = 306023,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306024
	};

%% 气血强化
skill_info(306024) ->
	#skill_info{
		mode_id       = 306024,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306025
	};

%% 气血强化
skill_info(306025) ->
	#skill_info{
		mode_id       = 306025,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306026
	};

%% 气血强化
skill_info(306026) ->
	#skill_info{
		mode_id       = 306026,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306027
	};

%% 气血强化
skill_info(306027) ->
	#skill_info{
		mode_id       = 306027,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306028
	};

%% 气血强化
skill_info(306028) ->
	#skill_info{
		mode_id       = 306028,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306029
	};

%% 气血强化
skill_info(306029) ->
	#skill_info{
		mode_id       = 306029,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 306030
	};

%% 气血强化
skill_info(306030) ->
	#skill_info{
		mode_id       = 306030,
		class_id      = 306,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 暴击强化
skill_info(307001) ->
	#skill_info{
		mode_id       = 307001,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307002
	};

%% 暴击强化
skill_info(307002) ->
	#skill_info{
		mode_id       = 307002,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307003
	};

%% 暴击强化
skill_info(307003) ->
	#skill_info{
		mode_id       = 307003,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307004
	};

%% 暴击强化
skill_info(307004) ->
	#skill_info{
		mode_id       = 307004,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307005
	};

%% 暴击强化
skill_info(307005) ->
	#skill_info{
		mode_id       = 307005,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307006
	};

%% 暴击强化
skill_info(307006) ->
	#skill_info{
		mode_id       = 307006,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307007
	};

%% 暴击强化
skill_info(307007) ->
	#skill_info{
		mode_id       = 307007,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307008
	};

%% 暴击强化
skill_info(307008) ->
	#skill_info{
		mode_id       = 307008,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307009
	};

%% 暴击强化
skill_info(307009) ->
	#skill_info{
		mode_id       = 307009,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307010
	};

%% 暴击强化
skill_info(307010) ->
	#skill_info{
		mode_id       = 307010,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307011
	};

%% 暴击强化
skill_info(307011) ->
	#skill_info{
		mode_id       = 307011,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307012
	};

%% 暴击强化
skill_info(307012) ->
	#skill_info{
		mode_id       = 307012,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307013
	};

%% 暴击强化
skill_info(307013) ->
	#skill_info{
		mode_id       = 307013,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307014
	};

%% 暴击强化
skill_info(307014) ->
	#skill_info{
		mode_id       = 307014,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307015
	};

%% 暴击强化
skill_info(307015) ->
	#skill_info{
		mode_id       = 307015,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307016
	};

%% 暴击强化
skill_info(307016) ->
	#skill_info{
		mode_id       = 307016,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307017
	};

%% 暴击强化
skill_info(307017) ->
	#skill_info{
		mode_id       = 307017,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307018
	};

%% 暴击强化
skill_info(307018) ->
	#skill_info{
		mode_id       = 307018,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307019
	};

%% 暴击强化
skill_info(307019) ->
	#skill_info{
		mode_id       = 307019,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307020
	};

%% 暴击强化
skill_info(307020) ->
	#skill_info{
		mode_id       = 307020,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307021
	};

%% 暴击强化
skill_info(307021) ->
	#skill_info{
		mode_id       = 307021,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307022
	};

%% 暴击强化
skill_info(307022) ->
	#skill_info{
		mode_id       = 307022,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307023
	};

%% 暴击强化
skill_info(307023) ->
	#skill_info{
		mode_id       = 307023,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307024
	};

%% 暴击强化
skill_info(307024) ->
	#skill_info{
		mode_id       = 307024,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307025
	};

%% 暴击强化
skill_info(307025) ->
	#skill_info{
		mode_id       = 307025,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307026
	};

%% 暴击强化
skill_info(307026) ->
	#skill_info{
		mode_id       = 307026,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307027
	};

%% 暴击强化
skill_info(307027) ->
	#skill_info{
		mode_id       = 307027,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307028
	};

%% 暴击强化
skill_info(307028) ->
	#skill_info{
		mode_id       = 307028,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307029
	};

%% 暴击强化
skill_info(307029) ->
	#skill_info{
		mode_id       = 307029,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 307030
	};

%% 暴击强化
skill_info(307030) ->
	#skill_info{
		mode_id       = 307030,
		class_id      = 307,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 格挡强化
skill_info(308001) ->
	#skill_info{
		mode_id       = 308001,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308002
	};

%% 格挡强化
skill_info(308002) ->
	#skill_info{
		mode_id       = 308002,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308003
	};

%% 格挡强化
skill_info(308003) ->
	#skill_info{
		mode_id       = 308003,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308004
	};

%% 格挡强化
skill_info(308004) ->
	#skill_info{
		mode_id       = 308004,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308005
	};

%% 格挡强化
skill_info(308005) ->
	#skill_info{
		mode_id       = 308005,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308006
	};

%% 格挡强化
skill_info(308006) ->
	#skill_info{
		mode_id       = 308006,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308007
	};

%% 格挡强化
skill_info(308007) ->
	#skill_info{
		mode_id       = 308007,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308008
	};

%% 格挡强化
skill_info(308008) ->
	#skill_info{
		mode_id       = 308008,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308009
	};

%% 格挡强化
skill_info(308009) ->
	#skill_info{
		mode_id       = 308009,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308010
	};

%% 格挡强化
skill_info(308010) ->
	#skill_info{
		mode_id       = 308010,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308011
	};

%% 格挡强化
skill_info(308011) ->
	#skill_info{
		mode_id       = 308011,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308012
	};

%% 格挡强化
skill_info(308012) ->
	#skill_info{
		mode_id       = 308012,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308013
	};

%% 格挡强化
skill_info(308013) ->
	#skill_info{
		mode_id       = 308013,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308014
	};

%% 格挡强化
skill_info(308014) ->
	#skill_info{
		mode_id       = 308014,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308015
	};

%% 格挡强化
skill_info(308015) ->
	#skill_info{
		mode_id       = 308015,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308016
	};

%% 格挡强化
skill_info(308016) ->
	#skill_info{
		mode_id       = 308016,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308017
	};

%% 格挡强化
skill_info(308017) ->
	#skill_info{
		mode_id       = 308017,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308018
	};

%% 格挡强化
skill_info(308018) ->
	#skill_info{
		mode_id       = 308018,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308019
	};

%% 格挡强化
skill_info(308019) ->
	#skill_info{
		mode_id       = 308019,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308020
	};

%% 格挡强化
skill_info(308020) ->
	#skill_info{
		mode_id       = 308020,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308021
	};

%% 格挡强化
skill_info(308021) ->
	#skill_info{
		mode_id       = 308021,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308022
	};

%% 格挡强化
skill_info(308022) ->
	#skill_info{
		mode_id       = 308022,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308023
	};

%% 格挡强化
skill_info(308023) ->
	#skill_info{
		mode_id       = 308023,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308024
	};

%% 格挡强化
skill_info(308024) ->
	#skill_info{
		mode_id       = 308024,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308025
	};

%% 格挡强化
skill_info(308025) ->
	#skill_info{
		mode_id       = 308025,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308026
	};

%% 格挡强化
skill_info(308026) ->
	#skill_info{
		mode_id       = 308026,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308027
	};

%% 格挡强化
skill_info(308027) ->
	#skill_info{
		mode_id       = 308027,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308028
	};

%% 格挡强化
skill_info(308028) ->
	#skill_info{
		mode_id       = 308028,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308029
	};

%% 格挡强化
skill_info(308029) ->
	#skill_info{
		mode_id       = 308029,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 308030
	};

%% 格挡强化
skill_info(308030) ->
	#skill_info{
		mode_id       = 308030,
		class_id      = 308,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 闪避强化
skill_info(309001) ->
	#skill_info{
		mode_id       = 309001,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309002
	};

%% 闪避强化
skill_info(309002) ->
	#skill_info{
		mode_id       = 309002,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309003
	};

%% 闪避强化
skill_info(309003) ->
	#skill_info{
		mode_id       = 309003,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309004
	};

%% 闪避强化
skill_info(309004) ->
	#skill_info{
		mode_id       = 309004,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309005
	};

%% 闪避强化
skill_info(309005) ->
	#skill_info{
		mode_id       = 309005,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309006
	};

%% 闪避强化
skill_info(309006) ->
	#skill_info{
		mode_id       = 309006,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309007
	};

%% 闪避强化
skill_info(309007) ->
	#skill_info{
		mode_id       = 309007,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309008
	};

%% 闪避强化
skill_info(309008) ->
	#skill_info{
		mode_id       = 309008,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309009
	};

%% 闪避强化
skill_info(309009) ->
	#skill_info{
		mode_id       = 309009,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309010
	};

%% 闪避强化
skill_info(309010) ->
	#skill_info{
		mode_id       = 309010,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309011
	};

%% 闪避强化
skill_info(309011) ->
	#skill_info{
		mode_id       = 309011,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309012
	};

%% 闪避强化
skill_info(309012) ->
	#skill_info{
		mode_id       = 309012,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309013
	};

%% 闪避强化
skill_info(309013) ->
	#skill_info{
		mode_id       = 309013,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309014
	};

%% 闪避强化
skill_info(309014) ->
	#skill_info{
		mode_id       = 309014,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309015
	};

%% 闪避强化
skill_info(309015) ->
	#skill_info{
		mode_id       = 309015,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309016
	};

%% 闪避强化
skill_info(309016) ->
	#skill_info{
		mode_id       = 309016,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309017
	};

%% 闪避强化
skill_info(309017) ->
	#skill_info{
		mode_id       = 309017,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309018
	};

%% 闪避强化
skill_info(309018) ->
	#skill_info{
		mode_id       = 309018,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309019
	};

%% 闪避强化
skill_info(309019) ->
	#skill_info{
		mode_id       = 309019,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309020
	};

%% 闪避强化
skill_info(309020) ->
	#skill_info{
		mode_id       = 309020,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309021
	};

%% 闪避强化
skill_info(309021) ->
	#skill_info{
		mode_id       = 309021,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309022
	};

%% 闪避强化
skill_info(309022) ->
	#skill_info{
		mode_id       = 309022,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309023
	};

%% 闪避强化
skill_info(309023) ->
	#skill_info{
		mode_id       = 309023,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309024
	};

%% 闪避强化
skill_info(309024) ->
	#skill_info{
		mode_id       = 309024,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309025
	};

%% 闪避强化
skill_info(309025) ->
	#skill_info{
		mode_id       = 309025,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309026
	};

%% 闪避强化
skill_info(309026) ->
	#skill_info{
		mode_id       = 309026,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309027
	};

%% 闪避强化
skill_info(309027) ->
	#skill_info{
		mode_id       = 309027,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309028
	};

%% 闪避强化
skill_info(309028) ->
	#skill_info{
		mode_id       = 309028,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309029
	};

%% 闪避强化
skill_info(309029) ->
	#skill_info{
		mode_id       = 309029,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 309030
	};

%% 闪避强化
skill_info(309030) ->
	#skill_info{
		mode_id       = 309030,
		class_id      = 309,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 命中强化
skill_info(310001) ->
	#skill_info{
		mode_id       = 310001,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310002
	};

%% 命中强化
skill_info(310002) ->
	#skill_info{
		mode_id       = 310002,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310003
	};

%% 命中强化
skill_info(310003) ->
	#skill_info{
		mode_id       = 310003,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310004
	};

%% 命中强化
skill_info(310004) ->
	#skill_info{
		mode_id       = 310004,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310005
	};

%% 命中强化
skill_info(310005) ->
	#skill_info{
		mode_id       = 310005,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310006
	};

%% 命中强化
skill_info(310006) ->
	#skill_info{
		mode_id       = 310006,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310007
	};

%% 命中强化
skill_info(310007) ->
	#skill_info{
		mode_id       = 310007,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310008
	};

%% 命中强化
skill_info(310008) ->
	#skill_info{
		mode_id       = 310008,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310009
	};

%% 命中强化
skill_info(310009) ->
	#skill_info{
		mode_id       = 310009,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310010
	};

%% 命中强化
skill_info(310010) ->
	#skill_info{
		mode_id       = 310010,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310011
	};

%% 命中强化
skill_info(310011) ->
	#skill_info{
		mode_id       = 310011,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310012
	};

%% 命中强化
skill_info(310012) ->
	#skill_info{
		mode_id       = 310012,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310013
	};

%% 命中强化
skill_info(310013) ->
	#skill_info{
		mode_id       = 310013,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310014
	};

%% 命中强化
skill_info(310014) ->
	#skill_info{
		mode_id       = 310014,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310015
	};

%% 命中强化
skill_info(310015) ->
	#skill_info{
		mode_id       = 310015,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310016
	};

%% 命中强化
skill_info(310016) ->
	#skill_info{
		mode_id       = 310016,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310017
	};

%% 命中强化
skill_info(310017) ->
	#skill_info{
		mode_id       = 310017,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310018
	};

%% 命中强化
skill_info(310018) ->
	#skill_info{
		mode_id       = 310018,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310019
	};

%% 命中强化
skill_info(310019) ->
	#skill_info{
		mode_id       = 310019,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310020
	};

%% 命中强化
skill_info(310020) ->
	#skill_info{
		mode_id       = 310020,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310021
	};

%% 命中强化
skill_info(310021) ->
	#skill_info{
		mode_id       = 310021,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310022
	};

%% 命中强化
skill_info(310022) ->
	#skill_info{
		mode_id       = 310022,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310023
	};

%% 命中强化
skill_info(310023) ->
	#skill_info{
		mode_id       = 310023,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310024
	};

%% 命中强化
skill_info(310024) ->
	#skill_info{
		mode_id       = 310024,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310025
	};

%% 命中强化
skill_info(310025) ->
	#skill_info{
		mode_id       = 310025,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310026
	};

%% 命中强化
skill_info(310026) ->
	#skill_info{
		mode_id       = 310026,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310027
	};

%% 命中强化
skill_info(310027) ->
	#skill_info{
		mode_id       = 310027,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310028
	};

%% 命中强化
skill_info(310028) ->
	#skill_info{
		mode_id       = 310028,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310029
	};

%% 命中强化
skill_info(310029) ->
	#skill_info{
		mode_id       = 310029,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 310030
	};

%% 命中强化
skill_info(310030) ->
	#skill_info{
		mode_id       = 310030,
		class_id      = 310,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 幸运强化
skill_info(311001) ->
	#skill_info{
		mode_id       = 311001,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311002
	};

%% 幸运强化
skill_info(311002) ->
	#skill_info{
		mode_id       = 311002,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311003
	};

%% 幸运强化
skill_info(311003) ->
	#skill_info{
		mode_id       = 311003,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311004
	};

%% 幸运强化
skill_info(311004) ->
	#skill_info{
		mode_id       = 311004,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311005
	};

%% 幸运强化
skill_info(311005) ->
	#skill_info{
		mode_id       = 311005,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311006
	};

%% 幸运强化
skill_info(311006) ->
	#skill_info{
		mode_id       = 311006,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311007
	};

%% 幸运强化
skill_info(311007) ->
	#skill_info{
		mode_id       = 311007,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311008
	};

%% 幸运强化
skill_info(311008) ->
	#skill_info{
		mode_id       = 311008,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311009
	};

%% 幸运强化
skill_info(311009) ->
	#skill_info{
		mode_id       = 311009,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311010
	};

%% 幸运强化
skill_info(311010) ->
	#skill_info{
		mode_id       = 311010,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311011
	};

%% 幸运强化
skill_info(311011) ->
	#skill_info{
		mode_id       = 311011,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311012
	};

%% 幸运强化
skill_info(311012) ->
	#skill_info{
		mode_id       = 311012,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311013
	};

%% 幸运强化
skill_info(311013) ->
	#skill_info{
		mode_id       = 311013,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311014
	};

%% 幸运强化
skill_info(311014) ->
	#skill_info{
		mode_id       = 311014,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311015
	};

%% 幸运强化
skill_info(311015) ->
	#skill_info{
		mode_id       = 311015,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311016
	};

%% 幸运强化
skill_info(311016) ->
	#skill_info{
		mode_id       = 311016,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311017
	};

%% 幸运强化
skill_info(311017) ->
	#skill_info{
		mode_id       = 311017,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311018
	};

%% 幸运强化
skill_info(311018) ->
	#skill_info{
		mode_id       = 311018,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311019
	};

%% 幸运强化
skill_info(311019) ->
	#skill_info{
		mode_id       = 311019,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311020
	};

%% 幸运强化
skill_info(311020) ->
	#skill_info{
		mode_id       = 311020,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311021
	};

%% 幸运强化
skill_info(311021) ->
	#skill_info{
		mode_id       = 311021,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311022
	};

%% 幸运强化
skill_info(311022) ->
	#skill_info{
		mode_id       = 311022,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311023
	};

%% 幸运强化
skill_info(311023) ->
	#skill_info{
		mode_id       = 311023,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311024
	};

%% 幸运强化
skill_info(311024) ->
	#skill_info{
		mode_id       = 311024,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311025
	};

%% 幸运强化
skill_info(311025) ->
	#skill_info{
		mode_id       = 311025,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311026
	};

%% 幸运强化
skill_info(311026) ->
	#skill_info{
		mode_id       = 311026,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311027
	};

%% 幸运强化
skill_info(311027) ->
	#skill_info{
		mode_id       = 311027,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311028
	};

%% 幸运强化
skill_info(311028) ->
	#skill_info{
		mode_id       = 311028,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311029
	};

%% 幸运强化
skill_info(311029) ->
	#skill_info{
		mode_id       = 311029,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 311030
	};

%% 幸运强化
skill_info(311030) ->
	#skill_info{
		mode_id       = 311030,
		class_id      = 311,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 破甲强化
skill_info(312001) ->
	#skill_info{
		mode_id       = 312001,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312002
	};

%% 破甲强化
skill_info(312002) ->
	#skill_info{
		mode_id       = 312002,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312003
	};

%% 破甲强化
skill_info(312003) ->
	#skill_info{
		mode_id       = 312003,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312004
	};

%% 破甲强化
skill_info(312004) ->
	#skill_info{
		mode_id       = 312004,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312005
	};

%% 破甲强化
skill_info(312005) ->
	#skill_info{
		mode_id       = 312005,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312006
	};

%% 破甲强化
skill_info(312006) ->
	#skill_info{
		mode_id       = 312006,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312007
	};

%% 破甲强化
skill_info(312007) ->
	#skill_info{
		mode_id       = 312007,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312008
	};

%% 破甲强化
skill_info(312008) ->
	#skill_info{
		mode_id       = 312008,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312009
	};

%% 破甲强化
skill_info(312009) ->
	#skill_info{
		mode_id       = 312009,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312010
	};

%% 破甲强化
skill_info(312010) ->
	#skill_info{
		mode_id       = 312010,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312011
	};

%% 破甲强化
skill_info(312011) ->
	#skill_info{
		mode_id       = 312011,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312012
	};

%% 破甲强化
skill_info(312012) ->
	#skill_info{
		mode_id       = 312012,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312013
	};

%% 破甲强化
skill_info(312013) ->
	#skill_info{
		mode_id       = 312013,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312014
	};

%% 破甲强化
skill_info(312014) ->
	#skill_info{
		mode_id       = 312014,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312015
	};

%% 破甲强化
skill_info(312015) ->
	#skill_info{
		mode_id       = 312015,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312016
	};

%% 破甲强化
skill_info(312016) ->
	#skill_info{
		mode_id       = 312016,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312017
	};

%% 破甲强化
skill_info(312017) ->
	#skill_info{
		mode_id       = 312017,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312018
	};

%% 破甲强化
skill_info(312018) ->
	#skill_info{
		mode_id       = 312018,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312019
	};

%% 破甲强化
skill_info(312019) ->
	#skill_info{
		mode_id       = 312019,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312020
	};

%% 破甲强化
skill_info(312020) ->
	#skill_info{
		mode_id       = 312020,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312021
	};

%% 破甲强化
skill_info(312021) ->
	#skill_info{
		mode_id       = 312021,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312022
	};

%% 破甲强化
skill_info(312022) ->
	#skill_info{
		mode_id       = 312022,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312023
	};

%% 破甲强化
skill_info(312023) ->
	#skill_info{
		mode_id       = 312023,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312024
	};

%% 破甲强化
skill_info(312024) ->
	#skill_info{
		mode_id       = 312024,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312025
	};

%% 破甲强化
skill_info(312025) ->
	#skill_info{
		mode_id       = 312025,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312026
	};

%% 破甲强化
skill_info(312026) ->
	#skill_info{
		mode_id       = 312026,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312027
	};

%% 破甲强化
skill_info(312027) ->
	#skill_info{
		mode_id       = 312027,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312028
	};

%% 破甲强化
skill_info(312028) ->
	#skill_info{
		mode_id       = 312028,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312029
	};

%% 破甲强化
skill_info(312029) ->
	#skill_info{
		mode_id       = 312029,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 312030
	};

%% 破甲强化
skill_info(312030) ->
	#skill_info{
		mode_id       = 312030,
		class_id      = 312,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 反击强化
skill_info(313001) ->
	#skill_info{
		mode_id       = 313001,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313002
	};

%% 反击强化
skill_info(313002) ->
	#skill_info{
		mode_id       = 313002,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313003
	};

%% 反击强化
skill_info(313003) ->
	#skill_info{
		mode_id       = 313003,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313004
	};

%% 反击强化
skill_info(313004) ->
	#skill_info{
		mode_id       = 313004,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313005
	};

%% 反击强化
skill_info(313005) ->
	#skill_info{
		mode_id       = 313005,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313006
	};

%% 反击强化
skill_info(313006) ->
	#skill_info{
		mode_id       = 313006,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313007
	};

%% 反击强化
skill_info(313007) ->
	#skill_info{
		mode_id       = 313007,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313008
	};

%% 反击强化
skill_info(313008) ->
	#skill_info{
		mode_id       = 313008,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313009
	};

%% 反击强化
skill_info(313009) ->
	#skill_info{
		mode_id       = 313009,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313010
	};

%% 反击强化
skill_info(313010) ->
	#skill_info{
		mode_id       = 313010,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313011
	};

%% 反击强化
skill_info(313011) ->
	#skill_info{
		mode_id       = 313011,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313012
	};

%% 反击强化
skill_info(313012) ->
	#skill_info{
		mode_id       = 313012,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313013
	};

%% 反击强化
skill_info(313013) ->
	#skill_info{
		mode_id       = 313013,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313014
	};

%% 反击强化
skill_info(313014) ->
	#skill_info{
		mode_id       = 313014,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313015
	};

%% 反击强化
skill_info(313015) ->
	#skill_info{
		mode_id       = 313015,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313016
	};

%% 反击强化
skill_info(313016) ->
	#skill_info{
		mode_id       = 313016,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313017
	};

%% 反击强化
skill_info(313017) ->
	#skill_info{
		mode_id       = 313017,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313018
	};

%% 反击强化
skill_info(313018) ->
	#skill_info{
		mode_id       = 313018,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313019
	};

%% 反击强化
skill_info(313019) ->
	#skill_info{
		mode_id       = 313019,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313020
	};

%% 反击强化
skill_info(313020) ->
	#skill_info{
		mode_id       = 313020,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313021
	};

%% 反击强化
skill_info(313021) ->
	#skill_info{
		mode_id       = 313021,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313022
	};

%% 反击强化
skill_info(313022) ->
	#skill_info{
		mode_id       = 313022,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313023
	};

%% 反击强化
skill_info(313023) ->
	#skill_info{
		mode_id       = 313023,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313024
	};

%% 反击强化
skill_info(313024) ->
	#skill_info{
		mode_id       = 313024,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313025
	};

%% 反击强化
skill_info(313025) ->
	#skill_info{
		mode_id       = 313025,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313026
	};

%% 反击强化
skill_info(313026) ->
	#skill_info{
		mode_id       = 313026,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313027
	};

%% 反击强化
skill_info(313027) ->
	#skill_info{
		mode_id       = 313027,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313028
	};

%% 反击强化
skill_info(313028) ->
	#skill_info{
		mode_id       = 313028,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313029
	};

%% 反击强化
skill_info(313029) ->
	#skill_info{
		mode_id       = 313029,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 313030
	};

%% 反击强化
skill_info(313030) ->
	#skill_info{
		mode_id       = 313030,
		class_id      = 313,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 致命强化
skill_info(314001) ->
	#skill_info{
		mode_id       = 314001,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314002
	};

%% 致命强化
skill_info(314002) ->
	#skill_info{
		mode_id       = 314002,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314003
	};

%% 致命强化
skill_info(314003) ->
	#skill_info{
		mode_id       = 314003,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314004
	};

%% 致命强化
skill_info(314004) ->
	#skill_info{
		mode_id       = 314004,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314005
	};

%% 致命强化
skill_info(314005) ->
	#skill_info{
		mode_id       = 314005,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314006
	};

%% 致命强化
skill_info(314006) ->
	#skill_info{
		mode_id       = 314006,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314007
	};

%% 致命强化
skill_info(314007) ->
	#skill_info{
		mode_id       = 314007,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314008
	};

%% 致命强化
skill_info(314008) ->
	#skill_info{
		mode_id       = 314008,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314009
	};

%% 致命强化
skill_info(314009) ->
	#skill_info{
		mode_id       = 314009,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314010
	};

%% 致命强化
skill_info(314010) ->
	#skill_info{
		mode_id       = 314010,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314011
	};

%% 致命强化
skill_info(314011) ->
	#skill_info{
		mode_id       = 314011,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314012
	};

%% 致命强化
skill_info(314012) ->
	#skill_info{
		mode_id       = 314012,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314013
	};

%% 致命强化
skill_info(314013) ->
	#skill_info{
		mode_id       = 314013,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314014
	};

%% 致命强化
skill_info(314014) ->
	#skill_info{
		mode_id       = 314014,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314015
	};

%% 致命强化
skill_info(314015) ->
	#skill_info{
		mode_id       = 314015,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314016
	};

%% 致命强化
skill_info(314016) ->
	#skill_info{
		mode_id       = 314016,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314017
	};

%% 致命强化
skill_info(314017) ->
	#skill_info{
		mode_id       = 314017,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314018
	};

%% 致命强化
skill_info(314018) ->
	#skill_info{
		mode_id       = 314018,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314019
	};

%% 致命强化
skill_info(314019) ->
	#skill_info{
		mode_id       = 314019,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314020
	};

%% 致命强化
skill_info(314020) ->
	#skill_info{
		mode_id       = 314020,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314021
	};

%% 致命强化
skill_info(314021) ->
	#skill_info{
		mode_id       = 314021,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314022
	};

%% 致命强化
skill_info(314022) ->
	#skill_info{
		mode_id       = 314022,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314023
	};

%% 致命强化
skill_info(314023) ->
	#skill_info{
		mode_id       = 314023,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314024
	};

%% 致命强化
skill_info(314024) ->
	#skill_info{
		mode_id       = 314024,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314025
	};

%% 致命强化
skill_info(314025) ->
	#skill_info{
		mode_id       = 314025,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314026
	};

%% 致命强化
skill_info(314026) ->
	#skill_info{
		mode_id       = 314026,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314027
	};

%% 致命强化
skill_info(314027) ->
	#skill_info{
		mode_id       = 314027,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314028
	};

%% 致命强化
skill_info(314028) ->
	#skill_info{
		mode_id       = 314028,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314029
	};

%% 致命强化
skill_info(314029) ->
	#skill_info{
		mode_id       = 314029,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 314030
	};

%% 致命强化
skill_info(314030) ->
	#skill_info{
		mode_id       = 314030,
		class_id      = 314,
		type          = 5,
		effect        = 2,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 背水一战
skill_info(401001) ->
	#skill_info{
		mode_id       = 401001,
		class_id      = 401,
		type          = 4,
		effect        = 1,
		level_up_exp  = 490,
		next_skill_id = 401002
	};

%% 背水一战
skill_info(401002) ->
	#skill_info{
		mode_id       = 401002,
		class_id      = 401,
		type          = 4,
		effect        = 1,
		level_up_exp  = 2300,
		next_skill_id = 401003
	};

%% 背水一战
skill_info(401003) ->
	#skill_info{
		mode_id       = 401003,
		class_id      = 401,
		type          = 4,
		effect        = 1,
		level_up_exp  = 7340,
		next_skill_id = 401004
	};

%% 背水一战
skill_info(401004) ->
	#skill_info{
		mode_id       = 401004,
		class_id      = 401,
		type          = 4,
		effect        = 1,
		level_up_exp  = 16150,
		next_skill_id = 401005
	};

%% 背水一战
skill_info(401005) ->
	#skill_info{
		mode_id       = 401005,
		class_id      = 401,
		type          = 4,
		effect        = 1,
		level_up_exp  = 39100,
		next_skill_id = 401006
	};

%% 背水一战
skill_info(401006) ->
	#skill_info{
		mode_id       = 401006,
		class_id      = 401,
		type          = 4,
		effect        = 1,
		level_up_exp  = 76500,
		next_skill_id = 401007
	};

%% 背水一战
skill_info(401007) ->
	#skill_info{
		mode_id       = 401007,
		class_id      = 401,
		type          = 4,
		effect        = 1,
		level_up_exp  = 154700,
		next_skill_id = 401008
	};

%% 背水一战
skill_info(401008) ->
	#skill_info{
		mode_id       = 401008,
		class_id      = 401,
		type          = 4,
		effect        = 1,
		level_up_exp  = 289000,
		next_skill_id = 401009
	};

%% 背水一战
skill_info(401009) ->
	#skill_info{
		mode_id       = 401009,
		class_id      = 401,
		type          = 4,
		effect        = 1,
		level_up_exp  = 527000,
		next_skill_id = 401010
	};

%% 背水一战
skill_info(401010) ->
	#skill_info{
		mode_id       = 401010,
		class_id      = 401,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 战意激荡
skill_info(402001) ->
	#skill_info{
		mode_id       = 402001,
		class_id      = 402,
		type          = 4,
		effect        = 1,
		level_up_exp  = 550,
		next_skill_id = 402002
	};

%% 战意激荡
skill_info(402002) ->
	#skill_info{
		mode_id       = 402002,
		class_id      = 402,
		type          = 4,
		effect        = 1,
		level_up_exp  = 2570,
		next_skill_id = 402003
	};

%% 战意激荡
skill_info(402003) ->
	#skill_info{
		mode_id       = 402003,
		class_id      = 402,
		type          = 4,
		effect        = 1,
		level_up_exp  = 8210,
		next_skill_id = 402004
	};

%% 战意激荡
skill_info(402004) ->
	#skill_info{
		mode_id       = 402004,
		class_id      = 402,
		type          = 4,
		effect        = 1,
		level_up_exp  = 18050,
		next_skill_id = 402005
	};

%% 战意激荡
skill_info(402005) ->
	#skill_info{
		mode_id       = 402005,
		class_id      = 402,
		type          = 4,
		effect        = 1,
		level_up_exp  = 43700,
		next_skill_id = 402006
	};

%% 战意激荡
skill_info(402006) ->
	#skill_info{
		mode_id       = 402006,
		class_id      = 402,
		type          = 4,
		effect        = 1,
		level_up_exp  = 85500,
		next_skill_id = 402007
	};

%% 战意激荡
skill_info(402007) ->
	#skill_info{
		mode_id       = 402007,
		class_id      = 402,
		type          = 4,
		effect        = 1,
		level_up_exp  = 172900,
		next_skill_id = 402008
	};

%% 战意激荡
skill_info(402008) ->
	#skill_info{
		mode_id       = 402008,
		class_id      = 402,
		type          = 4,
		effect        = 1,
		level_up_exp  = 323000,
		next_skill_id = 402009
	};

%% 战意激荡
skill_info(402009) ->
	#skill_info{
		mode_id       = 402009,
		class_id      = 402,
		type          = 4,
		effect        = 1,
		level_up_exp  = 589000,
		next_skill_id = 402010
	};

%% 战意激荡
skill_info(402010) ->
	#skill_info{
		mode_id       = 402010,
		class_id      = 402,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 龙牙突刺
skill_info(403001) ->
	#skill_info{
		mode_id       = 403001,
		class_id      = 403,
		type          = 4,
		effect        = 1,
		level_up_exp  = 490,
		next_skill_id = 403002
	};

%% 龙牙突刺
skill_info(403002) ->
	#skill_info{
		mode_id       = 403002,
		class_id      = 403,
		type          = 4,
		effect        = 1,
		level_up_exp  = 2300,
		next_skill_id = 403003
	};

%% 龙牙突刺
skill_info(403003) ->
	#skill_info{
		mode_id       = 403003,
		class_id      = 403,
		type          = 4,
		effect        = 1,
		level_up_exp  = 7340,
		next_skill_id = 403004
	};

%% 龙牙突刺
skill_info(403004) ->
	#skill_info{
		mode_id       = 403004,
		class_id      = 403,
		type          = 4,
		effect        = 1,
		level_up_exp  = 16150,
		next_skill_id = 403005
	};

%% 龙牙突刺
skill_info(403005) ->
	#skill_info{
		mode_id       = 403005,
		class_id      = 403,
		type          = 4,
		effect        = 1,
		level_up_exp  = 39100,
		next_skill_id = 403006
	};

%% 龙牙突刺
skill_info(403006) ->
	#skill_info{
		mode_id       = 403006,
		class_id      = 403,
		type          = 4,
		effect        = 1,
		level_up_exp  = 76500,
		next_skill_id = 403007
	};

%% 龙牙突刺
skill_info(403007) ->
	#skill_info{
		mode_id       = 403007,
		class_id      = 403,
		type          = 4,
		effect        = 1,
		level_up_exp  = 154700,
		next_skill_id = 403008
	};

%% 龙牙突刺
skill_info(403008) ->
	#skill_info{
		mode_id       = 403008,
		class_id      = 403,
		type          = 4,
		effect        = 1,
		level_up_exp  = 289000,
		next_skill_id = 403009
	};

%% 龙牙突刺
skill_info(403009) ->
	#skill_info{
		mode_id       = 403009,
		class_id      = 403,
		type          = 4,
		effect        = 1,
		level_up_exp  = 527000,
		next_skill_id = 403010
	};

%% 龙牙突刺
skill_info(403010) ->
	#skill_info{
		mode_id       = 403010,
		class_id      = 403,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 虚空炽炎
skill_info(404001) ->
	#skill_info{
		mode_id       = 404001,
		class_id      = 404,
		type          = 4,
		effect        = 1,
		level_up_exp  = 550,
		next_skill_id = 404002
	};

%% 虚空炽炎
skill_info(404002) ->
	#skill_info{
		mode_id       = 404002,
		class_id      = 404,
		type          = 4,
		effect        = 1,
		level_up_exp  = 2570,
		next_skill_id = 404003
	};

%% 虚空炽炎
skill_info(404003) ->
	#skill_info{
		mode_id       = 404003,
		class_id      = 404,
		type          = 4,
		effect        = 1,
		level_up_exp  = 8210,
		next_skill_id = 404004
	};

%% 虚空炽炎
skill_info(404004) ->
	#skill_info{
		mode_id       = 404004,
		class_id      = 404,
		type          = 4,
		effect        = 1,
		level_up_exp  = 18050,
		next_skill_id = 404005
	};

%% 虚空炽炎
skill_info(404005) ->
	#skill_info{
		mode_id       = 404005,
		class_id      = 404,
		type          = 4,
		effect        = 1,
		level_up_exp  = 43700,
		next_skill_id = 404006
	};

%% 虚空炽炎
skill_info(404006) ->
	#skill_info{
		mode_id       = 404006,
		class_id      = 404,
		type          = 4,
		effect        = 1,
		level_up_exp  = 85500,
		next_skill_id = 404007
	};

%% 虚空炽炎
skill_info(404007) ->
	#skill_info{
		mode_id       = 404007,
		class_id      = 404,
		type          = 4,
		effect        = 1,
		level_up_exp  = 172900,
		next_skill_id = 404008
	};

%% 虚空炽炎
skill_info(404008) ->
	#skill_info{
		mode_id       = 404008,
		class_id      = 404,
		type          = 4,
		effect        = 1,
		level_up_exp  = 323000,
		next_skill_id = 404009
	};

%% 虚空炽炎
skill_info(404009) ->
	#skill_info{
		mode_id       = 404009,
		class_id      = 404,
		type          = 4,
		effect        = 1,
		level_up_exp  = 589000,
		next_skill_id = 404010
	};

%% 虚空炽炎
skill_info(404010) ->
	#skill_info{
		mode_id       = 404010,
		class_id      = 404,
		type          = 4,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 雄军云集
skill_info(405001) ->
	#skill_info{
		mode_id       = 405001,
		class_id      = 405,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	};

%% 万夫莫开
skill_info(406001) ->
	#skill_info{
		mode_id       = 406001,
		class_id      = 406,
		type          = 1,
		effect        = 1,
		level_up_exp  = 0,
		next_skill_id = 0
	}.


%%================================================
%% 根据刷新技能的个数获取对应消耗的银币
get_refresh_cost(1) -> 10000;

get_refresh_cost(2) -> 20000;

get_refresh_cost(3) -> 30000;

get_refresh_cost(4) -> 40000;

get_refresh_cost(5) -> 50000;

get_refresh_cost(6) -> 60000;

get_refresh_cost(7) -> 70000;

get_refresh_cost(8) -> 80000.


%%================================================
%% 根据刷新技能的个数获取对应消耗的银币
get_fixed_cost(1) -> 5;

get_fixed_cost(2) -> 20;

get_fixed_cost(3) -> 30;

get_fixed_cost(4) -> 40;

get_fixed_cost(5) -> 50;

get_fixed_cost(6) -> 60;

get_fixed_cost(7) -> 70;

get_fixed_cost(8) -> 80.


%%================================================
%% 根据平均天赋值获取对应的技能孔数量
get_skill_hole_nums(AverageTalent) when AverageTalent >= 350 -> 6;

get_skill_hole_nums(AverageTalent) when AverageTalent >= 300 -> 5;

get_skill_hole_nums(AverageTalent) when AverageTalent >= 250 -> 4;

get_skill_hole_nums(AverageTalent) when AverageTalent >= 200 -> 3;

get_skill_hole_nums(AverageTalent) when AverageTalent >= 150 -> 2;

get_skill_hole_nums(AverageTalent) when AverageTalent >= 60 -> 1;

get_skill_hole_nums(AverageTalent) when AverageTalent >= 0 -> 0.


%%================================================
%% 根据技能模型id获取其加成
get_role_added_attri(204001) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_baoji      = 5
	};

get_role_added_attri(204002) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_baoji      = 8
	};

get_role_added_attri(204003) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_baoji      = 13
	};

get_role_added_attri(204004) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_baoji      = 21
	};

get_role_added_attri(204005) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_baoji      = 32
	};

get_role_added_attri(204006) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_baoji      = 43
	};

get_role_added_attri(204007) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_baoji      = 55
	};

get_role_added_attri(204008) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_baoji      = 68
	};

get_role_added_attri(204009) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_baoji      = 83
	};

get_role_added_attri(204010) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_baoji      = 90
	};

get_role_added_attri(208001) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_def         = 40
	};

get_role_added_attri(208002) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_def         = 110
	};

get_role_added_attri(208003) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_def         = 220
	};

get_role_added_attri(208004) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_def         = 400
	};

get_role_added_attri(208005) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_def         = 640
	};

get_role_added_attri(208006) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_def         = 880
	};

get_role_added_attri(208007) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_def         = 1140
	};

get_role_added_attri(208008) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_def         = 1430
	};

get_role_added_attri(208009) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_def         = 1760
	};

get_role_added_attri(208010) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_def         = 2200
	};

get_role_added_attri(209001) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_def         = 40
	};

get_role_added_attri(209002) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_def         = 110
	};

get_role_added_attri(209003) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_def         = 220
	};

get_role_added_attri(209004) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_def         = 400
	};

get_role_added_attri(209005) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_def         = 640
	};

get_role_added_attri(209006) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_def         = 880
	};

get_role_added_attri(209007) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_def         = 1140
	};

get_role_added_attri(209008) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_def         = 1430
	};

get_role_added_attri(209009) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_def         = 1760
	};

get_role_added_attri(209010) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_def         = 2200
	};

get_role_added_attri(210001) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_speed      = 10
	};

get_role_added_attri(210002) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_speed      = 40
	};

get_role_added_attri(210003) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_speed      = 70
	};

get_role_added_attri(210004) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_speed      = 130
	};

get_role_added_attri(210005) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_speed      = 200
	};

get_role_added_attri(210006) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_speed      = 280
	};

get_role_added_attri(210007) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_speed      = 360
	};

get_role_added_attri(210008) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_speed      = 460
	};

get_role_added_attri(210009) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_speed      = 560
	};

get_role_added_attri(210010) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_speed      = 700
	};

get_role_added_attri(211001) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_att         = 30
	};

get_role_added_attri(211002) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_att         = 70
	};

get_role_added_attri(211003) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_att         = 130
	};

get_role_added_attri(211004) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_att         = 230
	};

get_role_added_attri(211005) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_att         = 380
	};

get_role_added_attri(211006) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_att         = 520
	};

get_role_added_attri(211007) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_att         = 680
	};

get_role_added_attri(211008) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_att         = 850
	};

get_role_added_attri(211009) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_att         = 1040
	};

get_role_added_attri(211010) ->
	#role_update_attri{
		gd_liliang    = 0,
		p_att         = 1300
	};

get_role_added_attri(212001) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_att         = 30
	};

get_role_added_attri(212002) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_att         = 70
	};

get_role_added_attri(212003) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_att         = 130
	};

get_role_added_attri(212004) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_att         = 230
	};

get_role_added_attri(212005) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_att         = 380
	};

get_role_added_attri(212006) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_att         = 520
	};

get_role_added_attri(212007) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_att         = 680
	};

get_role_added_attri(212008) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_att         = 850
	};

get_role_added_attri(212009) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_att         = 1040
	};

get_role_added_attri(212010) ->
	#role_update_attri{
		gd_liliang    = 0,
		m_att         = 1300
	};

get_role_added_attri(213001) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_maxHp      = 70
	};

get_role_added_attri(213002) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_maxHp      = 170
	};

get_role_added_attri(213003) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_maxHp      = 330
	};

get_role_added_attri(213004) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_maxHp      = 590
	};

get_role_added_attri(213005) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_maxHp      = 960
	};

get_role_added_attri(213006) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_maxHp      = 1320
	};

get_role_added_attri(213007) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_maxHp      = 1720
	};

get_role_added_attri(213008) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_maxHp      = 2150
	};

get_role_added_attri(213009) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_maxHp      = 2640
	};

get_role_added_attri(213010) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_maxHp      = 3300
	};

get_role_added_attri(215001) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_gedang     = 5
	};

get_role_added_attri(215002) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_gedang     = 14
	};

get_role_added_attri(215003) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_gedang     = 24
	};

get_role_added_attri(215004) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_gedang     = 33
	};

get_role_added_attri(215005) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_gedang     = 43
	};

get_role_added_attri(215006) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_gedang     = 52
	};

get_role_added_attri(215007) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_gedang     = 62
	};

get_role_added_attri(215008) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_gedang     = 71
	};

get_role_added_attri(215009) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_gedang     = 81
	};

get_role_added_attri(215010) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_gedang     = 90
	};

get_role_added_attri(216001) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_shanbi     = 5
	};

get_role_added_attri(216002) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_shanbi     = 8
	};

get_role_added_attri(216003) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_shanbi     = 13
	};

get_role_added_attri(216004) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_shanbi     = 21
	};

get_role_added_attri(216005) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_shanbi     = 32
	};

get_role_added_attri(216006) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_shanbi     = 43
	};

get_role_added_attri(216007) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_shanbi     = 55
	};

get_role_added_attri(216008) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_shanbi     = 68
	};

get_role_added_attri(216009) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_shanbi     = 83
	};

get_role_added_attri(216010) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_shanbi     = 100
	};

get_role_added_attri(217001) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_mingzhong  = 5
	};

get_role_added_attri(217002) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_mingzhong  = 8
	};

get_role_added_attri(217003) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_mingzhong  = 12
	};

get_role_added_attri(217004) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_mingzhong  = 19
	};

get_role_added_attri(217005) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_mingzhong  = 29
	};

get_role_added_attri(217006) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_mingzhong  = 39
	};

get_role_added_attri(217007) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_mingzhong  = 50
	};

get_role_added_attri(217008) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_mingzhong  = 62
	};

get_role_added_attri(217009) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_mingzhong  = 75
	};

get_role_added_attri(217010) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_mingzhong  = 90
	};

get_role_added_attri(218001) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_xingyun    = 5
	};

get_role_added_attri(218002) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_xingyun    = 8
	};

get_role_added_attri(218003) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_xingyun    = 12
	};

get_role_added_attri(218004) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_xingyun    = 19
	};

get_role_added_attri(218005) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_xingyun    = 29
	};

get_role_added_attri(218006) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_xingyun    = 39
	};

get_role_added_attri(218007) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_xingyun    = 50
	};

get_role_added_attri(218008) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_xingyun    = 62
	};

get_role_added_attri(218009) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_xingyun    = 75
	};

get_role_added_attri(218010) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_xingyun    = 150
	};

get_role_added_attri(220001) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_pojia      = 5
	};

get_role_added_attri(220002) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_pojia      = 8
	};

get_role_added_attri(220003) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_pojia      = 12
	};

get_role_added_attri(220004) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_pojia      = 19
	};

get_role_added_attri(220005) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_pojia      = 29
	};

get_role_added_attri(220006) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_pojia      = 39
	};

get_role_added_attri(220007) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_pojia      = 50
	};

get_role_added_attri(220008) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_pojia      = 62
	};

get_role_added_attri(220009) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_pojia      = 75
	};

get_role_added_attri(220010) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_pojia      = 90
	};

get_role_added_attri(221001) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_fanji      = 5
	};

get_role_added_attri(221002) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_fanji      = 8
	};

get_role_added_attri(221003) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_fanji      = 12
	};

get_role_added_attri(221004) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_fanji      = 19
	};

get_role_added_attri(221005) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_fanji      = 29
	};

get_role_added_attri(221006) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_fanji      = 39
	};

get_role_added_attri(221007) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_fanji      = 50
	};

get_role_added_attri(221008) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_fanji      = 62
	};

get_role_added_attri(221009) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_fanji      = 75
	};

get_role_added_attri(221010) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_fanji      = 90
	};

get_role_added_attri(222001) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_zhiming    = 5
	};

get_role_added_attri(222002) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_zhiming    = 8
	};

get_role_added_attri(222003) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_zhiming    = 12
	};

get_role_added_attri(222004) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_zhiming    = 19
	};

get_role_added_attri(222005) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_zhiming    = 29
	};

get_role_added_attri(222006) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_zhiming    = 39
	};

get_role_added_attri(222007) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_zhiming    = 50
	};

get_role_added_attri(222008) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_zhiming    = 62
	};

get_role_added_attri(222009) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_zhiming    = 75
	};

get_role_added_attri(222010) ->
	#role_update_attri{
		gd_liliang    = 0,
		gd_zhiming    = 160
	}.


%%================================================
%% 帮派技能消耗 技能ID -> {开启等级,消耗功勋,消耗银币} 
get_guild_skill_cost(301001) -> {4,1680, 16900};

get_guild_skill_cost(301002) -> {4,1920, 21100};

get_guild_skill_cost(301003) -> {4,2160, 26300};

get_guild_skill_cost(301004) -> {4,2520, 32900};

get_guild_skill_cost(301005) -> {4,2760, 41100};

get_guild_skill_cost(301006) -> {5,3240, 51400};

get_guild_skill_cost(301007) -> {5,3600, 56500};

get_guild_skill_cost(301008) -> {5,4080, 62100};

get_guild_skill_cost(301009) -> {5,4680, 68200};

get_guild_skill_cost(301010) -> {5,5280, 75000};

get_guild_skill_cost(301011) -> {5,5880, 82400};

get_guild_skill_cost(301012) -> {6,6600, 90600};

get_guild_skill_cost(301013) -> {6,7320, 99500};

get_guild_skill_cost(301014) -> {6,8160, 109400};

get_guild_skill_cost(301015) -> {6,9000, 120200};

get_guild_skill_cost(301016) -> {6,10080, 132100};

get_guild_skill_cost(301017) -> {6,11400, 145100};

get_guild_skill_cost(301018) -> {7,12960, 159500};

get_guild_skill_cost(301019) -> {7,14760, 175200};

get_guild_skill_cost(301020) -> {7,16680, 192600};

get_guild_skill_cost(301021) -> {8,18960, 211600};

get_guild_skill_cost(301022) -> {8,21600, 232500};

get_guild_skill_cost(301023) -> {8,24480, 255500};

get_guild_skill_cost(301024) -> {9,27840, 280800};

get_guild_skill_cost(301025) -> {9,31680, 308600};

get_guild_skill_cost(301026) -> {9,36000, 339100};

get_guild_skill_cost(301027) -> {9,40920, 372600};

get_guild_skill_cost(301028) -> {10,46440, 409500};

get_guild_skill_cost(301029) -> {10,52800, 450000};

get_guild_skill_cost(301030) -> {10,60000, 500000};

get_guild_skill_cost(302001) -> {4,1680, 16900};

get_guild_skill_cost(302002) -> {4,1920, 21100};

get_guild_skill_cost(302003) -> {4,2160, 26300};

get_guild_skill_cost(302004) -> {4,2520, 32900};

get_guild_skill_cost(302005) -> {4,2760, 41100};

get_guild_skill_cost(302006) -> {5,3240, 51400};

get_guild_skill_cost(302007) -> {5,3600, 56500};

get_guild_skill_cost(302008) -> {5,4080, 62100};

get_guild_skill_cost(302009) -> {5,4680, 68200};

get_guild_skill_cost(302010) -> {5,5280, 75000};

get_guild_skill_cost(302011) -> {5,5880, 82400};

get_guild_skill_cost(302012) -> {6,6600, 90600};

get_guild_skill_cost(302013) -> {6,7320, 99500};

get_guild_skill_cost(302014) -> {6,8160, 109400};

get_guild_skill_cost(302015) -> {6,9000, 120200};

get_guild_skill_cost(302016) -> {6,10080, 132100};

get_guild_skill_cost(302017) -> {6,11400, 145100};

get_guild_skill_cost(302018) -> {7,12960, 159500};

get_guild_skill_cost(302019) -> {7,14760, 175200};

get_guild_skill_cost(302020) -> {7,16680, 192600};

get_guild_skill_cost(302021) -> {8,18960, 211600};

get_guild_skill_cost(302022) -> {8,21600, 232500};

get_guild_skill_cost(302023) -> {8,24480, 255500};

get_guild_skill_cost(302024) -> {9,27840, 280800};

get_guild_skill_cost(302025) -> {9,31680, 308600};

get_guild_skill_cost(302026) -> {9,36000, 339100};

get_guild_skill_cost(302027) -> {9,40920, 372600};

get_guild_skill_cost(302028) -> {10,46440, 409500};

get_guild_skill_cost(302029) -> {10,52800, 450000};

get_guild_skill_cost(302030) -> {10,60000, 500000};

get_guild_skill_cost(303001) -> {1,1400, 16900};

get_guild_skill_cost(303002) -> {1,1600, 21100};

get_guild_skill_cost(303003) -> {1,1800, 26300};

get_guild_skill_cost(303004) -> {1,2100, 32900};

get_guild_skill_cost(303005) -> {1,2300, 41100};

get_guild_skill_cost(303006) -> {2,2700, 51400};

get_guild_skill_cost(303007) -> {2,3000, 56500};

get_guild_skill_cost(303008) -> {2,3400, 62100};

get_guild_skill_cost(303009) -> {3,3900, 68200};

get_guild_skill_cost(303010) -> {3,4400, 75000};

get_guild_skill_cost(303011) -> {4,4900, 82400};

get_guild_skill_cost(303012) -> {4,5500, 90600};

get_guild_skill_cost(303013) -> {4,6100, 99500};

get_guild_skill_cost(303014) -> {5,6800, 109400};

get_guild_skill_cost(303015) -> {5,7500, 120200};

get_guild_skill_cost(303016) -> {5,8400, 132100};

get_guild_skill_cost(303017) -> {6,9500, 145100};

get_guild_skill_cost(303018) -> {6,10800, 159500};

get_guild_skill_cost(303019) -> {6,12300, 175200};

get_guild_skill_cost(303020) -> {7,13900, 192600};

get_guild_skill_cost(303021) -> {7,15800, 211600};

get_guild_skill_cost(303022) -> {7,18000, 232500};

get_guild_skill_cost(303023) -> {8,20400, 255500};

get_guild_skill_cost(303024) -> {8,23200, 280800};

get_guild_skill_cost(303025) -> {8,26400, 308600};

get_guild_skill_cost(303026) -> {9,30000, 339100};

get_guild_skill_cost(303027) -> {9,34100, 372600};

get_guild_skill_cost(303028) -> {9,38700, 409500};

get_guild_skill_cost(303029) -> {10,44000, 450000};

get_guild_skill_cost(303030) -> {10,50000, 500000};

get_guild_skill_cost(304001) -> {1,1400, 16900};

get_guild_skill_cost(304002) -> {1,1600, 21100};

get_guild_skill_cost(304003) -> {1,1800, 26300};

get_guild_skill_cost(304004) -> {1,2100, 32900};

get_guild_skill_cost(304005) -> {2,2300, 41100};

get_guild_skill_cost(304006) -> {2,2700, 51400};

get_guild_skill_cost(304007) -> {2,3000, 56500};

get_guild_skill_cost(304008) -> {3,3400, 62100};

get_guild_skill_cost(304009) -> {3,3900, 68200};

get_guild_skill_cost(304010) -> {3,4400, 75000};

get_guild_skill_cost(304011) -> {4,4900, 82400};

get_guild_skill_cost(304012) -> {4,5500, 90600};

get_guild_skill_cost(304013) -> {4,6100, 99500};

get_guild_skill_cost(304014) -> {5,6800, 109400};

get_guild_skill_cost(304015) -> {5,7500, 120200};

get_guild_skill_cost(304016) -> {5,8400, 132100};

get_guild_skill_cost(304017) -> {6,9500, 145100};

get_guild_skill_cost(304018) -> {6,10800, 159500};

get_guild_skill_cost(304019) -> {6,12300, 175200};

get_guild_skill_cost(304020) -> {7,13900, 192600};

get_guild_skill_cost(304021) -> {7,15800, 211600};

get_guild_skill_cost(304022) -> {7,18000, 232500};

get_guild_skill_cost(304023) -> {8,20400, 255500};

get_guild_skill_cost(304024) -> {8,23200, 280800};

get_guild_skill_cost(304025) -> {8,26400, 308600};

get_guild_skill_cost(304026) -> {9,30000, 339100};

get_guild_skill_cost(304027) -> {9,34100, 372600};

get_guild_skill_cost(304028) -> {9,38700, 409500};

get_guild_skill_cost(304029) -> {10,44000, 450000};

get_guild_skill_cost(304030) -> {10,50000, 500000};

get_guild_skill_cost(305001) -> {4,1400, 16900};

get_guild_skill_cost(305002) -> {4,1600, 21100};

get_guild_skill_cost(305003) -> {4,1800, 26300};

get_guild_skill_cost(305004) -> {4,2100, 32900};

get_guild_skill_cost(305005) -> {4,2300, 41100};

get_guild_skill_cost(305006) -> {5,2700, 51400};

get_guild_skill_cost(305007) -> {5,3000, 56500};

get_guild_skill_cost(305008) -> {5,3400, 62100};

get_guild_skill_cost(305009) -> {5,3900, 68200};

get_guild_skill_cost(305010) -> {5,4400, 75000};

get_guild_skill_cost(305011) -> {5,4900, 82400};

get_guild_skill_cost(305012) -> {6,5500, 90600};

get_guild_skill_cost(305013) -> {6,6100, 99500};

get_guild_skill_cost(305014) -> {6,6800, 109400};

get_guild_skill_cost(305015) -> {6,7500, 120200};

get_guild_skill_cost(305016) -> {6,8400, 132100};

get_guild_skill_cost(305017) -> {6,9500, 145100};

get_guild_skill_cost(305018) -> {7,10800, 159500};

get_guild_skill_cost(305019) -> {7,12300, 175200};

get_guild_skill_cost(305020) -> {7,13900, 192600};

get_guild_skill_cost(305021) -> {7,15800, 211600};

get_guild_skill_cost(305022) -> {7,18000, 232500};

get_guild_skill_cost(305023) -> {8,20400, 255500};

get_guild_skill_cost(305024) -> {8,23200, 280800};

get_guild_skill_cost(305025) -> {8,26400, 308600};

get_guild_skill_cost(305026) -> {8,30000, 339100};

get_guild_skill_cost(305027) -> {9,34100, 372600};

get_guild_skill_cost(305028) -> {9,38700, 409500};

get_guild_skill_cost(305029) -> {9,44000, 450000};

get_guild_skill_cost(305030) -> {9,50000, 500000};

get_guild_skill_cost(306001) -> {2,1400, 16900};

get_guild_skill_cost(306002) -> {2,1600, 21100};

get_guild_skill_cost(306003) -> {2,1800, 26300};

get_guild_skill_cost(306004) -> {3,2100, 32900};

get_guild_skill_cost(306005) -> {3,2300, 41100};

get_guild_skill_cost(306006) -> {3,2700, 51400};

get_guild_skill_cost(306007) -> {4,3000, 56500};

get_guild_skill_cost(306008) -> {4,3400, 62100};

get_guild_skill_cost(306009) -> {4,3900, 68200};

get_guild_skill_cost(306010) -> {4,4400, 75000};

get_guild_skill_cost(306011) -> {5,4900, 82400};

get_guild_skill_cost(306012) -> {5,5500, 90600};

get_guild_skill_cost(306013) -> {5,6100, 99500};

get_guild_skill_cost(306014) -> {5,6800, 109400};

get_guild_skill_cost(306015) -> {5,7500, 120200};

get_guild_skill_cost(306016) -> {6,8400, 132100};

get_guild_skill_cost(306017) -> {6,9500, 145100};

get_guild_skill_cost(306018) -> {6,10800, 159500};

get_guild_skill_cost(306019) -> {6,12300, 175200};

get_guild_skill_cost(306020) -> {7,13900, 192600};

get_guild_skill_cost(306021) -> {7,15800, 211600};

get_guild_skill_cost(306022) -> {7,18000, 232500};

get_guild_skill_cost(306023) -> {7,20400, 255500};

get_guild_skill_cost(306024) -> {8,23200, 280800};

get_guild_skill_cost(306025) -> {8,26400, 308600};

get_guild_skill_cost(306026) -> {8,30000, 339100};

get_guild_skill_cost(306027) -> {9,34100, 372600};

get_guild_skill_cost(306028) -> {9,38700, 409500};

get_guild_skill_cost(306029) -> {10,44000, 450000};

get_guild_skill_cost(306030) -> {10,50000, 500000};

get_guild_skill_cost(307001) -> {2,1400, 16900};

get_guild_skill_cost(307002) -> {2,1600, 21100};

get_guild_skill_cost(307003) -> {2,1800, 26300};

get_guild_skill_cost(307004) -> {3,2100, 32900};

get_guild_skill_cost(307005) -> {3,2300, 41100};

get_guild_skill_cost(307006) -> {3,2700, 51400};

get_guild_skill_cost(307007) -> {4,3000, 56500};

get_guild_skill_cost(307008) -> {4,3400, 62100};

get_guild_skill_cost(307009) -> {4,3900, 68200};

get_guild_skill_cost(307010) -> {4,4400, 75000};

get_guild_skill_cost(307011) -> {5,4900, 82400};

get_guild_skill_cost(307012) -> {5,5500, 90600};

get_guild_skill_cost(307013) -> {5,6100, 99500};

get_guild_skill_cost(307014) -> {5,6800, 109400};

get_guild_skill_cost(307015) -> {5,7500, 120200};

get_guild_skill_cost(307016) -> {6,8400, 132100};

get_guild_skill_cost(307017) -> {6,9500, 145100};

get_guild_skill_cost(307018) -> {6,10800, 159500};

get_guild_skill_cost(307019) -> {6,12300, 175200};

get_guild_skill_cost(307020) -> {7,13900, 192600};

get_guild_skill_cost(307021) -> {7,15800, 211600};

get_guild_skill_cost(307022) -> {7,18000, 232500};

get_guild_skill_cost(307023) -> {7,20400, 255500};

get_guild_skill_cost(307024) -> {8,23200, 280800};

get_guild_skill_cost(307025) -> {8,26400, 308600};

get_guild_skill_cost(307026) -> {8,30000, 339100};

get_guild_skill_cost(307027) -> {9,34100, 372600};

get_guild_skill_cost(307028) -> {9,38700, 409500};

get_guild_skill_cost(307029) -> {10,44000, 450000};

get_guild_skill_cost(307030) -> {10,50000, 500000};

get_guild_skill_cost(308001) -> {2,1400, 16900};

get_guild_skill_cost(308002) -> {2,1600, 21100};

get_guild_skill_cost(308003) -> {2,1800, 26300};

get_guild_skill_cost(308004) -> {3,2100, 32900};

get_guild_skill_cost(308005) -> {3,2300, 41100};

get_guild_skill_cost(308006) -> {3,2700, 51400};

get_guild_skill_cost(308007) -> {4,3000, 56500};

get_guild_skill_cost(308008) -> {4,3400, 62100};

get_guild_skill_cost(308009) -> {4,3900, 68200};

get_guild_skill_cost(308010) -> {4,4400, 75000};

get_guild_skill_cost(308011) -> {4,4900, 82400};

get_guild_skill_cost(308012) -> {5,5500, 90600};

get_guild_skill_cost(308013) -> {5,6100, 99500};

get_guild_skill_cost(308014) -> {5,6800, 109400};

get_guild_skill_cost(308015) -> {5,7500, 120200};

get_guild_skill_cost(308016) -> {6,8400, 132100};

get_guild_skill_cost(308017) -> {6,9500, 145100};

get_guild_skill_cost(308018) -> {6,10800, 159500};

get_guild_skill_cost(308019) -> {6,12300, 175200};

get_guild_skill_cost(308020) -> {7,13900, 192600};

get_guild_skill_cost(308021) -> {7,15800, 211600};

get_guild_skill_cost(308022) -> {7,18000, 232500};

get_guild_skill_cost(308023) -> {7,20400, 255500};

get_guild_skill_cost(308024) -> {8,23200, 280800};

get_guild_skill_cost(308025) -> {8,26400, 308600};

get_guild_skill_cost(308026) -> {8,30000, 339100};

get_guild_skill_cost(308027) -> {9,34100, 372600};

get_guild_skill_cost(308028) -> {9,38700, 409500};

get_guild_skill_cost(308029) -> {10,44000, 450000};

get_guild_skill_cost(308030) -> {10,50000, 500000};

get_guild_skill_cost(309001) -> {3,1400, 16900};

get_guild_skill_cost(309002) -> {3,1600, 21100};

get_guild_skill_cost(309003) -> {3,1800, 26300};

get_guild_skill_cost(309004) -> {3,2100, 32900};

get_guild_skill_cost(309005) -> {3,2300, 41100};

get_guild_skill_cost(309006) -> {3,2700, 51400};

get_guild_skill_cost(309007) -> {4,3000, 56500};

get_guild_skill_cost(309008) -> {4,3400, 62100};

get_guild_skill_cost(309009) -> {4,3900, 68200};

get_guild_skill_cost(309010) -> {4,4400, 75000};

get_guild_skill_cost(309011) -> {4,4900, 82400};

get_guild_skill_cost(309012) -> {5,5500, 90600};

get_guild_skill_cost(309013) -> {5,6100, 99500};

get_guild_skill_cost(309014) -> {5,6800, 109400};

get_guild_skill_cost(309015) -> {5,7500, 120200};

get_guild_skill_cost(309016) -> {6,8400, 132100};

get_guild_skill_cost(309017) -> {6,9500, 145100};

get_guild_skill_cost(309018) -> {6,10800, 159500};

get_guild_skill_cost(309019) -> {6,12300, 175200};

get_guild_skill_cost(309020) -> {7,13900, 192600};

get_guild_skill_cost(309021) -> {7,15800, 211600};

get_guild_skill_cost(309022) -> {7,18000, 232500};

get_guild_skill_cost(309023) -> {7,20400, 255500};

get_guild_skill_cost(309024) -> {8,23200, 280800};

get_guild_skill_cost(309025) -> {8,26400, 308600};

get_guild_skill_cost(309026) -> {8,30000, 339100};

get_guild_skill_cost(309027) -> {9,34100, 372600};

get_guild_skill_cost(309028) -> {9,38700, 409500};

get_guild_skill_cost(309029) -> {10,44000, 450000};

get_guild_skill_cost(309030) -> {10,50000, 500000};

get_guild_skill_cost(310001) -> {1,1400, 16900};

get_guild_skill_cost(310002) -> {1,1600, 21100};

get_guild_skill_cost(310003) -> {1,1800, 26300};

get_guild_skill_cost(310004) -> {1,2100, 32900};

get_guild_skill_cost(310005) -> {1,2300, 41100};

get_guild_skill_cost(310006) -> {2,2700, 51400};

get_guild_skill_cost(310007) -> {2,3000, 56500};

get_guild_skill_cost(310008) -> {2,3400, 62100};

get_guild_skill_cost(310009) -> {3,3900, 68200};

get_guild_skill_cost(310010) -> {3,4400, 75000};

get_guild_skill_cost(310011) -> {4,4900, 82400};

get_guild_skill_cost(310012) -> {4,5500, 90600};

get_guild_skill_cost(310013) -> {4,6100, 99500};

get_guild_skill_cost(310014) -> {5,6800, 109400};

get_guild_skill_cost(310015) -> {5,7500, 120200};

get_guild_skill_cost(310016) -> {5,8400, 132100};

get_guild_skill_cost(310017) -> {6,9500, 145100};

get_guild_skill_cost(310018) -> {6,10800, 159500};

get_guild_skill_cost(310019) -> {6,12300, 175200};

get_guild_skill_cost(310020) -> {7,13900, 192600};

get_guild_skill_cost(310021) -> {7,15800, 211600};

get_guild_skill_cost(310022) -> {7,18000, 232500};

get_guild_skill_cost(310023) -> {8,20400, 255500};

get_guild_skill_cost(310024) -> {8,23200, 280800};

get_guild_skill_cost(310025) -> {8,26400, 308600};

get_guild_skill_cost(310026) -> {9,30000, 339100};

get_guild_skill_cost(310027) -> {9,34100, 372600};

get_guild_skill_cost(310028) -> {9,38700, 409500};

get_guild_skill_cost(310029) -> {10,44000, 450000};

get_guild_skill_cost(310030) -> {10,50000, 500000};

get_guild_skill_cost(311001) -> {1,1400, 16900};

get_guild_skill_cost(311002) -> {1,1600, 21100};

get_guild_skill_cost(311003) -> {1,1800, 26300};

get_guild_skill_cost(311004) -> {1,2100, 32900};

get_guild_skill_cost(311005) -> {2,2300, 41100};

get_guild_skill_cost(311006) -> {2,2700, 51400};

get_guild_skill_cost(311007) -> {2,3000, 56500};

get_guild_skill_cost(311008) -> {3,3400, 62100};

get_guild_skill_cost(311009) -> {3,3900, 68200};

get_guild_skill_cost(311010) -> {3,4400, 75000};

get_guild_skill_cost(311011) -> {4,4900, 82400};

get_guild_skill_cost(311012) -> {4,5500, 90600};

get_guild_skill_cost(311013) -> {4,6100, 99500};

get_guild_skill_cost(311014) -> {5,6800, 109400};

get_guild_skill_cost(311015) -> {5,7500, 120200};

get_guild_skill_cost(311016) -> {5,8400, 132100};

get_guild_skill_cost(311017) -> {6,9500, 145100};

get_guild_skill_cost(311018) -> {6,10800, 159500};

get_guild_skill_cost(311019) -> {6,12300, 175200};

get_guild_skill_cost(311020) -> {7,13900, 192600};

get_guild_skill_cost(311021) -> {7,15800, 211600};

get_guild_skill_cost(311022) -> {7,18000, 232500};

get_guild_skill_cost(311023) -> {8,20400, 255500};

get_guild_skill_cost(311024) -> {8,23200, 280800};

get_guild_skill_cost(311025) -> {8,26400, 308600};

get_guild_skill_cost(311026) -> {9,30000, 339100};

get_guild_skill_cost(311027) -> {9,34100, 372600};

get_guild_skill_cost(311028) -> {9,38700, 409500};

get_guild_skill_cost(311029) -> {10,44000, 450000};

get_guild_skill_cost(311030) -> {10,50000, 500000};

get_guild_skill_cost(312001) -> {3,1400, 16900};

get_guild_skill_cost(312002) -> {3,1600, 21100};

get_guild_skill_cost(312003) -> {3,1800, 26300};

get_guild_skill_cost(312004) -> {3,2100, 32900};

get_guild_skill_cost(312005) -> {3,2300, 41100};

get_guild_skill_cost(312006) -> {3,2700, 51400};

get_guild_skill_cost(312007) -> {3,3000, 56500};

get_guild_skill_cost(312008) -> {4,3400, 62100};

get_guild_skill_cost(312009) -> {4,3900, 68200};

get_guild_skill_cost(312010) -> {4,4400, 75000};

get_guild_skill_cost(312011) -> {4,4900, 82400};

get_guild_skill_cost(312012) -> {4,5500, 90600};

get_guild_skill_cost(312013) -> {4,6100, 99500};

get_guild_skill_cost(312014) -> {5,6800, 109400};

get_guild_skill_cost(312015) -> {5,7500, 120200};

get_guild_skill_cost(312016) -> {5,8400, 132100};

get_guild_skill_cost(312017) -> {6,9500, 145100};

get_guild_skill_cost(312018) -> {6,10800, 159500};

get_guild_skill_cost(312019) -> {6,12300, 175200};

get_guild_skill_cost(312020) -> {7,13900, 192600};

get_guild_skill_cost(312021) -> {7,15800, 211600};

get_guild_skill_cost(312022) -> {7,18000, 232500};

get_guild_skill_cost(312023) -> {8,20400, 255500};

get_guild_skill_cost(312024) -> {8,23200, 280800};

get_guild_skill_cost(312025) -> {8,26400, 308600};

get_guild_skill_cost(312026) -> {9,30000, 339100};

get_guild_skill_cost(312027) -> {9,34100, 372600};

get_guild_skill_cost(312028) -> {9,38700, 409500};

get_guild_skill_cost(312029) -> {10,44000, 450000};

get_guild_skill_cost(312030) -> {10,50000, 500000};

get_guild_skill_cost(313001) -> {5,1400, 16900};

get_guild_skill_cost(313002) -> {5,1600, 21100};

get_guild_skill_cost(313003) -> {5,1800, 26300};

get_guild_skill_cost(313004) -> {5,2100, 32900};

get_guild_skill_cost(313005) -> {5,2300, 41100};

get_guild_skill_cost(313006) -> {5,2700, 51400};

get_guild_skill_cost(313007) -> {5,3000, 56500};

get_guild_skill_cost(313008) -> {5,3400, 62100};

get_guild_skill_cost(313009) -> {6,3900, 68200};

get_guild_skill_cost(313010) -> {6,4400, 75000};

get_guild_skill_cost(313011) -> {6,4900, 82400};

get_guild_skill_cost(313012) -> {6,5500, 90600};

get_guild_skill_cost(313013) -> {6,6100, 99500};

get_guild_skill_cost(313014) -> {6,6800, 109400};

get_guild_skill_cost(313015) -> {6,7500, 120200};

get_guild_skill_cost(313016) -> {6,8400, 132100};

get_guild_skill_cost(313017) -> {6,9500, 145100};

get_guild_skill_cost(313018) -> {7,10800, 159500};

get_guild_skill_cost(313019) -> {7,12300, 175200};

get_guild_skill_cost(313020) -> {7,13900, 192600};

get_guild_skill_cost(313021) -> {7,15800, 211600};

get_guild_skill_cost(313022) -> {7,18000, 232500};

get_guild_skill_cost(313023) -> {8,20400, 255500};

get_guild_skill_cost(313024) -> {8,23200, 280800};

get_guild_skill_cost(313025) -> {8,26400, 308600};

get_guild_skill_cost(313026) -> {9,30000, 339100};

get_guild_skill_cost(313027) -> {9,34100, 372600};

get_guild_skill_cost(313028) -> {9,38700, 409500};

get_guild_skill_cost(313029) -> {10,44000, 450000};

get_guild_skill_cost(313030) -> {10,50000, 500000};

get_guild_skill_cost(314001) -> {4,1400, 16900};

get_guild_skill_cost(314002) -> {4,1600, 21100};

get_guild_skill_cost(314003) -> {4,1800, 26300};

get_guild_skill_cost(314004) -> {4,2100, 32900};

get_guild_skill_cost(314005) -> {4,2300, 41100};

get_guild_skill_cost(314006) -> {5,2700, 51400};

get_guild_skill_cost(314007) -> {5,3000, 56500};

get_guild_skill_cost(314008) -> {5,3400, 62100};

get_guild_skill_cost(314009) -> {5,3900, 68200};

get_guild_skill_cost(314010) -> {5,4400, 75000};

get_guild_skill_cost(314011) -> {5,4900, 82400};

get_guild_skill_cost(314012) -> {6,5500, 90600};

get_guild_skill_cost(314013) -> {6,6100, 99500};

get_guild_skill_cost(314014) -> {6,6800, 109400};

get_guild_skill_cost(314015) -> {6,7500, 120200};

get_guild_skill_cost(314016) -> {6,8400, 132100};

get_guild_skill_cost(314017) -> {6,9500, 145100};

get_guild_skill_cost(314018) -> {7,10800, 159500};

get_guild_skill_cost(314019) -> {7,12300, 175200};

get_guild_skill_cost(314020) -> {7,13900, 192600};

get_guild_skill_cost(314021) -> {7,15800, 211600};

get_guild_skill_cost(314022) -> {7,18000, 232500};

get_guild_skill_cost(314023) -> {8,20400, 255500};

get_guild_skill_cost(314024) -> {8,23200, 280800};

get_guild_skill_cost(314025) -> {8,26400, 308600};

get_guild_skill_cost(314026) -> {8,30000, 339100};

get_guild_skill_cost(314027) -> {9,34100, 372600};

get_guild_skill_cost(314028) -> {9,38700, 409500};

get_guild_skill_cost(314029) -> {9,44000, 450000};

get_guild_skill_cost(314030) -> {9,50000, 500000}.


%%================================================
%% 帮派技能消耗 技能ID -> {属性类型,属性加成} 
get_guild_skill_attri(301001) -> {10,30};

get_guild_skill_attri(301002) -> {10,60};

get_guild_skill_attri(301003) -> {10,90};

get_guild_skill_attri(301004) -> {10,110};

get_guild_skill_attri(301005) -> {10,140};

get_guild_skill_attri(301006) -> {10,170};

get_guild_skill_attri(301007) -> {10,210};

get_guild_skill_attri(301008) -> {10,260};

get_guild_skill_attri(301009) -> {10,300};

get_guild_skill_attri(301010) -> {10,340};

get_guild_skill_attri(301011) -> {10,390};

get_guild_skill_attri(301012) -> {10,430};

get_guild_skill_attri(301013) -> {10,470};

get_guild_skill_attri(301014) -> {10,510};

get_guild_skill_attri(301015) -> {10,570};

get_guild_skill_attri(301016) -> {10,630};

get_guild_skill_attri(301017) -> {10,690};

get_guild_skill_attri(301018) -> {10,740};

get_guild_skill_attri(301019) -> {10,800};

get_guild_skill_attri(301020) -> {10,860};

get_guild_skill_attri(301021) -> {10,920};

get_guild_skill_attri(301022) -> {10,970};

get_guild_skill_attri(301023) -> {10,1030};

get_guild_skill_attri(301024) -> {10,1090};

get_guild_skill_attri(301025) -> {10,1140};

get_guild_skill_attri(301026) -> {10,1200};

get_guild_skill_attri(301027) -> {10,1260};

get_guild_skill_attri(301028) -> {10,1320};

get_guild_skill_attri(301029) -> {10,1370};

get_guild_skill_attri(301030) -> {10,1430};

get_guild_skill_attri(302001) -> {11,30};

get_guild_skill_attri(302002) -> {11,60};

get_guild_skill_attri(302003) -> {11,90};

get_guild_skill_attri(302004) -> {11,110};

get_guild_skill_attri(302005) -> {11,140};

get_guild_skill_attri(302006) -> {11,170};

get_guild_skill_attri(302007) -> {11,210};

get_guild_skill_attri(302008) -> {11,260};

get_guild_skill_attri(302009) -> {11,300};

get_guild_skill_attri(302010) -> {11,340};

get_guild_skill_attri(302011) -> {11,390};

get_guild_skill_attri(302012) -> {11,430};

get_guild_skill_attri(302013) -> {11,470};

get_guild_skill_attri(302014) -> {11,510};

get_guild_skill_attri(302015) -> {11,570};

get_guild_skill_attri(302016) -> {11,630};

get_guild_skill_attri(302017) -> {11,690};

get_guild_skill_attri(302018) -> {11,740};

get_guild_skill_attri(302019) -> {11,800};

get_guild_skill_attri(302020) -> {11,860};

get_guild_skill_attri(302021) -> {11,920};

get_guild_skill_attri(302022) -> {11,970};

get_guild_skill_attri(302023) -> {11,1030};

get_guild_skill_attri(302024) -> {11,1090};

get_guild_skill_attri(302025) -> {11,1140};

get_guild_skill_attri(302026) -> {11,1200};

get_guild_skill_attri(302027) -> {11,1260};

get_guild_skill_attri(302028) -> {11,1320};

get_guild_skill_attri(302029) -> {11,1370};

get_guild_skill_attri(302030) -> {11,1430};

get_guild_skill_attri(303001) -> {1,50};

get_guild_skill_attri(303002) -> {1,100};

get_guild_skill_attri(303003) -> {1,140};

get_guild_skill_attri(303004) -> {1,190};

get_guild_skill_attri(303005) -> {1,240};

get_guild_skill_attri(303006) -> {1,290};

get_guild_skill_attri(303007) -> {1,360};

get_guild_skill_attri(303008) -> {1,430};

get_guild_skill_attri(303009) -> {1,500};

get_guild_skill_attri(303010) -> {1,570};

get_guild_skill_attri(303011) -> {1,640};

get_guild_skill_attri(303012) -> {1,710};

get_guild_skill_attri(303013) -> {1,790};

get_guild_skill_attri(303014) -> {1,860};

get_guild_skill_attri(303015) -> {1,950};

get_guild_skill_attri(303016) -> {1,1050};

get_guild_skill_attri(303017) -> {1,1140};

get_guild_skill_attri(303018) -> {1,1240};

get_guild_skill_attri(303019) -> {1,1330};

get_guild_skill_attri(303020) -> {1,1430};

get_guild_skill_attri(303021) -> {1,1520};

get_guild_skill_attri(303022) -> {1,1620};

get_guild_skill_attri(303023) -> {1,1710};

get_guild_skill_attri(303024) -> {1,1810};

get_guild_skill_attri(303025) -> {1,1900};

get_guild_skill_attri(303026) -> {1,2000};

get_guild_skill_attri(303027) -> {1,2090};

get_guild_skill_attri(303028) -> {1,2190};

get_guild_skill_attri(303029) -> {1,2280};

get_guild_skill_attri(303030) -> {1,2380};

get_guild_skill_attri(304001) -> {2,50};

get_guild_skill_attri(304002) -> {2,100};

get_guild_skill_attri(304003) -> {2,140};

get_guild_skill_attri(304004) -> {2,190};

get_guild_skill_attri(304005) -> {2,240};

get_guild_skill_attri(304006) -> {2,290};

get_guild_skill_attri(304007) -> {2,360};

get_guild_skill_attri(304008) -> {2,430};

get_guild_skill_attri(304009) -> {2,500};

get_guild_skill_attri(304010) -> {2,570};

get_guild_skill_attri(304011) -> {2,640};

get_guild_skill_attri(304012) -> {2,710};

get_guild_skill_attri(304013) -> {2,790};

get_guild_skill_attri(304014) -> {2,860};

get_guild_skill_attri(304015) -> {2,950};

get_guild_skill_attri(304016) -> {2,1050};

get_guild_skill_attri(304017) -> {2,1140};

get_guild_skill_attri(304018) -> {2,1240};

get_guild_skill_attri(304019) -> {2,1330};

get_guild_skill_attri(304020) -> {2,1430};

get_guild_skill_attri(304021) -> {2,1520};

get_guild_skill_attri(304022) -> {2,1620};

get_guild_skill_attri(304023) -> {2,1710};

get_guild_skill_attri(304024) -> {2,1810};

get_guild_skill_attri(304025) -> {2,1900};

get_guild_skill_attri(304026) -> {2,2000};

get_guild_skill_attri(304027) -> {2,2090};

get_guild_skill_attri(304028) -> {2,2190};

get_guild_skill_attri(304029) -> {2,2280};

get_guild_skill_attri(304030) -> {2,2380};

get_guild_skill_attri(305001) -> {12,20};

get_guild_skill_attri(305002) -> {12,30};

get_guild_skill_attri(305003) -> {12,50};

get_guild_skill_attri(305004) -> {12,70};

get_guild_skill_attri(305005) -> {12,80};

get_guild_skill_attri(305006) -> {12,100};

get_guild_skill_attri(305007) -> {12,130};

get_guild_skill_attri(305008) -> {12,150};

get_guild_skill_attri(305009) -> {12,180};

get_guild_skill_attri(305010) -> {12,200};

get_guild_skill_attri(305011) -> {12,230};

get_guild_skill_attri(305012) -> {12,250};

get_guild_skill_attri(305013) -> {12,280};

get_guild_skill_attri(305014) -> {12,300};

get_guild_skill_attri(305015) -> {12,340};

get_guild_skill_attri(305016) -> {12,370};

get_guild_skill_attri(305017) -> {12,400};

get_guild_skill_attri(305018) -> {12,440};

get_guild_skill_attri(305019) -> {12,470};

get_guild_skill_attri(305020) -> {12,500};

get_guild_skill_attri(305021) -> {12,540};

get_guild_skill_attri(305022) -> {12,570};

get_guild_skill_attri(305023) -> {12,600};

get_guild_skill_attri(305024) -> {12,640};

get_guild_skill_attri(305025) -> {12,670};

get_guild_skill_attri(305026) -> {12,710};

get_guild_skill_attri(305027) -> {12,740};

get_guild_skill_attri(305028) -> {12,770};

get_guild_skill_attri(305029) -> {12,810};

get_guild_skill_attri(305030) -> {12,840};

get_guild_skill_attri(306001) -> {5,70};

get_guild_skill_attri(306002) -> {5,140};

get_guild_skill_attri(306003) -> {5,210};

get_guild_skill_attri(306004) -> {5,290};

get_guild_skill_attri(306005) -> {5,360};

get_guild_skill_attri(306006) -> {5,430};

get_guild_skill_attri(306007) -> {5,540};

get_guild_skill_attri(306008) -> {5,640};

get_guild_skill_attri(306009) -> {5,750};

get_guild_skill_attri(306010) -> {5,860};

get_guild_skill_attri(306011) -> {5,960};

get_guild_skill_attri(306012) -> {5,1070};

get_guild_skill_attri(306013) -> {5,1180};

get_guild_skill_attri(306014) -> {5,1290};

get_guild_skill_attri(306015) -> {5,1430};

get_guild_skill_attri(306016) -> {5,1570};

get_guild_skill_attri(306017) -> {5,1710};

get_guild_skill_attri(306018) -> {5,1860};

get_guild_skill_attri(306019) -> {5,2000};

get_guild_skill_attri(306020) -> {5,2140};

get_guild_skill_attri(306021) -> {5,2280};

get_guild_skill_attri(306022) -> {5,2430};

get_guild_skill_attri(306023) -> {5,2570};

get_guild_skill_attri(306024) -> {5,2710};

get_guild_skill_attri(306025) -> {5,2860};

get_guild_skill_attri(306026) -> {5,3000};

get_guild_skill_attri(306027) -> {5,3140};

get_guild_skill_attri(306028) -> {5,3280};

get_guild_skill_attri(306029) -> {5,3430};

get_guild_skill_attri(306030) -> {5,3570};

get_guild_skill_attri(307001) -> {6,3};

get_guild_skill_attri(307002) -> {6,6};

get_guild_skill_attri(307003) -> {6,9};

get_guild_skill_attri(307004) -> {6,12};

get_guild_skill_attri(307005) -> {6,15};

get_guild_skill_attri(307006) -> {6,18};

get_guild_skill_attri(307007) -> {6,21};

get_guild_skill_attri(307008) -> {6,24};

get_guild_skill_attri(307009) -> {6,27};

get_guild_skill_attri(307010) -> {6,30};

get_guild_skill_attri(307011) -> {6,33};

get_guild_skill_attri(307012) -> {6,36};

get_guild_skill_attri(307013) -> {6,39};

get_guild_skill_attri(307014) -> {6,42};

get_guild_skill_attri(307015) -> {6,45};

get_guild_skill_attri(307016) -> {6,48};

get_guild_skill_attri(307017) -> {6,51};

get_guild_skill_attri(307018) -> {6,54};

get_guild_skill_attri(307019) -> {6,57};

get_guild_skill_attri(307020) -> {6,60};

get_guild_skill_attri(307021) -> {6,63};

get_guild_skill_attri(307022) -> {6,66};

get_guild_skill_attri(307023) -> {6,69};

get_guild_skill_attri(307024) -> {6,72};

get_guild_skill_attri(307025) -> {6,75};

get_guild_skill_attri(307026) -> {6,78};

get_guild_skill_attri(307027) -> {6,81};

get_guild_skill_attri(307028) -> {6,84};

get_guild_skill_attri(307029) -> {6,87};

get_guild_skill_attri(307030) -> {6,90};

get_guild_skill_attri(308001) -> {7,3};

get_guild_skill_attri(308002) -> {7,6};

get_guild_skill_attri(308003) -> {7,9};

get_guild_skill_attri(308004) -> {7,12};

get_guild_skill_attri(308005) -> {7,15};

get_guild_skill_attri(308006) -> {7,18};

get_guild_skill_attri(308007) -> {7,21};

get_guild_skill_attri(308008) -> {7,24};

get_guild_skill_attri(308009) -> {7,27};

get_guild_skill_attri(308010) -> {7,30};

get_guild_skill_attri(308011) -> {7,33};

get_guild_skill_attri(308012) -> {7,36};

get_guild_skill_attri(308013) -> {7,39};

get_guild_skill_attri(308014) -> {7,42};

get_guild_skill_attri(308015) -> {7,45};

get_guild_skill_attri(308016) -> {7,48};

get_guild_skill_attri(308017) -> {7,51};

get_guild_skill_attri(308018) -> {7,54};

get_guild_skill_attri(308019) -> {7,57};

get_guild_skill_attri(308020) -> {7,60};

get_guild_skill_attri(308021) -> {7,63};

get_guild_skill_attri(308022) -> {7,66};

get_guild_skill_attri(308023) -> {7,69};

get_guild_skill_attri(308024) -> {7,72};

get_guild_skill_attri(308025) -> {7,75};

get_guild_skill_attri(308026) -> {7,78};

get_guild_skill_attri(308027) -> {7,81};

get_guild_skill_attri(308028) -> {7,84};

get_guild_skill_attri(308029) -> {7,87};

get_guild_skill_attri(308030) -> {7,90};

get_guild_skill_attri(309001) -> {8,4};

get_guild_skill_attri(309002) -> {8,7};

get_guild_skill_attri(309003) -> {8,11};

get_guild_skill_attri(309004) -> {8,15};

get_guild_skill_attri(309005) -> {8,18};

get_guild_skill_attri(309006) -> {8,22};

get_guild_skill_attri(309007) -> {8,26};

get_guild_skill_attri(309008) -> {8,29};

get_guild_skill_attri(309009) -> {8,33};

get_guild_skill_attri(309010) -> {8,37};

get_guild_skill_attri(309011) -> {8,40};

get_guild_skill_attri(309012) -> {8,44};

get_guild_skill_attri(309013) -> {8,48};

get_guild_skill_attri(309014) -> {8,51};

get_guild_skill_attri(309015) -> {8,55};

get_guild_skill_attri(309016) -> {8,59};

get_guild_skill_attri(309017) -> {8,62};

get_guild_skill_attri(309018) -> {8,66};

get_guild_skill_attri(309019) -> {8,70};

get_guild_skill_attri(309020) -> {8,73};

get_guild_skill_attri(309021) -> {8,77};

get_guild_skill_attri(309022) -> {8,81};

get_guild_skill_attri(309023) -> {8,84};

get_guild_skill_attri(309024) -> {8,88};

get_guild_skill_attri(309025) -> {8,92};

get_guild_skill_attri(309026) -> {8,95};

get_guild_skill_attri(309027) -> {8,99};

get_guild_skill_attri(309028) -> {8,103};

get_guild_skill_attri(309029) -> {8,106};

get_guild_skill_attri(309030) -> {8,110};

get_guild_skill_attri(310001) -> {3,3};

get_guild_skill_attri(310002) -> {3,6};

get_guild_skill_attri(310003) -> {3,9};

get_guild_skill_attri(310004) -> {3,12};

get_guild_skill_attri(310005) -> {3,15};

get_guild_skill_attri(310006) -> {3,18};

get_guild_skill_attri(310007) -> {3,21};

get_guild_skill_attri(310008) -> {3,24};

get_guild_skill_attri(310009) -> {3,27};

get_guild_skill_attri(310010) -> {3,30};

get_guild_skill_attri(310011) -> {3,33};

get_guild_skill_attri(310012) -> {3,36};

get_guild_skill_attri(310013) -> {3,39};

get_guild_skill_attri(310014) -> {3,42};

get_guild_skill_attri(310015) -> {3,45};

get_guild_skill_attri(310016) -> {3,48};

get_guild_skill_attri(310017) -> {3,51};

get_guild_skill_attri(310018) -> {3,54};

get_guild_skill_attri(310019) -> {3,57};

get_guild_skill_attri(310020) -> {3,60};

get_guild_skill_attri(310021) -> {3,63};

get_guild_skill_attri(310022) -> {3,66};

get_guild_skill_attri(310023) -> {3,69};

get_guild_skill_attri(310024) -> {3,72};

get_guild_skill_attri(310025) -> {3,75};

get_guild_skill_attri(310026) -> {3,78};

get_guild_skill_attri(310027) -> {3,81};

get_guild_skill_attri(310028) -> {3,84};

get_guild_skill_attri(310029) -> {3,87};

get_guild_skill_attri(310030) -> {3,90};

get_guild_skill_attri(311001) -> {4,3};

get_guild_skill_attri(311002) -> {4,6};

get_guild_skill_attri(311003) -> {4,10};

get_guild_skill_attri(311004) -> {4,13};

get_guild_skill_attri(311005) -> {4,16};

get_guild_skill_attri(311006) -> {4,19};

get_guild_skill_attri(311007) -> {4,24};

get_guild_skill_attri(311008) -> {4,29};

get_guild_skill_attri(311009) -> {4,34};

get_guild_skill_attri(311010) -> {4,38};

get_guild_skill_attri(311011) -> {4,43};

get_guild_skill_attri(311012) -> {4,48};

get_guild_skill_attri(311013) -> {4,53};

get_guild_skill_attri(311014) -> {4,58};

get_guild_skill_attri(311015) -> {4,64};

get_guild_skill_attri(311016) -> {4,70};

get_guild_skill_attri(311017) -> {4,77};

get_guild_skill_attri(311018) -> {4,83};

get_guild_skill_attri(311019) -> {4,90};

get_guild_skill_attri(311020) -> {4,96};

get_guild_skill_attri(311021) -> {4,102};

get_guild_skill_attri(311022) -> {4,109};

get_guild_skill_attri(311023) -> {4,115};

get_guild_skill_attri(311024) -> {4,122};

get_guild_skill_attri(311025) -> {4,128};

get_guild_skill_attri(311026) -> {4,134};

get_guild_skill_attri(311027) -> {4,141};

get_guild_skill_attri(311028) -> {4,147};

get_guild_skill_attri(311029) -> {4,154};

get_guild_skill_attri(311030) -> {4,160};

get_guild_skill_attri(312001) -> {9,3};

get_guild_skill_attri(312002) -> {9,6};

get_guild_skill_attri(312003) -> {9,9};

get_guild_skill_attri(312004) -> {9,12};

get_guild_skill_attri(312005) -> {9,15};

get_guild_skill_attri(312006) -> {9,18};

get_guild_skill_attri(312007) -> {9,21};

get_guild_skill_attri(312008) -> {9,24};

get_guild_skill_attri(312009) -> {9,27};

get_guild_skill_attri(312010) -> {9,30};

get_guild_skill_attri(312011) -> {9,33};

get_guild_skill_attri(312012) -> {9,36};

get_guild_skill_attri(312013) -> {9,39};

get_guild_skill_attri(312014) -> {9,42};

get_guild_skill_attri(312015) -> {9,45};

get_guild_skill_attri(312016) -> {9,48};

get_guild_skill_attri(312017) -> {9,51};

get_guild_skill_attri(312018) -> {9,54};

get_guild_skill_attri(312019) -> {9,57};

get_guild_skill_attri(312020) -> {9,60};

get_guild_skill_attri(312021) -> {9,63};

get_guild_skill_attri(312022) -> {9,66};

get_guild_skill_attri(312023) -> {9,69};

get_guild_skill_attri(312024) -> {9,72};

get_guild_skill_attri(312025) -> {9,75};

get_guild_skill_attri(312026) -> {9,78};

get_guild_skill_attri(312027) -> {9,81};

get_guild_skill_attri(312028) -> {9,84};

get_guild_skill_attri(312029) -> {9,87};

get_guild_skill_attri(312030) -> {9,90};

get_guild_skill_attri(313001) -> {14,3};

get_guild_skill_attri(313002) -> {14,6};

get_guild_skill_attri(313003) -> {14,9};

get_guild_skill_attri(313004) -> {14,12};

get_guild_skill_attri(313005) -> {14,15};

get_guild_skill_attri(313006) -> {14,18};

get_guild_skill_attri(313007) -> {14,21};

get_guild_skill_attri(313008) -> {14,24};

get_guild_skill_attri(313009) -> {14,27};

get_guild_skill_attri(313010) -> {14,30};

get_guild_skill_attri(313011) -> {14,33};

get_guild_skill_attri(313012) -> {14,36};

get_guild_skill_attri(313013) -> {14,39};

get_guild_skill_attri(313014) -> {14,42};

get_guild_skill_attri(313015) -> {14,45};

get_guild_skill_attri(313016) -> {14,48};

get_guild_skill_attri(313017) -> {14,51};

get_guild_skill_attri(313018) -> {14,54};

get_guild_skill_attri(313019) -> {14,57};

get_guild_skill_attri(313020) -> {14,60};

get_guild_skill_attri(313021) -> {14,63};

get_guild_skill_attri(313022) -> {14,66};

get_guild_skill_attri(313023) -> {14,69};

get_guild_skill_attri(313024) -> {14,72};

get_guild_skill_attri(313025) -> {14,75};

get_guild_skill_attri(313026) -> {14,78};

get_guild_skill_attri(313027) -> {14,81};

get_guild_skill_attri(313028) -> {14,84};

get_guild_skill_attri(313029) -> {14,87};

get_guild_skill_attri(313030) -> {14,90};

get_guild_skill_attri(314001) -> {13,4};

get_guild_skill_attri(314002) -> {13,7};

get_guild_skill_attri(314003) -> {13,11};

get_guild_skill_attri(314004) -> {13,14};

get_guild_skill_attri(314005) -> {13,18};

get_guild_skill_attri(314006) -> {13,22};

get_guild_skill_attri(314007) -> {13,27};

get_guild_skill_attri(314008) -> {13,32};

get_guild_skill_attri(314009) -> {13,38};

get_guild_skill_attri(314010) -> {13,43};

get_guild_skill_attri(314011) -> {13,49};

get_guild_skill_attri(314012) -> {13,54};

get_guild_skill_attri(314013) -> {13,59};

get_guild_skill_attri(314014) -> {13,65};

get_guild_skill_attri(314015) -> {13,72};

get_guild_skill_attri(314016) -> {13,79};

get_guild_skill_attri(314017) -> {13,86};

get_guild_skill_attri(314018) -> {13,94};

get_guild_skill_attri(314019) -> {13,101};

get_guild_skill_attri(314020) -> {13,108};

get_guild_skill_attri(314021) -> {13,115};

get_guild_skill_attri(314022) -> {13,122};

get_guild_skill_attri(314023) -> {13,130};

get_guild_skill_attri(314024) -> {13,137};

get_guild_skill_attri(314025) -> {13,144};

get_guild_skill_attri(314026) -> {13,151};

get_guild_skill_attri(314027) -> {13,158};

get_guild_skill_attri(314028) -> {13,166};

get_guild_skill_attri(314029) -> {13,173};

get_guild_skill_attri(314030) -> {13,180}.


%%================================================
%% 技能初始化 职业id, 等级 -> 技能 
get_initial_skill(1,1) -> [401001,101001];

get_initial_skill(1,12) -> [];

get_initial_skill(1,25) -> [];

get_initial_skill(1,30) -> [];

get_initial_skill(1,35) -> [];

get_initial_skill(2,1) -> [403001,102001];

get_initial_skill(2,12) -> [];

get_initial_skill(2,25) -> [];

get_initial_skill(2,30) -> [];

get_initial_skill(2,35) -> [];

get_initial_skill(3,1) -> [116001,103001];

get_initial_skill(3,12) -> [];

get_initial_skill(3,25) -> [];

get_initial_skill(3,30) -> [];

get_initial_skill(3,35) -> [].


%%================================================
%% 职业id -> 可获取技能的等级列表
get_can_add_skill_level(1) -> [1, 12, 25, 30, 35];

get_can_add_skill_level(2) -> [1, 12, 25, 30, 35];

get_can_add_skill_level(3) -> [1, 12, 25, 30, 35].


%%================================================
