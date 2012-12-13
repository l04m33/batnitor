-module(data_enable_system).

-compile(export_all).



%% 判断系统开启条件是否能满足
get(1) -> {2,163};

get(2) -> {2,48};

get(3) -> {2,100};

get(4) -> {2,9999};

get(5) -> {2,56};

get(6) -> {2,36};

get(7) -> {2,7};

get(8) -> {1,0};

get(9) -> {1,0};

get(10) -> {1,0};

get(11) -> {2,122};

get(12) -> {2,167};

get(13) -> {2,141};

get(14) -> {2,47};

get(15) -> {2,42};

get(16) -> {3,35};

get(17) -> {3,25};

get(18) -> {4,0};

get(19) -> {5,0};

get(20) -> {6,0};

get(21) -> {3,30};

get(22) -> {3,25};

get(23) -> {7,0};

get(24) -> {3,30};

get(25) -> {3,30};

get(26) -> {3,35};

get(27) -> {2,122};

get(28) -> {3,999};

get(29) -> {3,30};

get(30) -> {3,40};

get(31) -> {3,40};

get(32) -> {8,30};

get(33) -> {3,30};

get(34) -> {3,7}.


%%================================================
%% 获取所有的已配置的系统id
get_all_system() ->
	[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34].


%%================================================
%% 获取系统id，npc id对应
get_npc_id(1) ->
	17;

get_npc_id(2) ->
	10;

get_npc_id(3) ->
	2;

get_npc_id(4) ->
	0;

get_npc_id(5) ->
	61;

get_npc_id(6) ->
	0;

get_npc_id(7) ->
	0;

get_npc_id(8) ->
	0;

get_npc_id(9) ->
	0;

get_npc_id(10) ->
	0;

get_npc_id(11) ->
	4;

get_npc_id(12) ->
	5;

get_npc_id(13) ->
	9;

get_npc_id(14) ->
	13;

get_npc_id(15) ->
	20;

get_npc_id(16) ->
	0;

get_npc_id(17) ->
	0;

get_npc_id(18) ->
	0;

get_npc_id(19) ->
	0;

get_npc_id(20) ->
	21;

get_npc_id(21) ->
	0;

get_npc_id(22) ->
	0;

get_npc_id(23) ->
	0;

get_npc_id(24) ->
	22;

get_npc_id(25) ->
	0;

get_npc_id(26) ->
	29;

get_npc_id(27) ->
	6;

get_npc_id(28) ->
	8;

get_npc_id(29) ->
	11;

get_npc_id(30) ->
	18;

get_npc_id(31) ->
	12;

get_npc_id(32) ->
	0;

get_npc_id(33) ->
	0;

get_npc_id(34) ->
	0.


%%================================================
