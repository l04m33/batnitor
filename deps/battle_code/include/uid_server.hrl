%% 说明：
%% 		要使用uid的话，只有在下面添加一个类型的宏定义即可
%%		然后调用uid_server:get_seq_num(Your_Type_macro)

-define(UID_SKILL, 1).					%% 用于技能id分配
-define(UID_ITEMS, 2).					%% 用于物品ID分配


-record (uid, {
	key = {0, 0},			%% {server_index, type}
	max_id       = 1		%% 当前最大id
}).

-record (uid_types, {
	key = {{integer}, {integer}},
	max_id       = {integer}	
}).