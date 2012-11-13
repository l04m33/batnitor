%% 使用临时背包时，要自定义你自己的一个背包的类型，
%% 并在下面的ALL_TEMP_BAG宏定义中添加你定义的背包类型宏定义

-define(TEMP_BAG_ETS,cache_util:get_register_name(temp_bag)).

-define(TEMP_BAG_GUILD_COMP, guild_comp_temp_bag).			%% 公会战奖励临时背包
-define(TEMP_BAG_CYCLIC_REWARD, cyclic_temp_bag).			%% 循环任务奖励临时背包
-define(ALL_TEMP_BAG, 
	[?TEMP_BAG_GUILD_COMP, 
	 ?TEMP_BAG_CYCLIC_REWARD
	 ]).

-record(temp_bag,{
	bag_key     = {0, undefined}, 		%% {玩家id, 背包类型，为一个atom}
	item_list   = []			%% [{item_id, num_of_item, is_bind}]
	}).

-record(temp_bag_types,{
	bag_key     = {{integer}, {term}}, 		
	item_list   = {term}
	}).