-define(ONLINE,0).
-define(OFFLINE,1).

-define(FEMALE,0).
-define(MALE,1).

-define(CAN_PRAY,1).
-define(CAN_NOT_PRAY,0).

-define(PT_PRAY,0).
-define(PT_PRAYED,1).

-define(CACHE_FRIEND, cache_util:get_register_name(friend)).
-define(CACHE_BLACK_LIST, cache_util:get_register_name(black_list)).
-define(MAX_SHOW_FRIENDS_NUM, 200).
-define(MAX_SHOW_BLACK_NUM,100).

-define(REJECT_ADD_FRIEND,1).
-define(AGREE_ADD_FRIEND,0).

-record(friend,{
	key	=	{0,0}, %% use {id, friend_id} as key
	role_name	=	"",
	familiar	=	0,
	sex			=	?FEMALE,
	career		= 	0,
	last_pray_time = 0
}).

-record(friend_types,{
	key	=	{{integer}, {integer}}, %% use {id, friend_id} as key
	role_name	=	{term},
	familiar	=	{integer},
	sex			=	{integer},
	career		= 	{integer},
	last_pray_time = {integer}
}).

-record(black_list,{
	key	=	{0,0}, %% use {id, friend_id} as key
	role_name	=	"",
	familiar	=	0,
	sex			=	?FEMALE,
	career		= 	0
}).

-record(black_list_types,{
	key	=	{{integer}, {integer}}, %% use {id, friend_id} as key
	role_name	=	{term},
	familiar	=	{integer},
	sex			=	{integer},
	career		= 	{integer}
}).

-record(contact_dynamic_info,
	{
		level			=	0, %%level should be dynamic fetch
		online_status	=	?ONLINE, %% 1 as offline and 0 as online
		prayable		=	?CAN_NOT_PRAY,
    	last_logout_time=	0
	}).

