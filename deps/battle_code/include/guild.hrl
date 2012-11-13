
%% ets
-define(ETS_GUILD_INFO,  ets_guild_info).
-define(ETS_GUILD_MEM,   ets_guild_mem).
-define(ETS_GUILD_APPLY, ets_guild_apply).
-define(ETS_GUILD_CD,    ets_guild_cd).
-define(ETS_GUILD_NAME,  ets_guild_name).

-define(THREE_DAYS_SECS,     60 * 60 * 24 * 3).
-define(ONE_DAY_SECS, 60 * 60 * 24).

-type guild_id()    :: integer().
-type guild_level() :: integer().
-type guild_rank()  :: integer().
-type guild_exp()   :: integer().


%% 工会职位级别
-define(GUILD_PRESIDENT,      1).
-define(GUILD_VICE_PRESIDENT, 2).
-define(GUILD_OFFICER,        3).
-define(GUILD_ELITE,          4).
-define(GUILD_MEMBER,         5).

%% 工会排序查找
-define(GUILD_SORT_EXACT_POSITION, 0).
-define(GUILD_SORT_DEFAULT,        1).
-define(GUILD_SORT_LEVEL,          2).
-define(GUILD_SORT_EXPLOIT,        3).
-define(GUILD_SORT_MEMBER_NUM,     4).

%% 工会事件
-define(GUILD_EVENT_MAX, 30).
-define(GUILD_EVENT_CREATE,         1).
-define(GUILD_EVENT_JOIN,           2).
-define(GUILD_EVENT_QUIT,           3).
-define(GUILD_EVENT_DONATE,         3).
-define(GUILD_EVENT_TRANSFER,       5).
-define(GUILD_EVENT_BATTLE,         6).
-define(GUILD_EVENT_FIRE,           7).
-define(GUILD_EVENT_UPGRADE_LEVEL,  5).
-define(GUILD_EVENT_UPGRADE_SKILL,  9).
-define(GUILE_EVENT_LEARN_SKILL,   10).

%% 杂项
%% 分页大小
-define(GUILD_PAGE_SIZE, 10).


%% gen_cache
-define(CACHE_GUILD_INFO, cache_util:get_register_name(guild_info)).
-define(CACHE_GUILD_MEM,  cache_util:get_register_name(guild_member)).

%% this table's information will *not* update to the database. 
-record(guild_state,
	{
		next_id
	}	
).

-record(guild_info,                 %% 工会信息表
	{
		guild_id,                   %% 工会ID
		guild_name,                 %% 工会名字
		level           = 1,        %% 工会等级
		creator,                    %% 创建人ID (由于工会主席可能会转让, 所以主席不一定==创建人)
		create_time     = 0,        %% 创建时间 
		president,                  %% 工会主席ID
 	 	president_name,             %% 工会主席名字
		dismiss_time    = 0,        %% 解散时间
		max_member      = 20,       %% 最大人数
		members         = [],       %% 成员列表  list()
		exp             = 0,        %% 功勋  exploit,
		state           = normal,   %% normal | {dismiss, Time, TimerRef},
		manifesto       = "",
		event,                      %% #guild_event_tab{}
		apply_list      = []        %% {ID, AppTime} list
	}		
).

-record(guild_info_types, 
	{
	 	guild_id       = {integer},
		guild_name     = {string},
		level          = {integer},
		creator        = {integer},
		create_time    = {integer},
		president      = {integer},
		president_name = {string},
		dismiss_time   = {integer},
		max_member     = {integer},
		members        = {term},
		exp            = {integer},
		state          = {term},
		manifesto      = {string},
		event          = {term},
		apply_list     = {term}   
	}		
).

-record(guild_member,     %% 工会成员信息表, 而且可以用来做反向索引
	{
		id,
		name,         
	 	guild_id,
		rank          = 5,
		exp           = 0,     %% 功勋 exploit
		total_exp     = 0,     %% 总功勋(历史功勋)
		join_time     = 0,     %% 加入工会的时间
		donate_time   = 0,     %% 最近一次捐献的时间
		donate_count  = 0,     
		welfare_time  = 0,     %% 最近一次领取福利的时间
		leave_time    = 0
	}		   
).

-record(guild_member_types, 
	{
		id           = {integer},
		name         = {string},
		guild_id     = {integer}, 
		rank         = {integer},
		exp          = {integer},
		total_exp    = {integer},
		join_time    = {integer},
		donate_time  = {integer},
		donate_count = {integer},
		welfare_time = {integer},
		leave_time   = {integer}
	}			
).

-record(guild_event, 
	{
		type,
		time,
		content
	}	
).

-record(guild_event_tab,
	{
		index = 0,
		num   = 0,
	 	vec
	}	
).

-record(guild_apply,
	{
		id,
		name,
		%% [{GuildID, ApplyTime}] 
		%% note: for optimization, later implementation may use insertion sort to 
		%% store the tuple in the list
		apply_list = [] 
	}		
).

-record(guild_cd, 
	{
		id,
		quit_time   %% last time the player quit the guild
	}		
).

-record(guild_item, 
	{
		id,
		level,
		exp
	}		
).

-record(guild_skill, 
	{
		id,
		level,
		guild_level, %% when the guild reach this level, member can learn this skill
		value,       %% value of skill enhancement 
		exp,
		coin
	}		
).


































