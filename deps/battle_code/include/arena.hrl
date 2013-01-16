%%============================================================================================
%% 竞技排行榜 begin
%%============================================================================================					 
-record(arena_rec, {
					rank    = 0,
					id      = 0,    
					sustain_win = 0,
					win_record={0,0,0,0,0},
					challengetimes=0,
					last_battle_time = 0,
					queue      = queue:new()     %%记录最近五场报告
					}). 


-record(arena_rec_types, {
					rank             = {integer},
					id               = {integer},
					sustain_win      = {integer},
					win_record       = {term},
					challengetimes   = {integer}, 
					last_battle_time = {integer},
					queue            = {term}
					}). 

-record(rank_index, {
					id      = 0 ,
					rank    = 0  
					}). 


-record(award_index, {
					id      = 0 ,
					rank    = 0  
					}).

-record(recent_rec, {
					 id                 = 0,
                     challenger_name    = "",
					 challenged_name     = "",
                     win_rec            = 0,  %%2 lose,1  win
					 challenge_time      = 0,
					 ranking             = 0    %%0 排名不变， n排名变成第n名
                    }).

-record(arena_award,
					{
					 id = 0,
					 rank = 0
					 }).

-record(arena_award_types,
					{
					id = {integer},
					 rank = {integer}
					 }).
					 
-record(arena_champion_info,
					{
					fkey = arena_champion_info,
					id = 0,			%% 晚上统计时竞技场第一名的id
					level = 0		%% 晚上统计时竞技场第一名的等级
					}).
					
-record(arena_champion_info_types,
					{
					fkey = {term},
					id = {integer},
					level = {integer}
					}).

-record(player_arena_state, {
		id = 0,								%% 玩家id
		is_in_battle = 0,					%% 是否处于战斗状态中 0 否 1 是
		opponentId = 0						%% 被挑战者id，若不处于战斗状态中，为0
	}).

%% 定义竞技场挑战奖励数据结构
-record(arena_challenge_award, {
		level = 0,							%% 玩家大于该等级可获得奖励
		win_silver = 0,						%% 挑战胜利获得银币
		win_jungong = 0,					%% 挑战胜利获得军功
		lose_silver = 0,					%% 挑战失败获得银币
		lose_jungong = 0					%% 挑战失败获得军功
	}).

-define(CACHE_ARENA_AWARD, cache_util:get_register_name(arena_award)).
-define(MAX_CHALLENGE_TIMES, 15). 	%%每天最大挑战次数
-define(CACHE_ARENA_REC, cache_util:get_register_name(arena_rec)).
-define(CACHE_ARENA_CHAMPION_INFO, cache_util:get_register_name(arena_champion_info)).
-define(ARENA_UPDATE_TIME,22).
-define(BUY_ARENA_CHALLENGE_TIMES, 31007).
-define(BATTLE_CD, 10*60).				
-define(CLEAN_ARENA_BATTLE_CD, 31006).
-define(CHALLENGE_FIVE_TIMES, 5).  %%挑战5次翻牌
-define(INIT_WIN_RECORD, {0,0,0,0,0}).				%% 初始化竞技场历史战绩
-define(ARENA_RANK_MIN_JUNGONG_AWARD, 30).			%% 竞技场排名军功奖励最小值
%%============================================================================================
%% 竞技排行榜 end
%%============================================================================================	
