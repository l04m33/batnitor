-record(yun_biao, {
					id             			= 0,    
					state          			= 0,     %%0:未接  1：已接 2：被劫镖  3：已刷新过镖车
%% 					yun_biao_times = 0,
					gem_type       			= 1,
					last_yb_time   			= 0,
					success_robed_times     = 0,   %%被成功打劫次数
					robed_times    			= 0 %%被劫次数
					}).


-record(yun_biao_types, {
					id                   = {integer},
					state                = {integer},
%% 					yun_biao_times       = {integer},
					gem_type             = {integer},
					last_yb_time         = {integer},
					success_robed_times  = {integer},
					robed_times          = {integer}
					}). 

-record(jixing,{
				fkey 				= yunbiao,
      			starLevel   		= 0,      %%吉星等级
				addfactor    		= 0,      %%加成系数
			    yunshi_Point 		= 0,      %%当前运势点`
				up_yunshi_point 	= 0,   	  %%升级到下一级需要的运势点
				goldcount   		= 0,      %%转运需要消耗金币
				last_zhuanyun_time  = 0		  %%上次转运时间
               }).

-record(jixing_types, {
					   fkey          	  = {term},
					   startLevel    	  = {integer},
					   addfactor     	  = {integer},
					   yunshi_Point 	  = {integer},
					   up_yunshi_point	  = {integer},
					   goldcount   		  = {integer},
					   last_zhuanyun_time = {integer}
					   }).

-record(zhuanyun,{
					id                 = 0,     %%点击转运玩家id
					zhuanyun_times 	   = 0}).   %%记录转运次数

-record(zhuanyun_types, {
							id          		= {integer},
							zhuanyun_times 		= {integer}}).	

-define(REFREH_COST, 10).						%%刷新镖车10金
-define(ONEKEY_REFRESH_COST, 100).  			%%一键刷新到最高级镖车
-define(CACHE_JIXING, cache_util:get_register_name(jixing)).
-define(CACHE_YUN_BIAO, cache_util:get_register_name(yun_biao)).
-define(MAX_YUN_BIAO_TIMES, 5).					%%最大运镖次数
-define(GEM_PROBABILITY, [30, 30, 19, 19, 2]).	%%策划给的概率
-define(GOODSHELL_PROBABILITY, [33,33,34]).		%%镖车大卖概率
-define(Onekey_refresh_VipRequire, 5).			%%一键刷新vip等级要求
-define(ROB_TIMES, 3).							%%打劫次数	
-define(ROBED_TIMES, 2).						%%被劫次数
-define(CACHE_ZHUANYUN_REF,cache_util:get_register_name(zhuanyun)).

-define(FACTOR_RECORD, {1.3,1.2,1}).
-define(ROB_STATE, 26000).
-define(NO_ACCEPT_BIAO,   0).
-define(ACCEPT_BIAO, 	  1).
-define(ROBBING_BIAO, 	  2).
-define(FRESH_BIAO, 	  3).
%% 0:未接  1：已接 2：被劫镖  3：已刷新过镖车
-define(SELL_OUT_ONE_MOMENT, 1).%%一抢而空的出售

-define(YA_BIAO_BEST_TIME_START,{18,0,0}).		%% 押镖活动开始时间
-define(YA_BIAO_BEST_TIME_OVER,{20,0,0}).		%% 押镖活动结束时间
-define(ZHUANYUN_AWARD_TIME,{23,59,30}).		%% 转运奖励发放时间

-define(ENABLE_ROB_LEVEL, 30).					%% 允许劫镖等级