
-record (target, {
	key = {0, 0, 0},				%% {玩家ID，大类，小类}
	gd_IsFinish   		= 0,		%% 是否完成
	gd_IsAward 	 		= 0,		%% 是否有奖励
	gd_Progress  		= 0,	    %% 进度
	gd_Data 	 		= []    	%% 数据记录
	}).

-record (target_types, {
	key    = {{integer},{integer},{integer}},		%% 玩家id
	gd_IsFinish  	= {integer},		%% 是否完成
	gd_IsAward 	 	= {integer},		%% 是否有奖励
	gd_Progress  	= {integer},	    %% 进度
	gd_Data 	 	= {term}    		%% 数据记录
	}).