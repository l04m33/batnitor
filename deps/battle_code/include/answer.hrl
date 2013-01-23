
-define(QUESTION_NUM,		 data_system:get(74)). %% 问题个数
-define(TIME_PERPARE,		 data_system:get(66)).  %% 准备时间
-define(TIME_READ,			 data_system:get(67)). %% 读题时间
-define(TIME_ANSWER,		 data_system:get(68)). %% 答题时间
-define(TIME_SHOW,		 	 data_system:get(69)).  %% 答案公布时间
-define(TIME_AFTER,			 data_system:get(70)). %% 活动结束缓冲时间

-record(answer,{
			account_id = 0,
			question_id = 0,
			answer = 0,
			result = 0,
			effect = 0,
			options = [],
			time = 0,
			times = 1
	}).

-record(answer_result,{
			account_id = 0,
			point = 0,
			question_num = 0
	}).