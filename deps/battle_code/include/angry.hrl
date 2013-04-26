
-record(angry,{
		key = {0,0},
		tuple_list = []
	}).

-record(angry_types,{
		key = {{integer},{integer}},
		tuple_list = {term}
	}).

-define(ANGRY_TYPE_DUNGEON,			1). %% 副本
-define(ANGRY_TYPE_MARSTOWER,		2). %% 爬塔
-define(ANGRY_TYPE_BOSS,			3). %% 世界boss
-define(ANGRY_TYPE_DEFEND,			4). %% 群魔乱舞


