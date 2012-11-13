%%pending team record, use for maintaining pending team list.
%%leader_id,
%%leader_name
%%mate_id = none,
%%status = pending |  ready | playing
%% pending <=> ready
%% ready->playing
-record(team, {leader_id,
			   leader_name,
			   leader_level,
			   leader_career,

			   mate_id = 0,
			   mate_name=none,
			   mate_level = 0,
			   mate_career = 0,			

			   state = pending, %% state can be pending, ready, playing
				   				%% pending,队长建立了队伍,等待加入,显示在有效队伍列表,齐人后可变成ready状态	
				   				%% ready, 队伍两个队员齐备,还在npc旁，未跳入游戏,,显示在有效队伍列表,踢人后可变成pending状态
				   				%% playing, 挑战副本里面,正在游戏,不显示在有效队伍列表,只能解散.掉线后变成单人playing状态

			   scene_id = 0,
			   item_list = [],		%% list of item_pick_info.
			   npc_id = 0
			   }).

-record(team_index,{id = 0,
		scene_id = 0,
		role = lead,		%%role can be mate or lead
		lead_id = 0
	}).

-record(item_pick_info,
		{id = 0,
		item_id = 0,
		item_num = 0}
		). 

-define(TEAM_SUCCESS,0).
-define(TEAM_FAIL,1).

-define(INVITE_SUCCESS,0).
-define(JOIN_SUCCESS,0).

-define(TEAM_LEAVE,0).
-define(TEAM_DISMISS,1).
-define(TEAM_FIRE,2).
-define(TEAM_OFFLINE,3).