%% 本头文件包含程序中自定义的spec type
-ifndef(__SPEC_TYPES_HRL__).
-define(__SPEC_TYPES_HRL__, 1).

-type player_id() 		:: integer().			%% 玩家id
-type role_id()			:: integer().			%% 角色id
-type ps()				:: #player_status{}.
-type role()			:: #role{}.
-type scene_id()		:: integer().
-type ssf()				:: 0|1.					%% 玩家在场景中的状态标识
-type err_code()		:: integer().			%% 错误码
-type skill_tuple()		:: {integer(), integer(), integer()}. %% 角色模块用于记录技能的格式
-type npc_id()          :: integer().
-type item_id()         :: integer().

%% --------- 任务相关 ---------
-type task_id()         :: integer().
-type task_req_type()   :: integer().

-endif.