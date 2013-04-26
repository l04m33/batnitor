-module(data_ai).

-compile(export_all).

-include("common.hrl").

%% 怪物AI配置表
%% 首回合使用技能攻击敌方前排
get(1) ->
	[{{round,1},{action,p2,p3},{target,rival,front}}];

%% 首回合使用技能攻击敌方后排
get(2) ->
	[{{round,1},{action,p2,p3},{target,rival,rear}}];

%% 首回合使用技能攻击敌方/我方全部/随机（看技能）
get(3) ->
	[{{round,1},{action,p2,p3}}];

%% 首回合为己方前排增加buff
get(4) ->
	[{{round,1},{action,p2,p3},{target,friendly,front}}];

%% 首回合使己方后排增加buff
get(5) ->
	[{{round,1},{action,p2,p3},{target,friendly,rear}}];

%% 某条件下为己方前排增加buff/加血
get(6) ->
	[{{round,p1},{action,p2,p3},{target,friendly,front}}];

%% 某条件下为己方后排增加buff/加血
get(7) ->
	[{{round,p1},{action,p2,p3},{target,friendly,rear}}];

%% 某条件下为己方全部/随机增加buff/加血，或攻击敌方
get(8) ->
	[{{round,p1},{action,p2,p3}}];

%% 攻击敌方后排
get(9) ->
	[{{round,p1},{action,p2,p3},{target,rival,rear}}];

%% 攻击敌方前排
get(10) ->
	[{{round,p1},{action,p2,p3},{target,rival,front}}];

%% 第3回合使用技能攻击敌方前排
get(11) ->
	[{{round,3},{action,p2,p3},{target,rival,front}}];

%% 第3回合使用技能攻击敌方后排
get(12) ->
	[{{round,3},{action,p2,p3},{target,rival,rear}}];

%% 第3回合使用技能攻击敌方/我方全部/随机（看技能）
get(13) ->
	[{{round,3},{action,p2,p3}}];

%% 第3回合为己方前排增加buff、加血
get(14) ->
	[{{round,3},{action,p2,p3},{target,friendly,front}}];

%% 第3回合使己方后排增加buff、加血
get(15) ->
	[{{round,3},{action,p2,p3},{target,friendly,rear}}];

%% 血量低于70%时，攻击敌方后排目标
get(16) ->
	[{{hp,p1,0.7},{action,p2,p3},{target,rival,rear}}];

%% 血量低于70%时，攻击敌方前排目标
get(17) ->
	[{{hp,p1,0.7},{action,p2,p3},{target,rival,front}}];

%% 血量低于70%时，攻击敌方全部/随机(看技能)
get(18) ->
	[{{hp,p1,0.7},{action,p2,p3}}];

%% 血量低于50%时，攻击敌方后排目标
get(19) ->
	[{{hp,p1,0.5},{action,p2,p3},{target,rival,rear}}];

%% 血量低于50%时，攻击敌方前排目标
get(20) ->
	[{{hp,p1,0.5},{action,p2,p3},{target,rival,front}}];

%% 血量低于50%时，攻击敌方全部/随机(看技能)
get(21) ->
	[{{hp,p1,0.5},{action,p2,p3}}];

%% 血量低于40%时，攻击敌方后排目标
get(22) ->
	[{{hp,p1,0.4},{action,p2,p3},{target,rival,rear}}];

%% 血量低于40%时，攻击敌方前排目标
get(23) ->
	[{{hp,p1,0.4},{action,p2,p3},{target,rival,front}}];

%% 血量低于40%时，攻击敌方全部/随机(看技能)
get(24) ->
	[{{hp,p1,0.4},{action,p2,p3}}];

%% 血量低于30%时，攻击敌方后排目标
get(25) ->
	[{{hp,p1,0.3},{action,p2,p3},{target,rival,rear}}];

%% 血量低于30%时，攻击敌方前排目标
get(26) ->
	[{{hp,p1,0.3},{action,p2,p3},{target,rival,front}}];

%% 血量低于30%时，攻击敌方全部/随机(看技能)
get(27) ->
	[{{hp,p1,0.3},{action,p2,p3}}];

%% 血量低于70%时，增益我方后排目标
get(28) ->
	[{{hp,p1,0.7},{action,p2,p3},{target,friendly,rear}}];

%% 血量低于70%时，增益我方前排目标
get(29) ->
	[{{hp,p1,0.7},{action,p2,p3},{target,friendly,front}}];

%% 血量低于70%时，增益我方全部/随机（看技能）
get(30) ->
	[{{hp,p1,0.7},{action,p2,p3}}];

%% 血量低于50%时，增益我方后排目标
get(31) ->
	[{{hp,p1,0.5},{action,p2,p3},{target,friendly,rear}}];

%% 血量低于50%时，增益我方前排目标
get(32) ->
	[{{hp,p1,0.5},{action,p2,p3},{target,friendly,front}}];

%% 血量低于50%时，增益我方全部/随机（看技能）
get(33) ->
	[{{hp,p1,0.5},{action,p2,p3}}];

%% 血量低于40%时，增益我方后排目标
get(34) ->
	[{{hp,p1,0.4},{action,p2,p3},{target,friendly,rear}}];

%% 血量低于40%时，增益我方前排目标
get(35) ->
	[{{hp,p1,0.4},{action,p2,p3},{target,friendly,front}}];

%% 血量低于40%时，增益我方全部/随机（看技能）
get(36) ->
	[{{hp,p1,0.4},{action,p2,p3}}];

%% 血量低于30%时，增益我方后排目标
get(37) ->
	[{{hp,p1,0.3},{action,p2,p3},{target,friendly,rear}}];

%% 血量低于30%时，增益我方前排目标
get(38) ->
	[{{hp,p1,0.3},{action,p2,p3},{target,friendly,front}}];

%% 血量低于30%时，增益我方全部/随机（看技能）
get(39) ->
	[{{hp,p1,0.3},{action,p2,p3}}];

%% 后排全死，攻击敌方前排目标
get(40) ->
	[{{rear_all_dead,p1},{action,p2,p3},{target,rival,front}}];

%% 前排全死，攻击敌方前排目标
get(41) ->
	[{{front_all_dead,p1},{action,p2,p3},{target,rival,front}}];

%% 后排全死，攻击敌方后排目标
get(42) ->
	[{{rear_all_dead,p1},{action,p2,p3},{target,rival,rear}}];

%% 前排全死，攻击敌方后排目标
get(43) ->
	[{{front_all_dead,p1},{action,p2,p3},{target,rival,rear}}];

%% 后排全死，攻击敌方(看技能)
get(44) ->
	[{{rear_all_dead,p1},{action,p2,p3}}];

%% 前排全死，攻击敌方(看技能)
get(45) ->
	[{{front_all_dead,p1},{action,p2,p3}}];

%% 后排全死，增益己方前排
get(46) ->
	[{{rear_all_dead,p1},{action,p2,p3},{target,friendly,front}}];

%% 后排全死，增益己方后排
get(47) ->
	[{{front_all_dead,p1},{action,p2,p3},{target,friendly,rear}}];

%% 后排全死，增益己方(看技能)
get(48) ->
	[{{rear_all_dead,p1},{action,p2,p3}}];

%% 后排全死，增益己方(看技能)
get(49) ->
	[{{front_all_dead,p1},{action,p2,p3}}];

%% 血量低于30%时，用a技能
get(50) ->
	[{{hp,p1,0.3},{action,p2,p3}}];

%% 第x回合,对我方前排放a技能
get(51) ->
	[{{round,p1},{action,p2,p3},{target,friendly,front}}];

%% 血量低于50%时，用a技能
get(52) ->
	[{{hp,p1,0.5},{action,p2,p3}}].


%%================================================
