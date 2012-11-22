-ifndef(__COMMON_HRL__).
-define(__COMMON_HRL__, 1).

-include("rank.hrl").
-include("soul_ball.hrl").
-include("cool_down.hrl").
-include("temp_bag.hrl").
-include("economy.hrl").
-include("constant.hrl").
-include("player_record.hrl").
-include("ets_sql_map.hrl").
-include("scene.hrl").
-include("role.hrl"). 
-include("spec_types.hrl"). 
-include("error_code.hrl"). 
-include("battle.hrl").
-include("guild.hrl").
-include("items.hrl").
-include("relation.hrl").
-include("fengdi.hrl").
 
-include("arena.hrl").
-include("monster.hrl").
-include("uid_server.hrl"). 
-include("log_type.hrl"). 
-include("pet.hrl"). 
-include("counter.hrl"). 

-include("team.hrl").
-include("code_log.hrl").

-include("dungeon.hrl"). 
-include("horse.hrl"). 
-include("official.hrl"). 
-include("xunxian.hrl").

-include("guaji.hrl").
-include("task.hrl").
-include("marstower.hrl").
-include("achieve.hrl").
-include("vip.hrl").

-include("run_business.hrl").

-include("announcement.hrl").

-include("gen_callback_server.hrl").

-include("mail_record.hrl").
-include("boss.hrl").
-include("fcm.hrl").
-include("system_enable.hrl").
-include("competition.hrl").
-include("target.hrl").
-include("guide.hrl").
-include("consume.hrl").

-include("challenge_king.hrl").
-include("defence.hrl").

%%安全校验
-define(TICKET, "SDFSDESF123DFSDF").

%%flash843安全沙箱
-define(FL_POLICY_REQ, <<"<pol">>).
%-define(FL_POLICY_REQ, <<"<policy-file-request/>\0">>).
-define(FL_POLICY_FILE, 
		<<"<cross-domain-policy>"
		  		"<allow-access-from domain='*' to-ports='*' />"
		  "</cross-domain-policy>"
		>>).

%%tcp_server监听参数
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, 
					  {reuseaddr, true}, {nodelay, false}, 
					  {delay_send, true}, {send_timeout, 50000}, 
					  {keepalive, true}, {exit_on_close, true}]).


-endif.

%% %% wrapper for catch
%% -define(Catch(X),
%% 	case catch (X) of
%% 		{'EXIT', Reason} -> ?ERR("mod: ~w, id: ~w err: ~w", [?MODULE, get(id), Reason]);
%% 		V -> V
%% 	end).


