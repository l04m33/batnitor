-ifndef(__COMMON_HRL__).
-define(__COMMON_HRL__, 1).

-include("chief_disciple.hrl").
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
-include("dressing.hrl").
-include("defence.hrl").

-include("wealth.hrl").

-include("lottery.hrl").
-include("mines.hrl").

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

%%这张表用来维护server唯一的常量
%%用type做索引
%%如果用数字,取字段value_i
%%如果用字符串，取字段value_s
%%如果直接使用erlang的term，取字段value_t
%% type 类型
%% 招财进宝里的奖池总数
-define(G_SERVER_WEALTH_SILVER, 1).
%% 投壶高级物品获得记录
-define(ADVANCE_ITEM_HISTORY_FROM_LOTTERY, 2).
%% 投壶历史记录
-define(LOTTERY_HISTORY, 3).
-record(g_server_para,
	{
		type = 0,
		value_i = 0,
		value_t = undefined,
		value_s = ""
	}).

-record(g_server_para_types,
	{
		type = {integer},
		value_i = {integer},
		value_t = {term},
		value_s = {string}		
	}).

-define(G_SERVER_PARA_REF, cache_util:get_register_name(g_server_para)).


-endif.
