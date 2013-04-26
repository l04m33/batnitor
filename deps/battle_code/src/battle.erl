%%% -------------------------------------------------------------------
%%% Author  : xwz
%%% Description : Awalagaquya
%%%
%%% Created : 2012-6-12
%%% -------------------------------------------------------------------
-module(battle).
-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([set_battle_plot/2]).

-export([get_items_dispatch/3, merge_dispatch_list/3]).

-compile(export_all).

-define(FORMATION_TEAM_OPEN_POS, [1,2,3,4,5,6,7,8,9]).

%% ====================================================================
%% External functions
%% ====================================================================

-spec start(Start) -> {ok, Pid} | ignore | {error, Reason} when
	Start  :: #battle_start{},
	Pid    :: pid(),
	Reason :: term().
 
start(Start) ->
	gen_fsm:start(?MODULE, Start, []).

ready(BattlePID, PlayerID) ->
    case is_pid(BattlePID) of
        true ->
            gen_fsm:send_event(BattlePID, {ready, PlayerID});
        _ ->        % false
            ?INFO(battle, "stale protocol"),
            ok
    end.

auto_cmd(BattlePID, PlayerID) ->
    case is_pid(BattlePID) of
        true ->
            gen_fsm:send_event(BattlePID, {set_cmd, PlayerID, 0});
        _ ->        % false
            ?INFO(battle, "stale protocol"),
            ok
    end.

set_battle_command(BattlePid, ID, Cmd) ->
	gen_fsm:send_event(BattlePid, {set_battle_cmd, ID, Cmd}).

quit_battle(BattlePID, ID) ->
    case is_pid(BattlePID) of
        true ->
            gen_fsm:send_event(BattlePID, {quit, ID});
        _ ->        % false
            ?INFO(battle, "stale protocol"),
            ok
    end.

end_plot(BattlePID, ID) ->
    case is_pid(BattlePID) of
        true ->
            gen_fsm:send_event(BattlePID, {continue, ID});
        _ ->        % false
            ?INFO(battle, "stale protocol"),
            ok
    end.

logout(BattlePid, ID) ->
	gen_fsm:send_all_state_event(BattlePid, {logout, ID}).

%%判断是否需要加人，如果需要，返回plot list，否则返回undefined
set_battle_plot(Id,MonsterId)->
	case lists:member(MonsterId, data_battle_plot:get_monster_list() ) of
		true->
			Recv_task_needed = data_battle_plot:get_recv_task(MonsterId),
			case mod_task:check_ever_receive(Id, Recv_task_needed) of
				true->
					Plot_list = data_battle_plot:get_plot_list(MonsterId);
				false->
					Plot_list = []
			end;
		false->
			Plot_list = []
	end,
	?INFO(plot,"plot list is ~w",[Plot_list]),
	Plot_list.
				
			
				

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init(Start) ->
	?INFO(battle, "Start = ~w", [Start]),
	process_flag(trap_exit,true),
	{H, M, S} = now(),
	put(id, Start#battle_start.att_id),
    get_battle_id(),
	
	random:seed(H, M, S),
	
	%% timer:sleep(5000),
	BattleData = get_battle_data(Start),
	IDList1 = get_ids(att, BattleData),
	IDList2 = get_ids(def, BattleData),

    ?INFO(battle_init, "Player IDs involved: Attacker IDs = ~w, Defender IDs = ~w",
          [IDList1, IDList2]),

    case check_cross(get_online_ids(BattleData), BattleData) of
        true ->
            case catch check_battle(Start, IDList1, IDList2, BattleData) of
                true ->
                    ?INFO(battle, "Battle started."),
                    %% may be check the operation in the future
                    {ok, battle_init, BattleData#battle_data {timeout = {now(), 0}}, 0};
                False ->
                    clear_battle(IDList1 ++ IDList2, self()),
                    case False of
                    false ->
                        %% print the err msg here.
                        ?DEBUG(battle_init, "Check Battle Fail."),
                        ignore;
                    {false, Reason} ->
                        ?DEBUG(battle_init, "Check Battle Fail: Reason: ~w", [Reason]),
                        ignore;
                    {'EXIT', Reason} ->
                        %% reach here probably because player is not online
                        ?DEBUG(battle_init, "check battle Fail: Reason: ~w", [Reason]),
                        ignore
                    end
            end;

        false ->
            %% print the err msg here.
            ?DEBUG(battle_init, "check_cross failed."),
            ignore
    end.

clear_battle([], _Pid) -> ok;
clear_battle([ID | Rest], Pid) ->
	catch mod_player:clear_battle(ID, Pid),
	clear_battle(Rest, Pid).

%% init client side
%% init rounds, battle procedure list
battle_init(_Event, BattleData) ->
    __LogFileName = ?BATTLE_LOG_INIT,
    ?CHAT(battle, "本场战斗log: ~s", [__LogFileName]),
    ?BATTLE_LOG("~n++++++++++++++++++ 战斗初始值 ++++++++++++++++++"),
    dump_player_list(BattleData),
	NBattleData = start_new_round(BattleData),
	
	send_start_package(NBattleData),
    send_global_buff(NBattleData),

    case is_all_players_ready(NBattleData) of
        false ->
            {next_state, battle_wait_for_client, NBattleData#battle_data {
                timeout = {now(), ?BATTLE_WAIT_CLIENT_INIT}}, ?BATTLE_WAIT_CLIENT_INIT};
        true ->
            ?INFO(battle, "All players ready"),
            {next_state, battle_run, NBattleData#battle_data {
                timeout = {now(), ?BATTLE_WAIT_BEGIN}}, ?BATTLE_WAIT_BEGIN}
    end.

battle_wait_for_client({ready, ID}, BattleData) ->
	{Now, Timeout} = time_remain(BattleData),
    case get_player_info(ID, BattleData) of
        false ->
            ?INFO(battle, "get_player_info(...) = false ?!"),
            {next_state, battle_wait_for_client, BattleData#battle_data {
                timeout = {Now, Timeout}}, Timeout};
        PInfo ->
            NBattleData = set_player_info(ID, PInfo#player_info{ready = true}, BattleData),
            case is_all_players_ready(NBattleData) of
                true ->
                    ?INFO(battle, "All players ready"),
                    {next_state, battle_run, NBattleData#battle_data {
                        timeout = {now(), ?BATTLE_WAIT_BEGIN}}, ?BATTLE_WAIT_BEGIN};
                false ->
                    ?INFO(battle, "Not all players ready"),
                    {next_state, battle_wait_for_client, NBattleData#battle_data {
                        timeout = {Now, Timeout}}, Timeout}
            end
    end;

%% 这里也要处理set_cmd消息，不然战斗开始后的第一个命令包可能会漏掉
battle_wait_for_client({set_cmd, ID, Cmd}, BattleData) ->
	{Now, Timeout} = time_remain(BattleData),
    %% 这里故意不检查CD，防止正式进入战斗之后卡掉时序正常的命令包
    NBattleData = set_battle_cmd(ID, Cmd, BattleData),
    {next_state, battle_wait_for_client, NBattleData#battle_data {
        timeout = {Now, Timeout}}, Timeout};

battle_wait_for_client(timeout, BattleData) ->
    ?ERR(battle, "battle_wait_for_client timed out...."),
    {next_state, battle_run, BattleData#battle_data {
        timeout = {now(), ?BATTLE_WAIT_BEGIN}}, ?BATTLE_WAIT_BEGIN};

battle_wait_for_client(OtherMsg, BattleData) ->
    ?ERR(battle, "received in battle_wait_for_client: ~w", [OtherMsg]),
	{Now, Timeout} = time_remain(BattleData),
    {next_state, battle_wait_for_client, BattleData#battle_data {
        timeout = {Now, Timeout}}, Timeout}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------

%% StateName :: battle_run
battle_run(Event, BattleData) ->
	{Now, Timeout} = time_remain(BattleData),
	Round = BattleData#battle_data.round,
	?INFO(battle, "Round ~w received Event: ~w, Timeout = ~w", [Round, Event, Timeout]),
	
	NBattleData = 
		case Event of
			%% if (Cmd == 0) then client ask us to set the command for him/her
			{set_cmd, ID, Cmd} ->
                ForceEnd = false,
                case BattleData#battle_data.round =:= 1 
                        orelse check_and_set_cd(set_cmd_cd, ID, 0) of
                    true ->
                        BattleData1 = set_battle_cmd(ID, Cmd, BattleData),
                        case BattleData1#battle_data.round =:= 1
                                andalso is_command_set(BattleData1) of
                            true ->
                                BattleData2 = handle_command_ex(BattleData1, true),
                                BattleData2;
                            false ->
                                BattleData1
                        end;
                    false ->        % 发包太快，忽略
                        ?ERR(battle, "set_cmd command ignored, ID = ~w", [ID]),
                        BattleData
                end;
			{finish_play, ID} ->
                ForceEnd = false,
                case check_and_set_cd(finish_play_cd, ID, 0) of
                    true ->
                        %% every time we have sent the result to the client, 
                        %% we have to wait the client send us a signal to indicate the client has played the result
                        %% if so, we'll do another calculation
                        case get_player_info(ID, BattleData) of
                            false ->
                                BattleData;
                            Info ->
                                NInfo = Info#player_info{finish_play = true},
                                BattleData1 = set_player_info(ID, NInfo, BattleData),
                                                
                                case is_all_finish_play(BattleData1) of
                                    true -> 
                                        %% trigger a new battle action if all finish play
                                        BattleData2 = handle_command_ex(BattleData1, true),
                                        BattleData2;
                                    false ->
                                        BattleData1
                                end
                        end;
                    false ->        % 发包太快，忽略
                        ?ERR(battle, "finish_play command ignored, ID = ~w", [ID]),
                        BattleData
                end;
			timeout ->
                ?BATTLE_LOG("--------- 超时！ ---------"),
				?ERR(battle, "timeout... round = ~w", [BattleData#battle_data.round]),

                OldTOTimes = incr_timeout_times(),
                ?INFO(battle, "OldTOTimes = ~w", [OldTOTimes]),
                case OldTOTimes >= 2 of
                    true ->
                        ForceEnd = true,
                        BattleData;
                    _ ->        % false
                        ForceEnd = false,
                        %% trigger a new battle action if timeout
                        BattleData1 = handle_command_ex(BattleData, true),
                        BattleData1
                end;
			_ ->
                ForceEnd = false,
				BattleData
		end,
	
	case is_battle_end(NBattleData) of
		{true, Winner} ->
            case check_plot_trigger(NBattleData#battle_data{winner = ?UNDEFINED}) of
                {true, Plot, NBattleData1} ->
                    NBattleData2 = trigger_plot(Plot, start_new_round(NBattleData1)),
                    {next_state, battle_plot, NBattleData2#battle_data {
                        timeout = {Now, ?BATTLE_WAIT_PLOT}}, ?BATTLE_WAIT_PLOT};
                false ->
                    %% send 20005 to notify client, battle is complete!
                    Award = get_battle_award(NBattleData),
                    NBattleData1 = 
                        NBattleData#battle_data{
                            timeout = {Now, ?BATTLE_WAIT_QUIT},
                            winner  = Winner, 
                            award   = Award
                        },
                    ?INFO(battle, "sending battle award"),
                    send_battle_award(NBattleData1),
                    ?INFO(battle, "sending battle result"),
                    %% 如果没发过20001，客户端会一直在等20001，
                    %% 这时只有收到状态是 ?BATTLE_END_DISRUPTED 的20005，
                    %% 客户端才能结束战斗
                    case sent_any_20001() of
                        true ->
                            send_result_package(NBattleData1);
                        false ->
                            send_result_package(NBattleData1, ?BATTLE_END_DISRUPTED)
                    end,
                    case get_online_ids(NBattleData1) of
                        [] ->
                            GhostIDList = get_ids(NBattleData1),
                            [notify_complete_direct(GID, NBattleData1) || GID <- GhostIDList],
                            {stop, normal, NBattleData1};
                        _ ->
                            ?INFO(battle_end, "battle_complete...."),
                            {next_state, battle_complete, NBattleData1, ?BATTLE_WAIT_QUIT}
                    end
            end;
		
		false ->
			if (Event == timeout) ->
                case ForceEnd of
                    true ->         % 战斗timeout太多次了，马上结束
                        EmptyAward = #battle_award{},
                        NBattleData1 = NBattleData#battle_data{
                            timeout = {Now, ?BATTLE_WAIT_QUIT},
                            winner  = ?UNDEFINED, 
                            award   = EmptyAward
                        },
                        ?INFO(battle, "sending battle result"),
                        send_result_package(NBattleData1, ?BATTLE_END_DISRUPTED),
                        case get_online_ids(NBattleData1) of
                            [] ->
                                GhostIDList = get_ids(NBattleData1),
                                [notify_complete_direct(GID, NBattleData1) || GID <- GhostIDList],
                                {stop, normal, NBattleData1};
                            _ ->
                                ?INFO(battle_end, "battle_complete...."),
                                {next_state, battle_complete, NBattleData1, ?BATTLE_WAIT_QUIT}
                        end;
                    false ->
                        case check_plot_trigger(NBattleData) of
                            {true, Plot, NBattleData1} ->
                                NBattleData2 = trigger_plot(Plot, NBattleData1),
                                {next_state, battle_plot, NBattleData2#battle_data {
                                    timeout = {Now, ?BATTLE_WAIT_PLOT}}, ?BATTLE_WAIT_PLOT};
                            false ->
                                {next_state, battle_run, NBattleData#battle_data {
                                    timeout = {Now, ?BATTLE_WAIT_FINISH}}, ?BATTLE_WAIT_FINISH}
                        end
                end;
			true -> 
				%% some player has not acted yet(due to the same round), 
				%% update the timeout value
				if (BattleData#battle_data.round == NBattleData#battle_data.round) ->
					{next_state, battle_run, NBattleData#battle_data {
						timeout = {Now, Timeout}}, Timeout};
				true ->
					%% if is next turn...
                    case check_plot_trigger(NBattleData) of
                        {true, Plot, NBattleData1} ->
                            NBattleData2 = trigger_plot(Plot, NBattleData1),
                            {next_state, battle_plot, NBattleData2#battle_data {
                                timeout = {Now, ?BATTLE_WAIT_PLOT}}, ?BATTLE_WAIT_PLOT};
                        false ->
                            {next_state, battle_run, NBattleData#battle_data {
                                timeout = {Now, ?BATTLE_WAIT_FINISH}}, ?BATTLE_WAIT_FINISH}
                    end
				end
			end
	end.

%% 战斗中播剧情暂时不支持组队，所以只要有一个玩家发continue过来就会继续
battle_plot({continue, ID}, BattleData) ->
    ?INFO(battle_plot, "{continue, ~w} received", [ID]),
	{Now, _Timeout} = time_remain(BattleData),
    % gen_fsm:send_event(self(), {finish_play, ID}),
    {next_state, battle_run, BattleData#battle_data {
        timeout = {Now, ?BATTLE_WAIT_CONTINUE}}, ?BATTLE_WAIT_CONTINUE};

battle_plot(timeout, BattleData) ->
    ?INFO(battle_plot, "timeout received"),
	{Now, _Timeout} = time_remain(BattleData),
    {next_state, battle_run, BattleData#battle_data {
        timeout = {Now, ?BATTLE_WAIT_CONTINUE}}, ?BATTLE_WAIT_CONTINUE};

battle_plot(_Event, BattleData) ->
    ?INFO(battle_plot, "~w received", [_Event]),
	{Now, Timeout} = time_remain(BattleData),
    {next_state, battle_plot, BattleData#battle_data {
        timeout = {Now, Timeout}}, Timeout}.

%% 这里处理这个消息是为了让客户端表现得比较自然，
%% 不会出现动画还没播完就不能选技能的现象
battle_complete({set_cmd, ID, Cmd}, BattleData) ->
	{Now, Timeout} = time_remain(BattleData),
	case check_player(ID, BattleData) of
        false ->
            void;
        {true, _} ->
            Info = get_player_info(ID, BattleData),
            case Cmd of
                0 ->
                    %% catch住，免得人死光了ai:get_skill/2报错
                    try
                        {AISid, _, _, _, _} = ai:get_skill(Info#player_info.lead, BattleData),
                        send_command_package(Info#player_info.lead, AISid, BattleData)
                    catch _:_ ->
                        send_command_package(Info#player_info.lead, ?SKILL_COMMON_ATTACK, BattleData)
                    end;
                Sid when is_integer(Sid) ->
                    send_command_confirm_package(Info#player_info.lead, Sid, BattleData)
            end
    end,
	{next_state, battle_complete, BattleData#battle_data {timeout = {Now, Timeout}}, Timeout};

battle_complete(timeout, BattleData) ->
    ?INFO(battle_end, "battle_complete timed out...."),
    F = fun(PlayerID, BData) ->
        case get_player_info(PlayerID, BData) of
            false ->
                BData;
            Info ->
                case Info#player_info.finish_play of
                    quit ->
                        BData;
                    _ ->
                        ?INFO(battle, "notifying player ~w", [PlayerID]),
                        NInfo = Info#player_info{finish_play = quit},
                        NBData = set_player_info(PlayerID, NInfo, BData),
                        try
                            notify_complete(PlayerID, NBData)
                        catch ErrType : ErrReason ->
                            ?INFO(battle, "notify_complete failed, ~w: ~w", [ErrType, ErrReason])
                        end,
                        NBData
                end
        end
    end,
    NBattleData = lists:foldl(F, BattleData, get_online_ids(BattleData)),
	{stop, normal, NBattleData};

battle_complete({quit, ID}, BattleData) ->
	?INFO(battle_end, "receive command quit, ID = ~w", [ID]),
	{Now, Timeout} = time_remain(BattleData),
	
	NBattleData = 
	case get_player_info(ID, BattleData) of
		false -> BattleData;
		Info  -> 
            NInfo = Info#player_info{finish_play = quit},
            NBattleData0 = set_player_info(ID, NInfo, BattleData),
            {true, Camp} = check_player(ID, NBattleData0),
            %% 这里把通知同一支队伍战斗完成的动作推迟到所有队员都退出后才统一做
            %% 防止组队的时候队长和队员状态不一致
            notify_team_complete(Camp, NBattleData0),
            NBattleData0
	end,
	case is_all_quit_battle(NBattleData) of
		true  -> {stop, normal, NBattleData};
		false -> {next_state, battle_complete, NBattleData#battle_data {timeout = {Now, Timeout}}, Timeout}
	end;

battle_complete(Msg, BattleData) ->
	{Now, Timeout} = time_remain(BattleData),
	?INFO(battle_end, "unknown message: ~w", [Msg]),
	{next_state, battle_complete, BattleData#battle_data {timeout = {Now, Timeout}}, Timeout}.
	
%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(Event, StateName, BattleData) ->
	{Now, Timeout} = time_remain(BattleData),
	case Event of
		{logout, ID} -> 
			case get_player_info(ID, BattleData) of
				false -> ok;
				Info  -> 
					NBattleData = set_player_info(ID, Info#player_info {online = false}, BattleData),
					if (StateName == battle_init) ->
						?ERR(battle, "player ~w logout when battle is initializing!", [ID]),
						exit(normal);
					true ->
						ok
					end,
						
                    case {NBattleData#battle_data.mod, logout_info(NBattleData)} of
						{_, all} ->
							exit(normal);
						{_, none} -> 
							if (StateName == battle_run) ->
								battle_run({finish_play, ID}, NBattleData#battle_data {timeout = {Now, Timeout}});
							(StateName == battle_complete) ->
								battle_complete({quit, ID}, NBattleData#battle_data {timeout = {Now, Timeout}});
                            (StateName == battle_wait_for_client) ->
                                battle_wait_for_client({ready, ID}, NBattleData#battle_data {timeout = {Now, Timeout}});
							true ->
								{next_state, StateName, NBattleData#battle_data {timeout = {Now, Timeout}}, Timeout}
							end;
                        {pve, def} ->       % 这个情况在pve的时候即使还有玩家在也会出现
							if (StateName == battle_run) ->
								battle_run({finish_play, ID}, NBattleData#battle_data {timeout = {Now, Timeout}});
							(StateName == battle_complete) ->
								battle_complete({quit, ID}, NBattleData#battle_data {timeout = {Now, Timeout}});
                            (StateName == battle_wait_for_client) ->
                                battle_wait_for_client({ready, ID}, NBattleData#battle_data {timeout = {Now, Timeout}});
							true ->
								{next_state, StateName, NBattleData#battle_data {timeout = {Now, Timeout}}, Timeout}
							end;
						{_, Camp} ->
							Award = get_battle_award(BattleData),
							NBattleData1 = NBattleData#battle_data {
								award = Award, 
								winner = if (Camp == att) -> def; true -> att end, 
								timeout = {Now, ?BATTLE_WAIT_QUIT}
							},
							
                            if
                                StateName =:= battle_wait_for_client ->
                                    ?ERR(battle, "Oops, logout in battle_wait_for_client, ID = ~w", [ID]),
                                    battle_wait_for_client({ready, ID}, NBattleData1#battle_data{timeout = {Now, Timeout}});
                                StateName =:= battle_complete ->
                                    %% 在battle_complete状态说明已经发过结果包了，走battle_complete
                                    ?INFO(battle_end, "logout in battle_complete, ID = ~w", [ID]),
                                    battle_complete({quit, ID}, NBattleData#battle_data{timeout = {Now, Timeout}});
                                true ->
                                    send_battle_award(NBattleData1),
                                    ?INFO(battle, "sending battle result"),
                                    send_result_package(NBattleData1, ?BATTLE_END_DISRUPTED),
                                    ?INFO(battle_end, "battle_complete.... Camp = ~w", [Camp]),
                                    {next_state, battle_complete, NBattleData1, ?BATTLE_WAIT_QUIT}
                            end
					end
			end;
		_ -> 
			%% ignore this message
			{next_state, StateName, BattleData#battle_data {timeout = {Now, Timeout}}, Timeout}
	end.
  
%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, BattleData) ->
    Reply = ok,
    {reply, Reply, StateName, BattleData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(debug, StateName, BattleData) ->
	?INFO(battle, "StateName = ~w, BattleData = ~w", [StateName, BattleData]),
	{next_state, StateName, BattleData};

handle_info({'EXIT', _, Reason}, _, BattleData) ->
    ?INFO(battle,"exit:~w", [Reason]),
    {stop, Reason, BattleData};

handle_info(_Info, StateName, BattleData) ->
    {next_state, StateName, BattleData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(Reason, _StateName, BattleData) ->
    case Reason of
        normal ->
            ?INFO(battle, "terminating.. Reason = ~w~n, BattleData = ~w", [Reason, BattleData]);
        _ ->
            ?CHAT(battle, "服务器战斗进程挂了！看log看log！！"),
            ?ERR(battle, "terminating.. Reason = ~w~n, BattleData = ~w", [Reason, BattleData])
    end,
	Winner = BattleData#battle_data.winner,
	?INFO(battle, "Winner = ~w", [Winner]),
	
    ?BATTLE_LOG("~n++++++++++++++++++ 战斗结束 ++++++++++++++++++"),
    ?BATTLE_LOG("原因: ~p", [Reason]),
    ?BATTLE_LOG("胜方: ~p", [Winner]),
    dump_player_list(BattleData),

	Ids = get_online_ids(BattleData),
	F = fun(ID) ->
			Info = get_player_info(ID, BattleData),
			if (Info#player_info.finish_play == quit) ->
				%% battle has notified this player
				ok;
			true ->
				?INFO(battle, "notify complete id = ~w", [ID]),
                case (catch notify_complete(ID, BattleData)) of
                    {'EXIT', Error} ->
                        ?INFO(battle, "notify_complete failed, Error = ~w", [Error]);
                    _ ->
                        void
                end
			end
		end,
	lists:foreach(F, Ids),

    ?BATTLE_LOG_CLOSE,
    ok.

notify_complete(ID, BattleData) ->
	?INFO(battle_end, "calling notify complete, ID = ~w", [ID]),
	
	Callback = BattleData#battle_data.callback,
	Type     = BattleData#battle_data.type,
	Winner   = BattleData#battle_data.winner,
	
	{true, Camp} = check_player(ID, BattleData),
	IsWin = (Winner == Camp),
	
	Statistic = get_battle_statistic(ID, BattleData),
	%% ?INFO(battle, "statistic = ~w", [Statistic]),
	
	case is_number(BattleData#battle_data.monster) 
			andalso (Winner == att) 
			andalso (Camp == att) 
			andalso (Type =/= ?BATTLE_TYPE_BOSS) of
		true ->
			send_battle_award(ID, BattleData);
		false ->
			ok
	end,
	
	WinHPList1 = get_battle_hp_list(Winner, BattleData),
    FullHPList = get_full_battle_hp_list(BattleData),
    ?INFO(battle, "FullHPList = ~w", [FullHPList]),
    AccMPList  = get_player_mp_list(BattleData),
    ?INFO(battle, "AccMPList = ~w", [AccMPList]),
	F1 = fun({Pos, HP}) ->
				 {(Pos - 1) rem (?BATTLE_FIELD_SIZE div 2) + 1, HP}
		 end,
	WinHPList = lists:map(F1, WinHPList1),
	
	Res = 
		#battle_result {
			is_win    = IsWin, 
			mon_id    = BattleData#battle_data.monster,
			type      = Type, 
            battle_pid = self(),
			hp_list   = WinHPList,
            full_hp_list = FullHPList,
            player_mp = AccMPList,
			callback  = Callback, 
			statistic = Statistic
		},
	
	mod_player:battle_complete(ID, Res).

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, BattleData, _Extra) ->
    {ok, StateName, BattleData}.

%====================================================================================================
% Battle Initialization
%====================================================================================================
get_battle_data(Start) ->
	Mod      = Start#battle_start.mod,
	Type     = Start#battle_start.type,
	AttID    = Start#battle_start.att_id,
	AttMer   = Start#battle_start.att_mer,
	DefID    = Start#battle_start.def_id,
	DefMer   = Start#battle_start.def_mer,
	MonID    = Start#battle_start.monster,
	Caller   = Start#battle_start.caller,
	Callback = Start#battle_start.callback, 
	MakeTeam = Start#battle_start.maketeam,
	Array    = array:new(?BATTLE_FIELD_SIZE + 1),
    OfflineList = Start#battle_start.off_line_list,
    AccMPList = Start#battle_start.player_mp,
	
	case Mod of
		pve -> 
			?INFO(battle, "calling get_mer_info"),
			{Attacker, Array1} = get_mer_info(att, AttID, AttMer, MakeTeam, Array, Type, OfflineList, AccMPList),
			?INFO(battle, "calling get_mon_info"),
			{Defender, Array2} = get_mon_info(def, MonID, Start#battle_start.monster_hp, Array1),
			?INFO(battle, "done..."),
            PlayerLevel = mod_role:get_main_level(AttID),
            PlotList = [transform_plot(AttID, PlayerLevel, P) || P <- Start#battle_start.plot];
		pvp ->
			{Attacker, Array1} = get_mer_info(att, AttID, AttMer, MakeTeam, Array, Type, OfflineList, AccMPList),
            case Type of
                ?BATTLE_TYPE_BOSS ->
                    {Defender, Array2} = get_mer_info(boss_def, DefID, DefMer, MakeTeam, Array1, Type, OfflineList, AccMPList);
                _ ->
                    {Defender, Array2} = get_mer_info(def, DefID, DefMer, MakeTeam, Array1, Type, OfflineList, AccMPList)
            end,
            PlotList = []       % PVP不支持加人和播剧情……
	end,
	
	BattleData =
		#battle_data {
			mod       = Mod,
			type      = Type,
			procedure = [],
			attacker  = Attacker,
			defender  = Defender,
			player    = Array2,
			monster   = MonID,
            initial_monster_hp = 
                case is_list(Start#battle_start.monster_hp) of
                    true ->
                        lists:map(
                            fun({RelPos, MHP}) ->
                                {RelPos + ?BATTLE_FIELD_SIZE div 2, MHP}
                            end,
                            Start#battle_start.monster_hp);
                    _ ->    % false
                        Start#battle_start.monster_hp
                end,
            plot      = PlotList,
			caller    = Caller,
			maketeam  = MakeTeam,
			callback  = Callback
		},
	BattleData.

%======================================================================================================
% battle utilities function
% check_player, get_player_info, get_ids, pos_to_id
%======================================================================================================


%% check whether the player is in the BattleData
-spec check_player(Camp, ID, BattleData) -> true | false when
	Camp :: camp(),
	ID :: integer(),
	BattleData :: #battle_data{}.
																
check_player(Camp, ID, BattleData) ->
	List = 
		case Camp of
			att -> BattleData#battle_data.attacker;
			def -> BattleData#battle_data.defender
		end,
	case lists:keysearch(ID, #player_info.id, List) of
		{value, _} -> true;
		false -> false
	end.

-spec check_player(ID, BattleData) -> {true, camp()} | false when
	ID :: integer(),
	BattleData :: #battle_data{}.

check_player(ID, BattleData) ->
	case check_player(att, ID, BattleData) of
		true -> 
			{true, att};
		false ->
			case check_player(def, ID, BattleData) of
				true -> {true, def};
				false -> false
			end
	end.
		
-spec get_player_info(integer(), #battle_data{}) -> false | #player_info{}.
get_player_info(ID, BattleData) ->
	case check_player(ID, BattleData) of
		false -> false;
		{true, Camp} ->
            case Camp of
				att -> lists:keyfind(ID, #player_info.id, BattleData#battle_data.attacker);
				def -> lists:keyfind(ID, #player_info.id, BattleData#battle_data.defender)
			end
	end.

-spec get_player_info(integer(), camp(), #battle_data{}) -> false | #player_info{}.
get_player_info(ID, Camp, BattleData) ->
	case Camp of
		att -> lists:keyfind(ID, #player_info.id, BattleData#battle_data.attacker);
		def -> lists:keyfind(ID, #player_info.id, BattleData#battle_data.defender)
	end.

-spec set_player_info(ID :: integer(), Info :: #player_info{}, #battle_data{}) -> #battle_data{}.
set_player_info(ID, Info, BattleData) ->
	case check_player(ID, BattleData) of
		{true, Camp} ->
			InfoList = 
				case Camp of
					att -> BattleData#battle_data.attacker;
					def -> BattleData#battle_data.defender
				end,
			NInfoList = lists:keyreplace(ID, #player_info.id, InfoList, Info),
			case Camp of
				att -> BattleData#battle_data {attacker = NInfoList};
				def -> BattleData#battle_data {defender = NInfoList}
			end;
		false ->
			BattleData
	end.		
				
-spec get_ids(camp(), #battle_data{}) -> [integer()].
get_ids(Camp, BattleData) ->
    get_ids(Camp, BattleData, all).

-spec get_ids(camp(), #battle_data{}, boolean()) -> [integer()].
get_ids(Camp, BattleData, AllOrOnline) ->
	case {Camp, AllOrOnline} of
		{att, all} -> [E#player_info.id || E <- BattleData#battle_data.attacker, is_integer(E#player_info.id)];
		{def, all} -> [E#player_info.id || E <- BattleData#battle_data.defender, is_integer(E#player_info.id)];
		{att, online} -> [E#player_info.id 
                            || E <- BattleData#battle_data.attacker, 
                               is_integer(E#player_info.id), 
                               E#player_info.online =:= true];
		{def, online} -> [E#player_info.id 
                            || E <- BattleData#battle_data.defender, 
                               is_integer(E#player_info.id), 
                               E#player_info.online =:= true]
	end.

-spec get_ids(#battle_data{}) -> [integer()].
get_ids(BattleData) ->
	get_ids(att, BattleData, all) ++ 
	get_ids(def, BattleData, all).

-spec get_online_ids(#battle_data{}) -> [integer()].
get_online_ids(BattleData) ->
	get_ids(att, BattleData, online) ++ 
	get_ids(def, BattleData, online).


%% pos_to_id finds out the PlayerID (or undefined if Player is monter) 
%% of whose mercenary's position is Pos  
-spec pos_to_id(Pos, BattleData) -> integer() | ?UNDEFINED when
	Pos :: integer(),
	BattleData :: #battle_data{}.

pos_to_id(Pos, BattleData) ->
	InfoList = BattleData#battle_data.attacker ++ BattleData#battle_data.defender,
	pos_to_id_1(Pos, InfoList).

pos_to_id_1(Pos, [Info | Rest]) ->
	case lists:member(Pos, Info#player_info.mer_list) of
		true  -> Info#player_info.id;
		false -> pos_to_id_1(Pos, Rest)
	end;

pos_to_id_1(_Pos, []) ->
	?UNDEFINED.

-spec get_mer_list(ID, BattleData) -> MerList when
	ID :: integer(),
	MerList :: [integer()],
	BattleData :: #battle_data{}.
												
get_mer_list(ID, BattleData) ->
	case get_player_info(ID, BattleData) of
		false -> [];
		PInfo -> PInfo#player_info.mer_list
	end.
	
-spec get_mer_info(Camp, ID, MerList, MakeTeam, Array, Type, OfflineList, AccMPList) -> {PlayerInfoList, NArray} when
	Camp           :: battle_camp(),
	ID             :: integer() | undefined,
	MerList        :: [any()], %% #mercenary{}
	MakeTeam       :: true | false,
	PlayerInfoList :: [#player_info{}],
	Array          :: array(),
    Type           :: integer(),
    OfflineList    :: [integer()],
    AccMPList      :: [{integer(), integer()}],
	NArray         :: array().
																					 
get_mer_info(Camp, ID, MerList, MakeTeam, Array, Type, OfflineList, AccMPList) ->
    {PlayerLevel, PlayerOnline} = case is_integer(ID) of
        true ->
            %% 竞技场（等）战斗是假的pvp，把防守方的在线状态强制设为不在线，
            %% battle_run状态中就不会等防守方的命令了
            ForceOffline = 
                ((Camp =:= def orelse Camp =:= boss_def)
                    andalso lists:member(Type, ?BATTLE_OFFLINE_PVP_TYPES))
                 orelse lists:member(ID, OfflineList),
            PlayerOnline0 = case ForceOffline of
                true  -> false;
                false -> true
            end,
            {mod_role:get_main_level(ID), PlayerOnline0};
        false ->
            {0, false}
    end,

    ?INFO(battle, "PlayerOnline = ~w", [PlayerOnline]),

	RoleFun = 
		fun (Role, Acc) ->
			if (Camp == att) ->
                    [Role#role.gd_isBattle | Acc];
               (Camp == def) ->
                    [Role#role.gd_isBattle + ?BATTLE_FIELD_SIZE div 2 | Acc];
               (Camp == boss_def) ->
                    [?BOSS_POSITION | Acc]
			end
		end,
	
	ArrayFun = 
		fun (Role, {CurID, CurLevel, Protectors, Arr}) ->
			if (Camp == att) ->
                    Pos = Role#role.gd_isBattle;
               (Camp == def) ->
                    Pos = Role#role.gd_isBattle + ?BATTLE_FIELD_SIZE div 2;
               (Camp == boss_def) ->
                    Pos = ?BOSS_POSITION
			end,

            AccMP = case Role#role.gd_roleRank of
                1 -> 
                    case lists:keyfind(CurID, 1, AccMPList) of
                        false -> 0;
                        {_, AMP} -> AMP
                    end;
                _ ->
                    0
            end,

            BS = role_2_bs(CurID, CurLevel, Role, AccMP),
            {NProtectors, NBS} = case BS#battle_status.is_lead of
                true ->
                    {Protectors, BS#battle_status{protectors = Protectors}};
                false ->
                    case get_passive_skill_helper(?PSKILL_PROTECT, BS#battle_status.p_skill) of
                        {true, PRSkillInfo} ->
                            {PRRate, PRCoef} = PRSkillInfo#battle_skill.param,
                            {[{Pos, {PRRate, PRCoef}} | Protectors], BS};
                        false ->
                            {Protectors, BS}
                    end
            end,
            {CurID, CurLevel, NProtectors, array:set(Pos, NBS, Arr)}
		end,

	if (MakeTeam == true) ->
		?INFO(battle, "check make team..."),
        PlayerPID = case PlayerOnline of
            true ->
                {true, Ps} = mod_player:is_online(ID),
                Ps#player_status.player_pid;
            false ->
                ?UNDEFINED
        end,
		case catch mod_team:prepare_team_battle(ID) of
			false ->
				?INFO(battle, "not make team"),
                List0ToFilter = case MerList of
                    [] -> mod_role:get_on_battle_list(ID);
                    _  -> MerList
                end,
                List0 = filter_duplicate_pos(List0ToFilter),       % XXX: 为了防止卡战斗临时加上的

				PInfo = 
					#player_info {
						id       = ID, 
						pid      = PlayerPID,
						lead     = get_mer_leader(Camp, List0), 
						mer_list = lists:foldl(RoleFun, [], List0),
                        online   = PlayerOnline,
                        g_buffs  = init_global_buff(ID, PlayerLevel, List0)
					},

                % 排序为了保证主角在最后，可以给他设置protectors……
                List  = lists:keysort(#role.gd_roleRank, List0),
                {_, _, _, NArray} = lists:foldl(ArrayFun, {ID, PlayerLevel, [], Array}, List),
				{[PInfo], NArray};
			
			[{ID, List00}, {ID2, List200}] ->
				?INFO(battle, "make team"),
				?INFO(battle, "ID1 = ~w, RoleList1 = ~w", [ID, List00]),
				?INFO(battle, "ID2 = ~w, RoleList2 = ~w", [ID2, List200]),

                %% 领地战用，如果传了MerList进来，用MerList
                {List0ToFilter, List20ToFilter} = case MerList of
                    [] ->
                        {List00, List200};
                    _ ->
                        TeamInfo = mod_team:get_team_info(ID),
                        {_, NMerList} = mod_formation:prepare_role_list_for_team_battle(TeamInfo, ID, MerList),
                        ML1 = lists:filter(
                            fun(M) ->
                                {OwnerID, _RoleID} = M#role.key,
                                OwnerID =:= ID
                            end,
                            NMerList),
                        ML2 = lists:filter(
                            fun(M) ->
                                {OwnerID, _RoleID} = M#role.key,
                                OwnerID =:= ID2
                            end,
                            NMerList),
                        {ML1, ML2}
                end,
                List0 = filter_duplicate_pos(List0ToFilter),
                List20 = filter_duplicate_pos(List20ToFilter),
				
                {TeammateOnline, TeammatePID} = 
                    case mod_team:is_team_mate_leave(ID) of
                        true ->
                            {false, ?UNDEFINED};
                        false ->
                            case lists:member(ID2, OfflineList) of
                                true  -> {false, ?UNDEFINED};
                                false ->
                                    case mod_player:is_online(ID2) of
                                        {true, Ps20} -> {true,  Ps20#player_status.player_pid};
                                        false        -> {false, ?UNDEFINED}
                                    end
                            end
                    end,

				PInfo = 
					#player_info {
						id       = ID,		  
						pid      = PlayerPID,
						lead     = get_mer_leader(Camp, List0),
						mer_list = lists:foldl(RoleFun, [], List0),
                        online   = PlayerOnline,
                        g_buffs  = init_global_buff(ID, PlayerLevel, List0)
					},
				
                TeammateLevel = mod_role:get_main_level(ID2),

				PInfo2 = 
					#player_info {
						id       = ID2,
						pid      = TeammatePID,
						lead     = get_mer_leader(Camp, List20),
						mer_list = lists:foldl(RoleFun, [], List20),
                        online   = TeammateOnline,
                        g_buffs  = init_global_buff(ID2, TeammateLevel, List20)
					},

                List  = lists:keysort(#role.gd_roleRank, List0),
                List2 = lists:keysort(#role.gd_roleRank, List20),
				
                {_, _, _, NArray0} = lists:foldl(ArrayFun, {ID, PlayerLevel, [], Array}, List),
                {_, _, _, NArray} = lists:foldl(ArrayFun, {ID2, TeammateLevel, [], NArray0}, List2),
				{[PInfo, PInfo2], NArray};
			_Other ->
				?ERR(battle, "mod_team returns: ~w", [_Other])
		end;
	true -> %% maketeam = false;
		List0ToFilter = 
			if (MerList =/= []) ->
				MerList;
			true ->
			   %% ID must be specified if MerList is []
			   %%(is_integer(ID)) ->
				mod_role:get_on_battle_list(ID)
			end,
        List0 = filter_duplicate_pos(List0ToFilter),

		?INFO(battle, "On Battle List = ~w", [List0]),
		
		Leader = case get_mer_leader(Camp, List0) of
            data_not_exist ->
                FirstRole = hd(MerList),
                if 
                    (Camp == att) -> FirstRole#role.gd_isBattle;
                    (Camp == def) -> FirstRole#role.gd_isBattle + ?BATTLE_FIELD_SIZE div 2;
                    (Camp == boss_def) -> ?BOSS_POSITION
                end;
            L -> L
        end,
		List1 = lists:foldl(RoleFun, [], List0),
		PInfo   = 
			#player_info {
				id       = ID,
				%% pid   = Ps#player_status.player_pid, 
				lead     = Leader,
				mer_list = List1,
                online   = PlayerOnline,
                g_buffs  = init_global_buff(ID, PlayerLevel, List0)
			},

        List = lists:keysort(#role.gd_roleRank, List0),
        {_, _, _, NArray} = lists:foldl(ArrayFun, {ID, PlayerLevel, [], Array}, List),
		{[PInfo], NArray}
	end.

get_mer_leader(_, []) ->
	data_not_exist;
	
get_mer_leader(Camp, [Role | Rest]) ->
	if (Role#role.gd_roleRank == 1) ->
		if (Camp == att) ->
				Role#role.gd_isBattle;
		   (Camp == def) ->
				Role#role.gd_isBattle + ?BATTLE_FIELD_SIZE div 2;
           (Camp == boss_def) ->
                ?BOSS_POSITION
		end;
	true ->
		get_mer_leader(Camp, Rest)
	end.
			   
%% role to battle status
role_2_bs(ID, PlayerLevel, Role, AccMP) ->
	?INFO(battle, "ID = ~w", [ID]),
	?INFO(battle, "Skill From Mer = ~w", [Role#role.gd_skill]),
	{ActSkills0, PasSkills, NormSkills} = transform_skill(Role#role.gd_skill),

    ?INFO(battle, "ActSkills0 = ~w", [ActSkills0]),
    %% 因为AI要求按面板上的顺序从左到右发动技能，所以主角的技能要事先排一下序……
    ActSkills = case Role#role.gd_roleRank of
        1 ->
            get_battle_skill_order(ID, ActSkills0);
        _ ->
            ActSkills0
    end,
	
	?INFO(battle, "ActSkills = ~w, PasSkills = ~w", [ActSkills, PasSkills]),
	?INFO(battle, "StartLevel = ~w", [Role#role.star_lv]),
	?INFO(battle, "p_att = ~w, hp = ~w, maxhp = ~w",[Role#role.p_att,Role#role.gd_currentHp,Role#role.gd_maxHp]),

    NewbieMP = get_newbie_mp(PlayerLevel),
    AngryMP = case get_passive_skill_helper(?PSKILL_ANGER, PasSkills) of
        {true, AngerSkillInfo} ->
            {AngryMP0} = AngerSkillInfo#battle_skill.param,
            ?BATTLE_LOG("被动技能 ~w 生效, 增加怒气 ~w", 
                        [skill:get_skill_uid(?PSKILL_ANGER, AngerSkillInfo#battle_skill.level), AngryMP0]),
            AngryMP0;
        false ->
            0
    end,

    %% NOTE 因为这里有可能给主角增加HP，所以战斗结束后返回的HP列表里的值是
    %%      有可能比角色最大值要大的……
    {CurHP, MaxHP} = {Role#role.gd_currentHp, Role#role.gd_maxHp},

    %% XXX: 现在只有主角有avatar信息
    {AvatarInfo, OfficialAttEnhance} = 
    case is_integer(ID) andalso Role#role.gd_roleRank =:= 1 of
        true ->
            {
                mod_dressing:getAllDress4Server(ID)
                    ++ mod_items:getMainRoleEquipInfo(ID),
                mod_official:get_position_battle_rate(ID)
            };
        _ ->    % false
            {[], 0}
    end,
    ?INFO(battle, "AvatarInfo = ~w", [AvatarInfo]),

    %% 跨服战斗要在玩家名字里加上服务器名……
    RoleName = case is_integer(ID) andalso Role#role.gd_roleRank =:= 1 
            andalso ID >= ?CROSS_INDEX_BASE of
        true ->
            NodeNum = ID div ?CROSS_INDEX_BASE,
            Role#role.gd_name ++ " (" ++ integer_to_list(NodeNum) ++ "服)";
        false ->
            Role#role.gd_name
    end,

    MinMP = Role#role.gd_angry,
    MaxMP = Role#role.gd_max_angry,
    ?INFO(battle, "Role#role.key = ~w, MinMP = ~w, MaxMP = ~w", 
          [Role#role.key, MinMP, MaxMP]),

    InitMP = min(MaxMP, MinMP + NewbieMP + AngryMP),

	#battle_status {
        id        = element(2, Role#role.key),
        player_id = ID,
        name      = RoleName,
        level     = Role#role.gd_roleLevel,
        star      = Role#role.star_lv,
        job       = Role#role.gd_careerID,
        skill     = ActSkills,
        p_skill   = PasSkills,
        n_skill   = NormSkills,
        hp        = CurHP,
        hp_max    = MaxHP,
        mp        = case AccMP > InitMP of
                        true -> AccMP;
                        _    -> InitMP
                    end,
        mp_max    = MaxMP,
        mp_up_extra = Role#role.gd_up_angry,
        p_att     = Role#role.p_att,
        p_def     = Role#role.p_def,
        m_att     = Role#role.m_att,
        m_def     = Role#role.m_def,
        dodge     = Role#role.gd_shanbi,
        hit       = Role#role.gd_mingzhong,
        agility   = Role#role.gd_minjie, 
        speed     = Role#role.gd_speed,
        break     = Role#role.gd_pojia,
        fatal     = Role#role.gd_zhiming,
        crit      = Role#role.gd_baoji,
        luck      = Role#role.gd_xingyun,
        counter   = Role#role.gd_fanji,
        block     = Role#role.gd_gedang,
        is_lead   = Role#role.gd_roleRank == 1,
        avatar_info = AvatarInfo,
        official_att_enhance = OfficialAttEnhance
	}.

mon_2_bs(MonAttr, Pos, MonHp) ->
    Res = transform_skill(MonAttr#mon_attr.skills),
    ?INFO(battle, "monster skill = ~w, start = ~w", [Res, MonAttr#mon_attr.star]),
    
    {ActiveSkills, _, _} = Res,

    MaxHP = if
        is_integer(MonHp) -> MonHp;
        is_list(MonHp) ->
            case lists:keyfind(Pos, 1, MonHp) of
                false   -> MonAttr#mon_attr.hp;
                {_, HP} -> HP
            end
    end,
    CurHP = min(MonAttr#mon_attr.hp, MaxHP),

    ?INFO(battle, "mon at pos ~w, HP = ~w", [Pos, CurHP]),

    AIList = case MonAttr#mon_attr.ai_id of
        0 -> [];
        AIID ->
            ai:transform_ai(data_ai:get(AIID), [], MonAttr#mon_attr.ai_param)
    end,
    
    #battle_status {
        id      = MonAttr#mon_attr.id,
        job     = MonAttr#mon_attr.cat,
        skill   = ActiveSkills,
        name    = MonAttr#mon_attr.name, 
        level   = MonAttr#mon_attr.level,
        star    = MonAttr#mon_attr.star,
        hp      = CurHP,
        hp_max  = MonAttr#mon_attr.hp,
        mp      = MonAttr#mon_attr.mp,
        p_att   = MonAttr#mon_attr.p_att,
        p_def   = MonAttr#mon_attr.p_def,
        m_att   = MonAttr#mon_attr.m_att,
        m_def   = MonAttr#mon_attr.m_def,
        dodge   = MonAttr#mon_attr.dodge,
        counter = MonAttr#mon_attr.counter,
        block   = MonAttr#mon_attr.block,
        break   = MonAttr#mon_attr.break,
        fatal   = MonAttr#mon_attr.fatal,
        luck    = MonAttr#mon_attr.luck,
        hit     = MonAttr#mon_attr.hit,
        crit    = MonAttr#mon_attr.crit,
        speed   = MonAttr#mon_attr.speed,
        is_lead = false,
        is_alive = (CurHP > 0),
        ai      = AIList
    }.

get_mon_info(Camp, MonGroupID, Array) ->
	get_mon_info(Camp, MonGroupID, ?HP_MAX, Array).

get_mon_info(Camp, MonGroupID, MonHp, Array) ->
	Monster = data_mon_group:get(MonGroupID),

	?INFO(battle, "MonGroupID = ~w, Monster = ~w", [MonGroupID, Monster]),
    %% 打个log方便调试，下面照样crash
    case Monster of
        ?UNDEFINED ->
            ?ERR(battle, "Illegal monster group ID: ~w", [MonGroupID]);
        _ ->
            void
    end,

	F = fun({MonID, Pos}, {Arr, List}) ->
			MonAttr = data_mon_attr:get(MonID),
            BattleStatus = mon_2_bs(MonAttr, Pos, MonHp),
			if (Camp == att) ->
				NArray = array:set(Pos, BattleStatus, Arr),
				NList = [Pos | List],
				{NArray, NList};
			true ->
				NArray = array:set(Pos + ?BATTLE_FIELD_SIZE div 2, BattleStatus, Arr),
				NList = [Pos + ?BATTLE_FIELD_SIZE div 2 | List],
				{NArray, NList}
			end
		end,
	{NArray, MerList} = lists:foldl(F, {Array, []}, Monster#mon_group.pos),
	
	PInfo = 
		#player_info {
			id = mon,
			pid = mon,
			mer_list = MerList
		},
	{[PInfo], NArray}.


check_battle_pve(AttID, AttLeaderPID, TeamMateID) ->
    case mod_player:check_battle(AttLeaderPID, self(), TeamMateID) of
        {false, in_battle} ->
            catch mod_err:send_err(AttID, ?ERR_BATTLE_ATTACKER_IS_ON_BATTLE),
            {false, in_battle};
        Other -> Other
    end.

check_battle_pvp(AttID, AttLeaderPID, DefLeaderPID, AttTeamMateID, DefTeamMateID) ->
    case mod_player:check_pvp_battle(
            min(AttLeaderPID, DefLeaderPID), 
            max(AttLeaderPID, DefLeaderPID),
            self(), AttTeamMateID, DefTeamMateID) of
        {false, rival_in_battle} ->
            catch mod_err:send_err(AttID, ?ERR_BATTLE_DEFENDER_IS_ON_BATTLE),
            {false, rival_in_battle};
        Other -> Other
    end.

%% check battle : set the is_battle flag in the PlayerStatus
%% if the att ID or def ID is specified, check it
%% otherwise we can ignore it.

-spec check_battle(BattleStart, IDList1, IDList2, BattleData) -> true | false when 
	IDList1 :: [player_id()],
	IDList2 :: [player_id()],
	BattleStart:: #battle_start{},
	BattleData :: #battle_data{}.

check_battle(Start, IDList1, IDList2, BattleData) ->
	Mod       = Start#battle_start.mod,
	AttID     = Start#battle_start.att_id,
	DefID     = Start#battle_start.def_id,

    GetTeammateID = fun(L) ->
        case tl(L) of 
            [] -> 0;
            [TID] -> TID
        end
    end,

    AttPlayerInfo = get_player_info(AttID, BattleData),
	case erlang:is_integer(AttID) andalso (AttPlayerInfo#player_info.online) of
		true ->
			case mod_player:is_online(AttID) of
                {true, Ps} ->
                    Pid = Ps#player_status.player_pid,
                    TeamMateID = GetTeammateID(IDList1),

                    if (Mod =/= pvp) ->
                        check_battle_pve(AttID, Pid, TeamMateID);
                    true ->
                        DefPlayerInfo = get_player_info(DefID, BattleData),
                        case erlang:is_integer(DefID) andalso (DefPlayerInfo#player_info.online) of
                            true ->
                                %% 竞技场战斗的防守方必定不在线（就算在线也不参与战斗），
                                %% 这时检查进攻方就可以了
                                case lists:member(Start#battle_start.type, ?BATTLE_OFFLINE_PVP_TYPES) of
                                    true ->
                                        check_battle_pve(AttID, Pid, TeamMateID);
                                    false ->
                                        case mod_player:is_online(DefID) of
                                            {true, Ps2} ->
                                                RivalTeamMateID = GetTeammateID(IDList2),
                                                check_battle_pvp(AttID, 
                                                                 Ps#player_status.player_pid,
                                                                 Ps2#player_status.player_pid,
                                                                 TeamMateID, RivalTeamMateID);
                                            false ->
                                                catch mod_err:send_err(AttID, ?ERR_BATTLE_DEFENDER_IS_NOT_ON_LINE),
                                                {false, rival_not_on_line}
                                        end
                                end;
                            false ->
                                check_battle_pve(AttID, Pid, TeamMateID)
                        end
                    end;

                false ->
                    {false, player_not_on_line}
			end;
		false ->
			true
	end.

-spec get_battle_status(Pos, BattleData) -> BattleStatus when
	Pos :: integer(),
	BattleData :: #battle_data{},
	BattleStatus :: #battle_status{}.
														  						 
get_battle_status(Pos, BattleData) ->
	array:get(Pos, BattleData#battle_data.player).

get_battle_status(Pos, Key, BattleData) ->
	Stat = get_battle_status(Pos, BattleData),
	element(Key, Stat).

set_battle_status(Pos, State, BattleData) ->
	Player  = BattleData#battle_data.player,
	NPlayer = array:set(Pos, State, Player),
	BattleData#battle_data {player = NPlayer}.

set_battle_status(Pos, Key, Value, BattleData = #battle_data {player = Player}) ->
	Stat   = get_battle_status(Pos, BattleData),
	NStat  = erlang:setelement(Key, Stat, Value),
	BattleData#battle_data {player = array:set(Pos, NStat, Player)}.
	
%% -spec get_player_list returns the list of position of all the players
get_player_list(BattleData) ->
	get_player_list(BattleData, 1, []).


get_player_list(_BattleData, Index, List) when Index > ?BATTLE_FIELD_SIZE -> List;
get_player_list(BattleData, Index, List) ->
	Elem = get_battle_status(Index, BattleData),
	if (Elem == ?UNDEFINED) ->
			get_player_list(BattleData, Index + 1, List);
	   (Elem#battle_status.is_alive == false) ->
		   	get_player_list(BattleData, Index + 1, List);
	  	true ->
			get_player_list(BattleData, Index + 1, [Index | List])
	end.

%======================================================================================================
% command handler
%======================================================================================================

%% set_battle_cmd just change the battle command of the specified player
%% but *will not* validate the command  
-spec set_battle_cmd(ID, Cmd, BattleData) -> NBattleData when
	ID :: integer(),														   
	Cmd :: battle_cmd(),
	BattleData :: #battle_data{},
	NBattleData :: #battle_data{}.
														   
set_battle_cmd(ID, Cmd, BattleData) ->
	?INFO(battle, "setting battle command"),
	Round = BattleData#battle_data.round,
	{_, Timeout} = BattleData#battle_data.timeout,
	
	case check_player(ID, BattleData) of
		false -> 
			BattleData;
		{true, _} ->
			case Cmd of
				{_Sid, Src, _Tar} ->
					Info = get_player_info(ID, BattleData),
					case lists:member(Src, Info#player_info.mer_list) of
						true ->
							State = get_battle_status(Src, BattleData),
							NState = State#battle_status{cmd = Cmd},
							set_battle_status(Src, NState, BattleData);
						false ->
							BattleData
					end;
				Sid when is_integer(Sid) ->
					Info   = get_player_info(ID, BattleData),
					Lead   = Info#player_info.lead,
					State  = get_battle_status(Lead, BattleData),

                    %% 这里本来要检查玩家是否带“被嘲讽”buff的，但是会造成
                    %% 玩家被嘲讽的那个回合从一开始就不能选到技能，所以把
                    %% 检查放到发动技能之前做好了……

					%% if Sid == 0 then we use AI to set command
					if (Sid == 0) ->						 
                        {RealSID, _, Tar, NewSkillIndex, _} = ai:get_skill(Lead, BattleData),
                        send_command_package(Lead, RealSID, BattleData),

                        %Tar = ai:get_skill_target(RealSID, Lead, BattleData),
                        ?INFO(battle, "Tar = ~w", [Tar]),
						NState = State#battle_status {cmd = {RealSID, Lead, Tar}, skill_index = NewSkillIndex};

					true ->
                        RealSID = case ai:validate_skill(Sid, Lead, BattleData) of
                            true  -> Sid;
                            false -> ?SKILL_COMMON_ATTACK;
                            {false, _InvalidReason} -> 
                                ?INFO(battle, "InvalidReason = ~w", [_InvalidReason]),
                                ?SKILL_COMMON_ATTACK
                        end,
                        send_command_confirm_package(Info#player_info.lead, RealSID, BattleData),

                        Tar = ai:get_skill_target(RealSID, Lead, BattleData),
                        ?INFO(battle, "Tar = ~w", [Tar]),
						NState = State#battle_status {cmd = {RealSID, Lead, Tar}},
						?INFO(battle, "set battle cmd: Sid = ~w, Lead = ~w, Round = ~w, Timeout = ~w", 
						  	[RealSID, Lead, Round, Timeout])
					end,

                    ?INFO(battle, "Final cmd: ~w", [NState#battle_status.cmd]),

					set_battle_status(Lead, NState, BattleData); %% returns a BattleData
				_ ->
					BattleData
			end
	end. 

%% handle command ex handle commands repeatlly until reach the player's pos
-spec handle_command_ex(#battle_data{}, Repeat) -> #battle_data{} 
	when Repeat :: boolean().

handle_command_ex(BattleData, Repeat) ->
    Type      = BattleData#battle_data.type,
	Round     = BattleData#battle_data.round,
	OrderList = BattleData#battle_data.attorder,
	Src       = hd(OrderList),
	SrcStat   = get_battle_status(Src, BattleData),
	SIndex    = SrcStat#battle_status.skill_index,

	?INFO(battle, "Calling Handle command Ex in Round ~w", [Round]),
	?INFO(battle, "Src = ~w", [Src]),
	
	IsFaint = is_buff_exist(?BUFF_FAINT, SrcStat),
    if
        IsFaint =:= true ->
            ?BATTLE_LOG("~n--------- 攻击者站位: ~w ---------", [Src]),
            ?BATTLE_LOG("站位 ~w Buff效果: 晕", [Src]),
            ?BATTLE_LOG("    Buff类型: 晕");
        true -> void
    end,
	
	%% calculate a new BattleData through handling the skill, 
	%% indicated by NBattleData
	NBattleData = 
		if (SrcStat#battle_status.is_alive == false) ->
            ?INFO(battle, "Skip = true ?!"),
			Skip = true,
			BattleData;
		true ->
			Skip = false,
			{Sid, Src, Tar, Idx, SkillStat} = 
				if (IsFaint == true) -> 
					{0, Src, 0, SIndex, ?BATTLE_SKILL_STAT_NORMAL};
				true ->
					?INFO(battle, "Preset Src = ~w, Cmd = ~w", [Src, SrcStat#battle_status.cmd]),
		
					case SrcStat#battle_status.cmd of
						?UNDEFINED ->
                            NeedRandSkill = case SrcStat#battle_status.is_lead of
                                false -> 
                                    true;       % 玩家角色外的所有角色都应该自动选技能……
                                true ->
                                    case SrcStat#battle_status.player_id of
                                        ?UNDEFINED -> 
                                            true;       % 是玩家角色，但是玩家不在线，自动选技能……
                                        CurPlayerID ->
                                            CurPlayerInfo = get_player_info(CurPlayerID, BattleData),
                                            case CurPlayerInfo#player_info.online of
                                                true  -> false;     % 是玩家角色，而且玩家在线，认为玩家超时了也没选技能
                                                false -> true       % 是玩家角色，但是玩家不在线，自动选技能……
                                            end
                                    end
                            end,

							if 
                                NeedRandSkill =:= false ->
                                    T = ai:get_skill_target(?SKILL_COMMON_ATTACK, Src, BattleData),
                                    {?SKILL_COMMON_ATTACK, Src, T, SIndex, ?BATTLE_SKILL_STAT_NORMAL};
                                true ->  
                                    %% 这里是怪物AI，处理嘲讽buff选技能的逻辑……
                                    case lists:keyfind(?BUFF_SCORNED, #buff.name, SrcStat#battle_status.buff) of
                                        false ->
                                            ai:get_skill(Src, BattleData);
                                        _ ->
                                            ?BATTLE_LOG("~n--------- 攻击者站位: ~w ---------", [Src]),
                                            ?BATTLE_LOG("站位 ~w Buff效果: 嘲讽", [Src]),
                                            ?BATTLE_LOG("    Buff类型: 被嘲讽, 新技能ID: ~w", [?SKILL_COMMON_ATTACK]),
                                            T = ai:get_skill_target(?SKILL_COMMON_ATTACK, Src, BattleData),
                                            {?SKILL_COMMON_ATTACK, Src, T, SIndex, ?BATTLE_SKILL_STAT_SCORNED}
                                    end
							end;

						{S, Src, OTar} ->
							%% case (2): 

                            %% 可能在选了技能之后被嘲讽了，这里要再检查一下……
                            NewS = case lists:keyfind(?BUFF_SCORNED, #buff.name, SrcStat#battle_status.buff) of
                                false -> S;
                                _     -> ?SKILL_COMMON_ATTACK
                            end,
                            ?INFO(battle, "OTar = ~w", [OTar]),
                            T = case is_integer(OTar) andalso OTar > 0 andalso OTar =< ?BATTLE_FIELD_SIZE 
                                    andalso (array:get(OTar, BattleData#battle_data.player))#battle_status.is_alive of
                                true  -> OTar;
                                false -> 
                                    case OTar of
                                        {ai_override, _} -> OTar;      % XXX: 从AI来的目标列表，直接放过去
                                        _ -> ai:get_skill_target(NewS, Src, BattleData)
                                    end
                            end,
                            ?INFO(battle, "T = ~w", [T]),
                            case NewS of
                                ?SKILL_COMMON_ATTACK ->
                                    ?INFO(battle, "NewS = ~w, S = ~w", [NewS, S]),
                                    case NewS =:= S of
                                        true ->
                                            {NewS, Src, T, SIndex, ?BATTLE_SKILL_STAT_NORMAL};
                                        false ->
                                            {NewS, Src, T, SIndex, ?BATTLE_SKILL_STAT_SCORNED}
                                    end;
                                _ ->
                                    case ai:validate_skill(NewS, Src, BattleData) of
                                        true ->
                                            ?INFO(battle, "validation passed."),
                                            ?INFO(battle, "Sid = ~w, Src = ~w, Tar = ~w", [NewS, Src, T]),
                                            {NewS, Src, T, SIndex, ?BATTLE_SKILL_STAT_NORMAL};
                                        false ->
                                            ?INFO(battle, "validation not passed."),
                                            {?SKILL_COMMON_ATTACK, Src, T, SIndex, ?BATTLE_SKILL_STAT_NORMAL};
                                        {false, ValidateStat} ->
                                            ?INFO(battle, "validation not passed. ValidateStat = ~w", [ValidateStat]),
                                            {?SKILL_COMMON_ATTACK, Src, T, SIndex, ValidateStat}
                                    end
                            end
					end
				end, 
			
			BattleData1 = skill:handle_skill(Sid, SkillStat, Src, Tar, BattleData),

			%% send cd package
			if (SrcStat#battle_status.is_lead == true) ->
				send_cd_package(Src, BattleData1); 
			true ->
				ok
			end,
			
            ?INFO(battle, "Sending process package for pos ~w", [Src]),
			%% procedure package
			send_procedure_package(BattleData1),
			
			NSrcStat = get_battle_status(Src, BattleData1),	
            case NSrcStat#battle_status.is_lead of
                true ->
                    ?INFO(battle, "Idx = ~w", [Idx]),
                    ?INFO(battle, "Cmd = ~w", [SrcStat#battle_status.cmd]);
                false ->
                    void
            end,
			set_battle_status(Src, NSrcStat#battle_status {cmd = ?UNDEFINED, skill_index = Idx}, BattleData1)
		end,
		
	%% the following piece of code calculate NBattleData1
	if (Skip == false) ->
		case Type == ?BATTLE_TYPE_BOSS andalso Src =< ?BATTLE_FIELD_SIZE div 2 of
		true -> 
			BossID     = BattleData#battle_data.monster,
			[ID | _]   = get_ids(BattleData),
			BossState1 = get_battle_status(?BOSS_POSITION, BattleData),
			BossState2 = get_battle_status(?BOSS_POSITION, NBattleData),
			BossDamage = BossState1#battle_status.hp - BossState2#battle_status.hp,
			?INFO(boss, "Damage dealt to boss = ~w", [BossDamage]),

			case catch g_boss:get_boss_hp(ID, BossID, BossDamage) of
				alive -> BossAlive = true;
				dead  -> BossAlive = {false, dead};
				kill  -> 
					?INFO(boss, "boss is killed by player"),
					BossAlive = {false, kill};
				{'EXIT', _} -> BossAlive = true %% boss's blood must large enough so boss can not be kill 
			end;
		false ->
			BossAlive = true
		end,
		
		if (Type =/= ?BATTLE_TYPE_BOSS orelse BossAlive == true) ->
				NBattleData1 = NBattleData;
		   (BossAlive == {false, dead}) ->
				NBattleData1 = NBattleData#battle_data{winner = def};
		   (BossAlive == {false, kill}) ->
				NBattleData1 = NBattleData#battle_data{winner = att}
		end;
	true ->
		NBattleData1 = NBattleData
	end,
			
	NBattleData2 = 
		case is_battle_end(NBattleData1) of
		{true, Camp} -> NBattleData1#battle_data{winner = Camp};
		false ->
            ?INFO(battle, "OrderList = ~w", [OrderList]),
			case length(OrderList) of
				1 -> 
					if NBattleData1#battle_data.type =/= ?BATTLE_TYPE_CROSS_PVE
                            andalso NBattleData1#battle_data.round =:= 30 ->
					    NBattleData#battle_data{winner = def};
					true ->
						%% if this player is the last one on the attack list
						%% it means the round is over, we have to start a new one
						start_new_round(NBattleData1)
					end;
				_ ->
					%% update the order list according to the speed
					NBattleData1#battle_data {attorder = update_order_list(tl(OrderList), NBattleData1)}
			end
		end,
    
    case Skip of
        false ->
            send_order_package(NBattleData2);
        _ ->
            void
    end,
	
	%% notice: is_battle_end returns false | {true, camp()}
	case (Repeat == false orelse is_battle_end(NBattleData2) =/= false) of
		true -> 
			NBattleData2;
		false ->
			NOrderList = NBattleData2#battle_data.attorder,
            ?INFO(battle, "NOrderList = ~w", [NOrderList]),
			NextSrc = hd(NOrderList),
			?INFO(battle, "NextSrc = ~w", [NextSrc]),
			NextSrcStat = get_battle_status(NextSrc, NBattleData2),

            NextPlayerOnline = case NextSrcStat#battle_status.player_id of
                ?UNDEFINED -> false;
                NextPlayerID ->
                    case get_player_info(NextPlayerID, NBattleData2) of
                        false -> false;
                        PInfo -> PInfo#player_info.online
                    end
            end,

            OnlineIDList = get_online_ids(NBattleData2),
			case (NextSrcStat#battle_status.is_lead == true 
                        andalso NextSrcStat#battle_status.is_alive =:= true
                        andalso NextPlayerOnline =:= true)
                    orelse (OnlineIDList =/= [] 
                            andalso length(OrderList) =:= 1) of
                true ->
                    NBattleData2;
                _ ->        % false
                    handle_command_ex(NBattleData2, true)
			end
	end.

handle_command(BattleData) ->
	handle_command_ex(BattleData, false).

%% start new round generates a new battle_pro record, a new speed list,
%% clear the finish_play flag ...
-spec start_new_round(BattleData) -> NBattleData when
	BattleData :: #battle_data{},
	NBattleData :: #battle_data{}.

start_new_round(BattleData0) ->
    BattleData = end_round_hook(BattleData0),

	Round     = BattleData#battle_data.round,
	?INFO(battle, "starting new round, Round = ~w", [Round + 1]),
	
	BattlePro = #battle_pro {round = Round + 1, attack_pro = []},
	%% get the attack order list for further processing.
	AttOrder  = get_att_order(BattleData),
	Attacker  = BattleData#battle_data.attacker,
	Defender  = BattleData#battle_data.defender,

	%% update the finish_play state
	NAttacker = [PlayerInfo#player_info{finish_play = false} || PlayerInfo <- Attacker],
	NDefender = [PlayerInfo#player_info{finish_play = false} || PlayerInfo <- Defender],

	NBattleData = BattleData#battle_data {
		round     = Round + 1,
		attorder  = AttOrder, 
	%% if report is needed
	%%	procedure = [BattlePro | BattleData#battle_data.procedure],
	%% else
		procedure = [BattlePro],
	%% endif
		attacker  = NAttacker,
		defender  = NDefender
	},
    start_round_hook(NBattleData).

get_att_order(BattleData) ->
	?INFO(battle, "calling get_player_list"),
	update_order_list(get_player_list(BattleData), BattleData).

adjust_target(_Sid, Tar, BattleData) ->
	TarStat = get_battle_status(Tar, BattleData),
	
	if ((TarStat =/= ?UNDEFINED) andalso (TarStat#battle_status.is_alive == true)) -> Tar;
		true ->
			if (Tar =< ?BATTLE_FIELD_SIZE div 2) ->
				adjust_target_1(1, BattleData);
				true ->
					adjust_target_1(?BATTLE_FIELD_SIZE div 2 + 1, BattleData)
			end
	end.

adjust_target_1(Tar, BattleData) ->
	State = get_battle_status(Tar, BattleData),
	if ((State =/= ?UNDEFINED) andalso (State#battle_status.is_alive == true)) -> Tar;
		true -> adjust_target_1(Tar + 1, BattleData)
	end.
	
%% (3) is all player finish playing the animation?
is_all_finish_play(BattleData) ->
	InfoList = 
		BattleData#battle_data.attacker ++ 
		BattleData#battle_data.defender,
	Pred = 
		fun(Info) ->
			Info#player_info.finish_play == true orelse
			Info#player_info.online == false orelse
			not is_integer(Info#player_info.id)
		end,
	lists:all(Pred, InfoList).

-spec logout_info(#battle_data{}) -> all | att | def | none.
logout_info(BattleData) ->
	AttLogout = is_all_logout(att, BattleData),
	DefLogout = is_all_logout(def, BattleData),
			
	if (AttLogout == true andalso DefLogout == true) ->
			all;
	   (AttLogout == true) ->
			att;
	   (DefLogout == true) ->
			def;
	   true ->
			none
	end.

is_all_logout(Camp, BattleData) ->
	InfoList = 
		case Camp of
			att -> BattleData#battle_data.attacker;
			def -> BattleData#battle_data.defender
		end,
	Pred = 
		fun (Info) ->
			Info#player_info.online == false orelse not is_integer(Info#player_info.id)
		end,
	lists:all(Pred, InfoList).

is_all_quit_battle(BattleData) ->
	InfoList = 
		BattleData#battle_data.attacker ++
		BattleData#battle_data.defender,
	Pred = 
		fun(Info) ->
			Info#player_info.finish_play == quit orelse
			Info#player_info.online == false orelse
			not is_integer(Info#player_info.id)
		end,
	lists:all(Pred, InfoList).
	
%% is battle finish? all the player of some camp have been dead?
%% or the battle round is > 30
is_battle_end(BattleData) ->
	if (BattleData#battle_data.winner =/= ?UNDEFINED) -> 
			{true, BattleData#battle_data.winner};
		true ->
			case is_battle_end(att, 1, BattleData) of
				true  -> {true, def};
				false -> 
					case is_battle_end(def, ?BATTLE_FIELD_SIZE div 2 + 1, BattleData) of
						true  -> {true, att};
						false -> false
					end
			end
	end.
	
%% check if all the attacker is dead
is_battle_end(Camp, Index, BattleData) -> 
	State = get_battle_status(Index, BattleData),
	if (State =/= ?UNDEFINED) andalso (State#battle_status.is_alive == true) ->
		false;
	true ->
		case (Index == ?BATTLE_FIELD_SIZE div 2) orelse (Index == ?BATTLE_FIELD_SIZE) of
			true  -> true;
			false -> is_battle_end(Camp, Index + 1, BattleData) 
		end
	end.


is_buff_exist(BuffName, State) ->
	BuffList = State#battle_status.buff,
	case lists:keysearch(BuffName, #buff.name, BuffList) of
		false -> false;
		{value, _} -> true
	end.

%% absorb life from enemy
get_hp_absorb(Src, Tar, AttSpec, Dm, BattleData) ->
	SrcStat = get_battle_status(Src, BattleData),
	TarStat = get_battle_status(Tar, BattleData),
	Buffs = AttSpec#attack_spec.buff_add ++ SrcStat#battle_status.buff,
	
	LifeDrain = case lists:keysearch(?BUFF_LIFE_DRAIN, #buff.name, Buffs) of
		{value, #buff{value = V}} -> 
            LifeDrain0 = round(min(Dm, TarStat#battle_status.hp) * V),
            ?BATTLE_LOG("        站位 ~w Buff效果: 吸血", [Src]),
            ?BATTLE_LOG("            Buff类型: 吸血, 系数: ~w, 吸血点数: ~w", [V, LifeDrain0]),
            LifeDrain0;
		false ->
			0
	end,
    LifeDrain.

get_mp_absorb(Src, Tar, AttSpec, BattleData) ->
	SrcStat = get_battle_status(Src, BattleData),
	TarStat = get_battle_status(Tar, BattleData), 
	Buffs = AttSpec#attack_spec.buff_add ++ SrcStat#battle_status.buff,

	case lists:keysearch(?BUFF_MANA_DRAIN, #buff.name, Buffs) of
		{value, #buff{value = {A, S}, by_rate = ByRate}} ->
            {Add0, Sub0} = case ByRate of
                false -> {A, S};
                true  -> 
                    TarMP = TarStat#battle_status.mp,
                    {round(TarMP * A), round(TarMP * S)}
            end,
            ?BATTLE_LOG("        站位 ~w Buff效果: 吸怒气", [Src]),
            ?BATTLE_LOG("            Buff类型: 吸怒气, 攻击者+: ~w, 目标-: ~w", 
                        [Add0, Sub0]),
            SubF = min(Sub0, TarStat#battle_status.mp),
            AddT = min(SubF, Add0),
            {AddT, SubF};
		false ->
            case lists:keysearch(?BUFF_MANA_ADJUST, #buff.name, Buffs) of
                {value, #buff{value = {A, S}, by_rate = ByRate}} ->
                    {Add0, Sub0} = case ByRate of
                        false -> {A, S};
                        true  -> 
                            SrcMP = SrcStat#battle_status.mp,
                            TarMP = TarStat#battle_status.mp,
                            {round(SrcMP * A), round(TarMP * S)}
                    end,
                    ?BATTLE_LOG("        站位 ~w Buff效果: 调整怒气", [Src]),
                    ?BATTLE_LOG("            Buff类型: 调整怒气, 攻击者+: ~w, 目标-: ~w", 
                                [Add0, Sub0]),
                    {Add0, Sub0};
                false ->
                    {0, 0}
            end
	end.

get_rebound(Src, Tar, _AttSpec, Dm, BattleData) ->
	TarStat = get_battle_status(Tar, BattleData),	
	Buffs = TarStat#battle_status.buff,
	
	RRB = case TarStat#battle_status.hp =< Dm of
		true ->
			0;
		false ->
            %% XXX: 坑！！策划要求世界Boss对反震技能免疫
            RB = case BattleData#battle_data.type =:= ?BATTLE_TYPE_BOSS
                        andalso Src =:= ?BOSS_POSITION of
                false ->
                    case lists:keysearch(?BUFF_REBOUND, #buff.name, Buffs) of
                        {value, #buff{value = Rebound}} ->
                            RB0 = round(Dm * Rebound),
                            ?BATTLE_LOG("        站位 ~w Buff效果: 反弹", [Tar]),
                            ?BATTLE_LOG("            Buff类型: 反弹, 系数: ~w, 反弹伤害: ~w", [Rebound, RB0]),
                            RB0;
                        false ->
                            case get_passive_skill(?PSKILL_REBOUND, Tar, BattleData) of
                                {true, RBSkillInfo} ->
                                    {RBRate, RBVal} = RBSkillInfo#battle_skill.param,
                                    RBRand = random:uniform(),
                                    ?BATTLE_LOG("        被动技能 ~w, 几率: ~w, 随机数: ~w, 生效: ~w",
                                                [skill:get_skill_uid(?PSKILL_REBOUND, RBSkillInfo#battle_skill.level), 
                                                 RBRate, RBRand, RBRand =< RBRate]),
                                    case RBRand =< RBRate of
                                        true ->
                                            RB0 = round(Dm * RBVal),
                                            ?BATTLE_LOG("            被动技能 ~w 生效, 反弹系数: ~w, 反弹伤害: ~w",
                                                        [skill:get_skill_uid(?PSKILL_REBOUND, RBSkillInfo#battle_skill.level),
                                                         RBVal, RB0]),
                                            RB0;
                                        _ ->        % false
                                            0
                                    end;
                                false ->
                                    0
                            end
                    end;
                true ->
                    0
            end,
            RB
	end,
    RRB.

%% Damage is the damage from Src dealed to Tar
get_counter(Src, Tar, SkillId, _AttSpec, Dm, BattleData) ->
	CT = if (SkillId =/= ?SKILL_COMMON_ATTACK_ID) ->
		%% skill that is not common attack can not be countered.
		false;
	true ->
		TarStat = get_battle_status(Tar, BattleData),
		Buffs = TarStat#battle_status.buff,
		
		case TarStat#battle_status.hp =< Dm of
			true -> 
				false; %% this player is now dead, so don't need to check counter
			false ->
				case lists:keysearch(?BUFF_COUNTER, #buff.name, Buffs) of
					{value, #buff{value = _Counter}} ->
                        ?BATTLE_LOG("        站位 ~w Buff效果: 反击", [Tar]),
                        ?BATTLE_LOG("            Buff类型: 反击"),
						true;
					false ->
                        SrcStat = get_battle_status(Src, BattleData),
                        ParamX = 
                            case SrcStat#battle_status.is_lead == true orelse 
                                TarStat#battle_status.is_lead == true of
                                true  -> 0;
                                false -> 1
                            end,
                        {AStar, DStar} = {SrcStat#battle_status.star, TarStat#battle_status.star},
                        CounterAttr = get_adjust_value(counter, TarStat#battle_status.counter, Tar, BattleData),
                        CounterRate = max(0, CounterAttr / 1666 + ParamX * 0.03 * (AStar - DStar)),
					
                        CounterRand = random:uniform(),
                        ?BATTLE_LOG("        反击几率: ~w, 随机数: ~w, 反击触发: ~w",
                                    [CounterRate, CounterRand, CounterRand =< CounterRate]),
						case CounterRand =< CounterRate of
							true  -> true;
							false -> false
						end
				end
		end
	end,

    case CT of
        true ->
            ?BATTLE_LOG("        ......... 反击伤害计算 ........."),
            CounterAttSpec = #attack_spec {
                addition = 0.4,
                targets  = [Src]
            },
            {_, _, {NewDmgShieldBuff, AbsorbedDmg}, CounterDmg} = do_attack(Tar, Src, CounterAttSpec, BattleData),
            ?INFO(battle, "NewDmgShieldBuff = ~w", [NewDmgShieldBuff]),
            ?BATTLE_LOG("        ......... 反击伤害计算结束, 反击伤害: ~w .........", [CounterDmg]),
            {{NewDmgShieldBuff, AbsorbedDmg}, CounterDmg};
        false ->
            {{none, 0}, 0}
    end.

%==============================================================================================================
% handling passive SKILL
%==============================================================================================================

-spec get_passive_skill(SkillID, Pos, BattleData) -> false | {true, Skill} when
	SkillID    :: integer(),
	Pos        :: integer(),
	BattleData :: #battle_data{},
    Skill      :: #battle_skill{}.

get_passive_skill(SkillID, Pos, BattleData) ->
	State = get_battle_status(Pos, BattleData),
    get_passive_skill_helper(SkillID, State#battle_status.p_skill).

get_passive_skill_helper(SkillID, [UID | Rest]) ->
    case skill:get_skill_id_level(UID) of
        {SkillID, Level} ->
            {true, data_skill_table:get(SkillID, Level)};
        _ ->
            get_passive_skill_helper(SkillID, Rest)
    end;
get_passive_skill_helper(_SkillID, []) ->
    false.

get_norm_skill(SkillID, Pos, BattleData) ->
	State = get_battle_status(Pos, BattleData),
    get_norm_skill_helper(SkillID, State#battle_status.n_skill).

get_norm_skill_helper(SkillID, [UID | Rest]) ->
    case skill:get_skill_id_level(UID) of
        {SkillID, _Level} ->
            {true, data_skill:get_role_added_attri(UID)};
        _ ->
            get_norm_skill_helper(SkillID, Rest)
    end;
get_norm_skill_helper(_SkillID, []) ->
    false.

%==============================================================================================================
% handling buff value
%==============================================================================================================
%% get_buff_value returns the buff effect 

-spec get_buff_value(buff_id(), Pos :: integer(), #battle_data{}) -> 
		  {Rate :: float() | integer(), Num :: integer()}. 

get_buff_value(BuffName, Pos, BattleData) ->
    get_buff_value(BuffName, Pos, BattleData, []).

get_buff_value(BuffName, Pos, BattleData, ExtraBuffList) ->
	State = get_battle_status(Pos, BattleData),
	%% ?INFO(battle, "Pos = ~w, State = ~w", [Pos, State]),
	Buffs = State#battle_status.buff ++ ExtraBuffList,
	%% ?INFO(battle, "Buffs = ~w", [Buffs]),
	{F, I} = get_buff_value_1(BuffName, Buffs, {0, 0}),
	%% ?INFO(battle, "F = ~w, I = ~w", [F, I]),
	{F, I}.

get_buff_value_1(_BuffName, [], {F, I}) -> {F, I};
get_buff_value_1(BuffName, [#buff{name = Name, by_rate = B, value = Value} | Rest], {F, I}) ->
	if (BuffName =/= Name) ->
		get_buff_value_1(BuffName, Rest, {F, I});
	true ->
		if (B == true) ->
				get_buff_value_1(BuffName, Rest, {Value + F, I});
		   (B == false) ->
				get_buff_value_1(BuffName, Rest, {F, Value + I});
		 	true ->
				get_buff_value_1(BuffName, Rest, {F, I})
		end
	end.

get_adjust_value(Tag, Attr, Pos, BattleData) ->
    get_adjust_value(Tag, Attr, Pos, BattleData, []).

get_adjust_value(Tag, Attr, Pos, BattleData, ExtraBuff) ->
	{Buff, DeBuff} = 
		case Tag of
			pdef -> {?BUFF_PDEF_UP, ?BUFF_PDEF_DOWN};
			mdef -> {?BUFF_MDEF_UP, ?BUFF_MDEF_DOWN};
			att  -> {?BUFF_ATT_UP,  ?BUFF_ATT_DOWN};
			crit -> {?BUFF_CRIT_UP, ?BUFF_CRIT_DOWN};
			luck -> {?BUFF_LUCK_UP, ?BUFF_LUCK_DOWN};
            speed -> {?BUFF_SPEED_UP, ?BUFF_SPEED_DOWN};
            hit  -> {?BUFF_HIT_UP,  ?BUFF_HIT_DOWN};
            dodge -> {?BUFF_DODGE_UP, ?BUFF_DODGE_DOWN};
            block -> {?BUFF_BLOCK_UP, ?BUFF_BLOCK_DOWN};
            fatal -> {?BUFF_FATAL_UP, ?BUFF_FATAL_DOWN};
            counter -> {?BUFF_COUNTER_UP, ?BUFF_COUNTER_DOWN};
            cast_damage -> {?BUFF_CAST_DMG_UP, ?BUFF_CAST_DMG_DOWN};
            recv_damage -> {?BUFF_RECV_DMG_UP, ?BUFF_RECV_DMG_DOWN};
            att_enhance -> {?BUFF_ATT_ENHANCE, ?BUFF_ATT_WORSEN}
		end,
	{F1, I1} = get_buff_value(Buff,   Pos, BattleData, ExtraBuff), 
	{F2, I2} = get_buff_value(DeBuff, Pos, BattleData, ExtraBuff),
    if 
        F1 =/= 0 orelse I1 =/= 0 orelse F2 =/= 0 orelse I2 =/= 0 ->
            ?BATTLE_LOG("        站位 ~w Buff效果: ~s", [Pos, tag_to_buff_effect(Tag)]),
            ?BATTLE_LOG("            Buff类型: ~s, 浮点系数: ~w, 整数值: ~w", 
                        [buff_type_to_str(Buff), F1, I1]),
            ?BATTLE_LOG("            Debuff类型: ~s, 浮点系数: ~w, 整数值: ~w", 
                        [buff_type_to_str(DeBuff), F2, I2]),
            Output = round(Attr * (1 + F1 - F2)) + I1 - I2,
            ?BATTLE_LOG("            综合输出: ~w / ~w / ~w", [Attr, Output, Output - Attr]),
            Output;
        true ->
            Attr
    end.

%==============================================================================================================
% attack handler and assist handler
%==============================================================================================================

get_target_list(RangeList, BattleData) ->
	get_target_list([], RangeList, BattleData#battle_data.player).

get_target_list(TargetList, [], _Player) ->
	TargetList;

get_target_list(TargetList, [Head | Tail], Player) -> 
	Elem = array:get(Head, Player),
	if (Elem == undefined) orelse (Elem#battle_status.is_alive == false) ->
			get_target_list(TargetList, Tail, Player);
		true ->
			get_target_list(TargetList ++ [Head], Tail, Player)
	end.

%% return a list of range(integer) according to the type of range
%% and then pass to the function get_target_list,
%% get_target_list will filter out those not suitable position
%% by the information of BattleData; 

calc_range(Target, Range) ->		
	RangeList = 
		case Range of 
			?ALLFRIENDLY -> %% Target's teammates;
				if (Target =< ?BATTLE_FIELD_SIZE div 2) ->
					lists:seq(1, ?BATTLE_FIELD_SIZE div 2);	
					true -> lists:seq(?BATTLE_FIELD_SIZE div 2 + 1, ?BATTLE_FIELD_SIZE)
				end;
			
			?ALLENEMY -> %% Target's enemies;
				if (Target =< ?BATTLE_FIELD_SIZE div 2) -> 
					lists:seq(?BATTLE_FIELD_SIZE div 2 + 1, ?BATTLE_FIELD_SIZE);	   
					true -> lists:seq(1, ?BATTLE_FIELD_SIZE div 2)
				end;
			
			?HORIZONTAL -> 
				lists:seq(Target - (Target - 1) rem ?BATTLE_ROW_SIZE, 
					Target - (Target - 1) rem ?BATTLE_ROW_SIZE + ?BATTLE_ROW_SIZE - 1);
			
			?VERTICAL -> 
				if (Target =< ?BATTLE_FIELD_SIZE div 2) ->
					lists:seq((Target - 1) rem ?BATTLE_ROW_SIZE + 1, 
							?BATTLE_FIELD_SIZE div 2, ?BATTLE_ROW_SIZE);
					true -> lists:seq((Target - 1) rem ?BATTLE_ROW_SIZE + 1 + ?BATTLE_FIELD_SIZE div 2, 
							?BATTLE_FIELD_SIZE, ?BATTLE_ROW_SIZE)
				end;
			?SINGLE -> [Target]; 
			
			?ALL -> lists:seq(1, ?BATTLE_FIELD_SIZE);
			
			?ENEMYFRONT ->
				if (Target =< ?BATTLE_FIELD_SIZE div 2) ->
					lists:seq(?BATTLE_FIELD_SIZE div 2 + 1, ?BATTLE_FIELD_SIZE div 2 + ?BATTLE_ROW_SIZE);
				   	true -> lists:seq(1, ?BATTLE_ROW_SIZE)
				end;
						
			?ENEMYBACK  ->
				if (Target =< ?BATTLE_FIELD_SIZE div 2) ->
					lists:seq(?BATTLE_FIELD_SIZE - ?BATTLE_ROW_SIZE + 1, ?BATTLE_FIELD_SIZE);
					true -> lists:seq(?BATTLE_FIELD_SIZE div 2 - ?BATTLE_ROW_SIZE + 1, 
									  ?BATTLE_FIELD_SIZE div 2)
				end;
				
			?FRIENDFRONT ->
				if (Target =< ?BATTLE_FIELD_SIZE div 2) ->
					lists:seq(1, ?BATTLE_ROW_SIZE);
					true -> lists:seq(?BATTLE_FIELD_SIZE div 2 + 1, 
									  ?BATTLE_FIELD_SIZE div 2 + ?BATTLE_ROW_SIZE)
				end;
						
			?FRIENDBACK  ->
				if (Target =< ?BATTLE_FIELD_SIZE div 2) ->
					lists:seq(?BATTLE_FIELD_SIZE div 2 - ?BATTLE_ROW_SIZE + 1, ?BATTLE_FIELD_SIZE);
				   	true -> lists:seq(?BATTLE_FIELD_SIZE - ?BATTLE_ROW_SIZE + 1, ?BATTLE_FIELD_SIZE)
				end;

			?ADJACENT -> 
				List1 = 
					if ((Target - 1) rem ?BATTLE_ROW_SIZE > 0) -> [Target - 1];
						true -> []
					end,
				List2 = 
					if ((Target rem ?BATTLE_ROW_SIZE) /= 0) -> [Target + 1 | List1];
						true -> List1
					end,
				List3 = 
					if (Target + ?BATTLE_ROW_SIZE =< ?BATTLE_FIELD_SIZE div 2) orelse
					   ((Target > ?BATTLE_FIELD_SIZE div 2) andalso
						(Target + ?BATTLE_ROW_SIZE =< ?BATTLE_FIELD_SIZE)) -> [Target + ?BATTLE_ROW_SIZE | List2];
					true ->
						List2
					end,
				List4 = 
					if (Target - ?BATTLE_ROW_SIZE > ?BATTLE_FIELD_SIZE div 2) orelse
					   ((Target =< ?BATTLE_FIELD_SIZE div 2) andalso
					    (Target - ?BATTLE_ROW_SIZE > 0)) -> [Target - ?BATTLE_ROW_SIZE | List3];
					true ->
						List3
					end,
				[Target | List4];

			?OBLIQUE -> 
				List1 = 
					if ((Target - 1) rem ?BATTLE_ROW_SIZE > 0) -> 
						[Target - 1 - ?BATTLE_ROW_SIZE, Target - 1 + ?BATTLE_ROW_SIZE];
					true -> 
						[]
					end,
				List2 = 
					if (Target rem ?BATTLE_ROW_SIZE) /= 0 ->
						[Target + 1 - ?BATTLE_ROW_SIZE, Target + 1 + ?BATTLE_ROW_SIZE | List1];
					true ->
						List1
					end,
				F = fun(P) ->
						if (P < 0 orelse P > ?BATTLE_FIELD_SIZE) -> false;
						   (P =< ?BATTLE_FIELD_SIZE div 2) andalso (Target =< ?BATTLE_FIELD_SIZE div 2) -> true;
						   (P > ?BATTLE_FIELD_SIZE div 2) andalso (Target > ?BATTLE_FIELD_SIZE div 2) -> true;
							true -> false
						end
					end,
				[Target | lists:filter(F, List2)]	
		end,
	RangeList.

%% pre_attack calculate whether this attack will miss or not
-spec pre_attack(Src, Tar, AttSpec, BattleData) -> boolean() when
	Src        :: integer(),
	Tar        :: integer(),
	AttSpec    :: #attack_spec{},
	BattleData :: #battle_data{}.

pre_attack(Src, Tar, AttSpec, BattleData) ->
	SrcStat = get_battle_status(Src, BattleData),
	TarStat = get_battle_status(Tar, BattleData),
	
	{Hit, Dodge} = {
        get_adjust_value(hit, SrcStat#battle_status.hit, Src, BattleData, AttSpec#attack_spec.buff_add), 
        get_adjust_value(dodge, TarStat#battle_status.dodge, Tar, BattleData)
    },
	{AStar, DStar} = {SrcStat#battle_status.star, TarStat#battle_status.star},

	Param6 = 
		case SrcStat#battle_status.is_lead == true orelse
			 TarStat#battle_status.is_lead == true of
		true -> 0;
		false -> 0      % 策划可能会改这个系数，所以还是和上面那个零分开返回……
		end,

	HitRate = Hit * 10 / (Hit * 9.6 + Dodge) + Param6 * (AStar - DStar),
    NHitRate = min(1.0, max(0.4, HitRate)),
	
	Rand = random:uniform(), 
    ?BATTLE_LOG("        命中几率: ~w, 随机数: ~w, 命中: ~w", [NHitRate, Rand, Rand =< NHitRate]),
    Rand =< NHitRate.

		
-spec do_attack(Src, Tar, AttSpec, BattleData) -> {CritcalHit, Blocked, Damage} when
	Src        :: integer(),
	Tar        :: integer(),
	AttSpec    :: #attack_spec{},
	BattleData :: #battle_data{},
	CritcalHit :: true | false,
    Blocked    :: true | false,
	Damage     :: integer().

do_attack(Src, Tar, AttSpec, BattleData) ->
	SrcStat = get_battle_status(Src, BattleData),
	
	Buff = SrcStat#battle_status.buff,
	BuffAdd = AttSpec#attack_spec.buff_add,
	
	TarStat = get_battle_status(Tar, BattleData),
	Job = SrcStat#battle_status.job,
	?INFO(battle, "Job = ~w", [Job]),
	
	ParamX = 
		case SrcStat#battle_status.is_lead == true orelse 
			TarStat#battle_status.is_lead == true of
			true  -> 0;
			false -> 1
		end,
	
	{AStar, DStar}    = {SrcStat#battle_status.star, TarStat#battle_status.star},
	{ALevel, _DLevel} = {SrcStat#battle_status.level, TarStat#battle_status.level},

	{Att0, Def0, DefTag} =
		case (Job == ?CAREER_HUWEI) orelse (Job == ?CAREER_MENGJIANG) of
			true -> 
				{SrcStat#battle_status.p_att, TarStat#battle_status.p_def, pdef};
			false ->
				{SrcStat#battle_status.m_att, TarStat#battle_status.m_def, mdef}
		end,
	
	{Att, Def} =
		begin	
            BreakRate   = max(0, SrcStat#battle_status.break / 1000 + ParamX * 0.03 * (AStar - DStar)),
            BreakRand   = random:uniform(),
            ?BATTLE_LOG("        破甲几率: ~w, 随机数: ~w, 破甲: ~w",
                        [BreakRate, BreakRand, BreakRand =< BreakRate]),
			BreakAdjust = 
				case BreakRand =< BreakRate of
					true  -> 0.7; %% break the defense
					false -> 1
				end,

            EnhancedAtt = get_adjust_value(att_enhance, Att0, Src, BattleData, BuffAdd),
			
			{get_adjust_value(att, EnhancedAtt, Src, BattleData, BuffAdd),
			 get_adjust_value(DefTag, Def0, Tar,  BattleData) * BreakAdjust}
		end,

    CompromisedDef = case get_passive_skill_helper(?PSKILL_DEF_IGNORE, SrcStat#battle_status.p_skill) of
        {true, DefIgnoreSkillInfo} ->
            {CompromiseRate} = DefIgnoreSkillInfo#battle_skill.param,
            ?INFO(battle, "CompromiseRate = ~w", [CompromiseRate]),
            round(max(1 - CompromiseRate, 0) * Def);
        false ->
            Def
    end,

	{Crit, Luck} = {
		get_adjust_value(crit, SrcStat#battle_status.crit, Src, BattleData, BuffAdd),
		get_adjust_value(luck, TarStat#battle_status.luck, Tar, BattleData)},

    LuckRate = max(0.20, min(0.94, Luck * 10 / (Luck * 12 + Crit) + ParamX * 0.03 * (AStar - DStar))),
	CritRate = 
		case lists:keysearch(?BUFF_CRIT, #buff.name, Buff ++ BuffAdd) of
			false -> 
                Rate0 = 1 - LuckRate,
                BuffAddedRate = case lists:keysearch(?BUFF_EXTRA_CRIT_RATE, #buff.name, Buff ++ BuffAdd) of
                    false ->
                        0;
                    {value, #buff{value = CritRateV}} ->
                        ?BATTLE_LOG("        站位 ~w Buff效果: 额外暴击几率", [Src]),
                        ?BATTLE_LOG("            Buff类型: 额外暴击几率, 系数: ~w", [CritRateV]),
                        CritRateV
                end,
                PSkillAddedRate = case get_passive_skill_helper(?PSKILL_CRIT_BOOST, SrcStat#battle_status.p_skill) of
                    {true, CritBoostInfo} ->
                        {CritBoostRate} = CritBoostInfo#battle_skill.param,
                        ?INFO(battle, "CritBoostRate = ~w", [CritBoostRate]),
                        CritBoostRate;
                    false ->
                        0
                end,
                min(1.0, Rate0 + BuffAddedRate + PSkillAddedRate);
			{value, #buff{value = _CritV}} ->
                ?BATTLE_LOG("        站位 ~w Buff效果: 暴击", [Src]),
                ?BATTLE_LOG("            Buff类型: 必暴击, 系数: ~w", [_CritV]),
				1.0
		end,
		
    CritRand = random:uniform(),
	IsCrit = CritRand =< CritRate,

    ?BATTLE_LOG("        暴击几率: ~w, 随机数: ~w, 暴击: ~w",
                [CritRate, CritRand, IsCrit]),
	
	Fatality = get_adjust_value(fatal, SrcStat#battle_status.fatal, Src, BattleData, BuffAdd),
	
	?INFO(battle, "Src = ~w, Job = ~w, Att = ~w, CompromisedDef = ~w, ALevel = ~w", [Src, Job, Att, CompromisedDef, ALevel]),
	
    LevelDefCoef = data_system:get(122),
    Param1 = case BattleData#battle_data.mod of
        pvp -> LevelDefCoef * ALevel;
        _   -> LevelDefCoef * ALevel
    end,
	Param2 = ParamX * 0.08,
	Param3 = (0.9 + random:uniform() / 5),

    OfficialAttEnhanceRate = (SrcStat#battle_status.official_att_enhance + 1),
    ?INFO(battle, "OfficialAttEnhanceRate = ~w", [OfficialAttEnhanceRate]),
		
    Damage0 = case BattleData#battle_data.type of
        ?BATTLE_TYPE_BOSS ->
            OfficialAttEnhanceRate * Att * (1 - CompromisedDef / (CompromisedDef + 45000)) 
                * Param3 * AttSpec#attack_spec.addition;
        _ ->
            OfficialAttEnhanceRate * Att * (1 - CompromisedDef / (CompromisedDef + Param1)) 
                * (1 + Param2 * (AStar - DStar)) * Param3 * AttSpec#attack_spec.addition
    end,

	Damage1 = 
		case IsCrit of
			true  -> round(Damage0 * (1.5 + Fatality / 1250));
			false -> round(Damage0)
		end,
	
	Damage2 = get_damage_value(Damage1, Tar, BattleData),

    Damage3 = get_adjust_value(cast_damage, Damage2, Src, BattleData, BuffAdd),
	
    AdjustedBlock = get_adjust_value(block, TarStat#battle_status.block, Tar, BattleData),
	BlockRate = 
         max(0, AdjustedBlock / 2000 + ParamX * 0.03 * (AStar - DStar)),
	
    BlockRand = random:uniform(),
	IsBlock = BlockRand =< BlockRate, 
    ?BATTLE_LOG("        格挡几率: ~w, 随机数: ~w, 格挡: ~w",
                [BlockRate, BlockRand, IsBlock]),

	if (IsBlock == true) ->
		Damage4 = round(Damage3 * 0.8);
	true ->
		Damage4 = round(Damage3)
	end,

    {NewDmgShieldBuff, AbsorbedDmg, Damage5} = handle_dmg_shield(Tar, Damage4, TarStat#battle_status.buff),
    ?INFO(battle, "Tar = ~w, AbsorbedDmg = ~w, NewDmgShieldBuff = ~w", [Tar, AbsorbedDmg, NewDmgShieldBuff]),

    ?BATTLE_LOG("        最终伤害值: ~w", [Damage5]),
	
	{IsCrit, IsBlock, {NewDmgShieldBuff, AbsorbedDmg}, Damage5}.

%% attack entry
%% (1) attack function generate a list of *attack_info*, 
%% (2) and then change the battle data according to the information in the AttInfoList
%%     including the buff and cd status;
-spec attack(SkillId, Src, AttSpec, BattleData) -> NBattleData when
	SkillId     :: integer(),
	Src         :: integer(),
	AttSpec     :: #attack_spec{},
	BattleData  :: #battle_data{},
	NBattleData :: #battle_data{}. 
															 
attack(SkillId, Src, AttSpec, BattleData) ->
	
	AttInfoList = attack(SkillId, Src, AttSpec, AttSpec#attack_spec.targets, BattleData),
	%% battle data should *not* change when processing attack/5
	BattleData1 = handle_attack_info(SkillId, Src, AttInfoList, BattleData),
	
	case is_battle_end(BattleData1) of
		{true, _} -> BattleData1;
		false ->
			IsMiss = lists:all(fun(AttInfo) -> AttInfo#attack_info.is_miss end, AttInfoList),
			%% get_buff target will do special logic to make sure
			%% the element in TarList is unique.
			TarList = get_buff_target(AttInfoList),
			%% if not miss, then buff will be add to self, the debuff will be add to enemy
			%% if miss, buff is ignored.
			do_att_buff(Src, AttSpec, IsMiss, TarList, BattleData1)
	end.

-spec attack(SkillId, Src, AttSpec, Targets, BattleData) -> AttInfoList when
	SkillId     :: integer(),
	Src         :: integer(),
    AttSpec     :: #attack_spec{} | number(),
	Targets     :: [integer()],
	BattleData  :: #battle_data{},
	AttInfoList :: [#attack_info{}].

attack(SkillId, Src, AttRate, Targets, BattleData) when is_number(AttRate) ->
    AttSpec = #attack_spec {
        addition = AttRate,
        targets  = Targets
    },
    attack(SkillId, Src, AttSpec, Targets, BattleData);

attack(SkillId, Src, AttSpec, Targets, BattleData) ->
    %% 处理204xxx技能：攻击目标不止一个时，要将已经加上的暴击值减掉，
    %% 形成打的人越多暴击越低的效果
    NAttSpec = case get_norm_skill(?NSKILL_CRIT, Src, BattleData) of
        {true, AddedAttr} ->
            N = length(Targets),
            case N > 1 of
                true ->
                    SubCrit = AddedAttr#role_update_attri.gd_baoji,
                    AddCrit = erlang:round(AddedAttr#role_update_attri.gd_baoji / N),
                    CritBuff = #buff {
                        name    = ?BUFF_CRIT_DOWN,
                        by_rate = false,
                        value    = SubCrit - AddCrit
                    },
                    AttSpec#attack_spec{
                        buff_add = [CritBuff | AttSpec#attack_spec.buff_add]
                    };
                false ->
                    AttSpec
            end;
        false ->
            AttSpec
    end,

    AdditionList = case is_number(NAttSpec#attack_spec.addition) of
        true ->
            lists:duplicate(length(Targets), NAttSpec#attack_spec.addition);
        false ->
            NAttSpec#attack_spec.addition
    end,

	attack(SkillId, Src, NAttSpec#attack_spec{addition = AdditionList}, Targets, [], BattleData).
													  
%% Travers the Target list, do some calculation, and put the result in the AttInfoList
attack(SkillId, Src, AttSpec = #attack_spec{addition = [CurAddition | RestAddition]}, 
       [Tar | Rest], AttInfoList, BattleData) ->
    ?INFO(battle, "AttSpec = ~w", [AttSpec]),

	SrcStat = get_battle_status(Src, BattleData),
	TarStat = get_battle_status(Tar, BattleData),
	SrcBuff = SrcStat#battle_status.buff,
		
	{Skip, _IsAssist, NTar} = 
		%% Tar is dead, so we must skip this attack
		if (TarStat#battle_status.is_alive == false) ->
			{true, false, Tar};
		true ->
			%% Tar is not dead, check if someone would be the new target of this player
			case lists:keysearch(?BUFF_SCORNED, #buff.name, SrcBuff) of
				{_, #buff {value = T}} -> 
					IsAlive = get_battle_status(T, #battle_status.is_alive, BattleData),
					TarRet = if (IsAlive == true) ->
						{false, true, T};
					true ->
						{false, false, Tar}
					end,
                    {_, _, _NTar0} = TarRet,
                    ?BATTLE_LOG("站位 ~w Buff效果: 嘲讽", [Src]),
                    ?BATTLE_LOG("    Buff类型: 被嘲讽, 嘲讽者站位: ~w, 新目标站位: ~w", [T, _NTar0]),
                    TarRet;
				false -> 
					{false, false, Tar}
			end
		end,
	
	if (Skip == true) ->	   
		attack(SkillId, Src, AttSpec#attack_spec{addition = RestAddition}, 
               Rest, AttInfoList, BattleData);
	true ->
        ?BATTLE_LOG("    目标站位: ~w", [NTar]),
		case pre_attack(Src, NTar, AttSpec, BattleData) of
			false ->
				%% miss target, so the other field in attack_info may be 0 (is_miss == true, is_crit = false) 
				?INFO(battle, "battle attack: "),
				AttInfo = #attack_info {pos = NTar, is_miss = true},
				attack(SkillId, Src, AttSpec#attack_spec{addition = RestAddition}, 
                       Rest, [AttInfo | AttInfoList], BattleData);

			true ->
				%% TODO: may be change the AttSpec here
				{Cr, Bl, {_NewDmgShieldBuff, AbsorbedDmg}, Dm} = do_attack(Src, NTar, AttSpec#attack_spec{addition = CurAddition}, BattleData),
				HpDrain  = get_hp_absorb(Src, NTar, AttSpec, Dm, BattleData),
				{Ma, Ms} = get_mp_absorb(Src, NTar, AttSpec, BattleData), %% MpDrain = {MpAdd, MpSub}
				Rebound  = get_rebound(Src, NTar, AttSpec, Dm, BattleData),
				{{_NewSrcDmgShieldBuff, SrcAbsorbedDmg}, Counter}  = get_counter(Src, NTar, SkillId, AttSpec, Dm, BattleData),
				
				?INFO(battle, "battle attack: "),
				
				?INFO(battle, "Skillid = ~w, Src = ~w, NTar = ~w, Crit = ~w, Damage = ~w, HpDrain = ~w, MpDrain = ~w", 
					  [SkillId, Src, NTar, Cr, Dm, HpDrain, Ma]),
				
				print_battle_status(Src, BattleData),
				print_battle_status(NTar, BattleData),

                AttInfo  = #attack_info {
                    pos        = NTar,			  
                    is_miss    = false,
                    is_crit    = Cr,       			%% is critical? boolean()
                    is_block   = Bl,                %% 是否有格挡效果？
                    hp_inc     = -Dm,      			%% hp incretement
                    mp_inc     = -Ms,               %% mp incretement
                    hp_absorb  = HpDrain,  			%% hp absorb >= 0
                    mp_absorb  = Ma,       			%% mp absorb >= 0
                    hp_counter = -Counter, 			%% counter strike
                    hp_rebound = -Rebound, 			%% rebound damage
                    dmg_absorbed = AbsorbedDmg,
                    counter_dmg_absorbed = SrcAbsorbedDmg
                },
                {NewAttInfoList0, _, _, _} = lists:foldl(fun buff_att_helper/2, 
                                                        {[AttInfo], SkillId, NTar, BattleData},
                                                        [{buff, ?BUFF_SCORN},
                                                         % XXX ?PSKILL_PROTECT的效果也在?BUFF_ASSISTED里处理……
                                                         {buff, ?BUFF_ASSISTED},
                                                         {buff, ?BUFF_DMG_ABSORB_TARGET},
                                                         {p_skill, ?PSKILL_REVIVE}]),

                %% 如果最后结算NTar会挂掉，清空它的反弹和反击伤害值……
                %% 目前应该没实际作用，因为上面get_counter函数里已经判断过NTar会不会挂了
                %% 但是如果以后上面的foldl里加入了增加伤害的东西，就一定要做下面的判断
                NewAttInfoList = case Counter =/= 0 orelse Rebound =/= 0 of
                    true ->
                        NTarStat = get_battle_status(NTar, BattleData),
                        case summarize_att_info(lists:reverse(NewAttInfoList0),
                                                {NTarStat#battle_status.hp, 
                                                 NTarStat#battle_status.mp,
                                                 NTarStat#battle_status.mp_max}) of
                            {0, _} ->
                                ?BATTLE_LOG("        站位 ~w 已挂, 取消反击和反弹效果", [NTar]),
                                DummyInfo = hd(NewAttInfoList0),
                                [DummyInfo#attack_info{hp_counter = 0, hp_rebound = 0} | tl(NewAttInfoList0)];
                            _Other ->
                                NewAttInfoList0
                        end;
                    _ ->        % false
                        NewAttInfoList0
                end,

                attack(SkillId, Src, AttSpec#attack_spec{addition = RestAddition}, 
                       Rest, lists:reverse(NewAttInfoList) ++ AttInfoList, BattleData)
		end
	end;

%% All the targets have been traversed
attack(_SkillId, _Src, _AttSpec, [], AttInfoList, _BattleData) ->
	AttInfoList.
	
%% handle_attack_info/4 calls handle_attack_info/5
handle_attack_info(SkillId, Src, AttInfoList, BattleData) ->
	handle_attack_info(SkillId, Src, [], AttInfoList, BattleData).

%% handle attack info update the battle_data according to the
%% attack_info list return by function attack/5
handle_attack_info(_SkillId, Src, AttInfoList, [], BattleData) ->
	AttPro  = get_attack_pro(BattleData),
	Status  = get_battle_status(Src, BattleData),
    ?INFO(battle, "Status#battle_status.mp = ~w", [Status#battle_status.mp]),
	OldList = AttPro#attack_pro.attack_info, 
	NAttPro = 
		AttPro#attack_pro {
			hp      = Status#battle_status.hp,				   
			mp      = Status#battle_status.mp,
			%% attack_info has the format of [[...], [...], ...]
			attack_info = [AttInfoList | OldList] 
		},
	set_attack_pro(NAttPro, BattleData);

%% for each AttInfo in the AttInfoList, we fill the hp, mp according to the 
%% damage deal to this player.

%% handle_attack_info(SkillId, Src, NAttInfoList, AttInfoList, BattleData)
handle_attack_info(SkillId, Src, AttInfoList, [AttInfo | Rest], BattleData) ->
	Tar = AttInfo#attack_info.pos,
	SrcStat = get_battle_status(Src, BattleData),
	TarStat = get_battle_status(Tar, BattleData),
	
	%% S is short for source, T is short for Target
	SDamDeal = SrcStat#battle_status.damage_deal,
	SDamSuff = SrcStat#battle_status.damage_suffer,
	TDamDeal = TarStat#battle_status.damage_deal,
	TDamSuff = TarStat#battle_status.damage_suffer,
	
	%% before handling attack info, we must check whether the src or the tar is dead
	%% because some skill may ask the src to attack the tar continuously, and the 
	%% tar may have some passive skill like rebound or counter which would hurt the src.
	case AttInfo#attack_info.is_miss    == true  orelse 
		 SrcStat#battle_status.is_alive == false orelse 
		 TarStat#battle_status.is_alive == false of

		true ->
            case SrcStat#battle_status.is_alive =:= false 
                    orelse TarStat#battle_status.is_alive =:= false of
                true ->
                    handle_attack_info(SkillId, Src, AttInfoList, Rest, BattleData);
                false ->
                    NAttInfo = 
                        AttInfo#attack_info {
                            hp = TarStat#battle_status.hp,
                            mp = TarStat#battle_status.mp
                        },

                    %% 虎卫即使miss了也要加怒气
                    AddedMP = case (Rest =:= []) andalso (SrcStat#battle_status.job =:= ?CAREER_HUWEI)
                            andalso is_integer(SrcStat#battle_status.player_id)
                            andalso (SrcStat#battle_status.is_lead) of
                        true ->
                            get_att_added_mp_by_skill(SkillId, SrcStat, [NAttInfo | AttInfoList]);
                        false ->
                            0
                    end,

                    NBattleData = case AddedMP of
                        0 -> BattleData;
                        _ -> 
                            NSrcStat = SrcStat#battle_status{mp = SrcStat#battle_status.mp + AddedMP},
                            set_battle_status(Src, NSrcStat, BattleData)
                    end,
                    handle_attack_info(SkillId, Src, [NAttInfo | AttInfoList], Rest, NBattleData)
            end;

		false ->
            case SrcStat#battle_status.is_alive =:= false 
                    orelse TarStat#battle_status.is_alive =:= false of
                true ->
                    handle_attack_info(SkillId, Src, AttInfoList, Rest, BattleData);
                false ->
                    %% calc defender's final hp, mp, stores in NAttInfo
                    NAttInfo = 
                        AttInfo#attack_info {
                            hp = max(0, TarStat#battle_status.hp + AttInfo#attack_info.hp_inc),
                            mp = max(0, min(TarStat#battle_status.mp_max, 
                                            TarStat#battle_status.mp + AttInfo#attack_info.mp_inc))
                        },
                    %% ?INFO(battle, "NattInfo = ~w", [NAttInfo]),
                    
                    HpAbsorb = AttInfo#attack_info.hp_absorb,  %% +
                    MpAbsorb = AttInfo#attack_info.mp_absorb,  %% +
                    Rebound  = AttInfo#attack_info.hp_rebound, %% -
                    Counter  = AttInfo#attack_info.hp_counter, %% -

                    DmgAbsorbed = AttInfo#attack_info.dmg_absorbed,
                    CounterDmgAbsorbed = AttInfo#attack_info.counter_dmg_absorbed,
                
                    AttOldHp = SrcStat#battle_status.hp,
                    AttOldMp = SrcStat#battle_status.mp,
                    ?INFO(battle, "Src = ~w, AttOldMp = ~w, MpAbsorb = ~w", [Src, AttOldMp, MpAbsorb]),
                    AttMaxHp = SrcStat#battle_status.hp_max,
                    AttMaxMp = SrcStat#battle_status.mp_max,

                    MpAddBySkill = %% mp add by skill 
                        case Rest =/= [] orelse Src == Tar of
                            %% Rest =/= [] is used to avoid adding mp more than once when attacking
                            true  -> 
                                case Rest =:= [] andalso (SrcStat#battle_status.job =:= ?CAREER_HUWEI)
                                        andalso is_integer(SrcStat#battle_status.player_id)
                                        andalso (SrcStat#battle_status.is_lead) of
                                    true ->
                                        get_att_added_mp_by_skill(SkillId, SrcStat, [NAttInfo | AttInfoList]);
                                    false ->
                                        0
                                end;
                            false -> 
                                get_att_added_mp_by_skill(SkillId, SrcStat, [NAttInfo | AttInfoList])
                        end,

                    ?INFO(battle, "Src = ~w, maxMp = ~w, OldMp = ~w, MpAddbyskill = ~w", 
                          [Src, AttMaxMp, AttOldMp, MpAddBySkill]),

                    AttNewHp = max(0, (min(AttMaxHp, AttOldHp + HpAbsorb + Rebound + Counter))),
                    AttNewMp = max(0, (min(AttMaxMp, AttOldMp + MpAbsorb + MpAddBySkill))),
                    ?INFO(battle, "AttNewMp = ~w", [AttNewMp]),
                    
                    DefNewHp = NAttInfo#attack_info.hp,
                    DefNewMp = NAttInfo#attack_info.mp,
                    
                    %% if player is dead (att & def)?
                    %% update this information and store in the BattleData
                    NSDamDeal = max(SDamDeal, -AttInfo#attack_info.hp_inc),
                    NTDamSuff = max(TDamSuff, -AttInfo#attack_info.hp_inc),
                    
                    NSDamSuff = max(SDamSuff, max(0, -Counter) + max(0, -Rebound)),
                    NTDamDeal = max(TDamDeal, max(0, -Counter) + max(0, -Counter)),

                    ?BATTLE_LOG("    站位 ~w, 血: ~w / ~w / ~w, 怒气: ~w / ~w / ~w",
                                [Src, SrcStat#battle_status.hp, AttNewHp, AttNewHp - SrcStat#battle_status.hp,
                                 SrcStat#battle_status.mp, AttNewMp, AttNewMp - SrcStat#battle_status.mp]),
                    ?BATTLE_LOG("    站位 ~w, 血: ~w / ~w / ~w, 怒气: ~w / ~w / ~w",
                                [Tar, TarStat#battle_status.hp, DefNewHp, DefNewHp - TarStat#battle_status.hp,
                                 TarStat#battle_status.mp, DefNewMp, DefNewMp - TarStat#battle_status.mp]),
                        
                    NSrcBuffList = update_buff_value_rel(?BUFF_DMG_SHIELD, -CounterDmgAbsorbed, 0, 
                                                         SrcStat#battle_status.buff),
                    NSrcStat = 
                        SrcStat#battle_status {
                            hp = AttNewHp, 
                            mp = AttNewMp, 
                            damage_deal = NSDamDeal,
                            damage_suffer = NSDamSuff,
                            buff = NSrcBuffList,
                            is_alive = (AttNewHp > 0)
                        },
                    
                    NTarBuffList = update_buff_value_rel(?BUFF_DMG_SHIELD, -DmgAbsorbed, 0,
                                                         TarStat#battle_status.buff),

                    % 打自己的时候，TarStat里的mp也要更新……
                    DefNewMp1 = case Tar =:= Src of
                        true  -> DefNewMp + MpAddBySkill;
                        false -> DefNewMp
                    end,

                    NTarStat = 
                        TarStat#battle_status {
                            hp = DefNewHp, 
                            mp = DefNewMp1, 
                            damage_deal = NTDamDeal,
                            damage_suffer = NTDamSuff,
                            buff = NTarBuffList,
                            is_alive = (DefNewHp > 0)
                        },
                    BattleData1 = set_battle_status(Src, NSrcStat, BattleData),
                    NBattleData = set_battle_status(Tar, NTarStat, BattleData1),
                    handle_attack_info(SkillId, Src, [NAttInfo | AttInfoList], Rest, NBattleData)
            end
	end.

%% assist skill
assist(SkillId, Src, AssSpecList, BattleData) ->
	AttInfoList = assist(SkillId, Src, AssSpecList, [], BattleData),
	?INFO(battle, "SKillID = ~w, AttInfoList = ~w", [SkillId, AttInfoList]),
	
	BattleData1 = handle_attack_info(SkillId, Src, AttInfoList, BattleData),

	case is_battle_end(BattleData1) of
		{true, _} ->
			BattleData1;
		false ->
			do_ass_buff(Src, AssSpecList, BattleData1)
	end.
	
assist(_SkillId, _Src, [], AttInfoList, _BattleData) ->
	AttInfoList;

assist(SkillID, Src, [AssSpec | Rest], AttInfoList, BattleData) ->
	Rate = AssSpec#assist_spec.rate, 
	Tar  = AssSpec#assist_spec.pos,
	Eff  = AssSpec#assist_spec.eff,

    SrcStat = get_battle_status(Src, BattleData),
    TarStat = get_battle_status(Tar, BattleData),

    case SrcStat#battle_status.is_alive =:= false 
            orelse TarStat#battle_status.is_alive =:= false of
        true ->
            assist(SkillID, Src, Rest, AttInfoList, BattleData);
        false ->
            IsMiss = random:uniform() > Rate,
            AttInfo =
                #attack_info {
                    pos     = Tar,			  
                    is_crit = false,
                    is_miss = IsMiss
                },
            
            if (IsMiss == true) ->
                    assist(SkillID, Src, Rest, [AttInfo | AttInfoList], BattleData);
               true ->
                    NAttInfo = assist_1(Src, Tar, Eff, AttInfo, BattleData),
                    assist(SkillID, Src, Rest, [NAttInfo | AttInfoList], BattleData)
            end
    end.
	
assist_1(_Src, _Tar, [], AttInfo, _BattleData) ->
	AttInfo;

assist_1(Src, Tar, [Eff | Rest], AttInfo, BattleData) ->
	{Type, Value, ByRate} = Eff,
	SrcStat  = get_battle_status(Src, BattleData),
	TarStat  = get_battle_status(Tar, BattleData),
	Buffs    = TarStat#battle_status.buff, 
	NAttInfo = 
		case Type of
			heal ->
				Inc = 
					if (ByRate == true) ->
						round(TarStat#battle_status.hp_max * Value);
					true ->
						Value
					end,
				Inc1 = 
					case lists:keysearch(?BUFF_WEAKNESS, #buff.name, Buffs) of
						{value, #buff{by_rate = true, value = V}} -> 
                            Inc10 = round(Inc * (1 - V)),
                            ?BATTLE_LOG("        站位 ~w Buff效果: 降低治疗量", [Tar]),
                            ?BATTLE_LOG("            Buff效果: 降低治疗量, 系数: ~w, 降低数量: ~w",
                                        [V, Inc10]),
                            Inc10;
						false -> Inc
					end,
                MaxDiff = TarStat#battle_status.hp_max - TarStat#battle_status.hp,
				AttInfo#attack_info {hp_inc = min(Inc1, MaxDiff)};
			mana -> 
				Inc = 
					if (ByRate == true) ->
						round(TarStat#battle_status.mp * Value);
					true ->
						Value
					end,
                MaxDiff = TarStat#battle_status.mp_max - TarStat#battle_status.mp,
				AttInfo#attack_info {mp_inc = min(Inc, MaxDiff)};
			hp_absorb -> %% absorb !!
				Inc = 
					if (ByRate == true) ->
						round(TarStat#battle_status.hp * Value);
					true ->
						Value
					end,
                MaxHPLoss = TarStat#battle_status.hp,
                MaxHPGain = SrcStat#battle_status.hp_max - SrcStat#battle_status.hp,
				AttInfo#attack_info {hp_inc = -min(Inc, MaxHPLoss), hp_absorb = min(Inc, MaxHPGain)};
			_ -> %% buff or other skills are not relative to hp, mp
				AttInfo
		end,
	assist_1(Src, Tar, Rest, NAttInfo, BattleData).

get_battle_pro(BattleData) ->
	BattlePro = hd(BattleData#battle_data.procedure),
	BattlePro.
	
set_battle_pro(Procedure, BattleData) ->
	BattleData#battle_data {
		procedure = [Procedure | tl(BattleData#battle_data.procedure)]						
	}.

get_attack_pro(BattleData) ->
	BattlePro = get_battle_pro(BattleData),
	hd(BattlePro#battle_pro.attack_pro).

add_attack_pro(AttackPro, BattleData) ->
	BattlePro = get_battle_pro(BattleData),
	NBattlePro = BattlePro#battle_pro {attack_pro = [AttackPro | BattlePro#battle_pro.attack_pro]},
	set_battle_pro(NBattlePro, BattleData).

set_attack_pro(AttackPro, BattleData) ->
	BattlePro = get_battle_pro(BattleData),
	NBattlePro = BattlePro#battle_pro {attack_pro = [AttackPro | tl(BattlePro#battle_pro.attack_pro)]},
	set_battle_pro(NBattlePro, BattleData).

add_attack_info(AttInfoList, BattleData) ->
	AttPro = get_attack_pro(BattleData),
	NAttPro = AttPro#attack_pro {attack_info = [AttInfoList | AttPro#attack_pro.attack_info]},
	set_attack_pro(NAttPro, BattleData).

add_buff_info(BuffInfoList, BattleData) ->
	AttPro = get_attack_pro(BattleData),
	OldList = AttPro#attack_pro.buff_info,
	NAttPro = AttPro#attack_pro {buff_info = OldList ++ BuffInfoList},
	set_attack_pro(NAttPro, BattleData).


%============================================================================================================
% buff and cd 
%============================================================================================================

%% settle_buff returns {BuffInfoList and BattleData}
%% BuffInfoList is the information will passed to client side
%% NBattleData  is the new BattleData after settle the buffs 
-spec settle_buff(Settle, Pos, #battle_data{}) -> {BuffInfoList, #battle_data{}} when
	Settle :: buff_settle(),
	Pos :: integer(),
	BuffInfoList :: [#buff_info{}].
																				   
settle_buff(Settle, Pos, BattleData) ->
	State = get_battle_status(Pos, BattleData),
	Buffs = State#battle_status.buff,
	%% when return from settle_buff/6, NState's buff list has already change to the new one.
	{BuffInfoList, NState} = settle_buff(Settle, Pos, Buffs, [], [], State),
    ?INFO(battle, "Pos = ~w, BuffInfoList = ~w", [Pos, BuffInfoList]),
	
	BattleData1 = set_battle_status(Pos, NState, BattleData),
	add_buff_info(BuffInfoList, BattleData1).

-spec settle_buff(Settle, Pos, BuffsBeforeIter, BuffsAfterIter, InfoList, State) ->
	{FinalInfoList, NState} when
	
	Settle          :: buff_settle(),
	Pos             :: integer(),
	BuffsBeforeIter :: [#buff{}],         %% Buffs for iteration, this initial value is 
										  %% from the BattleStatus's buff field
	BuffsAfterIter  :: [#buff{}],         %% Buffs after iteration, when iterating the buff
	                                      %% we filter the buff by settlement and the duration(when iterating, duration is
	                                      %% reduce by 1)
	InfoList        :: [#buff_info{}],    %% buffinfo list
	FinalInfoList   :: [#buff_info{}],    %% buffinfo list to return
	State           :: #battle_status{},  %% battle_status to iter
	NState          :: #battle_status{}.  %% battle_status to return

settle_buff(_Settle, _Pos, [], BuffList, BuffInfoList, State) ->
	NState = State#battle_status {buff = BuffList},
	{BuffInfoList, NState};

settle_buff(Settle, Pos, [Buff | Rest], BuffList, BuffInfoList, State) ->
	case Buff#buff.settle == Settle of
		false -> 
			settle_buff(Settle, Pos, Rest, [Buff | BuffList], BuffInfoList, State);
		true  ->
			Duration = Buff#buff.duration - 1,
			NBuff    = Buff#buff{duration = Duration},	
			BuffName = Buff#buff.name,
			ByRate   = Buff#buff.by_rate,
            InEffect = Buff#buff.in_effect,
			Value    = if (ByRate == false) ->
					       round(Buff#buff.value); 
					   true -> 
						   round(Buff#buff.value * 100) 
					   end,
			
			{BuffInfo, NState} = 
                case InEffect of
                    true ->
                        case BuffName of
                            ?BUFF_TOXIC ->
                                %% toxic lose hp per turn
                                ?INFO(battle, "handling buff toxic.."),
                                %HpMax  = State#battle_status.hp_max,
                                Hp     = State#battle_status.hp,
                                HpLost = get_hp_lose_value(Hp, Buff),
                                ?BATTLE_LOG("站位 ~w Buff效果: 中毒", [Pos]),
                                ?BATTLE_LOG("    Buff类型: 中毒, 系数: ~w, 失血量: ~w", 
                                            [Buff#buff.value, HpLost]),
                                NewHp  = max(0, Hp - HpLost),
                                BInfo  = 
                                    #buff_info {
                                        name      = Buff#buff.name,
                                        owner     = Pos,			
                                        settle    = Settle,
                                        hp        = NewHp,
                                        mp        = State#battle_status.mp,
                                        hp_inc    = NewHp - Hp,
                                        mp_inc    = 0,
                                        is_new    = false,
                                        by_rate   = ByRate,
                                        value     = Value,
                                        duration  = max(0, Duration),
                                        is_remove = (Duration =< 0)
                                    },
                                State1 = State#battle_status {hp = NewHp, is_alive = (NewHp > 0)},
                                {BInfo, State1};
                            ?BUFF_REFRESH -> 
                                HpMax = State#battle_status.hp_max,
                                Hp    = State#battle_status.hp,
                                HpAdd = get_hp_recover_value(HpMax, Buff),
                                ?BATTLE_LOG("站位 ~w Buff效果: 回血", [Pos]),
                                ?BATTLE_LOG("    Buff类型: 回血, 系数: ~w, 回血量: ~w", 
                                            [Buff#buff.value, HpAdd]),
                                NewHp = min(HpMax, Hp + HpAdd),
                                BInfo = 
                                    #buff_info {
                                        name      = Buff#buff.name,
                                        owner     = Pos,			
                                        settle    = Settle,
                                        hp        = NewHp,
                                        mp        = State#battle_status.mp,
                                        hp_inc    = NewHp - Hp,
                                        mp_inc    = 0,
                                        is_new    = false,
                                        by_rate   = ByRate,
                                        value     = Value,
                                        duration  = max(0, Duration),
                                        is_remove = (Duration =< 0)
                                    },
                                State1 = State#battle_status {hp = NewHp, is_alive = true},
                                {BInfo, State1};
                            ?BUFF_CURSED ->
                                ?BATTLE_LOG("站位 ~w Buff效果: 诅咒", [Pos]),
                                ?BATTLE_LOG("    Buff类型: 被诅咒, 剩余回合数: ~w", [Duration]),
                                Hp    = State#battle_status.hp,
                                Mp    = State#battle_status.mp,
                                {BInfo, State1} = case Duration > 0 of
                                    true ->
                                        {
                                            #buff_info {
                                                name      = Buff#buff.name,
                                                owner     = Pos,
                                                settle    = Settle,
                                                hp        = Hp,
                                                mp        = Mp,
                                                hp_inc    = 0,
                                                mp_inc    = 0,
                                                is_new    = false,
                                                by_rate   = ByRate,
                                                value     = max(0, Duration),
                                                duration  = max(0, Duration),
                                                is_remove = (Duration =< 0)
                                            },
                                            State
                                        };
                                    false ->
                                        {
                                            #buff_info {
                                                name      = Buff#buff.name,
                                                owner     = Pos,
                                                settle    = Settle,
                                                hp        = 0,
                                                mp        = Mp,
                                                hp_inc    = -Hp,
                                                mp_inc    = 0,
                                                is_new    = false,
                                                by_rate   = ByRate,
                                                value     = max(0, Duration),
                                                duration  = max(0, Duration),
                                                is_remove = (Duration =< 0)
                                            },
                                            State#battle_status {hp = 0, is_alive = false}
                                        }
                                end,
                                {BInfo, State1};
                            ?BUFF_DMG_SHIELD ->
                                BInfo = #buff_info {
                                    name      = Buff#buff.name,
                                    owner     = Pos,
                                    settle    = Settle,
                                    hp        = State#battle_status.hp,
                                    mp        = State#battle_status.mp,
                                    hp_inc    = 0,
                                    mp_inc    = 0,
                                    is_new    = false,
                                    by_rate   = ByRate,
                                    value     = Value,
                                    duration  = max(0, Duration),
                                    is_remove = (Duration =< 0 orelse Buff#buff.value =< 0) 
                                },
                                {BInfo, State};
                            _ ->
                                BInfo = #buff_info {
                                    name      = Buff#buff.name,
                                    owner     = Pos,
                                    settle    = Settle,
                                    hp        = State#battle_status.hp,
                                    mp        = State#battle_status.mp,
                                    hp_inc    = 0,
                                    mp_inc    = 0,
                                    is_new    = false,
                                    by_rate   = ByRate,
                                    value     = Value,
                                    duration  = max(0, Duration),
                                    is_remove = (Duration =< 0) 
                                },
                                {BInfo, State}
                        end;

                    false ->        % Buff已经失效了，直接移除
                        ?INFO(battle, "pending buff to delete: ~w", [Buff#buff.name]),
                        BInfo = #buff_info {
                            name      = Buff#buff.name,
                            owner     = Pos,
                            settle    = Settle,
                            hp        = State#battle_status.hp,
                            mp        = State#battle_status.mp,
                            hp_inc    = 0,
                            mp_inc    = 0,
                            is_new    = false,
                            by_rate   = ByRate,
                            value     = Value,
                            duration  = 0,
                            is_remove = true
                        },
                        {BInfo, State}
                end,
			NBuffList = if 
                BInfo#buff_info.is_remove =:= true ->
                    ?BATTLE_LOG("站位 ~w 删除Buff:", [Pos]),
                    ?BATTLE_LOG(format_buff(NBuff)),
                    BuffList; 
                true -> 
                    [NBuff | BuffList] 
            end, 
			settle_buff(Settle, Pos, Rest, NBuffList, [BuffInfo | BuffInfoList], NState)
	end.			


-spec do_att_buff (Src, AttSpec, IsMiss, TarList, BattleData) -> #battle_data{} when
	Src        :: integer(),
	AttSpec    :: #attack_spec{},
	IsMiss     :: boolean(),
	TarList    :: [integer()],    %% TarList is used in adding the debuff list
	BattleData :: #battle_data{}.

do_att_buff(Src, AttSpec, IsMiss, TarList, BattleData) -> 
	BuffSpec = 
		case IsMiss orelse BattleData#battle_data.type == ?BATTLE_TYPE_BOSS of
			false ->
				[{Src, AttSpec#attack_spec.buff}] ++ 
				[{Tar, AttSpec#attack_spec.debuff} || Tar <- TarList];
			true ->
				[]
		end,
	
	?INFO(battle, "Src = ~w, BuffSpec = ~w", [Src, BuffSpec]),
	_BattleData1 = settle_and_add_buff(Src, BuffSpec, [], BattleData).


-spec do_ass_buff (Src, AssSpecList, BattleData) -> #battle_data{} when
	Src         :: integer(),
	AssSpecList :: [#assist_spec{}],
	BattleData  :: #battle_data{}.

do_ass_buff(Src, AssSpecList, BattleData) ->
	BuffSpec = [{AssSpec#assist_spec.pos, AssSpec#assist_spec.buff} || AssSpec <- AssSpecList],
	_BattleData1 = settle_and_add_buff(Src, BuffSpec, [], BattleData).


settle_and_add_buff(Src, BuffSpec, BuffInfoList, BattleData) ->
	BattleData1 = settle_buff(post, Src, BattleData),
	_BattleData2 = do_add_buff(BuffSpec, BuffInfoList, BattleData1).
	

%% do_add_buff add the buffs, then produce a list of BuffInfo
%% then update the BattleData with this BuffInfo
-spec do_add_buff(BuffSpec, BuffInfoList, BattleData) -> #battle_data{} when
	BuffSpec     :: [{Tar :: integer(), BuffOp :: [{#buff{}, rate, buff_op()}]}],
	BuffInfoList :: [#buff_info{}],
	BattleData   :: #battle_data{}.

do_add_buff([], BuffInfoList, BattleData) ->
	add_buff_info(BuffInfoList, BattleData);

do_add_buff([{Tar, BuffOpList} | Rest], BList, BattleData) ->
	{NBList, NBattleData} = 
		update_buffs(Tar, BuffOpList, BattleData),
	do_add_buff(Rest, NBList ++ BList, NBattleData).

%% update_buffs indeed adds a list of buffs (from BuffList) to the specified player
update_buffs(Pos, BuffList, BattleData) ->
	update_buffs(Pos, BuffList, [], BattleData).

update_buffs(_Pos, [], BuffInfoList, BattleData) ->
	{BuffInfoList, BattleData};

update_buffs(Pos, [{Buff, Rate, Op} | Rest], BuffInfoList, BattleData) ->
    ?INFO(battle, "Calling update_buffs/4"),
    BuffRand = random:uniform(),
    ?BATTLE_LOG("更新Buff, 站位: ~w, 操作: ~w, 持续回合: ~w, 几率: ~w, 随机数: ~w, 操作成功: ~w",
                [Pos, Op, Buff#buff.duration, Rate, BuffRand, not (BuffRand > Rate)]),
    ?BATTLE_LOG(format_buff(Buff)),
	case BuffRand > Rate of
		true ->
			?INFO(battle, "miss??, Buff = ~w, Rate = ~w, Op = ~w", [Buff, Rate, Op]),
			update_buffs(Pos, Rest, BuffInfoList, BattleData);
		false ->
			State  = get_battle_status(Pos, BattleData),
			
			case Op of
				add ->
                    %% XXX: 坑！！策划要求世界Boss不受特殊buff的影响
                    IsBossImmune = lists:member(Buff#buff.name, [?BUFF_FAINT, ?BUFF_TOXIC, ?BUFF_CURSED]),
                    case IsBossImmune 
                            andalso BattleData#battle_data.type =:= ?BATTLE_TYPE_BOSS
                            andalso Pos =:= ?BOSS_POSITION of
                        true ->
                            ?INFO(battle, "type = ~w", [BattleData#battle_data.type]),
                            update_buffs(Pos, Rest, BuffInfoList, BattleData);

                        _ ->        % false
                            {NBuff, NBattleData} = add_buff(Buff, Pos, BattleData),
                            case NBuff of
                                none ->
                                    update_buffs(Pos, Rest, BuffInfoList, NBattleData);
                                _ ->
                                    ByRate = NBuff#buff.by_rate,
                                    Value  = if 
                                        ByRate == false -> 
                                            round(NBuff#buff.value); 
                                        true -> 
                                            round(NBuff#buff.value * 100) 
                                    end,
                                    BuffInfo = #buff_info {
                                        %% this field is always set to post, to let the client 
                                        %% show the buff after the player using his skill
                                        settle    = post,                   
                                        name      = NBuff#buff.name,
                                        owner     = Pos,
                                        hp        = State#battle_status.hp,
                                        mp        = State#battle_status.mp,	
                                        hp_inc    = 0,
                                        mp_inc    = 0,
                                        is_new    = true,
                                        by_rate   = ByRate,
                                        value     = Value,
                                        duration  = NBuff#buff.duration,
                                        is_remove = false
                                    },
                                    update_buffs(Pos, Rest, [BuffInfo | BuffInfoList], NBattleData)
                            end
                    end;

				remove ->
					case is_buff_exist(Buff, Pos, BattleData) of
						false ->
							update_buffs(Pos, Rest, BuffInfoList, BattleData);
						true ->
							BuffInfo = 
								#buff_info {
									settle    = post,
									name      = Buff#buff.name,
									owner     = Pos,
									hp        = State#battle_status.hp,
									mp        = State#battle_status.mp,
									hp_inc    = 0,
									mp_inc    = 0,
									is_new    = false,
									by_rate   = Buff#buff.by_rate,
									value     = Buff#buff.value,
									duration  = 0,
									is_remove = true 			
								},
							NBattleData = remove_buff(Buff, Pos, BattleData),
							update_buffs(Pos, Rest, [BuffInfo | BuffInfoList], NBattleData)
					end
			end
	end.	

is_buff_exist(Buff, Pos, BattleData) ->
	State = get_battle_status(Pos, BattleData),
	case lists:keysearch(Buff#buff.name, #buff.name, State#battle_status.buff) of
		{value, _} -> true;
		false -> false
	end.

is_all_players_ready(BattleData) ->
    IDList = get_online_ids(BattleData),
    F = fun(ID) ->
        PInfo = get_player_info(ID, BattleData),
        PInfo#player_info.ready =:= true
    end,
    lists:all(F, IDList).


is_command_set(BattleData) ->
	IDList = get_online_ids(BattleData),
	F = fun(ID) ->
			PInfo = get_player_info(ID, BattleData),
			Lead  = PInfo#player_info.lead,
			Bs    = get_battle_status(Lead, BattleData),
            Bs#battle_status.is_lead =/= true 
                orelse (Bs#battle_status.is_alive =:= false 
                        orelse Bs#battle_status.cmd =/= ?UNDEFINED)
		end,
	lists:all(F, IDList).

%% get_buff_target finds out which player will add buff to
get_buff_target(AttInfoList) ->
	get_buff_target(AttInfoList, []).

get_buff_target([], TarList) -> TarList;
get_buff_target([AttInfo | Rest], TarList) ->
	case AttInfo#attack_info.is_miss of
		false -> 
            case AttInfo#attack_info.assist_pos of
                -1 ->       % ignore protectors
                    get_buff_target(Rest, TarList);
                _  ->
                    get_buff_target(Rest, [AttInfo#attack_info.pos | TarList])
            end;
		_ -> get_buff_target(Rest, TarList)
	end.

add_buff(Buff, Pos, BattleData) ->
	State = get_battle_status(Pos, BattleData),
	BList = State#battle_status.buff,
    {NewBuff, NState} = case lists:keysearch(Buff#buff.name, #buff.name, BList) of
        false -> 
            {Buff, State#battle_status {buff = [Buff | BList]}};
        {value, OldBuff} ->
            NBList = case Buff#buff.add_method of
                override ->
                    ?BATTLE_LOG("Buff已存在, 覆盖: "),
                    ?BATTLE_LOG(format_buff(Buff)),
                    NBuff = Buff,
                    lists:keyreplace(Buff#buff.name, #buff.name, BList, Buff);
                overlay ->
                    ?BATTLE_LOG("Buff已存在, 叠加: "),
                    ?BATTLE_LOG(format_buff(Buff)),
                    NBuff = Buff#buff{value = Buff#buff.value + OldBuff#buff.value},
                    lists:keyreplace(NBuff#buff.name, #buff.name, BList, NBuff);
                noop ->
                    ?BATTLE_LOG("Buff已存在, 不做操作: "),
                    ?BATTLE_LOG(format_buff(Buff)),
                    NBuff = none,
                    BList
            end,
            {NBuff, State#battle_status {buff = NBList}}
    end,
    {NewBuff, set_battle_status(Pos, NState, BattleData)}.

%% 
remove_buff(Buff, Pos, BattleData) ->
	State = get_battle_status(Pos, BattleData),
	BList = State#battle_status.buff,
	NBList = lists:keydelete(Buff#buff.name, #buff.name, BList),
	NState = State#battle_status {buff = NBList},
	set_battle_status(Pos, NState, BattleData).

%% add_global_buff(Buff, Pos, BattleData) ->
%% 	Camp = 
%% 		if (Pos =< ?BATTLE_FIELD_SIZE div 2) -> 
%% 			att;
%% 		true ->
%% 			def
%% 		end,
%% 	BuffList = 
%% 		case Camp of
%% 			att -> BattleData#battle_data.att_buff;
%% 			def -> BattleData#battle_data.def_buff
%% 		end,
%% 	NBuffList = 
%% 		case lists:keysearch(Buff#buff.name, #buff.name, BuffList) of
%% 			false -> [Buff | BuffList];
%% 			true -> BuffList
%% 		end,
%% 	
%% 	BattleData.

%% CD Handler
-spec add_cd(Pos, SkillId, CD, BattleData) -> #battle_data{} when 
	Pos        :: integer(),
	SkillId    :: integer(),
	CD         :: integer(),
	BattleData :: #battle_data{}.
															   
add_cd(Pos, SkillId, CD, BattleData) ->
	if (CD == 0) ->
		BattleData;
	true ->
		SrcStat  = get_battle_status(Pos, BattleData),
		CdList   = SrcStat#battle_status.cd,
		NSrcStat = SrcStat#battle_status {cd = [{SkillId, CD} | CdList]},
		set_battle_status(Pos, NSrcStat, BattleData)
	end.

-spec update_cd(Pos, Sid, CD, BattleData) -> #battle_data{} when
	Pos :: integer(),
	Sid :: integer(), %% SkillId
	CD  :: integer(), %% CD duration
	BattleData :: #battle_data{}.

%% reduce the cd duration by 1...
%% Sid :: skillid,, CD skill's cd 
%% update_cd reduce the cd value in the cd list
update_cd(Pos, Sid, CD, BattleData) ->
	SrcStat  = get_battle_status(Pos, BattleData),
	CdList   = SrcStat#battle_status.cd,
	NCdList  = update_cd_list(CdList, Sid, CD),
	NSrcStat = SrcStat#battle_status {cd = NCdList},
	set_battle_status(Pos, NSrcStat, BattleData).
	
update_cd_list(CdList, Sid, CD) ->
	if (CD == 0) ->
		update_cd_list(CdList, []);
	true ->
		[{Sid, CD} | update_cd_list(CdList, [])]
	end.

update_cd_list([], CdList) ->
	CdList;

update_cd_list([{SkillId, Duration} | Rest], NCdList) ->
	if (Duration == 1) ->
		update_cd_list(Rest, NCdList);
	true ->
		update_cd_list(Rest, [{SkillId, Duration - 1} | NCdList])
	end.

%% reference get_buff_value
-spec update_order_list([integer()], #battle_data{}) -> [integer()].
update_order_list(List, BattleData) ->
	?INFO(battle, "List = ~w", [List]),
	
	MF = fun(P) ->
			State  = get_battle_status(P, BattleData),
			Speed  = State#battle_status.speed,
            NSpeed = get_adjust_value(speed, Speed, P, BattleData),
			{P, NSpeed}
		end,
	List1 = lists:map(MF, List),
	%% ?INFO(battle, "List1 = ~w", [List1]),
	
	SF = fun({_P1, S1}, {_P2, S2}) ->
			if (S1 =< S2) -> false; true -> true end
		 end,
	List2 = lists:sort(SF, List1),
	List3 = [P || {P, _} <- List2],
	%% ?INFO(battle, "List3 = ~w", [List3]),
	List3.

%======================================================================================================
% buff value handling
%======================================================================================================

get_hp_lose_value(MaxHp, #buff {name = ?BUFF_TOXIC, by_rate = ByRate, value = Value}) ->
	if (ByRate == true) ->
		round(MaxHp * Value);
	true ->
		Value
	end.

get_hp_recover_value(MaxHp, #buff {name = ?BUFF_REFRESH, by_rate = ByRate, value = Value}) ->
	if (ByRate == true) ->
		round(MaxHp * Value);
	true ->
		Value
	end.
			
get_damage_value(Dm, Pos, BattleData) ->
	{F1, I1} = get_buff_value(?BUFF_SCORN, Pos, BattleData),
    Damage0 = get_adjust_value(recv_damage, Dm, Pos, BattleData),
    if
        F1 =/= 0 orelse I1 =/= 0 ->
            ?BATTLE_LOG("        站位 ~w Buff效果: 嘲讽", [Pos]),
            ?BATTLE_LOG("            Buff类型: 嘲讽, 浮点系数: ~w, 整数值: ~w", [F1, I1]);
        true -> void
    end,
	round(Damage0 * (1 - F1) - I1).

%========================================================================================================
% skill transform handler
%========================================================================================================

%% -spec transform_skill(Skills) -> {ActiceSkills, PassiveSkills}.
transform_skill(Skills) ->
    transform_skill(Skills, [], [], []).

transform_skill([], ActiveSkills, PassiveSkills, NormalModeSkills) ->
	{ActiveSkills, PassiveSkills, NormalModeSkills};

transform_skill([Elem | Rest], ActiveSkills, PassiveSkills, NormalModeSkills) ->
	SkillID = 
		case Elem of
			{_, S, _} -> S; %% skill from role
			_ -> Elem       %% skill from monster
		end,
	
	SkillInfo = data_skill:skill_info(SkillID),
	Type      = SkillInfo#skill_info.type,
	Effect    = SkillInfo#skill_info.effect,
	
	if (Effect == ?SKILL_EFFECT_BATTLE) ->
		if (Type =/= ?SKILL_GIFT andalso Type =/= ?SKILL_NORMAL) ->
			transform_skill(Rest, [SkillID | ActiveSkills], PassiveSkills, NormalModeSkills);
		true ->
			transform_skill(Rest, ActiveSkills, [SkillID | PassiveSkills], NormalModeSkills)
		end;
	true ->
		transform_skill(Rest, ActiveSkills, PassiveSkills, [SkillID | NormalModeSkills])
	end.

%========================================================================================================
% protocol handler
%========================================================================================================
%% pt write 20000
get_mercenary_num(Tag, BattleData) ->
	F = fun(PlayerInfo, Num) ->
			Num + length(PlayerInfo#player_info.mer_list)
		end,
	case Tag of 
		pvp ->
			lists:foldl(F, 0, 
				BattleData#battle_data.attacker ++
				BattleData#battle_data.defender);
		pve ->
			lists:foldl(F, 0, BattleData#battle_data.attacker)
	end.

send_start_package(BattleData) ->
	BinIDList = 
		case BattleData#battle_data.mod of
			pve -> pt_20:write(20000, BattleData);
			pvp -> pt_20:write(20002, BattleData);
			_Other ->
				?ERR(battle, "battle mod = ~w", [_Other])
		end,
	
	?INFO(battle, "BinIDList = ~w", [BinIDList]),
	IDList = get_online_ids(BattleData),

    case BattleData#battle_data.type of
        ?BATTLE_TYPE_BOSS ->        % 世界boss的场景太小，广播状态会shi的
            void;
        _ ->
            lists:foreach(fun (ID) -> 
                catch scene:set_scene_state(ID, ?SCENE_STATE_BATTLE, 0) end, 
            IDList)
    end,

	send_player_package(BinIDList),
    case {BattleData#battle_data.mod, is_list(BattleData#battle_data.initial_monster_hp)} of
        {pve, true} ->
            MonHPBin = pt_20:write(20010, BattleData#battle_data.initial_monster_hp),
            send_group_package(MonHPBin, IDList);
        _ ->
            void
    end,
    ok.


%% pt 20001
send_procedure_package(BattleData) ->
    mark_20001_sent(),
	Bin = pt_20:write(20001, BattleData),
	IDList = get_online_ids(BattleData),
	send_group_package(Bin, IDList).

%% pt 20003
send_order_package(BattleData) ->
	Bin = pt_20:write(20003, BattleData),
	IDList = get_online_ids(BattleData),
	send_group_package(Bin, IDList).

%% pt 20005
send_result_package(BattleData) ->
    send_result_package(BattleData, ?BATTLE_END_NORMAL).

send_result_package(BattleData, EndState) ->
	BinIDList = pt_20:write(20005, {BattleData, EndState}),
	send_player_package(BinIDList).

send_command_confirm_package(Pos, NewSkillID, BattleData) ->
    ?INFO(battle, "sending command confirm package, Pos = ~w, NewSkillID = ~w", [Pos, NewSkillID]),
    Bin = pt_20:write(20013, {Pos, NewSkillID}),
	IDList = get_online_ids(BattleData),
	send_group_package(Bin, IDList).

%% pt 20007 auto set command
send_command_package(Pos, SkillID, BattleData) ->
    ?INFO(battle, "sending command package, Pos = ~w, SkillID = ~w", 
          [Pos, SkillID]),
	Bin = pt_20:write(20007, {Pos, SkillID}),
	IDList = get_online_ids(BattleData),
	send_group_package(Bin, IDList).

%% send cd_package will only send to one player!
send_cd_package(Pos, BattleData) ->
	Bin = pt_20:write(20006, {Pos, BattleData}),
	ID = pos_to_id(Pos, BattleData),
	send_group_package(Bin, [ID]).

send_player_package(BinIDList) ->
    F = fun({ID, Bin}) ->
            case ets:lookup(?ETS_ONLINE, ID) of
                [] -> 
                    case g_cross_slave:is_cross(ID) of
                        false ->
                            ok;
                        Record_cross_play ->
                            ?INFO(cross,"send cross packet to ~w", [ID]),
                            g_cross_slave:cross_send(Record_cross_play, ID,Bin)
                     end;
                [#ets_online {send_pid = SendPid}] ->
                    lib_send:send(SendPid, Bin)
            end
        end,
    lists:foreach(F, BinIDList).

send_plot_package(Plot, BattleData) ->
    Packet = pt_20:write(20012, Plot),
	IDList = get_online_ids(BattleData),
	send_group_package(Packet, IDList).

send_group_package(Bin, IDList) ->
    F = fun(ID) ->
            case ets:lookup(?ETS_ONLINE, ID) of
                [] -> 
                    case g_cross_slave:is_cross(ID) of
                        false ->
                            skip;
                        Record_cross_play ->
                            g_cross_slave:cross_send(Record_cross_play, ID,Bin)
                    end;
                [#ets_online {send_pid = SendPid}] ->
                    lib_send:send(SendPid, Bin)
            end
        end,
    lists:foreach(F, IDList).

%=========================================================================================================
% other helper functions
%=========================================================================================================

time_remain(BattleData) ->
	{LastTime, Timeout} = BattleData#battle_data.timeout,
	Now = now(),
	NTimeout = max(0, Timeout - trunc(timer:now_diff(Now, LastTime) / 1000)),
	{Now, NTimeout}.

get_battle_award(BattleData) ->
	MonID  = BattleData#battle_data.monster,

	if (not is_integer(MonID)) -> 
		#battle_award {};
	true ->
		MonGroup     = data_mon_group:get(MonID),
		IDList       = get_ids(att, BattleData, online),     % 只给在线的玩家发奖
		Silver 		 = get_silver_battle_award(IDList, MonGroup#mon_group.silver, MonID),
		Exp 		 = get_exp_battle_award(IDList, MonGroup#mon_group.exp, MonID), 
		DispatchList = get_items_battle_reward(IDList, MonGroup#mon_group.items, 
						MonGroup#mon_group.drop_type, MonID),
		?INFO(battle, "Items = ~w, Silver = ~w, DispatchList = ~w", 
			  [MonGroup#mon_group.items, Silver, DispatchList]),
		#battle_award {
			gold   = 0,
			silver = Silver,
			donate = 0,
			exp    = Exp,
			items  = DispatchList				   
		}
	end.

get_items_battle_reward(IDList, Items, DropType, MonId) ->
	DispatchList = get_items_dispatch(IDList, DropType, Items),
	FestDispatchList = get_items_battle_reward(IDList, MonId, []),
	merge_dispatch_list(DispatchList, FestDispatchList, IDList).

get_items_battle_reward([], _MonId, Res) -> 
	?INFO(qingming, "get_items_battle_reward ~w", [Res]),
	Res;
get_items_battle_reward([Id | Rest], MonId, Res) ->
	FestivalCheck = [{g_fest, qm_can_add_double_award, [], ?FEST_QINGMING}, 
					 {g_fest, qm_can_get_taqing_seed, [MonId], ?FEST_QINGMING},
					 {g_fest, qm_can_get_jinyinku_award, [MonId], ?FEST_QINGMING},
					 {fest_may, can_get_silver_award, [MonId], ?FEST_MAY}],
	F = fun({Module, Func, ExtraArgs, FestType}, Acc) ->
				case Module:Func([Id|ExtraArgs]) of
					true ->
                        try
                            MonFest = data_mon_fest:get_mon_fest(MonId, FestType),
                            FestDispatchList = get_items_dispatch([Id], unified, MonFest#mon_fest.items),
                            FestDispatchList ++ Acc
                        catch error : function_clause ->
                            ?INFO(fest, "No festival items MonId = ~w, FestType = ~w", [MonId, FestType]),
                            Acc
                        end;
					false ->
						Acc
				end
		end,
	NewRes = lists:foldl(F, Res, FestivalCheck),
	get_items_battle_reward(Rest, MonId, NewRes).

get_silver_battle_award(IDList, OriSilver, MonId) ->
	get_silver_battle_award(IDList, OriSilver, MonId, []).

get_silver_battle_award([], _OriSilver, _MonId, Res) -> 
	?INFO(qingming, "get_silver_battle_award ~w", [Res]),
	Res;
get_silver_battle_award([Id | Rest], OriSilver, MonId, Res) ->
	FestivalCheck = [{g_fest, qm_can_add_double_award, [], ?FEST_QINGMING}],
	F = fun({M, F, ExtraArgs, FestType}, Acc) ->
				case M:F([Id|ExtraArgs]) of
					true ->
                        try
                            MonFest = data_mon_fest:get_mon_fest(MonId, FestType),
                            MonFest#mon_fest.silver + Acc
                        catch error : function_clause ->
                            ?INFO(fest, "No festival items MonId = ~w, FestType = ~w", [MonId, FestType]),
                            Acc
                        end;
					false ->
						Acc
				end
		end,
	FinalSilver = lists:foldl(F, OriSilver, FestivalCheck),
	get_silver_battle_award(Rest, OriSilver, MonId, [{Id, FinalSilver}|Res]).

get_exp_battle_award(IDList, OriExp, MonId) ->
	get_exp_battle_award(IDList, OriExp, MonId, []).

get_exp_battle_award([], _OriExp, _MonId, Res) -> 
	?INFO(qingming, "get_exp_battle_award ~w", [Res]),
	Res;
get_exp_battle_award([Id | Rest], OriExp, MonId, Res) ->
	FestivalCheck = [{g_fest, qm_can_add_double_award, [], ?FEST_QINGMING}],
	F = fun({M, F, ExtraArgs, FestType}, Acc) ->
				case M:F([Id|ExtraArgs]) of
					true ->
                        try
                            MonFest = data_mon_fest:get_mon_fest(MonId, FestType),
                            MonFest#mon_fest.exp + Acc
                        catch error : function_clause ->
                            ?INFO(fest, "No festival items MonId = ~w, FestType = ~w", [MonId, FestType]),
                            Acc
                        end;
					false ->
						Acc
				end
		end,
	FinalExp = lists:foldl(F, OriExp, FestivalCheck),
	get_exp_battle_award(Rest, OriExp, MonId, [{Id, FinalExp}|Res]).

get_full_battle_hp_list(BattleData) ->
    get_full_battle_hp_list(1, BattleData, []).

get_full_battle_hp_list(Idx, _, AccList) when Idx > ?BATTLE_FIELD_SIZE ->
    AccList;
get_full_battle_hp_list(Idx, BattleData, AccList) ->
    case get_battle_status(Idx, BattleData) of
        ?UNDEFINED ->
            get_full_battle_hp_list(Idx + 1, BattleData, AccList);
        BS ->
            NewEntry = {{BS#battle_status.player_id, BS#battle_status.id}, BS#battle_status.hp},
            get_full_battle_hp_list(Idx + 1, BattleData, [NewEntry | AccList])
    end.

get_battle_hp_list(Winner, BattleData) ->
	case Winner of
		att -> get_battle_hp_list(1, ?BATTLE_FIELD_SIZE div 2 + 1, [], BattleData);
		def -> get_battle_hp_list(?BATTLE_FIELD_SIZE div 2 + 1, ?BATTLE_FIELD_SIZE + 1, [], BattleData);
        ?UNDEFINED ->
            lists:zip(lists:seq(1, 6), lists:duplicate(6, 0))
	end.

get_battle_hp_list(Limit, Limit, List, _BattleData) -> List;
get_battle_hp_list(Index, Limit, List, BattleData) ->
	Stat = get_battle_status(Index, BattleData),
	if (Stat == ?UNDEFINED) ->
		get_battle_hp_list(Index + 1, Limit, List, BattleData);
	true ->
		get_battle_hp_list(Index + 1, Limit, [{Index, Stat#battle_status.hp} | List], BattleData)
	end.

get_battle_statistic(ID, BattleData) ->
	MerList = get_mer_list(ID, BattleData),
	{DamDeal, DamSuff} = get_damage_data(MerList, BattleData),
	Round = BattleData#battle_data.round,

	#battle_statistic {
		round = Round, 
		max_damage_deal = DamDeal, 
		max_damage_suffer = DamSuff
	}.

get_damage_data(MerList, BattleData) ->
	get_damage_data({0, 0}, MerList, BattleData).

get_damage_data(Data, [], _) -> Data;
get_damage_data({D, S}, [H | T], BattleData) ->
	Stat = get_battle_status(H, BattleData),
	if (Stat == ?UNDEFINED) ->
		get_damage_data({D, S}, T, BattleData);
	true ->
		D1 = Stat#battle_status.damage_deal,
		S1 = Stat#battle_status.damage_suffer,
		get_damage_data({max(D, D1), S + S1}, T, BattleData)
	end.

merge_dispatch_list(Alist, Blist, IdList) ->
	merge_dispatch_list(Alist, Blist, IdList, []).

merge_dispatch_list(_Alist, _Blist, [], Res) -> Res;
merge_dispatch_list(Alist, Blist, [Id | Rest], Res) ->
	Item1 = 
	case lists:keyfind(Id, 1, Alist) of
		false -> [];
		{_Id1, AItems} -> AItems
	end,
	Item2 = 
	case lists:keyfind(Id, 1, Blist) of
		false -> [];
		{_Id2, BItems} -> BItems
	end,
	NewR = [{Id, fold_duplicate_items(Item2++Item1)}|Res],
	merge_dispatch_list(Alist, Blist, Rest, NewR).

%% get_items_dispatch(Ids, ItemInfo) -> [{player_id(), [{ItemID, Count, BindInfo}]}]
get_items_dispatch(Ids, DropType, ItemInfo) ->
	get_items_dispatch(Ids, DropType, ItemInfo, []).
	
get_items_dispatch([], _, _, DspList) -> DspList;
get_items_dispatch([Id | Rest], DropType, ItemInfo, DspList) ->
	Items = 
		case DropType of
			unified   -> get_items_dispatch_1(ItemInfo);
			exclusive -> get_items_dispatch_2(ItemInfo)
		end,
    Items1 = fold_duplicate_items(Items),
    ?INFO(battle, "Items1 = ~w", [Items1]),
	get_items_dispatch(Rest, DropType, ItemInfo, [{Id, Items1} | DspList]).

get_items_dispatch_1(ItemInfo) ->
	get_items_dispatch_1(ItemInfo, []).

get_items_dispatch_1([], Items) -> Items;
get_items_dispatch_1([{ItemID, Count, Rate} | Rest], Items) ->
	case random:uniform() =< Rate of
		true ->
			get_items_dispatch_1(Rest, [{ItemID, Count, 1} | Items]);
		false ->
			get_items_dispatch_1(Rest, Items)
	end. 

get_items_dispatch_2(ItemInfo) -> 
	get_items_dispatch_2(ItemInfo, random:uniform()).

get_items_dispatch_2([], _) -> [];
get_items_dispatch_2([{ItemID, Count, Rate} | Rest], F) ->
	case F =< Rate of
		true  -> [{ItemID, Count, 1}];
		false ->
			get_items_dispatch_2(Rest, F - Rate)
	end.

%% send battle award to client
send_battle_award(BattleData) ->
	Mod    = BattleData#battle_data.mod,
	Winner = BattleData#battle_data.winner,
	
	case is_number(BattleData#battle_data.monster) 
		andalso (Winner == att)
		andalso (Mod == pve) of
	true ->
		BinIDList = pt_20:write(20009, BattleData),
        ?INFO(battle, "BinIDList = ~w", [BinIDList]),
		send_player_package(BinIDList);
	false ->
		ok
	end.

%% send battle award to server!
send_battle_award(ID, BattleData) ->
	Award  = BattleData#battle_data.award,
	ExpList    = Award#battle_award.exp,
	{_, Exp} = lists:keyfind(ID, 1, ExpList),
	Items  = Award#battle_award.items,
	SilverList = Award#battle_award.silver,
	{_, Silver} = lists:keyfind(ID, 1, SilverList),
	
	?INFO(battle, "sending battle award, Award = ~w, Items = ~w", [Award, Items]),

	%%目前存在无限挂机或者按键精灵24小时刷怪的问题。
	%%需要加一个规则：与野外怪战斗超过800次，则无法获得经验奖励。
	case check_anti_auto_battle(ID,BattleData) of
		false->
			Desc = io_lib:format("怪物ID：~w", [BattleData#battle_data.monster]),
			MerList = get_mer_list(ID, BattleData),
			G = fun(Pos) ->
					Stat = get_battle_status(Pos, BattleData),
					mod_role:add_exp(ID, {Stat#battle_status.id, Exp}, ?FROM_BATTLE, true, Desc)
			end,	
			%% add exp
			lists:foreach(G, MerList),
			%% add item
			IDItems = 
				case lists:keysearch(ID, 1, Items) of
					{value, {ID, Value}} -> Value;
					false -> []
				end,
			case length(IDItems) == 0 of
		        false -> mod_team:update_item(ID, IDItems);
		        true ->skip
		    end,
			?INFO(battle,"createItems, PlayerID = ~w, Itemlist = ~w", [IDItems, ID]),
			
		    case BattleData#battle_data.type of
		        ?BATTLE_TYPE_DEFENCE ->
		            catch mod_economy:add_silver(ID, Silver, ?FROM_DEFENCE),
		            catch mod_items:createItems(ID, IDItems, ?FROM_DEFENCE);
				?BATTLE_TYPE_STAGE ->
					GenAward = 
					#gen_award{
						economy = #economy{ gd_silver = Silver },
						items = IDItems,
						log_type = ?FROM_STAGE
					},
					Mail = 
					#mail{
						mail_title = "挑战魂将奖励",
						mail_content = "恭喜你在挑战魂将中获得胜利，这是你的战利品，请查收附件。",
						from = ?FROM_STAGE
					},
					g_award:send_mix_award(ID, GenAward, Mail, ?SYSTEM_AWARD_STAGE);
                ?BATTLE_TYPE_DUNGEON ->
                    catch mod_economy:add_silver(ID, Silver, ?FROM_DUNGEON, true, Desc),
                    catch mod_items:createItems(ID, IDItems, ?FROM_DUNGEON),
                    catch mod_dungeon:add_exp(ID, Exp),
                    catch mod_dungeon:add_silver(ID, Silver);
                ?BATTLE_TYPE_MARS_TOWER ->
                    catch mod_economy:add_silver(ID, Silver, ?FROM_MARSTOWER, true, Desc),
                    catch mod_items:createItems(ID, IDItems, ?FROM_MARSTOWER);
		        _ ->
		            catch mod_economy:add_silver(ID, Silver, ?FROM_BATTLE, true, Desc),
		            catch mod_items:createItems(ID, IDItems, ?FROM_BATTLE)
		    end;
		true ->
			mod_err:send_err(ID,?ERR_BATTLE_TOO_MUCH_BATTLE_SINGLE_DAY)
	end.
%==============================================================================================================
% Debug function
%==============================================================================================================

print_battle_status(Pos, BattleData) ->
	State = get_battle_status(Pos, BattleData),
	Hp = State#battle_status.hp,
	Mp = State#battle_status.mp,
	Cd = State#battle_status.cd,
	Bf = State#battle_status.buff,
	
	?INFO(battle, "State: Pos = ~w, Hp = ~w, Mp = ~w, Cd = ~w, Buff = ~w", [Pos, Hp, Mp, Cd, Bf]).

test_pve(ID, MonID) ->
	Start = 
		#battle_start {
			mod     = pve,
			type    = ?BATTLE_TYPE_CROSS_PVE,
			att_id  = ID,
			att_mer = [],
			monster = MonID,
            off_line_list = [],
            player_mp = [],
            callback = {battle, test_callback, []}
		},
	battle:start(Start).

test_team_pve(ID, MonID) ->
	Start = 
		#battle_start {
			mod     = pve,
			type    = 0,
			att_id  = ID,
			att_mer = [],
			monster = MonID,
            maketeam = true,
            off_line_list = [],
            callback = {battle, test_callback, []}
		},
	battle:start(Start).

test_callback(ID, Result, Args) ->
    ?INFO(battle, "test_callback called, ID = ~w, Result = ~w, Args = ~w",
          [ID, Result, Args]).

test_pve_plot(ID, MonID) ->
    RoleList = lists:map(
        fun({Pos, RID}) ->
            R = data_role:get(RID),
            R#role{
                key = {ID, RID},
                gd_maxHp = 20000,
                gd_currentHp = 20000,
                p_att = 100,
                m_att = 100,
                p_def = 2000,
                m_def = 2000,
                gd_isBattle = Pos
            }
        end,
        [{1, 22}, {2, 23}]),
    MonList = [{7, data_mon_attr:get(911)}, {8, data_mon_attr:get(910)}],

	Start = 
		#battle_start {
			mod     = pve,
			type    = 0,
			att_id  = ID,
			att_mer = [],
			monster = MonID,
            plot    = [
                #battle_plot{
                    trigger = {?BATTLE_PLOT_TRIGGER_ROUNDS, 2},
                    plots   = [1, 2],
                    new_roles = RoleList ++ MonList
                }]
		},
	battle:start(Start).

test_pvp(ID1, ID2) ->
	{NID2, MerList} = 
		case mod_player:is_online(ID2) of
			{true, _} -> {ID2, []};
			_ -> 
				{?UNDEFINED, mod_role:get_on_battle_list(ID2)} 
		end,
	
	Start = 
		#battle_start {
			mod       = pvp,
		 	type      = 0,     		%% 
			att_id    = ID1,   		%% Attacker's ID
			att_mer   = [],    		%% Attacker's mercenary list
			def_id    = NID2,  		%% Defender's ID
			def_mer   = MerList,    %% Defender's Mercenary list
			maketeam  = false, 		%% true | false
			checklist = [],    		%% [check_specp()]
			caller    = [],    		%% caller module's name or pid
			callback  = []     		%% term()
		},
	battle:start(Start).

end_round_hook(BattleData) ->
    %% Everything that should be done after a round is finished
    ?BATTLE_LOG("~n--------- 回合 ~w 结束状态 ---------", [BattleData#battle_data.round]),
    dump_brief_player_list(BattleData),
    BattleData.

start_round_hook(BattleData) ->
    %% Everything that should be done before a new round starts
    ?BATTLE_LOG("~n================== 回合数: ~w ==================", [BattleData#battle_data.round]),
    mutate_attack(BattleData).

mutate_attack(BattleData) ->
    Round = BattleData#battle_data.round,
    case lists:member(Round, [11, 16, 21, 26]) of
        true ->
            ?BATTLE_LOG("--------- 越战越勇效果生效 ---------"),
            LastRate = 1 + ((Round - 11) div 5) * 0.25,
            NewRate = LastRate + 0.25,
            AttEnhanceBuff = #buff {
                name     = ?BUFF_ATT_ENHANCE,
                value    = NewRate - 1,
                by_rate  = true,
                duration = 5,
                settle   = post
            },
            NBattleData = lists:foldl(
                fun(Pos, BD) ->
                    case get_battle_status(Pos, BD) of
                        ?UNDEFINED -> 
                            BD;
                        #battle_status{is_alive = true} ->
                            {_, NBD} = add_buff(AttEnhanceBuff, Pos, BD),
                            NBD;
                        _ ->    % 已经挂掉的
                            BD
                    end
                end,
                BattleData,
                lists:seq(1, ?BATTLE_FIELD_SIZE)),

            NewGBuff = #buff{
                name = ?G_BUFF_ATT_ENHANCE,
                value = erlang:round(NewRate * 100)
            },
            Packet = pt_20:write(20011, [NewGBuff]),
            send_group_package(Packet, get_online_ids(NBattleData)),
            NBattleData;

        false ->
            BattleData
    end.

get_n_pos_by(Type, MaxFlag, N, PosList, BattleData) ->
    {_, TarList} = lists:foldl(
        fun(_, {CList, AccList}) ->
            case CList of
                [] -> {CList, AccList};
                _  ->
                    {NP, _} = battle:get_pos_by(Type, MaxFlag, CList, BattleData),
                    NCList = lists:delete(NP, CList),
                    {NCList, [NP | AccList]}
            end
        end,
        {PosList, []},
        lists:seq(1, N)),
    TarList.

get_pos_by(Type, MaxFlag, PosList, BattleData) ->
    {FieldIdx, TotalFieldIdx} = case Type of
        hp     -> {#battle_status.hp, 0};
        hp_rel -> {#battle_status.hp, #battle_status.hp_max};   % TotalFieldIdx非零代表按百分比算
        mp     -> {#battle_status.mp, 0};                       % TotalFieldIdx是零代表按整数值算
        p_def  -> {#battle_status.p_def, 0};
        m_def  -> {#battle_status.m_def, 0}
    end,
    Op = case MaxFlag of
        max -> '>';
        min -> '<'
    end,
    First = hd(PosList),
    FirstStat = get_battle_status(First, BattleData),
    FirstVal = case TotalFieldIdx of
        0 -> element(FieldIdx, FirstStat);
        _ -> element(FieldIdx, FirstStat) / element(TotalFieldIdx, FirstStat)
    end,
    get_pos_by(PosList, BattleData, {FieldIdx, TotalFieldIdx}, 
               Op, {First, FirstVal}).

get_pos_by([], _, _, _, {_CurMinPos, _CurMinVal} = Ret) ->
    Ret;
get_pos_by([H | T], BattleData, {FieldIdx, TotalFieldIdx}, Op, {CurPos, CurVal}) ->
    Stat = get_battle_status(H, BattleData),
    Val = case TotalFieldIdx of
        0 -> element(FieldIdx, Stat);
        _ -> element(FieldIdx, Stat) / element(TotalFieldIdx, Stat)
    end,
    case erlang:Op(Val, CurVal) of
        true ->
            get_pos_by(T, BattleData, {FieldIdx, TotalFieldIdx}, Op, {H, Val});
        _ ->    % false
            get_pos_by(T, BattleData, {FieldIdx, TotalFieldIdx}, Op, {CurPos, CurVal})
    end.

row_filter(Pos, {Front, Middle, Rear}) ->
    CBoundary = ?BATTLE_FIELD_SIZE div 2,
    if
        (Pos > 0 andalso Pos =< ?BATTLE_ROW_SIZE) 
                orelse (Pos > CBoundary andalso Pos =< (CBoundary + ?BATTLE_ROW_SIZE)) ->
            {[Pos | Front], Middle, Rear};
        (Pos > ?BATTLE_ROW_SIZE andalso Pos =< (?BATTLE_ROW_SIZE * 2))
                orelse (Pos > (CBoundary + ?BATTLE_ROW_SIZE) andalso Pos =< (CBoundary + ?BATTLE_ROW_SIZE * 2)) ->
            {Front, [Pos | Middle], Rear};
        (Pos > (?BATTLE_ROW_SIZE * 2) andalso Pos =< (?BATTLE_ROW_SIZE * 3))
                orelse (Pos > (CBoundary + ?BATTLE_ROW_SIZE * 2) andalso Pos =< (CBoundary + ?BATTLE_ROW_SIZE * 3)) ->
            {Front, Middle, [Pos | Rear]};
        true ->
            ?ERR(battle, "Oops, WTF is this battle field Pos = ~w", [Pos]),
            {Front, Middle, Rear}
    end.

get_first_row(PosList, N, _BattleData) ->
    {Front, Middle, Rear} = lists:foldl(fun row_filter/2, {[], [], []}, PosList),
    if
        Front =/= [] ->
            util:get_rand_list_elems(Front, N);
        Middle =/= [] ->
            util:get_rand_list_elems(Middle, N);
        Rear =/= [] ->
            util:get_rand_list_elems(Rear, N);
        true ->
            []
    end.

get_last_row(PosList, N, _BattleData) ->
    {Front, Middle, Rear} = lists:foldl(fun row_filter/2, {[], [], []}, PosList),
    if
        Rear =/= [] ->
            util:get_rand_list_elems(Rear, N);
        Middle =/= [] ->
            util:get_rand_list_elems(Middle, N);
        Front =/= [] ->
            util:get_rand_list_elems(Front, N);
        true ->
            []
    end.

get_player_pos(PosList, N, BattleData) ->
    NewPosList = get_player_pos(PosList, N, BattleData, []),
    util:get_rand_list_elems(NewPosList, N).

get_player_pos([H | T], N, BattleData, AccList) ->
    S = get_battle_status(H, BattleData),
    case is_integer(S#battle_status.player_id) of
        true ->
            case S#battle_status.is_lead of
                true ->
                    get_player_pos(T, N, BattleData, [H | AccList]);
                false ->
                    get_player_pos(T, N, BattleData, AccList)
            end;
        false ->
            get_player_pos(T, N, BattleData, AccList)
    end;
get_player_pos([], _N, _BattleData, AccList) ->
    AccList.

get_hit_list(AttackInfoList, BattleData) ->
    get_hit_list(AttackInfoList, [], BattleData).

get_hit_list([AttackInfo | Rest], HitList, BattleData) ->
    case AttackInfo#attack_info.is_miss of
        true  -> 
            get_hit_list(Rest, HitList, BattleData);
        false -> 
            HitPos = AttackInfo#attack_info.pos,
            HitStatus = get_battle_status(HitPos, BattleData),
            case HitStatus#battle_status.is_alive of
                true ->
                    get_hit_list(Rest, [HitPos | HitList], BattleData);
                false ->
                    get_hit_list(Rest, HitList, BattleData)
            end
    end;
get_hit_list([], HitList, _BattleData) ->
    lists:usort(HitList).

transform_plot(ID, PlayerLevel, Plot) ->
    F = fun(E) ->
        case E of
            {Pos, MonAttr} when is_record(MonAttr, mon_attr) ->
                BS = mon_2_bs(MonAttr, Pos, ?HP_MAX),
                {Pos, BS};
            Role when is_record(Role, role) ->
                Pos = Role#role.gd_isBattle,
                BS = role_2_bs(ID, PlayerLevel, Role, 0),
                {Pos, BS};
            {Pos, BS} when is_record(BS, battle_status) ->
                {Pos, BS}
        end
    end,
    Plot#battle_plot{
        new_roles = [F(R) || R <- Plot#battle_plot.new_roles]
    }.

check_plot_trigger(BattleData) ->
    check_plot_trigger(BattleData#battle_data.plot, BattleData, []).

check_plot_trigger([], _BattleData, _NewPlots) ->
    false;
check_plot_trigger([P | Rest], BattleData, NewPlots) ->
    case check_single_plot_trigger(P, BattleData) of
        true ->
            {true, P, 
             BattleData#battle_data{
                plot = lists:reverse(NewPlots) ++ Rest
             }};
        false ->
            check_plot_trigger(Rest, BattleData, [P | NewPlots])
    end.

check_single_plot_trigger(#battle_plot{trigger = {?BATTLE_PLOT_TRIGGER_ROUNDS, Round}}, BattleData) ->
    case BattleData#battle_data.round >= Round of
        true -> true;
        _    -> false
    end;
check_single_plot_trigger(#battle_plot{trigger = {?BATTLE_PLOT_TRIGGER_DEATH, Pos}}, BattleData) ->
    Stat = get_battle_status(Pos, BattleData),
    case Stat#battle_status.is_alive of
        false -> true;
        true  -> false
    end.

get_empty_pos(Cur, Max, _BattleData) when Cur >= Max ->
    0;
get_empty_pos(Cur, Max, BattleData) ->
    case get_battle_status(Cur, BattleData) of
        ?UNDEFINED -> Cur;
        _ -> get_empty_pos(Cur + 1, Max, BattleData)
    end.

trigger_plot(Plot, BattleData) ->
    ?INFO(battle_plot, "Plot triggered: ~w", [Plot]),
    {BattleData1, NewPlot} = lists:foldl(
        fun({Pos, BS}, {BD, NP}) ->
            NPos = case Pos =< (?BATTLE_FIELD_SIZE div 2) of
                true ->     % 玩家加的佣兵
                    get_empty_pos(1, ?BATTLE_FIELD_SIZE div 2, BD);
                false ->    % 敌人加的佣兵
                    get_empty_pos((?BATTLE_FIELD_SIZE div 2) + 1, ?BATTLE_FIELD_SIZE, BD)
            end,
            case NPos of
                0 -> 
                    exit(no_pos_for_plot_role);     % never returns
                _ -> 
                    ?INFO(battle_plot, "Adding new role in pos ~w", [NPos]),
                    ?BATTLE_LOG("~n--------- 触发剧情, 加人 ---------"),
                    dump_single_player(NPos, BS),
                    {set_battle_status(NPos, BS, BD),
                     NP#battle_plot{new_roles = [{NPos, BS} | NP#battle_plot.new_roles]}}
            end
        end,
        {BattleData, Plot#battle_plot{new_roles = []}},
        Plot#battle_plot.new_roles),
    send_plot_package(NewPlot, BattleData1),
    NewPosList = [P || {P, _} <- NewPlot#battle_plot.new_roles],
    BattleData1#battle_data{attorder = BattleData1#battle_data.attorder ++ update_order_list(NewPosList, BattleData1)}.

get_alive_num(BattleData) ->
    L = get_target_list(calc_range(0, ?ALL), BattleData),
    length(L).

get_camp_alive_num(Pos, BattleData) ->
    L = get_target_list(calc_range(Pos, ?ALLFRIENDLY), BattleData),
    length(L).

fold_duplicate_items(Items) ->
    fold_duplicate_items(Items, dict:new()).

fold_duplicate_items([], AccItems) ->
    [{ID, Count, Bound} || {{ID, Bound}, Count} <- dict:to_list(AccItems)];
fold_duplicate_items([{ItemID, Count, Bound} | Rest], AccItems) ->
    NDict = case dict:find({ItemID, Bound}, AccItems) of
        error -> 
            dict:store({ItemID, Bound}, Count, AccItems);
        {ok, OldCount} ->
            dict:store({ItemID, Bound}, OldCount + Count, AccItems)
    end,
    fold_duplicate_items(Rest, NDict).

buff_att_helper({buff, ?BUFF_SCORN}, {AttInfoList, SkillID, Tar, BattleData}) ->
    TarBuffList = (get_battle_status(Tar, BattleData))#battle_status.buff,
    AttInfo = hd(AttInfoList),

    AddedMP = case lists:keyfind(?BUFF_SCORN, #buff.name, TarBuffList) of
        false ->
            DataSkill = data_skill_table:get(SkillID, 1),   % XXX: 总是用 Lv.1 的配置……
            DataSkill#battle_skill.hit_mp_add;
        #buff{value = _ScornV} ->
            ?BATTLE_LOG("        站位 ~w Buff效果: 嘲讽", [Tar]),
            ?BATTLE_LOG("            Buff类型: 嘲讽, 怒气增加: ~w", [0]),
            0
    end,
    
    NAttInfo = AttInfo#attack_info {
        mp_inc = AttInfo#attack_info.mp_inc + AddedMP
    },
    {[NAttInfo | tl(AttInfoList)], SkillID, Tar, BattleData};

buff_att_helper({buff, ?BUFF_ASSISTED}, {AttInfoList, SkillID, Tar, BattleData} = Orig) ->
    TarStat = get_battle_status(Tar, BattleData),
    TarBuffList = TarStat#battle_status.buff,
    AttInfo = hd(AttInfoList),

    case lists:keyfind(?BUFF_ASSISTED, #buff.name, TarBuffList) of
        false ->
            %% XXX: ?PSKILL_PROTECT 的效果也在这里处理
            case get_protector(TarStat#battle_status.protectors, BattleData) of
                none -> Orig;
                {Protector, {_PrRate, PrCoef}} -> 
                    NAttInfo = AttInfo#attack_info{
                        hp_inc = round(AttInfo#attack_info.hp_inc * (1 - PrCoef)),
                        assist_pos = Protector
                    },
                    AttInfo2 = #attack_info {
                        pos        = Protector,
                        is_miss    = false,
                        is_crit    = false,
                        hp_inc     = round(AttInfo#attack_info.hp_inc * PrCoef),
                        assist_pos = -1     % 客户端用这个字段来判断当前目标是不是援护者，-1就是援护者
                    },
                    {[NAttInfo | tl(AttInfoList)] ++ [AttInfo2], SkillID, Tar, BattleData}
            end;
                    
        #buff{data = Pos, by_rate = true, value = V} ->
            AssStat = get_battle_status(Pos, BattleData),
            if 
                (AssStat#battle_status.is_alive == true) ->
                    ?BATTLE_LOG("        站位 ~w Buff效果: 援护", [Tar]),
                    ?BATTLE_LOG("            Buff类型: 被援护, 伤害减少系数: ~w, 援护者站位: ~w", [V, Pos]),

                    NAttInfo = AttInfo#attack_info{
                        hp_inc = round(AttInfo#attack_info.hp_inc * (1 - V)),
                        assist_pos = Pos
                    },
                    AttInfo2 = #attack_info {
                        pos        = Pos,
                        is_miss    = false,
                        is_crit    = false,
                        hp_inc     = round(AttInfo#attack_info.hp_inc * V),
                        assist_pos = -1     % 客户端用这个字段来判断当前目标是不是援护者，-1就是援护者
                    },
                    {[NAttInfo | tl(AttInfoList)] ++ [AttInfo2], SkillID, Tar, BattleData};

                true ->
                    Orig
            end
    end;

buff_att_helper({buff, ?BUFF_DMG_ABSORB_TARGET}, {AttInfoList, SkillID, Tar, BattleData} = Orig) ->
    TarBuffList = (get_battle_status(Tar, BattleData))#battle_status.buff,
    AttInfo = hd(AttInfoList),

    case AttInfo#attack_info.hp_inc < 0 of
        true ->
            case lists:keyfind(?BUFF_DMG_ABSORB_TARGET, #buff.name, TarBuffList) of
                false ->
                    Orig;
                #buff{data = Pos, by_rate = true, value = V} ->
                    AbsTarStat = get_battle_status(Pos, BattleData),
                    case AbsTarStat#battle_status.is_alive of
                        true ->
                            ?BATTLE_LOG("        站位 ~w Buff效果: 伤害吸收", [Tar]),
                            ?BATTLE_LOG("            Buff类型: 伤害被吸收, 系数: ~w, 吸收者站位: ~w", [V, Pos]),
                            AttInfo2 = #attack_info {
                                pos        = Pos,
                                is_miss    = false,
                                is_crit    = false,
                                hp_inc     = -round(AttInfo#attack_info.hp_inc * V)
                            },
                            {AttInfoList ++ [AttInfo2], SkillID, Tar, BattleData};
                        false ->
                            Orig
                    end
            end;

        _ ->        % false
            Orig
    end;

buff_att_helper({p_skill, ?PSKILL_REVIVE}, {AttInfoList, SkillID, Tar, BattleData} = Orig) ->
    TarStat = get_battle_status(Tar, BattleData),
    case get_passive_skill_helper(?PSKILL_REVIVE, TarStat#battle_status.p_skill) of
        {true, RVSkillInfo} ->
            AttInfo = hd(AttInfoList),      % 这个位置总是对Tar的伤害，（目前）有且只有一次
            NewRawHP = AttInfo#attack_info.hp_inc + TarStat#battle_status.hp,
            case NewRawHP =< 0 of
                true ->
                    {RVRate, RVCoef} = RVSkillInfo#battle_skill.param,
                    RVRand = random:uniform(),
                    ?BATTLE_LOG("        站位 ~w 被动技能 ~w, 几率: ~w, 随机数: ~w, 生效: ~w",
                                [Tar, skill:get_skill_uid(?PSKILL_REVIVE, 
                                                          RVSkillInfo#battle_skill.level),
                                 RVRate, RVRand, RVRand =< RVRate]),
                    case RVRand =< RVRate of
                        true ->
                            RVHP = round(TarStat#battle_status.hp_max * RVCoef),
                            NAttInfo = AttInfo#attack_info {
                                hp_inc = -TarStat#battle_status.hp
                            },
                            NAttInfo1 = #attack_info {
                                pos     = Tar,
                                is_miss = false,
                                is_crit = false,
                                hp_inc  = RVHP
                            },
                            ?BATTLE_LOG("        站位 ~w 被动技能 ~w 生效", 
                                        [Tar, skill:get_skill_uid(?PSKILL_REVIVE, 
                                                                  RVSkillInfo#battle_skill.level)]),
                            ?BATTLE_LOG("            新伤害: ~w, 加血: ~w", 
                                        [TarStat#battle_status.hp, RVHP]),
                            {[NAttInfo, NAttInfo1 | tl(AttInfoList)] , SkillID, Tar, BattleData};
                        _ ->        % false
                            Orig
                    end;
                
                _ ->    % false
                    Orig
            end;

        false ->
            Orig
    end.


get_protector([{Pos, {Rate, _Coef} = Params} | Rest], BattleData) ->
    S = get_battle_status(Pos, BattleData),
    case S#battle_status.is_alive of
        true  -> 
            PrRand = random:uniform(),
            ?BATTLE_LOG("        站位 ~w 被动技能 ~wxxx, 几率: ~w, 随机数: ~w, 生效: ~w",
                        [Pos, ?PSKILL_PROTECT, Rate, PrRand, PrRand =< Rate]),
            case PrRand =< Rate of
                true -> 
                    ?BATTLE_LOG("        站位 ~w 被动技能 ~wxxx 生效:",
                                [Pos, ?PSKILL_PROTECT]),
                    ?BATTLE_LOG("            分担伤害系数: ~w", [_Coef]),
                    {Pos, Params};
                _ -> 
                    get_protector(Rest, BattleData)
            end;
        false -> 
            get_protector(Rest, BattleData)
    end;
get_protector([], _BattleData) ->
    none.

summarize_att_info([A | Rest], {CurHP, CurMP, MaxMP}) ->
    NewHP = max(0, CurHP + A#attack_info.hp_inc),
    NewMP = max(0, min(MaxMP, CurMP + A#attack_info.mp_inc)),
    summarize_att_info(Rest, {NewHP, NewMP, MaxMP});
summarize_att_info([], {CurHP, CurMP, _MaxMP}) ->
    {CurHP, CurMP}.

check_anti_auto_battle(Id,Battle_data)->
	?INFO(battle,"battle data is ~w",[Battle_data]),
	case Battle_data#battle_data.caller of
		monster ->
			?INFO(battle,"wild monster flight, check whether exceed each day max allow"),
			mod_counter:add_counter(Id,?COUNTER_ANTI_TOO_MANY_BATTLE),
			Counter = mod_counter:get_counter(Id,?COUNTER_ANTI_TOO_MANY_BATTLE),
			%%判断次数是否超过
			case Counter > data_system:get(36) of
				true->
					?INFO(battle,"battle more than 800 times, no award"),
					true;
				false->
					?INFO(battle,"battle less than 800 times, no award"),
					false
			end;
		_->
			?INFO(battle,"caller is ~w",[Battle_data#battle_data.caller]),
			false
	end.

notify_team_complete(Camp, BattleData) ->
    InfoList = case Camp of
        att -> BattleData#battle_data.attacker;
        def -> BattleData#battle_data.defender
    end,
    AllQuit = lists:all(
        fun(P) ->
            P#player_info.finish_play =:= quit 
                orelse P#player_info.online =:= false
        end,
        InfoList),
    case AllQuit of
        true ->
            ?INFO(battle_end, "AllQuit = true"),
            NotifyF = fun(P) ->
                case P#player_info.online of
                    true ->
                        ID = P#player_info.id,
                        case (catch notify_complete(ID, BattleData)) of
                            {'EXIT', Error} ->
                                ?INFO(battle, "notify_complete failed, Error = ~w", [Error]);
                            _ ->
                                void
                        end;
                    false ->
                        void
                end
            end,
            lists:foreach(NotifyF, InfoList);
        false ->
            ok
    end.

check_and_set_cd(CDName, PlayerID, CDTime) ->
    Now = util:longunixtime(),
    LastTime = case erlang:get({CDName, PlayerID}) of
        undefined -> 0;
        T -> T
    end,
    case Now - LastTime >= CDTime of
        true ->
            erlang:put({CDName, PlayerID}, Now),
            true;
        _ ->        % false
            false
    end.

incr_timeout_times() ->
    case erlang:get(battle_timeout_times) of
        undefined ->
            erlang:put(battle_timeout_times, 1),
            0;
        Times ->
            erlang:put(battle_timeout_times, Times + 1)
    end.

save_battle_skill_order(ID, OrderList) ->
    UpdateList = [{#battle_skill_order.order_list, OrderList}],
    case gen_cache:update_element(?CACHE_BATTLE_SKILL_ORDER_REF, ID, UpdateList) of
        false ->
            NewOrderRec = #battle_skill_order{
                gd_accountID = ID,
                order_list = OrderList
            },
            gen_cache:insert(?CACHE_BATTLE_SKILL_ORDER_REF, NewOrderRec);
        ok ->
            skip
    end,
    ok.

get_battle_skill_order(ID, RoleSkillList) ->
    %% XXX: 之前保存的自动战斗顺序设置可能会导致玩家技能顺序乱掉，
    %%      暂时全用默认顺序

    %case gen_cache:lookup(?CACHE_BATTLE_SKILL_ORDER_REF, ID) of
    %    [] ->
            F = fun(UID1, UID2) ->
                {SID1, Lv1} = skill:get_skill_id_level(UID1),
                Info1 = data_skill_table:get(SID1, Lv1),

                {SID2, Lv2} = skill:get_skill_id_level(UID2),
                Info2 = data_skill_table:get(SID2, Lv2),

                Info1#battle_skill.slot =< Info2#battle_skill.slot
            end,
            SortedSkillList = lists:sort(F, RoleSkillList),

    %        NewOrderRec = #battle_skill_order{
    %            gd_accountID = ID,
    %            order_list = SortedSkillList
    %        },
    %        gen_cache:insert(?CACHE_BATTLE_SKILL_ORDER_REF, NewOrderRec),
            SortedSkillList.%;

    %    [OldOrderRec | _] ->
    %        OldList0 = OldOrderRec#battle_skill_order.order_list,
    %        OldList = [S || S <- OldList0, lists:member(S, RoleSkillList)],
    %        case lists:sort(RoleSkillList) =/= lists:sort(OldList) of
    %            true ->     % 获得新技能或者有其他变动之后会跑到这里……
    %                OldLen = length(OldList),
    %                CurLen = length(RoleSkillList),
    %                OldList1 = lists:ukeysort(1, lists:zip(OldList, lists:seq(1, OldLen))),
    %                RoleSkillList1 = lists:ukeysort(1, lists:zip(RoleSkillList, lists:duplicate(CurLen, 9999))),
    %                MergedList = lists:keysort(2, lists:ukeymerge(1, OldList1, RoleSkillList1)),
    %                {NewList, _} = lists:unzip(MergedList),
    %                UpdateList = [{#battle_skill_order.order_list, NewList}],
    %                gen_cache:update_element(?CACHE_BATTLE_SKILL_ORDER_REF, ID, UpdateList),
    %                NewList;
    %            false ->
    %                OldList
    %        end
    %end.

notify_complete_direct(ID, BattleData) ->
    ?INFO(battle, "Calling notify_complete_direct for player ~w", [ID]),

	Callback = BattleData#battle_data.callback,
	Type     = BattleData#battle_data.type,
	Winner   = BattleData#battle_data.winner,
	
	{true, Camp} = check_player(ID, BattleData),
	IsWin = (Winner == Camp),
	
	Statistic = get_battle_statistic(ID, BattleData),
	%% ?INFO(battle, "statistic = ~w", [Statistic]),
	
	case is_number(BattleData#battle_data.monster) 
			andalso (Winner == att) 
			andalso (Camp == att) 
			andalso (Type =/= ?BATTLE_TYPE_BOSS) 
            andalso (Type =/= ?BATTLE_TYPE_CAVE) of
		true ->
			send_battle_award(ID, BattleData);
		false ->
			ok
	end,
	
	WinHPList1 = get_battle_hp_list(Winner, BattleData),
    FullHPList = get_full_battle_hp_list(BattleData),
    ?INFO(battle, "FullHPList = ~w", [FullHPList]),
    AccMPList  = get_player_mp_list(BattleData),
    ?INFO(battle, "AccMPList = ~w", [AccMPList]),
	F1 = fun({Pos, HP}) ->
				 {(Pos - 1) rem (?BATTLE_FIELD_SIZE div 2) + 1, HP}
		 end,
	WinHPList = lists:map(F1, WinHPList1),
	
	BattleResult = 
		#battle_result {
			is_win    = IsWin, 
			mon_id    = BattleData#battle_data.monster,
			type      = Type, 
            battle_pid = self(),
			hp_list   = WinHPList,
            full_hp_list = FullHPList,
            player_mp = AccMPList,
			callback  = Callback, 
			statistic = Statistic
		},

	case BattleResult#battle_result.callback of
        {Mod, Fun, Args} ->
            Mod:Fun(ID, BattleResult, Args);
        _Other ->
            ?ERR(battle, "Unsupported battle callback: ~w", [_Other])
    end.

get_newbie_mp(PlayerLevel) ->
    {_I,_F,MinNewbieMPLevel,_L} = data_enable_system:get(?BATTLE_EXTRA_MP),
    {_I,_F,MaxNewbieMPLevel,_L} = data_enable_system:get(?BATTLE_EXTRA_MP_END),

    case PlayerLevel >= MinNewbieMPLevel andalso 
            PlayerLevel =< MaxNewbieMPLevel of
        true -> 80;
        false -> 0
    end.

init_global_buff(PlayerID, PlayerLevel, MerList) ->
    RawList = [?G_BUFF_NEWBIE_MP],
    {_, _, _, GBuffList} = lists:foldl(fun init_one_global_buff/2, 
                                       {PlayerID, PlayerLevel, MerList, []},
                                       RawList),
    GBuffList.

init_one_global_buff(?G_BUFF_NEWBIE_MP, {PlayerID, PlayerLevel, MerList, AccList}) ->
    case get_newbie_mp(PlayerLevel) of
        0 ->
            {PlayerID, PlayerLevel, MerList, AccList};
        NewbieMP ->
            NewBuff = #buff{
                name = ?G_BUFF_NEWBIE_MP,
                value = NewbieMP
            },
            {PlayerID, PlayerLevel, MerList, [NewBuff | AccList]}
    end.

send_global_buff(BattleData) ->
    IDPacketList = pt_20:write(20011, BattleData),
    send_player_package(IDPacketList).

filter_duplicate_pos(List) ->
    filter_duplicate_pos(List, dict:new(), []).

filter_duplicate_pos([Role | Rest], PosDict, AccList) ->
    CurPos = Role#role.gd_isBattle,
    case dict:find(CurPos, PosDict) of
        error ->
            filter_duplicate_pos(Rest, dict:store(CurPos, true, PosDict), [Role | AccList]);
        _ ->
            ?ERR(battle, "Duplicate position: key = ~w, pos = ~w", 
                 [Role#role.key, Role#role.gd_isBattle]),
            filter_duplicate_pos(Rest, PosDict, AccList)
    end;
filter_duplicate_pos([], _PosDict, AccList) ->
    ?INFO(battle, "_PosDict = ~w", [dict:to_list(_PosDict)]),
    AccList.

%% XXX: 这里可能会有时序问题……
check_cross(OnlineIDList, _BattleData) ->
    lists:all(fun(ID) ->
                not mod_cross:is_in_cross_state(ID)
        end,
        OnlineIDList).

handle_dmg_shield(_Tar, OrigDmg, BuffList) ->
    case lists:keysearch(?BUFF_DMG_SHIELD, #buff.name, BuffList) of
        false ->
            {none, 0, OrigDmg};
        {value, Buff = #buff{value = DmgAbsVal}} ->
            ?INFO(battle, "Buff = ~w", [Buff]),
            ?BATTLE_LOG("        站位 ~w Buff效果: 护盾", [_Tar]),
            ?BATTLE_LOG("            Buff类型: 护盾, 伤害吸收数量: ~w", [DmgAbsVal]),
            NewAbsVal = max(0, DmgAbsVal - OrigDmg),
            NewDmg = max(0, OrigDmg - DmgAbsVal),
            {Buff#buff{value = NewAbsVal}, DmgAbsVal - NewAbsVal, NewDmg}
    end.

update_buff_value_rel(BuffName, DiffValue, BuffList) ->
    case lists:keysearch(BuffName, #buff.name, BuffList) of
        false ->
            ?INFO(battle, "Buff search missed"),
            BuffList;
        {value, Buff = #buff{value = Val}} ->
            ?INFO(battle, "Old val = ~w", [Val]),
            NewVal = Val + DiffValue,
            ?INFO(battle, "NewVal = ~w", [NewVal]),
            lists:keyreplace(BuffName, #buff.name, BuffList, Buff#buff{value = NewVal})
    end.

update_buff_value_rel(BuffName, DiffValue, Threshold, BuffList) ->
    case lists:keysearch(BuffName, #buff.name, BuffList) of
        false ->
            ?INFO(battle, "Buff search missed"),
            BuffList;
        {value, Buff = #buff{value = Val}} ->
            ?INFO(battle, "Old val = ~w, DiffValue = ~w", [Val, DiffValue]),
            NewVal = if
                DiffValue > 0 ->
                    min(Val + DiffValue, Threshold);
                DiffValue < 0 ->
                    max(Val + DiffValue, Threshold);
                true ->
                    Val
            end,
            ?INFO(battle, "NewVal = ~w", [NewVal]),
            lists:keyreplace(BuffName, #buff.name, BuffList, Buff#buff{value = NewVal})
    end.

del_buff_list(BuffList, TarList, BattleData) ->
    ?INFO(battle, "Deleting BuffList = ~w", [BuffList]),
    lists:foldl(
        fun(BuffName, BD) ->
            del_buff(BuffName, TarList, BD)
        end,
        BattleData,
        BuffList).

del_buff(BuffName, TarList, BattleData) ->
    lists:foldl(
        fun(Tar, BD) ->
            TarStat = get_battle_status(Tar, BD),
            NewBuffList = del_buff_1(BuffName, TarStat#battle_status.buff, []),
            set_battle_status(Tar, TarStat#battle_status{buff = NewBuffList}, BD)
        end,
        BattleData,
        TarList).

del_buff_1(BuffName, [H | T], AccList) ->
    case H#buff.name of
        BuffName ->
            ?INFO(battle, "deleting buff ~w", [BuffName]),
            NBuff = H#buff{settle = pre, in_effect = false},
            del_buff_1(BuffName, T, [NBuff | AccList]);
        _ ->
            del_buff_1(BuffName, T, [H | AccList])
    end;
del_buff_1(_BuffName, [], AccList) ->
    AccList.

%% AttInfo里最后元素（如果有的话）一定是当前攻击目标，
%% 可以用来判断暴击、命中等状态
get_att_added_mp_by_skill(SkillID, SrcStat, AttInfo) ->
    SkillInfo = data_skill_table:get(SkillID, 1),
    BaseMP = case SkillInfo#battle_skill.mp_add of
        {MainRoleMPAdd, MerRoleMPAdd} ->
            case is_integer(SrcStat#battle_status.player_id)
                    andalso (SrcStat#battle_status.is_lead) of
                true  -> MainRoleMPAdd;
                false -> MerRoleMPAdd
            end;
        MPAdd when is_integer(MPAdd) ->
            MPAdd
    end,

    ExtraMP = case is_integer(SrcStat#battle_status.player_id) 
            andalso (SrcStat#battle_status.is_lead) of
        true ->
            case SrcStat#battle_status.job of
                ?CAREER_HUWEI -> 
                    %% 虎卫：每次行动时恢复10点怒气（无论是对敌行动还是对己行动，没有行动则不增加）
                    data_system:get(126);
                ?CAREER_MENGJIANG ->
                    %% 猛将：每次暴击时恢复5点怒气（命中并暴击就增加，即使伤害被完全吸收）
                    lists:foldl(
                        fun(A, Sum) ->
                            case A#attack_info.is_crit of
                                true  -> Sum + data_system:get(127);
                                false -> Sum
                            end
                        end,
                        0, AttInfo);
                ?CAREER_JUNSHI_ATT ->
                    %% 军师：每次命中敌方目标时恢复2点怒气（未命中不增加，命中但没造成伤害也加）
                    lists:foldl(
                        fun(A, Sum) ->
                            case A#attack_info.is_miss of
                                false -> Sum + data_system:get(128);
                                true  -> Sum
                            end
                        end,
                        0, AttInfo);
                ?CAREER_JUNSHI_HELP ->
                    %% 军师：每次命中敌方目标时恢复2点怒气（未命中不增加，命中但没造成伤害也加）
                    lists:foldl(
                        fun(A, Sum) ->
                            case A#attack_info.is_miss of
                                false -> Sum + data_system:get(128);
                                true  -> Sum
                            end
                        end,
                        0, AttInfo)
            end;
        false ->
            case is_integer(SrcStat#battle_status.player_id) of
                true ->     % 这是玩家的武将……
                    %% 武将每回合自动恢复15点怒气
                    data_system:get(129);
                false ->    % 这是怪物……
                    %% 怪物每回合自动恢复20点怒气
                    data_system:get(130)
            end
    end,

    BaseMP + ExtraMP + SrcStat#battle_status.mp_up_extra.


get_tar_for_common_attack(Src, Tar, BattleData) ->
    case Tar of
        {ai_override, _} ->         % XXX: AI设置的目标优先级最高
            Tar;
        _ ->
            AllTarList = get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
            SrcCareer = get_battle_status(Src, #battle_status.job, BattleData),
            case SrcCareer of
                ?CAREER_JUNSHI_ATT ->
                    %% 军师随机选目标
                    Tar;        % Tar本来就是随机的，直接返回就行了……
                ?CAREER_JUNSHI_HELP ->
                    Tar;
                ?CAREER_HUWEI ->
                    %% 虎卫先打前排
                    [NTar | _] = get_first_row(AllTarList, 1, BattleData),
                    NTar;
                ?CAREER_MENGJIANG ->
                    %% 猛将先打前排
                    [NTar | _] = get_first_row(AllTarList, 1, BattleData),
                    NTar;
                _Other ->
                    ?INFO(battle, "_Other = ~w", [_Other]),     % 世界boss会返回0……
                    Tar
            end
    end.

merge_actions(BattleData) ->
    AttPro = get_attack_pro(BattleData),
    NAttackInfo = case AttPro#attack_pro.attack_info of
        [A1, A2 | Rest] ->
            [A2 ++ A1 | Rest];
        Other ->
            Other
    end,
    NAttPro = AttPro#attack_pro{attack_info = NAttackInfo},
    set_attack_pro(NAttPro, BattleData).

mark_20001_sent() ->
    erlang:put(is_20001_sent, true).

sent_any_20001() ->
    case erlang:get(is_20001_sent) of
        true -> true;
        _ -> false      % undefined
    end.

get_rel_hp(Pos, BattleData) ->
    case get_battle_status(Pos, BattleData) of
        ?UNDEFINED ->
            0;
        BS ->
            BS#battle_status.hp / BS#battle_status.hp_max
    end.

get_player_mp_list(BattleData) ->
    IDList = get_ids(BattleData),
    F = fun(ID) ->
        PInfo = get_player_info(ID, BattleData),
        LeadBS = get_battle_status(PInfo#player_info.lead, BattleData),
        {ID, LeadBS#battle_status.mp}
    end,
    [F(ID) || ID <- IDList].

get_battle_id() ->
    case erlang:get(batt_id) of
        ?UNDEFINED ->
            Self = self(),
            StrSelf = pid_to_list(Self),
            FirstDotIdx = string:chr(StrSelf, $.),
            LastDotIdx = string:rchr(StrSelf, $.),
            Len = LastDotIdx - FirstDotIdx - 1,
            StrID = string:substr(StrSelf, FirstDotIdx + 1, Len),
            BattID = list_to_integer(StrID),
            erlang:put(batt_id, BattID),
            BattID;
        BattID ->
            BattID
    end.

-ifdef(debug).

format_buff(#buff{name = ?BUFF_SCORNED} = Buff) ->
    io_lib:format("    Buff类型: 被嘲讽, 嘲讽者站位: ~w", [Buff#buff.value]);
format_buff(#buff{name = ?BUFF_ASSISTED} = Buff) ->
    io_lib:format("    Buff类型: 被援护, 系数: ~w, 援护者站位: ~w", [Buff#buff.value, Buff#buff.data]);
format_buff(#buff{name = ?BUFF_DMG_ABSORB_TARGET} = Buff) ->
    io_lib:format("    Buff类型: 伤害被吸收, 系数: ~w, 吸收者站位: ~w", [Buff#buff.value, Buff#buff.data]);
format_buff(Buff) ->
    io_lib:format("    Buff类型: ~s, 系数: ~w", 
                  [buff_type_to_str(Buff#buff.name), Buff#buff.value]).

buff_type_to_str(Type) ->
    case Type of
        ?BUFF_PDEF_UP     -> "物防+";
        ?BUFF_PDEF_DOWN   -> "物防-";
        ?BUFF_MDEF_UP     -> "魔防+";
        ?BUFF_MDEF_DOWN   -> "魔防-";
        ?BUFF_ATT_UP      -> "攻击+";
        ?BUFF_ATT_DOWN    -> "攻击-";
        ?BUFF_LUCK_UP     -> "幸运+";
        ?BUFF_LUCK_DOWN   -> "幸运-";

        ?BUFF_SPEED_UP    -> "速度+";
        ?BUFF_SPEED_DOWN  -> "速度-";
        ?BUFF_CRIT        -> "必暴击";
        ?BUFF_FATAL_UP    -> "致命+";
        ?BUFF_FATAL_DOWN  -> "致命-";
        ?BUFF_HIT_UP      -> "命中+";
        ?BUFF_HIT_DOWN    -> "命中-";
        ?BUFF_REBOUND     -> "反弹";
        ?BUFF_COUNTER     -> "反击";
        ?BUFF_LIFE_DRAIN  -> "吸血";
        ?BUFF_MANA_DRAIN  -> "吸怒气"; 
        ?BUFF_MANA_ADJUST -> "调整怒气";
        ?BUFF_REFRESH     -> "回血";
        ?BUFF_SCORN       -> "嘲讽";
        ?BUFF_SCORNED     -> "被嘲讽";
        ?BUFF_ASSIST      -> "援护";
        ?BUFF_ASSISTED    -> "被援护";
        ?BUFF_FAINT       -> "晕";
        ?BUFF_FRENZY      -> "狂暴";
        ?BUFF_WEAKNESS    -> "降低治疗量";
        ?BUFF_TOXIC       -> "中毒";

        ?BUFF_DODGE_UP    -> "闪避+";
        ?BUFF_DODGE_DOWN  -> "闪避-";

        ?BUFF_BLOCK_UP    -> "格挡+";
        ?BUFF_BLOCK_DOWN  -> "格挡-";

        ?BUFF_CRIT_UP     -> "暴击+";
        ?BUFF_CRIT_DOWN   -> "暴击-";

        ?BUFF_CAST_DMG_UP -> "输出伤害+";
        ?BUFF_RECV_DMG_DOWN -> "输入伤害-";
        ?BUFF_CAST_DMG_DOWN -> "输出伤害-";
        ?BUFF_RECV_DMG_UP -> "输入伤害+";

        ?BUFF_DMG_ABSORB  -> "伤害吸收";
        ?BUFF_DMG_ABSORB_TARGET -> "伤害被吸收";

        ?BUFF_CURSED      -> "被诅咒";

        ?BUFF_COUNTER_UP   -> "反击+";
        ?BUFF_COUNTER_DOWN -> "反击-";

        ?BUFF_ATT_ENHANCE -> "越战越勇";
        ?BUFF_ATT_WORSEN  -> "越战越挫";

        ?BUFF_EXTRA_CRIT_RATE -> "额外暴击几率";
        ?BUFF_DMG_SHIELD  -> "护盾"
    end.

tag_to_buff_effect(Tag) ->
    case Tag of
        pdef -> "物防";
        mdef -> "魔防";
        att  -> "攻击";
        crit -> "暴击";
        luck -> "幸运";
        speed -> "速度";
        hit  -> "命中";
        dodge -> "闪避";
        block -> "格挡";
        fatal -> "致命";
        counter -> "反击";
        cast_damage -> "输出伤害";
        recv_damage -> "输入伤害";
        att_enhance -> "越战越勇"
    end.

dump_player_list(BattleData) ->
    F = fun(Pos) ->
        dump_single_player(Pos, array:get(Pos, BattleData#battle_data.player))
    end,
    lists:foreach(F, lists:seq(1, ?BATTLE_FIELD_SIZE)).

dump_single_player(Pos, P) ->
    case P of
        ?UNDEFINED -> void;
        P ->
            ?BATTLE_LOG("站位: ~w, ID: ~w, 名字: ~s, 职业: ~w, 等级: ~w, 星级: ~w",
                        [Pos, P#battle_status.id, P#battle_status.name, 
                         P#battle_status.job, P#battle_status.level, P#battle_status.star]),
            ?BATTLE_LOG("    血: ~w, 怒气: ~w, 怒气上限: ~w, 物攻: ~w, 魔攻: ~w, 物防: ~w, 魔防: ~w", 
                        [P#battle_status.hp, P#battle_status.mp, P#battle_status.mp_max,
                         P#battle_status.p_att, P#battle_status.m_att,
                         P#battle_status.p_def, P#battle_status.m_def]),
            ?BATTLE_LOG("    速度: ~w, 命中: ~w, 闪避: ~w, 暴击: ~w, 幸运: ~w, 反击: ~w, 格挡: ~w, 破甲: ~w, 敏捷: ~w", 
                        [P#battle_status.speed, P#battle_status.hit, 
                         P#battle_status.dodge, P#battle_status.crit,
                         P#battle_status.luck, P#battle_status.counter,
                         P#battle_status.block, P#battle_status.break,
                         P#battle_status.agility]),
            ?BATTLE_LOG("    技能: ~w", [P#battle_status.skill]),
            ?BATTLE_LOG("    被动技能: ~w", [P#battle_status.p_skill]),
            ?BATTLE_LOG("    数值加成技能: ~w", [P#battle_status.n_skill])
    end.

dump_brief_player_list(BattleData) ->
    F = fun(Pos) ->
        case array:get(Pos, BattleData#battle_data.player) of
            undefined -> void;
            P ->
                ?BATTLE_LOG("站位: ~w, 血: ~w, 怒气: ~w", [Pos, P#battle_status.hp, P#battle_status.mp])
        end
    end,
    lists:foreach(F, lists:seq(1, ?BATTLE_FIELD_SIZE)).

-else.

format_buff(_) -> ok.

buff_type_to_str(_) -> ok.

tag_to_buff_effect(_) -> ok.

dump_player_list(_) -> ok.

dump_single_player(_, _) -> ok.

dump_brief_player_list(_) -> ok.

-endif.

