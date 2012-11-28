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

-compile(export_all).

%% ====================================================================
%% External functions
%% ====================================================================

-spec start(Start) -> {ok, Pid} | ignore | {error, Reason} when
	Start  :: #battle_start{},
	Pid    :: pid(),
	Reason :: term().
 
start(Start) ->
	gen_fsm:start(?MODULE, Start, []).

set_battle_command(BattlePid, ID, Cmd) ->
	gen_fsm:send_event(BattlePid, {set_battle_cmd, ID, Cmd}).

quit_battle(BattlePid, ID) ->
	gen_fsm:send_event(BattlePid, {quit, ID}).

logout(BattlePid, {logout, ID}) ->
	gen_fsm:send_all_state_event(BattlePid, {logout, ID}).

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
	{H, M, S} = now(),
	put(id, Start#battle_start.att_id),
	
	random:seed(H, M, S),
	
	%% timer:sleep(5000),
	BattleData = get_battle_data(Start),
	IDList1 = get_ids(att, BattleData),
	IDList2 = get_ids(def, BattleData),
	
	case catch check_battle(Start, IDList1, IDList2) of
		true ->
			?INFO(battle, "Battle started."),
			%% may be check the operation in the future
			{ok, battle_init, BattleData#battle_data {timeout = {now(), 0}}, 0};
		False ->
			clear_battle(IDList1 ++ IDList2, self()),
			case False of
			false ->
				%% print the err msg here.
				?ERR(battle, "Check Battle Fail."),
				ignore;
			{false, Reason} ->
				?ERR(battle, "Check Battle Fail: Reason: ~w", [Reason]),
				ignore;
			{'EXIT', Reason} ->
				%% reach here probably because player is not online
				?ERR(battle, "check battle Fail: Reason: ~w", [Reason]),
				ignore
			end
	end.

clear_battle([], _Pid) -> ok;
clear_battle([ID | Rest], Pid) ->
	catch mod_player:clear_battle(ID, Pid),
	clear_battle(Rest, Pid).

%% init client side
%% init rounds, battle procedure list
battle_init(_Event, BattleData) ->
	NBattleData = start_new_round(BattleData),
	
	send_start_package(NBattleData),

	{next_state, battle_run, NBattleData#battle_data {
		timeout = {now(), ?BATTLE_WAIT_BEGIN}}, ?BATTLE_WAIT_BEGIN}.

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
				BattleData1 = set_battle_cmd(ID, Cmd, BattleData),
				%% when is the first round, every player has about 5 secs to set the instruction
				%% when 5 secs is reached or all the player has set the instruction, battle begin
				%% it means we do some calculations then send the result to the client side
				case (Round == 1 andalso is_command_set(BattleData1)) of
					true ->
						handle_command_ex(BattleData1, true);
					false ->
						BattleData1
				end;
			{finish_play, ID} ->
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
			timeout ->
				?INFO(battle, "timeout... Timeout = ~w", [Timeout]),
				%% trigger a new battle action if timeout
				BattleData1 = handle_command_ex(BattleData, true),
				BattleData1;
			_ ->
				BattleData
		end,
	
	case is_battle_end(NBattleData) of
		{true, Winner} ->
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
			send_result_package(NBattleData1),
			{next_state, battle_complete, NBattleData1, ?BATTLE_WAIT_QUIT};
		
		false ->
			if (Event == timeout) ->
				{next_state, battle_run, NBattleData#battle_data {
					timeout = {Now, ?BATTLE_WAIT_FINISH}}, ?BATTLE_WAIT_FINISH};
			true -> 
				%% some player has not acted yet(due to the same round), 
				%% update the timeout value
				if (BattleData#battle_data.round == NBattleData#battle_data.round) ->
					{next_state, battle_run, NBattleData#battle_data {
						timeout = {Now, Timeout}}, Timeout};
				true ->
					%% if is next turn...
					{next_state, battle_run, NBattleData#battle_data {
						timeout = {Now, ?BATTLE_WAIT_FINISH}}, ?BATTLE_WAIT_FINISH}
				end
			end
	end.

battle_complete(timeout, BattleData) ->
	{stop, normal, BattleData};

battle_complete({quit, ID}, BattleData) ->
	?INFO(battle, "receive command quit"),
	{Now, Timeout} = time_remain(BattleData),
	
	NBattleData = 
	case get_player_info(ID, BattleData) of
		false -> BattleData;
		Info  -> 
			case Info#player_info.finish_play == quit of
				true  -> BattleData;
				false -> 
					?INFO(battle, "notify player ID = ~w", [ID]),
					catch notify_complete(ID, BattleData),
					%% if a player_info with finish_play = quit,
					%% we will not send the award and notify again.
					NInfo = Info#player_info{finish_play = quit},
					set_player_info(ID, NInfo, BattleData)
			end
	end,
	case is_all_quit_battle(NBattleData) of
		true  -> {stop, normal, NBattleData};
		false -> {next_state, battle_complete, NBattleData#battle_data {timeout = {Now, Timeout}}, Timeout}
	end;

battle_complete(Msg, BattleData) ->
	{Now, Timeout} = time_remain(BattleData),
	?INFO(battle, "unknown message: ~w", [Msg]),
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
						
					case logout_info(NBattleData) of
						all ->
							exit(normal);
						none -> 
							if (StateName == battle_run) ->
								battle_run({finish_play, ID}, NBattleData#battle_data {timeout = {Now, Timeout}});
							(StateName == battle_complete) ->
								battle_complete({quit, ID}, NBattleData#battle_data {timeout = {Now, Timeout}});
							true ->
								{next_state, StateName, BattleData#battle_data {timeout = {Now, Timeout}}, Timeout}
							end;
						Camp ->
							Award = get_battle_award(BattleData),
							NBattleData1 = NBattleData#battle_data {
								award = Award, 
								winner = if (Camp == att) -> def; true -> att end, 
								timeout = {Now, ?BATTLE_WAIT_QUIT}
							},
							
							send_battle_award(NBattleData1),
							send_result_package(NBattleData1),
							{next_state, battle_complete, NBattleData1, ?BATTLE_WAIT_QUIT}
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

handle_info(_Info, StateName, BattleData) ->
    {next_state, StateName, BattleData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(Reason, _StateName, BattleData) ->
	?INFO(battle, "terminating.. Reason = ~w~n, BattleData = ~w", [Reason, BattleData]),
	Winner = BattleData#battle_data.winner,
	?INFO(battle, "Winner = ~w", [Winner]),
	
	Ids = get_ids(BattleData),
	F = fun(ID) ->
			Info = get_player_info(ID, BattleData),
			if (Info#player_info.finish_play == quit) ->
				%% battle has notified this player
				ok;
			true ->
				?INFO(battle, "notify complete id = ~w", [ID]),
				catch notify_complete(ID, BattleData)
			end
		end,
	lists:foreach(F, Ids).

notify_complete(ID, BattleData) ->
	?INFO(battle, "calling notify complete, ID = ~w", [ID]),
	
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
	F1 = fun({Pos, HP}) ->
				 {(Pos - 1) rem (?BATTLE_FIELD_SIZE div 2) + 1, HP}
		 end,
	WinHPList = lists:map(F1, WinHPList1),
	
	Res = 
		#battle_result {
			is_win    = IsWin, 
			mon_id    = BattleData#battle_data.monster,
			type      = Type, 
			hp_list   = WinHPList,
			callback  = Callback, 
			statistic = Statistic
		},
	
	catch mod_player:battle_complete(ID, Res).

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
	
	case Mod of
		pve -> 
			?INFO(battle, "calling get_mer_info"),
			{Attacker, Array1} = get_mer_info(att, AttID, AttMer, MakeTeam, Array),
			?INFO(battle, "calling get_mon_info"),
			{Defender, Array2} = get_mon_info(def, MonID, Start#battle_start.monster_hp, Array1),
			?INFO(battle, "done..."),
			{Defender, Array2};
		pvp ->
			{Attacker, Array1} = get_mer_info(att, AttID, AttMer, MakeTeam, Array),
			{Defender, Array2} = get_mer_info(def, DefID, DefMer, MakeTeam, Array1)
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
	case Camp of
		att -> [E#player_info.id || E <- BattleData#battle_data.attacker, is_integer(E#player_info.id)];
		def -> [E#player_info.id || E <- BattleData#battle_data.defender, is_integer(E#player_info.id)]
	end.

-spec get_ids(#battle_data{}) -> [integer()].
get_ids(BattleData) ->
	get_ids(att, BattleData) ++ 
	get_ids(def, BattleData).


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
	
-spec get_mer_info(Camp, ID, MerList, MakeTeam, Array) -> {PlayerInfoList, NArray} when
	Camp           :: battle_camp(),
	ID             :: integer() | undefined,
	MerList        :: [any()], %% #mercenary{}
	MakeTeam       :: true | false,
	PlayerInfoList :: [#player_info{}],
	Array          :: array(),
	NArray         :: array().
																					 
get_mer_info(Camp, ID, MerList, MakeTeam, Array) ->
	RoleFun = 
		fun (Role, Acc) ->
			if (Camp == att) ->
				[Role#role.gd_isBattle | Acc];
			true ->
				[Role#role.gd_isBattle + ?BATTLE_FIELD_SIZE div 2 | Acc]
			end
		end,
	
	ArrayFun = 
		fun (Role, Arr) ->
			if (Camp == att) ->
				Pos = Role#role.gd_isBattle;
			true ->
				Pos = Role#role.gd_isBattle + ?BATTLE_FIELD_SIZE div 2
			end,
			array:set(Pos, role_2_bs(ID, Role), Arr)
		end,
			
	if (MakeTeam == true) ->
		?INFO(battle, "check make team..."),
		{true, Ps} = mod_player:is_online(ID),
		case catch mod_team:prepare_team_battle(ID) of
			false ->
				?INFO(battle, "not make team"),
				List = mod_role:get_on_battle_list(ID),
				PInfo = 
					#player_info {
						id       = ID, 
						pid      = Ps#player_status.player_pid, 
						lead     = get_mer_leader(Camp, List), 
						mer_list = lists:foldl(RoleFun, [], List)
					},	  
				NArray = lists:foldl(ArrayFun, Array, List),
				{[PInfo], NArray};
			
			[{ID, List}, {ID2, List2}] ->
				?INFO(battle, "make team"),
				?INFO(battle, "ID1 = ~w, RoleList1 = ~w", [ID, List]),
				?INFO(battle, "ID2 = ~w, RoleList2 = ~w", [ID2, List2]),
				
				{true, Ps2} = mod_player:is_online(ID2),
				PInfo = 
					#player_info {
						id       = ID,		  
						pid      = Ps#player_status.player_pid,
						lead     = get_mer_leader(Camp, List),
						mer_list = lists:foldl(RoleFun, [], List)
					},
				
				PInfo2 = 
					#player_info {
						id       = ID2,
						pid      = Ps2#player_status.player_pid, 
						lead     = get_mer_leader(Camp, List2),
						mer_list = lists:foldl(RoleFun, [], List2)
					},
				
				NArray = lists:foldl(ArrayFun, Array, List ++ List2),
				{[PInfo, PInfo2], NArray};
			_Other ->
				?ERR(battle, "mod_team returns: ~w", [_Other])
		end;
	true -> %% maketeam = false;
		List = 
			if (MerList =/= []) ->
				MerList;
			true ->
			   %% ID must be specified if MerList is []
			   %%(is_integer(ID)) ->
				mod_role:get_on_battle_list(ID)
			end,
			
		?INFO(battle, "On Battle List = ~w", [List]),
		
		Leader  = 
			case get_mer_leader(att, List) of
			data_not_exist ->
				hd(MerList);
			L ->
				L
			end,
		List1 = lists:foldl(RoleFun, [], List),
		PInfo   = 
			#player_info {
				id       = ID,
				%% pid   = Ps#player_status.player_pid, 
				lead     = Leader,
				mer_list = List1
			},
		NArray = lists:foldl(ArrayFun, Array, List),
		{[PInfo], NArray}
	end.

get_mer_leader(_, []) ->
	data_not_exist;
	
get_mer_leader(Camp, [Role | Rest]) ->
	if (Role#role.gd_roleRank == 1) ->
		if (Camp == att) ->
				Role#role.gd_isBattle;
		   (Camp == def) ->
				Role#role.gd_isBattle + ?BATTLE_FIELD_SIZE div 2
		end;
	true ->
		get_mer_leader(Camp, Rest)
	end.
			   
%% role to battle status
role_2_bs(ID, Role) ->
	?INFO(battle, "ID = ~w", [Role#role.gd_roleRank]),
	?INFO(battle, "Skill From Mer = ~w", [Role#role.gd_skill]),
	{ActSkills, PasSkills} = transform_skill(Role#role.gd_skill),
	
	?INFO(battle, "ActSkills = ~w, PasSkills = ~w", [ActSkills, PasSkills]),
	?INFO(battle, "StartLevel = ~w", [Role#role.star_lv]),
	?INFO(battle, "p_att = ~w, hp = ~w, maxhp = ~w",[Role#role.p_att,Role#role.gd_currentHp,Role#role.gd_maxHp]),
	
	#battle_status {
		id        = element(2, Role#role.key),
		player_id = ID,
		name      = Role#role.gd_name,      %% gd_name is a string, which means a list.
		level     = Role#role.gd_roleLevel,
		star      = Role#role.star_lv,
		job       = Role#role.gd_careerID,
		skill     = ActSkills,
		%% skill    = [245001, 279001],
		p_skill   = PasSkills,
		hp        = Role#role.gd_currentHp ,
		hp_max    = Role#role.gd_maxHp,
		p_att     = Role#role.p_att,
		p_def     = Role#role.p_def,
		m_att     = Role#role.m_att,
		m_def     = Role#role.m_def,
		dodge     = Role#role.gd_shanbi,
		hit       = Role#role.gd_mingzhong,
		agility   = Role#role.gd_minjie, 
		speed     = Role#role.gd_speed,
		break     = Role#role.gd_pojia,
		crit      = Role#role.gd_baoji,
		luck      = Role#role.gd_xingyun,
		is_lead   = Role#role.gd_roleRank == 1 
	}.

get_mon_info(Camp, MonGroupID, Array) ->
	get_mon_info(Camp, MonGroupID, ?HP_MAX, Array).

get_mon_info(Camp, MonGroupID, MonHp, Array) ->
	Monster = data_mon_group:get(MonGroupID),
	
	?INFO(battle, "MonGroupID = ~w, Monster = ~w", [MonGroupID, Monster]),
	
	F = fun({MonID, Pos}, {Arr, List}) ->
			MonAttr = data_mon_attr:get(MonID),
			Res = transform_skill(MonAttr#mon_attr.skills),
			?INFO(battle, "monster skill = ~w, start = ~w", [Res, MonAttr#mon_attr.star]),
			
			{ActiveSkills, _} = Res,

            MaxHP = if
                is_integer(MonHp) -> MonHp;
                is_list(MonHp) ->
                    case lists:keyfind(Pos, 1, MonHp) of
                        false   -> MonAttr#mon_attr.hp;
                        {_, HP} -> HP
                    end
            end,
            CurHP = min(MonAttr#mon_attr.hp, MaxHP),
			
			BattleStatus = 
				#battle_status {
					id      = MonID,
					job     = MonAttr#mon_attr.cat,
					skill   = ActiveSkills,
					name    = "", 
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
					block   = MonAttr#mon_attr.block,
					break   = MonAttr#mon_attr.break,
					luck    = MonAttr#mon_attr.luck,
					hit     = MonAttr#mon_attr.hit,
					crit    = MonAttr#mon_attr.crit,
					is_lead = 0,
                    is_alive = case CurHP of
                                   0 -> false;
                                   _ -> true
                               end
				},
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

%% check battle : set the is_battle flag in the PlayerStatus
%% if the att ID or def ID is specified, check it
%% otherwise we can ignore it.

-spec check_battle(BattleData, IDList1, IDList2) -> true | false when 
	IDList1 :: [player_id()],
	IDList2 :: [player_id()],
	BattleData :: #battle_data{}.

check_battle(Start, IDList1, IDList2) ->
	Mod       = Start#battle_start.mod,
	AttID     = Start#battle_start.att_id,
	DefID     = Start#battle_start.def_id,
	
	case erlang:is_integer(AttID) of
		true ->
			case mod_player:is_online(AttID) of
			{true, Ps} ->
				Pid = Ps#player_status.player_pid,
				TeamMateID = 
					case tl(IDList1) of 
						[] -> 0;
						[TID] -> TID
					end,
				if (Mod =/= pvp) ->
					case mod_player:check_battle(Pid, self(), TeamMateID) of
						{false, in_battle} ->
							catch mod_err:send_err(AttID, ?ERR_BATTLE_ATTACKER_IS_ON_BATTLE),
							{false, in_battle};
						Other -> Other
					end;
				true ->
					case erlang:is_integer(DefID) of
					true ->
						case mod_player:is_online(DefID) of
						{true, Ps2} ->
							RivalTeamMateID = 
								case tl(IDList2) of
									[] -> 0;
									[TID2] -> TID2
								end,
							case mod_player:check_pvp_battle(
								min(Ps#player_status.player_pid, Ps2#player_status.player_pid), 
								max(Ps#player_status.player_pid, Ps2#player_status.player_pid),
								self(), TeamMateID, RivalTeamMateID) of
								{false, rival_in_battle} ->
									catch mod_err:send_err(AttID, ?ERR_BATTLE_DEFENDER_IS_ON_BATTLE),
									{false, rival_in_battle};
								Other -> Other
							end;
						false ->
							catch mod_err:send_err(AttID, ?ERR_BATTLE_DEFENDER_IS_NOT_ON_LINE),
							{false, rival_not_on_line}
						end;
					false ->
						mod_player:check_battle(Pid, self(), TeamMateID)
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
					%% if Sid == 0 then we use AI to set command
					if (Sid == 0) ->						 
						{AISid, _, _, _} = ai:get_skill(Lead, BattleData),
						%% if this skill is common attack, do not send this message to client
						if (AISid == ?SKILL_COMMON_ATTACK) ->
							ok;
						true -> 
							send_command_package(Lead, AISid, BattleData)
						end,
						NState = State#battle_status {cmd = {AISid, Lead, 0}};
					true ->
						NState = State#battle_status {cmd = {Sid, Lead, 0}},
						?INFO(battle, "set battle cmd: Sid = ~w, Lead = ~w, Round = ~w, Timeout = ~w", 
						  	[Sid, Lead, Round, Timeout])
					end,
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
	
	IsFaint = is_buff_exist(?BUFF_FAINT, SrcStat),
	
	%% calculate a new BattleData through handling the skill, 
	%% indicated by NBattleData
	NBattleData = 
		if (SrcStat#battle_status.is_alive == false) ->
			Skip = true,
			BattleData;
		true ->
			Skip = false,
			{Sid, Src, Tar, Idx} = 
				if (IsFaint == true) -> 
					{0, Src, 0, SIndex};
				true ->
					?INFO(battle, "Preset Src = ~w, Cmd = ~w", [Src, SrcStat#battle_status.cmd]),
		
					case SrcStat#battle_status.cmd of
						?UNDEFINED ->
							if (SrcStat#battle_status.is_lead == true andalso
								SrcStat#battle_status.player_id =/= ?UNDEFINED) -> 
								
								T = ai:get_skill_target(?SKILL_COMMON_ATTACK, Src, BattleData),
								{?SKILL_COMMON_ATTACK, Src, T, SIndex};
							true ->  
								ai:get_skill(Src, BattleData)
							end;
						{S, Src, _} ->
							%% case (2): 
							case ai:validate_skill(S, Src, BattleData) of
								true ->
									?INFO(battle, "validation passed."),
									T = ai:get_skill_target(S, Src, BattleData),
									?INFO(battle, "Sid = ~w, Src = ~w, Tar = ~w", [S, Src, T]),
									{S, Src, T, SIndex};
								false ->
									?INFO(battle, "validation not passed."),
									T = ai:get_skill_target(?SKILL_COMMON_ATTACK, Src, BattleData),
									{?SKILL_COMMON_ATTACK, Src, T, SIndex}
									%% ai:get_skill_by_ai(Src, SrcStat#battle_status.id, BattleData)
							end
					end
				end, 
			
			%% order package
			send_order_package(BattleData),
			BattleData1 = skill:handle_skill(Sid, Src, Tar, BattleData),
			
			%% procedure package
			send_procedure_package(BattleData1),
			
			%% send cd package
			if (SrcStat#battle_status.is_lead == true) ->
				send_cd_package(Src, BattleData1); 
			true ->
				ok
			end,
			
			NSrcStat = get_battle_status(Src, BattleData1),	
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
			case length(OrderList) of
				1 -> 
					if (NBattleData1#battle_data.round =:= 30) ->
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
	
	%% notice: is_battle_end returns false | {true, camp()}
	case (Repeat == false orelse is_battle_end(NBattleData2) =/= false) of
		true -> 
			NBattleData2;
		false ->
			NOrderList = NBattleData2#battle_data.attorder,
			NextSrc = hd(NOrderList),
			?INFO(battle, "NextSrc = ~w", [NextSrc]),
			NextSrcStat = get_battle_status(NextSrc, NBattleData2),
			if (NextSrcStat#battle_status.is_lead == true) ->
				NBattleData2;
			true ->
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
	
	case lists:keysearch(?BUFF_LIFE_DRAIN, #buff.name, Buffs) of
		{value, #buff{value = V}} -> 
			round(min(Dm, TarStat#battle_status.hp) * V);
		false ->
			0
	end.

get_mp_absorb(Src, Tar, AttSpec, BattleData) ->
	SrcStat = get_battle_status(Src, BattleData),
	TarStat = get_battle_status(Tar, BattleData), 
	Buffs = AttSpec#attack_spec.buff_add ++ SrcStat#battle_status.buff,
	
	case lists:keysearch(?BUFF_MANA_DRAIN, #buff.name, Buffs) of
		{value, #buff{value = {A, S}}} ->
			SubFrom = min(S, TarStat#battle_status.mp),
			AddTo   = min(SubFrom, A),
			{AddTo, SubFrom};
		false ->
			{0, 0}
	end.

get_rebound(_Src, Tar, _AttSpec, Dm, BattleData) ->
	TarStat = get_battle_status(Tar, BattleData),	
	Buffs = TarStat#battle_status.buff,
	
	case TarStat#battle_status.hp =< Dm of
		true ->
			0;
		false ->
			case lists:keysearch(?BUFF_REBOUND, #buff.name, Buffs) of
				{value, #buff{value = Rebound}} ->
					Rebound;
				false ->
					0
			end
	end.

%% Damage is the damage from Src dealed to Tar
get_counter(_Src, Tar, SkillId, _AttSpec, Dm, BattleData) ->
	if (SkillId =/= ?SKILL_COMMON_ATTACK) ->
		%% skill that is not common attack can not be countered.
		0;
	true ->
		TarStat = get_battle_status(Tar, BattleData),
		Buffs = TarStat#battle_status.buff,
		
		case TarStat#battle_status.hp =< Dm of
			true -> 
				0; %% this player is now dead, so don't need to check counter
			false ->
				case lists:keysearch(?BUFF_COUNTER, #buff.name, Buffs) of
					{value, #buff{value = Counter}} ->
						?CHAT(battle, "skill counter"),
						Counter;
					false ->
						CounterRate = TarStat#battle_status.counter / 1666,
						?CHAT(battle, "CounterRate = ~w", [CounterRate]),
					
						case random:uniform() =< CounterRate of
							true -> 
								?CHAT(battle, "trigger counter"),
								100;
							false -> 0
						end
				end
		end
	end.

%==============================================================================================================
% handling passive SKILL
%==============================================================================================================

-spec get_passive_skill(SkillID, Pos, BattleData) -> false | {true, Level} when
	SkillID    :: integer(),
	Pos        :: integer(),
	Level      :: integer(),
	BattleData :: #battle_data{}.

get_passive_skill(SkillID, Pos, BattleData) ->
	State = get_battle_status(Pos, BattleData),
	case lists:keysearch(SkillID, 1, State#battle_status.p_skill) of
		{value, {_, Level}} -> {true, Level};
		false -> false
	end.

%==============================================================================================================
% handling buff value
%==============================================================================================================
%% get_buff_value returns the buff effect 

-spec get_buff_value(buff_id(), Pos :: integer(), #battle_data{}) -> 
		  {Rate :: float() | integer(), Num :: integer()}. 

get_buff_value(BuffName, Pos, BattleData) ->
	State = get_battle_status(Pos, BattleData),
	%% ?INFO(battle, "Pos = ~w, State = ~w", [Pos, State]),
	Buffs = State#battle_status.buff,
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
	{Buff, DeBuff} = 
		case Tag of
			pdef -> {?BUFF_PDEF_UP, ?BUFF_PDEF_DOWN};
			mdef -> {?BUFF_MDEF_UP, ?BUFF_MDEF_DOWN};
			att  -> {?BUFF_ATT_UP,  ?BUFF_ATT_DOWN};
			crit -> {?BUFF_CRIT_UP, ?BUFF_CRIT_DOWN};
			luck -> {?BUFF_LUCK_UP, ?BUFF_LUCK_DOWN}
		end,
	{F1, I1} = get_buff_value(Buff,   Pos, BattleData), 
	{F2, I2} = get_buff_value(DeBuff, Pos, BattleData),
	round(Attr * (1 + F1 - F2)) + I1 - I2.

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

pre_attack(Src, Tar, _AttSpec, BattleData) ->
	SrcStat = get_battle_status(Src, BattleData),
	TarStat = get_battle_status(Tar, BattleData),
	
	{Hit, Dodge} = {SrcStat#battle_status.hit, TarStat#battle_status.dodge},
	{AStar, DStar} = {SrcStat#battle_status.star, TarStat#battle_status.star},

	Param6 = 
		case SrcStat#battle_status.is_lead == true orelse
			 TarStat#battle_status.is_lead == true of
		true -> 0;
		false -> 0.05
		end,
		
	HitRate = Hit * 10 / (Hit * 10 + Dodge) + Param6 * (AStar - DStar),
	%% ?INFO(battle, "Hit = ~w, Dodge = ~w, Param = ~w, IsHit = ~w", [Hit, Dodge, Param6, IsHit]),
	
	NHitRate = max(0.4, HitRate),
	
	?CHAT(battle, "Hit = ~w, Dodge = ~w, HitRate = ~w", [Hit, Dodge, NHitRate]),
	random:uniform() =< NHitRate.

		
-spec do_attack(Src, Tar, AttSpec, BattleData) -> {CritcalHit, Damage} when
	Src        :: integer(),
	Tar        :: integer(),
	AttSpec    :: #attack_spec{},
	BattleData :: #battle_data{},
	CritcalHit :: true | false,
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
			%% DefNoBreak  = get_adjust_value(DefTag, Def0, NTar, BattleData),
			BreakRate   = SrcStat#battle_status.break / 1000 + ParamX * 0.04 * (AStar - DStar),
			BreakAdjust = 
				case random:uniform() =< BreakRate of
					true  -> 0.7; %% break the defense
					false -> 1
				end,
			
			{get_adjust_value(att,  Att0,  Src,  BattleData),
			 get_adjust_value(DefTag, Def0, Tar,  BattleData) * BreakAdjust}
		end,
	
	?CHAT(value, "{Att0, Def0} = {~w, ~w}, {Att, Def} = {~w, ~w}, {AStar, DStar}  = {~w, ~w}", 
		  [Att0, Def0, Att, Def, AStar, DStar]),
	
	{Crit, Luck} = {
		get_adjust_value(crit, SrcStat#battle_status.crit, Src, BattleData),
		get_adjust_value(luck, TarStat#battle_status.luck, Tar, BattleData)},
	
	CritRate = 
		case lists:keysearch(?BUFF_CRIT, #buff.name, Buff ++ BuffAdd) of
			false -> 
				1 - Luck * 10 / (Luck * 12 + Crit) - ParamX * 0.05 * (AStar - DStar);
			{value, _} ->
				1.0
		end,
		
	?CHAT(battle, "Crit = ~w, Luck = ~w, CritRate = ~w", [Crit, Luck, CritRate]),
	
	IsCrit = 
		random:uniform() =< CritRate,
	
	Deadly = 
		case lists:keysearch(?BUFF_FATAL, #buff.name, SrcStat#battle_status.buff) of
			false -> 0;
			{value, #buff {value = V}} -> V
		end,	
	
	?INFO(battle, "Src = ~w, Job = ~w, Att = ~w, Def = ~w, ALevel = ~w", [Src, Job, Att, Def, ALevel]),
	
	Param1 = 450 * ALevel,
	Param2 = ParamX * 0.2,
	Param3 = (0.9 + random:uniform() / 5),
		
	Damage0 = 
		Att * (1 - Def / (Def + Param1)) * (1 + Param2 * (AStar - DStar)) * 
			Param3 * AttSpec#attack_spec.addition,
	Damage1 = 
		case IsCrit of
			true -> round(Damage0 * 1.5 + Deadly / 600);
			false -> round(Damage0)
		end,
	
	Damage2 = 
		case BattleData#battle_data.mod of
			boss ->
				Att * (Def / Def + 45000);
			_ ->
				get_damage_value(Damage1, Tar, BattleData)
		end,
		
	Damage3 = 
		case lists:keysearch(?BUFF_DAMAGE_ADD, #buff.name, Buff ++ BuffAdd) of
			false -> Damage2;
			{value, #buff{value = Value}} ->
				Damage2 + Value
		end,
	
	BlockRate = 
		TarStat#battle_status.block / 2000 + ParamX * 0.05 * (AStar - DStar),
	
	?CHAT(battle, "BlockRate = ~w", [BlockRate]),

	IsBlock = 
		random:uniform() =< BlockRate, 
		
	if (IsCrit == true) ->
		?CHAT(battle, "*** critcal hit ***");
	true ->
		ok
	end,
	
	if (IsCrit == true andalso Deadly > 0) ->
		?CHAT(battle, "*** deadly hit ***");
	true ->
		ok
	end,
	
	if (IsBlock == true) ->
		?CHAT(battle, "**** blocked ****"),
		Damage4 = round(Damage3 * 0.8);
	true ->
		Damage4 = round(Damage3)
	end,
	
	{IsCrit, max(Damage4, 1)}.

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
	AttSpec     :: #attack_spec{},
	Targets     :: [integer()],
	BattleData  :: #battle_data{},
	AttInfoList :: [#attack_info{}].

attack(SkillId, Src, AttSpec, Targets, BattleData) ->
	attack(SkillId, Src, AttSpec, Targets, [], BattleData).
													  
%% Travers the Target list, do some calculation, and put the result in the AttInfoList
attack(SkillId, Src, AttSpec, [Tar | Rest], AttInfoList, BattleData) ->

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
				{_, #buff {value = T}} when T =/= Tar -> 
					IsAlive = get_battle_status(T, #battle_status.is_alive, BattleData),
					if (IsAlive == true) ->
						{false, true, T};
					true ->
						{false, false, Tar}
					end;
				false -> 
					{false, false, Tar}
			end
		end,
	
	NTarStat =
		if (NTar == Tar) -> TarStat; true -> get_battle_status(NTar, BattleData) end,
	TarBuff =
		NTarStat#battle_status.buff, 
		
	if (Skip == true) ->	   
		attack(SkillId, Src, AttSpec, Rest, AttInfoList, BattleData);
	true ->
		case pre_attack(Src, Tar, AttSpec, BattleData) of
			false ->
				%% miss target, so the other field in attack_info may be 0 (is_miss == true, is_crit = false) 
				
				?INFO(battle, "****************battle attack: ***************************************"),
				?CHAT(battle, "Src ~w --miss--> Tar ~w", [Src, Tar]),
				AttInfo = #attack_info {pos = Tar, is_miss = true},
				attack(SkillId, Src, AttSpec, Rest, [AttInfo | AttInfoList], BattleData);
			true ->
				%% TODO: may be change the AttSpec here
				{Cr, Dm} = do_attack(Src, NTar, AttSpec, BattleData),
				HpDrain  = get_hp_absorb(Src, NTar, AttSpec, Dm, BattleData),
				{Ma, Ms} = get_mp_absorb(Src, NTar, AttSpec, BattleData), %% MpDrain = {MpAdd, MpSub}
				Rebound  = get_rebound(Src, NTar, AttSpec, Dm, BattleData),
				Counter  = get_counter(Src, NTar, SkillId, AttSpec, Dm, BattleData),
				
				?INFO(battle, "****************battle attack: ***************************************"),
				?CHAT(battle, "Src ~w use ~w ---> Tar ~w, deals ~w hitpoint(s), crit = ~w, hp+ = ~w, mp+ = ~w, counter = ~w", 
					  [Src, SkillId, NTar, Dm, Cr, HpDrain, Ma, Counter]),
				
				?INFO(battle, "Skillid = ~w, Src = ~w, Tar = ~w, Crit = ~w, Damage = ~w, HpDrain = ~w, MpDrain = ~w", 
					  [SkillId, Src, NTar, Cr, Dm, HpDrain, Ma]),
				
				print_battle_status(Src, BattleData),
				print_battle_status(NTar, BattleData),
				
				MpAddByAtt = 
					case is_buff_exist(?BUFF_SCORN, NTarStat) of
						true -> 0;
						_    -> 20
					end,
				
				case lists:keysearch(?BUFF_ASSIST, #buff.name, TarBuff) of
					{value, #buff {data = Pos, by_rate = true, value = V}} when Pos =/= Tar, Tar == NTar ->	
		
					AssStat = get_battle_status(Pos, BattleData),
					if (AssStat#battle_status.is_alive == true) ->
						AttInfo1 = 
							#attack_info {
								pos        = NTar,			  
								is_miss    = false,
								is_crit    = Cr,       			%% is critical? boolean()
								hp_inc     = -round(Dm * V),    %% hp incretement
								mp_inc     = -Ms + MpAddByAtt,  %% mp incretement
								hp_absorb  = HpDrain,  			%% hp absorb >= 0
								mp_absorb  = Ma,       			%% mp absorb >= 0
								hp_counter = -Counter, 			%% counter strike
								hp_rebound = -Rebound  			%% rebound damage
							},
						AttInfo2 = 
							#attack_info {
								pos        = Pos,			  
								is_miss    = false,
								is_crit    = Cr,       			
								hp_inc     = -round(Dm * (1 - V)),    
								mp_inc     = 0, 
								hp_absorb  = 0,  			
								mp_absorb  = 0,       			
								hp_counter = 0, 			
								hp_rebound = 0 
							},
						%% update the max damage
						attack(SkillId, Src, AttSpec, Rest, [AttInfo1, AttInfo2 | AttInfoList], BattleData);
					true ->
						AttInfo  = 
							#attack_info {
								pos        = NTar,			  
								is_miss    = false,
								is_crit    = Cr,       			%% is critical? boolean()
								hp_inc     = -Dm,      			%% hp incretement
								mp_inc     = -Ms + MpAddByAtt,  %% mp incretement
								hp_absorb  = HpDrain,  			%% hp absorb >= 0
								mp_absorb  = Ma,       			%% mp absorb >= 0
								hp_counter = -Counter, 			%% counter strike
								hp_rebound = -Rebound  			%% rebound damage
							},
						attack(SkillId, Src, AttSpec, Rest, [AttInfo | AttInfoList], BattleData)
					end;
				_ ->
					AttInfo  = 
						#attack_info {
							pos        = NTar,			  
							is_miss    = false,
							is_crit    = Cr,       			%% is critical? boolean()
							hp_inc     = -Dm,      			%% hp incretement
							mp_inc     = -Ms + MpAddByAtt,  %% mp incretement
							hp_absorb  = HpDrain,  			%% hp absorb >= 0
							mp_absorb  = Ma,       			%% mp absorb >= 0
							hp_counter = -Counter, 			%% counter strike
							hp_rebound = -Rebound  			%% rebound damage
						},
					attack(SkillId, Src, AttSpec, Rest, [AttInfo | AttInfoList], BattleData)
				end
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
			NAttInfo = 
				AttInfo#attack_info {
					hp = TarStat#battle_status.hp,
					mp = TarStat#battle_status.mp
				},
			NBattleData = BattleData;
		false ->
			%% calc defender's final hp, mp, stores in NAttInfo
			NAttInfo = 
				AttInfo#attack_info {
					hp = max(0, TarStat#battle_status.hp + AttInfo#attack_info.hp_inc),
					mp = max(0, min(100, TarStat#battle_status.mp + AttInfo#attack_info.mp_inc))
				},
			%% ?INFO(battle, "NattInfo = ~w", [NAttInfo]),
			
			HpAbsorb = AttInfo#attack_info.hp_absorb,  %% +
			MpAbsorb = AttInfo#attack_info.mp_absorb,  %% +
			Rebound  = AttInfo#attack_info.hp_rebound, %% -
			Counter  = AttInfo#attack_info.hp_counter, %% -
		
			AttOldHp = SrcStat#battle_status.hp,
			AttOldMp = SrcStat#battle_status.mp,
			?INFO(battle, "Src = ~w, AttOldMp = ~w, MpAbsorb = ~w", [Src, AttOldMp, MpAbsorb]),
			AttMaxHp = SrcStat#battle_status.hp_max,
			AttMaxMp = SrcStat#battle_status.mp_max, %% 100

			MpAddBySkill = %% mp add by skill 
				case Rest =/= [] orelse Src == Tar of
					%% Rest =/= [] is used to avoid adding mp more than once when attacking
					true  -> 0;
					false -> 
                        DataSkill = data_skill_table:get(SkillId, 1),   % XXX: 总是用 Lv.1 的配置……
                        DataSkill#battle_skill.mp_add
				end,

			?INFO(battle, "Src = ~w, maxMp = ~w, OldMp = ~w, MpAddbyskill = ~w", 
				[Src, AttMaxMp, AttOldMp, MpAddBySkill]),
			
			AttNewHp = max(0, (min(AttMaxHp, AttOldHp + HpAbsorb + Rebound + Counter))),
			AttNewMp = max(0, (min(AttMaxMp, AttOldMp + MpAbsorb + MpAddBySkill))),
			
			DefNewHp = NAttInfo#attack_info.hp,
			DefNewMp = NAttInfo#attack_info.mp,
			
			%% if player is dead (att & def)?
			%% update this information and store in the BattleData
			NSDamDeal = max(SDamDeal, -AttInfo#attack_info.hp_inc),
			NTDamSuff = max(TDamSuff, -AttInfo#attack_info.hp_inc),
			
			NSDamSuff = max(SDamSuff, max(0, -Counter) + max(0, -Rebound)),
			NTDamDeal = max(TDamDeal, max(0, -Counter) + max(0, -Counter)),
	            
			NSrcStat = 
				SrcStat#battle_status {
					hp = AttNewHp, 
					mp = AttNewMp, 
					damage_deal = NSDamDeal,
					damage_suffer = NSDamSuff,
					is_alive = (AttNewHp > 0)
				},
			
			NTarStat = 
				TarStat#battle_status {
					hp = DefNewHp, 
					mp = DefNewMp, 
					damage_deal = NTDamDeal,
					damage_suffer = NTDamSuff,
					is_alive = (DefNewHp > 0)
				},
			BattleData1 = set_battle_status(Src, NSrcStat, BattleData),
			NBattleData = set_battle_status(Tar, NTarStat, BattleData1)
	end,
	handle_attack_info(SkillId, Src, [NAttInfo | AttInfoList], Rest, NBattleData).

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
	%% Eff  = AssSpec#assist_spec.eff,
	%% AttInfo here does not contain the full information,
	%% we simply put the hp_inc and | or mp_inc, is_crit, is_miss in it
	%% and the other field will be adjust through the function handle_attack_info
	IsMiss = random:uniform() > Rate,
	AttInfo =
		#attack_info {
			pos     = Tar,			  
			is_crit = false,
			is_miss = IsMiss
		},
	
	assist(SkillID, Src, Rest, [AttInfo | AttInfoList], BattleData).
	
%% 	if (IsMiss == true) ->
%% 			assist(SkillId, Src, Rest, [AttInfo | AttInfoList], BattleData);
%% 	   (Eff == []) ->
%% 		   	assist(SkillId, Src, Rest, AttInfoList, BattleData);
%% 	   true ->   
%% 		    NAttInfo = assist_1(Tar, Eff, AttInfo, BattleData),
%% 			assist(SkillId, Src, Rest, [NAttInfo | AttInfoList], BattleData)
%% 	end.
	
assist_1(_Tar, [], AttInfo, _BattleData) ->
	AttInfo;

assist_1(Tar, [Eff | Rest], AttInfo, BattleData) ->
	{Type, Value, ByRate} = Eff,
	TarStat  = get_battle_status(Tar, BattleData),
	Buffs    = TarStat#battle_status.buff, 
	NAttInfo = 
		case Type of
			heal ->
				Inc = 
					if (ByRate == true) ->
						round(TarStat#battle_status.hp * Value);
					true ->
						Value
					end,
				Inc1 = 
					case lists:keysearch(?BUFF_WEAKNESS, #buff.name, Buffs) of
						{value, #buff{by_rate = true, value = V}} -> round(Inc * (1 - V));
						false -> Inc
					end,
				AttInfo#attack_info {hp_inc = Inc1};
			mana -> 
				Inc = 
					if (ByRate == true) ->
						round(TarStat#battle_status.mp * Value);
					true ->
						Value
					end,
				AttInfo#attack_info {mp_inc = Inc};
			hp_absorb -> %% absorb !!
				Inc = 
					if (ByRate == true) ->
						round(TarStat#battle_status.hp * Value);
					true ->
						Value
					end,
				AttInfo#attack_info {hp_inc = -Inc, hp_absorb = Inc};
			_ -> %% buff or other skills are not relative to hp, mp
				AttInfo
		end,
	assist_1(Tar, Rest, NAttInfo, BattleData).

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
			Value    = if (ByRate == false) ->
					       round(Buff#buff.value); 
					   true -> 
						   round(Buff#buff.value * 100) 
					   end,
			
			{BuffInfo, NState} = 
				case BuffName of
					?BUFF_TOXIC ->
						%% toxic lose hp per turn
						?INFO(battle, "handling buff toxic.."),
						HpMax  = State#battle_status.hp_max,
						Hp     = State#battle_status.hp,
						HpLost = get_hp_lose_value(HpMax, Buff),
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
				end,
			NBuffList = if (Duration == 0) -> BuffList; true -> [NBuff | BuffList] end, 
			settle_buff(Settle, Pos, Rest, NBuffList, [BuffInfo | BuffInfoList], NState)
	end.			


-spec do_att_buff (Src, AttSpec, IsMiss, TarList, BattleData) -> #battle_data{} when
	Src        :: integer(),
	AttSpec    :: #attack_spec{},
	IsMiss     :: boolean(),
	TarList    :: [integer()],    %% TarList is used in adding the debuff list
	BattleData :: #battle_data{}.

%% do_buff will do:
%% (1) settle the buffs which are marked with post
%% (2) add the debuff to the targets in the TarList
%% (3) add the buff to the source

do_att_buff(Src, AttSpec, IsMiss, TarList, BattleData) -> 
	BattleData1 = settle_buff(post, Src, BattleData),
	BuffSpec = 
		case IsMiss orelse BattleData#battle_data.type == ?BATTLE_TYPE_BOSS of
			false ->
				[{Src, AttSpec#attack_spec.buff}] ++ 
				[{Tar, AttSpec#attack_spec.debuff} || Tar <- TarList];
			true ->
				[]
		end,
	
	?INFO(battle, "Src = ~w, BuffSpec = ~w", [Src, BuffSpec]),
	_BattleData2 = do_add_buff(BuffSpec, [], BattleData1).

-spec do_ass_buff (Src, AssSpecList, BattleData) -> #battle_data{} when
	Src         :: integer(),
	AssSpecList :: [#assist_spec{}],
	BattleData  :: #battle_data{}.
	

do_ass_buff(Src, AssSpecList, BattleData) ->
	BattleData1 = settle_buff(post, Src, BattleData),
	BuffSpec = [{AssSpec#assist_spec.pos, AssSpec#assist_spec.buff} || AssSpec <- AssSpecList],
	
	_BattleData2 = do_add_buff(BuffSpec, [], BattleData1).
	
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
	case random:uniform() > Rate of
		true ->
			?INFO(battle, "miss??, Buff = ~w, Rate = ~w, Op = ~w", [Buff, Rate, Op]),
			update_buffs(Pos, Rest, BuffInfoList, BattleData);
		false ->
			State  = get_battle_status(Pos, BattleData),
			ByRate = Buff#buff.by_rate,
			Value  = if (ByRate == false) -> 
						round(Buff#buff.value); 
					 true -> 
						round(Buff#buff.value * 100) 
					 end,
			
			case Op of
				add ->
					BuffInfo = 
						#buff_info {
							%% this field is always set to post, to let the client 
							%% show the buff after the player using his skill
							settle    = post,                   
							name      = Buff#buff.name,
							owner     = Pos,
							hp        = State#battle_status.hp,
							mp        = State#battle_status.mp,	
							hp_inc    = 0,
							mp_inc    = 0,
							is_new    = true,
							by_rate   = ByRate,
							value     = Value,
							duration  = Buff#buff.duration,
							is_remove = false
						},
					NBattleData = add_buff(Buff, Pos, BattleData),
					update_buffs(Pos, Rest, [BuffInfo | BuffInfoList], NBattleData);
				
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


is_command_set(BattleData) ->
	IDList = get_ids(BattleData),
	F = fun(ID) ->
			PInfo = get_player_info(ID, BattleData),
			Lead  = PInfo#player_info.lead,
			Bs    = get_battle_status(Lead, BattleData),
			Bs#battle_status.cmd =/= ?UNDEFINED
		end,
	lists:all(F, IDList).

%% get_buff_target finds out which player will add buff to
get_buff_target(AttInfoList) ->
	get_buff_target(AttInfoList, []).

get_buff_target([], TarList) -> TarList;
get_buff_target([AttInfo | Rest], TarList) ->
	case AttInfo#attack_info.is_miss of
		false -> get_buff_target(Rest, [AttInfo#attack_info.pos | TarList]);
		_ -> get_buff_target(Rest, TarList)
	end.

add_buff(Buff, Pos, BattleData) ->
	State = get_battle_status(Pos, BattleData),
	BList = State#battle_status.buff,
	NState = 
		case is_buff_exist(Buff, Pos, BattleData) of
			false -> 
				State#battle_status {buff = [Buff | BList]};
			true ->
				NBList = lists:keyreplace(Buff#buff.name, #buff.name, BList, Buff),
				State#battle_status {buff = NBList}
		end,
	set_battle_status(Pos, NState, BattleData).

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
			%% ?INFO(battle, "Pos = ~w", [P]),
			State    = get_battle_status(P, BattleData),
			%% ?INFO(battle, "State = ~w", [State]),
			Speed    = State#battle_status.speed,
			%% ?INFO(battle, "Speed = ~w", [Speed]),
			{F1, I1} = get_buff_value(?BUFF_SPEED_UP, P, BattleData),
			%% ?INFO(battle, "F = ~w, I = ~w", [F1, I1]),
			{F2, I2} = get_buff_value(?BUFF_SPEED_DOWN, P, BattleData),
			NSpeed   = round(Speed * (1 + F1 - F2) + I1 - I2),
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
	{F1, _I} = get_buff_value(?BUFF_SCORN, Pos, BattleData),
	if (F1 =/= 0) ->
		?INFO(value, "Damage is reduced ~w% by scorn buff. ", [F1 * 100]);
	true ->
		ok
	end,
	
	{F2, I2} = get_buff_value(?BUFF_DMAAGE_SUB, Pos, BattleData),
	round(Dm * (1 - F1) * (1 - F2) - I2).

%========================================================================================================
% skill transform handler
%========================================================================================================

%% -spec transform_skill(Skills) -> {ActiceSkills, PassiveSkills}.
transform_skill(Skills) ->
	transform_skill(Skills, [], []).

transform_skill([], ActiveSkills, PassiveSkills) ->
	{ActiveSkills, PassiveSkills};

transform_skill([Elem | Rest], ActiveSkills, PassiveSkills) ->
	SkillID = 
		case Elem of
			{_, S, _} -> S; %% skill from role
			_ -> Elem       %% skill from monster
		end,
	
	SkillInfo = data_skill:skill_info(SkillID),
	Type      = SkillInfo#skill_info.type,
	Effect    = SkillInfo#skill_info.effect,
	
	if (Effect == ?SKILL_EFFECT_BATTLE) ->		
		if (Type =/= 2 andalso Type =/= 3) ->
			transform_skill(Rest, [SkillID | ActiveSkills], PassiveSkills);
		true ->
			transform_skill(Rest, ActiveSkills, [SkillID | PassiveSkills])
		end;
	true ->
		transform_skill(Rest, ActiveSkills, PassiveSkills)
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
	IDList = get_ids(BattleData),
	lists:foreach(fun (ID) -> 
		catch scene:set_scene_state(ID, ?SCENE_STATE_BATTLE, 0) end, 
	IDList),

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
	Bin = pt_20:write(20001, BattleData),
	IDList = get_ids(BattleData),
	send_group_package(Bin, IDList).

%% pt 20003
send_order_package(BattleData) ->
	Bin = pt_20:write(20003, BattleData),
	IDList = get_ids(BattleData),
	send_group_package(Bin, IDList).

%% pt 20005
send_result_package(BattleData) ->
	BinIDList = pt_20:write(20005, BattleData),
	send_player_package(BinIDList).

%% pt 20007 auto set command
send_command_package(Pos, SkillID, BattleData) ->
	Bin = pt_20:write(20007, {Pos, SkillID}),
	IDList = get_ids(BattleData),
	send_group_package(Bin, IDList).

%% send cd_package will only send to one player!
send_cd_package(Pos, BattleData) ->
	Bin = pt_20:write(20006, {Pos, BattleData}),
	ID = pos_to_id(Pos, BattleData),
	send_group_package(Bin, [ID]).

send_player_package(BinIDList) ->
	F = fun({ID, Bin}) ->
			case ets:lookup(?ETS_ONLINE, ID) of
				[] -> ok;
				[#ets_online {send_pid = SendPid}] ->
					lib_send:send(SendPid, Bin)
			end
		end,
	lists:foreach(F, BinIDList).

send_group_package(Bin, IDList) ->
	F = fun(ID) ->
			case ets:lookup(?ETS_ONLINE, ID) of
				[] -> ok;
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
    IDList = get_ids(att, BattleData),
	MonID  = BattleData#battle_data.monster,

	if (not is_integer(MonID)) -> 
		#battle_award {};
	true ->
		MonGroup     = data_mon_group:get(MonID),
		IDList       = get_ids(att, BattleData),
		Exp          = MonGroup#mon_group.exp,
		Items        = MonGroup#mon_group.items,
		Silver       = MonGroup#mon_group.silver,
		DropType     = MonGroup#mon_group.drop_type,
		DispatchList = get_items_dispatch(IDList, DropType, Items),
		
		?INFO(battle, "Items = ~w, Silver = ~w, DispatchList = ~w", [Items, Silver, DispatchList]),
		#battle_award {
			gold   = 0,
			silver = Silver,
			donate = 0,
			exp    = Exp,
			items  = DispatchList				   
		}
	end.

get_battle_hp_list(Winner, BattleData) ->
	case Winner of
		att -> get_battle_hp_list(1, ?BATTLE_FIELD_SIZE div 2 + 1, [], BattleData);
		def -> get_battle_hp_list(?BATTLE_FIELD_SIZE div 2 + 1, ?BATTLE_FIELD_SIZE + 1, [], BattleData)
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
	get_items_dispatch(Rest, DropType, ItemInfo, [{Id, Items} | DspList]).

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
		send_player_package(BinIDList);
	false ->
		ok
	end.

%% send battle award to server!
send_battle_award(ID, BattleData) ->
	Award  = BattleData#battle_data.award,
	Exp    = Award#battle_award.exp,
	Items  = Award#battle_award.items,
	Silver = Award#battle_award.silver,
	
	?INFO(battle, "sending battle award, Award = ~w, Items = ~w", [Award, Items]),

	%% add practice
	%% TODO: Add LogType
	MerList = get_mer_list(ID, BattleData),
	G = fun(Pos) ->
			Stat = get_battle_status(Pos, BattleData),
			mod_role:add_exp(ID, {Stat#battle_status.id, Exp}, ?EXP_FROM_BATTLE)
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
	?INFO(battle,"createItems, PlayerID = ~w, Itemlist = ~w", [ID, IDItems]),
	
	catch mod_economy:add_silver(ID, Silver, ?SILVER_FROM_MONSTER),
	catch mod_items:createItems(ID, IDItems, ?ITEM_FROM_BATTLE).
		
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
			type    = 0,
			att_id  = ID,
			att_mer = [],
			monster = MonID
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
    BattleData.

start_round_hook(BattleData) ->
    %% Everything that should be done before a new round starts
    mutate_attack(BattleData).

mutate_attack(BattleData) ->
    Round = BattleData#battle_data.round,
    case lists:member(Round, [5, 11, 16, 21, 26]) of
        true ->
            LastRate = 1 + ((Round - 11) div 5) * 0.25,
            NewRate = LastRate + 0.25,
            NBattleData = lists:foldl(
                fun(Pos, BD) ->
                    case get_battle_status(Pos, BD) of
                        ?UNDEFINED -> BD;
                        State ->
                            NewPAtt = erlang:round(State#battle_status.p_att / LastRate * NewRate),
                            NewMAtt = erlang:round(State#battle_status.m_att / LastRate * NewRate),
                            ?INFO(battle_dbg, "Pos = ~w, p_att = ~w, NewPAtt = ~w", 
                                  [Pos, State#battle_status.p_att, NewPAtt]),
                            ?INFO(battle_dbg, "Pos = ~w, m_att = ~w, NewMAtt = ~w", 
                                  [Pos, State#battle_status.m_att, NewMAtt]),
                            NState = State#battle_status{
                                p_att = NewPAtt,
                                m_att = NewMAtt
                            },
                            set_battle_status(Pos, NState, BD)
                    end
                end,
                BattleData,
                lists:seq(1, ?BATTLE_FIELD_SIZE)),

            Packet = pt_20:write(20011, erlang:round(NewRate * 100) - 100),
            send_group_package(Packet, get_ids(NBattleData)),
            NBattleData;

        false ->
            BattleData
    end.

