-module(skill).
-include("common.hrl").
-export([handle_skill/4, handle_skill/6]).

-define(p1, erlang:element(1, Param)).
-define(p2, erlang:element(2, Param)).
-define(p3, erlang:element(3, Param)).
-define(p4, erlang:element(4, Param)).
-define(p5, erlang:element(5, Param)).


get_skill_id_level(SkillUID) ->
	SkillID = SkillUID div 1000,
	Level   = SkillUID rem 1000,
	{SkillID, Level}.

get_skill_uid(SkillID, Level) ->
	SkillID * 1000 + Level.

-spec pre_handle_skill(Src, BattleData) -> #battle_data{} when 
	Src        :: integer(),
	BattleData :: #battle_data{}.

pre_handle_skill(Src, BattleData) ->
	State  = battle:get_battle_status(Src, BattleData),
	AttPro = 
		#attack_pro {
			skillid = 0,
			pos = Src,
			hp = State#battle_status.hp,
			mp = State#battle_status.mp	
		},
	
	?INFO(skill, "Src = ~w, hp = ~w", [Src, State#battle_status.hp]),
	
	BattleData1  = battle:add_attack_pro(AttPro, BattleData),
	_BattleData2 = battle:settle_buff(pre, Src, BattleData1).

%% handle_skill/4
-spec handle_skill(SkillUID, Src, Tar, BattleData) -> #battle_data{} when
	SkillUID   :: integer(),
	Src        :: integer(),
	Tar        :: integer(),
	BattleData :: #battle_data{}.

handle_skill(0, Src, 0, BData) -> %% for faint, just generate a 'blank' structure
	?INFO(battle, "Fainting..."),
	BattleData  = pre_handle_skill(Src, BData), 
	BattleData1 = battle:settle_buff(post, Src, BattleData),
	battle:update_cd(Src, 0, 0, BattleData1);

handle_skill(SkillUID, Src, Tar, BData) ->
	?INFO(battle, "SkillUID = ~w", [SkillUID]),
	BattleData = pre_handle_skill(Src, BData),
	case battle:is_battle_end(BattleData) of
		{true, _} -> BattleData;
		false ->
			{SkillId, Level} = get_skill_id_level(SkillUID),	
			SrcStat  = battle:get_battle_status(Src, BattleData),
			Skill    = data_skill_table:get(SkillId, Level),
			HpCost   = Skill#battle_skill.hp, 
			Cd       = Skill#battle_skill.cd,
			Param    = Skill#battle_skill.param,
			Hp       = max(1, round(SrcStat#battle_status.hp * (1 - HpCost))),
			MpCost   = if (SkillId == 105 orelse SkillId == 110 orelse SkillId == 115) -> 
					       0; 
					   true -> 
						   Skill#battle_skill.mp 
					   end,
			Mp       = max(0, SrcStat#battle_status.mp - MpCost),
		
			%% update hp and mp
			SrcStat     = battle:get_battle_status(Src, BattleData),
			NSrcStat    = SrcStat#battle_status {hp = Hp, mp = Mp},
			BattleData1 = battle:set_battle_status(Src, NSrcStat, BattleData),

			AttPro  = battle:get_attack_pro(BattleData),
			NAttPro = 
				AttPro#attack_pro {
					%% set the unique ID here
					skillid = SkillUID,
					pos     = Src,
					hp      = Hp,
					mp      = Mp,
					hp_inc  = Hp - SrcStat#battle_status.hp,
					mp_inc  = Mp - SrcStat#battle_status.mp
				},
	
			BattleData2 = battle:set_attack_pro(NAttPro, BattleData1),
			?INFO(battle, "SkillId = ~w, Src = ~w, Tar = ~w, Level = ~w, Param = ~w", 
				[SkillId, Src, Tar, Level, Param]),
			
			NBattleData = handle_skill(SkillId, Src, Tar, Level, Param, BattleData2),
			%% update cd
			%% first reduce the cd value of each element in the cd list
			%% then add this cd into the list..
			battle:update_cd(Src, SkillUID, Cd, NBattleData)
	end.

%======================================================================================================
% warrior skill
%======================================================================================================

%% 普通攻擊: 
%% 如果有連擊輔助技能可能會觸發多次攻擊!
%% 如果有毒撃辅助技能将会触发中毒
%% 如果有吸血辅助技能将会触发吸血
%% 事实上普通攻击才是牛B的技能

-spec get_passive_skill_buffs(Pos, BattleData) -> {BuffList, DebuffList, PreAddList} when
	Pos        :: integer(),
	BattleData :: #battle_status {},
	BuffList   :: list(),
	DebuffList :: list(),
	PreAddList :: list().			
	
get_passive_skill_buffs(Pos, BattleData) ->
	State = battle:get_battle_status(Pos, BattleData),
	get_passive_skill_buffs(State#battle_status.p_skill, [], [], []).

get_passive_skill_buffs([PSkillUID | Rest], BL, DL, PL) ->
	?INFO(skill, "PSkillUID = ~w", [PSkillUID]),
	{PSkill, Level} = get_skill_id_level(PSkillUID),
	
	Param = data_skill_table:get(PSkill, Level),
	
	case PSkill of
		?PSKILL_POISON -> 
			?INFO(skill, "toxic..."), %% p1 = rate, p2 = hp lose value
			Type = debuff,
			Buff = {#buff {name = ?BUFF_TOXIC, value = ?p2, by_rate = true, settle = pre, duration = 1}, ?p1, add};
		
		?PSKILL_LIFE_DRAIN ->
			?INFO(skill, "hp drain..."), %% p1 = rate, p2 = hp drain value
			Type = pre_add,
			Buff = {#buff {name = ?BUFF_LIFE_DRAIN, value = ?p2, settle = pre, by_rate = true}, ?p1, add};
		_ ->
			Type = none,
			Buff = none
	end,
	
	case Type of
		debuff ->
			get_passive_skill_buffs(Rest, BL, [Buff | DL], PL);
		buff ->
			get_passive_skill_buffs(Rest, [Buff | BL], DL, PL);
		pre_add ->
			get_passive_skill_buffs(Rest, BL, DL, [Buff | PL]);
		_ ->
			get_passive_skill_buffs(Rest, BL, DL, PL)
	end;
					
get_passive_skill_buffs([], BL, DL, PL) ->
	{BL, DL, PL}.

-spec handle_skill(SkillId, Src, Tar, Level, Param, BattleData) -> #battle_data{} when
	SkillId    :: integer(),
	Src        :: integer(),
	Tar        :: integer(),
	Level      :: integer(),
	Param      :: tuple(),
	BattleData :: #battle_data{}.

handle_skill(SkillId = ?SKILL_COMMON_ATTACK, Src, Tar, _Level, _Param, BattleData) ->
	AttCount = 
		case battle:get_passive_skill(?PSKILL_DOUBLE_HIT, Src, BattleData) of
			false -> 1;
			{true, _Level} -> 2
		end,
	
	{BL, DL, PL} = get_passive_skill_buffs(Src, BattleData),
	?INFO(skill, "Src = ~w, DL = ~w", [Src, DL]),
	
	AttSpec = 
		#attack_spec {
			addition = 1.0,		  
			targets  = [Tar],
			buff_add = PL,
			buff     = BL,
			debuff   = DL
		},
	
	?INFO(skill, "Src = ~w, AttSpec = ~w", [Src, AttSpec]),
	
	F = fun(_T, {C, Data}) ->
			if (C == false) ->
				{false, Data};
			true ->
				%% we must use attack/5 here to avoid settle the buff
				AttInfoList = battle:attack(SkillId, Src, AttSpec, [Tar], Data), 			
				Data1       = battle:handle_attack_info(SkillId, Src, AttInfoList, Data),
				SrcStat     = battle:get_battle_status(Src, Data1),
				TarStat     = battle:get_battle_status(Tar, Data1),
				
				if (SrcStat#battle_status.is_alive == false orelse 
					TarStat#battle_status.is_alive == false) ->
					{false, Data1};
				true ->
					{true, Data1}
				end
			end
		end,
	{_, NBattleData} = lists:foldl(F, {true, BattleData}, lists:seq(1, AttCount)),
	battle:do_att_buff(Src, AttSpec, false, [Tar], NBattleData);


%% 威震四方: 进行3次物理攻击, 并且随机嘲讽N一个目标, 在嘲讽状态下受到攻击不加怒气
%% N = max(3, (Level + 2) div 3)
%% handle_skill(SkillId = 104, Src, Tar, _Level, Param, BattleData) ->	
%% 	Buff      = #buff{name = ?BUFF_SCORN,   duration = 1, settle = pre, by_rate = true,  value = ?p1},
%% 	Debuff    = #buff{name = ?BUFF_SCORNED, duration = 1, settle = pre, by_rate = false, value = Src},
%% 	
%% 	BuffOps   = [{Buff,   1.0, add}],
%% 	DebuffOps = [{Debuff, 1.0, add}],
%% 	
%% 	TarList   = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
%% 	AttSpec   = 
%% 		#attack_spec {
%% 			addition = 1.0,
%% 			%% targets  = lists:sublist(TarList, min(3, (Level + 2) div 3)),
%% 			targets  = lists:sublist(TarList, 3),
%% 			buff     = BuffOps,
%% 			debuff   = DebuffOps					
%% 		},
%% 	battle:attack(SkillId, Src, AttSpec, BattleData);
handle_skill(SkillId = 104, Src, Tar, Level, Param, BattleData) ->	
	Buff      = #buff{name = ?BUFF_SCORN,   duration = 1, settle = pre, by_rate = true,  value = ?p1},
	Debuff    = #buff{name = ?BUFF_SCORNED, duration = 1, settle = pre, by_rate = false, value = Src},
	
	BuffOps   = [{Buff,   1.0, add}],
	DebuffOps = [{Debuff, 1.0, add}],
	
	TarList   = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
	AttSpec   = 
		#attack_spec {
			addition = 1.0,
			%% targets  = lists:sublist(TarList, min(3, (Level + 2) div 3)),
			targets  = lists:sublist(TarList, 3),
			buff     = [],
			debuff   = []					
		},
	AttInfoList = battle:attack(SkillId, Src, AttSpec, AttSpec#attack_spec.targets, BattleData),
	BattleData1 = battle:handle_attack_info(SkillId, Src, AttInfoList, BattleData),
	
	TarList1 = lists:sublist(TarList, (Level + 2) div 3),
	BuffSpec = [{Src, BuffOps} | lists:map(fun(Pos) -> {Pos, DebuffOps} end, TarList1)],
	
	battle:do_add_buff(BuffSpec, [], BattleData1);
		

%% 坚若磐石: 使己方全体加物防和法防 
handle_skill(SkillId = 105, Src, _Tar, _Level, Param, BattleData) ->
	Buffs = [#buff{name = ?BUFF_DMAAGE_SUB, duration = 2, value = ?p1, by_rate = true, settle = pre}],
		
	?INFO(skill, "value = ~w", [?p1]),
	
	BuffOps = 
		[{Buff, 1.0, add} || Buff <- Buffs],
	
	FriendList = 
		battle:get_target_list(battle:calc_range(Src, ?ALLFRIENDLY), BattleData),
	
	AssistSpec = 
		[
			#assist_spec {
				pos = P, 
				eff = [], 
				buff = BuffOps
			} || P <- FriendList
		],
	
	battle:assist(SkillId, Src, AssistSpec, BattleData);

%% 背水一战: 消耗自己的气血, 如果命中则对敌人造成较强的伤害
handle_skill(SkillId = 106, Src, Tar, _Level, Param, BattleData) ->
	%% hp -2000 for testing
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets = [Tar]			  
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);


%% 浴血狂击, 对敌人进行一次物理攻击, 如果命中则吸取伤害(吸血)
handle_skill(SkillId = 107, Src, Tar, _Level, Param, BattleData) ->
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			buff_add = [#buff{name = ?BUFF_LIFE_DRAIN, value = ?p2, by_rate = true}],
			targets  = [Tar]						
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);


%% 战意激荡: 将自己的防御转化为攻击
handle_skill(SkillId = 108, Src, _Tar, _Level, Param, BattleData) ->
	Buff = #buff {name = ?BUFF_FRENZY, duration = 2, settle = pre, value = ?p1, by_rate = true},
	AssSpec = 
		[
			#assist_spec {
				pos  = Src, 
				eff  = [], 
				buff = [{Buff, 1.0, add}]
			}      
		],
	battle:assist(SkillId, Src, AssSpec, BattleData);

%======================================================================================================
% fighter skill
%======================================================================================================

%% 霸刃连斩: 对同一个目标连续攻击N次, N视等级而决定
handle_skill(SkillId = 109, Src, Tar, Level, Param, BattleData) ->		
	F = fun(N, {C, Data}) ->
			if (C == false) ->
				{false, Data};	   
			true ->
				Addition =
					case N of
						1 -> ?p1;
						2 -> ?p2;
						3 -> ?p3;
						4 -> ?p4
					end,
				
				AttSpec = 
					#attack_spec {
						targets  = [Tar],
						addition = Addition
					},
				
				%% we must use attack/5 here to avoid settle the buff
				AttInfoList = battle:attack(SkillId, Src, AttSpec, [Tar], Data), 			
				Data1   = battle:handle_attack_info(SkillId, Src, AttInfoList, Data),
				SrcStat = battle:get_battle_status(Src, Data1),
				TarStat = battle:get_battle_status(Tar, Data1),
				
				?INFO(skill, "Tar Hp = ~w", [TarStat#battle_status.hp]),
	
				if (SrcStat#battle_status.is_alive == false orelse 
					TarStat#battle_status.is_alive == false) ->
					{false, Data1};
				true ->
					{true, Data1}
				end
			end
		end,
	{_, NBattleData} = lists:foldl(F, {true, BattleData}, lists:seq(1, min(4, 1 + (Level + 2) div 3))),
	%% no buff to add
	battle:do_att_buff(Src, #attack_spec{}, true, [], NBattleData);


%% 横扫千军: 对敌方3个目标进行攻击
handle_skill(SkillId = 110, Src, Tar, _Level, Param, BattleData) ->
	List = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
	NList = lists:sublist(List, 3),
	
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = NList			  
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 对敌人进行一次物理攻击，如果命中，则提高自己一定百分比的暴击，持续一定回合。
handle_skill(SkillId = 111, Src, Tar, _Level, Param, BattleData) ->
	Buff = #buff {
		name     = ?BUFF_CRIT_UP,
		duration = ?p3,
		value    = ?p2,
		by_rate  = true
	},
	
	AttSpec = #attack_spec {
		addition = ?p1,
		targets  = [Tar],
		buff     = [{Buff, 1.0, add}]			  
	},

	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 乘胜追击: 自动攻击对面气血最少的单位，如果成功杀死目标，
%% 则会对剩下单位中气血最少的单位进行一次追击，级别越高，追击伤害越高。
handle_skill(SkillId = 112, Src, Tar0, _Level, Param, BattleData) ->
	Targets0 = battle:get_target_list(battle:calc_range(Tar0, ?ALLFRIENDLY), BattleData),
	TarStat0 = battle:get_battle_status(Tar0, BattleData),
	Hp0 = TarStat0#battle_status.hp,
	
	%% find out whose hp is minimum.
	F = fun(N, {Pos, Hp}) ->
			Stat = battle:get_battle_status(Pos, BattleData),
			Hp1 = Stat#battle_status.hp,
			
			if (Stat#battle_status.is_lead) ->
					{Pos, Hp};
			   (Hp1 < Hp) ->
					{N, Hp1};
				true ->
					{Pos, Hp}
			end
		end,
	{Tar, _} = lists:foldl(F, {Tar0, Hp0}, Targets0),
				
	AttSpec = 
		#attack_spec {
			addition = ?p1,				
			targets = [Tar]		
		},
	
	AttInfoList = battle:attack(SkillId, Src, AttSpec, [Tar], BattleData),
	BattleData1 = battle:handle_attack_info(SkillId, Src, AttInfoList, BattleData),
	
	TarStat = battle:get_battle_status(Tar, BattleData1),
	SrcStat = battle:get_battle_status(Src, BattleData1),
	
	Targets = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData1),
	
	case TarStat#battle_status.is_alive == false andalso 
		 SrcStat#battle_status.is_alive == true  andalso 
		 Targets =/= [] of							
		false ->
			battle:do_att_buff(Src, AttSpec, true, [], BattleData1);
		true ->
			AttSpec1 = 
				#attack_spec {
					addition = ?p2,
					targets = [hd(Targets)]					 
				},
			_BattleData2 = battle:attack(SkillId, Src, AttSpec1, BattleData1)
	end;

%% 破军之势, 令敌人眩晕2回合
handle_skill(SkillId = 113, Src, Tar, _, Param, BattleData) ->
	Buff = 
		#buff {
			name     = ?BUFF_FAINT,
			by_rate  = false,
			value    = 0,
			duration = 2,
			settle   = pre 
		},
	
	AttSpec = #attack_spec {
		addition = ?p1,
		targets  = [Tar],
		debuff   = [{Buff, 1.0, add}]				
	},
	
	battle:attack(SkillId, Src, AttSpec, BattleData);

%======================================================================================================
% magician skill
%======================================================================================================

%% 龙战八方: 奥义技 ,对敌方N个目标造成一定百分比的伤害, N由等级决定
handle_skill(SkillId = 114, Src, Tar, Level, Param, BattleData) ->
	List = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
	NList = 
		if (Level =< 3) -> lists:sublist(List, 3);
		   (Level =< 6) -> lists:sublist(List, 4);
			true -> List
		end,
	
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = NList		  
		},
	
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 奇门遁甲:	小奥义, 对敌方三个目标进行一次法术攻击，有一定概率附加固定伤害
handle_skill(SkillId = 115, Src, Tar, _Level, Param, BattleData) ->
	List = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
	NList = lists:sublist(List, 3),
	
	Buff = 
		#buff {
			name    = ?BUFF_DAMAGE_ADD,
			value   = ?p3,
			by_rate = false
		},

	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = NList,
			buff_add = [{Buff, 1.0, null}]
		},
	
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 龙落雷: 对敌人进行一次法术攻击，如果命中，则提高自己一定百分比的致命，持续一定回合。
handle_skill(SkillId = 116, Src, Tar, _Level, Param, BattleData) ->
	?INFO(skill, "Param = ~w", [Param]),
	Buff = 
		#buff {
			name     = ?BUFF_FATAL,
			duration = 2,
			settle   = post,
			value    = ?p2,
			by_rate  = true
		},
	
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = [Tar],
			buff     = [{Buff, 1.0, add}]
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 强兵咒: 对敌人进行一次法术攻击, 如果命中则给对方增加一个降低治疗量的BUFF
handle_skill(SkillId = 117, Src, Tar, _Level, Param, BattleData) ->
	Buff = 
		#buff {
			name = ?BUFF_WEAKNESS, 
			duration = 2,
			settle   = pre,
			value    = ?p2,
			by_rate  = true
		},
	
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = [Tar],
			debuff   = [{Buff, 1.0, add}]
		},
	
	battle:attack(SkillId, Src, AttSpec, BattleData);
	
%% 破军咒: 对敌人进行一次法术攻击，必爆。
handle_skill(SkillId = 118, Src, Tar, _Level, Param, BattleData) ->
	Buff = 
		#buff {
			name     = ?BUFF_CRIT,
			duration = 0,
			value    = 0,
			by_rate  = 0   
		},
	
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = [Tar],
			buff_add = [Buff]	  
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 老周1
handle_skill(SkillId = 119, Src, Tar, _Level, _Param, BattleData) ->
	TarList = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
	Tar0    = hd(TarList),
	Hp0     = battle:get_battle_status(Tar0, #battle_status.hp, BattleData),
	
	G = fun(Pos, {P, Hp}) ->
			NHp = battle:get_battle_status(Pos, #battle_status.hp, BattleData),	
			if (NHp < Hp) ->
				{Pos, NHp};
			true ->
				{P, Hp}
			end
		end,
	
	{NTar, _} = lists:foldl(G, {Tar0, Hp0}, tl(TarList)),
	
	Buff = 
		#buff {
			name     = ?BUFF_ATT_UP, 
			by_rate  = true,
			value    = 0.1,
			duration = 1
		},

	AttSpec = 
		#attack_spec {
			addition = 1,
			buff     = [],
			targets  = [NTar]
		},
	
	AttInfoList = battle:attack(SkillId, Src, AttSpec, AttSpec#attack_spec.targets, BattleData),
	BattleData1 = battle:handle_attack_info(SkillId, Src, AttInfoList, BattleData),
	
	FriendList  = battle:get_target_list(battle:calc_range(Src, ?ALLFRIENDLY), BattleData),	
	F = fun(Pos) ->
			{Pos, [{Buff, 1.0, add}]}
		end,
	BuffSpec = lists:map(F, FriendList),
	
	?INFO(skill, "BuffSpec = ~w", [BuffSpec]),
	
	battle:do_add_buff(BuffSpec, [], BattleData1);

%========================================================================================================
% warrior skill
%========================================================================================================

%% 扰乱军心; ==> 威慑4方  Lv1
handle_skill(_SkillId = 223, Src, Tar, _Level, Param, BattleData) ->
	handle_skill(104, Src, Tar, 1, Param, BattleData);

%% 破阵攻心: ==> 威慑4方  Lv9
handle_skill(_SkillId = 224, Src, Tar, _Level, Param, BattleData) ->
	handle_skill(104, Src, Tar, 9, Param, BattleData);


%% 天护之阵: 
handle_skill(SkillId = 225, Src, _Tar, _Level, Param, BattleData) ->
	Buff = 
		#buff {
			name     = ?BUFF_ASSIST,
			value    = ?p1,
			duration = 2,
			settle   = post,
			data     = Src,
			by_rate  = true  
		},
	TeamList = battle:get_target_list(battle:calc_range(Src, ?ALLFRIENDLY), BattleData),
	AssSpecList = 
		[
			#assist_spec {
				pos = Pos,			  
				buff = [{Buff, 1.0, add}]   
			} || Pos <- TeamList
		],
	battle:assist(SkillId, Src, AssSpecList, BattleData);
	

%% 镇守;
handle_skill(SkillId = 226, Src, Tar, _Level, Param, BattleData) ->
	Buff = 
		#buff {
			name     = ?BUFF_BLOCK_UP,	   
			value    = ?p2,
			duration = 2,
			settle   = post,
			by_rate  = true
		},
	
	AttSpec = 
		#attack_spec {
			addition = ?p1,			  
			targets  = [Tar],
			buff     = [{Buff, 1.0, add}]  
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 怒袭;
handle_skill(SkillId = 227, Src, Tar, _Level, Param, BattleData) ->
	Buff = 
		#buff {
			name     = ?BUFF_MANA_DRAIN,	   
			value    = {100, 100},
			duration = 0,
			by_rate  = 0 
		},
	
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = [Tar],
			buff_add = [Buff]
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 复仇 牺牲自己一定百分比的气血, 对目标造成大量的伤害
handle_skill(SkillId = 228, Src, Tar, _Level, Param, BattleData) ->
	AttSpec = 
		#attack_spec {
			addition = ?p1,			  
			targets  = [Tar]
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);
	
%% 吸血--好技能不解释
handle_skill(SkillId = 229, Src, Tar, _Level, Param, BattleData) ->
	Buff = 
		#buff {
			name    = ?BUFF_LIFE_DRAIN,
			value   = ?p2,
			by_rate = true
		},
	
	AttSpec = 
		#attack_spec {
			addition = ?p1,			  
			targets  = [Tar],
			buff_add = [Buff]
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);
  

%========================================================================================================
% fighter skill
%========================================================================================================

%% 绝杀, 给敌人造成一次沉重的打击
handle_skill(SkillId = 230, Src, Tar, _Level, Param, BattleData) ->
	AttSpec = 
		#attack_spec {
			addition = ?p1,			  
			targets  = [Tar]
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 追魂之刃: 对目标连续攻击2次, 伤害不递减
handle_skill(SkillId = 231, Src, Tar, _Level, Param, BattleData) ->
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = [Tar]			  
		},
	
	F = fun(_T, {C, Data}) ->
			if (C == false) ->
				{false, Data};
			true ->
				AttInfoList = battle:attack(SkillId, Src, AttSpec, [Tar], Data), 			
				Data1   = battle:handle_attack_info(SkillId, Src, AttInfoList, Data),
				SrcStat = battle:get_battle_status(Src, Data1),
				TarStat = battle:get_battle_status(Tar, Data1),
				
				?INFO(skill, "Tar Hp = ~w", [TarStat#battle_status.hp]),
	
				if (SrcStat#battle_status.is_alive == false orelse 
					TarStat#battle_status.is_alive == false) ->
					{false, Data1};
				true ->
					{true, Data1}
				end
			end
		end,
	{_, NBattleData} = lists:foldl(F, {true, BattleData}, lists:seq(1, 2)),
	battle:do_att_buff(Src, AttSpec, true, [], NBattleData);

%% 虚空一击: 对目标进行一次强力的物理攻击, 命中后有一定几率使目标昏迷一回合
handle_skill(SkillId = 232, Src, Tar, _Level, Param, BattleData) ->
	Buff = 
		#buff {
			name     = ?BUFF_FAINT,	   
			duration = 2,
			settle   = pre
		},
	
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = [Tar],
			debuff   = [{Buff, 0.5, add}]		  
		},
	
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 流云刺: 对目标进行一次物理攻击, 目标气血越低, 附加的伤害值越高
handle_skill(SkillId = 233, Src, Tar, _Level, _Param, BattleData) ->
	AttSpec = 
		#attack_spec {
			addition = 1,
			targets  = [Tar]
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 呼啸破: 以降低自己的防御为代价, 对目标进行一次物理攻击, 必爆击
handle_skill(SkillId = 234, Src, Tar, _Level, Param, BattleData) ->
	BuffDefDown = 
		#buff {
			name     = ?BUFF_PDEF_DOWN,
			value    = ?p1,
			by_rate  = true,
			duration = 1,
			settle   = pre
		},
	BuffCrit =
		#buff {
			name     = ?BUFF_CRIT
		},
	
	AttSpec = 
		#attack_spec {
			addition = ?p2,		  
			targets  = [Tar],
			buff_add = [BuffCrit],
			buff     = [{BuffDefDown, 1.0, add}]
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 连环杀阵: 对敌人攻击3次 每次目标都是随机选取
handle_skill(SkillId = 235, Src, _Tar, _Level, Param, BattleData) ->
	F = fun(_N, {C, Data}) ->
			if (C == false) ->
				{false, Data};
			true ->
				T = ai:get_skill_target(SkillId, Src, Data),
				AttSpec = 
					#attack_spec {
						addition = ?p1,
						targets  = [T]			  
					},
				
				%% we must use attack/5 here to avoid settle the buff
				AttInfoList = battle:attack(SkillId, Src, AttSpec, [T], Data), 			
				Data1       = battle:handle_attack_info(SkillId, Src, AttInfoList, Data),
				SrcStat     = battle:get_battle_status(Src, Data1),
				
				TarList = battle:get_target_list(battle:calc_range(Src, ?ALLENEMY), Data1),
	
				if (SrcStat#battle_status.is_alive == false orelse TarList == []) ->
					{false, Data1};
				true ->
					{true, Data1}
				end
			end
		end,
	{_, NBattleData} = lists:foldl(F, {true, BattleData}, lists:seq(1, 3)),
	battle:do_att_buff(Src, #attack_spec{}, true, [], NBattleData);


%========================================================================================================
% magician skill
%========================================================================================================

%% 冰凌笺  对敌方三个目标造成一定百分比的伤害
handle_skill(SkillId = 236, Src, Tar, _Level, Param, BattleData) ->
	List  = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
	NList = lists:sublist(List, 3),
		   
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = NList		  
		},
	
	battle:attack(SkillId, Src, AttSpec, BattleData);


%% 祭风术 对目标进行一次强力的法术攻击, 如果命中, 则有一定概率降低目标一定量的怒氣值
handle_skill(SkillId = 237, Src, Tar, _Level, Param, BattleData) ->
	Buff = 
		#buff {
			name  = ?BUFF_MANA_DRAIN,
			value = {0, ?p3}	   
		},
	
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = [Tar],
			buff_add = [{Buff, ?p2, none}]
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 分光诀:   随机对地方两个单位进行法术攻击, 针对每个目标的伤害会降低
handle_skill(SkillId = 238, Src, Tar, _Level, Param, BattleData) ->	
	List = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
	NList = lists:sublist(List, 2),
	
	AttSpec = 
		#attack_spec {
			%% addition = [?p1, ?p2],
			addition = ?p1,
			targets  = NList			  
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 凝劲术:   降低自己的命中率随机对敌方3个目标进行一次强力的法术攻击
handle_skill(SkillId = 239, Src, Tar, _Level, Param, BattleData) ->
	List  = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
	NList = lists:sublist(List, 3),

	Buff = 
		#buff {
			name    = ?BUFF_HIT_DOWN,
			value   = ?p1,
			by_rate = true   
		},
	
	AttSpec = 
		#attack_spec {
			addition = ?p2,
			targets  = NList,
			buff_add = [{Buff, 1.0, null}]			  
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);
	

%% 激狂诀:   临时提高自己的暴击进行法术攻击
handle_skill(SkillId = 240, Src, Tar, _Level, Param, BattleData) ->
	Buff = 
		#buff {
			name    = ?BUFF_CRIT_UP,
			value   = ?p1,
			by_rate = true 	   
		},
	AttSpec = 
		#attack_spec {
			addition = ?p2,
			targets  = [Tar],		  
			buff_add = [{Buff, 1.0, null}]
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 狂风划影: 随机对地方三个目标进行一次法术伤害, 命中后有一定概率降低其一半的怒氣值

handle_skill(SkillId = 241, Src, Tar, _Level, Param, BattleData) ->
	List  = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
	NList = lists:sublist(List, 3),
	
	Buff =
		#buff {
			name    = ?BUFF_MANA_DRAIN,
			value   = {0, ?p3},
			by_rate = false
		},
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = NList,
			buff_add = [{Buff, ?p2, null}]	  
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);
	

%========================================================================================================
% doctor skill
%========================================================================================================

%% 三魂回春: 对己方3个目标进行强力治疗并增加目标10点怒气值
handle_skill(SkillId = 242, Src, Tar, _Level, Param, BattleData) ->
	List = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
	NList = lists:sublist(List, 3),
	
	AssSpecList = 
		[
			#assist_spec {
				pos  = P,
				eff  = [{mana, 10, false}, {heal, 3000, false}],
				buff = []		  
			} || P <- NList
		],
	battle:assist(SkillId, Src, AssSpecList, BattleData);


%% 元灵之光: 对己方3个目标进行治疗, 并概率性增加物理防御和法术防御
handle_skill(SkillId = 243, Src, Tar, _Level, Param, BattleData) ->
	List  = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
	NList = lists:sublist(List, 3),
	
	Buffs = [#buff{name = ?BUFF_MDEF_UP, value = 30, by_rate = false}, 
			 #buff{name = ?BUFF_PDEF_UP, value = 30, by_rate = false}],
	
	BuffOps = [{Buff, 0.5, add} || Buff <- Buffs],
	
	AssSpecList = 
		[
			#assist_spec {
				pos  = P,
				eff  = [{heal, 300, false}],
				buff = BuffOps	  
			} || P <- NList
		],
	battle:assist(SkillId, Src, AssSpecList, BattleData);


%% 净衣术: 对一个目标进行较强的治疗, 并优先治疗气血较少的单位
handle_skill(SkillId = 244, Src, Tar, _Level, Param, BattleData) ->
	AssSpecList = 
		[
			#assist_spec {
				pos  = Tar,
				eff  = [{heal, 300, false}],
				buff = []			  
			}
		],
	battle:assist(SkillId, Src, AssSpecList, BattleData);

%% 药王经: 对一个目标进行治疗,　增加物理防御和法术防御
handle_skill(SkillId = 245, Src, Tar, _Level, Param, BattleData) ->
	Buffs = [#buff{name = ?BUFF_MDEF_UP, value = 30, duration = 2}, 
			 #buff{name = ?BUFF_PDEF_UP, value = 30, duration = 2}],
	BuffOps = [{Buff, 0.5, add} || Buff <- Buffs],
	
	AssSpecList = 
		[
			#assist_spec {
				pos  = Tar,
				rate = 1.0,
				eff  = [{heal, 300, false}],
				buff = BuffOps
			}
		],
	battle:assist(SkillId, Src, AssSpecList, BattleData);

%% 仙风万里
handle_skill(SkillId = 246, Src, Tar, _Level, Param, BattleData) ->
	Buffs = [#buff{name = ?BUFF_ATT_UP, value = 30, duration = 2}],
	
	BuffOps = [{Buff, 0.5, add} || Buff <- Buffs],
	AssSpecList = 
		[
			#assist_spec {
				pos  = Tar, 
				eff  = [{heal, 300, false}],
				buff = BuffOps
			}
		],
	battle:assist(SkillId, Src, AssSpecList, BattleData);

%% 清心咒: 给随机一个己方目标加一个回血BUFF
handle_skill(SkillId = 247, Src, Tar, _Level, _Param, BattleData) ->
	SrcStat = battle:get_battle_status(Src, BattleData),
	Att     = SrcStat#battle_status.m_att,
	Lev     = SrcStat#battle_status.level,
	V       = 0.6 * math:pow((Att * 0.49 + Lev * 12), 1.02),
	
	Buffs       = [#buff{name = ?BUFF_REFRESH, value = V, by_rate = false, duration = 2}],
	BuffOps     = [{Buff, 1.0, add} || Buff <- Buffs],
	AssSpecList = 
		[
			#assist_spec {
				pos  = Tar,
				eff  = [],
				buff = BuffOps 			  
			}
		],
	battle:assist(SkillId, Src, AssSpecList, BattleData);

%======================================================================================================================
% monster skills
%======================================================================================================================

%% 坚若磐石
handle_skill(_SkillId = 248, Src, Tar, Level, Param, BattleData) ->
	handle_skill(105, Src, Tar, Level, {?p1}, BattleData);

%% 背水一战
handle_skill(_SkillId = 249, Src, Tar, Level, Param, BattleData) ->
	handle_skill(106, Src, Tar, Level, {?p1}, BattleData);

%% 战意激荡
handle_skill(_SkillId = 250, Src, Tar, Level, Param, BattleData) ->
	handle_skill(108, Src, Tar, Level, {?p1}, BattleData);

%% 霸刃连斩
handle_skill(_SkillId = 251, Src, Tar, Level, Param, BattleData) ->
	handle_skill(109, Src, Tar, Level, {?p1, ?p2, ?p3, 0}, BattleData);

%% 横扫千军
handle_skill(_SkillId = 252, Src, Tar, Level, Param, BattleData) ->
	handle_skill(110, Src, Tar, Level, {?p1}, BattleData);

%% 暴怒冲锋
handle_skill(_Skill = 253, Src, Tar, Level, Param, BattleData) ->
	handle_skill(111, Src, Tar, Level, {?p1, ?p2, ?p3}, BattleData);

%% 乘胜追击
handle_skill(_Skill = 254, Src, Tar, Level, Param, BattleData) ->
	handle_skill(112, Src, Tar, Level, Param, BattleData);

%% 破军之势
handle_skill(_Skill = 255, Src, Tar, Level, Param, BattleData) ->
	handle_skill(113, Src, Tar, Level, Param, BattleData);

%% 龙战8方
handle_skill(_Skill = 256, Src, Tar, Level, Param, BattleData) ->
	handle_skill(114, Src, Tar, Level, Param, BattleData);

%% 雷光咒
handle_skill(_Skill = 257, Src, Tar, Level, Param, BattleData) ->
	handle_skill(116, Src, Tar, Level, Param, BattleData);

%% 强兵咒
handle_skill(_Skill = 258, Src, Tar, Level, Param, BattleData) ->
	handle_skill(117, Src, Tar, Level, Param, BattleData);

%% 破军咒
handle_skill(_Skill = 259, Src, Tar, Level, Param, BattleData) ->
	handle_skill(118, Src, Tar, Level, Param, BattleData);

%% 睡眠
handle_skill(SkillId = 261, Src, Tar, _Level, Param, BattleData) ->
	handle_skill(279, Src, Tar, _Level, Param, BattleData);

%% 摧枯拉朽
handle_skill(SkillId = 262, Src, Tar, _Level, Param, BattleData) ->
	TarList = battle:get_target_list(
		battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),

	AttSpec = 
		#attack_spec {
			addition = ?p1,			  
			targets  = TarList
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 大地震击
handle_skill(SkillId = 263, Src, Tar, _Level, Param, BattleData) ->
	TarList = battle:get_target_list(
		battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
	Buff = 
		#buff {
			name = ?BUFF_FAINT,	   
			duration = ?p2,
			settle = pre			
		},
	AttSpec = 
		#attack_spec {
			addition = ?p1,			  
			targets = TarList,
			debuff = [{Buff, 1.0, add}]
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 血性饥渴
handle_skill(SkillId = 270, Src, Tar, _Level, Param, BattleData) ->
	Buff = 
		#buff {
			name    = ?BUFF_LIFE_DRAIN,   
			value   = ?p2,
			by_rate = true
		},
	AttSpec = 
		#attack_spec {
			addition = ?p1,		  
			targets  = [Tar],
			buff_add = [{Buff, 1.0, none}]  
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);
	
	
%% 华光普照	治疗己方全体50% 的气血
handle_skill(SkillId = 271, Src, _Tar, _Level, Param, BattleData) ->
	TarList = battle:get_target_list(
				battle:calc_range(Src, ?ALLFRIENDLY), BattleData),
	
	AssSpecList = 
		[
			#assist_spec {pos = T, rate = 1.0, eff = [{heal, ?p1, true}], buff = []} || 
		T <- TarList],
	
	battle:assist(SkillId, Src, AssSpecList, BattleData);
	
%% 疯狂狙击
handle_skill(_SkillId = 272, Src, Tar, _Level, Param, BattleData) ->
	handle_skill(112, Src, Tar, _Level, {?p1, ?p1}, BattleData);


%% 刺钉护盾
handle_skill(SkillId = 275, Src, Tar, _Level, Param, BattleData) ->
	Buff = #buff {name = ?BUFF_REBOUND, value = ?p1, duration = ?p2, settle = pre, by_rate = true},
	BuffOps = [{Buff, 1.0, add}],
	AssSpecList = 
		[
			#assist_spec {pos = Tar, rate = 1.0, eff = [], buff = BuffOps} 
		],
	battle:assist(SkillId, Src, AssSpecList, BattleData);


%% 背水一战
handle_skill(_SkillId = 276, Src, Tar, Level, Param, BattleData) ->
	handle_skill(106, Src, Tar, Level, {?p1}, BattleData);


%% 吸取: 直接吸掉对方一定百分比的血
handle_skill(SkillId = 277, Src, Tar, _Level, Param, BattleData) ->
	AssSpecList = 
		[
			#assist_spec {pos = Tar, eff = [{hp_absorb, ?p1, true}], buff = []} 
		],
	battle:assist(SkillId, Src, AssSpecList, BattleData);
	
%% 睡眠
handle_skill(SkillId = 279, Src, Tar, _Level, _Param, BattleData) ->
	Buff = #buff {name = ?BUFF_FAINT, value = 0, duration = 2, settle = pre},
	BuffOps = [{Buff, 0.3, add}],
	AssSpecList = [ #assist_spec {pos = Tar, eff = [], buff = BuffOps} ],
	battle:assist(SkillId, Src, AssSpecList, BattleData);


%% 重击 $$ 强攻
handle_skill(SkillId, Src, Tar, _Level, Param, BattleData)
  	when SkillId =:= 280; 
		 SkillId =:= 281 ->
	
	handle_skill(230, Src, Tar, _Level, {?p1}, BattleData);

%======================================================================================================================
% spare skills
%======================================================================================================================

%% 补血
handle_skill(SkillId = 106, Src, _Tar, _, _Param, BattleData) ->
	AssistSpec = 
		[
		 	#assist_spec {pos = Src, eff = [{heal, 100, false}], buff = []}
		],
	battle:assist(SkillId, Src, AssistSpec, BattleData);

%% 反击
handle_skill(SkillId = 1107, Src, Tar, _, _Param, BattleData) ->
	BuffList = 
		[
		 	%% #buff {name = ?BUFF_LIFE_DRAIN, duration = 2, value = 100, settle = pre}
			   #buff {name = ?BUFF_COUNTER, duration = 2, value = 100, settle = pre}
			%% #buff {name = ?BUFF_REBOUND, duration = 2, value = 50, settle = pre}
		],
	
	AttSpec = #attack_spec {
		addition = 1,
		targets  = [Tar],
		buff     = [{Buff, 1.0, add} || Buff <- BuffList]				
	},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 连击+群攻: 对同一个目标连续攻击N次, N视等级而决定
handle_skill(SkillId = 1109, Src, Tar, _Level, _Param, BattleData) ->
	AttSpec = 
		#attack_spec {
			addition = 1,
			targets = battle:get_target_list(
				battle:calc_range(Tar, ?ALLFRIENDLY), BattleData)
		},
		
	F = fun(_T, {C, Data}) ->
			if (C == false) ->
				{false, Data};
			true ->
				%% we must use attack/5 here to avoid settle the buff
				TarList = battle:get_target_list(
							battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
				
				AttInfoList = battle:attack(SkillId, Src, AttSpec, TarList, Data), 
									
				Data1       = battle:handle_attack_info(SkillId, Src, AttInfoList, Data),
				SrcStat     = battle:get_battle_status(Src, Data1),
				TarStat     = battle:get_battle_status(Tar, Data1),
				
				?INFO(skill, "Tar Hp = ~w", [TarStat#battle_status.hp]),
	
				if (SrcStat#battle_status.is_alive == false orelse 
					TarStat#battle_status.is_alive == false) ->
					{false, Data1};
				true ->
					{true, Data1}
				end
			end
		end,
	{_, NBattleData} = lists:foldl(F, {true, BattleData}, lists:seq(1, 3)),
	battle:do_att_buff(Src, AttSpec, true, [], NBattleData);


%% default
handle_skill(_SkillId, Src, Tar, _Level, _Param, BattleData) ->
	AttSpec = 
		#attack_spec {
			addition = 1,
			targets = [Tar]		  
		},
	battle:attack(11, Src, AttSpec, BattleData).


















