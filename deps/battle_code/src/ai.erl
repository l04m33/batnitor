%%----------------------------------------------------------------------------
%% @Module : mod_ai
%% @Author : xwz
%% @Description : Battle AI and Validation
%%----------------------------------------------------------------------------

-module(ai).
-include("common.hrl").
-export([
	get_skill_by_ai/3, 
	validate_skill/3,
	get_skill/2,
	get_skill_target/3]).

-type ai_attr() :: hp | mp | p_att | m_att | p_def | m_def.
-type ai_cmp()  :: lt | gt | le | ge | max | min.
-type ai_job()  :: integer().
-type ai_spec() :: {ai_attr(), ai_cmp(), integer() | float()}
			| {buff, integer(), boolean()} | {ai_attr(), max | min}.

-record(battle_ai, 
	{
		id                     :: integer(),
		rate     = 1.0         :: float(),
		num      = 1           :: integer(), 
		pos      = ?UNDEFINED  :: ?UNDEFINED | [integer()],         
		job      = ?UNDEFINED  :: ai_job(), 
	 	tar      = enemy       :: self | enemy | friend,
		tar_spec = []          :: [ai_spec()],
		con      = []          
	}		
). 

%===================================================================================================
% Skill Validation
%===================================================================================================

%% SkillID is unique skill id 6-digits number
-spec validate_skill(SkillId, Src, BattleData) -> boolean() when
	SkillId    :: integer(),
	Src        :: integer(),
	BattleData :: #battle_data{}.

validate_skill(SkillUID, Src, BattleData) ->
	SrcStat = battle:get_battle_status(Src, BattleData),
	case is_learn_skill(SkillUID, SrcStat#battle_status.skill) of
		{true, {Skill, Level}} ->
			validate_skill(SkillUID, Skill, Level, Src, BattleData);
		false ->
			?INFO(battle, "Player has not learned this skill"),
			false
	end.

validate_skill(SkillUID, SkillId, Level, Src, BattleData) ->
	case data_skill_table:get(SkillId, Level) of
		?UNDEFINED  -> false;
		BattleSkill ->
			Hp       = BattleSkill#battle_skill.hp,
			Mp       = BattleSkill#battle_skill.mp,
			Stat     = battle:get_battle_status(Src, BattleData),
			_ByRate  = BattleSkill#battle_skill.hp_by_rate,
			InCd     = 
				%% CDList containes the Unique Skill ID 
				case lists:keysearch(SkillUID, 1, Stat#battle_status.cd) of
					false -> false;
					{value, _} -> true
				end,
			
			if (Stat#battle_status.mp < Mp)  -> 
				   ?INFO(skill, "Mp not enough. "),
				   false;
			   (Stat#battle_status.hp =< Hp) -> 
				   ?INFO(skill, "Hp not enough. "),
				   false; 
			   (InCd == true) -> 
				   ?INFO(skill, "in CD state. CD list = ~w", [Stat#battle_status.cd]),
				   false;
			   true -> true
			end
	end.

-spec is_learn_skill(SkillUID, SkillList) -> false | {true, {SkillID, Level}} when
	SkillUID  :: integer(),
	SkillID   :: integer(),
	Level     :: integer(),
	SkillList :: [{integer(), integer()}].
	
is_learn_skill(SkillId, [S | Rest]) ->
	if (SkillId == S) ->
		{true, {S div 1000, S rem 1000}};
	true ->
		is_learn_skill(SkillId, Rest)
	end;
	
is_learn_skill(_SkillId, []) ->
	false.
	
%===================================================================================================
% AI handling
%===================================================================================================

-spec get_skill(integer(), #battle_data{}) -> 
	{SkillUID :: integer(), Src :: integer(), Tar :: integer(), Index :: integer()}.
				
get_skill(Src, BattleData) -> 
	SrcStat = battle:get_battle_status(Src, BattleData),
	SList   = SrcStat#battle_status.skill,
	SIndex  = SrcStat#battle_status.skill_index,
	SLen    = length(SList),
	
	?INFO(ai, "Src = ~w, SList = ~w", [Src, SList]),
	
	case get_super_skill(SList) of
		false ->
			get_skill_1(SIndex, SIndex, SLen, SList, Src, BattleData);
		{true, SkillUID} ->
			case validate_skill(SkillUID, Src, BattleData) of
				true  -> 
					Tar = get_skill_target(SkillUID, Src, BattleData),
					{SkillUID, Src, Tar, SIndex};
				false ->
                    ?INFO(ai, "Skill ~w not valid", [SkillUID]),
					get_skill_1(SIndex, SIndex, SLen, SList, Src, BattleData)
			end
	end.

get_super_skill([]) -> false;
get_super_skill([SkillUID | Rest]) ->
	Skill = data_skill:skill_info(SkillUID),
	%% super skill 
	if (Skill#skill_info.type == ?SKILL_SPECIAL) ->
		{true, SkillUID};
	true ->
		get_super_skill(Rest)
	end.

get_skill_1(SIndex, OldIndex, SLen, _SList, Src, BattleData) when SIndex >= OldIndex + SLen ->
	Tar = get_skill_target(?SKILL_COMMON_ATTACK, Src, BattleData),
	{?SKILL_COMMON_ATTACK, Src, Tar, 1};

get_skill_1(SIndex, OldIndex, SLen, SList, Src, BattleData) ->
	SkillUID = lists:nth((SIndex - 1) rem SLen + 1, SList),
	case validate_skill(SkillUID, Src, BattleData) of
		true ->
			Tar = get_skill_target(SkillUID, Src, BattleData),
			{SkillUID, Src, Tar, SIndex rem SLen + 1};
		false ->
            ?INFO(ai, "Skill ~w not valid", [SkillUID]),
			get_skill_1(SIndex + 1, OldIndex, SLen, SList, Src, BattleData)
	end.


%============================================================================================================
% target handling
%============================================================================================================

get_skill_target(SkillUID, Src, BattleData) ->
	?INFO(ai, "SkillUID = ~w", [SkillUID]),
	Skill = data_skill_table:get(SkillUID div 1000, 1),
	
	case Skill#battle_skill.target of
		self -> 
			Src;
		friend ->
			Camp = if (Src =< ?BATTLE_FIELD_SIZE div 2) -> att; true -> def end,
			get_random_target(Camp, BattleData);
		enemy ->
			Camp = if (Src =< ?BATTLE_FIELD_SIZE div 2) -> def; true -> att end,
			get_random_target(Camp, BattleData)
	end.
		

%% choose a random target from the BattleData of the camp
-spec get_random_target(Camp, BattleData) -> Tar when
	Camp :: battle_camp(),
	Tar :: integer(),
	BattleData :: #battle_data{}.

get_random_target(Camp, BattleData) ->
	List = 
		case Camp of
			att ->
				battle:get_target_list(
				 	battle:calc_range(1, ?ALLFRIENDLY), BattleData);
			def ->
				battle:get_target_list(
					battle:calc_range(?BATTLE_FIELD_SIZE div 2 + 1, ?ALLFRIENDLY), BattleData)
		end,
	Len = length(List),
	lists:nth(random:uniform(Len), List).


%=========================================================================================================
% old version of AI handling (obsolete)
%=========================================================================================================


-spec get_skill_by_ai(Src, ID, BattleData) -> {Sid, Src, Tar} when
	Src :: integer(),
	Tar :: integer(),
	ID  :: integer(), %% Role ID or monster ID
	Sid :: integer(),
	BattleData :: #battle_data{}.

get_skill_by_ai(Src, ID, BattleData) ->
	?INFO(battle, "get_skill_by_ai: Src = ~w", [Src]),
	SrcStat = battle:get_battle_status(Src, BattleData),
	AIList = 
		case data_ai:get(ID) of
		?UNDEFINED -> 
			SkillList = SrcStat#battle_status.skill,
			F = fun(SkillUID, Acc) ->
					Skill  = data_skill_table:get(SkillUID div 1000, 1),
					Target = Skill#battle_skill.target,
					
					case Target of
						enemy ->
							[
								#battle_ai {id = SkillUID, tar = enemy, rate = 0.6, job = 1},
						 		#battle_ai {id = SkillUID, tar = enemy} |
								Acc
							];
						_Other ->
							[ #battle_ai {id = SkillUID, tar = Target} | Acc]
					end
				end, 
			%% SkillList has the format[{SkillId, Level}]
			lists:foldl(F, [], SkillList);
		_AI -> _AI
		end,
	
	%% ?INFO(battle, "AIList = ~w", [AIList]),
	parse_skills(AIList, Src, BattleData).

filter_target(Pos, Job, Num, TarList, BattleData) ->
	%% (1) filter by the specified position
	NTarList = 
		case Pos of
			?UNDEFINED -> TarList;
			_  -> [T || T <- TarList, lists:member(T, Pos)]
		end,
	
	%% (2) then filter by number
	if (length(NTarList) < Num) ->
			[];
		(Job == ?UNDEFINED) ->
			NTarList;
		true ->
			%% (3) then by job
			filter_target_1(Job, [], NTarList, BattleData)
	end.

%% if no skill is suitable, then
%% common attack is used...
parse_skills([], Src, BattleData) -> 
	%% all the skill is not suitable, 
    %% choose the common attack skill
	Tar = 
		case Src =< ?BATTLE_FIELD_SIZE div 2 of
			true ->
				get_random_target(def, BattleData);
			false ->
				get_random_target(att, BattleData)
		end,
	{?SKILL_COMMON_ATTACK, Src, Tar};

parse_skills([Skill | Rest], Src, BattleData) ->
	ID      = Skill#battle_ai.id,
	Num     = Skill#battle_ai.num,
	Job     = Skill#battle_ai.job,
	Pos     = Skill#battle_ai.pos,
	TarType = Skill#battle_ai.tar,
	
	case validate_skill(ID, Src, BattleData) of
		false -> parse_skills(Rest, Src, BattleData);
		true  ->
			TarList = 
				case TarType of
					enemy  -> battle:get_target_list(battle:calc_range(Src, ?ALLENEMY), BattleData);
					friend -> battle:get_target_list(battle:calc_range(Src, ?ALLFRIENDLY), BattleData);
					self   -> [Src]
				end,
			%% ?INFO(ai, "TarList = ~w", [TarList]),
			
			%% filter...
			NTarList = filter_target(Pos, Job, Num, TarList, BattleData),
			%% ?INFO(ai, "NTarList = ~w", [TarList]),
			
			case NTarList of
				[] -> parse_skills(Rest, Src, BattleData);
				_  -> 
					Res = parse_skill_target(Skill, NTarList, BattleData),
					%% ?INFO(ai, "parse result = ~w", [Res]),
					case Res of
						{true, Tar} -> {ID, Src, Tar};
						false -> parse_skills(Rest, Src, BattleData)
					end
			end
	end.

-spec parse_skill_target(Skill, TarList, BattleData) -> {true, Tar} | false when
	Skill      :: #battle_ai{},
	TarList    :: [integer()],
	Tar        :: integer(),
	BattleData :: #battle_data{}.
		  
		  
parse_skill_target(Skill, TarList, BattleData) ->
	L = parse_skill_target(Skill#battle_ai.tar_spec, [TarList], TarList, BattleData),
	Len = length(L),
	
	case Len of
		0 -> false;
		_ -> {true, lists:nth(random:uniform(Len), L)}
	end.

parse_skill_target([TarSpec | Rest], ListofList, TarList, BattleData)  ->
	Candidate = parse_skill_target_1(TarSpec, [], TarList, TarList, BattleData),
	parse_skill_target(Rest, [Candidate | ListofList], TarList, BattleData);

parse_skill_target([], ListofList, _TarList,  _BattleData) ->
	intersect(ListofList).

-spec parse_skill_target_1(TarSpec, PassedList, RestList, UnchangedList, BattleData) -> [integer()] when
	TarSpec       :: ai_spec(),
	PassedList    :: [integer()],
	RestList      :: [integer()],
	UnchangedList :: [integer()],
	BattleData    :: #battle_data{}.
	
		  
parse_skill_target_1(TarSpec, PassedList, [T | Rest], TarList, BattleData) ->
	case check_tar_spec(TarSpec, T, TarList, BattleData) of
		true ->
			parse_skill_target_1(TarSpec, [T | PassedList], Rest, TarList, BattleData);
		false ->
			parse_skill_target_1(TarSpec, PassedList, Rest, TarList, BattleData)
	end;

parse_skill_target_1(_TarSpec, PassedList, [], _TarList, _BattleData) ->
	PassedList.
		

%% filter according to the specified job
filter_target_1(_Job, List, [], _BattleData) -> List;

filter_target_1(Job, List, [Tar | Rest], BattleData) ->
	TarStat = battle:get_battle_status(Tar, BattleData),
	
	if (TarStat#battle_status.job == Job) ->
		filter_target_1(Job, [Tar | List], Rest, BattleData);
	true ->
		filter_target_1(Job, List, Rest, BattleData)
	end.

check_tar_spec(TarSpec, T, TarList, BattleData) ->
	case TarSpec of
		{_, max}     -> check_tar_stat(TarSpec, T, TarList, BattleData);
 		{_, min}     -> check_tar_stat(TarSpec, T, TarList, BattleData);
		{buff, _, _} -> check_buff_stat(TarSpec, T, BattleData);
		{_, _, _}    -> check_value_stat(TarSpec, T, BattleData)
	end.

check_tar_stat(TarSpec, T, TarList, BattleData) ->
	TarStat = battle:get_battle_status(T, BattleData),
	{Attr, State} = TarSpec,
	
	F = fun(Bs) ->
			{L, R} = 
				case Attr of
					hp    -> {TarStat#battle_status.hp,    Bs#battle_status.hp};
					mp    -> {TarStat#battle_status.mp,    Bs#battle_status.mp};
					p_att -> {TarStat#battle_status.p_att, Bs#battle_status.p_att};
					p_def -> {TarStat#battle_status.p_def, Bs#battle_status.p_def};
					m_att -> {TarStat#battle_status.m_att, Bs#battle_status.m_att};
					m_def -> {TarStat#battle_status.m_def, Bs#battle_status.m_def}
				end,
			if (State == min) ->
				if (L =< R) -> true; true -> false end;
			true ->
				if (L >= R) -> true; true -> false end
			end
		end,
			
	lists:all(F, TarList).
	
check_value_stat(TarSpec, T, BattleData) ->
	TarStat = battle:get_battle_status(T, BattleData),
	{Attr, Pred, Value} = TarSpec,
	
	{L, R} = 
		case Attr of
			hp    -> {TarStat#battle_status.hp,    Value};
		    mp    -> {TarStat#battle_status.mp,    Value};
			p_att -> {TarStat#battle_status.p_att, Value};
			p_def -> {TarStat#battle_status.p_def, Value};
			m_att -> {TarStat#battle_status.m_att, Value};
			m_def -> {TarStat#battle_status.m_def, Value}
		end,
	
	case Pred of
		le -> if (L =< R) -> true; true -> false end;
		ge -> if (L >= R) -> true; true -> false end;		
		lt -> if (L < R)  -> true; true -> false end;
		gt -> if (L > R)  -> true; true -> false end
	end.

check_buff_stat(TarSpec, T, BattleData) ->
	TarStat = battle:get_battle_status(T, BattleData),
	Buff = TarStat#battle_status.buff,
	{buff, Name, Exist} = TarSpec,
	
	IsExist = 
		case lists:keysearch(Name, #buff.name, Buff) of
			false -> false;
			_ -> true
		end,
	
	if (IsExist == Exist) -> 
		true;
	true ->
		false
	end.

intersect(List1, List2) ->
	[X || X <- List1, lists:member(X, List2)].

%% List is a list of list
intersect([]) -> [];
intersect([List]) -> List;
intersect([List1, List2 | Rest]) ->
	intersect([intersect(List1, List2) | Rest]).







































































