-module(skill).
-include("common.hrl").
-export([handle_skill/5, handle_skill/6, get_skill_id_level/1, get_skill_uid/2]).


-define(p1, begin 
                ?BATTLE_LOG("~s: ~w", ["参数#1", erlang:element(1, Param)]), 
                erlang:element(1, Param)
            end).
-define(p2, begin 
                ?BATTLE_LOG("~s: ~w", ["参数#2", erlang:element(2, Param)]),
                erlang:element(2, Param)
            end).
-define(p3, begin 
                ?BATTLE_LOG("~s: ~w", ["参数#3", erlang:element(3, Param)]), 
                erlang:element(3, Param)
            end).
-define(p4, begin 
                ?BATTLE_LOG("~s: ~w", ["参数#4", erlang:element(4, Param)]), 
                erlang:element(4, Param)
            end).
-define(p5, begin 
                ?BATTLE_LOG("~s: ~w", ["参数#5", erlang:element(5, Param)]), 
                erlang:element(5, Param)
            end).
-define(p6, begin 
                ?BATTLE_LOG("~s: ~w", ["参数#6", erlang:element(6, Param)]), 
                erlang:element(6, Param)
            end).
-define(p7, begin 
                ?BATTLE_LOG("~s: ~w", ["参数#7", erlang:element(7, Param)]), 
                erlang:element(7, Param)
            end).
-define(p8, begin 
                ?BATTLE_LOG("~s: ~w", ["参数#8", erlang:element(8, Param)]), 
                erlang:element(8, Param)
            end).


get_skill_id_level(SkillUID) ->
	SkillID = SkillUID div 1000,
	Level   = SkillUID rem 1000,
	{SkillID, Level}.

get_skill_uid(SkillID, Level) ->
	SkillID * 1000 + Level.

-spec pre_handle_skill(Src, BattleData, SkillStat) -> #battle_data{} when 
	Src        :: integer(),
    SkillStat  :: integer(),
	BattleData :: #battle_data{}.

pre_handle_skill(Src, BattleData, SkillStat) ->
	State  = battle:get_battle_status(Src, BattleData),
	AttPro = 
		#attack_pro {
			skillid = 0,
			pos = Src,
			hp = State#battle_status.hp,
			mp = State#battle_status.mp,
            skill_stat = SkillStat
		},

	?INFO(skill, "Src = ~w, hp = ~w", [Src, State#battle_status.hp]),

	BattleData1  = battle:add_attack_pro(AttPro, BattleData),
	_BattleData2 = battle:settle_buff(pre, Src, BattleData1).

%% handle_skill/4
-spec handle_skill(SkillUID, SkillStat, Src, Tar, BattleData) -> #battle_data{} when
    SkillUID   :: integer(),
    SkillStat  :: integer(),
	Src        :: integer(),
	Tar        :: integer(),
	BattleData :: #battle_data{}.

handle_skill(0, SkillStat, Src, 0, BData) -> %% for faint, just generate a 'blank' structure
	?INFO(battle, "Fainting..."),
	BattleData  = pre_handle_skill(Src, BData, SkillStat), 
	BattleData1 = battle:settle_buff(post, Src, BattleData),
	battle:update_cd(Src, 0, 0, BattleData1);

handle_skill(SkillUID, SkillStat, Src, Tar, BData) ->
	?INFO(battle, "SkillUID = ~w", [SkillUID]),
    ?BATTLE_LOG("~n--------- 攻击者站位: ~w, 技能 ID: ~w ---------", [Src, SkillUID]),
	BattleData = pre_handle_skill(Src, BData, SkillStat),
    SrcStat = battle:get_battle_status(Src, BattleData),
    %% 这里不用判断整场战斗是否结束，只要判断当前角色有没挂就好了，
    %% 因为pre_handle_skill里的操作（目前）只能影响到当前角色，
    %% 而战斗结束的判断在battle模块调用handle_skill之后会做
    case SrcStat#battle_status.is_alive of
        true ->
            {SkillId, Level} = get_skill_id_level(SkillUID),	
            Skill    = data_skill_table:get(SkillId, Level),
            HpCost   = Skill#battle_skill.hp, 
            Cd       = Skill#battle_skill.cd,
            Param    = Skill#battle_skill.param,
            Hp       = max(1, round(SrcStat#battle_status.hp * (1 - HpCost))),
            MpCost   = Skill#battle_skill.mp,
            Mp       = max(0, SrcStat#battle_status.mp - MpCost),
        
            %% update hp and mp
            NSrcStat    = SrcStat#battle_status {hp = Hp, mp = Mp},
            ?BATTLE_LOG("更新攻击者消耗, 血: ~w / ~w / ~w, 怒气: ~w / ~w / ~w",
                        [SrcStat#battle_status.hp, Hp, Hp - SrcStat#battle_status.hp,
                         SrcStat#battle_status.mp, Mp, Mp - SrcStat#battle_status.mp]),
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
            
            %% XXX: 策划需求，按职业选择目标（暂时的，下周改回来）
            NTar = battle:get_tar_for_common_attack(Src, Tar, BattleData2),

            NBattleData = handle_skill(SkillId, Src, NTar, Level, Param, BattleData2),
            %% update cd
            %% first reduce the cd value of each element in the cd list
            %% then add this cd into the list..
            battle:update_cd(Src, SkillUID, Cd, NBattleData);

        false ->
            %% 在pre_handle_skill里的时候，Src有可能因为Buff直接挂掉
            %% 这时attack_pro里已经有buff_info了，可以说明Src已经挂掉，但是需要
            %% 客户端配合处理才行……
            ?BATTLE_LOG("Oops, 攻击者已经挂了……"),
            AttPro  = battle:get_attack_pro(BattleData),
            NAttPro = AttPro#attack_pro {
                skillid = 0,
                pos     = Src,
                hp      = 0,
                mp      = SrcStat#battle_status.mp,
                hp_inc  = 0,
                mp_inc  = 0
            },
            BattleData1 = battle:set_attack_pro(NAttPro, BattleData),
            BattleData2 = battle:settle_buff(post, Src, BattleData1),       % XXX: 有必要吗……
            battle:update_cd(Src, 0, 0, BattleData2)
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

    Param = (data_skill_table:get(PSkill, Level))#battle_skill.param,

	case PSkill of
		?PSKILL_POISON -> 
			?INFO(skill, "toxic..."), %% p1 = rate, p2 = hp lose value
			Type = debuff,
			Buff = {#buff {name = ?BUFF_TOXIC, value = ?p2, by_rate = true, settle = pre, duration = 1}, ?p1, add};
		
		?PSKILL_LIFE_DRAIN ->
			?INFO(skill, "hp drain..."), %% p1 = rate, p2 = hp drain value
			Type = pre_add,
			Buff = {#buff {name = ?BUFF_LIFE_DRAIN, value = ?p2, by_rate = true}, ?p1, add};

        ?PSKILL_CALM ->
            Type = pre_add,
            Buff = {#buff {name = ?BUFF_MANA_DRAIN, value = {0, ?p1}, by_rate = false}, ?p2, add};

		_ ->
			Type = none,
			Buff = none
	end,

    {B, Rate, Op} = case Type of
        none -> {none, -1, add};
        _    -> Buff
    end,
    BuffRand = random:uniform(),

    case Type of
        none -> void;
        _ ->
            ?BATTLE_LOG("被动技能 ~w, 几率: ~w, 随机数: ~w, 生效: ~w",
                        [PSkillUID, Rate, BuffRand, BuffRand =< Rate])
    end,

    case BuffRand =< Rate of
        true ->
            case Type of
                debuff ->
                    ?BATTLE_LOG("    被动技能添加Debuff: ~s", [battle:buff_type_to_str(B#buff.name)]),
                    ?BATTLE_LOG("        系数: ~w, 持续回合数: ~w", [B#buff.value, B#buff.duration]),
                    get_passive_skill_buffs(Rest, BL, [{B, 1.0, Op} | DL], PL);
                buff ->
                    get_passive_skill_buffs(Rest, [Buff | BL], DL, PL);
                pre_add ->
                    ?BATTLE_LOG("    被动技能添加临时Buff: ~s", [battle:buff_type_to_str(B#buff.name)]),
                    ?BATTLE_LOG("        系数: ~w", [B#buff.value]),
                    get_passive_skill_buffs(Rest, BL, DL, [B | PL])
            end;

        _ ->        % false
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

%% {攻击系数}
handle_skill(SkillId = ?SKILL_COMMON_ATTACK_ID, Src, Tar, _Level, Param, BattleData) ->
    %% XXX: 外面已经选过了，暂时不用重复选
    % NTar = battle:get_tar_for_common_attack(Src, Tar, BattleData),

    RealTarList = get_real_tar_list(Tar),

	{AttCount, AttRate1, AttRate2} = 
		case battle:get_passive_skill(?PSKILL_DOUBLE_HIT, Src, BattleData) of
			false -> 
                {1, ?p1, 0};
			{true, DHSkillInfo} -> 
                _DHLevel = DHSkillInfo#battle_skill.level,
                {Rate, A1, A2} = DHSkillInfo#battle_skill.param,
                DHRand = random:uniform(),
                ?BATTLE_LOG("被动技能 ~w, 几率: ~w, 随机数: ~w, 生效: ~w",
                            [?PSKILL_DOUBLE_HIT * 1000 + _DHLevel, Rate, DHRand, DHRand =< Rate]),
                case DHRand =< Rate of
                    true ->
                        ?BATTLE_LOG("    被动技能 ~w 生效, 攻击系数1: ~w, 攻击系数2: ~w", 
                                    [?PSKILL_DOUBLE_HIT * 1000 + _DHLevel, A1, A2]),
                        {2, A1, A2};
                    _ ->    % false
                        {1, ?p1, 0}
                end
		end,
	
	{BL, DL, PL} = get_passive_skill_buffs(Src, BattleData),
	?INFO(skill, "Src = ~w, DL = ~w", [Src, DL]),
	
	AttSpec = 
		#attack_spec {
			targets  = RealTarList,
			buff_add = PL,
			buff     = BL,
			debuff   = DL
		},
	
	?INFO(skill, "Src = ~w, AttSpec = ~w", [Src, AttSpec]),
	
	F = fun(T, {C, Data}) ->
			if (C == false) ->
				{false, Data};
			true ->
                NAttSpec = AttSpec#attack_spec {
                    addition = element(T, {AttRate1, AttRate2})
                },
				%% we must use attack/5 here to avoid settle the buff
				AttInfoList = battle:attack(SkillId, Src, NAttSpec, RealTarList, Data), 			
				Data1       = battle:handle_attack_info(SkillId, Src, AttInfoList, Data),
				SrcStat     = battle:get_battle_status(Src, Data1),

                FF = fun(TT) ->
                    TStat = battle:get_battle_status(TT, Data1),
                    not TStat#battle_status.is_alive
                end,
				
                case SrcStat#battle_status.is_alive =:= false 
                        orelse lists:all(FF, RealTarList) of
                    true ->
                        {false, Data1};
                    false ->
                        {true, Data1}
				end
			end
		end,
	{_, NBattleData} = lists:foldl(F, {true, BattleData}, lists:seq(1, AttCount)),
	battle:do_att_buff(Src, AttSpec, false, RealTarList, NBattleData);


%% 背水一战: 消耗自己的气血, 如果命中则对敌人造成较强的伤害
%% {攻击系数}
handle_skill(SkillId = 401, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets = RealTarList
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);


%% 战意激荡: 将自己的防御转化为攻击
%% {防御转换系数}
handle_skill(SkillId = 402, Src, _Tar, _Level, Param, BattleData) ->
	Buff = #buff {name = ?BUFF_FRENZY, duration = 2, settle = post, value = ?p1, by_rate = true},
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

%% 对敌人进行一次物理攻击，如果命中，则提高自己一定百分比的暴击，持续一定回合。
%% {攻击系数, 暴击增加系数, 持续回合数}
handle_skill(SkillId = 403, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

	Buff = #buff {
		name     = ?BUFF_CRIT_UP,
		duration = ?p3,
		value    = ?p2,
		by_rate  = true,
        settle   = post
	},
	
	AttSpec = #attack_spec {
		addition = ?p1,
		targets  = RealTarList,
		buff     = [{Buff, 1.0, add}]			  
	},

	battle:attack(SkillId, Src, AttSpec, BattleData);

%======================================================================================================
% magician skill
%======================================================================================================

%% 强兵咒: 对敌人进行一次法术攻击, 如果命中则给对方增加一个降低治疗量的BUFF
%% {攻击系数, 降低治疗量系数}
handle_skill(SkillId = 404, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

	Buff = 
		#buff {
			name = ?BUFF_WEAKNESS, 
			duration = 2,
			settle   = post,
			value    = ?p2,
			by_rate  = true
		},
	
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = RealTarList,
			debuff   = [{Buff, 1.0, add}]
		},
	
	battle:attack(SkillId, Src, AttSpec, BattleData);
	
%% {攻击系数, 攻击提升系数}
handle_skill(SkillId = 119, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ ->
            TarList = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
            {NTar, _} = battle:get_pos_by(hp, min, TarList, BattleData),
            [NTar]
    end,

	Buff = 
		#buff {
			name     = ?BUFF_ATT_UP, 
			by_rate  = true,
			value    = ?p2,
			duration = 1,
            settle   = post
		},

	AttSpec = 
		#attack_spec {
			addition = 1,
			buff     = [],
			targets  = RealTarList
		},
	
	AttInfoList = battle:attack(SkillId, Src, AttSpec, AttSpec#attack_spec.targets, BattleData),
	BattleData1 = battle:handle_attack_info(SkillId, Src, AttInfoList, BattleData),
	
	FriendList  = battle:get_target_list(battle:calc_range(Src, ?ALLFRIENDLY), BattleData),	
    BuffSpec    = [{Pos, [{Buff, 1.0, add}]} || Pos <- FriendList],
	
	battle:settle_and_add_buff(Src, BuffSpec, [], BattleData1);

%========================================================================================================
% warrior skill
%========================================================================================================

%% 扰乱军心 ==> 威慑4方  Lv1
%% {攻击系数, 伤害减少系数}
handle_skill(SkillId = 223, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

	Buff      = #buff{name = ?BUFF_SCORN,   duration = 1, settle = post, by_rate = true,  value = ?p2},
	Debuff    = #buff{name = ?BUFF_SCORNED, duration = 1, settle = post, by_rate = false, value = Src},

	BuffOps   = [{Buff,   1.0, add}],
	DebuffOps = [{Debuff, 1.0, add}],

	AttSpec   = 
		#attack_spec {
			addition = ?p1,
			targets  = RealTarList,
			buff     = [],
			debuff   = []
		},
	AttInfoList = battle:attack(SkillId, Src, AttSpec, AttSpec#attack_spec.targets, BattleData),
	BattleData1 = battle:handle_attack_info(SkillId, Src, AttInfoList, BattleData),

    HitList = battle:get_hit_list(AttInfoList, BattleData1),

	BuffSpec = [{Src, BuffOps} | lists:map(fun(Pos) -> {Pos, DebuffOps} end, HitList)],

	battle:settle_and_add_buff(Src, BuffSpec, [], BattleData1);

%% 雄军云集
%% {攻击系数, 伤害减少系数}
handle_skill(SkillId = 405, Src, Tar, _Level, Param, BattleData) ->
	Buff      = #buff{name = ?BUFF_SCORN,   duration = 1, settle = post, by_rate = true,  value = ?p2},
	Debuff    = #buff{name = ?BUFF_SCORNED, duration = 1, settle = post, by_rate = false, value = Src},

	BuffOps   = [{Buff,   1.0, add}],
	DebuffOps = [{Debuff, 1.0, add}],

    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ -> 
            TarList   = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
            util:get_rand_list_elems(TarList, 2)
    end,

	AttSpec   = 
		#attack_spec {
			addition = ?p1,
			targets  = RealTarList,
			buff     = [],
			debuff   = []
		},
	AttInfoList = battle:attack(SkillId, Src, AttSpec, AttSpec#attack_spec.targets, BattleData),
	BattleData1 = battle:handle_attack_info(SkillId, Src, AttInfoList, BattleData),

    HitList = battle:get_hit_list(AttInfoList, BattleData1),

	BuffSpec = [{Src, BuffOps} | lists:map(fun(Pos) -> {Pos, DebuffOps} end, HitList)],

	battle:settle_and_add_buff(Src, BuffSpec, [], BattleData1);

%% 破阵攻心 ==> 威慑4方  Lv9
%% {攻击系数, 伤害减少系数}
handle_skill(SkillId = 224, Src, Tar, _Level, Param, BattleData) ->
	Buff      = #buff{name = ?BUFF_SCORN,   duration = 1, settle = post, by_rate = true,  value = ?p2},
	Debuff    = #buff{name = ?BUFF_SCORNED, duration = 1, settle = post, by_rate = false, value = Src},

	BuffOps   = [{Buff,   1.0, add}],
	DebuffOps = [{Debuff, 1.0, add}],

    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ ->
            TarList   = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
            util:get_rand_list_elems(TarList, 2)
    end,

	AttSpec   = 
		#attack_spec {
			addition = ?p1,
			targets  = RealTarList,
			buff     = [],
			debuff   = []
		},
	AttInfoList = battle:attack(SkillId, Src, AttSpec, AttSpec#attack_spec.targets, BattleData),
	BattleData1 = battle:handle_attack_info(SkillId, Src, AttInfoList, BattleData),

    HitList = battle:get_hit_list(AttInfoList, BattleData1),

	BuffSpec = [{Src, BuffOps} | lists:map(fun(Pos) -> {Pos, DebuffOps} end, HitList)],

	battle:settle_and_add_buff(Src, BuffSpec, [], BattleData1);


%% 天护之阵: 
%% {分担伤害系数}
handle_skill(SkillId = 225, Src, _Tar, _Level, Param, BattleData) ->
	Buff = 
		#buff {
			name     = ?BUFF_ASSIST,
			value    = ?p1,
			duration = 2,
			settle   = post,
			by_rate  = true
		},

	PBuff = 
		#buff {
			name     = ?BUFF_ASSISTED,
			value    = ?p1,
			duration = 2,
			settle   = post,
			data     = Src,
			by_rate  = true
		},

	TeamList = battle:get_target_list(battle:calc_range(Src, ?ALLFRIENDLY), BattleData),
    TeamTarList = lists:filter(fun(P) -> P =/= Src end, TeamList),
	AssSpecList = 
		[
            #assist_spec {pos = Src, buff = [{Buff, 1.0, add}]} |
			[#assist_spec{pos = Pos, buff = [{PBuff, 1.0, add}]} || Pos <- TeamTarList]
		],
	battle:assist(SkillId, Src, AssSpecList, BattleData);


%% 镇守;
%% {攻击系数, 增加格挡系数}
handle_skill(SkillId = 226, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

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
			targets  = RealTarList,
			buff     = [{Buff, 1.0, add}]
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 怒袭;
%% {攻击系数, 吸怒气点数}
handle_skill(SkillId = 227, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

	Buff = 
		#buff {
			name     = ?BUFF_MANA_DRAIN,	   
			value    = {?p2, ?p2},
			by_rate  = false
		},
	
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = RealTarList,
			buff_add = [Buff]
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 复仇 牺牲自己一定百分比的气血, 对目标造成大量的伤害
%% {攻击系数}
handle_skill(SkillId = 228, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

	AttSpec = 
		#attack_spec {
			addition = ?p1,			  
			targets  = RealTarList
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);
	
%% 吸血--好技能不解释
%% {攻击系数, 吸血系数}
handle_skill(SkillId = 229, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

	Buff = 
		#buff {
			name    = ?BUFF_LIFE_DRAIN,
			value   = ?p2,
			by_rate = true
		},
	
	AttSpec = 
		#attack_spec {
			addition = ?p1,			  
			targets  = RealTarList,
			buff_add = [Buff]
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);
  

%========================================================================================================
% fighter skill
%========================================================================================================

%% 绝杀, 给敌人造成一次沉重的打击
%% {攻击系数}
handle_skill(SkillId = 230, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

	AttSpec = 
		#attack_spec {
			addition = ?p1,			  
			targets  = RealTarList
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 追魂之刃: 对目标连续攻击2次, 伤害不递减
%% {攻击系数}
handle_skill(SkillId = 231, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = RealTarList		  
		},
	
	F = fun(_T, {C, Data}) ->
			if (C == false) ->
				{false, Data};
			true ->
				AttInfoList = battle:attack(SkillId, Src, AttSpec, RealTarList, Data), 			
				Data1   = battle:handle_attack_info(SkillId, Src, AttInfoList, Data),
				SrcStat = battle:get_battle_status(Src, Data1),

                FF = fun(TT) ->
                    TStat = battle:get_battle_status(TT, Data1),
                    not TStat#battle_status.is_alive
                end,

                case SrcStat#battle_status.is_alive =:= false
                        orelse lists:all(FF, RealTarList) of
                    true ->
                        {false, Data1};
                    false ->
                        {true, Data1}
				end
			end
		end,
	{_, NBattleData} = lists:foldl(F, {true, BattleData}, lists:seq(1, 2)),
	battle:do_att_buff(Src, AttSpec, true, [], NBattleData);

%% 虚空一击: 对目标进行一次强力的物理攻击, 命中后有一定几率使目标昏迷一回合
%% {攻击系数, 晕概率, 持续回合数}
handle_skill(SkillId = 232, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

	Buff = 
		#buff {
			name     = ?BUFF_FAINT,	   
			duration = ?p3,
			settle   = post
		},
	
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = RealTarList,
			debuff   = [{Buff, ?p2, add}]		  
		},
	
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 流云刺: 对目标进行一次物理攻击, 目标气血越低, 附加的伤害值越高
%% {}
handle_skill(SkillId = 233, Src, Tar, _Level, _Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),
    %% XXX: 这里只按照第一个目标的血量来计算……
    FirstTar = hd(RealTarList),

    SrcStat = battle:get_battle_status(Src, BattleData),
    TarStat = battle:get_battle_status(FirstTar, BattleData),
    Job = SrcStat#battle_status.job,
	Att0 =
		case (Job == ?CAREER_HUWEI) orelse (Job == ?CAREER_MENGJIANG) of
			true  -> SrcStat#battle_status.p_att;
			false -> SrcStat#battle_status.m_att
		end,
    Att = battle:get_adjust_value(att, Att0, Src, BattleData),

    ExtraDmg = erlang:round((2.2 - TarStat#battle_status.hp / TarStat#battle_status.hp_max) * Att),

	AttSpec = 
		#attack_spec {
			addition = 1,
			targets  = RealTarList,
            buff_add = [#buff{name = ?BUFF_CAST_DMG_UP, by_rate = false, value = ExtraDmg}]
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 呼啸破: 以降低自己的防御为代价, 对目标进行一次物理攻击, 必爆击
%% {防御减少系数, 攻击系数}
handle_skill(SkillId = 234, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

	BuffPDefDown = 
		#buff {
			name     = ?BUFF_PDEF_DOWN,
			value    = ?p1,
			by_rate  = true,
			duration = 1,
			settle   = post
		},
	BuffMDefDown = 
		#buff {
			name     = ?BUFF_MDEF_DOWN,
			value    = ?p1,
			by_rate  = true,
			duration = 1,
			settle   = post
		},
	BuffCrit =
		#buff {
			name     = ?BUFF_CRIT
		},
	
	AttSpec = 
		#attack_spec {
			addition = ?p2,		  
			targets  = RealTarList,
			buff_add = [BuffCrit],
			buff     = [{BuffPDefDown, 1.0, add}, {BuffMDefDown, 1.0, add}]
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 连环杀阵: 对敌人攻击3次 每次目标都是随机选取
%% {攻击系数}
handle_skill(SkillId = 235, Src, _Tar, _Level, Param, BattleData) ->
    %% XXX: 这个……就没办法override了……
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
				
				TarList = battle:get_target_list(battle:calc_range(T, ?ALLFRIENDLY), Data1),
	
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
%% {攻击系数}
handle_skill(SkillId = 236, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ ->
            List  = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
            util:get_rand_list_elems(List, 3)
    end,
		   
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = RealTarList
		},
	
	battle:attack(SkillId, Src, AttSpec, BattleData);


%% 祭风术 对目标进行一次强力的法术攻击, 如果命中, 则有一定概率降低目标一定量的怒氣值
%% {攻击系数, 概率, 减少怒气值}
handle_skill(SkillId = 237, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

    BuffList = case random:uniform() =< ?p2 of
        true ->
            [#buff {
                name  = ?BUFF_MANA_DRAIN,
                value = {0, ?p3},
                by_rate = false
             }];
        _ ->        % false
            []
    end,
	
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = RealTarList,
			buff_add = BuffList
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 分光诀:   随机对地方两个单位进行法术攻击, 针对每个目标的伤害会降低
%% {伤害系数1, 伤害系数2}
handle_skill(SkillId = 238, Src, Tar, _Level, Param, BattleData) ->	
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ ->
            List = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
            util:get_rand_list_elems(List, 2)
    end,
	
    F = fun(T, {N, BD}) ->
        AttSpec = 
            #attack_spec {
                addition = element(N, {?p1, ?p2}),
                targets  = [T]
            },
        AttInfoList = battle:attack(SkillId, Src, AttSpec, [T], BD), 
        {N + 1, battle:handle_attack_info(SkillId, Src, AttInfoList, BD)}
    end,
    {_, NBattleData} = lists:foldl(F, {1, BattleData}, RealTarList),
    battle:settle_and_add_buff(Src, [], [], NBattleData);

%% 凝劲术: 对敌方所有目标进行一次强力的法术攻击
%% {攻击系数}       % TODO
handle_skill(SkillId = 239, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ -> battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData)
    end,

	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = RealTarList
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 激狂诀:   临时提高自己的暴击进行法术攻击
%% {暴击增加系数, 攻击系数}
handle_skill(SkillId = 240, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

	Buff = 
		#buff {
			name    = ?BUFF_CRIT_UP,
			value   = ?p1,
			by_rate = true 	   
		},
	AttSpec = 
		#attack_spec {
			addition = ?p2,
			targets  = RealTarList,
			buff_add = [Buff]
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 狂风划影: 随机对地方三个目标进行一次法术伤害, 命中后有一定概率降低怒氣值
%% {攻击系数, 概率, 怒气减少数量}
handle_skill(SkillId = 241, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ ->
            List  = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
            util:get_rand_list_elems(List, 3)
    end,

    BuffList = case random:uniform() =< ?p2 of
        true ->
            [#buff {
                name    = ?BUFF_MANA_DRAIN,
                value   = {0, ?p3},
                by_rate = false
             }];
        _ ->        % false
            []
    end,
	AttSpec = 
		#attack_spec {
			addition = ?p1,
			targets  = RealTarList,
			buff_add = BuffList
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);
	

%========================================================================================================
% doctor skill
%========================================================================================================

%% 元灵之光: 对己方3个目标进行治疗, 并概率性增加物理防御和法术防御
%% {治疗系数, 概率, 物理防御系数, 法术防御系数}
handle_skill(SkillId = 243, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ ->
            List  = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
            battle:get_n_pos_by(hp_rel, min, 3, List, BattleData)
    end,
	
	Buffs = [#buff{name = ?BUFF_MDEF_UP, value = ?p4, by_rate = true, settle = post, duration = 1}, 
			 #buff{name = ?BUFF_PDEF_UP, value = ?p3, by_rate = true, settle = post, duration = 1}],
	
	BuffOps = [{Buff, ?p2, add} || Buff <- Buffs],
	
	AssSpecList = 
		[
			#assist_spec {
				pos  = P,
				eff  = [{heal, ?p1, true}],
				buff = BuffOps	  
			} || P <- RealTarList
		],
	battle:assist(SkillId, Src, AssSpecList, BattleData);

%% 药王经: 对一个目标进行治疗,　增加物理防御和法术防御
%% {治疗系数, 防御增加系数}
handle_skill(SkillId = 245, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ -> 
            CandList = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
            {NTar, _} = battle:get_pos_by(hp_rel, min, CandList, BattleData),
            [NTar]
    end,

	Buffs = [#buff{name = ?BUFF_MDEF_UP, value = ?p2, duration = 2, by_rate = true, settle = post}, 
			 #buff{name = ?BUFF_PDEF_UP, value = ?p2, duration = 2, by_rate = true, settle = post}],
	BuffOps = [{Buff, 1.0, add} || Buff <- Buffs],

	AssSpecList = 
		[
			#assist_spec {
				pos  = T,
				rate = 1.0,
				eff  = [{heal, ?p1, true}],
				buff = BuffOps
			} || T <- RealTarList
		],
	battle:assist(SkillId, Src, AssSpecList, BattleData);

%% 清心咒: 给随机一个己方目标加一个回血BUFF
%% {回血系数, 持续回合数}
handle_skill(SkillId = 247, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

	Buffs       = [#buff{name = ?BUFF_REFRESH, value = ?p1, by_rate = true, duration = ?p2, settle = pre}],
	BuffOps     = [{Buff, 1.0, add} || Buff <- Buffs],
	AssSpecList = 
		[
			#assist_spec {
				pos  = T,
				eff  = [],
				buff = BuffOps 			  
			} || T <- RealTarList
		],
	battle:assist(SkillId, Src, AssSpecList, BattleData);

%======================================================================================================================
% monster skills
%======================================================================================================================

%% 坚若磐石
%% {防御增加系数}
handle_skill(SkillId = 248, Src, Tar, _Level, Param, BattleData) ->
    Buffs   = [#buff{name = ?BUFF_PDEF_UP, duration = 2, value = ?p1, by_rate = true, settle = post},
               #buff{name = ?BUFF_MDEF_UP, duration = 2, value = ?p1, by_rate = true, settle = post}],
    BuffOps = [{Buff, 1.0, add} || Buff <- Buffs],
    
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ ->
            battle:get_target_list(battle:calc_range(Src, ?ALLFRIENDLY), BattleData)
    end,
    
    AssistSpec = 
        [
            #assist_spec {
                    pos = P, 
                    eff = [], 
                    buff = BuffOps
            } || P <- RealTarList
        ],
     battle:assist(SkillId, Src, AssistSpec, BattleData);

%% 破军之势
%% {攻击系数, 速度减少系数}
handle_skill(Skill = 255, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ ->
            TarList = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
            {RealTar, _} = battle:get_pos_by(mp, max, TarList, BattleData),
            [RealTar]
    end,

    Buff = #buff {
        name     = ?BUFF_FAINT,
        by_rate  = false,
        duration = 1,
        settle   = post
    },
    
    AttSpec = #attack_spec {
        addition = ?p1,
        targets  = RealTarList,
        debuff   = [{Buff, 1.0, add}]                           
    },
    battle:attack(Skill, Src, AttSpec, BattleData);

%% 破军咒
%% {攻击系数}
handle_skill(SkillId = 259, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

    Buff = #buff {
        name     = ?BUFF_CRIT,
        duration = 0,
        value    = 0,
        by_rate  = 0   
    },
    
    AttSpec = #attack_spec {
        addition = ?p1,
        targets  = RealTarList,
        buff_add = [Buff]         
    },
    battle:attack(SkillId, Src, AttSpec, BattleData);

handle_skill(SkillId = 260, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

	Buff = #buff {name = ?BUFF_CAST_DMG_DOWN, value = ?p1, duration = 1, settle = post, by_rate = true},
	BuffOps = [{Buff, 1.0, add}],
	AssSpecList = 
		[#assist_spec {pos = T, rate = 1.0, eff = [], buff = BuffOps} || T <- RealTarList],
	battle:assist(SkillId, Src, AssSpecList, BattleData);

%% 摧枯拉朽
%% {攻击系数}
handle_skill(SkillId = 262, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ ->
            battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData)
    end,

	AttSpec = 
		#attack_spec {
			addition = ?p1,			  
			targets  = RealTarList
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 大地震击
%% {攻击系数, 晕回合数}
handle_skill(SkillId = 263, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ ->
            battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData)
    end,

	Buff = 
		#buff {
			name = ?BUFF_FAINT,	   
			duration = ?p2,
			settle = post
		},
	AttSpec = 
		#attack_spec {
			addition = ?p1,			  
			targets = RealTarList,
			debuff = [{Buff, 1.0, add}]
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% {攻击系数, 降怒气几率, 降怒气点数}
handle_skill(SkillId = 264, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ ->
            TarList = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
            util:get_rand_list_elems(TarList, 3)
    end,

    BuffList = case random:uniform() =< ?p2 of
        true -> 
            [#buff{name = ?BUFF_MANA_DRAIN, by_rate = true, value = {0, ?p3}}];
        false -> []
    end,
	AttSpec = 
		#attack_spec {
			addition = ?p1,			  
			targets  = RealTarList,
            buff_add = BuffList
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% {攻击系数, 伤害降低系数, 持续回合数}
handle_skill(SkillId = 265, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

	AttSpec = 
		#attack_spec {
			addition = ?p1,			  
			targets  = RealTarList
		},
    AttInfoList = battle:attack(SkillId, Src, AttSpec, RealTarList, BattleData), 
    BattleData1 = battle:handle_attack_info(SkillId, Src, AttInfoList, BattleData),

    TarList = battle:get_target_list(battle:calc_range(hd(RealTarList), ?ALLFRIENDLY), BattleData1),
	Buff    = #buff{name = ?BUFF_SCORN,   duration = 1, settle = post, by_rate = true,  value = ?p2},
	Debuff  = #buff{name = ?BUFF_SCORNED, duration = 1, settle = post, by_rate = false, value = Src},
	BuffOps   = [{Buff,   1.0, add}],
	DebuffOps = [{Debuff, 1.0, add}],

    BuffSpec = [{Src, BuffOps} | [{T, DebuffOps} || T <- TarList]],
	battle:settle_and_add_buff(Src, BuffSpec, [], BattleData1);

%% {攻击系数}
handle_skill(SkillId = 266, Src, _Tar, _Level, Param, BattleData) ->
    %% XXX: 呃……这个就没办法override了……
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
				
				AttInfoList = battle:attack(SkillId, Src, AttSpec, [T], Data), 			
				Data1       = battle:handle_attack_info(SkillId, Src, AttInfoList, Data),
				SrcStat     = battle:get_battle_status(Src, Data1),
				
				TarList = battle:get_target_list(battle:calc_range(T, ?ALLFRIENDLY), Data1),
	
				if (SrcStat#battle_status.is_alive == false orelse TarList == []) ->
					{false, Data1};
				true ->
					{true, Data1}
				end
			end
		end,
	{_, NBattleData} = lists:foldl(F, {true, BattleData}, lists:seq(1, 3)),
	battle:do_att_buff(Src, #attack_spec{}, true, [], NBattleData);

%% {攻击系数, 输出伤害减少系数}
handle_skill(SkillId = 267, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),
	AttSpec = 
		#attack_spec {
			addition = ?p1,			  
			targets  = RealTarList
		},
    AttInfoList = battle:attack(SkillId, Src, AttSpec, RealTarList, BattleData), 
    BattleData1 = battle:handle_attack_info(SkillId, Src, AttInfoList, BattleData),

    TarList = battle:get_target_list(battle:calc_range(hd(RealTarList), ?ALLFRIENDLY), BattleData1),
	Buff    = #buff{name = ?BUFF_CAST_DMG_DOWN, duration = 1, settle = post, by_rate = true, value = ?p2},
    BuffSpec = [{T, [{Buff, 1.0, add}]} || T <- TarList],
	battle:settle_and_add_buff(Src, BuffSpec, [], BattleData1);

%% {攻击系数, 受到伤害增加系数}
handle_skill(SkillId = 268, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ -> battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData)
    end,
    Buff = #buff {
        name    = ?BUFF_RECV_DMG_UP,
        by_rate = true,
        value   = ?p2,
        duration = 30,
        settle  = post,
        add_method = overlay
    },
	AttSpec = #attack_spec {
        addition = ?p1,			  
        targets  = RealTarList,
        debuff   = [{Buff, 1.0, add}]
    },
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% {}
handle_skill(SkillId = 269, Src, Tar, _Level, _Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ ->
            TarList = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
            {NTar, _} = battle:get_pos_by(hp, min, TarList, BattleData),
            [NTar]
    end,

	AttSpec = #attack_spec {
        addition = 1,
        targets  = RealTarList,
        buff_add = [#buff {
                        name  = ?BUFF_CAST_DMG_UP,
                        value = 1,
                        by_rate = true
                    }]
    },
	battle:attack(SkillId, Src, AttSpec, BattleData);

%% 血性饥渴
%% {攻击系数, 吸血系数}
handle_skill(SkillId = 270, Src, Tar, _Level, Param, BattleData) ->
    SrcStat = battle:get_battle_status(Src, BattleData),
    Job = SrcStat#battle_status.job,
    Tag = 
		case (Job == ?CAREER_HUWEI) orelse (Job == ?CAREER_MENGJIANG) of
			true  -> p_def;
			false -> m_def
		end,

    RealTarList = case Tar of
        {ai_override, TL} ->
            TL;
        _ ->
            TarList = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
            {NTar, _} = battle:get_pos_by(Tag, min, TarList, BattleData),
            [NTar]
    end,

	Buff = 
		#buff {
			name    = ?BUFF_LIFE_DRAIN,   
			value   = ?p2,
			by_rate = true
		},
	AttSpec = 
		#attack_spec {
			addition = ?p1,		  
			targets  = RealTarList,
			buff_add = [Buff]
		},
	battle:attack(SkillId, Src, AttSpec, BattleData);
	
	
%% 华光普照	治疗己方全体50% 的气血
%% {治疗系数}
handle_skill(SkillId = 271, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} ->
            TL;
        _ ->
            battle:get_target_list(battle:calc_range(Src, ?ALLFRIENDLY), BattleData)
    end,
	
	AssSpecList = 
		[#assist_spec {pos = T, rate = 1.0, eff = [{heal, ?p1, true}], buff = []} || 
		 T <- RealTarList],
	battle:assist(SkillId, Src, AssSpecList, BattleData);

	
%% 疯狂狙击
%% {攻击系数}
handle_skill(_SkillId = 272, Src, Tar, _Level, Param, BattleData) ->
	handle_skill(112, Src, Tar, _Level, {?p1, ?p1}, BattleData);

%% {死亡回合数}
handle_skill(SkillId = 273, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

	Buff = 
		#buff {
			name     = ?BUFF_CURSED,
			duration = ?p1,
			settle   = post,
            add_method = noop
		},
    AssistSpecList = [#assist_spec{pos = T, buff = [{Buff, 1.0, add}]} || T <- RealTarList],
	battle:assist(SkillId, Src, AssistSpecList, BattleData);

%% {吸收伤害系数}
handle_skill(SkillId = 274, Src, Tar, _Level, Param, BattleData) ->
	Buff = 
		#buff {
			name     = ?BUFF_DMG_ABSORB,
			value    = ?p1,
			duration = 1,
			settle   = post,
			by_rate  = true
		},

	PBuff = 
		#buff {
			name     = ?BUFF_DMG_ABSORB_TARGET,
			value    = ?p1,
			duration = 1,
			settle   = post,
			data     = Src,
			by_rate  = true
		},

    RealTarList = case Tar of
        {ai_override, TL} ->
            TL;
        _ ->
            TeamList = battle:get_target_list(battle:calc_range(Src, ?ALLFRIENDLY), BattleData),
            lists:filter(fun(P) -> P =/= Src end, TeamList)
    end,
	AssSpecList = 
		[
            #assist_spec {pos = Src, buff = [{Buff, 1.0, add}]} |
			[#assist_spec{pos = Pos, buff = [{PBuff, 1.0, add}]} || Pos <- RealTarList]
		],
	battle:assist(SkillId, Src, AssSpecList, BattleData);

%% 刺钉护盾
%% {反弹系数, 持续回合数}
handle_skill(SkillId = 275, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} ->
            TL;
        _ ->
            battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData)
    end,

	Buff = #buff {name = ?BUFF_REBOUND, value = ?p1, duration = ?p2, settle = post, by_rate = true},
	BuffOps = [{Buff, 1.0, add}],
	AssSpecList = 
		[
			#assist_spec {pos = T, rate = 1.0, eff = [], buff = BuffOps} ||
                T <- RealTarList
		],
	battle:assist(SkillId, Src, AssSpecList, BattleData);


%% 背水一战
%% {攻击系数}
%% handle_skill(_SkillId = 276, Src, Tar, Level, Param, BattleData) ->
%% 	handle_skill(401, Src, Tar, Level, {?p1}, BattleData);


%% 吸取: 直接吸掉对方一定百分比的血
%% {吸血系数}
handle_skill(SkillId = 277, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),
	AssSpecList = 
		[#assist_spec {pos = T, eff = [{hp_absorb, ?p1, true}], buff = []} ||
            T <- RealTarList],
	battle:assist(SkillId, Src, AssSpecList, BattleData);
	
%% 睡眠
%% {几率, 持续回合数}
handle_skill(SkillId = 279, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),
	Buff = #buff {name = ?BUFF_FAINT, value = 0, duration = ?p2, settle = post},
	BuffOps = [{Buff, ?p1, add}],
	AssSpecList = [ #assist_spec {pos = T, eff = [], buff = BuffOps} || T <- RealTarList],
	battle:assist(SkillId, Src, AssSpecList, BattleData);

%% {输出伤害降低系数, 持续回合数}
handle_skill(SkillId = 282, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ ->
            battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData)
    end,
	Buff = #buff {name = ?BUFF_CAST_DMG_DOWN, value = ?p1, duration = ?p2, settle = post, by_rate = true},
	BuffOps = [{Buff, 1.0, add}],
	AssSpecList = [#assist_spec{pos = T, eff = [], buff = BuffOps} || T <- RealTarList],
	battle:assist(SkillId, Src, AssSpecList, BattleData);

%% {几率, 每回合减血系数, 持续回合数}
handle_skill(SkillId = 283, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),
	Buff = #buff {name = ?BUFF_TOXIC, value = ?p2, duration = ?p3, settle = pre, by_rate = true},
	BuffOps = [{Buff, ?p1, add}],
	AssSpecList = [#assist_spec{pos = T, eff = [], buff = BuffOps} || T <- RealTarList],
	battle:assist(SkillId, Src, AssSpecList, BattleData);

%% {几率, 每回合减血系数, 持续回合数}
handle_skill(SkillId = 284, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ ->
            battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData)
    end,
	Buff = #buff {name = ?BUFF_TOXIC, value = ?p2, duration = ?p3, settle = pre, by_rate = true},
	BuffOps = [{Buff, ?p1, add}],
	AssSpecList = [#assist_spec{pos = T, eff = [], buff = BuffOps} || T <- RealTarList],
	battle:assist(SkillId, Src, AssSpecList, BattleData);

%% {反弹系数}
handle_skill(SkillId = 285, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),
	Buff = #buff {name = ?BUFF_REBOUND, value = ?p1, duration = 1, settle = post, by_rate = true},
	BuffOps = [{Buff, 1.0, add}],
	AssSpecList = 
		[#assist_spec {pos = T, rate = 1.0, eff = [], buff = BuffOps} || T <- RealTarList],
	battle:assist(SkillId, Src, AssSpecList, BattleData);

%% {几率, 回合数}
handle_skill(_SkillId = 287, Src, Tar, _Level, Param, BattleData) ->
	handle_skill(279, Src, Tar, _Level, Param, BattleData);

%% 重击 $$ 强攻
%% {攻击系数}
handle_skill(SkillId, Src, Tar, _Level, Param, BattleData)
  	when SkillId =:= 280; 
		 SkillId =:= 281;
		 SkillId =:= 288;
		 SkillId =:= 289 ->

	handle_skill(230, Src, Tar, _Level, {?p1}, BattleData);

%======================================================================================================================
% spare skills
%======================================================================================================================

%% 补血
handle_skill(SkillId = 401, Src, Tar, _, _Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ -> [Src]
    end,
	AssistSpec = 
		[
		 	#assist_spec {pos = T, eff = [{heal, 100, false}], buff = []}
                || T <- RealTarList
		],
	battle:assist(SkillId, Src, AssistSpec, BattleData);

%======================================================================================================================
% New skills 2012-11-27
%======================================================================================================================

%% 对敌人攻击3次 每次目标都是随机选取，附带晕buff
%% {攻击系数1, 攻击系数2, 攻击系数3, 晕概率, 持续回合数}
handle_skill(SkillId = 406, Src, _Tar, _Level, Param, BattleData) ->
    %% XXX: 这个没办法override……
	F = fun(N, {C, Data, BSpec}) ->
			if (C == false) ->
				{false, Data, BSpec};
			true ->
				T = ai:get_skill_target(SkillId, Src, Data),
				AttSpec = 
					#attack_spec {
                        addition = element(N, {?p1, ?p2, ?p3}),
						targets  = [T]
					},
				
				%% we must use attack/5 here to avoid settle the buff
				AttInfoList = battle:attack(SkillId, Src, AttSpec, [T], Data), 			
				Data1       = battle:handle_attack_info(SkillId, Src, AttInfoList, Data),
				SrcStat     = battle:get_battle_status(Src, Data1),

                FirstAttInfo = hd(AttInfoList),
                NBSpec = case FirstAttInfo#attack_info.is_miss of
                    true  -> BSpec;
                    false -> 
                        TStat = battle:get_battle_status(T, Data1),
                        case TStat#battle_status.is_alive of
                            true ->
                                Buff = #buff {
                                    name  = ?BUFF_FAINT,
                                    value = 0,
                                    duration = ?p5,
                                    settle = post
                                },
                                [{T, [{Buff, ?p4, add}]} | BSpec];
                            false ->
                                BSpec
                        end
                end,
				
				TarList = battle:get_target_list(battle:calc_range(T, ?ALLFRIENDLY), Data1),
	
				if (SrcStat#battle_status.is_alive == false orelse TarList == []) ->
					{false, Data1, NBSpec};
				true ->
					{true, Data1, NBSpec}
				end
			end
		end,
    {_, NBattleData, BuffSpec} = lists:foldl(F, {true, BattleData, []}, lists:seq(1, 3)),
    battle:settle_and_add_buff(Src, BuffSpec, [], NBattleData);

%======================================================================================================================
% New skills 2012-11-27 end
%======================================================================================================================

%======================================================================================================================
% New skills 2012-12-04
%======================================================================================================================

%% 对敌人进行一次物理攻击,造成{P1}的伤害,同时随机为己方{P2}个目标减免{P3}的伤害,持续{P4}回合
%% {攻击系数, 减少伤害人数, 减少伤害系数, 持续回合}
handle_skill(SkillID = 407, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

    Buff = #buff {
        name     = ?BUFF_RECV_DMG_DOWN,
        value    = ?p3,
        duration = ?p4,
        settle   = post,
        by_rate  = true
    },

    AttSpec = #attack_spec {
        addition = ?p1,
        targets  = RealTarList,
        buff     = []
    },
    AttInfoList = battle:attack(SkillID, Src, AttSpec, RealTarList, BattleData),
	BattleData1 = battle:handle_attack_info(SkillID, Src, AttInfoList, BattleData),

    FriendList = battle:get_target_list(battle:calc_range(Src, ?ALLFRIENDLY), BattleData1),
    FriendTarList = util:get_rand_list_elems(FriendList, ?p2),
    BuffSpec = [{Pos, [{Buff, 1.0, add}]} || Pos <- FriendTarList],
	battle:settle_and_add_buff(Src, BuffSpec, [], BattleData1);

%% 破阵攻心：进行一次物理攻击，造成100%伤害，如果命中则击晕目标一回合。
%% {攻击系数, 晕回合数}
handle_skill(SkillID = 408, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),

    Buff = #buff {
        name     = ?BUFF_FAINT,
        value    = 0,
        duration = ?p2,
        settle   = post,
        by_rate  = false
    },

    AttSpec = #attack_spec {
        addition = ?p1,
        targets  = RealTarList,
        debuff   = [{Buff, 1.0, add}]
    },

	battle:attack(SkillID, Src, AttSpec, BattleData);

%% 流云刺：群体物理攻击，打两个目标，对每个目标都造成80%伤害。
%% {攻击系数}
handle_skill(SkillID = 409, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ ->
            EnemyList = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
            util:get_rand_list_elems(EnemyList, 2)
    end,

    AttSpec = #attack_spec {
        addition = ?p1,
        targets  = RealTarList
    },
	battle:attack(SkillID, Src, AttSpec, BattleData);

%% 虎啸破：群体物理攻击，随机打击两个目标，对每个目标都造成100%伤害。
%% {攻击系数}
handle_skill(SkillID = 410, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} -> TL;
        _ ->
            EnemyList = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
            util:get_rand_list_elems(EnemyList, 2)
    end,

    AttSpec = #attack_spec {
        addition = ?p1,
        targets  = RealTarList
    },
	battle:attack(SkillID, Src, AttSpec, BattleData);

%% 冰凌筏：打2个目标，分别造成80%伤害，有概率睡眠1回合。
%% {攻击系数, 晕的概率, 晕回合数}
handle_skill(SkillID = 411, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = case Tar of
        {ai_override, TL} ->
            TL;
        _ ->
            EnemyList = battle:get_target_list(battle:calc_range(Tar, ?ALLFRIENDLY), BattleData),
            util:get_rand_list_elems(EnemyList, 2)
    end,

    Buff = #buff {
        name     = ?BUFF_FAINT,
        by_rate  = false,
        value    = 0,
        duration = ?p3,
        settle   = post
    },

    AttSpec = #attack_spec {
        addition = ?p1,
        targets  = RealTarList,
        debuff   = [{Buff, ?p2, add}]
    },
	battle:attack(SkillID, Src, AttSpec, BattleData);

%% 世界boss的技能
%% {攻击系数, 加致命点数, 持续回合}
handle_skill(SkillID = 123, Src, Tar, _Level, Param, BattleData) ->
    RealTarList = get_real_tar_list(Tar),
    Buff = #buff {
        name     = ?BUFF_FATAL_UP,
        duration = ?p3,
        value    = ?p2,
        by_rate  = false,
        settle   = post
    },
	AttSpec = #attack_spec {
        addition = ?p1,
        targets  = RealTarList,
        buff     = [{Buff, 1.0, add}]
    },
	battle:attack(SkillID, Src, AttSpec, BattleData);

%======================================================================================================================
% New skills 2012-12-04 end
%======================================================================================================================

%% default
handle_skill(SkillID, Src, Tar, Level, Param, BattleData) ->
    'Elixir-Skills':handle_skill(SkillID, Src, Tar, Level, Param, BattleData).


get_real_tar_list(Tar) ->
    case Tar of
        {ai_override, TL} -> TL;
        _ -> [Tar]
    end.

