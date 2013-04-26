-module(pt_20).
-import(battle).
-export([read/2, write/2]).

-include("common.hrl").

-spec read(Cmd, Bin) -> any() when
	Cmd :: integer(),
	Bin :: binary().

read(20000, <<NoUse:8>>) ->
	?INFO(battle, "read 20000"),
	{ok, NoUse};

read(20001, <<SkillId:32>>) ->
	?INFO(battle, "read 20001: SkillId = ~w", [SkillId]),
	
	case SkillId of
		0 -> {ok, finish_play};
		_ -> {ok, SkillId}
	end;

read(20002, <<NoUse:8>>) ->
	{ok, NoUse};

%% auto set command
read(20007, _Bin) ->
	{ok, <<>>};

read(20008, <<Dummy:8>>) ->
	{ok, Dummy};

read(20012, <<NoUse:8>>) ->
    {ok, NoUse};

read(20100, <<MonsterUniqueId:32>>) ->
	{ok, MonsterUniqueId};

%%----------------for performance test-----------
read(20201,<<MonsterUniqueId:32>>) ->
	{ok,MonsterUniqueId};
%%-----------------------------------------------
read(_Cmd, _Bin) ->
	{ok, <<>>}.

-spec write(Cmd, Bin) -> binary() when
	Cmd :: integer(),
	Bin :: binary().
%% pve battle 
write(20000, BattleData) ->
    gen_fsm:send_event(self(), {ready, get(id)}),
    gen_fsm:send_event(self(), {set_cmd, get(id), 0}),
	Type     = BattleData#battle_data.type,
	MonId    = BattleData#battle_data.monster,
	MakeTeam = if (BattleData#battle_data.maketeam == true) -> 1; true -> 0 end,
	MerNum   = battle:get_mercenary_num(pve, BattleData),
	
	{Class, MerList} =
		get_mer_list([att_lead, att_mem], BattleData),
		
	BinList = 
		get_mer_bin(Class, MerList, BattleData),
	
	?INFO(pt_20, "Type = ~w, MonId = ~w, MerNum = ~w", [Type, MonId, MerNum]),
	
	lists:map(
	   	fun ({ID, Bin}) ->
			{ID, pt:pack(20000, <<Type:8, MonId:32, MakeTeam:8, MerNum:16, Bin/binary>>)}
		end, BinList);

%% pvp battle 
write(20002, BattleData) ->
	Type   = BattleData#battle_data.type,
	Mod    = 0,
	MerNum = battle:get_mercenary_num(pvp, BattleData),
	
	{Class, MerList} = 
		get_mer_list([att_lead, att_mem, def_lead, def_mem], BattleData),
	
	BinList = 
		get_mer_bin(Class, MerList, BattleData),
	
	lists:map(
		fun ({ID, Bin}) ->
			{ID, pt:pack(20002, <<Type:8, Mod:8, MerNum:16, Bin/binary>>)}
		end, BinList);

write(20001, BattleData) ->
    gen_fsm:send_event(self(), {set_cmd, get(id), 0}),
    gen_fsm:send_event(self(), {finish_play, get(id)}),
	%% Type = BattleData#battle_data.type,
	
	IsLastAct = 
		case battle:is_battle_end(BattleData) of
			false -> 0;
			{true, _} -> 1
		end,
	
	Type    = BattleData#battle_data.type,
	Round   = BattleData#battle_data.round,
	Pro     = battle:get_battle_pro(BattleData),
	AttPro  = battle:get_attack_pro(BattleData),
	ActInd  = length(Pro#battle_pro.attack_pro),
	Pos     = AttPro#attack_pro.pos,
	SkillId = AttPro#attack_pro.skillid,
	Hp      = round(AttPro#attack_pro.hp),
	Mp      = AttPro#attack_pro.mp,
	HpInc   = round(AttPro#attack_pro.hp_inc),
	MpInc   = AttPro#attack_pro.mp_inc,
	TarBin  = get_tar_bin(AttPro#attack_pro.attack_info),
	
	TarNum  = get_att_info_len(AttPro#attack_pro.attack_info),
	
	BuffNum = length(AttPro#attack_pro.buff_info),
	BuffBin = get_buff_bin(AttPro#attack_pro.buff_info),
	?INFO(battle, "Round = ~w, ActionIndex = ~w, Pos = ~w, SkillId = ~w, Hp = ~w, Mp = ~w",
		[Round, ActInd, Pos, SkillId, Hp, Mp]),
	
	pt:pack(20001, <<Type:8, Round:8, IsLastAct:8, ActInd:8, Pos:8, SkillId:32, Hp:32, 
					Mp:16, HpInc:32, MpInc:16, TarNum:16, TarBin/binary, BuffNum:16, BuffBin/binary>>);
	
write(20003, BattleData) ->
	Type      = BattleData#battle_data.type,
	Round     = BattleData#battle_data.round,
	Bin       = get_order_bin(BattleData),
	pt:pack(20003, <<Type:8, Round:8, Bin/binary>>);

	
write(20005, {BattleData, BattleEndState}) ->
    gen_fsm:send_event(self(), {quit, get(id)}),
    calc_simulator_result(BattleData),
	Type = BattleData#battle_data.type,
	Winner = BattleData#battle_data.winner,
	
	AttIdList = battle:get_ids(att, BattleData, online),
	DefIdList = battle:get_ids(def, BattleData, online),
	
	case Winner of
		att -> 
			[{ID, pt:pack(20005, <<Type:8, 1:8,  BattleEndState:8>>)}  || ID <- AttIdList] ++
			[{ID, pt:pack(20005, <<Type:8, 11:8, BattleEndState:8>>)} || ID <- DefIdList];
		def ->
			[{ID, pt:pack(20005, <<Type:8, 11:8, BattleEndState:8>>)} || ID <- AttIdList] ++
			[{ID, pt:pack(20005, <<Type:8, 1:8,  BattleEndState:8>>)}  || ID <- DefIdList]
	end;


%% CD package
write(20006, {Pos, BattleData}) ->
	Type = BattleData#battle_data.type,
	Bs   = battle:get_battle_status(Pos, BattleData),
	Cd   = Bs#battle_status.cd, %% cd list;
	Len  = length(Cd), 
	Bin  = get_cd_bin(Cd),
	pt:pack(20006, <<Type:8, Pos:8, Len:16, Bin/binary>>);

write(20007, {Pos, SkillID}) ->
	pt:pack(20007, <<Pos:8, SkillID:32>>);

write(20009, Battle_data) ->
	Award = Battle_data#battle_data.award,
	%%这是一个坑，希望别掉人~
	%%每天刷野怪800次的话，不给经验
	case Battle_data#battle_data.caller of
		monster ->
			case mod_counter:get_counter(get(id),?COUNTER_ANTI_TOO_MANY_BATTLE) < data_system:get(36) of
				true->
					No_benifit = false;
				false->
					No_benifit = true
			end;
		_->
			No_benifit = false
	end,

	if 
		No_benifit == false->
			ExpList     = Award#battle_award.exp,
			SilverList  = Award#battle_award.silver,
			Pts     = 0,
			Donate  = Award#battle_award.donate,
			Items   = Award#battle_award.items;
		true->
			ExpList = [],
			SilverList = [],
			Pts = 0,
			Donate = 0,
			Items = []
	end,

    ?INFO(battle, "Items = ~w", [Items]),
	
	F = fun({ID, Item}, Acc) ->
			Len = length(Item),
			ItemBin = get_item_bin(Item),
			Exp = case lists:keyfind(ID, 1, ExpList) of
						   false -> 0;
						   {_, ExpRes} -> ExpRes
					   end,
			Silver = case lists:keyfind(ID, 1, SilverList) of
						 false -> 0;
						 {_, SilverRes} -> SilverRes
					 end,
			[{ID, pt:pack(20009, <<Exp:32, Silver:32, Pts:32, Donate:32, Len:16, ItemBin/binary>>)} | Acc]
		end,
	lists:foldl(F, [], Items);

write(20010, MonsterHPList) ->
    BinList = lists:map(
        fun({Pos, HP}) ->
            <<Pos:8, HP:32>>
        end,
        MonsterHPList),
    Payload = list_to_binary([<<(length(MonsterHPList)):16>> | BinList]),
    pt:pack(20010, Payload);

write(20011, BattleData) when is_record(BattleData, battle_data) ->
    IDList = battle:get_online_ids(BattleData),
    BuffWriter = fun(B, {Len, AccBin}) ->
            Type = B#buff.name,
            Value = B#buff.value,
            Show = ?G_BUFF_SHOW,
            {Len + 1, <<AccBin/binary, Type:8, Value:16, Show:8>>}
        end,
    lists:map(fun(ID) ->
            PInfo = battle:get_player_info(ID, BattleData),
            GBuffList = PInfo#player_info.g_buffs,
            {Len, BuffBin} = lists:foldl(BuffWriter, {0, <<>>}, GBuffList),
            {ID, pt:pack(20011, <<Len:16, BuffBin/binary>>)}
        end,
        IDList);

write(20011, GBuffList) when is_list(GBuffList) ->
    BuffWriter = fun(B, {Len, AccBin}) ->
            Type = B#buff.name,
            Value = B#buff.value,
            Show = ?G_BUFF_SHOW,
            {Len + 1, <<AccBin/binary, Type:8, Value:16, Show:8>>}
        end,
    {Len, BuffBin} = lists:foldl(BuffWriter, {0, <<>>}, GBuffList),
    pt:pack(20011, <<Len:16, BuffBin/binary>>);


write(20012, Plot) ->
    [PrePlot, PostPlot | _] = Plot#battle_plot.plots,
    NewRoles = Plot#battle_plot.new_roles,

    F = fun({Pos, BS}, AccBin) ->
        % XXX: 这里只分自己的佣兵和敌人，没有队友
        RoleType = case Pos > (?BATTLE_FIELD_SIZE div 2) of
            true -> 3;  % 敌人
            _    -> 1   % 自己
        end,

        <<AccBin/binary,
          (BS#battle_status.id):16,
          Pos:8,
          (BS#battle_status.level):8,
          (BS#battle_status.hp):32,
          (BS#battle_status.mp):16,
          (BS#battle_status.hp_max):32,
          (BS#battle_status.mp_max):16,
          (pt:write_string(BS#battle_status.name))/binary,
          (pt:write_string(""))/binary,
          RoleType:8>>
    end,
    Payload = lists:foldl(F, <<PrePlot:32, PostPlot:32, (length(NewRoles)):16>>, NewRoles),
    pt:pack(20012, Payload);

write(20013, NewSkillID) ->
    pt:pack(20013, <<NewSkillID:32>>);

write(_Cmd, _Data) ->
	erlang:exit("protocol error: ").

%% used by 20000|20002
get_mer_bin(Class, MerList, BattleData) ->
	get_mer_bin([], Class, MerList, BattleData).

get_mer_bin(BinList, [Tag | Rest], MerList, BattleData) ->
	PInfo = 
		case Tag of
			att_lead -> lists:nth(1, BattleData#battle_data.attacker);
			att_mem  -> lists:nth(2, BattleData#battle_data.attacker);
			def_lead -> lists:nth(1, BattleData#battle_data.defender);
			def_mem  -> lists:nth(2, BattleData#battle_data.defender)
		end,
	ID = PInfo#player_info.id,
	if (is_integer(ID) andalso PInfo#player_info.online =:= true) -> %% monster has no id, id will be *undefined*
		Bin = get_mer_bin_1(Tag, MerList, BattleData),
		get_mer_bin([{ID, Bin} | BinList], Rest, MerList, BattleData);
	true ->
		get_mer_bin(BinList, Rest, MerList, BattleData)
	end;
	
get_mer_bin(BinList, [], _MerList, _BattleData) ->
	BinList.

%% get_mer_bin can be invoke by get
get_mer_bin_1(Tag, MerList, BattleData) ->
	get_mer_bin_1(<<>>, Tag, MerList, BattleData).

get_mer_bin_1(MerBin, _Tag1, [], _BattleData) ->
	MerBin;

get_mer_bin_1(MerBin, Tag1, [{Tag2, Pos} | Rest], BattleData) ->
	Type = 
		if (Tag1 == Tag2) -> 1;
		   (Tag1 == att_lead andalso Tag2 == att_mem)  -> 2;
		   (Tag1 == def_lead andalso Tag2 == def_mem)  -> 2;
		   (Tag1 == att_mem  andalso Tag2 == att_lead) -> 2;
		   (Tag1 == def_mem  andalso Tag2 == def_lead) -> 2;
			true -> 3
		end,

	Status = battle:get_battle_status(Pos, BattleData),
	ID     = Status#battle_status.id,
	Hp     = Status#battle_status.hp,
	HpMax  = Status#battle_status.hp_max,
	Mp     = Status#battle_status.mp,
	MpMax  = Status#battle_status.mp_max,
	Level  = Status#battle_status.level,
	Name   = pt:write_string(Status#battle_status.name),
	Avatar = pt:write_equip_wing_horse(Status#battle_status.avatar_info),

	SkillNum = length(Status#battle_status.skill),
	SkillBin = get_skill_bin(Status#battle_status.skill) ,

	Bin = <<ID:16, Pos:8, Level:8, Hp:32, Mp:16, HpMax:32, MpMax:16, Name/binary, 
			Avatar/binary, Type:8, SkillNum:16, SkillBin/binary>>,

	get_mer_bin_1(<<MerBin/binary, Bin/binary>>, Tag1, Rest, BattleData).

get_tar_bin(AttInfoList) ->
	get_tar_bin(<<>>, 1, lists:reverse(AttInfoList)).

get_tar_bin(TarBin, _Index, []) ->
	TarBin;

get_tar_bin(TarBin, Index, [AttInfoList | Rest]) ->
	Bin = get_tar_bin_1(Index, AttInfoList),
	get_tar_bin(<<TarBin/binary, Bin/binary>>, Index + 1, Rest).

get_tar_bin_1(Index, AttInfoList) ->
	get_tar_bin_1(<<>>, Index, AttInfoList).

get_tar_bin_1(Bin, _Index, []) ->
	Bin;

get_tar_bin_1(TarBin, Index, [AttInfo | Rest]) ->
	AssPos = AttInfo#attack_info.assist_pos,
	Pos    = AttInfo#attack_info.pos,
	Hit    = if (AttInfo#attack_info.is_miss == true) -> 1; true -> 0 end,
	Crit   = if (AttInfo#attack_info.is_crit == true) -> 1; true -> 0 end,
    Block  = if (AttInfo#attack_info.is_block == true) -> 1; true -> 0 end,
	Hp     = round(AttInfo#attack_info.hp),
	Mp     = AttInfo#attack_info.mp,
	HpInc  = round(AttInfo#attack_info.hp_inc),
	MpInc  = AttInfo#attack_info.mp_inc,
	HpAbs  = round(AttInfo#attack_info.hp_absorb),
	HpReb  = round(AttInfo#attack_info.hp_rebound), %% rebound,
	HpCnt  = round(AttInfo#attack_info.hp_counter),
	MpReb  = 0,
		
	?INFO(battle, "AssPos = ~w, Pos = ~w, Hit = ~w, Crit = ~w, Block =~w, " ++ 
			  "Hp = ~w, Mp = ~w, HpInc = ~w, MpInc = ~w, HpReb = ~w, HpCnt = ~w", 
		  [AssPos, Pos, Hit, Crit, Block, Hp, Mp, HpInc, MpInc, HpReb, HpCnt]),
	
	NTarBin = <<TarBin/binary, AssPos:8, Pos:8, Crit:8, Block:8, Hit:8, Hp:32,
				 Mp:16, HpInc:32, MpInc:16, HpCnt:32, HpReb:32, MpReb:16, HpAbs:32, Index:8>>,

	get_tar_bin_1(NTarBin, Index, Rest).

%=================================================================================================
% get skill binary
%=================================================================================================
%% skill = [{SkillId, Level}].

get_skill_bin(SkillList) ->
	get_skill_bin(<<>>, SkillList).

get_skill_bin(Bin, []) ->
	Bin;

get_skill_bin(Bin, [Skill | Rest]) ->
	get_skill_bin(<<Bin/binary, Skill:32>>, Rest).

%=================================================================================================
% get buff binary
%=================================================================================================

get_buff_bin(BuffInfoList) ->
	?INFO(battle, "BuffInfo List = ~w", [BuffInfoList]),
	get_buff_bin(<<>>, BuffInfoList).


get_buff_bin(Bin, []) ->
	Bin;

%% order is reverse...
get_buff_bin(Bin, [BuffInfo | Rest]) ->
	Settle   = if (BuffInfo#buff_info.settle == post)    -> 1; true -> 0 end,
	IsNew    = if (BuffInfo#buff_info.is_new == true)    -> 1; true -> 0 end,
	IsRemove = if (BuffInfo#buff_info.is_remove == true) -> 1; true -> 0 end,
	ByRate   = if (BuffInfo#buff_info.by_rate == true)   -> 1; true -> 0 end,
	
	Value    = BuffInfo#buff_info.value,
	Name     = BuffInfo#buff_info.name,
	Owner    = BuffInfo#buff_info.owner,
	Hp       = round(BuffInfo#buff_info.hp),
	Mp       = BuffInfo#buff_info.mp,
	HpInc    = round(BuffInfo#buff_info.hp_inc),
	MpInc    = BuffInfo#buff_info.mp_inc,
	Duration = 
		if (IsRemove == 1) -> 
			0;
		true ->
			BuffInfo#buff_info.duration
		end,
	
	?INFO(battle, "assembling, Name = ~w, Owner = ~w, Hp = ~w, Mp = ~w, HpInc = ~w, MpInc = ~w, Duration = ~w"
			"Value = ~w, ByRate = ~w",
		  [Name, Owner, Hp, Mp, HpInc, MpInc, Duration, Value, ByRate]),
	
	NBin = <<Bin/binary, Settle:8, Name:8, Owner:8, ByRate:8, Value:32, Hp:32, Mp:16, HpInc:32, MpInc:16,
			IsNew:8, Duration:8>>,
	
	?INFO(battle, "done"),
	get_buff_bin(NBin, Rest).
	
%=================================================================================================
% get order binary
%=================================================================================================

get_order_bin(BattleData) ->
	get_order_bin(<<>>, 1, BattleData#battle_data.attorder, BattleData).


get_order_bin(Bin, Index, [Pos | Rest], BattleData) ->
	State = battle:get_battle_status(Pos, BattleData),
	if (State#battle_status.is_alive) ->
		get_order_bin(<<Bin/binary, Pos:8, Index:8>>, Index + 1, Rest, BattleData);
	true ->
		get_order_bin(Bin, Index, Rest, BattleData)
	end;

get_order_bin(Bin, Index, [], _BattleData) ->
	<<(Index - 1):16, Bin/binary>>.


get_cd_bin(CdList) ->
	get_cd_bin(<<>>, CdList).

get_cd_bin(Bin, [{SkillId, Cd} | Rest]) ->
	get_cd_bin(<<SkillId:32, Cd:8, Bin/binary>>, Rest);


get_cd_bin(Bin, []) ->
	Bin.

%% AttInfo is of the form [[], ..., []]
get_att_info_len(ListOfAttInfoList) ->
	get_att_info_len(0, ListOfAttInfoList).


get_att_info_len(Num, [List | Rest]) ->
	get_att_info_len(Num + length(List), Rest);

get_att_info_len(Num, []) ->
	Num.


%% -spec get_pvp_mer_list(Tags, BattleData) -> {Valid Class List, Valid MerList } 
get_mer_list(Tags, BattleData) ->
	get_mer_list([], [], Tags, BattleData).

get_mer_list(Tags, List, [Tag | Rest], BattleData) ->
	if (Tag == att_lead orelse Tag == def_lead) ->
		PlayerInfo = 
			case Tag == att_lead of
				true  -> hd(BattleData#battle_data.attacker);
				false -> hd(BattleData#battle_data.defender)
			end,
		MerList = [{Tag, Pos} || Pos <- PlayerInfo#player_info.mer_list],
		get_mer_list([Tag | Tags], MerList ++ List, Rest, BattleData);
	true ->
		PlayerInfo = 
			case Tag == att_mem of
				true  -> tl(BattleData#battle_data.attacker);
				false -> tl(BattleData#battle_data.defender)
			end,
		
		if (PlayerInfo =:= []) ->
			get_mer_list(Tags, List, Rest, BattleData);
		true ->
			[PlayerInfoReal] = PlayerInfo,
			MerList = [{Tag, Pos} || Pos <- PlayerInfoReal#player_info.mer_list],
			get_mer_list([Tag | Tags], MerList ++ List, Rest, BattleData)
		end
	end;
			
get_mer_list(Tags, MerList, [], _BattleData) ->
	{Tags, MerList}.
	
get_item_bin(Items) ->
	get_item_bin(<<>>, Items).

get_item_bin(Bin, []) -> Bin;
get_item_bin(Bin, [{ItemID, Count, _} | Rest]) ->
	NBin = <<Bin/binary, ItemID:32, Count:8>>,
	get_item_bin(NBin, Rest).


get_battle_hp_list(Winner, BattleData) ->
    case Winner of
        att -> get_battle_hp_list(1, ?BATTLE_FIELD_SIZE div 2 + 1, [], BattleData);
        def -> get_battle_hp_list(?BATTLE_FIELD_SIZE div 2 + 1, ?BATTLE_FIELD_SIZE + 1, [], BattleData)
    end.

get_battle_status(Pos, BattleData) ->
    array:get(Pos, BattleData#battle_data.player).

get_battle_hp_list(Limit, Limit, List, _BattleData) -> List;
get_battle_hp_list(Index, Limit, List, BattleData) ->
    Stat = get_battle_status(Index, BattleData),
    if (Stat == ?UNDEFINED) ->
        get_battle_hp_list(Index + 1, Limit, List, BattleData);
    true ->
        get_battle_hp_list(Index + 1, Limit, [{Index, Stat#battle_status.hp} | List], BattleData)
    end.

calc_simulator_result(BattleData) ->
    Winner = BattleData#battle_data.winner,
    DefHPList = get_battle_hp_list(def, BattleData),
    AttHPList = get_battle_hp_list(att, BattleData),
    Rounds = BattleData#battle_data.round,
    {PlayerRoleID, MonsterGroupID, MaxGroupID, SimTimes, MaxSimTimes} = BattleData#battle_data.callback,
    gen_server:cast(batnitor_simulator, {battle_finish, {PlayerRoleID, MonsterGroupID, MaxGroupID, SimTimes, MaxSimTimes, Winner, Rounds, AttHPList, DefHPList}}).

