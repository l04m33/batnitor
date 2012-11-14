-module(pt_20).
-import(battle).
-export([read/2, write/2]).

-include("common.hrl").

-spec read(Cmd, Bin) -> any() when
	Cmd :: integer(),
	Bin :: binary().

read(20000, _) ->
	?INFO(battle, "read 20000"),
	{ok, void};

read(20001, <<SkillId:32>>) ->
	?INFO(battle, "read 20001: SkillId = ~w", [SkillId]),
	
	case SkillId of
		0 -> {ok, finish_play};
		_ -> {ok, SkillId}
	end;

%% auto set command
read(20007, _Bin) ->
	{ok, <<>>};

read(20008, <<Dummy:8>>) ->
	{ok, Dummy};

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
			{ID, pt:pack(20000, <<0:8, MonId:32, MakeTeam:8, MerNum:16, Bin/binary>>)}
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
	%% Type = BattleData#battle_data.type,
	
	IsLastAct = 
		case battle:is_battle_end(BattleData) of
			false -> 0;
			{true, _} -> 1
		end,
	Round   = BattleData#battle_data.round,
	Pro     = battle:get_battle_pro(BattleData),
	AttPro  = battle:get_attack_pro(BattleData),
	ActInd  = length(Pro#battle_pro.attack_pro),
	Pos     = AttPro#attack_pro.pos,
	SkillId = AttPro#attack_pro.skillid,
	Hp      = AttPro#attack_pro.hp,
	Mp      = AttPro#attack_pro.mp,
	HpInc   = AttPro#attack_pro.hp_inc,
	MpInc   = AttPro#attack_pro.mp_inc,
	TarBin  = get_tar_bin(AttPro#attack_pro.attack_info),
	
	TarNum  = get_att_info_len(AttPro#attack_pro.attack_info),
	
	BuffNum = length(AttPro#attack_pro.buff_info),
	BuffBin = get_buff_bin(AttPro#attack_pro.buff_info),
	?INFO(battle, "Round = ~w, ActionIndex = ~w, Pos = ~w, SkillId = ~w, Hp = ~w, Mp = ~w",
		[Round, ActInd, Pos, SkillId, Hp, Mp]),
	
	pt:pack(20001, <<0:8, Round:8, IsLastAct:8, ActInd:8, Pos:8, SkillId:32, Hp:32, 
					Mp:16, HpInc:32, MpInc:16, TarNum:16, TarBin/binary, BuffNum:16, BuffBin/binary>>);
	
write(20003, BattleData) ->
	Type      = BattleData#battle_data.type,
	Round     = BattleData#battle_data.round,
	Bin       = get_order_bin(BattleData),
	pt:pack(20003, <<Type:8, Round:8, Bin/binary>>);

	
write(20005, BattleData) ->
	Type = BattleData#battle_data.type,
	Winner = BattleData#battle_data.winner,

    {PlayerRoleID, MonsterGroupID} = BattleData#battle_data.callback,
    gen_server:cast(batnitor_simulator, {battle_finish, {PlayerRoleID, MonsterGroupID, Winner}}),
	
	AttIdList = battle:get_ids(att, BattleData),
	DefIdList = battle:get_ids(def, BattleData),
	
	case Winner of
		att -> 
			[{ID, pt:pack(20005, <<Type:8, 1:8>>)}  || ID <- AttIdList] ++
			[{ID, pt:pack(20005, <<Type:8, 11:8>>)} || ID <- DefIdList];
		def ->
			[{ID, pt:pack(20005, <<Type:8, 11:8>>)} || ID <- AttIdList] ++
			[{ID, pt:pack(20005, <<Type:8, 1:8>>)}  || ID <- DefIdList]
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

write(20009, #battle_data{award = Award}) ->
	Exp     = Award#battle_award.exp,
	Silver  = Award#battle_award.silver,
	Pts     = 0,
	Donate  = Award#battle_award.donate,
	Items   = Award#battle_award.items,
	
	F = fun({ID, Item}, Acc) ->
			Len = length(Item),
			ItemBin = get_item_bin(Item),
			[{ID, pt:pack(20009, <<Exp:32, Silver:32, Pts:32, Donate:32, Len:16, ItemBin/binary>>)} | Acc]
		end,
	lists:foldl(F, [], Items);

%% write(20009, {ID, Award}) ->
%% 	Exp     = Award#battle_award.exp,
%% 	Gold    = Award#battle_award.gold,
%% 	Pts     = 0,
%% 	Donate  = Award#battle_award.donate,
%% 	Items   = 
%% 		case lists:keysearch(ID, 1, Award#battle_award.items) of
%% 			false -> [];
%% 			{value, {ID, I}} -> I
%% 		end,
%% 	Len     = length(Items),
%% 	ItemBin = get_item_bin(Items),
%% 	pt:pack(20009, <<Exp:32, Gold:32, Pts:32, Donate:32, Len:16, ItemBin/binary>>);
	
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
	if (is_integer(ID)) -> %% monster has no id, id will be *undefined*
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
	MpMax  = 100,
	Level  = Status#battle_status.level,
	Name   = pt:write_string(Status#battle_status.name),
	Avatar = pt:write_string(""),
	
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
	Hp     = AttInfo#attack_info.hp,
	Mp     = AttInfo#attack_info.mp,
	HpInc  = AttInfo#attack_info.hp_inc,
	MpInc  = AttInfo#attack_info.mp_inc,
	HpAbs  = AttInfo#attack_info.hp_absorb,
	HpReb  = AttInfo#attack_info.hp_rebound, %% rebound,
	HpCnt  = AttInfo#attack_info.hp_counter,
	MpReb  = 0,
		
	?INFO(battle, "AssPos = ~w, Pos = ~w, Hit = ~w, Crit = ~w, " ++ 
			  "Hp = ~w, Mp = ~w, HpInc = ~w, MpInc = ~w, HpReb = ~w, HpCnt = ~w", 
		  [AssPos, Pos, Hit, Crit, Hp, Mp, HpInc, MpInc, HpReb, HpCnt]),
	
	NTarBin = <<TarBin/binary, AssPos:8, Pos:8, Crit:8, Hit:8, Hp:32,
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
	Hp       = BuffInfo#buff_info.hp,
	Mp       = BuffInfo#buff_info.mp,
	HpInc    = BuffInfo#buff_info.hp_inc,
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












	










