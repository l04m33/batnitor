-module(batnitor_simulator).
-behaviour(gen_server).

-include("batnitor.hrl").
-include("common.hrl").
-include_lib("wx/include/wx.hrl").

-export([
         start_link/0,
         stop/0
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, 
        {
         gui_ref = none
        }).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).


init(_) ->
    ?I("~s up and running.", [?MODULE]),
    erlang:process_flag(trap_exit, true),

    ets:new(ets_role_rec, [named_table, public, {keypos, #role.key}, set]),
    ets:new(ets_role_misc_rec, [named_table, public, {keypos, 1}, set]),
    data_mon_group:init_ets(),
    data_mon_attr:init_ets(),

    ets:new(ets_online, [named_table, public, {keypos, #ets_online.id}, set]),      % dummy table 

    {wx_ref, _, _, GPID} = batnitor_gui:start_link(),
    State = #state{gui_ref = GPID},
    {ok, State}.


handle_call(check_data_all_set, _From, State) ->
    RolesEmpty = (ets:info(ets_role_rec, size) == 0),
    MonAttrEmpty = data_mon_attr:is_empty(),
    MonGroupEmpty = data_mon_group:is_empty(),
    Reply = if
        RolesEmpty ->
            {false, role};
        MonAttrEmpty ->
            {false, monster_attr};
        MonGroupEmpty ->
            {false, monster_group};
        true ->
            true
    end,
    {reply, Reply, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast({set_role_list, RoleMiscList}, State) ->
    ets:delete_all_objects(ets_role_rec),
    ets:delete_all_objects(ets_role_misc_rec),
    {RoleList, MiscList} = lists:unzip(RoleMiscList),
    ets:insert(ets_role_rec, RoleList),
    ets:insert(ets_role_misc_rec, MiscList),
    {noreply, State};

handle_cast({set_monster_group_list, MonsterGroupList}, State) ->
    data_mon_group:set(MonsterGroupList),
    {noreply, State};

handle_cast({set_monster_attr_list, MonsterAttrList}, State) ->
    data_mon_attr:set(MonsterAttrList),
    {noreply, State};

handle_cast(do_simulation, State) ->
    start_one_battle(1),        %% TODO
    {noreply, State};

handle_cast({battle_finish, {PlayerRoleID, MonsterGroupID, Winner} = Res}, State) ->
    ?I("PlayerRoleID = ~w, MonsterGroupID = ~w, Winner = ~w", [PlayerRoleID, MonsterGroupID, Winner]),
    gen_server:cast(batnitor_gui, {append_battle_result, Res}),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'EXIT', GPID, normal}, #state{gui_ref = GPID} = State) ->
    init:stop(),
    {noreply, State};

handle_info(_Msg, State) ->
    ?I("_Msg = ~w", [_Msg]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ?I("~s shutting down.", [?MODULE]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




start_one_battle(MonGroupID) ->
    MonGroup = data_mon_group:get(MonGroupID),
    PosList = lists:keysort(2, MonGroup#mon_group.pos),
    [{PlayerRoleID, _} | _] = PosList,
    [RoleInfo] = ets:lookup(ets_role_rec, {0, PlayerRoleID}),
    [MiscInfo] = ets:lookup(ets_role_misc_rec, {0, PlayerRoleID}),
    PlayerRoleList = prepare_mon_attr(MonGroup, RoleInfo, MiscInfo),
    ?I("Starting battle process...."),
    Start = #battle_start {
        mod = pve,
	 	type = 0,
		%att_id = undefined,
		att_mer = PlayerRoleList,
	 	monster = MonGroupID,
		caller = batnitor_simulator,
		callback = {PlayerRoleID, MonGroupID}
    },
    {ok, PID} = battle:start(Start),
    ?I("Battle process PID = ~w", [PID]).

prepare_mon_attr(MonGroup, RoleInfo, MiscInfo) ->
    {_, GuaiDaRen, RenDaGuai, NanDu, LeiXing} = MiscInfo,
    F = fun({MonID, _}) ->
        MonAttr = data_mon_attr:get(MonID),
        NewAttr = MonAttr#mon_attr {
            cat         = LeiXing, 
            level       = RoleInfo#role.gd_roleLevel,

            hp          = erlang:round(
                              RenDaGuai * RoleInfo#role.p_att * 
                              (1 - (RoleInfo#role.p_def / (RoleInfo#role.p_def + RoleInfo#role.gd_roleLevel * 450)))),

            p_att       = case LeiXing of
                              %% physical attacker
                              1 -> erlang:round(
                                       RoleInfo#role.gd_maxHp / 
                                       ((1 - RoleInfo#role.p_def / 
                                         (RoleInfo#role.p_def + RoleInfo#role.gd_roleLevel * 450)) * GuaiDaRen));
                              %% magical attacker
                              3 -> 0
                          end,

            m_att       = case LeiXing of
                              %% physical attacker
                              3 -> erlang:round(
                                       RoleInfo#role.gd_maxHp / 
                                       ((1 - RoleInfo#role.m_def / 
                                         (RoleInfo#role.m_def + RoleInfo#role.gd_roleLevel * 450)) * GuaiDaRen));
                              %% magical attacker
                              1 -> 0
                          end,

            p_def       = case LeiXing of
                              1 -> erlang:round(RoleInfo#role.p_def * 1.2);
                              3 -> erlang:round(RoleInfo#role.p_def * 0.8)
                          end,

            m_def       = case LeiXing of
                              3 -> erlang:round(RoleInfo#role.m_def * 1.2);
                              1 -> erlang:round(RoleInfo#role.m_def * 0.8)
                          end,

            speed       = case NanDu of
                              %% normal
                              1 -> erlang:round(0.9 * RoleInfo#role.gd_speed);
                              %% elite
                              2 -> erlang:round(1.0 * RoleInfo#role.gd_speed);
                              %% boss
                              3 -> erlang:round(1.1 * RoleInfo#role.gd_speed)
                          end
        },
        data_mon_attr:set(NewAttr)
    end,

    lists:foreach(F, MonGroup#mon_group.pos),
    NewGroup = MonGroup#mon_group {
        level = RoleInfo#role.gd_roleLevel,
        type  = case NanDu of
                    1 -> soldier;
                    2 -> soldier;
                    3 -> boss
                end
    },
    data_mon_group:set(NewGroup),

    RoleF = fun(Pos) ->
        RoleInfo#role {
            gd_isBattle = Pos,
            gd_roleRank = case Pos of
                              1 -> 1;
                              _ -> 0
                          end
        }
    end,
    lists:map(RoleF, lists:seq(1, 3)).

