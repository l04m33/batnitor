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

handle_call({get_calculated_monster_attr, MinGroupID, MaxGroupID}, _From, State) ->
    Reply = calc_all_mon_attr(MinGroupID, MaxGroupID, []),
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

handle_cast({do_simulation, MinGroupID, MaxGroupID, MinSimTimes, MaxSimTimes}, State) ->
    case MinGroupID =< MaxGroupID of
        true ->
            case MinSimTimes =< MaxSimTimes of
                true ->
                    case start_one_battle(MinGroupID, MaxGroupID, MinSimTimes, MaxSimTimes) of
                        ok -> void;
                        error ->
                            gen_server:cast(self(), {do_simulation, MinGroupID + 1, MaxGroupID, 1, MaxSimTimes});
                        ignore ->
                            gen_server:cast(self(), {do_simulation, MinGroupID + 1, MaxGroupID, 1, MaxSimTimes})
                    end;
                _ ->        % false
                    gen_server:cast(self(), {do_simulation, MinGroupID + 1, MaxGroupID, 1, MaxSimTimes})
            end;
        _ ->        % false
            void
    end,
    {noreply, State};

handle_cast({battle_finish, {PlayerRoleID, MonsterGroupID, _MaxGroupID, _SimTimes, _MaxSimTimes, 
                             Winner, Rounds, PlayerHPList, MonHPList}}, State) ->
    ?I("PlayerRoleID = ~w, MonsterGroupID = ~w, Winner = ~w, Rounds = ~w, PlayerHPList = ~w, MonHPList = ~w", 
       [PlayerRoleID, MonsterGroupID, Winner, Rounds, PlayerHPList, MonHPList]),

    PlayerTotalHP = calc_role_total_hp(PlayerRoleID),
    MonTotalHP = calc_mon_total_hp(MonsterGroupID),
    PlayerRemHP = calc_rem_hp(PlayerHPList),
    MonRemHP = calc_rem_hp(MonHPList),

    gen_server:cast(batnitor_gui, {append_battle_result, {PlayerRoleID, MonsterGroupID, Winner, Rounds, 
                                                          PlayerRemHP/PlayerTotalHP, MonRemHP/MonTotalHP}}),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'DOWN', Ref, process, PID, Reason}, State) ->
    ?I("Ref = ~w, PID = ~w, Reason = ~p", [Ref, PID, Reason]),
    case erlang:erase(cur_battle_ref) of
        {Ref, MonGroupID, MaxGroupID, SimTimes, MaxSimTimes, RoleID} ->
            case Reason of
                normal ->
                    gen_server:cast(self(), {do_simulation, MonGroupID, MaxGroupID, SimTimes + 1, MaxSimTimes});
                noproc ->
                    gen_server:cast(self(), {do_simulation, MonGroupID, MaxGroupID, SimTimes + 1, MaxSimTimes});
                _ ->
                    gen_server:cast(batnitor_gui, {append_battle_result, 
                                                   {RoleID, MonGroupID, lists:flatten(io_lib:format("~p", [Reason])), 0, 0.0, 0.0}}),
                    gen_server:cast(self(), {do_simulation, MonGroupID, MaxGroupID, SimTimes + 1, MaxSimTimes})
            end;
        Other ->
            ?I("cur_battle_ref = ~w", [Other])
    end,
    {noreply, State};

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




start_one_battle(MonGroupID, MaxGroupID, SimTimes, MaxSimTimes) ->
    case data_mon_group:get(MonGroupID) of
        undefined ->
            gen_server:cast(batnitor_gui, {append_battle_result, 
                                           {0, MonGroupID, 
                                            lists:flatten(io_lib:format("Unknown monster group: ~p", [MonGroupID])), 
                                            0, 0.0, 0.0}}),
            error;
        MonGroup ->
            PosList = lists:keysort(2, MonGroup#mon_group.pos),
            case PosList of
                [{PlayerRoleID, _} | _] ->
                    case prepare_mon_attr(MonGroup) of
                        {ok, _} ->
                            ?I("Starting battle process...."),
                            Start = #battle_start {
                                mod = pve,
                                type = 0,
                                att_id = PlayerRoleID,
                                monster = MonGroupID,
                                caller = batnitor_simulator,
                                callback = {PlayerRoleID, MonGroupID, MaxGroupID, SimTimes, MaxSimTimes}
                            },

                            case battle:start(Start) of
                                {ok, PID} ->
                                    ?I("Battle process PID = ~w", [PID]),
                                    Ref = erlang:monitor(process, PID),
                                    erlang:put(cur_battle_ref, {Ref, MonGroupID, MaxGroupID, SimTimes, MaxSimTimes, PlayerRoleID}),
                                    ok;
                                {error, Reason} ->
                                    case Reason of
                                        {function_clause, [{data_skill, skill_info, [BadSkillID|_], _} | _]} ->
                                            gen_server:cast(batnitor_gui, {append_battle_result, 
                                                                           {PlayerRoleID, MonGroupID, 
                                                                            lists:flatten(io_lib:format("Unknown skill: ~p", [BadSkillID])), 
                                                                            0, 0.0, 0.0}});
                                        _ ->
                                            ?I("Reason = ~p", [Reason])
                                    end,
                                    error;
                                ignore ->
                                    gen_server:cast(batnitor_gui, {append_battle_result, 
                                                                   {PlayerRoleID, MonGroupID, 
                                                                    lists:flatten(io_lib:format("~p", [ignore])), 
                                                                    0, 0.0, 0.0}}),
                                    error
                            end;

                        {bad_roles, BadRolesList} ->
                            gen_server:cast(batnitor_gui, {append_battle_result, 
                                                           {PlayerRoleID, MonGroupID, 
                                                            lists:flatten(io_lib:format("Role ID not found: ~w", [BadRolesList])), 
                                                            0, 0.0, 0.0}}),
                            error
                    end;

                [] ->
                    gen_server:cast(batnitor_gui, {append_battle_result, 
                                                   {0, MonGroupID, 
                                                    lists:flatten(io_lib:format("Empty monster group: ~p", [MonGroupID])), 
                                                    0, 0.0, 0.0}}),
                    error
            end
    end.

prepare_mon_attr(MonGroup) ->
    F = fun({MonID, _}) ->
        RoleInfoR = ets:lookup(ets_role_rec, {0, MonID}),
        MiscInfoR = ets:lookup(ets_role_misc_rec, {0, MonID}),
        case RoleInfoR of
            [] -> {no_role, MonID};
            [RoleInfo] ->
                [MiscInfo] = MiscInfoR,
                {_, GuaiDaRen, RenDaGuai, NanDu, LeiXing, _Skills} = MiscInfo,
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
                data_mon_attr:set(NewAttr),
                NewAttr
        end
    end,

    NewAttrList = lists:map(F, MonGroup#mon_group.pos),
    BadRoleList = lists:filter(fun(E) -> element(1, E) =:= no_role end, NewAttrList),
    case BadRoleList of
        [] ->
            NewGroup = MonGroup#mon_group {
                level = 1,
                type  = soldier
            },
            data_mon_group:set(NewGroup),
            {ok, NewAttrList};
        _ ->
            {bad_roles, lists:usort([R || {_, R} <- BadRoleList])}
    end.

calc_mon_total_hp(MonsterGroupID) ->
    MonGroup = data_mon_group:get(MonsterGroupID),
    F = fun({MonID, _}, TotalHP) ->
        MonAttr = data_mon_attr:get(MonID),
        MonAttr#mon_attr.hp + TotalHP
    end,
    lists:foldl(F, 0, MonGroup#mon_group.pos).

calc_role_total_hp(PlayerRoleID) ->
    MList = mod_role:get_on_battle_list(PlayerRoleID),
    F = fun(R, TotalHP) ->
        R#role.gd_maxHp + TotalHP
    end,
    lists:foldl(F, 0, MList).

calc_rem_hp(HPList) ->
    F = fun({_, HP}, TotalHP) ->
        HP + TotalHP
    end,
    lists:foldl(F, 0, HPList).

calc_all_mon_attr(GroupID, MaxGroupID, AccList) when GroupID > MaxGroupID ->
    lists:reverse(AccList);
calc_all_mon_attr(GroupID, MaxGroupID, AccList) ->
    case data_mon_group:get(GroupID) of
        undefined ->
            calc_all_mon_attr(GroupID + 1, MaxGroupID, AccList);
        MonGroup ->
            PosList = lists:keysort(2, MonGroup#mon_group.pos),
            case PosList of
                [{_PlayerRoleID, _} | _] ->
                    case prepare_mon_attr(MonGroup) of
                        {ok, NewAttrList} ->
                            UniqAttrList = lists:ukeysort(#mon_attr.id, NewAttrList),
                            calc_all_mon_attr(GroupID + 1, MaxGroupID, [{GroupID, UniqAttrList} | AccList]);
                        {bad_roles, _} ->
                            calc_all_mon_attr(GroupID + 1, MaxGroupID, AccList)
                    end;

                [] ->
                    calc_all_mon_attr(GroupID + 1, MaxGroupID, AccList)
            end
    end.

