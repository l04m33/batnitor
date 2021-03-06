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
         gui_ref = none,
         roles_order = undefined
        }).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).


init(_) ->
    ?I("~s up and running.", [?MODULE]),
    erlang:process_flag(trap_exit, true),

    ets:new(ets_role_rec, [named_table, public, {keypos, #role.key}, set]),
    ets:new(ets_role_misc_rec, [named_table, public, {keypos, #misc_info.key}, set]),
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

handle_call(get_calculated_monster_attr, _From, State) ->
    Reply = lists:map(
        fun(M) ->
            case (catch prepare_mon_attr(M)) of
                {'EXIT', _Reason} ->
                    #mon_attr{
                        id      = M,
                        name    = "",
                        skills  = [],
                        _       = 0
                    };
                MonAttr ->
                    MonAttr
            end
        end,
        State#state.roles_order),
    {reply, Reply, State};

handle_call({get_rounds_list_by_mon_group, MonGroupID}, _From, State) ->
    Reply = case data_mon_group:get(MonGroupID) of
        undefined -> [];
        MonGroup ->
            MonIDList = lists:usort([M || {M, _P} <- MonGroup#mon_group.pos]),
            F = fun(M) ->
                case ets:lookup(ets_role_misc_rec, {0, M}) of
                    [MInfo] ->
                        #misc_info{
                            guai_da_ren = GuaiDaRen, 
                            ren_da_guai = RenDaGuai, 
                            nan_du = NanDu
                        } = MInfo,
                        {M, NanDu, GuaiDaRen, RenDaGuai};
                    [] ->
                        {no_role, M}
                end
            end,
            RoundsList = lists:map(F, MonIDList),
            BadRolesList = lists:filter(fun(E) -> element(1, E) =:= no_role end, RoundsList),
            case BadRolesList of
                [] -> RoundsList;
                _ -> []
            end
    end,
    {reply, Reply, State};

handle_call(get_roles_order, _From, State) ->
    Reply = State#state.roles_order,
    {reply, Reply, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast({set_role_list, RoleMiscList}, State) ->
    ets:delete_all_objects(ets_role_rec),
    ets:delete_all_objects(ets_role_misc_rec),
    {RoleList, MiscList} = lists:unzip(RoleMiscList),
    RolesOrder = [element(2, R#role.key) || R <- RoleList, is_integer(element(2, R#role.key))],
    ets:insert(ets_role_rec, RoleList),
    ets:insert(ets_role_misc_rec, MiscList),
    {noreply, State#state{roles_order = RolesOrder}};

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

handle_cast({battle_finish, {PlayerRoleID, MonsterGroupID, _MaxGroupID, SimTimes, _MaxSimTimes, 
                             Winner, Rounds, PlayerHPList, MonHPList}}, State) ->
    ?I("PlayerRoleID = ~w, MonsterGroupID = ~w, Winner = ~w, Rounds = ~w, PlayerHPList = ~w, MonHPList = ~w", 
       [PlayerRoleID, MonsterGroupID, Winner, Rounds, PlayerHPList, MonHPList]),

    PlayerTotalHP = calc_role_total_hp(PlayerRoleID),
    MonTotalHP = calc_mon_total_hp(MonsterGroupID),
    PlayerRemHP = calc_rem_hp(PlayerHPList),
    MonRemHP = calc_rem_hp(MonHPList),

    append_battle_result(PlayerRoleID, MonsterGroupID, SimTimes, Winner, Rounds,
                         PlayerRemHP/PlayerTotalHP, MonRemHP/MonTotalHP),

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
                    append_battle_result(RoleID, MonGroupID, SimTimes,
                                         lists:flatten(io_lib:format("~p", [Reason]))),
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
            append_battle_result(0, MonGroupID, SimTimes,
                                 lists:flatten(io_lib:format("Unknown monster group: ~p", [MonGroupID]))), 
            error;
        MonGroup ->
            PosList = lists:keysort(2, MonGroup#mon_group.pos),
            case PosList of
                [{PlayerRoleID, _} | _] ->
                    case (catch prepare_mon_attr(MonGroup)) of
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
                                            append_battle_result(PlayerRoleID, MonGroupID, SimTimes,
                                                                 lists:flatten(io_lib:format("Unknown skill: ~p", [BadSkillID]))); 
                                        _ ->
                                            append_battle_result(PlayerRoleID, MonGroupID, SimTimes,
                                                                 lists:flatten(io_lib:format("~p", [Reason]))) 
                                    end,
                                    error;
                                ignore ->
                                    append_battle_result(PlayerRoleID, MonGroupID, SimTimes,
                                                         lists:flatten(io_lib:format("~p", [ignore]))), 
                                    error
                            end;

                        {bad_roles, BadRolesList} ->
                            append_battle_result(PlayerRoleID, MonGroupID, SimTimes,
                                                 lists:flatten(io_lib:format("Role ID not found: ~w", [BadRolesList]))), 
                            error;
                        
                        {'EXIT', Reason} ->
                            append_battle_result(PlayerRoleID, MonGroupID, SimTimes,
                                                 lists:flatten(io_lib:format("prepare_mon_attr failed: ~w", [Reason]))),
                            error
                    end;

                [] ->
                    append_battle_result(0, MonGroupID, SimTimes,
                                         lists:flatten(io_lib:format("Empty monster group: ~p", [MonGroupID]))), 
                    error
            end
    end.

prepare_mon_attr(MonID) when is_integer(MonID) ->
    ?I("Preparing MonID = ~w", [MonID]),
    RoleInfoR = ets:lookup(ets_role_rec, {0, MonID}),
    MiscInfoR = ets:lookup(ets_role_misc_rec, {0, MonID}),
    case RoleInfoR of
        [] -> {no_role, MonID};
        [RoleInfo] ->
            [MiscInfo] = MiscInfoR,
            #misc_info{
                guai_da_ren = GuaiDaRen, 
                ren_da_guai = RenDaGuai, 
                nan_du = NanDu, 
                guai_lei_xing = LeiXing,
                guai_zhi_ye = ZhiYe
            } = MiscInfo,

            {AttackBoost, HPBoost, SpeedBoost} = case ZhiYe of
                1 -> {0.85,  1.2, 0.95};
                2 -> {   1,    1, 1.05};
                3 -> { 1.1,  0.8,    1};
                4 -> {   1,    1,    1}
            end,

            NewAttr = case data_mon_attr:get(MonID) of
                undefined ->
                    #mon_attr {
                        id      = MonID,
                        name    = "",
                        skills  = [],
                        _       = 1
                    };

                MonAttr ->
                    NewAttr0 = MonAttr#mon_attr {
                        cat         = LeiXing, 
                        level       = RoleInfo#role.gd_roleLevel,

                        hp          = erlang:round(
                                          HPBoost *
                                          RenDaGuai * RoleInfo#role.p_att * 
                                          (1 - (RoleInfo#role.p_def / (RoleInfo#role.p_def + RoleInfo#role.gd_roleLevel * 250)))),

                        p_att       = case LeiXing of
                                          %% physical attacker
                                          1 -> erlang:round(
                                                   AttackBoost *
                                                   RoleInfo#role.gd_maxHp / 
                                                   ((1 - RoleInfo#role.p_def / 
                                                     (RoleInfo#role.p_def + RoleInfo#role.gd_roleLevel * 250)) * GuaiDaRen));
                                          %% magical attacker
                                          3 -> 0
                                      end,

                        m_att       = case LeiXing of
                                          3 -> erlang:round(
                                                   AttackBoost *
                                                   RoleInfo#role.gd_maxHp / 
                                                   ((1 - RoleInfo#role.m_def / 
                                                     (RoleInfo#role.m_def + RoleInfo#role.gd_roleLevel * 250)) * GuaiDaRen));
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
                                          1 -> erlang:round(SpeedBoost * 0.9 * RoleInfo#role.gd_speed);
                                          %% elite
                                          2 -> erlang:round(SpeedBoost * 1.0 * RoleInfo#role.gd_speed);
                                          %% boss
                                          3 -> erlang:round(SpeedBoost * 1.1 * RoleInfo#role.gd_speed)
                                      end
                    },
                    data_mon_attr:set(NewAttr0),
                    NewAttr0
            end,

            NewAttr
    end;

prepare_mon_attr(MonGroup) when is_record(MonGroup, mon_group) ->
    F = fun({MonID, _}) ->
        prepare_mon_attr(MonID)
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

append_battle_result(PlayerRoleID, MonGroupID, SimTimes, 
                     Result, Rounds, PlayerHPRate, MonsterHPRate) ->
    gen_server:cast(batnitor_gui, {append_battle_result, 
                                   {PlayerRoleID, MonGroupID, SimTimes, Result, 
                                    Rounds, PlayerHPRate, MonsterHPRate}}).

append_battle_result(PlayerRoleID, MonGroupID, SimTimes, Result) ->
    gen_server:cast(batnitor_gui, {append_battle_result, 
                                   {PlayerRoleID, MonGroupID, SimTimes, Result, 
                                    0, 0.0, 0.0}}).

