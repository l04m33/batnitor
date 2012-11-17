-module(batnitor_gui).
-behaviour(wx_object).

-include("batnitor.hrl").
-include("common.hrl").
-include_lib("wx/include/wx.hrl").

-export([
         start_link/0, 
         start_link/1,
         stop/0
        ]).

-export([
         init/1,
         terminate/2,
         code_change/3,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         handle_event/2
        ]).


-define(ID_OPEN_PLAYER_PROP_FILE,   10001).
-define(ID_OPEN_MONSTER_PROP_FILE,  10002).
-define(ID_OPEN_MONSTER_GROUP_FILE, 10003).
-define(ID_OPEN_VERSUS_FILE,        10004).
-define(ID_DO_SIMULATION,           10005).
-define(ID_SAVE_RESULTS,            10006).
-define(ID_SAVE_CALC_MONSTER_PROP,  10007).

-define(ID_TEXT_PLAYER_FILE,        20001).
-define(ID_TEXT_MONSTER_FILE,       20002).
-define(ID_TEXT_MONSTER_GROUP_FILE, 20003).
-define(ID_GRID_RESULTS,            20004).
-define(ID_TEXT_MIN_MONSTER_GROUP_ID, 20005).
-define(ID_TEXT_MAX_MONSTER_GROUP_ID, 20006).
-define(ID_TEXT_SIMULATION_TIMES,   20007).


-record(state, {
    main_frame = none,
    player_file_field = none,
    monster_file_field = none,
    monster_group_file_field = none,
    result_grid = none,
    min_mon_group_id_field = none,
    max_mon_group_id_field = none,
    sim_times_field = none}).


start_link() ->
    start_link([]).

start_link(Config) ->
    wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).

stop() ->
    gen_server:cast(?MODULE, stop).


init(Config) ->
    ?I("~s up and running.", [?MODULE]),
    erlang:process_flag(trap_exit, true),
    wx:new(Config),
    gen_server:cast(self(), read_saved_paths),
    wx:batch(fun () -> create_main_layout(Config) end).


handle_event(#wx{id = ?ID_OPEN_PLAYER_PROP_FILE, 
                 event = #wxCommand{type = command_menu_selected}}, State) ->
    case choose_file_by_dialog(State#state.main_frame, open) of
        {ok, FPath} ->
            case read_config_file(player_file, FPath, State) of
                ok ->
                    void;
                {error, illegal_file_format} ->
                    show_message(State#state.main_frame, "Illegal file format: " ++ FPath);
                {error, cannot_open_file} ->
                    show_message(State#state.main_frame, "Cannot open file: " ++ FPath)
            end;

        cancel -> void
    end,
    {noreply, State};

handle_event(#wx{id = ?ID_OPEN_MONSTER_PROP_FILE, 
                 event = #wxCommand{type = command_menu_selected}}, State) ->
    case choose_file_by_dialog(State#state.main_frame, open) of
        {ok, FPath} ->
            case read_config_file(monster_file, FPath, State) of
                ok ->
                    void;
                {error, illegal_file_format} ->
                    show_message(State#state.main_frame, "Illegal file format: " ++ FPath);
                {error, cannot_open_file} ->
                    show_message(State#state.main_frame, "Cannot open file: " ++ FPath)
            end;

        cancel -> void
    end,
    {noreply, State};

handle_event(#wx{id = ?ID_OPEN_MONSTER_GROUP_FILE, 
                 event = #wxCommand{type = command_menu_selected}}, State) ->
    case choose_file_by_dialog(State#state.main_frame, open) of
        {ok, FPath} ->
            case read_config_file(monster_group_file, FPath, State) of
                ok ->
                    void;
                {error, illegal_file_format} ->
                    show_message(State#state.main_frame, "Illegal file format: " ++ FPath);
                {error, cannot_open_file} ->
                    show_message(State#state.main_frame, "Cannot open file: " ++ FPath)
            end;

        cancel -> void
    end,
    {noreply, State};

handle_event(#wx{id = ?ID_SAVE_CALC_MONSTER_PROP, 
                 event = #wxCommand{type = command_menu_selected}}, State) ->
    case choose_file_by_dialog(State#state.main_frame, save) of
        {ok, FPath} ->
            MinGroupIDStr = wxTextCtrl:getValue(State#state.min_mon_group_id_field),
            MaxGroupIDStr = wxTextCtrl:getValue(State#state.max_mon_group_id_field),
            {MinGroupID, MaxGroupID} = try
                {list_to_integer(MinGroupIDStr), list_to_integer(MaxGroupIDStr)}
            catch _:_ ->
                {0, 0}
            end,
            case MinGroupID > 0 andalso MaxGroupID > 0 of
                true ->
                    case file:open(FPath, write) of
                        {ok, FHandle} ->
                            MonList = gen_server:call(batnitor_simulator, 
                                                      {get_calculated_monster_attr, MinGroupID, MaxGroupID}),
                            write_mon_attr(MonList, FHandle),
                            file:close(FHandle);
                        _ ->
                            show_message(State#state.main_frame, "Cannot open file: " ++ FPath)
                    end;

                _ ->        % false
                    show_message(State#state.main_frame, "Illegal Monster Group IDs: " ++ 
                                                         MinGroupIDStr ++ " - " ++ MaxGroupIDStr)
            end;

        cancel -> void
    end,
    {noreply, State};

handle_event(#wx{id = ?ID_SAVE_RESULTS, 
                 event = #wxCommand{type = command_menu_selected}}, State) ->
    case choose_file_by_dialog(State#state.main_frame, save) of
        {ok, FPath} ->
            case file:open(FPath, write) of
                {ok, FHandle} ->
                    write_results(State#state.result_grid, FHandle),
                    file:close(FHandle);
                _ ->
                    show_message(State#state.main_frame, "Cannot open file: " ++ FPath)
            end;

        cancel -> void
    end,
    {noreply, State};

handle_event(#wx{id = ?wxID_EXIT, 
                 event = #wxCommand{type = command_menu_selected}}, State) ->
    stop(),
    {noreply, State};

handle_event(#wx{id = ?ID_DO_SIMULATION, 
                 event = #wxCommand{type = command_menu_selected}}, State) ->
    case gen_server:call(batnitor_simulator, check_data_all_set) of
        {false, EmptyType} ->
            ErrMsg = case EmptyType of
                role            -> "Player config file not specified.";
                monster_attr    -> "Monster config file not specified.";
                monster_group   -> "Monster group config file not specified."
            end,
            show_message(State#state.main_frame, ErrMsg);
        true ->
            MinGroupIDStr = wxTextCtrl:getValue(State#state.min_mon_group_id_field),
            MaxGroupIDStr = wxTextCtrl:getValue(State#state.max_mon_group_id_field),
            SimTimesStr = wxTextCtrl:getValue(State#state.sim_times_field),
            try
                MinGroupID = list_to_integer(MinGroupIDStr),
                MaxGroupID = list_to_integer(MaxGroupIDStr),
                SimTimes =  list_to_integer(SimTimesStr),
                %wxGrid:clearGrid(State#state.result_grid),
                NumRows = wxGrid:getNumberRows(State#state.result_grid),
                wxGrid:deleteRows(State#state.result_grid, [{numRows, NumRows}]),
                gen_server:cast(batnitor_simulator, {do_simulation, MinGroupID, MaxGroupID, 1, SimTimes})
            catch _:_ ->
                show_message(State#state.main_frame, "Illegal Monster Group IDs or Bad Simulation Times: " ++ 
                                                     MinGroupIDStr ++ " - " ++ MaxGroupIDStr ++ " : " ++ SimTimesStr)
            end
    end,
    {noreply, State};

handle_event(#wx{id = ?ID_GRID_RESULTS,
                 event = #wxGrid{type = grid_label_left_click, row = Row}}, State) ->
    ?I("Row = ~w", [Row]),
    case Row >= 0 of
        true ->
            MonGroupIDStr = wxGrid:getCellValue(State#state.result_grid, Row, 0),
            MonGroupID = try
                list_to_integer(MonGroupIDStr)
            catch _:_ ->
                0
            end,
            case MonGroupID > 0 of
                true ->
                    SimTimes = list_to_integer(wxGrid:getCellValue(State#state.result_grid, Row, 2)),
                    case erlang:erase({grid_expanded, MonGroupID, SimTimes}) of
                        undefined ->
                            RoundsList = gen_server:call(batnitor_simulator, {get_rounds_list_by_mon_group, MonGroupID}),
                            wxGrid:insertRows(State#state.result_grid, [{pos, Row + 1}, {numRows, length(RoundsList) + 1}]),
                            set_cell_value(State#state.result_grid, Row + 1, 1, "Monster ID", 
                                           read_only, {150, 150, 150}),
                            set_cell_value(State#state.result_grid, Row + 1, 2, "Monster Type", 
                                           read_only, {150, 150, 150}),
                            set_cell_value(State#state.result_grid, Row + 1, 3, "Guai Da Ren", 
                                           read_only, {150, 150, 150}),
                            set_cell_value(State#state.result_grid, Row + 1, 4, "Ren Da Guai", 
                                           read_only, {150, 150, 150}),
                            fill_expanded_rows(State#state.result_grid, RoundsList, Row + 2),
                            erlang:put({grid_expanded, MonGroupID, SimTimes}, length(RoundsList) + 1);

                        NumExpandedRows ->
                            wxGrid:deleteRows(State#state.result_grid, [{pos, Row + 1}, {numRows, NumExpandedRows}])
                    end;
                false ->
                    void
            end;

        false ->
            void
    end,
    {noreply, State};

handle_event(#wx{id = ?ID_GRID_RESULTS,
                 event = #wxGrid{type = grid_cell_change, row = Row, col = Col}}, State) ->
    case Col of
        3 ->        % Guai Da Ren
            do_update_rounds_value(State#state.result_grid, Row, Col);
        4 ->        % Ren Da Guai
            do_update_rounds_value(State#state.result_grid, Row, Col);
        _ ->
            void
    end,
    {noreply, State};

handle_event(WX, State) ->
    ?I("wx event: ~w", [WX]),
    {noreply, State}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(read_saved_paths, State) ->
    case file:consult("file_paths.config") of
        {ok, FilePathList} ->
            F = fun({Type, FPath}) ->
                case FPath of
                    undefined -> void;
                    _ -> read_config_file(Type, FPath, State)
                end
            end,
            lists:foreach(F, FilePathList);

        _ -> void
    end,
    {noreply, State};

handle_cast({append_battle_result, {PlayerRoleID, MonsterGroupID, SimTimes, Winner, Rounds, 
                                    PlayerHPRate, MonsterHPRate}}, State) ->
    %wxGrid:appendRows(State#state.result_grid, [{numRows, 1}]),
    %RowID = wxGrid:getNumberRows(State#state.result_grid) - 1,
    RowID = get_row_id_by_mon_group(State#state.result_grid, MonsterGroupID, SimTimes),

    set_cell_value(State#state.result_grid, RowID, 0, integer_to_list(MonsterGroupID)),
    set_cell_value(State#state.result_grid, RowID, 1, integer_to_list(PlayerRoleID)),
    set_cell_value(State#state.result_grid, RowID, 2, integer_to_list(SimTimes)),
    set_cell_value(State#state.result_grid, RowID, 3, integer_to_list(Rounds)),
    set_cell_value(State#state.result_grid, RowID, 4, 
                   lists:flatten(io_lib:format("~.2f", [PlayerHPRate*100])) ++ "%"),
    set_cell_value(State#state.result_grid, RowID, 5, 
                   lists:flatten(io_lib:format("~.2f", [MonsterHPRate*100])) ++ "%"),

    case Winner of
        att ->
            set_cell_value(State#state.result_grid, RowID, 6, "Win", read_only, ?wxGREEN);
        def ->
            set_cell_value(State#state.result_grid, RowID, 6, "Lose", read_only, {255, 200, 0});
        _ ->
            set_cell_value(State#state.result_grid, RowID, 6, Winner, read_only, ?wxRED)
    end,
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ?I("~s shutting down.", [?MODULE]),
    case file:open("file_paths.config", write) of
        {ok, FHandle} ->
            PList = [{player_file, erlang:get(player_file)},
                     {monster_file, erlang:get(monster_file)},
                     {monster_group_file, erlang:get(monster_group_file)}],
            F = fun(T) ->
                io:format(FHandle, "~p.~n", [T])
            end,
            lists:foreach(F, PList),
            file:close(FHandle);

        _ -> void
    end,
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




create_main_layout(_Config) ->
    MainFrame = wxFrame:new(wx:null(), ?wxID_ANY, "Batnitor", [{size, {1000, 700}}]),

    create_menu_bar(MainFrame),
    create_status_bar(MainFrame),
    MainPanel = wxPanel:new(MainFrame, []),
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    wxPanel:setSizer(MainPanel, MainSizer),
    {PlayerPropField, MonAttrField, MonGroupField} = create_file_name_fields(MainPanel, MainSizer),
    {MinGroupIDField, MaxGroupIDField, SimTimesField} = create_monster_group_id_fields(MainPanel, MainSizer),
    ResultGrid = create_grid(MainPanel, MainSizer),

    wxFrame:show(MainFrame),
    {MainFrame, #state{main_frame = MainFrame,
                       player_file_field = PlayerPropField,
                       monster_file_field = MonAttrField,
                       monster_group_file_field = MonGroupField,
                       result_grid = ResultGrid,
                       min_mon_group_id_field = MinGroupIDField,
                       max_mon_group_id_field = MaxGroupIDField,
                       sim_times_field = SimTimesField}}.

create_monster_group_id_fields(Panel, Sizer) ->
    ParentSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, []),
    GIDSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, "Min && Max Monster Group IDs"}]),
    TimesSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, "Simulation Times"}]),

    MinText = wxTextCtrl:new(Panel, ?ID_TEXT_MIN_MONSTER_GROUP_ID, [{value, "1"}, {style, ?wxDEFAULT}, {size, {120, 29}}]),
    MaxText = wxTextCtrl:new(Panel, ?ID_TEXT_MAX_MONSTER_GROUP_ID, [{value, "1"}, {style, ?wxDEFAULT}, {size, {120, 29}}]),
    wxSizer:add(GIDSizer, MinText, []),
    wxSizer:addSpacer(GIDSizer, 10),
    wxSizer:add(GIDSizer, MaxText, []),

    SimTimesText = wxTextCtrl:new(Panel, ?ID_TEXT_SIMULATION_TIMES, [{value, "1"}, {style, ?wxDEFAULT}, {size, {120, 30}}]),
    wxSizer:add(TimesSizer, SimTimesText, []),

    wxSizer:add(ParentSizer, GIDSizer, [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(ParentSizer, 40),
    wxSizer:add(ParentSizer, TimesSizer, [{flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, ParentSizer, [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(Sizer, 10),
    {MinText, MaxText, SimTimesText}.

create_grid(Panel, Sizer) ->
    GSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Results"}]),
    Grid = wxGrid:new(Panel, ?ID_GRID_RESULTS, []),
    wxGrid:createGrid(Grid, 0, 7),

    wxGrid:setColLabelValue(Grid, 0, "Monster Group ID"),
    wxGrid:setColSize(Grid, 0, 150),

    wxGrid:setColLabelValue(Grid, 1, "Player Role ID"),
    wxGrid:setColSize(Grid, 1, 120),

    wxGrid:setColLabelValue(Grid, 2, "Sim. Times"),
    wxGrid:setColSize(Grid, 2, 120),

    wxGrid:setColLabelValue(Grid, 3, "Rounds"),
    wxGrid:setColSize(Grid, 3, 120),

    wxGrid:setColLabelValue(Grid, 4, "Player HP"),
    wxGrid:setColSize(Grid, 4, 120),

    wxGrid:setColLabelValue(Grid, 5, "Monster HP"),
    wxGrid:setColSize(Grid, 5, 120),

    wxGrid:setColLabelValue(Grid, 6, "Result"),
    wxGrid:setColSize(Grid, 6, 220),

    wxSizer:add(GSizer, Grid, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(Sizer, GSizer, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxGrid:enableEditing(Grid, true),
    wxGrid:connect(Grid, grid_label_left_click, []), %[{skip, true}]),
    wxGrid:connect(Grid, grid_cell_change, []),
    Grid.

create_file_name_fields(Panel, Sizer) ->
    PSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Player Config File: "}]),
    MASizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Monster Config File: "}]),
    MGSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Monster Group Config File: "}]),

    PText = wxTextCtrl:new(Panel, ?ID_TEXT_PLAYER_FILE, [{value, "none"}, {style, ?wxDEFAULT}]),
    wxTextCtrl:setEditable(PText, false),
    MAText = wxTextCtrl:new(Panel, ?ID_TEXT_MONSTER_FILE, [{value, "none"}, {style, ?wxDEFAULT}]),
    wxTextCtrl:setEditable(MAText, false),
    MGText = wxTextCtrl:new(Panel, ?ID_TEXT_MONSTER_GROUP_FILE, [{value, "none"}, {style, ?wxDEFAULT}]),
    wxTextCtrl:setEditable(MGText, false),

    wxSizer:add(PSizer, PText, [{flag, ?wxEXPAND}]),
    wxSizer:add(MASizer, MAText, [{flag, ?wxEXPAND}]),
    wxSizer:add(MGSizer, MGText, [{flag, ?wxEXPAND}]),

    wxSizer:add(Sizer, PSizer, [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(Sizer, 10),
    wxSizer:add(Sizer, MASizer, [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(Sizer, 10),
    wxSizer:add(Sizer, MGSizer, [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(Sizer, 10),

    {PText, MAText, MGText}.

create_menu_bar(Frame) ->
    MainMenuBar = wxMenuBar:new(),

    FileMenu = wxMenu:new([]),
    wxMenu:append(FileMenu, ?ID_OPEN_PLAYER_PROP_FILE, "Open &Player Config"),
    wxMenu:append(FileMenu, ?ID_OPEN_MONSTER_PROP_FILE, "Open &Monster Config"),
    wxMenu:append(FileMenu, ?ID_OPEN_MONSTER_GROUP_FILE, "Open Monster &Group Config"),
    wxMenu:appendSeparator(FileMenu),
    wxMenu:append(FileMenu, ?ID_SAVE_RESULTS, "Save &Results"),
    wxMenu:append(FileMenu, ?ID_SAVE_CALC_MONSTER_PROP, "Save &Calculated Monster Attributes"),
    wxMenu:appendSeparator(FileMenu),
    wxMenu:append(FileMenu, ?wxID_EXIT, "&Quit"),
    wxMenuBar:append(MainMenuBar, FileMenu, "&File"),

    SimMenu = wxMenu:new([]),
    wxMenu:append(SimMenu, ?ID_DO_SIMULATION, "&Do Simulation"),
    wxMenuBar:append(MainMenuBar, SimMenu, "&Simulation"),

    wxFrame:setMenuBar(Frame, MainMenuBar),
    wxFrame:connect(Frame, command_menu_selected).

create_status_bar(Frame) ->
    wxFrame:createStatusBar(Frame, []).

choose_file_by_dialog(MainFrame, OpenOrSave) ->
    Style = case OpenOrSave of
        open -> ?wxFD_OPEN;
        save -> ?wxFD_SAVE
    end,
    FDialog = wxFileDialog:new(MainFrame, [{style, Style}]),
    Ret = case wxFileDialog:showModal(FDialog) of
        ?wxID_OK ->
            FPath = wxFileDialog:getPath(FDialog),
            {ok, FPath};
        ?wxID_CANCEL ->
            cancel
    end,
    wxFileDialog:destroy(FDialog),
    Ret.

parse_csv_line({eof}, AccList) -> AccList;
parse_csv_line({newline, Line}, AccList) ->
    ?I("Line = ~p", [Line]),
    [Line | AccList].

show_message(MainFrame, Msg) ->
    MsgD = wxMessageDialog:new(MainFrame, Msg),
    wxMessageDialog:showModal(MsgD),
    wxMessageDialog:destroy(MsgD).

string_to_term(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.

row_to_role([ID, DengJi, GongJi, FangYu, Xue, SuDu, MingZhong, ShanBi, BaoJi, 
                  XingYun, GeDang, FanJi, PoJia, ZhiMing, GuaiDaRen, RenDaGuai, NanDu,
                  GuaiWuLeiXing, Skill1, Skill2, Skill3, Skill4, Skill5, Skill6]) ->
    SkillsList = lists:zip(lists:seq(1, 6), 
                           lists:map(fun string_to_term/1, 
                                     [Skill1, Skill2, Skill3, Skill4, Skill5, Skill6])),
    ?I("SkillsList = ~p", [SkillsList]),
    ValidSkillsList = lists:filter(fun({_, S}) -> is_list(S) andalso length(S) > 0 end, SkillsList),
    {#role {
        key                = {0, string_to_term(ID)},       %% 佣兵记录的key为一个记录:{player_id, mer_id}
        gd_roleRank        = 0,                             %% 角色类别，1：为领主佣兵，0：其他佣兵
        gd_isFired         = 0,                             %% 是否为玩家已解雇的佣兵，0：没解雇， 1：已解雇
        gd_roleLevel       = string_to_term(DengJi),        %% 等级
        gd_isBattle        = 1,                             %% 是否出战，若不出战，则为0，否则为站位的位子（1到6）
        gd_exp             = 0,                             %% 当前经验
        gd_skill           = [],                            %% 技能列表（运行时生成）
        
        %% 4 foster attribute
        gd_fliliang        = 0,                             %% 培养所获得腕力
        gd_fyuansheng      = 0,                             %% 培养所获得元神
        gd_ftipo           = 0,                             %% 培养所获得体魄
        gd_fminjie         = 0,                             %% 培养所获得敏捷
        
        gd_tliliang        = 0,                             %% 腕力天赋的提升值
        gd_tyuansheng      = 0,                             %% 元神天赋的提升值
        gd_ttipo           = 0,                             %% 体魄天赋的提升值
        gd_tminjie         = 0,                             %% 敏捷天赋的提升值
        
        gd_careerID        = 1,                             %% 职业类型编号 % TODO: 会影响战斗～要设置？
        gd_roleSex         = 0,                             %% 性别，0：男，1：女
        %% 4个基础属性
        gd_liliang         = 0,                             %% 腕力
        gd_yuansheng       = 0,                             %% 元神
        gd_tipo            = 0,                             %% 体魄
        gd_minjie          = 0,                             %% 敏捷	
        
        gd_liliangTalent   = 0,                             %% 腕力的天赋
        gd_yuanshengTalent = 0,                             %% 元神的天赋
        gd_tipoTalent      = 0,                             %% 体魄的天赋
        gd_minjieTalent    = 0,                             %% 敏捷的天赋
        
        gd_speed           = string_to_term(SuDu),          %% 攻击速度
        gd_baoji           = string_to_term(BaoJi),         %% 暴击
        gd_shanbi          = string_to_term(ShanBi),        %% 闪避
        gd_gedang          = string_to_term(GeDang),        %% 格挡
        gd_mingzhong       = string_to_term(MingZhong),     %% 命中率
        gd_zhiming         = string_to_term(ZhiMing),       %% 致命
        gd_xingyun         = string_to_term(XingYun),       %% 幸运
        gd_fanji           = string_to_term(FanJi),         %% 反击
        gd_pojia           = string_to_term(PoJia),         %% 破甲

        gd_currentHp       = string_to_term(Xue),           %% 当前血量
        gd_maxHp           = string_to_term(Xue),           %% 最大血量
        p_def              = string_to_term(FangYu),        %% 物理防御
        m_def              = string_to_term(FangYu),        %% 魔法防御
        p_att              = string_to_term(GongJi),        %% 攻击力
        m_att              = string_to_term(GongJi),        %% 魔攻
        star_lv            = 1,                             %% 武将星级     %% TODO: 要设置？

        gd_name            = ""                             %% 佣兵名称
    }, 
    {
        {0, string_to_term(ID)},
        string_to_term(GuaiDaRen),
        string_to_term(RenDaGuai),
        string_to_term(NanDu),
        string_to_term(GuaiWuLeiXing),
        ValidSkillsList
    }}.

row_to_mon_attr([ID, MingZhong, ShanBi, BaoJi, XingYun, GeDang, FanJi, PoJia, _ZhiMing, JiNeng]) ->
    #mon_attr {
        id          = string_to_term(ID),
		name        = "",
		cat         = 0,        % generated on the fly
		level       = 0,        % generated on the fly
		hp          = 0,        % generated on the fly
		mp          = 0,
		p_att       = 0,        % generated on the fly
		m_att       = 0,        % generated on the fly
		p_def       = 0,        % generated on the fly
		m_def       = 0,        % generated on the fly
		speed       = 0,        % generated on the fly
  		hit         = string_to_term(MingZhong),
 		dodge       = string_to_term(ShanBi),
 		crit        = string_to_term(BaoJi),
 		luck        = string_to_term(XingYun),
		break       = string_to_term(PoJia),
		agility     = 0,
      	strength    = 0,
		block       = string_to_term(GeDang),
		counter     = string_to_term(FanJi),
      	spirit      = 0,
      	physique    = 0,
        skills      = string_to_term(JiNeng),
 		star        = 1         % TODO
	}.

row_to_mon_group([GroupID, Mon1, Mon2, Mon3, Mon4, Mon5, Mon6]) ->
    L = [{string_to_term(Mon1), 1},
         {string_to_term(Mon2), 2},
         {string_to_term(Mon3), 3},
         {string_to_term(Mon4), 4},
         {string_to_term(Mon5), 5},
         {string_to_term(Mon6), 6}],
    PosList = lists:filter(fun({M, _P}) -> M =/= 0 end, L),
    #mon_group {
        id          = string_to_term(GroupID),
        level       = 0,        % TODO: generated on the fly
        type        = soldier,  % TODO: generated on the fly
        pos         = PosList,
        pts         = 0,
        items       = [],
        exp         = 0,
        silver      = 0,
        drop_type   = unified
    }.

read_csv_rows_from_file(FPath) when is_list(FPath) ->
    case file:open(FPath, [read]) of
        {ok, FHandle} ->
            {_, PlayerPropList} = ecsv:process_csv_file_with(FHandle, fun parse_csv_line/2, []),
            file:close(FPath),
            {ok, PlayerPropList, FPath};
        _ ->
            {error, cannot_open_file}
    end.

write_mon_attr(MonGroupList, FHandle) ->
    %io:format(FHandle, "\"怪物堆ID\",\"怪物ID\",\"怪物类型\",\"等级\",\"生命\",\"物攻\",\"魔攻\",\"物防\",\"魔防\",\"速度\"~n"),
    io:format(FHandle, "\"Monster Group ID\",\"Monster ID\",\"Type\",\"Level\",\"HP\","
                       "\"Physical Attack\",\"Magical Attack\",\"Physical Defence\",\"Magical Defence\",\"Speed\"~n", []),
    G = fun({GroupID, MonList}) ->
        F = fun(Mon) ->
            io:format(FHandle, "~w,~w,~w,~w,~w,~w,~w,~w,~w,~w~n", 
                      [GroupID,
                       Mon#mon_attr.id,
                       Mon#mon_attr.cat,
                       Mon#mon_attr.level,
                       Mon#mon_attr.hp,
                       Mon#mon_attr.p_att,
                       Mon#mon_attr.m_att,
                       Mon#mon_attr.p_def,
                       Mon#mon_attr.m_def,
                       Mon#mon_attr.speed])
        end,
        lists:foreach(F, MonList)
    end,
    lists:foreach(G, MonGroupList).

write_results(Grid, FHandle) ->
    io:format(FHandle, "\"Player Role ID\",\"Monster Group ID\",\"Rounds\",\"Player HP\",\"Monster HP\",\"Result\"~n", []),
    NumRows = wxGrid:getNumberRows(Grid),
    write_results(Grid, FHandle, 0, NumRows).

write_results(_Grid, _FHandle, Row, MaxRow) when Row >= MaxRow ->
    ok;
write_results(Grid, FHandle, Row, MaxRow) ->
    io:format(FHandle, "~s,~s,~s,\"~s\",\"~s\",\"~s\"~n", 
              [wxGrid:getCellValue(Grid, Row, 0),
               wxGrid:getCellValue(Grid, Row, 1),
               wxGrid:getCellValue(Grid, Row, 2),
               wxGrid:getCellValue(Grid, Row, 3),
               wxGrid:getCellValue(Grid, Row, 4),
               escape_stack_trace(wxGrid:getCellValue(Grid, Row, 5), [])]),
    write_results(Grid, FHandle, Row + 1, MaxRow).

escape_stack_trace([], Acc) -> 
    lists:reverse(Acc);
escape_stack_trace([C | Rest], Acc) ->
    case C of
        $\n ->
            escape_stack_trace(Rest, [$\s | Acc]);
        $\r ->
            escape_stack_trace(Rest, Acc);
        $" ->
            escape_stack_trace(Rest, [$' | Acc]);
        _ ->
            escape_stack_trace(Rest, [C | Acc])
    end.

fill_expanded_rows(_Grid, [], _RowID) -> ok;
fill_expanded_rows(Grid, [{RoleID, NanDu, GuaiDaRen, RenDaGuai} | Rest], RowID) ->
    set_cell_value(Grid, RowID, 1, integer_to_list(RoleID), read_only, {200, 200, 200}),
    set_cell_value(Grid, RowID, 2, integer_to_list(NanDu), read_only, {200, 200, 200}),
    set_cell_value(Grid, RowID, 3, lists:flatten(io_lib:format("~w", [GuaiDaRen])), 
                   read_write, {200, 200, 200}),
    set_cell_value(Grid, RowID, 4, lists:flatten(io_lib:format("~w", [RenDaGuai])), 
                   read_write, {200, 200, 200}),
    fill_expanded_rows(Grid, Rest, RowID + 1).

read_config_file(player_file, FPath, State) ->
    case read_csv_rows_from_file(FPath) of
        {ok, RowList, FPath} ->
            try
                gen_server:cast(batnitor_simulator, {set_role_list, lists:map(fun row_to_role/1, RowList)}),
                erlang:put(player_file, FPath),
                wxTextCtrl:setValue(State#state.player_file_field, FPath),
                ok
            catch Type:Err ->
                ?E("Type = ~p; Err = ~p", [Type, Err]),
                {error, illegal_file_format}
            end;

        Err -> Err
    end;

read_config_file(monster_file, FPath, State) ->
    case read_csv_rows_from_file(FPath) of
        {ok, RowList, FPath} ->
            try
                gen_server:cast(batnitor_simulator, {set_monster_attr_list, lists:map(fun row_to_mon_attr/1, RowList)}),
                erlang:put(monster_file, FPath),
                wxTextCtrl:setValue(State#state.monster_file_field, FPath),
                ok
            catch Type:Err ->
                ?E("Type = ~p; Err = ~p", [Type, Err]),
                {error, illegal_file_format}
            end;

        Err -> Err
    end;

read_config_file(monster_group_file, FPath, State) ->
    case read_csv_rows_from_file(FPath) of
        {ok, RowList, FPath} ->
            try
                gen_server:cast(batnitor_simulator, {set_monster_group_list, lists:map(fun row_to_mon_group/1, RowList)}),
                erlang:put(monster_group_file, FPath),
                wxTextCtrl:setValue(State#state.monster_group_file_field, FPath),
                ok
            catch Type:Err ->
                ?E("Type = ~p; Err = ~p", [Type, Err]),
                {error, illegal_file_format}
            end;

        Err -> Err
    end.

set_cell_value(Grid, Row, Col, Value) ->
    set_cell_value(Grid, Row, Col, Value, read_only, undefined).

%set_cell_value(Grid, Row, Col, Value, ReadOnly) ->
%    set_cell_value(Grid, Row, Col, Value, ReadOnly, undefined).

set_cell_value(Grid, Row, Col, Value, ReadOnly, BGColor) ->
    case BGColor of
        undefined -> void;
        _ -> wxGrid:setCellBackgroundColour(Grid, Row, Col, BGColor)
    end,
    case ReadOnly of
        read_only ->
            wxGrid:setReadOnly(Grid, Row, Col, [{isReadOnly, true}]);
        read_write ->
            wxGrid:setReadOnly(Grid, Row, Col, [{isReadOnly, false}])
    end,
    wxGrid:setCellAlignment(Grid, Row, Col, ?wxALIGN_CENTER, ?wxALIGN_CENTER),
    wxGrid:setCellValue(Grid, Row, Col, Value).

get_row_id_by_mon_group(Grid, MonsterGroupID, SimTimes) ->
    RowsNum = wxGrid:getNumberRows(Grid),
    get_row_id_by_mon_group(Grid, MonsterGroupID, 1, SimTimes, 0, RowsNum).

get_row_id_by_mon_group(Grid, _MonsterGroupID, _SumSimTimes, _SimTimes, RowID, RowsNum) when RowID >= RowsNum ->
    wxGrid:appendRows(Grid, [{numRows, 1}]),
    RowsNum;
get_row_id_by_mon_group(Grid, MonsterGroupID, SumSimTimes, SimTimes, RowID, RowsNum) ->
    case list_to_integer(wxGrid:getCellValue(Grid, RowID, 0)) of
        MonsterGroupID when is_integer(MonsterGroupID) ->
            case SumSimTimes of
                SimTimes ->
                    RowID;
                _ ->
                    get_row_id_by_mon_group(Grid, MonsterGroupID, SumSimTimes + 1, SimTimes, RowID + 1, RowsNum)
            end;
        _ ->
            get_row_id_by_mon_group(Grid, MonsterGroupID, 1, SimTimes, RowID + 1, RowsNum)
    end.

locate_prev_mon_group(Grid, RowID) ->
    case RowID >= 1 of
        true ->
            VStr = wxGrid:getCellValue(Grid, RowID - 1, 0),
            case length(VStr) of
                0 -> locate_prev_mon_group(Grid, RowID - 1);
                _ -> {list_to_integer(VStr), list_to_integer(wxGrid:getCellValue(Grid, RowID - 1, 2))}
            end;
        false ->
            {0, 0}
    end.

do_update_rounds_value(Grid, Row, Col) ->
    VStr = wxGrid:getCellValue(Grid, Row, Col),
    NewValue = try
        list_to_integer(VStr)
    catch _:_ -> 
        try
            list_to_float(VStr)
        catch _:_ ->
            0
        end
    end,

    case NewValue of
        0 -> 
            show_message(Grid, "Illegal Value: " ++ VStr);
        _ ->
            RoleID = list_to_integer(wxGrid:getCellValue(Grid, Row, 1)),
            ets:update_element(ets_role_misc_rec, {0, RoleID}, {Col - 1, NewValue}),
            {MonGroupID, SimTimes} = locate_prev_mon_group(Grid, Row),
            gen_server:cast(batnitor_simulator, {do_simulation, MonGroupID, MonGroupID, SimTimes, SimTimes})
    end.

