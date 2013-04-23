-ifndef(__EXP_RETRIEVE_HRL__).
-define(__EXP_RETRIEVE_HRL__, true).

-record(exp_retrieve, {
    key         = {0, 0, 0},    % {PlayerID, Type, DayNum}
    gd_level    = 0,            % 玩家当天最高的等级
    gd_times    = 0,            % 当天活动参加次数
    gd_retrieved = 0,           % 是否已经找回过了……
    gd_extraData = 0            % 其他需要记下来参与计算的数据（像国主的等级之类……）
}).

-record(exp_retrieve_types, {
    key         = {{integer}, {integer}, {integer}},
    gd_level    = {integer},
    gd_times    = {integer},
    gd_retrieved = {integer},
    gd_extraData = {term}
}).

-record(exp_calc_info, {
    type  = 0,
    date  = {0, 0, 0},
    level = 0,
    units = 0,
    exp   = 0,
    need_silver = 0,
    need_gold   = 0
}).

-record(exp_retrieve_type_config, {
    type        = 0,
    max_times   = 0,
    round_to    = 0,
    min_level   = 0,
    unit_silver = 0,
    unit_gold   = 0,
    extra_data  = 0
}).

-define(EXP_RETRIEVE_TYPE_DAZUO,            1).     % 打坐
-define(EXP_RETRIEVE_TYPE_SHIMEN_TASK,      2).     % 师门任务
-define(EXP_RETRIEVE_TYPE_BANGPAI_TASK,     3).     % 帮派任务
-define(EXP_RETRIEVE_TYPE_XUANSHANG_TASK,   4).     % 悬赏任务
-define(EXP_RETRIEVE_TYPE_JUNGONG_TASK,     5).     % 军功任务

-define(EXP_RETRIEVE_TYPE_HULAOGUAN_FB,     6).     % 虎牢关副本
-define(EXP_RETRIEVE_TYPE_WUCHAO_FB,        7).     % 乌巢副本
-define(EXP_RETRIEVE_TYPE_CHANGBANPO_FB,    8).     % 长坂坡副本
-define(EXP_RETRIEVE_TYPE_CHIBI_FB,         9).     % 赤壁副本

-define(EXP_RETRIEVE_TYPE_DEFENCE_ACT,      10).    % 群魔乱舞活动
-define(EXP_RETRIEVE_TYPE_SHENJIAN_ACT,     11).    % 天灵神剑活动
-define(EXP_RETRIEVE_TYPE_NORMAL_FB,        12).    % 普通副本
-define(EXP_RETRIEVE_TYPE_ELITE_FB,         13).    % 精英副本

-define(EXP_RETRIEVE_ALL_TYPES, [1,2,3,4,5,10,11,12,13]).
-define(EXP_RETRIEVE_ALL_DEPRECATED_TYPES, [6,7,8,9]).

-endif.

