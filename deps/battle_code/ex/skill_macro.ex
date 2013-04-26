defmodule SkillMacro do
    # 有几个变量在handle_skill函数里总是可用的：
    #     _tar_list_         - 目标站位列表，保存目前处理中的目标，用select宏更改
    #     __att_info_list__  - 攻击过程列表，保存每个角色动作的描述，用attack或者assist宏更改
    #     __last_att_info_list__ - 上次的攻击过程列表，保存每个角色动作的描述，用attack或者assist宏更改
    #     __buff_spec_list__ - buff声明列表，保存所有技能处理结束时需要添加的buff，用buff宏更改
    #     __tmp_buff_list__  - 临时buff列表，用tmp_buff宏更改
    #     _battle_data_      - 当前的战斗状态
    #
    # 以下变量是只读的，可以用来参与计算
    #
    #     _level_           - 当前技能的等级
    #     _src_             - 攻击者站位
    #     _self_            - 攻击者站位，_src_的别名
    #     _tar_             - AI选择的目标站位
    #     _param_           - 技能参数
    #

    defrecord AttackInfo, 
        Record.extract(:attack_info, from: "include/battle.hrl")
    defrecord AttackSpec, 
        Record.extract(:attack_spec, from: "include/battle.hrl")
    defrecord AssistSpec, 
        Record.extract(:assist_spec, from: "include/battle.hrl")
    defrecord Buff, 
        Record.extract(:buff, from: "include/battle.hrl")
    defrecord BattleStatus,
        Record.extract(:battle_status, from: "include/battle.hrl")
    defrecord BattleData,
        Record.extract(:battle_data, from: "include/battle.hrl")

    defmacro p(idx) do
        quote do
            elem(var!(_param_), unquote(idx) - 1)
        end
    end

    defmacro missed() do
        quote do
            var!(__last_att_info_list__) != []
                and :lists.all(
                        fn(a) ->
                            elem(a, SkillMacro.AttackInfo.__index__(:is_miss))
                        end,
                        var!(__last_att_info_list__))
        end
    end

    defmacro hit() do
        quote do
            var!(__last_att_info_list__) != []
                and :lists.any(
                        fn(a) ->
                            not elem(a, SkillMacro.AttackInfo.__index__(:is_miss))
                        end,
                        var!(__last_att_info_list__))
        end
    end

    defmacro is_critical() do
        quote do
            var!(__last_att_info_list__) != []
                and :lists.any(
                        fn(a) ->
                            elem(a, SkillMacro.AttackInfo.__index__(:is_crit))
                        end,
                        var!(__last_att_info_list__))
        end
    end

    defmacro target_killed() do
        quote do
            var!(_tar_list_) != []
                and :lists.all(
                        fn(t) ->
                            tar_stat = :battle.get_battle_status(t, var!(_battle_data_))
                            elem(tar_stat, SkillMacro.BattleStatus.__index__(:is_alive)) == false
                        end,
                        var!(_tar_list_))
        end
    end

    def get_tag_value(tag_list, tag, default) do
        case List.keyfind(tag_list, tag, 0) do
            nil         -> default
            {_, _, val} -> val
        end
    end

    def parse_do_tags(do_clause) do
        case do_clause do
            [do: {:__block__, _, tags}] ->
                tags;
            [do: tag] ->
                [tag]
        end
    end

    defmacro expand_select_minor(major_clause, minor_selector, args) do
        case minor_selector do
            :none ->
                quote do
                    unquote(major_clause)
                end
            :rand ->
                num = hd(args)
                quote do
                    :util.get_rand_list_elems(unquote(major_clause), 
                                              unquote(num))
                end
            :min_rel_hp ->
                num = hd(args)
                quote do
                    :battle.get_n_pos_by(:hp_rel, :min, unquote(num), 
                        unquote(major_clause), var!(_battle_data_))
                end
            :max_rel_hp ->
                num = hd(args)
                quote do
                    :battle.get_n_pos_by(:hp_rel, :max, unquote(num), 
                        unquote(major_clause), var!(_battle_data_))
                end
            :min_hp ->
                num = hd(args)
                quote do
                    :battle.get_n_pos_by(:hp, :min, unquote(num), 
                        unquote(major_clause), var!(_battle_data_))
                end
            :max_hp ->
                num = hd(args)
                quote do
                    :battle.get_n_pos_by(:hp, :max, unquote(num), 
                        unquote(major_clause), var!(_battle_data_))
                end
            :min_mp ->
                num = hd(args)
                quote do
                    :battle.get_n_pos_by(:mp, :min, unquote(num), 
                        unquote(major_clause), var!(_battle_data_))
                end
            :max_mp ->
                num = hd(args)
                quote do
                    :battle.get_n_pos_by(:mp, :max, unquote(num), 
                        unquote(major_clause), var!(_battle_data_))
                end
            :front ->
                num = hd(args)
                quote do
                    :battle.get_first_row(unquote(major_clause), unquote(num), 
                                          var!(_battle_data_))
                end
            :rear ->
                num = hd(args)
                quote do
                    :battle.get_last_row(unquote(major_clause), unquote(num), 
                                          var!(_battle_data_))
                end
            :player_role ->
                num = hd(args)
                quote do
                    :battle.get_player_pos(unquote(major_clause), unquote(num),
                                           var!(_battle_data_))
                end
        end
    end

    defmacro expand_select(major_selector, minor_selector, args) do
        major_clause = case major_selector do
            :all_rival ->
                quote do
                    :battle.get_target_list(:battle.calc_range(var!(_tar_), 4), 
                                            var!(_battle_data_))
                end
            :all_friendly ->
                quote do
                    :battle.get_target_list(:battle.calc_range(var!(_src_), 4), 
                                            var!(_battle_data_))
                end
            :all_friendly_but_self ->
                quote do
                    lc t inlist 
                        SkillMacro.expand_select(
                            :all_friendly,
                            unquote(minor_selector), 
                            unquote(args)), 
                        t != var!(_self_),
                        do: t
                end
            :all_hit ->
                quote do
                    :battle.get_hit_list(var!(__last_att_info_list__), var!(_battle_data_))
                end
            :_self_ ->
                quote do
                    [var!(_src_)]
                end
        end

        quote do
            SkillMacro.expand_select_minor(unquote(major_clause), 
                                           unquote(minor_selector), 
                                           unquote(args))
        end
    end

    defmacro select(selector) do
        case selector do
            {major_selector, line_no, nil} when is_atom(major_selector) ->
                quote do
                    case var!(_tar_) do
                        {:ai_override, override_tar_list} ->
                            var!(_tar_list_) = override_tar_list
                        _ ->
                            var!(_tar_list_) = SkillMacro.expand_select(
                                unquote(major_selector),
                                :none, [])
                    end
                end

            {{:., _, [{major_selector, _, nil}, minor_selector]}, _, args} ->
                quote do
                    case var!(_tar_) do
                        {:ai_override, override_tar_list} ->
                            var!(_tar_list_) = override_tar_list
                        _ ->
                            var!(_tar_list_) = SkillMacro.expand_select(
                                unquote(major_selector),
                                unquote(minor_selector), 
                                unquote(args))
                    end
                end

            _ ->
                throw(:illegal_selector)
        end
    end

    defmacro attack(att_rate) do
        quote do
            attack_spec = SkillMacro.AttackSpec.new(addition: unquote(att_rate),
                                                    targets:  var!(_tar_list_),
                                                    buff_add: var!(__tmp_buff_list__))
            var!(__last_att_info_list__) = 
                :battle.attack(var!(sid), var!(_src_), 
                               setelem(attack_spec, 0, :attack_spec),
                               var!(_tar_list_), 
                               var!(_battle_data_))
            if var!(__last_att_info_list__) != [] do
                var!(_battle_data_) = :battle.handle_attack_info(var!(sid), var!(_src_), 
                                                               var!(__last_att_info_list__), 
                                                               var!(_battle_data_))
                var!(__att_info_list__) = var!(__last_att_info_list__) ++ var!(__att_info_list__)
            end
            var!(__tmp_buff_list__) = []
        end
    end

    def expand_single_assist_effect({tag, _, args}) do
        case tag do
            :heal_rel ->
                # 按百分比
                rate = hd(args)
                quote do
                    {:heal, unquote(rate), :true}
                end
            :heal_abs ->
                # 按绝对值
                points = hd(args)
                quote do
                    {:heal, unquote(points), :false}
                end
            :mana ->
                # 怒气总是按具体点数来的
                num = hd(args)
                quote do
                    {:mana, unquote(num), :false}
                end
            :hp_absorb ->
                # 吸血总是按百分比来的
                rate = hd(args)
                quote do
                    {:hp_absorb, unquote(rate), :true}
                end
        end
    end

    defmacro assist(do_clause) do
        tag_list = parse_do_tags(do_clause)
        tag_list = 
            lc t inlist tag_list do
                expand_single_assist_effect t
            end

        quote do
            assist_spec = SkillMacro.AssistSpec.new(eff: unquote(tag_list))
            assist_spec_list = 
                lc t inlist var!(_tar_list_) do
                    dummy_spec = assist_spec.pos(t)
                    setelem(dummy_spec, 0, :assist_spec)
                end
            var!(__last_att_info_list__) = 
                :battle.assist(var!(sid), var!(_src_), 
                               assist_spec_list, [],
                               var!(_battle_data_))
            if var!(__last_att_info_list__) != [] do
                var!(_battle_data_) = :battle.handle_attack_info(var!(sid), var!(_src_), 
                                                               var!(__last_att_info_list__), 
                                                               var!(_battle_data_))
                var!(__att_info_list__) = var!(__last_att_info_list__) ++ var!(__att_info_list__)
            end
        end
    end

    defmacro heal_rel(heal_rate) do
        quote do
            SkillMacro.assist do
                heal_rel unquote(heal_rate)
            end
        end
    end

    defmacro heal_abs(heal_points) do
        quote do
            SkillMacro.assist do
                heal_abs unquote(heal_points)
            end
        end
    end

    defmacro merge_actions do
        quote do
            _battle_data_ = :battle.merge_actions(var!(_battle_data_))
        end
    end

    def buff_name_to_id_settle(name) do
        case name do
            :pdef_up           -> { 1, :post}
            :pdef_down         -> { 2, :post}
            :mdef_up           -> { 3, :post}
            :mdef_down         -> { 4, :post}
            :att_up            -> { 5, :post}
            :att_down          -> { 6, :post}
            :luck_up           -> { 7, :post}
            :luck_down         -> { 8, :post}
            :speed_up          -> { 9, :post}
            :speed_down        -> {10, :post}
            :crit              -> {12, :post}
            :fatal_up          -> {13, :post}
            :fatal_down        -> {39, :post}
            :hit_up            -> {14, :post}
            :hit_down          -> {15, :post}
            :rebound           -> {16, :post}
            :counter           -> {17, :post}
            :life_drain        -> {18, :post}
            :mana_drain        -> {19, :post}
            :refresh           -> {20, :pre}
            :scorn             -> {21, :post}
            :scorned           -> {22, :post}
            :assist            -> {23, :post}
            :assisted          -> {40, :post}
            :faint             -> {24, :post}
            :frenzy            -> {26, :post}
            :weakness          -> {27, :post}
            :toxic             -> {28, :pre}
            :dodge_up          -> {29, :post}
            :dodge_down        -> {30, :post}
            :block_up          -> {31, :post}
            :block_down        -> {32, :post}
            :crit_up           -> {33, :post}
            :crit_down         -> {34, :post}
            :cast_dmg_up       -> {35, :post}
            :recv_dmg_down     -> {36, :post}
            :cast_dmg_down     -> {37, :post}
            :recv_dmg_up       -> {38, :post}
            :dmg_absorb        -> {41, :post}
            :dmg_absorb_target -> {42, :post}
            :cursed            -> {43, :post}
            :counter_up        -> {44, :post}
            :counter_down      -> {45, :post}
            :att_enhance       -> {46, :post}
            :att_worsen        -> {47, :post}
            :mana_adjust       -> {48, :post}
            :extra_crit_rate   -> {49, :post}
            :dmg_shield        -> {50, :post}
        end
    end

    defmacro buff(name, do_clause) do
        {buff_id, buff_settle} = buff_name_to_id_settle(elem(name, 0))
        tag_list = parse_do_tags(do_clause)
        duration    = hd(get_tag_value(tag_list, :duration,    [1]))
        by_rate     = hd(get_tag_value(tag_list, :by_rate,     [true]))
        value       = hd(get_tag_value(tag_list, :value,       [0]))
        probability = hd(get_tag_value(tag_list, :probability, [1]))
        method      = hd(get_tag_value(tag_list, :method,      [:override]))
        data        = hd(get_tag_value(tag_list, :data,        [0]))

        quote do
            new_buff = SkillMacro.Buff.new(name:       unquote(buff_id),
                                           value:      unquote(value),
                                           data:       unquote(data),
                                           duration:   unquote(duration),
                                           settle:     unquote(buff_settle),
                                           by_rate:    unquote(by_rate),
                                           add_method: unquote(method))
            new_buff = setelem(new_buff, 0, :buff)
            new_buff_spec_list = 
                lc t inlist var!(_tar_list_) do
                    {t, [{new_buff, unquote(probability), :add}]}
                end
            var!(__buff_spec_list__) = new_buff_spec_list ++ var!(__buff_spec_list__)
        end
    end

    defmacro tmp_buff(name, do_clause) do
        {buff_id, buff_settle} = buff_name_to_id_settle(elem(name, 0))
        tag_list = parse_do_tags(do_clause)
        by_rate     = hd(get_tag_value(tag_list, :by_rate,     [true]))
        value       = hd(get_tag_value(tag_list, :value,       [0]))
        probability = hd(get_tag_value(tag_list, :probability, [1]))

        quote do
            buff_rand = :random.uniform()
            if buff_rand <= (unquote(probability)) do
                new_buff = SkillMacro.Buff.new(name:    unquote(buff_id),
                                               value:   unquote(value),
                                               settle:  unquote(buff_settle),
                                               by_rate: unquote(by_rate))
                new_buff = setelem(new_buff, 0, :buff)
                var!(__tmp_buff_list__) = [new_buff | var!(__tmp_buff_list__)]
            end
        end
    end

    defmacro get_attr(pos, field_name) do
        quote do
            idx = SkillMacro.BattleStatus.__index__(unquote(field_name)) + 1
            :battle.get_battle_status(unquote(pos), idx, var!(_battle_data_))
        end
    end

    defmacro get_rel_hp(pos) do
        quote do
            :battle.get_rel_hp(unquote(pos), var!(_battle_data_))
        end
    end

    defmacro current_round do
        quote do
            idx = SkillMacro.BattleData.__index__(:round)
            elem(var!(_battle_data_), idx)
        end
    end

    defmacro set_round_mark do
        quote do
            :erlang.put({:__round_mark__, var!(_src_)}, SkillMacro.current_round)
        end
    end

    defmacro round_mark do
        quote do
            case :erlang.get({:__round_mark__, var!(_src_)}) do
                :undefined -> 0
                rm -> rm
            end
        end
    end

    defmacro friendly_alive_num do
        quote do
            :battle.get_camp_alive_num(var!(_self_), var!(_battle_data_))
        end
    end

    def gen_del_buff_list(clause_list) do
        lc {buff_name, _, _} inlist clause_list do
            {buff_num, _} = buff_name_to_id_settle(buff_name)
            buff_num
        end
    end

    defmacro del_buff(do_clause) do
        [{:do, {:__block__, _, clause_list}}] = do_clause
        buff_num_list = gen_del_buff_list(clause_list)
        quote do
            var!(_battle_data_) = :battle.del_buff_list(unquote(buff_num_list), 
                                                        var!(_tar_list_), var!(_battle_data_))
        end
    end

    defmacro skill(skill_id, do_clause) do
        quote do
            def handle_skill(var!(sid) = unquote(skill_id), var!(_src_), var!(_tar_), 
                             var!(_level_), var!(_param_), var!(_battle_data_)) do
                var!(_self_) = var!(_src_)
                var!(_tar_list_) = []
                var!(__att_info_list__) = []
                var!(__last_att_info_list__) = []
                var!(__buff_spec_list__) = []
                var!(__tmp_buff_list__) = []

                unquote(do_clause)

                # 以下适用于没有攻击只加buff的情况
                if var!(__att_info_list__) == [] do
                    var!(__last_att_info_list__) = var!(__att_info_list__) = 
                        lc t inlist var!(_tar_list_) do
                            new_att_info = SkillMacro.AttackInfo.new(pos:     t,
                                                                     is_crit: false,
                                                                     is_miss: false)
                            setelem(new_att_info, 0, :attack_info)
                        end
                    var!(_battle_data_) = 
                        :battle.handle_attack_info(var!(sid), var!(_src_), 
                                                   var!(__last_att_info_list__), 
                                                   var!(_battle_data_))
                end

                :battle.settle_and_add_buff(var!(_src_), var!(__buff_spec_list__), [], var!(_battle_data_));
            end
        end
    end

    defmacro ai(mon_id, do_clause) do
        quote do
            def handle_ai(var!(_mid_) = unquote(mon_id), 
                          var!(_src_), var!(_battle_data_)) do
                var!(_self_) = var!(_src_)

                unquote(do_clause)
            end
        end
    end
end

