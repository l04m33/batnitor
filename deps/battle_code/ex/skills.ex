#
# 技能配置脚本
#
# skill中支持的操作：
#
# -----------------------------------------------------------------
#   select - 选择攻击目标
#       格式：select <选择符1>[.<选择符2>(<参数>)]
#       “选择符1”的可能取值：
#           all_rival               - 所有敌方角色
#           all_friendly            - 所有已方角色
#           all_friendly_but_self   - 除了自己外的所有已方角色
#           all_hit                 - 所有上次攻击中命中的角色
#           _self_                  - 自己
#       “选择符2"的可能取值：
#           rand                    - 随机角色
#           min_rel_hp              - 气血最少的角色（按百分比算）
#           max_rel_hp              - 气血最多的角色（按百分比算）
#           min_hp                  - 气血最少的角色（按绝对值算）
#           max_hp                  - 气血最多的角色（按绝对值算）
#           min_mp                  - 怒气最少的角色（按绝对值算）
#           max_mp                  - 怒气最多的角色（按绝对值算）
#           front                   - 最前排的角色
#           rear                    - 最后排的角色
#       “参数”目前只用来指定人数，例如：
#           select all_rival.min_hp(2)
#       会在所有敌方角色中选择气血绝对值最少的2个角色作为目标
#       “选择符2”和“参数”如果省略掉的话会选择某一方的所有角色
#
# -----------------------------------------------------------------
#   attack - 攻击已选的目标
#       格式：attack <伤害系数>
#       “伤害系数”可以是任意非负数字
#
# -----------------------------------------------------------------
#   assist - 向已选的目标添加辅助效果（加血、加怒气等）
#       格式：
#           assist do
#               <操作> <操作参数>
#           end
#       “操作”的可能取值：
#           heal_rel                - 治疗目标（按总血量百分比恢复）
#           heal_abs                - 治疗目标（按绝对值恢复）
#           mana                    - 给目标加怒气
#           hp_absorb               - 吸目标的血
#       “操作参数”是任意数字，根据操作不同含义不同：
#           heal_rel                - 参数是回复气血的百分比（1.0 = 100%），按目标总血量算
#           heal_abs                - 参数是回复气血的绝对值
#           mana                    - 参数是增加的怒气绝对值
#           hp_absorb               - 参数是吸血的百分比（1.0 = 100%），按目标总血量算
#
# -----------------------------------------------------------------
#   buff - 向已选的目标添加buff
#       格式：
#           buff <Buff名称> do
#               duration    <持续回合数>
#               by_rate     <是否按百分比计算>
#               value       <Buff的数值>
#               [probability <几率>    ]
#               [method      <叠加方式>]
#               [data        <其他数据>]
#           end
#       “几率”控制Buff生效的几率，可以省略，默认是1.0（100%）
#       “叠加方式”控制已经有相同类型Buff的情况下，新增的Buff如何处理，可能的取值有：
#           :override               - 新Buff把旧Buff覆盖掉
#           :overlay                - 新Buff的数值和旧Buff的数值叠加
#           :noop                   - 旧Buff维持原样，忽略新Buff
#       “其他数据”只在程序内部实现里用到，一般不用指定
#
#       可用的Buff名称：
#           pdef_up             物防+
#           pdef_down           物防-
#           mdef_up             魔防+
#           mdef_down           魔防-
#           att_up              攻击+
#           att_down            攻击-
#           luck_up             幸运+
#           luck_down           幸运-
#           speed_up            速度+
#           speed_down          速度-
#           crit                必定暴击
#           fatal_up            致命+
#           fatal_down          致命-
#           hit_up              命中+
#           hit_down            命中-
#           rebound             必定反弹伤害
#           counter             必定反击
#           life_drain          吸血
#           mana_drain          吸怒气
#           refresh             每回合自动回血
#           scorn               嘲讽
#           scorned             被嘲讽
#           assist              援护
#           assisted            被援护
#           faint               晕
#           frenzy              防御转攻击
#           weakness            降低被治疗时回血量
#           toxic               中毒
#           dodge_up            闪避+
#           dodge_down          闪避-
#           block_up            格挡+
#           block_down          格挡-
#           crit_up             暴击+
#           crit_down           暴击-
#           cast_dmg_up         输出伤害+
#           recv_dmg_down       输入伤害-
#           cast_dmg_down       输出伤害-
#           recv_dmg_up         输入伤害+
#           dmg_absorb          伤害吸收
#           dmg_absorb_target   伤害被吸收
#           cursed              被诅咒
#           counter_up          反击+
#           counter_down        反击-
#           att_enhance         越战越勇
#           att_worsen          越战越挫（内部用）
#           extra_crit_rate     额外增加暴击几率
#           dmg_shield          吸收伤害的护盾
#
# -----------------------------------------------------------------
#   tmp_buff - 临时Buff，用来临时更改攻击时攻击者的属性，例如“临时提高自己30%的暴击进行攻击”
#       格式：
#           tmp_buff <Buff名称> do
#               by_rate     <是否按百分比计算>
#               value       <Buff的数值>
#               [probability <几率>]
#           end
#       所有参数和buff命令的参数完全一样
#
# -----------------------------------------------------------------
#   if - 条件判断
#       格式：
#           if <条件> do
#               <其他命令1>
#           [else
#               <其他命令2>]
#           end
#       当“条件”返回true的时候，“其他命令1”会被执行；返回false的时候，则“其他命令2”会执行
#       “条件”除了通常的“>”、“<”、“==”、“!=”等表达式外，还可以用以下条件：
#           hit             - 上次攻击中有命中任意目标
#           missed          - 上次攻击中没有命中任何目标
#           is_critical     - 上次攻击中有出现暴击
#           target_killed   - 上次攻击把所有目标都杀死了
#
# -----------------------------------------------------------------
#   p - 读取技能参数
#       格式：p(<参数位置>)
#       例如p(1)是第一个参数，p(2)是第二个参数，以此类推
#


defmodule Skills do
    import SkillMacro, only: [skill: 2, select: 1, attack: 1, assist: 1, 
                              heal_rel: 1, heal_abs: 1, buff: 2, tmp_buff: 2, 
                              p: 1, missed: 0, hit: 0, is_critical: 0,
                              target_killed: 0, get_attr: 2, del_buff: 1,
                              merge_actions: 0]

    # ------------------ 虎卫技能 ------------------

    # 威震四方
    skill 104 do
        select all_rival.front(1)
        attack p(1)

        if hit do
            select all_friendly
            assist do
                mana p(2)
            end
            merge_actions
        end
    end

    # 坚若磐石
    skill 105 do
        select _self_
        shield = round(get_attr(_self_, :hp_max) * p(1))

        buff dmg_shield do
            duration p(2)
            value    shield
            by_rate  false
        end
    end

    # 背水一战
    skill 106 do
        select all_rival.front(1)
        attack p(1)

        if hit do
            select all_friendly
            buff recv_dmg_down do
                duration p(3)
                value    p(2)
                by_rate  true
                method :overlay
            end
        end
    end

    # 浴血狂击
    skill 107 do
        select all_rival
        attack p(1)
    end

    # 战意激荡
    skill 108 do
        select all_rival.min_hp(1)
        attack p(1)
    end

    # ------------------ 猛将技能 ------------------

    # 霸刃连斩
    skill 109 do
        select all_rival.rand(1)

        tmp_buff extra_crit_rate do
            value p(3)
            by_rate false
        end

        attack p(1)

        if hit do
            select all_hit
            buff faint do
                duration p(2)
            end
        end
    end

    # 横扫千军
    skill 110 do
        select all_rival

        tmp_buff extra_crit_rate do
            value p(3)
            by_rate false
        end
        
        attack p(1)

        select all_hit.rand(p(2))
        del_buff do
            pdef_up
            mdef_up
            att_up
            luck_up
            speed_up
            fatal_up
            hit_up
            rebound
            counter
            refresh
            dodge_up
            block_up
            crit_up
            cast_dmg_up
            recv_dmg_down
            dmg_absorb
            extra_crit_rate
            dmg_shield
        end
    end

    # 龙牙突刺
    skill 111 do
        select all_rival.rand(1)
        
        tmp_buff extra_crit_rate do
            value p(2)
            by_rate false
        end

        attack p(1)
    end

    # 乘胜追击
    skill 112 do
        select all_rival.rand(1)

        tmp_buff crit do
        end

        attack p(1)

        select all_rival.rand(1)

        tmp_buff crit do
        end

        attack p(1)
    end

    # 破军之势
    skill 113 do
        select all_rival.rear(1)

        attack p(1)

        select all_hit

        buff pdef_down do
            value p(2)
            duration 3
            method :overlay
            by_rate false
        end

        buff mdef_down do
            value p(2)
            duration 3
            method :overlay
            by_rate false
        end
    end

    # ------------------ 军师技能 ------------------

 # 龙战八方 对敌方前排造成x伤害
    skill 114 do
        select all_rival.rand(2)
        attack p(1)
    end

    # 奇门遁甲    对敌方随机单体造成x伤害,并清除其所有增益状态，优先攻击玩家
    skill 115 do
        select all_rival.player_role(1)
        if _tar_list_ == [] do
            select all_rival.rand(1)
        end
        attack p(1)

        select all_hit
        del_buff do
            pdef_up
            mdef_up
            att_up
            luck_up
            speed_up
            fatal_up
            hit_up
            rebound
            counter
            refresh
            dodge_up
            block_up
            crit_up
            cast_dmg_up
            recv_dmg_down
            dmg_absorb
            extra_crit_rate
            dmg_shield
        end
    end


    # 龙落雷 对敌方随机单体造成x伤害
    skill 116 do
        select all_rival.rand(1)
        attack p(1)
    end


    # 虚空炽炎 对敌方全体造成x伤害
    skill 117 do
        select all_rival.rand(9)
        attack p(1)
    end


    # 繁星流火 给己方血量百分比最低者恢复法攻的x气血
    skill 118 do
        select all_friendly.min_rel_hp(1)
        m_att = get_attr(_self_, :m_att)
        heal_points = m_att*p(1)
        heal_abs heal_points
    end

    # ------------------ 其他技能 ------------------

    skill 119 do
        select all_rival.min_hp(1)
        attack p(1)

        select all_friendly
        buff att_up do
            by_rate  true
            value    p(2)
            duration 1
        end
    end

    skill 223 do
        select all_rival.rand(1)
        attack p(1)

        select all_hit
        buff scorned do
            duration 1
            value    _self_
        end

        select _self_
        buff scorn do
            duration 1
            value    p(2)
            by_rate  true
        end
    end

    skill 224 do
        select all_rival.rand(2)
        attack p(1)

        select all_hit
        buff scorned do
            duration 1
            value    _self_
        end

        select _self_
        buff scorn do
            duration 1
            value    p(2)
            by_rate  true
        end
    end

    skill 225 do
        select all_friendly_but_self
        buff assisted do
            duration 2
            value    p(1)
            by_rate  true
            data     _self_
        end

        select _self_
        buff assist do
            duration 2
            value    p(1)
            by_rate  true
        end
    end

    skill 401 do
        select all_rival.rand(1)
        attack p(1)
    end

    skill 402 do
        select _self_
        buff frenzy do
            duration 2
            by_rate  true
            value    p(1)
        end
    end

    skill 403 do
        select all_rival.rand(1)
        attack p(1)

        if hit do
            select _self_
            buff crit_up do
                duration p(3)
                value    p(2)
                by_rate  true
            end
        end
    end

    skill 404 do
        select all_rival.rand(1)
        attack p(1)

        select all_hit
        buff weakness do
            duration 2
            value    p(2)
            by_rate  true
        end
    end

    skill 405 do
        select all_rival.rand(2)
        attack p(1)

        select all_hit
        buff scorned do
            duration 1
            value    _self_
        end

        select _self_
        buff scorn do
            duration 1
            value    p(2)
            by_rate  true
        end
    end

    skill 412 do
        select all_rival.rand(1)
        tmp_buff crit do
            by_rate false
        end
        tmp_buff mana_adjust do
            by_rate false
            value   {p(2), 0}
        end
        attack p(1)
    end

    skill 413 do
        select all_rival.rand(3)
        tmp_buff mana_drain do
            by_rate     false
            value       {0, p(3)}
            probability p(2)
        end
        attack p(1)
    end

    skill 414 do
        select all_rival.rand(1)
        attack p(1)

        select all_hit
        buff faint do
            duration p(2)
        end
    end

        # ------------------ 新增技能 ------------------

    # 对敌前排单人造成150%物理伤害,并吸收造成伤害50%的气血
    skill 415 do
        select all_rival.front(1)
        tmp_buff life_drain do
            by_rate true
            value   p(2)
        end
        attack p(1)
    end

    # 对敌气血最少单人造成120%物理伤害,并增加己方全体30%伤害1回合。
    skill 416 do
        select all_rival.min_hp(1)
        attack p(1)

        select all_friendly
        buff cast_dmg_up do
            duration p(2)
            value    p(3)
            by_rate  true
        end
    end



    # 对敌前排单人造成140%物理伤害,使己方全体下回合受到伤害减少39%
    skill 417 do
        select all_rival.front(1)
        attack p(1)

        select all_friendly
        buff recv_dmg_down do
            duration (1)
            value    p(2)
            by_rate  true
        end
    end

    # 对敌方怒气最高单人造成140%的法术伤害,并减少目标50点怒气
    skill 418 do
        select all_rival.max_mp(1)
        tmp_buff mana_drain do
            by_rate false
            value   {0, p(2)}
        end
        attack p(1)
    end

    #为我方气血百分比最少三人增加法功30%气血,并有50%增加其100怒气
    skill 419 do
        select all_friendly.min_rel_hp(3)
        m_att = get_attr(_self_, :m_att)
        heal_points = round(m_att*p(1))
        heal_abs heal_points

        mana_rand = :random.uniform()
        if mana_rand <= p(2) do             
            assist do
                mana p(3)
            end
            merge_actions
        end
    end

    # 对敌怒气最高单人造成125%物理伤害,有100%几率使其眩晕1回合
    skill 420 do
        select all_rival.max_mp(1)
        attack p(1)

        select all_hit

        faint_rand = :random.uniform()
        if faint_rand <= p(3) do
            buff faint do
                duration p(2)
            end
        end
    end

    #对敌前排三人造成125%物理伤害,嘲讽敌前排2人攻击自己,并且自身受到伤害降低55%(嘲讽状态下不增加怒气）
    skill 421 do
        select all_rival.rand(3)
        attack p(1)

        select all_rival.front(2)
        buff scorned do
            duration 1
            value    _self_
        end

        # XXX: 嘲讽这个技能有点特别，需要在攻击者身上放一个“嘲讽”buff，value是减少伤害的比率；
        #      在被攻击者身上放一个“被嘲讽”buff，value是_self_，也就是嘲讽者的位置；
        #      攻击者不需要另外加减伤害的buff
        select _self_
        buff scorn do
            duration 1
            value    p(2)
            by_rate  true
        end
    end


    # 对敌血量最少三人造成120%法术伤害,必暴击
    skill 422 do
        select all_rival.min_hp(3)
        tmp_buff crit do
            value 0
        end
        attack p(1)
    end

    #对敌随机单人造成90%的物理伤害,偷取目标70%怒气值,优先攻击主角
    skill 423 do
        select all_rival.player_role(1)         # XXX: 这一行只是去选择玩家角色，如果选不到就不会去选武将了；
        if _tar_list_ == [] do                   #      如果要选不到主角就选武将，应该像左边这样写～
            select all_rival.rand(1)
        end

        tmp_buff mana_drain do
            by_rate true
            value   {p(2), p(2)}            # XXX: 大括号里，前面一个值是给攻击者自己加的怒气，后一个值是给被攻击者减的怒气，两个都要写上才行（即使值是一样的）
        end
        attack p(1)
    end


    #--------------------原来一批默认调用主角的怪物技能，会报错，重写一遍-------------------

    # 为我方<font color='#ffff00'>随机三人</font>增加<font color='#ffff00'>25%</font>气血,并增加其<font color='#ffff00'>20点</font>怒气值
    skill 242 do
        select all_friendly.min_rel_hp(3)

        m_att = get_attr(_self_, :m_att)
        heal_points = m_att * p(1) + 300

        assist do
            heal_abs heal_points
            mana     p(2)
        end
    end

    # 为我方<font color='#ffff00'>气血百分比最少单人</font>增加<font color='#ffff00'>35%</font>气血
    skill 244 do
        select   all_friendly.min_rel_hp(1)

        m_att = get_attr(_self_, :m_att)
        heal_points = m_att * p(1) + 300

        heal_abs heal_points
    end

    # 为我方<font color='#ffff00'>气血百分比最少三人</font>增加<font color='#ffff00'>25%</font>气血，
    # 并<font color='#ffff00'>30%</font>几率增加<font color='#ffff00'>50%</font>物理和法术伤害
    skill 246 do
        select   all_friendly.min_rel_hp(3)

        m_att = get_attr(_self_, :m_att)
        heal_points = m_att * p(1) + 300

        heal_abs heal_points

        buff att_up do
            value       p(3)
            duration    2
            by_rate     true
            probability p(2)    # 这是加buff的几率
        end
    end

    #对敌<font color='#ffff00'>随机单人</font>造成<font color='#ffff00'>125%</font>的物理伤害
    skill 249 do
        select all_rival.front(1)
        attack p(1)
    end

    #将自己<font color='#ffff00'>10%</font>防御力转换成攻击,持续<font color='#ffff00'>2回合</font>
    skill 250 do
        select _self_

        buff frenzy do
            duration 2
            value p(1)
            by_rate true
        end
    end

    #连续三次攻击敌<font color='#ffff00'>随机单人</font>,造成<font color='#ffff00'>80%、60%和28%</font>的物理伤害
    skill 251 do
        select all_rival.front(1)
        attack p(1)
        attack p(2)
        attack p(3)
    end

    #对敌<font color='#ffff00'>随机三人</font>造成<font color='#ffff00'>68%</font>的物理伤害
    skill 252 do
        select all_rival.front(3)
        attack p(1)
    end

    #对敌<font color='#ffff00'>随机单人</font>造成<font color='#ffff00'>100%</font>物理伤害,提高<font color='#ffff00'>
    #35%</font>的暴击,持续<font color='#ffff00'>3回合</font>
    skill 253 do
        select all_rival.front(1)

        attack p(1)

        select _self_
        buff crit_up do
            duration p(3)
            value p(2)
            by_rate false
        end
    end

    #对敌<font color='#ffff00'>气血最少单人</font>造成<font color='#ffff00'>100%</font>的物理伤害；如果杀死目标,则对剩
    #余单位中<font color='#ffff00'>气血最少单人</font>进行一次追击,造成<font color='#ffff00'>75%</font>的物理伤害
    skill 254 do
        select all_rival.min_hp(1)
        attack p(1)

        if target_killed do
            select all_rival.min_hp(1)
            attack p(2)
        end
    end

    #对敌<font color='#ffff00'>全体</font>造成<font color='#ffff00'>120%</font>的伤害
    skill 256 do
        select all_rival
        attack p(1)
    end

    #对敌<font color='#ffff00'>随机单人</font>造成<font color='#ffff00'>100%</font>的法术伤害,增加
    #<font color='#ffff00'>50点</font>致命,持续<font color='#ffff00'>2回合</font>
    skill 257 do
        select all_rival.rand(1)
        attack p(1)

        if hit do
            select _self_
            buff fatal_up do
                duration p(3)
                value p(2)
                by_rate false
                method :override
            end
        end
    end

    #对敌<font color='#ffff00'>随机单人</font>造成<font color='#ffff00'>110%</font>的法术伤害,
    #使目标回血效果降低<font color='#ffff00'>22%</font>,持续<font color='#ffff00'>2回合</font>
    skill 258 do
        select all_rival.rand(1)
        attack p(1)

        if hit do
            select all_hit
            buff weakness do
                duration 2
                value p(2)
            end
        end
    end

    #<font color='#ffff00'>50%</font>几率对敌<font color='#ffff00'>随机单人</font>施加状态,使
    #其眩晕<font color='#ffff00'>2回合</font>
    skill 261 do
        suc_rate = :random.uniform()
        if suc_rate >= p(1) do
            select all_rival.rand(1)
            attack 0

            select all_hit
            buff faint do
                duration p(2)
                method :override
            end
        end
    end

    #减少自己<font color='#ffff00'>20%</font>的气血,对敌<font color='#ffff00'>随机单人</font>造
    #成<font color='#ffff00'>150%</font>的物理伤害
    skill 276 do
        select all_rival.front(1)
        attack p(1)
    end

    # ------------------- 这是找不到技能时的默认处理，不能在这后面加代码 -------------------
    def handle_skill(_skill_id, src, tar, _level, _param, battle_data) do
        attack_spec = SkillMacro.AttackSpec.new(addition: 1, targets: [tar])
        attack_spec = setelem(attack_spec, 0, :attack_spec)
        :battle.attack(11, src, attack_spec, battle_data)
    end

end

