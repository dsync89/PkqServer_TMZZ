-define(PRELOAD_CONFIG, [ 
                         %% 服务器通用配置
                         {"config/data_common.config",data_common, key_value, original},
                         
                         %% 运维配置
                         {"setting/setting.config", data_setting, key_value, original},
                         
                         %% 玩家经验配置
                         {"config/data_role_exp.config",data_role_exp, if_clause, original},
                         fun tk_config:load_data_role_level/0,
                         
                         %% 武将经验配置
                         {"config/data_ger_exp.config",data_ger_exp, if_clause, original},
                         fun tk_config:load_data_ger_level/0,
                         
                         %% 武将突破配置
                         {"config/data_ger_breakthrough.config", data_ger_breakthrough, key_value, original},
                         
                         %% 招财配置
                         {"config/data_coin_buy.config", data_coin_buy, key_value, original},
                         
                         %% 武将技能配置
                         {"config/data_skill.config", data_skill, key_value, original},
                         
                         %% 武将配置
                         {"config/data_ger.config",data_ger, key_value, original},
                         
                         %% 守护武将配置
                         {"config/data_guard.config",data_guard, key_value, original},
                         
                         %% 武将天命配置
                         {"config/data_destiny.config",data_destiny,key_value,original},
                         fun tk_config:load_data_destiny_rela/0,
                         
                         %% 体力类道具使用信息配置
                         {"config/data_item_use.config",data_item_use, key_value, original},
                         
                         %% 战役和探索功能的章节配置
                         {"config/data_chapter.config",data_chapter, key_value, original},
                         
                         %% 战役基础配置
                         {"config/data_battle_setting.config",data_battle_setting, key_value, original},
                         
                         %% 遭遇战和战役的关卡配置
                         {"config/data_dungeon.config",data_dungeon, key_value, fun role_battle:format_config/1},
                         
                         %% 怪物配置（未使用）
                         {"config/data_mon.config",data_mon, key_value, fun ger_lib:data_mon_list2ger_list/1},
                         
                         %% 武将可出战数量配置
                         {"config/data_ger_num.config",data_ger_num, if_clause, original},
                         
                         %% VIP特权配置
                         {"config/data_vip.config",data_vip, key_value, original},
                         
                         %% 联盟配置
                         {"config/data_family.config",data_family, key_value, original},
                         
                         %% 道具配置
                         {"config/data_item.config",data_item,key_value, original},
                         
                         %% 合成配置
                         {"config/data_combine.config",data_combine,key_value, original},
                         {"config/data_combine_crt.config",data_combine_crt,key_value, original},
                         {"config/data_combine_random.config",data_combine_random,key_value, original},
                         
                         %% 宝物的属性成长配置 
                         {"config/data_treasure_value.config",data_treasure_value,key_value,fun item_lib:data_treasure_value_list2key_value_list/1},
                         
                         %% 宝物经验配置
                         {"config/data_treasure_exp.config",data_treasure_exp, if_clause, original},
                         fun tk_config:load_data_treasure_rank/0,
                         {"config/data_treasure_exp2.config",data_treasure_exp2, if_clause, original},
                         fun tk_config:load_data_treasure_rank2/0,
                         {"config/data_trea_equip_rule.config",data_trea_equip_rule, key_value, original},
                         %% 探索的随机配置
                         {"config/data_encounter.config",data_encounter,if_clause, original},
                         {"config/data_fixed_encounter.config",data_fixed_encounter,key_value, fun role_explore:fixed_encounter_config_va/1},
                         
                         %% 商店配置
                         {"config/data_shop.config",data_shop,key_value,original},
                         {"config/data_sell.config",data_sell,key_value,original},
                         %% 奇遇、奇才商店配置
                         {"config/data_shop_etc.config",data_shop_etc,key_value,original},
                         %% 精灵宝库
                         {"config/data_shop_treasure.config",data_shop_treasure,key_value,original},
                         %% 官爵配置
                         {"config/data_title.config",data_title,key_value,original},
                         
                         %% 争霸排行榜配置
                         {"config/data_pvp.config",data_pvp,if_clause,original},
                         
                         %% 关卡掉落配置
                         {"config/data_drop.config",data_drop,key_value,original},
                         
                         %% 宝物合成配置
                         {"config/data_compose.config",data_compose,key_value,original},
                         fun tk_config:load_data_patch/0,
                         
                         %% 随机名称配置（data_name.config)
                         fun tk_config:load_data_name/0,
                         
                         %% 官爵奖励配置
                         {"config/data_title_reward.config",data_title_reward,key_value,original},
                         
                         %% 连续登录奖励配置
                         {"config/data_login_reward.config",data_login_reward,key_value,fun role_daily:transform_list/1},
                         
                         %% 装备强化配置
                         {"config/data_reinforce.config",data_reinforce,key_value,original},
                         
                         %% 服务器公告配置
                         {"config/data_notice.config", data_notice, key_value, fun role_message:data_notice_transform_list/1},
                         
                         %% 点将配置
                         {"config/data_card.config", data_card, key_value, original},
                         {"config/data_fixed_card.config", data_fixed_card, key_value, original},
                         
                         %% 武将升品配置
                         {"config/data_ger_up_rank.config", data_ger_up_rank, key_value, original},
                         
                         %% 道具品阶衰减配置
                         {"config/data_item_decay.config", data_item_decay, key_value, original},
                         
                         %% 道具升品配置
                         {"config/data_item_up_rank.config", data_item_up_rank, key_value, original},
                         
                         %% 虎牢关配置
                         {"config/data_hula.config", data_hula, key_value, fun hula_server:data_hula_transform_list/1},
                         
                         %% 战南蛮配置
                         {"config/data_nanm.config", data_nanm, key_value, fun nanm_server:data_nanm_transform_list/1},
                         
                         %% 演示战斗（创角第一场战斗）
                         {"config/data_demo_fight.config", data_demo_fight, key_value, fun role_role:data_demo_fight_transform_list/1},
                         
                         %% 华容道配置
                         {"config/data_hron.config", data_hron, key_value, original},
                         
                         %% 充值商店配置
                         {"config/data_pay.config", data_pay, key_value, original},
                         
                         %% 首充奖励配置
                         {"config/data_pay_reward.config", data_pay_reward, key_value, original},
                         
                         %% 道具、武魂碎片合成
                         {"config/data_compound.config", data_compound, key_value, original},
                         
                         %% 微博绑定、微博分享、邀请码绑定、邀请者首充奖励
                         {"config/data_invite.config", data_invite, key_value, original}, 
                         
                         %% 探索常规奖励界面
                         {"config/data_explore_reward.config", data_explore_reward, key_value, original},
                         
                         %% 升级礼包配置
                         {"config/data_levelup_reward.config", data_levelup_reward, key_value, original},
                         
                         %% 活动配置
                         {"config/data_activity.config", data_activity, key_value, original},
                         
                         %% 宝箱配置
                         {"config/data_box.config", data_box, key_value, fun role_box:config_format_va/1},
                         {"config/data_box_price.config", data_box_price, key_value, fun activity_server:load_box_price/1},
                         %% 付费元宝固定次数单抽宝箱
                         {"config/data_fixed_gold_box.config", data_fixed_gold_box, key_value, original},
                         %% 赠送元宝固定次数单抽宝箱
                         {"config/data_fixed_goldbonus_box.config", data_fixed_goldbonus_box, key_value, original},
                         
                         
                         %% 皇权配置
                         {"config/data_king.config",data_king, key_value, original},
                         
                         %% 皇权buff配置
                         {"config/data_king_buff.config",data_king_buff, key_value, original},
                         
                         %% 无模版邮件配置						
                         {"config/data_temp_mail.config",data_temp_mail, key_value, original},
                         
                         %% 自动发送邮件配置                     
                         {"config/data_auto_mail.config",data_auto_mail, key_value, original},
                         
                         %% 卡牌分解规则配置
                         {"config/data_card_split.config",data_card_split, key_value, original},
                         
                         %% 聊天配置
                         {"config/data_talk.config",data_talk, key_value, original},
                         
                         %% 关键词配置
                         {"config/data_words.config",data_words, key_value, original},
                         
                         %% 体力恢复活动配置					
                         {"config/data_get_energy.config",data_get_energy, key_value, original},
                         
                         %% 副将配置
                         {"config/data_lieu_add.config",data_lieu_add,if_clause, original},
                         {"config/data_lieu_clo_setting.config",data_lieu_clo_setting,key_value, original},
                         {"config/data_lieu_open_charge.config",data_lieu_open_charge,if_clause, original},
                         {"config/data_fixed_lieu_refresh.config",data_fixed_lieu_refresh,key_value, original},
                         
                         %% 扫荡配置
                         {"config/data_dungeon_raids.config",data_dungeon_raids,key_value, original},
                         
                         %% 帝王争霸战配置
                         {"config/data_emperor_bet.config",data_emperor_bet, if_clause, original},
                         {"config/data_emperor.config", data_emperor, key_value, original},
                         
                         %% 关卡配置信息
                         {"config/data_battle_setting.config", data_battle_setting, key_value, original},
                         
                         %% 跨服战配置信息
                         {"config/data_cross.config", data_cross, key_value, original},
                         
                         %% 华丽大赛配置信息
                         {"config/data_race.config", data_race, key_value, original},
                         
                         %% 异星战场配置信息
                         {"config/data_alien.config", data_alien, key_value, original},
                         
                         %% 秩序战场配置信息
                         {"config/data_rule.config", data_rule, key_value, original},
                         
                         %% 大乱斗配置信息
                         {"config/data_melee.config", data_melee, key_value, original},
                         
                         %% 训练师之路配置信息
                         {"config/data_road.config", data_road, key_value, original},
                         
                         %% 金币副本配置信息
                         {"config/data_coin_battle.config", data_coin_battle, key_value, original},
                         
                         %% 节日活动配置信息
                         {"config/data_festival.config", data_festival, key_value, original},
                         
                         %% 3v3配置信息
                         {"config/data_team_pk.config", data_team_pk, key_value, original},
                         
                         %% 放鞭炮配置信息
                         {"config/data_fire.config", data_fire, key_value, original},
                         
                         %% 汉帝宝库配置信息
                         {"config/data_fixed_treasure_free.config", data_fixed_treasure_free, key_value, original},
                         {"config/data_fixed_treasure_gold.config", data_fixed_treasure_gold, key_value, original},
                         {"config/data_treasure_box.config", data_treasure_box, key_value, original},
                         {"config/data_treasure_box_setting.config", data_treasure_box_setting, key_value, original},
                         {"config/data_treasure_box_baseReward.config", data_treasure_box_baseReward, if_clause, original},
                         
                         %% 排行榜类活动配置
                         {"config/data_activityRank.config", data_activityRank, key_value, original},
                         
                         %% 消费返利配置信息
                         {"config/data_rebate.config", data_rebate, key_value, original},
                         
                         %%套装配置信息
                         {"config/data_all_equipment.config",data_all_equipment,key_value, original},
                         %%召唤精灵和祈祷装备配置
                         {"config/data_fixed_box_withitem.config",data_fixed_box_withitem,key_value,original},
                         %% 冲级活动配置
                         {"config/data_levelRank.config", data_levelRank, key_value, original},
                         %%任务
                         {"config/data_task.config",data_task,key_value,original},
                         {"config/data_homestead.config",data_homestead,key_value,original},
                         {"config/data_push.config",data_push,key_value,original},
                         
                         %% 在线礼包
                         {"config/data_online_reward.config", data_online_reward, key_value, original},
                         
                         %% 等级礼包
                         {"config/data_level_reward.config", data_level_reward, key_value, original},
                         
                         %% 连续登陆礼包
                         {"config/data_days_reward.config", data_days_reward, key_value, original},
                         
                         %% vip每日礼包
                         {"config/data_days_vip.config", data_days_vip, key_value, original},
                         
                         %% 棱镜渠道代码对应渠道ID,ProductCode，ProductSecret配置
                         {"config/data_sdk_id.config", data_sdk_id, key_value, original},

                         %% 其他配置数据
                         {"config/data_etc.config", data_etc, key_value, original}
                        ]).
