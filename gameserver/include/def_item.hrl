%%道具类型定义
-define(weapon,weapon).
-define(armor,armor).
-define(wing,wing).                 %%翅膀
-define(headwear,headwear). %%头饰
-define(totem,totem).               %%图腾
-define(runestone,runestone).   %%符石
-define(horse,horse).
-define(treasure_physical_damage_addtion			,		treasure_physical_damage_addtion).
-define(treasure_physical_damage_reduction			,treasure_physical_damage_reduction).
-define(treasure_magic_damage_addtion				,treasure_magic_damage_addtion).
-define(treasure_magic_damage_reduction				,treasure_magic_damage_reduction).
-define(treasure_critic							,treasure_critic).
-define(treasure_critic_reduce						,treasure_critic_reduce).
-define(treasure_doom							,treasure_doom).
-define(treasure_miss							,treasure_miss).
-define(treasure_sp_init						,treasure_sp_init).
-define(treasure_tianqian						,treasure_tianqian).
-define(treasure_dikun							,treasure_dikun).
-define(treasure_leizhen						,treasure_leizhen).
-define(treasure_fengxun						,treasure_fengxun).
-define(treasure_shuikan						,treasure_shuikan).
-define(treasure_huoli							,treasure_huoli).
-define(treasure_shangeng						,treasure_shangeng).
-define(treasure_zedui							,treasure_zedui).
-define(treasure_sp_left							,treasure_sp_left).
-define(treasure_absorb							,treasure_absorb).
-define(treasure_damage_back						,treasure_damage_back).
-define(treasure_reel							,treasure_reel).
-define(treasure_reel_reduce						,treasure_reel_reduce).
-define(material								,material).
-define(patch_treasure								,patch_treasure).
-define(other									,other).
-define(soul_general	                        ,soul_general	).
-define(debris_weapon	                        ,debris_weapon	).
-define(debris_armor	                        ,debris_armor	).
-define(debris_wing                             ,debris_wing).%%翅膀
-define(debris_headwear                         ,debris_headwear).%%头饰
-define(debris_totem                            ,debris_totem).%%图腾
-define(debris_runestone                        ,debris_runestone).%%符石
-define(debris_horse	                        ,debris_horse	).
-define(box										,box).
-define(exp                                     ,exp).
-define(formula, formula).
-define(add_times, add_times).

%% 装备类型列表
-define(equip_type_list, [?weapon, ?headwear, ?armor, ?wing, ?runestone, ?totem]).

%% 装备位置定义
-define(ITEM_POS_WEAPON, 1).
-define(ITEM_POS_HEADWEAR,2).
-define(ITEM_POS_ARMOR, 3).
-define(ITEM_POS_WING,4).
-define(ITEM_POS_RUNESTONE,5).
-define(ITEM_POS_TOTEM,6).

-define(MAX_RANK_OF_MAIN_EQUIP, 19).

-define(MAX_RANK_OF_TREASURE, 9).