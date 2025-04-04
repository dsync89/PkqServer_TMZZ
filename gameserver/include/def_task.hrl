-define(TASK_STATUS_NOT_ACCEPT,1).				%%未接
-define(TASK_STATUS_WAS_ACCEPT,2).				%%已接
-define(TASK_STATUS_FINISH,3).							%%已完成
-define(TASK_STATUS_COMMIT,4).						%%已提交(日常和成就)


-define(TASK_TYPE_MAIN,1).					%%主线任务
-define(TASK_TYPE_TODAY,2).					%%日常任务
-define(TASK_TYPE_ACH,3).						%%成就

-define(TRIGGER_TASK_ID_LIST,trigger_task_id_list).
-define(TASK_CURR_ACH_TASKID,task_curr_ach_taskid).
-define(TASK_ACH_NEXT_TASKID,task_ach_next_taskid).

-define(dump_task_id_list, dump_task_id_list).

%%关卡类
-define(TASK_TRIGGER_ID_DUNGEON_NORMAL_PASS,100).							%%普通关卡  (num=次数 int_list=[关卡id])																									ok
-define(TASK_TRIGGER_ID_DUNGEON_HARD_PASS,101).							%%困难关卡  (num=次数 int_list=[关卡id])																									ok
-define(TASK_TRIGGER_ID_DUNGEON_FAST_HARD_PASS,102).							%%最困难关卡  (num=次数 int_list=[关卡id])																									ok
-define(TASK_TRIGGER_ID_DUNGEON_PASS,103).							%%任意关卡  (num=次数 int_list=[关卡类型])  1：普通关卡  2：困难关卡  3：最困难关卡																									ok

%%怪物相关
-define(TASK_TRIGGER_ID_KILL_MONSTER,200).					%%击杀怪物 (num=击杀数量 int_list=[怪物id])																									ok
-define(TASK_TRIGGER_ID_GER_GAIN_GER,201).					%%获取武将 (num=数量 int_list=[武将id])																											ok
-define(TASK_TRIGGER_ID_GER_GAIN_STAR_GER,202).				%%获得星级武将 (num=数量 int_list=[星级])
-define(TASK_TRIGGER_ID_GER_UP_QUALITY_TIMES,203).		%%宠物升品次数 (num=次数 int_list=[怪物id])																								
-define(TASK_TRIGGER_ID_GER_UP_QUALITY,204).				%%将X只宠物升品到Y级别(num=数量 int_list=[等级])																						ok
-define(TASK_TRIGGER_ID_GER_UP_LEVEL_TIMES,205).					%%升级X次宠物(num=次数 int_list=[怪物id])			

%%等级相关
-define(TASK_TRIGGER_ID_ROLE_UP_LEVLE,300).				%%角色升级到几级 (num=角色等级 int_list=[])																									ok
-define(TASK_TRIGGER_ID_GER_UP_LEVEL,301).					%%某只宠物升级到几级(num=宠物等级 int_list=[])																							ok




%%装备相关
-define(TASK_TRIGGER_ID_EQUIP_STRONG,400).					%%某个装备强化 (num=强化等级 int_list=[装备id] 如果int_list=[]则任意装备)												ok
-define(TASK_TRIGGER_ID_EQUIP_UP_QUALITY,401).			%%某个装备升品 (num=品质 int_list=[装备id] 如果int_list=[]则任意装备)														ok
-define(TASK_TRIGGER_ID_EQUIP_UP_EQUIP,402).				%%穿上一定数量装备 (num=数量 int_list=[装备id] 如果int_list=[]则任意装备)													ok
%%合成时装备减少	
-define(TASK_TRIGGER_ID_EQUIP_TOTAL_EQUIP,403).		%%总计获得多少件装备 (num=数量 int_list=[装备id] 如果int_list=[]则任意装备)												ok
-define(TASK_TRIGGER_ID_EQUIP_GAIN_STAR_EQUIP,404).			%%获得X件Y星级的装备(num=数量 int_list=[星级])	

-define(TASK_TRIGGER_ID_EQUIP_STRONG_TIMES,405).	%%装备强化次数 (num=次数 int_list=[装备id] 如果int_list=[]则任意装备)														ok
-define(TASK_TRIGGER_ID_EQUIP_UP_QUALITY_TIMES,406).	%%装备升品次数 (num=次数 int_list=[装备id] 如果int_list=[]则任意装备)													ok

-define(TASK_TRIGGER_ID_EQUIP_STRONG_1,407).				%%将X件装备强化到Y级别(num=数量 int_list=[等级])																						ok
-define(TASK_TRIGGER_ID_EQUIP_UP_QUALITY_1,408).			%%将X件装备升阶到Y级别(num=数量 int_list=[等级])																					ok


-define(TASK_TRIGGER_ID_EXPLORE_TIMES,500).					%%探索次数   (num=次数 int_list=[])

-define(TASK_TRIGGER_ID_COMBINE_TIMES,700).					%%合成次数  (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_COMBINE_NORMAL_TIMES,701).					%%配方合成次数  (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_COMBINE_RANDOM_TIMES,702).					%%随机合成次数  (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_COMBINE_GER_RANDOM_TIMES,703).			%%随机合成宠物次数 (num=次数 int_list=[])


-define(TASK_TRIGGER_ID_ADD_REWARD,1000).					%%获得东西     (num=数量 int_list=[类型]) 1: 银两  2: 元宝 3:声望    类型必须有
-define(TASK_TRIGGER_ID_ADD_FRIEND_NUM,1004).			%%添加X个好友  (num=数量 int_list=[]) 


-define(TASK_TRIGGER_ID_EXTRACT_CARD_TIMES,3001).					%%抽卡N次 (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_JOIN_HRON_TIMES,3002).							%%参加N次无尽深渊战斗 (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_JOIN_HULA_TIMES,3003).							%%参加N次玲玲塔战斗 (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_JOIN_NANM_TIMES,3004).						%%参加N次研究所战斗 (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_ADD_ENARGY_TO_FRIEND_TIMES,3005).				%%充能次数 (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_MATING_TO_FRIEND_TIMES,3006).						%%交配次数 (num=次数 int_list=[])

-define(TASK_TRIGGER_ID_ROLE_FIGHT_POWER,2001).						%%战斗力达到多少 (num=战斗力 int_list=[])
-define(TASK_TRIGGER_ID_ROLE_PVP_RANK,2002).							%%竞技场排名达到多少 (num=排名 int_list=[])
-define(TASK_TRIGGER_ID_PASS_HRON_DUNGEON,2003).				%%无尽深渊通过第N关卡 (num=关数 int_list=[])
-define(TASK_TRIGGER_ID_ALLEQUIPMENT,2004).								%%穿戴N件套 (num=件数 int_list=[])
-define(TASK_TRIGGER_ID_UP_GER_NUM,2005).									%%上阵武将个数 (num=个数 int_list=[])
-define(TASK_TRIGGER_ID_REPLACE_UP_GER_TIMES,2006).				%%进行N次换阵 (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_ADD_DIFF_SEX_FRIEND,2007).					%%添加N名异性好友(num=数量 int_list=[]) 
-define(TASK_TRIGGER_ID_CHAT_TIMES,2008).									%%聊天N次(num=数量 int_list=[]) 
-define(TASK_TRIGGER_ID_SIGN_RACE_TIMES,2009).							%%华丽大赛报名次数(num=数量 int_list=[]) 
-define(TASK_TRIGGER_ID_CHANGE_HEAD_TIMES,2010).					%%更换头像次数(num=数量 int_list=[]) 
-define(TASK_TRIGGER_ID_PVP_FIGHT_TIMES,2011).							%%竞技场战斗次数(num=数量 int_list=[]) 
-define(TASK_TRIGGER_ID_BUY_COIN_TIMES,2012).							%%招财次数(num=数量 int_list=[]) 
-define(TASK_TRIGGER_ID_ACTIVE_DESTINY_NUM,2013).					%%激活的天命条数(num=条数 int_list=[]) 