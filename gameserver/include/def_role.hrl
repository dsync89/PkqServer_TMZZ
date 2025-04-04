-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").

%% =================================
%% dictionary key in role server
-define(fillGoldBonus,fillGoldBonus).
-define(roleInfo, roleInfo).
-define(roleID, roleID).
-define(macAddr,macAddr).%% 玩家客户端设备的物理地址
-define(ip, ip).
-define(loginDatetime, loginDatetime).
-define(ger, ger).
-define(gw, gw).
-define(socket,socket).
-define(batProg, batProg).	%% 战役进度
-define(chapter, chapter).	%% 战役章节信息
-define(posList, posList).	%% 站位信息
-define(fighterList, fighterList).%% 出战列表
-define(roleTimes, roleTimes).%% 体力相关
-define(encounterList, encounterList). %% 遭遇站列表
-define(encounterInfo, encounterInfo). %% 遭遇战详细信息
-define(bagEquip, bagEquip).%% 背包中的所有装备
-define(bagItem, bagItem).%% 背包中的非装备
-define(gerEquip, gerEquip).%% 获取武将的装备
-define(gerTypeID, gerTypeID). %% 武将模版ID
-define(shopNumList, shopNumList).%% 商店已购买次数
-define(shopTreasure, shopTreasure).
-define(itemUseList, itemUseList).  %% 道具使用信息列表
-define(posTypeList, posTypeList).%% 出站武将的模版ID列表
-define(roleFightPower, roleFightPower).%% 主公总战斗力
-define(pvp_rank, pvp_rank). % pvp排名
-define(dailyInfo, dailyInfo). % 每日奖励信息
-define(interval_persist_flag, interval_persist_flag).%定时持久化标识
-define(cardInfo, cardInfo). % 点将信息
-define(hronInfo, hronInfo). % 华容道信息
-define(coinBattle, coinBattle). % 金币副本信息
-define(limitInfo,limitInfo).% 限制开启信息
-define(gatherInfo,gatherInfo).% 图鉴信息
-define(mainGerTypeID, mainGerTypeID).% 核心武将的模版ID
-define(gerBag, gerBag). % 武将背包
-define(weiboShareList, weiboShareList).% 可分享微博的事件列表
-define(randomShopList, randomShopList).% 奇遇商店列表
-define(rolePatch, rolePatch). % 宝物碎片列表
-define(guardGerPos,100). % 守护武将的base位置,主要用在数据库中区分上阵武将，非上阵武将与守护武将
-define(guardInfo,guardInfo).
-define(guardPosList,guardPosList).
-define(lieutenantInfo,lieutenantInfo). % 参军的格子配置信息
-define(lieuPosList,lieuPosList). % 副将武将列表
-define(lieu_add_attr,lieu_add_attr). % 副将攻击血量加成信息
-define(posListT,posListT). % 带有副将格子加成信息的主将信息
-define(gag_list,gag_list). %屏蔽发言列表
-define(challengeGodPos,challengeGodPos). %神将录出战武将位置
-define(sign_emperor_info, sign_emperor_info). %拜帝王数据记录
-define(treahouseInfo, treahouseInfo). %汉帝宝库数据信息
-define(fireGold, fireGold). %玩家放鞭炮总共消费的元宝
-define(rebateInfo, rebateInfo). %玩家返利信息
-define(guideVal, guideVal). %玩家新手引导具体步骤的值
-define(teamPkInfo, teamPkInfo). % 3v3 数据
-define(alienInfo, alienInfo).      %异星战场数据
-define(roleRewardInfo, roleRewardInfo).
-define(roleRoad, roleRoad).
-define(monthCard, monthCard).

-define(ROLE_TASK_LIST,role_task_list).										%%玩家任务列表
%% =================================

-define(sendself(Info), role_lib:send_self(Info)).

-define(route, route).

%% 获取#ger.gerBase的子属性的简写
-define(b(Ger, Field), (Ger#ger.gerBase#gerBase.Field)).

%% 获取#ger.gerAttr的子属性的简写
-define(a(Ger, Field), (Ger#ger.gerAttr#gerAttr.Field)).

-define(e(Ger, Field), (Ger#ger.gerExtra#data_ger.Field)).

-define(dict, process_dictionary).

-define(MAXSP, 100).

-define(SKILL_NEED_QUALITY_LIST,[-9999,-9999,2,5,10,15,20]).
-define(UNIQUE2_REPLACE_UNIQUE_NEED_QUALITY,15).

%% 特殊的ActionID
-define(ACTION_MISS, 1).		% 未命中
-define(ACTION_ABSORB, 2).		% 吸血
-define(ACTION_DAMAGE_BACK, 3).	% 反弹
-define(ACTION_DEAD, 4).		% 死亡
-define(ACTION_UNREEL, 5).		% 从眩晕状态中清醒
-define(ACTION_REBORN, 9).		% 复活

%% #p_action.state中的位值
-define(STATE_DEFAULT, 		2#0000000).% 无状态
-define(STATE_CRIT, 		2#1000000).% 暴击
-define(STATE_REEL, 		2#0100000).% 击晕
-define(STATE_DEAD, 		2#0010000).% 死亡
-define(STATE_ABSORB, 		2#0001000).% 吸血
-define(STATE_DAMAGE_BACK, 	2#0000100).% 反弹

-define(KINGDOM_QUN,1).
-define(KINGDOM_SHU, 2).
-define(KINGDOM_WEI, 3).
-define(KINGDOM_WU, 4).

-define(BEAT_ADD_SP, 50).%% 命中时攻击方与被攻击方增加的怒气
-define(HURT_ADD_SP, 25).%% 未命中时攻击方与被攻击方增加的怒气

-define(action(ActionID, Pos, TargetPos, AddHp, AddSp, State), #p_action{actionID=ActionID,
																		 gerPos=Pos,
																		 targetPos=TargetPos,
																		 addHp=AddHp,
																		 addSp=AddSp,
																		 state=State}).
																		 
-define(miss_action(Ger, AddSp), #p_action{actionID=?ACTION_MISS,
									gerPos=?b(Ger,gerPos),
									state=?STATE_DEFAULT,
									addHp=0,
									targetPos=[],
									addSp=AddSp}).


%% 用来测试的攻击者队伍
-define(test_attacker_list, [
#ger{gerID=1,gerBase=#gerBase{gerTypeID=1,gerPos=1},
	  gerAttr=#gerAttr{gerAttack=4000,gerHpMax=20000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=20000,gerSp=40},
#ger{gerID=2,gerBase=#gerBase{gerTypeID=2,gerPos=2},
	  gerAttr=#gerAttr{gerAttack=4000,gerHpMax=20000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=20000,gerSp=40},
#ger{gerID=3,gerBase=#gerBase{gerTypeID=3,gerPos=3},
	  gerAttr=#gerAttr{gerAttack=4000,gerHpMax=20000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=20000,gerSp=40},
#ger{gerID=4,gerBase=#gerBase{gerTypeID=4,gerPos=4},
	  gerAttr=#gerAttr{gerAttack=4000,gerHpMax=20000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=20000,gerSp=40},
#ger{gerID=5,gerBase=#gerBase{gerTypeID=5,gerPos=5},
	  gerAttr=#gerAttr{gerAttack=4000,gerHpMax=20000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=20000,gerSp=40},
#ger{gerID=6,gerBase=#gerBase{gerTypeID=6,gerPos=6},
	  gerAttr=#gerAttr{gerAttack=4000,gerHpMax=20000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=20000,gerSp=40}]).

%% 用来测试的防守者队伍
-define(test_defender_list, [
#ger{gerID=7,gerBase=#gerBase{gerTypeID=7,gerPos=1 },
	  gerAttr=#gerAttr{gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=2000,gerSp=40},
#ger{gerID=8,gerBase=#gerBase{gerTypeID=8,gerPos=2 },
	  gerAttr=#gerAttr{gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=2000,gerSp=40},
#ger{gerID=9,gerBase=#gerBase{gerTypeID=9,gerPos=3 },
	  gerAttr=#gerAttr{gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=2000,gerSp=40},
#ger{gerID=10,gerBase=#gerBase{gerTypeID=10,gerPos=4 },
	  gerAttr=#gerAttr{gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=2000,gerSp=40},
#ger{gerID=11,gerBase=#gerBase{gerTypeID=11,gerPos=5 },
	  gerAttr=#gerAttr{gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=2000,gerSp=40},
#ger{gerID=12,gerBase=#gerBase{gerTypeID=12,gerPos=6 },
	  gerAttr=#gerAttr{gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=2000,gerSp=40}]).							

			
%% ra = role_attribute
-define(ra_level(V), #sc_role_update_level{level=V}).
-define(ra_exp(V),	#sc_role_update_exp{exp=V}).
-define(ra_coin(V),#sc_role_update_coin{coin=V}).
-define(ra_reputation(V),#sc_role_update_reputation{reputation=V}).
-define(ra_gold(V),#sc_role_update_gold{gold=V}).
-define(ra_goldBonus(V),#sc_role_update_goldBonus{goldBonus=V}).
-define(ra_goldUsed(V),#sc_role_update_goldUsed{goldUsed=V}).
-define(ra_vipLevel(V, V1, V2),#sc_role_update_vipLevel{vipLevel=V, challengeGodFree=V1, challengeGodBuy=V2}).
-define(ra_energy(V,V2), #sc_role_update_energy{energy=V,nextEnergyTime=V2}).
-define(ra_dscv(V,V2), #sc_role_update_discoveryTimes{discoveryTimes=V,nextDscvTime=V2}).
-define(ra_alien(V,V2), #sc_alien_update_times{leftTimes=V,timestamp=V2}).
-define(ra_title(V), #sc_role_update_title{title=V}).
-define(ra_pvpTimes(V,V2), #sc_role_update_pvpTimes{pvpTimes=V, nextPvpTime=V2}).
-define(ra_ruleTimes(V,V2), #sc_role_update_ruleTimes{ruleTimes=V, nextRuleTime=V2}).
-define(ra_encounterFreeNum(V), #sc_role_update_encounterFreeNum{encounterFreeNum=V}).
-define(ra_weiboCount(V), #sc_role_update_weiboCount{weiboCount=V}).

-define(notify_update(Record), ?sendself(Record)).

-define(change_pos(Ger, NewPos), (Ger#ger{gerBase=((Ger#ger.gerBase)#gerBase{gerPos=NewPos})})).

-define(MAX_GER_RANK, 20).

-define(GATHER_TYPE_GER,    1).    %% 武将图鉴
-define(GATHER_TYPE_ITEM,   2).    %% 道具图鉴
-define(GATHER_TYPE_EQUIP,  3).    %% 装备图鉴

-define(SHAPE_BASE, 10000000).


-define(BATTLE_DUNGEON_TYPE_NORMAL,1).			%%普通关卡
-define(BATTLE_DUNGEON_TYPE_HARD,2).					%%困难关卡
-define(BATTLE_DUNGEON_TYPE_FAST_HARD,3).		%%最困难关卡


