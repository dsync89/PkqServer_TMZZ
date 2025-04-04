%% 各系统对武将的属性的 通用加成格式
-record(add_attr, {						
						gerAttack =0:: ?int32                          % 攻击力
						,gerHpMax =0:: ?int32                          % 血量上限
						,gerSpInit =0:: ?int32                         % 怒气初始值
						,gerSpMax =0:: ?int32                         % 怒气最大值
						,gerCritic =0:: ?int32                         % 暴击
						,gerCriticReduce =0:: ?int32                   % 暴击抵抗
						,gerDoom =0:: ?int32                           % 命中
						,gerMiss =0:: ?int32                           % 闪避
						,gerAbsorb =0:: ?int32                         % 吸血
						,gerDamageBack =0:: ?int32                     % 反弹
						,gerReel =0:: ?int32                           % 眩晕
						,gerReelReduce =0:: ?int32                     % 眩晕抵抗
						,gerPhyDefBite=0:: ?int32	         	   	     % 破甲
						,gerPhyDef=0:: ?int32                		     % 护甲
						,gerMagDefBite=0:: ?int32                 	 % 法穿 
						,gerMagDef=0:: ?int32                 		 % 法抗
						,gerAttackAddtion =0:: ?int16					 % 攻击增加万分比
						,gerHpMaxAddtion =0:: ?int16					 % 血量增加万份比/复活时的血量万分比
						,gerAntiWei = 0 :: ?int32                    % 克魏
						,gerAntiShu = 0 :: ?int32                    % 克蜀
						,gerAntiWu  = 0 :: ?int32                    % 克吴
						,gerAntiQun = 0 :: ?int32                    % 克群									  
				   }).


%% 奖励道具的定义
-record(new_item, {
				   itemTypeID		:: ?int16			%% item的模版ID
				   ,itemNum			:: ?int16			%% 道具数量
				   ,itemLevel		:: ?int8			%% 道具强化等级
				   ,itemRank		:: ?int8			%% 道具品阶
				  }).

%% 奖励一个新的武将
-record(new_ger, {
				  gerTypeID :: ?int16						%  武将的武将模版ID
				  ,gerLevel :: ?int8						%  武将的等级
				  ,gerQuality :: ?int8						%  武将的品阶
				 }).

%% 商店中商品的奖励
-record(sell_reward, {	 
					  coin=0	:: ?int32								% 奖励一定数量银两
					  ,roleExp=0 :: ?int32							% 奖励主公一定数量经验
					  ,gerExp=0 :: ?int32								% 奖励出战的每个武将一定数量经验
					  ,gold=0 :: ?int32								% 奖励一定数量元宝
					  ,item=[] :: [#new_item{}]|#new_item{}|0			% 奖励1种或n种道具实例，无则配置0
					  ,reputation=0 :: ?int32							% 奖励一定声望
					  ,newGer=[]	:: [#new_ger{}]|#new_ger{}|0		% 奖励一个或者多个武将,无则配置0  
					 }).
%升级奖励
-record(data_levelup_reward, {
							rolelevel :: ?int16						%主公升级到的等级，1-N
							,sell_reward :: #sell_reward{}|0		%主公升级到此等级可以领取到的奖励，无则配置0
						   }).

%% 每日奖励
-record(daily_reward, {
					  coin	:: ?int32								% 奖励一定数量银两
					  ,gold :: ?int32								% 奖励一定数量元宝
					  ,item :: [#new_item{}]|#new_item{}|0			% 奖励1种或n种道具实例，无则配置0
					  ,reputation :: ?int32							%  奖励一定声望
					  ,newGer	:: [#new_ger{}]|#new_ger{}|0		% 奖励一个或者多个武将,无则配置0  
					  ,vip	::	?int8								% 奖励vip等级
					 }).

-record(data_title_reward, {
							title :: ?int8							%官爵等级：0-9
							,daily_reward :: #daily_reward{}|0		%每个官爵的每日工资，无则配置0
						   }).

-record(data_login_reward, {
							accDay :: ?int16						%累计连续登录天数，0-N
							,daily_reward :: #daily_reward{}|0		%连续登录的奖励，无则配置0
						   }).


%% 技能配置
-record(data_skill, {
					 skillID	:: ?int16				%% 技能ID
					 ,attackActionID	:: ?int16		%% 攻击者Action
					 ,defendActionID	:: ?int16		%% 受击者Action
					 ,targetSelect	:: 					%% 目标选择方式
						 single
						 |single_back
						 |front_row
						 |back_row
						 |array
						 |random_two
						 |random_three
						 |all 
						 |own_self
						 |own_single
						 |own_front_row
						 |own_back_row
						 |own_all
						 |own_array
						 |own_single_back
						 |own_random_two
						 |own_random_three
					 ,damageType :: physic_hammer|physic_sword|physic_axe|physic_bow|physic_stone|physic_wind|physic_wave|physic_claw|magic_ice|magic_poison|magic_thunder|magic_fire %% 物理：锤、刀、斧、弓、飞石、飓风、音波、爪击。魔法：冰、毒、雷、火
					 ,damageRatio :: ?int16				%% 技能伤害系数
					 ,skillType :: normal|unique|god|enter|unique2|god2%% 普通技能 or 绝技  or 神将技能 or 登场技能 or 绝世技能 or 圣将技能
					 ,gerReel :: ?int16					%% 增加的击晕
					 ,gerPhyDefBite :: ?int16			%% 增加的破防
					 ,gerMagDefBite :: ?int16			%% 增加的法穿
					 ,gerCritic :: ?int16				%% 增加的暴击
					 ,gerAbsorb :: ?int16				%% 增加的吸血
					 ,gerDoom :: ?int16					%% 增加的命中
					 ,gerEnterOrGodAdd :: 0|#add_attr{}		%% 登场技能、无双技能和神将技能的加成属性
					 ,attackTimes :: ?int16					%% 连击次数
					}).


%% 掉落配置
-record(data_drop, {
					dropID		:: ?int16					%  掉落组ID
					,randomList  :: [{?int32,[#new_item{}|#new_ger{}]}] % 掉落随机表
				   }).

%% 战役、遭遇战的奖励的配置结构
-record(reward, {				 
				 coin	:: ?int32								% 奖励一定数量银两
				 ,roleExp :: ?int32								% 奖励主公一定数量经验
				 ,gerExp :: ?int32								% 奖励出战的每个武将一定数量经验
				 ,gold :: ?int32								% 奖励一定数量元宝
				 ,dropList :: [?int16]							% 0-5个掉落组ID
				 ,reputation :: ?int32							% 奖励声望
				}).

%% 战役的章节配置
-record(data_chapter, {
					   chapterID :: ?int8				%% 章节ID
					   ,activeNeedLevel :: ?int16		%% 激活需要的主公等级
					   ,activeNeedChapter :: ?int8 		%% 激活需要的前一个章节ID
					   ,dungeonIDList :: [?int16]|?int16%% 本章节的关卡ID列表，示例：[10010,10020,10030]
					   ,perfectReward :: #reward{}		%% 完美通关奖励
					   ,type :: 0|1|2|3|4|5				%% 0=战役，1=遭遇战,2=奇货商店,3=奇才招募,4=征收,5=探宝
					   ,closeTime	:: -1|non_neg_integer()%% 章节持续时间，为-1时表示不关闭
					   ,canGiveUp	:: boolean()		%% 是否能够放弃
					   ,sufChapterID	:: ?int16		%% 后置章节ID，如果无后置章节，则填0
					   ,shopID :: ?int16				%% 章节对应的奇遇商店ID和奇才商店ID, 如无，则填0
					   ,collectSec	:: ?int32			%% 征收和探宝：需要的秒数
					   ,collectReward :: #sell_reward{}|0 %% 征收或者探宝的奖励
					   ,forceCollectItem :: {?int16,?int16}%% 强行征收、快速探宝需要的道具{道具TypeID，道具数量}，不需要则填0
					   ,forceCollectReputation::?int32	%% 强行征收、快速探宝需要的声望，不需要则填0
					   ,forceCollectGold :: ?int32		%% 强行征收、快速探宝需要的元宝，不需要则填0
					  }).

%% 怪物的配置结构
-record(mon, {
			  gerTypeID :: ?int16						%% 怪物的武将模版ID
			  ,gerLevel :: ?int8						%% 怪物的等级
			  ,gerQuality :: ?int8						%% 怪物的品阶
			 }).
%% 关卡配置
-record(data_dungeon, {
					   dungeonID	:: ?int16			%% 关卡ID
                       ,dungeonName :: ?string          %% 关卡名字
					   ,costEnergy :: ?int16			%% 进入消耗体力
					   ,maxTimes :: ?int16				%% 每日最大挑战次数
					   ,chapterID :: ?int8				%% 所属章节ID
					   ,gerID1 :: #mon{}|0				%% 第一个位置的怪物，无怪物请填 0
					   ,gerID2 :: #mon{}|0				%% 第二个位置
					   ,gerID3 :: #mon{}|0				%% 第三个位置
					   ,gerID4 :: #mon{}|0				%% 第四个位置
					   ,gerID5 :: #mon{}|0				%% 第五个位置
					   ,gerID6 :: #mon{}|0				%% 第六个位置
					   ,reward :: #reward{}				%% 打赢关卡能获得的奖励		
					   ,preDungeonID :: ?int16			%%  前置关卡ID。如果无前置关卡，则配置0.	
					   ,sufDungeonID :: ?int16			%% 后置关卡ID。如果无后置关卡，则配置0.
					   ,addMorale :: ?int32				%%  华容道中增加的士气
					   ,dungeon_level :: ?int16			%% 0=无推荐等级，正整数表示本关卡推荐等级，玩家等级小于推荐等级时，关卡怪物品阶会变大
					   ,pass_num :: ?int8				%% dungeon可以讨伐的合格次数
					  }).

%% 武将模版
-record(data_ger, {     
				    gerTypeID :: ?int16                          % 模版ID      
				    ,gerName :: ?string                          % 名称	
				    ,gerStar:: ?int16                      	     % 星级    
				    ,gerSex :: boolean() 						 % 性别,true=男，false=女        
				    ,gerKingdom :: 1|2|3|4                       % 国家:1=群，2=蜀，3=魏，4=吴
				    %,gerSkillList :: [?int16]					 %技能列表 ，有多少技能，配置多少多少,内容为skillID
				    ,gerSkill1 :: ?int16                         % 普通攻击技能    
				    ,gerSkill2 :: ?int16                         % 怒气攻击技能 
				    ,gerSkill3 :: ?int16					 	 % 登场技能，没有则填0
				    ,gerSkill4 :: ?int16						 % 无双技能，没有则填0
				    ,gerSkill5 :: ?int16						 % 神将技能，没有则填0
				    ,gerSkill6 :: ?int16						 % 无双技能，没有则填0
				    ,gerSkill7 :: ?int16						 % 神将技能，没有则填0
				    ,gerAntiWei = 0 :: ?int32                    % 克魏   
				    ,gerAntiShu = 0 :: ?int32                    % 克蜀     
				    ,gerAntiWu  = 0 :: ?int32                    % 克吴       
				    ,gerAntiQun = 0 :: ?int32                    % 克群               
				    ,gerAttack :: ?int32                         % 攻击力              
				    ,gerHpMax :: ?int32                          % 血量上限                   
				    ,gerSpInit :: ?int32                         % 怒气初始值            
				    ,gerSpMax :: ?int32                         % 怒气最大值      
				    ,gerCritic :: ?int32                         % 暴击               
				    ,gerCriticReduce :: ?int32                   % 暴击抵抗             
				    ,gerDoom :: ?int32                           % 命中
					,gerMiss :: ?int32                           % 闪避
					,gerAbsorb :: ?int32                         % 吸血
					,gerDamageBack :: ?int32                     % 反弹
					,gerReel :: ?int32                           % 眩晕
					,gerReelReduce :: ?int32                     % 眩晕抵抗
					,gerPhyDefBite:: ?int32	         	   	     % 破甲
					,gerPhyDef:: ?int32                		     % 护甲
					,gerMagDefBite:: ?int32                 	 % 法穿
					,gerMagDef:: ?int32                 		 % 法抗
					,price	:: ?int32							 % 出售该武将获得的基础银
					,destinyIDList :: [?int16]					 % 天命ID列表,配置值如:[10010,10020]，无则配置[]
					,breakthroughIDList::[?int16]				 % 突破品阶的ID配置列表,配置值如:[1,2],无法升品阶则配置为[],可以达到的最高品阶由ID配置的需求品阶限制
				  }).


%% 怪物实例化配置
-record(data_mon, {
				   gerTypeID :: ?int16                          % 模版ID 
				   ,gerName :: ?string                          % 名称	
				   ,gerStar:: ?int16                      	    % 星级    
				   ,gerQuality:: ?int16                   	    % 品质   
				   ,gerSex :: boolean() 						% 性别,true=男，false=女        
				   ,gerKingdom :: 1|2|3|4                       % 国家:1=群，2=蜀，3=魏，4=吴
				   ,gerLevel :: ?int8                           % 等级    
				   ,gerSkill1 :: ?int16                         % 普通攻击技能    
				   ,gerSkill2 :: ?int16                         % 怒气攻击技能   
				   ,gerAbsorb :: ?int32                         % 吸血               
				   ,gerDamageBack :: ?int32                     % 反弹     
				   ,gerAntiWei = 0 :: ?int32                    % 克魏   
				   ,gerAntiShu = 0 :: ?int32                    % 克蜀     
				   ,gerAntiWu  = 0 :: ?int32                    % 克吴       
				   ,gerAntiQun = 0 :: ?int32                    % 克群               
				   ,gerAttack :: ?int32                         % 攻击力              
				   ,gerHpMax :: ?int32                          % 血量上限                   
				   ,gerSpInit :: ?int32                         % 怒气初始值            
				   ,gerSpMax :: ?int32                         % 怒气最大值      
				   ,gerCritic :: ?int32                         % 暴击               
				   ,gerCriticReduce :: ?int32                   % 暴击抵抗             
				   ,gerDoom :: ?int32                           % 命中               
				   ,gerMiss :: ?int32                           % 闪避                           
				   ,gerReel :: ?int32                           % 眩晕               
				   ,gerReelReduce :: ?int32                     % 眩晕抵抗             
				   ,gerPhyDefBite:: ?int32	         	   	    % 破甲               
				   ,gerPhyDef:: ?int32                		    % 护甲               
				   ,gerMagDefBite:: ?int32                 	    % 法穿               
				   ,gerMagDef:: ?int32                 		    % 法抗
				  }).

%% vip配置
-record(data_vip, {
				   vipLevel	:: ?int8							% vip等级	
				   ,energyBuyTimes :: ?int16					% 能购买体力的次数
				   ,needPayGold :: ?int32						% 需要的充值元宝数量
				   ,dscvBuyTimes :: ?int16						% 探索次数购买次数
				   ,pvpBuyTimes	:: ?int16						% 争霸次数购买次数
				   ,ruleBuyTimes :: ?int16					    % 秩序战场次数购买次数
				   ,coinBuyTimes :: ?int16						% 银两购买次数
				   ,challengeGodTimes:: ?int16					% 神将挑战购买次数
				   ,challengeGodFreeTimes::?int16				% 神将录挑战免费次数
                   ,explore_Num_Max :: ?int8                    %探索次数上限 
                   ,energy_Num_Max :: ?int8                     %体力值上限
				  ,max_friend_num :: ?int16						%最大好友上线
				  }).


%% 道具配置
-record(data_item, {
					itemTypeID	:: ?int16						% 道具模版ID
					,itemName 	:: ?string						% 道具名称
					,itemType	:: 								% 道具类型
						weapon									% 武器
						|armor									% 盔甲
                        |wing
                        |headwear
                        |totem
                        |runestone
						|horse									% 坐骑
						|treasure_physical_damage_addtion		% 各种宝物
						|treasure_physical_damage_reduction
						|treasure_magic_damage_addtion
						|treasure_magic_damage_reduction
						|treasure_critic
						|treasure_critic_reduce
						|treasure_doom
						|treasure_miss
						|treasure_sp_init
						|treasure_sp_left
						|treasure_absorb
						|treasure_damage_back
						|treasure_reel
						|treasure_reel_reduce
				   		|treasure_tianqian
				   		|treasure_dikun
				   		|treasure_leizhen
				   		|treasure_fengxun
				   		|treasure_shuikan
				   		|treasure_huoli
				   		|treasure_shangeng
				   		|treasure_zedui
						|material
				   		|patch_treasure							% 宝物碎片
						|other
						|soul_general							% 武将魂魄
				   		|debris_weapon							% 武器碎片
				   		|debris_armor							% 护具碎片
                        |debris_wing
                        |debris_headwear
                        |debris_totem
                        |debris_runestone
				   		|debris_horse							% 坐骑碎片
						|box									% 宝箱
                        |add_times                              % 恢复各种体力次数
                        |exp                                    % 加卡牌经验道具
					,itemStack	:: ?int32					    % 道具堆叠数量
					,itemCost	:: ?int16						% 出售的基础价格
					,itemStar	:: ?int8						% 道具初始星级
					,itemMaxRank:: ?int8						% 道具可以提升的品阶上限
					,itemLevel	:: ?int8						% 道具强化等级
					,itemRank	:: ?int8						% 道具初始品阶
					,addAttr	:: #add_attr{}					% 加成的属性
					,itemEffect ::								% 道具功能类型
						0										%   该道具无功能
						|gold									% 兑换赠送元宝,itemEffectArg=元宝数
                        |add_times                              % 恢复体力次数
					,itemEffectArg 								% 道具功能的参数
					,isDecay :: boolean()						% 是否会衰减品阶
				   }). 

%% 固定公式卡牌和道具合成的服务器数据结构   
-record(data_combine, {   
         formulaID              :: ?int16 							    %配方的ID
         ,coin                  :: ?int32 						        %配方合成所需的银两数量
         ,formula_itemID        :: ?int16 								%配方对应的道具ID
         ,combine_out_type      :: ?int8 							    %配方合成目标道具的类型,ger=1,equip=2
         ,combine_ID            :: ?int16 								%配方合成后的武将ID或者itemID
         ,need_item_list        :: [?int16]                             %所需材料列表
         }).

%% 体力恢复类道具信息数据结构
-record(data_item_use, {
         itemTypeID             :: ?int16                               %道具模版ID
         ,addType               ::                                      %恢复体力类型
                                    energy
                                    |dscv
                                    |pvp
                                    |rule
         ,addValue              :: ?int8                                %恢复值
         ,maxTimes              :: ?int8                                %每日最大使用次数
         }).

%% 宝物成长表
-record(data_treasure_value, {
							  itemType	:: 
								  treasure_physical_damage_addtion		% 各种宝物
								  |treasure_physical_damage_reduction
								  |treasure_magic_damage_addtion
								  |treasure_magic_damage_reduction
								  |treasure_critic
								  |treasure_critic_reduce
								  |treasure_doom
								  |treasure_miss
								  |treasure_sp_init
								  |treasure_sp_left
								  |treasure_absorb
								  |treasure_damage_back
								  |treasure_reel
								  |treasure_reel_reduce
							  ,itemRank	:: ?int8						%  宝物品阶
							  ,value :: ?int16							% 该宝物的属性值
							 }).


%% 商店中售卖的商品的配置
-record(data_sell, {
					sellID :: ?int32								%商品ID
					,costType ::									%消耗类型
						gold										% 元宝
						|coin										% 银两
						|reputation									% 声望
				        |score										% 积分
					,costNum :: ?int32								%消耗数量
					,sellReward :: #sell_reward{}					%购买所获得的东西
					,maxBuyNum :: ?int32							%最大购买数量，为-1时，表示不限制
					,needVipLevel :: ?int16							%购买所需要的vip等级, 为-1时表示不限制vip等级
				   }).

%% 奇遇奇才商店的随机列表
-record(data_sell_random, {
						   roleMaxLevel :: ?int16					% 玩家主公等级小于等于roleMaxLevel时读取本配置
						   ,randomList :: [{?int32,?int16}]			%随机列表,[{权重，sellID}],例如[{100, 11001},{1000, 11002}]
						  }).

%% 商店配置
-record(data_shop, {
					shopID :: ?int16								% 商店ID
					,shopType :: 1 | 2								% 商店类型，1=普通商店，2=奇遇商店
					,sellNum										%奇遇商店随机出来的商品个数
					,refreshSec :: ?int32							%奇遇刷新间隔秒数 
					,sellList :: [?int16 | #data_sell_random{}]		%普通商店=[sellID1,sellID2..]  奇遇商店=[{data_sell_random,1,[]},{data_sell_random,2,[]}]
				   }).

%% 天命配置
-record(data_destiny, {
					   destinyID :: ?int16							% 天命ID	
					   ,destinyName	:: ?string						% 天命名称：用来拼广播		
					   ,destinyType :: 1|2							% 1=队伍武将，2=装备道具
					   ,destinyNeedList :: [?int16]					% 激活需要的ID列表，配置如：[10010,10020],无则配置[]
					   ,addAttr :: #add_attr{}						% 增加的属性
					  }).

%% 宝物碎片合成配置
-record(data_compose, {
					   composeID	:: ?int8				
					   ,needItemList :: [{?int16,?int16}]			% 需要的各道具模版ID和数量，例如[{10010,1},{10012,2}]
					   ,product		:: #new_item{}					% 产出的新道具
					   ,composeSec	:: ?int32						%  合成需要的时间，单位：秒
					  }).

%% 公告
-record(notice, {
				 noticeID :: ?int32									%  公告ID		
				 ,title :: ?string									% 标题
				 ,content :: ?string								% 内容
				}).

%% 登录演示战斗
-record(data_demo_fight, {
						  type ::
							  1										% 登录的演示战斗
							  |2										%未定义
						  ,gerID1 :: #mon{}|0				%% 第一个位置的怪物，无怪物请填 0
						  ,gerID2 :: #mon{}|0				%% 第二个位置
						  ,gerID3 :: #mon{}|0				%% 第三个位置
						  ,gerID4 :: #mon{}|0				%% 第四个位置
						  ,gerID5 :: #mon{}|0				%% 第五个位置
						  ,gerID6 :: #mon{}|0				%% 第六个位置
						 }).

%% 充值商品配置
-record(data_pay, {
				   payID	::?int32                                          %充值商品ID
				   ,payGold	::?int32                                          %充值可以获得的元宝
				   ,payBonus ::?int32                                          %充值赠送的元宝
				   ,payPriceApp ::?int32                                         %appStore的价格
				   ,payPrice91 :: ?int32											%91平台的货币价格
				  }).

%% 碎片合成配置
-record(data_compound,{
					   itemTypeID :: ?int16								% 碎片的道具模版ID
					   ,needNum :: ?int16								% 需要的碎片的全部数量
					   ,baseNeedNum :: ?int16							% 需要的该类型碎片的最少数量
					   ,otherList :: any()								% 需要的其他类型的列表
									%[10001,20001],缺少的碎片将按先后顺序从列表中扣除
									%需要60个30001,有40个3001,baseNum为30,其余20个按顺序扣除10001和20001共20个
					   ,product :: #new_item{}|#new_ger{}				% 合成产物
					   }).

%% 奇遇的常规奖励配置
-record(data_explore_reward,{
							 level			:: ?int16					% 主公等级	
							 ,roleExp		:: ?int32                   % 赠送的主公经验
							 ,gerExp		:: ?int32                   % 赠送的武将经验
							 ,coin			:: ?int32                   % 赠送的银两
							}).
% 相对时间的格式
%% -record(t_time,{
%% 				 a	::?int16										% 参数1
%% 				,b	::?int16										% 参数2
%% 				,c  ::?int16										% 参数3
%% 				}).
%% -record(a_time,{													%{2,{14,20,10}} => 开服后第二天的14:20:10秒开启活动
%% 				 a_day	:: ?int16									%相对开服的天数
%% 				,time	:: #t_time{}								%当天的时间
%% 				}).
%% 活动可领取项的配置
-record(data_activity_draw, {
							 drawID			:: ?int16				%可领取项ID
							 ,description	:: ?string				%可领取项描述
							 ,maxDrawNum	:: ?int16				%最大领取次数 
							 ,condition		:: any()				%领取条件参数
									% 兑换材料的条件配置格式:[材料项1,材料项2,材料项3]
									% 材料项配置格式:
									%　　　元宝: 		{1, 100}.
									%　　　银两: 		{2, 100}.
									%　　　声望: 		{3, 100}.
									%　　　主公经验: 	{4, 100}.
									%　　　武将经验: 	{5, 100}.
									%　　　道具: 		{6, 10010, 1}.
									%　　　武将: 		{7, 10010, 1}.
							 ,reward		:: #sell_reward{}		%奖励具体内容
							}).
%% 活动配置
-record(data_activity, {
						activityID		:: ?int16
						,type			:: 
							pay_special_num							% 定额充值奖励活动
							|pay_acc_num							% 累计充值奖励活动
							|pay_day_num							% 充值天数奖励活动
							|exchange								% 兑换活动
							|exchange2								% 兑换活动
                            |exchange3                              % 免费领取
							|consume								% 消费活动
						,isForever		:: true|false				% 是否是永久活动
						,isDailyRefresh	:: true|false				% 是否是每日自动刷新活动
						,startTime		:: any()%calendar:datetime()		% 活动开始时间，如   { {2013,3,9}, {8,0,0} }|{0,{1,2,3}}
						,stopTime		:: any()%calendar:datatime()		% 活动结束时间，如   { {2013,3,10}, {8,0,0} }|{1,{2,3,4}}
						,level 			:: any()					% 等级限制,如 {level,1,10}
						,vip			:: any()					% vip限制,如 {vip,1,12}
						,activityName	:: ?string					% 活动名称
						,iconSrc		:: ?string					% 图标资源
						,description	:: ?string					% 活动描述
						,drawList		:: [#data_activity_draw{}]	% 可领取项
					   }).
%% 排行榜类活动的活动显示配置
-record(data_activity_rank,{activityID	:: ?int16
							,type		::
								treaHouse
						   ,startTime	::any()						%% 活动开启时间,时间格式同data_activity
						   ,stopTime	::any()						%% 活动结束时间
						   ,endTime		::any()						%% 活动关闭时间
						   ,level		::any()						%% 等级限制 
						   ,vip			::any()						%% vip限制
						   ,activityName::?string					%% 活动名称
						   ,iconSrc		::?string					%% 图标资源
						   ,description	::?string					%% 活动描述
 							}).

%% 商店开宝箱价格配置
-record(data_box_price2, {
						 itemTypeID		::?int16					% 宝箱的道具模版ID
						,oncePrice		::?int32					% 开一次宝箱的元宝消耗
						,tenTimesPrice ::?int32						% 开10次宝箱的元宝消耗
						,isOpenActivity=0	::?int8				%是否开启打折活动
						,discount=0				::?int32				%打折信息(1到100之间)
						,endtime=0				::?int32				%活动结束时间戳
						 }).

-record(data_box_price,{
						 id			:: ?int16						%% 宝箱活动id
						,type		:: ?int16						%% 时间配置类型，1 =>日期格式,2 =>相对开服时间格式，开服时间在data_common中配置
						,start_time	:: any()%calendar:datetime()	%% 活动开启时间
						,stop_time	:: any()%calendar:datetime()	%% 活动结束时间
						,itemTypeID	:: ?int16						%% 宝箱道具模版id
						,oncePriceDis	:: ?int16						%% 活动时开一次宝箱的元宝消耗
						,tentimesPriceDis	::?int32					%% 活动时开十次宝箱的元宝消耗
                        ,oncePrice   :: ?int16                       %% 开一次宝箱的元宝消耗
                        ,tentimesPrice   ::?int32                    %% 开十次宝箱的元宝消耗
						}).

%% 皇权激励的buff配置
-record(data_king_buff, {
						 buffLevel :: ?int16			% buff等级
						,addAttr :: #add_attr{}			% 每个buff等级对应的增加总属性
						 }).


%% 帐号服务器用的礼品码奖励配置
-record(data_gift_reward, { 
					  coin=0	:: ?int32								% 奖励一定数量银两
					  ,roleExp=0 :: ?int32							% 奖励主公一定数量经验
					  ,gerExp=0 :: ?int32								% 奖励出战的每个武将一定数量经验
					  ,gold=0 :: ?int32								% 奖励一定数量元宝
					  ,item=[] :: [#new_item{}]|#new_item{}|0			% 奖励1种或n种道具实例，无则配置0
					  ,reputation=0 :: ?int32							% 奖励一定声望
					  ,newGer=[]	:: [#new_ger{}]|#new_ger{}|0		% 奖励一个或者多个武将,无则配置0  
					 }).

%% 武将突破的配置
-record(data_ger_breakthrough, {
						id		:: ?int16								% 突破配置的id号
						,descption	::?string							% 突破描述
						,rank_condition	::?int8							% 突破需要的拼接
						,condition	::any() 								% 突破需要的其他材料
									% 材料的条件配置格式:[材料项1,材料项2,材料项3],配置为[]则表示无法继续提升品阶
									% 材料项配置格式:
									%　　　银两: 		{coin, 100}.
									%　　　道具: 		{item, 10010, 1}.
					 }).

-record(mail_template, {
					 content	::?string								% 富文本内容
					,reward	::[#sell_reward{}]
					}).

-record(data_temp_mail, {
					id	::?int16										% 邮件模版的编号
					,mailInfoList	::[#mail_template{}]				% 邮件模版的内容
					}).

-record(data_auto_mail, {
                    id  ::?int32                                        % id编号唯一，从1开始递增
                    ,dateTime :: calendar:datetime()                    % 发送时间,如       {{2014,1,19}, {23,59,59}}
                    ,srcTypeList :: [?int8]                             % 渠道ID列表，给指定渠道玩家发，条件不受等级和vip影响
                    ,levelRange ::any()                                 % 等级范围:大于等于min，小于max,如{minLevel, maxLevel}，不限等级填0
                    ,vipLevelRange ::any()                              % vip等级范围:大于等于min，小于max,如{minVipLevel, maxVipLevel}，不限vip等级填0
                    ,mailInfoList   ::[#mail_template{}]                % 邮件模版的内容
                    }).

-record(item_random_weight,{
					 itemID ::?int32									% itemTypeId
					,random_weight_list ::any()							% 概率-数量表[{weight1, Num1},{weight2, Num2}...]
}).

-record(data_card_split, {
					 id ::?int16										% 分解编号
					,coin ::?int32										%分解可获得银两数量
					,itemList ::[#item_random_weight{}]					% 分解可获得的物品

}).

-record(data_lieu_open_charge,{
					 pos		::?int8									% 想要解锁的副将位置
					,needLevel	::?int8									% 解锁该位置需要的等级
					,cost		::any()									% 解锁该位置需要消耗的物品,配置格式同data_activity
					,initList	::any()									% 解锁格子的初始化信息
}).


%% 返利项目配置
-record(data_rebate_draw, {
						rebateID		:: ?int16,		% ID
						name			:: ?string,		% 名称
						startTime		:: any(),		% 开始时间{A,B,C,D}
						closeTime		:: any(),		% 结束时间
						rewardTime		:: any(),		% 领奖时间
						ratioList		:: any()		% 返利比例[银两比例,元宝比例,声望比例]
						}).

%% 返利活动配置
-record(data_activity_rebate, {
						activityID		:: ?int16
						,type			:: 
							rebate							% 返利活动
						,startTime		:: any()%calendar:datetime()		% 活动开始时间，如   {{2014,1,19}, {23,59,59}}
						,closeTime		:: any()%calendar:datatime()		% 活动结束时间，如   {17,23,59,59}
						,level 			:: any()					% 等级限制,如 {level,1,10}
						,vip			:: any()					% vip限制,如 {vip,1,12}
						,activityName	:: ?string					% 活动名称
						,iconSrc		:: ?string					% 图标资源
						,description	:: ?string					% 活动描述
						,drawList		:: [#data_rebate_draw{}]	% 返利项目
						}).

%% 爆竹活动配置
-record(data_activity_fire, {
						activityID		:: ?int16
						,type			:: 
							fire							% 爆竹活动
						,startTime		:: any()%calendar:datetime()		% 活动开始时间，如   {{2014,1,19}, {23,59,59}}
						,stopTime		:: any()%calendar:datatime()		% 活动结束时间，如   {17,23,59,59}
						,closeTime		:: any()%calendar:datatime()		% 活动关闭时间，如   {17,23,59,59}
						,level 			:: any()					% 等级限制,如 {level,1,10}
						,vip			:: any()					% vip限制,如 {vip,1,12}
						,activityName	:: ?string					% 活动名称
						,iconSrc		:: ?string					% 图标资源
						,description	:: ?string					% 活动描述
						,discounts		:: any()					% 全区燃放次数及对应折扣额度,如 [{5000, 9},]
						,hiddenIncrease :: any()					% 隐藏增长次数，{开始时间，结束时间，目标次数} 如 [{{0,8,0,0}, {0,12,0,0}, 3204},]
						,rankLimit		:: ?int16					% 上榜资格:单个玩家燃放次数
						,rankMax		:: ?int16					% 规定最多上榜人数:显示到排行榜的人数
						,gold			:: ?int16					% 每次元宝消耗数
						,coin			:: ?int32					% 保底银币奖励
						,randomReward	:: ?int32					% 随机道具奖励box_id
						,hiddenReward	:: any()					% 燃放次数对应道具奖励box_id
						,rankRewards	:: any()					% 排行榜奖励
						}).

%%任务配置数据结构
-record(data_task,{
				task_id				   
				,task_type									%%类型 1：主线 2:日常
				,auto_accept								%%是否自动接受
				,level_limit 									%%等级限制(只用做接受任务时用)
				,pre_task_id 								%%前置任务  如果为0表示没有，现在这个字段暂时没有用
				,trigger_task_id_list						%%后置任务  []：没有
				,trigger_id									%%触发类型
  				,trigger_num								%%触发num
  				,trigger_int_list							%%触发参数
				,max_step 									%%最大步骤  可配置对话任务  任务状态(1未接 2已接  3已完成)大于max_step的时候任务会完成
				,step_reward								%%达到某个状态奖励[{状态,[]}].
				,reward										%%奖励(和def_reward.hrl中配置的一样)
				,type
			  	,sort
}).

%% 精灵宝库随机数据格式
-record(data_shop_treasure,
        {
         type,              %%类型 1：物品 2：卡牌
         typeID,            %%模板ID
         minNum,            %%最小数量
         maxNum,            %%最大数量
         costType,          %%单价消耗类型 1：金币 2：钻石 3：声望
         costValue          %%单价消耗数量
         }).

%% 节日活动配置
-record(data_festival,
        {
         id =0 ::?int32                                         % 有多条活动配置时，保证这个id不重复 ，且不为0
         ,startTime = {{1970,1,1},{8,0,0}}      :: any()%calendar:datetime()        % 活动开始时间，如   { {2013,3,9}, {8,0,0} }|{0,{1,2,3}}
         ,stopTime =  {{1970,1,1},{8,0,0}}      :: any()%calendar:datatime()        % 活动结束时间，如   { {2013,3,10}, {8,0,0} }|{1,{2,3,4}}
         ,title = ""   :: ?string                                 % 活动名称
         ,content = ""  :: ?string                                % 活动描述
         ,freeTimes = 0 :: ?int32                                % 每日免费次数
         ,onePrice = 0 ::?int32                                  % 单次价格
         ,tenPrice = 0 ::?int32                                  % 十次价格
         ,coinBoxID = 0 ::?int32                                 % 金币宝箱ID
         ,itemBoxID = 0 ::?int32                                 % 道具宝箱ID
         ,ruleBoxIDList = [] :: any()                             %道具宝箱潜规则列表,无潜规则为[], 格式[{特定次数1,特定boxID}, {特定次数2,特定boxID}, {特定次数3,特定boxID}...]
         ,accBoxIDList = [] ::any()                               %累计次数宝箱列表，格式[{累计次数1,特定boxID}, {累计次数2,特定boxID}, {累计次数3,特定boxID}...]
         ,rankRewardList = [] ::any()                             %格式[{起始排名,结束排名,该区间奖励 sell_reward}...]                           
        }).


