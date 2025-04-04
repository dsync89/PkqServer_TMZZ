%% key-value结构
-record(kv, {
			 key :: term()				% 键
			 ,value :: term()			% 值
			}).

%% 主公属性结构
-record(role,{
			  %% 基础属性
			  roleID :: ?int32
			  ,accid :: ?int64
			  ,roleName="" :: ?string
			  ,isMale=true	:: boolean()
			  ,description="" :: ?string
			  ,familyID=0 :: ?int32
              ,lastJoinFamily=0 :: ?int32
			  %% 成长属性		
			  ,level=1 :: ?int16
			  ,exp=0 :: ?int64
			  %% 货币管理
			  ,coin=0 :: ?int32
			  ,reputation=0 :: ?int32
			  ,gold=0:: ?int32
			  ,goldBonus=0 :: ?int32
			  ,goldUsed=0 :: ?int32
			  %% 充值vip
			  ,vipLevel=0 :: ?int8
			  ,goldTotalPaid=0 :: ?int32
			  ,title=0 :: ?int8				% 官爵
			  ,fightPower=0 :: ?int64		% 总战斗力
			  ,lastLogoutTime=0				% 上次下线时间 
			  ,head=0 :: ?int32					%选取头像   0：默认头像
              ,payExtReward=0  :: ?int32       % 充值额外奖励
			  ,location="" ::?string
              ,isFailed=false :: boolean()
              ,timeVipDraw=0  :: ?int32
              ,lastPayTime=0  :: ?int32
			  ,deviceID="" :: ?string
              ,srcType=0 :: ?int16
			 }).

%% 守护属性信息
-record(guard_info,{
                    pos=0 :: ?int8
                    ,list=[] :: [#p_ger_guard_attr{}]
                   }).

-record(role_guard,
        {
         count = 0  :: ?int32
         ,infoList = [] :: [#guard_info{}]
         }).

%% 礼包信息
-record(role_reward_info,{
                          onlineSecs = 0,
                          days = 1,
                          lastDays = 0,
                          getList = []
                          }).

%% 玩家公共属性
-record(rolePublic, {
					 roleID
                     ,accid
					 ,isMale
					 ,level
					 ,title
					 ,roleName
					 ,fightPower
					 ,lastLogoutTime
					 ,goldTotalPaid
  					 ,head
  					,location
					,viplevel
  					,srcType
					}).

%% 背包武将结构
-record(gerSimple, {
					gerID	:: ?int32							%   武将唯一ID
					,gerTypeID :: ?int16                        % 模版ID
					,gerQuality:: ?int16                   	    % 品质
					,gerLevel :: ?int8                          % 等级
					,gerExp :: ?int64                           % 经验
					,gerPos=0::?int8							% 在队伍中的站位
				   }).

%% 武将基础属性：实例化属性，根据这些属性，可以完整算出武将的其他属性
-record(gerBase, {
				   gerTypeID :: ?int16                         % 模版ID
				  ,gerQuality:: ?int16                   	   % 品质
				  ,gerLevel :: ?int8                           % 等级
				  ,gerPos :: ?int8                             % 在队伍中的站位
				  ,gerExp :: ?int64                            % 经验
				 }).

%% 武将战斗属性，重算属性时经常修改的
-record(gerAttr, {                             
				   gerAntiWei = 0 :: ?int16                    % 克魏
				  ,gerAntiShu = 0 :: ?int16                    % 克蜀
				  ,gerAntiWu  = 0 :: ?int16                    % 克吴
				  ,gerAntiQun = 0 :: ?int16                    % 克群
				  ,gerAttack :: ?int32                         % 攻击力
				  ,gerHpMax :: ?int32                          % 血量上限
				  ,gerSpInit :: ?int16                         % 怒气初始值
				  ,gerSpMax :: ?int16                         % 怒气最大值
				  ,gerCritic :: ?int16                         % 暴击
				  ,gerCriticReduce :: ?int16                   % 暴击抵抗
				  ,gerDoom :: ?int16                           % 命中
				  ,gerMiss :: ?int16                           % 闪避
				  ,gerAbsorb :: ?int16                         % 吸血
				  ,gerDamageBack :: ?int16                     % 反弹
				  ,gerReel :: ?int16                           % 眩晕
				  ,gerReelReduce :: ?int16                     % 眩晕抵抗
				  ,gerPhyDefBite:: ?int16	         	   	   % 破甲
				  ,gerPhyDef:: ?int16                		   % 护甲
				  ,gerMagDefBite:: ?int16                 	   % 法穿 
				  ,gerMagDef:: ?int16                 		   % 法抗
				  ,gerFightPower :: ?int64					   % 战斗力
				 }).

%% 武将所有属性
-record(ger,{
			 gerID	:: ?int32								% 武将唯一ID
			 ,gerBase :: #gerBase{}							% 武将基础属性
			 ,gerAttr	:: #gerAttr{}						% 武将战斗属性
			 ,gerExtra =0:: any()								% 存战斗时配置属性
			 ,gerHp :: ?int32                           % 当前血量
			 ,gerSp :: ?int32                           % 当前怒气
			}).

%% 武将技能
-record(gerSkill,{
				  normal :: [?int16]						% 普通技能
				 ,unique :: [?int16]						% 无双技能/绝世技能
				 ,wake	 :: [?int16]						% 觉醒技能
				 ,god	 :: [?int16]						% 神将技能
				 ,enter  :: [?int16]						% 登场技能
				 ,god2	 :: [?int16]						% 圣将技能
				 %,other	 :: [?int16]						% 其他技能
				}).

%% 体力相关
-record(roleTimes, {
					energy=0			    :: ?int16			% 体力	
					,challengeGodEnergy=0	:: ?int16			% 神将挑战次数
					,challengeGodBuyTimes=0	:: ?int16			% 神将购买次数
					,lastChallengeGodDate={2013,1,1} ::calendar:date() % 上次领取免费挑战神将录次数的时间
					,refreshLieuTimes=0::?int16					% 副将格子免费刷新次数
					,alreadyPayRefreshLieuTimes=0::?int32		% 使用元宝刷新副将的次数		
					,energyBuyTimes=0	:: ?int16			% 体力已购买次数
					,dscvBuyTimes=0	:: ?int16			% 探索已购买次数
					,meleeSignTimes=0 :: ?int8          % 大混战报名次数
                    ,lastMeleeSignDate=0 :: ?int8           % 上次大混战报名日期
                    ,pvpBuyTimes=0	:: ?int16			% 争霸已购买次数
					,ruleBuyTimes=0	:: ?int16			%秩序战场力已购买次数
					,coinBuyTimes=0::?int16					%银两购买次数
                    ,coinBossHP=0::?int64                   %招财boss血量
					,fireTimes=0::?int32					%放鞭炮次数
					,lastBuyTimesRefreshDate={2013,1,1} :: calendar:date() % 上次刷新各种体力购买次数的日期		
					,lastEnergyTime=0    :: ?int32			% 上次体力恢复时间 
					,discoveryTimes=0	:: ?int8			% 探索次数
					,lastDscvTime=0	    :: ?int32			% 上次探索次数恢复时间
					,dscvCount=0			:: ?int32		% 探索总计数
					,pvpTimes=0	:: ?int16					% pvp当前剩余次数
					,ruleTimes=0 :: ?int16				%   秩序战场剩余次数
					,weiboCount=0 :: ?int8					% 微博分享次数
					,nextWeiboCountRefreshSec=0 :: ?int32 	% 下次刷新微博分享次数的时间
					,lastWeiXinShareSec=0 ::?int32			% 上次微信分享时间
					,lastPvpTime=0		::	?int32			% 上次pvp次数恢复时间
					,lastRuleTime=0	::	?int32			    % 上次秩序战场次数恢复时间
				   }).

%% 体力类道具使用信息
-record(item_use_info, {
                        itemTypeID=0              :: ?int16              %道具模版ID
                        ,useDate={0,0,0}          :: calendar:date()     %最近一次使用日期
                        ,useTimes=0               :: ?int8               %当日累积使用次数
                        }).

%% 遭遇战章节信息
-record(echapter, {
				  id :: ?int16									%% 以玩家ID加章节ID为key
				  ,endTimerRef=0 :: timerRef()|0				%% 章节关闭倒计时
				  ,dungeonList=[] :: [#p_edungeon{}]|?int32 	%% 遭遇战关卡结构
				  ,collectEndTime=-1 :: ?int32					%% 章节征收、探宝倒计时
				  ,isCollected=false	:: boolean() 			%% 是否 已经征收、或者夺宝过				  
				  }).


%% 每日奖励信息
-record(daily, {
				lastTitleRewardDate=0 :: calendar:date()|0		% 上次领取官爵奖励的时间，创角时为0
			   ,lastDrawTitle=0:: ?int8							% 上次领取官爵奖励时的官爵，创角时为0
			   ,lastLoggedLoginDate=0 :: calendar:date()|0		% 上次记录下来的登录日期，创角时为0
			   ,lastDrawLoginRewardDays=0 :: ?int32				%上次领取连续登录奖励的连续登录日数
			   ,loginDays=0 :: ?int32							%当前的连续登录日数
			   ,lastDrawLevelUpLevel=0	::?int16				%上次领取的升级礼包的等级
               ,dailyDrawList                                            %签到日期记录
				}).
  
%% 卡片信息
-record(card, {
			   groupID	% 卡牌所在的group
			   ,type    % 卡牌类型
			   ,value   % 卡牌数值
			  }).
%% 点将信息
-record(cardInfo, {
			   openedCardList=[] :: [#p_opened_card{}] % 已抽取的卡牌
			   ,cardList =[]	:: [#card{}]		   % 等待抽取的列表	
			   ,drawCount=1		:: ?int32			   % 当前的总点击次数
			   }).

%% 武将站位信息
-record(posInfo,{
				 gerID
				 ,gerPos
				 ,gerTypeID
				}).

%% 道馆信息
-record(hronInfo, {
				   date :: calendar:date()					% 数据日期，活动每天开一次，每天的数据对应一个日期,验证使用
				   ,curDungeonNum :: ?int16					% 当前第几关，0=当前未开始挑战
                   ,maxDungeonNum=0 :: ?int16                 % 最高第几关
				   ,attackAdd::?int16						% 当前攻击增加百分比
				   ,hpAdd::?int16							% 当前血量增加百分比 
				   ,dungeonID::[?int16]					    % 随机出来的关卡
				   ,challengeTimes							% 挑战次数
                   ,coinBuyTimes                            % 金币购买次数
                   ,goldBuyTimes                            % 钻石购买次数
				  }).

%% 金币副本
-record(coinBattle,{
                    date :: calendar:date()
                    ,coolDown :: ?int32
                    ,times :: ?int16
                    }).

%% 月卡
-record(monthCard,{
                   endTime  :: ?int32
                   ,drawTime  :: ?int32
                   ,dayPayGold  :: ?int32
                  }).

%% 限制开启信息
-record(limitInfo,{
				   encounterNum=0 :: ?int16					% 奇遇解锁数量
				   ,isBindWeibo=false :: boolean()			%是否绑定了微博	
				   ,inviteRoleID=0 :: ?int32				%邀请你的玩家ID
				   ,inviteRoleName="" :: ?string			%要请你的角色名称
				   ,lastShareLevel=0::?int16				%上次微博分享的主公等级
				   ,spiritGoldBoxCount = 0::?int32				%付费元宝固定次数单抽宝箱计数
				   ,spiritGoldBonusBoxCount=0::?int32				%赠送元宝固定次数单抽宝箱计数
				   ,spiritItemBoxCount = 0:: ?int32				%道具抽取次数
				   ,equipGoldBoxCount = 0::?int32				%付费元宝固定次数单抽宝箱计数
				   ,equipGoldBonusBoxCount=0::?int32				%赠送元宝固定次数单抽宝箱计数
				   ,equipItemBoxCount = 0:: ?int32				%道具抽取次数
				  }).

%% 玩家的武将相关信息
-record(d_ger, {
				roleID		:: ?int32
				,posList :: [#ger{}]			% 站位列表
				,gerList :: [#gerSimple{}]		% 武将列表
			   }).

%% 玩家额外的无交互属性
-record(roleExtra, {
					roleID :: ?int32						% 玩家ID
					,battleProgress	:: ?int32				% 战役进度
					,battleProgressHard ::?int32			% 炼狱战役进度
					,battleProgressFastHard ::?int32	%最难困难副本
					,roleTimes	:: #roleTimes{}		% 体力系统相关
					,encounterList :: [#echapter{}]		% 遭遇战列表
					,shopNumList :: [#p_shop_num{}] % 商品已购买次数
					,dailyInfo :: #daily{} % 每日奖励信息
					,cardInfo :: #cardInfo{} % 点将信息
					,hronInfo :: #hronInfo{} % 华容道信息		
					,limitInfo ::#limitInfo{} % 限制开启信息
					,gatherList::[tuple()] % [武将图片sets，道具图片sets]
					,randomShopList::[#p_shop_random{}] % 奇遇商店列表
                    ,itemUseList::[]    %道具使用信息列表
				   }).

%% 精灵宝库
-record(shop_treasure, {
                        nextRefreshTime = 0,
                        itemList = []
                        }).
					
%% 道具结构
-record(item, {
			   itemUID	:: ?int64		
			   ,itemTypeID	:: ?int16						% 道具模版ID
			   %,itemName 	:: ?string						% 道具名称
			   ,itemType	:: 								% 道具类型
				   		weapon									% 武器
					   |armor									% 盔甲
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
					   |patch_treasure
					   |other
					   |soul_general
					   |debris_weapon
					   |debris_armor
					   |debris_horse
					   |box
                       |exp
			   %,itemStar	:: ?int8						% 道具初始星级
			   ,itemLevel	:: ?int8						% 道具强化等级
			   ,itemRank	:: ?int8						% 道具初始品阶
			   ,itemNum	:: ?int16						% 道具数量
			   ,itemPos :: ?int8						% 装备在身上=1-11.不在身上=0
			   ,itemDecay:: timerRef()|0				% 下次品阶衰减时间
			   ,addAttr	:: #add_attr{}|0					% 加成的属性,只有在身上的装备，此字段才有值，背包中和其他敌方的道具的此字段均为0
			   ,itemExp :: ?int16						% 宝物类型道具的经验
			  }).					

%% 宝物碎片结构
-record(patch, {
				 typeID	:: ?int16					% 道具模版ID
				,num	:: ?int16					% 道具数量
				}).


%% 邮件
-record(mail, {
			   mailUID	::?int64%邮件唯一ID
			   ,mailType::?int8		%邮件类型
			   % 1=> 系统消息                               
			   % 2=> 系统奖励                               
			   % 3=> 加好友申请                              
			   % 4=> 加联盟申请                              
			   % 5=> 私人邮件                               
			   % 6=> 联盟邮件                               
			   ,senderID=0::?int32		%邮件发送者的UID，0=系统发送的
			   ,senderName=""::?string		%发送者名字,如果本邮件是系统邮件，则此字段为空
			   ,content=""::?string			%内容
			   ,time=0::?int32			%发送时间(Unix时间戳)
			   ,mailTemplateID=0::?int16	%邮件模版ID
			   ,paramList=[]::[any()]		%动态类型参数列表
			   ,mailReward=[]	%可领取奖励
			   ,isRead=false :: boolean() % 是否被阅读过
			  })                                                                                                                                                 .

%% 邮件数据库结构
-record(d_mail, {
				 roleID :: ?int32
				 ,mail=[[],[],[]] :: [[#mail{}]]	%% 三种类型的邮件列表
				 ,unreadNum=[0,0,0]	::[?int32]%% 三种类型的邮件的未读数量
				}).

%% 好友数据库结构
-record(d_friend, {
				   roleID :: ?int32
				   ,pal=[] ::[?int32]	% 朋友 	
				   ,foe=[] ::[?int32]	% 仇人
				  }).

%% 邀请码相关的玩家数据
-record(d_invite, {
				   roleID :: ?int32					% 角色ID
				  ,inviteRoleIDList :: [?int32]		%邀请者列表
				  ,rewardNum						%领到的奖励数量
				  }).

%% 推送相关的玩家数据
-record(d_push, {
				 roleID
				 ,token			:: binary()				%推送用的Device Token 参数
				 ,isPVPPushOpen	:: boolean()			%争霸推送是否开启
				 ,isPushNightMute :: boolean()			%晚上是否免打扰
				}).

-record(draw,{
			  drawID
			  ,alreadyDrawTimes
			  ,ableDrawTimes
			 }).

%% 活动结构
-record(act,{
			 actID
			 ,value
			 ,list
			}).
%% 玩家活动相关数据
-record(dc, {
			 roleID
			 ,actList
			}).


%% encounter Chapter List 结构
-record(t_encounter,{attackTimes
  					,chapterID
  					,dungeonID
  					,fightInfo
  				    ,monTotalHp
  					}).

%% 参军的格子配置信息
-record(t_lieu,{pos
				,infoID1
  				,isLock1
  				,infoID2
  				,isLock2
  				,infoID3
  				,isLock3}).

%% 联盟成员信息
-record(family_member_info,{
    role_id=[] :: ?int32
    ,role_name=[] :: ?string
    ,family_id=[] :: ?int32
    ,family_contribution=[] :: ?int32
    ,left_family_contribution=[] :: ?int32
    ,use_gold_time=[] :: ?int32
    ,title=[] :: ?int8
    ,is_male=[] :: boolean()
    ,online=[] :: boolean()
    ,role_level=[] :: ?int16
    ,fight_power=[] :: ?int64
    ,family_title=0 :: ?int8
    ,join_time=0 :: ?int32
                         }).

%% 联盟详细信息
-record(family_info,{
    family_id=[] :: ?int32
    ,family_name=[] :: ?string
    ,level=[] :: ?int16
    ,create_role_id=[] :: ?int32
    ,create_role_name=[] :: ?string
    ,owner_role_id=[] :: ?int32
    ,owner_role_name=[] :: ?string
    ,cur_members=[] :: ?int16
    ,active_points=[] :: ?int32
    ,notice=[] :: ?string
    ,members=[] :: [#family_member_info{}]
    ,rank=[] :: ?int32
    ,create_time=[] :: ?int32
    ,talk_data=[]}).

%% 联盟申请加入信息
-record(family_request,{
    role_id=[] :: ?int32
    ,role_name=[] :: ?string
    ,level=[] :: ?int16
    ,fight_power=[] :: ?int64
    ,timestamp=[] :: ?int32
    ,family_id=[] :: ?int32}).

%% 汉帝宝库数据配置
-record(treaHouseInfo, {
						activityID,
						value_info,
						card_list,
						free_count,
						buy_count,
						free_times,
						mark,
						baseBoxGetList,
						isGetRankReward
						}).

%% 返利信息结构
-record(rebate_info,{id				%% 返利ID
					 ,amount		%% 消费数量[银两，元宝，声望]
					 ,get			%% 是否已经领取奖励
					}).

-record(friend_enargy,{
		roleID,
		toFriendList,				%%赠送好友体力列表
		toMeList,				    %%好友赠送给自己体力列表	
		addFriendList,				%%加好友请求列表记录
        beAddFriendList,            %%被加好友请求列表记录
        fightList,                  %%挑战列表	
		giveTimes,					%%今日领取次数
        fightTimes,                 %%可挑战次数
        refreshFightDate,           %%挑战次数刷新日期
		date						%%日期
}).

-record(alien_info,{times=0,
                    lastRecoverTime=0,
                    resetTime=0}).

-record(role_festival, {freeCount = 0,timestamp = 0,totalCount = 0, activeList = [], dropList = []}).

-record(role_road, {timestamp=0, nowID=0, extID=0, status=0, resetTimes=0}).


