%% @author admin
%% @doc 华容道 玩家处理模块
%% Created 2013-5-9


-module(role_hron).
-compile(export_all).
-include("def_role.hrl").
-include("def_battle.hrl").

-define(SUCC_REWARD_GER, 1).
-define(SUCC_REWARD_ITEM, 2).
-define(SUCC_REWARD_GOLD, 3).
-define(SUCC_REWARD_SILVER, 4).
-define(SUCC_REWARD_REPU, 5).

-define(WIN, 1).
-define(FAIL, 2).

-define(BUY_TYPE_COIN, 1).
-define(BUY_TYPE_GOLD, 2).

%% API functions
-export([]).

%% Internal functions 
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
is_open() ->
    Time = erlang:time(),
    StartTime = data_hron:get(start_time),
    StopTime = data_hron:get(stop_time),
    Time >= StartTime andalso Time =< StopTime.

random_dungeon(DungeonNum) ->
    case util:foldl(fun({{Low, High}, List}, Acc) ->
                            case DungeonNum >= Low andalso DungeonNum =< High of
                                true ->
                                    {return, List};
                                false ->
                                    Acc
                            end
                    end, [], data_hron:get(dungeon)) of
        [] ->
            0;
        RandomList ->
            util:random_one_from_list(RandomList)
    end.


cs_hron_info(#cs_hron_info{}) ->
    #hronInfo{dungeonID=DungeonID, challengeTimes=ChallengeTimes,curDungeonNum=CurDungeonNum,maxDungeonNum=MaxDungeonNum,
              attackAdd=AttackAdd,hpAdd=HpAdd,coinBuyTimes=CoinBuyTimes,goldBuyTimes=GoldBuyTimes} = role_data:get_hronInfo(),
    {Date, Time} = erlang:localtime(),
    StartTime = data_hron:get(start_time),
    StopTime = data_hron:get(stop_time),
    IsOpen = Time >= StartTime andalso Time =< StopTime,
    NextTimestamp =
        case IsOpen of
            true ->
                util:datetime_to_seconds({Date, StopTime}) + 1;
            false ->
                case Time < StartTime of
                    true ->
                        util:datetime_to_seconds({Date, StartTime});
                    false ->
                        util:datetime_to_seconds({Date, StartTime}) + ?ONE_DAY_SECONDS
                end
        end,
    Record = #sc_hron_info{attackAdd=AttackAdd,
                           curDungeonNum=CurDungeonNum,
                           maxDungeonNum=MaxDungeonNum,
                           dungeonID=DungeonID,
                           hpAdd=HpAdd,
                           challengeTimes=ChallengeTimes,
                           isOpen = IsOpen,
                           nextTime = NextTimestamp,
                           coinBuyTimes=CoinBuyTimes,
                           goldBuyTimes=GoldBuyTimes},
    ?sendself(Record).


cs_hron_buy(#cs_hron_buy{type=Type}) ->
    case check_buy(Type) of
        {true, NeedMoney, AddAttack, AddHp, HronInfo, RoleInfo} ->
            #hronInfo{attackAdd=Attack, hpAdd=Hp} = HronInfo,
            NewHpAdd = Hp+AddHp,
            NewAttackAdd = Attack+AddAttack,
            case Type of
                ?BUY_TYPE_COIN ->
                    HronInfo2 = HronInfo#hronInfo{attackAdd=NewAttackAdd,hpAdd=NewHpAdd,
                                                  coinBuyTimes=HronInfo#hronInfo.coinBuyTimes+1};
                ?BUY_TYPE_GOLD ->
                    HronInfo2 = HronInfo#hronInfo{attackAdd=NewAttackAdd,hpAdd=NewHpAdd,
                                                  goldBuyTimes=HronInfo#hronInfo.goldBuyTimes+1}
            end,
            role_data:set_hronInfo(HronInfo2),
            case Type of
                ?BUY_TYPE_COIN ->
                    role_lib:deduct_money_f(RoleInfo, coin, NeedMoney, ?MONEY_DEC_TYPE_HRON_BUY, ?BUY_TYPE_COIN, "");
                ?BUY_TYPE_GOLD ->
                    role_lib:deduct_money_f(RoleInfo, gold, NeedMoney, ?MONEY_DEC_TYPE_HRON_BUY, ?BUY_TYPE_GOLD, "")
            end,
            ?sendself(#sc_hron_buy{result=1,attackAdd=NewAttackAdd,hpAdd=NewHpAdd});
        {false, Reason} ->
            ?sendself(#sc_hron_buy{result=Reason,attackAdd=0,hpAdd=0})
    end.

cs_hron_fight(#cs_hron_fight{}) ->
    case check_fight() of
        {true,DataDungeon, HronInfo} ->
            do_fight(DataDungeon,HronInfo);
        {false, Reason} ->
            ?sendself(#sc_hron_fight{fightInfo=[],result=Reason,rewardInfo=[],dungeonID=0,curDungeonNum=0,challengeTimes=0})
    end.

cs_hron_raids(#cs_hron_raids{}) ->
    case check_raids() of
        {true, HronInfo} ->
            do_hron_raids(HronInfo);
        {false, Reason} ->
            ?sendself(#sc_hron_raids{result=Reason, dungeonID=0, reward=[]})
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

do_hron_raids(#hronInfo{dungeonID=OldDungeonID,curDungeonNum=CurDungeonNum,maxDungeonNum=MaxDungeonNum}=HronInfo) ->
    {NewDungeonID, RewardInfoList} = hron_raids(OldDungeonID, [], CurDungeonNum, MaxDungeonNum),
    HronInfo2 = HronInfo#hronInfo{curDungeonNum=MaxDungeonNum+1,dungeonID=NewDungeonID},
    role_data:set_hronInfo(HronInfo2),
    ?sendself(#sc_hron_raids{result=0, dungeonID=NewDungeonID, reward=lists:reverse(RewardInfoList)}).

hron_raids(OldDungeonID, RewardInfoList, MaxDungeonNum, MaxDungeonNum) ->
    {ok, RewardInfo} = hron_raids2(OldDungeonID, MaxDungeonNum),
    NewDungeonNum = MaxDungeonNum + 1,
    NewDungeonID = random_dungeon(NewDungeonNum),
    {NewDungeonID, [RewardInfo|RewardInfoList]};
hron_raids(OldDungeonID, RewardInfoList, CurDungeonNum, MaxDungeonNum) ->
    {ok, RewardInfo} = hron_raids2(OldDungeonID, CurDungeonNum),
    NewDungeonNum = CurDungeonNum + 1,
    NewDungeonID = random_dungeon(NewDungeonNum),
    hron_raids(NewDungeonID, [RewardInfo|RewardInfoList], NewDungeonNum, MaxDungeonNum).

hron_raids2(DungeonID, CurDungeonNum) ->
    #data_dungeon{reward=Reward} = data_dungeon:get(DungeonID),
    Role = role_data:get_roleInfo(),
    {_, GerAddExpList, RewardItemList, RewardGerList} = role_reward:handle_dungeon_reward_f(Role, Reward, ?MONEY_ADD_TYPE_HRON_RAIDS, DungeonID, ""),
    RewardItemView = [item_lib:new_item2p_item_view(E) || E<- RewardItemList],
    RewardGerView = [ger_lib:new_ger2p_ger_view(E) || E<- RewardGerList],
    Reward2 = role_lib:reward2p_reward(Reward),
    Reward3 = Reward2#p_reward{gerExpList=GerAddExpList, itemList=RewardItemView, gerList=RewardGerView},
    ?CATCH_1(role_task_trigger:handle({dispach_task,role_pass_hron,CurDungeonNum}),E3),
    {ok, Reward3}.

do_fight(DataDungeon, HronInfo) ->
	#hronInfo{dungeonID=OldDungeonID,curDungeonNum=CurDungeonNum,maxDungeonNum=MaxDungeonNum,attackAdd=AttackAdd, hpAdd=HpAdd,challengeTimes=ChallengeTimes} = HronInfo,
	DefenderList = get_mon_list(DataDungeon, CurDungeonNum),
	FighterList = role_data:get_fighter_list(),
	RoleLieuAdd = role_data:get_lieu_add_attr(),
	AttackerList = add_buff(FighterList, AttackAdd,HpAdd),
	RoleID = role_data:get_roleID(),
	{Result, FightRecord, _} = role_fight:new(RoleID,AttackerList, DefenderList,RoleLieuAdd, {0,0}, false),
	if Result =:= true ->
		   % 胜利了
			
		   % 普通关卡的发奖励
		   #data_dungeon{reward=Reward,dungeonID=DungeonID} = DataDungeon,
		   Role = role_data:get_roleInfo(),
		   {_, GerAddExpList, RewardItemList, RewardGerList} = role_reward:handle_dungeon_reward_f(Role, Reward, ?MONEY_ADD_TYPE_HRON_CHALLENGE, DungeonID, ""),
		   RewardItemView = [item_lib:new_item2p_item_view(E) || E<- RewardItemList],
		   RewardGerView = [ger_lib:new_ger2p_ger_view(E) || E<- RewardGerList],
		   Reward2 = role_lib:reward2p_reward(Reward),
		   Reward3 = Reward2#p_reward{gerExpList=GerAddExpList, itemList=RewardItemView, gerList=RewardGerView},
		   
		   % 华容道结算
		   NewDungeonNum = CurDungeonNum+1,
		   NewDungeonID = random_dungeon(NewDungeonNum),
		   NewMaxDungeonNum =
               case CurDungeonNum > MaxDungeonNum of
                   true ->
                       CurDungeonNum;
                   false ->
                       MaxDungeonNum
               end,
		   HronInfo2 = HronInfo#hronInfo{curDungeonNum=NewDungeonNum,maxDungeonNum=NewMaxDungeonNum,dungeonID=NewDungeonID},
		   role_data:set_hronInfo(HronInfo2),
		   ?sendself(#sc_hron_fight{fightInfo=[FightRecord],rewardInfo=[Reward3],result=1,dungeonID=NewDungeonID,
                                    curDungeonNum=NewDungeonNum,challengeTimes=ChallengeTimes}),
		   ?CATCH_1(role_task_trigger:handle({dispach_task,role_pass_hron,CurDungeonNum}),E3);
	   true ->
           NewChallengeTimes = ChallengeTimes - 1,
		   HronInfo2 = HronInfo#hronInfo{challengeTimes=NewChallengeTimes,attackAdd=0,hpAdd=0,coinBuyTimes=0,goldBuyTimes=0},
		   role_data:set_hronInfo(HronInfo2),
		   ?sendself(#sc_hron_fight{fightInfo=[FightRecord],rewardInfo=[],result=2,dungeonID=OldDungeonID,
                                    curDungeonNum=CurDungeonNum,challengeTimes=NewChallengeTimes})
	end,
	?CATCH(role_task_trigger:handle({dispach_task,role_join_hron})).


%% 动态计算华容道的怪物
get_mon_list(DataDungeon, CurDungeonNum) ->
	Rank = get_mon_rank(CurDungeonNum),
	#data_dungeon{gerID2=MonList,gerID3=IDList} = DataDungeon,
	F = fun(E) ->
				GerTypeID = E#mon.gerTypeID,
				ger_attr:new_mon(GerTypeID, E#mon.gerLevel, Rank, [], lists:delete(GerTypeID, IDList))
		end,
	[?change_pos(Ger, Pos)||{Pos, Mon} <- MonList, is_record(Mon, mon), Ger<-[F(Mon)], is_record(Ger, ger)].
	
%% 计算怪物的品阶
get_mon_rank(CurDungeonNum) when CurDungeonNum < 10 -> (1.1+0.05*(CurDungeonNum-1))*CurDungeonNum;
get_mon_rank(CurDungeonNum) -> 13.5+2*(CurDungeonNum-9).
	
	

%% 加buff
add_buff(FighterList, 0,0) ->
	FighterList;
add_buff(FighterList, AttackAdd,HpAdd) ->
	[add_buff2(Ger, AttackAdd,HpAdd) || Ger<- FighterList].

add_buff2(Ger, AttackAdd, HpAdd) ->
	#ger{gerAttr=GerAttr}=Ger,
	#gerAttr{gerAttack=GerAttack, gerHpMax=GerHpMax} = GerAttr,
	if AttackAdd > 0 ->
		   Attack2 = trunc(GerAttack*(1+AttackAdd/100));
	   true ->
		   Attack2 = GerAttack
	end,
	if HpAdd > 0 ->
		   GerHpMax2 = trunc(GerHpMax*(1+HpAdd/100));
	   true ->
		   GerHpMax2 = GerHpMax
	end,
	GerAttr2 = GerAttr#gerAttr{gerAttack=Attack2,gerHpMax=GerHpMax2},
	Ger#ger{gerAttr=GerAttr2,gerHp=GerHpMax2}.

check_raids() ->
    #role{level=Level, vipLevel=VipLevel, roleID=RoleID, srcType=SrcType} = role_data:get_roleInfo(),
    case tencent_pay:check_pay_arg(RoleID, SrcType, 0) of
        true ->
            case is_open() of
                true ->
                    #hronInfo{curDungeonNum=CurDungeonNum, maxDungeonNum=MaxDungeonNum} = HronInfo = role_data:get_hronInfo(),
                    case MaxDungeonNum >= CurDungeonNum of
                        true ->
                            case Level >= data_hron:get(need_level) of
                                true ->
                                    case VipLevel >= data_hron:get(need_vip_level) of
                                        true ->
                                            {true, HronInfo};
                                        false ->
                                            {false, 3}
                                    end;
                                false ->
                                    {false, 2}
                            end;
                        false ->
                            {false, 1}
                    end;
                _ ->
                    {false, 4}
            end;
        false ->
            {false, 255}
    end.

check_fight() ->
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			case is_open() of
				true ->
					#hronInfo{challengeTimes=ChallengeTimes,dungeonID=DungeonID} = HronInfo = role_data:get_hronInfo(),
					case ChallengeTimes =< 0 of
						true ->
							{false, 4};
						false ->
							{true,data_dungeon:get(DungeonID), HronInfo}
					end;
				_ ->
					{false, 3}
			end;
		false ->
			{false, 255}
	end.

check_buy(Type) ->
    case is_open() of
        true ->
            HronInfo = role_data:get_hronInfo(),
            #hronInfo{challengeTimes=ChallengeTimes,coinBuyTimes=CoinBuyTimes,goldBuyTimes=GoldBuyTimes} = HronInfo,
            case ChallengeTimes > 0 of
                true ->
                    #role{coin=Coin} = RoleInfo = role_data:get_roleInfo(),
                    case Type of
                        ?BUY_TYPE_COIN ->
                            case CoinBuyTimes =:= 0 of
                                true ->
                                    {NeedCoin, AddAttack, AddHp} = data_hron:get(buy_coin),
                                    case Coin >= NeedCoin of
                                        true ->
                                            {true, NeedCoin, AddAttack, AddHp, HronInfo, RoleInfo};
                                        false ->
                                            {false, 2}
                                    end;
                                false ->
                                    {false, 6}
                            end;
                        ?BUY_TYPE_GOLD ->
                            case GoldBuyTimes =:= 0 of
                                true ->
                                    {NeedGold, AddAttack, AddHp} = data_hron:get(buy_gold),
                                    case role_lib:check_money(RoleInfo, gold, NeedGold) of
                                        true ->
                                            {true, NeedGold, AddAttack, AddHp, HronInfo, RoleInfo};
                                        false ->
                                            {false, 3}
                                    end;
                                false ->
                                    {false, 6}
                            end
                    end;
                false ->
                    {false, 5}
            end;		
        false ->
            {false, 4}
    end.

		   