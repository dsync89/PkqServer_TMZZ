%% @author admin
%% @doc 宝箱功能
%% Created 2013-6-20


-module(role_box).
-compile(export_all).
-include("def_role.hrl").
-include("def_battle.hrl").
-include("def_item.hrl").
-include("def_weibo_share.hrl").
%% API functions
-export([]).

%% Internal functions 
-export([]).

-define(TAB_TYPE_SPIRIT,24001).%%24001：召唤精灵   24002：祈祷装备
-define(TAB_TYPE_EQUIP,24002).
-define(OPEN_BOX_ITEM,0).
-define(OPEN_BOX_GOLD_1,1).
-define(OPEN_BOX_GOLD_10,2).
%% ====================================================================
%% API functions
%% ====================================================================
cs_box_get_spirit_equip_count(_)->
	#limitInfo{spiritGoldBoxCount = SpiritGoldBoxCount
				   ,spiritGoldBonusBoxCount=SpiritGoldBonusBoxCount
				   ,equipGoldBoxCount = EquipGoldBoxCount
				   ,equipGoldBonusBoxCount=EquipGoldBonusBoxCount} = role_data:get_limitInfo(),
	Count1 =10 -((SpiritGoldBoxCount+SpiritGoldBonusBoxCount) rem 10),
	Count2 =10-((EquipGoldBoxCount+EquipGoldBonusBoxCount) rem 10),
	 {Type1,Num1} = data_fixed_box_withitem:get({need_item,?TAB_TYPE_SPIRIT}),
	 {Type2,Num2} = data_fixed_box_withitem:get({need_item,?TAB_TYPE_EQUIP}),
	
	RecordData = #sc_box_get_spirit_equip_count{count1=Count1,count2=Count2,needItemTypeID1=Type1,needNum1=Num1,needItemTypeID2=Type2,needNum2=Num2},
	?DEBUG("玩家次数信息:~w",[{RecordData,SpiritGoldBoxCount+SpiritGoldBonusBoxCount,EquipGoldBoxCount+EquipGoldBonusBoxCount}]),
	?sendself(RecordData).

cs_box_item(#cs_box_item{itemTypeID=ItemTypeID,num=Num}) ->
	case check_box_item(ItemTypeID, Num) of
		{true, BagOther2, RewardList, DelAcc, UpdateAcc, UpdateItemLogAcc} ->
			do_box_item(BagOther2, RewardList, ItemTypeID, DelAcc, UpdateAcc, UpdateItemLogAcc,Num);
		{false, Reason} ->
			?sendself(#sc_box_item{result=Reason,reward=[],itemTypeID=ItemTypeID,num=0})
	end.

cs_box_shop(#cs_box_shop{tab=Tab,type=Type}) ->
	case check_box_shop(Tab,Type) of
		{?OPEN_BOX_ITEM,BagOther,DelAcc,UpdataAcc,UpdateLogList}->
			do_box_shop_by_item(Tab,Type,BagOther,DelAcc,UpdataAcc,UpdateLogList);
		{_, Role, NeedGold,Use} ->
			do_box_shop_by_gold(Tab,Type, Role, NeedGold,Use);
		{false, Reason} ->
			?DEBUG("========错误===~w",[Reason]),
			?sendself(#sc_box_shop{result=Reason, reward=[]})
			end.

cs_box_shop_info(_) ->
	List = [data_box_price:get(E)||E<-data_box_price:get_list()],
	ShopInfo = [#p_shop_box_info{itemTypeID=ItemTypeID,valueOne=ValueOne,valueTen=ValueTen,discount=Discount,endtime=EndTime,isOpenActivity=IsOpenActivity} || #data_box_price2{itemTypeID=ItemTypeID,oncePrice=ValueOne,tenTimesPrice=ValueTen,discount=Discount,endtime=EndTime,isOpenActivity=IsOpenActivity}<-List],
	?sendself(#sc_box_shop_info{info=ShopInfo,stopTime=util:datetime_to_seconds(data_box:get(datetime))}),
	cs_box_get_spirit_equip_count(#cs_box_get_spirit_equip_count{}).
	
%% ====================================================================
%% Internal functions
%% ====================================================================
open_need_gold(DataBoxPrice, ?OPEN_BOX_GOLD_1) ->
	DataBoxPrice#data_box_price2.oncePrice;
open_need_gold(DataBoxPrice, ?OPEN_BOX_GOLD_10) ->
	DataBoxPrice#data_box_price2.tenTimesPrice.


random_reward(Config, Times, List) when Times > 0->
	R = util:random_one_from_weigh_list(Config),
	random_reward(Config, Times-1, [R|List]);
random_reward(_, _, List) ->
	List.

-define(upcounter_limitInfo(Filed),begin
									   LimitInfo = role_data:get_limitInfo(),
									   Count = LimitInfo#limitInfo.Filed,
									   NewCount = Count+1,
									    LimitInfo2 = LimitInfo#limitInfo{Filed=NewCount},
				   						role_data:set_limitInfo(LimitInfo2),
									   NewCount
								   end).
-define(upcounter_limitInfo(Filed1,Field2),begin
									   LimitInfo = role_data:get_limitInfo(),
									    Count = LimitInfo#limitInfo.Filed1,
									   Count2 = LimitInfo#limitInfo.Field2,
									   NewCount = Count+1,
									    LimitInfo2 = LimitInfo#limitInfo{Filed1=NewCount},
				   						role_data:set_limitInfo(LimitInfo2),
									   NewCount+Count2
								   end).
										  

do_box_shop_by_gold(Tab,Type, Role, NeedGold,Use) ->
	MainGerTypeID = role_data:get_mainGerTypeID(),
    case Tab of
        ?TAB_TYPE_SPIRIT->
            LogItemType1 = ?MONEY_DEC_TYPE_SHOP_SPIRIT_BOX,
            LogType = ?MONEY_ADD_TYPE_SHOP_SPIRITE_BOX;
        ?TAB_TYPE_EQUIP->
            LogItemType1 = ?MONEY_DEC_TYPE_SHOP_EQUIP_BOX,
            LogType = ?MONEY_ADD_TYPE_SHOP_EQUIP_BOX
    end,
	Role2 = role_lib:deduct_gold_f(Role, NeedGold, LogItemType1, Type, ""),
	?DEBUG("======》》》消耗：~w",[{LogItemType1,NeedGold}]),
	case Type of
		?OPEN_BOX_GOLD_1 ->
			?DEBUG("之前次数信息:~w",[role_data:get_limitInfo()]),
			NewCount1 =
				case Use of
					goldBonus ->
						case Tab of
							?TAB_TYPE_SPIRIT->
								?upcounter_limitInfo(spiritGoldBonusBoxCount,spiritGoldBoxCount);
							?TAB_TYPE_EQUIP->
								?upcounter_limitInfo(equipGoldBonusBoxCount,equipGoldBoxCount)
						end;
					gold ->
						case Tab of
							?TAB_TYPE_SPIRIT->
								?upcounter_limitInfo(spiritGoldBoxCount,spiritGoldBonusBoxCount);
							?TAB_TYPE_EQUIP->
								?upcounter_limitInfo(equipGoldBoxCount,equipGoldBonusBoxCount)
						end
				end,
			?DEBUG("之后次数信息:~w",[role_data:get_limitInfo()]),
			BoxID =
				case NewCount1 rem 10 =:= 0 of
					true->
						case data_fixed_box_withitem:get({five_star_box,Tab}) of
							?undefined->
								Tab;
							B->
								B
						end;
					_->
						case Use of
							goldBonus->
								case data_fixed_goldbonus_box:get({NewCount1, Tab}) of
									?undefined ->
										Tab;
									B1 ->
										B1
								end;
							gold->
								case data_fixed_gold_box:get({NewCount1, Tab}) of
									?undefined ->
										Tab;
									B1 ->
										B1
								end
						end
				end,
			?DEBUG("抽取的宝箱:~w",[{BoxID,MainGerTypeID}]),
			[R1|_] = data_box:get({BoxID, MainGerTypeID}),
			RewardList = [util:random_one_from_weigh_list(R1)],
            MaxAllNum = util:random_one_from_weigh_list(data_box:get(max_all_num_one));
		?OPEN_BOX_GOLD_10->
			[R1, R2, R3, R4, R5] = data_box:get({Tab, MainGerTypeID}),
			{R1T, R2T, R3T, R4T, R5T} = data_box:get(open_box_10),
			RL1 = random_reward(R1, R1T, []),
			RL2 = random_reward(R2, R2T, RL1),
			RL3 = random_reward(R3, R3T, RL2),
			RL4 = random_reward(R4, R4T, RL3),
			RL5 = random_reward(R5, R5T, RL4),
			RewardList = util:random_list2(RL5),
            MaxAllNum = util:random_one_from_weigh_list(data_box:get(max_all_num_ten))
	end,
    RewardList2 = calc_critical(RewardList, MaxAllNum),
	RewardView = role_reward:transform2p_reward_view(RewardList2, []),
	role_reward:handle_sys_reward(Role2, RewardList2, LogType, Type, ""),
	?sendself(#sc_box_shop{result=1, reward=RewardView}),
	cs_box_get_spirit_equip_count(#cs_box_get_spirit_equip_count{}),
	broadcast_box_reward(Role2#role.roleName, RewardList2),
	role_invite:do_add_weibo_share_mark(?SHARE_TYPE_SHOP_BOX),
	case Type of
		?OPEN_BOX_GOLD_1->
			?CATCH_1(role_task_trigger:handle({dispach_task,role_extract_card,1}),E1);
		?OPEN_BOX_GOLD_10->
			?CATCH_1(role_task_trigger:handle({dispach_task,role_extract_card,10}),E2)
	end.

calc_critical(RewardList, MaxAllNum) ->
    case util:now() =< util:datetime_to_seconds(data_box:get(datetime)) of
        true ->
            CanMaxAllNum = calc_can_max_all_num(RewardList),
            case CanMaxAllNum < MaxAllNum of
                true ->
                    RewardList;
                false ->
                    calc_critical2(MaxAllNum, RewardList, [])
            end;
        false ->
            RewardList
    end.

calc_critical2(MaxAllNum, RewardList1, RewardList2) ->
    NowMaxAllNum = calc_reward_num(RewardList1 ++ RewardList2),
    case NowMaxAllNum >= MaxAllNum of
        true ->
            lists:reverse(RewardList2) ++ RewardList1;
        false ->
            case RewardList1 of
                [] ->
                    [{Type, GerTypeID, Num}|RewardList1Left] = lists:reverse(RewardList2),
                    RewardList2Left = [];
                [{Type, GerTypeID, Num}|RewardList1Left] ->
                    RewardList2Left = RewardList2
            end,
            #data_ger{gerStar=Star} = data_ger:get(GerTypeID),
            ConfigList = data_box:get({star, Star}),
            {_, MaxNum} = lists:last(ConfigList),
            case Num >= MaxNum of
                true ->
                    calc_critical2(MaxAllNum, RewardList1Left, [{Type, GerTypeID, Num}|RewardList2Left]);
                false ->
                    NewNum = util:random_one_from_weigh_list(ConfigList),
                    case NewNum > Num of
                        true ->
                            case NowMaxAllNum - Num + NewNum > MaxAllNum of
                                true ->
                                    NewNum2 = MaxAllNum - NowMaxAllNum;
                                false ->
                                    NewNum2 = NewNum
                            end,
                            calc_critical2(MaxAllNum, RewardList1Left, [{Type, GerTypeID, NewNum2}|RewardList2Left]);
                        false ->
                            calc_critical2(MaxAllNum, RewardList1Left, [{Type, GerTypeID, Num}|RewardList2Left])
                    end
            end
    end.

calc_can_max_all_num(RewardList) ->
    lists:foldr(fun({_, GerTypeID, _}, Acc) ->
                        #data_ger{gerStar=Star} = data_ger:get(GerTypeID),
                        {_, MaxNum} = lists:last(data_box:get({star, Star})),
                        Acc + MaxNum
                end, 0, RewardList).

calc_reward_num(RewardList) ->
    lists:foldr(fun({_, _, Num}, Acc) ->
                        Acc + Num
                end, 0, RewardList).

do_box_shop_by_item(Tab,Type,BagOther,DelAcc,UpdateAcc,UpdateLogList)->
	MainGerTypeID = role_data:get_mainGerTypeID(),
	#role{roleID=RoleID} = Role = role_data:get_roleInfo(),
	%% 写道具日志
	LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogList),
	{Date, _} = Time = erlang:localtime(),
	case Tab of
		?TAB_TYPE_SPIRIT->
			LogDecType = ?MONEY_DEC_TYPE_SHOP_SPIRIT_BOX,
			LogAddType = ?MONEY_ADD_TYPE_SHOP_SPIRITE_BOX;
		?TAB_TYPE_EQUIP->
			LogDecType = ?MONEY_DEC_TYPE_SHOP_EQUIP_BOX,
			LogAddType = ?MONEY_ADD_TYPE_SHOP_EQUIP_BOX
	end,
	?DEBUG("====消耗物品==》》》~w",[{LogDecType,data_fixed_box_withitem:get({need_item,Tab})}]),
	behavior_item_consume:log(RoleID, LogItemList, Date, Time, LogDecType, Type, ""),
	case UpdateAcc =/= [] of
		true ->
			UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateAcc],
			?sendself(#sc_item_update{updateList=UpdateList});
		_ ->
			ignore
	end,
	DelItemIDList = [E||#item{itemUID=E}<-DelAcc],
	?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
	?DEBUG("之前次数信息:~w",[role_data:get_limitInfo()]),
	NewCount1 =
		case Tab of
			?TAB_TYPE_SPIRIT->
				?upcounter_limitInfo(spiritItemBoxCount);
			?TAB_TYPE_EQUIP->
				?upcounter_limitInfo(equipItemBoxCount)
		end,
	?DEBUG("之后次数信息:~w",[role_data:get_limitInfo()]),
	BoxID=
		case data_fixed_box_withitem:get({NewCount1,Tab}) of
			?undefined->
				case data_fixed_box_withitem:get({defualt_box,Tab}) of
					?undefined->
						Tab;
					C->
						C
				end;
			B->
				B
		end,
	[R1|_] = data_box:get({BoxID, MainGerTypeID}),
	RewardList = [util:random_one_from_weigh_list(R1)],
    RewardList2 = calc_critical(RewardList, util:random_one_from_weigh_list(data_box:get(max_all_num_one))),
	role_data:set_bagItem(BagOther),
	?DEBUG("奖励信息:~w",[{LogAddType,RewardList2}]),
	role_reward:handle_sys_reward(Role, RewardList2, LogAddType, Type, ""),
	?sendself(#sc_box_shop{result=1,reward=role_reward:transform2p_reward_view(RewardList2, [])}),
	broadcast_box_reward(Role#role.roleName, RewardList2),
	role_invite:do_add_weibo_share_mark(?SHARE_TYPE_BAG_BOX),
	?CATCH(role_task_trigger:handle({dispach_task,role_extract_card,1})).

broadcast_box_reward(RoleName, RewardList)	->
%将奖励列表读取出来，然后对4星以上武将进行广播
	{_, _, _, _, _, ItemList, GerList} = role_reward:transform2normal_reward(RewardList),
	lists:foreach(fun(X)->#new_ger{gerTypeID=GerTypeId, gerLevel=GerLevel, gerQuality=GerQuality}=X,
						  #data_ger{gerStar=GerStar}=data_ger:get(GerTypeId),
						  if	GerStar >= 4	->
									GerView=#p_ger_view{gerQuality=GerQuality, gerLevel=GerLevel, gerTypeID=GerTypeId},
									broadcast_server:bc_msgID(10013, [RoleName, GerView, "1"]);
								true	->
									ok
						  end
				  end, GerList),
	lists:foreach(fun(X)->
						  #new_item{itemTypeID=ItemTypeID,itemLevel=ItemLevel,itemRank=ItemRank,itemNum=ItemNum}=X,
						  #data_item{itemStar=ItemStar} = data_item:get(ItemTypeID),
						  if	ItemStar >= 5	->
									ItemView = #p_item_view{itemLevel=ItemLevel,itemNum=ItemNum,itemRank=ItemRank,itemTypeID=ItemTypeID},
									broadcast_server:bc_msgID(10032, [RoleName, ItemView, "1"]);
								true	->
									ok
						  end
				  end, ItemList).

check_box_shop(Tab,Type) ->
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			case check_box_shop_param(Tab,Type) of
				true->
					case Type of
						?OPEN_BOX_ITEM->
							case data_fixed_box_withitem:get({need_item,Tab}) of
								{ItemTypeID,Num}->
									case item_lib:check_material(ItemTypeID, Num) of
										false->
											?DEBUG("======>>>>>~w",[role_data:get_bagItem()]),
											{false,3};
										{true, BagOther2, DelAcc, UpdateAcc, UpdateLogList}->
											{Type,BagOther2,DelAcc,UpdateAcc,UpdateLogList}
									end;
								_->
									{false,4}
							end;
						_ ->
							case data_box_price:get(Tab) of
								#data_box_price2{} = DataBoxPrice ->
									NeedGold = open_need_gold(DataBoxPrice, Type),
									Role = role_data:get_roleInfo(),
									case role_lib:check_money(Role, gold, NeedGold) of
										true ->
											case Role#role.goldBonus > 0 of
												true ->
													{Type, Role, NeedGold,goldBonus};
												false ->
													{Type, Role, NeedGold,gold}
											end;
										false ->
											{false, 2}
									end;
								_ ->
									{false,4}
							end
					end;
				false ->
					{false,4}
			end;
		false ->
			{false, 255}
	end.

check_box_shop_param(Tab,Type)->
	case Tab=:=?TAB_TYPE_SPIRIT orelse Tab=:=?TAB_TYPE_EQUIP of
		true->
			case Type=:=?OPEN_BOX_ITEM orelse Type=:=?OPEN_BOX_GOLD_1 orelse Type=:=?OPEN_BOX_GOLD_10 of
				true->
					true;
				_->
					false
			end;
		false->
			false
	end.


do_box_item(BagOther2, RewardList, ItemTypeID, DelAcc, UpdateAcc, UpdateItemLogAcc,Num) ->
	#role{roleID=RoleID} = Role = role_data:get_roleInfo(),
	%% 写道具日志
	LogItemList = role_item:itemList2logItemList(DelAcc, UpdateItemLogAcc),
	{Date, _} = Time = erlang:localtime(),
	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_BAG_BOX, ItemTypeID, ""),

	role_data:set_bagItem(BagOther2),
	role_reward:handle_sys_reward(Role, RewardList, ?MONEY_ADD_TYPE_BAG_BOX, ItemTypeID, ""),
    case UpdateAcc of
        [] ->
            next;
        _ ->
            ?sendself(#sc_item_update{updateList=[#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateAcc]})
    end,
    case DelAcc of
        [] ->
            next;
        _ ->
            ?sendself(#sc_item_delete_notify{itemUIDList=[ItemUID||#item{itemUID=ItemUID}<-DelAcc]})
    end,
	?sendself(#sc_box_item{result=1,reward=role_reward:transform2p_reward_view(RewardList, []),itemTypeID=ItemTypeID,num=Num}),
	role_invite:do_add_weibo_share_mark(?SHARE_TYPE_BAG_BOX).


check_box_item(ItemTypeID, Num) when Num >= 1 ->
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			case data_item:get(ItemTypeID) of
				#data_item{itemType=?box} ->
					case item_lib:check_material(ItemTypeID, Num) of
						{true, BagOther2, DelAcc, UpdateAcc, UpdateLogList} ->
							MainGerTypeID = role_data:get_mainGerTypeID(),
							case data_box:get({ItemTypeID,MainGerTypeID}) of
								[RConfig|_] ->
									RewardList =
										lists:foldr(
										  fun(_, Acc) ->
												  Reward = util:random_one_from_weigh_list(RConfig),
												  [Reward|Acc]
										  end, [], lists:seq(1, Num)),
									{true, BagOther2, RewardList, DelAcc, UpdateAcc, UpdateLogList};
								_ ->
									{false, 4}
							end;
						false ->
							{false, 2}
					end;
				_ ->
					{false,3}
			end;
		false ->
			{false, 255}
	end.


	


config_format_va(List) ->
	lists:foldl(fun({E,Value},Acc) when is_tuple(E),is_integer(element(1,E))->
						L = tl(tuple_to_list(E)),
						ItemTypeID = element(1,E),
						AddList = [{{ItemTypeID, F},Value}||F<-L],
						AddList++Acc;
				   (E,Acc) ->
						[E|Acc]
				end, [], List).
						
						