%% @author admin
%% @doc 背包、装备、道具功能
%% Created 2013-3-15


-module(role_shop).
-compile(export_all).
-include("def_role.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

-define(SHOP_TYPE_NORMAL, 1).
-define(SHOP_TYPE_EXPLORE,2).

-define(SHOP_ID_JU_XIAN_ZHUANG, 6666).
-define(SHOP_ID_QI_HUO_PU, 8888).
-define(SHOP_ID_TREASURE, 9999).


-define(COST_TYPE_GOLD, 1).
-define(COST_TYPE_COIN, 2).
-define(COST_TYPE_REPU, 3).

-define(SHOP_TREASURE_ITEM, 1).
-define(SHOP_TREASURE_GER, 2).

%% ====================================================================
%% API functions
%% ====================================================================
refresh_shop_activity() ->
    {ok, ActivityName, ActivityEndTime, Mul, Discounts} = get_activity_info(),
    ?sendself(#sc_shop_treasure_new_activity{activityName=ActivityName,activityEndTime=ActivityEndTime,
                                             mul=Mul,discounts=Discounts}).

refresh_shop_treasure() ->
    #shop_treasure{nextRefreshTime = NextRefreshTime, itemList = ItemList} = role_data:get_shop_treasure(),
    Now = util:now(),
    case NextRefreshTime =< Now of
        true ->
            NewNextRefreshTime = get_next_refresh_time(Now),
            NewItemList = refresh_treasure(),
            role_data:set_shop_treasure(#shop_treasure{nextRefreshTime = NewNextRefreshTime,
                                                       itemList = NewItemList}),
            ?sendself(#sc_shop_treasure_new_shop{nextRefreshTime=NewNextRefreshTime,list=NewItemList});
        false ->
            ?sendself(#sc_shop_treasure_new_shop{nextRefreshTime=NextRefreshTime,list=ItemList})
    end.

cs_shop_treasure_buy(#cs_shop_treasure_buy{index=Index}) ->
    case catch check_shop_treasure_buy(Index) of
        {ok, RoleInfo, Type, TypeID, Num, CostType, NeedVal, NewShopTreasure} ->
            do_shop_treasure_buy(RoleInfo, Type, TypeID, Num, CostType, NeedVal, NewShopTreasure, Index);
        {false, Reason} ->
            ?sendself(#sc_shop_treasure_buy{result=Reason,index=Index})
    end.

check_shop_treasure_buy(Index) ->
    #shop_treasure{itemList = ItemList} = ShopTreasure = role_data:get_shop_treasure(),
    Item = lists:keyfind(Index, #p_treasure.index, ItemList),
    case erlang:is_record(Item, p_treasure) of
        false ->
            erlang:throw({false, 1});
        true ->
            next
    end,
    case Item#p_treasure.isBuy of
        true ->
            erlang:throw({false, 2});
        false ->
            next
    end,
    {ok, _ActivityName, _ActivityEndTime, Mul, Discounts} = get_activity_info(),
    #p_treasure{type=Type,typeID=TypeID,num=Num,costType=CostType,costVal=CostVal} = Item,
    NeedVal = erlang:trunc(Num * Mul * CostVal * Discounts / 10000),
    case CostType of
        ?COST_TYPE_COIN ->
            #role{coin=RoleCoin} = RoleInfo = role_data:get_roleInfo(),
            case RoleCoin >= NeedVal of
                true ->
                    next;
                false ->
                    erlang:throw({false, 3})
            end;
        ?COST_TYPE_GOLD ->
            RoleInfo = role_data:get_roleInfo(),
            case role_lib:check_money(RoleInfo, gold, NeedVal) of
                true ->
                    next;
                false ->
                    erlang:throw({false, 4})
            end;
        ?COST_TYPE_REPU ->
            #role{reputation=Reputation} = RoleInfo = role_data:get_roleInfo(),
            case Reputation >= NeedVal of
                true ->
                    next;
                false ->
                    erlang:throw({false, 5})
            end
    end,
    NewItemList = lists:keyreplace(Index, #p_treasure.index, ItemList, Item#p_treasure{isBuy=true}),
    NewShopTreasure = ShopTreasure#shop_treasure{itemList = NewItemList},
    {ok, RoleInfo, Type, TypeID, Num * Mul, CostType, NeedVal, NewShopTreasure}.

do_shop_treasure_buy(RoleInfo, Type, TypeID, Num, CostType, NeedVal, NewShopTreasure, Index) ->
    case Type of
        ?SHOP_TREASURE_ITEM ->
            ConsumeType = ?MONEY_DEC_TYPE_SHOP_TREASURE_BUY_ITEM;
        ?SHOP_TREASURE_GER ->
            ConsumeType = ?MONEY_DEC_TYPE_SHOP_TREASURE_BUY_GER
    end,
    case CostType of
        ?COST_TYPE_COIN ->
            RoleInfo2 = role_lib:deduct_money_f(RoleInfo, coin, NeedVal, ConsumeType, TypeID, "");
        ?COST_TYPE_GOLD ->
            RoleInfo2 = role_lib:deduct_money_f(RoleInfo, gold, NeedVal, ConsumeType, TypeID, "");
        ?COST_TYPE_REPU ->
            RoleInfo2 = role_lib:deduct_money_f(RoleInfo, reputation, NeedVal, ConsumeType, TypeID, "")
    end,
    case Type of
        ?SHOP_TREASURE_ITEM ->
            RewardItem = #new_item{itemTypeID=TypeID,itemNum=Num,itemLevel=1,itemRank=0},
            role_reward:handle_item_f(RoleInfo2, [RewardItem], ?MONEY_ADD_TYPE_SHOP_TREASURE_BUY, 0, "");
        ?SHOP_TREASURE_GER ->
            RewardGer = #new_ger{gerTypeID=TypeID, gerLevel=1, gerQuality=0},
            role_reward:handle_ger_f(lists:duplicate(Num, RewardGer), ?MONEY_ADD_TYPE_SHOP_TREASURE_BUY, 0, "")
    end,
    role_data:set_shop_treasure(NewShopTreasure),
    ?sendself(#sc_shop_treasure_buy{result=0,index=Index}).

cs_shop_refresh2(_) ->
    case check_shop_treasure_refresh() of
        {ok, NeedGold, NewItemList} ->
            do_shop_treasure_refresh_gold(NeedGold, NewItemList);
        {ok, BagOther, DelAcc, UpdateAcc, UpdateLogList, NewItemList} ->
            do_shop_treasure_refresh_item(BagOther, DelAcc, UpdateAcc, UpdateLogList, NewItemList);
        {false, Reason} ->
            ?sendself(#sc_shop_refresh2{result=Reason})
    end.

do_shop_treasure_refresh_gold(NeedGold, NewItemList) ->
    role_lib:deduct_money_f(role_data:get_roleInfo(), gold, NeedGold, ?MONEY_DEC_TYPE_SHOP_TREASURE_REFRESH, 0, ""),
    #shop_treasure{nextRefreshTime = NextRefreshTime} = role_data:get_shop_treasure(),
    role_data:set_shop_treasure(#shop_treasure{nextRefreshTime = NextRefreshTime,
                                                       itemList = NewItemList}),
    ?sendself(#sc_shop_refresh2{result=0, list=NewItemList}).

do_shop_treasure_refresh_item(BagOther, DelAcc, UpdateAcc, UpdateLogList, NewItemList) ->
    role_data:set_bagItem(BagOther),
    LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogList),
    {Date, _} = Time = erlang:localtime(),
    behavior_item_consume:log(role_data:get_roleID(), LogItemList, Date, Time, ?MONEY_DEC_TYPE_SHOP_TREASURE_REFRESH, 0, ""),
    
    case UpdateAcc =/= [] of
        true ->
            UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateAcc],
            ?sendself(#sc_item_update{updateList=UpdateList});
        _ ->
            next
    end,
    case DelAcc =/= [] of
        true ->
            DelItemIDList = [E||#item{itemUID=E}<-DelAcc],
            ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList});
        _ ->
            next
    end,
    
    #shop_treasure{nextRefreshTime = NextRefreshTime} = role_data:get_shop_treasure(),
    role_data:set_shop_treasure(#shop_treasure{nextRefreshTime = NextRefreshTime,
                                                       itemList = NewItemList}),
    ?sendself(#sc_shop_refresh2{result=0, list=NewItemList}).

check_shop_treasure_refresh() ->
    case item_lib:check_material(data_shop_treasure:get(need_item), 2) of
        false ->
            Role = role_data:get_roleInfo(),
            NeedGold = data_shop_treasure:get(need_gold),
            case role_lib:check_money(Role, gold, NeedGold) of
                true ->
                    {ok, NeedGold, refresh_treasure()};
                false ->
                    {false, 1}
            end;
        {true, BagOther2, DelAcc, UpdateAcc, UpdateLogList} ->
            {ok, BagOther2, DelAcc, UpdateAcc, UpdateLogList, refresh_treasure()}
    end.

cs_shop_treasure_info(_) ->
    {ok, ActivityName, ActivityEndTime, Mul, Discounts} = get_activity_info(),
    #shop_treasure{nextRefreshTime = NextRefreshTime, itemList = ItemList} = role_data:get_shop_treasure(),
    Now = util:now(),
    case NextRefreshTime =< Now of
        true ->
            NewNextRefreshTime = get_next_refresh_time(Now),
            NewItemList = refresh_treasure(),
            role_data:set_shop_treasure(#shop_treasure{nextRefreshTime = NewNextRefreshTime,
                                                       itemList = NewItemList}),
            ?sendself(#sc_shop_treasure_info{activityName=ActivityName,activityEndTime=ActivityEndTime,
                                             mul=Mul,discounts=Discounts,
                                             nextRefreshTime=NewNextRefreshTime,list=NewItemList});
        false ->
            ?sendself(#sc_shop_treasure_info{activityName=ActivityName,activityEndTime=ActivityEndTime,
                                             mul=Mul,discounts=Discounts,
                                             nextRefreshTime=NextRefreshTime,list=ItemList})
    end.

get_activity_info() ->
    Now = util:now(),
    case util:foldl(fun(Key, Acc) ->
                            case Key of
                                {activity, StartDateTime, EndDateTime} ->
                                    case Now >= util:datetime_to_seconds(StartDateTime)
                                        andalso Now =< util:datetime_to_seconds(EndDateTime) of
                                        true ->
                                            {return, Key};
                                        false ->
                                            Acc
                                    end;
                                _ ->
                                    Acc
                            end
                    end, false, data_shop_treasure:get_list()) of
        {activity, ActivityStartDateTime, ActivityEndDateTime} ->
            {ActivityName, Mul, Discounts} = data_shop_treasure:get({activity, ActivityStartDateTime, ActivityEndDateTime}),
            {ok, ActivityName, util:datetime_to_seconds(ActivityEndDateTime), Mul, Discounts};
        false ->
            {ok, <<"">>, 0, 1, 10000}
    end.

cs_shop_buy_num(_) ->
	do_get_shopNumList().


cs_shop_buy(#cs_shop_buy{num=Num,sellID=SellID,shopID=ShopID}) ->
	?ERR("REcv  cs_shop_buy  ,sellID=~w,shopID=~w,num=~w",[SellID,ShopID,Num]),
	case check_buy(Num, SellID, ShopID) of
		{true, RoleInfo, CostType, CostNum, DataSell} ->
			?ERR("REcv  cs_shop_buy  do_buy 1 "),
			do_buy(RoleInfo, CostType, CostNum, Num, DataSell);
		{true, RoleInfo, CostType, CostNum, DataSell, ShopNumList2,Ismore,ShopNumList4} ->
			?ERR("REcv  cs_shop_buy  do_buy 2 ShopNumList4=~w",[ShopNumList4]),
			role_data:set_shopNumList(ShopNumList2),
	
			case Ismore > 1 of
				true ->
					?sendself(#sc_shop_buy_num{shopNumList=ShopNumList4});
				false ->
					Pnum1=1
			end,
			do_buy(RoleInfo, CostType, CostNum, Num, DataSell);
		{false, Reason} ->
			?sendself(#sc_shop_buy{result=Reason})
	end.

cs_shop_encounter(#cs_shop_encounter{}) ->
	List = role_data:get_randomShopList(),
	?sendself(#sc_shop_encounter{shopList=transform_to_proto(List)}).

cs_shop_refresh(#cs_shop_refresh{shopID=ShopID}) ->
	case check_refresh(ShopID) of
		{true, Role, BagOther2, NeedGold, OldShop, RandomShopList2, DataShop, DelAcc, UpdateAcc, UpdateItemLogList} ->
			do_refresh(Role, BagOther2, NeedGold, OldShop, RandomShopList2, ShopID, DataShop, DelAcc, UpdateAcc, UpdateItemLogList);
		{false, Reason} ->
			?sendself(#sc_shop_refresh{result=Reason,newShop=[]})
    end.

init_randomShopList([]) ->
    #role{level=RoleLevel} = role_data:get_roleInfo(),
    ShopID = ?SHOP_ID_JU_XIAN_ZHUANG,
    #data_shop{refreshSec=RefreshSec} = DataShop = data_shop:get(ShopID),
    RandomList1 = random_shop(DataShop, RoleLevel),
	RandomList = fix_refresh(RandomList1),
    NowSec = util:now(),
    TimerRef = timer_wheel:add_plan(NowSec+RefreshSec, fun(TSec) ->planned_refresh(ShopID, TSec) end),
    NewShop = #p_shop_random{refreshSec = TimerRef, sellIDList=RandomList,shopID=ShopID},
    RandomShopList=[NewShop],
    role_data:set_randomShopList(RandomShopList);
init_randomShopList(RandomShopList) when is_list(RandomShopList)->
	NowSec = timer_wheel:nowsec(),
	#role{level=RoleLevel} = role_data:get_roleInfo(),
	RandomShopList2 = 
		lists:map(fun(#p_shop_random{refreshSec=RefreshSec,shopID=ShopID} = ShopRandom) ->
						  if RefreshSec > NowSec ->
								 TimerRef = timer_wheel:add_plan(RefreshSec, fun(TSec) -> planned_refresh(ShopID, TSec) end),
								 ShopRandom#p_shop_random{refreshSec=TimerRef};
							 true ->
								 #data_shop{refreshSec=RefreshInterval} = DataShop = data_shop:get(ShopID),
								 RandomList = random_shop(DataShop, RoleLevel),
								 TimerRef = timer_wheel:add_plan(NowSec+RefreshInterval, fun(TSec) ->planned_refresh(ShopID, TSec) end),
								 del_shop_num_by_shopID(ShopID),
								 ShopRandom#p_shop_random{refreshSec=TimerRef,sellIDList=RandomList}
						  end							 
				  end, RandomShopList),
	role_data:set_randomShopList(RandomShopList2);
init_randomShopList(_)-> ignore.

new_randomShop(ShopID) ->
	RandomShopList = role_data:get_randomShopList(),
	case lists:keyfind(ShopID, #p_shop_random.shopID, RandomShopList) of
		false ->
			case data_shop:get(ShopID) of
				#data_shop{refreshSec=RefreshSec,shopType=ShopType} = DataShop ->
					case ShopType of
						?SHOP_TYPE_EXPLORE ->
							#role{level=RoleLevel} = role_data:get_roleInfo(),
							RandomList = random_shop(DataShop, RoleLevel),
							NowSec = util:now(),
							TimerRef = timer_wheel:add_plan(NowSec+RefreshSec, fun(TSec) ->planned_refresh(ShopID, TSec) end),
							NewShop = #p_shop_random{refreshSec = TimerRef, sellIDList=RandomList,shopID=ShopID},
							RandomShopList2=[NewShop|RandomShopList],
							del_shop_num_by_shopID(ShopID),
							role_data:set_randomShopList(RandomShopList2),
							?sendself(#sc_shop_new{newShop=transform_to_proto(NewShop)});
						_ ->
							ignore
					end;
				_ ->
					ignore
			end;			
		_ ->
			ignore
	end.
		
%% ====================================================================
%% Internal functions
%% ====================================================================
false_reason(coin) ->
	3;
false_reason(gold) ->
	2;
false_reason(reputation) ->
	4;
false_reason(score) ->
	5;
false_reason(_) ->
	6.


random_shop(#data_shop{shopID=?SHOP_ID_JU_XIAN_ZHUANG}=DataShop, RoleLevel)  ->
	List1 = random_shop2(DataShop, RoleLevel),
    DataShop2 = data_shop:get(?SHOP_ID_QI_HUO_PU),
    List2 = random_shop2(DataShop2, RoleLevel),
    List1 ++ List2;
random_shop(DataShop, RoleLevel) ->
    random_shop2(DataShop, RoleLevel).

random_shop2(DataShop, RoleLevel) ->
    #data_shop{sellList=SellList,sellNum=SellNum} = DataShop,
    case util:fun_find(fun(#data_sell_random{roleMaxLevel=MaxLevel}) -> RoleLevel =< MaxLevel end, SellList) of
        #data_sell_random{randomList=RandomPool} ->
            List = util:random_weigh_list(RandomPool, SellNum),
            List2 = [E||{_Weigh, E} <- List],
            List2;
        Err ->
            ?ERR("random_shop err:~p~n",[Err]),
            []
    end.

check_refresh(ShopID) ->
	{ItemTypeID, ItemNum, GoldUnit} = data_shop_etc:get(explore_shop_refresh_need),
	BagOther = role_data:get_bagItem(),
	case item_lib:check_material2(BagOther, ItemTypeID, ItemNum) of
		{BagOther2, Rest, DelAcc, UpdateAcc, UpdateItemLogList} ->
			NeedGold = Rest*GoldUnit,
			Role = role_data:get_roleInfo(),
			case role_lib:check_money(Role, gold, NeedGold) of
				true ->
					RandomShopList = role_data:get_randomShopList(),
					case lists:keytake(ShopID, #p_shop_random.shopID, RandomShopList) of
						false ->
							{false, 3};
						{value, OldShop, RandomShopList2} ->							
							case data_shop:get(ShopID) of
								#data_shop{}=DataShop ->
									{true, Role, BagOther2, NeedGold, OldShop, RandomShopList2, DataShop, DelAcc, UpdateAcc, UpdateItemLogList};
										_ ->
											{false, 4}
							end
					end;
				false ->
					{false,2}
			end
	end.

do_refresh(Role, BagOther2, NeedGold, OldShop, RandomShopList2, ShopID, DataShop, DelAcc, UpdateAcc, UpdateItemLogList) ->
    #role{roleID=RoleID, level=RoleLevel} = Role,
    if NeedGold > 0 ->
           role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_REFRESH_ENCOUNTER_SHOP, ShopID, "");
       true ->
           ignore
    end,
    
    if DelAcc == [] andalso UpdateItemLogList == [] ->
           ignore;
       true ->
           %% 写道具日志
           LogItemList = role_item:itemList2logItemList(DelAcc, UpdateItemLogList),
           {Date, _} = Time = erlang:localtime(),
           behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_REFRESH_ENCOUNTER_SHOP, ShopID, "")
    end,
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
    role_data:set_bagItem(BagOther2),
    #p_shop_random{refreshSec=TimerRef} = OldShop,
    timer_wheel:cancel_plan(TimerRef),
    NewShop = do_refresh2(ShopID, RandomShopList2, RoleLevel, DataShop),
    ?sendself(#sc_shop_refresh{result=1,newShop=[transform_to_proto(NewShop)]}).

transform_to_proto(RandomShopList) when is_list(RandomShopList) ->
	[transform_to_proto(E) || E<-RandomShopList];
transform_to_proto(RandomShop) ->
	RandomShop#p_shop_random{refreshSec = timer_wheel:tarSec(RandomShop#p_shop_random.refreshSec)}.

planned_refresh(ShopID, Sec) ->
	ShopList = role_data:get_randomShopList(),
	case lists:keytake(ShopID, #p_shop_random.shopID, ShopList) of
		false ->
			ignore;
		{value, #p_shop_random{refreshSec=TimerRef}, ShopList2} ->
			?DEBUG("planned_refresh:~w\n",[TimerRef]),
			case timer_wheel:tarSec(TimerRef) of
				Sec ->
					case data_shop:get(ShopID) of
						#data_shop{} = DataShop ->
							#role{level=RoleLevel} = role_data:get_roleInfo(),
							NewShop = do_refresh2(ShopID, ShopList2, RoleLevel, DataShop),
							?sendself(#sc_shop_auto_refresh{updateShop=transform_to_proto(NewShop)});
						_ ->
							ignore
					end;
				_ ->
					ignore
			end
	end.

do_refresh2(ShopID, ShopList2, RoleLevel, DataShop) ->
	#data_shop{refreshSec=RefreshInterval} = DataShop,
	RandomList1 = random_shop(DataShop, RoleLevel),
	
	RandomList = fix_refresh(RandomList1),
	
	NowSec = util:now(),
	TimerRef = timer_wheel:add_plan(NowSec+RefreshInterval, fun(Sec) -> planned_refresh(ShopID, Sec) end), 
	NewShop = #p_shop_random{shopID=ShopID,refreshSec=TimerRef,sellIDList=RandomList},
	del_shop_num_by_shopID(ShopID),
	ShopList3 = [NewShop|ShopList2],
	role_data:set_randomShopList(ShopList3),
	NewShop.

fix_refresh(RandomList) ->
    case db_sql:get_guideState(role_data:get_roleID()) of
        Val when is_integer(Val)->
            CVal = data_shop_etc:get(shop_refresh_guide_num),
            case Val > CVal of
                true->
                    RandomList;
                false->
                    fix_refresh_data(RandomList)
            end;
        _->
            RandomList
    end.

fix_refresh_data(RandomList) ->
    case RandomList of   
        [E1,E2,E3,E4,E5,E6,E7,E8] ->
            case lists:keytake(21068, 1, [{E5},{E6},{E7},{E8}]) of
                {value, {SpecE}, [{LeftE1},{LeftE2},{LeftE3}]} ->
                    [E1,E2,E3,E4,SpecE|[LeftE1,LeftE2,LeftE3]];
                false ->
                    [E1,E2,E3,E4,21068|[E6,E7,E8]]
            end;
        _ ->
            RandomList
    end.
	
do_buy(RoleInfo, CostType, CostNum, BuyNum, DataSell) ->	
    #data_sell{sellID=SellID} = DataSell,
    RoleInfo2 = role_lib:deduct_money_f(RoleInfo, CostType, CostNum, ?MONEY_DEC_TYPE_SHOP_BUY, SellID, ""),
    role_reward:handle_sell_reward_f(RoleInfo2, mul_sell_reward(DataSell#data_sell.sellReward, BuyNum), ?MONEY_ADD_TYPE_SHOP_BUY, SellID, ""),
    ?sendself(#sc_shop_buy{result=1}).
	

mul_sell_reward(SellReward, 1) ->
    SellReward;
mul_sell_reward(#sell_reward{coin=Coin,roleExp=RoleExp,gerExp=GerExp,gold=Gold,item=Item,reputation=Repu,newGer=Ger}, BuyNum) ->
    NewItem =
        case Item of
            0 ->
                0;
            #new_item{itemNum=ItemNum} ->
                Item#new_item{itemNum=ItemNum*BuyNum};
            _ ->
                lists:map(
                  fun(#new_item{itemNum=ItemNum}=E) ->
                          E#new_item{itemNum=ItemNum*BuyNum}
                  end, Item)
        end,
    NewGer =
        case Ger of
            0 ->
                0;
            #new_ger{_='_'} ->
                lists:duplicate(BuyNum, Ger);
            _ ->
                lists:flatten(lists:duplicate(BuyNum, Ger))
        end,
    #sell_reward{coin=Coin*BuyNum,roleExp=RoleExp*BuyNum,gerExp=GerExp*BuyNum,gold=Gold*BuyNum,
                 item=NewItem,reputation=Repu*BuyNum,newGer=NewGer}.

check_buy(Num, SellID, ShopID) ->
	%% 判断非法数据
	if Num > 0 ->
		   %% 读取配置
		   case data_shop:get(ShopID) of
			   #data_shop{shopType=ShopType,sellList=SellList}  ->
				   %% 判断商店类型
				   case ShopType of
					   ?SHOP_TYPE_NORMAL ->
						   case lists:member(SellID, SellList) of
							   true ->
								   check_buy2(Num, ShopID, SellID);
							   false ->
								   {false, 13}
						   end;
					   ?SHOP_TYPE_EXPLORE ->
						  %% 奇遇商店
						   RandomShopList = role_data:get_randomShopList(),
						   case lists:keyfind(ShopID, #p_shop_random.shopID, RandomShopList) of
							   false ->
								   {false, 14};
							   #p_shop_random{sellIDList = SellList2} ->
								   ?INFO("sellIDList=~w,id=~w,",[SellList2,SellID]),
								   case lists:member(SellID, SellList2) of
									   true ->
										   check_buy2(Num, ShopID, SellID);
									   false ->
										   {false ,11}
								   end
						   end									   
				   end;
			   _ ->
				   {false,10}
		   end;
	   true ->
		   {false, 12}
	end.


check_buy2(Num, ShopID, SellID) ->
	case data_sell:get(SellID) of
		#data_sell{needVipLevel=NeedVipLevel} = DataSell ->
			RoleInfo = role_data:get_roleInfo(),
			%% 判断vip等级是否足够
			if RoleInfo#role.vipLevel >= NeedVipLevel ->
				   #data_sell{costType=CostType, costNum=CostNumT} = DataSell,
                   CostNum = CostNumT * Num,
				   %% 判断货币是否足够
				   case role_lib:check_money(RoleInfo, CostType, CostNum) of
					   false ->
						   {false, false_reason(CostType)};
					   true ->
						   MaxBuyNum = DataSell#data_sell.maxBuyNum,
						   %% 判断是否有购买次数限制
						   case MaxBuyNum < 0 of
							   true ->
								   {true, RoleInfo, CostType, CostNum, DataSell};
							   false ->
								   ShopNumList = role_data:get_shopNumList(),
								   ?ERR("REcv  check_buy2  ,shopNumList=~w",[ShopNumList]),
								   %% 读取已购买次数
								   case util:fun_take(fun(E) ->
															  E#p_shop_num.shopID=:= ShopID andalso E#p_shop_num.sellID=:= SellID
													  end, ShopNumList) of
										false ->
											ShopNumList2=ShopNumList,
											Num2 = Num;
										{value, #p_shop_num{buyNum=BuyedNum}, ShopNumList2} ->
											?ERR("REcv  check_buy2 11  ,ShopNumList2=~w,BuyedNum=~w,Num=~w",[ShopNumList2,BuyedNum,Num]),
											Num2 = Num + BuyedNum 
											
								   end,
								   %% 判断能否继续购买
								   if Num2 =< MaxBuyNum ->
										  ShopNumList3 = [#p_shop_num{buyNum=Num2,sellID=SellID,shopID=ShopID}|ShopNumList2],
										  case Num > 1 of
												true ->
													Num3= Num2 -1;
												false ->
													Num3 = Num2
										  end,
										  ShopNumList4 = [#p_shop_num{buyNum=Num3,sellID=SellID,shopID=ShopID}],
										  ?ERR("REcv  check_buy2 22  ,ShopNumList3=~w,SellID=~w,ShopID=~w",[ShopNumList3,SellID,ShopID]),
										  {true, RoleInfo, CostType, CostNum, DataSell, ShopNumList3,Num,ShopNumList4};
									  true ->
										  {false, 8}
								   end
						   end
				   end;
			   
			   true ->
				   {false, 7}
			end;
		_ ->
			{false, 9}
	end.


del_shop_num_by_shopID(ShopID) ->
	ShopNumList = role_data:get_shopNumList(),
	ShopNumList2 = lists:filter(fun(E) -> E#p_shop_num.shopID =/= ShopID end, ShopNumList),
	role_data:set_shopNumList(ShopNumList2),
	if ShopNumList2 == ShopNumList ->
		   ignore;
	   true ->
	?sendself(#sc_shop_buy_num{shopNumList=ShopNumList2})
	end,
	ShopNumList2.



do_get_shopNumList() ->
	ShopNumList = role_data:get_shopNumList(),
	?sendself(#sc_shop_buy_num{shopNumList=ShopNumList}).

refresh_treasure() ->
    #role{level=RoleLevel} = role_data:get_roleInfo(),
    List = random_shop2(data_shop:get(?SHOP_ID_TREASURE), RoleLevel),
    {NewList, _} =
        lists:foldr(fun(#data_shop_treasure{type=Type, typeID=TypeID, minNum=MinNum, maxNum=MaxNum,
                                            costType=CostType, costValue=CostValue}, {AccList, AccIndex}) ->
                            NewTreasure =
                                #p_treasure{
                                            type=Type
                                            ,typeID=TypeID
                                            ,num=MinNum + random:uniform(MaxNum - MinNum + 1) - 1
                                            ,costType=CostType
                                            ,costVal=CostValue
                                            ,isBuy=false
                                            ,index=AccIndex},
                            {[NewTreasure|AccList], AccIndex + 1}
                    end, {[], 1}, List),
    NewList.


get_next_refresh_time(Now) ->
    {_, Time} = util:seconds_to_datetime(Now),
    ConfigTimeList = data_shop_treasure:get(refresh_time),
    ConfigTimeList2 = lists:sort(fun(Time1, Time2) ->
                                        Time1 =< Time2
                                 end, ConfigTimeList),
    [ConfigMinTime|_] = ConfigTimeList2,
    get_next_refresh_time(ConfigMinTime, ConfigTimeList2, Time).

get_next_refresh_time(ConfigMinTime, [], _Time) ->
    util:datetime_to_seconds({erlang:date(), ConfigMinTime}) + ?ONE_DAY_SECONDS;
get_next_refresh_time(ConfigMinTime, [ConfigTime|ConfigTimeList], Time) ->
    if
        Time < ConfigTime ->
            util:datetime_to_seconds({erlang:date(), ConfigTime});
        true ->
            get_next_refresh_time(ConfigMinTime, ConfigTimeList, Time)
    end.

