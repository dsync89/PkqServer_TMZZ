%% @author admin
%% @doc 背包、装备、道具功能
%% Created 2013-3-15


-module(role_item).
-compile(export_all).
-include("def_role.hrl").
-include("def_battle.hrl").
-include("def_item.hrl").
-include("def_reward.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
cs_item_use_info(_) ->
    ItemUseList = role_data:get_itemUseList(),
    PItemUseInfoList =
        lists:map(fun(#item_use_info{itemTypeID=ItemTypeID,useTimes=UseTimes}) ->
                          #data_item_use{maxTimes=MaxTimes} = data_item_use:get(ItemTypeID),
                          #p_item_use_info{type_id=ItemTypeID, left_times=erlang:max(MaxTimes - UseTimes, 0)}
                  end, ItemUseList),
    ?sendself(#sc_item_use_info{use_info_list=PItemUseInfoList}).

cs_item_bag(_) ->
	BagEquip = role_data:get_bagEquip(),
	BagItem = role_data:get_bagItem(),
	AllItem = [item_lib:item2p_item(E)||E<-(BagEquip ++ BagItem)],
    case erlang:length(AllItem)  > 2000 of
        false ->
            ?sendself(#sc_item_bag{allItem=AllItem});
        true ->
            {AllItem1, AllItemRest} = lists:split(2000, AllItem),
            ?sendself(#sc_item_bag{allItem=AllItem1}),
            send_rest(AllItemRest)
    end.

send_rest(AllItem) ->
    case erlang:length(AllItem) > 2000 of
        true ->
            {AllItem1, AllItemRest} = lists:split(2000, AllItem),
            ?sendself(#sc_item_more{list=AllItem1}),
            send_rest(AllItemRest);
        false ->
            ?sendself(#sc_item_more{list=AllItem}),
            ?sendself(#sc_item_more{list=[]})
    end.

cs_item_equip(_) ->
	EquipedList = [item_lib:item2p_equip(E, GerID) || {GerID,EquipList}<-role_data:get_equiped_list(), E<-EquipList],
	?sendself(#sc_item_equip{allEquip=EquipedList}).
	


%% @doc 卖道具
%% cs_item_sell(#cs_item_sell{itemUIDList=ItemIDList}) ->
%% 	case check_sell(ItemIDList) of
%% 		{true, DelItemList, BagEquip2, BagOther2} ->
%% 			do_sell3(DelItemList, BagEquip2, BagOther2);
%% 		{false, Reason} ->
%% 			?sendself(#sc_item_sell{result=Reason,reward=[]})
%% 	end.
cs_item_sell(#cs_item_sell{itemUIDList=ItemUIDList})->
	case check_sell_item(ItemUIDList) of
		{false,Reason}->
			?sendself(#sc_item_sell{result=Reason,gold=0,reward=[]});
		{true,NewBagItem,NewBagEquip,SellItemList,SellEquipList,SellP}->
			#role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
			role_lib:add_coin_f(RoleInfo, SellP, ?MONEY_ADD_TYPE_SELL_ITEM, 0, ""),
			role_data:set_bagEquip(NewBagEquip),
			role_data:set_bagItem(NewBagItem),
			DelItemIDList = [E||#item{itemUID=E}<-SellItemList]++[E||#item{itemUID=E}<-SellEquipList],
			?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
			LogItemList1 = role_item:itemList2logItemList(SellItemList, []),
			LogItemList = role_item:itemList2logItemList(SellEquipList, LogItemList1),
			{Date, _} = Time = erlang:localtime(),
			behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_SELL_ITEM, 0, ""),
			?sendself(#sc_item_sell{result=1,gold=SellP,reward=[#p_reward_view3{type=1, typeID=21100, num=SellP}]})
	end.


%% @doc 卸下道具
cs_item_down_equip(#cs_item_down_equip{gerID=GerID,itemPos=ItemPos}) ->
	?DEBUG("in gameserver[role_item], debug item_down_equip,  ~p~n", ["laochen"]),
	case check_down_equip(GerID, ItemPos) of
		{true, Item, EquipList2}->
			do_down_equip(GerID, ItemPos, Item, EquipList2);
		{false, Reason} ->
			?sendself(#sc_item_down_equip{result=Reason})
	end.

%% @doc 穿上道具
cs_item_up_equip(#cs_item_up_equip{gerID=GerID,itemPos=ItemPos, itemUID=ItemUID, itemGerID= ItemGerID}) ->
	case check_up_equip(GerID, ItemPos, ItemUID, ItemGerID) of
		{true, UpItem, BagEquipList2, EquipList} ->
			do_up_equip(GerID, ItemPos, ItemUID, UpItem, BagEquipList2, EquipList, ItemGerID);
		{false, Reason} ->
			?sendself(#sc_item_up_equip{result=Reason, gerID=GerID,itemPos=ItemPos,itemUID=ItemUID})
	end.

cs_item_up_all_equip(#cs_item_up_all_equip{gerID=GerID}) ->
    case check_up_all_equip(GerID) of
        {true, NeedEquipList} ->
            lists:foreach(fun({ItemPos, ItemUID}) ->
                                  cs_item_up_equip(#cs_item_up_equip{gerID=GerID,itemPos=ItemPos, itemUID=ItemUID, itemGerID=0})
                          end, NeedEquipList),
            ?sendself(#sc_item_up_all_equip{result=0});
        {false, Reason} ->
            ?sendself(#sc_item_up_all_equip{result=Reason})
    end.

%% @doc 功能道具
cs_item_use(#cs_item_use{itemNum=ItemNum,itemUID=ItemUID}) ->
	case check_use_item(ItemNum, ItemUID) of
		{true, #item{itemTypeID=ItemTypeID,itemNum=CurNum}=Item, BagOther}->
			#data_item{itemEffect=EffectFun, itemEffectArg=EffectArg} = data_item:get(ItemTypeID),
			case catch item_effect:EffectFun(EffectArg,ItemNum) of
				{ok, UseNum} when UseNum < CurNum->
					BagOther2 = [Item#item{itemNum=CurNum-UseNum}|BagOther],
					role_data:set_bagItem(BagOther2),
                    ?sendself(#sc_item_update{updateList=[#p_item_num_update{itemUID=ItemUID,itemNum=CurNum-UseNum}]}),
					?sendself(#sc_item_use{itemNum=UseNum,itemUID=ItemUID,result=1});
				{ok, UseNum} ->
					BagOther2 = BagOther,
					role_data:set_bagItem(BagOther2),
                    ?sendself(#sc_item_delete_notify{itemUIDList=[ItemUID]}),
					?sendself(#sc_item_use{itemNum=UseNum,itemUID=ItemUID,result=1});
                {false, Reason} ->
                    ?sendself(#sc_item_use{itemNum=ItemNum,itemUID=ItemUID,result=Reason});
				Error ->
					throw({item_effect_error, Error})
			end;
		{false, Reason} ->
			?sendself(#sc_item_use{itemNum=ItemNum,itemUID=ItemUID,result=Reason})
	end.

%% @doc 装备强化
cs_item_reinforce(#cs_item_reinforce{gerID=GerID,itemUID=ItemUID}) ->
	case check_reinforce(ItemUID,GerID) of
		{true, NeedCoin, Item, EquipList2, Role, DataItem} ->
			do_reinforce(ItemUID, GerID, NeedCoin, Item, EquipList2, Role, DataItem);
		{false,Reason} ->
			?sendself(#sc_item_reinforce{itemUID=ItemUID,result=Reason,newLevel=0})
	end.

%% @doc 装备一键强化
cs_item_max_reinforce(#cs_item_max_reinforce{gerID=GerID, itemUID=ItemUID}) ->
	%% 此处先判断是否能进行一次强化
	case check_reinforce(ItemUID, GerID) of
		{true, NeedCoin, Item, EquipList2, Role, DataItem} ->
			do_max_reinforce(ItemUID, GerID, NeedCoin, Item, EquipList2, Role, DataItem);
		{false, Reason} ->
			?sendself(#sc_item_max_reinforce{itemUID=ItemUID,result=Reason,tempLevelList=[]})
	end.

%% @doc 装备升品
cs_item_up_rank(#cs_item_up_rank{srcItemGerID=SrcItemGerID,foodItemGerID=FoodItemGerID,foodItemUID=FoodItemUID,srcItemUID=SrcItemUID})->
	case check_up_rank(SrcItemUID, SrcItemGerID, FoodItemUID, FoodItemGerID) of
		{true, SrcItem, SrcList, FoodItem, FoodList2, DataItem, NeedCoin, NewRank, Role} ->
			%% 扣银两
			#role{roleName=RoleName,level=_RoleLevel,roleID=RoleID} = _Role2 = role_lib:deduct_coin_f(Role, NeedCoin, ?MONEY_DEC_TYPE_ITEM_UP_RANK, 0, ""),
			%% 计算新道具
			NowSec = timer_wheel:nowsec(),
			#item{itemLevel=ItemLevel, itemDecay=ItemDecay, itemTypeID=ItemTypeID, itemRank=CurItemRank} = SrcItem,
			%% 合并强化等级
			NewLevel = erlang:max(ItemLevel, FoodItem#item.itemLevel),
			#data_item{isDecay=IsDecay}=DataItem,
			ItemDecay2 = item_lib:item_decay(item_lib:next_decay_sec(IsDecay, NewRank, NowSec), SrcItemUID),
			SrcItem2 = refresh_item(SrcItem#item{itemRank=NewRank,itemDecay=ItemDecay2, itemLevel=NewLevel}),
			item_lib:cancel_decay(ItemDecay),
			item_lib:cancel_decay(FoodItem#item.itemDecay),
			if FoodItemGerID =:= SrcItemGerID ->
				   set_itemList(SrcItemGerID, [SrcItem2|FoodList2]);
			   true ->
				   set_itemList(SrcItemGerID, [SrcItem2|SrcList]),
				   set_itemList(FoodItemGerID,FoodList2)
			end,
			if SrcItemGerID =/= 0 ->
				   ger_attr:recacl_f(SrcItemGerID);
			   FoodItemGerID =/=0 andalso FoodItemGerID =/= SrcItemGerID ->
				   ger_attr:recacl_f(FoodItemGerID);
			   true ->
				   ignore
			end,			
			LogGerList = [[FoodItem#item.itemUID,FoodItem#item.itemTypeID,FoodItem#item.itemLevel,FoodItem#item.itemRank]],
			{Date, _} = Time = erlang:localtime(),
			%% 写道具消耗日志，和装备升品日志
			behavior_item_consume:log(RoleID, LogGerList, Date, Time, ?MONEY_DEC_TYPE_ITEM_UP_RANK, 0, integer_to_list(SrcItemUID)),
			behavior_item_uprank:log(RoleID, SrcItemUID, ItemTypeID, ItemLevel, NewLevel, CurItemRank, NewRank, FoodItemUID, Date, Time),
			?CATCH(role_task_trigger:handle({dispach_task,equip_up_quality,RoleID,SrcItemUID,ItemTypeID,NewRank})),
			?sendself(#sc_item_up_rank{foodItemUID=FoodItemUID,result=1,srcItemUID=SrcItemUID,newItemLevel=NewLevel,newItemRank=NewRank}),
			?sendself(#sc_item_update_rank{itemUID=SrcItemUID,newItemDecay=item_lib:itemDecay(ItemDecay2),newItemRank=NewRank}),
            case data_item:get(SrcItem2#item.itemTypeID) of
                #data_item{itemStar=ItemStarLevel} when ItemStarLevel >= 5 ->
                    broadcast_server:bc(#sc_message_item_uprank{itemInfo=item_lib:item2p_item_view(SrcItem2),roleName=RoleName});
                _ ->
                    ok
            end;
		{false, Reason} ->
			?sendself(#sc_item_up_rank{foodItemUID=FoodItemUID,result=Reason,srcItemUID=SrcItemUID,newItemLevel=0,newItemRank=0})
	end.


cs_item_compound(#cs_item_compound{typeID=ItemTypeID}) ->
    case check_compound(ItemTypeID) of
        {true, BagOther2, Product, _NeedNum, DelAcc, UpdateAcc, UpdateLogList} ->
            role_data:set_bagItem(BagOther2),
            RoleID = role_data:get_roleID(),
            %% 写道具日志
            LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogList),
            {Date, _} = Time = erlang:localtime(),
            behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_ITEM_COMPOUND, ItemTypeID, ""),
            
            if is_record(Product, new_item) ->
                   item_lib:add_item_f(Product, ?MONEY_ADD_TYPE_ITEM_COMPOUND, 0, "");
               true ->
                   ger_lib:add_ger(Product, ?MONEY_ADD_TYPE_ITEM_COMPOUND, 0, "")
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
            ?sendself(#sc_item_compound{typeID=ItemTypeID, result=1});
        {false, Reason} ->
            ?sendself(#sc_item_compound{typeID=ItemTypeID, result=Reason})
    end.

%宝物升品
cs_item_eat(#cs_item_eat{itemID=ItemID, itemGerID = ItemGerID, foodItemIDList = FoodItemIDList}) ->
	case check_treasure_eat(ItemID,ItemGerID, FoodItemIDList) of
		{true, SrcItem, SrcList, ItemList, ItemBag3}->
			%%计算新道具
			#item{itemExp = SrcItemExp,itemTypeID=ItemTypeID} = SrcItem,
			DataItem = data_item:get(ItemTypeID),
			ExpWillBe = lists:foldl(fun(E, Acc) -> Acc + item_lib:cacl_treasure_exp(E) end, SrcItemExp, ItemList),
			if DataItem#data_item.itemStar =:= 5 ->
				   ExpFix = erlang:min(ExpWillBe, max_treasure_exp2()),
				   NewRank = data_treasure_exp2:get(ExpFix);
			   true ->
				   ExpFix = erlang:min(ExpWillBe,max_treasure_exp()),
				   NewRank = data_treasure_exp:get(ExpFix)			   
			end,
			SrcItemNew = refresh_item(SrcItem#item{itemExp = ExpFix, itemRank = NewRank}),
			%%回写
			if ItemGerID =:= 0 ->
				   set_itemList(ItemGerID, [SrcItemNew|ItemBag3]);
			   true->
				   set_itemList(ItemGerID, [SrcItemNew|SrcList]),
				   set_itemList(0, ItemBag3),
				   ger_attr:recacl_f(ItemGerID)
			end,
			%% 写道具日志
			LogItemList = role_item:itemList2logItemList(ItemList, []),
			{Date, _} = Time = erlang:localtime(),
			RoleID = role_data:get_roleID(),
			behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_TREASURE_EAT, ItemTypeID, integer_to_list(ItemID)),
			?sendself(#sc_item_eat{result = 1, itemID = ItemID, itemExp = ExpFix, newItemRank = NewRank});
		{false, Reason} ->
			?sendself(#sc_item_eat{result = Reason, itemID = ItemID, itemExp = 0, newItemRank = 0})
	end.

check_compound(ItemTypeID) ->
	case data_compound:get(ItemTypeID) of
		#data_compound{needNum=NeedNum,product=Product,baseNeedNum=BaseNeedNum,otherList=OtherList} ->
			case item_lib:check_material(ItemTypeID, NeedNum) of
				false ->
					case check_compound_need_material(ItemTypeID, NeedNum, BaseNeedNum, OtherList) of
						false->
							{false, 2};
						{true, BagOther3, DelAcc, UpdateAcc, UpdateLogList} ->
							{true, BagOther3, Product, NeedNum, DelAcc, UpdateAcc, UpdateLogList}
					end;
				{true, BagOther2, DelAcc, UpdateAcc, UpdateLogList} ->
					{true, BagOther2, Product, NeedNum, DelAcc, UpdateAcc, UpdateLogList}
			end;			
		_ ->
			{false, 3}
	end.

check_compound_need_material(ItemTypeID, NeedNum, BaseNeedNum, OtherList) ->
	BagOther = role_data:get_bagItem(),
	case item_lib:check_material2(BagOther, ItemTypeID, BaseNeedNum) of
		{_, 0, _, _, _} ->
			{BagOther2, RestNum1, DelAcc, UpdateAcc, UpdateLogList} =
				item_lib:check_material2(BagOther, ItemTypeID, NeedNum),
			{BagOtherT, RestNumT, DelAccT, UpdateAccT, UpdateLogListT} = 
				lists:foldl(fun(_ItemTypeIDM, {BagOtherAcc, 0,DelAccAcc,UpdateAccAcc,UpdateLogAccAcc})->
									{BagOtherAcc, NeedNum, DelAccAcc,UpdateAccAcc,UpdateLogAccAcc};
							   (ItemTypeIDM, {BagOtherAcc, RestNumAcc,DelAccAcc,UpdateAccAcc,UpdateLogAccAcc})->
									{BagOtherAccT, RestNumAccT, DelAccAccT, UpdateAccAccT, UpdateLogAccAccT} = 
									item_lib:check_material2(BagOtherAcc, ItemTypeIDM, RestNumAcc),
									{BagOtherAccT, RestNumAccT, DelAccAccT ++ DelAccAcc, UpdateAccAccT ++ UpdateAccAcc, UpdateLogAccAccT ++ UpdateLogAccAcc}
							end, {BagOther2, RestNum1, DelAcc, UpdateAcc, UpdateLogList}, OtherList),
			if RestNumT =:= 0 ->
				   {true ,BagOtherT, DelAccT, UpdateAccT, UpdateLogListT};
			   true ->
				   false
			end;
		_ ->
			false
	end.


take_item(0=_ItemGerID, ItemUID) ->
	BagEquip = role_data:get_bagEquip(),
	lists:keytake(ItemUID, #item.itemUID, BagEquip);
take_item(ItemGerID, ItemUID) ->
	GerEquip = role_data:get_equip(ItemGerID),
	lists:keytake(ItemUID, #item.itemUID, GerEquip).

get_itemList(0) ->
	role_data:get_bagEquip();
get_itemList(ItemGerID) ->
	role_data:get_equip(ItemGerID).
set_itemList(0=_ItemGerID, List) ->
	role_data:set_bagEquip(List);
set_itemList(ItemGerID, List) ->
	role_data:set_equip(ItemGerID, List).

%% 计算升品需要的银两
cacl_up_rank_coin(NewQuality, Quality1, Quality2, GerStar) ->
	get_up_rank_coin(NewQuality, GerStar) - get_up_rank_coin(Quality1, GerStar) - get_up_rank_coin(Quality2, GerStar).

get_up_rank_coin(0, _GerStar) ->
	0;
get_up_rank_coin(Quality, GerStar) ->
	data_item_up_rank:get({GerStar, Quality}).

check_up_rank(SrcItemUID, SrcItemGerID, FoodItemUID, FoodItemGerID) ->
    case take_item(SrcItemGerID, SrcItemUID) of
        false ->
            {false, 3};
        {value, SrcItem, SrcList} ->
            #item{itemRank=ItemRank,itemTypeID=SrcItemTypeID} = SrcItem,
            case item_lib:is_equip(SrcItem) of
                false ->
                    {false, 7};
                true ->
                    #data_item{itemMaxRank=MaxRank} = DataItemSrc = data_item:get(SrcItemTypeID),
                    if ItemRank >= MaxRank ->							
                           {false, 2};
                       true ->
                           if SrcItemGerID =:= FoodItemGerID ->
                                  FoodList = SrcList;
                              true ->
                                  FoodList = get_itemList(FoodItemGerID)
                           end,
                           case lists:keytake(FoodItemUID, #item.itemUID, FoodList) of
                               false ->
                                   {false, 4};
                               {value, FoodItem, FoodList2} ->
                                   #item{itemTypeID=FoodItemTypeID,itemRank=FoodRank} = FoodItem,
                                   #data_item{itemType=SrcItemType, itemStar=SrcItemStar} = DataItemSrc,
                                   #data_item{itemType=FoodItemType, itemStar=FoodItemStar} = data_item:get(FoodItemTypeID),
                                   if SrcItemType =/= FoodItemType ->
                                          {false, 5};
                                      SrcItemStar =/= FoodItemStar ->
                                          {false, 8};
                                      true ->
                                          NewRank = erlang:min(FoodRank+ItemRank+1, MaxRank),
                                          NeedCoin = cacl_up_rank_coin(NewRank, FoodRank, ItemRank, SrcItemStar),
                                          Role = role_data:get_roleInfo(),
                                          if Role#role.coin >= NeedCoin ->
                                                 {true, SrcItem, SrcList, FoodItem, FoodList2, DataItemSrc, NeedCoin, NewRank, Role};
                                             true ->
                                                 {false, 6}
                                          end
                                   end
                           end
                    end
            end
    end.


max_treasure_exp() ->
	data_treasure_rank:get(?MAX_RANK_OF_TREASURE).

max_treasure_exp2() ->
	data_treasure_rank2:get(?MAX_RANK_OF_TREASURE).


check_treasure_eat(SrcItemUID, SrcItemGerID, FoodItemIDList) ->
	case length(FoodItemIDList) > 4 of
		true ->
			{false, 3};
		false->
			case take_item(SrcItemGerID, SrcItemUID) of
				false->
					{false, 5};
				{value, SrcItem, SrcList} ->
					#item{itemType=ItemType,itemExp=SrcItemExp} = SrcItem,
					case item_lib:is_treasure(ItemType) of
						false->
							{false, 2};
						true->
							DataItemSrc = data_item:get(ItemType),
							MaxExp = 
								if DataItemSrc#data_item.itemStar =:= 4 ->
									   max_treasure_exp();
								   true ->
									   max_treasure_exp2()
								end,
							if SrcItemExp >= MaxExp ->
								   {false, 4};
							   true->
								   if SrcItemGerID =:= 0 ->
										  FoodList = SrcList;
									  true->
										  FoodList = get_itemList(0)
								   end,
								   case util:foldl(fun(E, {ItemBagAcc,FoodAcc,JudgeAcc}) -> 
														   case lists:keytake(E, #item.itemUID, ItemBagAcc) of
															   false ->
																   {return, false};
															   {value,ItemT,ItemBagAcc2} ->
																   DataItem = data_item:get(ItemT#item.itemTypeID),
																   if DataItem#data_item.itemStar =:= 5 ->
																		  {ItemBagAcc2, [ItemT|FoodAcc], false};
																	  true ->
																		  {ItemBagAcc2, [ItemT|FoodAcc],true andalso JudgeAcc}
																   end
														   end
												   end, {FoodList,[], true}, FoodItemIDList) of
									   false ->
										   {false, 6};
									   {ItemBag3, ItemList, true} ->
										   {true, SrcItem, SrcList, ItemList, ItemBag3};
									   {_ItemBag3, _ItemList, false} ->
										   {false, 7}
								   end
							end
					end
			end
	end.		


do_max_reinforce(ItemUID, GerID, NeedCoin, Item, EquipList2, Role, DataItem) ->
	#item{itemLevel=ItemLevel,
		  itemType=ItemType ,
		  itemTypeID=ItemTypeID
		 % itemRank=ItemRank
		 } = Item,
	#data_item{itemStar=ItemStar} = DataItem,
	#role{vipLevel=VipLevel, coin=Coin, level=RoleLevel, roleID=RoleID} = Role,
	Coin2 = Coin - NeedCoin,
	AddLevel = random_add_level(VipLevel),
	ItemLevel2 = ItemLevel+AddLevel,
	{NewCoin, ItemNewLevel, TempList,Num} = max_reinforce(Coin2, VipLevel, ItemLevel2, ItemType, ItemStar, [ItemLevel2], RoleLevel,1),
	DeductCoin = Coin-NewCoin,
	role_lib:deduct_coin_f(Role, DeductCoin, ?MONEY_DEC_TYPE_ITEM_MAX_REINFORCE, 0, ""),
	NewItem2 = refresh_item(Item#item{itemLevel=ItemNewLevel}),
	EquipList3 = [NewItem2|EquipList2],
	if GerID =:= 0 ->
		   role_data:set_bagEquip(EquipList3);
	   true ->
		   role_data:set_equip(GerID, EquipList3),
		   ger_attr:recacl_f(GerID)
	end,
	
	%% 写道具日志
	{Date, _} = Time = erlang:localtime(),
	behavior_item_uplevel:log(RoleID, ItemUID, ItemTypeID, ItemNewLevel-ItemLevel, ItemNewLevel, length(TempList), DeductCoin, Date, Time),
	?CATCH(role_task_trigger:handle({dispach_task,equip_strong,RoleID,ItemUID,ItemTypeID,ItemNewLevel,Num})),
	?sendself(#sc_item_max_reinforce{itemUID=ItemUID,result=1,tempLevelList=TempList}).


max_reinforce(Coin, VipLevel, ItemLevel, ItemType, ItemStar, List, RoleLevel,Num) ->
	if ItemLevel >= RoleLevel ->
		   {Coin, ItemLevel, lists:reverse(List),Num};
	   true ->
		   NeedCoin = cacl_reinforce_coin(ItemType, ItemLevel, ItemStar),
		   if NeedCoin > Coin ->
				  {Coin, ItemLevel, lists:reverse(List),Num};
			  true ->
				  AddLevel = random_add_level(VipLevel),
				  NewLevel = ItemLevel+AddLevel,
				  max_reinforce(Coin-NeedCoin, VipLevel, NewLevel, ItemType, ItemStar, [NewLevel|List], RoleLevel,Num+1)
		   end
	end.



do_reinforce(ItemUID, GerID, NeedCoin, Item, EquipList2, Role, _DataItem) ->
	#item{itemLevel=ItemLevel,itemTypeID=ItemTypeID} = Item,
	%#data_item{addAttr=AddAttr,itemStar=ItemStar} = DataItem,
	#role{vipLevel=VipLevel,roleID=RoleID} = Role,
	role_lib:deduct_coin_f(Role, NeedCoin, ?MONEY_DEC_TYPE_ITEM_REINFORECE, 0, ""),
	AddLevel = random_add_level(VipLevel),
	ItemNewLevel = ItemLevel + AddLevel,
	NewItem2 = refresh_item(Item#item{itemLevel=ItemNewLevel}),
	EquipList3 = [NewItem2|EquipList2],
	if GerID =:= 0 ->
		   role_data:set_bagEquip(EquipList3);
	   true ->
		   role_data:set_equip(GerID, EquipList3),
		   ger_attr:recacl_f(GerID)
	end,
	%% 写道具日志
	{Date, _} = Time = erlang:localtime(),
	behavior_item_uplevel:log(RoleID, ItemUID, ItemTypeID, ItemNewLevel-ItemLevel, ItemNewLevel, 1, NeedCoin, Date, Time),
	?CATCH(role_task_trigger:handle({dispach_task,equip_strong,RoleID,ItemUID,ItemTypeID,ItemNewLevel,1})),
	?sendself(#sc_item_reinforce{itemUID=ItemUID,result=1,newLevel=ItemNewLevel}).

random_add_level(VipLevel) ->
	ProbList = data_reinforce:get(prob),
	RandomConfig = 
		util:foldl(fun({L,C}, Acc) ->
						   if VipLevel >= L ->
								  {return,C};
							  true ->
								  Acc
						   end
				   end, [1,0,0,0,0], ProbList),
	Total = lists:sum(RandomConfig),
	RandomValue = Total*random:uniform(),
	util:foldl(fun(E,{Acc,Num}) ->
					   NewAcc = Acc+E,
					   if NewAcc >= RandomValue ->
							  {return,Num+1};
						  true ->
							  {Acc+E, Num+1}
					   end
			   end, {0, 0}, RandomConfig).

check_reinforce(ItemUID, GerID) ->
	if GerID =:= 0 ->
		   EquipList = role_data:get_bagEquip();
	   true ->
		   EquipList = role_data:get_equip(GerID)
	end,
	%% 判断装备是否存在
	case lists:keytake(ItemUID, #item.itemUID, EquipList) of
		false ->
			{false, 4};
		{value, Item, EquipList2}->
			#role{level=RoleLevel, coin=Coin} = Role = role_data:get_roleInfo(),
			#item{itemLevel=ItemLevel, itemTypeID=ItemTypeID,itemType=ItemType} = Item,
			if ItemType =:= ?weapon orelse ItemType =:= ?armor orelse ItemType =:= ?wing orelse
				   ItemType =:= ?headwear orelse ItemType =:= ?totem orelse ItemType =:= ?runestone->	
				   %% 判断等级是否超过
				   case ItemLevel >= RoleLevel of
					   true ->
						   {false, 3};
					   false->
						   %% 判断道具配置是否存在
						   case data_item:get(ItemTypeID) of
							   #data_item{itemStar=ItemStar}=DataItem ->
								   NeedCoin = cacl_reinforce_coin(ItemType, ItemLevel, ItemStar),
								   %% 判断银两是否足够
								   case Coin >= NeedCoin of
									   false ->
										   {false, 2};
									   true ->
										   {true, NeedCoin, Item, EquipList2, Role, DataItem}
								   end;
							   _ ->
								   {false, 5}
						   end
				   end;
			   true ->
				   {false, 6}
			end
	end.

%% 计算强化需要的银两
cacl_reinforce_coin(?weapon,ItemLevel, ItemStar) ->
	trunc(data_reinforce:get(weapon_coin_ratio)
			  *data_reinforce:get({star_coin_ratio,ItemStar})
			  *data_reinforce:get({base_coin, ItemLevel}));
cacl_reinforce_coin(?armor,ItemLevel, ItemStar) ->
	trunc(data_reinforce:get(armor_coin_ratio)
			  *data_reinforce:get({star_coin_ratio,ItemStar})
			  *data_reinforce:get({base_coin, ItemLevel}));
cacl_reinforce_coin(?wing,ItemLevel, ItemStar) ->
	trunc(data_reinforce:get(wing_coin_ratio)
			  *data_reinforce:get({star_coin_ratio,ItemStar})
			  *data_reinforce:get({base_coin, ItemLevel}));
cacl_reinforce_coin(?headwear,ItemLevel, ItemStar) ->
	trunc(data_reinforce:get(headwear_coin_ratio)
			  *data_reinforce:get({star_coin_ratio,ItemStar})
			  *data_reinforce:get({base_coin, ItemLevel}));
cacl_reinforce_coin(?totem,ItemLevel, ItemStar) ->
	trunc(data_reinforce:get(totem_coin_ratio)
			  *data_reinforce:get({star_coin_ratio,ItemStar})
			  *data_reinforce:get({base_coin, ItemLevel}));
cacl_reinforce_coin(?runestone,ItemLevel, ItemStar) ->
	trunc(data_reinforce:get(runestone_coin_ratio)
			  *data_reinforce:get({star_coin_ratio,ItemStar})
			  *data_reinforce:get({base_coin, ItemLevel})).


check_use_item(ItemNum,ItemUID) ->
	BagOther = role_data:get_bagItem(),
	case lists:keytake(ItemUID, #item.itemUID, BagOther) of
		false ->
			{false,2};
		{value, Item, BagOther2} ->
			if Item#item.itemNum >= ItemNum ->
				   {true ,Item, BagOther2};
			   true ->
				   {false, 2}
			end
	end.






do_up_equip(GerID, ItemPos, ItemUID, UpItem, SrcEquipList, EquipList, ItemGerID) ->
	UpItem2 = item_lib:recacl(UpItem#item{itemPos=ItemPos}),
	if ItemGerID == 0 ->
		   BagEquipList2 = SrcEquipList;
	   true ->
		   BagEquipList2 = role_data:get_bagEquip()
	end,
	case lists:keytake(ItemPos, #item.itemPos, EquipList) of
		false ->
			EquipList2=EquipList,
			BagEquipList3=BagEquipList2;
		{value, DownItem, EquipList2} ->
			DownItem2 = DownItem#item{itemPos=0,addAttr=0},
			BagEquipList3=[DownItem2|BagEquipList2]
	end,
	case lists:keytake(UpItem#item.itemType, #item.itemType, EquipList2) of
		false ->
			EquipList3 = EquipList2,
			BagEquipList4 = BagEquipList3;
		{value, AnotherDownItem, EquipList3}->
			AnotherDownItem2 = AnotherDownItem#item{itemPos=0,addAttr=0},
			BagEquipList4 = [AnotherDownItem2|BagEquipList3]
	end,
	role_data:set_bagEquip(BagEquipList4),
	NewEquipList = [UpItem2|EquipList3],
	role_data:set_equip(GerID, NewEquipList),
	if ItemGerID == 0 ->
		   ignore;
	   true ->
		   role_data:set_equip(ItemGerID, SrcEquipList),
		   ger_attr:recacl_f(ItemGerID)
	end,
	?CATCH(role_task_trigger:handle({dispach_task,equip_up_equip,ItemUID,UpItem#item.itemTypeID})),
	ger_attr:recacl_f(GerID),
	?sendself(#sc_item_up_equip{gerID=GerID,itemPos=ItemPos,itemUID=ItemUID,result=1}).

check_up_equip(GerID, ItemPos, ItemUID, ItemGerID) ->
	case role_data:get_ger(GerID) of
		false ->
			{false, 7};
		{_,_,_,_,_,bag} ->
			{false, 7};
		_ ->
			if ItemGerID == GerID ->
				   {false, 5};
			   true ->
				   EquipList = role_data:get_equip(GerID),
				   case take_item(ItemGerID, ItemUID) of
					   false ->
						   {false, 3};
					   {value, UpItem, BagEquipList2}->
						   ItemType = UpItem#item.itemType,
						   case item_lib:is_itemType_equip(ItemType) of
							   false ->
								   {false, 4};
							   true ->
								   case check_pos(ItemType, ItemPos) of
									   false ->
										   {false, 2};
									   true ->
										   case check_treasure_type(ItemType, EquipList) of
											   false ->
												   {true, UpItem, BagEquipList2, EquipList};
											   true ->
												   {false, 6}
										   end
								   end
						   end
				   end
			end
	end.

check_up_all_equip(GerID) ->
    case role_data:get_ger(GerID) of
        {value, _, _, _, _, ger} ->
            EquipList = role_data:get_equip(GerID),
            BagEquipList = role_data:get_bagEquip(),
            case erlang:length(EquipList) =:= erlang:length(?equip_type_list) of
                true ->
                    {false, 2};
                false ->
                    EquipedTypeList = [ItemType||#item{itemType=ItemType}<-EquipList],
                    NeedEquipList =
                        lists:foldr(fun(NeedEquipType, AccNeedEquipList) ->
                                            case get_need_equip(NeedEquipType, BagEquipList) of
                                                0 ->
                                                    AccNeedEquipList;
                                                ItemUID ->
                                                    [{get_pos(NeedEquipType), ItemUID}|AccNeedEquipList]
                                            end
                                    end, [], ?equip_type_list -- EquipedTypeList),
                    case NeedEquipList of
                        [] ->
                            {false, 1};
                        _ ->
                            {true, NeedEquipList}
                    end
            end;
        _ ->
            {false, 3}
    end.

get_need_equip(NeedEquipType, BagEquipList) ->
    NeedEquipTypeBagList = [BagEquip||BagEquip<-BagEquipList, BagEquip#item.itemType =:= NeedEquipType],
    case NeedEquipTypeBagList of
        [] ->
            0;
        _ ->
            [#item{itemUID=ItemUID}|_] = sort_equip_for_all_equip(NeedEquipTypeBagList),
            ItemUID
    end.

sort_equip_for_all_equip(NeedEquipTypeBagList) ->
    lists:sort(
      fun(#item{itemTypeID=ItemTypeID1,itemRank=ItemRank1,itemLevel=ItemLevel1},
          #item{itemTypeID=ItemTypeID2,itemRank=ItemRank2,itemLevel=ItemLevel2}) ->
              #data_item{itemStar=ItemStar1} = data_item:get(ItemTypeID1),
              #data_item{itemStar=ItemStar2} = data_item:get(ItemTypeID2),
              if
                  ItemStar1 > ItemStar2 ->
                      true;
                  ItemStar1 =:= ItemStar2 ->
                      if
                          ItemRank1 > ItemRank2 ->
                              true;
                          ItemRank1 =:= ItemRank2 ->
                              if
                                  ItemLevel1 > ItemLevel2 ->
                                      true;
                                  true ->
                                      false
                              end;
                          true ->
                              false
                      end;
                  true ->
                      false
              end
      end, NeedEquipTypeBagList).

%% 取装备规则列表,若该装备在玩家的装备规则列表中,则装备动作返回失败,否则进入穿装备的流程
check_treasure_type(ItemType, EquipList) ->
	lists:foldl(fun(E,Acc)->
						check_treasure_rule(E#item.itemTypeID, ItemType) orelse Acc
				end, false, EquipList).

check_treasure_rule(ItemTypeID, ItemTypeID2)->
	RuleList = 
		case data_trea_equip_rule:get(ItemTypeID) of
			?undefined ->
				[];
			X ->
				X
		end,
	lists:member(ItemTypeID2, RuleList).

check_pos(?weapon, Pos) ->
	Pos =:= ?ITEM_POS_WEAPON;
check_pos(?headwear, Pos) ->
	Pos =:= ?ITEM_POS_HEADWEAR;
check_pos(?armor, Pos) ->
	Pos =:= ?ITEM_POS_ARMOR;
check_pos(?wing, Pos) ->
	Pos =:= ?ITEM_POS_WING;
check_pos(?runestone, Pos) ->
	Pos =:= ?ITEM_POS_RUNESTONE;
check_pos(?totem, Pos) ->
	Pos =:= ?ITEM_POS_TOTEM;
check_pos(_, _) ->
	false.

get_pos(?weapon)->
	?ITEM_POS_WEAPON;
get_pos(?headwear)->
	?ITEM_POS_HEADWEAR;
get_pos(?armor)->
	?ITEM_POS_ARMOR;
get_pos(?wing)->
	?ITEM_POS_WING;
get_pos(?runestone)->
	?ITEM_POS_RUNESTONE;
get_pos(?totem)->
	?ITEM_POS_TOTEM;
get_pos(_)->
	0.


do_down_equip(GerID, ItemPos, Item, EquipList2) ->
	Item2 = Item#item{itemPos=0,addAttr=0},
	role_data:set_equip(GerID, EquipList2),
	BagEquip = role_data:get_bagEquip(),
	role_data:set_bagEquip([Item2|BagEquip]),
	Reply = #sc_item_down_equip{gerID=GerID, itemPos=ItemPos, result=1},
	ger_attr:recacl_f(GerID),
	?sendself(Reply).


check_down_equip(GerID, ItemPos) ->
	case role_data:get_equip(GerID) of
		[] ->
			{false, 2};
		[_|_]=EquipList ->
			case lists:keytake(ItemPos, #item.itemPos, EquipList) of
				false ->
					{false, 2};
				{value, Item, EquipList2} ->
					{true, Item, EquipList2}
			end
	end.

cacl_coin([Item|DelItemList],Acc) ->
	#item{itemLevel=Level,itemRank=Rank, itemNum=Num,itemTypeID=ItemTypeID}=Item,
	case data_item:get(ItemTypeID) of
		#data_item{itemCost=Cost} ->
			Cost2 = Num * trunc(Cost * (1+ Level*0.1) *(1+ Rank*0.1)),
			cacl_coin(DelItemList, Acc+Cost2);
		_ ->
			cacl_coin(DelItemList, Acc)
	end;
cacl_coin([], Acc) ->
	Acc.

itemList2logItemList(ItemList, UpdateItemLogList) ->
	lists:foldl(fun(Item, Acc) ->
						[[Item#item.itemUID,Item#item.itemTypeID,Item#item.itemNum,Item#item.itemNum]|Acc]
				end, UpdateItemLogList, ItemList).

%% do_sell3(DelItemList, BagEquip, BagOther)->
%% 	role_data:set_bagItem(BagOther),
%% 	role_data:set_bagEquip(BagEquip),
%% 	#role{roleID=RoleID} = Role = role_data:get_roleInfo(),
%% 	LogItemList = role_item:itemList2logItemList(DelItemList, []),
%% 	{Date, _} = Time = erlang:localtime(),
%% 	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_ITEM_SELL, 0, ""),
%% 	{Coin, ItemList} = calc_split(DelItemList),
%% 	role_lib:add_coin_f(Role, Coin, ?MONEY_ADD_TYPE_SELL_ITEM, 0, ""),
%% 	item_lib:add_item_f(ItemList, ?MONEY_ADD_TYPE_SELL_ITEM, 0, ""),
%% 	P_reward_list = [#p_reward_view3{type=1,typeID=TypeID,num=Num}||#new_item{itemTypeID=TypeID, itemNum=Num}<-ItemList],
%% 	?sendself(#sc_item_sell{result=1, reward=[#p_reward_view3{type=1, typeID=21100, num=Coin}]++P_reward_list}).

%% calc_split(DelItemList)->
%% 	lists:foldl(fun(#item{itemTypeID=ItemTypeID, itemRank=ItemRank, itemLevel=ItemLevel}, {CoinAcc, ItemAcc})->
%% 						#data_item{itemCost=SplitID} = data_item:get(ItemTypeID),
%% 						#data_card_split{coin=Coin, itemList=ItemList} = data_card_split:get(SplitID),
%% 						{CoinT, NewItemList} = get_item_split(Coin, ItemList, ItemRank+1, ItemAcc, ItemRank, ItemLevel),
%% 						{CoinAcc + CoinT, NewItemList }
%% 				end, {0, []}, DelItemList).

%% get_item_split(Coin, ItemList, Times, ItemAcc, ItemRank, ItemLevel)->
%% 	CoinT = Coin * (1 + erlang:trunc((ItemRank + ItemLevel - 1) / 10) ),
%% 	ItemList2 = get_item_split2(ItemList, Times, ItemAcc),
%% 	{CoinT, ItemList2}.
%% get_item_split2(_ItemList, 0, A)->
%% 	A;
%% get_item_split2(ItemList, Times, List)->
%% 	List2 = lists:foldl(fun({ItemTypeID, RandList}, ListAcc)->
%% 								Num = util:random_one_from_weigh_list(RandList),
%% 								if Num =:= 0->
%% 									   ListAcc;
%% 								   true->
%% 									   case lists:keytake(ItemTypeID, #new_item.itemTypeID, ListAcc)of
%% 										   false ->
%% 											   [#new_item{itemTypeID=ItemTypeID, itemNum=Num, itemLevel=1, itemRank=0}|ListAcc];
%% 										   {value, NewItem, ListAcc2}->
%% 											   [NewItem#new_item{itemNum=Num + NewItem#new_item.itemNum}|ListAcc2]
%% 									   end
%% 								end
%% 						end, List, ItemList),
%% 	get_item_split2(ItemList, Times-1, List2).

%% 找出出售的道具信息
%% cacl_sell(ItemList, SIDList, DelList) ->
%% 	lists:foldl(fun(Item, {SellIDAcc, DelAcc, EquipAcc}) ->
%% 						#item{itemUID=ItemUID} = Item,
%% 						case lists:member(ItemUID, SellIDAcc) of
%% 							true ->
%% 								{lists:delete(ItemUID, SellIDAcc), [Item|DelAcc],EquipAcc};
%% 							false ->
%% 								{SellIDAcc, DelAcc, [Item|EquipAcc]}
%% 						end
%% 				end, {SIDList, DelList, []}, ItemList).
%% 
%% check_sell([_|_]=ItemIDList) ->
%% 	BagEquip = role_data:get_bagEquip(),
%% 	BagOther = role_data:get_bagItem(),
%% 	{SellIDList, DelItemList, BagEquip2} = cacl_sell(BagEquip, ItemIDList, []),
%% 	case SellIDList =:= [] of
%% 		true ->
%% 			{true, DelItemList, BagEquip2, BagOther};
%% 		false ->
%% 			{SellIDList2, DelItemList2, BagOther2} = 
%% 				cacl_sell(BagOther, SellIDList, DelItemList),
%% 			case SellIDList2 =:= [] of
%% 				true ->
%% 					{true, DelItemList2, BagEquip2, BagOther2};
%% 				false ->
%% 					{false, 2}
%% 			end
%% 	end.

check_sell_item(ItemUIDList)->
	case ItemUIDList of
		[]->
			{false,4};
		_->
			BagItem = role_data:get_bagItem(),
			BagEquip = role_data:get_bagEquip(),
			check_sell_item(ItemUIDList,BagItem,BagEquip,[],[],0)
	end.

check_sell_item([],BagItem,BagEquip,SellItemAcc,SellEquipAcc,SellP)->
	{true,BagItem,BagEquip,SellItemAcc,SellEquipAcc,SellP};
check_sell_item([ItemUID|ItemList],BagItem,BagEquip,SellItemAcc,SellEquipAcc,SellP)->
	case get_item(ItemUID,BagItem,BagEquip) of
		{true,Item,NewBugItem,NewBugEquip}->
			#item{itemType=ItemType,itemTypeID=ItemTypeID,itemNum=ItemNum} = Item,
			SellPrice = ((data_item:get(ItemTypeID))#data_item.itemCost) * ItemNum,
			case SellPrice > 0 of
				true->
					case item_lib:is_itemType_equip(ItemType) of
						true->
							check_sell_item(ItemList,NewBugItem,NewBugEquip,SellItemAcc,[Item|SellEquipAcc],SellP+SellPrice);
						false->
							check_sell_item(ItemList,NewBugItem,NewBugEquip,[Item|SellItemAcc],SellEquipAcc,SellP+SellPrice)
					end;
				false->
					{false,3}
			end;
		{false,Reason}->
			{false,Reason}
	end.

get_item(ItemUID,BagItem,BagEquip)->
	case lists:keytake(ItemUID, #item.itemUID, BagItem) of
		false ->
			case lists:keytake(ItemUID, #item.itemUID, BagEquip) of
				false ->
					{false,2};
				{value, Item, BagEquip2}->
					{true,Item,BagItem,BagEquip2}
			end;
		{value, Item, BagItem2}->
			{true,Item,BagItem2,BagEquip}
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
best_equip() ->
	Equip = [11010,12004, 13004],
	Treasure = [14002, 14003,14004,14005,14006,14010,14012,14014],
	List =
		[hd(item_lib:create_equip(#new_item{itemLevel=104,itemNum=1,itemRank=?MAX_RANK_OF_MAIN_EQUIP,itemTypeID=E}, data_item:get(E)))||E<-Equip] ++
			[hd(item_lib:create_equip(#new_item{itemLevel=104,itemNum=1,itemRank=?MAX_RANK_OF_TREASURE,itemTypeID=E}, data_item:get(E)))||E<-Treasure],
	{_,L}=lists:foldl(fun(E,{Pos, Acc}) ->
							  {Pos+1, [E#item{itemPos=Pos}|Acc]}
					  end, {1, []}, List),
	L.

refresh_item(Item) ->
	#item{itemPos=ItemPos} = Item,
	if ItemPos > 0 ->
		   item_lib:recacl(Item);
	   true ->
		   Item
	end.














