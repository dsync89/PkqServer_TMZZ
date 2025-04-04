%% @author admin
%% @doc 道具接口
%% Created 2013-3-14


-module(item_lib).
-compile(export_all).
-include("def_role.hrl").
-include("def_item.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc 判断装备背包中某种未精炼装备 是否足够
check_equip_num(GerBag, GerTypeID, Num) ->
	check_equip_num(GerBag, GerTypeID, Num, [],[]).

is_equip_material_match(Ger, GerTypeID) ->
	#item{itemLevel=ItemLevel,itemRank=ItemRank,itemTypeID=T} = Ger,
	T == GerTypeID andalso ItemLevel < 10 andalso ItemRank < 1.

check_equip_num(GerBag, _GerTypeID, 0, Result,DelAcc)->
	{true, GerBag++Result, DelAcc};
check_equip_num([], _GerTypeID, _Num, _Result,_DelAcc)->
	false;
check_equip_num([Ger|GerBag], GerTypeID, Num, Result, DelAcc) ->
	case is_equip_material_match(Ger, GerTypeID) of
		true ->
			check_equip_num(GerBag, GerTypeID, Num-1, Result, [Ger|DelAcc]);
		false ->
			check_equip_num(GerBag, GerTypeID, Num, [Ger|Result], DelAcc)
	end.

%% @doc 判断某个材料是否足够
check_material(ItemTypeID, Num) ->
	BagOther = role_data:get_bagItem(),
	case check_material2(BagOther, ItemTypeID, Num) of
		{BagOther2, 0, DelAcc, UpdateAcc, UpdateLogList} ->
			{true, BagOther2, DelAcc, UpdateAcc, UpdateLogList};
		_ ->
			false
	end.

%% return：{BagOther2, RestNum, DelItemList, UpdateItemList, UpdateItemLogList}
check_material2(BagOther, ItemTypeID, Num) ->
	util:foldl(fun(Item, {ItemAcc, NumRest, DelAcc, UpdateAcc, UpdateLogAcc}) ->
						   if NumRest =< 0 ->
								  {[Item|ItemAcc], NumRest, DelAcc, UpdateAcc, UpdateLogAcc};
							  true ->
								  #item{itemTypeID=TypeIDT,itemNum=NumT}=Item,
								  
								  if TypeIDT =:= ItemTypeID ->
										 if NumT > NumRest ->
												NewItem = Item#item{itemNum=NumT-NumRest},
												{[NewItem|ItemAcc], 0, DelAcc, [NewItem|UpdateAcc], [ [NewItem#item.itemUID,TypeIDT,NumRest,NumT] | UpdateLogAcc] };
											NumT =:= NumRest ->
												{ItemAcc, 0, [Item|DelAcc], UpdateAcc, UpdateLogAcc};
											true ->
												{ItemAcc, NumRest -NumT, [Item|DelAcc], UpdateAcc, UpdateLogAcc}
										 end;
									 true ->
										 {[Item|ItemAcc], NumRest, DelAcc,UpdateAcc, UpdateLogAcc}
								  end
						   end
			   end, {[], Num, [], [], []}, BagOther).

%% @doc 初始化宝物成长表配置
data_treasure_value_list2key_value_list(List) ->
	[data_treasure_value2key_value(E) || E<-List]. 

data_treasure_value2key_value(E) ->
	{data_treasure_value,A,B,C}=E,
	{{A,B},C}.

new_itemList2p_item_view(#new_item{}=NewItem) ->
	[new_item2p_item_view(NewItem)];
new_itemList2p_item_view(List) when is_list(List) ->
	[new_item2p_item_view(E)||E<-List];
new_itemList2p_item_view(_) ->
	[].

new_item2p_item_view(NewItem) ->
	#p_item_view{itemTypeID=NewItem#new_item.itemTypeID,
				 itemLevel=NewItem#new_item.itemLevel,
				 itemRank=NewItem#new_item.itemRank,
				 itemNum=NewItem#new_item.itemNum}.

item2p_item_view(Item) ->
	#p_item_view{itemTypeID=Item#item.itemTypeID,
				 itemLevel=Item#item.itemLevel,
				 itemRank=Item#item.itemRank,
				 itemNum=Item#item.itemNum}.

%% itemDecay 是什么,返回给客户端的时候,把衰减设置为0了
item2p_item_view_dtl([ItemUID,ItemTypeID,ItemPos,ItemLevel,ItemRank,ItemGerID,_ItemDecay,ItemExp])->
	#p_equip{itemUID=ItemUID, itemTypeID=ItemTypeID, itemLevel=ItemLevel, itemRank=ItemRank
			 ,itemGerID=ItemGerID, itemPos=ItemPos, itemDecay=0, itemExp=ItemExp};
item2p_item_view_dtl(_)->
	#p_equip{itemUID=0, itemTypeID=0, itemLevel=0, itemRank=0, itemGerID=0,itemPos=0, itemDecay=0, itemExp=0}.

item2p_item(Item) ->
	#p_item{itemLevel=Item#item.itemLevel,
			itemUID=Item#item.itemUID,
			itemTypeID=Item#item.itemTypeID,
			itemRank=Item#item.itemRank,
			itemNum=Item#item.itemNum,
			itemDecay=itemDecay(Item#item.itemDecay),
			itemExp = Item#item.itemExp}.

itemDecay({ItemDecay,_}) ->
	ItemDecay;
itemDecay(_) ->
	0.

item2p_equip(Item, GerID) ->
	#p_equip{itemGerID=GerID,
			 itemUID=Item#item.itemUID,
			 itemTypeID=Item#item.itemTypeID,
			 itemPos=Item#item.itemPos,
			 itemRank=Item#item.itemRank,
			 itemLevel=Item#item.itemLevel,
			 itemDecay=itemDecay(Item#item.itemDecay),
			 itemExp = Item#item.itemExp
			}.


%% 奖励1级0品装备
add_white_item_f(ItemTypeID, Type, ArgID, Desc) ->
	add_item_f(#new_item{itemLevel=1,itemNum=1,itemRank=0,itemTypeID=ItemTypeID}, Type, ArgID, Desc).

merge_update(UpdateAcc2,UpdateAcc) ->
    lists:foldr(fun(#item{itemUID=ItemUID}=Item, Acc) ->
                        case lists:keyfind(ItemUID, #item.itemUID, Acc) of
                            false ->
                                [Item|Acc];
                            _ ->
                                lists:keyreplace(ItemUID, #item.itemUID, Acc, Item)
                        end 
                end, UpdateAcc, UpdateAcc2).

%% @doc 给玩家增加道具
-spec add_item_f(NewItem :: [#new_item{}] | #new_item{}, Type :: ?int16, ArgID :: ?int32, Desc :: ?string) -> ok.
add_item_f(NewItemList, Type, ArgID, Desc) when is_list(NewItemList) ->
	BagEquip = role_data:get_bagEquip(),
	BagOther= role_data:get_bagItem(),
	{BagEquip2, BagOther2, NewList, UpdateList,AddEquipTypeIDList, AddItemTypeIDList, LogList} = 
		lists:foldl(fun(NewItem, {BagEquipAcc, BagOtherAcc, NewAcc, UpdateAcc, AddEquipTypeIDListAcc, AddItemTypeIDListAcc, LogAcc}) ->
							#new_item{itemTypeID=NewItemTypeID} = NewItem,
							#data_item{itemType=ItemType} = DataItem = data_item:get(NewItemTypeID),
							if
							   ItemType =:= ?other 
								   orelse ItemType =:= ?material 
								   orelse ItemType =:= ?soul_general 
								   orelse ItemType =:= ?debris_weapon
                                   orelse ItemType =:= ?debris_armor
								   orelse ItemType =:= ?debris_wing
                                   orelse ItemType =:= ?debris_headwear
                                   orelse ItemType =:= ?debris_totem
                                   orelse ItemType =:= ?debris_runestone
								   orelse ItemType =:= ?debris_horse 
							       orelse ItemType =:= ?box
                                   orelse ItemType =:= ?exp
                                   orelse ItemType =:= ?formula
                                   orelse ItemType =:= ?add_times
								 ->
								   {BagOtherAcc2, NewAcc2, UpdateAcc2, AddLog} = add_other_f(NewItem,BagOtherAcc,DataItem),
								   {BagEquipAcc, BagOtherAcc2, NewAcc2++NewAcc, merge_update(UpdateAcc2,UpdateAcc), AddEquipTypeIDListAcc, add_uelement(NewItemTypeID,AddItemTypeIDListAcc), AddLog ++ LogAcc};
							   true ->
								   {BagEquipAcc2, NewAcc2} = add_equip_f(NewItem,BagEquipAcc,DataItem),
								   AddLogList = [ [E#item.itemUID,NewItemTypeID,E#item.itemNum,0]|| E<-NewAcc2],
								   {BagEquipAcc2, BagOtherAcc, NewAcc2++NewAcc, UpdateAcc,add_uelement(NewItemTypeID,AddEquipTypeIDListAcc), AddItemTypeIDListAcc, AddLogList ++ LogAcc}
							end
					
					end, {BagEquip, BagOther, [], [], [], [], []}, NewItemList),
	RoleID = role_data:get_roleID(),
	%% 日志记录
	if LogList =/= [] ->
		   {Date, _} = Time = erlang:localtime(),
		   behavior_item_add:log(RoleID, LogList, Date, Time, Type, ArgID, Desc);
	   true ->
		   ignore
	end,
	
	
	%% 通知新增道具
	if NewList =/= [] ->
		   role_task_trigger:add_item_trigger(NewList),
		   NewInfoList = [item_lib:item2p_item(E)||E<-NewList],
		   ?sendself(#sc_item_new{newItemList=NewInfoList});
	   true ->
		   ignore
	end,

	%% 通知道具数量更新
	if UpdateList =/= [] ->
	UpdateInfoList = 
	lists:map(fun(Update) ->
					  #p_item_num_update{itemNum=Update#item.itemNum,itemUID=Update#item.itemUID}
			  end, UpdateList),
	?sendself(#sc_item_update{updateList=UpdateInfoList});
	   true ->
		   ignore
	end,

	
	if BagEquip =:= BagEquip2 ->
		   ignore;
	   true ->
		   role_data:set_bagEquip(BagEquip2)
	end,
	if BagOther =:= BagOther2 ->
		   ignore;
	   true ->
		   role_data:set_bagItem(BagOther2)
	end,
	role_gather:hook_add_item_list(AddItemTypeIDList),
    role_gather:hook_add_equip_list(AddEquipTypeIDList);
add_item_f(NewItem, Type, ArgID, Desc) ->
	add_item_f([NewItem], Type, ArgID, Desc).

add_uelement(E,List) ->
	case lists:member(E, List) of
		true ->
			List;
		false ->
			[E|List]
	end.

%% @doc 给玩家增加一个装备
add_equip_f(NewItem, BagEquip, DataItem) ->
	NewEquipInfoList = ?MODULE:create_equip(NewItem, DataItem),
	{NewEquipInfoList ++ BagEquip, NewEquipInfoList}.
	
add_other_f(NewItem, BagOther, DataItem) ->
	#new_item{itemTypeID=ItemTypeID, itemNum=AddNum} = NewItem,
    MaxOverlayNum = get_max_overlay_num(ItemTypeID),
	CheckFun = fun(E) ->
					   E#item.itemTypeID =:= ItemTypeID andalso E#item.itemNum < MaxOverlayNum
							  end,
	case util:fun_take(CheckFun, BagOther) of
		false ->
			ItemInfo = ?MODULE:create_other(NewItem, DataItem),
			{UpdateItem, NewItemList} = split_other(ItemInfo),
			NewItemList2 = [UpdateItem|NewItemList],
			AddLogList = [[E#item.itemUID,ItemTypeID, E#item.itemNum, 0]||E<-NewItemList2],
			{NewItemList2 ++BagOther, NewItemList2, [], AddLogList};
		{value, #item{itemNum=ItemNum,itemUID=ItemUID} = NotFullItem, BagOther2} ->
			{UpdateItem, NewItemList}= split_other(NotFullItem#item{itemNum=ItemNum + AddNum}),
			AddLogList = [[ItemUID,ItemTypeID,AddNum,ItemNum]| [[E#item.itemUID,ItemTypeID, E#item.itemNum, 0]||E<-NewItemList]],
			{[UpdateItem|NewItemList]++BagOther2, NewItemList, [UpdateItem], AddLogList}
	end.

split_other(#item{itemTypeID=ItemTypeID, itemNum=ItemNum}=Item)->
    MaxOverlayNum = get_max_overlay_num(ItemTypeID),
	case ItemNum > MaxOverlayNum of
		true ->
			Num = ItemNum div MaxOverlayNum,
			Tail = ItemNum rem MaxOverlayNum,
			NewList = [Item#item{itemUID=tk_id:gen_itemUID(),itemNum=MaxOverlayNum}||_<-lists:duplicate(Num-1, duplicate)],
			if Tail =:= 0 ->
			NewList2 = NewList; 
			   true ->
				   NewList2 = [Item#item{itemUID=tk_id:gen_itemUID(),itemNum=Tail}|NewList]
			end,
			{Item#item{itemNum=MaxOverlayNum}, NewList2};
		false ->
			{Item, []}
	end.

get_max_overlay_num(ItemTypeID) ->
    #data_item{itemStack=MaxOverlayNum} = data_item:get(ItemTypeID),
    MaxOverlayNum.

%% @doc 创造一个装备
create_equip(NewItem) ->
	create_equip(NewItem, data_item:get(NewItem#new_item.itemTypeID)).

create_equip(NewItem, DataItem) ->
	#new_item{itemTypeID=ItemTypeID,itemNum=ItemNum,itemLevel=ItemLevel,itemRank=ItemRank} = NewItem,
	%% 隐式的判断
	#data_item{itemType= ItemType, itemStar=_ItemStar, addAttr=_AddAttr,isDecay=IsDecay} = DataItem,
	FinalAddAttr =
		0,
		%cacl_attr(ItemType, AddAttr, ItemLevel, ItemRank, ItemStar),
	NowSec = timer_wheel:nowsec(),
	NextDecaySec = next_decay_sec(IsDecay, ItemRank, NowSec),
	ItemTemplate = #item{addAttr=FinalAddAttr,itemLevel=ItemLevel,itemNum=1,itemPos=0,itemRank=ItemRank,itemType=ItemType,itemTypeID=ItemTypeID},
	[begin
		 ItemUID=tk_id:gen_itemUID(),
		 ItemDecay = item_decay(NextDecaySec,ItemUID),
		 case is_treasure(ItemType) of
			false->
				ItemExp = 0;
			true->
				ItemExp = data_treasure_exp:get(ItemRank)
			end,
		 ItemTemplate#item{itemUID=ItemUID, itemDecay=ItemDecay, itemExp=ItemExp}
	 end || _A <- lists:duplicate(ItemNum, duplicate)].

create_equip_with_attr(NewItem, DataItem) ->
	EquipList = create_equip(NewItem, DataItem),
	[recacl(E) || E<-EquipList].

cancel_decay(0) ->
	ignore;
cancel_decay(ItemDecay) ->
	timer_wheel:cancel_plan(ItemDecay).

%% @doc 道具品阶衰减
do_decay(ItemUID, NextDecaySec, NowSec) ->
	BagEquip = role_data:get_bagEquip(),
	% 先从背包中找
	case lists:keytake(ItemUID, #item.itemUID, BagEquip) of
		false ->
			% 再从武将身上找
			do_decay2(ItemUID, NextDecaySec, NowSec);
		{value, Item, BagEquip2} ->
			#item{itemDecay=ItemDecay} = Item,
			case ItemDecay of
				% 判断本次定时器是否匹配
				{NextDecaySec, _} ->
					Item2 = recacl_item_decay(Item, NowSec, Item#item.itemRank-1),
					BagEquip3 = [Item2|BagEquip2],
					role_data:set_bagEquip(BagEquip3),
					notify_decay(Item2);
				_ ->
					ignore
			end
	end.

%% 通知前端品阶更新
notify_decay(Item) ->
	?sendself(#sc_item_update_rank{itemUID=Item#item.itemUID,
								   newItemRank=Item#item.itemRank,
			 newItemDecay=itemDecay(Item#item.itemDecay)}).

%% 计算装备衰减后的新装备
recacl_item_decay(Item, NowSec, ItemRank2) ->
	#item{itemType=ItemType,itemLevel=ItemLevel,itemTypeID=ItemTypeID,itemUID=ItemUID} = Item,
	#data_item{itemStar=ItemStar, addAttr=AddAttr,isDecay=IsDecay}=data_item:get(ItemTypeID),
	Attr2 = cacl_attr(ItemType, AddAttr,ItemLevel, ItemRank2, ItemStar),
	ItemDecay = item_decay(next_decay_sec(IsDecay, ItemRank2, NowSec), ItemUID),
	Item#item{itemRank=ItemRank2, addAttr=Attr2,itemDecay=ItemDecay}.

%% 重新登录时，计算衰减后的新装备
recacl_item_decay2(Item, NextDecaySec, ItemRank2) ->
	#item{itemType=ItemType,itemLevel=ItemLevel,itemTypeID=ItemTypeID,itemUID=ItemUID} = Item,
	#data_item{itemStar=ItemStar, addAttr=AddAttr}=data_item:get(ItemTypeID),
	Attr2 = cacl_attr(ItemType, AddAttr,ItemLevel, ItemRank2, ItemStar),
	ItemDecay = item_decay(NextDecaySec, ItemUID),
	Item#item{itemRank=ItemRank2, addAttr=Attr2,itemDecay=ItemDecay}.

%% 遍历上阵武将 身上的装备
do_decay2(ItemUID, NextDecaySec, NowSec) ->
	PosList = role_data:get_posList(),
	do_decay3(PosList, ItemUID, NextDecaySec, NowSec, PosList).

do_decay3([#ger{gerID=GerID}|PosList], ItemUID, NextDecaySec, NowSec, PosList) ->
	GerEquip = role_data:get_equip(GerID),
	case lists:keytake(ItemUID, #item.itemUID, GerEquip) of
		false ->
			% 找不到则继续遍历
			do_decay3(PosList, ItemUID, NextDecaySec, NowSec, PosList);
		{value, Item, GerEquip2} ->
			% 判断本次定时器是否匹配
			case Item#item.itemDecay of
				{NextDecaySec,_} ->
					Item2 = recacl_item_decay(Item, NowSec, Item#item.itemRank-1),
					GerEquip3 = [Item2|GerEquip2],
					role_data:set_equip(GerID, GerEquip3),
					notify_decay(Item2),
					ger_attr:recacl_f(GerID);
				_ ->
					ignore
			end
	end;
do_decay3([], ItemUID,_,_,_) ->
	?ERR("logic error, cannot find decayed equip,itemUID=~w,roleID=~w",[ItemUID, role_data:get_roleID()]).
	
				
%% @doc 设置衰减timer
item_decay(NextDecaySec, ItemUID) ->
	case NextDecaySec of
		0 ->
			ItemDecay=0;
		NextDecaySec ->
			ItemDecay= timer_wheel:add_plan(NextDecaySec, fun(E) -> do_decay(ItemUID, NextDecaySec, E) end)
	end,
	ItemDecay.

%% @doc 计算道具的下次衰减时间
next_decay_sec(false, _ItemRank, _NowSec) ->
	0;
next_decay_sec(true, ItemRank, NowSec) ->
	case data_item_decay:get(ItemRank) of
		?undefined ->
			0;
		IntervalSec ->
			NowSec + IntervalSec
	end.

%% @doc 创造一个非装备道具
create_other(NewItem, DataItem) ->
	#new_item{itemTypeID=ItemTypeID,itemNum=ItemNum,itemLevel=ItemLevel,itemRank=ItemRank} = NewItem,
	%% 隐式的判断
	#data_item{itemType= ItemType} = DataItem,
	#item{
		  itemLevel=ItemLevel,
		  itemNum=ItemNum,
		  itemPos=0,
		  itemRank=ItemRank,
		  itemType=ItemType,
		  itemTypeID=ItemTypeID,
		  itemUID=tk_id:gen_itemUID(),
		  addAttr=0,
		  itemExp=0}.


recacl(Item) ->
	#item{itemType=ItemType,itemTypeID=ItemTypeID,itemLevel=ItemLevel,itemRank=ItemRank}=Item,
	#data_item{addAttr=AddAttr,itemStar=ItemStar} = data_item:get(ItemTypeID),
	Attr = cacl_attr(ItemType,AddAttr,ItemLevel,ItemRank,ItemStar),
	Item#item{addAttr=Attr}.

cacl_attr(Item, ItemLevel, ItemRank) ->
	#item{itemType=ItemType,itemTypeID=ItemTypeID}=Item,
	#data_item{addAttr=AddAttr,itemStar=ItemStar} = data_item:get(ItemTypeID),
	cacl_attr(ItemType,AddAttr,ItemLevel,ItemRank,ItemStar).

%% 重算装备属性
cacl_attr(?weapon, AddAttr, ItemLevel, ItemRank, _ItemStar) ->
	#add_attr{gerAttack=GerAttack, gerHpMax=GerHpMax} = AddAttr,
	GerAttack2 = erlang:trunc(GerAttack * (1 + (ItemLevel-1)*0.1) * math:pow(4,(ItemRank * 0.1))),
	GerHpMax2  = erlang:trunc(GerHpMax  * (1 + (ItemLevel-1)*0.1) * math:pow(4,(ItemRank * 0.1))),
	AddAttr#add_attr{gerAttack=GerAttack2, gerHpMax=GerHpMax2};
cacl_attr(?armor, AddAttr, ItemLevel, ItemRank, _ItemStar) ->
	#add_attr{gerAttack=GerAttack, gerHpMax=GerHpMax} = AddAttr,
	GerAttack2 = erlang:trunc(GerAttack * (1 + (ItemLevel-1)*0.1) * math:pow(4,(ItemRank * 0.1))),
	GerHpMax2  = erlang:trunc(GerHpMax  * (1 + (ItemLevel-1)*0.1) * math:pow(4,(ItemRank * 0.1))),
	AddAttr#add_attr{gerAttack=GerAttack2, gerHpMax=GerHpMax2};
cacl_attr(?horse, AddAttr, ItemLevel, ItemRank, _ItemStar) ->
	#add_attr{gerAttack=GerAttack, gerHpMax=GerHpMax} = AddAttr,
	GerAttack2 = erlang:trunc(GerAttack * (1 + (ItemLevel-1)*0.1) * math:pow(4,(ItemRank * 0.1))),
	GerHpMax2  = erlang:trunc(GerHpMax  * (1 + (ItemLevel-1)*0.1) * math:pow(4,(ItemRank * 0.1))),
	AddAttr#add_attr{gerAttack=GerAttack2, gerHpMax=GerHpMax2};
cacl_attr(ItemType, AddAttr, ItemLevel, ItemRank, _ItemStar) when ItemType=:=?wing orelse  ItemType=:=?headwear orelse ItemType=:=?totem orelse ItemType=:=?runestone ->
	#add_attr{gerAttack=GerAttack, gerHpMax=GerHpMax} = AddAttr,
	GerAttack2 = erlang:trunc(GerAttack * (1 + (ItemLevel-1)*0.1) * math:pow(4,(ItemRank * 0.1))),
	GerHpMax2  = erlang:trunc(GerHpMax  * (1 + (ItemLevel-1)*0.1) * math:pow(4,(ItemRank * 0.1))),
	AddAttr#add_attr{gerAttack=GerAttack2, gerHpMax=GerHpMax2};
cacl_attr(ItemType, _AddAttr, _ItemLevel, ItemRank, _ItemStar)->
	data_treasure_value:get({ItemType, ItemRank}).
%% cacl_attr(?treasure_absorb=ItemType, _AddAttr, _ItemLevel, ItemRank, ItemStar) ->	
%% 	Value = trunc(data_treasure_value:get({ItemType, ItemRank}) * star_inc(ItemStar)),	
%% 	#add_attr{gerAbsorb=Value};
%% cacl_attr(?treasure_critic=ItemType, _AddAttr, _ItemLevel, ItemRank, ItemStar) ->	
%% 	Value = trunc(data_treasure_value:get({ItemType, ItemRank}) * star_inc(ItemStar)),	
%% 	#add_attr{gerCritic=Value};
%% cacl_attr(?treasure_critic_reduce=ItemType, _AddAttr, _ItemLevel, ItemRank, ItemStar) ->	
%% 	Value = trunc(data_treasure_value:get({ItemType, ItemRank}) * star_inc(ItemStar)),	
%% 	#add_attr{gerCriticReduce=Value};
%% cacl_attr(?treasure_damage_back=ItemType, _AddAttr, _ItemLevel, ItemRank, ItemStar) ->	
%% 	Value = trunc(data_treasure_value:get({ItemType, ItemRank}) * star_inc(ItemStar)),	
%% 	#add_attr{gerDamageBack=Value};
%% cacl_attr(?treasure_doom=ItemType, _AddAttr, _ItemLevel, ItemRank, ItemStar) ->	
%% 	Value = trunc(data_treasure_value:get({ItemType, ItemRank}) * star_inc(ItemStar)),	
%% 	#add_attr{gerDoom=Value};
%% cacl_attr(?treasure_magic_damage_addtion=ItemType, _AddAttr, _ItemLevel, ItemRank, ItemStar) ->	
%% 	Value = trunc(data_treasure_value:get({ItemType, ItemRank}) * star_inc(ItemStar)),	
%% 	#add_attr{gerMagDefBite=Value};
%% cacl_attr(?treasure_magic_damage_reduction=ItemType, _AddAttr, _ItemLevel, ItemRank, ItemStar) ->	
%% 	Value = trunc(data_treasure_value:get({ItemType, ItemRank}) * star_inc(ItemStar)),	
%% 	#add_attr{gerMagDef=Value};
%% cacl_attr(?treasure_miss=ItemType, _AddAttr, _ItemLevel, ItemRank, ItemStar) ->	
%% 	Value = trunc(data_treasure_value:get({ItemType, ItemRank}) * star_inc(ItemStar)),	
%% 	#add_attr{gerMiss=Value};
%% cacl_attr(?treasure_physical_damage_addtion=ItemType, _AddAttr, _ItemLevel, ItemRank, ItemStar) ->	
%% 	Value = trunc(data_treasure_value:get({ItemType, ItemRank}) * star_inc(ItemStar)),	
%% 	#add_attr{gerPhyDefBite=Value};
%% cacl_attr(?treasure_physical_damage_reduction=ItemType, _AddAttr, _ItemLevel, ItemRank, ItemStar) ->	
%% 	Value = trunc(data_treasure_value:get({ItemType, ItemRank}) * star_inc(ItemStar)),	
%% 	#add_attr{gerPhyDef=Value};
%% cacl_attr(?treasure_reel=ItemType, _AddAttr, _ItemLevel, ItemRank, ItemStar) ->	
%% 	%?ERR("recac...~w",[{ItemType, ItemRank,data_treasure_value:get({ItemType, ItemRank})}]),
%% 	Value = trunc(data_treasure_value:get({ItemType, ItemRank}) * star_inc(ItemStar)),	
%% 	#add_attr{gerReel=Value};
%% cacl_attr(?treasure_reel_reduce=ItemType, _AddAttr, _ItemLevel, ItemRank, ItemStar) ->	
%% 	Value = trunc(data_treasure_value:get({ItemType, ItemRank}) * star_inc(ItemStar)),	
%% 	#add_attr{gerReelReduce=Value};
%% cacl_attr(?treasure_sp_init=ItemType, _AddAttr, _ItemLevel, ItemRank, ItemStar) ->	
%% 	Value = trunc(data_treasure_value:get({ItemType, ItemRank}) * star_inc(ItemStar)),	
%% 	#add_attr{gerSpInit=Value};
%% cacl_attr(?treasure_sp_left=ItemType, _AddAttr, _ItemLevel, ItemRank, ItemStar) ->	
%% 	Value = trunc(data_treasure_value:get({ItemType, ItemRank}) * star_inc(ItemStar)),	
%% 	#add_attr{gerSpMax=Value}.

%%计算被吞噬宝物经验
cacl_treasure_exp(Item) ->
	#item{itemExp=ItemExp}=Item,
	ItemExp + 10.

%% @doc 判断一个道具是否是装备
is_equip(Item) when is_record(Item, item) ->
	ItemType = Item#item.itemType,
	is_itemType_equip(ItemType);
is_equip(DataItem) when is_record(DataItem, data_item) ->
	ItemType = DataItem#data_item.itemType,
	is_itemType_equip(ItemType).

is_normal_equip(?weapon                )->true;
is_normal_equip(?horse                 )->true;
is_normal_equip(?armor                 )->true;
is_normal_equip(_)->false.
	
is_itemType_equip(?weapon								  )->true;
is_itemType_equip(?armor								  )->true;
is_itemType_equip(?wing								  )->true;
is_itemType_equip(?headwear								  )->true;
is_itemType_equip(?totem								  )->true;
is_itemType_equip(?runestone								  )->true;
is_itemType_equip(_)->false.

is_main_equip(ItemType) when is_atom(ItemType)->
	ItemType =:= ?weapon orelse ItemType =:= ?horse orelse ItemType =:= ?armor.

is_treasure(?treasure_physical_damage_addtion	) -> true;
is_treasure(?treasure_physical_damage_reduction	) -> true;	
is_treasure(?treasure_magic_damage_addtion		) -> true;	
is_treasure(?treasure_magic_damage_reduction		) -> true;	
is_treasure(?treasure_critic						) -> true;	
is_treasure(?treasure_critic_reduce				) -> true;	
is_treasure(?treasure_doom						) -> true;	
is_treasure(?treasure_miss						) -> true;	
is_treasure(?treasure_sp_init					) -> true;	
is_treasure(?treasure_sp_left					) -> true;	
is_treasure(?treasure_absorb						) -> true;	
is_treasure(?treasure_damage_back				) -> true;	
is_treasure(?treasure_reel						) -> true;	
is_treasure(?treasure_reel_reduce				) -> true;	
is_treasure(?treasure_tianqian					) -> true;
is_treasure(?treasure_dikun						) -> true;
is_treasure(?treasure_leizhen					) -> true;
is_treasure(?treasure_fengxun					) -> true;
is_treasure(?treasure_shuikan					) -> true;
is_treasure(?treasure_huoli						) -> true;
is_treasure(?treasure_shangeng					) -> true;
is_treasure(?treasure_zedui						) -> true;
is_treasure(_	                                ) -> true.
	
star_inc(2) ->
	0.5;
star_inc(3) ->
	0.75;
star_inc(4) ->
	1.

is_patch(DataItem) ->
	DataItem#data_item.itemType =:= ?patch_treasure.
%% 增加
%% ====================================================================
%% Internal functions
%% ====================================================================

test_itemList() ->
	NewItemList=[#new_item{itemLevel=1,itemNum=1,itemRank=1,itemTypeID=ID}||ID<-data_item:get_list()],	
	lists:foldl(fun(NewItem, {Acc1,Acc2, Acc3}) ->
						DataItem = data_item:get(NewItem#new_item.itemTypeID),
						case is_equip(DataItem) of
							true ->
								{create_equip(NewItem, DataItem)++Acc1, Acc2, Acc3};
							false ->
								case is_patch(DataItem) of
									 true ->
										{Acc1,Acc2,[NewItem|Acc3]};
									false ->
								{Acc1, [create_other(NewItem, DataItem)|Acc2], Acc3}
								end
						end
				end, {[],[],[]}, NewItemList).

