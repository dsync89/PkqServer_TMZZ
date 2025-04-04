%% @author admin
%% @doc 卡牌和装备合成系统
%% Created 2014-3-3


-module(role_combine).
-compile(export_all).
-include("def_role.hrl").
-include("def_battle.hrl").
-include("def_item.hrl").
-include("def_reward.hrl").

-define(CombineTypeNormal, 1). 	%%固定合成
-define(CombineTypeRandom, 2).	%%随机合成
-define(CombineGer, 1).		%%合成卡牌
-define(CombineEquip, 2).		%%合成装备
-define(RandomCombineNeedNum, 5).

-define(CombineSucc, 0).		                     %%合成成功
-define(CombineFailBadCombineTypeID, 1).		     %%合成失败，错误的配方ID
-define(CombineFailNotEnough, 2).		             %%合成失败，材料不足
-define(CombineFailBadCombine, 3).		             %%合成失败，合成的材料组合与配方不一致
-define(CombineFailNeedFormulaItem, 4).		         %%合成失败，缺少配方道具
-define(CombineFailCoinNotEnough, 5).                %%合成失败，coin不足
-define(CombineFailRandomCombineNeedNumError, 6).    %%合成失败，合成所需的材料数量不对
-define(CombineFailStarLevelNotSame, 7).             %%合成失败，参与合成的材料星等不一致
-define(CombineFailStarLevelLimit, 8).               %%合成失败，参与合成的材料超过最大星等限制
-define(CombineFailExpGerCanNotCombine, 9).          %%合成失败，经验卡牌不能参与合成
-define(CombineFailHomesteadGerNot, 10).          %%合成失败，家园守护神不能参与合成
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
cs_combine_do(#cs_combine_do{combineType=CombineType,combineTypeID=CombineTypeID,combineOutType=CombineOutType,uIDList=UIDList}) ->
	case catch check_can_combine(CombineType, CombineTypeID, CombineOutType, UIDList) of
		{normal_combine_ger, NewBagGerList, DelGerList, NewItemBagList, UpdateItemBagList, DelItemBagList, NeedCoin, CombineDestTypeID, NewRank, NewLevel, RoleInfo} ->
            do_normal_combine_ger(NewBagGerList, DelGerList, NewItemBagList, UpdateItemBagList, DelItemBagList, NeedCoin, CombineDestTypeID, NewRank, NewLevel, RoleInfo);
        {normal_combine_equip, NewBagEquipList, DelEquipList, NewItemBagList, UpdateItemBagList, DelItemBagList, NeedCoin, CombineDestTypeID, NewRank, NewLevel, RoleInfo} ->
            do_normal_combine_equip(NewBagEquipList, DelEquipList, NewItemBagList, UpdateItemBagList, DelItemBagList, NeedCoin, CombineDestTypeID, NewRank, NewLevel, RoleInfo);
        {random_combine_ger, BoxID, DelGerList, NewGerBagList,NeedCoin, GerStarLevel} ->
            do_random_combine_ger(BoxID, DelGerList, NewGerBagList,NeedCoin, GerStarLevel);
        {random_combine_equip, BoxID, DelEquipList, NewEquipBagList,NeedCoin, EquipStarLevel} ->
            do_random_combine_equip(BoxID, DelEquipList, NewEquipBagList,NeedCoin,EquipStarLevel);
		{false, FailCode} ->
%%             ?ERR("FailCode:~w", [FailCode]),
			?sendself(#sc_combine_fail{result=FailCode})
	end.

do_random_combine_ger(BoxID, DelGerList, NewGerBagList,NeedCoin,GerStarLevel) ->
    role_data:set_gerBag(NewGerBagList),
    {Date, _} = Time = erlang:localtime(),
    {LogGerList,TotalQuality, NewLevel} = lists:foldl(fun(#gerSimple{gerID=GerID, gerTypeID=GerTypeID, gerQuality=GerQuality, gerLevel=GerLevel},{Acc,Sum,AccLevel}) ->
                                   {[[GerID,GerTypeID,GerLevel,GerQuality]|Acc] ,Sum+GerQuality,erlang:max(GerLevel, AccLevel)}  
                           end, {[],0,1},DelGerList),
    behavior_ger_consume:log(role_data:get_roleID(), LogGerList, Date, Time, ?MONEY_DEC_TYPE_COMBINE_RANDOM_GER, 0, ""),
    ?sendself(#sc_ger_del{gerIDList=lists:map(fun(#gerSimple{gerID=GerUID}) -> GerUID end, DelGerList)}),
    %%?ERR("~w", [{BoxID, role_data:get_mainGerTypeID()}]),
    [RConfig|_] =  data_box:get({BoxID, role_data:get_mainGerTypeID()}),
    Crt = get_ger_combine_crt(GerStarLevel),
	NewQuality = TotalQuality div ?RandomCombineNeedNum,
    AddGerList = lists:foldr(fun(_, Acc) ->
                                     {?REWARD_GER,GerTypeID,_} = util:random_one_from_weigh_list(RConfig),
                                     [#new_ger{gerLevel=NewLevel,gerQuality=NewQuality,gerTypeID=GerTypeID}|Acc]
                             end, [], lists:seq(1, Crt)),
	role_reward:handle_ger_f(AddGerList,  ?MONEY_ADD_TYPE_COMBINE_RANDOM_GER, 0, ""),
	RoleInfo = role_data:get_roleInfo(),
	role_lib:deduct_money_f(RoleInfo, coin, NeedCoin, ?MONEY_DEC_TYPE_COMBINE_RANDOM_GER, 0, ""),
    notify_random_reward(AddGerList, ger),
	?CATCH(role_task_trigger:handle({dispach_task,role_combime,random_combine_ger})),
	role_homestead:hook_ger_delete(-1,0,0,[GID||#gerSimple{gerID=GID}<-DelGerList]).

do_random_combine_equip(BoxID, DelEquipList, NewEquipBagList,NeedCoin,EquipStarLevel) ->
    role_data:set_bagEquip(NewEquipBagList),
    {Date, _} = Time = erlang:localtime(),
    {LogItemList,TotalQuality,NewLevel} = lists:foldl(fun(#item{itemUID=EquipUID, itemTypeID=EquipTypeID, itemNum=EquipNum,itemRank=IR,itemLevel=ItemLevel},{Acc,Sum,AccLevel}) ->
                                    {[[EquipUID,EquipTypeID,EquipNum,EquipNum]|Acc],Sum+IR,erlang:max(AccLevel, ItemLevel)}
                            end, {[],0,1},DelEquipList),
    behavior_item_consume:log(role_data:get_roleID(), LogItemList, Date, Time, ?MONEY_DEC_TYPE_COMBINE_RANDOM_EQUIP, 0, ""),
    ?sendself(#sc_item_delete_notify{itemUIDList=lists:map(fun(#item{itemUID=EquipUID}) -> EquipUID end, DelEquipList)}),
    %%?ERR("~w", [{BoxID, role_data:get_mainGerTypeID()}]),
    [RConfig|_] =  data_box:get({BoxID, role_data:get_mainGerTypeID()}),
    Crt = get_equip_combine_crt(EquipStarLevel),
    
	NewQuality = TotalQuality div ?RandomCombineNeedNum,
    AddItemList =
        lists:foldr(fun(_, Acc) ->
                            {?REWARD_ITEM,ItemTypeID,Num} = util:random_one_from_weigh_list(RConfig),
                            [#new_item{itemLevel=NewLevel,itemNum=Num,itemRank=NewQuality,itemTypeID=ItemTypeID}|Acc]
                    end, [], lists:seq(1, Crt)),
	RoleInfo = role_data:get_roleInfo(),
	role_reward:handle_item_f(RoleInfo, AddItemList, ?MONEY_ADD_TYPE_COMBINE_RANDOM_EQUIP, 0, ""),
	role_lib:deduct_money_f(RoleInfo, coin, NeedCoin, ?MONEY_DEC_TYPE_COMBINE_RANDOM_EQUIP, 0, ""),
    notify_random_reward(AddItemList, equip),
	?CATCH(role_task_trigger:handle({dispach_task,role_combime,normal_combine_equip})).

get_ger_combine_crt(GerStarLevel) ->
    case util:now() =< util:datetime_to_seconds(data_combine_crt:get(datetime)) of
        true ->
            case data_combine_crt:get({ger, GerStarLevel}) of
                ?undefined ->
                    1;
                WeightList ->
                    util:random_one_from_weigh_list(WeightList)
            end;
        false ->
            1
    end.

get_equip_combine_crt(EquipStarLevel) ->
    case util:now() =< util:datetime_to_seconds(data_combine_crt:get(datetime)) of
        true ->
            case data_combine_crt:get({equip, EquipStarLevel}) of
                ?undefined ->
                    1;
                WeightList ->
                    util:random_one_from_weigh_list(WeightList)
            end;
        false ->
            1
    end.



notify_random_reward(List, Type) ->
    case Type of
        equip ->
            notify_random_reward_equip(List);
        ger ->
            notify_random_reward_ger(List)
    end.

notify_random_reward_equip(NewItem) ->
    case erlang:is_list(NewItem) of
        true ->
            List =
                lists:map(fun(#new_item{itemTypeID=ItemTypeID,itemNum=ItemNum,itemLevel=ItemLevel,itemRank=ItemRank}) ->
                                  #p_newEquip{itemTypeID=ItemTypeID,itemNum=ItemNum,itemLevel=ItemLevel,itemRank=ItemRank}
                          end, NewItem);
        false ->
            #new_item{itemTypeID=ItemTypeID,itemNum=ItemNum,itemLevel=ItemLevel,itemRank=ItemRank} = NewItem,
            List = [#p_newEquip{itemTypeID=ItemTypeID,itemNum=ItemNum,itemLevel=ItemLevel,itemRank=ItemRank}]
    end,
    ?sendself(#sc_combine_equip{newEquip=List}).


notify_random_reward_ger(NewGer) ->
    case erlang:is_list(NewGer) of
        true ->
            List =
                lists:map(fun(#new_ger{gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality}) ->
                                  #p_newGer{gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality}
                          end, NewGer);
        false ->
            #new_ger{gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality} = NewGer,
            List = [#p_newGer{gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality}]
    end,
    ?sendself(#sc_combine_ger{newGer=List}).

do_normal_combine_ger(NewBagGerList, DelGerList, NewItemBagList, UpdateItemBagList, DelItemBagList, NeedCoin, CombineDestTypeID, NewRank, NewLevel, #role{roleID=RoleID}=RoleInfo) ->
    {Date, _} = Time = erlang:localtime(),
    role_data:set_bagItem(NewItemBagList),
    notify_bagItem_and_log(RoleID, UpdateItemBagList, DelItemBagList, ?MONEY_DEC_TYPE_COMBINE_NORMAL_GER, Date, Time),
    
    role_data:set_gerBag(NewBagGerList),
    LogGerList = lists:map(fun(#gerSimple{gerID=GerID, gerTypeID=GerTypeID, gerQuality=GerQuality, gerLevel=GerLevel}) ->
                                [GerID,GerTypeID,GerLevel,GerQuality]   
                           end, DelGerList),
    behavior_ger_consume:log(RoleID, LogGerList, Date, Time, ?MONEY_DEC_TYPE_COMBINE_NORMAL_GER, 0, ""),
    ?sendself(#sc_ger_del{gerIDList=lists:map(fun(#gerSimple{gerID=GerUID}) -> GerUID end, DelGerList)}),
    
    role_lib:deduct_money_f(RoleInfo, coin, NeedCoin, ?MONEY_DEC_TYPE_COMBINE_NORMAL_GER, 0, ""),
    
    NewGer = #new_ger{gerTypeID=CombineDestTypeID, gerLevel=NewLevel, gerQuality=NewRank},
    ger_lib:add_ger_list([NewGer], ?MONEY_ADD_TYPE_COMBINE_NORMAL_GER, 0, ""),
	?CATCH(role_task_trigger:handle({dispach_task,role_combime,normal_combine_ger})),
    ?sendself(#sc_combine_ger{newGer=[#p_newGer{gerTypeID=CombineDestTypeID, gerLevel=NewLevel, gerQuality=NewRank}]}),
	role_homestead:hook_ger_delete(-1,0,0,[GID||#gerSimple{gerID=GID}<-DelGerList]).

do_normal_combine_equip(NewBagEquipList, DelEquipList, NewItemBagList, UpdateItemBagList, DelItemBagList, NeedCoin, CombineDestTypeID, NewRank, NewLevel, #role{roleID=RoleID}=RoleInfo) ->
    {Date, _} = Time = erlang:localtime(),
    role_data:set_bagItem(NewItemBagList),
    notify_bagItem_and_log(RoleID, UpdateItemBagList, DelItemBagList, ?MONEY_DEC_TYPE_COMBINE_NORMAL_EQUIP, Date, Time),
    
    role_data:set_bagEquip(NewBagEquipList),
    LogItemList = lists:map(fun(#item{itemUID=EquipUID, itemTypeID=EquipTypeID, itemNum=EquipNum}) ->
                                    [EquipUID,EquipTypeID,EquipNum,EquipNum]
                            end, DelEquipList),
    behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_COMBINE_NORMAL_EQUIP, 0, ""),
    ?sendself(#sc_item_delete_notify{itemUIDList=lists:map(fun(#item{itemUID=EquipUID}) -> EquipUID end, DelEquipList)}),
    
    role_lib:deduct_money_f(RoleInfo, coin, NeedCoin, ?MONEY_DEC_TYPE_COMBINE_NORMAL_EQUIP, 0, ""),
    
    NewEquip = #new_item{itemTypeID=CombineDestTypeID,itemNum=1,itemLevel=NewLevel,itemRank=NewRank},
    item_lib:add_item_f([NewEquip], ?MONEY_ADD_TYPE_COMBINE_NORMAL_EQUIP, 0, ""),
	?CATCH(role_task_trigger:handle({dispach_task,role_combime,normal_combine_equip})),
    ?sendself(#sc_combine_equip{newEquip=[#p_newEquip{itemTypeID=CombineDestTypeID, itemNum=1, itemLevel=NewLevel, itemRank=NewRank}]}).

notify_bagItem_and_log(RoleID, UpdateItemBagList, DelItemBagList, ConsumeType, Date, Time) ->
    case UpdateItemBagList of
        [] ->
            next;
        _ ->
%%             ?ERR("~w", [UpdateItemBagList]),
            UpdateList = lists:map(fun(#item{itemUID=UID, itemNum=Num}) ->
                                      #p_item_num_update{itemNum=Num, itemUID=UID}     
                                   end, UpdateItemBagList),
            ?sendself(#sc_item_update{updateList=UpdateList}),
            
            LogList1 = lists:map(fun(#item{itemUID=UID, itemTypeID=ItemTypeID, itemNum=Num}) ->
                                         [UID, ItemTypeID, 1, Num + 1]
                                 end, UpdateItemBagList),
            behavior_item_consume:log(RoleID, LogList1, Date, Time, ConsumeType, 0, "")
    end,
    case DelItemBagList of
        [] ->
            next;
        _ ->
%%             ?ERR("~w", [DelItemBagList]),
            UIDList = lists:map(fun(#item{itemUID=UID}) ->
                                        UID
                                end, DelItemBagList),
            ?sendself(#sc_item_delete_notify{itemUIDList=UIDList}),
            
            LogList2 = lists:map(fun(#item{itemUID=UID, itemTypeID=ItemTypeID, itemNum=Num}) ->
                                         [UID, ItemTypeID, Num, Num]
                                 end, DelItemBagList),
            behavior_item_consume:log(RoleID, LogList2, Date, Time, ConsumeType, 0, "")
    end.

check_can_combine(CombineType, CombineTypeID, CombineOutType, UIDList) ->
%%     ?ERR("CombineType:~w, CombineTypeID:~w, CombineOutType:~w, UIDList:~w", [CombineType, CombineTypeID, CombineOutType, UIDList]),
	case CombineType of
		?CombineTypeNormal ->
            check_combine_normal(CombineTypeID, UIDList);
		?CombineTypeRandom ->
			check_combine_random(CombineOutType, UIDList)
	end.

check_combine_random(CombineOutType, UIDList) ->
    case CombineOutType of
        ?CombineGer ->
            GerBagList = role_data:get_gerBag(),
            {DelGerList, NewGerBagList} =
                lists:foldr(fun(UID, {AccDelGerList, AccGerBagList}) ->
                                    case lists:keytake(UID, #gerSimple.gerID, AccGerBagList) of
                                        {value, #gerSimple{gerTypeID=DelGerTypeID}=DelGer, NewAccGerBagList} ->
                                            case util:is_exp_card(DelGerTypeID) of
                                                false ->
                                                    {[DelGer|AccDelGerList], NewAccGerBagList};
                                                true ->
                                                    erlang:throw({false, ?CombineFailExpGerCanNotCombine})
                                            end;
                                        false ->
                                            erlang:throw({false, ?CombineFailNotEnough})
                                    end
                            end, {[], GerBagList}, UIDList),
            case erlang:length(DelGerList) =:= ?RandomCombineNeedNum of
                true ->
                    next;
                false ->
                    erlang:throw({false, ?CombineFailRandomCombineNeedNumError})
            end,
            GerStarLevel = check_ger_star_level(DelGerList),
            case GerStarLevel =< data_combine_random:get(max_ger_star_level) of
                true ->
                    next;
                false ->
                    erlang:throw({false, ?CombineFailStarLevelLimit})
            end,
            {BoxID, NeedCoin} = data_combine_random:get({ger, GerStarLevel}),
            case role_lib:check_money(role_data:get_roleInfo(), coin, NeedCoin) of
                true ->
                    next;
                false ->
                    erlang:throw({false, ?CombineFailCoinNotEnough})
            end,
			RoleID = role_data:get_roleID(),
			case homestead_server:has_homestead_ger(RoleID, DelGerList) of
				true->
					erlang:throw({false, ?CombineFailHomesteadGerNot});
				false->
					{random_combine_ger, BoxID, DelGerList, NewGerBagList,NeedCoin, GerStarLevel}
			end;
        ?CombineEquip ->
            EquipBagList = role_data:get_bagEquip(),
            {DelEquipList, NewEquipBagList} =
                lists:foldr(fun(UID, {AccDelEquipList, AccEquipBagList}) ->
                                    case lists:keytake(UID, #item.itemUID, AccEquipBagList) of
                                        {value, DelEquip, NewAccEquipBagList} ->
                                            {[DelEquip|AccDelEquipList], NewAccEquipBagList};
                                        false ->
                                            erlang:throw({false, ?CombineFailNotEnough})
                                    end
                            end, {[], EquipBagList}, UIDList),
            case erlang:length(DelEquipList) =:= ?RandomCombineNeedNum of
                true ->
                    next;
                false ->
                    erlang:throw({false, ?CombineFailRandomCombineNeedNumError})
            end,
            EquipStarLevel = check_equip_star_level(DelEquipList),
            case EquipStarLevel =< data_combine_random:get(max_equip_star_level) of
                true ->
                    next;
                false ->
                    erlang:throw({false, ?CombineFailStarLevelLimit})
            end,
            {BoxID, NeedCoin} = data_combine_random:get({equip, EquipStarLevel}),
            case role_lib:check_money(role_data:get_roleInfo(), coin, NeedCoin) of
                true ->
                    next;
                false ->
                    erlang:throw({false, ?CombineFailCoinNotEnough})
            end,
            {random_combine_equip, BoxID, DelEquipList, NewEquipBagList,NeedCoin, EquipStarLevel}
    end.

check_ger_star_level([#gerSimple{gerTypeID=GerTypeID}|GerList]) ->
    #data_ger{gerStar=SpecStarLevel} = data_ger:get(GerTypeID),
    check_ger_star_level(GerList, SpecStarLevel).

check_ger_star_level([], SpecStarLevel) ->
    SpecStarLevel;
check_ger_star_level([#gerSimple{gerTypeID=GerTypeID}|GerList], SpecStarLevel) ->
    #data_ger{gerStar=StarLevel} = data_ger:get(GerTypeID),
    case StarLevel of
        SpecStarLevel ->
            check_ger_star_level(GerList, SpecStarLevel);
        _ ->
            erlang:throw({false, ?CombineFailStarLevelNotSame})
    end.

check_equip_star_level([#item{itemTypeID=ItemTypeID}|EquipList]) ->
    #data_item{itemStar=SpecStarLevel} = data_item:get(ItemTypeID),
    check_equip_star_level(EquipList, SpecStarLevel).

check_equip_star_level([], SpecStarLevel) ->
    SpecStarLevel;
check_equip_star_level([#item{itemTypeID=ItemTypeID}|EquipList], SpecStarLevel) ->
    #data_item{itemStar=StarLevel} = data_item:get(ItemTypeID),
    case StarLevel of
        SpecStarLevel ->
            check_equip_star_level(EquipList, SpecStarLevel);
        _ ->
            erlang:throw({false, ?CombineFailStarLevelNotSame})
    end.

check_combine_normal(CombineTypeID, UIDList) ->
	DataCombine = data_combine:get(CombineTypeID),
    case erlang:is_record(DataCombine, data_combine) of
        true ->
            next;
        false ->
            erlang:throw({false, ?CombineFailBadCombineTypeID})
    end,
    #data_combine{coin=NeedCoin, formula_itemID=FormulaItemTypeID, combine_ID=CombineDestTypeID, combine_out_type=OutType} = DataCombine,
    ItemBagList = role_data:get_bagItem(),
%%     ?ERR("ItemBagList:~w", [ItemBagList]),
%%     ?ERR("~w", [role_data:get_bagEquip()]),
    {NewItemBagList, UpdateItemBagList, DelItemBagList} =
        case lists:keytake(FormulaItemTypeID, #item.itemTypeID, ItemBagList) of
            false ->
%%                 ?ERR("FormulaItemTypeID:~w", [FormulaItemTypeID]),
                erlang:throw({false, ?CombineFailNeedFormulaItem});
            {value, #item{itemNum=FormulaItemNum}=FormulaItem, ItemBagList2} ->
                case FormulaItemNum > 1 of
                    true ->
                        {[FormulaItem#item{itemNum=FormulaItemNum-1}|ItemBagList2], [FormulaItem#item{itemNum=FormulaItemNum-1}], []};
                    false ->
                        {ItemBagList2, [], [FormulaItem]}
                end
        end,
    RoleInfo = role_data:get_roleInfo(),
    case role_lib:check_money(RoleInfo, coin, NeedCoin) of
        true ->
            next;
        false ->
            erlang:throw({false, ?CombineFailCoinNotEnough})
    end,
    case OutType of
        ?CombineGer ->
            check_combine_normal_ger(NewItemBagList, UpdateItemBagList, DelItemBagList, CombineDestTypeID, RoleInfo, NeedCoin, UIDList, DataCombine);
        ?CombineEquip ->
            check_combine_normal_equip(NewItemBagList, UpdateItemBagList, DelItemBagList, CombineDestTypeID, RoleInfo, NeedCoin, UIDList, DataCombine)
    end.

check_combine_normal_ger(NewItemBagList, UpdateItemBagList, DelItemBagList, CombineDestTypeID, RoleInfo, NeedCoin, UIDList, DataCombine) ->
    {NewBagGerList, DelGerList} = check_ger_enough(DataCombine, UIDList),
    NewRank = get_new_ger_rank(DelGerList),
    NewLevel = get_new_ger_level(DelGerList),
	RoleID = role_data:get_roleID(),
	case homestead_server:has_homestead_ger(RoleID, DelGerList) of
		true->
			erlang:throw({false, ?CombineFailHomesteadGerNot});
		false->
    		{normal_combine_ger, NewBagGerList, DelGerList, NewItemBagList, UpdateItemBagList, DelItemBagList, NeedCoin, CombineDestTypeID, NewRank, NewLevel, RoleInfo}
	end.

check_combine_normal_equip(NewItemBagList, UpdateItemBagList, DelItemBagList, CombineDestTypeID, RoleInfo, NeedCoin, UIDList, DataCombine) ->
    {NewBagEquipList, DelEquipList} = check_equip_enough(DataCombine, UIDList),
    NewRank = get_new_equip_rank(DelEquipList),
    NewLevel = get_new_equip_level(DelEquipList),
    {normal_combine_equip, NewBagEquipList, DelEquipList, NewItemBagList, UpdateItemBagList, DelItemBagList, NeedCoin, CombineDestTypeID, NewRank, NewLevel, RoleInfo}.
    
get_new_ger_rank(GerList) ->
    RankSum = lists:foldr(fun(#gerSimple{gerQuality=Rank}, Acc) ->
                                  Acc + Rank
                          end, 0, GerList),
    erlang:trunc(RankSum / ?RandomCombineNeedNum).

get_new_ger_level(GerList) ->
    lists:foldr(fun(#gerSimple{gerLevel=GerLevel}, AccLevel) ->
                        case GerLevel > AccLevel of
                            true ->
                                GerLevel;
                            false ->
                                AccLevel
                        end
                end, 1, GerList).

get_new_equip_rank(EquipList) ->
    {RankSum, _Num} = lists:foldr(fun(#item{itemRank=Rank, itemNum=Num}, {AccRankSum, AccNum}) ->
                                         {AccRankSum + Rank * Num, AccNum + Num}
                                 end, {0, 0}, EquipList),
    erlang:trunc(RankSum / ?RandomCombineNeedNum).

get_new_equip_level(EquipList) ->
    lists:foldr(fun(#item{itemLevel=ItemLevel}, AccLevel) ->
                        case ItemLevel > AccLevel of
                            true ->
                                ItemLevel;
                            false ->
                                AccLevel
                        end
                end, 1, EquipList).

check_ger_enough(DataCombine, UIDList) ->
    #data_combine{need_item_list=NeedGerTypeIDList} = DataCombine,
    BagGerList = role_data:get_gerBag(),
    {NewBagGerList, DelGerList} =
        lists:foldr(fun(GerUID, {AccBagGerList, AccDelGerList}) ->
                            case lists:keytake(GerUID, #gerSimple.gerID, AccBagGerList) of
                                {value, DelGer, NewAccBagGerList} ->
                                    {NewAccBagGerList, [DelGer|AccDelGerList]};
                                false ->
%%                                     ?ERR("GerUID:~w", [GerUID]),
                                    erlang:throw({false, ?CombineFailNotEnough})
                            end    
                    end, {BagGerList, []}, UIDList),
    GerTypeIDList =
        lists:map(fun(#gerSimple{gerTypeID=GerTypeID}) ->
                          GerTypeID
                  end, DelGerList),
    case lists:sort(NeedGerTypeIDList) =:= lists:sort(GerTypeIDList) of
        true ->
            {NewBagGerList, DelGerList};
        false ->
            erlang:throw({false, ?CombineFailBadCombine})
    end.

check_equip_enough(DataCombine, UIDList) ->
    #data_combine{need_item_list=NeedEquipTypeIDList} = DataCombine,
    BagEquipList = role_data:get_bagEquip(),
    {NewBagEquipList, DelEquipList} =
        lists:foldr(fun(EquipUID, {AccBagEquipList, AccDelEquipList}) ->
                            case lists:keyfind(EquipUID, #item.itemUID, AccBagEquipList) of
                                false ->
%%                                     ?ERR("EquipUID:~w", [EquipUID]),
                                    erlang:throw({false, ?CombineFailNotEnough});
                                #item{itemNum=Num}=NeedDelEquip ->
                                    case Num > 1 of
                                        true ->
                                            NewAccBagEquipList = lists:keyreplace(EquipUID, #item.itemUID, AccBagEquipList, NeedDelEquip#item{itemNum=Num-1});
                                        false ->
                                            NewAccBagEquipList = lists:keydelete(EquipUID, #item.itemUID, AccBagEquipList)
                                    end,
                                    NewAccDelEquipList = 
                                        case lists:keyfind(EquipUID, #item.itemUID, AccDelEquipList) of
                                            false ->
                                                [NeedDelEquip#item{itemNum=1}|AccDelEquipList];
                                            #item{itemNum=DelNum}=DelEquip ->
                                                [DelEquip#item{itemNum=DelNum+1}|AccDelEquipList]
                                        end,
                                    {NewAccBagEquipList, NewAccDelEquipList}
                            end
                    end, {BagEquipList, []}, UIDList),
    EquipTypeIDList =
        lists:foldr(fun(#item{itemTypeID=ItemTypeID,itemNum=ItemNum}, AccEquipTypeIDList) ->
                            lists:duplicate(ItemNum, ItemTypeID) ++ AccEquipTypeIDList
                    end, [], DelEquipList),
    case lists:sort(NeedEquipTypeIDList) =:= lists:sort(EquipTypeIDList) of
        true ->
            {NewBagEquipList, DelEquipList};
        false ->
            erlang:throw({false, ?CombineFailBadCombine})
    end.

cs_combine_info(_) ->
    ?sendself(#sc_combine_info{stopTime=util:datetime_to_seconds(data_combine_crt:get(datetime)),
                               content=data_combine_crt:get(content),
                               gerStarList=get_ger_star_list(),
                               equipStarList=get_equip_star_list()}).

get_ger_star_list() ->
    lists:foldr(fun(Star,Acc) ->
                        case data_combine_crt:get({ger, Star}) of
                            ?undefined ->
                                Acc;
                            _ ->
                                [Star|Acc]
                        end
                end, [], lists:seq(1, 20)).

get_equip_star_list() ->
    lists:foldr(fun(Star,Acc) ->
                        case data_combine_crt:get({equip, Star}) of
                            ?undefined ->
                                Acc;
                            _ ->
                                [Star|Acc]
                        end
                end, [], lists:seq(1, 20)).









