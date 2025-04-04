%% @author admin
%% @doc 处理奖励接口
%% Created 2013-3-7


-module(role_reward).
-compile(export_all).
-include("def_role.hrl").
-include("def_reward.hrl").
%% API functions
-export([
		]).


%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
%% 点将系统的奖励
handle_card_reward_f(1, GerTypeID) ->
	ger_lib:add_ger(#new_ger{gerLevel=1,gerQuality=0,gerTypeID=GerTypeID}, ?MONEY_ADD_TYPE_DRAW_CARD,0,"");
handle_card_reward_f(2, ItemTypeID) ->
	item_lib:add_white_item_f(ItemTypeID, ?MONEY_ADD_TYPE_DRAW_CARD,0,"");
handle_card_reward_f(3, Gold) ->
	Role = role_data:get_roleInfo(),
	role_lib:add_gold_f(Role, Gold, ?MONEY_ADD_TYPE_DRAW_CARD,0,"");
handle_card_reward_f(4, Coin) ->
	Role = role_data:get_roleInfo(),
	role_lib:add_coin_f(Role, Coin, ?MONEY_ADD_TYPE_DRAW_CARD,0,"");
handle_card_reward_f(5, Reputation) ->
	Role = role_data:get_roleInfo(),
	role_lib:add_reputation_f(Role, Reputation, ?MONEY_ADD_TYPE_DRAW_CARD,0,"").

%% 汉帝宝库奖励,Type=1,单抽,Type=2,十连抽
handle_treaHouse_reward_f(1, GerTypeID, Type) ->
	#data_ger{gerStar=Star} = data_ger:get(GerTypeID), 
	if Star >= 5 ->
		   #role{roleName=RoleName} = role_data:get_roleInfo(),
		   GerView=#p_ger_view{gerQuality=0, gerLevel=1, gerTypeID=GerTypeID},
		   broadcast_server:bc_msgID(10011, [RoleName, GerView, "1"]);
	   true ->
		   ignore
	end,
	ger_lib:add_ger(#new_ger{gerLevel=1,gerQuality=0,gerTypeID=GerTypeID}, ?MONEY_ADD_TYPE_TREAHOUSE_EXPLORE,Type,"");
handle_treaHouse_reward_f(2, ItemTypeID,Type) ->
	#data_item{itemStar=Star} = data_item:get(ItemTypeID),
	if Star >=  5 ->
		   #role{roleName=RoleName} = role_data:get_roleInfo(),
		   ItemView=#p_item_view{itemTypeID=ItemTypeID, itemLevel=1, itemRank=0, itemNum=1},
		   broadcast_server:bc_msgID(10012, [RoleName,ItemView ,"1"]);
	   true ->
		   ignore
	end,
	item_lib:add_white_item_f(ItemTypeID, ?MONEY_ADD_TYPE_TREAHOUSE_EXPLORE,Type,"");
handle_treaHouse_reward_f(3, Gold,Type) ->
	Role = role_data:get_roleInfo(),
	role_lib:add_gold_f(Role, Gold, ?MONEY_ADD_TYPE_TREAHOUSE_EXPLORE,Type,"");
handle_treaHouse_reward_f(4, Coin,Type) ->
	Role = role_data:get_roleInfo(),
	role_lib:add_coin_f(Role, Coin, ?MONEY_ADD_TYPE_TREAHOUSE_EXPLORE,Type,"");
handle_treaHouse_reward_f(5, Reputation,Type) ->
	Role = role_data:get_roleInfo(),
	role_lib:add_reputation_f(Role, Reputation, ?MONEY_ADD_TYPE_TREAHOUSE_EXPLORE,Type,"");
handle_treaHouse_reward_f(6, Exp,_Type) ->
	Role = role_data:get_roleInfo(),
	role_lib:add_exp(Role, Exp).
	
%% @doc 给玩家发送奖励
handle_dungeon_reward_f(Role, Reward, Type, ArgID, Desc) ->
	#reward{coin=AddCoin,gerExp=AddGerExp,roleExp=AddRoleExp,gold=AddGold,dropList=DropList,reputation=AddRepu}=Reward,
	{Role2, GerAddExpList} = reward_normal(Role, AddCoin, AddGold,  AddGerExp, AddRoleExp, AddRepu, Type, ArgID, Desc),
	RandomSelect = random_drop(DropList),
	{RewardItemList, RewardGerList} = partition_drop(RandomSelect),
	handle_item_f(Role2, RewardItemList, Type, ArgID, Desc),
	handle_ger_f(RewardGerList, Type, ArgID, Desc),
	role_data:set_roleInfo(Role2),
	{Role2, GerAddExpList, RewardItemList, RewardGerList}.

handle_sell_reward_f(Role, Reward, Type, ArgID, Desc) ->
	#sell_reward{coin=AddCoin,gerExp=AddGerExp,gold=AddGold,item=ItemList,reputation=AddReputation,roleExp=AddRoleExp,newGer=NewGer} = Reward,
	{Role2, _GerAddExpList} = reward_normal(Role, AddCoin, AddGold,  AddGerExp, AddRoleExp, AddReputation, Type, ArgID, Desc),
	handle_ger_f(NewGer, Type, ArgID, Desc),
	Role3 = handle_item_f(Role2, ItemList, Type, ArgID, Desc),
	role_data:set_roleInfo(Role3).

handle_daily_reward_f(Role, Reward, Type, ArgID, Desc) ->
	#daily_reward{coin=AddCoin,gold=AddGold,item=ItemList,newGer=NewGer,reputation=AddReputation, vip=Vip} = Reward,
	{Role2, _GerAddExpList} = reward_normal(Role, AddCoin, AddGold,  0, 0, AddReputation, Type, ArgID, Desc),
	Role3=handle_role_vip_f(Role2, Vip),
	handle_ger_f(NewGer, Type, ArgID, Desc),
	Role4 = handle_item_f(Role3, ItemList, Type, ArgID, Desc),
	role_data:set_roleInfo(Role4),
	Role3.
  
%% 领取系统奖励
handle_sys_reward(Role, Reward, Type, ArgID, Desc) ->
	{AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, NewItem, NewGer} = transform2normal_reward(Reward),
	{Role2, GerExp} = reward_normal(Role, AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, Type, ArgID, Desc),
	role_data:set_roleInfo(Role2),
	handle_ger_f(NewGer, Type, ArgID, Desc),
	Role3 = handle_item_f(Role2, NewItem, Type, ArgID, Desc),
	{Role3,GerExp}.

handle_role_vip_f(Role, Vip)->
    if Role#role.vipLevel < Vip ->
           VipChallengeGodFreeTimes = role_lib:get_max_challengeGodFreeTimes(Vip),
           RoleTimes=role_data:get_roleTimes(),
           VipChallengeGodBuyTimes = RoleTimes#roleTimes.challengeGodBuyTimes,%role_lib:get_max_challengeGodBuyTimes(Vip),
           NewChallengeGodEnergy = VipChallengeGodFreeTimes - (role_lib:get_max_challengeGodFreeTimes(Role#role.vipLevel) - RoleTimes#roleTimes.challengeGodEnergy),
           RoleTimes2 = RoleTimes#roleTimes{challengeGodEnergy=NewChallengeGodEnergy,
                                            challengeGodBuyTimes=VipChallengeGodBuyTimes
                                           },
           role_data:set_roleTimes(RoleTimes2),
           ?notify_update(?ra_vipLevel(Vip, NewChallengeGodEnergy,VipChallengeGodBuyTimes)),
           MaxDscv = role_lib:get_max_dscv_times(Vip),
           MaxEnergy = role_lib:get_max_energy(Vip),
           ?sendself(#sc_role_base_config{dscvMax=MaxDscv,energyMax=MaxEnergy}),
           Role#role{vipLevel=Vip};
       true ->
           Role
    end.

handle_ger_f(0, _Type, _ArgID, _Desc) ->
	ignore;
handle_ger_f([], _Type, _ArgID, _Desc) ->
	ignore;
handle_ger_f(NewGer, Type, ArgID, Desc) -> 
	ger_lib:add_ger_list(NewGer, Type, ArgID, Desc).
	
handle_item_f(Role, 0, _Type, _ArgID, _Desc) ->
	Role;
handle_item_f(Role, [], _Type, _ArgID, _Desc) ->
	Role;
handle_item_f(Role, ItemList, Type, ArgID, Desc) ->
	item_lib:add_item_f(ItemList, Type, ArgID, Desc),
	Role.

reward_normal(Role, 
			  AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, Type, ArgID, Desc) ->
	{Date, _} = Time = erlang:localtime(),
	%% 主公加经验
	if AddRoleExp == 0 ->
		   #role{level=Level2,exp=Exp2}=Role,
		   Role1 = Role;
	   true ->
		   case role_lib:add_exp(Role, AddRoleExp) of
			   {level_up, Level2, Exp2} ->
				   Role1 = role_lib:hook_level_up(Role, Level2),
				   ?notify_update(?ra_exp(Exp2)),
				   ?notify_update(?ra_level(Level2));
			   {level_not_up, Level2, Exp2} ->
				   Role1 = Role,
				   ?notify_update(?ra_exp(Exp2));
			   {level_max,Level2,Exp2} ->
				   Role1 = Role
		   end
	end,
	#role{vipLevel=VipLevel, roleID=RoleID, coin=CurCoin, goldBonus=CurGoldBonus, reputation=Repu, srcType=SrcType}=Role1,
	%% 加钱
	if AddCoin > 0 ->
		   Coin2 = CurCoin + AddCoin,
		   ?CATCH_1( role_task_trigger:handle({dispach_task,role_add_reward,AddCoin,?REWARD_COIN}),E1),
		   behavior_coin_add:log(RoleID, VipLevel, AddCoin, CurCoin, Date, Time, Type, ArgID, Desc),
		   ?notify_update(?ra_coin(Coin2));
	   true ->
		   Coin2 = CurCoin
	end,
	%% 加元宝
	if AddGold > 0 ->
		   GoldBonus2 = CurGoldBonus+ AddGold,
           tencent_pay:add_gold(RoleID, SrcType, AddGold),
		   behavior_gold_bonus_add:log(RoleID, VipLevel, AddGold, CurGoldBonus, Date, Time, Type, ArgID, Desc),
		   ?CATCH_1(role_task_trigger:handle({dispach_task,role_add_reward,AddGold,?REWARD_GOLD}),E2),
		   ?notify_update(?ra_goldBonus(GoldBonus2));
	   true ->
		   GoldBonus2 = CurGoldBonus
	end,
	%% 加声望
	if Reputation > 0 ->
		   Reputation2 = Repu + Reputation,
		   behavior_repu_add:log(RoleID, VipLevel, Reputation, Repu, Date, Time, Type, ArgID, Desc),
		   ?CATCH_1(role_task_trigger:handle({dispach_task,role_add_reward,Reputation,?REWARD_REPU}),E3),
		   ?notify_update(?ra_reputation(Reputation2));
	   true ->
		   Reputation2 = Repu
	end,
		
	%% 出战武将加经验
	if AddGerExp == 0 ->
		   GerAddExpList =[];
	   true ->
		   io:format("exp:~w",[AddGerExp]),
		   PosList = role_data:get_posList(),
		   {GerAddExpList,GerList} = lists:foldl(fun(Ger,{EAcc, GAcc}) ->
														 %% 以新的主公等级来计算武将经验
														 #ger{gerBase=#gerBase{gerPos=GerPos}} = Ger,
														 {IsLevelUpgraded, NewGer, RealAddExp} = ger_lib:add_exp_and_notify(Ger, AddGerExp, Level2,PosList),
														 {[#p_ger_add_exp{gerPos=GerPos, addExp=RealAddExp, isUpgraded=IsLevelUpgraded}|EAcc], [NewGer|GAcc]}
												 end, {[],[]}, PosList),
		   role_data:set_posList(GerList)
	end,
	
	Role2 = Role1#role{coin=Coin2,goldBonus=GoldBonus2,level=Level2,exp=Exp2,reputation=Reputation2},
	{Role2, GerAddExpList}.
			



	
transform2normal_reward(#sell_reward{coin=AddCoin,gerExp=AddGerExp,gold=AddGold,item=ItemList,reputation=AddReputation,roleExp=AddRoleExp,newGer=NewGer}) ->
	{AddCoin, AddGold,  AddGerExp, AddRoleExp, AddReputation,ItemList,NewGer};
transform2normal_reward(Reward) ->
	transform2normal_reward(Reward, 0, 0, 0, 0, 0, [], []).


transform2normal_reward([],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, NewItem, NewGer) ->
	{AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, NewItem, NewGer};
transform2normal_reward([{?REWARD_GOLD,Gold}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, NewItem, NewGer) ->
	transform2normal_reward(List, AddCoin,AddGold+Gold, AddGerExp, AddRoleExp, Reputation, NewItem, NewGer);
transform2normal_reward([{?REWARD_COIN,Coin}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, NewItem, NewGer) ->
	transform2normal_reward(List, AddCoin+Coin,AddGold, AddGerExp, AddRoleExp, Reputation, NewItem, NewGer);
transform2normal_reward([{?REWARD_REPU,Repu}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, NewItem, NewGer) ->
	transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation+Repu, NewItem, NewGer);
transform2normal_reward([{?REWARD_ITEM,ItemTypeID,Num}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, NewItem, NewGer) ->
	transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, [#new_item{itemLevel=1,itemNum=Num,itemRank=0,itemTypeID=ItemTypeID}|NewItem], NewGer);
transform2normal_reward([{?REWARD_GER,GerTypeID,Num}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, NewItem, NewGer) ->
	Ger = #new_ger{gerLevel=1,gerQuality=0,gerTypeID=GerTypeID},
	AddGer = lists:duplicate(Num, Ger),
	transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, NewItem, AddGer++NewGer);
transform2normal_reward([{?REWARD_GER_EXP,GerExp}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, NewItem, NewGer) ->
	transform2normal_reward(List, AddCoin,AddGold, AddGerExp+GerExp, AddRoleExp, Reputation, NewItem, NewGer);
transform2normal_reward([{?REWARD_ROLE_EXP,RoleExp}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, NewItem, NewGer) ->
	transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp+RoleExp, Reputation, NewItem, NewGer);
transform2normal_reward([#new_item{}=E|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, NewItem, NewGer) ->
	transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, [E|NewItem], NewGer);
transform2normal_reward([#new_ger{}=E|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, NewItem, NewGer) ->
	transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, NewItem, [E|NewGer]).





transform2p_mail_reward([]) ->
	#p_mail_reward{gerList=[],itemList=[]};
transform2p_mail_reward(#sell_reward{coin=Coin,gold=Gold,item=Item,newGer=NewGer,reputation=Reputation,roleExp=RoleExp}) ->
	if is_record(Item, new_item) ->
		   Item3 = [#p_id_num{num=Item#new_item.itemNum,typeID=Item#new_item.itemTypeID}];
	   is_list(Item) ->		
		   %Item3 = [#p_id_num{num=A,typeID=B}||#new_item{itemNum=A,itemTypeID=B}<-Item];
		   Item3 = lists:foldl(fun(#new_item{itemNum=A, itemTypeID=B}, Acc)->
									   case lists:keytake(B, #p_id_num.typeID, Acc) of
										   false ->
											   [#p_id_num{num=A,typeID=B}|Acc];
										   {value, #p_id_num{num=Num}=Info, Acc2}->
											   [Info#p_id_num{num=A+Num}|Acc2]
									   end
							   end, [], Item);
	   true ->
		   Item3 = []
	end,
	
	if Coin > 0 ->
		   Item4 = [#p_id_num{num=Coin,typeID=30007}|Item3];
	   true ->
		   Item4 = Item3
	end,
	if Gold > 0 ->
		   Item5 = [#p_id_num{num=Gold,typeID=30008}|Item4];
	   true ->
		   Item5 = Item4
	end,
	if Reputation > 0 ->
		   Item6 = [#p_id_num{num=Reputation,typeID=30006}|Item5];
	   true ->
		   Item6 = Item5
	end,
	if RoleExp > 0 ->
		   Item7 = [#p_id_num{num=RoleExp,typeID=30005}|Item6];
	   true ->
		   Item7 = Item6
	end,
%% 	if GerExp > 0 ->
%% 		   Item8 = [#p_id_num{num=GerExp,typeID=20004}|Item7];
%% 	   true ->
%% 		   Item8 = Item7
%% 	end,
	if is_record(NewGer, new_ger) ->
		   Ger3 = [#p_id_num{num=1,typeID=NewGer#new_ger.gerTypeID}];
	   is_list(NewGer) ->
		   %Ger3 = [#p_id_num{num=1,typeID=A}||#new_ger{gerTypeID=A}<-NewGer];
		   Ger3 = lists:foldl(fun(#new_ger{gerTypeID=GerTypeID}, Acc)->
							   case lists:keytake(GerTypeID, #p_id_num.typeID, Acc) of
								   false ->
									   [#p_id_num{num=1, typeID=GerTypeID}|Acc];
								   {value, #p_id_num{num=Num}=Info, Acc2} ->
									   [Info#p_id_num{num=Num+1}|Acc2]
							   end
					   end, [], NewGer);
	   true ->
		   Ger3 = []
	end,
	#p_mail_reward{gerList=Ger3,itemList=Item7};		   
transform2p_mail_reward(Reward) ->
	{ItemList, GerList} = transform2p_mail_reward(Reward, [], []),
	#p_mail_reward{gerList=GerList,itemList=ItemList}.

transform2p_mail_reward([], ItemList, GerList) ->
	{ItemList,GerList};
transform2p_mail_reward([{?REWARD_GOLD,Gold}|List], ItemList, GerList) ->
	transform2p_mail_reward(List, [#p_id_num{num=Gold,typeID=30008}|ItemList],GerList);
transform2p_mail_reward([{?REWARD_COIN,Gold}|List], ItemList, GerList) ->
	transform2p_mail_reward(List, [#p_id_num{num=Gold,typeID=30007}|ItemList],GerList);
transform2p_mail_reward([{?REWARD_REPU,Gold}|List], ItemList, GerList) ->
	transform2p_mail_reward(List, [#p_id_num{num=Gold,typeID=30006}|ItemList],GerList);
transform2p_mail_reward([{?REWARD_ROLE_EXP,Gold}|List], ItemList, GerList) ->
	transform2p_mail_reward(List, [#p_id_num{num=Gold,typeID=30005}|ItemList],GerList);
%% transform2p_mail_reward([{?REWARD_GER_EXP,Gold}|List], ItemList, GerList) ->
%% 	transform2p_mail_reward(List, [#p_id_num{num=Gold,typeID=20004}|ItemList],GerList);
transform2p_mail_reward([{?REWARD_ITEM,TypeID,Gold}|List], ItemList, GerList) ->
	transform2p_mail_reward(List, [#p_id_num{num=Gold,typeID=TypeID}|ItemList],GerList);
transform2p_mail_reward([{?REWARD_GER,TypeID,Gold}|List], ItemList, GerList) ->
	transform2p_mail_reward(List, ItemList, [#p_id_num{num=Gold,typeID=TypeID}|GerList]);
transform2p_mail_reward([#new_item{itemTypeID=TypeID,itemNum=Num}|List], ItemList, GerList) ->
	case lists:keytake(TypeID, #new_item.itemTypeID, ItemList) of
		false ->
			transform2p_mail_reward(List, [#p_id_num{num=Num,typeID=TypeID}|ItemList],GerList);
		{value, #p_id_num{num=NumA}=Info, ItemList2} ->
			transform2p_mail_reward(List, [Info#p_id_num{num=Num+NumA}|ItemList2], GerList)
	end;
transform2p_mail_reward([#new_ger{gerTypeID=TypeID}|List], ItemList, GerList) ->
	case lists:keytake(TypeID, #new_ger.gerTypeID, GerList) of
		false ->
			transform2p_mail_reward(List, ItemList, [#p_id_num{num=1,typeID=TypeID}|GerList]);
		{value, #p_id_num{num=Num}=Info, GerList2} ->
			transform2p_mail_reward(List, ItemList, [Info#p_id_num{num=Num+1}|GerList2])
	end.

transform2p_reward_view([], List) ->
	List;
transform2p_reward_view([{?REWARD_GOLD,Gold}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view{type=?REWARD_GOLD,value=Gold}|RewardViewList]);
transform2p_reward_view([{?REWARD_COIN,Gold}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view{type=?REWARD_COIN,value=Gold}|RewardViewList]);
transform2p_reward_view([{?REWARD_REPU,Gold}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view{type=?REWARD_REPU,value=Gold}|RewardViewList]);
transform2p_reward_view([{?REWARD_ROLE_EXP,Gold}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view{type=?REWARD_ROLE_EXP,value=Gold}|RewardViewList]);
transform2p_reward_view([{?REWARD_GER_EXP,Gold}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view{type=?REWARD_GER_EXP,value=Gold}|RewardViewList]);
transform2p_reward_view([{?REWARD_ITEM,TypeID,Gold}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view2{type=?REWARD_ITEM,typeID=TypeID,num=Gold}|RewardViewList]);
transform2p_reward_view([{?REWARD_GER,TypeID,Gold}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view2{type=?REWARD_GER,typeID=TypeID,num=Gold}|RewardViewList]);
transform2p_reward_view([#new_item{itemTypeID=TypeID,itemNum=Num}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view2{type=?REWARD_ITEM,typeID=TypeID,num=Num}|RewardViewList]);
transform2p_reward_view([#new_ger{gerTypeID=TypeID}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view2{type=?REWARD_GER,typeID=TypeID,num=1}|RewardViewList]).
	
	

	
%% ====================================================================
%% Internal functions
%% ====================================================================

%% 随机掉落
random_drop([]) ->
	[];
random_drop(DropList) ->
	GlobalDropRate = data_common:get(global_drop_rate),
	random_drop(DropList, [], GlobalDropRate).

random_drop([0|DropList], Result, GlobalDropRate) ->
	random_drop(DropList, Result, GlobalDropRate);
random_drop([DropID|DropList], Result, GlobalDropRate) ->
	#data_drop{randomList=RandomList} = data_drop:get(DropID),
	if (not is_list(RandomList)) orelse RandomList =:= [] ->
		   random_drop(DropList, Result, GlobalDropRate);
	   true ->
		   random_drop(DropList, random_drop2(RandomList,GlobalDropRate) ++ Result , GlobalDropRate)
	end;
random_drop([],Result, _GlobalDropRate) ->
	Result.

random_drop2([{R0,_}|List]=RandomList, GlobalDropRate) ->
	Num = R0 + lists:foldl(fun({N,_}, Acc) -> N*GlobalDropRate+Acc end, 0, List),
	Value = random:uniform() * Num,
	ItemList =
	util:foldl(fun({R,I}, Acc) ->
						Acc2 = Acc + R,
						if Acc2 >= Value ->
							   {return,I};
						   true ->
							   Acc2
						end
			   end, 0, RandomList),
	case ItemList of
		[_|_] ->
			[util:random_one_from_list(ItemList)];
		_ ->
			[]
	end.

partition_drop(List) ->
	lists:partition(fun(E) ->is_record(E, new_item) end, List).