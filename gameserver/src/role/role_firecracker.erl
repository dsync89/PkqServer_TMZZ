%% @author admin
%% @doc 放爆竹玩家信息处理
%% Created 2014-01-08
-module(role_firecracker).
-compile(export_all).
-include("def_role.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

%% 检查角色VIP等级
check_vip_level({vip, Lower, Upper}, VipLevel)->
	VipLevel>=Lower andalso VipLevel=<Upper.
%% 检查角色等级
check_role_level({level, Lower, Upper}, RoleLevel)->
	RoleLevel>=Lower andalso RoleLevel=<Upper.

%% 查看鞭炮信息
cs_firecracker_open(#cs_firecracker_open{})->
	case fire_server:get_activity_info() of
		?undefined->
			?sendself(#sc_firecracker_open{status=0,startTime=0,rewardTime=0,closeTime=0,total=0,markedPrice=0,tradedPrice=0,count=0,rank=0,canReward=0,returnGold=0});
		#data_activity_fire{activityName=Name,description=Description,iconSrc=Icon,startTime=StartTime_,stopTime=StopTime_,closeTime=CloseTime_,vip=Vip,level=Level}->
			#role{roleID=RoleID, vipLevel=VipLevel, level=RoleLevel} = role_data:get_roleInfo(),
			case check_vip_level(Vip, VipLevel) andalso check_role_level(Level, RoleLevel) of
				true->
					StartTime = util:datetime_to_seconds(StartTime_),
					RewardTime = StartTime + fire_server:relative_time(StopTime_),
					CloseTime = RewardTime + fire_server:relative_time(CloseTime_),
					{Total, Count, Rank, CanReward, ReturnGold} = fire_server:open(RoleID),
					Record = #sc_firecracker_open{status=1, name=Name, description=Description, icon=Icon,
												  startTime=StartTime, rewardTime=RewardTime, closeTime=CloseTime, 
												  total=Total, markedPrice=fire_server:get_marked_price(), tradedPrice=fire_server:get_traded_price(Total),
												  count=Count, rank=Rank, canReward=CanReward, returnGold=ReturnGold,
												  discounts=[#p_discount{amount=Amount, discount=Discount} || {Amount, Discount}<-fire_server:get_discounts()]
												 },
					?sendself(Record);
				false->
					?sendself(#sc_firecracker_open{status=0,startTime=0,rewardTime=0,closeTime=0,total=0,markedPrice=0,tradedPrice=0,count=0,rank=0,canReward=0,returnGold=0})
			end
	end.

%% 燃放鞭炮
cs_firecracker_setoff(#cs_firecracker_setoff{type=Type})->
	case fire_server:get_activity_status() of
		1 ->
			#data_activity_fire{vip=Vip,level=Level} = fire_server:get_activity_info(),
			#role{roleID=RoleID, vipLevel=VipLevel, level=RoleLevel} = role_data:get_roleInfo(),
			case check_vip_level(Vip, VipLevel) andalso check_role_level(Level, RoleLevel) of
				true->
					Times =
						case Type of
							1->
								1;
							2->
								10;
							_->
								?undefined
						end,
					case Times of
						?undefined->
							?sendself(#sc_firecracker_setoff{result=4,count=0,returnGold=0,canReward=0});
						_->
							DeductGold = fire_server:calc_need_gold(Times),
							RoleInfo = role_data:get_roleInfo(),
							case role_lib:check_money(RoleInfo, gold, DeductGold) of
								true ->
									NewRoleInfo = role_lib:deduct_gold_f(RoleInfo, DeductGold, ?MONEY_DEC_TYPE_FIRECRACKER, 0, ""),
									{Rewards, Count, ReturnGold, CanReward} = fire_server:setoff(RoleID, DeductGold, Times),
									{RewardInfo, SellRewards} = do_fire_reward(Rewards),
									?sendself(#sc_firecracker_setoff{result=1,count=Count,returnGold=ReturnGold,canReward=CanReward,
																	 rewardInfo=RewardInfo}),
									%% 给予奖励
									SellReward = 
										lists:foldr(fun({sell_reward,A1,B1,C1,D1,E1,F1,G1}, {sell_reward,A2,B2,C2,D2,E2,F2,G2})->
															{sell_reward, A1+A2, B1+B2, C1+C2, D1+D2, E1++E2, F1+F2, G1++G2}
													end, {sell_reward, 0, 0, 0, 0, [], 0, []}, SellRewards),
									role_reward:handle_sell_reward_f(NewRoleInfo, SellReward, ?MONEY_ADD_TYPE_FIRECRACKER, 0, "");
								false ->
									?sendself(#sc_firecracker_setoff{result=3,count=0,returnGold=0,canReward=0})
							end
					end;
				false->
					?sendself(#sc_firecracker_setoff{result=2,count=0,returnGold=0,canReward=0})
			end;
		_->
			?sendself(#sc_firecracker_setoff{result=2,count=0,returnGold=0,canReward=0})
	end.

%% 发送奖励
do_fire_reward(Rewards)->
	lists:foldr(fun({RewardCoin, BoxID}, {RewardInfoAcc,SellRewardAcc}=Acc)->
						case data_box:get({BoxID, 0}) of
							[RConfig|_] ->
								RoleInfo = role_data:get_roleInfo(),
								Reward = util:random_one_from_weigh_list(RConfig),
								
								%% 广播4星以上道具武将
								#role{roleName=RoleName}=RoleInfo,
								{A, B, C, D, E, Item, Ger} = role_reward:transform2normal_reward([Reward]),
								ItemList = get_list(Item),
								GerList = get_list(Ger),
								lists:foreach(fun(X)->#new_item{itemTypeID=ItemTypeID, itemNum=ItemNum, itemRank=ItemRank, itemLevel=ItemLevel} = X,
													  #data_item{itemStar=ItemStar,itemType=ItemType} = data_item:get(ItemTypeID),
													  case need_to_broadcast(ItemStar, ItemType) of 
														  true -> 
															  ItemView=#p_item_view{itemTypeID=ItemTypeID, itemLevel=ItemLevel, itemRank=ItemRank, itemNum=ItemNum},
															  broadcast_server:bc_msgID(10030, [RoleName, ItemView,erlang:integer_to_list(ItemNum)]);
														  _ ->
															  ignore
													  end
											  end, ItemList),
								lists:foreach(fun(X)->#new_ger{gerTypeID=GerTypeId, gerLevel=GerLevel, gerQuality=GerQuality}=X,
													  #data_ger{gerStar=GerStar}=data_ger:get(GerTypeId),
													  if GerStar>=4 ->
															 GerView=#p_ger_view{gerQuality=GerQuality, gerLevel=GerLevel, gerTypeID=GerTypeId},
															 broadcast_server:bc_msgID(10031, [RoleName, GerView, "1"]);
														 true ->
															 ignore
													  end
											  end, GerList),
								
								%% 添加保底银币奖励
								SellReward = #sell_reward{coin=A+RewardCoin,roleExp=D,gerExp=C,gold=B,item=Item,reputation=E,newGer=Ger},
								
								{ [ activity_server:sell_reward2p_reward_info(SellReward) | RewardInfoAcc ], [ SellReward | SellRewardAcc ] };
							_ ->
								?ERR("error box id:~w",[BoxID]),
								Acc
						end
				end, {[],[]}, Rewards).

need_to_broadcast(ItemStar, ItemType)->
	if ItemStar > 4 ->
		   true;
	   ItemStar =:= 4->
		   case item_lib:is_main_equip(ItemType) of
			   true ->
				   true;
			   false ->
				   false
		   end;
	   true ->
		   false
	end.

get_list(List)	->
	if	List =:= 0	->
			[];
		is_tuple(List) ->
			[List];
		true	->
			List
	end.

%% 领取排行奖励
cs_firecracker_get_reward(#cs_firecracker_get_reward{})->
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			case fire_server:get_activity_status() of
				2 ->
					RoleID = role_data:get_roleID(),
					case fire_server:get_reward(RoleID) of
						{true, SellReward} ->
							RoleInfo = role_data:get_roleInfo(),
							role_reward:handle_sell_reward_f(RoleInfo, SellReward, ?MONEY_ADD_TYPE_FIRECRACKER_RANK, 0, ""),
							?sendself(#sc_firecracker_get_reward{result=1, reward=[activity_server:sell_reward2p_reward_info(SellReward)]});
						{false, ErrCode} ->
							?sendself(#sc_firecracker_get_reward{result=ErrCode})
					end;
				_ ->
					?sendself(#sc_firecracker_get_reward{result=2})
			end;
		false ->
			?sendself(#sc_firecracker_get_reward{result=255})
	end.
