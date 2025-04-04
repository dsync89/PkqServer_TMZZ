%% @author admin
%% @doc 汉帝宝库功能
%% Created 2013-4-23


-module(role_treaHouse).
-compile(export_all).
-include("def_role.hrl").
-include("def_weibo_share.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

-define(bomob_clo, 11).
-define(double_clo, 12).
-define(NULL_REWARD, #sell_reward{coin=0,gerExp=0,gold=0,item=0,newGer=0,reputation=0,roleExp=0}).
%% ====================================================================
%% API functions
%% ====================================================================

%% 免费抽取次数的恢复与神将录的恢复走相同流程
%% 如果id发生改变,即
cs_treaHouse_get_list(#cs_treaHouse_get_list{}) ->
	{EndTime, StopTime} = get_end_time(),

	OneTimeNeedGold = data_treasure_box:get(oneTimeCost),
	RefreshNeedCoin = data_treasure_box:get(refresh_cost),
	#treaHouseInfo{card_list=CardList} = TreaHouseInfo = role_data:get_treaHouseInfo(),
	case check_enter_treaHouse() of
		true ->
			ActivityID = activityRank_server:get_treaHouse_activityID(),
			ActivityID2 = TreaHouseInfo#treaHouseInfo.activityID,
			if CardList =:= [] orelse ActivityID =/= ActivityID2->
				   CardList2 = random_treaHouse_list(),
				   TreaHouseInfo2 = TreaHouseInfo#treaHouseInfo{activityID=ActivityID,card_list=CardList2},
				   role_data:set_treaHouseInfo(TreaHouseInfo2),
				   ?sendself(#sc_treaHouse_get_list{isOpen=1,endTime=EndTime,
													cardList=treaHouseCard2p_treaHouse_card(CardList2),
													freeTimes = TreaHouseInfo#treaHouseInfo.free_times,
													boxProcess = 0,boxOpenProcess=[],stopTime=StopTime,
													oneTimeNeedGold=OneTimeNeedGold, refreshNeedCoin=RefreshNeedCoin});
%% 			   ActivityID =/= ActivityID2 ->
%% 				   FreeTimes = data_treasure_box:get(treasure_house_free_times),
%% 				   TreaHouseInfo2 = #treaHouseInfo{value_info=1, card_list=[],free_count=0,buy_count=0, free_times=FreeTimes,
%% 								  mark=0, baseBoxGetList=0,isGetRankReward=[],activityID=ActivityID},
%% 				   role_data:set_treaHouseInfo(TreaHouseInfo2),
%% 				   ?sendself(#sc_treaHouse_get_list{isOpen=1,endTime=EndTime,
%% 													cardList=treaHouseCard2p_treaHouse_card([]),
%% 													freeTimes = TreaHouseInfo#treaHouseInfo.free_times,
%% 													boxProcess = 0,boxOpenProcess=[]});
			   true ->
				   BoxProcess = data_treasure_box_baseReward:get(TreaHouseInfo#treaHouseInfo.mark),
				   ?sendself(#sc_treaHouse_get_list{isOpen=1,endTime=EndTime,
													cardList=treaHouseCard2p_treaHouse_card(CardList),
													freeTimes = TreaHouseInfo#treaHouseInfo.free_times,
													boxProcess = BoxProcess,stopTime=StopTime,
													boxOpenProcess = TreaHouseInfo#treaHouseInfo.baseBoxGetList,
													oneTimeNeedGold=OneTimeNeedGold, refreshNeedCoin=RefreshNeedCoin})
			end;
		_ ->
			IsOpen = 
				if EndTime == 0 ->
					   CardList2=[],
				   2;
				   true ->
					   if CardList == [] ->
							  CardList2 = random_treaHouse_list();
						  true ->
							  CardList2=CardList
					   end,
					   3
				end,
			BoxProcess = data_treasure_box_baseReward:get(TreaHouseInfo#treaHouseInfo.mark),
			?sendself(#sc_treaHouse_get_list{isOpen=IsOpen,endTime=EndTime,cardList=treaHouseCard2p_treaHouse_card(CardList2),
											 freeTimes = 0,boxProcess = BoxProcess,stopTime=StopTime,
											 boxOpenProcess = TreaHouseInfo#treaHouseInfo.baseBoxGetList,
											 oneTimeNeedGold=0, refreshNeedCoin=0})
	end.

cs_treaHouse_is_open(#cs_treaHouse_is_open{}) ->
	case check_enter_treaHouse2() of
		true ->
			?sendself(#sc_treaHouse_is_open{type = 1});
		_ ->
			?sendself(#sc_treaHouse_is_open{type = 2})
	end.

cs_treaHouse_explore_one(#cs_treaHouse_explore_one{})->
	case check_enter_treaHouse() of
		true ->
			case check_treaHouse_explore_one() of
				{true, TreaHouseInfo, SpecialID}->
					{Info,TreaHouseInfo2,Reward} = do_treaHouse_explore_one(TreaHouseInfo, SpecialID, 0),
					role_data:set_treaHouseInfo(TreaHouseInfo2),
					do_reward(Reward,1),
					#role{roleID=RoleID,roleName=RoleName} = role_data:get_roleInfo(),
					activityRank_server:update_treaHouse_roleRank(RoleID, RoleName, TreaHouseInfo2#treaHouseInfo.mark),
					BoxProcess = data_treasure_box_baseReward:get(TreaHouseInfo2#treaHouseInfo.mark),
					?sendself(#sc_treaHouse_explore_one{type=1, boxProcess=BoxProcess,info=[Info]});
				{false, Reason} ->
					?sendself(#sc_treaHouse_explore_one{type=Reason,boxProcess=0,info=[]})
			end;
		_ ->
			?sendself(#sc_treaHouse_explore_one{type=2,boxProcess=0,info=[]})
	end.

cs_treaHouse_explore_ten(#cs_treaHouse_explore_ten{})->
	case check_enter_treaHouse() of
		true ->
			case check_treaHouse_explore_ten() of
				{true, OpenTimes} ->
					{InfoList, TreaHouseInfo2} = do_treaHouse_explore_ten(OpenTimes),
					BoxProcess = data_treasure_box_baseReward:get(TreaHouseInfo2#treaHouseInfo.mark),
					role_data:set_treaHouseInfo(TreaHouseInfo2),
					#role{roleID=RoleID,roleName=RoleName} = role_data:get_roleInfo(),
					activityRank_server:update_treaHouse_roleRank(RoleID, RoleName, TreaHouseInfo2#treaHouseInfo.mark),
					?sendself(#sc_treaHouse_explore_ten{type=1, openTimes=OpenTimes, boxProcess=BoxProcess,infoList=lists:reverse(InfoList)});
				{false, Reason} ->
					?sendself(#sc_treaHouse_explore_ten{type=Reason, openTimes=0,boxProcess=0,infoList=[]})
			end;
		_ ->
			?sendself(#sc_treaHouse_explore_ten{type=2,openTimes=0, boxProcess=0,infoList=[]})
	end.

cs_treaHouse_refresh(#cs_treaHouse_refresh{})->
	case check_enter_treaHouse() of
		true ->
			case check_treaHouse_refresh() of
				true ->
					CardList = do_treaHouse_refresh(),
					?sendself(#sc_treaHouse_refresh{type=1, cardList=treaHouseCard2p_treaHouse_card(CardList)});
				{false, Reason} ->
					?sendself(#sc_treaHouse_refresh{type=Reason, cardList=[]})
			end;
		_ ->
			?sendself(#sc_treaHouse_refresh{type=2, cardList=[]})
	end.

cs_treaHouse_open_base_box(#cs_treaHouse_open_base_box{pos=Pos})->
	case check_enter_treaHouse2() of
		true ->
			case check_open_treaHouse_baseBox(Pos) of
				{true, TreaHouseInfo,_BoxOpenProcess} ->
					BoxOpenProcess2=do_open_treaHouse_baseBox(Pos, TreaHouseInfo),
					?sendself(#sc_treaHouse_open_base_box{type=1, boxOpenProcess=BoxOpenProcess2});
				{false, Reason,BoxOpenProcess} ->
					?sendself(#sc_treaHouse_open_base_box{type=Reason, boxOpenProcess=BoxOpenProcess})
			end;
		_ ->
			?sendself(#sc_treaHouse_open_base_box{type=2, boxOpenProcess=0})
	end.

cs_treaHouse_get_baseBoxRewardInfo(#cs_treaHouse_get_baseBoxRewardInfo{})->
	RewardList = data_treasure_box:get(base_reward),
	InfoList=lists:foldr(fun(0,Acc)->
						Acc;
				   (E,Acc)->
						Pos = data_treasure_box_baseReward:get(E),
						{_,Reward} = lists:keyfind(Pos, 1, RewardList),
						[#p_treaHouse_BaseReward_Info{pos=Pos,needMark=E, rewardInfo=activity_server:sell_reward2p_reward_info(Reward)}
						|Acc]
						end, [], data_treasure_box_baseReward:get_list()),
	%InfoList = [activity_server:sell_reward2p_reward_info(E)||{_,E}<-RewardList],
	?sendself(#sc_treaHouse_get_baseBoxRewardInfo{baseReaward_boxInfoList=InfoList}).

cs_treaHouse_get_rankInfo(#cs_treaHouse_get_rankInfo{})->
	#treaHouseInfo{mark=Mark}  = role_data:get_treaHouseInfo(),
	activityRank_server:treaHouse_get_rankInfo(role_data:get_roleID(), Mark).


%% ====================================================================
%% Internal functions
%% ====================================================================

do_open_treaHouse_baseBox(BoxPos, TreaHouseInfo) ->
	BaseBoxGetList = TreaHouseInfo#treaHouseInfo.baseBoxGetList,
	BoxList = data_treasure_box:get(base_reward),
	{BoxPos, Reward} = lists:keyfind(BoxPos, 1, BoxList),
	Role = role_data:get_roleInfo(),
	role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_TREAHOUSE_BASEBOX, BoxPos, ""),
	TreaHouseInfo2=TreaHouseInfo#treaHouseInfo{baseBoxGetList=[#p_baseBoxOpenInfo{pos=BoxPos, isOpen=2}|BaseBoxGetList]},
	role_data:set_treaHouseInfo(TreaHouseInfo2),
	TreaHouseInfo2#treaHouseInfo.baseBoxGetList.

check_open_treaHouse_baseBox(Pos) ->
	TreaHouseInfo = role_data:get_treaHouseInfo(),
	BoxProcess = data_treasure_box_baseReward:get(TreaHouseInfo#treaHouseInfo.mark),
	BoxOpenProcess = TreaHouseInfo#treaHouseInfo.baseBoxGetList,
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			if Pos > BoxProcess orelse Pos == 0 ->
				   {false, 3, BoxOpenProcess};
			   true ->
				   case lists:keyfind(Pos, #p_baseBoxOpenInfo.pos, BoxOpenProcess) of
					   false ->
						   {true, TreaHouseInfo,BoxOpenProcess};
					   _ ->
						   {false, 5, BoxOpenProcess}
				   end
			end;
		false ->
			{false, 255, BoxOpenProcess}
	end.

do_treaHouse_refresh()->
	TreaHouseInfo = role_data:get_treaHouseInfo(),
	CardList2 = random_treaHouse_list(),
	TreaHouseInfo2 = TreaHouseInfo#treaHouseInfo{card_list=CardList2},
	role_data:set_treaHouseInfo(TreaHouseInfo2),
	CardList2.

check_treaHouse_refresh()->
	Role = role_data:get_roleInfo(),
	RefreshNeedCoin = data_treasure_box:get(refresh_cost),
	case role_lib:check_money(Role, coin, RefreshNeedCoin) of
		false ->
			{false, 3};
		true ->
			role_lib:deduct_coin_f(Role, RefreshNeedCoin, ?MONEY_DEC_TYPE_TREAHOUSE_REFRESH, 0, ""),
			true
	end.
	
do_treaHouse_explore_ten(OpenTimes)->
	TreaHouseInfo = role_data:get_treaHouseInfo(),
	lists:foldl(fun(_,{InfoAcc, TreaHouseInfoAcc})->
						#treaHouseInfo{buy_count=BuyCount} = TreaHouseInfoAcc,
						TreaHouseInfoAcc2 = TreaHouseInfoAcc#treaHouseInfo{buy_count=BuyCount+1},
						SpecialID = data_fixed_treasure_gold:get(BuyCount + 1),
						{InfoT, TreaHouseInfoT,Reward} = do_treaHouse_explore_one(TreaHouseInfoAcc2,SpecialID,0),
						do_reward(Reward,2),
					 	{[InfoT|InfoAcc], TreaHouseInfoT}
					 end, {[], TreaHouseInfo}, lists:seq(1, OpenTimes)).

check_treaHouse_explore_ten() ->
	Role = role_data:get_roleInfo(),
	OneTimeNeedGold = data_treasure_box:get(oneTimeCost), 
	OpenTimes = calc_open_times(Role, OneTimeNeedGold),
	case role_lib:check_money(Role, gold, OneTimeNeedGold) of
		false ->
			{false, 3};
		true ->
			role_lib:deduct_money_f(Role, gold, OneTimeNeedGold * OpenTimes, ?MONEY_DEC_TYPE_TREAHOUSE_EXPLORE, OpenTimes, ""),
			{true, OpenTimes}
	end.

calc_open_times(#role{gold=GoldA, goldBonus=GoldB}, OneTimeNeedGold)->
	Gold = GoldA + GoldB,
	if Gold >= OneTimeNeedGold * 10 ->
		   10;
	   true ->
		   trunc(Gold/OneTimeNeedGold)
	end.

check_treaHouse_explore_one()->
	#treaHouseInfo{free_times=FreeTimes, free_count=FreeCount, buy_count=BuyCount} = TreaHouseInfo = role_data:get_treaHouseInfo(),
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			if FreeTimes > 0 ->
				   SpecialID = data_fixed_treasure_free:get(FreeCount + 1),
				   {true, TreaHouseInfo#treaHouseInfo{free_times=FreeTimes-1, free_count=FreeCount+1}, SpecialID};
			   true ->
				   Role = role_data:get_roleInfo(),
				   NeedGold = data_treasure_box:get(oneTimeCost),
				   case role_lib:check_money(Role, gold, NeedGold) of
					   false ->
						   {false,3};
					   true ->
						   SpecialID = data_fixed_treasure_gold:get(BuyCount + 1),
						   role_lib:deduct_money_f(Role, gold, NeedGold, ?MONEY_DEC_TYPE_TREAHOUSE_EXPLORE, 0, ""),
						   {true, TreaHouseInfo#treaHouseInfo{buy_count=BuyCount+1}, SpecialID}
				   end
			end;
		false ->
			{false, 255}
	end.

do_treaHouse_explore_one(TreaHouseInfo,SpecialID,Count )->
	if Count < 3 -> %% 计数防止死循环
		   PosList = data_treasure_box:get(pos_random),
		   Pos = 
			   case SpecialID of
				   X when is_integer(X) ->
					   X;
				   _ ->
					   
					   %util:random_one_from_list(PosList)
					   util:random_one_from_weigh_list(PosList)
			   end,
		   #treaHouseInfo{value_info=ValueInfo,card_list=CardList, mark=Mark} = TreaHouseInfo,
		   {value, {_, {Type, _Count, _}}=Val, CardList2} = lists:keytake(Pos, 1, CardList),
		   AddMark = data_treasure_box:get(add_mark),
		   %% 爆灯和双倍只有一个有效,所以如果抽取到的不是爆灯,则直接计算,如果抽取到的是爆灯,则判断是否是双倍状态,是双倍状态,重新抽取,如果不是双倍状态,则按概率随机抽取非双倍的pos
		   if Type =/= ?bomob_clo ->
				  NewCloset = get_one_box(Pos),
				  NewValueInfo = calc_value_info(ValueInfo, Type),
				  OpenList = [Val],
				  Reward = calc_reward(ValueInfo, Type, OpenList),
				  ReFillList = [NewCloset],
				  TreaHouseInfo2 = TreaHouseInfo#treaHouseInfo{value_info=NewValueInfo, card_list=[NewCloset|CardList2], mark=Mark+AddMark},
				  Info = cacl_explore_one_return(OpenList, ReFillList),
				  {Info, TreaHouseInfo2,Reward};
			  true ->
				  if ValueInfo > 1 ->
						 do_treaHouse_explore_one(TreaHouseInfo,?undefined, Count+1);
					 true ->
						 NewCloset = get_one_box(Pos),
						 %% 随机取出四个,然后选出不是双倍的三个
						 %% 						 OpenPosListT=util:random_list2(lists:delete(Pos, data_treasure_box:get(pos_list)),4),
						 %% 						 OpenPosList = lists:sublist(lists:keydelete(?double_clo, 1, OpenPosListT), 3),
						 OpenPosList = random_bomb_pos(Pos, 3, PosList, CardList),
						 {OpenList, NewValueInfo, ReFillList, NewCardList} = calc_bomb_info(CardList, OpenPosList, ValueInfo),
						 Reward = calc_reward(ValueInfo, Type, OpenList),
						 TreaHouseInfo2 = TreaHouseInfo#treaHouseInfo{value_info=NewValueInfo, card_list=NewCardList, mark=Mark+AddMark},
						 Info = cacl_explore_one_return([Val|OpenList], [NewCloset|ReFillList]),
						 {Info, TreaHouseInfo2,Reward}
				  end
		   end;
	   true ->
		   ?ERR("bad config in treaHouse, get bomb and double at the same time for too many times"),
		   Info = cacl_explore_one_return([],[]),
		   {Info, TreaHouseInfo, []}
	end.

random_bomb_pos(Pos, N, RandomList, CardList)->
	random_bomb_pos(Pos, N, RandomList, CardList, [],0).

random_bomb_pos(Pos, N, RandomList, CardList, List,Count)->
	case length(List) >= N orelse Count >= 20 of
		true ->
			List;
		_ ->
			TPos = util:random_one_from_weigh_list(RandomList),
			case lists:member(TPos, List) of
				true ->
					random_bomb_pos(Pos, N, RandomList, CardList, List,Count+1);
				_ ->
					{_,{Type2, _,_}}=lists:keyfind(TPos, 1, CardList),
					if Type2 =/= ?bomob_clo andalso Type2 =/= ?double_clo ->
						   random_bomb_pos(Pos, N, RandomList, CardList, [TPos|List],Count+1);
					   true ->
						   random_bomb_pos(Pos, N, RandomList, CardList, List,Count+1)
					end
			end
	end.

cacl_explore_one_return(OpenList, RefillList)->
	OpenCardList = treaHouseCard2p_treaHouse_card(OpenList),
	ReFillList = treaHouseCard2p_treaHouse_card(RefillList),
	#p_treaHouse_card_oneTime{openCardList=OpenCardList, newCardList=ReFillList}.

do_reward(RewardList,Type)->
	[begin
		 lists:foreach(fun(_)->role_reward:handle_treaHouse_reward_f(RewardType, Value,Type)end,lists:seq(1,Count))
	 end
	||{_,{RewardType, Count, Value}}<-RewardList].

calc_bomb_info(CardList, PosList,ValueInfo)->
%	{OpenList, ValueInfo2, ReFillList, NewCardList} = 
		lists:foldl(fun(Pos,{Acc1,Acc2, Acc3,CardListAcc})->
							{value, {_, {Type,_Count, _}}=Val, CardList2} = lists:keytake(Pos, 1, CardListAcc),
							NewCloset = get_one_box(Pos),
							if Type == ?bomob_clo ->
								   ?ERR("ERR config in treaHouse , too many bombs"),
								   {Acc1,Acc2,[NewCloset|Acc3],[NewCloset|CardList2]};
							   Type == ?double_clo ->
								   %?ERR("?ERR config in treaHouse, find double in bomb"),
								   {Acc1, calc_value_info(ValueInfo, Type), [NewCloset|Acc3],[NewCloset|CardList2]};
							   true ->
								   {[Val|Acc1],ValueInfo, [NewCloset|Acc3],[NewCloset|CardList2]}
							end
					end, {[],ValueInfo,[],CardList}, PosList).

calc_reward(ValueInfo, Type, ValList)->
	if Type == ?double_clo ->
		   [];
	   true ->
		   lists:foldl(fun(E,Acc)->
							   lists:duplicate(ValueInfo, E) ++ Acc
					   end, [], ValList)
	end.

calc_value_info(ValueInfo, Type) ->
	if Type =:= ?double_clo ->
		   case ValueInfo of
			   2 ->
				   4;
			   4 ->
				   8;
			   8 ->
				   8;
			   _ ->
				   2
		   end;
	   true ->
		   1
	end.


check_enter_treaHouse()->
	#role{level=Level} = role_data:get_roleInfo(),
	case Level >= data_treasure_box:get(level_limit) of
		true ->
			case check_open() of
				true ->
					true ;
				_ ->
					false
			end;
		_ ->
			false
	end.

check_open()->
	activityRank_server:is_treaHouse_activity_open().

check_enter_treaHouse2()->
	#role{level=Level} = role_data:get_roleInfo(),
	case Level >= data_treasure_box:get(level_limit) of
		true ->
			case check_open2() of
				true ->
					true ;
				_ ->
					false
			end;
		_ ->
			false
	end.

check_open2()->
	activityRank_server:is_treaHouse_activity_open2().

get_end_time()->
	EndTime = activityRank_server:get_treaHouse_activity_end_time(),
	case is_tuple(EndTime) of
		true ->
			EndTime;
		_ ->
			{0,0}
	end.

random_treaHouse_list()->
	PosList = data_treasure_box:get(pos_list),
	[get_one_box(P)||P<-PosList].

get_one_box(Pos)->
	RandomList = data_treasure_box_setting:get(Pos),
	BoxList = util:random_one_from_weigh_list(RandomList),
	Box = util:random_one_from_list(BoxList),
	{Pos, Box}.

treaHouseCard2p_treaHouse_card(CardList)->
	PosBaseID = data_treasure_box:get(pos_baseID),
	%% 	lists:foldr(fun({Pos,{Type, Value}},Acc)->
	%% 						if Type =:= 11 ->
	%% 							   PosType = 2, CardType = 0, CardValue = 0;
	%% 						   Type =:= 12 ->
	%% 							   PosType = 3, CardType = 0, CardValue = 0;
	%% 						   true ->
	%% 							   PosType = 1, CardType = Type , CardValue = Value
	%% 						end,
	%% 						[#p_treaHouse_card{pos=Pos, posType = PosType,cardType=CardType, value=CardValue}|Acc]
	%% 				end, [], CardList).
	[
	 begin
		 if Type =:= 11 ->
				PosType = 2, CardType = 0, Count=1,CardValue = 0;
			Type =:= 12 ->
				PosType = 3, CardType = 0, Count=1,CardValue = 0;
			true ->
				PosType = 1, CardType = Type , Count=Cnt,CardValue = Value
		 end,
		 #p_treaHouse_card{pos=Pos-PosBaseID, posType = PosType,cardType=CardType, count=Count,value=CardValue}
	 end
	 ||{Pos, {Type,Cnt, Value}}<-CardList].
