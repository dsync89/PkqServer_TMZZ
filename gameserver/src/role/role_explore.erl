%% @author admin
%% @doc 处理探索、遭遇站、夺宝、征收等功能
%% Created 2013-3-16


-module(role_explore).
-compile(export_all).
-include("def_role.hrl").
-include("def_explore.hrl").
-include("def_weibo_share.hrl").
%% API functions
-export([
		 ]).


%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
cs_explore_list(_) ->
	ChapterList = role_data:get_encounterList(),
	{_ExploreList, EncounterList} = 
		case ChapterList of
	 		{A,B} ->
				{A ,B};
			_ ->
				role_data:set_encounterList({[],[]}),
				{[],[]}
		end, 
	%PEchapterList = [echapter2p_echapter(E) || E<-ExploreList] ++ [echapter2p_echapter(E)||E<-EncounterList],
	%探索到后发生断线的未进行征收的事件，不返回给客户端
	PEchapterList = [] ++ [echapter2p_echapter(E)||E<-EncounterList],
	?sendself(#sc_explore_list{chapterList=PEchapterList}).

cs_explore_one(_) ->
	%ChapterList = role_data:get_encounterList(),
	case check_explore() of
		{true, RoleTimes} ->
			role_lib:deduct_dscv_f(RoleTimes, 1, RoleTimes#roleTimes.dscvCount),
			{RoleExp,GerExp,Coin,Crit,ItemList} = explore_reward(?undefined),
			?CATCH(role_task_trigger:handle({dispach_task,role_explore})),
			?sendself(#sc_explore_one{itemList=ItemList,result=1,gerExp=GerExp,roleExp=RoleExp,coin=Coin,crit=Crit});			
		{true, ChapterList, RoleTimes, NewCount, ChapterID, DataChapter} ->
			do_explore(ChapterList, RoleTimes, NewCount, ChapterID, DataChapter),
			?CATCH(role_task_trigger:handle({dispach_task,role_explore}));
		{false, Reason} ->
			?sendself(#sc_explore_one{result=Reason,itemList=[],roleExp=0,gerExp=0,coin=0,crit=0})
	end.

cs_explore_dungeon_list(#cs_explore_dungeon_list{chapterID=ChapterID}) ->
	{_ExploreList, EncounterInfoList} = role_data:get_encounterList(),
	case lists:keyfind(ChapterID, #echapter.id, EncounterInfoList) of
		false ->
			DungeonList = [];
		#echapter{dungeonList=DungeonList} ->
			next
	end,
	{_, EncounterList} = role_data:get_roleEncounterInfo(),
	case lists:keyfind(ChapterID, #t_encounter.chapterID, EncounterList) of
		#t_encounter{dungeonID=DungeonID, fightInfo=MonInfo,monTotalHp=MonTotalHp} ->
			if DungeonID =:= 0 ->
				   State = 0;
			   true ->
				   State=calc_mon_state(DungeonID, MonInfo,MonTotalHp)
			end;
		false ->
			State=100
	end,
	?sendself(#sc_explore_dungeon_list{dungeonList=DungeonList,chapterID=ChapterID,state=State}).

cs_explore_challenge_encounter(#cs_explore_challenge_encounter{dungeonID=DungeonID}) ->
	case check_challenge(DungeonID) of
		{true, DataDungeon, DataChapter, Chapter, ChapterList, RoleTimes} ->
			do_challenge(DataDungeon, DataChapter, Chapter, ChapterList, RoleTimes);
		{false, Reason} ->
			?sendself(#sc_explore_challenge_encounter{fightInfo=[],result=Reason,reward=[],score=0, state=0})
	end.

cs_explore_giveup_encounter(#cs_explore_giveup_encounter{chapterID=ChapterID}) ->
	case check_giveup(ChapterID) of
		{true, Chapter, NewChapterList} ->
			case Chapter#echapter.endTimerRef of
				{_,_}=TimerRef ->
					timer_wheel:cancel_plan(TimerRef);
				_ ->
					ignore
			end,
			{MonRank, EncounterList} = role_data:get_roleEncounterInfo(),
			EncounterList2 = lists:keydelete(ChapterID, #t_encounter.chapterID, EncounterList),
			role_data:set_roleEncounterInfo({MonRank, EncounterList2}),
			role_data:set_encounterList(NewChapterList),
			?sendself(#sc_explore_giveup_encounter{result=1});
		{false, Reason} ->
			?sendself(#sc_explore_giveup_encounter{result=Reason})
	end.

cs_explore_collect(#cs_explore_collect{chapterID=ChapterID}) ->
	case check_collect(ChapterID) of
		{true, Echapter, EncounterList, DataChapter} ->
			do_collect(ChapterID, Echapter, EncounterList, DataChapter); 
		{false, Reason} ->
			?sendself(#sc_explore_collect{chapterID=ChapterID,result=Reason})
			end.

cs_explore_force_collect(#cs_explore_force_collect{chapterID=ChapterID,type=Type}) ->
	case check_force_collect(ChapterID, Type) of
		{true, Echapter, EncounterList2, DataChapter, Info, DelAcc, UpdateAcc, UpdateLogList} ->
			do_force_collect(Echapter, EncounterList2, DataChapter, Type, Info, DelAcc, UpdateAcc, UpdateLogList);
		{false, Reason} ->
			?sendself(#sc_explore_force_collect{chapterID=ChapterID,type=Type,result=Reason})
			end.

cs_explore_auto_explore_check(#cs_explore_auto_explore_check{})->
	VipCharge = data_encounter:get(auto_explore_charge),
	?sendself(#sc_explore_auto_explore_check{result=VipCharge}).

cs_explore_free(_) ->
	case check_free() of
		{true, LimitInfo, NowFreeSeq, Price, Role} ->
			_Role2 = role_lib:deduct_gold_f(Role, Price, ?MONEY_DEC_TYPE_EXPLORE_FREE, NowFreeSeq, ""),
			LimitInfo2 = LimitInfo#limitInfo{encounterNum=NowFreeSeq},
			role_data:set_limitInfo(LimitInfo2),
			?notify_update(?ra_encounterFreeNum(NowFreeSeq)),
			?sendself(#sc_explore_free{result=1});			
		{false, Reason} ->
			?sendself(#sc_explore_free{result=Reason})
			end.

cs_explore_encounter_pass_reward(#cs_explore_encounter_pass_reward{chapterID=ChapterID})->
	case check_pass_chapter(ChapterID) of
		true ->
			{ExploreList, EncounterInfoList} = role_data:get_encounterList(),
			case lists:keytake(ChapterID, #echapter.id, EncounterInfoList) of
				false ->
					ignore;
				{value, _Chapter, NewChapterList} ->
					role_data:set_encounterList({ExploreList, NewChapterList})
			end,
			Role = role_data:get_roleInfo(),
			DataChapter = data_chapter:get(ChapterID),
			role_reward:handle_dungeon_reward_f(Role, DataChapter#data_chapter.perfectReward, ?MONEY_ADD_TYPE_ENCOUNTER_ALL_PASSED, ChapterID, ""),
			{MonRank, EncounterList} = role_data:get_roleEncounterInfo(),
			EncounterList2 = lists:keydelete(ChapterID, #t_encounter.chapterID, EncounterList),
			role_data:set_roleEncounterInfo({MonRank, EncounterList2}),
			?sendself(#sc_explore_encounter_pass_reward{result=1}),
			?sendself(#sc_explore_delete_encounter{chapterID=ChapterID});
		{false, Reason} ->
			?sendself(#sc_explore_encounter_pass_reward{result=Reason})
	end.

cs_explore_encounter_dungeon_state(#cs_explore_encounter_dungeon_state{chapterID=ChapterID})->
	{_, EncounterList} = role_data:get_roleEncounterInfo(),
	case lists:keyfind(ChapterID, #t_encounter.chapterID, EncounterList) of
		#t_encounter{dungeonID=DungeonID, fightInfo=MonInfo,monTotalHp=MonTotalHp} ->
			State=calc_mon_state(DungeonID, MonInfo,MonTotalHp),
			?sendself(#sc_explore_encounter_dungeon_state{result=1, chapterID=ChapterID, dungeonID=DungeonID, state=State});
		false ->
			?sendself(#sc_explore_encounter_dungeon_state{result=2, chapterID=ChapterID, dungeonID=0, state=0})
	end.

calc_mon_state(DungeonID, MonInfo,MonTotalHp)->
	if DungeonID =:= 0 ->
		   0;
	   true ->
		   Hp = 
			   lists:foldl(fun(#ger{gerHp=GerHp}, HpAcc)->
								   HpAcc+GerHp
						   end, 0, MonInfo),
		   if MonTotalHp =:= 0 ->
				  ?ERR("MonTotalHp = 0 in dungeon:~w",[DungeonID]),
				  0;
			   Hp =:= MonTotalHp ->
				  100;
			   Hp > 0 ->
				  erlang:trunc(100*Hp/MonTotalHp) + 1; %% hp向下取整等于进度向上取整，hp向上取整等于进度向下取整，进度=100-hp%
			  true ->
				  0
		   end
	end.

calc_mon_Total_Hp(MonInfo)->
	lists:foldl(fun(#ger{gerAttr=GerAttr}, MaxHpAcc)->
						MaxHpAcc+GerAttr#gerAttr.gerHpMax
				end, 0, MonInfo).

check_pass_chapter(ChapterID)->
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			{_,EncounterList} = role_data:get_roleEncounterInfo(),
			%?ERR("pass:~w",[EncounterList]),
			case lists:keyfind(ChapterID, #t_encounter.chapterID, EncounterList) of
				#t_encounter{dungeonID=DungeonID} ->
					if DungeonID =:= 0 ->
						   true;
					   true ->
						   {false, 3}
					end;
				false ->
					{false, 2}
			end;
		false ->
			{false, 255}
	end.

check_free() ->
	#limitInfo{encounterNum=FreeNum} = LimitInfo = role_data:get_limitInfo(),
	MaxFreeNum = data_common:get(free_encounter_num),
	if FreeNum >= MaxFreeNum ->
		   {false, 3};
	   true ->
		   NowFreeSeq = FreeNum+1,
		   FreePriceList = data_common:get(free_encounter_price),
		   if length(FreePriceList) >= NowFreeSeq ->
				  Price = lists:nth(NowFreeSeq, FreePriceList),
				  Role = role_data:get_roleInfo(),
				  case role_lib:check_money(Role, gold, Price) of
					  false ->
						  {false, 2};
					  true ->
						  {true, LimitInfo, NowFreeSeq, Price, Role}
				  end;
			  true ->
				  {false, 3}
		   end
	end.
	
get_max_num() ->
	LimitInfo = role_data:get_limitInfo(),
	LimitInfo#limitInfo.encounterNum+data_common:get(default_encounter_num).

do_force_collect(Echapter, EncounterList2, DataChapter, 1=Type, BagOther2, DelAcc, _UpdateAcc, UpdateLogList) ->
	role_data:set_bagItem(BagOther2),
	#role{roleID=RoleID} = Role = role_data:get_roleInfo(),
	%% 写道具日志
	LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogList),
	{Date, _} = Time = erlang:localtime(),
	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_FORCE_COLLECT, DataChapter#data_chapter.chapterID, ""),

	do_force_collect2(Echapter, EncounterList2, DataChapter, Type, Role);
do_force_collect(Echapter, EncounterList2, DataChapter, 2=Type, NeedReputation,_,_,_) ->
	Role = role_data:get_roleInfo(),
	Role2 = role_lib:deduct_reputation_f(Role, NeedReputation, ?MONEY_DEC_TYPE_FORCE_COLLECT, DataChapter#data_chapter.chapterID, ""),
	do_force_collect2(Echapter, EncounterList2, DataChapter, Type, Role2);
do_force_collect(Echapter, EncounterList2, DataChapter, 3=Type, NeedGold,_,_,_) ->
	Role = role_data:get_roleInfo(),
	Role2 = role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_FORCE_COLLECT, DataChapter#data_chapter.chapterID, ""),
	do_force_collect2(Echapter, EncounterList2, DataChapter, Type, Role2).
	
do_force_collect2(Echapter, EncounterList, DataChapter, Type, Role) ->
	#data_chapter{collectReward=CollectReward} = DataChapter,
	%Echapter2 = Echapter#echapter{isCollected=true},
	role_data:set_encounterList(EncounterList),
	role_reward:handle_sell_reward_f(Role, CollectReward, ?MONEY_ADD_TYPE_ENCOUNTER_COLLECT, DataChapter#data_chapter.chapterID, ""),
	Reply = #sc_explore_force_collect{chapterID=Echapter#echapter.id,result=1,type=Type},
	?sendself(Reply),
	role_invite:do_add_weibo_share_mark(?SHARE_TYPE_EXPLORE).
	
check_force_collect(ChapterID, Type) ->
	EncounterList = role_data:get_encounterList(),
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			case lists:keytake(ChapterID, #echapter.id, EncounterList) of
				false ->
					{false, 3};
				{value, #echapter{isCollected=IsCollected}=Echapter, EncounterList2} ->
					if IsCollected =:= true ->
						   {false, 6};
					   true ->
						   case data_chapter:get(ChapterID) of
							   #data_chapter{type=ChapterType} =DataChapter->
								   if ChapterType =:= ?chapter_type_collect_coin orelse ChapterType =:= ?chapter_type_collect_item ->
										  check_force_collect_type(Echapter, EncounterList2, DataChapter, Type);
									  true ->
										  {false, 5}
								   end;
							   _ ->
								   {false, 5}
						   end
					end
			end;
		false ->
			{false, 255}
	end.

check_force_collect_type(Echapter, EncounterList2, DataChapter, 1) ->
	#data_chapter{forceCollectItem=NeedItem}=DataChapter,
	case NeedItem of
		{ItemTypeID, Num} ->
			case item_lib:check_material(ItemTypeID, Num) of
				{true, BagOther2, DelAcc, UpdateAcc, UpdateLogList} ->
					{true, Echapter, EncounterList2, DataChapter, BagOther2, DelAcc, UpdateAcc, UpdateLogList};
				false ->
					{false, 4}
			end;
		_ ->
			{false, 5}
	end;
check_force_collect_type(Echapter, EncounterList2, DataChapter, 2) ->
	#data_chapter{forceCollectReputation=NeedReputation}=DataChapter,
	Role = role_data:get_roleInfo(),
	case role_lib:check_money(Role, reputation, NeedReputation) of
		false ->
			{false, 4};
		true ->
			{true, Echapter, EncounterList2, DataChapter, NeedReputation, [] ,[], []}
	end;
check_force_collect_type(Echapter, EncounterList2, DataChapter, 3) ->
	#data_chapter{forceCollectGold=NeedReputation}=DataChapter,
	Role = role_data:get_roleInfo(),
	case role_lib:check_money(Role, gold, NeedReputation) of
		false ->
			{false, 4};
		true ->
			{true, Echapter, EncounterList2, DataChapter, NeedReputation, [], [], []}
	end.
	
	
	
do_collect(ChapterID, _Echapter, {_ExploreList, EncounterList}, DataChapter) ->
	#data_chapter{collectReward=CollectReward} = DataChapter,
	%% 收取成功后，直接删除
	%Echapter2 = Echapter#echapter{isCollected=true},
	role_data:set_encounterList({[],EncounterList}),
	Role = role_data:get_roleInfo(),
	role_reward:handle_sell_reward_f(Role, CollectReward, ?MONEY_ADD_TYPE_ENCOUNTER_COLLECT, ChapterID, ""),
	Reply = #sc_explore_collect{chapterID=ChapterID,result=1},
	?sendself(Reply),
	broadcast_reward(CollectReward, Role),
	role_invite:do_add_weibo_share_mark(?SHARE_TYPE_EXPLORE).

broadcast_reward(Reward, Role)	->
	#role{roleName=RoleName}=Role,
	#sell_reward{item=Item ,newGer=Ger} = Reward,
	ItemList = get_list(Item),
	GerList = get_list(Ger),
	lists:foreach(fun(X)->#new_item{itemTypeID=ItemTypeID, itemNum=ItemNum, itemRank=ItemRank, itemLevel=ItemLevel} = X,
						  #data_item{itemStar=ItemStar,itemType=ItemType} = data_item:get(ItemTypeID),
						  IsEquip = item_lib:is_itemType_equip(ItemType),
						  if	ItemStar >= 3 andalso IsEquip -> 
									ItemView=#p_item_view{itemTypeID=ItemTypeID, itemLevel=ItemLevel, itemRank=ItemRank, itemNum=ItemNum},
									broadcast_server:bc_msgID(10012, [RoleName,ItemView ,erlang:integer_to_list(ItemNum)]);
								true	->
									ok
						  end
				  end, ItemList),
	lists:foreach(fun(X)->#new_ger{gerTypeID=GerTypeId, gerLevel=GerLevel, gerQuality=GerQuality}=X,
						  #data_ger{gerStar=GerStar}=data_ger:get(GerTypeId),
						  if	GerStar >= 3	->
									GerView=#p_ger_view{gerQuality=GerQuality, gerLevel=GerLevel, gerTypeID=GerTypeId},
									broadcast_server:bc_msgID(10011, [RoleName, GerView, "1"]);
								true	->
									ok
						  end
				  end, GerList).

get_list(List)	->
	if	List =:= 0	->
			[];
		is_tuple(List) ->
			[List];
		true	->
			List
	end.

check_collect(ChapterID) ->										 
	{ExploreList,EncounterList} = role_data:get_encounterList(),
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			case lists:keytake(ChapterID, #echapter.id, ExploreList) of
				false ->
					{false, 3};
				{value, #echapter{collectEndTime=CollectEndTime,isCollected=IsCollected}=Echapter, ExploreList2} ->
					if IsCollected =:= true ->
						   {false, 5};
					   true ->
						   case CollectEndTime > util:now() of
							   true ->
								   {false, 2};
							   false ->
								   case data_chapter:get(ChapterID) of
									   #data_chapter{type=Type} =DataChapter->
										   if Type =:= ?chapter_type_collect_coin orelse Type =:= ?chapter_type_collect_item ->
												  {true, Echapter, {ExploreList2, EncounterList}, DataChapter};
											  true ->
												  {false, 4}
										   end;
									   _ ->
										   {false, 4}
								   end
						   end
					end
			end;
		false ->
			{false, 255}
	end.
			
check_giveup(ChatperID) ->
	case data_chapter:get(ChatperID) of
		#data_chapter{canGiveUp=true} ->
			{ExploreList, EncounterList} = role_data:get_encounterList(),
			#data_chapter{type=Type} = data_chapter:get(ChatperID),
			case Type of
				1 ->
					case lists:keytake(ChatperID, #echapter.id, EncounterList) of
						false ->
							{false, 2};
						{value, Chapter, NewChapterList} ->
							{true, Chapter, {ExploreList, NewChapterList}}
					end;
				_ ->
					case lists:keytake(ChatperID, #echapter.id, ExploreList) of
						false ->
							{false, 2};
						{value, Chapter, NewChapterList} ->
							{true, Chapter, {NewChapterList, EncounterList}}
					end
			end;
		_ ->
			{false, 3}
	end.
do_challenge(#data_dungeon{chapterID=ChapterID,dungeonID=DungeonID}=DataDungeon, DataChapter, Chapter, ChapterList, RoleTimes) ->
	#role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
	%% 判断重新生成team还是读取历史team,	没找到则重新添加一个元素到EncounterList
	{MonRank, EncounterList} = EncounterInfo = role_data:get_roleEncounterInfo(),
	case lists:keyfind(ChapterID, #t_encounter.chapterID, EncounterList) of
		#t_encounter{}=Info ->
			case lists:keyfind(DungeonID, #t_encounter.dungeonID, EncounterList) of
				#t_encounter{attackTimes=AttackTimes,fightInfo=MonTeamInfo,monTotalHp=MonTotalHp}=Info->
					MonTeam=MonTeamInfo;
				_->
					AttackTimes=0,
					MonTeam = get_mon_list(DataDungeon, MonRank),
					MonTotalHp = calc_mon_Total_Hp(MonTeam)
			end;
		_ ->
			AttackTimes=0,
			MonTeam = get_mon_list(DataDungeon, MonRank),
			MonTotalHp = calc_mon_Total_Hp(MonTeam)
	end,
	RoleFighterList = role_data:get_fighter_list(),
	RoleLieuAdd = role_data:get_lieu_add_attr(),
	{Result, FightRecord, {_,_,_,FinalState}} = role_fight:new(RoleID,RoleFighterList, MonTeam,RoleLieuAdd, {0,0}, false),
	case Result of
		true ->
			do_challenge_win(Chapter, ChapterList, DataDungeon, FightRecord, RoleTimes, DataChapter, RoleInfo,AttackTimes,MonTotalHp);
		false ->
			do_challenge_fail(FightRecord,FinalState, EncounterInfo, DataDungeon,RoleTimes,MonTotalHp)
	end,
	ok.	

%% 动态计算遭遇战的怪物
get_mon_list(DataDungeon, MonRank) ->
	#data_dungeon{gerID2=MonList,gerID3=IDList} = DataDungeon,
	F = fun(E) ->
				GerTypeID = E#mon.gerTypeID,
				GerQuality = E#mon.gerQuality,
				ger_attr:new_mon(GerTypeID, E#mon.gerLevel, GerQuality+MonRank, [], lists:delete(GerTypeID, IDList))
		end,
	[?change_pos(Ger, Pos)||{Pos, Mon} <- MonList, is_record(Mon, mon), Ger<-[F(Mon)], is_record(Ger, ger)].

do_challenge_fail(FightRecord, NewState, {MonRank, EncounterList},DataDungeon, RoleTimes, MonTotalHp) ->
	#data_dungeon{chapterID=ChapterID,dungeonID=DungeonID, costEnergy=NeedEnergy,gerID3=IDList}=DataDungeon,
	role_lib:deduct_energy_f(RoleTimes, NeedEnergy),
	case lists:keyfind(ChapterID, #t_encounter.chapterID, EncounterList) of
		#t_encounter{attackTimes=AttackTimes}->
		%% 更新怪物的hp和sp
			FightInfo2 = rebuild_mon_ger_info(NewState,IDList),
			EncounterList2 = 
				lists:keyreplace(ChapterID, #t_encounter.chapterID, EncounterList
								, #t_encounter{attackTimes=AttackTimes+1, chapterID=ChapterID, dungeonID=DungeonID, fightInfo=FightInfo2,monTotalHp=MonTotalHp});
		_ ->
			FightInfo2 = rebuild_mon_ger_info(NewState,IDList),
			EncounterList2 = [#t_encounter{attackTimes=1, chapterID=ChapterID, dungeonID=DungeonID, fightInfo=FightInfo2,monTotalHp=MonTotalHp}|EncounterList]
	end,
	role_data:set_roleEncounterInfo({MonRank, EncounterList2}),
	State = calc_mon_state(DungeonID, FightInfo2,MonTotalHp),
	Reply = #sc_explore_challenge_encounter{result=1, fightInfo=[FightRecord],reward=[],score=0, state=State},
	?sendself(Reply).

rebuild_mon_ger_info(NewState,IDList)->
	lists:foldl(fun(#ger{gerBase=Base, gerHp=Hp},GerAcc)->
						if Hp =< 0->
							   GerAcc;
						   true ->
							   GerPos = Base#gerBase.gerPos,
							   Base2=Base#gerBase{gerPos = -GerPos},
							   Ger2 = ger_attr:new_mon(Base#gerBase.gerTypeID, Base#gerBase.gerLevel,Base#gerBase.gerQuality, [], IDList),
							   %[Ger#ger{gerBase=Base2,gerExtra=0,gerSp=0}|GerAcc]
								[Ger2#ger{gerHp=Hp,gerBase=Base2}|GerAcc]
						end
				end, [], NewState).

do_challenge_win(Chapter, {ExploreList, EncounterInfoList}, DataDungeon, FightRecord, RoleTimes, DataChapter, RoleInfo,AttackTimes,MonTotalHp) ->
	{MonRank, EncounterList} = role_data:get_roleEncounterInfo(),
	#data_dungeon{chapterID=ChapterID,reward=Reward, costEnergy=NeedEnergy,dungeonID=DungeonID, pass_num=PassNum,sufDungeonID=SufDungeonID} = DataDungeon,
	if AttackTimes < PassNum ->
		   MonRank2 = MonRank+1;
	   true ->
		   MonRank2 = MonRank
	end,
	%% 先扣体力，后面发奖励时有可能主公升级，会刷满体力
	role_lib:deduct_energy_f(RoleTimes, NeedEnergy),
	
	{_, GerAddExpList, RewardItemList, RewardGerList} = role_reward:handle_dungeon_reward_f(RoleInfo, Reward, ?MONEY_ADD_TYPE_ENCOUNTER_CHALLENGE, DungeonID, ""),
	RewardItemView = [item_lib:new_item2p_item_view(E) || E<- RewardItemList],
	RewardGerView = [ger_lib:new_ger2p_ger_view(E) || E<- RewardGerList],
	Dungeon2 = #p_edungeon{dungeonID=DungeonID,isPassed=true},
	#echapter{dungeonList=DungeonList} = Chapter,
	Reward2 = role_lib:reward2p_reward(Reward),
	Reward3 = Reward2#p_reward{gerExpList=GerAddExpList, itemList=RewardItemView, gerList=RewardGerView},
	Reply = #sc_explore_challenge_encounter{result=1, fightInfo=[FightRecord],reward=[Reward3],score=0, state=0},
	?sendself(Reply),
	%% 判断是否完成关卡
	#data_chapter{dungeonIDList=DungeonIDList, chapterID=ChapterID} = DataChapter,
	case length(DungeonList) +1 >= length(DungeonIDList) of
		true ->
			case lists:keyfind(ChapterID, #t_encounter.chapterID, EncounterList) of
				false ->
					EncounterList2 = [#t_encounter{attackTimes=0, chapterID=ChapterID, dungeonID=0, fightInfo=0,monTotalHp=MonTotalHp}|EncounterList];
				_ ->
					EncounterList2=lists:keyreplace(ChapterID, #t_encounter.chapterID, EncounterList, #t_encounter{attackTimes=0, chapterID=ChapterID, dungeonID=0, fightInfo=0,monTotalHp=MonTotalHp})
			end;
		false ->
			MonTeam = get_mon_list(data_dungeon:get(SufDungeonID), MonRank2),
			MonTotalHp2 = calc_mon_Total_Hp(MonTeam),
			NewEncounterInfo = #t_encounter{attackTimes=0, chapterID=ChapterID, dungeonID=SufDungeonID, fightInfo=MonTeam,monTotalHp=MonTotalHp2},
			EncounterList2 = lists:keyreplace(ChapterID, #t_encounter.chapterID, EncounterList, NewEncounterInfo)
	end, 
	Chapter2 = Chapter#echapter{dungeonList=[Dungeon2|DungeonList]},
	ChapterList2 = {ExploreList, lists:keyreplace(ChapterID, #echapter.id, EncounterInfoList, Chapter2)},
	role_data:set_roleEncounterInfo({MonRank2, EncounterList2}),
	role_data:set_encounterList(ChapterList2).

gen_mon_team(DataDungeon) ->
	#data_dungeon{gerID1=D1,gerID2=D2,gerID3=D3,gerID4=D4,gerID5=D5,gerID6=D6}=DataDungeon,				 
	List = [{1,D1},{2,D2},{3,D3},{4,D4},{5,D5},{6,D6}],
	[?change_pos(Ger, Pos)||{Pos, ID} <- List, is_integer(ID), ID > 0, Ger<-[data_mon:get(ID)], is_record(Ger, ger)].

check_challenge(DungeonID) ->
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			case data_dungeon:get(DungeonID) of
				?undefined ->
					{false, 6};
				#data_dungeon{chapterID=ChapterID,preDungeonID=PreDungeonID, costEnergy=NeedEnergy}=DataDungeon ->
					#roleTimes{energy=Energy} = RoleTimes = role_data:get_roleTimes(),
					case Energy >= NeedEnergy of
						false ->
							{false, 2};
						true ->
							case data_chapter:get(ChapterID) of
								?undefined ->
									{false, 7};
								#data_chapter{} = DataChapter ->
									{_ExploreList, EncounterList}=ChapterList = role_data:get_encounterList(),
									%% 是否拥有此章节
									case lists:keyfind(ChapterID, #echapter.id, EncounterList) of
										false ->
											{false, 5};
										#echapter{dungeonList=DungeonList}=Chapter ->
											%% 是否已挑战
											case lists:keyfind(DungeonID, #p_edungeon.dungeonID, DungeonList) of
												false ->
													%% 检查此关卡是否激活
													case (PreDungeonID=:=0 orelse lists:keyfind(PreDungeonID, #p_edungeon.dungeonID, DungeonList) =/= false) of
														true ->
															{true, DataDungeon, DataChapter, Chapter, ChapterList, RoleTimes};
														false ->
															{false, 4}
													end;
												_ ->
													{false, 3}
											end
									end
							end
					end
			end;
		false ->
			{false, 255}
	end.



%% 检查是否能成功探索
%% exploreList 只保存当前探索到的内容，下次探索后，exploreList会被清空。
%% encounterList 只保存遭遇战内容，需要去重。
check_explore() ->
	RoleTimes = role_data:get_roleTimes(),
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			case RoleTimes#roleTimes.discoveryTimes > 0 of
				true ->
					{_ExploreList, EncounterList} = ChapterList = role_data:get_encounterList(),
					#role{level=Level} = _RoleInfo = role_data:get_roleInfo(),
					case random_explore(EncounterList, RoleTimes, Level) of
						false ->
							{true,RoleTimes};
						{NewCount, ChapterID} ->
							case data_chapter:get(ChapterID) of
								?undefined ->
									{true,RoleTimes};
								#data_chapter{type=Type}=DataChapter ->
									if Type =:= 1 ->
										   case length(EncounterList) < get_max_num() of
											   true ->
												   {true, ChapterList, RoleTimes, NewCount, ChapterID, DataChapter};
											   _ ->
												   {true, RoleTimes}
										   end;
									   true ->
										   {true, ChapterList, RoleTimes, NewCount, ChapterID, DataChapter}
									end
							end
					end;
				false ->
					{false, 3}
			end;
		false ->
			{false, 255}
	end.

do_explore({ExploreList, EncounterList}=_ChapterList, RoleTimes, NewCount, ChapterID, DataChapter) ->
    role_lib:deduct_dscv_f(RoleTimes, 1, NewCount),
	#data_chapter{type=ChapterType} = DataChapter,
	if ChapterType == ?chapter_type_item orelse ChapterType == ?chapter_type_ger ->
		   role_shop:new_randomShop(DataChapter#data_chapter.shopID),
		   {RoleExp,GerExp,Coin,Crit,ItemList} = explore_reward(?undefined),
		   ?sendself(#sc_explore_one{result=6,itemList=ItemList,roleExp=RoleExp,gerExp=GerExp,coin=Coin,crit=Crit});
	   true ->
		   NewChapter = new_chapter(DataChapter),
		   case ChapterType of
			   ?chapter_type_encounter ->
				   EncounterList2 = lists:keydelete(ChapterID, #t_encounter.chapterID, EncounterList),
				   ChapterList2 = {ExploreList, [NewChapter|EncounterList2]},
                   {RoleExp,GerExp,Coin,Crit,ItemList} = explore_reward(?undefined);
			   _ ->
				   ChapterList2 = {ExploreList, EncounterList},
                   {RoleExp,GerExp,Coin,Crit,ItemList} = explore_reward(DataChapter)
		   end,
		   role_data:set_encounterList(ChapterList2),
		   Reply = #sc_explore_one{itemList=ItemList,result=1,roleExp=RoleExp,gerExp=GerExp,coin=Coin,crit=Crit},
		   ?sendself(Reply)
	end.

explore_reward(DataChapter) ->
    #role{level=Level} = RoleInfo = role_data:get_roleInfo(),
    #data_explore_reward{roleExp=RoleExpBase,gerExp=GerExpBase,coin=CoinBase} = data_explore_reward:get(Level),
    CritProb = data_common:get(explore_crit_prob),
    Crit = random_explore_reward_crit(CritProb),
    RoleExp = trunc(Crit*RoleExpBase),
    GerExp = trunc(Crit*GerExpBase),
    Coin = trunc(Crit*CoinBase),
    {Role2, _} = role_reward:reward_normal(RoleInfo, Coin, 0, GerExp, RoleExp, 0, ?MONEY_ADD_TYPE_DO_EXPLORE, 0, ""),
    role_data:set_roleInfo(Role2),
    case DataChapter of
        #data_chapter{chapterID=ChapterID, collectReward=CollectReward} ->
            role_reward:handle_sell_reward_f(Role2, CollectReward, ?MONEY_ADD_TYPE_DO_EXPLORE, ChapterID, ""),
            broadcast_reward(CollectReward, Role2),
            role_invite:do_add_weibo_share_mark(?SHARE_TYPE_EXPLORE),
            #sell_reward{item=ItemList} = CollectReward,
            ItemList2 = lists:map(fun(#new_item{itemTypeID=ItemTypeID,itemNum=ItemNum}) ->
                                          #p_id_num{typeID=ItemTypeID,num=ItemNum}
                                  end, ItemList);
        _ ->
            ItemList2 = []
    end,
    {RoleExp, GerExp, Coin, Crit, ItemList2}.

random_explore_reward_crit(Prob) ->
	Random = random:uniform(),
	util:foldl(fun({E, UnitWeigh}, Acc) ->
						if UnitWeigh + Acc > Random ->
							   {return, E};
						   true ->
							   Acc + UnitWeigh
						end
		  end, 0, Prob).
	
echapter2p_echapter(Echapter) ->
	#echapter{id=ChapterID,endTimerRef=EndTimerRef,isCollected=IsCollected,collectEndTime=CollectEndTime} = Echapter,
	#p_echapter{chapterID=ChapterID,endTime=endTime(EndTimerRef),isCollected=IsCollected,value=CollectEndTime}.

endTime({EndTime,_}) ->
	EndTime;
endTime(_) ->
	-1.
	
%% 创建一个新的奇遇章节
new_chapter(DataChapter) ->
	#data_chapter{closeTime=CloseSec, chapterID=ChapterID, type=Type, collectSec=CollectSec} = DataChapter,	
	NowSec = util:now(),
	case CloseSec >0 of
		true ->
			EndTime = NowSec+CloseSec,
			TimerRef=timer_wheel:plan(EndTime, fun() -> del_chapter(ChapterID) end);
		false ->
			TimerRef=0
	end,
	case Type of
		?chapter_type_encounter -> #echapter{endTimerRef=TimerRef, id=ChapterID};
		?chapter_type_item ->#echapter{dungeonList=DataChapter#data_chapter.shopID,endTimerRef=TimerRef, id=ChapterID};
		?chapter_type_ger->#echapter{dungeonList=DataChapter#data_chapter.shopID,endTimerRef=TimerRef, id=ChapterID};
		?chapter_type_collect_coin->#echapter{endTimerRef=TimerRef,id=ChapterID,collectEndTime=NowSec+CollectSec,isCollected=false};
		?chapter_type_collect_item->#echapter{endTimerRef=TimerRef,id=ChapterID,collectEndTime=NowSec+CollectSec,isCollected=false}
	end.

del_chapter(ChapterID) ->
	{ExploreList, EncounterList} = role_data:get_encounterList(),
	#data_chapter{type=Type} = data_chapter:get(ChapterID),
	case Type of
		1 ->
			ChapterList2 = {ExploreList, lists:keydelete(ChapterID, #echapter.id, EncounterList)},
			{MonRank, Encounter} = role_data:get_roleEncounterInfo(),
			Encounter2 = lists:keydelete(ChapterID, #t_encounter.chapterID, Encounter),
			role_data:set_roleEncounterInfo({MonRank, Encounter2});
		_ ->
			ChapterList2 = {lists:keydelete(ChapterID, #echapter.id, ExploreList), EncounterList}
	end,
	?sendself(#sc_explore_delete_encounter{chapterID=ChapterID}),
	role_data:set_encounterList(ChapterList2).

%% 测试需要多少次探索，才能随处某一种章节
test_random(N,Type,Level) ->
	test_random(1,N,Type,Level).
test_random(Max,Max,_Type,_Level) ->
	fail;
test_random(Count,Max, Type,Level) ->
	case random_explore([], #roleTimes{dscvCount=Count}, Level) of
		{Count2,ChapterID} ->
			#data_chapter{type=TmpType} = data_chapter:get(ChapterID),
			if TmpType =:= Type ->
				   {Count,ChapterID};
			   true ->
				   test_random(Count2,Max,Type,Level)
			end
	end.
			   
%% 固定关卡
random_explore(ChapterList, #roleTimes{dscvCount=Count}, Level) ->
	Count2 = Count +1,
	case data_fixed_encounter:get(Count2) of
		?undefined ->
			random_explore2(ChapterList, Count2, Level,30);
		List ->
			MainGerTypeID = role_data:get_mainGerTypeID(),
			{_, ChapterID} = lists:keyfind(MainGerTypeID, 1, List),
			{Count2, ChapterID}
	end.

%% Retry参数防止配置错误导致死循环。
random_explore2(ChapterList, Count, Level, Retry) ->
	WeighList = data_encounter:get(Level),
	random_explore3(ChapterList, Count, Level, Retry, WeighList).

random_explore3(ChapterList, Count, Level, Retry, WeighList) when Retry >0->
	ChapterIDList = util:random_one_from_weigh_list(WeighList),
	if ChapterIDList == [] ->
		   false;
	   true ->
		   ChapterID = util:random_one_from_list(ChapterIDList),
		   case lists:keyfind(ChapterID, #echapter.id, ChapterList) of
			   false ->
				   {Count, ChapterID};
			   _ ->
				   random_explore3(ChapterList, Count+1, Level, Retry-1, WeighList)
		   end
	end;
random_explore3(_ChapterList, _Count, _Level, _Reply, _WeighList) ->
	false.


%% @doc 初始化固定抽取配置
fixed_encounter_config_va([MainGerTypeIDList| ConfigList]) ->
	IDList = MainGerTypeIDList,
	IDList2= IDList ++ [0],
	L = 
	[begin
		 case length(F) of
			 3 ->
		 		{E, lists:zip(IDList,F)};
			 4 ->
				 {E, lists:zip(IDList2, F)}
		 end
	 end ||{E,F}<-ConfigList],
	[{?mainGerTypeID,IDList}|L].
		   
%% ====================================================================
%% Internal functions
%% ====================================================================


