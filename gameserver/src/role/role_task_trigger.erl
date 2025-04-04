%% @author crimoon26
%% @doc @todo Add description to role_task_trigger.


-module(role_task_trigger).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-compile(export_all).

-include("def_role.hrl").
-include("def_task.hrl").
%% ====================================================================
%% Internal functions
%% ====================================================================

handle({dispach_task,role_up_level,_RoleID,Level})->
	?CATCH(dispach_task(?TASK_TRIGGER_ID_ROLE_UP_LEVLE,Level));
handle({dispach_task,ger_up_level,_GerID,GerTypeID,Level})->
	dispach_task(?TASK_TRIGGER_ID_GER_UP_LEVEL_TIMES,GerTypeID),
	dispach_task(?TASK_TRIGGER_ID_GER_UP_LEVEL,Level);
handle({dispach_task,dungeon_pass,_RoleID,DungeonID,Type,Times})->
	case Type of
		?BATTLE_DUNGEON_TYPE_NORMAL->
			dispach_task(?TASK_TRIGGER_ID_DUNGEON_NORMAL_PASS,{DungeonID,Times});
		?BATTLE_DUNGEON_TYPE_HARD->
			dispach_task(?TASK_TRIGGER_ID_DUNGEON_HARD_PASS,{DungeonID,Times});
		?BATTLE_DUNGEON_TYPE_FAST_HARD->
			dispach_task(?TASK_TRIGGER_ID_DUNGEON_FAST_HARD_PASS,{DungeonID,Times});
		_->
			ignore
	end,
	dispach_task(?TASK_TRIGGER_ID_DUNGEON_PASS,{Type,Times});
handle({dispach_task,kill_monster,[]})->
	ignore;
handle({dispach_task,kill_monster,KillMonsterList})->
	dispach_task(?TASK_TRIGGER_ID_KILL_MONSTER,KillMonsterList);
handle({dispach_task,equip_strong,_RoleID,EquipUID,EquipType,Level,Num})->
	dispach_task(?TASK_TRIGGER_ID_EQUIP_STRONG,{EquipType,Level}),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_STRONG_TIMES,{Num,EquipType}),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_STRONG_1,{EquipUID,Level});
handle({dispach_task,equip_up_quality,_RoleID,EquipUID,EquipTypeID,Quality})->
	dispach_task(?TASK_TRIGGER_ID_EQUIP_UP_QUALITY,{EquipTypeID,Quality}),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_TIMES,EquipTypeID),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_1,{EquipUID,Quality});
handle({dispach_task,ger_up_quality,GerUID,GerTypeID,Quality})->
	dispach_task(?TASK_TRIGGER_ID_GER_UP_QUALITY_TIMES,GerTypeID),
	dispach_task(?TASK_TRIGGER_ID_GER_UP_QUALITY,{GerUID,Quality});
handle({dispach_task,equip_up_equip,EquipUID,EquipType})->
	dispach_task(?TASK_TRIGGER_ID_EQUIP_UP_EQUIP,{EquipUID,EquipType});
handle({dispach_task,gain_ger,GerUID,GerTypeID,Star,Quality,Level})->
	case lists:member(GerTypeID, [2,3,4,5,6]) of
		true->
			ignore;
		false->
			dispach_task(?TASK_TRIGGER_ID_GER_UP_LEVEL,Level),
			dispach_task(?TASK_TRIGGER_ID_GER_UP_QUALITY,{GerUID,Quality}),
			dispach_task(?TASK_TRIGGER_ID_GER_GAIN_GER,{GerUID,GerTypeID}),
			dispach_task(?TASK_TRIGGER_ID_GER_GAIN_STAR_GER,{GerUID,Star})
	end;
handle({dispach_task,create_equip,EquipUID,EquipType,Star,Quality,Level})->%%创建物品的时候可能很多操作
	dispach_task(?TASK_TRIGGER_ID_EQUIP_TOTAL_EQUIP,{EquipUID,EquipType}),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_GAIN_STAR_EQUIP,{EquipUID,Star}),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_UP_QUALITY,{EquipUID,Quality}),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_1,{EquipUID,Quality}),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_STRONG_1,{EquipUID,Level}),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_STRONG,{EquipUID,Level});
handle({dispach_task,role_explore})->
	dispach_task(?TASK_TRIGGER_ID_EXPLORE_TIMES, undefined);
handle({dispach_task,role_combime,Type})->
	dispach_task(?TASK_TRIGGER_ID_COMBINE_TIMES, undefined),
	case Type of
		random_combine_equip->
			dispach_task(?TASK_TRIGGER_ID_COMBINE_RANDOM_TIMES, undefined);
		normal_combine_equip->
			dispach_task(?TASK_TRIGGER_ID_COMBINE_NORMAL_TIMES, undefined);
		random_combine_ger->
			dispach_task(?TASK_TRIGGER_ID_COMBINE_RANDOM_TIMES, undefined),
			dispach_task(?TASK_TRIGGER_ID_COMBINE_GER_RANDOM_TIMES, undefined);
		normal_combine_ger->
			dispach_task(?TASK_TRIGGER_ID_COMBINE_NORMAL_TIMES, undefined)
	end;
handle({dispach_task,role_add_reward,Num,Type})->
	dispach_task(?TASK_TRIGGER_ID_ADD_REWARD,{Num,Type});
handle({dispach_task,role_add_friend,FriendRoleID,IsDeffSex})->
	dispach_task(?TASK_TRIGGER_ID_ADD_FRIEND_NUM,FriendRoleID),
	case IsDeffSex of
		true->
			dispach_task(?TASK_TRIGGER_ID_ADD_DIFF_SEX_FRIEND,FriendRoleID);
		false->
			ignore
	end;
handle({dispach_task,role_extract_card,Times})->
	dispach_task(?TASK_TRIGGER_ID_EXTRACT_CARD_TIMES,Times);
handle({dispach_task,role_join_hron})->
	dispach_task(?TASK_TRIGGER_ID_JOIN_HRON_TIMES,1);
handle({dispach_task,role_join_hula})->
	dispach_task(?TASK_TRIGGER_ID_JOIN_HULA_TIMES,1);
handle({dispach_task,role_join_nanm})->
	dispach_task(?TASK_TRIGGER_ID_JOIN_NANM_TIMES,1);
handle({dispach_task,role_add_enargy_to_friend})->
	dispach_task(?TASK_TRIGGER_ID_ADD_ENARGY_TO_FRIEND_TIMES,1);
handle({dispach_task,role_mating_to_friend})->
	dispach_task(?TASK_TRIGGER_ID_MATING_TO_FRIEND_TIMES,1);
handle({dispach_task,role_fight_power,FightPower})->
	dispach_task(?TASK_TRIGGER_ID_ROLE_FIGHT_POWER,FightPower);
handle({dispach_task,role_pvp_fight,Rank})->
	case Rank of
		0->
			ignore;
		_->
			dispach_task(?TASK_TRIGGER_ID_ROLE_PVP_RANK,Rank)
	end,
	dispach_task(?TASK_TRIGGER_ID_PVP_FIGHT_TIMES,1);
handle({dispach_task,role_pass_hron,N})->
	dispach_task(?TASK_TRIGGER_ID_PASS_HRON_DUNGEON,N);
handle({dispach_task,role_allequipment,N})->
	dispach_task(?TASK_TRIGGER_ID_ALLEQUIPMENT,N);
handle({dispach_task,role_up_ger_num,N})->
	dispach_task(?TASK_TRIGGER_ID_UP_GER_NUM,N);
handle({dispach_task,role_ger_move_pos_times})->
	dispach_task(?TASK_TRIGGER_ID_REPLACE_UP_GER_TIMES,1);
handle({dispach_task,role_chat_times})->
	dispach_task(?TASK_TRIGGER_ID_CHAT_TIMES,1);
handle({dispach_task,role_sign_race_times})->
	dispach_task(?TASK_TRIGGER_ID_SIGN_RACE_TIMES,1);
handle({dispach_task,role_change_head_times})->
	dispach_task(?TASK_TRIGGER_ID_CHANGE_HEAD_TIMES,1);
handle({dispach_task,role_buy_coin_times})->
	dispach_task(?TASK_TRIGGER_ID_BUY_COIN_TIMES,1);
handle({dispach_task,role_active_destiny_num,Num})->
	dispach_task(?TASK_TRIGGER_ID_ACTIVE_DESTINY_NUM,Num).
	
add_ger_trigger(GerSimpleList)->
	lists:foreach(fun(#gerSimple{gerTypeID=GerTypeID,gerID=GerID,gerQuality=Quality,gerLevel=Level})->
						  #data_ger{gerStar=GerStar} = data_ger:get(GerTypeID),
						  role_task_trigger:handle({dispach_task,gain_ger,GerID,GerTypeID,GerStar,Quality,Level})
				  end, GerSimpleList).
	
add_item_trigger(ItemList)->
	lists:foreach(fun(#item{itemUID=ItemUID,itemTypeID=ItemTypeID,itemType=ItemType,itemRank=Quality,itemLevel=ItemLevel})->
						  case item_lib:is_itemType_equip(ItemType) of
							  true->
								  #data_item{itemStar=ItemStar} = data_item:get(ItemTypeID),
								  role_task_trigger:handle({dispach_task,create_equip,ItemUID,ItemTypeID,ItemStar,Quality,ItemLevel});
							  false->
								  ignore
						  end
				  end, ItemList).
	

dispach_task(TriggerID,TriggerData)->
	TaskIDList = role_task:get_trigger_task_id(TriggerID),
	disapach_task(TriggerID,TriggerData,TaskIDList).
	
disapach_task(_TriggerID,_TriggerData,[])->
	ignore;
disapach_task(TriggerID,TriggerData,[TaskID|TailTaskIDList])->
	trigger_task(TriggerID,TaskID,TriggerData),
	disapach_task(TriggerID,TriggerData,TailTaskIDList).


trigger_task(TriggerID,TaskID,TriggerData)->
	#data_task{trigger_num=Num,trigger_int_list=IntList,task_type=TaskType} = data_task:get(TaskID),
	AllTaskList = role_data:get_task_list(TaskType),
	CheckFun = fun(#r_task{task_id=TID}) ->TID =:= TaskID end,
	case util:fun_take(CheckFun, AllTaskList) of
		false->
			ignore;
		{value,Task,TailTaskList}->
			#r_task{status=Status,trigger_num=CurrNum,trigger_notes=TriggerNotes} = Task,
			case Status of
				?TASK_STATUS_WAS_ACCEPT->
					case check_data(TriggerID,TriggerData,CurrNum,TriggerNotes,Num,IntList) of
						true->
							role_task:do_change_task_status(Task,TailTaskList);
						{true,NewNum}->
							NewTask = Task#r_task{trigger_num=NewNum},
							role_task:do_change_task_status(NewTask,TailTaskList);
						{true,NewNum,NewTriggerNotes} ->
							NewTask = Task#r_task{trigger_notes=NewTriggerNotes,trigger_num=NewNum},
							role_task:do_change_task_status(NewTask,TailTaskList);
						{false,NewNum}->
							case role_task:transaction(fun()->
													 NewTask = Task#r_task{trigger_num=NewNum},
													 role_data:set_task_list(TaskType,[NewTask|TailTaskList]),
													 NewTask
											 end) of
								{atomic,NewTask}->
									role_task:send_notify_change(TaskType,[NewTask],[]);
								{abort,Reason}->
									?ERR("触发任务时发生错误:~w~n任务数据为:~w",[Reason,Task]),
									ok
							end;
						{false,NewNum,NewTriggerNotes}->
							case role_task:transaction(fun()->
													 NewTask = Task#r_task{trigger_num=NewNum,trigger_notes=NewTriggerNotes},
													 role_data:set_task_list(TaskType,[NewTask|TailTaskList]),
													 NewTask
											 end) of
								{atomic,NewTask}->
									role_task:send_notify_change(TaskType,[NewTask],[]);
								{abort,Reason}->
									?ERR("触发任务时发生错误:~w~n任务数据为:~w",[Reason,Task]),
									ok
							end;
						false->
								ignore
					end;
				_->
					ignore
			end
	end.
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,IntList) when TriggerID=:=?TASK_TRIGGER_ID_DUNGEON_NORMAL_PASS 
																		   orelse TriggerID=:=?TASK_TRIGGER_ID_DUNGEON_HARD_PASS 
																		   orelse TriggerID=:=?TASK_TRIGGER_ID_DUNGEON_FAST_HARD_PASS 
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_DUNGEON_PASS->
	NewCurrNum = get_next_num_7(TriggerData,CurrNum,IntList),
	return_2(NewCurrNum,CurrNum,Num);
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,IntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_GER_UP_QUALITY_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_GER_UP_LEVEL_TIMES->
	NewCurrNum = get_next_num_2(TriggerData,CurrNum,IntList),
	return_2(NewCurrNum,CurrNum,Num);
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,IntList) when TriggerID=:=?TASK_TRIGGER_ID_KILL_MONSTER->
	MonsterID = 
		case IntList of
			[]->
					0;
			[MID]->
				MID
		end,
		Sum = lists:foldl(fun({MID,N},T)->
							  case MonsterID=:=0 orelse MonsterID=:=MID of
								  true->
									  T+N;
								  false->
									  T
							  end
					  end, 0, TriggerData),
	NewCurrNum = CurrNum + Sum,
	return_2(NewCurrNum,CurrNum,Num);
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,IntList) when  TriggerID=:=?TASK_TRIGGER_ID_EQUIP_STRONG
														 orelse	 TriggerID=:=?TASK_TRIGGER_ID_EQUIP_UP_QUALITY->
	NewCurrNum = get_next_num_1(TriggerData,Num,IntList),
	return_2(NewCurrNum,CurrNum,Num);
check_data(TriggerID,TriggerData,CurrNum,TriggerNotes,Num,IntList) when TriggerID=:=?TASK_TRIGGER_ID_GER_UP_QUALITY
																		orelse TriggerID=:=?TASK_TRIGGER_ID_EQUIP_STRONG_1
																		orelse TriggerID=:=?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_1 ->
	{NewCurrNum,NewTriggerNotes} = get_next_num_3(TriggerData,CurrNum,TriggerNotes,IntList),
	return_3(NewCurrNum,CurrNum,Num,NewTriggerNotes);
check_data(TriggerID,TriggerData,CurrNum,TriggerNotes,Num,IntList) when TriggerID=:=?TASK_TRIGGER_ID_GER_GAIN_STAR_GER
                                                                        orelse TriggerID=:=?TASK_TRIGGER_ID_EQUIP_GAIN_STAR_EQUIP->
    {NewCurrNum,NewTriggerNotes} = get_next_num_3_2(TriggerData,CurrNum,TriggerNotes,IntList),
    return_3(NewCurrNum,CurrNum,Num,NewTriggerNotes);
check_data(TriggerID,TriggerData,CurrNum,TriggerNotes,Num,IntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_UP_EQUIP
																		orelse TriggerID=:=?TASK_TRIGGER_ID_EQUIP_TOTAL_EQUIP
																		orelse TriggerID=:=?TASK_TRIGGER_ID_GER_GAIN_GER->
	{NewCurrNum,NewTriggerNotes} = get_next_num_4(TriggerData,CurrNum,TriggerNotes,IntList),
	return_3(NewCurrNum,CurrNum,Num,NewTriggerNotes);
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,_IntList) when TriggerID=:=?TASK_TRIGGER_ID_ROLE_UP_LEVLE
																		   orelse TriggerID=:=?TASK_TRIGGER_ID_GER_UP_LEVEL
																		   ->
	return_2(TriggerData,CurrNum,Num);

check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,IntList) when TriggerID=:=?TASK_TRIGGER_ID_ADD_REWARD
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_EQUIP_STRONG_TIMES
																		 ->
	NewCurrNum = get_next_num_5(TriggerData, CurrNum, IntList),
	return_2(NewCurrNum,CurrNum,Num);
	
check_data(TriggerID,_TriggerData,CurrNum,_TriggerNotes,Num,_IntList) when TriggerID=:=?TASK_TRIGGER_ID_EXPLORE_TIMES
																		   orelse TriggerID=:=?TASK_TRIGGER_ID_COMBINE_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_COMBINE_NORMAL_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_COMBINE_RANDOM_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_COMBINE_GER_RANDOM_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_REPLACE_UP_GER_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_CHAT_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_SIGN_RACE_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_CHANGE_HEAD_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_BUY_COIN_TIMES
																		   ->
	NewCurrNum = get_next_num(CurrNum),
	return_2(NewCurrNum,CurrNum,Num);
check_data(TriggerID,TriggerData,CurrNum,TriggerNotes,Num,_IntList) when TriggerID =:=?TASK_TRIGGER_ID_ADD_FRIEND_NUM
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_ADD_DIFF_SEX_FRIEND->
	{NewCurrNum, NewTriggerNotes} = get_next_num_6(TriggerData,CurrNum,TriggerNotes),
	return_3(NewCurrNum,CurrNum,Num,NewTriggerNotes);
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,_IntList) when TriggerID =:=?TASK_TRIGGER_ID_EXTRACT_CARD_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_JOIN_HRON_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_JOIN_HULA_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_JOIN_NANM_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_ADD_ENARGY_TO_FRIEND_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_MATING_TO_FRIEND_TIMES
																		  orelse TriggerID=:=?TASK_TRIGGER_ID_PVP_FIGHT_TIMES
																		  ->
	NewCurrNum = get_next_num_8(TriggerData,CurrNum),
	return_2(NewCurrNum,CurrNum,Num);
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,_IntList) when TriggerID=:=?TASK_TRIGGER_ID_ROLE_PVP_RANK->
	return_4(TriggerData, CurrNum,Num);
%%默认比大小
check_data(_TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,_IntList) ->
	return_2(TriggerData,CurrNum,Num).
						
get_next_num(CurrNum)->
	CurrNum+1.

%%TriggerData====>>>{T,L}
%%IntList不为空时：当T在列表中且L大于等于ConfigNum时返回L
%%IntList为空时：当L大于等于ConfigNum时返回L
get_next_num_1(TriggerData,ConfigNum,IntList)->
	{T,Num} = TriggerData,
	case IntList of
		[]->
			Num;
		[T]->
			erlang:min(Num, ConfigNum);
		_->
			0
	end.
%%TriggerData=====>>>T
%%IntList不为空时：当T在列表中时返回CurrNum+1
%%IntList为空时直接返回CurrNum+1
get_next_num_2(TriggerData,CurrNum,IntList)->
	case IntList of
		[]->
			CurrNum+1;
		[TriggerData]->
			CurrNum+1;
		_->
			CurrNum
	end.
%%TriggerData=====>>>{T,L}
%%IntList=========>>>[J]
%%T在TriggerNotes里返回CurrNum和TriggerNotes
%%T没有在TriggerNotes里且L大于等于J时返回CurrNum+1和[T|TriggerNotes]
get_next_num_3(TriggerData,CurrNum,TriggerNotes,IntList)->
	{Type,Level} = TriggerData,
	[ConfigLevel] = IntList,
	case lists:member(Type,TriggerNotes) of
		true->
			{CurrNum,TriggerNotes};
		false->
				case Level>=ConfigLevel of
					true->
						NewCurrNum =CurrNum+1,
						{NewCurrNum,[Type|TriggerNotes]};
					false->
						{CurrNum,TriggerNotes}
				end
	end.

get_next_num_3_2(TriggerData,CurrNum,TriggerNotes,IntList)->
    {UID,StarLevel} = TriggerData,
    [ConfigStarLevel] = IntList,
    case lists:member(UID,TriggerNotes) of
        true->
            {CurrNum,TriggerNotes};
        false->
                case StarLevel =:= ConfigStarLevel of
                    true->
                        NewCurrNum =CurrNum+1,
                        {NewCurrNum,[UID|TriggerNotes]};
                    false->
                        {CurrNum,TriggerNotes}
                end
    end.

%%TriggerData=====>>>{T,L}
%%IntList=========>>>[J]
%%T在TriggerNotes里返回CurrNum和TriggerNotes
%%T没有在TriggerNotes里且(IntList为空或L在IntList里)时返回CurrNum+1和[T|TriggerNotes]
get_next_num_4(TriggerData,CurrNum,TriggerNotes,IntList)->
	{UID,Type} = TriggerData,
	case lists:member(UID,TriggerNotes) of
		true->
			{CurrNum,TriggerNotes};
		false->
				case IntList=:=[] orelse lists:member(Type,IntList) of
					true->
						NewCurrNum =CurrNum+1,
						{NewCurrNum,[UID|TriggerNotes]};
					false->
						{CurrNum,TriggerNotes}
				end
	end.

get_next_num_5(TriggerData,CurrNum,IntList)->
	{AddNum,Type} = TriggerData,
	case IntList of
		[Type]->
			CurrNum+AddNum;
		[]->
			CurrNum+AddNum;
		_->
			CurrNum
	end.

get_next_num_6(TriggerData,CurrNum,TriggerNotes)->
	case lists:member(TriggerData, TriggerNotes) of
		true->
			{CurrNum, TriggerNotes};
		false->
			{CurrNum+1, [TriggerData|TriggerNotes]}
	end.

%%TriggerData=====>>>{T,N}
%%IntList不为空时：当T在列表中时返回CurrNum+N
%%IntList为空时直接返回CurrNum+N
get_next_num_7(TriggerData,CurrNum,IntList)->
	{T,N} = TriggerData,
	case IntList of
		[]->
			CurrNum+N;
		[T]->
			CurrNum+N;
		_->
			CurrNum
	end.

get_next_num_8(TriggerData,CurrNum)->
	CurrNum+TriggerData.

%%NewCurrNum大于等于Num是返回改变状态
return_2(NewCurrNum,CurrNum,Num)->
	if
		NewCurrNum>=Num->
			{true,Num};
		NewCurrNum>CurrNum->
			{false,NewCurrNum};
		true->
			false
	end.

return_3(NewCurrNum,CurrNum,Num,NewTriggerNotes)->
	if
		NewCurrNum>=Num->
			{true,Num,NewTriggerNotes};
		NewCurrNum>CurrNum->
			{false,NewCurrNum,NewTriggerNotes};
		true->
			false
	end.

return_4(NewCurrNum,CurrNum,Num)->
	if
		NewCurrNum=<Num->
			{true,NewCurrNum};
		NewCurrNum<CurrNum->
			{false,NewCurrNum};
		CurrNum=:=0->
			{false,NewCurrNum};
		true->
			false
	end.

%% %%得到更大值
%% get_lager_num(TriggerData,CurrNum,ConfigNum,IntList)->
%% 	{T,Num} = TriggerData,
%% 	case IntList of
%% 		[]->
%% 			CurrNum;
%% 		_->
%% 			case lists:member(T, IntList) of
%% 				true->
%% 					erlang:min(Num, ConfigNum);
%% 				false->
%% 					CurrNum
%% 			end
%% 	end.

get_trigger_num(TriggerID,TriggerNum,_TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_ROLE_UP_LEVLE->
	#role{level=Level} = role_data:get_roleInfo(),
	case Level>=TriggerNum of
		true->
			finish;
		_->
			0
	end;
get_trigger_num(TriggerID,TriggerNum,_TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_GER_UP_LEVEL->
	case lists:any(fun(GerSimple)->
					  GerSimple#gerSimple.gerLevel>=TriggerNum
			  end,role_data:get_gerBag()) of
		true->
			finish;
		false->
			0
	end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_STRONG->
	EquipedList = [E || {_,EquipList}<-role_data:get_equiped_list(), E<-EquipList],
	BagEquipList = role_data:get_bagEquip(),
	AllEquipList = EquipedList++BagEquipList,
	IsOK =
		case TriggerIntList of
			[]->
				lists:any(fun(#item{itemLevel=ItemLevel})->
								  ItemLevel>=TriggerNum
						  end,AllEquipList);
			[Type]->
				lists:any(fun(#item{itemLevel=ItemLevel,itemTypeID=ITI})->
								  ItemLevel>=TriggerNum andalso Type=:=ITI
						  end,AllEquipList)
		end,
	case IsOK of
		true->
			finish;
		false->
			0
	end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_UP_QUALITY->
	EquipedList = [E || {_,EquipList}<-role_data:get_equiped_list(), E<-EquipList],
	BagEquipList = role_data:get_bagEquip(),
	AllEquipList = EquipedList++BagEquipList,
	IsOK =
		case TriggerIntList of
			[]->
				lists:any(fun(#item{itemRank=ItemRank})->
								  ItemRank>=TriggerNum
						  end,AllEquipList);
			[Type]->
				lists:any(fun(#item{itemRank=ItemRank,itemTypeID=ItemTypeID})->
								   ItemRank>=TriggerNum andalso Type=:=ItemTypeID
						  end,AllEquipList)
		end,
	case IsOK of
		true->
			finish;
		false->
			0
	end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_UP_EQUIP->
	EquipedList = [E || {_,EquipList}<-role_data:get_equiped_list(), E<-EquipList],
	IsOK =
		case TriggerIntList of
			[]->
				length(EquipedList)>=TriggerNum;
			[Type]->
				Len = length(lists:filter(fun(#item{itemTypeID=ITI})->
												  Type=:=ITI
										  end,EquipedList)),
				Len>=TriggerNum
		end,
	case IsOK of
		true->
			finish;
		false->
			0
	end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_STRONG_TIMES->
	EquipedList = [E || {_,EquipList}<-role_data:get_equiped_list(), E<-EquipList],
	BagEquipList = role_data:get_bagEquip(),
	AllEquipList = EquipedList++BagEquipList,
	Sum =
		case TriggerIntList of
			[]->
				 lists:foldl(fun(#item{itemLevel=ItemLevel},N)->
									ItemLevel+N
							end,0,AllEquipList);
			[Type]->
				lists:foldl(fun(#item{itemLevel=ItemLevel,itemTypeID=ITI},N)->
									case ITI =:= Type of
										true->
											ItemLevel+N;
										false->
											N
									end
							end,0,AllEquipList)
		end,
	case Sum>=TriggerNum of
		true->
			finish;
		false->
			Sum
	end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_GER_GAIN_STAR_GER->
	[L] = TriggerIntList,
	case lists:foldl(fun(GerSimple,S)->
							 GerTypeID = GerSimple#gerSimple.gerTypeID,
							 case lists:member(GerTypeID, [2,3,4,5,6]) of
								 true->
									 S;
								 false->
									 case  (data_ger:get(GerTypeID))#data_ger.gerStar =:= L of
										 true->
											 S+1;
										 false->
											 S
									 end
							 end
			  end,0,role_data:get_gerBag()) of
		N when N>=TriggerNum->
			finish;
		N->
			N
	end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_GAIN_STAR_EQUIP->
    [ConfigStarLevel] = TriggerIntList,
    EquipedList = [E || {_,EquipList}<-role_data:get_equiped_list(), E<-EquipList],
    BagEquipList = role_data:get_bagEquip(),
    AllEquipList = EquipedList++BagEquipList,
    Sum =
        lists:foldl(
          fun(#item{itemTypeID=ItemTypeID},N)->
                  #data_item{itemStar=ItemStarLevel} = data_item:get(ItemTypeID),
                  case ItemStarLevel =:= ConfigStarLevel of
                      true ->
                          N + 1;
                      false ->
                          N
                  end
          end,0,AllEquipList),
    case Sum >= TriggerNum of
        true->
            finish;
        false->
            Sum
    end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_GER_UP_QUALITY->
	[L] = TriggerIntList,
	case lists:foldl(fun(#gerSimple{gerID=GerID,gerQuality=Qualiry},{S,Acc}=Info)->
						  case  Qualiry >=L of
							  true->
								  {S+1,[GerID|Acc]};
							  false->
								  Info
						  end
			  end,{0,[]},role_data:get_gerBag()) of
		{N,Acc} when N>=TriggerNum->
			{finish,Acc};
		{N,Acc}->
			{N,Acc}
	end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_1->
	EquipedList = [E || {_,EquipList}<-role_data:get_equiped_list(), E<-EquipList],
	BagEquipList = role_data:get_bagEquip(),
	AllEquipList = EquipedList++BagEquipList,
	[L] = TriggerIntList,
	case lists:foldl(fun(#item{itemUID=ItemUID,itemRank=ItemRank},{S,Acc}=Info)->
						  case  ItemRank >=L of
							  true->
								  {S+1,[ItemUID|Acc]};
							  false->
								  Info
						  end
			  end,{0,[]},AllEquipList) of
		{N,Acc} when N>=TriggerNum->
			{finish,Acc};
		{N,Acc}->
			{N,Acc}
	end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_STRONG_1->
	EquipedList = [E || {_,EquipList}<-role_data:get_equiped_list(), E<-EquipList],
	BagEquipList = role_data:get_bagEquip(),
	AllEquipList = EquipedList++BagEquipList,
	[L] = TriggerIntList,
	case lists:foldl(fun(#item{itemUID=ItemUID,itemLevel=ItemLevel},{S,Acc}=Info)->
						  case  ItemLevel >=L of
							  true->
								  {S+1,[ItemUID|Acc]};
							  false->
								  Info
						  end
			  end,{0,[]},AllEquipList) of
		{N,Acc} when N>=TriggerNum->
			{finish,Acc};
		{N,Acc}->
			{N,Acc}
	end;
get_trigger_num(_TriggerID,_TriggerNum,_TriggerIntList)->
	0.

%%=====================================================================
%%添加好友
offline_add_friend(RoleID,FriendRoleID,IsDiffSex)->
	IDList = get_taskid_config_4_triggerid(?TASK_TRIGGER_ID_ADD_FRIEND_NUM),
	IDList1 = 
		case IsDiffSex of
			true->
				get_taskid_config_4_triggerid(?TASK_TRIGGER_ID_ADD_DIFF_SEX_FRIEND);
			false->
				[]
		end,
	case IDList=:=[] andalso IDList1=:=[] of
		[]->
			ignore;
		_->
			spawn(fun()->
						  offline_add_friend_1(RoleID,FriendRoleID,IDList++IDList1)
				  end)
	end.
offline_add_friend_1(RoleID,FriendRoleID,IDList)->
    ChangeTaskList =
        lists:foldr(
          fun(TaskID, Acc)->
                  case db_sql:get_task_by_id(RoleID, TaskID) of
                      ?undefined ->
                          Acc;
                      #r_task{status=Status,trigger_num=TriggerNum,trigger_notes=TriggerNotes} = Task ->
                          case Status=:=?TASK_STATUS_WAS_ACCEPT andalso not lists:member(FriendRoleID,TriggerNotes) of
                              true->
                                  NewCurrNum = TriggerNum+1,
                                  NewTriggerNotes = [FriendRoleID|TriggerNotes],
                                  #data_task{trigger_num=Num} = data_task:get(TaskID),
                                  NewStatus =
                                      case NewCurrNum>=Num of
                                          true->
                                              Status+1;
                                          false->
                                              Status
                                      end,
                                  NewTask = Task#r_task{status=NewStatus,trigger_num=NewCurrNum,trigger_notes=NewTriggerNotes},
                                  [NewTask|Acc];
                              false->
                                  Acc
                          end
                  end
          end, [], IDList),
    db_sql:set_task2(RoleID, ChangeTaskList).



get_taskid_config_4_triggerid(TriggerID)->
	TaskList = data_task:get_list(),
	lists:filter(fun(TaskID)->
						 #data_task{trigger_id=TID} = data_task:get(TaskID),
						 TID =:= TriggerID
				 end, TaskList).