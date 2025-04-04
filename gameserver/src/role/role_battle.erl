%% @author admin
%% @doc 战役功能
%% Created 2013-3-5


-module(role_battle).
-compile(export_all).
-include("def_role.hrl").
-include("def_battle.hrl").

-define(BATTLE_DUNGEON_TYPE_LIST,[1,2,3]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

hook_zero_clock() ->
    #coinBattle{coolDown=CoolDown} = role_data:get_coinBattle(),
    role_data:set_coinBattle(#coinBattle{date=erlang:date(), coolDown=CoolDown, times=0}),
    ?sendself(#sc_battle_coin_info{coolDown=CoolDown, times=0}).

cs_battle_coin_info(_) ->
    #coinBattle{times=Times,coolDown=CoolDown} = role_data:get_coinBattle(),
    ?sendself(#sc_battle_coin_info{times=Times,coolDown=CoolDown}).

cs_battle_coin_fight(#cs_battle_coin_fight{type=Type}) ->
    case catch check_coin_fight(Type) of
        {true, IsWin, AddCoin, FightInfo, NewCoinBattle, RoleInfo} ->
            role_data:set_coinBattle(NewCoinBattle),
            role_lib:add_coin_f(RoleInfo, AddCoin, ?MONEY_ADD_TYPE_COIN_BATTLE, Type, ""),
            ?sendself(#sc_battle_coin_fight{result=0,coolDown=NewCoinBattle#coinBattle.coolDown,isWin=IsWin,coin=AddCoin,fightInfo=[FightInfo]});
        {false, Reason} ->
            ?sendself(#sc_battle_coin_fight{result=Reason,coolDown=0,isWin=false,coin=0,fightInfo=[]})
    end.

check_coin_fight(Type) ->
    #role{roleID=RoleID, vipLevel=VipLevel, level=Level} = RoleInfo = role_data:get_roleInfo(),
    case Level >= data_coin_battle:get(need_level) of
        true ->
            next;
        false ->
            erlang:throw({false, 3})
    end,
    #coinBattle{coolDown=CoolDown, times=Times} = CoinBattle = role_data:get_coinBattle(),
    case util:now() >= CoolDown of
        true ->
            next;
        false ->
            erlang:throw({false, 2})
    end,
    MaxTimes = lists:nth(VipLevel, data_coin_battle:get(vip_times)),
    case Times < MaxTimes of
        true ->
            next;
        false ->
            erlang:throw({false, 1})
    end,
    case Type of
        1 ->
            DefenderList = get_mon_list(data_dungeon:get(data_coin_battle:get(easy)), Level, Type),
            WinCoin = data_coin_battle:get(easy_coin),
            LoseRatio = data_coin_battle:get(easy_ratio);
        2 ->
            DefenderList = get_mon_list(data_dungeon:get(data_coin_battle:get(normal)), Level, Type),
            WinCoin = data_coin_battle:get(normal_coin),
            LoseRatio = data_coin_battle:get(normal_ratio);
        3 ->
            DefenderList = get_mon_list(data_dungeon:get(data_coin_battle:get(hard)), Level, Type),
            WinCoin = data_coin_battle:get(hard_coin),
            LoseRatio = data_coin_battle:get(hard_ratio)
    end,
    AttackerList = role_data:get_fighter_list(),
    RoleLieuAdd = role_data:get_lieu_add_attr(),
    {IsWin, FightInfo, _} = role_fight:new(RoleID,AttackerList, DefenderList,RoleLieuAdd, {0,0}, false),
    case IsWin of
        true ->
            AddCoin = WinCoin;
        false ->
            AddCoin = erlang:trunc(WinCoin * LoseRatio)
    end,
    {true, IsWin, AddCoin, FightInfo, CoinBattle#coinBattle{coolDown=util:now()+data_coin_battle:get(cool_down), times=Times+1}, RoleInfo}.

get_mon_list(DataDungeon, RoleLevel, Type) ->
    #data_dungeon{gerID2=MonList,gerID3=IDList} = DataDungeon,
    F = fun(#mon{gerTypeID=GerTypeID,gerQuality=GerQuality,gerLevel=GerLevel}) ->
                ger_attr:new_mon(GerTypeID, GerLevel, get_mon_rank(GerQuality, RoleLevel, Type), [], lists:delete(GerTypeID, IDList))
        end,
    [?change_pos(Ger, Pos)||{Pos, Mon} <- MonList, is_record(Mon, mon), Ger<-[F(Mon)], is_record(Ger, ger)].
    
%% 计算怪物的品阶
get_mon_rank(GerQuality, RoleLevel, 3) ->
    GerQuality + erlang:trunc(RoleLevel * data_coin_battle:get(ratio));
get_mon_rank(GerQuality, _RoleLevel, _) ->
    GerQuality.

%% @doc 处理请求：获取战役进度
cs_battle_progress(_) ->
	Fun = fun(Type)->
				  CurDungeonID = get_progress(Type),
				  #data_dungeon{chapterID=CurChapterID} = data_dungeon:get(CurDungeonID),
				  #data_chapter{dungeonIDList=DungeonIDList} = data_chapter:get(CurChapterID),
				  Len = length(DungeonIDList),
				  #p_battle_progress{type=Type,dungeonID=CurDungeonID,chapterID=CurChapterID,dungeonCount=Len}
		  end,
	BPList = lists:map(Fun, ?BATTLE_DUNGEON_TYPE_LIST),
	BestPassChapterIDList = db_sql:get_bestPassChapterID(role_data:get_roleID()),
	?DEBUG("batProg=~w",[BPList]),
	?sendself(#sc_battle_progress{bpList=BPList,bestPassChapterID=BestPassChapterIDList}).

%% @doc 处理请求：获取章节信息
cs_battle_info(#cs_battle_info{chapterID=ChapterID0,type=Type}) ->
	case lists:member(Type, ?BATTLE_DUNGEON_TYPE_LIST) of
		true->
			ChapterID = 
				if
					ChapterID0 =:= 0 ->
						(data_dungeon:get(get_progress(Type)))#data_dungeon.chapterID;
					true ->
						ChapterID0
				end,
			RoleID = role_data:get_roleID(),
			case data_chapter:get(ChapterID) of
				?undefined ->
					ignore;
				_ ->			
					Chapter = get_chapter(RoleID, ChapterID),
					#chapter{dungeonList=DungeonInfo, perfectRewarded= PerfectRewarded}= Chapter,
					#data_chapter{dungeonIDList=DungeonIDList} = data_chapter:get(ChapterID),
				 	 Len = length(DungeonIDList),
					?sendself(#sc_battle_info{type=Type,chapterID=ChapterID,perfectRewarded=PerfectRewarded,dungeonInfo=DungeonInfo,dungeonCount=Len})
			end;
		false->
			ignore
	end.

cs_battle_challenge(#cs_battle_challenge{dungeonID=DungeonID,type=Type})->
	case check_challenge(DungeonID,Type) of
		{true, Progress, DataDungeon, RoleTimes, Chapter, Dungeon, DataChapter} ->
			case do_fight(DataDungeon) of
				{true, FightRecord, Score, RoleInfo} ->
					do_fight_win(Progress, Type,DataDungeon, RoleTimes, Chapter, Dungeon, Score, FightRecord, DataChapter, RoleInfo);
				{false, FightRecord, _Score, RoleInfo} ->
					do_fight_fail(DataDungeon, RoleTimes, Chapter, Dungeon, FightRecord, RoleInfo)
			end;
		{false, Reason} ->
			?sendself(#sc_battle_challenge{result=Reason,score=0})
	end.

cs_battle_dungeon_raids(#cs_battle_dungeon_raids{dungeonID=DungeonID})->
	case check_raids(DungeonID) of
		{true, Energy, RestTimes,RoleTimes,Dungeon,Chapter,RoleInfo,DataDungeon}->
			do_raids(Energy, RestTimes, RoleTimes, Dungeon,Chapter,RoleInfo,DataDungeon);
		{false, Reason}->
			?sendself(#sc_battle_dungeon_raids{result=Reason, raidsTimes=0, reward=[]})
	end.

do_raids(Energy, RestTimes, RoleTimes, Dungeon,Chapter,RoleInfo,DataDungeon)->
    CostEnergy = DataDungeon#data_dungeon.costEnergy,
	RaidsTimes = erlang:min(erlang:min(Energy div CostEnergy, RestTimes),10),
	role_lib:deduct_energy_f(RoleTimes, RaidsTimes * CostEnergy),
	Dungeon2 = Dungeon#p_dungeon{restTimes=Dungeon#p_dungeon.restTimes-RaidsTimes},
	#data_dungeon{dungeon_level=DungeonLevel,reward=Reward,dungeonID=DungeonID} = DataDungeon,
	{RewardList,_} = 
		lists:foldl(fun(_,{RewardAcc, RoleAcc})->
							{RoleAcc2, GerAddExpList, RewardItemList, RewardGerList} = role_reward:handle_dungeon_reward_f(RoleAcc, Reward, ?MONEY_ADD_TYPE_BATTLE, DungeonID, ""),
							RewardItemView = [item_lib:new_item2p_item_view(E) || E<- RewardItemList],
							RewardGerView = [ger_lib:new_ger2p_ger_view(E) || E<- RewardGerList],
							Reward2 = role_lib:reward2p_reward(Reward),
							Reward3 = Reward2#p_reward{gerExpList=GerAddExpList, itemList=RewardItemView, gerList=RewardGerView},
							{[Reward3|RewardAcc], RoleAcc2}
					end,{[], RoleInfo},lists:seq(1,RaidsTimes)),
	set_dungeon(Chapter, Dungeon2),
	
	#role{level=RoleLevel,roleID=RoleID} = RoleInfo,
	MonTeam = get_mon_team(DataDungeon, RoleLevel, DungeonLevel),
	
	DDeadList1 = lists:foldl(fun(G,Acc)->
						role_fight:add_born_list(?b(G,gerTypeID),Acc)
				end, [], MonTeam),
	DDeadList = [{T,N*RaidsTimes}||{T,N}<-DDeadList1],
	case DDeadList of
		[]->
			ignore;
		_->
			?CATCH_1(role_task_trigger:handle({dispach_task,kill_monster,DDeadList}),E1)
	end,
	?CATCH_1(role_task_trigger:handle({dispach_task,dungeon_pass,RoleID,DungeonID,1,RaidsTimes}),E2),
	behavior_dungen_fight:log( RoleID, DungeonID,1,2,RaidsTimes),
	?sendself(#sc_battle_dungeon_raids{result=1, raidsTimes=RaidsTimes, reward=RewardList}).

check_raids(DungeonID)->
	#role{roleID=RoleID,level=Level,vipLevel=VipLevel} =RoleInfo= role_data:get_roleInfo(),
	#data_dungeon{chapterID=ChapterID} = DataDungeon = data_dungeon:get(DungeonID),
	Chapter = get_chapter(RoleID, ChapterID),
	DungeonList = Chapter#chapter.dungeonList,
	
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			case Level < data_dungeon_raids:get(need_level) of
				true ->
					{false, 6};
				_ ->
					case VipLevel < data_dungeon_raids:get(need_vipLevel) of
						true ->
							{false, 7};
						_ ->
							case lists:keyfind(DungeonID, #p_dungeon.dungeonID, DungeonList) of
								false ->
									{false, 5};
								#p_dungeon{bestScore=BestScore, restTimes=RestTimes}=Dungeon->
									case BestScore =:= ?MAX_DUNGEON_SCORE of
										false ->
											{false, 4};
										true ->
											case RestTimes =:= 0 of
												true ->
													{false, 3};
												_ ->
													#roleTimes{energy=Energy} = RoleTimes = role_data:get_roleTimes(),
													case Energy =:= 0 of
														true ->
															{false,2};
														_ ->
															%% 													Progress = 
															%% 														case DungeonID > data_battle_setting:get(hard_battle_start_dungeonID) of
															%% 															true ->
															%% 																get_progress();
															%% 															_ ->
															%% 																get_progress_hard()
															%% 														end,
															%% 													if Progress < DungeonID->
															%% 														   {false, 5};
															%% 													   true ->
															{true, Energy, RestTimes,RoleTimes,Dungeon,Chapter,RoleInfo,DataDungeon}
													%%													end
													end
											end
									end
							end
					end
			end;
		false ->
			{false, 255}
	end.



-define(test_reward_view, ([#p_item_view{itemLevel=1,itemNum=2,itemRank=1,itemTypeID=11001},
							#p_item_view{itemLevel=1,itemNum=2,itemRank=1,itemTypeID=11002}])).
%% 赢了发奖励、扣体力、次数，打分。
do_fight_win(Progress,Type, DataDungeon, RoleTimes, Chapter, Dungeon, Score, FightRecord, DataChapter, RoleInfo) ->
	role_lib:deduct_energy_f(RoleTimes, DataDungeon#data_dungeon.costEnergy),
	Dungeon2 = Dungeon#p_dungeon{restTimes=Dungeon#p_dungeon.restTimes-1,bestScore=erlang:max(Score,Dungeon#p_dungeon.bestScore)},
	Reward = DataDungeon#data_dungeon.reward,
	#role{roleID=RoleID} = RoleInfo,
	DungeonID = DataDungeon#data_dungeon.dungeonID,

	Chapter2 = set_dungeon(Chapter, Dungeon2),
	
	%% 如果本关卡第一次达到3星，则判断是否完美通关，并加入到完美通关列表中
	if Dungeon#p_dungeon.bestScore =/= 3 andalso Score =:= 3 ->
		   case check_perfect(Chapter2, DataChapter) of
			   true ->
				   ChapterID = DataChapter#data_chapter.chapterID,
				   List =  db_sql:get_bestPassChapterID(RoleID),
				   case lists:member(ChapterID, List) of
					   true->
						   ignore;
					   false->
				   			db_sql:add_bestPassChapterID(RoleID, ChapterID)
				   end;
			   {false,_} ->
				   ignore
		   end;
	   true ->
		   ignore
	end,
	
	Progress2 = 
		case DataDungeon#data_dungeon.sufDungeonID of
			0 ->
				#data_chapter{sufChapterID=SufChapterID}=DataChapter,
				if SufChapterID =:= 0 ->
					   Progress;
				   true ->
					   case data_chapter:get(SufChapterID) of
						   #data_chapter{dungeonIDList=[NextDungeonID|_]} ->
							   NextDungeonID;
						   _ ->
							   Progress
					   end
				end;
			SufDungeonID ->
				erlang:max(Progress, SufDungeonID)
		end,
	set_progress(Type,Progress2),
    case Progress2 > Progress of
        true ->
            case Progress2 >= data_common:get(rank_min_dungeon_id) of
                true ->
                    battle_rank_server:update_rank_info(RoleID, Progress2, Type);
                false ->
                    next
            end;
        false ->
            next
    end,
    RewardFix = fix_reward(Progress, Progress2, Type, Reward),
    {_, GerAddExpList, RewardItemList, RewardGerList} = role_reward:handle_dungeon_reward_f(RoleInfo, RewardFix, ?MONEY_ADD_TYPE_BATTLE, DungeonID, ""),
    RewardItemView = [item_lib:new_item2p_item_view(E) || E<- RewardItemList],
    RewardGerView = [ger_lib:new_ger2p_ger_view(E) || E<- RewardGerList],
	Reward2 = role_lib:reward2p_reward(RewardFix),
	Reward3 = Reward2#p_reward{gerExpList=GerAddExpList, itemList=RewardItemView, gerList=RewardGerView},
	
	if RewardItemView =/= [] ->
		   RoleName = role_lib:get_name(role_data:get_roleID()),
		   lists:foreach(fun(X)->#p_item_view{itemTypeID=ItemTypeID, itemNum=ItemNum} = X,
								 #data_item{itemStar=ItemStar,itemType=ItemType} = data_item:get(ItemTypeID),
								 IsEquip = item_lib:is_normal_equip(ItemType),
								 if	ItemStar >= 4 andalso IsEquip -> 
										broadcast_server:bc(#sc_battle_broadcast_get_item{roleName=RoleName,itemTypeID=ItemTypeID,num=ItemNum,dungeonID=Dungeon#dungeon.dungeonID,chapterID=Chapter#chapter.id});
									true	->
										ignore
								 end
						 end, RewardItemView);
	   true ->
		   ignore
	end,
	?CATCH(role_task_trigger:handle({dispach_task,dungeon_pass,RoleID,DungeonID,Type,1})),
	behavior_dungen_fight:log( RoleID, DungeonID,1,1,0),
    ?sendself(#sc_battle_challenge{result=1,fightInfo=[FightRecord],reward=[Reward3],score=Score}).

fix_reward(Progress, Progress2, Type, Reward) ->
    case Type =:= 1 andalso Progress2 =:= 30005 andalso Progress =:= 30004 of
        true ->
            #reward{dropList=DropList} = Reward,
            Reward#reward{dropList=[3020|DropList]};
        false ->
            Reward
    end.

%% 输了，什么都不影响
do_fight_fail(DataDungeon, _Role, _Chapter, _Dungeon, FightRecord, RoleInfo) ->
	%Role2 = Role#role{energy=Role#role.energy-DataDungeon#data_dungeon.costEnergy},
	%Dungeon2 = Dungeon#p_dungeon{restTimes=Dungeon#p_dungeon.restTimes-1},
	%role_data:set_roleInfo(Role2),
	%set_dungeon(Chapter, Dungeon2),
	%Reward = DataDungeon#data_dungeon.reward,
    role_data:set_roleInfo(RoleInfo#role{isFailed=true}),
	DungeonID = DataDungeon#data_dungeon.dungeonID,
	#role{roleID=RoleID} = RoleInfo,
	behavior_dungen_fight:log( RoleID, DungeonID,0,1,0),
	?sendself(#sc_battle_challenge{result=1,fightInfo=[FightRecord],reward=[], score=0}).

get_mon_team(DataDungeon, RoleLevel, Dungeonlevel) when Dungeonlevel==0 orelse RoleLevel >= Dungeonlevel ->
	DataDungeon#data_dungeon.gerID1;
get_mon_team(DataDungeon, RoleLevel, Dungeonlevel) ->
	#data_dungeon{dungeonID=DungeonID} = DataDungeon,
	case ets:lookup(?ETS_DUNGEON_MON_CACHE, {DungeonID,RoleLevel}) of
		[] ->
			Team = get_mon_team2(DataDungeon, RoleLevel, Dungeonlevel),
			%% 在ets中缓存起来
			ets:insert(?ETS_DUNGEON_MON_CACHE, {{DungeonID,RoleLevel}, Team}),
			Team;
		[{_,Team}] ->
			Team			
	end.

get_mon_team2(DataDungeon, RoleLevel, Dungeonlevel) ->
	AddRank = 2*(Dungeonlevel - RoleLevel),
	#data_dungeon{gerID2=MonList,gerID3=IDList} = DataDungeon,
	F = fun(E) ->
				GerTypeID = E#mon.gerTypeID,
				ger_attr:new_mon(GerTypeID, E#mon.gerLevel, E#mon.gerQuality+AddRank, [], lists:delete(GerTypeID, IDList))
		end,
	[?change_pos(Ger, Pos)||{Pos, Mon} <- MonList, is_record(Mon, mon), Ger<-[F(Mon)], is_record(Ger, ger)].



do_fight(#data_dungeon{dungeon_level=DungeonLevel} = DataDungeon) ->
	#role{level=RoleLevel,roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
	MonTeam = get_mon_team(DataDungeon, RoleLevel, DungeonLevel),
	RoleFighterList = role_data:get_fighter_list(),
	RoleLieuAdd = role_data:get_lieu_add_attr(),
	{Result, FightRecord, {FinalState,_,_,_}} = role_fight:new(RoleID,RoleFighterList, MonTeam, RoleLieuAdd,{0,0},false),
	if Result =:= false ->
		   Score = 0;
	   true ->
		   DeadNum = 
			   lists:foldl(fun({_,Hp,_},Acc) ->
								   if Hp =:= 0 ->
										  Acc+1;
									  true ->
										  Acc
								   end
						   end, 0, FinalState),
		   if DeadNum >= 2 ->
				  Score = 1;
			  DeadNum =:= 1 ->
				  Score = 2;
			  true ->
				  Score = 3
		   end
	end,
	{Result, FightRecord, Score, RoleInfo}.

is_pass(DungeonID)->
	RoleID = role_data:get_roleID(),
	Chapter = get_chapter_by_dungeonID(RoleID, DungeonID),
	#p_dungeon{bestScore=BestScore} = get_dungeon_by_chapter(Chapter, DungeonID),
	case BestScore of
		0->
			false;
		_->
			true
	end.
	

check_challenge(DungeonID,Type) ->
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			case lists:member(Type, ?BATTLE_DUNGEON_TYPE_LIST) of
				true->
					Progress = get_progress(Type),
					if Progress >= DungeonID->
						   #data_dungeon{chapterID=ChapterID} = DataDungeon = data_dungeon:get(DungeonID),
						   #role{roleID=RoleID, level=Level} = role_data:get_roleInfo(),
						   #roleTimes{energy=Energy} = RoleTimes = role_data:get_roleTimes(),
						   #data_chapter{activeNeedLevel=ActiveNeedLevel} = DataChapter = data_chapter:get(ChapterID),
						   if Level < ActiveNeedLevel ->
								  {false, 5};
							  true ->
								  if Energy >= DataDungeon#data_dungeon.costEnergy ->
										 Chapter = get_chapter_by_dungeonID(RoleID, DungeonID),
										 Dungeon = get_dungeon_by_chapter(Chapter, DungeonID),
										 if Dungeon#p_dungeon.restTimes >= 1 ->
												{true, Progress, DataDungeon, RoleTimes, Chapter, Dungeon, DataChapter};
											true ->
												{false, 2}
										 end;
									 true ->
										 {false, 3}
								  end
						   end;
					   true ->
						   {false, 4}
					end;
				false->
					{false,5}
			end;
		false ->
			{false, 255}
	end.

%% check_challenge_hard(DungeonID) ->
%% 	Progress = get_progress_hard(),
%% 	if Progress >= DungeonID->
%% 		   #data_dungeon{chapterID=ChapterID} = DataDungeon = data_dungeon:get(DungeonID),
%% 		   #role{roleID=RoleID, level=Level} = role_data:get_roleInfo(),
%% 		   #roleTimes{energy=Energy} = RoleTimes = role_data:get_roleTimes(),
%% 		   #data_chapter{activeNeedLevel=ActiveNeedLevel} = DataChapter = data_chapter:get(ChapterID),
%% 		   if Level < ActiveNeedLevel ->
%% 				  {false, 5};
%% 			  true ->
%% 				  if Energy >= DataDungeon#data_dungeon.costEnergy ->
%% 						 Chapter = get_chapter_by_dungeonID(RoleID, DungeonID),
%% 						 Dungeon = get_dungeon_by_chapter(Chapter, DungeonID),
%% 						 if Dungeon#p_dungeon.restTimes >= 1 ->
%% 								{true, Progress, DataDungeon, RoleTimes, Chapter, Dungeon, DataChapter};
%% 							true ->
%% 								{false, 2}
%% 						 end;
%% 					 true ->
%% 						 {false, 3}
%% 				  end
%% 		   end;
%% 	   true ->
%% 		   {false, 4}
%% 	end.

%% @doc 领取完美通关奖励
cs_battle_perfect_reward(#cs_battle_perfect_reward{chapterID=ChapterID}) ->
	#role{roleID=RoleID} = Role = role_data:get_roleInfo(),
	Chapter = get_chapter(RoleID, ChapterID),
	DataChapter = data_chapter:get(ChapterID),
	case check_perfect(Chapter, DataChapter) of
		true ->
			NewChapter = Chapter#chapter{perfectRewarded=true},
			set_chapter(NewChapter),
			?sendself(#sc_battle_perfect_reward{result=1}),
            role_reward:handle_sell_reward_f(Role, DataChapter#data_chapter.perfectReward, ?MONEY_ADD_TYPE_BATTLE_PERFECT, ChapterID, "");
%% 			role_reward:handle_dungeon_reward_f(Role, DataChapter#data_chapter.perfectReward, ?MONEY_ADD_TYPE_BATTLE_PERFECT, ChapterID, "");
		{false,Reason} ->
			
			?sendself(#sc_battle_perfect_reward{result=Reason})
	end.

check_perfect(Chapter, DataChapter) ->
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			if Chapter#chapter.perfectRewarded =:= false ->
				   DungeonList = Chapter#chapter.dungeonList,
				   if length(DungeonList) =:= length(DataChapter#data_chapter.dungeonIDList) ->
						  case lists:all(fun(E) -> E#p_dungeon.bestScore =:= ?MAX_DUNGEON_SCORE end, DungeonList) of
							  true ->
								  true;
							  false ->
								  {false,3}
						  end;
					  true ->
						  {false, 3}
				   end;
			   true ->
				   {false,2}
			end;
		false ->
			{false, 255}
	end.



%% ====================================================================
%% Internal functions
%% ====================================================================
set_progress(Type,Progress) ->
	put({?batProg,Type}, Progress).

get_progress(Type) ->
	case get({?batProg,Type}) of
		Progress when is_integer(Progress) ->
			Progress;
		?undefined ->
			hd(data_dungeon:get_list())
	end.


get_dungeon_by_chapter(Chapter, DungeonID) ->
	case lists:keyfind(DungeonID, #p_dungeon.dungeonID, Chapter#chapter.dungeonList) of
		false ->
			new_dungeon(DungeonID);
		DungeonInfo ->
			DungeonInfo
	end.

get_chapter_by_dungeonID(RoleID, DungeonID) ->
	#data_dungeon{chapterID=ChapterID} = data_dungeon:get(DungeonID),
	get_chapter(RoleID, ChapterID).

get_chapter(RoleID, ChapterID) ->	
	case get({?chapter, ChapterID}) of
		Info when is_record(Info, chapter) ->
			%% 刷新一次
			case regular(Info) of
				Info ->
					Info;
				Info2 ->
					set_chapter(Info2),
					Info2
			end;
		?undefined ->
			case db_sql:get_chapter(RoleID, ChapterID) of
				?undefined->
					%% 新创建的不需要regular
					NewChapter = new_chapter(RoleID,ChapterID),
					set_chapter(NewChapter),
					NewChapter;
				#chapter{}=Chapter ->
					RegChapter = regular(Chapter),
					set_chapter(RegChapter),
					RegChapter					
			end
	end.

new_chapter(_RoleID,ChapterID) ->
	#chapter{
			 perfectRewarded=false,
			 curDate=erlang:date(),
			 dungeonList=[],
			 id=ChapterID}.

new_dungeon(DungeonID) ->
	#p_dungeon{
			   dungeonID=DungeonID,
			   bestScore=0,
			   restTimes=(data_dungeon:get(DungeonID))#data_dungeon.maxTimes
			  }.

regular(Chapter) ->
	NowDate = erlang:date(),
	case NowDate =:= Chapter#chapter.curDate of
		false ->
			DungeonList = [Dungeon#p_dungeon{restTimes=((data_dungeon:get(Dungeon#p_dungeon.dungeonID))#data_dungeon.maxTimes)}
											|| Dungeon <- Chapter#chapter.dungeonList],
			Chapter#chapter{curDate=NowDate,dungeonList=DungeonList};
		true ->
			Chapter
	end.

%% 	case Chapter#chapter.

set_dungeon(Chapter, Dungeon) ->
	DungeonList2 = lists:keystore(Dungeon#p_dungeon.dungeonID, #p_dungeon.dungeonID, Chapter#chapter.dungeonList, Dungeon),
	Chapter2 = Chapter#chapter{dungeonList=DungeonList2},
	set_chapter(Chapter2),
	Chapter2.

set_chapter(#chapter{id=ChapterID}=Chapter) ->
	put({?chapter,ChapterID}, Chapter).

test()->
	user_default:l(),
	timer:sleep(2000),
	ID=user_default:id(),
	user_default:emu(ID,#cs_battle_info{chapterID=1}),
	user_default:emu(ID,#cs_battle_progress{}),
	user_default:emu(ID,#cs_battle_challenge{dungeonID=1}),
	user_default:emu(ID,#cs_battle_perfect_reward{chapterID=1}).


format_config(List) ->
	Result=lists:map(fun format_config2/1, List),
	io:format("finish cacl data_dungeon\n"),
	Result.

format_config2(DataDungeon) ->
	#data_dungeon{gerID1=D1,gerID2=D2,gerID3=D3,gerID4=D4,gerID5=D5,gerID6=D6}=DataDungeon,		
	MonList = gen_mon_list(D1,D2,D3,D4,D5,D6),
	List = [{1,D1},{2,D2},{3,D3},{4,D4},{5,D5},{6,D6}],
	IDList = [ID||{_Pos, #mon{gerTypeID=ID}} <- List],
	DataDungeon#data_dungeon{gerID1=MonList,gerID2=List,gerID3=IDList,gerID4=0,gerID5=0,gerID6=0}.

gen_mon_list(D1,D2,D3,D4,D5,D6) ->
	List = [{1,D1},{2,D2},{3,D3},{4,D4},{5,D5},{6,D6}],
	IDList = [ID||{_Pos, #mon{gerTypeID=ID}} <- List],
	F = fun(E) ->
				GerTypeID = E#mon.gerTypeID,
				ger_attr:new_mon(GerTypeID, E#mon.gerLevel, E#mon.gerQuality, [], lists:delete(GerTypeID, IDList))
		end,
	[?change_pos(Ger, Pos)||{Pos, Mon} <- List, is_record(Mon, mon), Ger<-[F(Mon)], is_record(Ger, ger)].


get_first_dungeonID() ->
	data_battle_setting:get(common_battle_start_dungeonID).
%hd((data_chapter:get(hd(data_chapter:get_list())))#data_chapter.dungeonIDList).

%% 如何获取第一个章节的id
get_first_hard_dungeonID() ->
	data_battle_setting:get(hard_battle_start_dungeonID).

get_first_fast_hard_dungeonID()->
	data_battle_setting:get(fast_hard_battle_start_dungeonID).