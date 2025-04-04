%% @author admin
%% @doc 玩家数据持久化接口
%% Created 2013-3-11

%% 等待优化点：只遍历一次进程字典，完成玩家数据持久化。

-module(role_persist).
-include("def_role.hrl").
-include("def_battle.hrl").
-compile(export_all).
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc 持久化所有数据到db
persist_all_data() ->
	RoleID = role_data:get_roleID(),
	%% 先做一次重要数据的定时持久化
	do_interval_persist(RoleID),
	%% 再做不重要数据的持久化
	do_less_important_data_persist(RoleID).

do_less_important_data_persist(RoleID) ->
	persist_ger_list(RoleID),
	persist_battle(RoleID),
	persist_gag_list(RoleID),
	persist_sign_emperor_info(RoleID),
	lists:foreach(fun({Key,Value}) ->persist_other(RoleID, Key,Value) end, erlang:get()),
	ok.

do_interval_persist(RoleID) ->
	persist_roleInfo(),
	persist_roleExtra(RoleID),
	persist_role_encounterInfo(RoleID),
	persist_fighter_list(RoleID),	
	persist_roleBag(RoleID),
	persist_role_guard(RoleID),
	persist_role_treaHouseInfo(RoleID),
	persist_role_task_change(RoleID),
    persist_role_road(RoleID),
    persist_month_card(RoleID),
	ok.
	
persist_battle(RoleID) ->
	ChapterList = [Chapter|| {{?chapter, _}, Chapter} <- erlang:get()],
	if ChapterList =/= [] ->
	{ok,_} = db_sql:set_chapter(RoleID, ChapterList);
	   true ->
		   ignore
	end.
	
persist_gag_list(RoleID)->
	GagList = role_data:get_gag_list(),
	db_sql:set_role_gag_list(RoleID, GagList).

persist_sign_emperor_info(RoleID)->
	Info = role_data:get_sign_emperor_info(),
	db_sql:set_sign_emperor_info(RoleID, Info).

persist_role_guard(RoleID)->
	RoleGuardInfo=role_data:get_roleGuardInfo(),
	db_sql:set_role_guard_info(RoleID, RoleGuardInfo).

persist_role_treaHouseInfo(RoleID)->
	TreaHouseInfo = role_data:get_treaHouseInfo(),
	db_sql:set_treasure_house_info(RoleID, TreaHouseInfo). 

persist_role_task(RoleID)->
	TaskList = role_data:get_task_list(),
	db_sql:set_task(RoleID,TaskList).

persist_role_task_change(RoleID)->
    TaskList = role_data:get_task_list(),
    case role_data:erase_dump_task_id_list() of
        [] ->
            next;
        ChangeTaskIDList ->
            ChangeTaskList = lists:filter(fun(#r_task{task_id=TaskID}) -> lists:member(TaskID, ChangeTaskIDList) end, TaskList),
            db_sql:set_task2(RoleID,ChangeTaskList)
    end.

%% 持久化其他数据
persist_other(RoleID, ?guideVal, {GuideVal, DateTime}) ->
    db_sql:log_guide(RoleID, GuideVal, DateTime);
persist_other(RoleID, ?teamPkInfo, TeamPkInfo) ->
    db_sql:set_team_pk_data(RoleID, TeamPkInfo);
persist_other(RoleID, ?roleRewardInfo, #role_reward_info{onlineSecs=OnlineSecs}=RoleRewardInfo) ->
    db_sql:set_reward_data(RoleID, RoleRewardInfo#role_reward_info{onlineSecs=OnlineSecs+role_state:get_online_seconds()});
persist_other(RoleID, ?alienInfo, AlienInfo) ->
    db_sql:set_alien_data(RoleID, AlienInfo);
persist_other(RoleID, ?shopTreasure, ShopTreasure) ->
    db_sql:set_shop_treasure(RoleID, ShopTreasure);
persist_other(RoleID, ?coinBattle, CoinBattle) ->
    db_sql:set_coinBattle(RoleID, CoinBattle);
persist_other(_,_,_) ->
    ok.

persist_month_card(RoleID) ->
    db_sql:set_monthCard(RoleID, role_data:get_month_card()).
persist_role_road(RoleID) ->
    db_sql:set_road_data(RoleID, role_data:get_role_road()).
persist_roleInfo() ->
	NowSec = util:now(),
	RoleInfo = role_data:get_roleInfo(),
	FightPower = role_data:get_roleFightPower(),
	RoleInfo2 = RoleInfo#role{fightPower=FightPower,lastLogoutTime=NowSec},
	db_sql:update_roleInfo(RoleInfo2),
	role_lib:update_rolePublic(RoleInfo2).

persist_roleExtra(RoleID) ->
	RoleExtra = role_data:get_roleExtra(RoleID),
	persist_roleExtra2(RoleID,RoleExtra).

persist_roleExtra2(RoleID, RoleExtra) ->	
	#roleExtra{battleProgress=BattleProgress,
			   battleProgressHard=BattleProgressHard,
			   battleProgressFastHard=BattleProgressFastHard,
			   cardInfo=CardInfo,
			   dailyInfo=DailyInfo,
			   encounterList=EncounterList,
			   gatherList=_GatherList,
			   hronInfo=HronInfo,
			   limitInfo=LimitInfo,
			   roleTimes=RoleTimes,
			   shopNumList=ShopNumList,
			   randomShopList=RandomShopList,
               itemUseList=ItemUseList} = RoleExtra,
	?DEBUG("set randomShopList=~w",[RandomShopList]),
	db_sql:set_roleExtra(RoleID, BattleProgress,BattleProgressHard,BattleProgressFastHard, RoleTimes, EncounterList, DailyInfo, RandomShopList, ItemUseList),
	if is_record(CardInfo, cardInfo) ->
		   db_sql:set_cardInfo(RoleID, CardInfo);
	   true ->
		   ignore
	end,
	db_sql:set_hronInfo(RoleID, HronInfo),
	db_sql:set_limitInfo(RoleID, LimitInfo),
	if ShopNumList =/= [] ->
		   db_sql:set_shopNumList(RoleID, ShopNumList);
	   true ->
		   ignore
	end.

persist_role_encounterInfo(RoleID)->
	EncounterInfo = role_data:get_roleEncounterInfo(),
	db_sql:set_role_encounterList(RoleID, EncounterInfo).

persist_ger_list(RoleID) ->
	GerList = role_data:get_gerBag(),
	PosList = role_data:get_posList(),
    GuardGerList = role_data:get_guardPosList(),
	db_sql:set_gerList(RoleID, PosList, GerList,GuardGerList).
	
persist_fighter_list(RoleID) ->
	FighterList = role_data:get_fighter_list(),
	?DEBUG("fight:~w",[FighterList]),
	LieuInfoList = role_data:get_lieuInfoList(),
	{AtkAdd, HpAdd} = role_data:get_lieu_add_attr(),
	db_sql:set_fighterList(RoleID, FighterList,LieuInfoList, AtkAdd, HpAdd).
	
persist_roleBag(RoleID) ->
	{GerEquipList, BagEquip, BagItem} = role_data:get_all_item(),
	db_sql:set_equipList(RoleID, [{0,BagEquip}|GerEquipList]),
	db_sql:set_bagItem(RoleID, BagItem).

%% ====================================================================
%% Internal functions
%% ====================================================================


