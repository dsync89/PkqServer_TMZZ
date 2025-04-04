%% @author admin
%% @doc 玩家数据操作接口
%% Created 2013-2-26


-module(role_data).
-compile(export_all).
-include("def_role.hrl").
-include("def_task.hrl").
%% API functions
-export([
		 get_ger/1
		,get_roleInfo/0
		,set_roleInfo/1
		,get_roleID/0
		,set_roleID/1
		]).


%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

get_role_road() ->
    erlang:get(?roleRoad).

set_role_road(RoleRoad) ->
    erlang:put(?roleRoad, RoleRoad).

get_task_list_group()->
	{get_task_list(?TASK_TYPE_MAIN),get_task_list(?TASK_TYPE_TODAY),get_task_list(?TASK_TYPE_ACH)}.
set_task_list_group({MList,TList,AList})->
	set_task_list(?TASK_TYPE_MAIN, MList),
	set_task_list(?TASK_TYPE_TODAY, TList),
	set_task_list(?TASK_TYPE_ACH, AList).
get_task_list()->
	lists:append([get_task_list(TaskType)||TaskType<-[?TASK_TYPE_MAIN,?TASK_TYPE_TODAY,?TASK_TYPE_ACH]]).

get_task_list(TaskType)->
	case erlang:get({?ROLE_TASK_LIST,TaskType}) of
		?undefined->
			[];
		L when is_list(L)->
			L;
		_->
			[]
	end.
set_task_list(TaskType,RoleTaskList) when is_list(RoleTaskList)->
	OldTaskList = erlang:put({?ROLE_TASK_LIST,TaskType}, RoleTaskList),
    case erlang:is_list(OldTaskList) of
        true ->
            ChangeTaskList = RoleTaskList -- OldTaskList,
            add_dump_task_id_list(ChangeTaskList);
        false ->
            add_dump_task_id_list(RoleTaskList)
    end.

add_dump_task_id_list([]) ->
    next;
add_dump_task_id_list(ChangeTaskList) ->
    List = get_dump_task_id_list(),
    NewList =
        lists:foldr(fun(#r_task{task_id=TaskID}, AccList) ->
                            case lists:member(TaskID, AccList) of
                                true ->
                                    AccList;
                                false ->
                                    [TaskID|AccList]
                            end
                    end, List, ChangeTaskList),
    erlang:put(?dump_task_id_list, NewList).

get_dump_task_id_list() ->
    case erlang:get(?dump_task_id_list) of
        List when erlang:is_list(List) ->
            List;
        _ ->
            []
    end.

erase_dump_task_id_list() ->
    case erlang:erase(?dump_task_id_list) of
        List when erlang:is_list(List) ->
            List;
        _ ->
            []
    end.

get_ach_curr_taskID(Type)->
	get({?TASK_CURR_ACH_TASKID,Type}).
set_ach_curr_taskID(Type,TaskID)->
	put({?TASK_CURR_ACH_TASKID,Type},TaskID).
set_ach_task_next(TaskID,NextTaskID)->
	put({?TASK_ACH_NEXT_TASKID,TaskID},NextTaskID).
get_ach_task_next(TaskID)->
	get({?TASK_ACH_NEXT_TASKID,TaskID}).

%% 获取物理地址
get_macAddr() ->
	case erlang:get(?macAddr) of
		?undefined ->
			"";
		Addr ->
			Addr
	end.

set_macAddr(Addr) ->
	erlang:put(?macAddr, Addr).

%% 获取ip
get_ip() ->
	case erlang:get(?ip) of
		?undefined ->
			"";
		Addr ->
			Addr
	end.

set_ip(Addr) ->
	erlang:put(?ip, Addr).

%% 获取登入时间
get_login_datetime() ->
    case erlang:get(?loginDatetime) of
        ?undefined ->
            erlang:localtime();
        DateTime ->
            DateTime
    end.

set_login_datetime() ->
    erlang:put(?loginDatetime, erlang:localtime()).

init_gag_list(RoleID)->
	GagList = db_sql:get_role_gag_list(RoleID),
	set_gag_list(GagList).

get_gag_list() ->
	case erlang:get(?gag_list) of
		X when is_list(X)->
			X;
		_ ->
			[]
	end.

set_gag_list(GagList)->
	erlang:put(?gag_list, GagList).

init_sign_emperor_info(RoleID)->
	Info = db_sql:get_sign_emperor_info(RoleID),
	put(?sign_emperor_info, Info).

set_sign_emperor_info(Info)->
	put(?sign_emperor_info, Info).

get_sign_emperor_info()->
	case erlang:get(?sign_emperor_info) of
		X when is_tuple(X) ->
			X;
		_ ->
			{{1989,7,17},0,0,0}
	end.

%% 设置需要间隔持久化标识
set_interval_persist_flag() ->
	put(?interval_persist_flag, true).

clear_interval_persist_flag() ->
	erase(?interval_persist_flag).

get_posTypeList() ->
	case erlang:get(?posTypeList) of
		?undefined ->
			PosList = get_posList(),
			PosTypeList = [GerTypeID
						  ||
						   #ger{gerBase=#gerBase{gerTypeID=GerTypeID}}<-PosList
						  ],
			put(?posTypeList, PosTypeList),
			PosTypeList;
		PosTypeList ->
			PosTypeList
	end.
									  
get_posList() ->
	case erlang:get(?posList) of
		PosList when is_list(PosList) ->
			PosList;
		_ ->
			[]
	end.

%% @doc 大乱斗 测试用
robot_set_posList(PosList) ->
	erlang:put(?posList, PosList).

init_posList(PosList) ->
	PosList2 = [ger_attr:recacl(Ger, lists:delete(Ger, PosList))||Ger<-PosList],
	set_posList(PosList2).

%% 主将只保留未计算副将加成时的数据.副将的影响在每次取副将数据的时候,再进行计算。
set_posList(PosList) ->
	erlang:put(?posList, PosList),
	ger_attr:recacl_gers(PosList),
	role_data:clear_mainGerTypeID().

set_posList2(PosList) ->
	erlang:put(?posList, PosList),
	role_data:clear_mainGerTypeID().

init_lieuList(LPosList)->
	PosList2 = [ger_attr:recacl_lieu(Ger, lists:delete(Ger, LPosList))||Ger<-LPosList],
	set_lieuposList(PosList2).

set_lieu_add_attr(X)->
	erlang:put(?lieu_add_attr, X).

%% {atkAdd, hpAdd}
get_lieu_add_attr()->
	case erlang:get(?lieu_add_attr) of
		X  when is_tuple(X) ->
			X;
		_ ->
			{0,0}
	end.

% 保存副将,然后重算主将并推送信息到客户端
set_lieuposList(PosList)->
	erlang:put(?lieuPosList, PosList),
	ger_attr:recacl_gers().

set_lieuposList2(PosList)->
	[ger_lib:notify_update(E)||E<-PosList],
	erlang:put(?lieuPosList, PosList).

get_lieuposList()->
	case erlang:get(?lieuPosList) of
		PosList when is_list(PosList) ->
			PosList;
		_ ->
			[]
	end.

%% 玩家攻击,血量,战斗力保存与计算方法
%% gFighterList与posList保存未添加副将加成时的属性,posListT保存的添加了副将加成的属性
%% 战斗力计算与cs请求属性列表时都是使用的posListT,战斗计算使用的是posList,在战斗中重新获取副将加成,并加该加成与主将的登场技等叠加计算战斗时的武将属性
%% 新的战斗力值更新到role信息即可,玩家进程每分钟会自动更新rolePublic的数据
set_posListT(PosList)->
	erlang:put(?posListT, PosList),
	FightPower = cacl_roleFightPower(),
	Role = get_roleInfo(),
	Role2=Role#role{fightPower=FightPower},
	OldFightPower = get(role_old_fightPower),
	if
		OldFightPower=:=?undefined->
			put(role_old_fightPower,FightPower),
			?CATCH_1(role_task:send_dispach(self(),{dispach_task,role_fight_power,FightPower}),E1);
		FightPower>OldFightPower->
			put(role_old_fightPower,FightPower),
			?CATCH_1(role_task:send_dispach(self(),{dispach_task,role_fight_power,FightPower}),E2);
		true->
			ignore
	end,
	set_roleInfo(Role2).

get_posListT() ->
	case erlang:get(?posListT) of
		X when is_list(X) ->
			X;
		_ ->
			[]
	end.

%% 守护武将信息

get_roleGuardInfo()->
    erlang:get(?guardInfo).

set_roleGuardInfo(RoleGuardInfo) ->
    erlang:put(?guardInfo,RoleGuardInfo).

get_guardPosList() ->
    erlang:get(?guardPosList).

set_guardPosList(GuardPosList) ->
    erlang:put(?guardPosList, GuardPosList).

init_LieutenantInfo(RoleLieutenant)->
	case RoleLieutenant of
		X when is_list(X) ->
			erlang:put(?lieutenantInfo,X);
		_ ->
			init_LieutenantInfo([])
	end.

get_lieutenantInfo()->
	case erlang:get(?lieutenantInfo) of
		?undefined->
			init_LieutenantInfo([]);
		X ->
			X
	end.

set_lieutenantInfo(X) when is_list(X)->
	erlang:put(?lieutenantInfo,X);
set_lieutenantInfo(X) ->
	?ERR("the lieutenantInfo is wrong:~w",[X]),
	ok.

get_lieuInfoList()->
    LieuInfo = get_lieutenantInfo(),
    LieuList = get_lieuposList(),
    lists:foldl(fun(#ger{gerBase=Base}, Acc)->
                        case lists:keyfind(Base#gerBase.gerPos, #t_lieu.pos, LieuInfo) of
                            #t_lieu{infoID1=ID1} ->
                                if Base#gerBase.gerTypeID =:= ID1 ->
                                       [#p_lieu_view{lieuGerTypeID=ID1}|Acc];
                                   true ->
                                       Acc
                                end;
                            _ ->
                                Acc
                        end
                end, [], LieuList).

get_roleInfo() ->
	erlang:get(?roleInfo).

get_roleFightPower() ->
	cacl_roleFightPower().

cacl_roleFightPower() ->
	lists:foldl(fun(Ger, Acc) ->
						Acc + ?a(Ger, gerFightPower)
				end, 0, get_posListT()).

clear_roleFightPower() ->
	erlang:erase(?roleFightPower).

set_roleInfo(RoleInfo) ->
	erlang:put(?roleInfo, RoleInfo),
	set_interval_persist_flag().

get_roleID() ->
	erlang:get(?roleID).

set_roleID(RoleID) ->
	erlang:put(?roleID, RoleID).

set_gatewayClientPid(Pid) ->
	erlang:put(?gw, Pid).

set_socket(Socket) ->
	erlang:put(?socket,Socket).

get_gatewayClientPid() ->
	erlang:get(?gw).

get_pvp_rank(RoleID) ->
	case get(?pvp_rank) of
		?undefined ->
			Rank = pvp_server:call_get_rank(RoleID),
			put(?pvp_rank, Rank),
			Rank;
		Rank ->
			Rank
	end.

set_pvp_rank(Rank) ->
	erlang:put(?pvp_rank, Rank).

%% @doc 获取武将背包
get_gerBag() ->
	case erlang:get(?gerBag) of
		List when is_list(List) ->
			List;
		_ ->
			[]
	end.

%% @doc 保存武将背包
set_gerBag(List) ->
	%?ERR("gerbag:~w",[List]),
	erlang:put(?gerBag,List).



%% @doc 获取武将信息
get_ger(GerID) ->
	PosList = get_posList(),
	GerBag = get_gerBag(),
	LPosList = get_lieuposList(),
	role_ger:take_ger(GerID, PosList, LPosList, GerBag).

get_ger2(GerID) ->
    PosList = get_posList(),
    GerBag = get_gerBag(),
    LPosList = get_lieuposList(),
    GuardGerList = get_guardPosList(),
    case role_ger:take_ger(GerID, PosList, LPosList, GerBag) of
        false ->
            case lists:keytake(GerID, #gerSimple.gerID, GuardGerList) of
                false ->
                    false;
                {value, SrcGer, GuardGerList2} ->
                    {value, SrcGer, PosList, LPosList, GerBag, GuardGerList2, guard}
            end;
        {value, SrcGer, PosList2, LPosList2, GerBag2, Type} ->
            {value, SrcGer, PosList2, LPosList2, GerBag2, GuardGerList, Type}
    end.
	
clear_mainGerTypeID() ->
	erlang:erase(?mainGerTypeID).

get_mainGerTypeID() ->
	case erlang:get(?mainGerTypeID) of
		?undefined ->
			MainType = role_lib:cacl_mainGerTypeID(),
			put(?mainGerTypeID, MainType),
			MainType;
		MainType ->
			MainType
	end.

%% 汉帝宝库信息
init_treaHouseInfo(RoleID)->
    #treaHouseInfo{mark=Mark,baseBoxGetList=BaseBoxGetList}=TreaHouseInfo = db_sql:get_treasure_house_info(RoleID),
    ActivityID = activityRank_server:get_treaHouse_activityID(),
    BoxList = data_treasure_box:get(base_reward),
    GetList = 
        case data_treasure_box:get(is_baseReward_send_by_mail) of
            "1"->
                lists:foldl(fun(Pos,Acc)->
                                    case lists:keyfind(Pos, #p_baseBoxOpenInfo.pos, Acc) of
                                        false ->
                                            {_, Reward} = lists:keyfind(Pos, 1, BoxList),
                                            mail_server:send_notemp_mail_with_reward(RoleID, 2, integer_to_list(Pos), Reward),
                                            [#p_baseBoxOpenInfo{pos=Pos,isOpen=2}|Acc];
                                        _ ->
                                            Acc
                                    end
                            end, BaseBoxGetList, lists:seq(1,data_treasure_box_baseReward:get(Mark)));
            _ ->
                BaseBoxGetList
        end,
    TreaHouseInfo2 = 
        case TreaHouseInfo#treaHouseInfo.activityID == ActivityID of
            true ->
                TreaHouseInfo#treaHouseInfo{baseBoxGetList=GetList};
            _ ->
                FreeTimes = data_treasure_box:get(treasure_house_free_times),
                #treaHouseInfo{value_info=1, card_list=role_treaHouse:random_treaHouse_list(),free_count=0,buy_count=0, free_times=FreeTimes,
                               mark=0, baseBoxGetList=[],isGetRankReward=0,activityID=0}
        end,
    set_treaHouseInfo(TreaHouseInfo2).

get_treaHouseInfo()->
	case erlang:get(?treahouseInfo) of
		?undefined ->
			db_sql:get_treasure_house_info(get_roleID());
		#treaHouseInfo{}=X ->
			X
	end.

set_treaHouseInfo(Info) when erlang:is_record(Info, treaHouseInfo) ->
	erlang:put(?treahouseInfo, Info).

%% 月卡信息
get_month_card() ->
    erlang:get(?monthCard).

set_month_card(MonthCard) ->
    erlang:put(?monthCard, MonthCard).

init_month_card(MonthCard, LastLogoutTime) ->
    {LastLogoutDate, _} = util:seconds_to_datetime(LastLogoutTime),
    case erlang:date() of
        LastLogoutDate ->
            set_month_card(MonthCard);
        _ ->
            set_month_card(MonthCard#monthCard{dayPayGold=0})
    end.

%% @doc 拼装玩家的roleExtra
get_roleExtra(RoleID) ->
	BatProg = role_battle:get_progress(?BATTLE_DUNGEON_TYPE_NORMAL),
	BatProgHard = role_battle:get_progress(?BATTLE_DUNGEON_TYPE_HARD),
	BatProgFastHard = role_battle:get_progress(?BATTLE_DUNGEON_TYPE_FAST_HARD),
	RoleTimes=get_roleTimes(),
	EncounterList = get_encounterList(),
	ShopNumList = get_shopNumList(),
    ItemUseList = get_itemUseList(),
	DailyInfo = get_dailyInfo(),
	CardInfo = get_cardInfo(),
	HronInfo = get_hronInfo(),
	LimitInfo = get_limitInfo(),
	RandomShopList = role_shop:transform_to_proto(get_randomShopList()),	
	#roleExtra{roleID=RoleID
			  ,battleProgress=BatProg
			  ,battleProgressHard=BatProgHard
			  ,battleProgressFastHard=BatProgFastHard
			  ,roleTimes=RoleTimes
			  ,encounterList=EncounterList
			  ,shopNumList=ShopNumList
			  ,dailyInfo=DailyInfo
			  ,cardInfo=CardInfo
			  ,hronInfo=HronInfo
			  ,limitInfo=LimitInfo
			  ,randomShopList=RandomShopList
              ,itemUseList=ItemUseList}.

%% @doc 设置玩家额外的各功能属性
set_roleExtra(RoleExtra, RoleInfo, AlienInfo) when is_record(RoleExtra, roleExtra) ->
	role_battle:set_progress(?BATTLE_DUNGEON_TYPE_NORMAL,RoleExtra#roleExtra.battleProgress),
	role_battle:set_progress(?BATTLE_DUNGEON_TYPE_HARD,RoleExtra#roleExtra.battleProgressHard),
	role_battle:set_progress(?BATTLE_DUNGEON_TYPE_FAST_HARD,RoleExtra#roleExtra.battleProgressFastHard),
	role_lib:init_roleTimes_recover_timer(RoleExtra#roleExtra.roleTimes, RoleInfo, AlienInfo),
	DiscardShopIDList = init_encounterList(RoleExtra#roleExtra.encounterList),
	init_shopNumList(RoleExtra#roleExtra.shopNumList,DiscardShopIDList),
	init_dailyInfo(RoleExtra#roleExtra.dailyInfo),
	set_cardInfo(RoleExtra#roleExtra.cardInfo),
	set_hronInfo(RoleExtra#roleExtra.hronInfo),
	set_limitInfo(RoleExtra#roleExtra.limitInfo),
	init_gatherInfo(RoleExtra#roleExtra.gatherList),
    init_itemUseList(RoleExtra#roleExtra.itemUseList),
	role_shop:init_randomShopList(RoleExtra#roleExtra.randomShopList);
set_roleExtra(?undefined, _, _) ->
	ignore.

%%　@doc 奇遇商店列表
get_randomShopList() ->
	case erlang:get(?randomShopList) of
		L when is_list(L) ->
			L;
		_ ->
			[]
	end.

set_randomShopList(L) ->
	erlang:put(?randomShopList, L).

init_gatherInfo(?undefined) ->
	ignore;
init_gatherInfo([GatherGerSets, GatherItemSets, GatherEquipSets]) ->
	set_gatherInfo(?GATHER_TYPE_GER, GatherGerSets),
	set_gatherInfo(?GATHER_TYPE_ITEM, GatherItemSets),
    set_gatherInfo(?GATHER_TYPE_EQUIP, GatherEquipSets).

init_shopNumList(ShopNumList, DShopIDList) ->
	ShopNumList2 = 
	lists:filter(fun(E) ->
						 #p_shop_num{shopID=ShopID} = E,
						 not lists:member(ShopID,DShopIDList)
				 end, ShopNumList),
	set_shopNumList(ShopNumList2).

%% @doc 获取已购买列表
get_shopNumList() ->
	case erlang:get(?shopNumList) of
		ShopNumList when is_list(ShopNumList) ->
			ShopNumList;
		_ ->
			[]
	end.

%% @doc 设置已购买列表
set_shopNumList(ShopNumList) when is_list(ShopNumList) ->
	erlang:put(?shopNumList, ShopNumList);
set_shopNumList(_) ->
	ignore.

init_shop_treasure(#shop_treasure{nextRefreshTime=NextRefreshTime}=ShopTreasure) ->
    Now = util:now(),
    case NextRefreshTime =< Now of
        true ->
            erlang:put(?shopTreasure, #shop_treasure{nextRefreshTime = role_shop:get_next_refresh_time(Now),
                                                     itemList = role_shop:refresh_treasure()});
        false ->
            erlang:put(?shopTreasure, ShopTreasure)
    end.

get_shop_treasure() ->
    erlang:get(?shopTreasure).

set_shop_treasure(ShopTreasure) ->
    erlang:put(?shopTreasure, ShopTreasure).

%% @doc 获取道具使用信息列表
get_itemUseList() ->
    case erlang:get(?itemUseList) of
        ItemUseList when is_list(ItemUseList) ->
            {Date, _Time} = erlang:localtime(),
            ItemUseList2 =
                lists:foldr(fun(#item_use_info{useDate=UseDate}=ItemUseInfo, Acc) ->
                                    case UseDate =:= Date of
                                        true ->
                                            [ItemUseInfo|Acc];
                                        false ->
                                            [ItemUseInfo#item_use_info{useDate=Date,useTimes=0}|Acc]
                                    end
                            end, [], ItemUseList),
                case ItemUseList =:= ItemUseList2 of
                    true ->
                        ItemUseList;
                    false ->
                        erlang:put(?itemUseList, ItemUseList2),
                        ItemUseList2
                end;
        _ ->
            []
    end.

%% @doc 设置道具使用信息列表
set_itemUseList(ItemUseList) when is_list(ItemUseList) ->
    erlang:put(?itemUseList, ItemUseList);
set_itemUseList(_) ->
    ignore.

%% @doc 初始化道具使用信息
init_itemUseList(ItemUseList) when is_list(ItemUseList) ->
    {Date, _Time} = erlang:localtime(),
    ItemUseList2 =
        lists:foldr(fun(ItemTypeID, Acc) ->
                            case lists:keyfind(ItemTypeID, #item_use_info.itemTypeID, ItemUseList) of
                                false ->
                                    [#item_use_info{itemTypeID=ItemTypeID,useDate=Date,useTimes=0}|Acc];
                                #item_use_info{useDate=UseDate,useTimes=UseTimes} ->
                                    case UseDate =:= Date of
                                        true ->
                                            [#item_use_info{itemTypeID=ItemTypeID,useDate=Date,useTimes=UseTimes}|Acc];
                                        false ->
                                            [#item_use_info{itemTypeID=ItemTypeID,useDate=Date,useTimes=0}|Acc]
                                    end
                            end
                    end, [], data_item_use:get_list()),
    erlang:put(?itemUseList, ItemUseList2);
init_itemUseList(_) ->
    ignore.

get_fill_gold_bonus() ->
    case erlang:get(?fillGoldBonus) of
        Val when erlang:is_integer(Val),Val > 0 ->
            Val;
        _ ->
            0
    end.

set_fill_gold_bonus(Val) when erlang:is_integer(Val),Val > 0 ->
    erlang:put(?fillGoldBonus, Val).

fill_gold_bonus(AddVal) ->
    ?ERR("fill_gold_bonus:~w", [AddVal]),
    set_fill_gold_bonus(get_fill_gold_bonus() + AddVal).

clear_gold_bonus() ->
    GoldBonus = erlang:erase(?fillGoldBonus),
    case erlang:is_integer(GoldBonus) andalso GoldBonus > 0 of
        true ->
            ?ERR("clear_gold_bonus:~w", [GoldBonus]),
            #role{roleID=RoleID,srcType=SrcType} = get_roleInfo(),
            tencent_pay:add_gold(RoleID, SrcType, GoldBonus);
        false ->
            next
    end.

%% @doc 
request_role_data(RoleID) ->
    RoleInfoT = db_sql:get_roleInfo(RoleID),
    if is_record(RoleInfoT, role) ->
           #role{srcType=SrcType} = RoleInfoT,
           RoleInfo =
               case SrcType =:= ?ACCOUNT_TYPE_QQ orelse SrcType =:= ?ACCOUNT_TYPE_WEIXIN of
                   true ->
                       case tencent_pay:get_gold(RoleID, SrcType) of
                           {true, VGold, VGoldBonus} ->
                               case VGoldBonus =/= RoleInfoT#role.goldBonus of
                                   true ->
                                       ?ERR("sync_gold_bonus, tencent_bonus:~w, role_bonus:~w", [VGoldBonus, RoleInfoT#role.goldBonus]);
                                   false ->
                                       next
                               end,
                               GoldBonus2 =
                                   case VGoldBonus >= RoleInfoT#role.goldBonus of
                                       true ->
                                           VGoldBonus;
                                       false ->
                                           tencent_pay:add_gold(RoleID, SrcType, RoleInfoT#role.goldBonus - VGoldBonus),
                                           RoleInfoT#role.goldBonus
                                   end,
                               case VGold =< RoleInfoT#role.gold of
                                   true ->
                                       PayAddGold = 0,
                                       RoleInfoT#role{gold=VGold,goldBonus=GoldBonus2};
                                   false ->
                                       PayAddGold = VGold - RoleInfoT#role.gold,
                                       RoleInfoT#role{goldBonus=GoldBonus2}
                               end;
                           {false, _} ->
                               tencent_pay:notify_client_update_pay_arg(RoleID),
                               PayAddGold = 0,
                               erlang:throw({'EXIT', tencent_check_error})
                       end;
                   false ->
                       PayAddGold = 0,
                       RoleInfoT
               end;
       true ->
           exit({cannot_fetch_roleInfo, RoleID,RoleInfoT}),
           PayAddGold = 0,
           RoleInfo = ?undefined
    end,
	{BattleProgress, BattleProgressHard,BattleProgressFastHard,RoleTimes, EncounterList, DailyInfo, RandomShopList, ItemUseList} = db_sql:get_roleExtra(RoleID),
	CardInfo = db_sql:get_cardInfo(RoleID),
	GatherList = db_sql:get_gatherInfo(RoleID),
	HronInfo = db_sql:get_hronInfo(RoleID),
	LimitInfo = db_sql:get_limitInfo(RoleID),
	ShopNumList = db_sql:get_shopNumList(RoleID),
	RoleExtra=
		#roleExtra{battleProgress=BattleProgress,
				   battleProgressHard=BattleProgressHard,
					battleProgressFastHard=BattleProgressFastHard,
				   cardInfo=CardInfo,
				   dailyInfo=DailyInfo,
				   encounterList=EncounterList,
				   gatherList=GatherList,
				   hronInfo=HronInfo,
				   limitInfo=LimitInfo,
				   roleTimes=RoleTimes,
				   shopNumList=ShopNumList,
				   randomShopList=RandomShopList,
                   itemUseList=ItemUseList},
	{PosList, GuardGerList, GerBag} = db_sql:get_gerList(RoleID),
	{ListOfGerEquipList, BagEquip} = db_sql:get_equipList(RoleID),
	BagItem = db_sql:get_bagItem(RoleID),
	RoleGuardInfo = db_sql:get_role_guard_info(RoleID),
    TeamPkInfo = db_sql:get_team_pk_data(RoleID),
    RoleRewardInfo = db_sql:get_reward_data(RoleID),
    AlienInfo = db_sql:get_alien_data(RoleID),
    ShopTreasure = db_sql:get_shop_treasure(RoleID),
    MonthCard = db_sql:get_monthCard(RoleID),
	{RoleInfo, RoleExtra,GerBag, PosList, ListOfGerEquipList, BagEquip, BagItem, GuardGerList,
     RoleGuardInfo, TeamPkInfo, AlienInfo, ShopTreasure, RoleRewardInfo, MonthCard, PayAddGold}.



%% @doc 获取别的玩家的出战单位列表
get_otherRoleFighter(RoleID) ->
	case role_lib:is_online(RoleID) of
		true ->
			case catch role_lib:call_server(RoleID, get_fighter_list) of
				{'EXIT',_} ->
					db_sql:get_fighterList_and_lieu_add(RoleID);
				X ->
					X
				end;
		_ ->
			db_sql:get_fighterList_and_lieu_add(RoleID)
	end.

%% 获取别的玩家角色信息
get_otherRoleInfo(RoleID) ->
    case role_lib:is_online(RoleID) of
        true ->
            case catch role_lib:call_server(RoleID, get_role_info) of
                {'EXIT',_} ->
                    db_sql:get_roleInfo(RoleID);
                X ->
                    X
                end;
        _ ->
            db_sql:get_roleInfo(RoleID)
    end.
		
get_otherRoleItemEquips(RoleID) ->
	db_sql:get_equipedList(RoleID).
	
%% @doc 获取出战列表
get_fighter_list() ->
	get_posList().

%% @doc 获取体力相关数据
get_roleTimes() ->
	case erlang:get(?roleTimes) of
		#roleTimes{} = RoleTimes ->
			RoleTimes;
		_ ->
			#roleTimes{}
	end.

%% @doc 设置体力相关数据
set_roleTimes(ER) ->
	erlang:put(?roleTimes, ER).

init_encounterList({ExploreList, EncounterList}) when is_list(EncounterList) ->
	NowSec = util:now(),
	{EncounterList2, DiscardIDList} = 
		partition(fun(E) -> 
						  EndSec = role_explore:endTime(E#echapter.endTimerRef),
						  if EndSec == -1 ->
								 {true, E};
							  EndSec > NowSec ->
								  TimerRef = timer_wheel:add_plan(EndSec, fun() -> role_explore:del_chapter(E#echapter.id) end),
								  {true, E#echapter{endTimerRef=TimerRef}};
							  true ->
								  {MonRank, Encounter} = get_roleEncounterInfo(),
								  ChapterID = E#echapter.id,
								  Encounter2 = lists:keydelete(ChapterID, #t_encounter.chapterID, Encounter),
								  set_roleEncounterInfo({MonRank, Encounter2}),
								  false
						  end
				  
				  end, EncounterList),	
	set_encounterList({ExploreList, EncounterList2}),
	DiscardIDList;
init_encounterList(_) ->
	[].
	
partition(Pred, L) ->
    partition(Pred, L, [], []).

partition(Pred, [H | T], As, Bs) ->
    case Pred(H) of
	{true, H2}-> partition(Pred, T, [H2 | As], Bs);
	false -> partition(Pred, T, As, [H#echapter.dungeonList| Bs])
    end;
partition(Pred, [], As, Bs) when is_function(Pred, 1) ->
    {lists:reverse(As), lists:reverse(Bs)}.

%% @doc 获取遭遇战列表
get_encounterList() ->
	case erlang:get(?encounterList) of
		L when is_tuple(L) ->
			L;
		_ ->
			{[],[]}
	end.

%% @doc 设置遭遇战列表
set_encounterList(List) ->
	erlang:put(?encounterList, List).

set_roleEncounterInfo(EncounterInfo, #role{lastLogoutTime=LastTime})->
	{Date, _} = util:seconds_to_datetime(LastTime),
	case Date == erlang:date() of
		true ->
			set_roleEncounterInfo(EncounterInfo);
		_ ->
			{MonRank, Info} = EncounterInfo,
			update_encounter_monRank_info2(MonRank, Info)
	end.

update_encounter_monRank_info2(MonRank, Info)->
	RankDecay = data_common:get(encounterDecay),
	NewRank = MonRank - RankDecay,
	NewMonRank2 = 
		if NewRank >= 0 ->
			   NewRank;
		   true ->
			   0
		end,
	set_roleEncounterInfo({NewMonRank2, Info}).
			

set_roleEncounterInfo(EncounterInfo)->
	%?ERR("set:~w",[EncounterInfo]),
	erlang:put(?encounterInfo, EncounterInfo).

get_roleEncounterInfo()->
	case erlang:get(?encounterInfo) of
		L when is_tuple(L) ->
			%?ERR("get:~w",[L]),
			L;
		_ ->
			{0,[]}
	end.

init_roleRewardInfo(#role_reward_info{days = Days, lastDays = LastDays, getList = GetList}=RoleRewardInfo) ->
    NowDays = calendar:date_to_gregorian_days(erlang:date()),
    if
        LastDays =:= NowDays ->
            set_roleRewardInfo(RoleRewardInfo);
        LastDays + 1  =:= NowDays ->
            case lists:keyfind(?REWARD_TYPE_DAYS, 1, GetList) of
                false ->
                    set_roleRewardInfo(RoleRewardInfo);
                _ ->
                    NewDays = cacl_acc_days(Days),
                    set_roleRewardInfo(RoleRewardInfo#role_reward_info{days = NewDays, getList = lists:keydelete(?REWARD_TYPE_DAYS, 1, GetList)})
            end;
        true ->
            set_roleRewardInfo(RoleRewardInfo#role_reward_info{days = 1, lastDays = NowDays - 1, getList = lists:keydelete(?REWARD_TYPE_DAYS, 1, GetList)})
    end.

cacl_acc_days(Days) ->
    NewDays = Days + 1,
    Max = lists:max(data_days_reward:get_list()),
    if
        NewDays > Max ->
            1;
        true ->
            NewDays
    end.

set_roleRewardInfo(RoleRewardInfo) ->
    erlang:put(?roleRewardInfo, RoleRewardInfo).

get_roleRewardInfo()->
    erlang:get(?roleRewardInfo).

set_roleTeamPkInfo(TeamPkInfo)->
    erlang:put(?teamPkInfo, TeamPkInfo).

get_roleTeamPkInfo()->
    erlang:get(?teamPkInfo).

set_roleAlienInfo(AlienInfo) ->
    erlang:put(?alienInfo, AlienInfo).

get_roleAlienInfo()->
    erlang:get(?alienInfo).


%% @doc 获取背包中的装备列表
get_bagEquip() ->
	case erlang:get(?bagEquip) of
		L when is_list(L) ->
			L;
		_ ->
			[]
	end.

set_bagEquip(ItemList) ->
	erlang:put(?bagEquip, ItemList).


%% @doc 获取背包中的非装备
get_bagItem() ->
	case erlang:get(?bagItem) of
		L when is_list(L) ->
			L;
		_ ->
			[]
	end.
	

set_bagItem(ItemList) ->
	erlang:put(?bagItem, ItemList). 

%% @doc 获取武将身上穿的装备
get_equip(GerID) ->
	case erlang:get({?gerEquip,GerID}) of
		L when is_list(L) ->
			L;
		_ ->
			[]
	end.

%% @doc 设置武将身上的装备列表
set_equip(GerID, EquipList) ->
	role_allequipment:init_all_equipment(GerID, EquipList),
	erlang:put({?gerEquip, GerID}, EquipList).

%% @doc 两个武将交换装备
swap_equip(GerID1, GerID2) ->
	case erlang:get({?gerEquip, GerID1}) of
		[_|_] = GerEquip2->
			put({?gerEquip, GerID2}, GerEquip2);
		_ ->
			ignore
	end,
	case erlang:get({?gerEquip, GerID2}) of
		[_|_] = GerEquip1->
			put({?gerEquip, GerID1}, GerEquip1);
		_ ->
			ignore
	end.
				
%% @doc GerID1出战武将时的装备替换
replace_equip(GerIDUp, GerIDDown) ->
	EquipList = erlang:erase({?gerEquip,GerIDDown}),
	set_equip(GerIDUp, EquipList),
	EquipList.

%% @doc 获取已装备列表
get_equiped_list() ->
	lists:foldl(fun(#ger{gerID=GerID},Acc) ->
						EquipList = get_equip(GerID),
						[{GerID,EquipList}| Acc]
				end, [], get_lieuposList()++get_posList()).

get_equipts_on_ger()->
	lists:foldl(fun(#ger{gerID=GerID},Acc) ->
						EquipList = get_equip(GerID),
						if EquipList =:= [] ->
							   Acc;
						   true->
							   Equips=lists:foldl(fun(#item{itemUID=ItemUID, itemTypeID=ItemTypeID, itemLevel=ItemLevel,
															itemRank=ItemRank,itemPos=ItemPos,itemDecay=ItemDecay,itemExp=ItemExp},Acc1)->
														  %Equip=#p_equip{itemUID=ItemUID, itemTypeID=ItemTypeID, itemLevel=ItemLevel
														  %				, itemRank=ItemRank,itemGerID=GerID, itemPos=ItemPos
														  %				, itemDecay=ItemDecay, itemExp=ItemExp},
														  Equip=[ItemUID,ItemTypeID,ItemPos,ItemLevel,ItemRank,GerID,ItemDecay,ItemExp],
														  [Equip|Acc1]
												  end, [], EquipList),
							   Equips++Acc
						end
				end, [], get_lieuposList()++get_posList()).


%% @doc 获取玩家的所有道具信息
get_all_item() ->
	BagEquip = get_bagEquip(),
	BagOther = get_bagItem(),
	GerEquipList = [{GerID, EquipList}||
					#ger{gerID=GerID} <- get_posList(),
					EquipList <- [get_equip(GerID)],
					is_list(EquipList),
					EquipList =/= []],
	LGerEquipList = [{GerID, EquipList}||
					 #ger{gerID=GerID} <- get_lieuposList(),
					 EquipList <- [get_equip(GerID)],
					 is_list(EquipList),
					 EquipList =/= []],
	{LGerEquipList++GerEquipList, BagEquip, BagOther}.

%% @doc 初始化玩家的所有道具
init_all_item(ListOfGerEquipList, BagEquip, BagItem) ->
	lists:foreach(fun({GerID, EL}) ->
						  init_ger_equip(GerID, EL)
				  end, ListOfGerEquipList),
	init_bagEquip(BagEquip),
	set_bagItem(BagItem).


%% 计算离线这段时间衰减后的新品阶和下次衰减的的时间
cacl_decay_rank(NextSec, NowSec, Rank) ->
	case data_item_decay:get(Rank) of
		?undefined ->
			{Rank, NextSec};
		Sec ->
			NextSec2 = NextSec+Sec,
			if NextSec2 > NowSec ->
				{Rank, NextSec2};
			   NextSec2 =:= NowSec ->
				   {Rank-1, NowSec};
				true ->
					cacl_decay_rank(NextSec2, NowSec, Rank-1)
			end
	end.

%% 判断是否需要计算品阶衰减，并重算道具属性
cacl_decay(Item, NowSec) ->
	#item{itemTypeID=ItemTypeID, itemRank=ItemRank, itemDecay=ItemDecay, itemUID=ItemUID} = Item,
	case ItemDecay of
		{NextSec,_} ->
			if NextSec > NowSec ->
				   ItemDecay2 = item_lib:item_decay(NextSec, ItemUID),
				   Item#item{itemDecay=ItemDecay2};
			   true ->
				   {ItemRank2, NextDecaySec} = cacl_decay_rank(NowSec, NowSec, ItemRank-1),
				   item_lib:recacl_item_decay2(Item, NextDecaySec, ItemRank2)
			end;
		_ ->
			#data_item{isDecay=IsDecay} = data_item:get(ItemTypeID),
			case IsDecay of
				true ->					
					ItemDecay2 = item_lib:item_decay(item_lib:next_decay_sec(IsDecay, ItemRank, NowSec), ItemUID),
					Item#item{itemUID=ItemUID, itemDecay=ItemDecay2};
				false ->
					Item
			end
	end.
			
%% @doc 初始化背包装备
init_bagEquip(BagEquip) ->
	NowSec = timer_wheel:nowsec(),
	BagEquip2 = [cacl_decay(Item,NowSec)||Item<-BagEquip],
	role_data:set_bagEquip(BagEquip2).

%% @doc 初始化上阵武将的装备
init_ger_equip(GerID, EquipList) ->
	NowSec = timer_wheel:nowsec(),
	{_IsChanged, EquipList2} = 
	lists:foldl(fun(Item,{Flag,Acc}) ->
						Item2 = cacl_decay(Item,NowSec),
						if Flag orelse Item2 =/= Item ->
							   {true, [Item2|Acc]};
						   true ->
							   {false, [Item2|Acc]}
						end
				end, {false, []}, EquipList),
	EquipList3 = lists:reverse(EquipList2),
	role_data:set_equip(GerID, EquipList3).

%% @doc	获取玩家单挑神将录的武将位置
get_challengeGod_pos()->
	case erlang:get(?challengeGodPos) of
		X when erlang:is_integer(X), X < 7 ->
			X;
		_ ->
			data_common:get(challengeGodPos)
	end.

set_challengeGod_pos(Pos) when erlang:is_integer(Pos), Pos < 7->
	erlang:put(?challengeGodPos, Pos);
set_challengeGod_pos(_)->
	ok.


%% @doc 获取玩家pvp挑战次数
get_pvp_times() ->
	#roleTimes{pvpTimes=PvpTimes} = get_roleTimes(),
	PvpTimes.
	
%% @doc 设置玩家pvp挑战次数
set_pvp_times(Times) ->
	RoleTimes = get_roleTimes(),
	RoleTimes2 = RoleTimes#roleTimes{pvpTimes=Times},
	?notify_update(?ra_pvpTimes(Times, RoleTimes2#roleTimes.lastPvpTime+role_lib:to_sec(data_common:get(pvp_recover_interval)))),
	set_roleTimes(RoleTimes2).
	
	
%% pvp挑战次数减一
dec_pvp_times() ->
	#roleTimes{pvpTimes=PvpTimes} = RoleTimes = get_roleTimes(),
	case  PvpTimes > 0 of
		true ->
			NewPvpTimes = PvpTimes-1,
			RoleTimes2 = RoleTimes#roleTimes{pvpTimes=NewPvpTimes},
			?notify_update(?ra_pvpTimes(NewPvpTimes, RoleTimes2#roleTimes.lastPvpTime+role_lib:to_sec(data_common:get(pvp_recover_interval)))),
			set_roleTimes(RoleTimes2),
			NewPvpTimes;
		false ->
			NewPvpTimes = 0,
			?ERR("dec pvp times when ~w\n.",[RoleTimes]),
			RoleTimes2 = RoleTimes#roleTimes{pvpTimes=NewPvpTimes},
			?notify_update(?ra_pvpTimes(NewPvpTimes, RoleTimes2#roleTimes.lastPvpTime+role_lib:to_sec(data_common:get(pvp_recover_interval)))),
			set_roleTimes(RoleTimes2),
			NewPvpTimes
	end.

%% @doc 获取玩家秩序战场挑战次数
get_rule_times() ->
	#roleTimes{ruleTimes=RuleTimes} = get_roleTimes(),
	RuleTimes.
	
%% @doc 设置玩家秩序战场挑战次数
set_rule_times(Times) ->
	RoleTimes = get_roleTimes(),
	RoleTimes2 = RoleTimes#roleTimes{ruleTimes=Times},
	?notify_update(?ra_ruleTimes(Times, RoleTimes2#roleTimes.lastRuleTime+role_lib:to_sec(data_common:get(rule_recover_interval)))),
	set_roleTimes(RoleTimes2).
	
	
%% 夺宝挑战次数减一
%% return：
dec_rule_times() ->
	#roleTimes{ruleTimes=RuleTimes} = RoleTimes = get_roleTimes(),
	case  RuleTimes > 0 of
		true ->
			NewRuleTimes = RuleTimes-1,
			RoleTimes2 = RoleTimes#roleTimes{ruleTimes=NewRuleTimes},
			?notify_update(?ra_ruleTimes(NewRuleTimes, RoleTimes2#roleTimes.lastRuleTime+role_lib:to_sec(data_common:get(rule_recover_interval)))),
			set_roleTimes(RoleTimes2),
			NewRuleTimes;
		false ->
			NewRuleTimes = 0,
			?ERR("dec rule times when ~w\n.",[RoleTimes]),
			RoleTimes2 = RoleTimes#roleTimes{ruleTimes=NewRuleTimes},
			?notify_update(?ra_ruleTimes(NewRuleTimes, RoleTimes2#roleTimes.lastRuleTime+role_lib:to_sec(data_common:get(rule_recover_interval)))),
			set_roleTimes(RoleTimes2),
			NewRuleTimes
	end.

%% 每日奖励信息
get_dailyInfo() ->
	case erlang:get(?dailyInfo) of
		#daily{}=Info ->
			Info;
		_ ->			
			Info = #daily{lastLoggedLoginDate=erlang:date(),loginDays=1,lastDrawLevelUpLevel=0,lastDrawLoginRewardDays=0,lastTitleRewardDate={2013,1,1}},
			set_dailyInfo(Info),
			Info
	end.

set_dailyInfo(Daily) ->
	erlang:put(?dailyInfo, Daily).

init_dailyInfo(#daily{lastLoggedLoginDate=LastLoggedDate,loginDays=LoginDays,dailyDrawList=DailyDrawList} = Daily) ->
	{Year,Month,_} = NowDate = erlang:date(),
    DailyDrawList2 = init_daily_draw_list(DailyDrawList,Year,Month),
	case NowDate > LastLoggedDate of
		true ->
		   %顺路更新一下activity中的每日重置活动
		   activity_server:refresh_daily_activity(role_data:get_roleID()),
			Daily2 = Daily#daily{lastLoggedLoginDate=NowDate,loginDays=LoginDays+1,dailyDrawList=DailyDrawList2};
		false ->
			Daily2 = Daily#daily{dailyDrawList=DailyDrawList2}
	end,
	set_dailyInfo(Daily2);	
init_dailyInfo(_) ->
	ignore.

init_daily_draw_list(DailyDrawList,Year,Month) ->
    case DailyDrawList of
        {{Year,Month},_} ->
            DailyDrawList;
        _ ->
            {{Year,Month},[]}
    end.

%% 获取点将信息
get_cardInfo() ->
	case erlang:get(?cardInfo) of
		#cardInfo{}=Info ->
			Info;
		_ ->
			#cardInfo{}
	end.

%% @doc 设置点将信息
set_cardInfo(CardInfo) ->
	erlang:put(?cardInfo, CardInfo).

%% @doc 获取华容道数据
get_hronInfo() ->
	case erlang:get(?hronInfo) of
		#hronInfo{date=Date,maxDungeonNum=MaxDungeonNum} =HronInfo->
			case erlang:date() of
				Date ->
					HronInfo;
				_ ->
					default_hronInfo(MaxDungeonNum)
			end;
		_ ->
			default_hronInfo(0)
	end.

default_hronInfo(MaxDungeonNum) ->
	#role{vipLevel=VipLevel} = role_data:get_roleInfo(),
    MaxChallengeTimes =
        case data_hron:get({max_challenge_times, VipLevel}) of
            Value when erlang:is_integer(Value) ->
                Value;
            _ ->
                0
        end,
	#hronInfo{
              date=erlang:date(),
              attackAdd=0,
              hpAdd=0,
			  challengeTimes=MaxChallengeTimes,
			  curDungeonNum=1,
              maxDungeonNum=MaxDungeonNum,
			  dungeonID=role_hron:random_dungeon(1),
              coinBuyTimes=0,
              goldBuyTimes=0
			  }.


%% @doc 设置华容道数据
set_hronInfo(HronInfo) ->
	erlang:put(?hronInfo, HronInfo).

get_coinBattle() ->
    erlang:get(?coinBattle).

set_coinBattle(CoinBattle) ->
    erlang:put(?coinBattle, CoinBattle).

%% @doc 限制开启信息
get_limitInfo() ->
	case erlang:get(?limitInfo) of
		#limitInfo{}=L ->
			L;
		_ ->
			#limitInfo{encounterNum=0,inviteRoleID=0,inviteRoleName="",isBindWeibo=false,lastShareLevel=0}
	end.

%% @doc 设置限制开启信息
set_limitInfo(LimitInfo) ->
	erlang:put(?limitInfo, LimitInfo).

%% @doc 图鉴
get_gatherInfo(Type) ->
	case erlang:get({?gatherInfo,Type}) of
		?undefined ->
			[];
		List ->
			List
	end.

%% @doc 设置图鉴
set_gatherInfo(Type, List) ->
	erlang:put({?gatherInfo,Type},List).

%% @doc 完美通关的章节ID列表
get_bestPassChapterID(RoleID) ->
	case db_sql:get_bestPassChapterID(RoleID) of
		List when is_list(List) ->
			List;
		_ ->
			[]
	end.

%% 获取可微博分享的事件
get_weibo_share_list() ->
	case erlang:get(?weiboShareList) of
		List when is_list(List) ->
			List;
		_ ->
			[]
	end.

set_weibo_share_list(List) ->
	erlang:put(?weiboShareList, List).

%% 获取微博分享次数
get_weibo_count() ->
	#roleTimes{weiboCount=WeiboCount,nextWeiboCountRefreshSec=NextRefreshSec} = RoleTimes = get_roleTimes(),
	NowSec = timer_wheel:nowsec(),
	case  NowSec >= NextRefreshSec of
		true ->
			NewWeiboCount = data_common:get(max_weibo_count),
			RefreshTime = data_common:get(weibo_refresh_time),
			NextWeiboSec2 = util:datetime_to_seconds({erlang:date(),RefreshTime}) + ?ONE_DAY_SECONDS,
			RoleTimes2 = RoleTimes#roleTimes{weiboCount=NewWeiboCount,nextWeiboCountRefreshSec=NextWeiboSec2},
			set_roleTimes(RoleTimes2),
			?notify_update(?ra_weiboCount(NewWeiboCount)),
			NewWeiboCount;
		false ->
			WeiboCount
	end.

set_weibo_count(Num) ->
	RoleTimes = get_roleTimes(),
	RoleTimes2 = RoleTimes#roleTimes{weiboCount=Num},
	?notify_update(?ra_weiboCount(Num)),
	set_roleTimes(RoleTimes2).

dec_weibo_count() ->
	#roleTimes{weiboCount=WeiboCount,nextWeiboCountRefreshSec=NextRefreshSec} = RoleTimes = get_roleTimes(),
	NowSec = timer_wheel:nowsec(),
	case  NowSec >= NextRefreshSec of
		true ->
			NewWeiboCount = erlang:max(data_common:get(max_weibo_count) - 1,0),
			RefreshTime = data_common:get(weibo_refresh_time),
			NextWeiboCountSec2 = util:datetime_to_seconds({erlang:date(),RefreshTime}) + ?ONE_DAY_SECONDS,
			RoleTimes2 = RoleTimes#roleTimes{weiboCount=NewWeiboCount,nextWeiboCountRefreshSec=NextWeiboCountSec2},
			?notify_update(?ra_weiboCount(NewWeiboCount)),
			set_roleTimes(RoleTimes2),
			NewWeiboCount;
		false ->
			NewWeiboCount = erlang:max(WeiboCount-1, 0),
			RoleTimes2 = RoleTimes#roleTimes{weiboCount=NewWeiboCount},
			?notify_update(?ra_weiboCount(NewWeiboCount)),			
			set_roleTimes(RoleTimes2),
			NewWeiboCount
	end.
		

%% ====================================================================
%% Internal functions
%% ====================================================================

posList_filter(PosList, LPosList)->
	{filt_PosList(PosList), filt_PosList(LPosList)}.

filt_PosList(PosList)->
	lists:foldl(fun(#ger{gerBase=Base}=Ger, Acc)->
						if Base#gerBase.gerPos > 6 ->
							   do_down_equip(Ger#ger.gerID),
							   do_down_ger(Ger),
							   Acc;
						   true ->
							   [Ger|Acc]
						end;
				   (#gerSimple{gerID=GerID,gerPos=GerPos}=GerSimple,Acc)->
						if GerPos > 6 ->
							   do_down_equip(GerID),
							   do_down_ger(GerSimple),
							   Acc;
						   true ->
							   [GerSimple|Acc]
						end
				end, [], PosList).

do_down_equip(GerID)->
	GerEquip = 
		lists:foldl(fun(Item,Acc)->[Item#item{itemPos=0}|Acc] end, [], get_equip(GerID)),
	BagEquip = get_bagEquip(),
	set_bagEquip(GerEquip++BagEquip).

do_down_ger(#ger{gerBase=Base}=Ger)->
	GerBag = get_gerBag(),
	Ger=#gerSimple{gerExp=Base#gerBase.gerExp,gerID=Ger#ger.gerID,gerTypeID=Base#gerBase.gerTypeID,
				   gerQuality=Base#gerBase.gerQuality,gerLevel=Base#gerBase.gerLevel,gerPos=0},
	set_gerBag([Ger|GerBag]);
do_down_ger(#gerSimple{}=GerSimple)->
	GerBag = get_gerBag(),
	Ger = GerSimple#gerSimple{gerPos=0},
	set_gerBag([Ger|GerBag]).
	
