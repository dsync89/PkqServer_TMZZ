-module(role_road).

-compile(export_all).

-include("def_role.hrl").

-define(STATUS_NOT_COMPLETE, 0).
-define(STATUS_COMPLETE, 1).
-define(STATUS_REWARDED, 2).

hook_zero_clock() ->
    #role{vipLevel=VipLevel} = role_data:get_roleInfo(),
    #role_road{nowID=NowID, extID=ExtID, status=Status, resetTimes=ResetTimes} = NewRoleRoad = init_role_road2(VipLevel),
    role_data:set_role_road(NewRoleRoad),
    ?sendself(#sc_road_info{resetTimes=ResetTimes, nowID=NowID, status=Status, extID=ExtID}).

init_role_road(RoleID, VipLevel) ->
    NewRoleRoad =
        case db_sql:get_road_data(RoleID) of
            ?undefined ->
                init_role_road2(VipLevel);
            #role_road{timestamp=Timestamp} = RoleRoad ->
                {Date, _} = util:seconds_to_datetime(Timestamp),
                case Date =/= erlang:date() of
                    true ->
                        init_role_road2(VipLevel);
                    false ->
                        RoleRoad
                end
        end,
    role_data:set_role_road(NewRoleRoad).

init_role_road2(VipLevel) ->
    AddTimes = get_vip_add_times(VipLevel),
   #role_road{timestamp=util:now(), nowID=data_road:get(start_id), extID=0, status=0, resetTimes=data_road:get(free_times) + AddTimes}.

get_vip_add_times(VipLevel) ->
    case lists:keyfind(VipLevel, 1, data_road:get(vip_times)) of
        false ->
            0;
        {_, Times} ->
            Times
    end.

add_vip_times(VipLevel) ->
    #role_road{resetTimes=ResetTimes} = RoleRoad = role_data:get_role_road(),
    AddTimes = get_vip_add_times(VipLevel),
    role_data:set_role_road(RoleRoad#role_road{resetTimes=ResetTimes + AddTimes}).


cs_road_info(_) ->
    #role_road{nowID=NowID, extID=ExtID, status=Status, resetTimes=ResetTimes} = role_data:get_role_road(),
    ?sendself(#sc_road_info{resetTimes=ResetTimes, nowID=NowID, status=Status, extID=ExtID}).

cs_road_reset(_) ->
    #role_road{resetTimes=ResetTimes} = RoleRoad = role_data:get_role_road(),
    case ResetTimes > 0 of
        true ->
            role_data:set_role_road(RoleRoad#role_road{nowID=data_road:get(start_id), extID=0, status=0, resetTimes=ResetTimes-1}),
            ?sendself(#sc_road_reset{result=0});
        false ->
            ?sendself(#sc_road_reset{result=1})
    end.

cs_road_fight(_) ->
    case catch check_can_fight() of
        {ok, RoleInfo, RoleRoad, IsWin, FightRecord, DataDungeon} ->
            do_road_fight(RoleInfo, RoleRoad, IsWin, FightRecord, DataDungeon);
        {false, Reason} ->
            ?sendself(#sc_road_fight{result=Reason,isWin=false,extID=0})
    end.

do_road_fight(RoleInfo, #role_road{extID=ExtID}=RoleRoad, IsWin, FightRecord, DataDungeon) ->
    case IsWin of
        false ->
            RewardView = [],
            NewExtID = ExtID;
        true ->
            case ExtID of
                0 ->
                    NewExtID = gen_ext_id();
                _ ->
                    NewExtID = ExtID
            end,
            NewStatus = ?STATUS_COMPLETE,
            NewRoleRoad = RoleRoad#role_road{extID=NewExtID, status=NewStatus},
            role_data:set_role_road(NewRoleRoad),
            #data_dungeon{dungeonID=DungeonID, reward=Reward} = DataDungeon,
            #reward{coin=AddCoin, gold=AddGold, reputation=AddRepu} = Reward,
            {_, _, RewardItemList, RewardGerList} = role_reward:handle_dungeon_reward_f(RoleInfo, Reward, ?MONEY_ADD_TYPE_ROAD, DungeonID, ""),
            RewardView = [role_reward:transform2p_mail_reward(#sell_reward{coin=AddCoin, gold=AddGold, reputation=AddRepu,item=RewardItemList, newGer=RewardGerList})]
    end,
    ?sendself(#sc_road_fight{result=0, isWin=IsWin, fightInfo=[FightRecord], reward=RewardView,extID=NewExtID}).

    

gen_ext_id() ->
    case random:uniform(10000) =< data_road:get(probability) of
        true ->
            ExtList = data_road:get(ext_list),
            lists:nth(random:uniform(erlang:length(ExtList)), ExtList);
        false ->
            0
    end.

check_can_fight() ->
    #role_road{nowID=NowID, status=Status} = RoleRoad = role_data:get_role_road(),
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true  -> next;
		false -> erlang:throw({false, 255})
	end,
    case Status of
        ?STATUS_NOT_COMPLETE ->
            next;
        ?STATUS_COMPLETE ->
            erlang:throw({false, 2});
        ?STATUS_REWARDED ->
            erlang:throw({false, 2})
    end,
    #role{level=RoleLevel} = RoleInfo = role_data:get_roleInfo(),
    DataDungeon = data_dungeon:get(NowID),
    DefenderList = get_mon_list(DataDungeon, RoleLevel),
    AttackerList = role_data:get_fighter_list(),
    RoleLieuAdd = role_data:get_lieu_add_attr(),
    RoleID = role_data:get_roleID(),
    {IsWin, FightRecord, _} = role_fight:new(RoleID,AttackerList, DefenderList,RoleLieuAdd, {0,0}, false),
    {ok, RoleInfo, RoleRoad, IsWin, FightRecord, DataDungeon}.

get_mon_list(DataDungeon, RoleLevel) ->
    #data_dungeon{gerID2=MonList,gerID3=IDList} = DataDungeon,
    F = fun(#mon{gerTypeID=GerTypeID,gerQuality=GerQuality,gerLevel=GerLevel}) ->
                ger_attr:new_mon(GerTypeID, GerLevel, get_mon_rank(GerQuality, RoleLevel), [], lists:delete(GerTypeID, IDList))
        end,
    [?change_pos(Ger, Pos)||{Pos, Mon} <- MonList, is_record(Mon, mon), Ger<-[F(Mon)], is_record(Ger, ger)].
    
%% 计算怪物的品阶
get_mon_rank(GerQuality, RoleLevel) ->
    GerQuality + erlang:trunc(RoleLevel * data_road:get(hard_ratio)).

cs_road_fight_ext(_) ->
    case catch check_road_fight_ext() of
        {ok, RoleInfo, RoleRoad, IsWin, FightRecord, DataDungeon} ->
            do_road_fight_ext(RoleInfo, RoleRoad, IsWin, FightRecord, DataDungeon);
        {false, Reason} ->
            ?sendself(#sc_road_fight_ext{result=Reason,isWin=false})
    end.

do_road_fight_ext(RoleInfo, RoleRoad, IsWin, FightRecord, DataDungeon) ->
    case IsWin of
        false ->
            RewardView = [];
        true ->
            NewRoleRoad = RoleRoad#role_road{extID=0},
            role_data:set_role_road(NewRoleRoad),
            #data_dungeon{dungeonID=DungeonID, reward=Reward} = DataDungeon,
            #reward{coin=AddCoin, gold=AddGold, reputation=AddRepu} = Reward,
            {_, _, RewardItemList, RewardGerList} = role_reward:handle_dungeon_reward_f(RoleInfo, Reward, ?MONEY_ADD_TYPE_ROAD, DungeonID, ""),
            RewardView = [role_reward:transform2p_mail_reward(#sell_reward{coin=AddCoin, gold=AddGold, reputation=AddRepu, item=RewardItemList, newGer=RewardGerList})]
    end,
    ?sendself(#sc_road_fight_ext{result=0, isWin=IsWin, fightInfo=[FightRecord], reward=RewardView}).

check_road_fight_ext() ->
    #role_road{extID=ExtID} = RoleRoad = role_data:get_role_road(),
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true  -> next;
		false -> erlang:throw({false, 255})
	end,
    case ExtID of
        0 ->
            erlang:throw({false, 1});
        _ ->
            next
    end,
    #role{level=RoleLevel} = RoleInfo = role_data:get_roleInfo(),
    DataDungeon = data_dungeon:get(ExtID),
    DefenderList = get_mon_list(DataDungeon, RoleLevel),
    AttackerList = role_data:get_fighter_list(),
    RoleLieuAdd = role_data:get_lieu_add_attr(),
    RoleID = role_data:get_roleID(),
    {IsWin, FightRecord, _} = role_fight:new(RoleID,AttackerList, DefenderList,RoleLieuAdd, {0,0}, false),
    {ok, RoleInfo, RoleRoad, IsWin, FightRecord, DataDungeon}.

cs_road_box(_) ->
    case catch check_road_box() of
        {ok, RoleRoad, Reward} ->
            do_road_box(RoleRoad, Reward);
        {false, Reason} ->
            ?sendself(#sc_road_box{result=Reason, reward=[]})
    end.

do_road_box(#role_road{nowID=NowID}=RoleRoad, Reward) ->
    role_reward:handle_sys_reward(role_data:get_roleInfo(), [Reward], ?MONEY_ADD_TYPE_ROAD, 0, ""),
    EndID = data_road:get(end_id),
    case NowID of
        EndID ->
            NewNowID = EndID,
            NewStatus = ?STATUS_REWARDED;
        _ ->
            NewNowID = NowID + 1,
            NewStatus = 0
    end,
    role_data:set_role_road(RoleRoad#role_road{nowID=NewNowID, status=NewStatus}),
    ?sendself(#sc_road_box{result=0, reward=[role_reward:transform2p_mail_reward([Reward])]}).

check_road_box() ->
    #role_road{nowID=NowID,status=Status} = RoleRoad = role_data:get_role_road(),
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true  -> next;
		false -> erlang:throw({false, 255})
	end,
    case Status of
        ?STATUS_COMPLETE ->
            next;
        ?STATUS_NOT_COMPLETE ->
            erlang:throw({false, 1});
        ?STATUS_REWARDED ->
            erlang:throw({false, 2})
    end,
    BoxID = data_road:get(NowID),
    [RConfig|_] = data_box:get({BoxID, 0}),
    Reward = util:random_one_from_weigh_list(RConfig),
    {ok, RoleRoad, Reward}.












