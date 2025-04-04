%% @author admin
%% @doc 每日工资
%% Created 2013-4-15


-module(role_daily).
-compile(export_all).
-include("def_role.hrl").
%% API functions
-export([do_get_list/0]).

%% Internal functions
-export([]).


%% ====================================================================
%% API functions
%% ====================================================================
cs_daily_get_list(_) ->
	do_get_list().

cs_daily_draw(#cs_daily_draw{type=Type}) ->
	do_draw(Type).

cs_daily_reward_list(_) ->
    do_get_reward_list().

cs_daily_reward_get(#cs_daily_reward_get{type=Type, needValue=NeedValue}) ->
    do_reward_get(Type, NeedValue).

cs_daily_vip_info(_) ->
    #role{vipLevel=VipLevel,timeVipDraw=TimeVipDraw}=role_data:get_roleInfo(),
    {DrawDate, _} = util:seconds_to_datetime(TimeVipDraw),
    IsVipDraw = DrawDate =:= erlang:date(),
    case data_days_vip:get(VipLevel) of
        SellReward when erlang:is_record(SellReward, sell_reward) ->
            ?sendself(#sc_daily_vip_info{isDraw=IsVipDraw,reward=role_reward:transform2p_mail_reward(SellReward)});
        _ ->
            ?sendself(#sc_daily_vip_info{isDraw=IsVipDraw,reward=#p_mail_reward{}})
    end.

cs_daily_vip_draw(_) ->
    #role{vipLevel=VipLevel,timeVipDraw=TimeVipDraw} = RoleInfo = role_data:get_roleInfo(),
    {DrawDate, _} = util:seconds_to_datetime(TimeVipDraw),
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
    		case DrawDate =:= erlang:date() of
				false ->
					case data_days_vip:get(VipLevel) of
						SellReward when erlang:is_record(SellReward, sell_reward) ->
							role_reward:handle_sell_reward_f(RoleInfo#role{timeVipDraw=util:now()}, SellReward, ?MONEY_ADD_TYPE_REWARD_DAYS_VIP, 0, ""),
							?sendself(#sc_daily_vip_draw{result=0});
						_ ->
							?sendself(#sc_daily_vip_draw{result=2})
					end;
				true ->
            		?sendself(#sc_daily_vip_draw{result=1})
    		end;
		false ->
			?sendself(#sc_daily_vip_draw{result=255})
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
reset_vip_days_reward() ->
    #role{vipLevel=VipLevel}=RoleInfo=role_data:get_roleInfo(),
    role_data:set_roleInfo(RoleInfo#role{timeVipDraw=0}),
    case data_days_vip:get(VipLevel) of
        SellReward when erlang:is_record(SellReward, sell_reward) ->
            ?sendself(#sc_daily_vip_info{isDraw=false,reward=role_reward:transform2p_mail_reward(SellReward)});
        _ ->
            ?sendself(#sc_daily_vip_info{isDraw=false,reward=#p_mail_reward{}})
    end.

hook_zero_clock() ->
	#daily{loginDays=CurLoginDays} = Daily = role_data:get_dailyInfo(),
	NowDate = erlang:date(),
	Daily2 = Daily#daily{loginDays=CurLoginDays+1,lastLoggedLoginDate=NowDate},
    #daily{dailyDrawList=DailyDrawList} = Daily2,
    {_, List} = NewDailyDrawList = get_daily_draw_list(DailyDrawList),
	role_data:set_dailyInfo(Daily2#daily{dailyDrawList=NewDailyDrawList}),
	DailyList = get_list(Daily2),
    ?sendself(#sc_daily_get_list{dailyList = DailyList,dailyDrawList=List}),
	
	%%RoleRewardInfo = role_data:get_roleRewardInfo(),
	%%#role_reward_info{onlineSecs = OnlineSecs, getList = GetList} = RoleRewardInfo,
	%%role_data:set_roleRewardInfo(RoleRewardInfo#role_reward_info{onlineSecs=0,getList = lists:keyreplace(?REWARD_TYPE_ONLINE, 1, GetList, {?REWARD_TYPE_ONLINE,[]}) }),
	%%?ERR("清空在线奖励=~p",[role_data:get_roleRewardInfo()]),
    %%?sendself(#sc_daily_reward_list{list=[role_daily:get_days_info(role_data:get_roleRewardInfo())]}),
	
	%% 推送神将录次数更新
	update_challengeGodInfo(),
	%% 推送汉帝宝库更新
	update_treaHouseInfo(),
	%% 推送副将刷新信息
	update_refreshLieuInfo(),
	%% 遭遇战自动降阶
	update_encounter_monRank_info(),
	% activity_server主动推送每日充值活动的更新
	activity_server:refresh_daily_activity(role_data:get_roleID()),
    
    update_roleRewardInfo(),
    
    update_dayPayReward().

update_dayPayReward() ->
    role_data:set_roleInfo((role_data:get_roleInfo())#role{payExtReward=0, lastPayTime=util:now()}),
    ?sendself(#sc_role_update_pay_ext{pay_ext=0}).

update_roleRewardInfo() ->
	?ERR("0点清空在线奖励====================================================~p",[role_data:get_roleRewardInfo()]),
    role_data:init_roleRewardInfo(role_data:get_roleRewardInfo()),
	RoleRewardInfo = role_data:get_roleRewardInfo(),
	#role_reward_info{onlineSecs = OnlineSecs, getList = GetList} = RoleRewardInfo,
	role_data:set_roleRewardInfo(RoleRewardInfo#role_reward_info{onlineSecs=0,getList = lists:keyreplace(?REWARD_TYPE_ONLINE, 1, GetList, {?REWARD_TYPE_ONLINE,[]}) }),
	?ERR("00000点清空在线奖励====================================================~p",[role_data:get_roleRewardInfo()]),
    ?sendself(#sc_daily_reward_list{list=[get_days_info(role_data:get_roleRewardInfo())]}),
	?sendself(#sc_daily_reward_list{list=[get_online_info(role_data:get_roleRewardInfo())]}),
	
    cs_daily_vip_info(#cs_daily_vip_info{}).

update_refreshLieuInfo()->
	RoleTimes = role_data:get_roleTimes(),
	FreeTimes = data_lieu_clo_setting:get(daily_free_refresh),
	role_data:set_roleTimes(RoleTimes#roleTimes{refreshLieuTimes=FreeTimes}),
	?sendself(#sc_ger_lieu_refresh_freeTimes{times=FreeTimes}).
		
update_challengeGodInfo()->
	RoleTimes = role_data:get_roleTimes(),
	%ChallengeGodEnergy = data_common:get(challengeGodTimes),
	Role=role_data:get_roleInfo(),
	ChallengeGodEnergy = role_lib:get_max_challengeGodFreeTimes(Role#role.vipLevel),
	role_data:set_roleTimes(RoleTimes#roleTimes{challengeGodEnergy=ChallengeGodEnergy,challengeGodBuyTimes=0, lastChallengeGodDate=erlang:date()}),
	ChallengePos = role_data:get_challengeGod_pos(),
	Price = data_common:get(buy_challengeGod_gold),
	?notify_update(#sc_challengeGod_info{freeTimes=ChallengeGodEnergy,buyTimes=0,gerPos=ChallengePos, price=Price}).

update_treaHouseInfo()->
    TreaHouseInfo = role_data:get_treaHouseInfo(),
    if TreaHouseInfo#treaHouseInfo.card_list =:= [] ->
           ignore;
       true ->
           FreeTimes = data_treasure_box:get(treasure_house_free_times),
           TreaHouseInfo2 = TreaHouseInfo#treaHouseInfo{free_times=FreeTimes},
           BoxProcess = data_treasure_box_baseReward:get(TreaHouseInfo#treaHouseInfo.mark),
           role_data:set_treaHouseInfo(TreaHouseInfo2),
           {EndTime, StopTime} = role_treaHouse:get_end_time(),
           OneTimeNeedGold = data_treasure_box:get(oneTimeCost),
           RefreshNeedCoin = data_treasure_box:get(refresh_cost),
           case role_treaHouse:check_enter_treaHouse() of
               true ->
                   ?notify_update(#sc_treaHouse_get_list{isOpen=1,endTime=EndTime,boxProcess = BoxProcess,stopTime=StopTime,
                                                         cardList=role_treaHouse:treaHouseCard2p_treaHouse_card(TreaHouseInfo2#treaHouseInfo.card_list),
                                                         freeTimes = FreeTimes,boxOpenProcess = TreaHouseInfo#treaHouseInfo.baseBoxGetList,
                                                         oneTimeNeedGold=OneTimeNeedGold, refreshNeedCoin=RefreshNeedCoin});
               _ ->
                   ignore
           end
    end.

update_encounter_monRank_info()->
	{MonRank, Info}=role_data:get_roleEncounterInfo(),
	role_data:update_encounter_monRank_info2(MonRank, Info).

do_get_list() ->
	Daily = role_data:get_dailyInfo(),
	DailyList = get_list(Daily),
    #daily{dailyDrawList=DailyDrawList} = Daily,
    {_, List} = NewDailyDrawList = get_daily_draw_list(DailyDrawList),
    case NewDailyDrawList of
        DailyDrawList ->
            next;
        _ ->
           role_data:set_dailyInfo(Daily#daily{dailyDrawList=NewDailyDrawList}) 
    end,
	?sendself(#sc_daily_get_list{dailyList = DailyList, dailyDrawList=List}).

get_daily_draw_list(DailyDrawList) ->
    {Year, Month, _} = erlang:date(),
    role_data:init_daily_draw_list(DailyDrawList, Year, Month).

get_continous_login_daily(Daily) ->
	#daily{lastDrawLoginRewardDays=LastDrawDays,
		   loginDays=LoginDays0} = Daily, 
	LoginDays = login_days2(LoginDays0),
	if LastDrawDays < LoginDays0 ->
		   case get_login_reward_config(LoginDays) of
			   #daily_reward{} ->
				   [#p_daily{type=2,value=LoginDays,isDrawed=false}];
			   _ ->
				   []
		   end;
	   LastDrawDays =:= LoginDays0 ->
		   [#p_daily{type=2,value=LoginDays,isDrawed=true}];
	   true ->
		   []
	end.

get_title_daily(Daily, Role) ->
	NowDate = erlang:date(),
	#daily{lastDrawTitle=LastDrawTitle,
		   lastTitleRewardDate=LastTitleDate} = Daily, 
	#role{title=Title} = Role,
		if LastTitleDate < NowDate ->		   
			   case data_title_reward:get(Title) of
				   #daily_reward{} ->
					   [#p_daily{type=1,value=Title,isDrawed=false}];
				   _ ->
					   []
			   end;
		   LastTitleDate =:= NowDate ->
			   [#p_daily{type=1,value=LastDrawTitle,isDrawed=true}];
		   true ->
			   []
		end.

get_levelup_daily(Daily, #role{level=RoleLevel}) ->
	#daily{lastDrawLevelUpLevel=LastLevel} = Daily,
	RewardLevelList = data_levelup_reward:get_list(),
	case list_next(LastLevel, RewardLevelList) of
		false ->
			if LastLevel == 0 ->
				   [];
			   true ->
				   [#p_daily{type=3,value=LastLevel,isDrawed=true}]
			end;
		{value, Nextlevel} ->
			if Nextlevel =< RoleLevel ->
				   [#p_daily{type=3,value=Nextlevel,isDrawed=false}];
			   true ->
				   if LastLevel == 0 ->
						  [];
					  true ->
						  [#p_daily{type=3,value=LastLevel, isDrawed=true}]
				   end
			end
	end.

get_list(Daily) ->
	DailyLogin = get_continous_login_daily(Daily),
		
	Role = role_data:get_roleInfo(),	
	DailyTitle = get_title_daily(Daily, Role),
	
	LevelUpDaily = get_levelup_daily(Daily, Role),
	DailyTitle ++ DailyLogin ++ LevelUpDaily.
	

login_days2(LoginDays) ->
%% 	((LoginDays-1) rem (data_login_reward:get(max))) +1.
	if LoginDays > 31 ->
		   (LoginDays - 31) rem (data_login_reward:get(max) - 30) + 31;
	   true ->
		   LoginDays
	end.

get_login_reward_config(LoginDays) 
  when is_integer(LoginDays) andalso LoginDays > 0->
	case data_login_reward:get(LoginDays) of
		#daily_reward{}=Reward ->
			Reward;
		_ ->
			data_login_reward:get(max)
	end;
get_login_reward_config(_) ->
	?undefined.

do_draw(2) ->
	#daily{lastDrawLoginRewardDays=LastDrawDays,
		   loginDays=LoginDays0,dailyDrawList=DailyDrawList} = DailyInfo = role_data:get_dailyInfo(),
	LoginDays = login_days2(LoginDays0),
	LoginRewardFlag = 
		case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
			true ->
				if LastDrawDays < LoginDays0 ->
					   case get_login_reward_config(LoginDays) of
						   #daily_reward{}=Reward   ->
							   Role = role_data:get_roleInfo(),
							   role_reward:handle_daily_reward_f(Role, Reward, ?MONEY_ADD_TYPE_CON_LOGIN_REWARD, LoginDays, ""),
							   LastLoginDrawDays2 = LoginDays0,
							   {Year, Month, Day} = erlang:date(),
							   {_, OldList} = role_data:init_daily_draw_list(DailyDrawList, Year, Month),
							   NewDailyDrawList = {{Year, Month}, [Day|OldList]},
							   true;
						   _ ->
							   LastLoginDrawDays2 = LastDrawDays,
							   NewDailyDrawList = DailyDrawList,
							   false
					   end;
				   true ->
					   LastLoginDrawDays2 = LastDrawDays,
					   NewDailyDrawList = DailyDrawList,
					   false
				end;
			false ->
				LastLoginDrawDays2 = LastDrawDays,
				NewDailyDrawList = DailyDrawList,
				false
		end,
	DailyInfo2 = DailyInfo#daily{lastDrawLoginRewardDays=LastLoginDrawDays2,dailyDrawList=NewDailyDrawList},
    {_, NewList} = NewDailyDrawList,
	role_data:set_dailyInfo(DailyInfo2),
	if LoginRewardFlag->
		   ?sendself(#sc_daily_draw{result=1,newDaily = #p_daily{type=2,value=LoginDays,isDrawed=true},dailyDrawList=NewList});
	   true ->
		   ?sendself(#sc_daily_draw{result=2,newDaily = #p_daily{type=2,value=0,isDrawed=false},dailyDrawList=NewList})		   
	end;
do_draw(3) ->
	#daily{lastDrawLevelUpLevel=LastLevel} = DailyInfo = role_data:get_dailyInfo(),
	RewardLevelList = data_levelup_reward:get_list(),
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			case list_next(LastLevel, RewardLevelList) of
				false ->
					?sendself(#sc_daily_draw{result=2, newDaily=#p_daily{type=3,value=LastLevel,isDrawed=true}});
				{value, NextLevel} ->
					#role{level=Level} = RoleInfo = role_data:get_roleInfo(),
					if NextLevel =< Level ->
						   DailyInfo2 = DailyInfo#daily{lastDrawLevelUpLevel=NextLevel},
						   role_data:set_dailyInfo(DailyInfo2),
						   Reward = data_levelup_reward:get(NextLevel),
						   role_reward:handle_sell_reward_f(RoleInfo, Reward, ?MONEY_ADD_TYPE_LEVEL_UP_GIFT, NextLevel, ""),
						   ?sendself(#sc_daily_draw{result=1, newDaily=#p_daily{type=3,value=NextLevel,isDrawed=true}});
					   true ->
						   ?sendself(#sc_daily_draw{result=2, newDaily=#p_daily{type=3,value=LastLevel,isDrawed=true}})
					end
			end;
		false ->
			?sendself(#sc_daily_draw{result=255, newDaily=#p_daily{type=3,value=LastLevel,isDrawed=true}})
	end.

do_reward_get(?REWARD_TYPE_ONLINE, NeedValue) ->
	
    RoleRewardInfo = role_data:get_roleRewardInfo(),
    #role_reward_info{onlineSecs = OnlineSecs, getList = GetList} = RoleRewardInfo,
    case catch check_online_reward_can_get(NeedValue, OnlineSecs + role_state:get_online_seconds(), GetList) of
        {true, SellReward, NewGetList} ->
            role_data:set_roleRewardInfo(RoleRewardInfo#role_reward_info{getList = NewGetList}),
            role_reward:handle_sell_reward_f(role_data:get_roleInfo(), SellReward, ?MONEY_ADD_TYPE_REWARD_ONLINE, NeedValue, ""),
            ?sendself(#sc_daily_reward_get{type=?REWARD_TYPE_ONLINE, needValue=NeedValue, result=0});
        {false, Reason} ->
            ?sendself(#sc_daily_reward_get{type=?REWARD_TYPE_ONLINE, needValue=NeedValue, result=Reason})
    end;

do_reward_get(?REWARD_TYPE_LEVEL, NeedValue) ->
    RoleRewardInfo = role_data:get_roleRewardInfo(),
    #role_reward_info{getList = GetList} = RoleRewardInfo,
    #role{level=Level} = role_data:get_roleInfo(),
    case catch check_level_reward_can_get(NeedValue, Level, GetList) of
        {true, SellReward, NewGetList} ->
            role_data:set_roleRewardInfo(RoleRewardInfo#role_reward_info{getList = NewGetList}),
            role_reward:handle_sell_reward_f(role_data:get_roleInfo(), SellReward, ?MONEY_ADD_TYPE_REWARD_LEVEL, NeedValue, ""),
            ?sendself(#sc_daily_reward_get{type=?REWARD_TYPE_LEVEL, needValue=NeedValue, result=0});
        {false, Reason} ->
            ?sendself(#sc_daily_reward_get{type=?REWARD_TYPE_LEVEL, needValue=NeedValue, result=Reason})
    end;

do_reward_get(?REWARD_TYPE_DAYS, NeedValue) ->
    RoleRewardInfo = role_data:get_roleRewardInfo(),
    #role_reward_info{getList = GetList, days=Days} = RoleRewardInfo,
    case catch check_days_reward_can_get(NeedValue, Days, GetList) of
        {true, SellReward, NewGetList} ->
            role_data:set_roleRewardInfo(RoleRewardInfo#role_reward_info{getList = NewGetList,lastDays=calendar:date_to_gregorian_days(erlang:date())}),
            role_reward:handle_sell_reward_f(role_data:get_roleInfo(), SellReward, ?MONEY_ADD_TYPE_REWARD_DAYS, NeedValue, ""),
            ?sendself(#sc_daily_reward_get{type=?REWARD_TYPE_DAYS, needValue=NeedValue, result=0});
        {false, Reason} ->
            ?sendself(#sc_daily_reward_get{type=?REWARD_TYPE_DAYS, needValue=NeedValue, result=Reason})
    end.

check_days_reward_can_get(NeedValue, Days, GetList) ->
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of 
		true  -> next;
		false -> erlang:throw({false, 255})
	end,
    case NeedValue =:= Days of
        true ->
            next;
        false ->
            erlang:throw({false, 2})
    end,
    SellReward = data_days_reward:get(NeedValue),
    case erlang:is_record(SellReward, sell_reward) of
        true ->
            next;
        false ->
            erlang:throw({false, 3})
    end,
    case lists:keyfind(?REWARD_TYPE_DAYS, 1, GetList) of
        false ->
            {true, SellReward, [{?REWARD_TYPE_DAYS,[NeedValue]}|GetList]};
        {?REWARD_TYPE_DAYS, _} ->
            erlang:throw({false, 1})
    end.

check_online_reward_can_get(NeedValue, OnlineSecs, GetList) ->
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of 
		true  -> next;
		false -> erlang:throw({false, 255})
	end,
    case NeedValue =< OnlineSecs of 
        true ->
            next;
        false ->
            erlang:throw({false, 2})
    end,
    SellReward = data_online_reward:get(NeedValue),
    case erlang:is_record(SellReward, sell_reward) of
        true ->
            next;
        false ->
            erlang:throw({false, 3})
    end,
    case lists:keyfind(?REWARD_TYPE_ONLINE, 1, GetList) of
        false ->
            {true, SellReward, [{?REWARD_TYPE_ONLINE,[NeedValue]}|GetList]};
        {?REWARD_TYPE_ONLINE, GetList2} ->
            case lists:member(NeedValue, GetList2) of
                false ->
                    {true, SellReward, lists:keyreplace(?REWARD_TYPE_ONLINE, 1, GetList, {?REWARD_TYPE_ONLINE, [NeedValue|GetList2]})};
                true ->
                    erlang:throw({false, 1})
            end
    end.

check_level_reward_can_get(NeedValue, Level, GetList) ->
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of 
		true  -> next;
		false -> erlang:throw({false, 255})
	end,
    case NeedValue =< Level of
        true ->
            next;
        false ->
            erlang:throw({false, 2})
    end,
    SellReward = data_level_reward:get(NeedValue),
    case erlang:is_record(SellReward, sell_reward) of
        true ->
            next;
        false ->
            erlang:throw({false, 3})
    end,
    case lists:keyfind(?REWARD_TYPE_LEVEL, 1, GetList) of
        false ->
            {true, SellReward, [{?REWARD_TYPE_LEVEL,[NeedValue]}|GetList]};
        {?REWARD_TYPE_LEVEL, GetList2} ->
            case lists:member(NeedValue, GetList2) of
                false ->
                    {true, SellReward, lists:keyreplace(?REWARD_TYPE_LEVEL, 1, GetList, {?REWARD_TYPE_LEVEL, [NeedValue|GetList2]})};
                true ->
                    erlang:throw({false, 1})
            end
    end.

do_get_reward_list() ->
    RoleRewardInfo = role_data:get_roleRewardInfo(),
    RewardInfoList =
        lists:foldr(fun(Type, AccRewardInfoList) ->
                            SpecTypeInfo =
                                case Type of
                                    ?REWARD_TYPE_ONLINE ->
                                        get_online_info(RoleRewardInfo);
                                    ?REWARD_TYPE_LEVEL ->
                                        get_level_info(RoleRewardInfo);
                                    ?REWARD_TYPE_DAYS ->
                                        get_days_info(RoleRewardInfo)
                                end,
                            [SpecTypeInfo|AccRewardInfoList]
                    end, [], [?REWARD_TYPE_ONLINE, ?REWARD_TYPE_LEVEL, ?REWARD_TYPE_DAYS]),
    ?sendself(#sc_daily_reward_list{list=RewardInfoList}).

get_online_info(#role_reward_info{onlineSecs = OnlineSecs, getList = GetList}) ->
    case lists:keyfind(?REWARD_TYPE_ONLINE, 1, GetList) of
        false ->
            GetList2 = [];
        {?REWARD_TYPE_ONLINE, GetList2} ->
            next
    end,
    RewardList =
        lists:map(fun(Key) ->
                    #p_daily_reward{needValue=Key, isGet=lists:member(Key, GetList2), reward=role_reward:transform2p_mail_reward(data_online_reward:get(Key))}
                  end, data_online_reward:get_list()),
    #p_daily_reward_info{type=?REWARD_TYPE_ONLINE, nowValue=OnlineSecs+role_state:get_online_seconds(), list=RewardList}.

get_level_info(#role_reward_info{getList = GetList}) ->
    case lists:keyfind(?REWARD_TYPE_LEVEL, 1, GetList) of
        false ->
            GetList2 = [];
        {?REWARD_TYPE_LEVEL, GetList2} ->
            next
    end,
    RewardList =
        lists:map(fun(Key) ->
                    #p_daily_reward{needValue=Key, isGet=lists:member(Key, GetList2), reward=role_reward:transform2p_mail_reward(data_level_reward:get(Key))}
                  end, data_level_reward:get_list()),
    #p_daily_reward_info{type=?REWARD_TYPE_LEVEL, nowValue=(role_data:get_roleInfo())#role.level, list=RewardList}.

get_days_info(#role_reward_info{days = Days, getList = GetList}) ->
    case lists:keyfind(?REWARD_TYPE_DAYS, 1, GetList) of
        false ->
            GetList2 = [];
        {?REWARD_TYPE_DAYS, GetList2} ->
            next
    end,
    RewardList =
        lists:map(fun(Key) ->
                    #p_daily_reward{needValue=Key, isGet=lists:member(Key, GetList2), reward=role_reward:transform2p_mail_reward(data_days_reward:get(Key))}
                  end, data_days_reward:get_list()),
    #p_daily_reward_info{type=?REWARD_TYPE_DAYS, nowValue=Days, list=RewardList}.

list_next(E, [R|_]) when R > E->
	{value,R};
list_next(E, [_|L]) ->
	list_next(E,L);
list_next(_E,[]) ->
	false.

transform_list(List) ->
	{MaxKey,_} = util:keymax(List, 1),
    catch role_role:bc_login_reward(List),
	[{max,MaxKey}|List].
	
	

can_draw_loginreward()->
	#daily{lastDrawLoginRewardDays=LastDrawDays,
		   loginDays=LoginDays0} = role_data:get_dailyInfo(), 
	if LastDrawDays < LoginDays0 ->
				LoginDays = login_days2(LoginDays0),
				{true,LoginDays};
			true->
				false
	end.

