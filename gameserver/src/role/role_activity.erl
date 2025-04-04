%% @author admin
%% @doc 活动 玩家处理模块
%% Created 2013-12-7


-module(role_activity).
-compile(export_all).
-include("def_role.hrl").

%% API functions
-export([]).

%% Internal functions 
-export([]).
-define(NULL_REWARD, #sell_reward{coin=0,gerExp=0,gold=0,item=0,newGer=0,reputation=0,roleExp=0}).
%% ====================================================================
%% API functions
%% ====================================================================
cs_activity_day_pay_mul(_) ->
    ?sendself(#sc_activity_day_pay_mul{mul=data_pay_reward:get(day_pay_mul)}).

cs_activity_get_list(#cs_activity_get_list{}) ->
    ActivityList = activity_server:get_activity_list(),
    do_get_list(ActivityList). 
 
cs_activity_info(#cs_activity_info{activityID=ID}) ->
	RoleID = role_data:get_roleID(),
	erlang:send(activity_server, {get_info,RoleID,ID}).

cs_activity_draw(#cs_activity_draw{activityID=ActivityID,drawID=DrawID}) ->
	RoleID = role_data:get_roleID(),
	erlang:send(activity_server, {activity_draw,RoleID,ActivityID,DrawID}).


%% isEmperor: 帝王换届后,上一届帝王和本届帝王都可以领取帝王的连续签到奖励,这里用isEmperor记录,当第一次签到的时候,记录下玩家是否是帝王
%% 当玩家领取奖励的时候,再次获取玩家是否是帝王,然后给玩家发奖.玩家领取连续签到的宝箱后,清理isEmperor的状态。
cs_activity_sign_emperor_info(_)->
	{LastSignDate, SignedDays,_, IsGetBox} = role_data:get_sign_emperor_info(),
	IsSigned = 
		case LastSignDate =:= erlang:date() of
			true ->
			   1;
		   _ ->
			   2
		end,
	{EmperorID, EmperorName} = race_server:get_champion(),
	RoleID = role_data:get_roleID(),
	IsEmperor =
		if RoleID =:= EmperorID ->
			   1;
		   true ->
			   2
		end,
	%?ERR("id:~w,~w,~w",[RoleID, EmperorID,LastSignDate]),
	LastSignDate2 = calendar:date_to_gregorian_days(LastSignDate),
	Today = calendar:date_to_gregorian_days(erlang:date()),
	SignedDays2 = 
		if Today - LastSignDate2 > 1 ->
			0;
		   true ->
			   if SignedDays =:= 5 andalso Today - LastSignDate2 =:= 1 ->
					  0;
				  true ->
			   SignedDays
			   end
		end,
	?sendself(#sc_activity_sign_emperor_info{isSign=IsSigned, signDays=SignedDays2,isGetBox=IsGetBox, isEmperor=IsEmperor, emperorName=EmperorName}).

cs_activity_sign_get_reward(_)->
	{_, SignedDays,_, IsGetBox} =Info= role_data:get_sign_emperor_info(),
	case check_sign_get_reward(SignedDays, IsGetBox) of
		{false, Reason} ->
			?sendself(#sc_activity_sign_get_reward{result=Reason, reward=[]});
		true ->
			do_sign_get_reward(Info)
	end.

cs_activity_sign_up(_)->
	{LastSignDate, SignedDays,_, _} =Info= role_data:get_sign_emperor_info(),
	case check_sign_emperor(LastSignDate, SignedDays) of
		{false, Reason}->
			?sendself(#sc_activity_sign_up{result=Reason, reward=[]});
		{true, Reason, Gold}->
			do_sign_emperor(Reason, Info, Gold)
	end.

cs_activity_levelRank_open(_)->
	{ServerDate, _} = data_setting:get(serverOpenTime),
	TimePoint = data_levelRank:get(timePoint),
	SPeriod = data_levelRank:get(showPeriod),
	APeriod = data_levelRank:get(activityPeriod),
	STime = util:datetime_to_seconds({ServerDate,TimePoint}) + (APeriod + SPeriod) * ?ONE_DAY_SECONDS,
	ATime = util:datetime_to_seconds({ServerDate,TimePoint}) + APeriod * ?ONE_DAY_SECONDS,
	case util:now() > STime of
		true ->
			?sendself(#sc_activity_levelRank_open{stopTime=ATime,endTime=STime,rankerInfoList=[]});
		_ ->
            ?sendself(#sc_activity_levelRank_open{stopTime=ATime,endTime=STime,rankerInfoList=[]})
%% 			levelRank_server:get_rank_info(ATime, STime, role_data:get_roleID())
	end.

cs_activity_levelRank_refresh(_) ->
	{ServerDate,_} = data_setting:get(serverOpenTime),
	TimePoint = data_levelRank:get(timePoint),
	SPeroid = data_levelRank:get(showPeriod),
	APeriod = data_levelRank:get(activityPeriod),
	STime = util:datetime_to_seconds({ServerDate, TimePoint}) + (APeriod + SPeroid) * ?ONE_DAY_SECONDS,
	case util:now() > STime of
		true ->
			?sendself(#sc_activity_levelRank_refresh{rankerInfoList=[]});
		_ ->
            ?sendself(#sc_activity_levelRank_refresh{rankerInfoList=[]})
%% 			levelRank_server:get_rank_info(role_data:get_roleID())
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

do_sign_get_reward({LastSignDate, SignedDays, IsEmperor, _})->
	{EmperorID, _} = race_server:get_champion(),
	RoleID = role_data:get_roleID(),
	BoxID = 
		if RoleID =:= EmperorID orelse IsEmperor ->
			   data_common:get(signEmperorBoxID5);
		   true ->
			   data_common:get(signEmperorBoxID6)
		end,
	
%% 	Reward = {sell_reward,0,0,0,0,[{new_item,BoxID,1,1,0}],0,[]},
	Role = role_data:get_roleInfo(),
%% 	role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_SIGN_EMPEROR, 1, ""),
	case data_box:get({BoxID,0}) of
		[RConfig|_] ->
			Reward = util:random_one_from_weigh_list(RConfig),
			role_reward:handle_sys_reward(Role, [Reward], ?MONEY_ADD_TYPE_SIGN_EMPEROR, 1, ""),
			role_data:set_sign_emperor_info({LastSignDate,SignedDays,0,1}),
			?sendself(#sc_activity_sign_get_reward{result=1, reward=role_reward:transform2p_reward_view([Reward], [])});
		_ ->
			?sendself(#sc_activity_sign_get_reward{result=4, reward=[]})
	end.
	

check_sign_get_reward(SignedDays, IsGetBox)->
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			if SignedDays =:= 5 ->
				   if IsGetBox =:= 1 ->
						  {false, 2};
					  true ->
						  #role{level=Level} = role_data:get_roleInfo(),
						  case Level >= data_common:get(signEmperorNeedLevel) of
							  true ->
								  true;
							  _ ->
								  {false, 3}
						  end
				   end;
			   true ->
				   {false, 3}
			end;
		false ->
			{false, 255}
	end.

do_sign_emperor(0, {D,_,_,_}, Gold)->
	{EmperorID, _} = race_server:get_champion(),
	RoleID = role_data:get_roleID(),
	A = if RoleID =:= EmperorID ->
			   1;
		   true ->
			   0
		end, 
	do_sign_emperor(1,{D,0,A,0},Gold);
do_sign_emperor(1, {_,S,A,B},_Gold)->
	
	{EmperorID, _} = race_server:get_champion(),
	#role{roleID = RoleID} = RoleInfo = role_data:get_roleInfo(),
	JudgeGold = data_common:get(signEmperorJudgeGold),
	IsGoldEnough = not role_lib:check_money(RoleInfo, gold, JudgeGold),
	BoxID = 
		if EmperorID =:= RoleID ->
			   if IsGoldEnough =:= true ->
					  data_common:get(signEmperorBoxID1);
				  true ->
					  data_common:get(signEmperorBoxID2)
			   end;
		   true ->
			   if IsGoldEnough =:= true ->
					  data_common:get(signEmperorBoxID3);
				  true ->
					  data_common:get(signEmperorBoxID4)
			   end
		end,
%% 	Reward = {sell_reward,0,0,0,0,[{new_item,BoxID,1,1,0}],0,[]},
	Role = role_data:get_roleInfo(),
%% 	role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_SIGN_EMPEROR, 0, ""),
	case data_box:get({BoxID,0}) of
		[RConfig|_] ->
			Reward = util:random_one_from_weigh_list(RConfig),
			role_reward:handle_sys_reward(Role, [Reward], ?MONEY_ADD_TYPE_SIGN_EMPEROR, 0, ""),
			role_data:set_sign_emperor_info({erlang:date(),S+1,A,B}),
			?sendself(#sc_activity_sign_up{result=1, reward=role_reward:transform2p_reward_view([Reward], [])});
		X ->
		?ERR("err when get sign emperor box :~w,~w",[X, BoxID]),
			?sendself(#sc_activity_sign_up{result=4, reward=[]})
	end.

check_sign_emperor(LastSignDate, SignedDays)->
	LastSignDate2 = calendar:date_to_gregorian_days(LastSignDate),
	Today = calendar:date_to_gregorian_days(erlang:date()),
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			if Today =:= LastSignDate2 ->
				   {false, 2};
			   true ->
				   #role{level=Level, gold=Gold,goldBonus=GoldBonus} = role_data:get_roleInfo(),
				   case Level >= data_common:get(signEmperorNeedLevel) of
					   true ->
						   if Today - LastSignDate2 =:= 1 ->
								  if SignedDays =:= 5 ->
										 {true ,0, Gold+GoldBonus};%%5, restart
									 true ->
										 {true, 1, Gold+GoldBonus}
								  end;
							  true ->
								  {true, 0, Gold+GoldBonus}
						   end;
					   _ ->
						   {false, 3}
				   end
			end;
		false ->
			{false, 255}
	end.

%% 检查角色VIP等级
check_rebate_vip_level({vip, Lower, Upper}, VipLevel)->
	VipLevel>=Lower andalso VipLevel=<Upper.
%% 检查角色等级
check_rebate_role_level({level, Lower, Upper}, RoleLevel)->
	RoleLevel>=Lower andalso RoleLevel=<Upper.

%% 获取返利信息
cs_activity_rebate_info(#cs_activity_rebate_info{}) ->
	case rebate_server:get_rebate_activity_info() of
		?undefined->
			?sendself(#sc_rebate_info{status=0, startTime=0, closeTime=0});
		#data_activity_rebate{vip=Vip, level=Level, activityName=Name, description=Description, iconSrc=Icon, startTime=StartTime_, closeTime=CloseTime_} ->
			#role{roleID=RoleID, vipLevel=VipLevel, level=RoleLevel} = role_data:get_roleInfo(),
			case check_rebate_vip_level(Vip, VipLevel) andalso check_rebate_role_level(Level, RoleLevel) of
				true->
					StartTime = util:datetime_to_seconds(StartTime_),
					CloseTime = StartTime + rebate_server:relative_time(CloseTime_),
					RebateList = rebate_server:get_rebate_list(RoleID),
					?sendself(#sc_rebate_info{status=1, name=Name, description=Description, icon=Icon, startTime=StartTime,closeTime=CloseTime, rebateList=RebateList});
				false->
					?sendself(#sc_rebate_info{status=0, startTime=0, closeTime=0})
			end
	end.

%% 领取返利奖励
cs_activity_rebate_get_reward(#cs_activity_rebate_get_reward{rebateID=RebateID}) ->
	RoleID = role_data:get_roleID(),
	RebateInfo = rebate_server:get_roleinfo(RoleID),
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			case rebate_server:get_reward(RebateID, RebateInfo) of
				{1, Reward} ->
					#p_rebate_reward{coin=Coin, gold=Gold, reputation=Reputation} = Reward,
					RoleInfo = role_data:get_roleInfo(),
					
					SellReward = #sell_reward{coin=Coin,gerExp=0,gold=Gold,item=0,newGer=0,reputation=Reputation,roleExp=0},
					role_reward:handle_sell_reward_f(RoleInfo, SellReward, ?MONEY_ADD_TYPE_REBATE, RebateID, ""),
					
					update_reward_status(RebateID, RebateInfo),
					?sendself(#sc_rebate_get_reward{result=1, reward=Reward});
				{X, _} ->
					?sendself(#sc_rebate_get_reward{result=X})
			end;
		false ->
			?sendself(#sc_rebate_get_reward{result=255})
	end.

%% 标记奖励已经领取
update_reward_status(RebateID, RebateInfo)->
	RoleID = role_data:get_roleID(),
	case lists:keytake(RebateID, #rebate_info.id, RebateInfo) of
		false ->
			ignore;
		{value, Info, RebateInfo2}->
			NewRebateInfo = [Info#rebate_info{get=true} | RebateInfo2],
			rebate_server:set_roleinfo(RoleID, NewRebateInfo)
	end.

%%===================================================================================

update_rebateInfo_value([C,G,R], 1, Value)->
	[C+Value,G,R];
update_rebateInfo_value([C,G,R], 2, Value)->
	[C,G+Value,R];
update_rebateInfo_value([C,G,R], 3, Value)->
	[C,G,R+Value];
update_rebateInfo_value(Amount,_,_)->
	Amount.

update_rebate_info(Type, Value)->
	case rebate_server:get_rebate_activity_info() of
		?undefined ->
			ignore;
		#data_activity_rebate{vip=Vip, level=Level} ->
			#role{vipLevel=VipLevel, level=RoleLevel} = role_data:get_roleInfo(),
			case check_rebate_vip_level(Vip, VipLevel) andalso check_rebate_role_level(Level, RoleLevel) of
				true->
					RebateID = rebate_server:get_enabled_rebate_id(),
					update_rebate_info({update_rebate_info, RebateID, Type, Value});
				false->
					ignore
			end
	end.

%%	RebateInfo = [ #rebate_info{id=0,amount=[100,200,300], get=true}, #rebate_info{id=1,amount=[100,200,300], get=false}]
update_rebate_info({update_rebate_info, RebateID, Type, Delta})->
	RoleID = role_data:get_roleID(),
	RebateInfo = rebate_server:get_roleinfo(RoleID),
	NewRebateInfo=
		case lists:keytake(RebateID, #rebate_info.id, RebateInfo) of
			false ->
				[#rebate_info{id=RebateID, amount=update_rebateInfo_value([0,0,0],Type,Delta), get=false}|RebateInfo];
			{value, #rebate_info{amount=Amount}=Info, RebateInfo2}->
				[Info#rebate_info{amount=update_rebateInfo_value(Amount,Type,Delta)}|RebateInfo2]
		end,
	rebate_server:set_roleinfo(RoleID, NewRebateInfo),
	?sendself(#sc_rebate_update{}).

hook_zero_clock() ->
    MonthCard = role_data:get_month_card(),
    NewMonthCard = MonthCard#monthCard{dayPayGold=0},
    role_data:set_month_card(NewMonthCard),
    ?sendself(to_month_card_info(NewMonthCard)).

pay_hook_month_card(PayGold) ->
	?ERR("月卡 pay_hook_month_card    :~w\n",[PayGold]),
	%%MonthCard = #monthCard{dayPayGold=DayPayGold} = role_data:get_month_card(),
	%%NewMonthCard = MonthCard#monthCard{dayPayGold=DayPayGold + PayGold},
    %%role_data:set_month_card(NewMonthCard),
    %%?sendself(to_month_card_info(NewMonthCard)).
	%%购买月卡
	activity_month_buy().
	
activity_month_buy() ->
	?ERR("月卡 cs_activity_month_buy    "),
    case catch check_month_buy() of
        {true, NewMonthCard, NeedGold, RoleInfo} ->
            role_lib:deduct_gold_f(RoleInfo, NeedGold, ?MONEY_DEC_TYPE_BUY_MONTH_CARD, 0, ""),
            role_data:set_month_card(NewMonthCard),
            ?sendself(to_month_card_info(NewMonthCard)),
            ?sendself(#sc_activity_month_buy{result=0});
        {false, Reason} ->
            ?sendself(#sc_activity_month_buy{result=Reason})
    end.
	
    

to_month_card_info(#monthCard{endTime=EndTime,drawTime=DrawTime,dayPayGold=DayPayGold}) ->
    {DrawDate, _} = util:seconds_to_datetime(DrawTime),
    DrawDays = calendar:date_to_gregorian_days(DrawDate),
    {EndDate, _} = util:seconds_to_datetime(EndTime),
    EndDays = calendar:date_to_gregorian_days(EndDate),
    NowDays = calendar:date_to_gregorian_days(erlang:date()),
    case EndDays > NowDays of
        true ->
            LeftDays = EndDays - NowDays;
        false ->
            LeftDays = 0
    end,
    case DrawDays =:= NowDays of
        true ->
            IsDraw = true;
        false ->
            IsDraw = false
    end,
    #sc_activity_month{dayPayGold=DayPayGold,
                       leftDays=LeftDays,
                       isDraw=IsDraw,
                       needPayGold=data_common:get(month_card_need_pay),
                       monthPrice=data_common:get(month_card_price),
                       dayGetGold=data_common:get(month_card_draw)}.

cs_activity_month(_) ->
    ?sendself(to_month_card_info(role_data:get_month_card())).

cs_activity_month_buy(_) ->
	?ERR("月卡 cs_activity_month_buy    "),
    case catch check_month_buy() of
        {true, NewMonthCard, NeedGold, RoleInfo} ->
            role_lib:deduct_gold_f(RoleInfo, NeedGold, ?MONEY_DEC_TYPE_BUY_MONTH_CARD, 0, ""),
            role_data:set_month_card(NewMonthCard),
            ?sendself(to_month_card_info(NewMonthCard)),
            ?sendself(#sc_activity_month_buy{result=0});
        {false, Reason} ->
            ?sendself(#sc_activity_month_buy{result=Reason})
    end.

check_month_buy() ->
	?ERR("月卡 check_month_buy    "),
    #monthCard{dayPayGold=DayPayGold, endTime=EndTime} = MonthCard = role_data:get_month_card(),
    RoleInfo = role_data:get_roleInfo(),
    NeedGold = data_common:get(month_card_price),
    case role_lib:check_money(RoleInfo, gold, NeedGold) of
        true ->
            next;
        false ->
            erlang:throw({false, 1})
    end,
    NeedPayGold = data_common:get(month_card_need_pay),
    case DayPayGold >= NeedPayGold of
        true ->
            next;
        false ->
            erlang:throw({false, 2})
    end,
    {EndDate, _} = util:seconds_to_datetime(EndTime),
    EndDays = calendar:date_to_gregorian_days(EndDate),
    NowDays = calendar:date_to_gregorian_days(erlang:date()),
    case EndDays > NowDays of
        true ->
            erlang:throw({false, 3});
        false ->
            next
    end,
    LeftDays = data_common:get(month_card_days),
    {true, MonthCard#monthCard{endTime=util:datetime_to_seconds({erlang:date(), {23,59,59}}) + LeftDays * ?ONE_DAY_SECONDS}, NeedGold, RoleInfo}.

cs_activity_month_draw(_) ->
    case catch check_month_draw() of
        {true, NewMonthCard, RoleInfo, AddGold} ->
            role_data:set_month_card(NewMonthCard),
            role_lib:add_gold_f(RoleInfo, AddGold, ?MONEY_ADD_TYPE_DRAW_MONTH_CARD, 0, ""),
            ?sendself(to_month_card_info(NewMonthCard)),
            ?sendself(#sc_activity_month_draw{result=0});
        {false, Reason} ->
            ?sendself(#sc_activity_month_draw{result=Reason})
    end.

check_month_draw() ->
    RoleInfo = role_data:get_roleInfo(),
    case tencent_pay:check_pay_arg(RoleInfo) of
        true ->
            next;
        false ->
            erlang:throw({false, 255})
    end,
    #monthCard{endTime=EndTime, drawTime=DrawTime} = MonthCard = role_data:get_month_card(),
    {DrawDate, _} = util:seconds_to_datetime(DrawTime),
    DrawDays = calendar:date_to_gregorian_days(DrawDate),
    {EndDate, _} = util:seconds_to_datetime(EndTime),
    EndDays = calendar:date_to_gregorian_days(EndDate),
    NowDays = calendar:date_to_gregorian_days(erlang:date()),
    case DrawDays =:= NowDays of
        true ->
            erlang:throw({false, 1});
        false ->
            next
    end,
    case EndDays > NowDays of
        true ->
            next;
        false ->
            erlang:throw({false, 2})
    end,
    AddGold = data_common:get(month_card_draw),
    {true, MonthCard#monthCard{drawTime=util:now()}, RoleInfo, AddGold}.
    
do_get_list({IconList, DataList}) ->
    {ok, IconList2} = do_get_list2({IconList, DataList}),
    ?sendself(#sc_activity_get_list{iconList=IconList2}).

do_get_list2({IconList, DataList}) ->
    #role{vipLevel=VipLevel, level=RoleLevel} = role_data:get_roleInfo(),
    {IconList2, _} = lists:foldl(fun(Icon, {Acc1,Acc2}) ->
                        [#data_activity{level=TLevel,vip=TVip}|Acc4] = Acc2,
                        %% 检查等级限制
                        case TLevel of
                            {level,MinLevel,MaxLevel} when MinLevel =< RoleLevel andalso MaxLevel >= RoleLevel ->
                                %% 检查vip限制
                                case TVip of
                                    {vip,MinVip,MaxVip} when MinVip =< VipLevel andalso MaxVip >= VipLevel ->
                                        {[Icon|Acc1],Acc4};
                                    _ ->
                                        {Acc1,Acc4}
                                end;
                            _ ->
                                {Acc1, Acc4}
                        end     
                    end, {[],DataList}, IconList),
    {ok, IconList2}.

