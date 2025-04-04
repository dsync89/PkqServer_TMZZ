%% @author admin
%% @doc 角色基础接口
%% Created 2013-3-4


-module(role_lib).
-compile(export_all).
-include("def_role.hrl").
-include("def_reward.hrl").
-include("def_mail.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
gw(RoleID) ->
	whereis(gatewayRegName(RoleID)).

gatewayRegName(RoleID) ->
	list_to_atom("gw"++integer_to_list(RoleID)).

regName(RoleID) ->
	list_to_atom("role"++integer_to_list(RoleID)).

pid(RoleID) ->
	whereis(regName(RoleID)).

send_client_no_block(RoleID, Info) ->
    case catch process_info(gw(RoleID),dictionary) of
    {dictionary, [{socket,Socket}]} ->
        ?CATCH(tk_misc:send_sock_no_block(Socket, Info));
    _ ->
    ignore
end.

send_client(RoleID, Info) ->
	case catch process_info(gw(RoleID),dictionary) of
	{dictionary, [{socket,Socket}]} ->
		?CATCH(tk_misc:send_sock(Socket, Info));
	_ ->
	ignore
end.
	%catch erlang:send(gatewayRegName(RoleID), {send_client, Info}).

send_client2(GatewayPid, Info) ->
	case catch process_info(GatewayPid,dictionary) of
	{dictionary, [{socket,Socket}]} ->
		?CATCH(tk_misc:send_sock(Socket, Info));
	_ ->
	ignore
end.
	%catch erlang:send(GatewayPid, {send_client, Info}).

send_client_force(GatewayPid, Info)->
	case catch process_info(GatewayPid, dictionary) of
		{dictionary, [{socket, Socket}]} ->
			?CATCH(tk_misc:send_sock_force(Socket, Info));
		_ ->
			ignore
	end.

get_sock(RoleID) ->
	case process_info(gw(RoleID),dictionary) of
	{dictionary, [{socket,Socket}]} ->
		Socket;
	_ ->
	undefined
end.
	
send_server(RoleID, Info) ->
	erlang:send(regName(RoleID), Info).
	
send_pid(Pid, Info)->
	erlang:send(Pid, Info).

call_server(RoleID, Info)->
	gen_server:call(regName(RoleID), Info).

send_self(Info) ->
	tk_misc:send_sock(get(?socket), Info).

send_every_server_msg(Info)->
	[?CATCH(send_server(E, Info))||{E,_}<-ets:tab2list(?ETS_ROLE_ONLINE)].

join_online_table(RoleID, _GateWayPid, Socket, SrcType) ->
	catch broadcast_server:join(Socket),
	ets:insert(?ETS_ROLE_ONLINE, {RoleID,SrcType}).

leave_online_table(RoleID) ->	
	ets:delete(?ETS_ROLE_ONLINE, RoleID),
	catch fire_server:role_exit_firecrack(RoleID),
	catch nanm_server:offline(RoleID),
	catch hula_server:offline(RoleID).

is_online(RoleID) ->
	ets:member(?ETS_ROLE_ONLINE, RoleID).


%% @doc 加经验，计算等级成长，
-spec add_exp(#role{}, ?int32) -> 
		  {level_up, NewLevel::?int16, NewExp::?int64} | 
			  {level_not_up, OldLevel::?int16, NewExp::?int64} | 
			  {level_max, OldLevel::?int16, OldExp::?int64}.
add_exp(Role, AddExp) ->
	#role{exp=Exp,level=Level}=Role,
	MaxLevel = data_common:get(max_role_level),
	MaxExp = data_role_level:get(MaxLevel+1)-1,
	if MaxExp > Exp->
		   NewExp=erlang:min(MaxExp, Exp+AddExp),
		   NewLevel = data_role_exp:get(NewExp),
		   case NewLevel >= data_levelRank:get(baseLevel) of
			   true ->
				   {ServerDate,_} = data_setting:get(serverOpenTime),
				   TimePoint = data_levelRank:get(timePoint),
				   APeriod = data_levelRank:get(activityPeriod),
				   STime = util:datetime_to_seconds({ServerDate, TimePoint}) + APeriod  * ?ONE_DAY_SECONDS,
				   case util:now() > STime of
					   true ->
						   ignore;
					   _ ->
                           ignore
%% 						   levelRank_server:refresh_ranker_info(RoleID, RoleName, NewExp)
				   end;
			   _ ->
				   ignore
		   end,
		   if NewLevel > Level ->
				  {level_up, NewLevel, NewExp};
			  true ->
				  {level_not_up, Level, NewExp}
		   end;
	   true ->
		   {level_max, Level, Exp}
	end.

%% @doc 是否拥有id为GerID的武将
has_gerID(GerID) ->
	case role_data:get_ger(GerID) of
		false  ->
			false;
		_ ->
			true
	end.

reward2p_reward(Reward) ->
	#reward{coin=Coin,roleExp=RoleExp,gold=Gold,reputation=Reputation} = Reward,		   
	#p_reward{coin=Coin,roleExp=RoleExp,gold=Gold,reputation=Reputation}.

%% @doc 角色属性更新
notify_update(Record) ->
	?sendself(Record).

to_sec({A,B,C}) ->
	A*3600+B*60+C.

-define(maxPvp, (data_common:get(max_pvp_times))).
-define(maxRule, (data_common:get(max_rule_times))).
%% @doc 初始化体力、探索次数回复
init_roleTimes_recover_timer(RoleTimes, RoleInfo, AlienInfo) ->
	NowSec = util:now(),	
	#roleTimes{energy=Energy0, 
			   	challengeGodEnergy=ChallengeGodEnergy,
			    challengeGodBuyTimes=ChallengeGodBuyTimes,
				lastChallengeGodDate=LastCGD,
			    refreshLieuTimes = RefreshLieuTimes,
			   	lastEnergyTime=LastET,
			   	discoveryTimes=Dscv,
			   	lastDscvTime=LastDscv,
			   	pvpTimes=PvpTimes,
			   	ruleTimes=RuleTimes,
				weiboCount=WeiboCount,
				nextWeiboCountRefreshSec=NextWeiboCountRefreshSec,
				lastPvpTime = LastPvpTime,
				lastRuleTime = LastRuleTime
			  } =RoleTimes,
    case RoleInfo of
        #role{vipLevel=VipLevel,level=RoleLevel} ->
            next;
        _ ->
            VipLevel = 0,
            RoleLevel = 0
    end,
	{LastLogOutDate,_} = 
		case RoleInfo of
			#role{lastLogoutTime=RLLT} ->
				util:seconds_to_datetime(RLLT);
			_ ->
				{{1970,7,1},{0,0,0}}
		end,
	RefreshLieuFreeTimes =
		case LastLogOutDate == erlang:date() of
			true ->
				RefreshLieuTimes;
			_ ->
				data_lieu_clo_setting:get(daily_free_refresh)
		end,
	EStartTime = data_common:get(energy_recover_start_time),
	EInterval = to_sec(data_common:get(energy_recover_interval)),
    MaxEnergy = get_max_energy(VipLevel),
    DayNum = calc_offline_day_num(LastLogOutDate),
    ZeroClockAddEnergy = data_common:get(zero_clock_add_energy) * DayNum,
    Energy1 = Energy0 + ZeroClockAddEnergy,
    Energy =
        case Energy1 > MaxEnergy of
            true ->
                MaxEnergy;
            false ->
                Energy1
        end,
	{Energy2, LastET2} = cacl_add_num(Energy, MaxEnergy, LastET, NowSec, EStartTime, EInterval),
	DStartTime = data_common:get(dscv_recover_start_time),
	DInterval = to_sec(data_common:get(dscv_recover_interval)),
	{Dscv2, LastDscv2} = cacl_add_num(Dscv, get_max_dscv_times(VipLevel), LastDscv, NowSec, DStartTime, DInterval),
	PvpStartTime = data_common:get(pvp_recover_start_time),
	PvpInterval = to_sec(data_common:get(pvp_recover_interval)),
	{PvpTimes2, LastPvpTime2} = cacl_add_num(PvpTimes, ?maxPvp, LastPvpTime, NowSec, PvpStartTime, PvpInterval),
	RuleStartTime = data_common:get(rule_recover_start_time),
	RuleInterval = to_sec(data_common:get(rule_recover_interval)),
	{RuleTimes2, LastRuleTime2} = cacl_add_num(RuleTimes, ?maxRule, LastRuleTime, NowSec, RuleStartTime, RuleInterval),
    AStartTime = data_common:get(alien_recover_start_time),
    AInterval = to_sec(data_common:get(alien_recover_interval)),
    case AlienInfo of
        ?undefined ->
            AlienTimes = 0,
            LastAT = 0,
            {_AlienTimes2, LastAT2} = cacl_add_num(AlienTimes, get_max_alien_times(VipLevel, RoleLevel), LastAT, NowSec, AStartTime, AInterval);
        #alien_info{times=AlienTimes, lastRecoverTime=LastAT} ->
            {AlienTimes2, LastAT2} = cacl_add_num(AlienTimes, get_max_alien_times(VipLevel, RoleLevel), LastAT, NowSec, AStartTime, AInterval),
            role_data:set_roleAlienInfo(AlienInfo#alien_info{times=AlienTimes2, lastRecoverTime=LastAT2})
    end,
    
	%% 刷新微博分享次数
	if NowSec >= NextWeiboCountRefreshSec ->
			NewWeiboCount = erlang:max(data_common:get(max_weibo_count),WeiboCount),
			WeiboRefreshTime = data_common:get(weibo_refresh_time),
			NextWeiboSec2 = util:datetime_to_seconds({erlang:date(),WeiboRefreshTime}) + ?ONE_DAY_SECONDS;
	   true ->
		   NewWeiboCount = WeiboCount,
		   NextWeiboSec2 = NextWeiboCountRefreshSec
	end,
	
	{NChallengeGodEnergy, NChallengeGodBuyTimes} = 
		case LastCGD =:= erlang:date() of
			true ->
				{ChallengeGodEnergy, ChallengeGodBuyTimes};
			_ ->

				VipChallengeGodFreeTimes = get_max_challengeGodFreeTimes(VipLevel),
				{VipChallengeGodFreeTimes,0}
		end, 
			 
	RoleTimes2 = RoleTimes#roleTimes{energy=Energy2, 
									 challengeGodEnergy=NChallengeGodEnergy,
									 challengeGodBuyTimes=NChallengeGodBuyTimes,
									 lastChallengeGodDate=erlang:date(),
									 refreshLieuTimes=RefreshLieuFreeTimes,
									 lastEnergyTime=LastET2,
									 discoveryTimes=Dscv2,
									 lastDscvTime=LastDscv2,
									 pvpTimes=PvpTimes2,
									 lastPvpTime=LastPvpTime2,
									 ruleTimes=RuleTimes2,
									 lastRuleTime=LastRuleTime2,	
									 weiboCount=NewWeiboCount,					
									 nextWeiboCountRefreshSec=NextWeiboSec2
									},
	role_data:set_roleTimes(RoleTimes2),

	timer_wheel:plan(LastET2+EInterval, fun add_energy_interval/1),
    timer_wheel:plan(LastAT2+AInterval, fun add_alien_interval/1),
	timer_wheel:plan(LastDscv2+DInterval, fun add_dscv_interval/1),
	timer_wheel:plan(LastPvpTime2+PvpInterval, fun add_pvp_interval/1),
	timer_wheel:plan(LastRuleTime2+RuleInterval, fun add_rule_interval/1).

calc_offline_day_num(LastLogOutDate) ->
    NowDays = calendar:date_to_gregorian_days(erlang:date()),
    LastDays = calendar:date_to_gregorian_days(LastLogOutDate),
    case NowDays > LastDays of
        true ->
            NowDays - LastDays;
        false ->
            0
    end.

%% @doc 增加体力
add_energy_interval(NowSec) ->
	#roleTimes{energy=Energy} = RoleTimes = role_data:get_roleTimes(),
	EInterval = to_sec(data_common:get(energy_recover_interval)),
	NextTick = NowSec + EInterval,
    case role_data:get_roleInfo() of
        #role{vipLevel=VipLevel} ->
            next;
        _ ->
            VipLevel = 0
    end,
	case RoleTimes#roleTimes.energy >= get_max_energy(VipLevel) of
		true ->
			?notify_update(?ra_energy(Energy, NextTick)),
			RoleTimes2 = RoleTimes#roleTimes{lastEnergyTime=NowSec};
		false ->
			?notify_update(?ra_energy(Energy+1, NextTick)),
			RoleTimes2 = RoleTimes#roleTimes{energy=Energy+1, lastEnergyTime=NowSec}
	end,
	role_data:set_roleTimes(RoleTimes2),
	timer_wheel:plan(NextTick, fun add_energy_interval/1).

%% @doc 增加探索次数
add_dscv_interval(NowSec) ->
	#roleTimes{discoveryTimes=D} = RoleTimes = role_data:get_roleTimes(),
	DInterval = to_sec(data_common:get(dscv_recover_interval)),
	NextTick = NowSec + DInterval,
    case role_data:get_roleInfo() of
        #role{vipLevel=VipLevel} ->
            next;
        _ ->
            VipLevel = 0
    end,
	case RoleTimes#roleTimes.discoveryTimes >= get_max_dscv_times(VipLevel) of
		true ->
			?notify_update(?ra_dscv(D, NextTick)),
			RoleTimes2 = RoleTimes#roleTimes{lastDscvTime=NowSec};
		false ->
			?notify_update(?ra_dscv(D+1, NextTick)),
			RoleTimes2 = RoleTimes#roleTimes{discoveryTimes=D+1, lastDscvTime=NowSec}
	end,
	role_data:set_roleTimes(RoleTimes2),
	timer_wheel:plan(NextTick, fun add_dscv_interval/1).

%% 增加异星次数
add_alien_interval(NowSec) ->
    AInterval = to_sec(data_common:get(alien_recover_interval)),
    NextTick = NowSec + AInterval,
    case role_data:get_roleAlienInfo() of
        #alien_info{times=A} = AlienInfo ->
            case role_data:get_roleInfo() of
                #role{vipLevel=VipLevel,level=RoleLevel} ->
                    next;
                _ ->
                    VipLevel = 0,
                    RoleLevel = 0
            end,
            case A >= get_max_alien_times(VipLevel, RoleLevel) of
                true ->
                    ?notify_update(?ra_alien(A, NextTick)),
                    AlienInfo2 = AlienInfo#alien_info{lastRecoverTime=NowSec};
                false ->
                    ?notify_update(?ra_alien(A+1, NextTick)),
                    AlienInfo2 = AlienInfo#alien_info{times=A+1, lastRecoverTime=NowSec}
            end,
            role_data:set_roleAlienInfo(AlienInfo2);
        ?undefined ->
            next
    end,
    timer_wheel:plan(NextTick, fun add_alien_interval/1).

%% @doc 增加pvp次数
add_pvp_interval(NowSec) ->
	#roleTimes{pvpTimes=PvpTimes} = RoleTimes = role_data:get_roleTimes(),
	PvpInterval = to_sec(data_common:get(pvp_recover_interval)),
	NextTick = NowSec + PvpInterval,
	case PvpTimes >= ?maxPvp of
		true ->
			?notify_update(?ra_pvpTimes(PvpTimes, NextTick)),
			RoleTimes2 = RoleTimes#roleTimes{lastPvpTime=NowSec};
		false ->
			?notify_update(?ra_pvpTimes(PvpTimes+1, NextTick)),
			RoleTimes2 = RoleTimes#roleTimes{pvpTimes=PvpTimes+1, lastPvpTime=NowSec}
	end,
	role_data:set_roleTimes(RoleTimes2),
	timer_wheel:plan(NextTick, fun add_pvp_interval/1).

%% @doc 增加夺宝次数
add_rule_interval(NowSec) ->
	#roleTimes{ruleTimes=RuleTimes} = RoleTimes = role_data:get_roleTimes(),
	RuleInterval = to_sec(data_common:get(rule_recover_interval)),
	NextTick = NowSec + RuleInterval,
	case RoleTimes#roleTimes.ruleTimes >= ?maxRule of
		true ->
			?notify_update(?ra_ruleTimes(RuleTimes, NextTick)),
			RoleTimes2 = RoleTimes#roleTimes{lastRuleTime=NowSec};
		false ->
			?notify_update(?ra_ruleTimes(RuleTimes+1, NextTick)),
			RoleTimes2 = RoleTimes#roleTimes{ruleTimes=RuleTimes+1, lastRuleTime=NowSec}
	end,
	role_data:set_roleTimes(RoleTimes2),
	timer_wheel:plan(NextTick, fun add_rule_interval/1).

cacl_add_num(CurNum, MaxNum, LastSec, NowSec, Start, Itv) ->
	DaySec = util:datetime_to_seconds({{1989,7,17}, Start}),
	Inter1 = (NowSec - DaySec ) div Itv,
	NewLastSec = DaySec+Inter1*Itv,
	if CurNum >= MaxNum ->
		   {CurNum, NewLastSec};
	   true ->
		   Inter2 = (LastSec- DaySec ) div Itv,
		   Num = Inter1 - Inter2,
		   {erlang:min(CurNum+Num, MaxNum), NewLastSec}
	end.
		   
	
%% @doc 加银两
add_coin_f(#role{coin=Coin,roleID=RoleID, vipLevel=VipLevel}= RoleInfo, AddCoin, Type, ArgID, Desc) ->
	Coin2 = AddCoin+Coin,
	RoleInfo2 = RoleInfo#role{coin=Coin2},
	{Date, _} = Time = erlang:localtime(),
	behavior_coin_add:log(RoleID, VipLevel, AddCoin, Coin, Date, Time, Type, ArgID, Desc),
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_coin(Coin2)),
	?CATCH(role_task_trigger:handle({dispach_task,role_add_reward,AddCoin,?REWARD_COIN})),
	RoleInfo2.

%% @doc 加元宝
add_gold_f(#role{goldBonus=GoldBonus,roleID=RoleID, vipLevel=VipLevel,srcType=SrcType}= RoleInfo, AddGold, Type, ArgID, Desc) ->
	GoldBonus2 = AddGold+GoldBonus,
	RoleInfo2 = RoleInfo#role{goldBonus=GoldBonus2},
	{Date, _} = Time = erlang:localtime(),
    tencent_pay:add_gold(RoleID, SrcType, AddGold),
	behavior_gold_bonus_add:log(RoleID, VipLevel, AddGold, GoldBonus, Date, Time, Type, ArgID, Desc),
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_goldBonus(GoldBonus2)),
	?CATCH(role_task_trigger:handle({dispach_task,role_add_reward,AddGold,?REWARD_GOLD})),
	RoleInfo2.
		

%% @doc 加声望
add_reputation_f(#role{reputation=Rep,roleID=RoleID, vipLevel=VipLevel}= RoleInfo, AddRep, Type, ArgID, Desc) ->
	Rep2 = AddRep+Rep,
	RoleInfo2 = RoleInfo#role{reputation=Rep2},
	{Date, _} = Time = erlang:localtime(),
	behavior_repu_add:log(RoleID, VipLevel, AddRep, Rep, Date, Time, Type, ArgID, Desc),
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_reputation(Rep2)),
	?CATCH(role_task_trigger:handle({dispach_task,role_add_reward,AddRep,?REWARD_REPU})),
	RoleInfo2.

%% @doc 扣银两
deduct_coin_f(#role{coin=Coin,roleID=RoleID, vipLevel=VipLevel}=RoleInfo, NeedCoin, Type, ArgID, Desc) when Coin >= NeedCoin ->
	Coin2 = Coin-NeedCoin,
	Role2 = RoleInfo#role{coin=Coin2},
	{Date, _} = Time = erlang:localtime(),
	behavior_coin_consume:log(RoleID, VipLevel, NeedCoin, Coin, Date, Time, Type, ArgID, Desc),
	role_data:set_roleInfo(Role2),
	%activity_server:role_consume(coin, RoleID, NeedCoin),
	?notify_update(?ra_coin(Coin2)),
	role_activity:update_rebate_info(1, NeedCoin),
	Role2.


%% @doc 扣消费积分
deduct_score_f(#role{goldUsed=GoldUsed,roleID=RoleID, vipLevel=VipLevel}=RoleInfo, NeedGoldUsed, Type, ArgID, Desc) when GoldUsed >= NeedGoldUsed ->
	GoldUsed2 = GoldUsed -NeedGoldUsed,
	Role2 = RoleInfo#role{goldUsed=GoldUsed2},
	{Date, _} = Time = erlang:localtime(),
	behavior_score_consume:log(RoleID, VipLevel, NeedGoldUsed, GoldUsed, Date, Time, Type, ArgID, Desc),
	role_data:set_roleInfo(Role2),
	?notify_update(?ra_goldUsed(GoldUsed2)),
	Role2.


		
%% @doc 扣声望
deduct_reputation_f(#role{reputation=Reputation,roleID=RoleID, vipLevel=VipLevel}=RoleInfo, NeedReputation, Type, ArgID, Desc) when Reputation>= NeedReputation->
	Reputation2= Reputation-NeedReputation,
	Role2 = RoleInfo#role{reputation=Reputation2},
	{Date, _} = Time = erlang:localtime(),
	behavior_repu_consume:log(RoleID, VipLevel, NeedReputation, Reputation, Date, Time, Type, ArgID, Desc),
	role_data:set_roleInfo(Role2),
	%activity_server:role_consume(repu, RoleID, NeedReputation),
	?notify_update(?ra_reputation(Reputation2)),
	role_activity:update_rebate_info(3,NeedReputation),
	Role2.

%% @doc 扣元宝,优先扣goldBonus
deduct_gold_f(#role{gold=Gold, goldBonus=GoldBonus, goldUsed=GoldUsed, roleID=RoleID, vipLevel=VipLevel, srcType=SrcType}=RoleInfo, NeedGold, Type, ArgID, Desc) ->
	GoldUsed2 = GoldUsed+NeedGold,
	
	{Date, _} = Time = erlang:localtime(),
	
	if GoldBonus >= NeedGold ->
		   GoldBonus2 = GoldBonus - NeedGold,
		   RoleInfo2 = RoleInfo#role{goldBonus=GoldBonus2,goldUsed=GoldUsed2},
           sync_if_is_tencent(RoleID, SrcType, 0, NeedGold, Type, ArgID, Desc),
		   activity_server:role_consume(gold, RoleID, NeedGold),
		   role_activity:update_rebate_info(2,NeedGold),
		   ?notify_update(?ra_goldBonus(GoldBonus2)),
		   ?notify_update(?ra_goldUsed(GoldUsed2)),
			behavior_gold_consume:log(RoleID, VipLevel, 0, NeedGold, Gold, GoldBonus, Date, Time, Type, ArgID, Desc),
		   role_data:set_roleInfo(RoleInfo2),
			RoleInfo2;
	   GoldBonus + Gold >= NeedGold ->
		   Gold2=Gold+GoldBonus-NeedGold,
		   RoleInfo2=RoleInfo#role{gold=Gold2,goldBonus=0,goldUsed=GoldUsed2},
           sync_if_is_tencent(RoleID, SrcType, NeedGold-GoldBonus, GoldBonus, Type, ArgID, Desc),
		   activity_server:role_consume(gold, RoleID, NeedGold),
		   role_activity:update_rebate_info(2,NeedGold),
           behavior_gold_consume:log(RoleID, VipLevel, NeedGold-GoldBonus, GoldBonus, Gold, GoldBonus, Date, Time, Type, ArgID, Desc),
		   role_data:set_roleInfo(RoleInfo2),
		   ?notify_update(?ra_gold(Gold2)),
		   ?notify_update(?ra_goldBonus(0)),
		   ?notify_update(?ra_goldUsed(GoldUsed2)),
		   RoleInfo2;
	   true ->
		   exit(wrong_logic)
	end.

sync_if_is_tencent(RoleID, SrcType, DelGold, DelGoldBonus, Type, ArgID, Desc) ->
    case SrcType =:= ?ACCOUNT_TYPE_QQ orelse SrcType =:= ?ACCOUNT_TYPE_WEIXIN of
        false ->
            next;
        true ->
            case DelGold + DelGoldBonus > 0 of
                true ->
                    case tencent_pay:pay_gold(RoleID, SrcType, DelGold, DelGoldBonus, Type, ArgID, Desc) of
                        true ->
                            next;
                        {false, Reason} ->
                            ?ERR("sync_tencent, RoleID:~w, Reason:~w", [RoleID, Reason]),
                            exit(gold_not_enough)
                    end;
                false ->
                    next
            end
    end.


%% @doc 扣体力
deduct_energy_f(#roleTimes{energy=E}=RoleTimes, NeedE) ->
	E2 = E-NeedE,
	RoleTimes2=RoleTimes#roleTimes{energy=E2},
	role_data:set_roleTimes(RoleTimes2),
	?notify_update(?ra_energy(E2, RoleTimes#roleTimes.lastEnergyTime+role_lib:to_sec(data_common:get(energy_recover_interval)))),
	RoleTimes2.

%% @doc 扣探索次数
deduct_dscv_f(#roleTimes{discoveryTimes=E}=RoleTimes, NeedE, NewCount) ->
	E2 = E-NeedE,
	RoleTimes2=RoleTimes#roleTimes{discoveryTimes=E2,dscvCount=NewCount},
	role_data:set_roleTimes(RoleTimes2),
	?notify_update(?ra_dscv(E2, RoleTimes#roleTimes.lastDscvTime+role_lib:to_sec(data_common:get(dscv_recover_interval)))),
	RoleTimes2.

%% @doc 判断货币是否足够
check_money(#role{roleID=RoleID,srcType=SrcType}=Role, gold, Gold) ->
	Role#role.gold + Role#role.goldBonus >= Gold andalso tencent_pay:check_pay_arg(RoleID, SrcType, Gold);
check_money(Role, coin, Coin) ->
	Role#role.coin >= Coin;
check_money(Role, reputation, Reputation) ->
	Role#role.reputation >= Reputation;
check_money(Role, score, Score) ->
	Role#role.goldUsed >= Score;
check_money(_,_,_) ->
	false.

%% @doc 扣取配置的货币
deduct_money_f(Role, gold, Gold, Type, ArgID, Desc) ->
	deduct_gold_f(Role, Gold, Type, ArgID, Desc);
deduct_money_f(Role, coin, Coin, Type, ArgID, Desc) ->
	deduct_coin_f(Role, Coin, Type, ArgID, Desc);
deduct_money_f(Role, reputation, Reputation, Type, ArgID, Desc) ->
	deduct_reputation_f(Role, Reputation, Type, ArgID, Desc);
deduct_money_f(Role, score, Score, Type, ArgID, Desc) ->
	deduct_score_f(Role,Score, Type, ArgID, Desc);
deduct_money_f(_,_,_, _Type, _ArgID, _Desc) ->
	exit("wrong config").

pvp(TarRoleID) ->
	MyFighterList = role_data:get_fighter_list(),
		RoleLieuAdd = role_data:get_lieu_add_attr(),
	case erlang:whereis(regName(TarRoleID)) of
		?undefined ->
			{TarFighterList,TarLieuAdd} = role_data:get_otherRoleFighter(TarRoleID),
			role_fight:new(MyFighterList, TarFighterList, RoleLieuAdd, TarLieuAdd);
		_ ->
			Ref = erlang:make_ref(),
			Info = {pvp_attack, MyFighterList,RoleLieuAdd, self(), Ref},
			role_lib:send_server(TarRoleID, Info),
			role_fight:get_result(Ref)
	end.

get_max_alien_times(VipLevel, RoleLevel) ->
    NeedVipLevel = data_alien:get(need_vip_level),
    NeedRoleLevel = data_alien:get(need_role_level),
    case RoleLevel >= NeedRoleLevel andalso VipLevel >= NeedVipLevel of
        true ->
            data_common:get(max_alien_times);
        false ->
            0
    end.

get_max_dscv_times(VipLevel) ->
    case data_vip:get(VipLevel) of
        ?undefined ->
            0;
        #data_vip{explore_Num_Max=Val} ->
            Val
    end.

get_max_energy(VipLevel) ->
    case data_vip:get(VipLevel) of
        ?undefined ->
            0;
        #data_vip{energy_Num_Max=Val} ->
            Val
    end.

get_max_challengeGodFreeTimes(VipLevel)->
	case data_vip:get(VipLevel) of
		?undefined ->
			2;
		#data_vip{challengeGodFreeTimes = Val}->
			Val
	end.

get_max_challengeGodBuyTimes(VipLevel)->
	case data_vip:get(VipLevel) of
		?undefined ->
			0;
		#data_vip{challengeGodTimes=Val} ->
			Val
	end.

%% @doc 主公升级hook
hook_level_up(Role, NewLevel) ->
	#role{roleID=RoleID, vipLevel=VipLevel, level=OldLevel, accid=Accid} = Role,
	?CATCH(role_task_trigger:handle({dispach_task,role_up_level,RoleID,NewLevel})),
	pvp_server:update_level(RoleID, NewLevel),
	RoleTimes = role_data:get_roleTimes(),
	MaxDscv = get_max_dscv_times(VipLevel),
	MaxEnergy = get_max_energy(VipLevel),
	AddDscv = data_common:get(level_up_add_dscv),
	AddEnergy = data_common:get(level_up_add_energy),
	#roleTimes{discoveryTimes=Dscv, energy=Energy} = RoleTimes,
	if Dscv >= MaxDscv ->
		   Dscv2 = Dscv;
	   true ->
		   Dscv2 = erlang:min(MaxDscv, Dscv+AddDscv),
		   ?notify_update(?ra_dscv(Dscv2, RoleTimes#roleTimes.lastDscvTime+role_lib:to_sec(data_common:get(dscv_recover_interval))))
	end,
	
	if Energy >= MaxEnergy ->
		   Energy2 = Energy;
	   true ->
		   Energy2 = erlang:min(MaxEnergy, Energy+AddEnergy),
		   ?notify_update(?ra_energy(Energy2, RoleTimes#roleTimes.lastEnergyTime+role_lib:to_sec(data_common:get(energy_recover_interval))))
		   
	end,
    if
        NewLevel >= 10 ->
            catch erlang:send(rank_server, {level_up, RoleID, NewLevel});
        true ->
            next
    end,
    catch send_etc_reward(NewLevel, Accid, RoleID),
	case NewLevel =:= data_treasure_box:get(level_limit) of
		true ->
			role_lib:send_server(RoleID, {route, role_treaHouse, #cs_treaHouse_get_list{}});
		_ ->
			ignore
	end,
	
	RoleTimes2 = RoleTimes#roleTimes{discoveryTimes=Dscv2,energy=Energy2},
	role_data:set_roleTimes(RoleTimes2),
    role_rule:hook_role_levelup(Role#role{level=NewLevel}),
	role_homestead:hook_role_levelup(RoleID,OldLevel, NewLevel),
	Role.

send_etc_reward(5, Accid, RoleID) ->
    case erlang:localtime() < data_etc:get(end_time) of
        true ->
            case catch gen_server:call({global, util:get_platform_server()}, {get_special_reward, Accid rem ?AccidBase}, infinity) of
                {true, IsReward, ReturnGold} ->
                    send_etc_reward2(RoleID, IsReward, ReturnGold);
                Err ->
                    ?ERR("Err:~w", [Err])
            end;
        false ->
            next
    end;
send_etc_reward(_, _, _) ->
    next.

send_etc_reward2(RoleID, IsReward, ReturnGold) ->
    case IsReward of
        true ->
            #data_temp_mail{mailInfoList=[#mail_template{content=Content, reward=Reward}]} = data_temp_mail:get(2),
            mail_server:send_mail(0,"",RoleID, ?MAIL_TYPE_REWARD, ?MAIL_NONE_TEMPLATE_MAIL,[], Content, Reward);
        false ->
            next
    end,
    case ReturnGold > 0 of
        true ->
            #data_temp_mail{mailInfoList=[#mail_template{content=Content2}]} = data_temp_mail:get(3),
            mail_server:send_mail(0,"",RoleID, ?MAIL_TYPE_REWARD, ?MAIL_NONE_TEMPLATE_MAIL,[], Content2, #sell_reward{gold=ReturnGold});
        false ->
            next
    end.
			

get_name(RoleID) ->
	case ets:lookup(?ETS_ROLE_PUBLIC, RoleID) of
		[#rolePublic{}=RolePublic] ->
			RolePublic#rolePublic.roleName;
		[] ->
			case db_sql:get_roleInfo(RoleID) of
				#role{roleName=Name} ->
					Name;
				_ ->
					""
			end
	end.

%% @doc 非玩家A逻辑进程，扣取玩家A元宝的接口
%% return: ok|not_exist|not_enough_gold
%% 离线扣玩家钱的思路：如果玩家在线，则通知他的role_server，扣取，
%%				如果玩家不在线，则将这次扣钱操作记录到表dbOfflineDeductGold中，等玩家上线时，处理这次扣取
%% 为什么要这么做？减小复杂度，保证服务器开放时，写玩家相关数据的只有玩家的进程。
%% 如果不这样，则 ：在玩家重复登录的间歇、跨进程扣钱操作，发生在同一时刻时，有几率出现脏数据bug
deduct_gold_d(RoleID, Type, DeductGold) ->
	Msg = {deduct_gold_d, Type, DeductGold},
	case catch role_lib:send_server(RoleID, Msg) of
		{'EXIT',{badarg,_}} ->
			deduct_gold_d2(RoleID, DeductGold);
		Msg ->
			%% 异步扣取
			async
	end.


deduct_gold_d2(RoleID, DeductGold) ->
	F = fun() ->
				%% 读取玩家信息
				case db_sql:get_gold(RoleID) of
					{Gold,GoldBonus} ->
						%% 读取已扣取离线元宝
						AlreadyDeductedGold = db_sql:get_offlineDeductGold(RoleID),
						%% 判断元宝是否足够
						if Gold + GoldBonus >= DeductGold+AlreadyDeductedGold ->
							   db_sql:set_offlineDeductGold(RoleID, AlreadyDeductedGold+DeductGold);
						   true ->
							   {false, not_enough_gold}
						end;
					_ ->
						{false, not_exist}
				end
		end,
	case F() of
		{ok,_} ->
			ok;
		{false,not_exist} ->
			not_exist;
		_ ->
			not_enough_gold
	end.

do_pay(AppItemID, Receipt, Md5, SrcType) ->
	#data_pay{payBonus=PayBonus,payGold=PayGold} = data_pay:get(AppItemID),
    #role{roleID=RoleID,accid=Accid,gold=Gold,goldBonus=GoldBonus,goldTotalPaid=GoldTotalPaid,vipLevel=CurVipLevel,deviceID=DeviceID,payExtReward=PayExtReward} = Role = role_data:get_roleInfo(),
    DayBonus = get_day_bonus(PayGold),
    {PayExtReward2, HaveExt} = check_pay_ext_reward(AppItemID, PayExtReward),
	if GoldTotalPaid == 0 ->
		   %% 首充加倍，且有礼包
		   BonusRate = data_pay_reward:get(first_pay_gold_rate),
           case HaveExt of
               false ->
                   PayBonus2 = erlang:max(0,(PayBonus+PayGold)*BonusRate- PayGold);
               true ->
                   PayBonus2 = DayBonus
           end,
		   Gold2 = Gold+PayGold,
		   GoldBonus2 = GoldBonus+PayBonus2,
		   GoldTotalPaid2 = GoldTotalPaid+PayGold,
		   IsFirstPaid = true;
	   true ->
           case HaveExt of
               false ->
                   PayBonus2 = PayBonus,
                   GoldBonus2 = GoldBonus+PayBonus2;
               true ->
                   PayBonus2 = DayBonus,
                   GoldBonus2 = GoldBonus+PayBonus2
           end,
		   IsFirstPaid = false,
		   Gold2 = Gold+PayGold,
		   GoldTotalPaid2 = GoldTotalPaid+PayGold
	end,
	VipLevel2 = cacl_vipLevel(CurVipLevel,GoldTotalPaid2),
    hook_vip_level_change(VipLevel2, CurVipLevel),
    Role2 = Role#role{goldBonus=GoldBonus2,gold=Gold2,goldTotalPaid=GoldTotalPaid2,vipLevel=VipLevel2,payExtReward=PayExtReward2},
    case PayBonus2 > 0 of
        true ->
            tencent_pay:add_gold(RoleID, SrcType, PayBonus2);
        false ->
            next
    end,
    role_data:set_roleInfo(Role2),
	behavior_gold_pay_add:log(Role#role.roleID, CurVipLevel, PayGold, Gold, PayBonus2, GoldBonus, ?MONEY_ADD_TYPE_NORMAL_PAY ,AppItemID,Md5,Accid,DeviceID,SrcType),
	if VipLevel2 =/= CurVipLevel->
		   RoleTimes=role_data:get_roleTimes(),
		   VipChallengeGodFreeTimes = get_max_challengeGodFreeTimes(VipLevel2),
		   VipChallengeGodBuyTimes = RoleTimes#roleTimes.challengeGodBuyTimes,%get_max_challengeGodBuyTimes(VipLevel2),
		   NewChallengeGodEnergy = VipChallengeGodFreeTimes - (get_max_challengeGodFreeTimes(CurVipLevel) - RoleTimes#roleTimes.challengeGodEnergy),
		   RoleTimes2 = RoleTimes#roleTimes{challengeGodEnergy=NewChallengeGodEnergy,
											challengeGodBuyTimes=VipChallengeGodBuyTimes
											},
		   role_data:set_roleTimes(RoleTimes2),
           MaxDscv = role_lib:get_max_dscv_times(VipLevel2),
           MaxEnergy = role_lib:get_max_energy(VipLevel2),
           ?sendself(#sc_role_base_config{dscvMax=MaxDscv,energyMax=MaxEnergy}),
		   ?notify_update(?ra_vipLevel(VipLevel2, NewChallengeGodEnergy,VipChallengeGodBuyTimes)),
           role_road:add_vip_times(VipLevel2),
		   role_lib:insert_rolePublic(Role2);
	   true ->
		   ignore
	end,
	?notify_update(?ra_gold(Gold2)),
	?notify_update(?ra_goldBonus(GoldBonus2)),
	if IsFirstPaid ->
		   FirstPayReward = data_pay_reward:get(first_pay_reward),
		   role_reward:handle_sell_reward_f(Role2, FirstPayReward, ?MONEY_ADD_TYPE_FIRST_PAY, 0, ""),
		   %% 邀请者获奖励
		   #role{roleID=RoleID,roleName=RoleName} = Role2,
		   #limitInfo{inviteRoleID=Inviter} = role_data:get_limitInfo(),
		   if is_integer(Inviter),Inviter>0 ->
				  invite_server:first_pay(RoleID, RoleName, Inviter, PayGold);
			  true ->
				  ignore
		   end;
	   true ->
		   ignore
	end,
	case SrcType of
        0 ->
           ?sendself(#sc_role_pay_ios{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,receipt=Receipt,result=1});
	   	1 ->
		   ?sendself(#sc_role_pay_ios{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,receipt=Receipt,result=1});
		2 ->	   
		   ?sendself(#sc_role_pay_91{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,receipt=Receipt,result=1});
		3 ->
		   ?sendself(#sc_role_pay_uc{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
		4 ->
		   ?sendself(#sc_role_pay_dl{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
		5 ->
		   ?sendself(#sc_role_pay_zz{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
		6 ->
		   ?sendself(#sc_role_pay_360{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
		7 ->
		   ?sendself(#sc_role_pay_wdj{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
		8 ->
		   ?sendself(#sc_role_pay_dk{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
		9 ->
		   ?sendself(#sc_role_pay_mi{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
		10 ->
		   ?sendself(#sc_role_pay_az{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
        ?ACCOUNT_TYPE_QQ ->
            ?sendself(#sc_role_pay_tencent{result=0,isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2});
        ?ACCOUNT_TYPE_WEIXIN ->
            ?sendself(#sc_role_pay_tencent{result=0,isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2});
		_ ->
			case is_integer(SrcType) of
				true ->
					?sendself(#sc_role_pay_zz{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
				_ ->
					ignore
			end
	end,
    ?sendself(#sc_role_update_pay_ext{pay_ext=PayExtReward2}),
	?CATCH(role_task_trigger:handle({dispach_task,role_add_reward,PayGold + GoldBonus2 - GoldBonus,?REWARD_GOLD})),
    role_activity:pay_hook_month_card(PayGold).

do_pay_amount(PayGold, Receipt, Md5, SrcType) ->
    #role{roleID=RoleID,accid=Accid,gold=Gold,goldBonus=GoldBonus,goldTotalPaid=GoldTotalPaid,vipLevel=CurVipLevel,deviceID=DeviceID} = Role = role_data:get_roleInfo(),
    %% 儲值獎勵值
    PayBonus = 0,
    
    if GoldTotalPaid == 0 ->
           %% 首充加倍，且有禮包
           BonusRate = data_pay_reward:get(first_pay_gold_rate),
           PayBonus3 = erlang:max(0,(PayBonus+PayGold)*BonusRate- PayGold),
           PayBonus2 = erlang:min(6500,PayBonus3),
           Gold2 = Gold+PayGold,
           GoldBonus2 = GoldBonus+PayBonus2,
           GoldTotalPaid2 = GoldTotalPaid+PayGold,
           IsFirstPaid = true;
       true ->
           PayBonus2=PayBonus,
           IsFirstPaid = false,
           Gold2 = Gold+PayGold,
           GoldBonus2 = GoldBonus+PayBonus,
           GoldTotalPaid2 = GoldTotalPaid+PayGold
    end,
    VipLevel2 = cacl_vipLevel(CurVipLevel,GoldTotalPaid2),
    hook_vip_level_change(VipLevel2, CurVipLevel),
    Role2 = Role#role{goldBonus=GoldBonus2,gold=Gold2,goldTotalPaid=GoldTotalPaid2,vipLevel=VipLevel2},
    TencentAddGold = GoldBonus2 - GoldBonus,
    case TencentAddGold > 0 of
        true ->
            tencent_pay:add_gold(RoleID, SrcType, TencentAddGold);
        false ->
            next
    end,
    role_data:set_roleInfo(Role2),
    behavior_gold_pay_add:log(Role#role.roleID, CurVipLevel, PayGold, Gold, PayBonus2, GoldBonus, ?MONEY_ADD_TYPE_NORMAL_PAY ,0,Md5,Accid,DeviceID,SrcType),
    if VipLevel2 =/= CurVipLevel->
           RoleTimes=role_data:get_roleTimes(),
           VipChallengeGodFreeTimes = get_max_challengeGodFreeTimes(VipLevel2),
           VipChallengeGodBuyTimes = RoleTimes#roleTimes.challengeGodBuyTimes,
           NewChallengeGodEnergy = VipChallengeGodFreeTimes - (get_max_challengeGodFreeTimes(CurVipLevel) - RoleTimes#roleTimes.challengeGodEnergy),
           RoleTimes2 = RoleTimes#roleTimes{challengeGodEnergy=NewChallengeGodEnergy,
                                            challengeGodBuyTimes=VipChallengeGodBuyTimes
                                            },
           role_data:set_roleTimes(RoleTimes2),
           MaxDscv = role_lib:get_max_dscv_times(VipLevel2),
           MaxEnergy = role_lib:get_max_energy(VipLevel2),
           ?sendself(#sc_role_base_config{dscvMax=MaxDscv,energyMax=MaxEnergy}),
           ?notify_update(?ra_vipLevel(VipLevel2, NewChallengeGodEnergy,VipChallengeGodBuyTimes)),
           role_road:add_vip_times(VipLevel2),
           role_lib:insert_rolePublic(Role2);
       true ->
           ignore
    end,
    ?notify_update(?ra_gold(Gold2)),
    ?notify_update(?ra_goldBonus(GoldBonus2)),
    if IsFirstPaid ->
           FirstPayReward = data_pay_reward:get(first_pay_reward),
           role_reward:handle_sell_reward_f(Role2, FirstPayReward, ?MONEY_ADD_TYPE_FIRST_PAY, 0, ""),
           %% 邀請者獲獎勵
           #role{roleID=RoleID,roleName=RoleName} = Role2,
           #limitInfo{inviteRoleID=Inviter} = role_data:get_limitInfo(),
           if is_integer(Inviter),Inviter>0 ->
                  invite_server:first_pay(RoleID, RoleName, Inviter, PayGold);
              true ->
                  ignore
           end;
       true ->
           ignore
    end,
    case SrcType of
        0 ->
           ?sendself(#sc_role_pay_ios{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,receipt=Receipt,result=1});
        1 ->
           ?sendself(#sc_role_pay_ios{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,receipt=Receipt,result=1});
        2 ->       
           ?sendself(#sc_role_pay_91{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,receipt=Receipt,result=1});
        3 ->
           ?sendself(#sc_role_pay_uc{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
        4 ->
           ?sendself(#sc_role_pay_dl{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
        5 ->
           ?sendself(#sc_role_pay_zz{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
        6 ->
           ?sendself(#sc_role_pay_360{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
        7 ->
           ?sendself(#sc_role_pay_wdj{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
        8 ->
           ?sendself(#sc_role_pay_dk{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
        9 ->
           ?sendself(#sc_role_pay_mi{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
        10 ->
           ?sendself(#sc_role_pay_az{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
        ?ACCOUNT_TYPE_QQ ->
            ?sendself(#sc_role_pay_tencent{result=0,isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2});
        ?ACCOUNT_TYPE_WEIXIN ->
            ?sendself(#sc_role_pay_tencent{result=0,isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2});
        _ ->
            case is_integer(SrcType) of
                true ->
                    ?sendself(#sc_role_pay_zz{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
                _ ->
                    ignore
            end
    end,
    ?CATCH(role_task_trigger:handle({dispach_task,role_add_reward,PayGold + GoldBonus2 - GoldBonus,?REWARD_GOLD})),
    role_activity:pay_hook_month_card(PayGold).

check_pay_ext_reward(ID, PayExtReward) ->
    BitList = to_bit_list(PayExtReward),
    ID2 = ID rem 1000,
    case lists:nth(ID2, BitList) of
        0 ->
            {gen_new_pay_ext_reward(ID2, PayExtReward), true};
        1 ->
            {PayExtReward, false}
    end.

update_pay_ext_reward(Val, LastPayTime) ->
    {Date, _} = util:seconds_to_datetime(LastPayTime),
    case erlang:date() =:= Date of
        true ->
            {Val, LastPayTime};
        false ->
            {0, util:now()}
    end.

gen_new_pay_ext_reward(ID, Val) ->
    case ID of
        1 ->
            Val bor 2#00000001;
        2 ->
            Val bor 2#00000010;
        3 ->
            Val bor 2#00000100;
        4 ->
            Val bor 2#00001000;
        5 ->
            Val bor 2#00010000;
        6 ->
            Val bor 2#00100000;
        7 ->
            Val bor 2#01000000;
        8 ->
            Val bor 2#10000000;
        9 ->
            Val bor 2#10000000
     end.

to_bit_list(Val) when erlang:is_integer(Val) ->
    to_bin_list2(<<Val:32>>, []).

to_bin_list2(<<>>, BitList) ->
%%     ?ERR("BitList:~w", [BitList]),
    BitList;
to_bin_list2(<<Byte:8, Left/binary>>, BitList) ->
    <<Bit8:1, Bit7:1, Bit6:1, Bit5:1, Bit4:1, Bit3:1, Bit2:1, Bit1:1>> = <<Byte:8>>,
    to_bin_list2(Left, [Bit1,Bit2,Bit3,Bit4,Bit5,Bit6,Bit7,Bit8|BitList]).
	
cacl_vipLevel(CurVipLevel, Gold) ->
	NextLevel = CurVipLevel+1,
	case data_vip:get(NextLevel) of
		?undefined ->
			CurVipLevel;
		#data_vip{needPayGold=Need} ->
			if Gold < Need ->
				   CurVipLevel;
			   Gold == Need ->
				   NextLevel;
			   true ->
				   cacl_vipLevel(NextLevel, Gold)
			end
	end.

role2rolePublic(#role{roleName=RoleName,roleID=RoleID,accid=Accid,level=Level,lastLogoutTime=LastLogoutTime,isMale=IsMale,title=Title,fightPower=FightPower,goldTotalPaid=GoldTotalPaid,head=Head,location=Location,vipLevel=VipLevel,srcType=SrcType}) ->
	#rolePublic{roleName=RoleName,
				roleID=RoleID,
                accid=Accid,
				level=Level,
				lastLogoutTime=LastLogoutTime,
				isMale=IsMale,
				title=Title,
				fightPower=FightPower,
				goldTotalPaid=GoldTotalPaid,
				head=Head,
				location=Location,
				viplevel=VipLevel,
				srcType=SrcType
				}.

get_rolePublic(RoleID) ->
	case ets:lookup(?ETS_ROLE_PUBLIC, RoleID) of
		[] ->
			case db_sql:get_roleInfo(RoleID) of
				#role{}=Role ->
					RolePublic = role2rolePublic(Role),
					ets:insert(?ETS_ROLE_PUBLIC, RolePublic),
					RolePublic;
				_ ->
					[]
			end;
		[RolePublic] ->
			RolePublic
	end.

insert_rolePublic(RoleInfo) ->
	ets:insert(?ETS_ROLE_PUBLIC, role2rolePublic(RoleInfo)).

update_rolePublic(Role) ->
	case ets:member(?ETS_ROLE_PUBLIC, Role#role.roleID) of
		true ->
			ets:insert(?ETS_ROLE_PUBLIC, role2rolePublic(Role));
		false ->
			ignore
	end,
	#role{roleID=RoleID,fightPower=FightPower} = Role,
	pvp_server:update_fightPower(RoleID,FightPower),
    race_server:update_role_info(Role).


is_more_main_than(A,B) ->
	#ger{gerBase=#gerBase{gerLevel=LA,gerQuality=QA,gerTypeID=TA}} =A,
	#ger{gerBase=#gerBase{gerLevel=LB,gerQuality=QB,gerTypeID=TB}} =B,
	{LA,QA,TB} > {LB,QB,TA}.
	   
cacl_mainGerTypeID() ->
	MainGerTypeIDList = data_fixed_encounter:get(?mainGerTypeID),
	PosList = role_data:get_posList(),
	List = [Ger||#ger{gerBase=#gerBase{gerTypeID=GerTypeID}}=Ger<-PosList, lists:member(GerTypeID, MainGerTypeIDList)],
	if List == [] ->
		   0;
	   true ->
		   Ger = util:foldl(fun(E,Acc) ->
									case is_more_main_than(E, Acc) of
										true ->
											E;
										false ->
											Acc
									end
							end, hd(List), tl(List)),
		   Ger#ger.gerBase#gerBase.gerTypeID
	end.

hook_vip_level_change() ->
    erlang:send(erlang:self(), reset_vip_days).


hook_vip_level_change(NewVipLevel, CurVipLevel) ->
    case NewVipLevel > CurVipLevel of
        true ->
            erlang:send(erlang:self(), reset_vip_days);
        false ->
            next
    end.

get_day_bonus(PayGold) ->
    case data_pay_reward:get(day_pay_mul) of
        ?undefined ->
            0;
        Mul ->
            PayGold * Mul
    end.
	
%% ====================================================================
%% Internal functions
%% ====================================================================



