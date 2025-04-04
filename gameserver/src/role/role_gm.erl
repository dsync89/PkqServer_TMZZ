%% @author admin
%% @doc gm命令接口
%% Created 2013-4-17


-module(role_gm).
-compile(export_all).
-include("def_role.hrl").
%% API functions
-export([
		]).


%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
add_reputation({add_reputation, Value}) ->
	Role = role_data:get_roleInfo(),
	role_lib:add_reputation_f(Role, Value, ?MONEY_ADD_TYPE_GM_CMD, 0, "").

set_vip({set_vip, Num}) when is_integer(Num)->
    RoleInfo = role_data:get_roleInfo(),
    RoleInfo2 = RoleInfo#role{vipLevel=Num},
    role_data:set_roleInfo(RoleInfo2),
    RoleTimes=role_data:get_roleTimes(),
    VipChallengeGodFreeTimes = role_lib:get_max_challengeGodFreeTimes(Num),
    VipChallengeGodBuyTimes = RoleTimes#roleTimes.challengeGodBuyTimes,%role_lib:get_max_challengeGodBuyTimes(Num),
    NewChallengeGodEnergy = VipChallengeGodFreeTimes - (role_lib:get_max_challengeGodFreeTimes(RoleInfo#role.vipLevel) - RoleTimes#roleTimes.challengeGodEnergy),
    RoleTimes2 = RoleTimes#roleTimes{challengeGodEnergy=NewChallengeGodEnergy,
                                     challengeGodBuyTimes=VipChallengeGodBuyTimes
                                    },
    role_data:set_roleTimes(RoleTimes2),
    ?notify_update(?ra_vipLevel(Num, NewChallengeGodEnergy,VipChallengeGodBuyTimes)),
    MaxDscv = role_lib:get_max_dscv_times(Num),
    MaxEnergy = role_lib:get_max_energy(Num),
    ?sendself(#sc_role_base_config{dscvMax=MaxDscv,energyMax=MaxEnergy}),
    role_road:add_vip_times(Num),
    role_lib:hook_vip_level_change().

set_energy({set_energy, Value}) ->
	RoleTimes = role_data:get_roleTimes(),
	RoleTimes2 = RoleTimes#roleTimes{energy=Value},
	role_data:set_roleTimes(RoleTimes2).

refresh_energy({refresh_energy}) ->
	RoleTimes = role_data:get_roleTimes(),
	RoleTimes2 = RoleTimes#roleTimes{energy=300,discoveryTimes=300,ruleTimes=100,pvpTimes=100},
	role_data:set_roleTimes(RoleTimes2).
	

add_coin({add_coin, AddCoin}) ->	
	Role = role_data:get_roleInfo(),
	role_lib:add_coin_f(Role, AddCoin, ?MONEY_ADD_TYPE_GM_CMD, 0, "").

set_gold({set_gold, Gold}) ->	
	Role = role_data:get_roleInfo(),
	Role2 = Role#role{gold=Gold},
	role_data:set_roleInfo(Role2),
	?notify_update(?ra_gold(Gold)).


set_level({set_level,Level}) ->
	Role = role_data:get_roleInfo(),
	Exp = data_role_level:get(Level+1)-1,
	Role2 = Role#role{level=Level,exp=Exp},
	role_lib:hook_level_up(Role, Level),
		?notify_update(?ra_exp(Exp)),
	?notify_update(?ra_level(Level)),
	role_data:set_roleInfo(Role2).

refresh_today_task(_)->
	role_task:hook_zero_clock().

add_ger({add_ger,GerTypeID}) ->
	ger_lib:add_ger(#new_ger{gerTypeID=GerTypeID,gerLevel=1,gerQuality=0}, ?MONEY_ADD_TYPE_GM_CMD, 0, "").

add_ger2({add_ger2,GerTypeID,Level,Rank}) ->
	ger_lib:add_ger(#new_ger{gerTypeID=GerTypeID,gerLevel=Level,gerQuality=Rank}, ?MONEY_ADD_TYPE_GM_CMD, 0, "").

add_item({add_item, ItemTypeID, Num}) ->
	item_lib:add_item_f([#new_item{itemLevel=32,itemNum=Num,itemRank=1,itemTypeID=ItemTypeID}], ?MONEY_ADD_TYPE_GM_CMD, 0, "").

add_item2({add_item2, ItemTypeID, Num, Level, Rank}) ->
	item_lib:add_item_f([#new_item{itemLevel=Level,itemNum=Num,itemRank=Rank,itemTypeID=ItemTypeID}], ?MONEY_ADD_TYPE_GM_CMD, 0, "").


%% ====================================================================
%% Internal functions
%% ====================================================================

-define(MATCH_LIST, [
					{"H ~d ~d ~d",fun([A,B,C])->["homestead",A,B,C] end},
					 {"H ~d ~d",fun([A,B])->["homestead",A,B] end},
					 {"H ~d",fun([A])->["homestead",A] end},
                    {"fightboss1 ~d ~d ~d", fun([A,B,C]) -> [fightboss1, A, B, C] end},
                    {"fightboss2 ~d ~d ~d", fun([A,B,C]) -> [fightboss2, A, B, C] end},
					{"I ~d ~d级~d阶~d",fun([A,B,C,D])->["ItemID",A,B,C,D] end},
					{"I ~d ~d级~d阶",fun([A,B,C])->["ItemID",A,B,C,1] end},
					{"I ~d ~d阶~d",fun([A,B,C])->["ItemID",A,1,B,C] end},
					{"I ~d ~d",fun([A,B])->["ItemID",A,1,0,B] end},
					{"I ~d",fun([A])->["ItemID",A,1,0,1] end},
					{"G ~d ~d级~d阶~d",fun([A,B,C,D])->["GerID",A,B,C,D] end},
					{"G ~d ~d级~d阶",fun([A,B,C])->["GerID",A,B,C,1] end},
					{"G ~d ~d阶~d",fun([A,B,C])->["GerID",A,1,B,C] end},
					{"G ~d ~d",fun([A,B])->["GerID",A,1,0,B] end},
					{"G ~d",fun([A])->["GerID",A,1,0,1] end},
					 {"~s ~d级~d阶~d",fun(E) -> E end},
					 {"~s ~d级~d阶~d",fun([A,C,B,D]) -> [A,B,C,D] end},
					 {"~s ~d级~d阶",  fun([A,B,C]) ->[A,B,C,1] end},
					 {"~s ~d级~d", 	fun([A,B,D])	->[A,B,0,D] end},
					 {"~s ~d阶~d",	fun([A,C,D]) 	->[A,1,C,D] end},
					 {"~s ~d",		fun([A,D]) ->	[A,1,0,D] end},
					 {"~s",			fun([A]) ->[A,1,0,1] end}
					]).

test_num(NS) ->
	list_to_integer(string:strip(NS, both)).
test("机器人"++NS) ->
    Robot = test_num(NS),
    melee_server:test_robot_sign(Robot),
    test_ok();
test("混战"++NS) ->
    Time = test_num(NS),
    role_melee:call_master_server({melee_start, Time}),
    test_ok();
test("关闭混战") ->
    role_melee:cast_master_server(battle_close),
    test_ok();
test("帮助") ->
	test_error();
test("元宝"++NS) ->
	Num = test_num(NS),
	RoleInfo = role_data:get_roleInfo(),
	RoleInfo2 = RoleInfo#role{gold=Num},
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_gold(Num)),
	test_ok();
test("绑定元宝"++NS) ->
	Num = test_num(NS),
	RoleInfo = role_data:get_roleInfo(),
	RoleInfo2 = RoleInfo#role{goldBonus=Num},
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_goldBonus(Num)),
	test_ok();
test("银两"++NS) ->
	Num = test_num(NS),
	RoleInfo = role_data:get_roleInfo(),
	RoleInfo2 = RoleInfo#role{coin=Num},
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_coin(Num)),
	test_ok();
test("声望"++NS) ->
	Num = test_num(NS),
	RoleInfo = role_data:get_roleInfo(),
	RoleInfo2 = RoleInfo#role{reputation=Num},
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_reputation(Num)),
	test_ok();
test("等级"++NS) ->
	Num = test_num(NS),
	RoleInfo = role_data:get_roleInfo(),
	Exp = data_role_level:get(Num+1)-1,
	RoleInfo2 = RoleInfo#role{level=Num,exp=Exp},
	role_lib:hook_level_up(RoleInfo, Num),
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_exp(Exp)),
	?notify_update(?ra_level(Num)),
	test_ok();
test("官爵"++NS) ->
	Num = test_num(NS),
	RoleInfo = role_data:get_roleInfo(),
	RoleInfo2 = RoleInfo#role{title=Num},
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_title(Num)),
	test_ok();
test("vip"++NS) ->
	Num = test_num(NS),
	RoleInfo = role_data:get_roleInfo(),
	RoleInfo2 = RoleInfo#role{vipLevel=Num},
	role_data:set_roleInfo(RoleInfo2),
	RoleTimes=role_data:get_roleTimes(),
	VipChallengeGodFreeTimes = role_lib:get_max_challengeGodFreeTimes(Num),
	VipChallengeGodBuyTimes = RoleTimes#roleTimes.challengeGodBuyTimes,%role_lib:get_max_challengeGodBuyTimes(Num),
	NewChallengeGodEnergy = VipChallengeGodFreeTimes - (role_lib:get_max_challengeGodFreeTimes(RoleInfo#role.vipLevel) - RoleTimes#roleTimes.challengeGodEnergy),
	?ERR("V:~w",[VipChallengeGodFreeTimes]),
	RoleTimes2 = RoleTimes#roleTimes{challengeGodEnergy=NewChallengeGodEnergy,
									 challengeGodBuyTimes=VipChallengeGodBuyTimes
									},
	role_data:set_roleTimes(RoleTimes2),
	?notify_update(?ra_vipLevel(Num, NewChallengeGodEnergy,VipChallengeGodBuyTimes)),
    MaxDscv = role_lib:get_max_dscv_times(Num),
    MaxEnergy = role_lib:get_max_energy(Num),
    ?sendself(#sc_role_base_config{dscvMax=MaxDscv,energyMax=MaxEnergy}),
    role_road:add_vip_times(Num),
    role_lib:hook_vip_level_change(),
	test_ok();
test("体力"++_) ->
	RoleTimes = role_data:get_roleTimes(),
    case role_data:get_roleInfo() of
        #role{vipLevel=VipLevel} ->
            next;
        _ ->
            VipLevel = 0
    end,
	MaxDscv = role_lib:get_max_dscv_times(VipLevel),
	MaxEnergy = role_lib:get_max_energy(VipLevel),
	MaxRuleTimes = data_common:get(max_rule_times),
	MaxPVPTimes = data_common:get(max_pvp_times),
	RoleTimes2 = RoleTimes#roleTimes{energy=MaxEnergy,
								discoveryTimes=MaxDscv,
								ruleTimes=MaxRuleTimes,
								pvpTimes=MaxPVPTimes},
	role_data:set_roleTimes(RoleTimes2),
	?notify_update(?ra_dscv(MaxDscv, RoleTimes#roleTimes.lastDscvTime+role_lib:to_sec(data_common:get(dscv_recover_interval)))),
	?notify_update(?ra_energy(MaxEnergy, RoleTimes#roleTimes.lastEnergyTime+role_lib:to_sec(data_common:get(energy_recover_interval)))),
	test_ok();	
test("充值"++NS) ->
	Num = test_num(NS),
	List = [data_pay:get(E)||E<-data_pay:get_list()],
	case lists:keyfind(Num, #data_pay.payGold, List) of
		false ->
			test_error();
		#data_pay{payID=PayID} ->
			gen_server:call(pay_server, {func, fun pay_server:do_pay/5, [role_data:get_roleID(), PayID, "", "",0]}),
			test_ok()
	end;
test("guide" ++ NS) ->
    Value = test_num(NS),
    db_sql:set_guideState(role_data:get_roleID(), Value),
    test_ok();
test("genrobot") ->
    gen_account:get_config(),
    test_ok();
test("我是上帝") ->
	user_default:best_fire(role_data:get_roleID()),
	test_ok();
test("华丽跳过") ->
    catch race_server:to_next(),
    test_ok();
test("resetrule") ->
    erlang:send(rule_server, test_reset_rule),
    test_ok();
test("signrule"++NS) ->
    Value = test_num(NS),
    rule_server:test_sign_others(Value),
    test_ok();
test("worldboss1") ->
    catch erlang:send(hula_server, test_state_begin),
    test_ok();
test("worldboss2") ->
    catch erlang:send(nanm_server, test_state_begin),
    test_ok();
test("华丽报名"++NS) ->
    Max = test_num(NS),
    catch race_server:test_sign_others(Max),
    test_ok();
test("刷新日常任务")->
	role_task:add_all_today_task(),
	test_ok();
test(Str)->
	Result =  scanf(Str),
	case Result of
		["ItemID",ItemTypeID,Level,Rank,NumT]->
            Num = limit_num(NumT),
			case data_item:get(ItemTypeID) of
				?undefined->
					test_error();
				_->
					NewItem = #new_item{itemTypeID=ItemTypeID,itemNum=Num,itemRank=Rank,itemLevel=Level},
					item_lib:add_item_f([NewItem], ?MONEY_ADD_TYPE_GM_CMD, 0, ""),
					test_ok()
			end;
		["GerID",GerTypeID,Level,Rank,NumT]->
            Num = limit_num(NumT),
			case data_ger:get(GerTypeID) of
				?undefined->
					test_error();
				_->
					NewGer = lists:duplicate(Num, #new_ger{gerTypeID=GerTypeID,gerLevel=Level,gerQuality=Rank}),
					ger_lib:add_ger_list(NewGer, ?MONEY_ADD_TYPE_GM_CMD, 0, ""),
					test_ok()
			end;
		["homestead",1]->%%重置次数
			RoleID = role_data:get_roleID(),
			role_homestead:test_refresh_times(RoleID),
			test_ok();
		["homestead",2,S]->%%缩短交配冷却时间
			RoleID = role_data:get_roleID(),
			role_homestead:test_refresh_matingCoolSecond(RoleID,S),
			test_ok();
		["homestead",3,Num,S]->%%缩短种子成熟时间
			RoleID = role_data:get_roleID(),
			role_homestead:test_refresh_machine_endSecond(RoleID,Num,S),
			test_ok();
		["homestead",4,Num,S]->%%缩短充能冷却世间
			RoleID = role_data:get_roleID(),
			role_homestead:test_refresh_machine_addEnergyEndS(RoleID,Num,S),
			test_ok();
        [fightboss1, StartRank, StopRank, Timeout] ->
            erlang:send(hula_server, {test_robot_fight, StartRank, StopRank, Timeout}),
            test_ok();
        [fightboss2, StartRank, StopRank, Timeout] ->
            erlang:send(nanm_server, {test_robot_fight, StartRank, StopRank, Timeout}),
            test_ok();
	[Name,Level,Rank,NumT] ->
            Num = limit_num(NumT),
			case find_name(Name) of
				false ->
					test_error();
				{item, ItemTypeID} ->
					NewItem = #new_item{itemTypeID=ItemTypeID,itemNum=Num,itemRank=Rank,itemLevel=Level},
					item_lib:add_item_f([NewItem], ?MONEY_ADD_TYPE_GM_CMD, 0, ""),
					test_ok();
				{ger, GerTypeID} ->
					NewGer = lists:duplicate(Num, #new_ger{gerTypeID=GerTypeID,gerLevel=Level,gerQuality=Rank}),
					ger_lib:add_ger_list(NewGer, ?MONEY_ADD_TYPE_GM_CMD, 0, ""),
					test_ok()
			end;
		_ ->
			test_error()
	end.

limit_num(NumT) ->
    case NumT > 9999 of
        true ->
            9999;
        false ->
            NumT
    end.

scanf(Str) ->
	util:foldl(fun({E,F}, _Acc) ->
						   case io_lib:fread(E, Str) of
							   {ok,List,_} ->
								   {return,F(List)};
							   _ ->
								   next
						   end
				   end, 0, ?MATCH_LIST).

test_error() ->
ErrorMsg = 
"元宝100
绑定元宝100
机器人5
混战300
关闭混战
银两100
声望100
体力刷新
等级20
vip10
方天画戟 100级19阶2把
吕布 60级10阶2头
曹仁魂魄 20个
鬼头骰（一） 1000个
充值60(充值数字必须为充值商店中的元宝数字)
",
	?sendself(#sc_message_test{result=2,errorMsg=ErrorMsg}).

test_ok()->
	?sendself(#sc_message_test{result=1,errorMsg=""}).

find_name(Name) ->
	case find_item(Name) of
		false ->
			find_ger(Name);
		Result ->
			Result
	end.

find_item(Name) ->
	ItemTypeID = 
	util:foldl(fun(E,Acc) ->
		case data_item:get(E) of
			#data_item{itemName=Name,itemTypeID=ID} ->
				{return, ID};
			_ ->
				Acc
		end
	end, 0, data_item:get_list()),
	if ItemTypeID ==0 ->
		false;
		true ->
		{item, ItemTypeID}
	end.

find_ger(Name) ->
	GerTypeID = 
	util:foldl(fun(E,Acc) ->
		case data_ger:get(E) of
			#data_ger{gerName=Name,gerTypeID=ID} ->
				{return, ID};
			_ ->
				Acc
		end
	end, 0, data_ger:get_list()),
	if GerTypeID ==0 ->
		false;
		true ->
		{ger, GerTypeID}
	end.
	
t() ->
	[find_name("方天画戟"),
	find_name("吕布"),
scanf("曹操 1阶1级"),
scanf("方天画戟 2阶2把")].
	
	



