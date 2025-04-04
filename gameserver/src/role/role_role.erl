%% @author admin
%% @doc 主公信息基础协议
%% Created 2013-3-4


-module(role_role).
-compile(export_all).
-include("def_role.hrl").
-include("def_mail.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
cs_role_check_tencent(_) ->
    case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
        true ->
            ?sendself(#sc_role_check_tencent{result=0});
        false ->
            ?sendself(#sc_role_check_tencent{result=1})
    end.

cs_role_change_name(#cs_role_change_name{newName=NewName}) ->
    case catch check_change_name(NewName) of
        {ok, RoleInfo, NeedGold} ->
            do_change_name_gold(RoleInfo, NewName, NeedGold);
        {ok, RoleInfo, BagOther2, DelAcc, UpdateAcc, UpdateLogList} ->
            do_change_name_item(RoleInfo, NewName, BagOther2, DelAcc, UpdateAcc, UpdateLogList);
        {false, Reason} ->
            ?sendself(#sc_role_change_name{result=Reason})            
    end.

do_change_name_gold(#role{roleID=RoleID,familyID=FamilyID}=RoleInfo, NewName, NeedGold) ->
    Role2 = role_lib:deduct_gold_f(RoleInfo, NeedGold, ?MONEY_DEC_TYPE_CHANGE_NAME, 0, ""),
    
    db_sql:update_roleName(RoleID, NewName),
    NewRoleInfo = Role2#role{roleName=NewName},
    
    role_data:set_roleInfo(NewRoleInfo),
    role_lib:update_rolePublic(NewRoleInfo),
    
    pvp_server:update_roleName(RoleID,NewName),
    family_server:update_role_name(RoleID, FamilyID, NewName),
    
    ?sendself(#sc_role_change_name{result=0}).

do_change_name_item(#role{roleID=RoleID,familyID=FamilyID}=RoleInfo, NewName, BagOther, DelAcc, UpdateAcc, UpdateLogList) ->
    role_data:set_bagItem(BagOther),
    
    db_sql:update_roleName(RoleID, NewName),
    NewRoleInfo = RoleInfo#role{roleName=NewName},
    
    role_data:set_roleInfo(NewRoleInfo),
    role_lib:update_rolePublic(NewRoleInfo),
    
    pvp_server:update_roleName(RoleID,NewName),
    family_server:update_role_name(RoleID, FamilyID, NewName),
    
    LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogList),
    {Date, _} = Time = erlang:localtime(),
    behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_CHANGE_NAME, 0, ""),
    case UpdateAcc =/= [] of
        true ->
            UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateAcc],
            ?sendself(#sc_item_update{updateList=UpdateList});
        _ ->
            ignore
    end,
    DelItemIDList = [E||#item{itemUID=E}<-DelAcc],
    ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
    ?sendself(#sc_role_change_name{result=0}).

check_change_name(NewName) ->
    {NeedItemTypeID, NeedGold} = data_common:get(change_name),
    RoleInfo = role_data:get_roleInfo(),
    case item_lib:check_material(NeedItemTypeID, 1) of
        false->
            case role_lib:check_money(RoleInfo, gold, NeedGold) of
                true ->
                    Return = {ok, RoleInfo, NeedGold};
                false ->
                    erlang:throw({false, 1}),
                    Return = ?undefined
            end;
        {true, BagOther2, DelAcc, UpdateAcc, UpdateLogList}->
            Return = {ok, RoleInfo, BagOther2, DelAcc, UpdateAcc, UpdateLogList}
    end,
    case gw:check_roleName(NewName) of
        {false, Reason} ->
            erlang:throw({false, Reason});
        ok ->
            Return
    end.
        

cs_role_login_reward(_) ->
    List = lists:foldr(fun(Key, Acc) ->
                              case erlang:is_integer(Key) of
                                  true ->
                                      Reward = data_login_reward:get(Key),
                                      [role_reward:transform2p_mail_reward(trans2sellreward(Reward))|Acc];
                                  false ->
                                      Acc
                              end
                       end, [], data_login_reward:get_list()),
    ?sendself(#sc_role_login_reward{list=List}).

bc_login_reward(List) ->
    List2 = [role_reward:transform2p_mail_reward(trans2sellreward(Reward))||{_, Reward}<-List],
    broadcast_server:bc(#sc_role_login_reward{list=List2}).

trans2sellreward(Reward) ->
    #daily_reward{coin=Coin,gold=Gold,item=Item,reputation=Rep,newGer=Ger} = Reward,
    #sell_reward{coin=Coin,gold=Gold,item=Item,reputation=Rep,newGer=Ger}.

cs_role_pay_tencent(#cs_role_pay_tencent{value=PayGold}) ->
    case check_pay_tencent(PayGold) of
        {true, PayID,Receipt,Md5,SrcType, RoleID} ->
            %% 将这个充值收据保存，以防止重复
            db_sql:add_pay_receipt(Md5, RoleID, Receipt,SrcType,PayGold),
            activity_server:pay(RoleID, PayGold),
            role_lib:do_pay(PayID,Receipt,Md5,SrcType);
        {amount, Receipt, Md5, SrcType, RoleID} ->
            %% 将这个充值收据保存，以防止重复
            db_sql:add_pay_receipt(Md5, RoleID, Receipt,SrcType,PayGold),
            activity_server:pay(RoleID, PayGold),
            role_lib:do_pay_amount(PayGold,Receipt,Md5,SrcType);
        {false, Reason, ReasonDetail, RoleID} ->
            ?ERR("tencent pay fail, RoleID:~w, ReasonDetail:~w", [RoleID, ReasonDetail]),
            ?sendself(#sc_role_pay_tencent{result=Reason,isGetFirstChargeReward=false,newGoldTotalPaid=0})
    end.

check_pay_tencent(PayGold) when PayGold > 0 ->
    #role{srcType=SrcType, roleID=RoleID, gold=Gold, goldBonus=_GoldBonus} = role_data:get_roleInfo(),
    case SrcType =:= ?ACCOUNT_TYPE_QQ orelse SrcType =:= ?ACCOUNT_TYPE_WEIXIN of
        true ->
            case tencent_pay:get_gold(RoleID, SrcType) of
                {true, NewGold, _NewGoldBonus} ->
                    case Gold + PayGold =:= NewGold of
                        true ->
                            Receipt = integer_to_list(SrcType) ++ integer_to_list(RoleID)++integer_to_list(PayGold)++integer_to_list(util:now())++integer_to_list(tk_id:gen_payID()),
                            Md5 = util:md5(Receipt),
                            case pay_server:check_pay_receipt_duplicate_inlogin2(Receipt,Md5,RoleID,SrcType,PayGold) of
                                true ->
                                    case db_sql:check_pay_receipt_duplicate(Md5) of
                                        true ->
                                            List = [data_pay:get(E)||E<-data_pay:get_list()],
                                            case lists:keyfind(PayGold, #data_pay.payGold, List) of
                                                false ->
                                                    {amount, Receipt, Md5, SrcType, RoleID};
                                                #data_pay{payID=PayID} ->
                                                    {true,PayID, Receipt, Md5, SrcType, RoleID}
                                            end;
                                        false ->
                                            {false, 4, dup_gPay, RoleID}
                                    end;
                                false ->
                                    {false, 4, dup_pay_server, RoleID}
                            end;
                        false ->
                            ?ERR("Gold:~w, PayGold:~w, NewGold:~w", [Gold, PayGold, NewGold]),
                            {false, 1, bad_gold_value, RoleID}
                    end;
                {false, Reason} ->
                    {false, to_code(Reason), Reason, RoleID}
            end;
        false ->
            {false, 4, bad_src_type, RoleID}
    end.

to_code(Reason) ->
    case Reason of
        no_arg ->
            3;
        network_error ->
            2;
        unknow_error ->
            4;
        check_fail ->
            3;
        arg_error ->
            4
    end.

cs_role_change_location(#cs_role_change_location{location=Location})->
	DecodeList = util:gen_utf8_decode_list(erlang:list_to_binary(Location), []),
	Len = length(DecodeList),
	case Len>29 of
		true->
			ignore;
		false->
			RoleInfo = role_data:get_roleInfo(),
			NewRoleInfo = RoleInfo#role{location=Location},
			role_data:set_roleInfo(NewRoleInfo)
	end.

cs_role_change_head(#cs_role_change_head{head=Head})->
	case check(Head) of
		true->
			#role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
			NewRoleInfo = RoleInfo#role{head=Head},
			role_data:set_roleInfo(NewRoleInfo),
			role_lib:update_rolePublic(NewRoleInfo),
			pvp_server:update_head(RoleID,Head),
			?CATCH(role_task_trigger:handle({dispach_task,role_change_head_times})),
			?sendself(#sc_role_change_head{head=Head});
		{false,Reason}->
			?sendself(#sc_role_change_head{result=Reason})
	end.

check(Head)->
	case Head of
		0->
			true;
		_->
			GatherSet = role_data:get_gatherInfo(?GATHER_TYPE_GER),
			case  lists:member(Head,GatherSet) of
				true->
					GerTypeID = Head rem ?SHAPE_BASE,
					#data_ger{gerStar=GerStar} = data_ger:get(GerTypeID),
					case GerStar>=4 of
						true->
							true;
						false->
							{false,3}
					end;
				false->
					{false,2}
			end
	end.

cs_role_info(_) ->
	?INFO("request roleInfo:~w",[get(roleID)]),
	RoleInfo = role_data:get_roleInfo(),
	RoleTimes = role_data:get_roleTimes(),
	RoleTimes2 = refresh_roleTimes(RoleTimes),
	LimitInfo = role_data:get_limitInfo(),
	PushInfo = push_server:get_db_push(RoleInfo#role.roleID),
	Reply = roleInfo2sc_role_info(RoleInfo, RoleTimes2, LimitInfo, PushInfo),
	?sendself(Reply),
	%% 将服务器基本配置常数发给客户端
	VipLevel = RoleInfo#role.vipLevel,
	MaxDscv = role_lib:get_max_dscv_times(VipLevel),
	MaxEnergy = role_lib:get_max_energy(VipLevel),
	?sendself(#sc_role_base_config{dscvMax=MaxDscv,energyMax=MaxEnergy}),
	RoleID=get(roleID),
	hula_server:highlight_push(RoleID),
	nanm_server:highlight_push(RoleID).

refresh_roleTimes(RoleTimes) ->
	#roleTimes{lastBuyTimesRefreshDate=Date} = RoleTimes,
	Today = erlang:date(),
	if Today > Date ->
		   RoleTimes#roleTimes{energyBuyTimes=0,coinBuyTimes=0,coinBossHP=0,dscvBuyTimes=0,ruleBuyTimes=0,pvpBuyTimes=0,lastBuyTimesRefreshDate=Today};
	   true ->
		   RoleTimes
	end.

-define(BUY_TYPE_ENERGY, 1).
-define(BUY_TYPE_DSCV,2).
-define(BUY_TYPE_PVP,3).
-define(BUY_TYPE_RULE,4).
-define(BUY_TYPE_COIN,5).
cs_role_buy_energy(#cs_role_buy_energy{type=Type}) ->
	case check_buy_energy(Type) of
		{true ,_RoleTimes, RoleInfo, NeedGold, _Today, NewBuyTimes, RoleTimes2, NewValue,Add}when Type =/=?BUY_TYPE_COIN ->
			role_data:set_roleTimes(RoleTimes2),
			role_lib:deduct_gold_f(RoleInfo, NeedGold, ?MONEY_DEC_TYPE_BUY_ENERGY, Type, ""),
			#role{roleID=RoleID,vipLevel=VipLevel} = role_data:get_roleInfo(),
			behavior_log_times:log(RoleID, VipLevel, NewBuyTimes, NewValue, Add,Type),
			?sendself(#sc_role_buy_energy{result=1,newBuyTimes=NewBuyTimes,newEnergy=NewValue,type=Type,
                                          getCoin=0,killCoin=0});
		{true ,_RoleTimes, RoleInfo, NeedGold, _Today, NewBuyTimes, RoleTimes2, _NewValue,_Add}->
            {ok, GetCoin, KillCoin, FightInfo} = fight_for_coin(RoleTimes2),
			RoleInfo2 = role_lib:deduct_gold_f(RoleInfo, NeedGold, ?MONEY_DEC_TYPE_BUY_COIN, 0, ""),
			#role{coin=NewCoin} = role_lib:add_coin_f(RoleInfo2, GetCoin + KillCoin, ?MONEY_ADD_TYPE_BUY_COIN, 0, ""),
			?CATCH(role_task_trigger:handle({dispach_task,role_buy_coin_times})),
			#role{roleID=RoleID,vipLevel=VipLevel} = role_data:get_roleInfo(),
			behavior_log_times:log(RoleID, VipLevel, NewBuyTimes, NewCoin, GetCoin + KillCoin,Type),
			?sendself(#sc_role_buy_energy{result=1,newBuyTimes=NewBuyTimes,newEnergy=0,type=Type,
                                          getCoin=GetCoin,killCoin=KillCoin,fightInfo=[FightInfo]});		
		{false, Reason} ->
			?sendself(#sc_role_buy_energy{result=Reason,newEnergy=0,newBuyTimes=0,type=Type,
                                          getCoin=0,killCoin=0})
	end.

fight_for_coin(#roleTimes{coinBossHP=CoinBossHP}=RoleTimes2) ->
    {GerTypeID, GerLevel, GerPos, GerQuality} = data_coin_buy:get(boss_info),
    BossGer = ger_attr:new_ger(GerTypeID, GerLevel, GerQuality, [], []),
    BossGer2 = ?change_pos(BossGer, GerPos),
    case CoinBossHP > 0 of
        true ->
            BossGer3 = BossGer2#ger{gerHp=CoinBossHP},
            BossTotalHp = CoinBossHP;
        false ->
            BossGer3 = BossGer2,
            BossTotalHp = BossGer2#ger.gerHp
    end,
    {Result, FightInfo, {_, [{_,NewBossHp,_}],_,_}} =
        role_fight:new3(role_data:get_fighter_list(),[BossGer3],role_data:get_lieu_add_attr(), {0,0}),

    case Result of
        true ->
            KillCoin = data_coin_buy:get(kill_coin);
        false ->
            KillCoin = 0
    end,
    Damage = BossTotalHp - NewBossHp,
    MinCoin = data_coin_buy:get(min_coin),
    MaxCoin = data_coin_buy:get(max_coin),
    DamageMul = data_coin_buy:get(damage_mul),
    FightCoin = erlang:trunc(Damage * DamageMul),
    GetCoin =
        if
            FightCoin > MaxCoin ->
                MaxCoin;
            FightCoin > MinCoin ->
                FightCoin;
            true ->
                MinCoin
        end,
    role_data:set_roleTimes(RoleTimes2#roleTimes{coinBossHP=NewBossHp}),
    {ok, GetCoin, KillCoin, FightInfo}.

%% 创角时赠送武将
cs_role_select_ger(#cs_role_select_ger{gerTypeID=GerTypeID}) ->
	case check_select_ger(GerTypeID) of
		{true, PosList, LogList, GatherList} ->
			role_data:set_posList(PosList),
			%% 添加主公到图鉴
			role_gather:hook_add_ger_list(GatherList),
			RoleID = role_data:get_roleID(),
			db_sql:log_selectGer(RoleID, GerTypeID),
			db_sql:set_gerList(RoleID, PosList, [],[]),
			{Date, _} = Time = erlang:localtime(),
			behavior_ger_add:log(RoleID, LogList, Date, Time, ?MONEY_ADD_TYPE_CREATE_ROLE, 0, ""),
			?sendself(#sc_ger_update_standlist{posList=ger_lib:gerList2p_ger_pos(PosList)}),
			% 成功
			?sendself(#sc_role_select_ger{result=1}),
            #role{accid=Accid} = role_data:get_roleInfo(),
            gw:post_create_process_to_platform(Accid, RoleID, 1),
			
			RoleInfo = role_data:get_roleInfo(),
			pvp_server:init_role_pvp(RoleInfo),
			role_lib:insert_rolePublic(RoleInfo);
		{false, Reason} ->
			?sendself(#sc_role_select_ger{result=Reason})
	end.

gen_init_ger_data(List) ->
    {PosList, LogList, GatherList} =
        lists:foldr(fun({TypeID, Pos}, {AccPosList, AccLogList, AccGatherList}) ->
                            #ger{gerBase=#gerBase{gerQuality=GerQuality}} = GerInfo = ger_attr:new_ger(TypeID, 1, 0, [], []),
                            NewGerInfo = ?change_pos(GerInfo, Pos),
                            {[NewGerInfo|AccPosList], [[NewGerInfo#ger.gerID,TypeID,1,0]|AccLogList], [{TypeID,GerQuality}|AccGatherList]}
                    end, {[], [], []}, List),
    {true, PosList, LogList, GatherList}.

check_select_ger(GerTypeID) ->
    %% 检查是否是创角状态
    case (role_data:get_posList() =:= [] andalso role_data:get_gerBag() == []) of
        true ->
            case data_common:get({create_ger_list, GerTypeID}) of
                ?undefined ->
                    {false, 3};
                List when erlang:is_list(List) ->
                    gen_init_ger_data(List)
            end;
        false ->
            {false, 2}
    end.

%% 演示战斗
cs_role_demo_fight(#cs_role_demo_fight{type=Type}) ->
    MainGerTypeID = role_data:get_mainGerTypeID(),
    case MainGerTypeID of
        ?undefined ->
            ?sendself(#sc_role_demo_fight{type=Type,fightInfo=[]});
        _ ->
            case data_common:get(is_open_fight_demo) of
                true ->
                    FightInfo = [data_common:get({fight_demo, MainGerTypeID})];
                _ ->
                    FightInfo = []
            end,
			?ERR("FightInfo====================~w",[FightInfo]),
            ?sendself(#sc_role_demo_fight{type=Type,fightInfo=FightInfo})
    end.

gen_demo_fight() ->
    #data_demo_fight{gerID1=MonList} = data_demo_fight:get(1),
    #data_demo_fight{gerID1=MonList2} = data_demo_fight:get(2),
    {_, FightRecord, _} = role_fight:new(MonList2, MonList,{0,0},{0,0}),
    FightRecord.

%% 新的token
cs_role_token(#cs_role_token{token=Token}) ->
	Token2 = list_to_integer(Token,16),
	Token3 = << Token2:256/integer>>,
	RoleID = role_data:get_roleID(),
	push_server:update_token(RoleID, Token3).

cs_role_buy_coin_value(_)->
	Value = data_coin_buy:get(min_coin),
	?sendself(#sc_role_buy_coin_value{value=Value}).

check_buy_energy(Type) ->
	case lists:member(Type, [?BUY_TYPE_DSCV,?BUY_TYPE_ENERGY,?BUY_TYPE_RULE,?BUY_TYPE_PVP,?BUY_TYPE_COIN]) of
		true ->
			Today = erlang:date(),
			#role{vipLevel=VIPlevel} = RoleInfo = role_data:get_roleInfo(),	
			DataVIP = data_vip:get(VIPlevel),
			RoleTimes = role_data:get_roleTimes(),
			case new_buy_times(RoleTimes, Type, Today, DataVIP) of
				false ->
					{false,3};
				{true, NewBuyTimes, RoleTimes2, NewValue,Add} ->
					BuyMoneyList = buy_energy_gold_list(Type),
					NeedGold = lists:nth(NewBuyTimes, BuyMoneyList),
					case role_lib:check_money(RoleInfo, gold, NeedGold) of
						true ->
							{true ,RoleTimes, RoleInfo, NeedGold, Today, NewBuyTimes, RoleTimes2, NewValue,Add};
						false ->
							{false, 2}
					end
			end;
		false ->
			{false, 4}
	end.


buy_energy_gold_list(?BUY_TYPE_ENERGY)  -> data_common:get(buy_energy_gold);
buy_energy_gold_list(?BUY_TYPE_DSCV) 	  -> data_common:get(buy_dscv_gold);
buy_energy_gold_list(?BUY_TYPE_PVP) 	  -> data_common:get(buy_pvp_gold);
buy_energy_gold_list(?BUY_TYPE_RULE) -> data_common:get(buy_rule_gold);
buy_energy_gold_list(?BUY_TYPE_COIN) 	  -> data_common:get(buy_coin_gold).


new_buy_times(RoleTimes, Type,Today, DataVIP) ->
	%?ERR("data_vip=~1000p",[DataVIP]),
	#roleTimes{lastBuyTimesRefreshDate=Date} = RoleTimes,
	{RoleTimes2,NewAlreadyBuy, NewValue, Add} = 
		buy_times(RoleTimes, Type, Today, Date),
	CanBuyTimes = can_buy_times(DataVIP, Type),
	if CanBuyTimes >= NewAlreadyBuy ->
		   {true, NewAlreadyBuy, RoleTimes2,NewValue,Add};
	   true ->
		   false
	end.

can_buy_times(DataVIP, ?BUY_TYPE_COIN) 	 -> DataVIP#data_vip.coinBuyTimes;
can_buy_times(DataVIP, ?BUY_TYPE_ENERGY) 	 -> DataVIP#data_vip.energyBuyTimes;
can_buy_times(DataVIP, ?BUY_TYPE_DSCV) 	 -> DataVIP#data_vip.dscvBuyTimes;
can_buy_times(DataVIP, ?BUY_TYPE_PVP) 	 -> DataVIP#data_vip.pvpBuyTimes;
can_buy_times(DataVIP, ?BUY_TYPE_RULE)  -> DataVIP#data_vip.ruleBuyTimes.


buy_times(RoleTimes, ?BUY_TYPE_ENERGY, Today, Date) 		-> 		
	Add=data_common:get(buy_energy_recover),
	R=RoleTimes#roleTimes.energy+Add,
	if Today > Date ->
		   T=1,
		   RoleTimes2 = RoleTimes#roleTimes{lastBuyTimesRefreshDate=Today,coinBuyTimes=0,energyBuyTimes=T,dscvBuyTimes=0,pvpBuyTimes=0,ruleBuyTimes=0,energy=R};
	   true ->	
		   T=RoleTimes#roleTimes.energyBuyTimes+1,
		   RoleTimes2 = RoleTimes#roleTimes{energyBuyTimes=T,energy=R}
	end,
	{RoleTimes2, T, R, Add};
buy_times(RoleTimes, ?BUY_TYPE_DSCV, Today, Date) 		-> 	
	Add=data_common:get(buy_dscv_recover),		
	R=RoleTimes#roleTimes.discoveryTimes+Add,
	if Today > Date ->
		   T=1,
		   RoleTimes2 = RoleTimes#roleTimes{lastBuyTimesRefreshDate=Today,coinBuyTimes=0,energyBuyTimes=0,dscvBuyTimes=T,pvpBuyTimes=0,ruleBuyTimes=0,discoveryTimes=R};
	   true ->	
		   T=RoleTimes#roleTimes.dscvBuyTimes		+1,
		   RoleTimes2 = RoleTimes#roleTimes{dscvBuyTimes		=T,discoveryTimes=R}
	end,
	{RoleTimes2, T, R, Add};
buy_times(RoleTimes, ?BUY_TYPE_PVP, Today, Date) 		-> 
	Add=data_common:get(buy_pvp_recover),	
	R=RoleTimes#roleTimes.pvpTimes+Add,	
	if Today > Date ->
		   T=1,
		   RoleTimes2 = RoleTimes#roleTimes{lastBuyTimesRefreshDate=Today,coinBuyTimes=0,energyBuyTimes=0,dscvBuyTimes=0,pvpBuyTimes=T,ruleBuyTimes=0,pvpTimes=R};
	   true ->	
		   T=RoleTimes#roleTimes.pvpBuyTimes		+1,
		   RoleTimes2 = RoleTimes#roleTimes{pvpBuyTimes		    =T,pvpTimes=R}
	end,
	{RoleTimes2, T, R, Add};
buy_times(RoleTimes, ?BUY_TYPE_COIN, Today, Date) 		-> 
	if Today > Date ->
		   T=1,
		   RoleTimes2 = RoleTimes#roleTimes{lastBuyTimesRefreshDate=Today,coinBuyTimes=T,energyBuyTimes=0,dscvBuyTimes=0,pvpBuyTimes=0,ruleBuyTimes=0};
	   true ->	
		   T=RoleTimes#roleTimes.coinBuyTimes		+1,
		   RoleTimes2 = RoleTimes#roleTimes{coinBuyTimes		    =T}
	end,
	{RoleTimes2, T, 0, 0};
buy_times(RoleTimes, ?BUY_TYPE_RULE, Today, Date) 		-> 	
	Add=data_common:get(buy_rule_recover),
	R=RoleTimes#roleTimes.ruleTimes+Add,	
	if Today > Date ->
		   T=1,
		   RoleTimes2 = RoleTimes#roleTimes{lastBuyTimesRefreshDate=Today,coinBuyTimes=0,energyBuyTimes=0,dscvBuyTimes=0,pvpBuyTimes=0,ruleBuyTimes=T,ruleTimes=R};
	   true ->	
		   T=RoleTimes#roleTimes.ruleBuyTimes	+1,
		   RoleTimes2 = RoleTimes#roleTimes{ruleBuyTimes     =T,ruleTimes=R}
	end,
	{RoleTimes2, T, R, Add}.

cs_role_get_guide_state(_) ->
	RoleID = role_data:get_roleID(),
	GuideState = db_sql:get_guideState(RoleID),
    case GuideState of
        0 ->
            #role{accid=Accid} = role_data:get_roleInfo(),
            gw:post_create_process_to_platform(Accid, RoleID, 2);
        _ ->
            next
    end,
	?sendself(#sc_role_get_guide_state{value=GuideState}).

cs_role_set_guide_state(#cs_role_set_guide_state{value=Value}) ->
	RoleID = role_data:get_roleID(),
	case db_sql:set_guideState(RoleID, Value) of
		{ok, _} ->
			?sendself(#sc_role_set_guide_state{result=1});
		_ ->
			?sendself(#sc_role_set_guide_state{result=2})
	end.

cs_role_log_guide_state(#cs_role_log_guide_state{value=Value}) ->
    erlang:put(?guideVal, {Value, erlang:localtime()}).

cs_role_setting(_)->
	SettingList = data_common:get(client_setting),
	Reply = #sc_role_setting{idList=SettingList},
    ?sendself(Reply).

cs_role_get_energy(#cs_role_get_energy{click_times=ClickTimes})->
	RoleID = role_data:get_roleID(),
	activity_server:role_energy_activity(RoleID, ClickTimes).

cs_role_weixin_share(_) ->
	#roleTimes{lastWeiXinShareSec=LastWXSec} = RoleTimes = role_data:get_roleTimes(),
	case check_weixin_share_reward(LastWXSec) of
		true ->
			role_data:set_roleTimes(RoleTimes#roleTimes{lastWeiXinShareSec=util:now()}),
			Reward = data_common:get(weixin_share_reward),
			mail_server:send_sys_mail(role_data:get_roleID(), ?MAIL_WEIXIN_SHARE_REWARD, [], "", Reward);
		_ ->
			ignore
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
roleInfo2sc_role_info(RoleInfo, RoleTimes, LimitInfo, PushInfo) ->
    case role_data:get_roleAlienInfo() of
        ?undefined ->
            AlienTimes = 0,
            LastRecoverTime = 0;
        #alien_info{times=AlienTimes, lastRecoverTime=LastRecoverTime} ->
            next
    end,
    case ets:lookup(?ETS_ETC, pay_list) of
        [] ->
            PayList = [];
        [{pay_list, PayList}] ->
            next
    end,
	#sc_role_info{
				  roleID			  = RoleInfo#role.roleID			
				  ,roleName		  = RoleInfo#role.roleName		
				  ,isMale			  = RoleInfo#role.isMale			
				  ,description		  = RoleInfo#role.description		
				  ,familyID			  = RoleInfo#role.familyID			
				  ,level			  = RoleInfo#role.level			
				  ,exp				  = RoleInfo#role.exp				
				  ,coin			  = RoleInfo#role.coin			
				  ,reputation		  = RoleInfo#role.reputation		
				  ,gold			  = RoleInfo#role.gold			
				  ,goldBonus		  = RoleInfo#role.goldBonus		
				  ,goldUsed		  = RoleInfo#role.goldUsed		
				  ,vipLevel		  = RoleInfo#role.vipLevel		
				  ,goldTotalPaid	  = RoleInfo#role.goldTotalPaid	
				  ,energy			  = RoleTimes#roleTimes.energy
				  ,energyBuyTimes =RoleTimes#roleTimes.energyBuyTimes
				  ,challengeGodEnergy = RoleTimes#roleTimes.challengeGodEnergy
				  ,challengeGodBuyTimes = RoleTimes#roleTimes.challengeGodBuyTimes
				  ,nextEnergyTime =RoleTimes#roleTimes.lastEnergyTime+role_lib:to_sec(data_common:get(energy_recover_interval))
				  ,discoveryTimes =RoleTimes#roleTimes.discoveryTimes
				  ,nextDscvTime	 =RoleTimes#roleTimes.lastDscvTime+role_lib:to_sec(data_common:get(dscv_recover_interval))
				  ,pvpTimes		 =RoleTimes#roleTimes.pvpTimes
				  ,ruleTimes	 =RoleTimes#roleTimes.ruleTimes
				  ,randomPVPTimes =0
				  ,singlePVPTimes  =0
				  ,title=RoleInfo#role.title
				  ,encounterFreeNum = LimitInfo#limitInfo.encounterNum
				  ,isPVPPushOpen = PushInfo#d_push.isPVPPushOpen
				  ,isPushNightMute = PushInfo#d_push.isPushNightMute
				  ,dscvBuyTimes = RoleTimes#roleTimes.dscvBuyTimes
				  ,pvpBuyTimes = RoleTimes#roleTimes.pvpBuyTimes
				  ,ruleBuyTimes = RoleTimes#roleTimes.ruleBuyTimes
				  ,coinBuyTimes	= RoleTimes#roleTimes.coinBuyTimes
				  ,weiboCount = role_data:get_weibo_count()
				  ,nextPvpTime = RoleTimes#roleTimes.lastPvpTime + role_lib:to_sec(data_common:get(pvp_recover_interval))
				  ,nextRuleTime = RoleTimes#roleTimes.lastRuleTime + role_lib:to_sec(data_common:get(rule_recover_interval))
				  ,lastWeiXinShareSec = RoleTimes#roleTimes.lastWeiXinShareSec
				  ,head=RoleInfo#role.head
                  ,payExtReward = RoleInfo#role.payExtReward
                  ,isFailed = RoleInfo#role.isFailed
                  ,alienTimes=AlienTimes
                  ,lastAlienTime=LastRecoverTime + role_lib:to_sec(data_common:get(alien_recover_interval))
                  ,payList=PayList
				 }.

get_coin_role_can_buy(RoleLevel) ->
    case data_coin_buy:get(RoleLevel) of
        Value when erlang:is_integer(Value) ->
            Value;
        _ ->
            0
    end.


%% 演示战斗配置预处理
data_demo_fight_transform_list(List) ->
	[data_demo_fight_transform(E)||E<-List].

data_demo_fight_transform(E) ->
	#data_demo_fight{gerID1=D1,gerID2=D2,gerID3=D3,gerID4=D4,gerID5=D5,gerID6=D6}=E,
	MonList = role_battle:gen_mon_list(D1, D2, D3, D4, D5, D6),
	E#data_demo_fight{gerID1=MonList,gerID2=0,gerID3=0,gerID4=0,gerID5=0,gerID6=0}.

check_weixin_share_reward(LastWXSec)->
	{LastDate,_} = util:seconds_to_datetime(LastWXSec),
	LastWeekDay = calendar:day_of_the_week(LastDate),
	{RefreshD,RefreshC} = data_common:get(weixin_share_time),
	RefreshSec = util:datetime_to_seconds({LastDate,RefreshC}) + (RefreshD - LastWeekDay) * ?ONE_DAY_SECONDS,
	case RefreshSec > util:now() of 
		true ->
			false;
		_ ->
			true
	end.

cs_role_suggest_open(#cs_role_suggest_open{}) ->
    ?sendself(#sc_role_suggest_open{is_open=data_common:get(is_open_suggest)}).

cs_role_suggest(#cs_role_suggest{title=Title, content=Content}) ->
    case catch check_role_suggest(Title, Content) of
        {ok, Accid, RoleID, RoleName} ->
            db_sql:log_suggest(RoleID, Accid, RoleName, Title, Content, erlang:localtime()),
            ?sendself(#sc_role_suggest{result=0});
        {error, Reason} ->
            ?sendself(#sc_role_suggest{result=Reason})
    end.

check_role_suggest(Title, Content) ->
    #role{accid=Accid, roleID=RoleID, roleName=RoleName} = role_data:get_roleInfo(),
    case data_common:get(is_open_suggest) of
        true ->
            next;
        false ->
            erlang:throw({error, 1})
    end,
    case db_sql:get_recent_suggest_datetime(RoleID) of
        ?undefined ->
            next;
        Timestamp ->
            Now = util:now(),
            case Now - Timestamp > data_common:get(suggest_interval_seconds) of
                true ->
                    next;
                false ->
                    erlang:throw({error, 2})
            end
    end,
    case Title of
        [] ->
            erlang:throw({error, 3});
        _ ->
            case erlang:length(Title) > data_common:get(suggest_title_length) of
                false ->
                    next;
                true ->
                    erlang:throw({error, 4})
            end
    end,
    case Content of
        [] ->
            erlang:throw({error, 5});
        _ ->
            case erlang:length(Content) > data_common:get(suggest_content_length) of
                false ->
                    next;
                true ->
                    erlang:throw({error, 6})
            end
    end,
    {ok, Accid, RoleID, RoleName}.



