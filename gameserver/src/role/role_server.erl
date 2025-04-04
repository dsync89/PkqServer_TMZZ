%% @author admin
%% @doc 玩家逻辑进程
%% Created 2013-2-20


-module(role_server).
-compile(export_all).
-behaviour(gen_server).
-include("def_role.hrl").
-include("def_weibo_share.hrl").
-include("def_family.hrl").
-export([start_link/7,start/1,stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([]).

-define(interval_dump_tick, 5*60).
-define(stop_role_server_flag, stop_role_server_flag).

stop(RoleServerPid) when is_pid(RoleServerPid) ->
	supervisor:delete_child(role_sup, RoleServerPid),
	catch gen_server:cast(RoleServerPid, stop); 
stop(RoleID) when is_integer(RoleID)->
	stop(role_lib:pid(RoleID)).

start(Args) ->
    supervisor:start_child(role_sup, Args).

start_link(RoleID, GatewayClientPid, Socket, MacAddr, Ip, DeviceID, SrcType) ->	
	?DEBUG("role_server start, ID=~w",[RoleID]),
    gen_server:start_link(?MODULE, [RoleID,  GatewayClientPid, Socket, MacAddr, Ip, DeviceID, SrcType], []).

-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:init/1	
init([RoleID, GatewayClientPid, Socket, MacAddr, Ip, DeviceID, SrcType]) ->
	?INFO("init role_server=~w",[RoleID]),
	process_flag(trap_exit, true),
	random:seed(util:gen_random_seed()),
	%% 开始时间轮
	timer_wheel:init(),
	%% 先注册名称，并加入在线玩家表，避免从数据库读到脏数据
	case role_state:login(RoleID, self()) of
		false ->
			{stop, not_exit};
		true ->
			role_lib:join_online_table(RoleID, GatewayClientPid, Socket, SrcType),
			
			%% 判断加载数据是否成功
			case ?CATCH(init_role_data(RoleID, GatewayClientPid, Socket, MacAddr, Ip,DeviceID)) of
				{'EXIT',_} ->
					role_state:logoff_without_flush(RoleID),
					{stop, load_data_error2};
				{RoleInfo,LastLogoutTime} ->
					
					
					%% 当前秒数
					NowSec = timer_wheel:nowsec(),
					%% 秒循环
					timer_wheel:add_plan(NowSec+1,fun sec_loop/1),
					
					%% 凌晨hook
%% 					NextZeroClockSec = util:datetime_to_seconds({erlang:date(),{0,0,1}}) + ?ONE_DAY_SECONDS,
%% 					timer_wheel:add_plan(NextZeroClockSec, fun hook_zero_clock/0),
					{Hour,_,_} = time(),
					NextZeroClockSec = util:datetime_to_seconds({erlang:date(),{Hour,0,1}}) + ?ONE_HOUR_SECONDS,
					timer_wheel:add_plan(NextZeroClockSec, fun hook_hour/0),
					
					%% link
					erlang:link(GatewayClientPid),
					
					%% 定时持久化
					interval_dump_tick(RoleID),
					notice_family_online(true),
					role_homestead:init_role_homestead(RoleInfo,LastLogoutTime),
					enargy_server:enargy_role_online(RoleID),
					catch etc_server:role_online(RoleID),
					{ok, RoleID}
			end
	end.

interval_dump_tick(RoleID) ->
	Now = timer_wheel:nowsec(),
	TarSec = Now+?interval_dump_tick,
	timer_wheel:add_plan(TarSec, 
						 fun() ->
								 case get(?interval_persist_flag) =:= true andalso role_state:is_stop_server() =:= false of
									 true ->
										 role_persist:do_interval_persist(RoleID),
										 role_data:clear_interval_persist_flag();
									 _ ->
										 ignore
								 end,
                                 ?CATCH(role_data:clear_gold_bonus()),
								 interval_dump_tick(RoleID)
						 end).

cacl_deduct_gold(RoleInfo, DeductGold) ->
	#role{gold=G,goldBonus=GB,goldUsed=GU} = RoleInfo,
	if GB >= DeductGold ->
		   {G, GB-DeductGold, DeductGold+GU};
	   true ->
		   {G+GB-DeductGold, 0, DeductGold+GU}
	end.

init_role_data(RoleID, GatewayClientPid, Socket, MacAddr, Ip, DeviceID) ->
    case role_data:request_role_data(RoleID) of
        {RoleInfo, RoleExtra,GerList, PosList, ListOfGerEquipList, BagEquip, BagItem, GuardGerList,
         RoleGuardInfo, TeamPkInfo, AlienInfo, ShopTreasure, RoleRewardInfo, MonthCard, PayAddGold} ->
            next;
        {RoleInfo, RoleExtra,GerList, PosList, ListOfGerEquipList, BagEquip, BagItem, GuardGerList,
         RoleGuardInfo, TeamPkInfo, AlienInfo, ShopTreasure, RoleRewardInfo, MonthCard} ->
            PayAddGold = 0
    end,
	%% 设置mac地址和IP
	role_data:set_macAddr(MacAddr),
	role_data:set_ip(Ip),
	role_data:set_login_datetime(),
	%% 初始化玩家各功能数据
	role_data:set_roleID(RoleID),
	role_data:set_roleInfo(RoleInfo),
	%% 读取离线扣取的元宝
	
	OfflineDeductGold = db_sql:get_offlineDeductGold(RoleID),
	db_sql:set_offlineDeductGold(RoleID, 0),
	EncounterInfo = db_sql:get_role_encounterList(RoleID),
	
	hist_server:role_login(RoleID),
	{Gold, GoldBonus, GoldUsed} = cacl_deduct_gold(RoleInfo, OfflineDeductGold),
	LastLogoutTime = RoleInfo#role.lastLogoutTime,
    
	role_data:set_roleEncounterInfo(EncounterInfo, RoleInfo), 
	
	role_data:set_gatewayClientPid(GatewayClientPid),
	role_data:set_socket(Socket),
	role_data:set_roleExtra(RoleExtra, RoleInfo, AlienInfo),
	%% 先初始化装备，再初始化武将(因为要计算武将属性)
	%% 先初始化武将，再初始化副将，然后再计算副将对主将的影响(天命与加成)
	role_data:init_LieutenantInfo([]),
    role_data:set_roleGuardInfo(RoleGuardInfo),
    role_data:set_guardPosList(GuardGerList),
	role_data:init_all_item(ListOfGerEquipList, BagEquip, BagItem),
	role_data:set_gerBag(GerList),
    role_data:init_month_card(MonthCard, LastLogoutTime),
	
	{PosListT, LPosListT} = role_data:posList_filter(PosList, []),

	role_data:init_posList(PosListT),
	role_data:init_lieuList(LPosListT),
	role_data:init_shop_treasure(ShopTreasure),
	role_data:init_gag_list(RoleID),
	role_data:init_sign_emperor_info(RoleID),
	role_data:init_treaHouseInfo(RoleID),
	role_data:set_roleTeamPkInfo(TeamPkInfo),
    role_data:init_roleRewardInfo(RoleRewardInfo),
	IsSelectGer = PosListT =/= [] orelse GerList =/= [],
	{Rank, Title} = 
		case IsSelectGer of
			true->
				pvp_server:call_get_title_rank(RoleID);
			false->
				{9999,0}
		end,
	RoleInfo2 = RoleInfo#role{title=Title,goldBonus=GoldBonus,gold=Gold,goldUsed=GoldUsed,lastLogoutTime=0,deviceID=DeviceID},
	case IsSelectGer of
		true->
			role_lib:insert_rolePublic(RoleInfo);
		false-> 
			ignore
	end,
	role_data:set_roleInfo(RoleInfo2),
	role_task:init_task_data(RoleID,LastLogoutTime),
	role_data:set_pvp_rank(Rank),
    role_road:init_role_road(RoleID, RoleInfo2#role.vipLevel),
    role_data:set_coinBattle(db_sql:get_coinBattle(RoleID)),
    %% 自由額度處理儲值
    ChargeListAmount = db_sql:get_offlinePayAmountLog(RoleID),
    if ChargeListAmount =/= [] ->
           db_sql:clear_offlinePayAmountLog(RoleID);
       true ->
           ignore
    end,
    lists:foreach(fun([Amount, Receipt2, Md52, SrcType2]) -> role_lib:do_pay_amount(Amount, Receipt2, Md52, SrcType2) end, ChargeListAmount),
    
	%% 处理充值
	ChargeList = db_sql:get_offlinePayLog(RoleID),
	if ChargeList =/= [] ->
		   db_sql:clear_offlinePayLog(RoleID);
	   true ->
		   ignore
	end,
	lists:foreach(fun([PayItemID, Receipt, Md5, SrcType]) -> role_lib:do_pay(PayItemID, Receipt, Md5, SrcType) end, ChargeList),
    tencent_pay_fill(PayAddGold, RoleID, RoleInfo#role.srcType),
	{RoleInfo2,LastLogoutTime}.

tencent_pay_fill(PayAddGold, RoleID, SrcType) when PayAddGold > 0 ->
    FillList = gen_fill_list(PayAddGold, RoleID, SrcType),
    ?ERR("FillList:~10000p", [FillList]),
    lists:foreach(fun({PayGold, PayID,Receipt,Md5}) ->
                          pay_server:check_pay_receipt_duplicate_inlogin2(Receipt,Md5,RoleID,SrcType,PayGold),
                          db_sql:add_pay_receipt(Md5, RoleID, Receipt,SrcType,PayGold),
                          activity_server:pay(RoleID, PayGold),
                          role_lib:do_pay(PayID,Receipt,Md5,SrcType);
                     ({PayGold, Receipt, Md5}) ->
                          pay_server:check_pay_receipt_duplicate_inlogin2(Receipt,Md5,RoleID,SrcType,PayGold),
                          db_sql:add_pay_receipt(Md5, RoleID, Receipt,SrcType,PayGold),
                          activity_server:pay(RoleID, PayGold),
                          role_lib:do_pay_amount(PayGold,Receipt,Md5,SrcType)
                  end, FillList);
tencent_pay_fill(_PayAddGold, _RoleID, _SrcType) ->
    ok.

gen_fill_list(PayAddGold, RoleID, SrcType) ->
    gen_fill_list(PayAddGold, RoleID, SrcType, [], lists:reverse(lists:keysort(2, [{PayID, (data_pay:get(PayID))#data_pay.payGold}||PayID<-data_pay:get_list()]))).

gen_fill_list(PayAddGold, RoleID, SrcType, FillList, ConfigList) ->
    case calc_pay_gold(PayAddGold, ConfigList, RoleID, SrcType) of
        {PayGold, PayID, Receipt,Md5} ->
            NewFillList = [{PayGold, PayID, Receipt,Md5}|FillList];
        {PayGold, Receipt,Md5} ->
            NewFillList = [{PayGold, Receipt,Md5}|FillList]
    end,
    LeftGold  = PayAddGold - PayGold,
    case LeftGold > 0 of
        true ->
            gen_fill_list(LeftGold, RoleID, SrcType, NewFillList, ConfigList);
        false ->
            NewFillList
    end.

calc_pay_gold(PayAddGold, ConfigList, RoleID, SrcType) ->
    case calc_pay_gold(PayAddGold, ConfigList) of
        {pay, PayGold, PayID} ->
            Receipt = integer_to_list(SrcType) ++ integer_to_list(RoleID)++integer_to_list(PayGold)++integer_to_list(util:now())++integer_to_list(tk_id:gen_payID()),
            Md5 = util:md5(Receipt),
            {PayGold, PayID,Receipt,Md5};
        {amount, PayGold} ->
            Receipt = integer_to_list(SrcType) ++ integer_to_list(RoleID)++integer_to_list(PayGold)++integer_to_list(util:now())++integer_to_list(tk_id:gen_payID()),
            Md5 = util:md5(Receipt),
            {PayGold, Receipt, Md5}
    end.

calc_pay_gold(PayAddGold, []) ->
    {amount, PayAddGold};
calc_pay_gold(PayAddGold, [{PayID, PayGold}|ConfigList]) ->
    case PayAddGold >= PayGold of
        true ->
            {pay, PayGold, PayID};
        false ->
            calc_pay_gold(PayAddGold, ConfigList)
    end.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% @doc gen_server:init/1
handle_call({login_again, GatewayPid, Socket, MacAddr, Ip,DeviceID}, _From, RoleID) ->
    case erlang:get(?stop_role_server_flag) of
        ?undefined ->
            do_login_again(GatewayPid, Socket, MacAddr, Ip, DeviceID, RoleID),
            Reply = ok;
        _ ->
            Reply = ?stop_role_server_flag
    end,
    {reply, Reply, RoleID};
handle_call({func, F, Args}, _From, State) ->
	Result = ?CATCH(apply(F,Args)),
	{reply, Result, State};
handle_call(get_fighter_list, _From, State)->
	PosList = role_data:get_posList(),
	LieuAdd = role_data:get_lieu_add_attr(),
	Result = {PosList, LieuAdd},
	{reply, Result, State};
handle_call(get_role_info, _From, State) ->
    Reply = role_data:get_roleInfo(),
    {reply, Reply, State};
handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:handle_cast/2
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(Msg, State) ->
	?ERR("handle_cast function clause:request=~100p",[Msg]),
    {noreply, State}.


-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:handle_info/2
handle_info({client_msg, Module, Info}, State) ->
	?DEBUG("recv msg=~w",[{client_msg, Module, Info}]),
	FuncName = element(1, Info),
	?CATCH(Module:FuncName(Info)),
    {noreply, State};
handle_info({?route, Module, Info}, State) ->
	?DEBUG("recv msg=~w",[{?route, Module, Info}]),
	FuncName = element(1, Info),
	?CATCH(Module:FuncName(Info)),
    {noreply, State};
handle_info({func, F, Args}, State) ->
	?CATCH(apply(F,Args)),
	{noreply, State};
handle_info({timer_wheel_tick, LastTick}, State) ->
	timer_wheel:work(LastTick),
	{noreply, State};
handle_info({'EXIT',From,Reason},State) ->
    if From =/= self() andalso Reason == login_again ->
           ?ERR("login_again"),
           {noreply, State};
       true ->
           case role_data:get_gatewayClientPid() of
               From ->
                   {stop, {gateway_stop,From,Reason},State};
               _ ->
                   ?ERR("unkown From:~w,Reason:~w,State:~w,~w", [From,Reason,State,role_data:get_gatewayClientPid()]),
                   erlang:unlink(From),
                   {noreply, State}
           end
    end;
handle_info(Info, State) ->
	?CATCH(do_handle_info(Info, State)),
	{noreply, State}.

-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% @doc gen_server:terminate/2
%% 初始化失败时，直接退出
terminate(load_data_error, State) ->
	RoleID = State,
	role_state:logoff_without_flush(RoleID),
	ok;
terminate(Reason, State) ->
	RoleID = State,
	#role{deviceID=DeviceID, accid=Accid,srcType=SrcType} = role_data:get_roleInfo(),
    erlang:put(?stop_role_server_flag, true),
	role_state:logoff_with_flush(RoleID,Accid,DeviceID,SrcType),
	if Reason == forced_stop orelse Reason == normal orelse Reason == shutdown orelse element(1,Reason) == gateway_stop->
		   ignore;	   
	   true ->
		   ?ERR("~w terminate for \nReason=~300p\nState=~300p\n",[{role_server,RoleID}, Reason,  State])
	end,
	ok.

%% 严格的处理所有收到的消息
flush_msg(State) ->
	receive
		{'$gen_call',{From,_},Msg} ->
			handle_call(Msg, From, State),
			flush_msg(State);
		{'$gen_cast',Msg} ->
			handle_cast(Msg, State),
			flush_msg(State);
		Msg ->
			handle_info(Msg, State),
			flush_msg(State)
	after 0 ->
			ok
	end.


-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% @doc gen_server:code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% 秒循环
sec_loop(NowSec) ->
	timer_wheel:add_plan(NowSec+1, fun sec_loop/1),
	do_sec_loop(NowSec),
	if (NowSec rem 60) =:= 0 ->
		   do_minute_loop(NowSec);
	   true ->
		   ignore
	end.

do_sec_loop(_NowSec) ->
	ok.

do_minute_loop(_NowSec) ->
	%RoleID = role_data:get_roleID(),
	%% 更新ETS_ROLE_PUBLIC
	
	
	%%RoleRewardInfo = role_data:get_roleRewardInfo(),
	%%?ERR("测试奖励=~p",[RoleRewardInfo]),
	%%#role_reward_info{onlineSecs = OnlineSecs, getList = GetList} = RoleRewardInfo,
    
	
	%%清空在线奖励
	%%role_data:set_roleRewardInfo(RoleRewardInfo#role_reward_info{onlineSecs=0,getList = lists:keyreplace(?REWARD_TYPE_ONLINE, 1, GetList, {?REWARD_TYPE_ONLINE,[]}) }),
	%%?ERR("测试奖励=~p",[role_data:get_roleRewardInfo()]),
    %%?sendself(#sc_daily_reward_list{list=[role_daily:get_days_info(role_data:get_roleRewardInfo())]}),
	
	role_lib:update_rolePublic(role_data:get_roleInfo()),
	ok.

%%每个小时执行
hook_hour()->
	{Hour,_,_} = time(),
	NextZeroClockSec = util:datetime_to_seconds({erlang:date(),{Hour,0,1}}) + ?ONE_HOUR_SECONDS,
	timer_wheel:add_plan(NextZeroClockSec, fun hook_hour/0),
	case Hour of
		0->
			hook_zero_clock(),
            erlang:send(erlang:self(), {route, role_role, #cs_role_info{}});
		_->
			ignore
	end,
    role_team:erase_replay_record(),
	role_homestead:hook_hour(Hour).

%% 凌晨0点hook
hook_zero_clock() ->
    add_energy_zero_clock(),
    role_road:hook_zero_clock(),
    role_activity:hook_zero_clock(),
    role_battle:hook_zero_clock(),
	%% 每日领取奖励通知
	role_daily:hook_zero_clock(),
	%%任务每日刷新
	role_task:hook_zero_clock().

add_energy_zero_clock() ->
    #roleTimes{energy=Energy,lastEnergyTime=LastTick} = RoleTimes = role_data:get_roleTimes(),
    Energy2 = Energy + data_common:get(zero_clock_add_energy),
    #role{vipLevel=VipLevel} = role_data:get_roleInfo(),
    MaxEnergy= role_lib:get_max_energy(VipLevel),
    NowSec = util:now(),
    NewEnergy =
        case Energy2 > MaxEnergy of
            true ->
                MaxEnergy;
            false ->
                Energy2
        end,
    RoleTimes2 = RoleTimes#roleTimes{energy=NewEnergy},
    EInterval = role_lib:to_sec(data_common:get(energy_recover_interval)),
    NextTick = LastTick - NowSec + EInterval,
    ?notify_update(?ra_energy(NewEnergy, NextTick)),
    role_data:set_roleTimes(RoleTimes2).
		  
sort([]) ->
	[];
sort([H|L]) ->
	sort([E||E<-L,E =< H]) ++ [H] ++ sort([E||E<-L, E>H]).
		   
		   
do_handle_info({dispach_task,Msg},_)->
	?CATCH(role_task_trigger:handle(Msg));
%% 处理特殊消息
do_handle_info({pvp_attack, Attacker,LieuAddA, From, Ref}, _RoleID) ->
	Defender = role_data:get_fighter_list(),
	LieuAddD = role_data:get_lieu_add_attr(),
	role_fight:new(0,false,false,Attacker, Defender,LieuAddA, LieuAddD, From, Ref);
do_handle_info({pvp_rank, Rank}, _RoleID) ->
	role_data:set_pvp_rank(Rank);
do_handle_info({title, Title}, _RoleID) ->
	RoleInfo = role_data:get_roleInfo(),
	RoleInfo2 = RoleInfo#role{title=Title},
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_title(Title));
do_handle_info({do_pay,AppItemID,Receipt,Md5,SrcType},_RoleID) ->
	role_lib:do_pay(AppItemID,Receipt,Md5,SrcType);
do_handle_info({do_pay_amount,Amount,Receipt,Md5,SrcType},_RoleID) ->
    role_lib:do_pay_amount(Amount,Receipt,Md5,SrcType);
do_handle_info({do_ger_view, SrcRoleID, SrcServerName}, RoleID) ->
	RolePublic=role_lib:get_rolePublic(RoleID),
	FighterList=role_data:get_fighter_list(),
	Reply=role_ger:ger_view_info(RolePublic, FighterList),
    case SrcServerName of
        ?undefined ->
            ?unicast(SrcRoleID,Reply);
        _ ->
            ?CATCH(global:send(SrcServerName, {cross_ger_view_return, SrcRoleID, Reply}))
    end;
do_handle_info({do_ger_view_dtl, SrcRoleID, SrcServerName}, _RoleID) ->
	Role=role_data:get_roleInfo(),
	FighterList=role_data:get_posList(),
	EquipList=role_data:get_equipts_on_ger(),
	{AtkAdd, HpAdd} = role_data:get_lieu_add_attr(),
	LieuInfoList = role_data:get_lieuInfoList(),
	Reply=role_ger:ger_view_info_dtl(Role, FighterList,EquipList, AtkAdd, HpAdd, LieuInfoList),
    case SrcServerName of
        ?undefined ->
	       ?unicast(SrcRoleID,Reply);
        _ ->
            ?CATCH(global:send(SrcServerName, {cross_ger_view_dtl_return, SrcRoleID, Reply}))
    end;
do_handle_info({draw_mail_reward,Reward,MailTemplateID}, _RoleID) ->
	Role = role_data:get_roleInfo(),
	role_reward:handle_sys_reward(Role, Reward, ?MONEY_ADD_TYPE_MAIL_REWARD, MailTemplateID, ""),
	role_invite:do_add_weibo_share_mark(?SHARE_TYPE_MAIL);
do_handle_info({draw_activity_reward,ActivityID,Reward},_RoleID) ->
	Role = role_data:get_roleInfo(),
	role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_ACTIVITY_DRAW, ActivityID, "");
do_handle_info({draw_activity_reward,ActivityID,DrawID,Reward},_RoleID) ->
    Role = role_data:get_roleInfo(),
    role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_ACTIVITY_DRAW, ActivityID, erlang:integer_to_list(DrawID));
do_handle_info({activity_exchange, ActivityID, DrawID, Condition, Reward}, RoleID) ->
	case catch role_exchange:handle_exchange(RoleID, ActivityID, DrawID, Condition, Reward) of
        {'EXIT',gold_not_enough} ->
            erlang:send(activity_server, {erase_exchange_flag, RoleID});
        _ ->
            role_invite:do_add_weibo_share_mark(?SHARE_TYPE_EXCHANGE)
    end;
do_handle_info({draw_activity_rank_reward, Reward, ActivityID, Rank}, _RoleID) ->
	Role = role_data:get_roleInfo(),
	role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_TREAHOUSE_RANKREWARD, ActivityID, integer_to_list(Rank));
do_handle_info({role_energy_activity, Eng},_RoleID) ->
	#roleTimes{energy=Energy,lastEnergyTime=LastTick} = RoleTimes = role_data:get_roleTimes(),
	Energy2 = Energy + Eng,
	%?ERR("Energy:~w,~w",[Energy, Energy2]),
	NowSec = util:now(),
	RoleTimes2 = RoleTimes#roleTimes{energy=Energy2},
	EInterval = role_lib:to_sec(data_common:get(energy_recover_interval)),
	NextTick = LastTick - NowSec + EInterval,
	?notify_update(?ra_energy(Energy2, NextTick)),
	role_data:set_roleTimes(RoleTimes2);
do_handle_info({bet_emperor, Money}, _RoleID)->
	Role = role_data:get_roleInfo(),
	role_lib:deduct_coin_f(Role, Money, ?MONEY_DEC_TYPE_BET_EMPEROR, 0, "");
do_handle_info({hula_harm_reward,AddCoin,AddRepu},_RoleID) ->
	Role = role_data:get_roleInfo(),
	role_reward:handle_sell_reward_f(Role, #sell_reward{coin=AddCoin,reputation=AddRepu}, ?MONEY_ADD_TYPE_HULA_CHALLENGE, 0, "");
do_handle_info({nanm_harm_reward,AddCoin,AddRepu},_RoleID) ->
	Role = role_data:get_roleInfo(),
	role_reward:handle_sell_reward_f(Role, #sell_reward{coin=AddCoin,reputation=AddRepu}, ?MONEY_ADD_TYPE_NANM_CHALLENGE, 0, "");
do_handle_info({add_weibo_share_mark, Type}, _RoleID) ->
	role_invite:do_add_weibo_share_mark(Type);
do_handle_info({cancel_tencent_pay, Billno, ReturnGold}, RoleID) ->
    #role{srcType=SrcType} = role_data:get_roleInfo(),
    tencent_pay:cancel_pay(RoleID, SrcType, Billno, ReturnGold);

do_handle_info({race_guess_succ, GuessCoin}, _) ->
    #role{coin=Coin} = RoleInfo = role_data:get_roleInfo(),
    case GuessCoin > 0 andalso Coin > 0 of
        true ->
            NeedCoin =
                case Coin >= GuessCoin of
                    true ->
                        GuessCoin;
                    false ->
                        Coin
                end,
            role_lib:deduct_money_f(RoleInfo, coin, NeedCoin, ?MONEY_DEC_TYPE_RACE_GUESS, 0, "");
        false ->
            next
    end;

do_handle_info({get_rule_hist, Record}, RoleID) ->
    ?sendself(Record#sc_rule_info{list=hist_server:get_rule_hist(RoleID)});
do_handle_info({rule_fight, MasterServer}, _) ->
    role_rule:rule_fight(MasterServer);

do_handle_info(refresh_shop_activity, _) ->
    role_shop:refresh_shop_activity();
do_handle_info(refresh_shop_treasure, _) ->
    role_shop:refresh_shop_treasure();

%% 联盟相关处理
do_handle_info({create_family_succ, FamilyInfo}, RoleID) ->
    RoleInfo = role_data:get_roleInfo(),
    FamilyID = FamilyInfo#p_family_info.family_id,
    db_sql:update_role_family_id(RoleID, FamilyID),
    role_data:set_roleInfo(RoleInfo#role{familyID=FamilyID}),
    family_misc:unlock_family_protect(RoleID),
%%     ?ERR("FamilyInfo:~w", [FamilyInfo]),
    ?sendself(#sc_family_create{result=0, family_info=FamilyInfo, timestamp=0});
do_handle_info({create_family_fail,  CostType, CostNum}, RoleID) ->
%%     ?ERR("CostType:~w, CostNum:~w", [CostType, CostNum]),
    RoleInfo = role_data:get_roleInfo(),
    case CostType of
        coin ->
            role_lib:add_coin_f(RoleInfo, CostNum, ?MONEY_ADD_TYPE_CREATE_FAMILY_FAIL, 0, "");
        gold ->
            role_lib:add_gold_f(RoleInfo, CostNum, ?MONEY_ADD_TYPE_CREATE_FAMILY_FAIL, 0, "")
    end,
    family_misc:unlock_family_protect(RoleID),
    ?sendself(#sc_family_create{result=8, family_info=family_misc:gen_p_family_info(), timestamp=0});

do_handle_info({update_family_id, FamilyID}, RoleID) ->
    RoleInfo = role_data:get_roleInfo(),
    case FamilyID =:= 0 of
        false ->
            db_sql:update_role_family_id(RoleID, FamilyID),
            role_data:set_roleInfo(RoleInfo#role{familyID=FamilyID});
        true ->
            Now = util:now(),
            db_sql:update_role_family_id(RoleID, FamilyID, Now),
            role_data:set_roleInfo(RoleInfo#role{familyID=FamilyID, lastJoinFamily=Now})
    end,
    family_misc:unlock_family_protect(RoleID);
do_handle_info(get_family_recent_talk, RoleID) ->
    #role{familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            family_misc:router_to_family_process(FamilyID, {get_family_recent_talk, RoleID});
        false ->
            ?sendself(#sc_talk_recent_list{list=[],channel=?CHAT_CHANNEL_FAMILY})
    end;

do_handle_info(reset_vip_days, _) ->
    role_daily:reset_vip_days_reward();
do_handle_info(hook_zero_clock, _) ->
    hook_zero_clock();

%% 修改玩家的vip信息
do_handle_info({change_role_vip_level, VipLevel,ensure_action}, _RoleID) ->
	case is_integer(VipLevel) andalso VipLevel < 13 of
		true ->
			Role = role_data:get_roleInfo(),
			Role2 = Role#role{vipLevel=VipLevel},
			role_data:set_roleInfo(Role2),
			RoleTimes=role_data:get_roleTimes(),
			VipChallengeGodFreeTimes = role_lib:get_max_challengeGodFreeTimes(VipLevel),
			VipChallengeGodBuyTimes = RoleTimes#roleTimes.challengeGodBuyTimes,%role_lib:get_max_challengeGodBuyTimes(VipLevel),
			NewChallengeGodEnergy = VipChallengeGodFreeTimes - (role_lib:get_max_challengeGodFreeTimes(Role#role.vipLevel) - RoleTimes#roleTimes.challengeGodEnergy),
			RoleTimes2 = RoleTimes#roleTimes{challengeGodEnergy=NewChallengeGodEnergy,
											 challengeGodBuyTimes=VipChallengeGodBuyTimes
											 },
			role_data:set_roleTimes(RoleTimes2),
			?notify_update(?ra_vipLevel(VipLevel, NewChallengeGodEnergy,VipChallengeGodBuyTimes));
		_ ->
			ignore
	end.

do_login_again(GatewayPid, Socket, MacAddr, Ip, DeviceID, RoleID) ->
    role_data:set_gatewayClientPid(GatewayPid),
    erlang:link(GatewayPid),
    role_data:set_socket(Socket),
    role_data:set_macAddr(MacAddr),
    role_data:set_ip(Ip),
    #role{srcType=SrcType} = Role = role_data:get_roleInfo(),
    NewRole = Role#role{deviceID=DeviceID},
    role_data:set_roleInfo(NewRole),
    role_lib:join_online_table(RoleID, GatewayPid, Socket, SrcType).

notice_family_online(IsOnline) ->
    #role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            family_misc:router_to_family_process(FamilyID, {update_role_online, RoleID, IsOnline});
        false ->
            ignore
    end.

write_term(Term) ->
	Bin = io_lib:format("~w",[Term]),
	file:write_file("d:/log",Bin,[append]).