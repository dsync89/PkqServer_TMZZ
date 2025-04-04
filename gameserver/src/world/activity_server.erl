%% @author admin
%% @doc 活动
%% Created 2013-6-8

%% buffer 说明：每次玩家读取活动数据时，将数据加载到内存，每天0晨hook时，将不在线玩家的缓存数据刷出内存。玩家数据更新，更新内存的同时，更新数据库数据，保证数据完整。

-module(activity_server).
-behaviour(gen_server).
-compile(export_all).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").
-include("def_reward.hrl").
-export([start_link/0,start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([i/0, update/0]).

-export([pay/2,role_energy_activity/2, get_activity_list/0]).

-define(TICK_INTERVAL, 3600). % 检查进程buff大小的时间间隔，单位：秒
-define(TICK_STATE_INTERVAL, 600). % 检查活动开启状态的时间间隔，单位：秒
-define(TICK_CHECK_ENERTY_TIME, 10). % 检查回复体力活动的时间间隔，单位：秒
-define(get_energy_roleList, get_energy_roleList). % 领取体力的玩家列表
-define(energy_activity_info, energy_activity_info). % 领取体力时间状态
-define(FOREVER, -1).% 活动永久有效
-define(box_price_info,box_price_info). % 点将价格信息
-define(exchange_flag, exchange_flag).

-define(REFRESH_CONFIG_PLAN_PLAG,refresh_config_plan_plag).
%% ===================Dict Key Begin =========================
%% ===================Dict Key End   =========================

role_energy_activity(RoleID, ClickTimes)->
	erlang:send(?MODULE, {role_get_energy, RoleID, ClickTimes}).

%% @doc 获取活动列表（读取ets表获取）
get_activity_list() ->
    case ets:lookup(?ETS_ETC, activity_list) of
        [{activity_list, ActivityList}] ->
            ActivityList;
        [] ->
            {[], []}
    end.

finish_exchange_condition(RoleID, ActivityID, DrawID)->
	erlang:send(?MODULE, {finish_exchange_condition, RoleID, ActivityID, DrawID}).

refresh_daily_activity(RoleID)->
	erlang:send(?MODULE, {refresh_daily_activity, RoleID}).
  
pay(RoleID, Gold) ->
	erlang:send(?MODULE,{pay, RoleID, Gold}).
role_consume(Type, RoleID, ConsumeValue)->
	erlang:send(?MODULE, {role_consume, Type, RoleID, ConsumeValue}).

i() ->
	gen_server:call(?MODULE, i).

update() ->
    gen_server:cast(?MODULE, update).

start() ->
    {ok,_}=supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:init/1	
init([]) ->
	random:seed(util:gen_random_seed()),
	process_flag(trap_exit,true),
	tick(),
	State = init_state(),
	tick_state(),
	timer_wheel:init(),
	put(?REFRESH_CONFIG_PLAN_PLAG,true),
	NextZeroClockSec = util:datetime_to_seconds({erlang:date(),{0,0,1}}) + ?ONE_DAY_SECONDS,
	timer_wheel:add_plan(NextZeroClockSec, fun hook_zero_clock/0),
	tick_energy_time(),
    add_activity_to_ets(State),
    {ok, State}.


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
handle_call(i, _From, State) ->
	{reply, State, State};
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
handle_cast(update, State)->
    keep_ets_exists(),
    add_activity_to_ets(State),
    {noreply, State};
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
handle_info(tick, State) ->
	{memory,Memory }= erlang:process_info(self(),memory),
	MemoryByM = Memory div (1024*1024),
	MaxBuffSizeByM = data_setting:get(friend_process_max_buff_size),
	if MemoryByM >= MaxBuffSizeByM ->
		   clear_buff();
	   true ->
		   ignore
	end,
	erlang:garbage_collect(),
	tick(),
	{noreply, State, hibernate};
handle_info(tick_state, _State) ->
	NewState = init_state(),
    add_activity_to_ets(NewState),
	tick_state(),
	refresh_config(),
	{noreply,NewState};
handle_info(tick_energy_time, State)->

	tick_energy_time(),
	{noreply, State};
handle_info({timer_wheel_tick, LastTick}, State) ->
	timer_wheel:work(LastTick),
	{noreply, State};
handle_info(Info, State) ->
	?CATCH(do_handle_info(Info, State)),
    {noreply, State}.



-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% @doc gen_server:terminate/2
terminate(Reason, State) ->
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
    ok.



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


do_handle_info({get_info,RoleID,ID}, _State) ->
	do_info(RoleID,ID);
do_handle_info({activity_draw,RoleID,ActivityID,DrawID}, _State) ->
	catch do_draw(RoleID, ActivityID, DrawID);

do_handle_info({pay, RoleID, Gold}, State) ->
	do_pay(RoleID, Gold,  State);
do_handle_info({finish_exchange_condition, RoleID, ActivityID, DrawID}, _State) ->
	do_finish_exchange_condition(RoleID, ActivityID, DrawID);
do_handle_info({role_get_energy, RoleID, ClickTimes}, _State) ->
	role_get_energy(RoleID, ClickTimes);
do_handle_info({refresh_daily_activity, RoleID}, _State) ->
	daily_fresh(RoleID);
do_handle_info({client_msg, RoleID, #cs_activity_energy{}}, _State) ->
	activity_energy_info(RoleID);
do_handle_info({clear_energy_state}, _State)->
	hook_zero_clock();
do_handle_info({role_consume, Type, RoleID, ConsumeValue}, State)->
	do_role_consume(Type, RoleID, ConsumeValue, State);
do_handle_info({erase_exchange_flag, RoleID}, _State) ->
    erase_exchange_flag(RoleID);
do_handle_info(Info, State) ->
	throw({cannot_handle,Info, State}).

activity_energy_info(RoleID)->
    List =
        lists:foldr(fun({Key, Info}, Acc) ->
                            [Info#p_energy_activity{isGet = is_role_get_energy2(RoleID, Key)}|Acc]
                    end, [], erlang:get(?energy_activity_info)),
    ?unicast(RoleID, #sc_activity_energy{activityList=List}).


role_get_energy(RoleID, ClickTimes)->	
	case get_now_energy() of
		false ->
			?unicast(RoleID, #sc_role_get_energy{result=2});
		Key ->
            {_Beg, _End, EngMax, EngMin, OneClick, _ClickSeconds, _DelaySeconds} = data_get_energy:get(Key),
			case is_role_get_energy(RoleID, Key) of
				true ->
					?unicast(RoleID, #sc_role_get_energy{result=3});
				false ->
					set_role_get_energy(RoleID, Key),
					role_lib:send_server(RoleID, {role_energy_activity, cacl_eng(EngMax, EngMin, OneClick, ClickTimes)}),
					?unicast(RoleID, #sc_role_get_energy{result=1})
			end
	end.

get_now_energy() ->
    util:fun_find(fun(Key) ->
                          {Beg, End, _EngMax, _EngMin, _OneClick, _ClickSeconds, _DelaySeconds} = data_get_energy:get(Key),
                          {_,NowTime} = erlang:localtime(),
                          NowTime > Beg andalso NowTime < End
                  end, data_get_energy:get_list()).

cacl_eng(EngMax, EngMin, OneClick, ClickTimes) ->
    ClickEng = erlang:trunc(ClickTimes * OneClick / 10),
    if
        ClickEng > EngMax ->
            EngMax;
        ClickEng > EngMin ->
            ClickEng;
        true ->
            EngMin
    end.

hook_zero_clock()->
	NextZeroClockSec = util:datetime_to_seconds({erlang:date(),{0,0,1}}) + ?ONE_DAY_SECONDS,
	timer_wheel:add_plan(NextZeroClockSec, fun hook_zero_clock/0),
	%% 重新加载活动配置和点将价钱配置
	user_default:lc(data_box_price),
    [erlang:put({?get_energy_roleList,Key},[])||Key<-data_get_energy:get_list()],
	clear_buff().

is_role_get_energy2(RoleID, Key) ->
    case is_role_get_energy(RoleID, Key) of
        true ->
            2;
        false ->
            1
    end.

is_role_get_energy(RoleID, Key)->
	lists:member(RoleID, get_role_get_energy({?get_energy_roleList, Key})).
set_role_get_energy(RoleID, Key)->
	erlang:put({?get_energy_roleList, Key}, [RoleID|get_role_get_energy({?get_energy_roleList, Key})]).

get_role_get_energy(D)->
	case erlang:get(D) of
		?undefined ->
			[];
		X ->
			X
	end.

is_exchange_flag(RoleID) ->
    case erlang:get({?exchange_flag, RoleID}) of
        ?undefined ->
            false;
        _ ->
            true
    end.

set_exchange_flag(RoleID) ->
    erlang:put({?exchange_flag, RoleID}, RoleID).

erase_exchange_flag(RoleID) ->
    erlang:erase({?exchange_flag, RoleID}).

do_finish_exchange_condition(RoleID, ActivityID, DrawID) ->

	#data_activity{drawList=ConfigDrawList,type=Type} =data_activity:get(ActivityID),
	#data_activity_draw{maxDrawNum=MaxDrawNum}=lists:keyfind(DrawID, #data_activity_draw.drawID, ConfigDrawList),
		
  	Info = get_info(RoleID),
	{#act{list=DrawList} = Act, ActList2} = take_act(Info, ActivityID, Type),
	
	case lists:keytake(DrawID, #draw.drawID, DrawList) of
		false ->
			DrawList2 = DrawList,
			AT2=1,
			if MaxDrawNum =:= ?FOREVER ->
				   CanDrawTimes2 = MaxDrawNum;
			   true->
				   CanDrawTimes2=MaxDrawNum-1
			end,
			Draw2=#draw{ableDrawTimes=CanDrawTimes2,drawID=DrawID,alreadyDrawTimes=AT2};
		{value, #draw{ableDrawTimes=CanDrawTimes,alreadyDrawTimes=AT}=Draw, DrawList2}->
			AT2=AT+1,
			if CanDrawTimes =:= ?FOREVER ->
				   CanDrawTimes2 = CanDrawTimes;
			   true->
				   CanDrawTimes2=CanDrawTimes-1
			end,
			Draw2=Draw#draw{ableDrawTimes=CanDrawTimes2,alreadyDrawTimes=AT2},
			next
	end,
   	DrawList3 = [Draw2|DrawList2],
   	Act2 = Act#act{list=DrawList3},
   	Info2 = Info#dc{actList=[Act2|ActList2]},
   	set_info(RoleID, Info2),
    erase_exchange_flag(RoleID),
	?unicast(RoleID, #sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=AT2,canDrawTimes=CanDrawTimes2,drawID=DrawID,result=1}).
		
		
%%　兑换活动领取
do_draw_exchange(RoleID, ActivityID, DrawID, _Info, _Act, _ActList2, DrawList, Condition, MaxDrawNum, Reward) ->
	case lists:keytake(DrawID, #draw.drawID, DrawList) of
		false ->
			_DrawList2 = DrawList,
			AT=0,
			CanDrawTimes=MaxDrawNum,
			_Draw=#draw{ableDrawTimes=CanDrawTimes,drawID=DrawID,alreadyDrawTimes=AT};
		{value, #draw{ableDrawTimes=CanDrawTimes,alreadyDrawTimes=AT}=_Draw, _DrawList2}->
			next
	end,
    if MaxDrawNum > AT orelse MaxDrawNum =:= ?FOREVER ->
           case is_exchange_flag(RoleID) of
               false ->
                   role_lib:send_server(RoleID, {activity_exchange, ActivityID, DrawID, Condition, Reward}),
                   set_exchange_flag(RoleID);
               true ->
                   ?unicast(RoleID, #sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=AT,canDrawTimes=CanDrawTimes,drawID=DrawID,result=2})
           end;
       true ->
           ?unicast(RoleID, #sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=AT,canDrawTimes=CanDrawTimes,drawID=DrawID,result=2})
    end.


%%　其他活动领取
do_draw(RoleID, ActivityID, DrawID) ->
	#rolePublic{srcType = SrcType} = role_lib:get_rolePublic(RoleID),
	case tencent_pay:check_pay_arg(RoleID, SrcType, 0) of
		true -> 
			next;
		false -> 
			?unicast(RoleID, #sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=0,canDrawTimes=0,drawID=DrawID,result=255}),
			erlang:throw(break)
	end,
	
	case data_activity:get(ActivityID) of
		#data_activity{drawList=ConfigDrawList,type=Type} ->
			case lists:keyfind(DrawID, #data_activity_draw.drawID, ConfigDrawList) of
				false ->
					?unicast(RoleID, #sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=0,canDrawTimes=0,drawID=DrawID,result=2});
				#data_activity_draw{reward=Reward,condition=Condition,maxDrawNum=MaxDrawNum} ->
					Info = get_info(RoleID), 
					{#act{list=DrawList} = Act, ActList2} = take_act(Info, ActivityID, Type),
					case Type of
						exchange ->
							do_draw_exchange(RoleID, ActivityID, DrawID, Info, Act, ActList2, DrawList, Condition, MaxDrawNum,Reward);
						exchange2 ->
							do_draw_exchange(RoleID, ActivityID, DrawID, Info, Act, ActList2, DrawList, Condition, MaxDrawNum,Reward);
                        exchange3 ->
                            do_draw_exchange(RoleID, ActivityID, DrawID, Info, Act, ActList2, DrawList, Condition, MaxDrawNum,Reward);
						_ ->
							case lists:keytake(DrawID, #draw.drawID, DrawList) of
								false ->
									?unicast(RoleID, #sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=0,canDrawTimes=0,drawID=DrawID,result=2});
								{value, #draw{ableDrawTimes=ADT,alreadyDrawTimes=AT}=Draw, DrawList2}->
									if ADT >0 ->
										   ADT2 = ADT-1,
										   AT2 = AT + 1,
										   Draw2 = Draw#draw{ableDrawTimes=ADT2,alreadyDrawTimes=AT2},
										   DrawList3 = [Draw2|DrawList2],
										   Act2 = Act#act{list=DrawList3},
										   Info2 = Info#dc{actList=[Act2|ActList2]},
										   set_info(RoleID, Info2),
										   role_lib:send_server(RoleID, {draw_activity_reward,ActivityID,DrawID,Reward}),
										   ?unicast(RoleID, #sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=AT2,canDrawTimes=ADT2,drawID=DrawID,result=1});
									   true ->
										   ?unicast(RoleID, #sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=0,canDrawTimes=0,drawID=DrawID,result=2})
									end
							
							end
					end
			end;
		_ ->
			?unicast(RoleID, #sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=0,canDrawTimes=0,drawID=DrawID,result=2})
	end.

do_role_consume(Type, RoleID, ConsumeValue,{_, ConfigList})->
	Info = get_info(RoleID),
	Info2 = 
		lists:foldl(fun(DataActivity, InfoAcc) ->
							case DataActivity#data_activity.type of
								consume ->
									do_role_consume2(Type, RoleID, ConsumeValue, DataActivity, InfoAcc);
								_ ->
									InfoAcc
							end
					end, Info, ConfigList),
	set_info(RoleID, Info2).
do_role_consume2(Type, RoleID, ConsumeValue, DataActivity, Info)->
	#data_activity{activityID=ID, drawList=ConfigDrawList} = DataActivity,
	{#act{list=DrawList,value=Value} = Act, ActList2} = take_act(Info, ID, consume),
	{Type, OldValue} = get_consume_value(Type, Value),
	NewValue = ConsumeValue + OldValue,
	GetList = lists:filter(fun(#data_activity_draw{condition=[{CType, Condition}]})->
								   CType2 = get_CType(CType),
								   CType2 == Type andalso Condition > OldValue andalso Condition =< NewValue;
							  (_)-> false
						    end, ConfigDrawList),
	%?ERR("list:,~w,~w,~w,~w\n~w",[GetList,Type,ConfigDrawList, DataActivity,DrawList]),
	DrawList2 = lists:foldl(fun(DataActivityDraw, Acc)->
									#data_activity_draw{drawID=DrawID} = DataActivityDraw,
									case lists:keyfind(DrawID, #draw.drawID, Acc) of
										false ->
											?unicast(RoleID, #sc_activity_update{activityID=ID, drawID=DrawID, canDrawTimes=1}),
											[#draw{drawID=DrawID, ableDrawTimes=1, alreadyDrawTimes=0}|Acc];
										#draw{}->
											Acc
									end
							end,DrawList, GetList),
	{Type, NowValue} = calc_new_consume_value(Type, Value, ConsumeValue),
	{_,GoldValue} = get_consume_value(gold, NowValue),
	?unicast(RoleID, #sc_activity_record_update{activityID=ID,typeValue=GoldValue}),
	Act2 = Act#act{list=DrawList2,value=NowValue},
	Info#dc{actList=[Act2|ActList2]}.
calc_new_consume_value(Type, {Gold, Coin, Repu}, ConsumeValue)->
	case Type of
		gold ->
			{Type, {Gold+ConsumeValue, Coin, Repu}};
		coin ->
			{Type, {Gold, Coin+ConsumeValue, Repu}};
		repu ->
			{Type, {Gold, Coin, Repu+ConsumeValue}};
		_ ->
			{Type, {Gold, Coin, Repu}}
	end.
get_consume_value(Type, {Gold, Coin, Repu})->
	case Type of
		gold ->
			{Type, Gold};
		coin ->
			{Type, Coin};
		repu ->
			{Type, Repu};
		_ ->
			?ERR("Error Type in activity : consume,Type = ~w",[Type]),
			{Type, 0}
	end.
get_CType(CType)->
	case CType of
		2 ->
			gold;
		_ ->
			repu
	end.
	
do_pay(RoleID, Gold, {_, ConfigList}) ->
	Info = get_info(RoleID),
	Info2 = 
		lists:foldl(fun(DataActivity, InfoAcc) -> 
							do_pay2(DataActivity#data_activity.type, DataActivity, InfoAcc, Gold, RoleID)
					end, Info, ConfigList),
	set_info(RoleID, Info2).

%% 定额充值奖励
do_pay2(pay_special_num=Type, DataActivity, Info, Gold, RoleID) ->
	#data_activity{activityID=ID,drawList=ConfigDrawList} = DataActivity,
	{#act{list=DrawList} = Act, ActList2} = take_act(Info, ID, Type),
	%%　判断是否配置了这个元宝项
	case lists:keyfind(Gold, #data_activity_draw.condition, ConfigDrawList) of
		false ->
			Info;
		#data_activity_draw{drawID=DrawID,maxDrawNum=Max} ->
			%% 判断是否有历史数据
			case lists:keytake(DrawID, #draw.drawID, DrawList) of
				false ->
					DrawList3 = [#draw{ableDrawTimes=1,alreadyDrawTimes=0,drawID=DrawID}|DrawList],
					?unicast(RoleID, #sc_activity_update{activityID=ID,drawID=DrawID,canDrawTimes=1});
				{value, #draw{ableDrawTimes=ADT,alreadyDrawTimes=AT}=Draw, DrawList2} ->
					%% 判断是否超过了最大可领取次数
                    Bool1 = (Max =:= -1),
                    Bool2 = (AT + ADT < Max),
                    Bool3 = Bool1 orelse Bool2,
                    if
                        Bool3 ->
                            ADT2 = ADT+1,
                            NewDraw = Draw#draw{ableDrawTimes=ADT2},
                            DrawList3 = [NewDraw|DrawList2],
                            ?unicast(RoleID, #sc_activity_update{activityID=ID,drawID=DrawID,canDrawTimes=ADT2});
                        true ->
                            DrawList3 = DrawList
                    end
			end,
			Act2 = Act#act{list=DrawList3},
			Info2 = Info#dc{actList=[Act2|ActList2]},
			Info2
	end;
%% 累计充值
do_pay2(pay_acc_num=Type, DataActivity, Info, Gold, RoleID) ->
	#data_activity{activityID=ID,drawList=ConfigDrawList} = DataActivity,
	{#act{list=DrawList,value=OldValue} = Act, ActList2} = take_act(Info, ID, Type),
	NewValue = OldValue+Gold,
	%%　判断是否配置了这个元宝项
	GetList = lists:filter(fun(#data_activity_draw{condition=Condition}) ->
								   Condition > OldValue andalso Condition =< NewValue end, ConfigDrawList),
	DrawList2 = 
		lists:foldl(fun(DataActivityDraw, Acc) ->
							#data_activity_draw{drawID=DrawID} = DataActivityDraw,
							case lists:keyfind(DrawID,#draw.drawID, Acc) of
								false ->
									?unicast(RoleID, #sc_activity_update{activityID=ID,drawID=DrawID,canDrawTimes=1}),
									[#draw{drawID=DrawID,ableDrawTimes=1,alreadyDrawTimes=0}|Acc];
								#draw{} ->
									Acc
							end
					end, DrawList, GetList),
	?unicast(RoleID, #sc_activity_record_update{activityID=ID,typeValue=NewValue}),
	Act2 = Act#act{list=DrawList2,value=NewValue},
	Info2 = Info#dc{actList=[Act2|ActList2]},
	Info2;
%% 累计充值天数
do_pay2(pay_day_num=Type, DataActivity, Info, _Gold, RoleID) ->
	#data_activity{activityID=ID,drawList=ConfigDrawList} = DataActivity,
	{#act{list=DrawList,value={OldValue,LastPayDate}} = Act, ActList2} = take_act(Info, ID, Type),
	NowDate = erlang:date(),
	%% 判断上次充值与这次充值的日期是否相同
	if LastPayDate =:= NowDate ->
		   Info;
	   true ->
		   NewValue = OldValue+1,
		   AddDrawList=
			   lists:foldl(fun(#data_activity_draw{drawID=DrawID, condition=Condition},Acc)->
								   if Condition =< NewValue ->
										  case lists:keyfind(DrawID, #draw.drawID, DrawList) of
											  false ->
												  ?unicast(RoleID, #sc_activity_update{activityID=ID, drawID=DrawID, canDrawTimes=1}),
												  [#draw{ableDrawTimes=1, alreadyDrawTimes=0, drawID=DrawID}|Acc];
											  _ ->
												  Acc
										  end;
									  true ->
										  Acc
								   end
						   end, [], ConfigDrawList),
		   DrawList2=AddDrawList ++ DrawList,
		   ?unicast(RoleID, #sc_activity_record_update{activityID=ID,typeValue=NewValue}),
		   Act2 = Act#act{list=DrawList2,value={NewValue,NowDate}},
		   Info2 = Info#dc{actList=[Act2|ActList2]},
		   Info2
	end;
do_pay2(_, _, Info, _, _) ->
	Info.
		   

do_info(RoleID, ID) ->
	{ok, Record} = do_info2(RoleID, ID),
	?unicast(RoleID, Record).

do_info2(RoleID, ID) ->
    Info = get_info(RoleID),
    #data_activity{type=Type, isForever=IsForever, isDailyRefresh=IsDailyRefresh,startTime=StartTime} = Config = data_activity:get(ID),
    %% value结构不同 充值天数,{0,0};其他:{0}
    if Type =:= pay_day_num ->
            #act{value={TypeValue,_TypeDate}} = Act = act(Info, ID, Type);
       Type =:= consume ->
           #act{value={TypeValue, _OtherType1, _OtherType2}} = Act = act(Info, ID, Type);
        true ->
            #act{value=TypeValue} = Act = act(Info, ID, Type)
    end,
    if IsForever =:= true->
           IsForever2 = 1;
       true ->
           IsForever2 = 2
    end,
    if IsDailyRefresh =:= true->
           IsDailyRefresh2 = 1;
       true ->
           IsDailyRefresh2 = 2
    end,
    {StartDay,StartSec} = StartTime,
    if erlang:is_tuple(StartDay) ->
           Record = #sc_activity_info{startTime=util:datetime_to_seconds(StartTime),
                                      stopTime=util:datetime_to_seconds(Config#data_activity.stopTime),
                                      type=type(Type),
                                      drawList=proto_draw_list(Act,Config),
                                      activityID=ID,
                                      description=Config#data_activity.description,
                                      isForever = IsForever2,
                                      isDailyRefresh = IsDailyRefresh2,
                                      typeValue=TypeValue};
       true ->
           {ServerOpenDate,_} = data_setting:get(serverOpenTime),
           Start = util:datetime_to_seconds({ServerOpenDate,StartSec}) + (StartDay - 1) * ?ONE_DAY_SECONDS,
           {StopDay, StopSec} = Config#data_activity.stopTime,
           Stop = util:datetime_to_seconds({ServerOpenDate,StopSec}) + (StopDay -1) * ?ONE_DAY_SECONDS,
           Record = #sc_activity_info{startTime=Start,
                                      stopTime=Stop,
                                      type=type(Type),
                                      drawList=proto_draw_list(Act,Config),
                                      activityID=ID,
                                      description=Config#data_activity.description,
                                      isForever = IsForever2,
                                      isDailyRefresh = IsDailyRefresh2,
                                      typeValue=TypeValue}
    end,
    {ok, Record}.

act(#dc{actList=List}, ID, Type) ->
	case lists:keyfind(ID, #act.actID, List) of
		false ->
			#act{actID=ID,list=[],value=default_value(Type)};
		#act{}=Act ->
			Act
	end.

take_act(#dc{actList=List}, ID, Type) ->
	case lists:keytake(ID, #act.actID, List) of
		false ->
			Act = #act{actID=ID,list=[],value=default_value(Type)},
			{Act, List};
		{value, #act{}=Act, List2} ->
			{Act, List2}
	end.

proto_draw_list(Act, DataActivity) ->
	#data_activity{drawList=DrawList} = DataActivity,
	#act{list=List} = Act,
	[begin
		 case lists:keyfind(ID, #draw.drawID, List) of
			 false ->
				 Already = 0,
				 CanDrawTimes = 0;
			 #draw{ableDrawTimes=CanDrawTimes,alreadyDrawTimes=Already} ->
				 next
		 end,
		 if is_list(Condition) ->
		 NeedMaterial =role_reward:transform2p_reward_view(Condition,[]);
			true ->
				NeedMaterial=[]
		 end,
		 #p_activity_draw{alreadyDrawTimes=Already,canDrawTimes=CanDrawTimes,description=Description,drawID=ID,maxDrawTimes=Max,
						  rewardInfo=sell_reward2p_reward_info(Reward),
						  needMaterial=NeedMaterial}						 
	 end || #data_activity_draw{description=Description,drawID=ID,maxDrawNum=Max,reward=Reward,condition=Condition} <- DrawList].
	
sell_reward2p_reward_info(SellReward) ->
	#sell_reward{coin=Coin,gerExp=GerExp,gold=Gold,item=Item,newGer=NewGer,reputation=Reputation,roleExp=RoleExp} = SellReward,
	NewGer2 = ger_lib:new_gerList2p_ger_view(NewGer),
	Item2 = item_lib:new_itemList2p_item_view(Item),
	#p_reward_info{coin=Coin,gerExp=GerExp,gerList=NewGer2,gold=Gold,itemList=Item2,reputation=Reputation,roleExp=RoleExp}.

get_info(RoleID) ->
	case ets:lookup(?ETS_ROLE_ACTIVITY, RoleID) of
		[{RoleID, #dc{}=Info}] ->
			Info;
		_ ->
			Info = get_db_info(RoleID),
			Info
	end.

get_db_info(RoleID) ->
	db_sql:get_activityInfo(RoleID).

set_info(RoleID, Info) ->
	ets:insert(?ETS_ROLE_ACTIVITY, {RoleID, Info}),
	db_sql:set_activityInfo(RoleID, Info).

clear_buff() ->
	lists:foreach(fun({RoleID, #dc{}}) when is_integer(RoleID) ->
						  case role_lib:is_online(RoleID) of
							  false->
                                  ets:delete(?ETS_ROLE_ACTIVITY, RoleID);
							  _ ->
								  ignore
						  end;
					 (_) ->
						  ignore				  
				  end, ets:tab2list(?ETS_ROLE_ACTIVITY)).

daily_fresh(RoleID)->
	#dc{actList=Info} = Acty = get_info(RoleID),
	%%　清理记录，将需要每日刷新的和过期的记录从数据库列表中删除，并通知客户端刷新活动内容
	{NewInfo, ChangeList} = 
		lists:foldl(fun(#act{actID=ID}=Act, {InfoAcc, ChangeAcc})->
							case need_refresh(ID) of
								true ->
									{InfoAcc,[ID|ChangeAcc]};
								_ ->
									{[Act|InfoAcc], ChangeAcc}
							end
					end, {[],[]}, Info),
	% delete outTime act
	set_info(RoleID, Acty#dc{actList=NewInfo}),
	lists:foreach(fun(E)->do_info(RoleID, E) end, ChangeList).
	
need_refresh(ID)->
	case data_activity:get(ID) of
		#data_activity{stopTime  = StopTime, isDailyRefresh = IsDailyRefresh} -> 
			%% out of time
			NowDate = erlang:localtime(),
			%% 计算绝对时间
			{StopDay,StopSec} = StopTime,
			if erlang:is_tuple(StopDay) ->
				   StopTime2 = StopTime;
			   true ->
				   {ServerOpenDate,_} = data_setting:get(serverOpenTime),
				   {StopDay,StopSec} = StopTime,
				   Stop = util:datetime_to_seconds({ServerOpenDate,StopSec}) + (StopDay - 1) * ?ONE_DAY_SECONDS,
				   StopTime2 = util:seconds_to_datetime(Stop)
			end,
			
			if IsDailyRefresh =:= true orelse NowDate > StopTime2 ->
				   true;
			   true ->
				   false
			end;
		_ ->
			false
	end.

clear_timeout_buff(RoleID) ->
	Info = get_info(RoleID),
	List = Info#dc.actList,
	{NewList,_} = lists:foldl(fun(ID, {ListNew,ListOld}) ->
				case lists:keytake(ID, #act.actID, ListOld) of
					false ->
						{ListNew,ListOld};
					{value, #act{}=Act, List2} ->
						{[Act|ListNew],List2}
				end
			end , {[],List}, data_activity:get_list()),
	Info2 = Info#dc{actList=NewList},
	set_info(RoleID,Info2).

%% 检查本进程buff的tick
tick() ->
	erlang:send_after(?TICK_INTERVAL*1000, self(), tick).

%% 更新活动状态
tick_state() ->
	Sec = ?TICK_STATE_INTERVAL - (util:now() rem ?TICK_STATE_INTERVAL), 
	erlang:send_after(Sec*1000,self(),tick_state).

tick_energy_time() ->
    erlang:send_after(?TICK_CHECK_ENERTY_TIME * 1000, self(), tick_energy_time),
    List =
        lists:foldr(fun(Key, Acc) ->
                            {Beg, End, EngMax, EngMin, OneClick, ClickSeconds, DelaySeconds} = data_get_energy:get(Key),
                            {Date, _Time} = erlang:localtime(),
                            [{Key, #p_energy_activity{startTime=util:datetime_to_seconds({Date, Beg}), endTime=util:datetime_to_seconds({Date, End}) - DelaySeconds,
                                                      energyMax=EngMax,energyMin=EngMin,oneClickGet=OneClick,clickSeconds=ClickSeconds}}|Acc]
                    end, [], data_get_energy:get_list()),
    erlang:put(?energy_activity_info, List).

time_diff({A,B,C},{E,F,G}) ->
	(E-A)*3600+(F-B)*60+(G-C).

type(exchange) ->1;
type(exchange2) ->5;
type(exchange3) ->7;
type(pay_special_num) ->3;
type(pay_acc_num) ->2;
type(pay_day_num) ->4;
type(consume) ->6.

default_value(pay_special_num) ->0;
default_value(pay_acc_num) ->0;
default_value(pay_day_num) ->{0, 0};
default_value(consume) ->{0, 0, 0};
default_value(exchange) ->0;
default_value(exchange2) ->0;
default_value(exchange3) ->0.

refresh_config()->
	Last_Price = get_last_price(),	
	% 同时更新下activity和box_price的配置
	tk_config:reload_config(data_box_price),
	tk_config:reload_config(data_activity),
    %% 判断是否通知客户端更新价格信息
	New_Price = lists:foldr(fun(E,Acc)->[data_box_price:get(E)|Acc] end, [], data_box_price:get_list()),
	case New_Price == Last_Price of
		true ->
			ignore;
		_ ->
            ?DEBUG("New_Price:~w, Last_Price:~w", [New_Price, Last_Price]),
			NextStopTime = lists:foldl(fun(#data_box_price2{isOpenActivity=IOA,endtime=ET},MinET)->
								case IOA of
									0->
										MinET;
									_->
										case MinET of
											0->
												ET;
											_->
												erlang:min(ET, MinET)
										end
								end
						end, 0, New_Price),
			?DEBUG("====NextStopTime====~w",[{util:now(),NextStopTime}]),
			timer_wheel:cancel_plan(get(?REFRESH_CONFIG_PLAN_PLAG)),
			case NextStopTime of
				0->
					ignore;
				_->
					{Sec,Ref} = timer_wheel:add_plan(NextStopTime+1, fun refresh_config/0),
					put(?REFRESH_CONFIG_PLAN_PLAG,{Sec,Ref})
			end,
			set_new_price(New_Price),
			ShopInfo = [#p_shop_box_info{itemTypeID=ItemTypeID,valueOne=ValueOne,valueTen=ValueTen,discount=Discount,isOpenActivity=IsOpenActivity,endtime=EndTime} || #data_box_price2{itemTypeID=ItemTypeID,oncePrice=ValueOne,tenTimesPrice=ValueTen,discount=Discount,isOpenActivity=IsOpenActivity,endtime=EndTime}<-New_Price],
			?DEBUG("===广播===~p",[ShopInfo]),
			broadcast_server:bc(#sc_box_shop_info{info=ShopInfo,stopTime=util:datetime_to_seconds(data_box:get(datetime))})
	end.

init_state() ->
	Now = util:now(),
	lists:foldl(fun(ID, {Acc1,Acc2}) ->
						#data_activity{activityName=Name,iconSrc=IconSrc,startTime=StartTime,stopTime=StopTime, isForever=IsForever} = DataActivity =  data_activity:get(ID),
						{StartDay,StartSec} = StartTime,
						if erlang:is_tuple(StartDay) ->
							   Start = util:datetime_to_seconds(StartTime),
							   Stop = util:datetime_to_seconds(StopTime);
						   true ->
							   {ServerOpenDate,_} = data_setting:get(serverOpenTime),
							   Start = util:datetime_to_seconds({ServerOpenDate,StartSec}) + (StartDay - 1) * ?ONE_DAY_SECONDS,
							   {StopDay,StopSec} = StopTime,
							   Stop = util:datetime_to_seconds({ServerOpenDate,StopSec}) + (StopDay - 1) * ?ONE_DAY_SECONDS
						end,
						if Now > Start , Now < Stop orelse IsForever =:= true->
							   StartTime2 = util:seconds_to_datetime(Start),
							   StopTime2 = util:seconds_to_datetime(Stop),
							   {[#p_activity_icon{activityID=ID,iconRrc=IconSrc,activityName=Name}|Acc1],[DataActivity#data_activity{startTime=StartTime2,stopTime=StopTime2}|Acc2]};
						   true ->
							   {Acc1,Acc2}
						end
				end , {[] ,[]}, data_activity:get_list()).


load_box_price(List) ->
    Now = util:now(),
    lists:foldl(fun(Info, Acc)->
                        #data_box_price{type=Type,start_time=StartTime, stop_time=StopTime,itemTypeID=ItemTypeID,
                                        oncePriceDis=OncePriceDis,tentimesPriceDis=TenTimesPriceDis,
                                        oncePrice=OncePrice,tentimesPrice=TenTimesPrice}=Info,
                        case Type of
                            1->
                                Start = util:datetime_to_seconds(StartTime),
                                Stop = util:datetime_to_seconds(StopTime);
                            2 ->
                                {ServerOpenDate,_} = data_setting:get(serverOpenTime),
                                {StartDay,StartSec} = StartTime,
                                Start = util:datetime_to_seconds({ServerOpenDate,StartSec}) + (StartDay -1) * ?ONE_DAY_SECONDS,
                                {StopDay, StopSec} = StopTime,
                                Stop = util:datetime_to_seconds({ServerOpenDate,StopSec}) + (StopDay -1) * ?ONE_DAY_SECONDS
                        end,
                        if Now >= Start ,Now =< Stop ->
                               Discount = trunc(OncePriceDis / OncePrice * 100),
                               Elem = #data_box_price2{itemTypeID=ItemTypeID,oncePrice=OncePriceDis,tenTimesPrice=TenTimesPriceDis,
                                                       isOpenActivity=1,endtime=Stop,discount=Discount},
                               case lists:keymember(ItemTypeID, #data_box_price2.itemTypeID, Acc) of
                                   true ->
                                       lists:keyreplace(ItemTypeID, #data_box_price2.itemTypeID, Acc, Elem);
                                   false ->
                                       [Elem|Acc]
                               end;
                           true ->
                               Elem = #data_box_price2{itemTypeID=ItemTypeID,oncePrice=OncePrice,tenTimesPrice=TenTimesPrice},
                               case lists:keymember(ItemTypeID, #data_box_price2.itemTypeID, Acc) of
                                   true ->
                                       Acc;
                                   false ->
                                       [Elem|Acc]
                               end
                        end
                end, [], List).

set_new_price(Price)->
	erlang:put(?box_price_info,Price).
get_last_price()->
	case erlang:get(?box_price_info) of
		?undefined ->
			lists:foldr(fun(E,Acc)->[data_box_price:get(E)|Acc] end, [], data_box_price:get_list());
		X ->
			X
	end.

keep_ets_exists() ->
    case lists:member(?ETS_ETC, ets:all()) of
        true  -> ok;
        false -> ets:new(?ETS_ETC, [{keypos, 1}, set, public, named_table]) 
    end,
    case lists:member(?ETS_ROLE_ACTIVITY, ets:all()) of
        true  -> ok;
        false -> ets:new(?ETS_ROLE_ACTIVITY, [{keypos, 1}, set, public, named_table]) %% the owner is activity_server pid now.
    end,
    %% 进程字典里面的玩家活动信息加入ets表
    lists:foreach(fun({RoleID, #dc{} = Info}) when erlang:is_integer(RoleID) -> 
                          true = ets:insert(?ETS_ROLE_ACTIVITY, {RoleID, Info}),
                          erlang:erase(RoleID);
                     (_Others) ->
                          ok
                  end, 
                  erlang:get()),
    ok.


add_activity_to_ets(State) ->
    ets:insert(?ETS_ETC, {activity_list, State}).



