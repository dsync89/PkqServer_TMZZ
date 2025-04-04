%% @author admin
%% @doc 精灵王活动进程


%% RankList 顺序为: 高分数在低位.

-module(hula_server).
-compile(export_all).

-behaviour(gen_server).
-include("def_role.hrl").
-include("def_mail.hrl").
-export([start_link/0,start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([fight/3,reborn/3,offline/1,update_new_king_list/1]).

-export([i/0,k/0,data_hula_transform_list/1]).

-define(state_begin, hula_state_begin).
-define(state_end, hula_state_end).

-define(RANK_REWARD_NUM, 10).
-define(BC_INTERVAL, (30 * 1000)).
-define(BC_BEFORE_BEGIN_SEC, 300).

%% ===================Dict Key Begin =========================
-define(joinFlag, joinFlag).% 玩家加入标志
-define(bcList, bcList). % 广播列表
-define(hula, hula). % 玩家状态
-define(rankList, rankList). %排行榜信息
-define(begin_time, begin_time).%本次活动开始时间
-define(harm, harm).% 等待广播的伤害列表
-define(first10,first10).% 前10名
-define(rebronPlayer,rebronPlayer).% 使用了复活的玩家列表（会增加下一回合的攻击百分比加成）
%% ===================Dict Key End   =========================

%% 虎牢关持久化信息
-record(d_hula,{
				bossQuality	%% boss当前等级
				,failTimesAcc %% 连续未杀死boss的次数
				,lastBeginTime%% 上次活动开始时间
                ,king_list=[] %%当前精灵王大赛四强列表
                ,new_king_list=[] %%新的精灵王大赛四强列表
			   }).

%% 世界boss全局信息
-record(st, {
			 state:: ?state_begin | ?state_end %活动状态
			 ,bossBase % BOSS基础信息
			 ,boss % BOSS实例
             ,quality=0
             ,king_list=[] %%当前精灵王大赛四强列表
             ,new_king_list=[] %%新的精灵王大赛四强列表
			}).

%% 排行榜信息
-record(rank, {
			   roleID 
			   ,name
			   ,harm
			  }).




i() ->
	gen_server:call(?MODULE, i).

k() ->
	db_sql:set_etc(?DB_ETC_KEY_HULA, []),
	user_default:kill(?MODULE).

update_new_king_list(RoleIDList) when erlang:length(RoleIDList) =:= 4 ->
    ?ERR("RoleIDList:~w", [RoleIDList]),
    erlang:send(?MODULE, {new_king_role_id_list, RoleIDList});
update_new_king_list(RoleIDList) ->
    ?ERR("RoleIDList:~w", [RoleIDList]).

fight(RoleID, FighterList, LieuAdd) ->
	erlang:send(?MODULE, {fight, RoleID, FighterList, LieuAdd}).

offline(RoleID) ->
	erlang:send(?MODULE, {offline, RoleID}).

is_state_begin() ->
    case gen_server:call(?MODULE, get_state) of
        #st{state= ?state_begin} ->
            true;
        _ ->
            false
    end.

reborn(RoleID, FighterList,LieuAdd) ->
	erlang:send(?MODULE, {reborn, RoleID, FighterList, LieuAdd}).

highlight_push(RoleID) ->
	erlang:send(?MODULE, {push_info, RoleID}).

start() ->
	{ok,_}=
    supervisor:start_child(world_sup, 
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
	process_flag(trap_exit,true),
	%% 获取持久化信息
	?ERR("hula_server init===================================== "),
	D_Hula = get_d_hula(),
	State = init_state(D_Hula),
	?ERR("bosslist D_Hula===================================== ~p~n",[D_Hula]),
	#d_hula{bossQuality=BossQuality,new_king_list=NewKingList} = D_Hula,
	?ERR("bosslist NewKingList===================================== ~p~n",[NewKingList]),
	?ERR("bosslist BossQuality===================================== ~p~n",[BossQuality]),
	BossList = init_boss(BossQuality, NewKingList),
	?ERR("bosslist BossList===================================== ~p~n",[BossList]),
    BossBaseList = init_boss_base(BossList),
	St = #st{boss=BossList,bossBase=BossBaseList,state=State,quality=BossQuality,king_list=NewKingList,new_king_list=NewKingList},
	sync_tick(1,BossBaseList),
    erlang:send_after(?BC_INTERVAL, self(), bc_state),
    {ok, St}.

init_boss_base(BossList) ->
    [{BossID,sum_ger_hp(GerList)}||{BossID, GerList}<-BossList].

init_boss(BossQuality,KingList) ->
    [
     begin
		  ?ERR("bosslist BossID11===================================== ~p~n",[BossID]),
         {BossID, [
                   begin
					   %%?ERR("bosslist BossID===================================== ~p~n",[BossID]),
					   %%?ERR("bosslist GerTypeID===================================== ~p~n",[GerTypeID]),
					   %%?ERR("bosslist GerQuality===================================== ~p~n",[GerQuality]),
                       NewGerTypeID = change_ger_type_id(GerTypeID, GerQuality),
                       ?change_pos((ger_attr:new_ger(NewGerTypeID, 1, BossQuality, [], [])), GerPos)
                   end
                   || {GerTypeID, GerPos, GerQuality}<-KingInfo]}
     end
     ||{BossID, KingInfo, _HeadInfo}<-KingList].

gen_new_king_list(RoleIDList) ->
    {_, KingList} =
        lists:foldr(fun(RoleID, {AccBossID, AccKingList}) ->
                            {AccBossID - 1, [gen_new_king_list2(RoleID, AccBossID)|AccKingList]}
                    end, {4, []}, RoleIDList),
    KingList.

gen_new_king_list2(RoleID, BossID) ->
    #rolePublic{isMale=IsMale, title=Title, roleName=RoleName, head=Head} = role_lib:get_rolePublic(RoleID),
    {GerList, _} = role_data:get_otherRoleFighter(RoleID),
    KingInfo = [{GerTypeID, GerPos, GerQuality}||#ger{gerBase=#gerBase{gerTypeID=GerTypeID, gerQuality=GerQuality, gerPos=GerPos}}<-GerList],
    {BossID, KingInfo, {RoleName,IsMale,Title,Head}}.

change_ger_type_id(GerTypeID, GerQuality) ->
    NewGerTypeID =
        if
            GerQuality < 10 ->
                GerTypeID + 10001;
            GerQuality < 20 ->
                GerTypeID + 10002;
            true ->
                GerTypeID + 10003
        end,
    case data_ger:get(NewGerTypeID) of
        ?undefined ->
            ?ERR("GerTypeID:~w, NewGerTypeID:~w", [GerTypeID, NewGerTypeID]),
            GerTypeID;
        _ ->
            NewGerTypeID
    end.

init_state(D_Hula) ->
	#d_hula{lastBeginTime=LastBeginTime} = D_Hula,
	
	StartTime = data_hula:get(start_time),
	StopTime  = data_hula:get(stop_time),
	
	NowTime = erlang:time(),
	NowSec = util:now(),
	?ERR("StartTime:~p, StopTime:~p~n", [StartTime, StopTime]),
	?ERR("NowTime:~p, NowSec:~p~n", [NowTime, NowSec]),
	%% 判断上次活动，是否是今天???
	case (( LastBeginTime =/=0 andalso element(1,util:seconds_to_datetime(LastBeginTime)) =:= erlang:date() ) 
		 orelse NowTime >= StopTime) of
		true ->
			?ERR("HULA SUCCESS LastBeginTime:~w NowTime:~w StopTime:~w",[LastBeginTime,NowTime,StopTime]),
			%% 明天开始活动
			BeginTime = util:datetime_to_seconds({erlang:date(),StartTime}) + ?ONE_DAY_SECONDS,
			SecTillBegin = BeginTime - NowSec,
			erlang:send_after(SecTillBegin*1000, self(), set_state_begin),
			put(?begin_time, BeginTime),
			?state_end;
		false ->		   
			%% 判断当前活动时间		
			?ERR("HULA FAIL. LastBeginTime:~w NowTime:~w StopTime:~w",[LastBeginTime,NowTime,StopTime]),
			if StartTime =< NowTime andalso NowTime < StopTime ->
				   %% 出现这种情况，应该是活动过程中误操作、bug等原因导致本进程重启，这种情况应该不去尝试扣取离线玩家的元宝，相应的本次活动也没有玩家会获得离线奖励
				   erlang:send_after(time_diff(NowTime, StopTime)*1000,self(),set_state_end),
				   put(?begin_time, NowSec),
				   ?state_begin;
			   true ->
				   BeginTime = util:datetime_to_seconds({erlang:date(),StartTime}),
				   SecTillBegin = BeginTime - NowSec,
				   erlang:send_after(SecTillBegin*1000, self(), set_state_begin),
				   put(?begin_time, BeginTime),
				   ?state_end
			end
	end.

sync_tick(Tick, BossHpList) ->
	erlang:send_after(1000, self(), {sync_tick,((Tick+1) rem 30), BossHpList}).
										   
time_diff({A,B,C},{E,F,G}) ->
	(E-A)*3600+(F-B)*60+(G-C).
	

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
handle_call(get_state, _From, State) ->
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
handle_info(Info, State) ->
	case ?CATCH(do_handle_info(State#st.state, State, Info)) of
		#st{} = NewState->
			{noreply, NewState};
		_ ->
			{noreply, State}
 end.


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

set_bcList(List) ->
	erlang:put(?bcList, List).
get_bcList() ->
	case erlang:get(?bcList) of
		List when is_list(List) ->
			List;
		_ ->
			[]
	end.

set_joinFlag(RoleID) ->
	erlang:put({?joinFlag,RoleID},1).
clear_joinFlag(RoleID) ->
	erlang:erase({?joinFlag,RoleID}).
is_join(RoleID) ->
	erlang:get({?joinFlag,RoleID}) =:= 1.

join_hula(RoleID) -> 
	case is_join(RoleID) of
		true ->
			ignore;
		false ->
			GW = role_lib:gw(RoleID),
			if is_pid(GW) ->
					set_joinFlag(RoleID),
					BcList = get_bcList(),
					case lists:member(GW,BcList) of
						true ->
							ignore;
						_ ->
							set_bcList([GW|BcList])		
					end;
				true ->
					ignore
			end
	end.
quit_hula(RoleID) ->
	case is_join(RoleID) of
		true ->
			clear_joinFlag(RoleID),
			GW = role_lib:gw(RoleID),
			if is_pid(GW) ->
					set_bcList(lists:delete(GW, get_bcList()));
				true ->
					ignore
			end;
		false ->
			ignore
	end.

sc_hula_open(RoleID, St) ->
	#st{bossBase=BossBaseList,state=State,quality=BossQuality,king_list=KingList} = St,
    case BossBaseList of
        [] ->
            IsOpen = false,
            BeginTime = 0;
        _ ->
            IsOpen = (State=:=?state_begin),
            BeginTime = get(?begin_time)
    end,
    Record =
        #sc_hula_open{
					  isOpen=IsOpen,
                      list=[begin
                                {RoleName,IsMale,Title,Head} = get_head_info(BossID, KingList),
                                #p_hula_boss_open{bossID=BossID,maxHp=MaxHp,bossQuality=BossQuality,
                                                  roleName=RoleName,isMale=IsMale,title=Title,head=Head}
                            end||{BossID,MaxHp}<-BossBaseList],
                      beginTime=BeginTime
                     },
    ?unicast(RoleID, Record).
			
-record(hula, {
			   harm % 累计伤害
			   ,coinAcc%累计获得银两
			   ,repuAcc%累计获得声望
			   ,rank%当前排名
			   ,rebornTime%复活时间
			   ,name%名字
			  }).

get_hula(RoleID) ->
	case erlang:get({?hula,RoleID}) of
		#hula{} =H ->
			H;
		_ ->
			RoleName = role_lib:get_name(RoleID),
			#hula{coinAcc=0,harm=0,name=RoleName,rank=0,rebornTime=0,repuAcc=0}
	end.

set_hula(RoleID, Hula) when is_record(Hula, hula)->
	erlang:put({?hula,RoleID}, Hula).
			
sc_hula_init_state(RoleID, St) ->
	#st{boss=BossList,king_list=KingList,bossBase=BossBaseList} = St,
	#hula{harm=Harm,rank=Rank,rebornTime=RebornTime} = get_hula(RoleID),
	Record = #sc_hula_init_state{curHarm=Harm,
                                 list=[begin
                                           {RoleName,IsMale,Title,Head} = get_head_info(BossID, KingList),
                                           {_, MaxHP} = lists:keyfind(BossID, 1, BossBaseList),
                                           #p_hula_boss_state{bossID=BossID, curHp=sum_ger_hp(GerList),
                                                              maxHP=MaxHP,roleName=RoleName,isMale=IsMale,title=Title,head=Head}
                                       end||{BossID, GerList}<-BossList],
                                 curRank=Rank,rebornTime=RebornTime, endTime= erlang:get(?begin_time) + data_hula:get(interval_sec)},
	?unicast(RoleID, Record).

get_head_info(BossID, KingList) ->
    {_, _, {RoleName,IsMale,Title,Head}} = lists:keyfind(BossID, 1, KingList),
	?ERR("get head_info=======~p~p~p~p",[RoleName,IsMale,Title,Head]),
    {RoleName,IsMale,Title,Head}.

sum_ger_hp(GerList) ->
    lists:foldr(fun(#ger{gerHp=GerHp}, Acc) ->
                        Acc + GerHp
                end, 0, GerList).

%% 请求复活时，修改数据
reborn(RoleID) ->
	%% 战斗力增强
	erlang:put({?rebronPlayer,RoleID}, data_hula:get(add_attackratio)),
	
	RoleHula = get_hula(RoleID),
	RoleHula2 = RoleHula#hula{rebornTime=0},
	set_hula(RoleID, RoleHula2). 
	
%% 当前战报
sc_hula_cur_info(RoleID) ->	
	?unicast(RoleID, #sc_hula_cur_info{hulaInfoList=get_first10()}).

%% ====================================================================
%% Internal functions
%% ====================================================================
do_handle_info(?state_begin, St, {client_msg, RoleID, #cs_hula_open{}}) ->
	?ERR("do_handle_info============~p,~p",[St,RoleID]),
	join_hula(RoleID),
	sc_hula_open(RoleID, St),
	sc_hula_init_state(RoleID, St);
do_handle_info(?state_begin, _St,{client_msg, RoleID, #cs_hula_close{}}) ->
	quit_hula(RoleID);
do_handle_info(?state_begin, St, {fight, RoleID, FighterList, LieuAdd}) ->
    join_hula(RoleID),
	do_fight(RoleID, St, FighterList,LieuAdd);
%% 活动结束时间到了
do_handle_info(?state_begin,St,set_state_end) ->
	do_fail(St);
do_handle_info(?state_begin, #st{boss=BossList}, set_state_begin) ->
    case BossList of
        [] ->
            ignore;
        _ ->
            broadcast_server:bc(#sc_push_highlight_Info{value=1, type=1})
    end;
do_handle_info(?state_begin, St, {reborn, RoleID, FighterList,LieuAdd}) ->
	reborn(RoleID),
	?unicast(RoleID,#sc_hula_reborn{result=1}),
	do_fight(RoleID, St, FighterList,LieuAdd);
do_handle_info(?state_begin, _St, {client_msg,RoleID, #cs_hula_cur_info{}}) ->
	sc_hula_cur_info(RoleID);
do_handle_info(?state_begin, St,{sync_tick,Tick, HPList}) ->
	do_sync_tick(St, Tick, HPList);
do_handle_info(?state_begin, _St, {client_msg,RoleID, #cs_hula_rank_sync{}}) ->
	sc_hula_cur_info(RoleID);
do_handle_info(?state_begin, #st{boss=BossList}, {client_msg,RoleID, #cs_hula_open_time{}}) ->
    case BossList of
        [] ->
            ?unicast(RoleID, #sc_hula_open_time{beginTime=0});
        _ ->
            ?unicast(RoleID, #sc_hula_open_time{beginTime=get(?begin_time)})
    end;
do_handle_info(?state_end, St, {client_msg, RoleID, #cs_hula_open{}}) ->
	sc_hula_open(RoleID,St);
do_handle_info(?state_end, _St, {client_msg, _RoleID, #cs_hula_close{}}) -> 
	ignore;
do_handle_info(?state_end, #st{boss=BossList}, {client_msg, RoleID, #cs_hula_open_time{}}) ->
    case BossList of
        [] ->
            ?unicast(RoleID, #sc_hula_open_time{beginTime=0});
        _ ->
            ?unicast(RoleID, #sc_hula_open_time{beginTime=get(?begin_time)})
    end;
do_handle_info(?state_end, _St, {fight, RoleID, _FighterList, _LieuAdd}) ->
	?unicast(RoleID, #sc_hula_fight{fightInfo=[],rebornTime=0,result=4,rewardCoin=0,rewardReputation=0});
%% 活动开始
do_handle_info(?state_end, #st{boss=BossList}=St, set_state_begin) ->
    case BossList of
        [] ->
            next;
        _ ->
            broadcast_server:bc(#sc_push_highlight_Info{value=1, type=1})
    end,
	NowSec = util:now(),
	EndTime = util:datetime_to_seconds({erlang:date(),data_hula:get(stop_time)}),
	Interval = EndTime - NowSec,
	erlang:send_after(Interval*1000, self(), set_state_end),
	put(?begin_time, NowSec),
	St#st{state=?state_begin};
do_handle_info(?state_end, #st{boss=BossList}=St, test_state_begin) ->
    case BossList of
        [] ->
            next;
        _ ->
            broadcast_server:bc(#sc_push_highlight_Info{value=1, type=1})
    end,
    NowSec = util:now(),
    erlang:send_after(data_hula:get(interval_sec)*1000, self(), set_state_end),
    put(?begin_time, NowSec),
    St#st{state=?state_begin};
do_handle_info(?state_begin, _St, {test_robot_fight, StartRank, StopRank, Timeout}) ->
    erlang:spawn(fun() -> test_robot_fight(StartRank, StopRank, Timeout) end);
do_handle_info(?state_end, _St, set_state_end) ->
	ignore;
do_handle_info(?state_end, _St, {reborn, RoleID,_}) ->
	?unicast(RoleID,#sc_hula_reborn{result=3});
do_handle_info(?state_end, _St, {reborn, RoleID}) ->
	?unicast(RoleID,#sc_hula_reborn{result=3});
do_handle_info(?state_end, _St, {client_msg,RoleID, #cs_hula_cur_info{}}) ->
	?unicast(RoleID,#sc_hula_cur_info{});
do_handle_info(?state_end, _St,{sync_tick,Tick, _BossOldHp}) ->
	sync_tick(Tick,[]);
%% 玩家离线，推出广播列表
do_handle_info(_, _St, {offline, RoleID}) ->
	quit_hula(RoleID);

do_handle_info(?state_begin, _St, {push_info, RoleID}) ->
	?unicast(RoleID, #sc_push_highlight_Info{value=1, type=1});
do_handle_info(?state_end, _St, {push_info, _RoleID}) ->
	ignore;
do_handle_info(?state_begin, #st{boss=BossList}, bc_state) ->
    case BossList of
        [] ->
            next;
        _ ->
            broadcast_server:bc_msgID(10034)
    end,
    erlang:send_after(?BC_INTERVAL, self(), bc_state);
do_handle_info(?state_end, #st{boss=BossList}, bc_state) ->
    erlang:send_after(?BC_INTERVAL, self(), bc_state),
    case erlang:get(?begin_time) of
        Timestamp when erlang:is_integer(Timestamp) ->
            Now = util:datetime_to_seconds(erlang:localtime()),
            case Now < Timestamp andalso Timestamp - Now =< ?BC_BEFORE_BEGIN_SEC andalso BossList =/= [] of
                true ->
                    broadcast_server:bc_msgID(10033);
                false ->
                    next
            end;
        _ ->
            next
    end;
do_handle_info(_, St, {new_king_role_id_list, RoleIDList}) ->
    NewKingList = gen_new_king_list(RoleIDList),
    St#st{new_king_list=NewKingList};
do_handle_info(_, St, reset_quality) ->
    St#st{quality=0};

do_handle_info(_, St, Info) ->
	?ERR("do_handle_info function clause:request=~100p,state=~100p",[Info,St]).
    

%% 每秒同步
do_sync_tick(St, Tick, HPList) ->
	#st{boss=BossList} = St,
    NewHPList = [{BossID,sum_ger_hp(GerList)}||{BossID, GerList}<-BossList],
	sync_tick(Tick, NewHPList),
	%% 如果boss血量修改了，则广播
    if
        HPList =/= NewHPList ->
            do_hp_sync(NewHPList, HPList);
        true ->
            ignore
    end,
	%% 广播伤害列表
	HarmList = clear_harmList(),
	if HarmList =:= [] ->
		   ignore;
	   true ->
		   do_refresh_bcList(),
		   bc(#sc_hula_harm_broadcast{harmList=HarmList})
	end,
	%% 3秒同步一次排行
	if (Tick rem 3) =:= 2 ->
		   do_rank_sync();
	   true ->
		   ignore
	end,
	if (Tick rem 5) =:= 4 ->
		   do_refresh_bcList();
	   true ->
		   ignore
	end.

%% 刷新已挂掉的网关进程
do_refresh_bcList() ->
	BcList = get_bcList(),
	BcList2 = [Pid||Pid<-BcList, is_pid(Pid) andalso is_process_alive(Pid)],
	set_bcList(BcList2).

do_hp_sync(NewHPList, HPList) ->
    lists:foreach(fun({BossID, BossHP}) ->
                          HpSyncRecord = #sc_hula_hp_sync{bossID=BossID,bossHp=BossHP},
                          bc(HpSyncRecord)
                  end, NewHPList -- HPList).

do_rank_sync() ->
	RankList = get_rankList(),
	do_rank_sync(RankList, 1).

do_rank_sync([#rank{roleID=RoleID}|RankList], Rank) ->
	case is_join(RoleID) of
		true ->
	?unicast(RoleID,#sc_hula_rank_sync{curRank=Rank});
		false ->
			ignore
	end,
	do_rank_sync(RankList,Rank+1);
do_rank_sync([], _) ->
	ok.


%% 消息广播接口
bc(Record) ->
	RecordBin = proto:encode(Record),
	lists:foreach(fun(GWPid) -> role_lib:send_client2(GWPid, RecordBin) end, get_bcList()).

do_fight(RoleID, St, FighterList,LieuAdd) ->
    
    #hula{rebornTime=RebornTime} = RoleHula = get_hula(RoleID),
    NowSec = util:now(),
    %% 给予一秒的允许误差
    if NowSec+1 >= RebornTime ->
           #st{boss=BossList} = St,
           case BossList of
               [] ->
                   ?unicast(RoleID, #sc_hula_fight{fightInfo=[],rebornTime=0,result=4,rewardCoin=0,rewardReputation=0});
               _ ->
                   {BossID, BossGerList} = get_boss_ger_list(BossList),
                   do_fight2(RoleID, St,FighterList,LieuAdd, RoleHula, NowSec,BossID, BossList,BossGerList)
           end;
       true ->
           ?unicast(RoleID, #sc_hula_fight{fightInfo=[],rebornTime=0,result=2,rewardCoin=0,rewardReputation=0})
    end.

do_fight2(RoleID, St, FighterList,LieuAdd, RoleHula, NowSec,BossID, BossList,BossGerList) ->
    %% 计算复活带来的buff加成
    BuffNum =
        case erlang:get({?rebronPlayer, RoleID}) of
            ?undefined ->
                0;
            N ->
                erlang:erase({?rebronPlayer, RoleID}),
                N
        end,
    FighterList2 = cacl_buff(FighterList, BuffNum),
    
	%% 发生战斗
	{_Result, FightRecord, {_,_,_,NewBossGerListT}} = role_fight:new(RoleID,FighterList2, BossGerList,LieuAdd,{0,0},false),
    NewBossGerList = recalc_fighter_list(BossGerList, NewBossGerListT),
    BossOldHp = sum_ger_hp(BossGerList),
    BossNewHp = sum_ger_hp(NewBossGerList),
    
	HarmT = BossOldHp - BossNewHp,
    case HarmT > 0 of
        true ->
            Harm = HarmT;
        false ->
            Harm = 0
    end,
	NewBossList = lists:keyreplace(BossID, 1, BossList, {BossID, NewBossGerList}),
	#hula{name=Name} = RoleHula,
	if Harm > 0 ->
		   %% 广播伤害
		   log_harm(Name, Harm),
		   %% 加声望、加银两、加累计伤害、重排名
		   {RoleHula2, AddCoin, AddRepu} = add_hula(RoleID, RoleHula, Harm);
	   true ->
		   RoleHula2 = RoleHula,
		   AddCoin=0,
		   AddRepu=0
	end,
	%% 判断是否胜利
	if BossNewHp =< 0 ->
		   RebornTime = 0;
	   true ->
		   RebornTime = NowSec + data_hula:get(recover_cooldown_time)
	end,
	RoleHula3 = RoleHula2#hula{rebornTime=RebornTime},
	set_hula(RoleID, RoleHula3),
	Record = #sc_hula_fight{fightInfo=[FightRecord],rebornTime=RebornTime,result=1,rewardCoin=AddCoin,rewardReputation=AddRepu},
	?unicast(RoleID, Record),
	%% 战斗结束的处理
	if BossNewHp =< 0 ->
           send_kill_reward(BossID),
           bc(#sc_hula_hp_sync{bossID=BossID,bossHp=0}),
           case BossID =:= 1 of
               true ->
                   do_win(RoleID, St, Name, RoleHula3#hula.harm);
               false ->
                   St#st{boss=NewBossList}
		   end;
	   true ->
		   St#st{boss=NewBossList}
	end.

recalc_fighter_list(FighterList, FighterList2) ->
    lists:foldr(fun(#ger{gerID=GerID}=Ger, Acc) ->
                        case lists:keyfind(GerID, #ger.gerID, FighterList2) of
                            false ->
                                Acc;
                            #ger{gerHp=GerHp} ->
                                case GerHp > 0 of
                                    true ->
                                        [Ger#ger{gerHp=GerHp}|Acc];
                                    false ->
                                        Acc
                                end
                        end
                end, [], FighterList).

get_boss_ger_list(BossList) ->
    get_boss_ger_list2(lists:reverse(BossList)).

get_boss_ger_list2([{BossID, GerList}|BossList]) ->
    case GerList of
        [] ->
            get_boss_ger_list2(BossList);
        _ ->
           {BossID, GerList} 
    end.

calc_next_boss_state_if_win(Interval, BossQuality)->
	if Interval >= 600 ->
%% 		   broadcast_server:bc_msgID(10002),
		   if BossQuality < 10 ->
				  NextBossQuality = BossQuality;
			  true->
				  NextBossQuality = BossQuality - 10
		   end;
	   Interval >= 400 ->
%% 		   broadcast_server:bc_msgID(10001,[<<",">>]),
		   NextBossQuality = BossQuality;
	   Interval >= 150 ->
%% 		   broadcast_server:bc_msgID(10001,[<<"，品阶+10，">>]),
		   NextBossQuality = BossQuality + 10;
	   Interval >= 75 ->
%% 		   broadcast_server:bc_msgID(10001,[<<"，品阶+20，">>]),
		   NextBossQuality = BossQuality + 20;
	   true->
%% 		   broadcast_server:bc_msgID(10001,[<<"，品阶+30，">>]),
		   NextBossQuality = BossQuality + 30
	end,
	NextBossQuality.

final_bc(Type, NextTime)->
	if Type =:= 1 ->
		   lists:foreach(fun(E)->
								 case E of
									 {{?joinFlag,Role},_}->
										 #hula{coinAcc=Coin,harm=Harm,repuAcc=Repu}=get_hula(Role),
										 ?unicast(Role, #sc_hula_stop{type=Type, roleSta=#p_role_stastic{harm=Harm, coin=Coin, repu=Repu},nextTime=NextTime});
									 _ ->
										 ignore
								 end
						 end, erlang:get());
	   true ->
		   ignore
	end.
		   

do_win(RoleID, St, Name, _KillerHarm) ->
	broadcast_server:bc(#sc_push_highlight_Info{value=1, type=2}),
	%% 杀死boss的世界广播
	broadcast_server:bc_msgID(10003, [Name]),

	#st{quality=BossQuality,new_king_list=NewKingList} = St,

	EndTime = util:now(),
	BeginTime = get(?begin_time),
	Interval = EndTime - BeginTime,
	%% 计算下次boss的等级
	NextBossQuality = calc_next_boss_state_if_win(Interval, BossQuality),
	%% 重新读boss的配置，并计算属性
	BossList = init_boss(NextBossQuality,NewKingList),
    BossBaseList = init_boss_base(BossList),
	

	DHula = #d_hula{bossQuality=NextBossQuality,failTimesAcc=0,lastBeginTime=BeginTime,king_list=NewKingList,new_king_list=NewKingList},
	set_d_hula(DHula),

	St2 = St#st{boss=BossList,bossBase=BossBaseList,state=init_state(DHula),quality=NextBossQuality,king_list=NewKingList},
	behavior_world_boss:log(RoleID, 2),
    
    case BossList of
        [] ->
            NextTime = 0;
        _ ->
            NextTime = erlang:get(?begin_time)
    end,
    %% 广播参与活动的玩家活动结束
    %% 给参与玩家通告本局统计(伤害，银两，声望    的统计)
    final_bc(1, NextTime),%% 1 : win

    %% 活动结束的相关处理
    do_finish_hula(),
    
	St2.

do_fail(St) ->
	broadcast_server:bc(#sc_push_highlight_Info{value=1, type=2}),
	%% 世界广播吕布苟幸活下来了
	broadcast_server:bc_msgID(10004),
	
	#d_hula{failTimesAcc=FailTimesAcc} = DHula = get_d_hula(),
	#st{quality=BossQuality,new_king_list=NewKingList} = St,
	LevelDownTimes = data_hula:get(boss_leveldown_times),
	FailTimesAcc2 = FailTimesAcc+1,
	case FailTimesAcc2 >= LevelDownTimes of
		true ->
			%% 广播boss降级了
			broadcast_server:bc_msgID(10002),
			if BossQuality > 10 ->
					NextBossQuality=BossQuality-10;
				true ->
					NextBossQuality=BossQuality
			end;
		false ->
			NextBossQuality=BossQuality
	end,

	
	BossList = init_boss(NextBossQuality,NewKingList),
    BossBaseList = init_boss_base(BossList),
	BeginTime = get(?begin_time),
	%% 持久化下次虎牢关的用到的信息
	DHula2 = #d_hula{bossQuality=NextBossQuality,failTimesAcc=FailTimesAcc2,lastBeginTime=BeginTime,king_list=NewKingList,new_king_list=NewKingList},
	set_d_hula(DHula2),

	St2 = St#st{bossBase=BossBaseList,boss=BossList,state=init_state(DHula),quality=NextBossQuality,king_list=NewKingList},
    
    case BossList of
        [] ->
            NextTime = 0;
        _ ->
            NextTime = erlang:get(?begin_time)
    end,
    %% 广播参与活动的玩家活动结束
    bc(#sc_hula_stop{type=2,roleSta=#p_role_stastic{harm=0, coin=0, repu=0},nextTime=NextTime}),
    %% 活动结束的相关处理
    do_finish_hula(),
    
	St2.



%% 活动结束的处理
do_finish_hula() ->
	%% 进行一次排名同步
	do_rank_sync(),

	clear_info().


%% 小概率重复的情况
random_seq(Scale, Num) when Scale >= 3*Num ->
	random_seq(Scale, Num, []);
random_seq(Scale, Num) ->
	util:random_list(lists:seq(1,Scale), Num).

random_seq(_, 0, List) ->
	List;
random_seq(Scale, Num, List) ->
	R = random:uniform(Scale),
	case lists:member(R, List) of
		true ->
			random_seq(Scale, Num, List);
		false ->
			random_seq(Scale, Num-1, [R|List])
	end.

%% 活动结束时的幸运奖励
do_send_lucky_reward() ->
	RankList = get_rankList(),
	LUCK_REWARD_NUM = trunc( length(RankList) * data_hula:get(reward_luck) / 100 ),
	Num = length(RankList) - ?RANK_REWARD_NUM,
	if Num =< 0 ->
		   RL=[];
	   Num =< LUCK_REWARD_NUM ->
		   RL = lists:sublist(RankList, ?RANK_REWARD_NUM + 1, ?RANK_REWARD_NUM + LUCK_REWARD_NUM);
	   true ->
		   SL = random_seq(Num, LUCK_REWARD_NUM),
		   RL = [lists:nth(?RANK_REWARD_NUM+E, RankList) || E<- SL]
	end,
	#sell_reward{} = LuckReward = data_hula:get(reward_luckers),
    LuckRoleIDList = [E||#rank{roleID=E}<-RL],
	lists:foreach(fun(E) -> mail_server:send_sys_mail(E, ?MAIL_HULA_LUCKY, [], "", LuckReward) end, LuckRoleIDList),
	{[#p_hula_info{harmValue=HV,roleName=RN} || #rank{harm=HV,name=RN} <- RL],
     LuckRoleIDList}.
				 
	
get_rank_reward(X) ->
	data_hula:get(erlang:list_to_atom(lists:concat([reward_rank,X]))).

%% 获取虎牢关持久化信息
get_d_hula() ->
	case db_sql:get_etc(?DB_ETC_KEY_HULA) of
		#d_hula{}=D_Hula ->
			D_Hula;
		[] ->
			BossQuality = data_hula:get(boss_init_quality),
			NewKingList = data_hula:get(boss_info),
			#d_hula{bossQuality=BossQuality,
					failTimesAcc=0,
					lastBeginTime=0,
					new_king_list=NewKingList}
	end.

set_d_hula(D_Hula) ->
	db_sql:set_etc(?DB_ETC_KEY_HULA, D_Hula).

%% 玩家本次活动信息
do_clear({?joinFlag, _}=Key,_) ->
    erlang:erase(Key);
do_clear({?hula,_}=Key, _) ->
	erlang:erase(Key);
do_clear({?rebronPlayer,_}=Key,_) ->
	erlang:erase(Key);
do_clear(?rankList=Key,_) ->
	erlang:erase(Key);
do_clear(?first10=Key,_) ->
	erlang:erase(Key);
do_clear(_,_) ->
	ignore.

cacl_buff(FighterList, 0) ->
    FighterList;
cacl_buff(FighterList, BuffNum) ->  
    [begin 
         GerAttack2 = trunc((GerAttr#gerAttr.gerAttack)*(1+BuffNum/100)),
         Ger#ger{gerAttr=GerAttr#gerAttr{gerAttack=GerAttack2}}
     end || 
     #ger{gerAttr=GerAttr}=Ger<-FighterList].

%% 活动结束后，清理活动信息
clear_info() ->
	%% 清理加入标识
	set_bcList([]),

	%% 清理进程字典
	lists:foreach(fun({Key,Value}) ->
						  do_clear(Key, Value)
				  end, erlang:get()).
							  


send_kill_reward(BossID) ->
    #sell_reward{} = Reward = data_hula:get({reward_kill, BossID}),
    lists:foreach(fun(#rank{roleID=E}) ->
                          mail_server:send_sys_mail(E, ?MAIL_HULA_RANK_JOIN, [], "", Reward)
                  end, get_rankList()).

add_hula(RoleID, RoleHula, Harm) ->
	#hula{coinAcc=CoinAcc,harm=HarmAcc,repuAcc=RepuAcc,name=Name} = RoleHula,
	{CoinRatio, CoinMax, RepuRatio, RepuMax, RepuMaxAcc} = data_hula:get(fight_reward_arg),
	AddCoin = erlang:min(CoinMax, trunc(Harm*CoinRatio)),
	if RepuAcc >= RepuMaxAcc ->
		   AddRepu = 0;
	   true ->
	AddRepu0 = erlang:min(RepuMax, trunc(Harm*RepuRatio)),
	AddRepu = erlang:min(RepuMaxAcc-RepuAcc, AddRepu0)
	end,
	catch role_lib:send_server(RoleID, {hula_harm_reward,AddCoin,AddRepu}),
	CoinAcc2 = CoinAcc+AddCoin,
	RepuAcc2 = RepuAcc+AddRepu,
	HarmAcc2 = HarmAcc+Harm,
	Rank2 = insert_rank(RoleID, HarmAcc2, Name, HarmAcc),
	RoleHula2 = RoleHula#hula{coinAcc=CoinAcc2,harm=HarmAcc2,rank=Rank2,repuAcc=RepuAcc2},
	{RoleHula2, AddCoin, AddRepu}.
				  

get_rankList() ->
	case erlang:get(?rankList) of
		L when is_list(L) ->
			L;
		_ ->
			[]
	end.

set_rankList(List) ->
	erlang:put(?rankList, List).

get_first10() ->
	case erlang:get(?first10) of
		L when is_list(L) ->
			L;
		_ ->
			[]
	end.


set_first10(First10) ->
	erlang:put(?first10,First10).
			
insert_rank(RoleID, Harm, Name, HarmAcc) ->
	Ranker = #rank{roleID=RoleID,harm=Harm,name=Name},
	RankList = get_rankList(),
	if HarmAcc =:= 0 ->
		   % 新的记录
		   {NewRank, NewRankList} = insert_rank2(Harm, Ranker,RankList,[],0);
	   true ->
		   % 排行榜中已有该玩家的信息
		   {NewRank, NewRankList} = insert_rank3(Harm, Ranker,RankList,[],0)
	end,
	set_rankList(NewRankList),
	if NewRank =< 10 ->
		   set_first10(
			 [#p_hula_info{harmValue=Harm1,roleName=Name1}
						  ||#rank{harm=Harm1,name=Name1}<-lists:sublist(NewRankList,1,10)]
					  );
	   true ->
		   ignore
	end,
	NewRank.

%% 新的记录的插入
insert_rank2(Harm, Ranker, [#rank{harm=Harm2}=H|RankList]=E,Tail,N) ->
	if Harm2 >= Harm ->
		   insert_rank2(Harm, Ranker, RankList, [H|Tail],N+1);
	   Harm2 < Harm ->
		   NewRankList = lists:reverse(Tail, [Ranker|E]),
		   {N + 1, NewRankList}
	end;	   
insert_rank2(_Harm, Ranker, [],Tail, N) ->
	NewRankList = lists:reverse([Ranker|Tail]),
	{N+1,NewRankList}.

%% 已存在旧的信息在排行榜
insert_rank3(Harm, Ranker, [#rank{harm=Harm2}=H|RankList]=E,Tail,N) ->
	if Harm2 >= Harm ->
	insert_rank3(Harm, Ranker, RankList, [H|Tail],N+1);
	   Harm2 < Harm ->
		   %% 删除旧的信息
		   NewRankList = lists:reverse(Tail,[Ranker|lists:keydelete(Ranker#rank.roleID, #rank.roleID, E)]),		   
		   {N + 1,NewRankList}
	end;	   
insert_rank3(_Harm, Ranker, [],Tail, N) ->
	NewRankList = lists:reverse([Ranker|Tail]),	
	{N+1,NewRankList}.

log_harm(Name, Harm) ->
	put(?harm, [#p_hula_harm{harm=Harm,name=Name}|get_harmList()]).

clear_harmList() ->
	case erlang:erase(?harm) of
		L when is_list(L) ->
			L;
		_ ->
			[]
	end.

get_harmList() ->
	case get(?harm) of
		L when is_list(L) ->
			L;
		_ ->
			[]
	end.


%% 仅当血量小于某个值时，才进行即时同步，大于这个值时，走每秒的定时同步
bc_hp(BossNewHp, BossMaxHp) ->
	if (10*BossNewHp) < BossMaxHp ->
		   bc(#sc_hula_hp_sync{bossHp=BossNewHp});
	   true ->
		   ignore
	end.
					
%% 预处理配置
%% 增加配置项，interval_sec
data_hula_transform_list(List) ->
	{start_time,ST} = lists:keyfind(start_time,1,List),
	{stop_time, PT} = lists:keyfind(stop_time, 1,List),
	IntervalSec = time_diff(ST, PT),
	[{interval_sec, IntervalSec}|List].

test_robot_fight(StartRank, StopRank, Timeout) ->
    lists:foreach(
      fun(RoleID) ->
              case Timeout > 0 of
                  true ->
                      timer:sleep(Timeout);
                  false ->
                      next
              end,
              {FighterList,LieuAdd} = db_sql:get_fighterList_and_lieu_add(RoleID),
              erlang:send(?MODULE, {fight, RoleID, FighterList,LieuAdd})          
      end, lists:seq(tk_id:rank_roleID(StartRank), tk_id:rank_roleID(StopRank))).