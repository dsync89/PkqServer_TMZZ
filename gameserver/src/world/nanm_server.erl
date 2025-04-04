%% @author admin
%% @doc 战南蛮活动进程
%% Created 2013-5-6

%% 本进程在玩家进程的上层（可以call玩家进程，但玩家进程不能call本进程）

%% RankList 顺序为: 高分数在低位.

-module(nanm_server).
-compile(export_all).

-behaviour(gen_server).
-include("def_role.hrl").
-include("def_mail.hrl").
-export([start_link/0,start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([add_buff/2,fight/3,reborn/3,offline/1,set_offlinePlayFlag/2]).

-export([i/0,k/0,data_nanm_transform_list/1]).

-define(state_begin, nanm_state_begin).
-define(state_end, nanm_state_end).

%% -define(LUCK_REWARD_NUM, 3).改用配置文件配置百分比
-define(RANK_REWARD_NUM, 10).
-define(BC_INTERVAL, (30 * 1000)).
-define(BC_BEFORE_BEGIN_SEC, 300).

%% ===================Dict Key Begin =========================
-define(joinFlag, joinFlag).% 玩家加入标志
-define(bcList, bcList). % 广播列表
-define(nanm, nanm). % 玩家状态
-define(lastInfo, lastInfo). %前回战报
-define(rankList, rankList). %排行榜信息
-define(begin_time, begin_time).%本次活动开始时间
-define(offlinePlayFlag, offlinePlayFlag).%离线参与的标识
-define(harm, harm).% 等待广播的伤害列表
-define(first10,first10).% 前10名
-define(rebronPlayer,rebronPlayer).% 使用了复活的玩家列表（会增加下一回合的攻击百分比加成）
%% ===================Dict Key End   =========================

%% 虎牢关持久化信息
-record(d_nanm,{
				bossQuality	%% boss当前等级
				,lastInfo	%% 上次战斗信息
				,failTimesAcc %% 连续未杀死boss的次数
				,lastBeginTime%% 上次活动开始时间
			   }).

%% 世界boss全局信息
-record(st, {
			 state:: ?state_begin | ?state_end %活动状态
			 ,bossBase :: {BossQuality :: ?int16, BossMaxHp :: ?int32}% BOSS基础信息
			 ,boss	:: #ger{}% BOSS实例
			 ,buffNum :: ?int16 %% 擂鼓层数
			 ,bossTotalHp :: ?int32 %% BOSS 现在的总血量
			}).

%% 排行榜信息
-record(rank, {
			   roleID 
			   ,name
			   ,harm
			  }).

-define(changeBossHp(Ger, NewHp),(Ger#ger{gerHp=NewHp})).


i() ->
	gen_server:call(?MODULE, i).

k() ->
	db_sql:set_etc(?DB_ETC_KEY_NANM, []),
	user_default:kill(?MODULE).

set_offlinePlayFlag(RoleID, OpenFlag) ->
	erlang:send(?MODULE, {set_offlinePlayFlag, RoleID, OpenFlag}).

add_buff(RoleID, BuffNum) ->
	erlang:send(?MODULE, {add_buff, RoleID, BuffNum}).

fight(RoleID, FighterList,LieuAdd) ->
	erlang:send(?MODULE, {fight, RoleID, FighterList,LieuAdd}).

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
	erlang:send(?MODULE, {reborn, RoleID, FighterList,LieuAdd}).

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
	random:seed(util:gen_random_seed()),
	process_flag(trap_exit,true),
	init_offlinePlayFlag(),
	%% 获取持久化信息
	D_Nanm = get_d_nanm(),
	set_last_info(D_Nanm#d_nanm.lastBeginTime, D_Nanm#d_nanm.lastInfo),
	State = init_state(D_Nanm),
	BossQuality = D_Nanm#d_nanm.bossQuality,
	{Boss, BossMaxHp} = init_boss(BossQuality),
	%BossMaxHp = Boss#ger.gerHp,
	St = #st{boss=Boss,bossBase={BossQuality,BossMaxHp},state=State, buffNum = get_buff() ,bossTotalHp = BossMaxHp},
	ets:new(?ETS_NANM_BUFF_FLAG, [set,public, {keypos,1},named_table]),
	sync_tick(1,BossMaxHp),
    erlang:send_after(?BC_INTERVAL, self(), bc_state),
    {ok, St}.

init_boss(BossQuality) ->
	BossInfo = data_nanm:get(boss_info),
	?INFO("boss info   ======:~w",[BossInfo]),
	{BossInfo2, BossTotalHp} = lists:foldl(fun({GerTypeID, GerLevel, GerPos},{BossAcc,HpAcc})->
						BossGer = ger_attr:new_ger(GerTypeID, GerLevel, BossQuality, [], []),
						BossGer2 = ?change_pos(BossGer, GerPos),
						{[BossGer2|BossAcc],BossGer2#ger.gerHp + HpAcc}
				end, {[],0}, BossInfo),
	BossInfo3 = lists:foldl(fun(BossGer, BossAcc)->
									%?ERR("boss:~w",[BossGer]),
									BossGerN = ?changeBossHp(BossGer, (erlang:trunc(BossTotalHp div 3) + 1)),
									%%BossGerN = ?changeBossHp(BossGer, BossTotalHp),
									[BossGerN|BossAcc]
							end, [], BossInfo2),
	{BossInfo3, BossTotalHp}.

init_state(D_Nanm) ->
	#d_nanm{lastBeginTime=LastBeginTime} = D_Nanm,
	
	StartTime = data_nanm:get(start_time),
	StopTime  = data_nanm:get(stop_time),
	NowTime = erlang:time(),
	NowSec = util:now(),
	%% 判断上次活动，是否是今天???
	case (( LastBeginTime =/=0 andalso element(1,util:seconds_to_datetime(LastBeginTime)) =:= erlang:date() ) 
		 orelse NowTime >= StopTime) of
		true ->
			?INFO("NANM SUCCESS LastBeginTime:~w NowTime:~w StopTime:~w",[LastBeginTime,NowTime,StopTime]),
			%% 明天开始活动
			BeginTime = util:datetime_to_seconds({erlang:date(),StartTime}) + ?ONE_DAY_SECONDS,
			SecTillBegin = BeginTime - NowSec, 
			erlang:send_after(SecTillBegin*1000, self(), set_state_begin),
			put(?begin_time, BeginTime),
			?state_end;
		false ->		   
			%% 判断当前活动时间		
			?INFO("NANM FAIL. LastBeginTime:~w NowTime:~w StopTime:~w",[LastBeginTime,NowTime,StopTime]),
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

sync_tick(Tick, BossHp) ->
	erlang:send_after(1000, self(), {sync_tick,((Tick+1) rem 30), BossHp}).

%% sec_till_begin(NowTime, StartTime) when NowTime <StartTime ->
%% 	time_diff(NowTime, StartTime);
%% sec_till_begin(NowTime, StartTime) ->
%% 	time_diff(NowTime, StartTime)+?ONE_DAY_SECONDS.
										   
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
	persist_offlinePlayList(),
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

join_nanm(RoleID) -> 
	case is_join(RoleID) of
		true ->
			ignore;
		false ->
			
			set_joinFlag(RoleID),
			GW = role_lib:gw(RoleID),
			set_bcList([GW|get_bcList()])
	end.
quit_nanm(RoleID) ->
	case is_join(RoleID) of
		true ->
			clear_joinFlag(RoleID),
			set_bcList(lists:delete(role_lib:gw(RoleID), get_bcList()));
		false ->
			ignore
	end.

get_buff_flag(RoleID) ->
	case ets:lookup(?ETS_NANM_BUFF_FLAG, RoleID) of
		[] ->
			false;
		[_] ->
			true
	end.



%% 从数据库中初始化离线参与的玩家列表
init_offlinePlayFlag() ->
	RoleIDList = get_db_offlinePlayList(),
	lists:foreach(fun(RoleID) -> set_offlinePlayFlag(RoleID) end, RoleIDList).

%% 离线参与虎牢关的标识
get_offlinePlayFlag(RoleID) ->
	case erlang:get({?offlinePlayFlag,RoleID}) of
		1 ->
			true;
		_ ->
			false
	end.

set_offlinePlayFlag(RoleID) ->
	erlang:put({?offlinePlayFlag, RoleID}, 1).

%% 获取离线参与虎牢关的玩家ID列表
get_offlinePlayList() ->
	[RoleID||{{?offlinePlayFlag,RoleID},1} <-erlang:get()].

get_db_offlinePlayList() ->
	case db_sql:get_etc(?DB_ETC_KEY_NANM_OFFLINE_PLAY) of	
		List when is_list(List)->
			List;
		_ ->
			[]
	end.
	
%% 持久化离线参与虎牢关的玩家ID
persist_offlinePlayList() ->
	db_sql:set_etc(?DB_ETC_KEY_NANM_OFFLINE_PLAY, get_offlinePlayList()).

sc_nanm_open(RoleID, St) ->
	#st{bossBase={BossQuality, BossMaxHp},buffNum=BuffNum,state=State} = St,
	Record = 
	#sc_nanm_open{
				  bossQuality=BossQuality,
				  buffNum=BuffNum,
				  isBuffed=get_buff_flag(RoleID),
				  isOpen=(State=:=?state_begin),
				  maxHp=BossMaxHp,
				  isOfflinePlay=get_offlinePlayFlag(RoleID),
				  beginTime=get(?begin_time)
				 },
	?INFO("sc_nanm_open:~w,~w\n",[RoleID,Record]),
	?unicast(RoleID, Record).
			
-record(nanm, {
			   harm % 累计伤害
			   ,coinAcc%累计获得银两
			   ,repuAcc%累计获得声望
			   ,rank%当前排名
			   ,rebornTime%复活时间
			   ,name%名字
			  }).

get_nanm(RoleID) ->
	case erlang:get({?nanm,RoleID}) of
		#nanm{} =H ->
			H;
		_ ->
			RoleName = role_lib:get_name(RoleID),
			#nanm{coinAcc=0,harm=0,name=RoleName,rank=0,rebornTime=0,repuAcc=0}
	end.

set_nanm(RoleID, Nanm) when is_record(Nanm, nanm)->
	erlang:put({?nanm,RoleID}, Nanm).
			
sc_nanm_init_state(RoleID, St) ->
	#st{bossTotalHp=BossHp}=St,
	#nanm{harm=Harm,rank=Rank,rebornTime=RebornTime} = get_nanm(RoleID),
	Record = #sc_nanm_init_state{curHarm=Harm,curHp=BossHp,curRank=Rank,rebornTime=RebornTime},
	?unicast(RoleID, Record).

get_last_info() ->
	case erlang:get(?lastInfo) of
		{_,_}=I ->
			I;
		?undefined ->
			LastInfo = #sc_nanm_last_info_fail{bossQuality=data_nanm:get(boss_init_quality),intervalSec=-1},
			InfoBin = proto:encode(LastInfo),
			Info = {0, InfoBin},
			put(?lastInfo,Info),
			Info
	end.

set_last_info(SaveID, Info) when is_tuple(Info)->
	put(?lastInfo, {SaveID,proto:encode(Info)}).

sc_last_info(RoleID, CurSaveID) ->
	case get_last_info() of
		{CurSaveID,_} ->
			?unicast(RoleID, #sc_nanm_last_info_ignore{});
		{_, InfoBin} ->
			?unicast(RoleID, InfoBin);
		_ ->
			LastInfo = #sc_nanm_last_info_fail{bossQuality=data_nanm:get(boss_init_quality),intervalSec=-1},
			InfoBin = proto:encode(LastInfo),
			put(?lastInfo,InfoBin)
	end.

%% 请求复活时，修改数据
reborn(RoleID) ->
	%% 战斗力增强
	erlang:put({?rebronPlayer,RoleID}, data_nanm:get(add_attackratio)),
	
	RoleNanm = get_nanm(RoleID),
	RoleNanm2 = RoleNanm#nanm{rebornTime=0},
	set_nanm(RoleID, RoleNanm2). 
	
%% 当前战报
sc_nanm_cur_info(RoleID) ->	
	?unicast(RoleID, #sc_nanm_cur_info{nanmInfoList=get_first10()}).

%% ====================================================================
%% Internal functions
%% ====================================================================
do_handle_info(?state_begin, St, {client_msg, RoleID, #cs_nanm_open{}}) ->
	join_nanm(RoleID),
	sc_nanm_open(RoleID, St),
	sc_nanm_init_state(RoleID, St);
do_handle_info(?state_begin, _St,{client_msg, RoleID, #cs_nanm_close{}}) ->
	quit_nanm(RoleID);
do_handle_info(?state_begin, St, {fight, RoleID, FighterList,LieuAdd}) ->
	do_fight(RoleID, St, FighterList,LieuAdd);
%% 活动结束时间到了
do_handle_info(?state_begin,St,set_state_end) ->
	do_fail(St);
do_handle_info(?state_begin, _St, set_state_begin) ->
	broadcast_server:bc(#sc_push_highlight_Info{value=2, type=1}),
	ignore;
do_handle_info(?state_begin, St, {reborn, RoleID, FighterList,LieuAdd}) ->
	reborn(RoleID),
	?unicast(RoleID,#sc_nanm_reborn{result=1}),
	do_fight(RoleID, St, FighterList,LieuAdd);
do_handle_info(?state_begin, _St, {client_msg,RoleID, #cs_nanm_cur_info{}}) ->
	sc_nanm_cur_info(RoleID);
do_handle_info(?state_begin, St,{sync_tick,Tick, BossOldHp}) ->
	do_sync_tick(St, Tick, BossOldHp);
do_handle_info(?state_begin, _St, {client_msg,RoleID, #cs_nanm_rank_sync{}}) ->
	sc_nanm_cur_info(RoleID);
do_handle_info(?state_begin, _St, {client_msg,RoleID, #cs_nanm_open_time{}}) ->
	?unicast(RoleID, #sc_nanm_open_time{beginTime=0});

do_handle_info(?state_end, St, {client_msg, RoleID, #cs_nanm_open{}}) ->
	sc_nanm_open(RoleID,St);
do_handle_info(?state_end, _St, {client_msg, _RoleID, #cs_nanm_close{}}) -> 
	ignore;
do_handle_info(?state_end, _St, {client_msg, RoleID, #cs_nanm_open_time{}}) ->
	?unicast(RoleID, #sc_nanm_open_time{beginTime=get(?begin_time)});
do_handle_info(?state_end, _St, {fight, RoleID, _FighterList, _LieuAdd}) ->
	?unicast(RoleID, #sc_nanm_fight{fightInfo=[],rebornTime=0,result=4,rewardCoin=0,rewardReputation=0});
%% 活动开始
do_handle_info(?state_end, St, set_state_begin) ->
	broadcast_server:bc(#sc_push_highlight_Info{value=2, type=1}),
	NowSec = util:now(),
	EndTime = util:datetime_to_seconds({erlang:date(),data_nanm:get(stop_time)}),
	Interval = EndTime - NowSec,
	erlang:send_after(Interval*1000, self(), set_state_end),
	put(?begin_time, NowSec),
	St#st{state=?state_begin};
do_handle_info(?state_end, St, test_state_begin) ->
    broadcast_server:bc(#sc_push_highlight_Info{value=2, type=1}),
    NowSec = util:now(),
    erlang:send_after(3600*1000, self(), set_state_end),
    put(?begin_time, NowSec),
    St#st{state=?state_begin};
do_handle_info(?state_begin, _St, {test_robot_fight, StartRank, StopRank, Timeout}) ->
    erlang:spawn(fun() -> test_robot_fight(StartRank, StopRank, Timeout) end);
do_handle_info(?state_end, _St, set_state_end) ->
	ignore;
do_handle_info(?state_end, _St, {reborn, RoleID,_}) ->
	?unicast(RoleID,#sc_nanm_reborn{result=3});
do_handle_info(?state_end, _St, {reborn, RoleID}) ->
	?unicast(RoleID,#sc_nanm_reborn{result=3});
do_handle_info(?state_end, _St, {client_msg,RoleID, #cs_nanm_cur_info{}}) ->
	?unicast(RoleID,#sc_nanm_cur_info_ignore{});
do_handle_info(?state_end, _St,{sync_tick,Tick, _BossOldHp}) ->
	sync_tick(Tick,-1);


do_handle_info(_, _St, {set_offlinePlayFlag,RoleID, OpenFlag}) ->
	if OpenFlag =:= true ->
		   set_offlinePlayFlag(RoleID)
	end,
	?unicast(RoleID,#sc_nanm_offline_play{result=1,newOpenFlag=OpenFlag});

do_handle_info(_, _St, {client_msg, RoleID, #cs_nanm_last_info{curSavedInfoID=CurSaveID}}) ->
	sc_last_info(RoleID, CurSaveID);

%% 擂鼓处理，已经扣了钱了。设计时已考虑到扣了钱，但是无法擂鼓。此bug由前端控制。
do_handle_info(_, St, {add_buff, _RoleID, BuffNum}) ->
	BuffNum2 = St#st.buffNum+BuffNum,
	set_buff2etc(BuffNum2),
	St2 = St#st{buffNum=BuffNum2},
	%% 每个玩家只能点一次擂鼓，故可以即时同步
	bc(#sc_nanm_buff_sync{buffNum=BuffNum2}),
	St2;
%% 玩家离线，推出广播列表
do_handle_info(_, _St, {offline, RoleID}) ->
	quit_nanm(RoleID);

do_handle_info(?state_begin, _St, {push_info, RoleID}) ->
	?unicast(RoleID, #sc_push_highlight_Info{value=2, type=1});
do_handle_info(?state_end, _St, {push_info, _RoleID}) ->
	ignore;
do_handle_info(?state_begin, _St, bc_state) ->
    erlang:send_after(?BC_INTERVAL, self(), bc_state),
    broadcast_server:bc_msgID(10036);
do_handle_info(?state_end, _St, bc_state) ->
    erlang:send_after(?BC_INTERVAL, self(), bc_state),
    case erlang:get(?begin_time) of
        Timestamp when erlang:is_integer(Timestamp) ->
            Now = util:datetime_to_seconds(erlang:localtime()),
            case Now < Timestamp andalso Timestamp - Now =< ?BC_BEFORE_BEGIN_SEC of
                true ->
                    broadcast_server:bc_msgID(10035);
                false ->
                    next
            end;
        _ ->
            next
    end;


do_handle_info(_, St, Info) ->
	?ERR("do_handle_info function clause:request=~100p,state=~100p",[Info,St]).
    

%% 每秒同步
do_sync_tick(St, Tick, BossOldHp) ->
	#st{bossTotalHp=BossHp} = St,
	sync_tick(Tick, BossHp),
	%% 如果boss血量修改了，则广播
	if BossOldHp =/= BossHp ->
		do_hp_sync(BossHp);
	   true ->
		   ignore
	end,
	%% 广播伤害列表
	HarmList = clear_harmList(),
	if HarmList =:= [] ->
		   ignore;
	   true ->
		   do_refresh_bcList(),
		   bc(#sc_nanm_harm_broadcast{harmList=HarmList})
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
	BcList2 = [Pid||Pid<-BcList, is_process_alive(Pid)],
	set_bcList(BcList2).

do_hp_sync(BossHp) ->
	HpSyncRecord = #sc_nanm_hp_sync{bossHp=BossHp},
	bc(HpSyncRecord).

do_rank_sync() ->
	RankList = get_rankList(),
	do_rank_sync(RankList, 1).

do_rank_sync([#rank{roleID=RoleID}|RankList], Rank) ->
	case is_join(RoleID) of
		true ->
	?unicast(RoleID,#sc_nanm_rank_sync{curRank=Rank});
		false ->
			ignore
	end,
	do_rank_sync(RankList,Rank+1);
do_rank_sync([], _) ->
	ok.


%% 消息广播接口
bc(Record) ->
	RecordBin = proto:encode(Record),
	%lists:foreach(fun(GWPid) -> role_lib:send_client_force(GWPid, RecordBin) end, get_bcList()).
	lists:foreach(fun(GWPid) -> role_lib:send_client2(GWPid, RecordBin) end, get_bcList()).

do_fight(RoleID, St, FighterList,LieuAdd) ->
	#nanm{rebornTime=RebornTime} = RoleNanm = get_nanm(RoleID),
	NowSec = util:now(),
	%% 给予一秒的允许误差
	if NowSec+1 >= RebornTime ->
		   %% 援军助阵时也允许挑战BOSS
		   do_fight2(RoleID, St,FighterList,LieuAdd, RoleNanm, NowSec);
%% 		   case get_offlinePlayFlag(RoleID) of
%% 			   true ->
%% 				   ?unicast(RoleID, #sc_nanm_fight{fightInfo=[],rebornTime=0,result=5,rewardCoin=0,rewardReputation=0});
%% 			   _ ->
%% 				   do_fight2(RoleID, St,FighterList,LieuAdd, RoleNanm, NowSec)
%% 		   end;
	   true ->
		   ?unicast(RoleID, #sc_nanm_fight{fightInfo=[],rebornTime=0,result=2,rewardCoin=0,rewardReputation=0})
		   end.

do_fight2(RoleID, St, FighterList,LieuAdd, RoleNanm, NowSec) ->
	#st{boss=Boss, buffNum=BuffNum, bossBase={_, BossMaxHp},bossTotalHp=BossTotalHp} = St,
	?INFO("boss:~w\n",[St]),
	
	%% 计算复活带来的buff加成
	BuffNum2 =
		case erlang:get({?rebronPlayer, RoleID}) of
			?undefined ->
				BuffNum;
			N ->
				erlang:erase({?rebronPlayer, RoleID}),
				BuffNum + N
		end,
	
	%% 计算援军助阵带来的buff加成
	BuffNum3 =
		case get_offlinePlayFlag(RoleID) of
			true ->
				BuffNum2 + data_nanm:get(add_reinforcements);
			_ ->
				BuffNum2
		end,

	%% 加擂鼓buff
    {FightBoss, BossTotalHp2} = lists:foldl(
                                  fun(BossGer,{BossAcc, BossHpAcc})->
                                          ChangeBossHP = erlang:trunc(BossTotalHp / 3) + 1,
                                          BossGer2 = ?changeBossHp(BossGer, ChangeBossHP), 
                                          {[BossGer2|BossAcc], BossHpAcc + ChangeBossHP}
                                  end, {[], 0}, Boss),
	FighterList2 = cacl_buff(FighterList, BuffNum3),
	%% 发生战斗
	{_Result, FightRecord, {_, NewBossState,_,_}} = role_fight:new(RoleID,FighterList2, FightBoss,LieuAdd, {0,0},false),
	%% 防止加生命的登场技能导致生命超出
	NewBossHp = lists:foldl(fun({_,Hp,_}, BossHpAcc)->
									 Hp + BossHpAcc
							 end,0, NewBossState),
	BossNewHp2 = erlang:min(NewBossHp,BossMaxHp),
	Harm = BossTotalHp2 - BossNewHp2,
	%Boss2 = Boss#ger{gerHp=BossNewHp2},
	
	#nanm{name=Name} = RoleNanm,
	if Harm > 0 ->
		   %% 广播伤害
		   log_harm(Name, Harm),
		   %% 广播同步boss血量
			?INFO("boss hp:~w boss hp_max:~w \n ",[BossNewHp2,BossMaxHp]),
		   % bc_hp(BossNewHp2, BossMaxHp),
		   %% 加声望、加银两、加累计伤害、重排名
		   {RoleNanm2, AddCoin, AddRepu} = add_nanm(RoleID, RoleNanm, Harm);
	   true ->
		   RoleNanm2 = RoleNanm,
		   AddCoin=0,
		   AddRepu=0
	end,
	%% 判断是否胜利
	if BossNewHp2 =< 0 ->
		   RebornTime = 0;
	   true ->
		   RebornTime = NowSec + data_nanm:get(recover_cooldown_time)
	end,
	RoleNanm3 = RoleNanm2#nanm{rebornTime=RebornTime},
	set_nanm(RoleID, RoleNanm3),
	Record = #sc_nanm_fight{fightInfo=[FightRecord],rebornTime=RebornTime,result=1,rewardCoin=AddCoin,rewardReputation=AddRepu},
	?unicast(RoleID, Record),
    case RoleID >= tk_id:robot_roleID_max() of
        true ->
	       ?CATCH(role_task:send_dispach(RoleID,{dispach_task,role_join_nanm}));
        false ->
            next
    end,
	behavior_world_boss:log(RoleID, 1),
	%% 战斗结束的处理
	if BossNewHp2 =< 0 ->
		   do_win(RoleID, St, Name, RoleNanm3#nanm.harm);
	   true ->
		   St#st{bossTotalHp = NewBossHp}
	end.

test(I,B)->
	calc_next_boss_state_if_win(I,B),
	?ERR("get info:~w,~w",[get_nanm(4000002),[<<"，品阶-10，">>]]),
	broadcast_server:bc_msgID(10017,["，品阶+30，"]).

calc_next_boss_state_if_win(Interval, BossQuality)->
	if Interval >= 600 ->
		   broadcast_server:bc_msgID(10015),
		   if BossQuality < 10 ->
				  NextBossQuality = BossQuality;
			  true->
				  NextBossQuality = BossQuality - 10
		   end;
	   Interval >= 400 ->
		   broadcast_server:bc_msgID(10016,[<<",">>]),
		   NextBossQuality = BossQuality;
	   Interval >= 150 ->
		   broadcast_server:bc_msgID(10016,[<<"，品阶+10，">>]),
		   NextBossQuality = BossQuality + 10;
	   Interval >= 75 ->
		   broadcast_server:bc_msgID(10016,[<<"，品阶+20，">>]),
		   NextBossQuality = BossQuality + 20;
	   true->
		   broadcast_server:bc_msgID(10016,[<<"，品阶+30，">>]),
		   NextBossQuality = BossQuality + 30
	end,
	NextBossQuality.

final_bc(Type)->
	if Type =:= 1 ->
		   lists:foreach(fun(E)->
								 %?ERR("role:~w",[E]),
								 case E of
									 {{?joinFlag,Role},_}->
										 #nanm{coinAcc=Coin,harm=Harm,repuAcc=Repu}=get_nanm(Role),
										 ?unicast(Role, #sc_nanm_stop{type=Type, roleSta=#p_role_stastic{harm=Harm, coin=Coin, repu=Repu}});
									 _ ->
										 ignore
								 end
						 end, erlang:get());
	   true ->
		   ignore
	end.
		   

do_win(RoleID, St, Name, KillerHarm) ->
	broadcast_server:bc(#sc_push_highlight_Info{value=2, type=2}),
	%% 杀死boss的世界广播
	broadcast_server:bc_msgID(10017, [Name]),
	%% 广播参与活动的玩家活动结束
	%% 给参与玩家通告本局统计(伤害，银两，声望    的统计)

	final_bc(1),%% 1 : win

	#st{bossBase={BossQuality,BossOldMaxHp}} = St,
	send_killer_reward(RoleID),

	EndTime = util:now(),
	BeginTime = get(?begin_time),
	Interval = EndTime - BeginTime,
	%% 计算下次boss的等级
	NextBossQuality = calc_next_boss_state_if_win(Interval, BossQuality),
	%% 重新读boss的配置，并计算属性
	{Boss, BossMaxHp} = init_boss(NextBossQuality),
	%BossMaxHp = Boss#ger.gerHp,
	
	%% 发送幸运奖励
	{LuckRoleList, LuckRoleIDList} = do_send_lucky_reward(),
	LuckRoleNameList = [E||#p_nanm_info{roleName=E}<-LuckRoleList],
	LastNanmInfoList = [#p_nanm_info{harmValue=KillerHarm,roleName=Name}|lists:sublist(get_first10(), ?RANK_REWARD_NUM)],
	LastInfo = #sc_nanm_last_info_win{bossQuality=BossQuality,
									  bossMaxHp=BossOldMaxHp,
									  nanmInfolist=LastNanmInfoList,
									  intervalSec=Interval,
									  luckyRoleList=LuckRoleNameList},
	set_last_info(BeginTime, LastInfo),
	DNanm = #d_nanm{bossQuality=NextBossQuality,failTimesAcc=0,lastBeginTime=BeginTime,lastInfo=LastInfo},
	set_d_nanm(DNanm),
	
	%% 给排名前10的玩家发奖励
	First10 = lists:sublist(get_rankList(),?RANK_REWARD_NUM),
    First10RoleIDList = [E||#rank{roleID=E}<-First10],
	lists:foldl(fun(E, Acc) ->
						#sell_reward{} = Reward = get_rank_reward(Acc),
						mail_server:send_sys_mail(E, ?MAIL_NANM_RANK_FIRST10, [Acc], "", Reward),
						Acc+1
				end	, 1, First10RoleIDList),
    
    %% 发送参与奖
    JoinReward = data_nanm:get(reward_join),
    RewardedRoleIDList = [RoleID] ++ First10RoleIDList ++ LuckRoleIDList,
    lists:foreach(fun(#rank{roleID=E}) ->
                          case lists:member(E, RewardedRoleIDList) of
                              false ->
                                  mail_server:send_sys_mail(E, ?MAIL_NANM_RANK_JOIN, [], "", JoinReward);
                              true ->
                                  next
                          end
                  end, get_rankList()),
	%% 活动结束的相关处理
	do_finish_nanm(),
	%% 新的虎牢关状态
	set_buff2etc(0),
	St2 = St#st{boss=Boss,bossBase={NextBossQuality,BossMaxHp},buffNum=0,state=init_state(DNanm),bossTotalHp = BossMaxHp},
	St2.

do_fail(St) ->
	broadcast_server:bc(#sc_push_highlight_Info{value=2, type=2}),
	%% 世界广播吕布苟幸活下来了
	broadcast_server:bc_msgID(10018),
	%% 广播参与活动的玩家活动结束
	bc(#sc_nanm_stop{type=2,roleSta=#p_role_stastic{harm=0, coin=0, repu=0}}),
	
	#d_nanm{failTimesAcc=FailTimesAcc} = DNanm = get_d_nanm(),
	#st{bossBase={BossQuality,_}} = St,
	LevelDownTimes = data_nanm:get(boss_leveldown_times),
	FailTimesAcc2 = FailTimesAcc+1,
	case FailTimesAcc2 >= LevelDownTimes of
		true ->
			%% 广播boss降级了
			broadcast_server:bc_msgID(10015),
			if BossQuality > 10 ->
					NextBossQuality=BossQuality-10;
				true ->
					NextBossQuality=BossQuality
			end;
		false ->
			NextBossQuality=BossQuality
	end,
	%% 发送幸运奖励
	%%do_send_lucky_reward(),
	
	{Boss, BossMaxHp} = init_boss(NextBossQuality),
	%BossMaxHp = Boss#ger.gerHp,
	BeginTime = get(?begin_time),
	LastInfo = #sc_nanm_last_info_fail{bossQuality=BossQuality,intervalSec=data_nanm:get(interval_sec)},
	set_last_info(BeginTime, LastInfo),
	%% 持久化下次虎牢关的用到的信息
	DNanm2 = #d_nanm{bossQuality=NextBossQuality,failTimesAcc=FailTimesAcc2,lastBeginTime=BeginTime,lastInfo=LastInfo},
	set_d_nanm(DNanm2),
	%% 活动结束的相关处理
	do_finish_nanm(),
	%% 新的虎牢关状态
	set_buff2etc(0),
	St2 = St#st{bossBase={NextBossQuality,BossMaxHp},boss=Boss,state=init_state(DNanm),buffNum=0,bossTotalHp = BossMaxHp},
	St2.



%% 活动结束的处理
do_finish_nanm() ->
	%% 进行一次排名同步
	do_rank_sync(),

	%% 给已付过钱的离线参与玩家发奖励
	PayedList = get_offlinePlayList(),
	#sell_reward{} = OfflinePayReward = data_nanm:get(offline_play_reward),
	lists:foreach(fun(RoleID) -> mail_server:send_sys_mail(RoleID, ?MAIL_NANM_OFFLINE, [], "", OfflinePayReward) end, PayedList),
	clear_info(),
	persist_offlinePlayList().


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
	LUCK_REWARD_NUM = trunc( length(RankList) * data_nanm:get(reward_luck) / 100 ),
	Num = length(RankList) - ?RANK_REWARD_NUM,
	if Num =< 0 ->
		   RL=[];
	   Num =< LUCK_REWARD_NUM ->
		   RL = lists:sublist(RankList, ?RANK_REWARD_NUM + 1, ?RANK_REWARD_NUM + LUCK_REWARD_NUM);
	   true ->
		   SL = random_seq(Num, LUCK_REWARD_NUM),
		   RL = [lists:nth(?RANK_REWARD_NUM+E, RankList) || E<- SL]
	end,
	#sell_reward{} = LuckReward = data_nanm:get(reward_luckers),
    LuckRoleIDList = [E||#rank{roleID=E}<-RL],
	lists:foreach(fun(E) -> mail_server:send_sys_mail(E, ?MAIL_NANM_LUCKY, [], "", LuckReward) end, LuckRoleIDList),
	{[#p_nanm_info{harmValue=HV,roleName=RN} || #rank{harm=HV,name=RN} <- RL],
     LuckRoleIDList}.
				 
	
get_rank_reward(X) ->
    data_nanm:get(erlang:list_to_atom(lists:concat([reward_rank,X]))).

%% 获取虎牢关持久化信息
get_d_nanm() ->
	case db_sql:get_etc(?DB_ETC_KEY_NANM) of
		#d_nanm{}=D_Nanm ->
			D_Nanm;
		[] ->
			BossQuality = data_nanm:get(boss_init_quality),
			#d_nanm{bossQuality=BossQuality,
					failTimesAcc=0,
					lastInfo=#sc_nanm_last_info_fail{bossQuality=BossQuality,intervalSec=-1},
					lastBeginTime=0}
	end.

set_d_nanm(D_Nanm) ->
	db_sql:set_etc(?DB_ETC_KEY_NANM, D_Nanm).


%% 擂鼓buff
get_buff() ->
	case db_sql:get_etc(?DB_ETC_KEY_NANM_BUFF) of
		BuffNum when is_integer(BuffNum) ->
			BuffNum;
		_ ->
			0
	end.
 
set_buff2etc(BuffNum) ->
	db_sql:set_etc(?DB_ETC_KEY_NANM_BUFF, BuffNum).

%% 玩家本次活动信息
do_clear({?nanm,_}=Key, _) ->
	erlang:erase(Key);
do_clear({?offlinePlayFlag,_}=Key,_) ->
	erlang:erase(Key);
do_clear({?rebronPlayer,_}=Key,_) ->
	erlang:erase(Key);
do_clear(?rankList=Key,_) ->
	erlang:erase(Key);
do_clear(?first10=Key,_) ->
	erlang:erase(Key);
do_clear(_,_) ->
	ignore.

%% 活动结束后，清理活动信息
clear_info() ->
	%%　擂鼓标识
	ets:delete_all_objects(?ETS_NANM_BUFF_FLAG),
	%% 清理进程字典
	lists:foreach(fun({Key,Value}) ->
						  do_clear(Key, Value)
				  end, erlang:get()).
							  


send_killer_reward(RoleID) ->
	#sell_reward{} = Reward = data_nanm:get(reward_killer),
	mail_server:send_sys_mail(RoleID, ?MAIL_NANM_KILL, [], "", Reward).

add_nanm(RoleID, RoleNanm, Harm) ->
	#nanm{coinAcc=CoinAcc,harm=HarmAcc,repuAcc=RepuAcc,name=Name} = RoleNanm,
	{CoinRatio, CoinMax, RepuRatio, RepuMax, RepuMaxAcc} = data_nanm:get(fight_reward_arg),
	AddCoin = erlang:min(CoinMax, trunc(Harm*CoinRatio)),
	if RepuAcc >= RepuMaxAcc ->
		   AddRepu = 0;
	   true ->
	AddRepu0 = erlang:min(RepuMax, trunc(Harm*RepuRatio)),
	AddRepu = erlang:min(RepuMaxAcc-RepuAcc, AddRepu0)
	end,
	catch role_lib:send_server(RoleID, {nanm_harm_reward,AddCoin,AddRepu}),
	CoinAcc2 = CoinAcc+AddCoin,
	RepuAcc2 = RepuAcc+AddRepu,
	HarmAcc2 = HarmAcc+Harm,
	Rank2 = insert_rank(RoleID, HarmAcc2, Name, HarmAcc),
	RoleNanm2 = RoleNanm#nanm{coinAcc=CoinAcc2,harm=HarmAcc2,rank=Rank2,repuAcc=RepuAcc2},
	{RoleNanm2, AddCoin, AddRepu}.
				  

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
			 [#p_nanm_info{harmValue=Harm1,roleName=Name1}
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

	
	
cacl_buff(FighterList, 0) ->
	FighterList;
cacl_buff(FighterList, BuffNum) ->	
	[begin 
		 GerAttack2 = trunc((GerAttr#gerAttr.gerAttack)*(1+BuffNum/100)),
		 Ger#ger{gerAttr=GerAttr#gerAttr{gerAttack=GerAttack2}}
	 end || 
	 #ger{gerAttr=GerAttr}=Ger<-FighterList].

log_harm(Name, Harm) ->
	put(?harm, [#p_nanm_harm{harm=Harm,name=Name}|get_harmList()]).

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
		   bc(#sc_nanm_hp_sync{bossHp=BossNewHp});
	   true ->
		   ignore
	end.


						
%% 预处理配置
%% 增加配置项，interval_sec
data_nanm_transform_list(List) ->
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


