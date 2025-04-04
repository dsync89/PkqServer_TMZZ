%% @author caijunjun
%% @doc 大乱斗跨服控制进程.


%% 说明：
%%    进程字典中存放的是所有报名的玩家, 战役情况，战斗玩家信息
%% 	  数据库只保存有#state{} 和报名玩家
%%	  
%% 补充：
%% 	  1.主服务器初次启动时，会同步管理的从服务器节点，直到满足到一半以上的从服务器为止
%%	  2.主服务器重启时，不会再同步节点。避免同一从服务器上的玩家在不同的主服务器上报名、战斗
%%	  3.每次战役开启时，会同步一次主服务器。添加节点会在这个时候生效，删除节点时也会在此时生效
%%		注意：某一从节点改变它的主节点时，必须保证删掉和添加的主服务器配置中的大乱斗时间（一般不会有变化）一致！！
%%	  4.从服务器可以任意开启、关闭，不会对游戏造成大影响
%%			影响情况：开启战斗中，不能与此从服务器的玩家PK(中间有3次重新找对手的机会)，所以 可能会有 没有找到对手的情况
%%	  5.主服务器关闭后，大乱斗 不能正常开放（报名和战斗都不行，查看显示未报名）
%%
%%

-module(melee_master_server).
-behaviour(gen_server).

-include("common.hrl").
-include("def_melee.hrl").
-include("def_mail.hrl").

-define(sign, sign).
-define(role_fight, role_fight).
-define(battle_ground, battle_ground).

-define(camp_miaomiao, 1).
-define(camp_pipi, 2).

-export([start_link/0, start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================


%% ====================================================================
%% Behavioural functions 
%% ====================================================================

-record(ground_state, {
                       ground_id = 0                        %% 战场id
                      
                       ,miaomiao_fans = []                  %% 喵喵粉丝团
                       ,pipi_fans = []                      %% 皮卡丘粉丝团
                       ,miaomiao_num = 0                    %% 喵喵人数
                       ,pipi_num = 0                        %% 皮卡丘人数
                       ,miaomiao_score = 0                  %% 喵喵粉丝团积分      
                       ,pipi_score = 0                      %% 皮卡丘粉丝团积分
                      }).

-record(state, {
                melee_master_server                         %% 主服务 跨服名
                ,melee_slave_server_list = []               %% 从服务   
                ,break_slave_server_list = []               %% 断开的从服务
                ,sign_battle_id = 1                         %% 报名战役ID
                ,sign_battle_seconds = 0                    %% 报名倒计时    (开始)
                ,fight_battle_id = 0                        %% 战斗战役ID
                ,fight_battle_seconds = 0                   %% 战斗倒计时    (结束)
                ,fight_battle_ground_num = 0                %% 战场个数
               }).


-define(DUMP_INTERVAL, 25000). 
%% 主节点3秒同步一次
-define(SYNC_MELEE_INTERVAL, 3000).

start() ->
    {ok, _} =
    supervisor:start_child(world_sup,
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []). 
   
i() ->
    gen_server:call(?MODULE, i).

clean_sign() ->
    gen_server:cast(?MODULE, clean_sign).
%% init/1
%% ====================================================================
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    %% 战场玩家
    process_flag(trap_exit, true),
    
    %% 战场规则
    MasterServerName = get_cross_server_name(),
    yes = global:register_name(MasterServerName, erlang:self()),
    
    %% 每天活动的开启关闭由主服务器控制（必须保证有且仅有一个战场处于报名状态）
    OpenTimeList = data_melee:get(battle_ground_open_time),
    
    Now = erlang:time(),
    Result = util:fun_find(fun({_BattleID, Open, _Close}) ->
                                if Now < Open ->
                                        true;
                                    true ->
                                        false
                                end
                           end,
                           OpenTimeList),
    case Result of
        {SignBattleID, SignOpenTime, _SignCloseTime} ->
            next;
        false ->
            [{SignBattleID, SignOpenTime, _SignCloseTime} | _] = OpenTimeList
    end,
    
    SignOpenSeconds = calendar:time_to_seconds(SignOpenTime),
    erlang:send_after(get_countdown2(SignOpenSeconds)*1000, self(), battle_open),
    
    case db_sql:get_etc(?DB_ETC_KEY_MELEE) of 
        [{state, #state{sign_battle_id = OldSignBattleID,
                        melee_slave_server_list = SlaveServerList}}| AllInfo] ->
            IsFristInit = false,
            case OldSignBattleID =:= SignBattleID of
                %% 同一个报名战场，加载报名信息
                true ->
                    lists:foreach(fun({Key,Value}) -> erlang:put(Key, Value) end, AllInfo);
                %% 非同一个，跳过
                false ->
                    next
            end;
        _ ->
            %% 第一次启动，需要同步节点
            IsFristInit = true,
            SlaveServerList = [],
            next
    end,
    
    clear_battle_ground_info(),
    
    State = #state{melee_master_server = MasterServerName,
                   melee_slave_server_list = SlaveServerList,
                   sign_battle_id = SignBattleID,
                   sign_battle_seconds = SignOpenSeconds},
    %% 启动也不需要做节点同步吧，每次战场开启报名前，同步一次节点，保证该节点所有玩家都是在同一跨服服务器
    case IsFristInit of
        true ->
            ?ERR("melee master server init at first time"),
            %% 节点第一次启动时，读取节点配置
            NewState = check_slave_server(State);
        false ->
            NewState = State
    end,
    
    %% 节点同步
    erlang:send_after(?SYNC_MELEE_INTERVAL, erlang:self(), sync_melee),
    %% dump数据
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    
    {ok, NewState}.


%% handle_call/3
%% ====================================================================
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
%% ====================================================================
%% handle_call({melee_fight, RoleID}, _From, #state{fight_battle_id = BattleID} = State) ->
%%     Result = select_melee_fighter(RoleID, BattleID),
%%     {reply, Result, State};
%% handle_call({melee_fight_result, RoleID, IsWin}, _From, State) ->
%%     Result = melee_fight_result(RoleID, IsWin),
%%     {reply, Result, State}; 
%% handle_call({get_melee_info, RoleID}, _From, State) ->
%%     MeleeInfo = get_role_melee_info(RoleID, State),
%%     {reply, MeleeInfo, State};
%% handle_call({melee_sign, RoleID}, _From, State) ->
%%     set_sign(RoleID),
%%     {reply, melee_sign_ack, State};
%% @warn FOR TEST
handle_call(i, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(clean_sign, State) ->
    clear_sign_list(),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w", [Exeption, Info, State]),
            {noreply, State}
    end.


%% terminate/2
%% ====================================================================
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_role_melee_info(RoleID, #state{sign_battle_seconds  = SignSeconds,
                                   fight_battle_seconds = FightSeconds,
%%                                    sign_battle_id       = SignBattleID,
                                   fight_battle_id      = FightBattleID}) ->
    %% 判断战场是否开始
    case FightBattleID == 0 of
        true ->
            %% 战场未开始
            case get_sign(RoleID) of
                ?undefined ->
                    %% 战场未开始，且没有报名
                    {melee_not_sign, get_countdown(SignSeconds)};
                _RoleID ->
                    %% 战场未开始，但是已经报名
                    {melee_signed, get_countdown(SignSeconds)}
            end;
        false ->
            %% 战场进行中    
            case get_role_fight(RoleID) of
                ?undefined ->
                    %% 战场进行中，但是没有报名
                    {melee_not_sign, get_countdown(SignSeconds)};
                #melee_fighter{groundID = GroundID, score = Score,
                               cur_win_times = CurWinTimes, camp = Camp} ->
                    %% 战场进行中，且已经报名
                    #ground_state{miaomiao_score = MiaoScore,
                                  pipi_score     = PiScore} = get_battle_ground(GroundID),
                    {melee_fighting, MiaoScore, PiScore, Score, CurWinTimes, Camp, get_countdown(FightSeconds)}
            end
    end.

melee_fight_result(RoleID, IsWin) ->
    case get_role_fight(RoleID) of
        ?undefined ->
            {ok, 0, 0, 0};
        #melee_fighter{groundID = GroundID, camp = Camp, cur_win_times = CurWinTimes} ->
            case IsWin of
                true ->
                    AddScore = data_melee:get(win_score),
                    add_fight_score(RoleID, AddScore),
                    
                    case get_battle_ground(GroundID) of
                        ?undefined ->
                            {ok, 0, 0, CurWinTimes};
                        #ground_state{miaomiao_score = MiaoScore,
                                      pipi_score = PiScore} = GroundState ->
                            case Camp of
                                ?camp_miaomiao ->
                                    NewMiaoScore = MiaoScore + AddScore,
                                    NewGroundState = GroundState#ground_state{miaomiao_score = NewMiaoScore},
                                    set_battle_ground(NewGroundState),
                                    {ok, NewMiaoScore, PiScore, CurWinTimes+1};
                                ?camp_pipi ->
                                    NewPiScore = PiScore + AddScore,
                                    NewGroundState = GroundState#ground_state{pipi_score = NewPiScore},
                                    set_battle_ground(NewGroundState),
                                    {ok, MiaoScore, NewPiScore, CurWinTimes+1}
                            end
                    end;
                false ->
                    add_fight_score(RoleID, 0),          
                    case get_battle_ground(GroundID) of
                        ?undefined ->
                            {ok, 0, 0, 0};
                        #ground_state{miaomiao_score = MiaoScore,
                                      pipi_score = PiScore} ->
                            {ok, MiaoScore, PiScore, 0}
                    end 
            end
    end.

%% 选择混战对手 {false, Result} | {true, TarRoleID}
-spec select_melee_fighter(RoleID :: integer(), BattleID :: integer()) ->
          {false, Result :: integer()} | {true, TarRoleID :: integer()}.
select_melee_fighter(_RoleID, 0) ->
    {false, 4};
select_melee_fighter(RoleID, _BattleID) ->
    case get_role_fight(RoleID) of
        ?undefined ->
            {false, 4};
        #melee_fighter{groundID = GroundID, camp = Camp} ->
            case get_battle_ground(GroundID) of
                ?undefined ->
                    {false, 4};
                #ground_state{} = GroundState ->
                    case catch select_from_queue(Camp, GroundState) of
                        TarRoleID when is_integer(TarRoleID) ->
                            {true, TarRoleID};
                        _ ->
                            {false, 5}
                    end
            end
    end.


select_from_queue(Camp, #ground_state{miaomiao_fans = MiaoFans, miaomiao_num = MiaoNum,
                                      pipi_fans = PiFans, pipi_num = PiNum}) ->
    case Camp of
        ?camp_miaomiao ->
            select_from_queue_1(PiFans, PiNum);
        ?camp_pipi ->
            select_from_queue_1(MiaoFans, MiaoNum)
    end.

select_from_queue_1(FanList, Length) ->
    Rand = random:uniform(Length),
    lists:nth(Rand, FanList).

%% 报名
set_sign(RoleID) ->
    erlang:put({?sign, RoleID}, RoleID).

get_sign(RoleID) ->
    erlang:get({?sign, RoleID}).

%% 获取所有报名的玩家
get_sign_list() ->
    All = erlang:get(),
    lists:filter(fun({{?sign, _RoleID}, _RoleID}) ->
                         true;
                    (_Other) ->
                         false
                 end, All).

%% 清理所有报名信息
clear_sign_list() ->
    lists:foreach(fun({{?sign, _RoleID} = Key, _Value}) ->
                          erlang:erase(Key);
                     (_) ->
                          next
                  end,
                  get_sign_list()).

%% 获取战场信息
-spec get_battle_ground(GroundID :: integer()) -> #ground_state{} | ?undefined.
get_battle_ground(GroundID) ->
    erlang:get({?battle_ground, GroundID}).

%% 设置战场信息
-spec set_battle_ground(#ground_state{}) -> ok.
set_battle_ground(#ground_state{ground_id = GroundID} = GroundState) ->
    erlang:put({?battle_ground, GroundID}, GroundState),
    ok.

%% 清理战场信息
clear_battle_ground() ->
    lists:foreach(fun({{?battle_ground, _GroundID} = Key, _GroundState}) ->
                          erlang:erase(Key);
                     (_) ->
                          next
                  end, erlang:get()).


%% 获取玩家的战场信息
-spec get_role_fight(RoleID :: integer()) -> #melee_fighter{} | ?undefined.
get_role_fight(RoleID) ->
    erlang:get({?role_fight, RoleID}).

%% 设置玩家的战场信息
-spec set_role_fight(#melee_fighter{}) -> ok.
set_role_fight(#melee_fighter{roleID = RoleID} = MeleeFighter) ->
    erlang:put({?role_fight, RoleID}, MeleeFighter),
    ok.

%% 获取某一战场的玩家
-spec get_ground_role_list(GroundID :: integer()) -> list().
get_ground_role_list(GroundID) ->
    lists:filter(fun({{?role_fight, _RoleID}, #melee_fighter{groundID = RoleGroundID}}) ->
                      GroundID =:= RoleGroundID
                 end, 
                 erlang:get()).

%% 清理战场玩家信息
clear_ground_role_list() ->
    lists:foreach(fun({{?role_fight, _RoleID} = Key, _MeleeFighter}) ->
                          erlang:erase(Key);
                     (_) ->
                          next
                  end, 
                  erlang:get()).


modify_fight_win_role(RoleID) ->
    case get_role_fight(RoleID) of
        ?undefined ->
            false;
        #melee_fighter{score = Score, cur_win_times = CurWinTimes, longest_win = LongestWin} = Fighter ->
            NewScore = date_melee:get(win_score) + Score,
            NewCurWinTimes = CurWinTimes + 1,
            if
                NewCurWinTimes > LongestWin ->
                    NewLongestWin = NewCurWinTimes;
                true ->
                    NewLongestWin = LongestWin
            end,
            NewFighter = Fighter#melee_fighter{score = NewScore,
                                               cur_win_times = NewCurWinTimes,
                                               longest_win = NewLongestWin},
            set_role_fight(NewFighter),
            {true, CurWinTimes}
    end.

modify_fight_lose_role(RoleID) ->
    case get_role_fight(RoleID) of
        ?undefined ->
            next;
        #melee_fighter{} = Fighter ->
            NewFighter = Fighter#melee_fighter{cur_win_times = 0},
            set_role_fight(NewFighter)
    end.

add_fight_score(RoleID, AddScore) when AddScore > 0 ->
    case get_role_fight(RoleID) of
        ?undefined ->
            next;
        #melee_fighter{score = Score, cur_win_times = CurWinTimes, longest_win = LongestWin} = MeleeFighter ->
            NewCurWinTimes = CurWinTimes + 1,
            if
                NewCurWinTimes > LongestWin ->
                    NewLongestWin = NewCurWinTimes;
                true ->
                    NewLongestWin = LongestWin
            end,
            NewMeleeFighter = MeleeFighter#melee_fighter{score = Score + AddScore,
                                                         cur_win_times = NewCurWinTimes,
                                                         longest_win = NewLongestWin},
            set_role_fight(NewMeleeFighter)
    end;
add_fight_score(RoleID, _AddScore) ->
    case get_role_fight(RoleID) of
        ?undefined ->
            next;
        #melee_fighter{} = MeleeFighter ->
            NewMeleeFighter = MeleeFighter#melee_fighter{cur_win_times = 0},
            set_role_fight(NewMeleeFighter)
    end.


do_handle_info({sync, From, {melee_fight, RoleID}}, 
               #state{fight_battle_id = BattleID} = State) ->
    Result = select_melee_fighter(RoleID, BattleID),
    send_msg_to_role_pid(From, Result),
    {noreply, State};
do_handle_info({sync, From, {melee_fight_result, RoleID, IsWin}}, State) ->
    Result = melee_fight_result(RoleID, IsWin),
    send_msg_to_role_pid(From, Result),
    {noreply, State};
do_handle_info({sync, From, {get_melee_info, RoleID}}, State) ->
    MeleeInfo = get_role_melee_info(RoleID, State),
    send_msg_to_role_pid(From, MeleeInfo),
    {noreply, State};
do_handle_info({sync, From, {melee_sign, RoleID}}, State) ->
	case get_sign(RoleID) of
		?undefined ->
    		set_sign(RoleID),
			send_msg_to_role_pid(From, {melee_sign_ack, 0});
		_ ->
			send_msg_to_role_pid(From, {melee_sign_ack, 1})
	end,
    {noreply, State};

do_handle_info(sync_melee, State) ->
    erlang:send_after(?SYNC_MELEE_INTERVAL, erlang:self(), sync_melee),
    NewState = sync_connected_slave_server(State),
    {noreply, NewState};
do_handle_info(dump_data, State) ->
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    do_persist(State), 
    {noreply, State};
do_handle_info(battle_open, #state{} = State) ->
    NewState = check_slave_server(State),
    NewState1 = do_battle_open(NewState),
    {noreply, NewState1};
do_handle_info(battle_close, #state{} = State) ->
    NewState = do_battle_close(State),
    {noreply, NewState};
do_handle_info(battle_reward, State) ->
    NewState = do_battle_reward(State),
    erlang:garbage_collect(),
    {noreply, NewState};
%% Only For Test
do_handle_info({sync, From, {melee_start, Time}}, State) ->
        %% 计算战场
    BattleGround = split_battle_ground(),
    %% 分战场
    NewState = open_battle_ground(BattleGround, State),
    %% 清理旧的报名信息
    clear_sign_list(),
    
    NowSeconds = calendar:time_to_seconds(erlang:time()),
    NewState1 = NewState#state{fight_battle_id =1,
                           fight_battle_seconds = NowSeconds + Time},
    erlang:send_after(Time*1000, self(), battle_close),
    send_msg_to_role_pid(From, ok),
    {noreply, NewState1};
do_handle_info(_info, State) ->
    {noreply, State}.

do_battle_open(State) ->
    %% 计算战场
    BattleGround = split_battle_ground(),
    
    %% 先把报名信息从进程字典中清除，减少进程字典数据
    clear_sign_list(),
    
    %% 分战场
    NewState = open_battle_ground(BattleGround, State),
    
    %% 新报名战场
    NewState1 = new_battle_open(NewState),
    
    NewState1.

do_battle_close(#state{fight_battle_id = BattleID} = State) when BattleID == 0 ->
    State#state{fight_battle_id = 0, fight_battle_seconds = 0};
do_battle_close(#state{} = State) ->
    %% 关闭战场， 确定奖励
    RewardTime = data_melee:get(delay_seconds)*1000,
    erlang:send_after(RewardTime, self(), battle_reward),
    State#state{fight_battle_id = 0, fight_battle_seconds = 0}.

do_battle_reward(#state{fight_battle_ground_num = GroundNum} = State) ->
    %% 发奖励
    RewardFun = fun(GroundID) ->
                        case get_battle_ground(GroundID) of
                            ?undefined -> 
                                next;
                            #ground_state{miaomiao_fans = MiaoFans, pipi_fans = PiFans,
                                          miaomiao_score = MiaoScore, pipi_score = PiScore} = GroundState ->
                                WinCamp = if
                                              MiaoScore > PiScore ->
                                                  ?camp_miaomiao;
                                              MiaoScore < PiScore ->
                                                  ?camp_pipi;
                                              true ->
                                                  case erlang:length(MiaoFans) < erlang:length(PiFans) of
                                                      true ->
                                                          ?camp_miaomiao;
                                                      false ->
                                                          ?camp_pipi
                                                  end
                                          end,
                                
                                do_battle_reward_1(WinCamp, GroundState)
                        end
                end,
                                    
    lists:foreach(RewardFun, lists:seq(1, GroundNum)),

    %% 清理战场
    clear_battle_ground_info(),
    State#state{fight_battle_ground_num = 0}.

do_battle_reward_1(WinCamp, #ground_state{miaomiao_fans = MiaoFans, pipi_fans = PiFans}) ->
    case WinCamp of
        ?camp_miaomiao ->
            send_battle_reward(MiaoFans, PiFans);
        ?camp_pipi ->
           send_battle_reward(PiFans, MiaoFans)
    end.

send_battle_reward(WinFans, LoseFans) ->
    WinRewards = data_melee:get(win_rank_reward_list),
    send_battle_reward_1(WinRewards, WinFans, ?MAIL_MELEE_RANK_WIN_REWARD),
    LoseRewards = data_melee:get(lose_rank_reward_list),
    send_battle_reward_1(LoseRewards, LoseFans, ?MAIL_MELEE_RANK_LOSE_REWARD).

send_battle_reward_1(Rewards, Roles, MailRankTempID) ->
    SortedRoles = sort_reward_roles(Roles),
    LongWinReward = data_melee:get(longest_win_reward),
    
    lists:foldr(fun(RoleID, Rank) -> 
                        ServerID = util:role_id_to_server_id(RoleID),
                        #melee_fighter{score = Score, longest_win = LongestWin} = get_role_fight(RoleID),
                        
                        %% 阵营排名奖励
                        case get_reward_by_rank(Rank, Rewards) of
                            {_RankMin,_RankMax, RankReward} ->
                                send_msg_to_slave_server_id(ServerID,{send_mail_reward, RoleID, RankReward, MailRankTempID, [Rank, Score]});
                            false ->
                                next 
                        end, 
                        
                        %% 连杀奖励
                        case find_longest_win_reward(LongestWin, LongWinReward) of
                            {_Times, Reward} ->
                                send_msg_to_slave_server_id(ServerID,
                                                            {send_mail_reward, RoleID, Reward, ?MAIL_MELEE_LONGEST_WIN_REWARD, [LongestWin]});
                            false ->
                                next
                        end,
                        Rank+1
                end,
                1,
                SortedRoles),
    ok.

find_longest_win_reward(LongestWin, Rewards) ->
    util:fun_find(fun({WinTimes, _Reward}) ->
                          if LongestWin >= WinTimes ->
                                 true;
                             true ->
                                 false
                          end
                  end, Rewards).

sort_reward_roles(Roles) ->
    [RoleID || 
     #melee_fighter{roleID = RoleID} <- lists:keysort(#melee_fighter.score, 
                                                      [get_role_fight(RoleID) || RoleID <- Roles])].                         
   
get_reward_by_rank(Rank, Rewards) ->
    util:fun_find(fun({Min, Max, _Reward}) 
                       when Min =< Rank andalso Rank =< Max ->
                          true;
                     (_) ->
                          false
                  end,
                  Rewards).
    
new_battle_open(#state{sign_battle_id = OldSignBattleId} = State) ->
    [{FirstBattleID, FirstBattleOpenTime, _} | _T] = BattleList = data_melee:get(battle_ground_open_time),
    case lists:keyfind(OldSignBattleId+1, 1, BattleList) of
        {NextBattleID, NextBattleOpenTime, _} ->
            next;
        false ->
            NextBattleID = FirstBattleID,
            NextBattleOpenTime = FirstBattleOpenTime
    end,
    
    DeadLine = calendar:time_to_seconds(NextBattleOpenTime),
    Seconds = get_countdown2(DeadLine),
    erlang:send_after(Seconds*1000, self(), battle_open),
    State#state{sign_battle_id = NextBattleID,
                sign_battle_seconds = DeadLine}.

split_battle_ground() ->
    SignRoleList = get_sign_list(),
    Len = erlang:length(SignRoleList),
    util:random_list(SignRoleList),
    MaxGroundNum = data_melee:get(max_battle_ground_role_num),
    GroundCount = Len div (MaxGroundNum + 1) + 1,

    {_, BattleGroundRole} = lists:foldl(fun({{?sign, RoleID}, RoleID}, {AccRoleCount, AccGround}) ->
                                                NthGround = (AccRoleCount rem GroundCount) + 1,
                                                Ground = erlang:element(NthGround, AccGround),
                                                {AccRoleCount + 1, erlang:setelement(NthGround, AccGround, [RoleID | Ground])}
                                        end, 
                                        {0, erlang:make_tuple(GroundCount, [])},
                                        SignRoleList),
    BattleGroundRole.

open_battle_ground(BattleGround, #state{sign_battle_id = SignBattleID} = State) ->
    BattleGroundNum = erlang:size(BattleGround),
    lists:foreach(fun(GroundID) ->
                          GroundRoles = erlang:element(GroundID, BattleGround),
                          {MiaoFans, PiFans} = split_fans_ground(GroundID, GroundRoles),
                          GroundState = #ground_state{ground_id = GroundID,
                                                      miaomiao_fans = MiaoFans,
                                                      miaomiao_num = erlang:length(MiaoFans),
                                                      pipi_fans = PiFans,
                                                      pipi_num = erlang:length(PiFans)
                                                     },
                          set_battle_ground(GroundState)
                  end,
                  lists:seq(1, BattleGroundNum)),
    
    BattleList = data_melee:get(battle_ground_open_time),
    {_, _, BattleCloseTime} = lists:keyfind(SignBattleID, 1, BattleList),
    
    DeadSeconds = calendar:time_to_seconds(BattleCloseTime),
    CountDown = get_countdown2(DeadSeconds),
    
    %% 战场关闭
    erlang:send_after(CountDown * 1000, self(), battle_close),
    
    State#state{fight_battle_id = SignBattleID,
                fight_battle_seconds = DeadSeconds,
                fight_battle_ground_num = BattleGroundNum}.
    

split_fans_ground(GroundID, RoleList) ->
    %% 
    RoleNum = erlang:length(RoleList),
    SplitPos = RoleNum div 2,
    
    {MiaoFans, PiFans} = Fans = lists:split(SplitPos, util:random_list2(RoleList)),
    
    lists:foreach(fun(RoleID) ->
                          MeleeFighter = #melee_fighter{roleID = RoleID,
                                                        groundID = GroundID,
                                                        camp = ?camp_miaomiao},
                          set_role_fight(MeleeFighter)
                  end,
                  MiaoFans),
    
    lists:foreach(fun(RoleID) ->
                          MeleeFighter = #melee_fighter{roleID = RoleID,
                                                        groundID = GroundID,
                                                        camp = ?camp_pipi},
                          set_role_fight(MeleeFighter)
                  end,
                  PiFans),
    
    Fans.

%% 清理战场信息
clear_battle_ground_info() ->
    clear_battle_ground(),
    clear_ground_role_list().

%% 获取倒计时
get_countdown(DeadLine) ->
    Now = calendar:time_to_seconds(erlang:time()),
    Diff = DeadLine - Now,
    if Diff > 0 ->
           Diff + util:now();
       true ->
           Diff + ?ONE_DAY_SECONDS + util:now()
    end.

get_countdown2(DeadLine) ->
    Now = calendar:time_to_seconds(erlang:time()),
    Diff = DeadLine - Now,
    if Diff > 0 ->
           Diff;
       true ->
           Diff + ?ONE_DAY_SECONDS
    end.

send_msg_to_slave(SlaveServerList, Msg) ->
    lists:foreach(fun(SlaveServerName) ->
                          case global:whereis_name(SlaveServerName) of
                              ?undefined ->
                                  ?ERR("SlaveServerName:~w is undefined, Msg:~w", [SlaveServerName, Msg]);
                              Pid when erlang:is_pid(Pid) ->
                                  global:send(SlaveServerName, Msg)
                          end
                  end, SlaveServerList).

send_msg_to_slave_server_id(ServerID, Msg) ->
    Platform = data_setting:get(platform),
    SlaveServerName = get_cross_server_name(ServerID, false, Platform),
    case global:whereis_name(SlaveServerName) of
        ?undefined ->
            ?ERR("send_msg_to_slave_sever_id error, SlaveServerName:~w is undefined.~nMsg:~w", [SlaveServerName, Msg]);
        Pid when erlang:is_pid(Pid) ->
            global:send(SlaveServerName, Msg)
    end.

send_msg_to_role_pid(From, Msg) ->
    erlang:send(From, {melee_msg_return, Msg}).

get_server_id() ->
    data_setting:get(server_id).

get_cross_server_name() ->
    ServerID = get_server_id(),
    IsCrossMaster = data_setting:get(is_cross_master),
    Platform = data_setting:get(platform),
    get_cross_server_name(ServerID, IsCrossMaster, Platform).

get_cross_server_name(ServerID, IsCrossMaster, Platform) ->
    case IsCrossMaster of
        true ->
            erlang:list_to_atom(lists:concat(['cross_master_melee_', Platform, ServerID]));
        false ->
            erlang:list_to_atom(lists:concat(['cross_slave_melee_', Platform, ServerID]))
    end.

get_master_node() ->
    MasterID = data_setting:get(server_id),
    MasterIP = data_setting:get(cross_master_ip),
    Platform = data_setting:get(platform),
    get_master_node(MasterIP, MasterID, Platform).

get_master_node(MasterIP, MasterID, Platform) ->
    NodeNameList = lists:concat([Platform, '_master_', MasterID, '@', MasterIP]),
    erlang:list_to_atom(NodeNameList).

get_slave_server_list() ->
    SlaveServerList = data_setting:get(cross_slave_server_list),
    Platform = data_setting:get(platform),
    lists:map(fun({SlaveServerID, _SlaveServerIP}) ->
                      get_cross_server_name(SlaveServerID, false, Platform)
              end, SlaveServerList).

%% 只在新战役开始的时候检查一次
%%      1.保证同一节点的所有玩家都能在同一个跨服服务器。节点不同时，可能会造成玩家报名两次，且两边都会有奖励，对其它节点的玩家不公平。
%%      2.主服务器重启不检查就是为了避免上诉情况
check_slave_server(#state{melee_master_server = MasterServerName,
                          break_slave_server_list = OldBreakSlaveServerList,
                          melee_slave_server_list = OldSlaveServerList} = State) ->
    %% 重新读取从服务器列表，告知从服务器 MasterServer
    ConfigSlaveServerList = get_slave_server_list(),
    
    %% 删掉的从服务器, 通知断开与主服务器的连接
    lists:foreach(fun(SlaveServer) ->
                          catch global:send(SlaveServer, {disconnect_with_master_server, MasterServerName})
                  end,
                  OldSlaveServerList -- ConfigSlaveServerList),
    
    BreakSlaveServerList = 
        lists:foldl(fun(SlaveServer,AccBreak) ->
                            case global:whereis_name(SlaveServer) of
                                ?undefined ->
                                    case lists:member(SlaveServer, OldSlaveServerList) of
                                        false ->
                                            %% 从主服务器配置中删除
                                            ?ERR("添加了新的从服务器 ~w, 但是连接失败", [SlaveServer]);
                                        true ->
                                            %% 从服务器不存在
                                            ?ERR("已经配置的从服务器：~w, 连接失败", [SlaveServer])
                                    end,
                                    [SlaveServer | AccBreak];
                                _Pid ->
                                    case lists:member(SlaveServer, OldSlaveServerList) of
                                        true ->
                                            case lists:member(SlaveServer, OldBreakSlaveServerList) of
                                                true ->
                                                    ?ERR("重连从服务器 ~w 成功", [SlaveServer]);
                                                false ->
                                                    %% 无变化
                                                    ok
                                            end;
                                        false ->
                                            %% 新添加的从服务器
                                            ?ERR("添加了新的从服务器 ~w, 连接成功", [SlaveServer])
                                    end,
                                    global:send(SlaveServer, 
                                                        {sync_melee_master_server, MasterServerName}),
                                    AccBreak
                            end
                    end,
                    [],
                    ConfigSlaveServerList),
    State#state{melee_slave_server_list = ConfigSlaveServerList,
                break_slave_server_list = BreakSlaveServerList}.

%% 同步主节点到配置的从节点中   
sync_connected_slave_server(#state{melee_master_server = MasterServerName,
                                   melee_slave_server_list = SlaveServerList} = State) ->
    BreakSlaves = lists:foldl(fun(SlaveServer, Breaks) ->
                                      case global:whereis_name(SlaveServer) of
                                          ?undefined ->
                                              [SlaveServer | Breaks];
                                          _Pid ->
                                              global:send(SlaveServer, 
                                                          {sync_melee_master_server, MasterServerName}),
                                              Breaks
                                      end
                              end, [], SlaveServerList),
    State#state{break_slave_server_list = BreakSlaves}.

do_persist(State) ->
    Info = [E || E<-erlang:get(), is_persist(E) ],
    Info2 = [{state, State} | Info],
    db_sql:set_etc(?DB_ETC_KEY_MELEE,Info2).

%% 只保存报名的信息
is_persist({{?sign, _}, _}) ->
    true;
is_persist(_) ->
    false.


