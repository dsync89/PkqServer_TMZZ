-module(alien_master_server).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_alien.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

start() ->
    {ok,_}=
    supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {count=0,session_id=0,status=0, next_status_left_seconds,master_node, master_server,
                slave_node_list=[], slave_server_list=[],
                is_all_connect=false, noticed_server_list=[],
                sign_list=[], guessOddNum=0, guessEvenNum=0,
                killNumRankList=[],killConRankList=[],fightRecordList=[],group_num_list=[]}).

-define(DUMP_INTERVAL, (1000 * 60 * 10)).
-define(CHECK_NODE_CONNECTION_INTERVAL, 1000).

-define(role_alien_info, role_alien_info).
-define(rank_to_role_id, rank_to_role_id).
-define(fight_lock, fight_lock).
-define(replayRecord,replayRecord).

-define(FIRST_FIVE_RANK_LIST, [1,2,3,4,5]).
-define(FIRST_SIX_RANK_LIST, [1,2,3,4,5,6]).

-define(KILL_NUM_RANK_MAX, data_alien:get(kill_num_rank_max)).
-define(KILL_CON_RANK_MAX, data_alien:get(kill_con_rank_max)).

-define(KILL_RANK_NEED_NUM, data_alien:get(kill_rank_need_num)).
-define(KILL_CON_RANK_NEED_NUM, data_alien:get(kill_con_rank_need_num)).
-define(KILL_CON_STOP_NUM, data_alien:get(kill_con_stop_num)).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
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
    process_flag(trap_exit,true),
    erlang:set_cookie(erlang:node(), data_setting:get(cookie)),
    case db_sql:get_etc(?DB_ETC_KEY_ALIEN) of
        [{state,OldState}|AllInfo] ->
            lists:foreach(fun({Key,Value}) -> erlang:put(Key, Value) end, AllInfo);
        _ ->
            OldState = #state{}
    end,
    SlaveNodeList = get_slave_node_list(),
    SlaveServerList = get_slave_server_list(),
    MasterNode = get_master_node(),
    MasterServerName = get_cross_server_name(get_server_id(), true, data_setting:get(platform)),
    yes = global:register_name(MasterServerName, erlang:self()),
    State = OldState#state{count=0,slave_node_list=SlaveNodeList,slave_server_list=SlaveServerList,
                           master_node=MasterNode, master_server=MasterServerName,noticed_server_list=[]},
    erlang:send(erlang:self(), check_node_connection),
    %% 何时dump数据由主服务器控制
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    NewState = init_state(State),
    {ok, NewState}.

init_state(#state{session_id=SessionID}=State) ->
    case SessionID =:= 0 of
        true ->
            StartTime = data_alien:get(start_time),
            {Date, Time} = erlang:localtime(),
            Now = util:now(),
            case Time < StartTime of
                true ->
                    NextTimestamp = util:datetime_to_seconds({Date, StartTime});
                false ->
                    NextTimestamp = util:datetime_to_seconds({Date, StartTime}) + ?ONE_DAY_SECONDS
            end,
            LeftSeconds = NextTimestamp - Now,
            State#state{next_status_left_seconds=LeftSeconds};
        false ->
            State
    end.

calc_left_seconds(NextStatusLeftSeconds) ->
    StartTime = data_alien:get(start_time),
    {Date, Time} = erlang:localtime(),
    Now = util:now(),
    case Time =< StartTime of
        true ->
            NextTimestamp = util:datetime_to_seconds({Date, StartTime});
        false ->
            NextTimestamp = util:datetime_to_seconds({Date, StartTime}) + ?ONE_DAY_SECONDS
    end,
    NewNextStatusLeftSeconds =
        case NextTimestamp - Now >= 1 of
            true ->
                NextTimestamp - Now;
            false ->
                1
        end,
    case NewNextStatusLeftSeconds =< NextStatusLeftSeconds of
        true ->
            NewNextStatusLeftSeconds;
        false ->
            1
    end.



%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
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
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().
%% ====================================================================
terminate(_Reason, State) ->
    do_persist(State),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
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
do_handle_info({do_alien_fight, RoleID, TarRoleID, TarTank, NeedTimes, NeedGold, ServerID}, State) ->
    do_alien_fight(RoleID, TarRoleID, TarTank, NeedTimes, NeedGold, ServerID, State),
    {noreply, State};
do_handle_info({fight_over, RoleID, ServerID, TarRoleID, IsWin, FightRecord, NewAtkFighterList,NewDefFighterList,NeedTimes, NeedGold}, State) ->
    {ok, NewState} = do_fight_over_succ(RoleID, ServerID, TarRoleID, IsWin, FightRecord, NewAtkFighterList,NewDefFighterList, NeedTimes, NeedGold, State),
    {noreply, NewState};
do_handle_info({fight_over, RoleID, ServerID, TarRoleID}, State) ->
    do_fight_over_error(RoleID, ServerID, TarRoleID),
    {noreply, State};
do_handle_info({get_alien_fight_replay, RoleID, ReplayUID, ServerID}, State) ->
    {FightRecord, _} = get_replay_record(ReplayUID),
    send_msg_to_slave_sever_id(ServerID, {get_alien_fight_replay_return, RoleID, FightRecord}),
    {noreply, State};
do_handle_info({get_alien_info, RoleID, AlienTimes, ResetTime, ServerID}, State) ->
    get_alien_info(RoleID, AlienTimes, ResetTime, ServerID, State),
    {noreply, State};
do_handle_info({get_alien_first_five, RoleID, ServerID}, State) ->
    get_alien_first_five(RoleID, ServerID),
    {noreply, State};
do_handle_info({alien_view_other, RoleID, TarRoleID, ServerID}, State) ->
    do_alien_view_other(RoleID, TarRoleID, ServerID),
    {noreply, State};
do_handle_info({alien_view_other_dtl, RoleID, TarRoleID, ServerID}, State) ->
    do_alien_view_other_dtl(RoleID, TarRoleID, ServerID),
    {noreply, State};
do_handle_info({get_alien_kill_con_rank, RoleID, Start, Num, ServerID}, #state{killConRankList=KillConRankListT}=State) ->
    case get_role_group_id(RoleID) of
        0 ->
            GroupID = 1;
        GroupID ->
            next
    end,
    case lists:keyfind(GroupID, 1, KillConRankListT) of
        false ->
            KillConRankList = [];
        {_, KillConRankList} ->
            next
    end,
    get_alien_kill_con_rank(RoleID, Start + 1, Num, ServerID, KillConRankList),
    {noreply, State};
do_handle_info({get_alien_kill_num_rank, RoleID, Start, Num, ServerID}, #state{killNumRankList=KillNumRankListT}=State) ->
    case get_role_group_id(RoleID) of
        0 ->
            GroupID = 1;
        GroupID ->
            next
    end,
    case lists:keyfind(GroupID, 1, KillNumRankListT) of
        false ->
            KillNumRankList = [];
        {_, KillNumRankList} ->
            next
    end,
    get_alien_kill_num_rank(RoleID, Start + 1, Num, ServerID, KillNumRankList),
    {noreply, State};
do_handle_info({get_alien_record, RoleID, Start, Num, ServerID}, #state{fightRecordList=FightRecordListT}=State) ->
    case get_role_group_id(RoleID) of
        0 ->
            GroupID = 1;
        GroupID ->
            next
    end,
    case lists:keyfind(GroupID, 1, FightRecordListT) of
        false ->
            FightRecordList = [];
        {_, FightRecordList} ->
            next
    end,
    get_alien_record(RoleID, Start + 1, Num, ServerID, FightRecordList),
    {noreply, State};
do_handle_info({get_alien_guess_info, RoleID, ServerID}, State) ->
    get_alien_guess_info(RoleID, ServerID, State),
    {noreply, State};
do_handle_info({do_alien_guess, RoleID, RoleInfo, GuessCoin, GuessType, ServerID}, State) ->
    {ok, NewState} = do_alien_guess(RoleID, RoleInfo, GuessCoin, GuessType, ServerID, State),
    {noreply, NewState};
do_handle_info({do_alien_sign, RoleID, RoleInfo, FighterList, ItemList, ServerID}, State) ->
    do_alien_sign(RoleID, RoleInfo, FighterList, ItemList, ServerID, State);
do_handle_info({do_alien_reset, RoleID, RoleInfo, FighterList, ItemList, ServerID}, State) ->
    do_alien_reset(RoleID, RoleInfo, FighterList, ItemList, ServerID, State),
    {noreply, State};
do_handle_info(do_sign, #state{session_id=SessionID}=State) ->
    ?ERR("开始报名", []),
    erlang:erase(),
    erlang:spawn(fun() -> db_sql:del_spec_type_replay(?REPLAY_TYPE_ALIEN) end),
    NewState = State#state{session_id=SessionID+1,sign_list=[], guessOddNum=0, guessEvenNum=0,
                           killNumRankList=[],killConRankList=[],fightRecordList=[],group_num_list=[]},
    send_msg_to_slave(NewState#state.slave_server_list, do_sign),
    {noreply, NewState};
do_handle_info(do_fight, State) ->
    ?ERR("开始比赛~~~~~~~~~~", []),
    {ok, NewState} = do_fight(State),
    send_msg_to_slave(NewState#state.slave_server_list, do_fight),
    erlang:send_after(1000, erlang:self(), update_fighter_list),
    {noreply, NewState};
do_handle_info(do_close, State) ->
    ?ERR("关闭跨服战", []),
    erlang:send_after(data_alien:get(delay_seconds) * 1000, erlang:self(), send_reward),
    send_msg_to_slave(State#state.slave_server_list, do_close),
    {noreply, State};
do_handle_info(send_reward, State) ->
    send_reward(State),
    {noreply, State};
do_handle_info(update_fighter_list, #state{status=?STATUS_FIGHT,slave_server_list=SlaveServerList}=State) ->
    erlang:send_after(1000, erlang:self(), update_fighter_list),
    update_fighter_list(SlaveServerList),
    {noreply, State};
do_handle_info(update_fighter_list, State) ->
    {noreply, State};
do_handle_info(check_node_connection, State) ->
    erlang:send_after(?CHECK_NODE_CONNECTION_INTERVAL, erlang:self(), check_node_connection),
    case data_setting:get(is_need_connect) of
        true ->
            NewState = check_connection(State),
            erlang:spawn(fun() -> update_status_to_slave(NewState) end);
        _ ->
            NewState = State#state{is_all_connect=false}
    end,
    {noreply, NewState};
do_handle_info(dump_data, #state{count=Count}=State) ->
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    do_persist(State),
    send_msg_to_slave(State#state.slave_server_list, dump_data),
    case Count rem 6 of
        0 ->
            erase_replay_record();
        _ ->
            next
    end,
    {noreply, State#state{count=Count+1}};
do_handle_info(change_to_next_status, State) ->
    NewState = change_to_next_status(State),
    {noreply, NewState};
do_handle_info(update_server_list, State) ->
    do_update_server_list(State);
do_handle_info(Info, State) ->
    ?ERR("unkown info:~w", [Info]),
    {noreply, State}.

do_persist(State) ->
    Info = [E || E<-erlang:get(), is_persist(E) ],
    Info2 = [{state,State} | Info],
    db_sql:set_etc(?DB_ETC_KEY_ALIEN,Info2).

is_persist({{?role_alien_info, _}, _}) ->
    true;
is_persist({{?rank_to_role_id, _, _}, _}) ->
    true;
is_persist(_) ->
    false.


%%--------------------------------------------------------------------------------------------------------------------------
recalc_fighter_list(FighterList, FighterList2) ->
    lists:foldr(fun(#ger{gerID=GerID}=Ger, Acc) ->
                        case lists:keyfind(GerID, #ger.gerID, FighterList2) of
                            false ->
                                [Ger|Acc];
                            #ger{gerHp=GerHp} ->
                                [Ger#ger{gerHp=GerHp}|Acc]
                        end
                end, [], FighterList).

do_fight_over_succ(RoleID, ServerID, TarRoleID, IsWin, FightRecord, AtkFighterListT,DefFighterListT, NeedTimes, NeedGold, State) ->
    #role_alien{roleName=AtkName,rank=RoleRank,killNum=RoleKillNum,
                killContinuousNum=RoleKillContinuousNum,
                maxKillContinuousNum=RoleMaxKillContinuousNum,
                group_id=GroupID,fighterList=RoleFighterList} = get_role_alien(RoleID),
    #role_alien{roleName=DefName,rank=TarRoleRank,killNum=TarRoleKillNum,
                killContinuousNum=TarRoleKillContinuousNum,
                maxKillContinuousNum=TarRoleMaxKillContinuousNum,
                group_id=GroupID, serverID=TarServerID,fighterList=TarRoleFighterList} = get_role_alien(TarRoleID),
    AtkFighterList = recalc_fighter_list(RoleFighterList, AtkFighterListT),
    DefFighterList = recalc_fighter_list(TarRoleFighterList, DefFighterListT),
    #state{group_num_list=GroupNumList} = State,
    {_, GroupNum} = lists:keyfind(GroupID, 1, GroupNumList),
    case IsWin of
        true ->
            NewRoleFighterList = AtkFighterList,
            NewRoleHPPercent = calc_hp_percent(AtkFighterList),
            NewRoleKillNum = RoleKillNum + 1,
            NewRoleKillContinuousNum = RoleKillContinuousNum + 1,
            NewRoleMaxKillContinuousNum =
                case NewRoleKillContinuousNum > RoleMaxKillContinuousNum of
                    true ->
                        NewRoleKillContinuousNum;
                    false ->
                        RoleMaxKillContinuousNum
                end,
            NewRoleIsInContinuous =
                case NewRoleKillContinuousNum >= 2 andalso NewRoleKillContinuousNum > RoleMaxKillContinuousNum of
                    true ->
                        true;
                    false ->
                        false
                end,
            AddCoin = data_alien:get(win_coin),
            
            NewTarRoleFighterList = full_hp(DefFighterList),
            NewTarRoleHPPercent = 100,
            NewTarRoleKillNum = TarRoleKillNum,
            NewTarRoleKillContinuousNum = 0,
            NewTarRoleMaxKillContinuousNum = TarRoleMaxKillContinuousNum,
            NewTarRoleIsInContinuous = false,
            
            
            case RoleRank > TarRoleRank of
                true ->
                    refresh_rank(RoleRank, TarRoleRank, RoleRank, GroupID, GroupNum);
                false ->
                    refresh_rank(0, 0, TarRoleRank, GroupID, GroupNum)
            end;
        false ->
            NewRoleFighterList = full_hp(AtkFighterList),
            NewRoleHPPercent = 100,
            NewRoleKillNum = RoleKillNum,
            NewRoleKillContinuousNum = 0,
            NewRoleMaxKillContinuousNum = RoleMaxKillContinuousNum,
            NewRoleIsInContinuous = false,
            AddCoin = data_alien:get(lose_coin),
            
            NewTarRoleFighterList = DefFighterList,
            NewTarRoleHPPercent = calc_hp_percent(DefFighterList),
            NewTarRoleKillNum = TarRoleKillNum + 1,
            NewTarRoleKillContinuousNum = TarRoleKillContinuousNum + 1,
            NewTarRoleMaxKillContinuousNum =
                case NewTarRoleKillContinuousNum > TarRoleMaxKillContinuousNum of
                    true ->
                        NewTarRoleKillContinuousNum;
                    false ->
                        TarRoleMaxKillContinuousNum
                end,
            NewTarRoleIsInContinuous =
                case NewTarRoleKillContinuousNum >= 2 andalso NewTarRoleKillContinuousNum > TarRoleMaxKillContinuousNum of
                    true ->
                        true;
                    false ->
                        false
                end,
            
            
            refresh_rank(0, 0, RoleRank, GroupID, GroupNum)
    end,
    update_role_alien(RoleID, NewRoleFighterList, NewRoleHPPercent, NewRoleKillNum,
                NewRoleKillContinuousNum, NewRoleMaxKillContinuousNum, NewRoleIsInContinuous),
    update_role_alien(TarRoleID, NewTarRoleFighterList, NewTarRoleHPPercent, NewTarRoleKillNum,
                NewTarRoleKillContinuousNum, NewTarRoleMaxKillContinuousNum, NewTarRoleIsInContinuous),
    
    send_msg_to_slave_sever_id(ServerID, {do_alien_fight_succ_return, RoleID, IsWin, DefName, FightRecord, NeedTimes,
                                          NeedGold, AddCoin,
                                          get_role_rank(RoleID), get_fighter_list(RoleID, true)}),
    send_msg_to_slave_sever_id(TarServerID, {be_fighted, TarRoleID, not IsWin, AtkName, get_role_rank(TarRoleID), FightRecord}),
    
    unlock(RoleID),
    unlock(TarRoleID),
    
    {ok, NewState} = update_state(RoleID, NewRoleKillNum,
                                  NewRoleKillContinuousNum,
                                  TarRoleID, NewTarRoleKillNum,
                                  NewTarRoleKillContinuousNum,
                                  RoleKillContinuousNum,
                                  RoleMaxKillContinuousNum,
                                  TarRoleKillContinuousNum,
                                  TarRoleMaxKillContinuousNum,
                                  IsWin, State, FightRecord,GroupID),
    
    {ok, NewState}.

update_state(RoleID, NewRoleKillNum,
             NewRoleKillContinuousNum,
             TarRoleID, NewTarRoleKillNum,
             NewTarRoleKillContinuousNum,
             RoleKillContinuousNum,
             RoleMaxKillContinuousNum,
             TarRoleKillContinuousNum,
             TarRoleMaxKillContinuousNum,
             IsWin, #state{killNumRankList=KillNumRankList,killConRankList=KillConRankList,fightRecordList=RecordList}=State, FightRecord,GroupID) ->
    case IsWin of
        true ->
            ?DEBUG("NewRoleKillContinuousNum:~w,RoleMaxKillContinuousNum:~w", [NewRoleKillContinuousNum,RoleMaxKillContinuousNum]),
            case NewRoleKillContinuousNum > ?KILL_CON_RANK_NEED_NUM andalso NewRoleKillContinuousNum > RoleMaxKillContinuousNum of
                true ->
                    NewKillConRankList = new_kill_con_rank_list(RoleID, NewRoleKillContinuousNum, KillConRankList,GroupID);
                false ->
                    NewKillConRankList = refresh_kill_con_rank_list(KillConRankList,GroupID)
            end,
            ?DEBUG("NewRoleKillNum:~w", [NewRoleKillNum]),
            case NewRoleKillNum > ?KILL_RANK_NEED_NUM of
                true ->
                    NewKillNumRankList = new_kill_num_rank_list(RoleID, NewRoleKillNum, KillNumRankList,GroupID);
                false ->
                    NewKillNumRankList = KillNumRankList
            end,
            Bool1 = lists:member(NewRoleKillContinuousNum, data_alien:get(kill_con_num_list)),
            Bool2 = TarRoleKillContinuousNum >= ?KILL_CON_STOP_NUM andalso TarRoleKillContinuousNum =:= TarRoleMaxKillContinuousNum,
            case Bool1 orelse Bool2 of
                true ->
                    NewRecordList = new_record_list(RoleID, NewRoleKillNum, NewRoleKillContinuousNum,
                                                    TarRoleID, TarRoleMaxKillContinuousNum,
                                                    RecordList, Bool1, FightRecord,GroupID);
                false ->
                    NewRecordList = RecordList
            end;
        false ->
            ?DEBUG("NewTarRoleKillContinuousNum:~w,TarRoleMaxKillContinuousNum:~w", [NewTarRoleKillContinuousNum,TarRoleMaxKillContinuousNum]),
            case NewTarRoleKillContinuousNum > ?KILL_CON_RANK_NEED_NUM andalso NewTarRoleKillContinuousNum > TarRoleMaxKillContinuousNum of
                true ->
                    NewKillConRankList = new_kill_con_rank_list(TarRoleID, NewTarRoleKillContinuousNum, KillConRankList,GroupID);
                false ->
                    NewKillConRankList = refresh_kill_con_rank_list(KillConRankList,GroupID)
            end,
            ?DEBUG("NewTarRoleKillNum:~w", [NewTarRoleKillNum]),
            case NewTarRoleKillNum > ?KILL_RANK_NEED_NUM of
                true ->
                    NewKillNumRankList = new_kill_num_rank_list(TarRoleID, NewTarRoleKillNum, KillNumRankList,GroupID);
                false ->
                    NewKillNumRankList = KillNumRankList
            end,
            Bool1 = lists:member(NewTarRoleKillContinuousNum, data_alien:get(kill_con_num_list)),
            Bool2 = RoleKillContinuousNum >= ?KILL_CON_STOP_NUM andalso RoleKillContinuousNum =:= RoleMaxKillContinuousNum,
            case Bool1 orelse Bool2 of
                true ->
                    NewRecordList = new_record_list(TarRoleID, NewTarRoleKillNum, NewTarRoleKillContinuousNum,
                                                    RoleID, RoleMaxKillContinuousNum,
                                                    RecordList, Bool1, FightRecord,GroupID);
                false ->
                    NewRecordList = RecordList
            end
    end,
    {ok, State#state{killNumRankList=NewKillNumRankList,killConRankList=NewKillConRankList,fightRecordList=NewRecordList}}.

new_record_list(RoleID, NewRoleKillNum, NewRoleKillContinuousNum,
                TarRoleID, TarRoleMaxKillContinuousNum,
                RecordListT, Bool, FightRecord,GroupID) ->
    case lists:keyfind(GroupID, 1, RecordListT) of
        false ->
            RecordList = [];
        {_, RecordList} ->
            next
    end,
    ReplayUID = tk_id:gen_replayUID(?REPLAY_TYPE_ALIEN),
    erlang:spawn(fun() -> db_sql:set_fightReplay(ReplayUID, FightRecord, ?REPLAY_TYPE_ALIEN) end),
    #role_alien{roleName=AtkName} = get_role_alien(RoleID),
    #role_alien{roleName=DefName} = get_role_alien(TarRoleID),
    NewRecord = 
        case Bool of
            true ->
                #p_alien_record{type=0,atkName=AtkName,defName=DefName,continuousCount=NewRoleKillContinuousNum,
                                killCount=NewRoleKillNum,replayUID=ReplayUID,timestamp=util:now()};
            false ->
                #p_alien_record{type=1,atkName=AtkName,defName=DefName,continuousCount=TarRoleMaxKillContinuousNum,
                                killCount=NewRoleKillNum,replayUID=ReplayUID,timestamp=util:now()}
        end,
    ?DEBUG("NewRecord:~w", [NewRecord]),
    NewRecordList = [NewRecord|RecordList],
    case erlang:length(NewRecordList) =<  data_alien:get(record_max_num) of
        true ->
            NewRecordList2 = NewRecordList;
        false ->
            #p_alien_record{replayUID=LastReplayUID} = LastRecord = lists:last(NewRecordList),
            erlang:spawn(fun() -> db_sql:del_fightReplay(LastReplayUID, ?REPLAY_TYPE_ALIEN) end),
            NewRecordList2 = lists:delete(LastRecord, NewRecordList)
    end,
    case lists:keyfind(GroupID, 1, RecordListT) of
        false ->
            [{GroupID, NewRecordList2}|RecordListT];
        _ ->
            lists:keyreplace(GroupID, 1, RecordListT, {GroupID, NewRecordList2})
    end.

new_kill_num_rank_list(RoleID, NewRoleKillNum, KillNumRankListT,GroupID) ->
    case lists:keyfind(GroupID, 1, KillNumRankListT) of
        false ->
            KillNumRankList = [];
        {_, KillNumRankList} ->
            next
    end,
    #role_alien{fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,serverID=ServerID} = get_role_alien(RoleID),
    NewKillNum = #alien_fighter2{roleID=RoleID ,
                                 fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,serverID=ServerID,
                                 killNum=NewRoleKillNum ,timestamp=util:now()},
    ?DEBUG("NewKillNum:~w", [NewKillNum]),
    case lists:keyfind(RoleID, #alien_fighter2.roleID, KillNumRankList) of
        false ->
            KillNumRankList2 = [NewKillNum|KillNumRankList];
        _ ->
            KillNumRankList2 = lists:keyreplace(RoleID, #alien_fighter2.roleID, KillNumRankList, NewKillNum)
    end,
    KillNumRankList3 = lists:sort(fun(#alien_fighter2{killNum=K1,timestamp=T1},
                                      #alien_fighter2{killNum=K2,timestamp=T2}) ->
                                          if
                                              K1 > K2 ->
                                                  true;
                                              K1 =:= K2 ->
                                                  if
                                                      T1 < T2 ->
                                                          true;
                                                      true ->
                                                          false
                                                  end;
                                              true ->
                                                  false
                                          end
                                  end, KillNumRankList2),
    {KillNumRankList4, _} = 
        lists:foldr(fun(Elem, {Acc, AccRank}) ->
                            {[Elem#alien_fighter2{rank=AccRank}|Acc], AccRank - 1}
                    end, {[], erlang:length(KillNumRankList3)}, KillNumRankList3),
    case lists:keyfind(GroupID, 1, KillNumRankListT) of
        false ->
            [{GroupID, lists:sublist(KillNumRankList4, ?KILL_NUM_RANK_MAX)}|KillNumRankListT];
        _ ->
            lists:keyreplace(GroupID, 1, KillNumRankListT, {GroupID, lists:sublist(KillNumRankList4, ?KILL_NUM_RANK_MAX)})
    end.

refresh_kill_con_rank_list(KillConRankListT, GroupID) ->
    case lists:keyfind(GroupID, 1, KillConRankListT) of
        false ->
            KillConRankList = [];
        {_, KillConRankList} ->
            next
    end,
    
    KillConRankList2 =
        lists:map(
          fun(#alien_fighter3{roleID=ID}=Elem) ->
                  #role_alien{isInContinuous=Bool} = get_role_alien(ID),
                  Elem#alien_fighter3{isInContinuous=Bool}
          end, KillConRankList),

    case lists:keyfind(GroupID, 1, KillConRankListT) of
        false ->
            [{GroupID,lists:sublist(KillConRankList2, ?KILL_CON_RANK_MAX)}|KillConRankListT];
        _ ->
            lists:keyreplace(GroupID, 1, KillConRankListT, {GroupID, lists:sublist(KillConRankList2, ?KILL_CON_RANK_MAX)})
    end.
    

new_kill_con_rank_list(RoleID, NewRoleKillContinuousNum, KillConRankListT,GroupID) ->
    case lists:keyfind(GroupID, 1, KillConRankListT) of
        false ->
            KillConRankList = [];
        {_, KillConRankList} ->
            next
    end,
    #role_alien{fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,serverID=ServerID} = get_role_alien(RoleID),
    NewKillCon = #alien_fighter3{roleID=RoleID,
                                 fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,serverID=ServerID,
                                 killContinuousNum=NewRoleKillContinuousNum, timestamp=util:now()},
    ?DEBUG("NewKillCon:~w", [NewKillCon]),
    case lists:keyfind(RoleID, #alien_fighter3.roleID, KillConRankList) of
        false ->
            KillConRankList2 = [NewKillCon|KillConRankList];
        #alien_fighter3{killContinuousNum=OldRoleKillContinuousNum} ->
            case NewRoleKillContinuousNum > OldRoleKillContinuousNum of
                true ->
                    KillConRankList2 = lists:keyreplace(RoleID, #alien_fighter3.roleID, KillConRankList, NewKillCon);
                false ->
                    KillConRankList2 = KillConRankList
            end
    end,
    KillConRankList3 = lists:sort(fun(#alien_fighter3{killContinuousNum=K1,timestamp=T1},
                                      #alien_fighter3{killContinuousNum=K2,timestamp=T2}) ->
                                          if
                                              K1 > K2 ->
                                                  true;
                                              K1 =:= K2 ->
                                                  if
                                                      T1 < T2 ->
                                                          true;
                                                      true ->
                                                          false
                                                  end;
                                              true ->
                                                  false
                                          end
                                  end, KillConRankList2),
    {KillConRankList4, _} =
        lists:foldr(fun(#alien_fighter3{roleID=ID}=Elem, {Acc, AccRank}) ->
                            #role_alien{isInContinuous=Bool} = get_role_alien(ID),
                            {[Elem#alien_fighter3{rank=AccRank, isInContinuous=Bool}|Acc], AccRank - 1}       
                    end, {[], erlang:length(KillConRankList3)}, KillConRankList3),
    case lists:keyfind(GroupID, 1, KillConRankListT) of
        false ->
            [{GroupID,lists:sublist(KillConRankList4, ?KILL_CON_RANK_MAX)}|KillConRankListT];
        _ ->
            lists:keyreplace(GroupID, 1, KillConRankListT, {GroupID, lists:sublist(KillConRankList4, ?KILL_CON_RANK_MAX)})
    end.

update_role_alien(RoleID, NewRoleFighterList, NewRoleHPPercent, NewRoleKillNum,
                NewRoleKillContinuousNum, NewRoleMaxKillContinuousNum, NewRoleIsInContinuous) ->
    RoleAlien = get_role_alien(RoleID),
    NewRoleFighterList2 =
        lists:map(
          fun(#ger{gerBase=GerBase}=Ger) ->
                  NewGerBase = GerBase#gerBase{gerPos=erlang:abs(GerBase#gerBase.gerPos)},
                  Ger#ger{gerBase=NewGerBase}
          end, NewRoleFighterList),
    set_role_alien(RoleAlien#role_alien{fighterList=NewRoleFighterList2,hpPercent=NewRoleHPPercent, killNum=NewRoleKillNum,
                                        killContinuousNum=NewRoleKillContinuousNum,
                                        maxKillContinuousNum=NewRoleMaxKillContinuousNum,
                                        isInContinuous=NewRoleIsInContinuous,
                                        canBeAtkTime=util:now() + data_alien:get(cool_down)}).

calc_hp_percent(FighterList) ->
    {SumHP, SumHPMax} = lists:foldr(fun(#ger{gerHp=HP, gerAttr=#gerAttr{gerHpMax=HPMax}}, {AccHP, AccHPMax}) ->
                             {AccHP + HP, AccHPMax + HPMax}
                     end, {0, 0}, FighterList),
    erlang:trunc(SumHP / SumHPMax * 100).

full_hp(FighterList) ->
    lists:map(fun(#ger{gerAttr=#gerAttr{gerHpMax=HPMax}}=Ger) ->
                      Ger#ger{gerHp=HPMax}
              end, FighterList).

refresh_rank(OldRoleRank, NewRoleRank, DropRank, GroupID, GroupNum) ->
    ?DEBUG("OldRoleRank:~w, NewRoleRank:~w, DropRank:~w, GroupID:~w, GroupNum:~w", [OldRoleRank, NewRoleRank, DropRank, GroupID, GroupNum]),
    case OldRoleRank > NewRoleRank of
        true ->
            change_rank(OldRoleRank, NewRoleRank,GroupID);
        false ->
            next
    end,
    drop_rank(DropRank, GroupID, GroupNum).

change_rank(LowRank, HighRank,GroupID) ->
    ?DEBUG("LowRank:~w, HighRank:~w,GroupID:~w", [LowRank, HighRank,GroupID]),
    LowAlien = get_role_alien_by_rank(LowRank, GroupID),
    HighAlien = get_role_alien_by_rank(HighRank, GroupID),
    ?DEBUG("LowAlien:~w, HighAlien:~w", [LowAlien#role_alien.roleID, HighAlien#role_alien.roleID]),
    set_role_alien(LowAlien#role_alien{rank=HighRank}),
    set_role_rank_to_role_id(HighRank, GroupID, LowAlien#role_alien.roleID),
    set_role_alien(HighAlien#role_alien{rank=LowRank}),
    set_role_rank_to_role_id(LowRank, GroupID, HighAlien#role_alien.roleID).

drop_rank(DropRank, GroupID, GroupNum) ->
    DropAlien = get_role_alien_by_rank(DropRank, GroupID),
    ?DEBUG("DropRank:~w, GroupID:~w, DropAlien:~w", [DropRank, GroupID, DropAlien#role_alien.roleID]),
    case DropRank < GroupNum of
        true ->
            UpAlienList = lists:map(fun(UpRank) ->
                                            get_role_alien_by_rank(UpRank, GroupID)
                                    end, lists:seq(DropRank + 1, GroupNum));
        false ->
            UpAlienList = []
    end,
    set_role_alien(DropAlien#role_alien{rank=GroupNum}),
    set_role_rank_to_role_id(GroupNum, GroupID, DropAlien#role_alien.roleID),
    lists:foreach(fun(#role_alien{rank=UpRank}=UpAlien) ->
                          set_role_alien(UpAlien#role_alien{rank=UpRank-1}),
                          set_role_rank_to_role_id(UpRank-1,GroupID,UpAlien#role_alien.roleID)
                  end, UpAlienList).

do_fight_over_error(RoleID, ServerID, TarRoleID) ->
    unlock(RoleID),
    unlock(TarRoleID),
    send_msg_to_slave_sever_id(ServerID, {do_alien_fight_error_return, RoleID, 12, []}).

do_alien_fight(RoleID, TarRoleID, TarTank, NeedTimes, NeedGold, ServerID, State) ->
    case catch check_alien_fight(RoleID, TarRoleID, TarTank, State) of
        {ok, RoleAlien, TarRoleAlien} ->
            ?DEBUG("RoleID:~w, TarRoleID:~w", [RoleID, TarRoleID]),
            do_alien_fight2(RoleID, ServerID, TarRoleID, RoleAlien, TarRoleAlien, NeedTimes, NeedGold);
        {false, Reason} ->
            case Reason of
                2 ->
                    NewFighterList = get_fighter_list(RoleID, true);
                _ ->
                    NewFighterList = []
            end,
            send_msg_to_slave_sever_id(ServerID, {do_alien_fight_error_return, RoleID, Reason, NewFighterList})
    end.

do_alien_fight2(RoleID, ServerID, TarRoleID, RoleAlien, TarRoleAlien, NeedTimes, NeedGold) ->
    AlienMasterServer = erlang:self(),
    #role_alien{fighterList=AttackerList} = RoleAlien,
    #role_alien{fighterList=DefenderList} = TarRoleAlien,
    lock(RoleID),
    lock(TarRoleID),
    erlang:spawn(fun() ->
                         case catch role_fight:new(filter_out_zero_hp(AttackerList), filter_out_zero_hp(DefenderList), {0,0}, {0,0}) of
                             {IsWin, FightRecord, {_,_,NewAtkFighterList,NewDefFighterList}} ->
                                 ?DEBUG("IsWin:~w,RoleID:~w,TarRoleID:~w", [IsWin,RoleID,TarRoleID]),
                                 erlang:send(AlienMasterServer, {fight_over, RoleID, ServerID, TarRoleID, IsWin, FightRecord, NewAtkFighterList,NewDefFighterList, NeedTimes, NeedGold});
                             {'EXIT', Err} ->
                                 ?ERR("Err:~w", [Err]),
                                 erlang:send(AlienMasterServer, {fight_over, RoleID, ServerID, TarRoleID})
                         end
                 end).

filter_out_zero_hp(List) ->
    lists:filter(fun(#ger{gerHp=GerHP}) ->
                         GerHP > 0
                 end, List).

check_alien_fight(RoleID, TarRoleID, TarTank, #state{status=Status, sign_list=SignList}) ->
    case Status of
        ?STATUS_FIGHT ->
            next;
        _ ->
            erlang:throw({false, 11})
    end,
    case lists:member(RoleID, SignList) of
        true ->
            next;
        false ->
            erlang:throw({false, 13})
    end,
    #role_alien{group_id=RoleGroupID,rank=RoleRank,canBeAtkTime=RoleCanBeAtkTime} = RoleAlien = get_role_alien(RoleID),
    TarRoleAlien = get_role_alien(TarRoleID),
    case TarRoleAlien of
        ?undefined ->
            erlang:throw({false, 8});
        #role_alien{group_id=TarRoleGroupID,rank=TarRank2} ->
            case RoleGroupID =:= TarRoleGroupID of
                true ->
                    case TarRank2 =:= TarTank of
                        true ->
                            case erlang:abs(RoleRank - TarRank2) =< 5 of
                                true ->
                                    next;
                                false ->
                                    erlang:throw({false, 15})
                            end;
                        false ->
                            erlang:throw({false, 2})
                    end;
                false ->
                    erlang:throw({false, 14})
            end
    end,
    Now = util:now(),
    case RoleCanBeAtkTime =< Now of
        true ->
            case TarRoleAlien#role_alien.canBeAtkTime =< Now of
                true ->
                    next;
                false ->
                    erlang:throw({false, 17})
            end;
        false ->
            erlang:throw({false, 16})
    end,
    case is_lock(RoleID) of
        false ->
            next;
        true ->
%%             next
            erlang:throw({false, 4})
    end,
    case is_lock(TarRoleID) of
        false ->
            next;
        true ->
%%             next
            erlang:throw({false, 5})
    end,
    {ok, RoleAlien, TarRoleAlien}.

get_alien_info(RoleID, AlienTimes, ResetTime, ServerID, State) ->
    #state{status=Status,sign_list=SignList, next_status_left_seconds=LeftSeconds} = State,
    case erlang:is_integer(LeftSeconds) of
        true ->
            EndTimestamp = util:now() + LeftSeconds;
        false ->
            EndTimestamp = 0
    end,
    IsSign = lists:member(RoleID, SignList),
    Msg =
        case Status of
            ?STATUS_SIGN ->
                {?STATUS_SIGN, IsSign, RoleID, EndTimestamp};
            ?STATUS_FIGHT ->
                {?STATUS_FIGHT, IsSign, get_role_group_id(RoleID), get_fighter_list(RoleID, IsSign), RoleID, EndTimestamp, AlienTimes, ResetTime};
            ?STATUS_CLOSE ->
                {?STATUS_CLOSE, get_role_group_id(RoleID), get_fighter_list(RoleID, IsSign), RoleID, EndTimestamp, AlienTimes}
        end,
    send_msg_to_slave_sever_id(ServerID, {get_alien_info_return, Msg}).

get_alien_first_five(RoleID, ServerID) ->
    send_msg_to_slave_sever_id(ServerID, {get_alien_info_return, RoleID, get_fighter_list(RoleID, false)}).

do_alien_view_other(RoleID, TarRoleID, ServerID) ->
    case get_role_alien(TarRoleID) of
        #role_alien{is_sign=true} = TarAlien ->
            case get_role_alien(RoleID) of
                #role_alien{is_sign=true} = RoleAlien ->
                    do_alien_view_other(RoleID, TarRoleID, ServerID ,RoleAlien, TarAlien);
                _ ->
                    do_alien_view_other(RoleID, TarRoleID, ServerID, ?undefined, TarAlien)
            end;
        _ ->
            next
    end.

do_alien_view_other_dtl(RoleID, TarRoleID, ServerID) ->
    case get_role_alien(TarRoleID) of
        #role_alien{is_sign=true} = TarAlien ->
            do_alien_view_other_dtl(RoleID, TarRoleID, ServerID, ?undefined, TarAlien);
        _ ->
            next
    end.

do_alien_view_other(RoleID, TarRoleID, ServerID, ?undefined, TarAlien) ->
    #role_alien{fightPower=FightPower,level=Level,roleName=RoleName,fighterList=FighterList} = TarAlien,
    GerViewList = [ger_lib:ger2p_ger_view(E)||E<-FighterList],
    Record = #sc_alien_view_other{tarRoleID1=0,roleName1= <<"">>,roleLevel1=0,fightPower1=0,gerList1=[],
                                  tarRoleID2=TarRoleID,roleName2=RoleName,roleLevel2=Level,fightPower2=FightPower,gerList2=GerViewList},
    send_msg_to_slave_sever_id(ServerID, {do_alien_view_other_return, RoleID, Record});
do_alien_view_other(RoleID, TarRoleID, ServerID ,RoleAlien, TarAlien) ->
    
    #role_alien{fightPower=FightPower1,level=Level1,roleName=RoleName1,fighterList=FighterList1} = RoleAlien,
    GerViewList1 = [ger_lib:ger2p_ger_view(E)||E<-FighterList1],
    
    #role_alien{fightPower=FightPower2,level=Level2,roleName=RoleName2,fighterList=FighterList2} = TarAlien,
    GerViewList2 = [ger_lib:ger2p_ger_view(E)||E<-FighterList2],
    
    Record = #sc_alien_view_other{tarRoleID1=RoleID,roleName1=RoleName1,roleLevel1=Level1,fightPower1=FightPower1,gerList1=GerViewList1,
                                  tarRoleID2=TarRoleID,roleName2=RoleName2,roleLevel2=Level2,fightPower2=FightPower2,gerList2=GerViewList2},
    
    send_msg_to_slave_sever_id(ServerID, {do_alien_view_other_return, RoleID, Record}).

do_alien_view_other_dtl(RoleID, TarRoleID, ServerID, ?undefined, TarAlien) ->
    #role_alien{fightPower=FightPower,level=Level,roleName=RoleName,fighterList=FighterListT,itemList=ItemList} = TarAlien,
    FighterList = ger_attr:refresh_other_fightPower(FighterListT, 0, 0),
    GerViewList = [ger_lib:ger2p_ger_view_dtl(E)||E<-FighterList],
    GerPosList = [ger_lib:ger2p_ger_pos(E)||E<-FighterList],
    EquipViewList = [item_lib:item2p_item_view_dtl(E)||E<-ItemList],
    Record = 
        #sc_ger_view_other_dtl{tarRoleID=TarRoleID,roleName=RoleName,roleLevel=Level,fightPower=FightPower, gerList=GerViewList
                               ,equipList=EquipViewList,gerPosList=GerPosList,atkAdd=0, hpAdd=0,lieuViewList=[]},
    send_msg_to_slave_sever_id(ServerID, {do_alien_view_other_dtl_return, RoleID, Record}).

get_alien_kill_con_rank(RoleID, Start, Num, ServerID, KillConRankList) ->
    case Start =< erlang:length(KillConRankList) of
        true ->
            List = lists:sublist(KillConRankList, Start, Num),
            send_msg_to_slave_sever_id(ServerID, {get_alien_kill_con_rank_return, RoleID, List});
        false ->
            send_msg_to_slave_sever_id(ServerID, {get_alien_kill_con_rank_return, RoleID, []})
    end.

get_alien_kill_num_rank(RoleID, Start, Num, ServerID, KillNumRankList) ->
    case Start =< erlang:length(KillNumRankList) of
        true ->
            List = lists:sublist(KillNumRankList, Start, Num),
            send_msg_to_slave_sever_id(ServerID, {get_alien_kill_num_rank_return, RoleID, List});
        false ->
            send_msg_to_slave_sever_id(ServerID, {get_alien_kill_num_rank_return, RoleID, []})
    end.

get_alien_record(RoleID, Start, Num, ServerID, FightRecordList) ->
    case Start =< erlang:length(FightRecordList) of
        true ->
            List = lists:sublist(FightRecordList, Start, Num),
            send_msg_to_slave_sever_id(ServerID, {get_alien_record_return, RoleID, List});
        false ->
            send_msg_to_slave_sever_id(ServerID, {get_alien_record_return, RoleID, []})
    end.

get_alien_guess_info(RoleID, ServerID, #state{guessOddNum=GuessOddNum,guessEvenNum=GuessEvenNum}) ->
    {Type, Coin} = get_role_guess_type_and_coin(RoleID),
    send_msg_to_slave_sever_id(ServerID, {get_alien_guess_info_return, RoleID, Type, Coin, GuessOddNum, GuessEvenNum}).

do_alien_guess(RoleID, RoleInfo, GuessCoin, GuessType, ServerID,
               #state{status=Status,next_status_left_seconds=LeftSeconds,
                      guessOddNum=GuessOddNum,guessEvenNum=GuessEvenNum}=State) ->
    case Status of
        ?STATUS_FIGHT ->
            case LeftSeconds >= ?ONE_HOUR_SECONDS of
                true ->
                    case get_role_alien(RoleID) of
                        ?undefined ->
                            OldGuessCoin = 0,
                            #role{isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName} = RoleInfo,
                            RoleAlien = #role_alien{roleID=RoleID,is_sign=true, group_id=0, fighterList=[],
                                                    fightPower=0,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,
                                                    rank = 0, serverID = ServerID, hpPercent=0, killNum=0, killContinuousNum=0,
                                                    isInContinuous=false, guessCoin=0, guessType=false};
                        #role_alien{guessCoin=OldGuessCoin} = RoleAlien ->
                            next
                    end,
                    case OldGuessCoin =:= 0  of
                        true ->
                            set_role_alien(RoleAlien#role_alien{guessCoin=GuessCoin, guessType=GuessType}),
                            send_msg_to_slave_sever_id(ServerID, {do_alien_guess_return, RoleID, 0, GuessCoin}),
                            case GuessType of
                                false ->
                                    NewGuessOddNum = GuessOddNum + 1,
                                    NewGuessEvenNum = GuessEvenNum;
                                true ->
                                    NewGuessOddNum = GuessOddNum,
                                    NewGuessEvenNum = GuessEvenNum + 1
                            end,
                            {ok, State#state{guessOddNum=NewGuessOddNum,guessEvenNum=NewGuessEvenNum}};
                        false ->
                            send_msg_to_slave_sever_id(ServerID, {do_alien_guess_return, RoleID, 5}),
                            {ok, State}
                    end;
                false ->
                    send_msg_to_slave_sever_id(ServerID, {do_alien_guess_return, RoleID, 4}),
                    {ok, State}
            end;
        _ ->
            send_msg_to_slave_sever_id(ServerID, {do_alien_guess_return, RoleID, 4}),
            {ok, State}
    end.

do_alien_sign(RoleID, RoleInfo, FighterList, ItemList, ServerID, #state{status=Status, sign_list=SignList}=State) ->
    case Status of
        ?STATUS_SIGN ->
            case lists:member(RoleID, SignList) of
                false ->
                    ?ERR("RoleID:~w sign succ", [RoleID]),
                    #role{fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName} = RoleInfo,
                    set_role_alien(#role_alien{roleID=RoleID,is_sign=true, group_id=0, fighterList=FighterList,itemList=ItemList,
                                               fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,
                                               rank = 0, serverID = ServerID, hpPercent=100, killNum=0, killContinuousNum=0,
                                               isInContinuous=false, guessCoin=0, guessType=false}),
                    send_msg_to_slave_sever_id(ServerID, {do_alien_sign_return, RoleID, 0}),
                    {noreply, State#state{sign_list=[RoleID|SignList]}};
                true ->
                    send_msg_to_slave_sever_id(ServerID, {do_alien_sign_return, RoleID, 4}),
                    {noreply, State}
            end;
        _ ->
            send_msg_to_slave_sever_id(ServerID, {do_alien_sign_return, RoleID, 3}),
            {noreply, State}
    end.

do_alien_reset(RoleID, RoleInfo, FighterList, ItemList, ServerID, #state{status=Status,sign_list=SignList}) ->
    case Status of
        ?STATUS_FIGHT ->
            case lists:member(RoleID, SignList) of
                true ->
                    #role{fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName} = RoleInfo,
                    RoleAlien = get_role_alien(RoleID),
                    NewRoleAlien = RoleAlien#role_alien{fighterList=FighterList,itemList=ItemList,fightPower=FightPower,isMale=IsMale,
                                                        title=Title,head=Head,level=Level,roleName=RoleName,hpPercent=100},
                    set_role_alien(NewRoleAlien),
                    NewFighterList = get_fighter_list(RoleID, true),
                    send_msg_to_slave_sever_id(ServerID, {do_alien_reset_return, RoleID, 0, NewFighterList});
                false ->
                    send_msg_to_slave_sever_id(ServerID, {do_alien_reset_return, RoleID, 3})
            end;
        _ ->
            send_msg_to_slave_sever_id(ServerID, {do_alien_reset_return, RoleID, 2})
    end.

do_fight(State) ->
    #state{sign_list=SignList} = State,
    Num = erlang:length(SignList),
    ?ERR("Num:~w, SignList:~w", [Num, SignList]),
    GroupNum = erlang:trunc(Num / data_alien:get(calc_arg)) + 1,
    EveryNum = Num div GroupNum,
    {GroupList, GroupNumList} = gen_group_list(GroupNum, util:random_list(SignList), [], [], EveryNum),
    ?ERR("GroupList:~w, GroupNumList:~w", [GroupList, GroupNumList]),
    lists:foreach(fun({GroupID, RoleIDList}) ->
                          init_group(GroupID, RoleIDList)
                  end, GroupList),
    {ok, State#state{group_num_list=GroupNumList}}.

init_group(GroupID, RoleIDList) ->
    RoleAlienList = lists:map(fun(RoleID) ->
                                      get_role_alien(RoleID)
                              end, RoleIDList),
    lists:foldr(fun(RoleAlien, Rank) ->
                        set_role_alien(RoleAlien#role_alien{rank=Rank,group_id=GroupID}),
                        set_role_rank_to_role_id(Rank, GroupID, RoleAlien#role_alien.roleID),
                        Rank - 1
                end, erlang:length(RoleAlienList), util:random_list(RoleAlienList)).

gen_group_list(1, SignList, GroupList, GroupNumList, _EveryNum) ->
    {[{1, SignList}|GroupList], [{1,erlang:length(SignList)}|GroupNumList]};
gen_group_list(GroupNum, SignList, GroupList, GroupNumList, EveryNum) ->
    NewList = lists:sublist(SignList, EveryNum),
    gen_group_list(GroupNum - 1, SignList -- NewList, [{GroupNum,NewList}|GroupList], [{GroupNum,EveryNum}|GroupNumList], EveryNum).

send_reward(#state{killNumRankList=KillNumRankListT,killConRankList=KillConRankListT}) ->
    ?ERR("KillNumRankListT:~w", [KillNumRankListT]),
    ?ERR("KillConRankListT:~w", [KillConRankListT]),
    lists:foreach(fun({GroupID, KillNumRankList}) ->
                        send_reward_kill_num(GroupID, KillNumRankList)  
                  end, KillNumRankListT),
    lists:foreach(fun({GroupID, KillConRankList}) ->
                          send_reward_kill_con(GroupID, KillConRankList)
                  end, KillConRankListT),
    lists:foreach(fun({GroupID, NormalRankList}) ->
                          send_reward_normal_rank(GroupID, NormalRankList)
                  end, get_all_rank_data()),
    lists:foreach(fun({GroupIDT, GuessList}) ->
                          case GroupIDT of
                              0 ->
                                  GroupID = 1;
                              GroupID ->
                                  next
                          end,
                          case lists:keyfind(GroupID, 1, KillConRankListT) of
                              false ->
                                  Bool = false;
                              {_, [#alien_fighter3{killContinuousNum=Val}|_]} ->
                                  case Val rem 2 of
                                      1 ->
                                          Bool = false;
                                      0 ->
                                          Bool = true
                                  end
                          end,
                          send_reward_guess(GuessList, Bool)
                  end, get_all_guess_data()).

send_reward_guess(GuessList, Bool) ->
    lists:foreach(fun(#role_alien{roleID=RoleID,guessCoin=GuessCoin,guessType=GuessType,serverID=ServerID}) ->
                          case GuessType =:= Bool of
                              true ->
                                  send_msg_to_slave_sever_id(ServerID,{send_mail_reward, RoleID,
                                                                       #sell_reward{coin=erlang:trunc(GuessCoin*data_alien:get(guess_right)),
                                                                                    roleExp=0,gerExp=0,gold=0,item=[],reputation=0,newGer=[]}, 1062, []});
                              false ->
                                  send_msg_to_slave_sever_id(ServerID,{send_mail_reward, RoleID,
                                                                       #sell_reward{coin=erlang:trunc(GuessCoin*data_alien:get(guess_error)),
                                                                                    roleExp=0,gerExp=0,gold=0,item=[],reputation=0,newGer=[]}, 1063, []})
                          end
                  end, GuessList).

send_reward_kill_num(_GroupID, KillNumRankList) ->
    lists:foreach(fun({Min, Max, SellReward}) ->
                          send_reward_kill_num2(Min, Max, SellReward, KillNumRankList)
                  end, data_alien:get(kill_num_rank_reward_list)).

send_reward_kill_num2(Min, Max, SellReward, KillNumRankList) ->
    KillNumRankList2 =
        lists:filter(fun(#alien_fighter2{rank=Rank}) ->
                             Rank >= Min andalso Rank =< Max
                     end, KillNumRankList),
    lists:foreach(fun(#alien_fighter2{roleID=RoleID,rank=Rank,serverID=ServerID}) ->
                          send_msg_to_slave_sever_id(ServerID,{send_mail_reward, RoleID, SellReward, 1060, [Rank]})
                  end, KillNumRankList2).
    

send_reward_kill_con(_GroupID, KillConRankList) ->
    lists:foreach(fun({Min, Max, SellReward}) ->
                          send_reward_kill_con2(Min, Max, SellReward, KillConRankList)
                  end, data_alien:get(kill_con_rank_reward_list)).

send_reward_kill_con2(Min, Max, SellReward, KillConRankList) ->
    KillConRankList2 =
        lists:filter(fun(#alien_fighter3{rank=Rank}) ->
                             Rank >= Min andalso Rank =< Max
                     end, KillConRankList),
    lists:foreach(fun(#alien_fighter3{roleID=RoleID,rank=Rank,serverID=ServerID}) ->
                          send_msg_to_slave_sever_id(ServerID,{send_mail_reward, RoleID, SellReward, 1061, [Rank]})
                  end, KillConRankList2).

send_reward_normal_rank(_GroupID, NormalRankList) ->
    lists:foreach(fun({Min, Max, SellReward}) ->
                          send_reward_normal_rank2(Min, Max, SellReward, NormalRankList)
                  end, data_alien:get(normal_rank_reward_list)).

send_reward_normal_rank2(Min, Max, SellReward, NormalRankList) ->
    NormalRankList2 =
        lists:filter(fun(#role_alien{rank=Rank}) ->
                             Rank >= Min andalso Rank =< Max
                     end, NormalRankList),
    lists:foreach(fun(#role_alien{roleID=RoleID,rank=Rank,serverID=ServerID}) ->
                          send_msg_to_slave_sever_id(ServerID,{send_mail_reward, RoleID, SellReward, 1059, [Rank]})
                  end, NormalRankList2).

%% -------------------------------------------------------------------------------------------------------------------------
is_lock(RoleID) ->
    case erlang:get({?fight_lock, RoleID}) of
        ?undefined ->
            false;
        _ ->
            true
    end.

lock(RoleID) ->
    erlang:put({?fight_lock, RoleID}, ?fight_lock).

unlock(RoleID) ->
    erlang:erase({?fight_lock, RoleID}).

get_fighter_list(RoleID, false) ->
    case get_role_group_id(RoleID) of
        0 ->
            GroupID = 1;
        GroupID ->
            next
    end,
    lists:foldr(fun(RoleRank, Acc) ->
                      case get_role_alien_by_rank(RoleRank, GroupID) of
                          ?undefined ->
                              Acc;
                          RoleAlien ->
                              [RoleAlien|Acc]
                      end
              end, [], ?FIRST_FIVE_RANK_LIST);
get_fighter_list(RoleID, true) ->
    Rank = get_role_rank(RoleID),
    case get_role_group_id(RoleID) of
        0 ->
            GroupID = 1;
        GroupID ->
            next
    end,
    RankList = get_rank_list(Rank),
    lists:foldr(fun(RoleRank, Acc) ->
                      case get_role_alien_by_rank(RoleRank,GroupID) of
                          ?undefined ->
                              Acc;
                          RoleAlien ->
                              [RoleAlien|Acc]
                      end
              end, [], RankList).

get_rank_list(Rank) when Rank > 5 ->
    lists:seq(Rank - 5, Rank);
get_rank_list(Rank) when Rank =< 5 ->
    ?FIRST_SIX_RANK_LIST.

get_role_guess_type_and_coin(RoleID) ->
    case get_role_alien(RoleID) of
        ?undefined ->
            {false, 0};
        #role_alien{guessCoin=Coin, guessType=Type} ->
            {Type, Coin}
    end.

get_role_rank(RoleID) ->
    case get_role_alien(RoleID) of
        ?undefined ->
            0;
        #role_alien{rank=Rank} ->
            Rank
    end. 

get_role_group_id(RoleID) ->
    case get_role_alien(RoleID) of
        ?undefined ->
            0;
        #role_alien{group_id=GroupID} ->
            GroupID
    end.

get_role_alien(RoleID) ->
    erlang:get({?role_alien_info, RoleID}).

set_role_alien(#role_alien{roleID=RoleID}=RoleAlien) ->
%%     ?ERR("RoleAlien:~w", [RoleAlien]),
    erlang:put({?role_alien_info, RoleID}, RoleAlien).

set_role_rank_to_role_id(RoleRank, GroupID, RoleID) ->
%%     ?ERR("RoleRank:~w, GroupID:~w, RoleID:~w", [RoleRank, GroupID, RoleID]),
    erlang:put({?rank_to_role_id, GroupID, RoleRank}, RoleID).

get_role_alien_by_rank(RoleRank, GroupID) ->
    case erlang:get({?rank_to_role_id, GroupID, RoleRank}) of
        ?undefined ->
            ?undefined;
        RoleID ->
            erlang:get({?role_alien_info, RoleID})
    end.

get_all_rank_data() ->
    lists:foldr(fun({{?role_alien_info, _}, V2}, Acc) ->
                        case V2 of
                            #role_alien{is_sign=true,group_id=GroupID} ->
                                case lists:keyfind(GroupID, 1, Acc) of
                                    false ->
                                        [{GroupID,[V2]}|Acc];
                                    {GroupID, List} ->
                                        lists:keyreplace(GroupID, 1, Acc, {GroupID, [V2|List]})
                                end;
                            _ ->
                                Acc
                        end;
                   (_, Acc) ->
                        Acc
                end, [], erlang:get()).

get_all_guess_data() ->
    lists:foldr(fun({{?role_alien_info, _}, V2}, Acc) ->
                        case V2 of
                            #role_alien{guessCoin=GuessCoin,group_id=GroupID} when GuessCoin > 0 ->
                                case lists:keyfind(GroupID, 1, Acc) of
                                    false ->
                                        [{GroupID,[V2]}|Acc];
                                    {GroupID, List} ->
                                        lists:keyreplace(GroupID, 1, Acc, {GroupID, [V2|List]})
                                end;
                            _ ->
                                Acc
                        end;
                   (_, Acc) ->
                        Acc
                end, [], erlang:get()).

get_role_alien_dict_data() ->
    lists:foldr(fun({{?rank_to_role_id, _, _}, _}=D, Acc) ->
                        [D|Acc];
                   ({{?role_alien_info, V1}, V2}, Acc) ->
                        [{{?role_alien_info, V1}, V2#role_alien{fighterList=[], itemList=[]}}|Acc];
                   (_, Acc) ->
                        Acc
                end, [], erlang:get()).

update_fighter_list(SlaveServerList) ->
    DictDataList = get_role_alien_dict_data(),
    erlang:spawn(fun() ->
                         lists:foreach(fun(SlaveServer) ->
                                               catch global:send(SlaveServer,  {update_fighter_list, DictDataList})
                                       end, SlaveServerList)  
                 end).

get_replay_record(ReplayUID) ->
    case erlang:get({?replayRecord, ReplayUID}) of
        undefined ->
            case db_sql:get_fightReplay(ReplayUID, ?REPLAY_TYPE_ALIEN) of
                []->
                    case ReplayUID of
                        0 ->
                            {#sc_fight_request{actionList=[],fighterList=[],result=true}, 1};
                        _ ->
                            {#sc_fight_request{actionList=[],fighterList=[],result=true}, 2}
                    end;
                Rec->
                    erlang:put({?replayRecord, ReplayUID}, Rec),
                    {Rec, 1}
            end;
        Cached ->
            {Cached, 1}
    end.

%% 清除所有的战报缓存
erase_replay_record() ->
    lists:foreach(fun({{?replayRecord, ReplayUID},_}) ->
                          erlang:erase({?replayRecord, ReplayUID});
                     (_) ->
                          next
                  end, erlang:get()).

%% 清除指定战报缓存
erase_replay_record(ReplayUIDList) ->
    lists:foreach(fun(ReplayUID) ->
                          erlang:erase({?replayRecord, ReplayUID})
                  end, ReplayUIDList).

%% -------------------------------------------------------------------------------------------------------------------------
send_msg_to_slave(SlaveServerList, Msg) ->
    lists:foreach(fun(SlaveServerName) ->
                          case global:whereis_name(SlaveServerName) of
                              ?undefined ->
                                  ?ERR("SlaveServerName:~w is undefined, Msg:~w", [SlaveServerName, Msg]);
                              Pid when erlang:is_pid(Pid) ->
%%                                   ?ERR("SlaveServerName:~w,Msg:~w", [SlaveServerName,Msg]),
                                  global:send(SlaveServerName, Msg)
                          end
                  end, SlaveServerList).

send_msg_to_slave_sever_id(ServerID, Msg) ->
    Platform = data_setting:get(platform),
    SlaveServerName = get_cross_server_name(ServerID, false, Platform),
    case global:whereis_name(SlaveServerName) of
        ?undefined ->
            ?ERR("send_msg_to_slave_sever_id error, SlaveServerName:~w is undefined.~nMsg:~w", [SlaveServerName, Msg]);
        Pid when erlang:is_pid(Pid) ->
            global:send(SlaveServerName, Msg)
    end.

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
            erlang:list_to_atom(lists:concat(['cross_master_', Platform, ServerID]));
        false ->
            erlang:list_to_atom(lists:concat(['cross_slave_', Platform, ServerID]))
    end.

get_slave_server_list() ->
    SlaveServerList = data_setting:get(cross_slave_server_list),
    Platform = data_setting:get(platform),
    lists:map(fun({SlaveServerID, _SlaveServerIP}) ->
                      get_cross_server_name(SlaveServerID, false, Platform)
              end, SlaveServerList).

get_master_node() ->
    MasterID = data_setting:get(server_id),
    MasterIP = data_setting:get(cross_master_ip),
    Platform = data_setting:get(platform),
    get_master_node(MasterIP, MasterID, Platform).

get_master_node(MasterIP, MasterID, Platform) ->
    NodeNameList = lists:concat([Platform, '_master_', MasterID, '@', MasterIP]),
    erlang:list_to_atom(NodeNameList).

get_slave_node_list() ->
    SlaveServerList = data_setting:get(cross_slave_server_list),
    Platform = data_setting:get(platform),
    lists:map(fun({SlaveServerID, SlaveServerIP}) ->
                      NodeNameList = lists:concat([Platform, SlaveServerID, '@', SlaveServerIP]),
                      erlang:list_to_atom(NodeNameList)
              end, SlaveServerList).

%% 主服务器检查与从服务器的连接
check_connection(State) ->
    #state{master_node=MasterNode, master_server=MasterServer,slave_node_list=SlaveNodeList, slave_server_list=SlaveServerList,
           next_status_left_seconds=NextStatusLeftSeconds, status=Status,session_id=SessionID,
           noticed_server_list=NoticedServerList} = State,
    %%lists:foreach(fun(SlaveNode) -> net_adm:ping(SlaveNode) end, SlaveNodeList),
    IsAllSlaveNodeConnect = lists:all(fun(SlaveNode) ->
                             case net_adm:ping(SlaveNode) of
                                 pong ->
                                     true;
                                 _ ->
                                     ?ERR("警告：不能连接到从服务器节点:~w", [SlaveNode]),
                                     false
                             end
                     end, SlaveNodeList),
    {ExistServerNum, NewNoticedServerList} = 
        lists:foldr(
          fun(SlaveServerName, {AccExistServerNum, AccNoticedServerList}) ->
                  case global:whereis_name(SlaveServerName) of
                      ?undefined ->
                          ?ERR("警告：从服务器 ~w 不存在", [SlaveServerName]),
                          {AccExistServerNum, lists:delete(SlaveServerName, AccNoticedServerList)};
                      _ ->
                          case lists:member(SlaveServerName, AccNoticedServerList) of
                              true ->
                                  {AccExistServerNum + 1, AccNoticedServerList};
                              false ->
                                  global:send(SlaveServerName, {update_server_list,MasterNode,MasterServer,NextStatusLeftSeconds,Status,SessionID}),
                                  {AccExistServerNum + 1, [SlaveServerName|AccNoticedServerList]}
                          end
                  end
          end, {0, NoticedServerList}, SlaveServerList),
    IsAllSlaveServerExist = (erlang:length(SlaveServerList) =:= ExistServerNum),
    IsAllConnect = IsAllSlaveNodeConnect andalso IsAllSlaveServerExist,
    case NextStatusLeftSeconds > 1 of
        true ->
            NewNextStatusLeftSeconds = calc_left_seconds(NextStatusLeftSeconds),
            State#state{is_all_connect=IsAllConnect, next_status_left_seconds=NewNextStatusLeftSeconds, noticed_server_list=NewNoticedServerList};
        false ->
            Msg = get_next_status_msg(Status),
            erlang:send(erlang:self(), Msg),
            case Status >= ?STATUS_FIGHT of
                true ->
                    NewStatus = ?STATUS_CLOSE;
                false ->
                    NewStatus = Status + 1
            end,
            NewNextStatusLeftSeconds = data_alien:get({left_seconds, NewStatus}),
            State#state{status=NewStatus, is_all_connect=IsAllConnect, next_status_left_seconds=NewNextStatusLeftSeconds, noticed_server_list=NewNoticedServerList}
    end.

update_status_to_slave(#state{session_id=SessionID, status=Status, next_status_left_seconds=LeftSeconds,
                              slave_server_list=SlaveServerList,master_node=MasterNode, master_server=MasterServerName}) ->
    lists:foreach(fun(SlaveServer) ->
                          case global:whereis_name(SlaveServer) of
                              ?undefined ->
                                  ?ERR("从服务器：~w 不存在，无法同步", [SlaveServer]);
                              _Pid ->
                                  global:send(SlaveServer, {update_status, SessionID, Status, LeftSeconds,MasterNode,MasterServerName})
                          end
                  end, SlaveServerList).

get_next_status_msg(Status) ->
    case Status of
        ?STATUS_CLOSE ->
            do_sign;
        ?STATUS_SIGN ->
            do_fight;
        ?STATUS_FIGHT ->
            do_close
    end.

change_to_next_status(State) ->
    #state{status=Status} = State,
    Msg = get_next_status_msg(Status),
    ?ERR("Msg:~w", [Msg]),
    erlang:send(erlang:self(), Msg),
    case Status >= ?STATUS_FIGHT of
        true ->
            NewStatus = ?STATUS_CLOSE;
        false ->
            NewStatus = Status + 1
    end,
    LeftSeconds = data_alien:get({left_seconds, NewStatus}),
    ?ERR("NewStatus:~w, LeftSeconds:~w", [NewStatus, LeftSeconds]),
    State#state{status=NewStatus, next_status_left_seconds=LeftSeconds}.

do_update_server_list(State) ->
    case State#state.status =:= ?STATUS_CLOSE of
        true ->
            SlaveNodeList = get_slave_node_list(),
            SlaveServerList = get_slave_server_list(),
            NewState = State#state{slave_node_list=SlaveNodeList, slave_server_list=SlaveServerList, noticed_server_list=[]},
            ?ERR("更新server_list成功~n master_node:~w,master_server:~w,slave_node_list:~w,slave_server_list:~w",
                 [NewState#state.master_node, NewState#state.master_server, NewState#state.slave_node_list, NewState#state.slave_server_list]),
            {noreply, NewState};
        false ->
            ?ERR("更新server_list失败，当前status:~w 不能进行更新，必须在跨服战关闭期间方可更新", [State#state.status]),
            {noreply, State}
    end.

%% ----------------------------------------------------------------------------------------------------------------------


test_to_next() ->
    erlang:send(?MODULE, change_to_next_status).

divide(List) when erlang:is_list(List) ->
    {List1, List2} =
        lists:foldr(fun({ID, _}=E, {Acc1, Acc2}) ->
                            case ID rem 2 of
                                1 ->
                                    {[E|Acc1], Acc2};
                                0 ->
                                    {Acc1, [E|Acc2]}
                            end
                    end, {[],[]}, List),
    io:format("~n~n", []),
    io:format("~10000p~n", [List1]),
    io:format("~n~n", []),
    io:format("~10000p~n", [List2]).






















