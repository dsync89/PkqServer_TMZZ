-module(alien_server).

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
-record(state, {count=0,session_id=0,status=0,next_status_left_seconds,master_node, master_server}).

-define(DUMP_INTERVAL, (1000 * 60 * 10)).
-define(CHECK_NODE_CONNECTION_INTERVAL, 20000).
-define(UPDATE_FIGHTER_LIST_INTERVAL, 1000).

-define(update_timestamp, update_timestamp).
-define(bc_list, bc_list).
-define(self_record_list, self_record_list).

-define(selfReplayRecord,selfReplayRecord).

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
        [{state,State}|AllInfo] ->
            lists:foreach(fun({Key,Value}) -> erlang:put(Key, Value) end, AllInfo);
        _ ->
            State = #state{}
    end,
    global:register_name(alien_master_server:get_cross_server_name(data_setting:get(server_id),false, data_setting:get(platform)),erlang:self()),
    erlang:send(self(), check_connection),
    ?ERR("State:~w", [State]),
    {ok, State#state{count=0}}.


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
do_handle_info({send_mail_reward, RoleID, SellReward, MailTemplateID, ArgList}, State) ->
    mail_server:send_sys_mail(RoleID, MailTemplateID, ArgList, "", SellReward),
    {noreply, State};
do_handle_info({alien_fight, RoleID, TarRoleID, TarTank, NeedTimes, NeedGold}, State) ->
    do_alien_fight(RoleID, TarRoleID, TarTank, NeedTimes, NeedGold, State),
    {noreply, State};
do_handle_info({do_alien_fight_succ_return, RoleID, IsWin, EnemyName, FightRecord, NeedTimes, NeedGold, AddCoin,NewRank,NewFighterList}, State) ->
    ?unicast(RoleID, #sc_alien_fight{result=1,fightInfo=[FightRecord],newRank=NewRank,addCoin=AddCoin,
                                     fighterList=trans2p_alien_fighterList(NewFighterList)}),
    catch role_lib:send_server(RoleID, {do_alien_fight_succ_return, NeedTimes, NeedGold, AddCoin}),
    save_fight_record(RoleID,IsWin, EnemyName, FightRecord,NewRank,true),
    {noreply, State};
do_handle_info({be_fighted, RoleID, IsWin, EnemyName, NewRank, FightRecord}, State) ->
    save_fight_record(RoleID,IsWin, EnemyName, FightRecord,NewRank,false),
    {noreply, State};
do_handle_info({do_alien_fight_error_return, RoleID, Reason, NewFighterList}, State) ->
    ?unicast(RoleID, #sc_alien_fight{result=Reason,fightInfo=[],newRank=0,addCoin=0,fighterList=[]}),
    case erlang:is_list(NewFighterList) andalso NewFighterList =/= [] of
        true ->
            ?unicast(RoleID, #sc_alien_new_fighter_list{fighterList=trans2p_alien_fighterList(NewFighterList)});
        false ->
            next
    end,
    {noreply, State};
do_handle_info({get_alien_info, RoleID, AlienTimes, ResetTime}, State) ->
    do_get_alien_info(RoleID, AlienTimes, ResetTime, State),
    {noreply, State};
do_handle_info({get_alien_info_return, Msg}, State) ->
    do_get_alien_info_return(Msg),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_view_other{tarRoleID=TarRoleID}}, #state{master_server=MasterServer}=State) ->
    case global:whereis_name(MasterServer) of
        ?undefined ->
            next;
        _ ->
            global:send(MasterServer, {alien_view_other, RoleID, TarRoleID, data_setting:get(server_id)})
    end,
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_view_other_dtl{tarRoleID=TarRoleID}}, #state{master_server=MasterServer}=State) ->
    case global:whereis_name(MasterServer) of
        ?undefined ->
            next;
        _ ->
            global:send(MasterServer, {alien_view_other_dtl, RoleID, TarRoleID, data_setting:get(server_id)})
    end, 
    {noreply, State};
do_handle_info({do_alien_view_other_return, RoleID, Record}, State) ->
    ?unicast(RoleID, Record),
    {noreply, State};
do_handle_info({do_alien_view_other_dtl_return, RoleID, Record}, State) ->
    ?unicast(RoleID, Record),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_first_five{}}, State) ->
    do_alien_first_five(RoleID, State),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_leave{}}, State) ->
    set_bc_list(lists:delete(RoleID,get_bc_list())),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_kill_continuous_rank{start=Start,num=Num}}, #state{master_server=MasterServer}=State) ->
    case Num > 0 of
        true ->
            case global:whereis_name(MasterServer) of
                ?undefined ->
                    next;
                _ ->
                    global:send(MasterServer, {get_alien_kill_con_rank, RoleID, Start, Num, data_setting:get(server_id)})
            end;
        false ->
            next
    end,
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_kill_num_rank{start=Start,num=Num}}, #state{master_server=MasterServer}=State) ->
    case Num > 0 of
        true ->
            case global:whereis_name(MasterServer) of
                ?undefined ->
                    next;
                _ ->
                    global:send(MasterServer, {get_alien_kill_num_rank, RoleID, Start, Num, data_setting:get(server_id)})
            end;
        false ->
            next
    end,
    {noreply, State};
do_handle_info({get_alien_kill_num_rank_return, RoleID, List}, State) ->
    case List of
        [] ->
            ?unicast(RoleID, #sc_alien_buy_times{result=20,newTimes=0});
        _ ->
            ?unicast(RoleID, #sc_alien_kill_num_rank{fighterList=transkillnumranklist(List)})
    end,
    {noreply, State};
do_handle_info({get_alien_kill_con_rank_return, RoleID, List}, State) ->
    case List of
        [] ->
            ?unicast(RoleID, #sc_alien_buy_times{result=20,newTimes=0});
        _ ->
            ?unicast(RoleID, #sc_alien_kill_continuous_rank{fighterList=transkillconranklist(List)})
    end,
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_self_fight_replay{replayUID=ReplayUID}}, State) ->
    {FightRecord, _} = get_replay_record(ReplayUID),
    ?unicast(RoleID, #sc_alien_self_fight_replay{fightInfoList=FightRecord}),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_fight_replay{replayUID=ReplayUID}}, #state{master_server=MasterServer}=State) ->
    case global:whereis_name(MasterServer) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_fight_repaly{fightInfoList=#sc_fight_request{actionList=[],fighterList=[],result=true}});
        _ ->
            global:send(MasterServer, {get_alien_fight_replay, RoleID, ReplayUID, data_setting:get(server_id)})
    end,
    {noreply, State};
do_handle_info({get_alien_fight_replay_return, RoleID, FightRecord}, State) ->
    ?unicast(RoleID, #sc_alien_fight_repaly{fightInfoList=FightRecord}),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_self_record{start=StartT,num=Num}}, State) ->
    Start = StartT + 1,
    case Num > 0 of
        true ->
            List = get_self_record(RoleID),
            case Start =< erlang:length(List) of
                true ->
                    ?unicast(RoleID, #sc_alien_self_record{recordList=lists:sublist(List, Start, Num)});
                false ->
                    ?unicast(RoleID, #sc_alien_self_record{recordList=[]})
            end;
        false ->
            ?unicast(RoleID, #sc_alien_self_record{recordList=[]})
    end,
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_record{start=Start,num=Num}}, #state{master_server=MasterServer}=State) ->
    case Num > 0 of
        true ->
            case global:whereis_name(MasterServer) of
                ?undefined ->
                    ?unicast(RoleID, #sc_alien_record{recordList=[]});
                _ ->
                    global:send(MasterServer, {get_alien_record, RoleID, Start, Num, data_setting:get(server_id)})
            end;
        false ->
            ?unicast(RoleID, #sc_alien_record{recordList=[]})
    end,
    {noreply, State};
do_handle_info({get_alien_record_return, RoleID, List}, State) ->
    ?unicast(RoleID, #sc_alien_record{recordList=List}),
    {noreply, State};
do_handle_info({role_offline, RoleID}, State) ->
    set_bc_list(lists:delete(RoleID,get_bc_list())),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_active{}}, #state{status=Status}=State) ->
    ?unicast(RoleID, #sc_alien_active{status=Status}),
    {noreply, State};
do_handle_info({get_alien_info_return, RoleID, FighterList}, State) ->
    do_get_alien_info_return(RoleID, FighterList),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_guess_info{}}, State) ->
    do_alien_guess_info(RoleID, State),
    {noreply, State};
do_handle_info({alien_guess, RoleID, RoleInfo, GuessCoin, GuessType}, State) ->
    do_alien_guess(RoleID, RoleInfo, GuessCoin, GuessType, State),
    {noreply, State};
do_handle_info({do_alien_guess_return, RoleID, 0, GuessCoin}, State) ->
    ?unicast(RoleID, #sc_alien_guess{result=0}),
    role_lib:send_server(RoleID, {do_alien_guess_succ_return, GuessCoin}),
    {noreply, State};
do_handle_info({do_alien_guess_return, RoleID, Reason}, State) ->
    ?unicast(RoleID, #sc_alien_guess{result=Reason}),
    {noreply, State};
do_handle_info({get_alien_guess_info_return, RoleID, Type, Coin, GuessOddNum, GuessEvenNum}, State) ->
    ?unicast(RoleID, #sc_alien_guess_info{guessCoin=Coin,guessType=Type,guessOddNum=GuessOddNum,guessEvenNum=GuessEvenNum,
                                          coinValList=data_alien:get(coin_val_list_odd)++data_alien:get(coin_val_list_even)}),
    {noreply, State};
do_handle_info({alien_sign, RoleInfo, FighterList, ItemList}, State) ->
    do_alien_sign(RoleInfo, FighterList, ItemList, State),
    {noreply, State};
do_handle_info({do_alien_sign_return, RoleID, Result}, State) ->
    ?unicast(RoleID, #sc_alien_sign{result=Result}),
    {noreply, State};
do_handle_info({alien_reset, RoleInfo, FighterList,ItemList}, State) ->
    do_alien_reset(RoleInfo, FighterList, ItemList,State),
    {noreply, State};
do_handle_info({do_alien_reset_return, RoleID, Reason}, State) ->
    ?unicast(RoleID, #sc_alien_reset{result=Reason,timestamp=0}),
    {noreply, State};
do_handle_info({do_alien_reset_return, RoleID, 0, NewFighterList}, State) ->
    ResetTime = util:now()+data_alien:get(reset_seconds),
    ?unicast(RoleID, #sc_alien_reset{result=0,fighterList=trans2p_alien_fighterList(NewFighterList),
                                     timestamp=ResetTime}),
    role_lib:send_server(RoleID, {do_alien_reset_return_succ, ResetTime}),
    {noreply, State};
do_handle_info({update_server_status, NewStatus, LeftSeconds}, State) ->
    NewState = State#state{next_status_left_seconds=LeftSeconds, status=NewStatus},
    {noreply, NewState};
do_handle_info({update_status, SessionID, Status, LeftSeconds,MasterNode,MasterServerName}, State) ->
    NewState = State#state{session_id=SessionID, status=Status, next_status_left_seconds=LeftSeconds,
                           master_node=MasterNode, master_server=MasterServerName},
    erlang:put(?update_timestamp, util:now()),
    {noreply, NewState};
do_handle_info({update_fighter_list, DictDataList}, State) ->
    update_fighter_list(get_bc_list(), DictDataList),
    {noreply, State};
do_handle_info(check_connection, State) ->
    erlang:send_after(?CHECK_NODE_CONNECTION_INTERVAL, self(), check_connection),
    NewState =
        case erlang:get(?update_timestamp) of
            ?undefined ->
                State#state{session_id=0,status=0,next_status_left_seconds=?undefined,master_node=?undefined, master_server=?undefined};
            Timestamp ->
                case util:now() - Timestamp > (?CHECK_NODE_CONNECTION_INTERVAL div 1000) of
                    true ->
                        State#state{session_id=0,status=0,next_status_left_seconds=?undefined,master_node=?undefined, master_server=?undefined};
                    false ->
                        State
                end
        end,
    {noreply, NewState};
do_handle_info(do_sign, State) ->
    ?ERR("开始报名", []),
    broadcast_server:bc(#sc_alien_active{status=?STATUS_SIGN}),
    UpdateTimestamp = erlang:get(?update_timestamp),
    BcList = get_bc_list(),
    erlang:erase(),
    erlang:spawn(fun() -> db_sql:del_spec_type_replay(?REPLAY_TYPE_ALIEN) end),
    erlang:put(?update_timestamp, UpdateTimestamp),
    set_bc_list(BcList),
    send_bc_list_msg({route, role_alien, #cs_alien_info{}}),
    {noreply, State};
do_handle_info(do_fight, State) ->
    ?ERR("开始比赛", []),
    broadcast_server:bc(#sc_alien_active{status=?STATUS_FIGHT}),
    send_bc_list_msg({route, role_alien, #cs_alien_info{}}),
    {noreply, State};
do_handle_info(do_close, State) ->
    ?ERR("关闭跨服战", []),
    broadcast_server:bc(#sc_alien_active{status=?STATUS_CLOSE}),
    send_bc_list_msg({route, role_alien, #cs_alien_info{}}),
    {noreply, State};
do_handle_info({update_server_list,MasterNode,MasterServer,NextStatusLeftSeconds,Status,SessionID}, State) ->
    erlang:put(?update_timestamp, util:now()),
    do_update_server_list(State,MasterNode,MasterServer,NextStatusLeftSeconds,Status,SessionID);
do_handle_info(dump_data, #state{count=Count}=State) ->
    do_persist(State),
    case Count rem 6 of
        0 ->
            erase_replay_record();
        _ ->
            next
    end,
    {noreply, State#state{count=Count+1}};
do_handle_info(Info, State) ->
    ?ERR("unkown info:~w", [Info]),
    {noreply, State}.

do_persist(State) ->
    Info = [E || E<-erlang:get(), is_persist(E) ],
    Info2 = [{state,State} | Info],
    db_sql:set_etc(?DB_ETC_KEY_ALIEN,Info2).

is_persist({{?self_record_list, _}, _}) ->
    true;
is_persist(_) ->
    false.

%% ------------------------------------------------------------------------------------------------------------------------
do_alien_fight(RoleID, TarRoleID, TarTank, NeedTimes, NeedGold, #state{master_server=MasterServer}) ->
    case global:whereis_name(MasterServer) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_fight{result=11,fightInfo=[],newRank=0,addCoin=0,fighterList=[]});
        _ ->
            global:send(MasterServer, {do_alien_fight, RoleID, TarRoleID, TarTank, NeedTimes, NeedGold, data_setting:get(server_id)})
    end.

do_alien_sign(#role{roleID=RoleID}=RoleInfo, FighterList, ItemList, #state{master_server=MasterServer}) ->
    case global:whereis_name(MasterServer) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_sign{result=3});
        _ ->
            global:send(MasterServer, {do_alien_sign, RoleID, RoleInfo, FighterList, ItemList, data_setting:get(server_id)})
    end.

do_alien_reset(#role{roleID=RoleID}=RoleInfo, FighterList, ItemList, #state{master_server=MasterServer}) ->
    case global:whereis_name(MasterServer) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_reset{result=2,timestamp=0});
        _ ->
            global:send(MasterServer, {do_alien_reset, RoleID, RoleInfo, FighterList, ItemList, data_setting:get(server_id)})
    end.

do_alien_guess(RoleID, RoleInfo, GuessCoin, GuessType, #state{master_server=MasterServer}) ->
    case global:whereis_name(MasterServer) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_guess{result=3});
        _ ->
            global:send(MasterServer, {do_alien_guess, RoleID, RoleInfo, GuessCoin, GuessType, data_setting:get(server_id)})
    end.

do_alien_guess_info(RoleID, #state{master_server=MasterServer}) ->
    case global:whereis_name(MasterServer) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_guess_info{guessCoin=0,guessType=false,guessOddNum=0,guessEvenNum=0,
                                                  coinValList=[]});
        _ ->
            global:send(MasterServer, {get_alien_guess_info, RoleID, data_setting:get(server_id)})
    end.   

do_alien_first_five(RoleID, #state{master_server=MasterServer}) ->
    case global:whereis_name(MasterServer) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_first_five{fighterList=[]});
        _ ->
            global:send(MasterServer, {get_alien_first_five, RoleID, data_setting:get(server_id)})
    end.

do_get_alien_info_return(RoleID, FighterList) ->
    ?unicast(RoleID, #sc_alien_first_five{fighterList=trans2p_alien_fighterList(FighterList)}).

do_get_alien_info(RoleID, AlienTimes,ResetTime,
                  #state{master_server=MasterServer}) ->
    case global:whereis_name(MasterServer) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_info{isOpen=false,endTimestamp=0,groupID=0,isSign=false,leftTimes=0,fighterList=[],
                                            resetTime=0,resetNeedGold=data_alien:get(reset_gold),
                                            maxTimes=data_common:get(max_alien_times),
                                            price=data_alien:get(price)});
        _ ->
            global:send(MasterServer, {get_alien_info, RoleID, AlienTimes, ResetTime, data_setting:get(server_id)})
    end.

do_get_alien_info_return(Msg) ->
    case Msg of
        {?STATUS_FIGHT, IsSign, GroupID, FighterList, RoleID, EndTimestamp, AlienTimes, ResetTime} ->
            ?unicast(RoleID, #sc_alien_info{isOpen=true,endTimestamp=EndTimestamp,groupID=GroupID,isSign=IsSign,
                                            leftTimes=AlienTimes,fighterList=trans2p_alien_fighterList(FighterList),
                                            resetTime=ResetTime,resetNeedGold=data_alien:get(reset_gold),
                                            maxTimes=data_common:get(max_alien_times),
                                            price=data_alien:get(price)});
        {?STATUS_CLOSE, GroupID, FighterList, RoleID, EndTimestamp, AlienTimes} ->
            ?unicast(RoleID, #sc_alien_info{isOpen=false,endTimestamp=EndTimestamp,groupID=GroupID,isSign=false,
                                            leftTimes=AlienTimes,fighterList=trans2p_alien_fighterList(FighterList),
                                            resetTime=0,resetNeedGold=data_alien:get(reset_gold),
                                            maxTimes=data_common:get(max_alien_times),
                                            price=data_alien:get(price)});
        {?STATUS_SIGN, IsSign, RoleID, EndTimestamp} ->
            NeedVipLevel = data_alien:get(need_vip_level),
            NeedRoleLevel = data_alien:get(need_role_level),
            ?unicast(RoleID, #sc_alien_sign_info{needVipLevel=NeedVipLevel,needLevel=NeedRoleLevel,
                                                 isSign=IsSign,signEndTimestamp=EndTimestamp})
    end,
    BcList = get_bc_list(),
    set_bc_list([RoleID|lists:delete(RoleID,BcList)]).

trans2p_alien_fighterList(List) ->
    lists:map(fun(#role_alien{roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,head=Head,
                              level=Level,roleName=RoleName,rank=Rank,serverID=ServerID,hpPercent=HPPercent,
                              canBeAtkTime=CanBeAtkTime}) ->
               #p_alien_fighter{roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,head=Head,
                                level=Level,roleName=RoleName,rank=Rank,serverID=ServerID,hpPercent=HPPercent,
                                canBeAtkTime=CanBeAtkTime}       
              end, List).

do_update_server_list(State,MasterNode,MasterServer,NextStatusLeftSeconds,Status,SessionID) ->
    NewState = State#state{session_id=SessionID,status=Status, next_status_left_seconds=NextStatusLeftSeconds,
                           master_node=MasterNode, master_server=MasterServer},
    ?ERR("更新server_list成功~n master_node:~w,master_server:~w",
         [NewState#state.master_node, NewState#state.master_server]),
    {noreply, NewState}.

save_fight_record(RoleID,IsWin, EnemyName, FightRecord,NewRank,IsAtk) ->
    ReplayUID = tk_id:gen_replayUID(?REPLAY_TYPE_ALIEN),
    erlang:spawn(fun() -> db_sql:set_fightReplay(ReplayUID, FightRecord, ?REPLAY_TYPE_ALIEN) end),
    NewSelfRecord =
        #p_alien_self_record{
                             isAtk=IsAtk
                             ,isWin=IsWin
                             ,enemyName=EnemyName
                             ,newRank=NewRank
                             ,replayUID=ReplayUID
                             ,timestamp=util:now()},
    SelfRecordList = get_self_record(RoleID),
    NewSelfRecordList = [NewSelfRecord|SelfRecordList],
    
    case erlang:length(NewSelfRecordList) =< data_alien:get(record_self_max_num) of
        true ->
            NewSelfRecordList2 = NewSelfRecordList;
        false ->
            #p_alien_self_record{replayUID=LastReplayUID} = LastSelfRecord = lists:last(NewSelfRecordList),
            erlang:spawn(fun() -> db_sql:del_fightReplay(LastReplayUID, ?REPLAY_TYPE_ALIEN) end),
            NewSelfRecordList2 = lists:delete(LastSelfRecord, NewSelfRecordList)
    end,
    set_self_record(RoleID, NewSelfRecordList2),
    ?unicast(RoleID, #sc_alien_new_self_record{}).

get_self_record(RoleID) ->
    case erlang:get({?self_record_list, RoleID}) of
        List when erlang:is_list(List) ->
            List;
        _ ->
            []
    end.

set_self_record(RoleID, NewList) when erlang:is_list(NewList) ->
    erlang:put({?self_record_list, RoleID}, NewList).

get_bc_list() ->
    case erlang:get(?bc_list) of
        ?undefined ->
            [];
        List ->
            List
    end.

set_bc_list(List) when erlang:is_list(List) ->
    erlang:put(?bc_list, List).

send_bc_list_msg(Msg) ->
    BcList = get_bc_list(),
    erlang:spawn(
      fun() ->
              lists:foreach(
                fun(RoleID) ->
                        catch role_lib:send_server(RoleID, Msg)
                end, BcList)
      end).

update_fighter_list(BcList, DictDataList) ->
    erlang:spawn(fun() ->
                         lists:foreach(fun({Key, Val}) ->
                                               erlang:put(Key, Val)
                                       end, DictDataList),
                         lists:foreach(fun(RoleID) ->
                                               case alien_master_server:get_role_alien(RoleID) of
                                                   #role_alien{is_sign=true} ->
                                                       FighterList = alien_master_server:get_fighter_list(RoleID, true),
                                                       ?unicast2(RoleID, #sc_alien_new_fighter_list{fighterList=trans2p_alien_fighterList(FighterList)});
                                                   _ ->
                                                       next
                                               end
                                       end, BcList)
                 end).

transkillnumranklist(List) ->
    lists:map(fun(#alien_fighter2{roleID=RoleID ,rank=Rank,
                                  fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,serverID=ServerID,
                                  killNum=NewRoleKillNum}) ->
                      #p_alien_fighter2{roleID=RoleID ,rank=Rank,
                                        fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,serverID=ServerID,
                                        killNum=NewRoleKillNum} 
              end, List).

transkillconranklist(List) ->
    lists:map(fun(#alien_fighter3{roleID=RoleID,rank=Rank,isInContinuous=IsInContinuous,
                                  fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,serverID=ServerID,
                                  killContinuousNum=NewRoleKillContinuousNum}) ->
                      #p_alien_fighter3{roleID=RoleID,rank=Rank,isInContinuous=IsInContinuous,
                                        fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,serverID=ServerID,
                                        killContinuousNum=NewRoleKillContinuousNum}
              end, List).

get_replay_record(ReplayUID)->
    case erlang:get({?selfReplayRecord, ReplayUID}) of
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
                    erlang:put({?selfReplayRecord, ReplayUID}, Rec),
                    {Rec, 1}
            end;
        Cached ->
            {Cached, 1}
    end.

%% 清除所有的战报缓存
erase_replay_record() ->
    lists:foreach(fun({{?selfReplayRecord, ReplayUID},_}) ->
                          erlang:erase({?selfReplayRecord, ReplayUID});
                     (_) ->
                          next
                  end, erlang:get()).

%% 清除指定战报缓存
erase_replay_record(ReplayUIDList) ->
    lists:foreach(fun(ReplayUID) ->
                          erlang:erase({?selfReplayRecord, ReplayUID})
                  end, ReplayUIDList).

%% ----------------------------------------------------------------------------------------------------------------------

test_sign_niubi(N) when erlang:is_integer(N) andalso N > 0 ->
    lists:foreach(fun(RoleID) ->
                          test_sign(RoleID)
                  end, get_niubi(N)).

get_niubi(N) ->
    Sql = io_lib:format("select roleID from gRole order by fightPower desc limit ~w;", [N]),
    case db_sql:get_all(Sql) of
        [_|_]=List ->
            [E||[E]<-List];
        _ ->
            []
    end.

test_sign(RoleID) ->
    RoleInfo = role_data:get_otherRoleInfo(RoleID),
    {FighterList, _} = role_data:get_otherRoleFighter(RoleID),
    ItemList = role_data:get_otherRoleItemEquips(RoleID),
    erlang:send(?MODULE, {alien_sign, RoleInfo, FighterList, ItemList}).

test_sign_others(Max) when Max >= 1, Max =< 9999 ->
    lists:foreach(fun(RoleID) ->
                          test_sign(RoleID)   
                  end, lists:seq(?ROLE_ID_BASE + 1, ?ROLE_ID_BASE + Max)).

















































