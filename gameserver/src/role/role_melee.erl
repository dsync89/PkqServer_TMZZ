%% @author caijunjun
%% @doc 玩家大乱斗模块.


-module(role_melee).

-include("def_role.hrl").
-include("def_melee.hrl").

%% 再次战斗时间
-define(MeleeFightInterval, 3).

-define(meleeLastFightTime, meleeLastFightTime).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

cs_melee_info(#cs_melee_info{}) ->
    AllowTimes = data_melee:get(melee_sign_times),
    Today = erlang:date(),
    #roleTimes{meleeSignTimes = MeleeSignTimes, lastMeleeSignDate=Date} = role_data:get_roleTimes(),
     
    LeftTimes = case Today =:= Date of
                    true -> AllowTimes - MeleeSignTimes;
                    false -> AllowTimes
                end,
    
    Result = call_master_server({get_melee_info, role_data:get_roleID()}),
    
    MeleeInfo = 
        case Result of
            {melee_not_sign, CountDown} ->
                #sc_melee_info{leftTimes = LeftTimes, 
                               melee_status = 1, 
                               countdown = CountDown,
                               miaomiao_score = 0,
                               pipi_score = 0,
                               self_score = 0,
                               camp = 0,
                               cur_win_times = 0};
            {melee_signed, CountDown} ->
                #sc_melee_info{leftTimes = LeftTimes, 
                               melee_status = 2, 
                               countdown = CountDown,
                               miaomiao_score = 0,
                               pipi_score = 0,
                               self_score = 0,
                               camp = 0,
                               cur_win_times = 0};
            {melee_fighting, MiaoScore, PiScore, SelfScore, CurWinTimes, Camp, CountDown} ->
                #sc_melee_info{leftTimes = LeftTimes, 
                               melee_status = 3, 
                               countdown = CountDown,
                               miaomiao_score = MiaoScore,
                               pipi_score = PiScore,
                               self_score = SelfScore,
                               camp = Camp,
                               cur_win_times = CurWinTimes};
            _ ->  %% undefined | {false, 2} %% 超时 或者 无主节点连接 分别返回
                %% 自己来算
                Now = erlang:time(),
                OpenTimeList = data_melee:get(battle_ground_open_time),
                
                FindResult = util:fun_find(fun({_BattleID, Open, _Close}) ->
                                               if Now < Open ->
                                                      true;
                                                  true ->
                                                      false
                                               end
                                       end,
                                       OpenTimeList),
                case FindResult of
                    {_SignBattleID, SignOpenTime, _SignCloseTime} ->
                        next;
                    false ->
                        [{_SignBattleID, SignOpenTime, _SignCloseTime} | _] = OpenTimeList
                end,
                
                CountDown = get_countdown(SignOpenTime),
                #sc_melee_info{leftTimes = LeftTimes,
                               melee_status = 1,
                               countdown = CountDown,
                               miaomiao_score = 0,
                               pipi_score = 0,
                               self_score = 0,
                               camp = 0,
                               cur_win_times = 0}
        end,
    ?sendself(MeleeInfo).
    

cs_melee_sign(#cs_melee_sign{} = Record) ->
    case check_sign() of 
        true ->
            case do_sign(Record) of
                true ->
                    ?sendself(#sc_melee_sign{result = 0});
                false ->
                    ?sendself(#sc_melee_sign{result = 2})
            end;
        {false, Result} ->
            ?sendself(#sc_melee_sign{result = Result})
    end.


cs_melee_fight(#cs_melee_fight{} = _Record) ->
    Result = check_fight(),
    case Result of
        %% 找到对手
        {true,TarRoleID} ->
            case get_fighter_info(TarRoleID) of
                ?undefined ->
                    %% 针对某些节点挂掉的情况
                    %% 再获取几次，获取不到再返回错误码
                    case get_fighter_info_1(3) of
                        ?undefined ->
                            ?sendself(get_fight_error_record(5));
                        {ok, TarFighterList, TarLieuAdd, TarRoleName} ->
                            do_fight(TarRoleID, TarRoleName, TarFighterList, TarLieuAdd)
                    end;
                {ok, TarFighterList, TarLieuAdd, TarRoleName} ->
                    do_fight(TarRoleID, TarRoleName, TarFighterList, TarLieuAdd)
            end;
        %% 错误
        {false, ErrorCode} ->
            ?sendself(get_fight_error_record(ErrorCode));
        Error ->
            ?ERR("melee unexpected error ~p", [Error]),
            ?sendself(get_fight_error_record(2))
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign() ->
    #role{level = Level} = role_data:get_roleInfo(),
    AllowLevel = data_melee:get(need_role_level),
    case Level >= AllowLevel of
        true ->
            AllowTimes = data_melee:get(melee_sign_times),
            Today = erlang:date(),
            #roleTimes{meleeSignTimes = MeleeSignTimes, lastMeleeSignDate=Date} = role_data:get_roleTimes(),
            if 
                Today =:= Date ->
                   case AllowTimes > MeleeSignTimes of
                        true  -> true;
                        false -> {false, 3}
                   end;
                true ->
                    %% 不是同一天
                    true
            end;
        false ->
            {false, 1}
    end.
                    
do_sign(_Record) -> 
    RoleID = role_data:get_roleID(),
    Result = call_master_server({melee_sign, RoleID}), 
    case Result of
		%% 未报名
        {melee_sign_ack, 0} ->
            Today = erlang:date(),
            #roleTimes{meleeSignTimes = MeleeSignTimesRoleTimes, lastMeleeSignDate = Date} = RoleTimes = role_data:get_roleTimes(),
            case Today =:= Date of
                true ->
                    RoleTimes2 = RoleTimes#roleTimes{meleeSignTimes = MeleeSignTimesRoleTimes + 1};
                false ->
                    RoleTimes2 = RoleTimes#roleTimes{meleeSignTimes = 1, lastMeleeSignDate = Today}
            end,
            role_data:set_roleTimes(RoleTimes2),
            true;
		%% 已经报过名
		{melee_sign_ack, 1} ->
			true;
        _Error ->
            false
    end.

check_fight() ->
    LastMeleeFightTime = get_last_fight_time(),
    Now = util:now(),
    
    %% 少一秒
    case Now - LastMeleeFightTime >= ?MeleeFightInterval of
        true  ->
            AllowLevel = data_melee:get(need_role_level),
            #role{level = RoleLevel} = role_data:get_roleInfo(),
            case AllowLevel =< RoleLevel of
                true  ->
                    case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
                        true  ->
                            RoleID = role_data:get_roleID(),
                            call_master_server({melee_fight, RoleID});
                        false ->
                            {false, 255}
                    end;
                false ->
                    {false, 1}
            end;
        false ->
            {false, 3}
    end.

get_fighter_info(TarRoleID) ->
    get_fighter_info(TarRoleID, 3000).

get_fighter_info(TarRoleID, WaitTime) ->
    TarServerID = util:role_id_to_server_id(TarRoleID),
    cast_slave_server(TarServerID, {get_melee_role_fighter, self(), TarRoleID}),
    receive
        {get_melee_role_fighter_return, [],{0,0}, _TarRoleName} ->
            ?undefined;
        {get_melee_role_fighter_return, TarFighterList, TarLieuAdd, TarRoleName} ->
            {ok, TarFighterList, TarLieuAdd, TarRoleName}
    after
        WaitTime ->
        ?undefined
    end.

get_fighter_info_1(0) ->
    ?undefined;
get_fighter_info_1(N) ->
    RoleID = role_data:get_roleID(),
    case call_master_server({melee_fight, RoleID}) of
        %% 找到对手
        {true,TarRoleID} ->
            case get_fighter_info(TarRoleID, 1000) of
                ?undefined ->
                    get_fighter_info_1(N-1);
                Result ->
                    Result
            end;
        %% 错误
        Error ->
            ?ERR("melee unexpected error ~p", [Error]),
            ?undefined
    end.

get_fight_error_record(Result) ->
    #sc_melee_fight{result=Result,tar_role_id=0,tar_role_name = <<"">>,
                    add_score=0,miaomiao_score=0,pipi_score=0,
                    reward=#p_mail_reward{},fightInfo=[],cur_win_times=0}.

    
%% do_fight(0, TarRoleName, _TarFighterList, _TarLieuAdd) ->
%%     RoleID = role_data:get_roleID(),
%%     WinScore = data_melee:get(win_score),
%%     set_last_fight_time(util:now()),
%%     
%%     %% 计算连胜
%%     SellReward = data_melee:get(win_reward),
%%     role_reward:handle_sell_reward_f(role_data:get_roleInfo(), SellReward, ?MONEY_ADD_TYPE_MELEE,  1, ""),
%%     Reward = role_reward:transform2p_mail_reward(SellReward),
%% 
%%     %% 发送战斗结果
%%     case call_master_server({melee_fight_result, RoleID, true}) of
%%         {ok, MiaoScore, PiScore, CurWinTimes} ->
%%             ok;
%%         _ ->
%%             MiaoScore = 0,
%%             PiScore = 0,
%%             CurWinTimes = 0
%%     end,
%%     
%%     ?sendself(#sc_melee_fight{result=0,
%%                               tar_role_id=0,
%%                               tar_role_name=TarRoleName,
%%                               add_score=WinScore,
%%                               miaomiao_score=MiaoScore,
%%                               pipi_score=PiScore,
%%                               reward=Reward,
%%                               fightInfo=[],
%%                               cur_win_times = CurWinTimes}),
%%     ok;
do_fight(TarRoleID, TarRoleName, TarFighterList, TarLieuAdd) ->
    MyFighterList = role_data:get_fighter_list(),
    RoleLieuAdd = role_data:get_lieu_add_attr(),
    RoleID = role_data:get_roleID(),
    WinScore = data_melee:get(win_score),
    
    case ?CATCH(role_fight:new(MyFighterList, TarFighterList, RoleLieuAdd, TarLieuAdd)) of
        {IsWin, FightRecord, _} ->
            set_last_fight_time(util:now()),
            
            case IsWin of
                true ->
                    %% 计算连胜
                    SellReward = data_melee:get(win_reward),
                    role_reward:handle_sell_reward_f(role_data:get_roleInfo(), SellReward, ?MONEY_ADD_TYPE_MELEE,  1, ""),
                    Reward = role_reward:transform2p_mail_reward(SellReward),
                    AddScore = WinScore;
                false ->
                    AddScore = 0,
                    Reward = #p_mail_reward{}
            end,
            
            %% 发送战斗结果
            case call_master_server({melee_fight_result, RoleID, IsWin}) of
                {ok, MiaoScore, PiScore, CurWinTimes} ->
                    ok;
                _ ->
                    MiaoScore = 0,
                    PiScore = 0,
                    CurWinTimes = 0
            end,
            
            ?sendself(#sc_melee_fight{result=0,
                                      tar_role_id=TarRoleID,
                                      tar_role_name=TarRoleName,
                                      add_score=AddScore,
                                      miaomiao_score=MiaoScore,
                                      pipi_score=PiScore,
                                      reward=Reward,
                                      fightInfo=[FightRecord],
                                      cur_win_times = CurWinTimes}),
            ok;
        _Err ->
            Record = get_fight_error_record(6),
            ?sendself(Record)
    end.
    

call_master_server(Msg) ->
    erlang:send(melee_server, {sync_melee_msg, {self(), Msg}}),
    receive 
        {melee_msg_return, Return} ->
            Return
    after 5000 ->
        ?undefined
    end.
%%     ?CATCH(gen_server:call({global, get_master_server()}, Msg)).


cast_slave_server(ServerID, Msg) ->
    LocalServerID = get_server_id(),
    case ServerID =:= LocalServerID of
        true ->
            ?CATCH(erlang:send(melee_server, Msg));
        false ->
            ?CATCH(global:send(get_slave_server(ServerID), Msg))
    end.

call_slave_server(ServerID, Msg) ->
    LocalServerID = get_server_id(),
    case ServerID =:= LocalServerID of
        true -> 
            ?CATCH(gen_server:call(melee_server, Msg));
        false ->
            ?CATCH(gen_server:call({global,get_slave_server(ServerID)}, Msg))
    end.

-spec get_master_server() -> MasterServerName :: atom().
get_master_server() ->
    MasterServerID = data_setting:get(cross_master_id),
    Platform = data_setting:get(platform),
    erlang:list_to_atom(lists:concat(['cross_master_melee_', Platform, MasterServerID])).


-spec get_slave_server(ServerID :: integer()) -> SlaveServerName :: atom().
get_slave_server(ServerID) ->
    Platform = data_setting:get(platform),
    erlang:list_to_atom(lists:concat(['cross_slave_melee_', Platform, ServerID])).


get_server_id() ->
    data_setting:get(server_id).

%% 上次战斗时间
get_last_fight_time() ->
    case erlang:get(?meleeLastFightTime) of
        N when is_integer(N) -> N;
        _ErrorData -> 0
    end.

set_last_fight_time(N) when erlang:is_integer(N), N >= 0 ->
    erlang:put(?meleeLastFightTime, N).

%% 获取倒计时
get_countdown(OpenTime) ->
    DeadLine = calendar:time_to_seconds(OpenTime),
    Now = calendar:time_to_seconds(erlang:time()),
    Diff = DeadLine - Now,
    if Diff > 0 ->
           Diff + util:now();
       true ->
           Diff + ?ONE_DAY_SECONDS + util:now()
    end.
