-module(rule_master_server).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_rule.hrl").

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

role_win(MasterServer, RoleID, AddScore, NewMyFightPower, NewTarFightPower, TarRoleID) ->
    case catch gen_server:call({global, MasterServer}, {add_score, RoleID, AddScore, NewMyFightPower, NewTarFightPower, TarRoleID}) of
        {ok, NewRoleRule, NewTarInfo} ->
            {ok, NewRoleRule, NewTarInfo};
        Exception ->
            ?ERR("Exception:~w", [Exception]),
            {ok, ?undefined}
    end.

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {session_id=0, next_status_left_seconds,master_node, master_server,
                slave_node_list=[], slave_server_list=[],
                is_all_connect=false, noticed_server_list=[], lastRankList=[]}).

-define(DUMP_INTERVAL, (1000 * 60 * 10)).
-define(CHECK_NODE_CONNECTION_INTERVAL, 1000).

-define(lowest_rank,lowest_rank). % 最低排名 
-define(role_rule_info, role_rule_info).
-define(rank_to_role_id, rank_to_role_id).

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
    case db_sql:get_etc(?DB_ETC_KEY_RULE) of
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
    State = OldState#state{slave_node_list=SlaveNodeList,slave_server_list=SlaveServerList,
                           master_node=MasterNode, master_server=MasterServerName,noticed_server_list=[]},
    erlang:send(erlang:self(), check_node_connection),
    %% 何时dump数据由主服务器控制
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    NewState = init_state(State),
    {ok, NewState}.

init_state(State) ->
    Date = erlang:date(),
    NowDayWeek = calendar:day_of_the_week(Date),
    NextTimestamp = util:datetime_to_seconds({Date, {0, 0, 0}}) + ?ONE_DAY_SECONDS * (8 - NowDayWeek),
    LeftSeconds = NextTimestamp - util:now(),
    erlang:send_after(LeftSeconds * 1000, erlang:self(), reset_rule),
    ?ERR("NextTimestamp:~w", [NextTimestamp]),
    State#state{next_status_left_seconds=LeftSeconds}.

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
handle_call({get_role_rule_tar_rule, RoleID}, _From, State) ->
    {reply, catch get_role_rule_tar_rule(RoleID), State};
handle_call({gen_fighterNew, RoleID}, _From, State) ->
    {reply, catch gen_fighterNew(RoleID), State};
handle_call({add_score, RoleID, AddScore, NewMyFightPower, NewTarFightPower, TarRoleID}, _From, State) ->
    {reply, catch add_score(RoleID, AddScore, NewMyFightPower, NewTarFightPower, TarRoleID), State};
handle_call(get_rule_rank, _From, State) ->
    {reply, {ok, get_rank_list()}, State};
handle_call(get_rule_last_rank, _From, #state{lastRankList=LastRankList}=State) ->
    {reply, {ok, LastRankList}, State};
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
do_handle_info({get_rule_info, RoleInfo, ServerID}, State) ->
    get_rule_info(RoleInfo, ServerID),
    {noreply, State};
do_handle_info({do_rule_sign, RoleID, RoleInfo, ServerID}, State) ->
    do_rule_sign(RoleID, RoleInfo, ServerID),
    {noreply, State};
do_handle_info(reset_rule, #state{session_id=SessionID}=State) ->
    ?ERR("reset_rule", []),
    OldRankList = get_rank_list(),
    AllRankData = get_all_rank_data(),
    init_dict_data(),
    NewState = init_state(State),
    NewState2 = NewState#state{session_id=SessionID+1,lastRankList=OldRankList},
    {noreply, NewState3} = do_update_server_list(NewState2),
    send_msg_to_slave(NewState3#state.slave_server_list, reset_rule),
    erlang:send_after(data_rule:get(delay_seconds) * 1000, erlang:self(), {send_reward, AllRankData}),
    {noreply, NewState3};
do_handle_info({send_reward, AllRankData}, State) ->
    send_reward(AllRankData),
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
do_handle_info(dump_data, State) ->
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    do_persist(State),
    send_msg_to_slave(State#state.slave_server_list, dump_data),
    send_msg_to_slave(State#state.slave_server_list, {check_other, data_setting:get(cross_slave_server_list)}),
    {noreply, State};
do_handle_info(update_server_list, State) ->
    do_update_server_list(State);
do_handle_info(check_other, State) ->
    send_msg_to_slave(State#state.slave_server_list, {check_other, data_setting:get(cross_slave_server_list)}),
    {noreply, State};
do_handle_info(Info, State) ->
    ?ERR("unkown info:~w", [Info]),
    {noreply, State}.

do_persist(State) ->
    Info = [E || E<-erlang:get(), is_persist(E) ],
    Info2 = [{state,State} | Info],
    db_sql:set_etc(?DB_ETC_KEY_RULE,Info2).

is_persist({{?role_rule_info, _}, _}) ->
    true;
is_persist({{?rank_to_role_id, _}, _}) ->
    true;
is_persist({?lowest_rank, _}) ->
    true;
is_persist(_) ->
    false.

init_dict_data() ->
    case erlang:get(?lowest_rank) of
        ?undefined ->
            RandomRankList = [];
        LowestRank ->
            RandomRankList = util:random_list_quick(lists:seq(1, LowestRank))
    end,
    lists:foldr(
      fun(E, AccRandomRankList) ->
              case E of
                  {{?role_rule_info, RoleID}, RoleRule} ->
                      case AccRandomRankList of
                          [] ->
                              NewTarRoleID = RoleID - (RoleID rem 1000000) + random:uniform(50),
                              erlang:put({?role_rule_info, RoleID},
                                         RoleRule#role_rule{score=0, winTimes=0, fightTimes=0, tarRoleID=NewTarRoleID,
                                                            winConTimes=0, winConMaxTimes=0, lastIsWin=false}),
                              AccRandomRankList;
                          [RandomRank|AccRandomRankList2] ->
                              #role_rule{roleID=TarRoleID} = get_role_rule_by_rank(RandomRank),
                              erlang:put({?role_rule_info, RoleID},
                                         RoleRule#role_rule{score=0, winTimes=0, fightTimes=0, tarRoleID=TarRoleID,
                                                            winConTimes=0, winConMaxTimes=0, lastIsWin=false}),
                              AccRandomRankList2
                      end;
                  _ ->
                      AccRandomRankList
              end
      end, RandomRankList,  erlang:get()).

%%--------------------------------------------------------------------------------------------------------------------------
get_rule_info(#role{roleID=RoleID}=RoleInfo, ServerID) ->
    case get_role_rule(RoleID) of
        ?undefined ->
            add_role_rule(RoleInfo, ServerID);
        _ ->
            update_role_rule(RoleInfo)
    end,
    send_msg_to_slave_sever_id(ServerID, {get_rule_info_return, get_fighter(RoleID), get_role_rule(RoleID)}).

do_rule_sign(RoleID, RoleInfo, ServerID) ->
    case get_role_rule(RoleID) of
        ?undefined ->
            ?ERR("RoleID:~w sign succ", [RoleID]),
            add_role_rule(RoleInfo, ServerID);
        _ ->
            ignore
    end.

get_tar_role_id(RoleID) ->
    case get_role_rule(RoleID) of
        ?undefined ->
            0;
        #role_rule{tarRoleID=TarRoleID} ->
            TarRoleID
    end.

send_reward(AllRankData) ->
    send_reward_normal_rank(AllRankData).

send_reward_normal_rank(NormalRankList) ->
    lists:foreach(fun({Min, Max, SellReward}) ->
                          send_reward_normal_rank2(Min, Max, SellReward, NormalRankList)
                  end, data_rule:get(normal_rank_reward_list)).

send_reward_normal_rank2(Min, Max, SellReward, NormalRankList) ->
    NormalRankList2 =
        lists:filter(fun(#role_rule{rank=Rank}) ->
                             Rank >= Min andalso Rank =< Max
                     end, NormalRankList),
    lists:foreach(fun(#role_rule{roleID=RoleID,score=Score,rank=Rank,serverID=ServerID}) ->
                          send_msg_to_slave_sever_id(ServerID,{send_mail_reward, RoleID, SellReward, 1075, [Score, Rank]})
                  end, NormalRankList2).

add_score(RoleID, AddScore, NewMyFightPower, NewTarFightPower, TarRoleID) ->
    update_fight_power(RoleID, NewMyFightPower),
    update_fight_power(TarRoleID, NewTarFightPower),
    update_for_fight(RoleID, AddScore),
    case AddScore > 0 of
        true ->
            udpate_rank(RoleID);
        false ->
            next
    end,
    NewTarInfo = gen_fighter(get_role_rule(RoleID)),
    {ok, get_role_rule(RoleID), NewTarInfo}.

udpate_rank(RoleID) ->
    #role_rule{score=RoleNewScore,rank=RoleOldRank} = RoleRule = get_role_rule(RoleID),
    RoleNewRank = find_new_rank(RoleNewScore, RoleOldRank),
    case RoleNewRank < RoleOldRank of
        true ->
            lists:foreach(fun(RankT) ->
                                  RoleRuleT = get_role_rule_by_rank(RankT),
                                  set_role_rule(RoleRuleT#role_rule{rank=RankT+1}),
                                  set_role_rank_to_role_id(RankT+1, RoleRuleT#role_rule.roleID)
                          end, lists:reverse(lists:seq(RoleNewRank, RoleOldRank - 1))),
            set_role_rule(RoleRule#role_rule{rank=RoleNewRank}),
            set_role_rank_to_role_id(RoleNewRank, RoleID);
        false ->
            ignore
    end.

find_new_rank(_RoleNewScore, 1) ->
    1;
find_new_rank(RoleNewScore, RoleOldRank) ->
    find_new_rank2(RoleNewScore, RoleOldRank - 1).

find_new_rank2(SpecScore, NowRank) ->
    case get_role_rule_by_rank(NowRank) of
        ?undefined ->
            NowRank + 1;
        #role_rule{score=RoleScore} ->
            case SpecScore > RoleScore of
                true ->
                    find_new_rank2(SpecScore, NowRank - 1);
                false ->
                    NowRank + 1
            end
    end.

get_rank_list() ->
    lists:foldr(fun(Rank, Acc) ->
                        case get_role_rule_by_rank(Rank) of
                            ?undefined ->
                                Acc;
                            RoleRule ->
                                [RoleRule|Acc]
                        end
                end, [], lists:seq(1, data_rule:get(rank_num))).

%% -------------------------------------------------------------------------------------------------------------------------

get_fighter(RoleID) ->
    #role_rule{tarRoleID=TarRoleID} = RoleRule = get_role_rule(RoleID),
    case is_robot_id(TarRoleID) of
        false ->
            case TarRoleID =:= 0 of
                true ->
                    gen_fighter(RoleRule);
                false ->
                    get_role_rule(TarRoleID)
            end;
        true ->
            case get_role_rule(TarRoleID) of
                ?undefined ->
                    TarRoleID;
                TarRule ->
                    TarRule
            end
    end.

gen_fighter(#role_rule{roleID=RoleID}=RoleRule) ->
    Rank = get_role_rank(RoleID),
    RankList = get_rank_list(Rank),
    case RankList of
        [] ->
            NewTarRoleID = RoleID - (RoleID rem 1000000) + random:uniform(50),
            set_role_rule(RoleRule#role_rule{tarRoleID=NewTarRoleID}),
            NewTarRoleID;
        _ ->
            TarRoleRule = get_role_rule_by_rank(lists:nth(random:uniform(erlang:length(RankList)), RankList)),
            set_role_rule(RoleRule#role_rule{tarRoleID=TarRoleRule#role_rule.roleID}),
            TarRoleRule
    end.

gen_fighterNew(RoleID) ->
	?ERR("Err-----gen_fighterNew ...........:~w", [RoleID]),
	#role_rule{tarRoleID=TarRoleID} = RoleRule = get_role_rule(RoleID),
    Rank = get_role_rank(RoleID),
    RankList = get_rank_list(Rank),
	?ERR("Err-----gen_fighterNew111 ...........:~w", [RoleID]),
    case RankList of
        [] ->
			?ERR("Err-----gen_fighterNew333...........:~w", [RankList]),
            NewTarRoleID = RoleID - (RoleID rem 1000000) + random:uniform(50),
            set_role_rule(RoleRule#role_rule{tarRoleID=NewTarRoleID}),
            NewTarRoleID;
        _ ->
			?ERR("Err-----gen_fighterNew4444...........:~w", [RankList]),
            TarRoleRule = get_role_rule_by_rank(lists:nth(random:uniform(erlang:length(RankList)), RankList)),
            set_role_rule(RoleRule#role_rule{tarRoleID=TarRoleRule#role_rule.roleID}),
            TarRoleRule
    end.

is_robot_id(TarRoleID) ->
    Rem = TarRoleID rem 1000000,
    Rem >= 1 andalso Rem =< 9999.

get_rank_list(Rank) when Rank > 5 ->
    HighRank = Rank - 5,
    LowRank = Rank + 5,
    HighList = lists:seq(HighRank, Rank - 1),
    LowestRank = get_lowest_rank(),
    case LowestRank >= LowRank of
        true ->
            HighList ++ lists:seq(Rank + 1, LowRank);
        false ->
            case LowestRank =:= Rank of
                true ->
                    List2 = HighList,
                    NeedHighNum = 5;
                false ->
                    List2 = HighList ++ lists:seq(Rank + 1, LowestRank),
                    NeedHighNum = LowRank - LowestRank
            end,
            case HighRank =:= 1 of
                true ->
                    List2;
                false ->
                    HighRank2 = HighRank - NeedHighNum,
                    case HighRank2 < 0 of
                        true ->
                            List2 ++ lists:seq(1, HighRank - 1);
                        false ->
                            List2 ++ lists:seq(HighRank2, HighRank - 1)
                    end
            end
    end;
get_rank_list(Rank) when Rank =< 5 ->
    LowestRank = get_lowest_rank(),
    case LowestRank > 11 of
        true ->
            lists:seq(1, 11) -- [Rank];
        false ->
            lists:seq(1, LowestRank) -- [Rank]
    end.

get_role_rank(RoleID) ->
    #role_rule{rank=Rank} = get_role_rule(RoleID),
    Rank. 

get_role_rule(RoleID) ->
    erlang:get({?role_rule_info, RoleID}).

set_role_rule(#role_rule{roleID=RoleID}=RoleAlien) ->
    erlang:put({?role_rule_info, RoleID}, RoleAlien).

set_role_rank_to_role_id(RoleRank, RoleID) ->
    erlang:put({?rank_to_role_id, RoleRank}, RoleID).

get_role_rule_by_rank(RoleRank) ->
    case erlang:get({?rank_to_role_id, RoleRank}) of
        ?undefined ->
            ?undefined;
        RoleID ->
            erlang:get({?role_rule_info, RoleID})
    end.

add_role_rule(RoleInfo, ServerID) ->
    #role{roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName} = RoleInfo,
    Rank = gen_lowest_rank(),
    set_role_rank_to_role_id(Rank, RoleID),
    set_role_rule(#role_rule{roleID=RoleID,
                             fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,
                             rank = Rank, score = 0, serverID = ServerID}).

update_fight_power(RoleID, FightPower) ->
    case get_role_rule(RoleID) of
        ?undefined ->
            ignore;
        RoleRule ->
            set_role_rule(RoleRule#role_rule{fightPower=FightPower})
    end.

update_role_rule(RoleInfo) ->
    #role{roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName} = RoleInfo,
    RoleRule = get_role_rule(RoleID),
    set_role_rule(RoleRule#role_rule{fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName}).

update_for_fight(RoleID, AddScore) ->
    #role_rule{score=Score,winTimes=WinTimes, fightTimes=FightTimes, winConTimes=WinConTimes,winConMaxTimes=WinConMaxTimes,lastIsWin=LastIsWin} = RoleRule = get_role_rule(RoleID),
    case AddScore > 0 of
        true ->
            case LastIsWin of
                true ->
                    NewWinConTimes = WinConTimes + 1;
                false ->
                    NewWinConTimes = 1
            end,
            case NewWinConTimes > WinConMaxTimes of
                true ->
                    NewWinConMaxTimes = NewWinConTimes;
                false ->
                    NewWinConMaxTimes = WinConMaxTimes
            end,
            set_role_rule(RoleRule#role_rule{score=Score+AddScore,winTimes=WinTimes + 1, fightTimes=FightTimes + 1, winConTimes=NewWinConTimes,winConMaxTimes=NewWinConMaxTimes,lastIsWin=true});
        false ->
            NewWinConTimes = 0,
            NewWinConMaxTimes = WinConMaxTimes,
            set_role_rule(RoleRule#role_rule{winTimes=WinTimes, fightTimes=FightTimes + 1, winConTimes=NewWinConTimes,winConMaxTimes=NewWinConMaxTimes,lastIsWin=false})
    end.

get_role_rule_tar_rule(RoleID) ->
	?ERR("Err-----get_role_rule_tar_rule ...........:~w", [RoleID]),
    case get_role_rule(RoleID) of
        ?undefined ->
			?ERR("Err-----get_role_rule_tar_rule  undefined ...........:~w", [RoleID]),
            {ok, ?undefined};
        RoleRule ->
			?ERR("Err-----get_role_rule_tar_rule   ...........:~w", [RoleRule]),
            TarRoleID = RoleRule#role_rule.tarRoleID,
            case TarRoleID =/= 0 of
                true ->
                    case get_role_rule(TarRoleID) of
                        ?undefined ->
                            {ok, RoleRule, ?undefined};
                        TarRoleRule ->
                            {ok, RoleRule, TarRoleRule}
                    end;
                false ->
                    {ok, ?undefined}
            end
    end.

get_lowest_rank() ->
    case get(?lowest_rank) of
        ?undefined ->
            0;
        Rank ->
            Rank
    end.

gen_lowest_rank() ->
    case get(?lowest_rank) of
        ?undefined ->
            put(?lowest_rank,1),
            1;
        Rank ->
            NewRank = Rank+1,
            put(?lowest_rank, NewRank),
            NewRank
    end.

get_all_rank_data() ->
    lists:foldr(fun({{?role_rule_info, _}, V2}, Acc) ->
                        [V2|Acc];
                   (_, Acc) ->
                        Acc
                end, [], erlang:get()).

%% -------------------------------------------------------------------------------------------------------------------------
send_msg_to_slave(SlaveServerList, Msg) ->
    lists:foreach(fun(SlaveServerName) ->
                          case global:whereis_name(SlaveServerName) of
                              ?undefined ->
                                  ?ERR("SlaveServerName:~w is undefined, Msg:~w", [SlaveServerName, Msg]);
                              Pid when erlang:is_pid(Pid) ->
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

get_slave_node(SlaveServerIP, SlaveServerID, Platform) ->
    NodeNameList = lists:concat([Platform, SlaveServerID, '@', SlaveServerIP]),
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
           next_status_left_seconds=NextStatusLeftSeconds,session_id=SessionID,
           noticed_server_list=NoticedServerList} = State,
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
                                  global:send(SlaveServerName, {update_server_list,MasterNode,MasterServer,NextStatusLeftSeconds,SessionID}),
                                  {AccExistServerNum + 1, [SlaveServerName|AccNoticedServerList]}
                          end
                  end
          end, {0, NoticedServerList}, SlaveServerList),
    IsAllSlaveServerExist = (erlang:length(SlaveServerList) =:= ExistServerNum),
    IsAllConnect = IsAllSlaveNodeConnect andalso IsAllSlaveServerExist,
    State#state{is_all_connect=IsAllConnect, noticed_server_list=NewNoticedServerList}.

update_status_to_slave(#state{session_id=SessionID, next_status_left_seconds=LeftSeconds,
                              slave_server_list=SlaveServerList,master_node=MasterNode, master_server=MasterServerName}) ->
    lists:foreach(fun(SlaveServer) ->
                          case global:whereis_name(SlaveServer) of
                              ?undefined ->
                                  ?ERR("从服务器：~w 不存在，无法同步", [SlaveServer]);
                              _Pid ->
                                  global:send(SlaveServer, {update_status, SessionID, LeftSeconds,MasterNode,MasterServerName})
                          end
                  end, SlaveServerList).

do_update_server_list(State) ->
    SlaveNodeList = get_slave_node_list(),
    SlaveServerList = get_slave_server_list(),
    NewState = State#state{slave_node_list=SlaveNodeList, slave_server_list=SlaveServerList, noticed_server_list=[]},
    lists:foreach(fun(SlaveNode) ->
                     case net_adm:ping(SlaveNode) of
                         pong ->
                             next;
                         _ ->
                             ?ERR("SlaveNode:~w can not connect", [SlaveNode])
                     end
                  end, SlaveNodeList),
    ?ERR("更新server_list成功~n master_node:~w,master_server:~w,slave_node_list:~w,slave_server_list:~w",
         [NewState#state.master_node, NewState#state.master_server, NewState#state.slave_node_list, NewState#state.slave_server_list]),
    {noreply, NewState}.


