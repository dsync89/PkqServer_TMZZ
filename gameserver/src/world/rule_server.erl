-module(rule_server).

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

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {session_id=0,next_status_left_seconds,master_node, master_server}).

-define(CHECK_NODE_CONNECTION_INTERVAL, 20000).

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
    case db_sql:get_etc(?DB_ETC_KEY_RULE) of
        [{state,State}|AllInfo] ->
            lists:foreach(fun({Key,Value}) -> erlang:put(Key, Value) end, AllInfo);
        _ ->
            State = #state{}
    end,
    global:register_name(rule_master_server:get_cross_server_name(data_setting:get(server_id),false, data_setting:get(platform)),erlang:self()),
    erlang:send(self(), check_connection),
    ?ERR("State:~w", [State]),
    {ok, State}.


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
do_handle_info({get_otherRoleFighter, RoleID, TarRoleID, CrossServer}, State) ->
    erlang:spawn(fun() ->
                         case catch role_data:get_otherRoleFighter(TarRoleID) of
                             {TarFighterList, TarLieuAdd} ->
                                 global:send(CrossServer, {get_otherRoleFighter_return, RoleID, TarFighterList,TarLieuAdd});
                             _ ->
                                 global:send(CrossServer, {get_otherRoleFighter_return, RoleID, [],{0,0}})
                         end
                 end),
    {noreply, State};
do_handle_info({get_otherRoleFighter_return, RoleID, TarFighterList,TarLieuAdd}, State) ->
    catch role_lib:send_server(RoleID, {get_otherRoleFighter_return, TarFighterList,TarLieuAdd}),
    {noreply, State};
do_handle_info({send_mail_reward, RoleID, SellReward, MailTemplateID, ArgList}, State) ->
    mail_server:send_sys_mail(RoleID, MailTemplateID, ArgList, "", SellReward),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_rule_fight{}}, State) ->
    cs_rule_fight(RoleID, State),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_rule_rank{}}, State) ->
    cs_rule_rank(RoleID, State),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_rule_last_rank{}}, State) ->
    cs_rule_last_rank(RoleID, State),
    {noreply, State};
do_handle_info({get_rule_info, RoleInfo}, State) ->
    do_get_rule_info(RoleInfo, State),
    {noreply, State};
do_handle_info({get_rule_info_return, TarInfo, RoleRule}, State) ->
    do_get_rule_info_return(TarInfo, RoleRule),
    {noreply, State};
do_handle_info({ger_view_other, RoleID, TarRoleID, CrossServer}, State) ->
    ger_view_other(RoleID, TarRoleID, CrossServer),
    {noreply, State};
do_handle_info({ger_view_other_dtl, RoleID, TarRoleID, CrossServer}, State) ->
    ger_view_other_dtl(RoleID, TarRoleID, CrossServer),
    {noreply, State};
do_handle_info({cross_ger_view_return, SrcRoleID, Reply}, State) ->
    ?unicast(SrcRoleID, Reply),
    {noreply, State};
do_handle_info({cross_ger_view_dtl_return, SrcRoleID, Reply}, State) ->
    ?unicast(SrcRoleID, Reply),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_rule_leave{}}, State) ->
    set_bc_list(lists:delete(RoleID,get_bc_list())),
    {noreply, State};
do_handle_info({role_offline, RoleID}, State) ->
    set_bc_list(lists:delete(RoleID,get_bc_list())),
    {noreply, State};
do_handle_info({rule_sign, RoleInfo}, State) ->
    do_rule_sign(RoleInfo, State),
    {noreply, State};
do_handle_info({update_status, SessionID, LeftSeconds,MasterNode,MasterServerName}, State) ->
    NewState = State#state{session_id=SessionID, next_status_left_seconds=LeftSeconds,
                           master_node=MasterNode, master_server=MasterServerName},
    erlang:put(?update_timestamp, util:now()),
    {noreply, NewState};
do_handle_info(check_connection, State) ->
    erlang:send_after(?CHECK_NODE_CONNECTION_INTERVAL, self(), check_connection),
    NewState =
        case erlang:get(?update_timestamp) of
            ?undefined ->
                State#state{session_id=0,next_status_left_seconds=?undefined,master_node=?undefined, master_server=?undefined};
            Timestamp ->
                case util:now() - Timestamp > (?CHECK_NODE_CONNECTION_INTERVAL div 1000) of
                    true ->
                        State#state{session_id=0,next_status_left_seconds=?undefined,master_node=?undefined, master_server=?undefined};
                    false ->
                        State
                end
        end,
    {noreply, NewState};
do_handle_info(reset_rule, State) ->
    ?ERR("reset_rule", []),
    UpdateTimestamp = erlang:get(?update_timestamp),
    BcList = get_bc_list(),
    erlang:erase(),
    erlang:put(?update_timestamp, UpdateTimestamp),
    set_bc_list(BcList),
    send_bc_list_msg({route, role_rule, #cs_rule_info{}}),
    {noreply, State};
do_handle_info(test_reset_rule, #state{master_server=MasterServer}=State) ->
    catch global:send(MasterServer, reset_rule),
    {noreply, State};
do_handle_info({update_server_list,MasterNode,MasterServer,NextStatusLeftSeconds,SessionID}, State) ->
    erlang:put(?update_timestamp, util:now()),
    do_update_server_list(State,MasterNode,MasterServer,NextStatusLeftSeconds,SessionID);
do_handle_info(dump_data, State) ->
    do_persist(State),
    {noreply, State};
do_handle_info({check_other, CrossSlaveServerList}, State) ->
    check_other(lists:keydelete(data_setting:get(server_id), 1, CrossSlaveServerList)),
    {noreply, State};
do_handle_info(Info, State) ->
    ?ERR("unkown info:~w", [Info]),
    {noreply, State}.

do_persist(State) ->
    Info = [E || E<-erlang:get(), is_persist(E) ],
    Info2 = [{state,State} | Info],
    db_sql:set_etc(?DB_ETC_KEY_RULE,Info2).

is_persist(_) ->
    false.

%% ------------------------------------------------------------------------------------------------------------------------
cs_rule_fight(RoleID, #state{master_server=MasterServer}) ->
    case global:whereis_name(MasterServer) of
        ?undefined ->
            Record = role_rule:get_fight_error_record(3),
            ?unicast(RoleID, Record);
        _ ->
            role_lib:send_server(RoleID, {rule_fight, MasterServer})
    end.

cs_rule_rank(RoleID,  #state{master_server=MasterServer}) ->
    case global:whereis_name(MasterServer) of
        ?undefined ->
            ?unicast(RoleID, #sc_rule_rank{list=[]});
        _ ->
            case catch gen_server:call({global, MasterServer}, get_rule_rank) of
                {ok, List} ->
                    ?unicast(RoleID, #sc_rule_rank{list=[role_rule2p_rule_fighter(E)||E<-List]});
                Exception ->
                    ?ERR("Exception", [Exception]),
                    ?unicast(RoleID, #sc_rule_rank{list=[]})
            end
    end.

cs_rule_last_rank(RoleID,  #state{master_server=MasterServer}) ->
    case global:whereis_name(MasterServer) of
        ?undefined ->
            ?unicast(RoleID, #sc_rule_last_rank{list=[]});
        _ ->
            case catch gen_server:call({global, MasterServer}, get_rule_last_rank) of
                {ok, List} ->
                    ?unicast(RoleID, #sc_rule_last_rank{list=[role_rule2p_rule_fighter(E)||E<-List]});
                Exception ->
                    ?ERR("Exception", [Exception]),
                    ?unicast(RoleID, #sc_rule_last_rank{list=[]})
            end
    end.

do_rule_sign(#role{roleID=RoleID}=RoleInfo, #state{master_server=MasterServer}) ->
    case global:whereis_name(MasterServer) of
        ?undefined ->
            ignore;
        _ ->
            global:send(MasterServer, {do_rule_sign, RoleID, RoleInfo, data_setting:get(server_id)})
    end.

do_get_rule_info(#role{roleID=RoleID}=RoleInfo,
                  #state{master_server=MasterServer}) ->
    case global:whereis_name(MasterServer) of
        ?undefined ->
            ?unicast(RoleID, #sc_rule_info{score=0,rank=0,winTimes=0,fightTimes=0,winConTimes=0,winConMaxTimes=0,
                                           tarFighter= #p_rule_fighter{roleID=0,fightPower=0,isMale=false,title=0,head=0,level=0,roleName= <<"">>,score=0,
                                                                       rank=0,winTimes=0,fightTimes=0,winConTimes=0,winConMaxTimes=0} ,list=[]});
        _ ->
            global:send(MasterServer, {get_rule_info, RoleInfo, data_setting:get(server_id)})
    end.

do_get_rule_info_return(TarInfo, RoleRule) ->
    #role_rule{roleID=RoleID, rank = Rank, score = Score, winTimes=WinTimes,
               fightTimes=FightTimes, winConTimes=WinConTimes,winConMaxTimes=WinConMaxTimes} = RoleRule,
    TarFighter = to_tar_fighter(TarInfo),
    case Rank > data_rule:get(rank_num) of
        true ->
            NewRank = 0;
        false ->
            NewRank = Rank
    end,
    role_lib:send_server(RoleID, {get_rule_hist, #sc_rule_info{score=Score,rank=NewRank,winTimes=WinTimes,fightTimes=FightTimes,
                                   winConTimes=WinConTimes,winConMaxTimes=WinConMaxTimes,tarFighter=TarFighter}}),
    BcList = get_bc_list(),
    set_bc_list([RoleID|lists:delete(RoleID,BcList)]).

ger_view_other(RoleID, TarRoleID, CrossServer) ->
    case erlang:whereis(role_lib:regName(TarRoleID)) of
        Pid when erlang:is_pid(Pid) ->
            role_lib:send_server(TarRoleID, {do_ger_view, RoleID, CrossServer});
        ?undefined ->
            erlang:spawn(fun() -> role_ger:do_offline_ger_view(RoleID, CrossServer, TarRoleID) end)
    end.

ger_view_other_dtl(RoleID, TarRoleID, CrossServer) ->
    case erlang:whereis(role_lib:regName(TarRoleID)) of
        Pid when erlang:is_pid(Pid) ->
            role_lib:send_server(TarRoleID, {do_ger_view_dtl, RoleID, CrossServer});
        ?undefined ->
            erlang:spawn(fun() -> role_ger:do_offline_ger_view_dtl(RoleID, CrossServer, TarRoleID) end)
    end.

to_tar_fighter(TarInfo) ->
    case erlang:is_integer(TarInfo) of
        true ->
            get_robot_fighter(TarInfo);
        false ->
            role_rule2p_rule_fighter(TarInfo)
    end.

role_rule2p_rule_fighter(RoleRule) ->
    #role_rule{roleID=RoleID,fightPower=FightPower, isMale=IsMale, title=Title, head=Head, level = Level,
               roleName = RoleName, rank = Rank, score = Score,
               winTimes=WinTimes, fightTimes=FightTimes, winConTimes=WinConTimes, winConMaxTimes=WinConMaxTimes} = RoleRule,
    #p_rule_fighter{roleID=RoleID,fightPower=FightPower, isMale=IsMale, title=Title, head=Head, level = Level,
               roleName = RoleName, rank = Rank, score = Score,
               winTimes=WinTimes, fightTimes=FightTimes, winConTimes=WinConTimes, winConMaxTimes=WinConMaxTimes}.

get_robot_fighter(RobotRoleID) ->
    case db_sql:get_roleInfo(RobotRoleID) of
        #role{fightPower=FightPower, isMale=IsMale, title=Title, head=Head, level = Level, roleName = RoleName} ->
            #p_rule_fighter{roleID=RobotRoleID,fightPower=FightPower, isMale=IsMale, title=Title, head=Head, level = Level,
                            roleName = RoleName, rank = 0, score = 0,
                            winTimes=0, fightTimes=0, winConTimes=0, winConMaxTimes=0};
        _ ->
            #p_rule_fighter{roleID=0,fightPower=0, isMale=false, title=0, head=0, level = 0,
                            roleName = <<"">>, rank = 0, score = 0,
                            winTimes=0, fightTimes=0, winConTimes=0, winConMaxTimes=0}
    end.

do_update_server_list(State,MasterNode,MasterServer,NextStatusLeftSeconds,SessionID) ->
    NewState = State#state{session_id=SessionID, next_status_left_seconds=NextStatusLeftSeconds,
                           master_node=MasterNode, master_server=MasterServer},
    ?ERR("更新server_list成功~n master_node:~w,master_server:~w",
         [NewState#state.master_node, NewState#state.master_server]),
    {noreply, NewState}.

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

check_other(OtherCrossSlaveServerList) ->
    Platform = data_setting:get(platform),
    lists:foreach(fun({SlaveServerID, SlaveServerIP}) ->
                          OtherServer = rule_master_server:get_cross_server_name(SlaveServerID, false, Platform),
                          case global:whereis_name(OtherServer) of
                              ?undefined ->
                                  OtherNode = rule_master_server:get_slave_node(SlaveServerIP, SlaveServerID, Platform),
                                  net_kernel:disconnect(OtherNode),
                                  net_kernel:connect(OtherNode),
                                  global:sync(),
                                  ?ERR("~w", [global:whereis_name(OtherServer)]);
                              _ ->
                                  next
                          end
                  end, OtherCrossSlaveServerList).

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
    erlang:send(?MODULE, {rule_sign, RoleInfo}).

test_sign_others(Max) when Max >= 1, Max =< 9999 ->
    lists:foreach(fun(RoleID) ->
                          test_sign(RoleID)   
                  end, lists:seq(?ROLE_ID_BASE + 1, ?ROLE_ID_BASE + Max)).

















































