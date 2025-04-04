-module(family_manager_server).

-behaviour(gen_server).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_family.hrl").

-define(LOOP_INTERVAL, (1000 * 60 * 10)).
-define(REQUEST_JOIN_TIMEOUT_SECONDS, (3600 * 24 * 3)).

%% API
-export([
         start/0,
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([
        ]).

-record(state, {}).
 
-define(all_family_info_list, all_family_info_list).
-define(create_family_name_cache, create_family_name_cache).


%% 创建城邦信件

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    {ok, _} =
        supervisor:start_child(world_sup,
                               {family_sup,
                                {family_sup, start_link, []},
                                permanent, infinity, supervisor, [family_sup]}),
    {ok, _} =
        supervisor:start_child(world_sup, 
                               {?MODULE,
                                {?MODULE, start_link, []},
                                permanent, 600000, worker, [?MODULE]}).

%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%--------------------------------------------------------------------

init([]) ->
    ets:new(?ETS_FAMILY_SUMMARY, [named_table, set, public,{keypos,2}]),
    ets:new(?ETS_FAMILY_REQUEST, [named_table, bag, public,{keypos,2}]),
    ets:new(?ETS_FAMILY_PROTECT, [named_table, set, public,{keypos,1}]),
    erlang:put(?create_family_name_cache, []),
    init_family_list(),
    init_request_list(),
    erlang:send(self(), loop),
    {ok, #state{}}.

%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->  
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
handle_info({'EXIT', PID, Reason}, State) ->
    ?ERR("~ts: ~w, ~w", ["联盟管理进程收到exit消息", PID, Reason]),
    {noreply, State};
handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w", [Exeption, Info, State]),
            {noreply, State}
    end.


%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================




sort_family_list(FamilyInfoList) ->
    FamilyInfoList2 = lists:sort(fun(#family_info{create_time=CreateTime1, level=Level1, cur_members=CurMembers1},
                                     #family_info{create_time=CreateTime2, level=Level2, cur_members=CurMembers2}) ->  
                                         if
                                             Level1 > Level2 ->
                                                 true;
                                             Level1 < Level2 ->
                                                 false;
                                             CurMembers1 > CurMembers2 ->
                                                 true;
                                             CurMembers1 < CurMembers2 ->
                                                 false;
                                             CreateTime1 < CreateTime2 ->
                                                 true;
                                             true ->
                                                 false
                                         end
                                 end, FamilyInfoList),
    
    {FamilyInfoList3, _} = lists:foldr(fun(FamilyInfo, {AccFamilyInfoList, Rank}) ->
                                               {[FamilyInfo#family_info{rank=Rank}|AccFamilyInfoList], Rank - 1}
                                       end, {[], erlang:length(FamilyInfoList2)}, FamilyInfoList2),
    
    FamilySummaryList = lists:foldr(fun(FamilyInfo, AccFamilySummaryList) ->
                                                 FamilySummary = family_misc:to_p_family_summary(FamilyInfo),
                                                 [FamilySummary|AccFamilySummaryList]
                                         end, [], FamilyInfoList3),
    erlang:put(?all_family_info_list, FamilyInfoList3),
    ets:insert(?ETS_FAMILY_SUMMARY, FamilySummaryList),
    erlang:spawn(fun() -> update_rank_to_each_family(FamilyInfoList3) end).

init_family_list() ->
    FamilyInfoList = db_sql:get_all_family_info(),
    FamilySummaryList = lists:foldr(fun(FamilyInfo, AccFamilySummaryList) ->
                                            FamilySummary = family_misc:to_p_family_summary(FamilyInfo),
                                            [FamilySummary|AccFamilySummaryList]
                                    end, [], FamilyInfoList),
    erlang:put(?all_family_info_list, FamilyInfoList),
    ets:insert(?ETS_FAMILY_SUMMARY, FamilySummaryList).

init_request_list() ->
    RequestList = db_sql:get_all_family_request(),
    ets:insert(?ETS_FAMILY_REQUEST, RequestList).


add_family(FamilyInfo) ->
    FamilyInfoList = erlang:get(?all_family_info_list),
    sort_family_list([FamilyInfo|FamilyInfoList]).


%% 获取联盟列表
do_handle_info({client_msg, RoleID, #cs_family_get_list{start=Start, num=Num}}, State) ->
    do_get_list(RoleID, Start, Num),
    {noreply, State};

%% 添加申请加入联盟的请求
do_handle_info({add_role_family_request, RoleInfo, FamilyID, OwnerRoleID}, State) ->
    #role{roleID=RoleID, roleName=RoleName, level=RoleLevel, fightPower=FightPower} = RoleInfo,
    FamilyRequest = #family_request{role_id=RoleID,role_name=RoleName,level=RoleLevel,fight_power=FightPower,timestamp=util:now(),family_id=FamilyID},
    db_sql:add_role_family_request(FamilyRequest),
    ets:insert(?ETS_FAMILY_REQUEST, FamilyRequest),
    %%?ERR("FamilyRequest:~w", [FamilyRequest]),
    ?unicast(RoleID, #sc_family_request_join{result=0, is_self=true, timestamp=0}),
    ?unicast(OwnerRoleID, #sc_family_request_join{result=0, is_self=false, timestamp=0}),
    {noreply, State};

do_handle_info({del_role_family_request, RoleID, FamilyID, FamilyRequest}, State) ->
    ets:delete_object(?ETS_FAMILY_REQUEST, FamilyRequest),
    db_sql:del_spec_family_request(RoleID, FamilyID),
%%     ?ERR("RoleID:~w, FamilyID:~w", [RoleID, FamilyID]),
    ?unicast(RoleID, #sc_family_cancel_join{result=0, is_self=true}),
    family_misc:router_to_family_process(FamilyID, {send_msg_to_owner, #sc_family_cancel_join{result=0, is_self=false}}),
    {noreply, State};

do_handle_info({refuse_join, RoleID, JoinRoleID, FamilyID, BeRefusedRoleIDList, FamilyRequestList}, State) ->
    lists:foreach(fun(FamilyRequest) -> ets:delete_object(?ETS_FAMILY_REQUEST, FamilyRequest) end, FamilyRequestList),
    case JoinRoleID of
        0 ->
            db_sql:del_family_request(FamilyID);
        _ ->
            db_sql:del_spec_family_request(JoinRoleID, FamilyID)
    end,
    ?unicast(RoleID, #sc_family_refuse_join{result=0, is_self=true}),
    lists:foreach(fun(BeRefusedRoleID) ->
                          ?unicast(BeRefusedRoleID, #sc_family_refuse_join{result=0, is_self=false})
                  end, BeRefusedRoleIDList),
    {noreply, State};

do_handle_info({role_join_family, NewFamilyInfo, FamilyID, JoinRoleID, _FamilyRequest}, State) ->
    del_role_request(JoinRoleID),
    update_family_info(NewFamilyInfo),
    update_role_family_id(JoinRoleID, FamilyID),
    {noreply, State};

do_handle_info({kick, NewFamilyInfo, KickRoleID}, State) ->
    update_family_info(NewFamilyInfo),
    update_role_family_id(KickRoleID, 0),
    {noreply, State};

do_handle_info({update_family_info, NewFamilyInfo}, State) ->
    update_family_info(NewFamilyInfo),
    {noreply, State};

do_handle_info({leave, NewFamilyInfo, RoleID}, State) ->
    update_family_info(NewFamilyInfo),
    update_role_family_id(RoleID, 0),
    {noreply, State};

do_handle_info({disband, FamilyID, RoleID}, State) ->
    update_role_family_id(RoleID, 0),
    del_family_info(FamilyID),
    del_create_family_name(FamilyID),
    {noreply, State};

%% 创建联盟
do_handle_info({do_create, RoleInfo, CostType, CostNum, FamilyName}, State) ->
    do_create(RoleInfo, CostType, CostNum, FamilyName),
    {noreply, State};

do_handle_info(loop, State) ->
    erlang:send_after(?LOOP_INTERVAL, self(), loop),
    loop(),
    {noreply, State};

do_handle_info(Info, State) ->
    ?ERR("~ts:~w", ["未知的消息", Info]),
    {noreply, State}.

loop() ->
    Now = util:now(),
    lists:foreach(fun(#family_request{role_id=RoleID,family_id=FamilyID,timestamp=Timestamp}=FamilyRequest) ->
                          case Timestamp + ?REQUEST_JOIN_TIMEOUT_SECONDS < Now of
                              true ->
                                  ets:delete_object(?ETS_FAMILY_REQUEST, FamilyRequest),
                                  db_sql:del_spec_family_request(RoleID, FamilyID);
                              false ->
                                  next
                          end
                  end, ets:match_object(?ETS_FAMILY_REQUEST, #family_request{_='_'})),
    sort_family_list(erlang:get(?all_family_info_list)).




%%创建联盟
do_create(RoleInfo, CostType, CostNum, FamilyName) ->
    case catch do_create2(RoleInfo, FamilyName) of
        {ok, FamilyInfo} ->
            RoleID = RoleInfo#role.roleID,
            role_lib:send_server(RoleID, {create_family_succ, family_misc:to_p_family_info(FamilyInfo)}),
            add_family(FamilyInfo),
            del_role_request(RoleID);
        ErrorInfo ->
            ?ERR("创建失败:ErrorInfo=~w", [ErrorInfo]),
            role_lib:send_server(RoleInfo#role.roleID, {create_family_fail,  CostType, CostNum})
    end.

do_create2(RoleInfo, FamilyName) ->
    FamilyID = tk_id:gen_familyID(),
    add_create_family_name(FamilyName, FamilyID),
    #role{roleID=RoleID, roleName=RoleName, title=Title, isMale=IsMale, level=RoleLevel, fightPower=FightPower} = RoleInfo,
    NewMember =
        #family_member_info{
                            role_id=RoleID
                            ,role_name=RoleName
                            ,family_id=FamilyID
                            ,family_contribution=0
                            ,left_family_contribution=0
                            ,use_gold_time=0
                            ,title=Title
                            ,is_male=IsMale
                            ,online=true
                            ,role_level=RoleLevel
                            ,fight_power=FightPower
                            ,family_title=?FAMILY_TITLE_OWNER
                            ,join_time=util:now()
                           },
    Members = [NewMember],
    FamilyInfo =
        #family_info{
                     family_id=FamilyID
                     ,family_name=FamilyName
                     ,level=1
                     ,create_role_id=RoleID
                     ,create_role_name=RoleName
                     ,owner_role_id=RoleID
                     ,owner_role_name=RoleName
                     ,cur_members=1
                     ,active_points=0
                     ,notice= data_family:get(default_notice)
                     ,members=Members
                     ,rank= erlang:length(erlang:get(?all_family_info_list)) + 1
                     ,create_time=util:now()},
    {ok, _Pid} = supervisor:start_child(family_sup,
                                        {family_misc:make_family_process_name(FamilyID) , 
                                         {family_server, start_link, [FamilyID, FamilyInfo]},
                                         transient, 300000, worker, [family_server]}),
    {ok, FamilyInfo}.

do_get_list(RoleID, Start, Num) ->
    FamilyInfoList = erlang:get(?all_family_info_list),
    Length = erlang:length(FamilyInfoList),
    case Start + 1 > Length of
        false ->
            FamilyInfoList2 = lists:sublist(FamilyInfoList, Start + 1, Num),
            FamilySummaryList = lists:map(fun(FamilyInfo) -> family_misc:to_p_family_summary(FamilyInfo) end, FamilyInfoList2),
            FamilySummaryList2 = update_family_summary_for_role_request(FamilySummaryList, RoleID),
%%             ?ERR("FamilySummaryList2:~w", [FamilySummaryList2]),
            ?unicast(RoleID, #sc_family_get_list{result=0, family_list=FamilySummaryList2});
        true ->
            case Length =:= 0 of
                true ->
                    ?unicast(RoleID, #sc_family_get_list{result=0, family_list=[]});
                false ->
                    ?unicast(RoleID, #sc_family_get_list{result=1, family_list=[]})
            end
    end.


update_family_summary_for_role_request(FamilySummaryList, RoleID) ->
    RoleRequestList = ets:lookup(?ETS_FAMILY_REQUEST, RoleID),
%%     ?ERR("RoleRequestList:~w, RoleID:~w", [RoleRequestList, RoleID]),
    lists:map(fun(#p_family_summary{family_id=FamilyID}=FamilySummary) ->
                      case lists:keyfind(FamilyID, #family_request.family_id, RoleRequestList) of
                          false ->
                              FamilySummary#p_family_summary{is_request=false};
                          _ ->
                              FamilySummary#p_family_summary{is_request=true}
                      end
              end, FamilySummaryList).

del_family_info(FamilyID) ->
    ets:delete(?ETS_FAMILY_SUMMARY, FamilyID),
    sort_family_list(lists:keydelete(FamilyID, #family_info.family_id, erlang:get(?all_family_info_list))).

update_family_info(NewFamilyInfo) ->
    FamilyID = NewFamilyInfo#family_info.family_id,
    FamilyInfoList = erlang:get(?all_family_info_list),
    NewFamilySummary = family_misc:to_p_family_summary(NewFamilyInfo),
    NewFamilyInfoList =
        case lists:keyfind(FamilyID, #family_info.family_id, FamilyInfoList) of
            false ->
                FamilyInfoList ++ [NewFamilyInfo];
            _ ->
                lists:keyreplace(FamilyID, #family_info.family_id, FamilyInfoList, NewFamilyInfo)
        end,
    erlang:put(?all_family_info_list, NewFamilyInfoList),
    ets:insert(?ETS_FAMILY_SUMMARY, NewFamilySummary).

update_role_family_id(JoinRoleID, FamilyID) ->
    case role_lib:is_online(JoinRoleID) of
        true ->
            role_lib:send_server(JoinRoleID, {update_family_id, FamilyID});
        false ->
            case FamilyID of
                0 ->
                    db_sql:update_role_family_id(JoinRoleID, FamilyID, util:now());
                _ ->
                    db_sql:update_role_family_id(JoinRoleID, FamilyID)
            end,
            family_misc:unlock_family_protect(JoinRoleID)
    end.

update_rank_to_each_family(FamilyInfoList) ->
    lists:foreach(fun(#family_info{rank=Rank, family_id=FamilyID}) ->
                          FamilyPName = family_misc:make_family_process_name(FamilyID),
                          case erlang:whereis(FamilyPName) of
                              ?undefined ->
                                  db_sql:update_family_rank(FamilyID, Rank);
                              _ ->
                                  erlang:send(FamilyPName, {update_family_rank, Rank})
                          end
                  end, FamilyInfoList).

del_role_request(RoleID) ->
    lists:foreach(
      fun(#family_request{family_id=FamilyID} = FamilyRequest) ->
              ets:delete_object(?ETS_FAMILY_REQUEST, FamilyRequest),
              db_sql:del_spec_family_request(RoleID, FamilyID),
              FamilyPName = family_misc:make_family_process_name(FamilyID),
              case erlang:whereis(FamilyPName) of
                  ?undefined ->
                      next;
                  _ ->
                      erlang:send(FamilyPName, {send_msg_to_owner, #sc_family_del_request{role_id=RoleID}})
              end
      end, ets:match_object(?ETS_FAMILY_REQUEST, #family_request{role_id=RoleID,_='_'})).

add_create_family_name(FamilyName, FamilyID) ->
    List = erlang:get(?create_family_name_cache),
    case lists:keyfind(FamilyName, 1, List) of
        false ->
            erlang:put(?create_family_name_cache, [{FamilyName, FamilyID}|List]);
        _ ->
            erlang:throw(family_name_used)
    end.

del_create_family_name(FamilyID) ->
    List = erlang:get(?create_family_name_cache),
    List2 = lists:keydelete(FamilyID, 2, List),
    erlang:put(?create_family_name_cache, List2).




