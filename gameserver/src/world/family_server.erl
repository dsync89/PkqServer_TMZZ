-module(family_server).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_family.hrl").
-include("def_mail.hrl").

-define(DUMP_INTERVAL, (1000 * 60 * 5)).
-define(online_role_id_list, online_role_id_list).
-define(RECENT_TALK_DATA_NUM, 10).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         start_link/1,
         start_link/2,
         update_role_name/3
        ]).

start_link(FamilyID) ->
    gen_server:start_link(?MODULE, [FamilyID], []).

start_link(FamilyID, FamilyInfo) ->
    gen_server:start_link(?MODULE, [FamilyID, FamilyInfo], []).

update_role_name(RoleID, FamilyID, NewName) when FamilyID > 0 ->
    FamilyPName = family_misc:make_family_process_name(FamilyID),
    case erlang:whereis(FamilyPName) of
        ?undefined ->
            db_sql:update_family_member_name(RoleID, NewName);
        _ ->
            erlang:send(FamilyPName, {update_role_name, RoleID, NewName})
    end;
update_role_name(_RoleID, _FamilyID, _NewName) ->
    ignore.
        
%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {family_info}).

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
init([FamilyID, FamilyInfo]) ->
    ?DEBUG("~ts:~w", ["初始化联盟进程", FamilyID]),
    erlang:process_flag(trap_exit, true),
    erlang:register(family_misc:make_family_process_name(FamilyID), self()),
    db_sql:set_family_info(FamilyInfo),
    init2(FamilyInfo);
init([FamilyID]) ->
    ?DEBUG("~ts:~w", ["初始化联盟进程", FamilyID]),
    erlang:process_flag(trap_exit, true),
    erlang:register(family_misc:make_family_process_name(FamilyID), self()),
    FamilyInfo = db_sql:get_family_info(FamilyID),
    init2(FamilyInfo).

init2(FamilyInfo) ->
    #family_info{members=Members} = FamilyInfo,
    NewMembers = init_online(Members),
    init_online_role_id_list(NewMembers),
    State = #state{family_info=FamilyInfo#family_info{members=NewMembers}},
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), loop),
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
handle_info({'EXIT', PID, Reason}, State) ->
    ?ERR("~ts: ~w, ~w", ["联盟进程收到exit消息", PID, Reason]),
    {noreply, State};
handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        {stop, normal, NewState} ->
            {stop, normal, NewState};
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
terminate(Reason, State) ->
    case erlang:is_record(State#state.family_info, family_info) of
        true ->
            db_sql:set_family_info(State#state.family_info);
        false ->
            next
    end,
    case Reason of
        shutdown ->
            ?DEBUG("Reason:~w, State:~w", [Reason, State]);
        _ ->
            ?ERR("Reason:~w, State:~w", [Reason, State])
    end,
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

init_online(Members) ->
    lists:map(fun(#family_member_info{role_id=RoleID}=Member) ->
                      Member#family_member_info{online=role_lib:is_online(RoleID)}
              end, Members).

init_online_role_id_list(Members) ->
    RoleIDList =
        lists:foldr(fun(#family_member_info{role_id=RoleID, online=IsOnline}, Acc) ->
                            case IsOnline of
                                true ->
                                    [RoleID|Acc];
                                false ->
                                    Acc
                            end     
                    end, [], Members),
    erlang:put(?online_role_id_list, RoleIDList).

update_online(RoleID, IsOnline) when erlang:is_boolean(IsOnline) ->
    OnlineRoleIDList = erlang:get(?online_role_id_list),
    case IsOnline of
        true ->
            erlang:put(?online_role_id_list, [RoleID|lists:delete(RoleID, OnlineRoleIDList)]);
        false ->
            erlang:put(?online_role_id_list, lists:delete(RoleID, OnlineRoleIDList))
    end.


update_online(RoleID, IsOnline, Members) when erlang:is_boolean(IsOnline) ->
    OnlineRoleIDList = erlang:get(?online_role_id_list),
    case IsOnline of
        true ->
            erlang:put(?online_role_id_list, [RoleID|lists:delete(RoleID, OnlineRoleIDList)]);
        false ->
            erlang:put(?online_role_id_list, lists:delete(RoleID, OnlineRoleIDList))
    end,
    case lists:keytake(RoleID, #family_member_info.role_id, Members) of
        {value, Member, Members2} ->
            [Member#family_member_info{online=IsOnline}|Members2];
        false ->
            Members
    end.

do_update_role_name(RoleID, NewName, Members) ->
    case lists:keytake(RoleID, #family_member_info.role_id, Members) of
        {value, Member, Members2} ->
            [Member#family_member_info{role_name=NewName}|Members2];
        false ->
            Members
    end.

do_handle_info({update_role_online, RoleID, IsOnline}, #state{family_info=FamilyInfo}) ->
    #family_info{members=Members} = FamilyInfo,
    NewMembers = update_online(RoleID, IsOnline, Members),
    {noreply, #state{family_info=FamilyInfo#family_info{members=NewMembers}}};

do_handle_info({update_role_name, RoleID, NewName}, #state{family_info=FamilyInfo}) ->
    #family_info{members=Members} = FamilyInfo,
    NewMembers = do_update_role_name(RoleID, NewName, Members),
    {noreply, #state{family_info=FamilyInfo#family_info{members=NewMembers}}};

do_handle_info({bc, Data}, #state{family_info=FamilyInfo}) ->
    {ok, NewFamilyInfo} = do_bc(Data, FamilyInfo),
    {noreply, #state{family_info=NewFamilyInfo}};

do_handle_info({get_family_recent_talk, RoleID}, #state{family_info=#family_info{talk_data=TalkData}}=State) ->
    ?unicast(RoleID, #sc_talk_recent_list{list=TalkData,channel=?CHAT_CHANNEL_FAMILY}),
    {noreply, State};

do_handle_info({agree_join, RoleID, FamilyID, JoinRoleID, FamilyRequest}, #state{family_info=FamilyInfo}) ->
    {ok, NewFamilyInfo} = do_agree_join(FamilyInfo, RoleID, FamilyID, JoinRoleID, FamilyRequest),
    {noreply, #state{family_info=NewFamilyInfo}};

do_handle_info({kick, RoleID, KickRoleID}, #state{family_info=FamilyInfo}) ->
    {ok, NewFamilyInfo} = do_kick(FamilyInfo, RoleID, KickRoleID),
    {noreply, #state{family_info=NewFamilyInfo}};

do_handle_info({leave, RoleID}, #state{family_info=FamilyInfo}=State) ->
    {ok, NewFamilyInfo} = do_leave(FamilyInfo, RoleID),
    case NewFamilyInfo of
        ?undefined ->
            {stop, normal, State#state{family_info=NewFamilyInfo}};
        _ ->
            {noreply, State#state{family_info=NewFamilyInfo}}
    end;

do_handle_info({refuse_join, RoleID, JoinRoleID}, #state{family_info=FamilyInfo}=State) ->
    do_refuse_join(FamilyInfo, RoleID, JoinRoleID),
    {noreply, State};

do_handle_info({get_family_info, RoleID}, #state{family_info=FamilyInfo}=State) ->
    %%?ERR("RoleID:~w, FamilyInfo:~w", [RoleID, FamilyInfo]),
%%     ?ERR("PFamilyInfo:~w", [family_misc:to_p_family_info(FamilyInfo)]),
    ?unicast(RoleID, #sc_family_get_info{result=0, family_info=family_misc:to_p_family_info(FamilyInfo)}),
    {noreply, State};

do_handle_info({change_notice, RoleID, Notice}, #state{family_info=FamilyInfo}) ->
    {ok, NewFamilyInfo} = do_change_notice(RoleID, Notice, FamilyInfo),
    erlang:send(family_manager_server, {update_family_info, NewFamilyInfo}),
    {noreply, #state{family_info=NewFamilyInfo}};

do_handle_info({send_msg_to_owner, Msg}, #state{family_info=#family_info{owner_role_id=OwnerRoleID}}=State) ->
    ?unicast(OwnerRoleID, Msg),
    {noreply, State};

do_handle_info({update_family_rank, Rank}, #state{family_info=FamilyInfo}=State) ->
    NewFamilyInfo = FamilyInfo#family_info{rank=Rank},
    {noreply, State#state{family_info=NewFamilyInfo}};

do_handle_info(loop, State) ->
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), loop),
    FamilyInfo = State#state.family_info,
    case erlang:is_record(FamilyInfo, family_info) of
        true ->
            NewFamilyInfo = update_family_member_info(FamilyInfo),
            db_sql:set_family_info(NewFamilyInfo),
            {noreply, State#state{family_info=NewFamilyInfo}};
        false ->
            {stop, normal, State}
    end;

do_handle_info(stop, State) ->
    {stop, normal, State};

do_handle_info(Info, State) ->
    ?ERR("Info:~w, State:~w", [Info, State]),
    {noreply, State}.


do_agree_join(FamilyInfo, RoleID, FamilyID, JoinRoleID, FamilyRequest) ->
    case catch check_can_agree_join(FamilyInfo, RoleID, JoinRoleID) of
        {ok, JoinRoleInfo} ->
            do_agree_join2(FamilyInfo, FamilyID, JoinRoleInfo, RoleID, JoinRoleID, FamilyRequest);
        {false, Reason} ->
%%             ?ERR("Reason:~w", [Reason]),
            family_misc:unlock_family_protect(JoinRoleID),
            ?unicast(RoleID, #sc_family_agree_join{result=Reason,is_self=true,family_info=family_misc:gen_p_family_info()}),
            {ok, FamilyInfo}
    end.

do_agree_join2(FamilyInfo, FamilyID, JoinRoleInfo, RoleID, JoinRoleID, FamilyRequest) ->
    #family_info{family_name=FamilyName, members=Members} = FamilyInfo,
    #role{roleID=JoinRoleID, roleName=JoinRoleName, title=JoinTitle, isMale=JoinIsMale, level=JoinRoleLevel, fightPower=JoinFightPower} = JoinRoleInfo,
    NewMember =
        #family_member_info{
                            role_id=JoinRoleID
                            ,role_name=JoinRoleName
                            ,family_id=FamilyID
                            ,family_contribution=0
                            ,left_family_contribution=0
                            ,use_gold_time=0
                            ,title=JoinTitle
                            ,is_male=JoinIsMale
                            ,online=true
                            ,role_level=JoinRoleLevel
                            ,fight_power=JoinFightPower
                            ,family_title=?FAMILY_TITLE_MEMBER
                            ,join_time=util:now()
                           },
    NewMembers = [NewMember|Members],
    NewFamilyInfo = FamilyInfo#family_info{members=NewMembers, cur_members=erlang:length(NewMembers)},
    db_sql:set_family_info(NewFamilyInfo),
    PFamilyInfo = family_misc:to_p_family_info(NewFamilyInfo),
%%     ?ERR("RoleID:~w, PFamilyInfo:~w", [RoleID, PFamilyInfo]),
    ?unicast(RoleID, #sc_family_agree_join{result=0, is_self=true, family_info=PFamilyInfo}),
    update_online(JoinRoleID, role_lib:is_online(JoinRoleID)),
    do_bc_msg_except(#sc_family_agree_join{result=0, is_self=false, family_info=PFamilyInfo}, [RoleID]),
    erlang:send(family_manager_server, {role_join_family, NewFamilyInfo, FamilyID, JoinRoleID, FamilyRequest}),
    catch mail_server:send_sys_mail(JoinRoleID, ?MAIL_FAMILY_BE_AGREE, [FamilyName], "", []),
    {ok, NewFamilyInfo}.

check_can_agree_join(FamilyInfo, RoleID, JoinRoleID) ->
    #family_info{level=FamilyLevel, owner_role_id=OwnerRoleID, cur_members=CurMembers, members=Members} = FamilyInfo,
    case RoleID =:= OwnerRoleID of
        false ->
            erlang:throw({false, 3});
        true ->
            next
    end,
    case CurMembers < data_family:get({max_role_num, FamilyLevel}) of
        true ->
            next;
        false ->
            erlang:throw({false, 4})
    end,
    case lists:keyfind(JoinRoleID, #family_member_info.role_id, Members) of
        false ->
            next;
        _ ->
            erlang:throw({false, 5})
    end,
    IsJoinRoleOnline = role_lib:is_online(JoinRoleID),
    JoinRoleInfo = 
        case IsJoinRoleOnline of
            true ->
                catch role_lib:call_server(JoinRoleID, get_role_info);
            false ->
                db_sql:get_roleInfo(JoinRoleID)
        end,
    case erlang:is_record(JoinRoleInfo, role) of
        true ->
            next;
        false ->
            erlang:throw({false, 6})
    end,
    #role{familyID=JoinRoleFamilyID, lastJoinFamily=LastJoinFamily} = JoinRoleInfo,
    case JoinRoleFamilyID =:= 0 of
        true ->
            next;
        false ->
            erlang:throw({false, 7})
    end,
    case LastJoinFamily + data_family:get(join_protect_seconds) < util:now() of
        true ->
            next;
        false ->
            erlang:throw({false, 8})
    end,
    {ok, JoinRoleInfo}.

do_refuse_join(FamilyInfo, RoleID, JoinRoleID) ->
    case catch check_can_refuse_join(FamilyInfo, RoleID, JoinRoleID) of
        {ok, FamilyID, BeRefusedRoleIDList, FamilyRequestList} ->
            %%?ERR("BeRefusedRoleIDList:~w, FamilyRequestList:~w", [BeRefusedRoleIDList, FamilyRequestList]),
            erlang:send(family_manager_server, {refuse_join, RoleID, JoinRoleID, FamilyID, BeRefusedRoleIDList, FamilyRequestList}),
            lists:foreach(fun(BeRefusedRoleID) ->
                              catch mail_server:send_sys_mail(BeRefusedRoleID, ?MAIL_FAMILY_BE_REFUSE, [FamilyInfo#family_info.family_name], "", [])    
                          end, BeRefusedRoleIDList);
        {false, Reason} ->
%%             ?ERR("Reason:~w", [Reason]),
            ?unicast(RoleID, #sc_family_refuse_join{result=Reason, is_self=true})
    end.

check_can_refuse_join(FamilyInfo, RoleID, JoinRoleID) ->
    #family_info{owner_role_id=OwnerRoleID, family_id=FamilyID} = FamilyInfo,
    case RoleID =:= OwnerRoleID of
        true ->
            next;
        false ->
            erlang:throw({false, 2})
    end,
    case JoinRoleID =:= 0 of
        false ->
            case ets:match_object(?ETS_FAMILY_REQUEST, #family_request{role_id=JoinRoleID, family_id=FamilyID, _='_'}) of
                [] ->
                    FamilyRequestList = [],
                    BeRefusedRoleIDList = [],
                    erlang:throw({false, 3});
                FamilyRequestList ->
                    BeRefusedRoleIDList = lists:map(fun(#family_request{role_id=BeRefusedRoleID}) -> BeRefusedRoleID end, FamilyRequestList)
            end;
        true ->
            case ets:match_object(?ETS_FAMILY_REQUEST, #family_request{family_id=FamilyID, _='_'}) of
                [] ->
                    FamilyRequestList = [],
                    BeRefusedRoleIDList = [],
                    erlang:throw({false, 3});
                FamilyRequestList ->
                    BeRefusedRoleIDList = lists:map(fun(#family_request{role_id=BeRefusedRoleID}) -> BeRefusedRoleID end, FamilyRequestList)
            end
    end,
    {ok, FamilyID, BeRefusedRoleIDList, FamilyRequestList}.

do_leave(FamilyInfo, RoleID) ->
    case catch check_can_leave(FamilyInfo, RoleID) of
        {ok, _KickMember, NewMembers, IsFamilyOwner} ->
            case IsFamilyOwner of
                false ->
                    do_leave2(FamilyInfo, NewMembers, RoleID);
                true ->
                    do_leave_owner(FamilyInfo, NewMembers, RoleID)
            end;
        {false, Reason} ->
            %%             ?ERR("Reason:~w", [Reason]),
            case Reason of
                5 ->
                    next;
                _ ->
                    family_misc:unlock_family_protect(RoleID)
            end,
            ?unicast(RoleID, #sc_family_leave{result=Reason, is_self=true, family_info=family_misc:gen_p_family_info()}),
            {ok, FamilyInfo}
    end.

%% 盟主离开，如果联盟只有盟主一人则解散，否则自动移交盟主给其他玩家
do_leave_owner(FamilyInfo, NewMembers, RoleID) ->
    case NewMembers of
        [] ->
            do_disband(FamilyInfo, RoleID);
        _ ->
            do_hand_over(FamilyInfo, NewMembers, RoleID)
    end.

do_disband(FamilyInfo, RoleID) ->
    FamilyID = FamilyInfo#family_info.family_id,
    db_sql:del_family_member(FamilyID, RoleID),
    db_sql:del_family_info(FamilyID),
    ?unicast(RoleID, #sc_family_leave{result=0, is_self=true, family_info=family_misc:gen_p_family_info()}),
    erlang:send(family_manager_server, {disband, FamilyID, RoleID}),
    {ok, ?undefined}.

gen_new_family_owner(NewMembers) ->
    [Member|LeftMembers] =
        lists:sort(fun(#family_member_info{role_level=RoleLevel1, fight_power=FightPower1, join_time=JoinTime1},
                       #family_member_info{role_level=RoleLevel2, fight_power=FightPower2, join_time=JoinTime2}) ->
                           if
                               RoleLevel1 > RoleLevel2 ->
                                   true;
                               RoleLevel1 < RoleLevel2 ->
                                   false;
                               FightPower1 > FightPower2 ->
                                   true;
                               FightPower1 < FightPower2 ->
                                   false;
                               JoinTime1 < JoinTime2 ->
                                   true;
                               true ->
                                   false
                           end
                   end, NewMembers),
    #family_member_info{role_id=RoleID, role_name=RoleName} = Member,
    {RoleID, RoleName, [Member#family_member_info{family_title=?FAMILY_TITLE_OWNER}|LeftMembers]}.

do_hand_over(FamilyInfo, NewMembers, RoleID) ->
    {NewOwnerRoleID, NewOwnerRoleName, NewMembers2} = gen_new_family_owner(NewMembers),
    NewFamilyInfo = FamilyInfo#family_info{members=NewMembers2, cur_members=erlang:length(NewMembers2), owner_role_id=NewOwnerRoleID, owner_role_name=NewOwnerRoleName},
    db_sql:del_family_member(NewFamilyInfo#family_info.family_id, RoleID),
    db_sql:set_family_info(NewFamilyInfo),
    ?unicast(RoleID, #sc_family_leave{result=0, is_self=true, family_info=family_misc:gen_p_family_info()}),
    update_online(RoleID, false),
    do_bc_msg_except(#sc_family_leave{result=0, is_self=false, family_info=family_misc:to_p_family_info(NewFamilyInfo)}, [RoleID]),
    erlang:send(family_manager_server, {leave, NewFamilyInfo, RoleID}),
    {ok, NewFamilyInfo}.
    
do_leave2(FamilyInfo, NewMembers, RoleID) ->
    NewFamilyInfo = FamilyInfo#family_info{members=NewMembers, cur_members=erlang:length(NewMembers)},
    db_sql:del_family_member(NewFamilyInfo#family_info.family_id, RoleID),
    db_sql:set_family_info(NewFamilyInfo),
%%     ?ERR("leave, RoleID:~w", [RoleID]),
    ?unicast(RoleID, #sc_family_leave{result=0, is_self=true, family_info=family_misc:gen_p_family_info()}),
    update_online(RoleID, false),
    do_bc_msg_except(#sc_family_leave{result=0, is_self=false, family_info=family_misc:to_p_family_info(NewFamilyInfo)}, [RoleID]),
    erlang:send(family_manager_server, {leave, NewFamilyInfo, RoleID}),
    {ok, NewFamilyInfo}.

check_can_leave(FamilyInfo, RoleID) ->
    #family_info{owner_role_id=OwnerRoleID, members=Members} = FamilyInfo,
    case family_misc:lock_family_protect(RoleID, ?PROTECT_FAMILY_LEAVE) of
        true ->
            next;
        false ->
            erlang:throw({false, 5})
    end,
    case lists:keytake(RoleID, #family_member_info.role_id, Members) of
        false ->
            erlang:throw({false, 4});
        {value, KickMember, NewMembers} ->
            {ok, KickMember, NewMembers, RoleID =:= OwnerRoleID}
    end.

do_kick(FamilyInfo, RoleID, KickRoleID) ->
    case catch check_can_kick(FamilyInfo, RoleID, KickRoleID) of
        {ok, KickMember, NewMembers} ->
            do_kick2(KickMember, NewMembers, FamilyInfo, RoleID, KickRoleID);
        {false, Reason} ->
%%             ?ERR("Reason:~w", [Reason]),
            case Reason of
                5 ->
                    next;
                _ ->
                    family_misc:unlock_family_protect(KickRoleID)
            end,
            ?unicast(RoleID, #sc_family_kick{result=Reason, is_self=true, family_info=family_misc:gen_p_family_info()}),
            {ok, FamilyInfo}
    end.

do_kick2(_KickMember, NewMembers, FamilyInfo, RoleID, KickRoleID) ->
    NewFamilyInfo = FamilyInfo#family_info{members=NewMembers, cur_members=erlang:length(NewMembers)},
    db_sql:del_family_member(NewFamilyInfo#family_info.family_id, KickRoleID),
    db_sql:set_family_info(NewFamilyInfo),
    PFamilyInfo = family_misc:to_p_family_info(NewFamilyInfo),
%%     ?ERR("RoleID:~w, KickRoleID:~w", [RoleID, KickRoleID]),
    ?unicast(RoleID, #sc_family_kick{result=0, is_self=true, family_info=PFamilyInfo}),
    ?unicast(KickRoleID, #sc_family_kick{result=0, is_self=false, family_info=family_misc:gen_p_family_info()}),
    update_online(KickRoleID, false),
    do_bc_msg_except(#sc_family_kick{result=0, is_self=false, family_info=PFamilyInfo}, [RoleID, KickRoleID]),
    erlang:send(family_manager_server, {kick, NewFamilyInfo, KickRoleID}),
    catch mail_server:send_sys_mail(KickRoleID, ?MAIL_FAMILY_BE_KICK, [], "", []),
    {ok, NewFamilyInfo}.

check_can_kick(FamilyInfo, RoleID, KickRoleID) ->
    #family_info{owner_role_id=OwnerRoleID, members=Members} = FamilyInfo,
    case family_misc:lock_family_protect(KickRoleID, ?PROTECT_FAMILY_KICK) of
        true ->
            next;
        false ->
            erlang:throw({false, 5})
    end,
    case RoleID =:= KickRoleID of
        true ->
            erlang:throw({false, 2});
        false ->
            next
    end,
    case RoleID =:= OwnerRoleID of
        true ->
            next;
        false ->
            erlang:throw({false, 3})
    end,
    case lists:keytake(KickRoleID, #family_member_info.role_id, Members) of
        false ->
            erlang:throw({false, 4});
        {value, KickMember, NewMembers} ->
            {ok, KickMember, NewMembers}
    end.

do_change_notice(RoleID, Notice, FamilyInfo) ->
    case catch check_can_change_notice(RoleID, Notice, FamilyInfo) of
        {ok, NewFamilyInfo} ->
%%             ?ERR("RoleID:~w, Notice:~w", [RoleID, Notice]),
            ?unicast(RoleID, #sc_family_change_notice{result=0,is_self=true, notice=Notice}),
            do_bc_msg_except(#sc_family_change_notice{result=0,is_self=false, notice=Notice}, [RoleID]),
            {ok, NewFamilyInfo};
        {false, Reason} ->
%%             ?ERR("Reason:~w", [Reason]),
            ?unicast(RoleID, #sc_family_change_notice{result=Reason, is_self=true, notice= <<"">>}),
            {ok, FamilyInfo}
    end.



check_can_change_notice(RoleID, Notice, FamilyInfo) ->
    #family_info{owner_role_id=OwnerRoleID} = FamilyInfo,
    case RoleID =:= OwnerRoleID of
        true ->
            next;
        false ->
            erlang:throw({false, 3})
    end,
    {ok, FamilyInfo#family_info{notice=Notice}}.

do_bc(Data, FamilyInfo) ->
    IsFamilyTalkMessage = erlang:is_record(Data, sc_talk_message),
    Data2 =
        case IsFamilyTalkMessage of
            false ->
                Data;
            true ->
                #sc_talk_message{roleID=RoleID} = Data,
                Data#sc_talk_message{familyTitle=get_role_family_title(RoleID, FamilyInfo)}
        end,
    lists:foreach(fun(RoleID) ->
%%                           ?ERR("RoleID:~w, Data:~w", [RoleID, Data]),
                          ?unicast(RoleID, Data2)
                  end, erlang:get(?online_role_id_list)),
    case IsFamilyTalkMessage of
        false ->
            {ok, FamilyInfo};
        true ->
            save_recent_talk_data(Data2, FamilyInfo)
    end.

do_bc_msg_except(Msg, ExceptRoleIDList) ->
    lists:foreach(fun(RoleID) ->
                          case lists:member(RoleID, ExceptRoleIDList) of
                              false ->
                                  ?unicast(RoleID, Msg);
                              true ->
                                  next
                          end
                  end, erlang:get(?online_role_id_list)).

save_recent_talk_data(Data, #family_info{talk_data=DataList}=FamilyInfo) ->
    NewDataList = lists:sublist([Data|DataList], ?RECENT_TALK_DATA_NUM),
    {ok, FamilyInfo#family_info{talk_data=NewDataList}}.
    
get_role_family_title(RoleID, FamilyInfo) ->
    #family_info{members=Members} = FamilyInfo,
    case lists:keyfind(RoleID, #family_member_info.role_id, Members) of
        false ->
            0;
        #family_member_info{family_title=FamilyTitle} ->
            FamilyTitle
    end.

update_family_member_info(FamilyInfo) ->
    #family_info{members=Members} = FamilyInfo,
    NewMembers =
        lists:foldr(fun(#family_member_info{role_id=RoleID}=Member, Acc) ->
                            NewMember = update_family_member_info(RoleID, Member),
                            [NewMember|Acc]
                    end, [], Members),
    FamilyInfo#family_info{members=NewMembers}.

update_family_member_info(RoleID, Member) ->
    case db_sql:get_roleInfo(RoleID) of
        #role{level=Level,title=Title,fightPower=FightPower} ->
            Member#family_member_info{role_level=Level, title=Title, fight_power=FightPower};
        _ ->
            Member
    end.













