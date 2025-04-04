-module(role_family).

-include("def_role.hrl").
-include("def_family.hrl").

-export([]).

cs_family_create(#cs_family_create{family_name=FamilyName, is_gold_create=IsGoldCreate}) ->
    case catch check_family_create(FamilyName, IsGoldCreate) of
        {true, RoleInfo, CostType, CostNum} ->
            RoleInfo2 = role_lib:deduct_money_f(RoleInfo, CostType, CostNum, ?MONEY_DEC_TYPE_CREATE_FAMILY, 0, ""),
            erlang:send(family_manager_server, {do_create, RoleInfo2, CostType, CostNum, FamilyName});
        {false, Reason} ->
            case Reason of
                10 ->
                    next;
                _ ->
                    family_misc:unlock_family_protect(role_data:get_roleID())
            end,
            %%?ERR("Reason:~w", [Reason]),
            #role{lastJoinFamily=LastJoinFamily} = role_data:get_roleInfo(),
            ?sendself(#sc_family_create{result=Reason, family_info=family_misc:gen_p_family_info(), timestamp=LastJoinFamily + data_family:get(join_protect_seconds)})
    end.

check_family_create(FamilyName, IsGoldCreate) ->
    #role{roleID=RoleID, level=RoleLevel, familyID=FamilyID, lastJoinFamily=LastJoinFamily} = RoleInfo = role_data:get_roleInfo(),
    case family_misc:lock_family_protect(RoleID, ?PROTECT_FAMILY_CREATE) of
        true ->
            next;
        false ->
             erlang:throw({false, 10})
    end,
    check_family_name(FamilyName),
    case RoleLevel >= data_family:get(family_open_level) of
        true ->
            next;
        false ->
            erlang:throw({false, 4})
    end,
    case FamilyID =:= 0 of
        true ->
            next;
        false ->
            erlang:throw({false,5})
    end,
    case LastJoinFamily + data_family:get(join_protect_seconds) >= util:now() of
        true ->
            erlang:throw({false, 9});
        false ->
            next
    end,
    case IsGoldCreate of
        true ->
            NeedGold = data_family:get(gold_create_need),
            case role_lib:check_money(RoleInfo, gold, NeedGold) of
                true ->
                    {true, RoleInfo, gold, NeedGold};
                false ->
                    erlang:throw({false, 6})
            end;
        false ->
            NeedCoin = data_family:get(coin_create_need),
            case role_lib:check_money(RoleInfo, coin, NeedCoin) of
                true ->
                    {true, RoleInfo, coin, NeedCoin};
                false ->
                    erlang:throw({false, 7})
            end
    end.

check_family_name(FamilyName) ->
%%     ?ERR("FamilyName:~w", [FamilyName]),
    DecodeList = util:gen_utf8_decode_list(erlang:list_to_binary(FamilyName), []),
    case util:check_blankName(DecodeList) of
        true ->
            Length = util:calc_name_length(DecodeList),
            {MinLength, MaxLength} = data_family:get(name_length_limit),
            if Length > MaxLength orelse Length < MinLength ->
                   erlang:throw({false, 2});
               true ->
                   case db_sql:search_familyName(util:latin1(FamilyName)) of
                       FamilyID when is_integer(FamilyID) ->
                           erlang:throw({false, 3});
                       ?undefined ->
                           ok
                   end
            end;
        _ ->
            erlang:throw({false, 1})
    end.

cs_family_request_join(#cs_family_request_join{family_id=FamilyID}) ->
    case catch check_can_request_join(FamilyID) of
        {ok, RoleInfo, OwnerRoleID} ->
            erlang:send(family_manager_server, {add_role_family_request, RoleInfo, FamilyID, OwnerRoleID});
        {false, Reason} ->
%%             ?ERR("Reason:~w", [Reason]),
            #role{lastJoinFamily=LastJoinFamily} = role_data:get_roleInfo(),
            ?sendself(#sc_family_request_join{result=Reason, is_self=true, timestamp=LastJoinFamily + data_family:get(join_protect_seconds)})
    end.

check_can_request_join(FamilyID) ->
    #role{roleID=RoleID,familyID=RoleFamilyID, level=RoleLevel, lastJoinFamily=LastJoinFamily} = RoleInfo = role_data:get_roleInfo(),
    case RoleFamilyID > 0 of
        false ->
            next;
        true ->
            erlang:throw({false, 1})
    end,
    case RoleLevel >= data_family:get(family_open_level) of
        true ->
            next;
        false ->
            erlang:throw({false, 2})
    end,
    case ets:lookup(?ETS_FAMILY_SUMMARY, FamilyID) of
        [] ->
            OwnerRoleID = 0,
            erlang:throw({false, 3});
        [#p_family_summary{cur_members=CurMembers, level=FamilyLevel, owner_role_id=OwnerRoleID}] ->
            case data_family:get({max_role_num, FamilyLevel}) > CurMembers of
                false ->
                    erlang:throw({false, 4});
                true ->
                    next
            end
    end,
    case ets:match_object(?ETS_FAMILY_REQUEST, #family_request{role_id=RoleID,family_id=FamilyID,_='_'}) of
        [] ->
            next;
        _ ->
            erlang:throw({false, 5})
    end,
    case erlang:length(ets:match_object(?ETS_FAMILY_REQUEST, #family_request{role_id=RoleID,_='_'})) >= data_family:get(role_request_max_num) of
        false ->
            next;
        true ->
            erlang:throw({false, 6})
    end,
    case erlang:length(ets:match_object(?ETS_FAMILY_REQUEST, #family_request{family_id=FamilyID,_='_'})) >= data_family:get(family_request_max_num) of
        false ->
            next;
        true ->
            erlang:throw({false, 7})
    end,
    case LastJoinFamily + data_family:get(join_protect_seconds) >= util:now() of
        true ->
            erlang:throw({false, 8});
        false ->
            next
    end,
    {ok, RoleInfo, OwnerRoleID}.

cs_family_cancel_join(#cs_family_cancel_join{family_id=FamilyID}) ->
    case catch check_can_cancel_join(FamilyID) of
        {ok, RoleID, FamilyRequest} ->
            erlang:send(family_manager_server, {del_role_family_request, RoleID, FamilyID, FamilyRequest});
        {false, Reason} ->
%%             ?ERR("FamilyID:~w, Reason:~w", [FamilyID, Reason]),
            ?sendself(#sc_family_cancel_join{result=Reason, is_self=true})
    end.

check_can_cancel_join(FamilyID) ->
    RoleID = role_data:get_roleID(),
    case ets:match_object(?ETS_FAMILY_REQUEST, #family_request{role_id=RoleID,family_id=FamilyID,_='_'}) of
        [] ->
            FamilyRequest = ?undefined,
            erlang:throw({false, 1});
        [FamilyRequest] ->
            next
    end,
    {ok, RoleID, FamilyRequest}.

cs_family_agree_join(#cs_family_agree_join{role_id=JoinRoleID}) ->
    case catch check_can_agree_join(JoinRoleID) of
        {ok, RoleID, FamilyID, FamilyRequest} ->
            family_misc:router_to_family_process(FamilyID, {agree_join, RoleID, FamilyID, JoinRoleID, FamilyRequest});
        {false, Reason} ->
%%             ?ERR("Reason:~w", [Reason]),
            case Reason of
                9 ->
                    next;
                _ ->
                    family_misc:unlock_family_protect(JoinRoleID)
            end,
            ?sendself(#sc_family_agree_join{result=Reason, is_self=true, family_info=family_misc:gen_p_family_info()})
    end.

check_can_agree_join(JoinRoleID) ->
    #role{roleID=RoleID, familyID=FamilyID} = _RoleInfo = role_data:get_roleInfo(),
    case family_misc:lock_family_protect(JoinRoleID, ?PROTECT_FAMILY_JOIN) of
        true ->
            next;
        false ->
            erlang:throw({false, 9})
    end,
    case FamilyID > 0 of
        true ->
            next;
        false ->
            erlang:throw({false, 1})
    end,
%%     ?ERR("FamilyID:~w, JoinRoleID:~w", [FamilyID, JoinRoleID]),
    case ets:match_object(?ETS_FAMILY_REQUEST, #family_request{role_id=JoinRoleID,family_id=FamilyID,_='_'}) of
        [] ->
            FamilyRequest = ?undefined,
            erlang:throw({false, 2});
        [FamilyRequest] ->
            next
    end,
    {ok, RoleID, FamilyID, FamilyRequest}.

cs_family_refuse_join(#cs_family_refuse_join{role_id=JoinRoleID}) ->
    case catch check_can_refuse_join(JoinRoleID) of
        {ok, RoleID, FamilyID} ->
            family_misc:router_to_family_process(FamilyID, {refuse_join, RoleID, JoinRoleID});
        {false, Reason} ->
%%             ?ERR("Reason:~w", [Reason]),
            ?sendself(#sc_family_refuse_join{result=Reason, is_self=true})
    end.

check_can_refuse_join(_JoinRoleID) ->
    #role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            next;
        false ->
            erlang:throw({false, 1})
    end,
    {ok, RoleID, FamilyID}.

cs_family_create_consume(#cs_family_create_consume{}) ->
    NeedCoin = data_family:get(coin_create_need),
    NeedGold = data_family:get(gold_create_need),
    ?sendself(#sc_family_create_consume{need_coin=NeedCoin, need_gold=NeedGold}).

cs_family_get_info(#cs_family_get_info{}) ->
    #role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            family_misc:router_to_family_process(FamilyID, {get_family_info, RoleID});
        false ->
            ?sendself(#sc_family_get_info{result=1, family_info=family_misc:gen_p_family_info()})
    end.

cs_family_kick(#cs_family_kick{kick_role_id=KickRoleID}) ->
    #role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            family_misc:router_to_family_process(FamilyID, {kick, RoleID, KickRoleID});
        false ->
            ?sendself(#sc_family_kick{result=1, is_self=true, family_info=family_misc:gen_p_family_info()})     
    end.

cs_family_leave(#cs_family_leave{}) ->
    #role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            family_misc:router_to_family_process(FamilyID, {leave, RoleID});
        false ->
%%             ?ERR("no family can not leave", []),
            ?sendself(#sc_family_leave{result=1, is_self=true, family_info=family_misc:gen_p_family_info()})     
    end.

cs_family_change_notice(#cs_family_change_notice{notice=Notice}) ->
    case catch check_can_change_notice(Notice) of
        {ok, RoleID, FamilyID} ->
            family_misc:router_to_family_process(FamilyID, {change_notice, RoleID, Notice});
        {false, Reason} ->
%%             ?ERR("Reason:~w", [Reason]),
            ?sendself(#sc_family_change_notice{result=Reason, is_self=true, notice= <<"">>})
    end.

check_can_change_notice(Notice) ->
    DecodeList = util:gen_utf8_decode_list(erlang:list_to_binary(Notice), []),
    Length = util:calc_name_length(DecodeList),
    MaxLength = data_family:get(notice_max_length),
    case Length =< MaxLength of
        true ->
            next;
        false ->
            erlang:throw({false, 1})
    end,
    #role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            next;
        false ->
            erlang:throw({false, 2})
    end,
    {ok, RoleID, FamilyID}.

cs_family_request_list(#cs_family_request_list{}) ->
    case catch check_can_get_request_list() of
        {ok, RequestList} ->
            %%?ERR("RequestList:~w", [RequestList]),
            ?sendself(#sc_family_request_list{request_list=RequestList, result=0});
        {false, Reason} ->
            ?sendself(#sc_family_request_list{request_list=[], result=Reason})     
    end.

check_can_get_request_list() ->
    #role{familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            next;
        false ->
            erlang:throw({false, 1})
    end,
%%     ?ERR("FamilyID:~w", [FamilyID]),
    RequestList = ets:match_object(?ETS_FAMILY_REQUEST, #family_request{family_id=FamilyID, _='_'}),
    {ok, lists:map(fun(Request) -> family_misc:to_p_family_request(Request) end, RequestList)}.

















