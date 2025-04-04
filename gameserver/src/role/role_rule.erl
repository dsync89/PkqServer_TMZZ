-module(role_rule).

-compile(export_all).

-include("def_role.hrl").
-include("def_rule.hrl").

hook_role_levelup(RoleInfo) ->
    case RoleInfo#role.level =:= data_rule:get(need_role_level) of
        true ->
            erlang:send(rule_server, {rule_sign, RoleInfo});
        false ->
            ignore
    end.

ger_view_other(RoleID, TarRoleID, ServerID, TarServerID) ->
    CrossServer = rule_master_server:get_cross_server_name(ServerID, false, data_setting:get(platform)),
    TarCrossServer = rule_master_server:get_cross_server_name(TarServerID, false, data_setting:get(platform)),
    ?CATCH(global:send(TarCrossServer, {ger_view_other, RoleID, TarRoleID, CrossServer})).

ger_view_other_dtl(RoleID, TarRoleID, ServerID, TarServerID) ->
    CrossServer = rule_master_server:get_cross_server_name(ServerID, false, data_setting:get(platform)),
    TarCrossServer = rule_master_server:get_cross_server_name(TarServerID, false, data_setting:get(platform)),
    ?CATCH(global:send(TarCrossServer, {ger_view_other_dtl, RoleID, TarRoleID, CrossServer})).

cs_rule_info(_) ->
    #role{level=RoleLevel} = RoleInfo = role_data:get_roleInfo(),
    NeedRoleLevel = data_rule:get(need_role_level),
    case RoleLevel >= NeedRoleLevel of
        true ->
            erlang:send(rule_server, {get_rule_info, RoleInfo});
        false ->
            ignore
    end.

rule_fight(MasterServer) ->
	?DEBUG("in gameserver[role_rule], rule_fight,  ~p~n", ["rule_fight"]),
	?ERR("Err-----role_fight...........:~w", [MasterServer]),
    case catch check_can_rule_fight(MasterServer) of
        {ok, RoleID, RuleTimes, RoleRule, TarRoleID, TarRoleName, TarFighterList, TarLieuAdd} ->
			?ERR("Err-----role_fight.do_rule_fight...........:~w", [MasterServer]),
            do_rule_fight(MasterServer, RoleID, RuleTimes, RoleRule, TarRoleID, TarRoleName, TarFighterList, TarLieuAdd);
        {false, Reason} ->
            Record = get_fight_error_record(Reason),
            ?sendself(Record)
    end.

do_rule_fight(MasterServer, RoleID, RuleTimes, #role_rule{score=Score}, TarRoleID, TarRoleName, TarFighterList, TarLieuAdd) ->
	?ERR("Err-----do_rule_fight  begin...........:~w", [MasterServer]),
    MyFighterList = role_data:get_fighter_list(),
    RoleLieuAdd = role_data:get_lieu_add_attr(),
	?ERR("Err-----do_rule_fight  MyFighterList...........:~w", [MyFighterList]),
	?ERR("Err-----do_rule_fight  RoleLieuAdd...........:~w", [RoleLieuAdd]),
    case catch role_fight:new(MyFighterList, TarFighterList, RoleLieuAdd, TarLieuAdd) of
        {IsWin, FightRecord, _} ->
            case IsWin of
                true ->
                    AddScore = data_rule:get(win_score),
                    SellReward = data_rule:get(win),
                    ArgID = 1;
                false ->
                    AddScore = 0,
                    SellReward = data_rule:get(lose),
                    ArgID = 0
            end,
            NewScore = Score + AddScore,
            NewMyFightPower = get_total_power(MyFighterList, RoleLieuAdd),
            NewTarFightPower = get_total_power(TarFighterList, TarLieuAdd),
            case rule_master_server:role_win(MasterServer, RoleID, AddScore, NewMyFightPower, NewTarFightPower, TarRoleID) of
                {ok, #role_rule{rank=NewRank,winTimes=NewWinTimes,fightTimes=NewFightTimes,winConTimes=NewWinConTimes,winConMaxTimes=NewWinConMaxTimes}, NewTarInfo} ->
                    role_data:set_rule_times(RuleTimes - 1),
                    role_reward:handle_sell_reward_f(role_data:get_roleInfo(), SellReward, ?MONEY_ADD_TYPE_RULE, ArgID, ""),
                    hist_server:hist_rule(TarRoleID, TarRoleName, RoleID, FightRecord, AddScore, NewScore),
                    Record =
                        #sc_rule_fight{result=0,isWin=IsWin,tarRoleName=TarRoleName,newScore=NewScore,
                                       rank=NewRank,winTimes=NewWinTimes,fightTimes=NewFightTimes,winConTimes=NewWinConTimes,winConMaxTimes=NewWinConMaxTimes,
                                       tarFighter= rule_server:to_tar_fighter(NewTarInfo),reward=role_reward:transform2p_mail_reward(SellReward),
                                       fightInfo=[FightRecord]},
                    ?sendself(Record);
                Err ->
                    ?ERR("Err:~w", [Err]),
                    Record = get_fight_error_record(7),
                    ?sendself(Record)
            end;
        Err ->
            ?ERR("Err:~w", [Err]),
            Record = get_fight_error_record(6),
            ?sendself(Record)
    end.

get_total_power(FighterList, {AtkAdd, HpAdd}) ->
    FighterList2 = ger_attr:refresh_other_fightPower(FighterList, AtkAdd, HpAdd),
    lists:foldl(fun(Ger, Acc) ->
                        Ger#ger.gerAttr#gerAttr.gerFightPower + Acc
                end, 0, FighterList2).

get_fight_error_record(Reason) ->
    #sc_rule_fight{result=Reason,isWin=false,tarRoleName= <<"">>,newScore=0,rank=0,winTimes=0,fightTimes=0,winConTimes=0,winConMaxTimes=0,
                   tarFighter= #p_rule_fighter{roleID=0,fightPower=0,isMale=false,title=0,head=0,level=0,
                                               roleName= <<"">>,score=0,rank=0,winTimes=0,fightTimes=0,winConTimes=0,winConMaxTimes=0},
                   fightInfo=[],reward=#p_mail_reward{}}.

check_can_rule_fight(MasterServer) ->
	?ERR("Err-----check_can_rule_fight...........:~w", [MasterServer]),
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true  -> next;
		false -> erlang:throw({false, 255})
	end,
	
    #role{roleID=RoleID,level=RoleLevel} = role_data:get_roleInfo(),
    NeedRoleLevel = data_rule:get(need_role_level),
    case RoleLevel >= NeedRoleLevel of
        true ->
            next;
        false ->
            erlang:throw({false, 1})
    end,
    RuleTimes = role_data:get_rule_times(),
    case RuleTimes > 0 of
        true ->
            next;
        false ->
            erlang:throw({false, 2})
    end,
	?ERR("Err-----gen_server:call ...........:~w", [MasterServer]),
    case catch gen_server:call({global, MasterServer}, {get_role_rule_tar_rule, RoleID}) of
        {ok, RoleRule, TarRoleRule} ->
            TarRoleID = RoleRule#role_rule.tarRoleID,
			?ERR("Err-----gen_server:call  targetid= ...........:~w", [TarRoleID]),
            case TarRoleRule of
                ?undefined ->
                   
					case catch gen_server:call({global, MasterServer}, {gen_fighterNew, RoleID}) of
						{ok, TarRoleIDNew} ->
							?ERR("Err-----gen_server:call  gen_fighterNew= ...........:~w", [TarRoleIDNew]);
						Exception ->
							?ERR("Err-----gen_server:call  gen_fighterNew1111111111111 ...........:~w", [MasterServer])
					end,
					#rolePublic{roleName = TarRoleName} = role_lib:get_rolePublic(TarRoleID);
                #role_rule{roleName =TarRoleName} ->
                    next
            end;
        Exception ->
			?ERR("Err-----gen_server:call  error ...........:~w", [MasterServer]),
            RoleRule = ?undefined,
            TarRoleID = ?undefined,
            TarRoleName = ?undefined,
            ?ERR("Exception:~w", [Exception]),
            erlang:throw({false, 4})
    end,
	?ERR("Err-----get_otherRoleFighter ...........:~w", [MasterServer]),
    case get_otherRoleFighter(RoleID, TarRoleID) of
        {ok, TarFighterList, TarLieuAdd} ->
			?ERR("Err-----get_otherRoleFighter1111 ...........:~w", [TarFighterList]),
            next;
        ?undefined ->
			?ERR("Err-----get_otherRoleFighter222222 ...........:~w", [MasterServer]),
            TarFighterList = ?undefined,
            TarLieuAdd = ?undefined,
            erlang:throw({false, 5})
    end,
	?ERR("Err-----check_can_rule_fight111...........:~w", [MasterServer]),
    {ok, RoleID, RuleTimes, RoleRule, TarRoleID, TarRoleName, TarFighterList, TarLieuAdd}.

get_otherRoleFighter(RoleID, TarRoleID) ->
    ServerID = util:role_id_to_server_id(RoleID),
    TarServerID = util:role_id_to_server_id(TarRoleID),
    case ServerID =/= TarServerID of
        true ->
            CrossServer = rule_master_server:get_cross_server_name(ServerID, false, data_setting:get(platform)),
            TarCrossServer = rule_master_server:get_cross_server_name(TarServerID, false, data_setting:get(platform)),
            ?CATCH(global:send(TarCrossServer, {get_otherRoleFighter, RoleID, TarRoleID, CrossServer})),
            receive
                {get_otherRoleFighter_return, [],{0,0}} ->
                    ?undefined;
                {get_otherRoleFighter_return, TarFighterList, TarLieuAdd} ->
                    {ok, TarFighterList, TarLieuAdd}
            after
                5000 ->
                ?undefined
            end;
        false ->
            case catch role_data:get_otherRoleFighter(TarRoleID) of
                {[],{0,0}} ->
                    ?undefined;
                {TarFighterList, TarLieuAdd} ->
                    {ok, TarFighterList, TarLieuAdd};
                _ ->
                    ?undefined
            end
    end.
