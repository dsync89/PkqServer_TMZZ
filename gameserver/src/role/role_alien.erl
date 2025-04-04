-module(role_alien).

-compile(export_all).

-include("def_role.hrl").

cs_alien_info(_) ->
    case role_data:get_roleAlienInfo() of
        ?undefined ->
            #role{level=RoleLevel, vipLevel=VipLevel} = role_data:get_roleInfo(),
            NeedRoleLevel = data_alien:get(need_role_level),
            NeedVipLevel = data_alien:get(need_vip_level),
            case RoleLevel >= NeedRoleLevel andalso VipLevel >= NeedVipLevel of
                true ->
                    AlienInfo = #alien_info{times=data_common:get(max_alien_times), lastRecoverTime=util:now()},
                    role_data:set_roleAlienInfo(AlienInfo),
                    erlang:send(alien_server, {get_alien_info, role_data:get_roleID(), data_common:get(max_alien_times), 0});
                false ->
                    erlang:send(alien_server, {get_alien_info, role_data:get_roleID(), 0, 0})
            end;
        #alien_info{times=AlienTimes,resetTime=ResetTime} ->
            erlang:send(alien_server, {get_alien_info, role_data:get_roleID(), AlienTimes, ResetTime})
    end.

cs_alien_sign(_) ->
    #role{level=RoleLevel, vipLevel=VipLevel} = RoleInfo = role_data:get_roleInfo(),
    FighterList = role_data:get_posList(),
    ItemList = role_data:get_equipts_on_ger(),
    NeedRoleLevel = data_alien:get(need_role_level),
    NeedVipLevel = data_alien:get(need_vip_level),
    case RoleLevel >= NeedRoleLevel of
        true ->
            case VipLevel >= NeedVipLevel of
                true ->
                    erlang:send(alien_server, {alien_sign, RoleInfo, FighterList, ItemList});
                false ->
                    ?sendself(#sc_alien_sign{result=2})
            end;
        false ->
            ?sendself(#sc_alien_sign{result=1})
    end.

cs_alien_fight(#cs_alien_fight{tarRoleID=TarRoleID, tarRank=TarTank}) ->
    case catch check_can_alien_fight(TarRoleID) of
        {ok, RoleID, NeedTimes, NeedGold} ->
            erlang:send(alien_server, {alien_fight, RoleID, TarRoleID, TarTank, NeedTimes, NeedGold});
        {false, Reason} ->
            ?sendself(#sc_alien_fight{result=Reason,fightInfo=[],newRank=0,addCoin=0,fighterList=[]})
    end.

check_can_alien_fight(TarRoleID) ->
    #role{roleID=RoleID,level=RoleLevel,vipLevel=VipLevel,gold=Gold,goldBonus=GoldBonus} = role_data:get_roleInfo(),
    NeedRoleLevel = data_alien:get(need_role_level),
    NeedVipLevel = data_alien:get(need_vip_level),
    case TarRoleID of
        RoleID ->
            erlang:throw({false, 6});
        _ ->
            next
    end,
    case RoleLevel >= NeedRoleLevel of
        true ->
            next;
        false ->
            erlang:throw({false, 9})
    end,
    case VipLevel >= NeedVipLevel of
        true ->
            next;
        false ->
            erlang:throw({false, 10})
    end,
    case role_data:get_roleAlienInfo() of
        ?undefined ->
            role_data:set_roleAlienInfo(#alien_info{times=data_common:get(max_alien_times), lastRecoverTime=util:now()}),
            Times = data_common:get(max_alien_times);
        #alien_info{times=Times} ->
            next
    end,
    case Times >= 1 of
        true ->
            {ok, RoleID, 1, 0};
        false ->
            NeedGold = data_alien:get(fight_need_gold),
            case Gold + GoldBonus >= NeedGold of
                true ->
                    {ok, RoleID, 0, NeedGold};
                false ->
                    {false, 7}
            end
    end.

cs_alien_reset(_) ->
    #role{gold=Gold,goldBonus=GoldBonus} = RoleInfo = role_data:get_roleInfo(),
    FighterList = role_data:get_posList(),
    ItemList = role_data:get_equipts_on_ger(),
    NeedGold = data_alien:get(reset_gold),
    case Gold + GoldBonus >= NeedGold of
        true ->
            case role_data:get_roleAlienInfo() of
                #alien_info{resetTime=ResetTime} ->
                    case ResetTime =< util:now() of
                        true ->
                            erlang:send(alien_server, {alien_reset, RoleInfo, FighterList, ItemList});
                        false ->
                            ?sendself(#sc_alien_reset{result=4,timestamp=ResetTime})
                    end
            end;
        false ->
            ?sendself(#sc_alien_reset{result=1,timestamp=0})
    end.

cs_alien_guess(#cs_alien_guess{guessCoin=GuessCoin,guessType=GuessType}) ->
    case GuessType of
        false ->
            CoinValList = data_alien:get(coin_val_list_odd);
        true ->
            CoinValList = data_alien:get(coin_val_list_even)
    end,
    #role{roleID=RoleID,coin=RoleCoin} = RoleInfo = role_data:get_roleInfo(),
    case lists:member(GuessCoin, CoinValList) of
        true ->
            case RoleCoin >= GuessCoin of
                true ->
                    erlang:send(alien_server, {alien_guess, RoleID, RoleInfo, GuessCoin, GuessType});
                false ->
                    ?sendself(#sc_alien_guess{result=2})
            end;
        false ->
            ?sendself(#sc_alien_guess{result=1})
    end.

cs_alien_buy_times(#cs_alien_buy_times{buyTimes=BuyTimes}) when BuyTimes > 0 ->
    case role_data:get_roleAlienInfo() of
        #alien_info{times=255} ->
            ?sendself(#sc_alien_buy_times{result=3,newTimes=0});
        #alien_info{times=Times} = AlienInfo ->
            NewBuyTimes =
                case BuyTimes > 255 - Times of
                    true ->
                        255 - Times;
                    false ->
                        BuyTimes
                end,
            Price = data_alien:get(price),
            NeedGold = Price * NewBuyTimes,
            RoleInfo = role_data:get_roleInfo(),
            case role_lib:check_money(RoleInfo, gold, NeedGold) of
                true ->
                    role_lib:deduct_money_f(RoleInfo, gold, NeedGold, ?MONEY_DEC_TYPE_ALIEN_BUY_TIMES, 0, ""),
                    role_data:set_roleAlienInfo(AlienInfo#alien_info{times=Times+NewBuyTimes}),
                    ?sendself(#sc_alien_buy_times{result=0,newTimes=Times+NewBuyTimes});
                false ->
                    ?sendself(#sc_alien_buy_times{result=1,newTimes=0})
            end;
        ?undefined ->
            ?sendself(#sc_alien_buy_times{result=2,newTimes=0})
    end.







