%% Author admin
-module(role_race).

-compile(export_all).
-include("def_role.hrl").
-include("def_battle.hrl").
-include("def_race.hrl").


%% ====================================================================
%% API functions
%% ====================================================================

cs_race_sign(#cs_race_sign{}) ->
    RoleInfo = role_data:get_roleInfo(),
    RoleID = role_data:get_roleID(),
    case erlang:whereis(race_server) of
        ?undefined ->
            ?sendself(#sc_race_sign{reason_code=?REASON_CODE_LOCAL_RACE_SERVER_IS_DOWN});
        _Pid ->
            erlang:send(race_server, {do_role_sign, RoleID, RoleInfo})
    end.

cs_race_auto_sign(#cs_race_auto_sign{}) ->
    RoleInfo = role_data:get_roleInfo(),
    RoleID = role_data:get_roleID(),
    case erlang:whereis(race_server) of
        ?undefined ->
            ?sendself(#sc_race_auto_sign{reason_code=?REASON_CODE_LOCAL_RACE_SERVER_IS_DOWN});
        _Pid ->
            erlang:send(race_server, {do_role_sign_auto, RoleID, RoleInfo})
    end.

cs_race_guess(#cs_race_guess{guessCoin=GuessCoin,roleID=GuessRoleID}) ->
    CoinValList = data_race:get(coin_val_list),
    case lists:member(GuessCoin, CoinValList) of
        true ->
            #role{roleID=RoleID,coin=RoleCoin} = role_data:get_roleInfo(),
            case RoleCoin >= GuessCoin of
                true ->
                    erlang:send(race_server, {do_race_guess, RoleID, GuessCoin, GuessRoleID});
                false ->
                    ?sendself(#sc_race_guess{result=1})
            end;
        false ->
            ?sendself(#sc_race_guess{result=5})
    end.